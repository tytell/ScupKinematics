require(dplyr)

add_head_tail_points <- function(df, k)
{
  ptlo <- min(df$point, na.rm = TRUE)
  pthi <- max(df$point, na.rm = TRUE)
  
  dfh <- df |> 
    filter(point == ptlo)
  dft <- df |> 
    filter(point == pthi)
  
  assertthat::assert_that(nrow(dfh) == 1)
  assertthat::assert_that(nrow(dft) == 1)
  
  dfh <- dfh |> 
    mutate(point = ptlo-1,
           xctr_L = 0,
           yctr_L = 0,
           xctr_R = 0,
           yctr_R = 0)
  
  dft <- dft |> 
    mutate(point = pthi + 1,
           xctr_L = x.tail - x.head,
           yctr_L = y.tail - y.head,
           xctr_R = x.tail - x.head,
           yctr_R = y.tail - y.head)
  
  bind_rows(dfh, df, dft)
}

## CONTINUE here
## Make spline curves and get perpendicular vector at each point on left
# and right. Then look for intersection of perpendicular vector with curve
# on opposite side (how??) and find the halfway point
get_midline <- function(df, k)
{
  
}

duplicate_head_tail <- function(df)
{
  ht <- df |> 
    filter(side %in% c("T", "H"))
  
  htR <- ht |> 
    mutate(side = "R")
  htL <- ht |> 
    mutate(side = "L")
  
  bind_rows(df, htL, htR) |> 
    filter(side %in% c("L", "R"))
}


Mode <- function(x, na.action = na.omit) 
{
  x <- na.action(x)
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

find_contiguous_blocks <- function(t, mindur = 0.5, tol = 1e-6)
{
  df <-
    tibble(t) |> 
    mutate(dt = lead(t) - t,
           dtm = Mode(dt),
           contig = as.numeric(!near(dt, dtm, tol = tol)),
           contig = replace_na(contig, 1),
           contigid = cumsum(contig))
  
  df |> 
    group_by(contigid) |> 
    mutate(dur = max(t) - min(t),
           contigid = if_else(dur >= mindur, contigid, NA),
           contigid = if_else(contig == 0, contigid, NA)) |> 
    pull(contigid)
}

filtfiltends <- function(filtcoefs, x, na.action = na.omit)
{
  xs <- rep_along(x, NA_real_)
  x1 <- na.action(x)
  
  if (length(x1) > 0) {
    x1end <- c(rev(x1), x1, rev(x1))
    x1ends <- gsignal::filtfilt(filtcoefs, x1end)
    
    x1ends <- x1ends[(length(x1)+1):(2*length(x1))]
    
    good <- setdiff(seq_along(x), attr(x1, "na.action"))
    xs[good] <- x1ends
  }
  xs
}

resample_pts <- function(df, t, pts, dt = NULL, t_even = NULL)
{
  t0 <- pull(df, {{t}})
  
  if (is.null(t_even)) {
    if (is.null(dt)) {
      dt0 <- lead(t0) - t0
      
      dt = Mode(dt0)
    }
    
    tstart <- min(t0, na.rm = TRUE)
    tend <- max(t0, na.rm = TRUE)
    
    t_even = seq(tstart, tend, by = dt)
  }
  
  df_even <-
    df |> 
    select(c({{pts}})) |> 
    map(\(c) approx(t0, c, t_even)$y) |> 
    as_tibble()
  
  df_even <-
    df_even |> 
    mutate("{{t}}" := t_even)
  
  df_even
}

smooth_pts <- function(df, t, pts, filtercoefs = NULL,
                      name_suffix = "_s", tol = 1e-6,
                      cutoff.Hz = NULL, order = 9)
{
  t1 <- pull(df, {{t}})
  
  if (is.null(filtercoefs)) {
    dt1 <- na.omit(lead(t1) - t1)
    dtm <- Mode(dt1)
    if (any(!dplyr::near(dt1, dtm, tol = tol))) {
      cli::cli_abort("smooth_pts does not work on unevenly sampled data (range(dt) = {range(dt1)})")
    }
    sampfreq <- 1/dt1[1]
  
    filtercoefs <- gsignal::butter(order, cutoff.Hz / (sampfreq/2), 'low', 
                                   ouput = "Sos")
  }
  
  xys <- select(df, c({{pts}})) |> 
    purrr::map(\(c) filtfiltends(filtercoefs, c)) |> 
    as_tibble()
  
  xys |> 
    rename_with(\(cname) paste0(cname, name_suffix)) |> 
    bind_cols(df)
}



interp_side <- function(df, mcol,ncol, m.tail, n.tail, m0)
{
  m1 <- c(0, pull(df, {{mcol}}))
  n1 <- c(0, pull(df, {{ncol}}))
  npt <- length(m1)
  
  m.tail1 <- pull(df, {{m.tail}})[[1]]
  n.tail1 <- pull(df, {{n.tail}})[[1]]
  

  if (m1[npt] > m.tail1) {
    if (any(m1[1:npt-1]) > m.tail1)
      cli::cli_alert_warning("Points are past the tail")

    m1[npt] <- m.tail1
    n1[npt] <- n.tail1
  } else {
    m1 <- c(m1, m.tail1)
    n1 <- c(n1, n.tail1)
  }

  # mn <- approx(m1, n1, xout = m0)
  # colnames(mn) <- c("m0", "{{n}}_e")
  mn <- as_tibble(approx(m1, n1, xout = m0))
  colnames(mn) <- c("m0", "n0")
  mn
}


get_com <- function(df, width)
{
  df |> 
    group_by(filename, imnum) |> 
    mutate(com = sum(n * width, na.rm = TRUE) / sum(width)) 
}

even_time <- function(df, t, cols, dt)
{
  t1 <- rlang::eval_tidy(enquo(t), data = df)
  # s <- enquo(s)
  # s1 <- rlang::eval_tidy(s, data = df)
  
  t.even <- seq(t1[1], t1[length(t1)], by = dt)
  
  
  ty <- purrr::map(select(df, {{cols}}), 
                   \(y) approx(t1, y, xout = t.even)) |> 
    as.data.frame()
  
  ty <- ty[,c(1, seq(2, ncol(ty), by = 2))]
  tynames <- colnames(ty) |> 
    str_replace("\\.y$", ".even")
  tynames[1] <- "t.even"

  colnames(ty) <- tynames
  # ty[,quo_name(s)] <- s1[1]
  
  ty
}



get_midline_time_chunks <- function(df, maxchunkgap, minchunkdur)
{
  df |> 
    ungroup() |> 
    group_by(filename, fish, speedHz, tsec) |> 
    summarize(ngood = sum(!is.na(mid.even))) |> 
    filter(ngood == max(ngood)) |> 
    group_by(filename, fish, speedHz) |> 
    mutate(tfill = tsec) |> 
    fill(tfill, .direction = "down") |> 
    mutate(dt = tfill - lag(tfill),
           newchunk = is.na(dt) | dt > maxchunkgap,
           chunk = cumsum(newchunk)) |> 
    group_by(filename, fish, speedHz, chunk) |> 
    mutate(chunkdur = max(tsec, na.rm = TRUE) - min(tsec, na.rm = TRUE)) |> 
    filter(chunkdur > minchunkdur)
}  

even_midline_timing <- function(df, chunks, dt)
{
  df |> 
    left_join(chunks |> 
                select(filename, tsec, chunk),
              by = c("filename", "fish", "speedHz", "tsec")) |> 
    filter(!is.na(chunk)) |> 
    group_by(filename, speedHz, chunk, pt) |>
    nest(midline = c(tsec, imnum, s.even, m.even, mid.even, com)) |> 
    mutate(midline.t.even = 
             purrr::map(midline, \(df) even_time(df, tsec, c(com, m.even, mid.even), 0.02)),
           s.mn = purrr::map_vec(midline, \(df) mean(df$s.even, na.rm = TRUE))) |> 
    select(-midline) |> 
    unnest(midline.t.even)
}