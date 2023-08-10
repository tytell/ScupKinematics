require(dplyr)

reorganize_outline_data <- function(df)
{
  df <-
    df |> 
    # first get the right columns
    select("filename", "tsec", "imnum", "speedHz", "tempC", 
           matches("[xy][LR]C[12]u_\\d+"),
           matches("(head|tail)C\\d[xy]u"),
           matches("(head|tail)[xyz]")) |> 
    # rename the head and tail columns
    rename_with(~str_replace(.x, "(head|tail)(C\\d)([xy])u", "\\3B\\2u_\\1")) |> 
    # pivot so that the outline is single x and y columns
    pivot_longer(matches("[xy][LRB]C[12]u_(head|tail|\\d+)"),
                 names_to = c('.value', "side", "camera", "point"),
                 names_pattern = "([xy])([LRB])(C[12])u_(.+)") |> 
    mutate(side = case_when(point == "head"  ~  "H",
                            point == "tail"  ~  "T",
                            .default = side))
  
  # get rid of points where nothing was found
  df <-
    df |> 
    mutate(notfound = y == x) |> 
    group_by(filename, imnum) |> 
    mutate(notfoundfr = all(notfound)) |> 
    ungroup() |> 
    filter(!notfoundfr) |> 
    select(-starts_with("notfound"))
  
  # and filter out points that are clear outliers
  df <-
    df |> 
    group_by(filename, camera, speedHz, imnum) |> 
    mutate(x.head = x[point == "head"],
           x.tail = x[point == "tail"],
           y.head = y[point == "head"],
           y.tail = y[point == "tail"],
           y.med = median(y, na.rm = TRUE),
           y.L.med = median(y[side == "L"], na.rm = TRUE),
           y.R.med = median(y[side == "R"], na.rm = TRUE),
           width.med = abs(y.R.med - y.L.med)) |> 
    filter((x >= x.tail) & (x <= x.head) &
             (abs(y - y.med) <= 1.5*width.med))
  
  df |> 
    ungroup() |> 
    mutate(xctr = x - x.head,
           yctr = y - y.head) |> 
    select(-contains("med"), -c(x, y)) |> 
    group_by(filename, imnum, camera) |> 
    nest(outline = c(xctr, yctr, side, point))
}

get_central_axis <- function(df, x, y)
{
  # this slightly dumb combination gives us a single long vector if x consists
  # multiple columns
  x1 <- select(df, {{x}}) |> 
    as.matrix() |> 
    as.vector()
  y1 <- select(df, {{y}}) |> 
    as.matrix() |> 
    as.vector()
  
  xy <- as.matrix(cbind(x1, y1))

  S <- svd(xy, nu = 0, nv = 2)
  
  # if (S$v[1,1] < 0)
  #   S$v[,1] <- -S$v[,1]
  
  tibble(swimdirx = S$v[1,1], swimdiry = S$v[2,1])
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

rotate_to_swimdir <- function(df, x, y, a, b, swimdirx, swimdiry)
{
  # xdf <- select(df, x)
  # ydf <- select(df, y)
  # 
  # if (ncol(xdf) != ncol(ydf)) {
  #   cli::cli_abort(c("x and y must have the same number of columns",
  #                    "i" = "x has {ncol(xdf)} column{?s} and y has {ncol(ydf)} column{?s}."))
  # }
  
  df |> 
    mutate("{a}" := .data[[x]] * swimdirx + .data[[y]] * swimdiry,
           "{b}" := .data[[x]] * (-swimdiry) + .data[[y]] * swimdirx)
}

interp_even_side <- function(df, m,n, m0, pt, names_suffix = "0")
{
  m <- enquo(m)
  n <- enquo(n)
  
  df <-
    df |> 
    distinct(!!m, .keep_all = TRUE)

  m1 <- rlang::eval_tidy(m, data = df)
  n1 <- rlang::eval_tidy(n, data = df)
  
  if (sum(!is.na(m1) & !is.na(n1)) <= 3) {
    mn <- tibble("{{m}}{names_suffix}" := rep_along(m0, NA_real_),
                 "{{n}}{names_suffix}" := rep_along(m0, NA_real_),
                 !!pt := seq_along(m0))
  } else {
    m1max <- max(m1, na.rm = TRUE)
    m0[length(m0)] <- m1max
    
    mn <- approx(m1, n1, xout = m0) |> 
      as_tibble()
    
    colnames(mn) <- c(paste0(quo_name(m), names_suffix),
                      paste0(quo_name(n), names_suffix))
    
    mn[,pt] <- seq_along(m0)
  }
  
  mn
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

process_filename <- function(filename)
{
  m <- str_match(filename, "(scup\\d+)_([\\d.]+)hz_(\\d{4}Y_\\d{2}M_\\d{2}D_\\d{2}h_\\d{2}m_\\d{2}s)")
  data <- m[,2:4] |> 
    as_tibble(.name_repair = "minimal")
  
  colnames(data) <- c("fish", "speedHz", "date")
  
  data |> 
    mutate(date = lubridate::ymd_hms(date, tz = "America/New_York"))
}

get_width <- function(df)
{
  df |> 
    unnest(edge.even) |> 
    group_by(fish, filename, imnum, camera, pt) |> 
    mutate(width = abs(n[2] - n[1])) |> 
    group_by(fish, pt) |> 
    summarize(width = median(width, na.rm = TRUE))
}

get_com <- function(df, width)
{
  df |> 
    group_by(filename, imnum) |> 
    mutate(com = sum(n * width, na.rm = TRUE) / sum(width)) 
}

remove_bad_frames <- function(df, width, nbadcutoff = 3)
{
  meanwidth <- mean(width$width, na.rm = TRUE)
  
  df |> 
    group_by(speedHz, camera, side) |> 
    unnest(edge.even) |> 
    mutate(side_sign = case_when(side == 'L'  ~  1,
                                 side == 'R'  ~  -1,
                                 .default = NA)) |> 
    left_join(width, by = c("fish", "pt")) |> 
    ungroup() |> 
    mutate(meanwidth = meanwidth) |> 
    group_by(camera, tsec) |> 
    mutate(goodpt = 
             case_when(pt < 20   ~  between(n*side_sign, -0.5*width, 2*width),
                       pt >= 20  ~  between(n*side_sign, -2*meanwidth, 2*meanwidth)),
           ngood = sum(goodpt, na.rm = TRUE),
           bad = ngood < n() - nbadcutoff) |> 
    filter(!bad) |> 
    mutate(n = if_else(!goodpt, NA, n))
}

even_points <- function(df, s,m,n, s.frac)
{
  s <- enquo(s)
  m <- enquo(m)
  n <- enquo(n)

  s1 <- rlang::eval_tidy(s, data = df)
  m1 <- rlang::eval_tidy(m, data = df)
  n1 <- rlang::eval_tidy(n, data = df)
  
  stopifnot(sum(!is.na(s1)) > 3)
  
  if (any(is.na(s1) | is.na(m1) | is.na(n1))) {
    dfnew <- tibble(s.even = rep_along(s.frac, NA_real_),
                    x = rep_along(s.frac, NA_real_),
                    y = rep_along(s.frac, NA_real_))
  }
  else {
    s0 <- s.frac * s1[length(s1)]
    
    tryCatch({
      m.even <- approx(s1, m1, xout = s0)
      n.even <- approx(s1, n1, xout = s0)
    },
    warning = function(w) {
      cli::cli_alert_warning(w)
    })
    
    dfnew = cbind(data.frame(pt = seq_along(s0)),
                  data.frame(m.even), 
                  data.frame(n.even$y))
  }
  
  colnames(dfnew) <- c("pt", "s.even",
                       paste0(quo_name(m), ".even"),
                       paste0(quo_name(n), ".even"))
  
  # bind_cols(df, dfnew)
  dfnew
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

get_midline_uneven <- function(df)
{
  df |> 
    ungroup() |> 
    mutate(n = if_else(m == 0, 0, n)) |> 
    group_by(filename, fish, speedHz, tsec, imnum, pt) |> 
    summarize(com = com[1],
              m = m[1],
              mid = mean(n, na.rm = TRUE))
}

even_midline_spacing <- function(df, s.frac)
{
  df |> 
    group_by(filename, imnum) |> 
    mutate(ds1 = sqrt((m - lag(m))^2 + (mid - lag(mid))^2),
           ds1 = replace_na(ds1, 0),
           s1 = cumsum(ds1)) |> 
    select(-ds1) |> 
    group_by(fish, filename, imnum) |> 
    nest(midline = c(pt, s1,m,mid)) |> 
    ungroup() |> 
    mutate(mid.even = purrr::map(midline, \(df) even_points(df, s1,m,mid, s.frac),
                                 .progress = TRUE)) |> 
    select(-midline) |> 
    unnest(mid.even)
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