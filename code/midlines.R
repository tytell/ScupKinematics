require(dplyr)
require(logger)

get_central_axis <- function(df, x, y, dist, len, headlen = 0.4)
{
  df <-
    df |> 
    dplyr::filter({{dist}} / {{len}} < headlen)
  
  if (nrow(df) == 0)
  {
    logger::log_warn("No head points detected!")
    tibble(swimdirx = NA, swimdiry = NA)
  }
  else {
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
    
    tibble(swimdirx = S$v[1,1], swimdiry = S$v[2,1])
  }
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
