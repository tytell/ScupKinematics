require(dplyr)

#' Gets the midline from an outline
#' 
#' Takes a data frame that contains (at least) columns corresponding to
#' x and y coordinates of the outline and a column indicating which side
#' ("L" or "R").
#'
#' ## Steps:
#' 1. Uses a smoothing spline to smooth points on each side with smoothing
#'    parameter spar.
#' 2. Computes the arc length along each side based on the smoothed points.
#' 3. Uses the same smoothing spline to re-interpolate points that are evenly
#'    spaced along the arc length of each outline side.
#' 4. Gets the midline by taking the midpoint between each point on the now
#'    evenly spaced left and right outline points. Note that this midline is
#'    not evenly spaced in terms of arc length.
#' 5. Computes the arc length along the midline
#' 6. Interpolates evenly spaced points along the midline, using a smoothing
#'    spline with with an spar value of 0, so that it does not smooth the
#'    midline more.
#' 7. Uses the original left and right smoothing splines to re-interpolate points
#'    along the outline that correspond to the evenly spaced midline points. These
#'    will then not be evenly spaced relative to the arc length along the outline.
#'    
#' @param outline Data frame that contains the outline
#' @param x,y,side Columns in the data frame with the `x`, `y`, and `side` 
#'      variables (unquoted)
#' @param spar Smoothing parameter for `smooth.spline`. Default = 0.3
#' @param headtailweight The head and tail points are located in a different way
#'      and so we usually want to give them more so that the spline doesn't
#'      smooth them as much. All of the middle points have weight 1. Default = 10
#' @param npt Number of points to interpolate in the end. Default = 26
#' @param debug Return debug output if TRUE.
get_midline <- function(outline,
                        spar = 0.3, headtailweight = 10,
                        npt = 26, debug = FALSE)
{
  # pull out the columns from the outline data frame and rename them
  # also generate the weight. Duplicate the head and tail points so that
  # they are included on both the left and right sides
  outline <- outline |> 
    mutate(w = case_when(side == "H"  ~  headtailweight,
                         side == "T"  ~  headtailweight,
                         .default = 1)) |> 
    duplicate_head_tail()
  
  # sort the data by x coordinate and number the points along each side
  # should work correctly even if there are not the same number of points
  # on each side
  outline <- outline |> 
    arrange(side, desc(xctr)) |> 
    mutate(pt = seq(0, n()-1))
  
  # smooth the left side
  splinesL <- outline |> 
    filter(side == "L") |> 
    smooth_left_right(spar, npt)
  
  # smooth the right side
  splinesR <-
    outline |> 
    filter(side == "R") |> 
    smooth_left_right(spar, npt)

  if (is.null(splinesL) || is.null(splinesR)) {
    bad <- tibble(s = rep(NA_real_, npt),
                  xmid = NA_real_, ymid = NA_real_,
                  xL = NA_real_, yL = NA_real_,
                  xR = NA_real_, yR = NA_real_,
                  lendiff = NA_real_)
    
    return(bad)
  }

  outlineL <- splinesL$outlineeven
  colnames(outlineL) <- c("s", "outptL", "lenL", "xL", "yL")

  outlineR <- splinesR$outlineeven |> 
    select(-s)
  colnames(outlineR) <- c("outptR", "lenR", "xR", "yR")

  # join the smoothed left and right sides
  # calculate the fractional difference between the lengths of the left and
  # right sides. If they're very different, that suggests that there was an
  # outlier
  # also generate the unevenly spaced midline
  outline_even <- bind_cols(outlineL, outlineR) |> 
    mutate(lendiff = abs(lenL[1] - lenR[1]) / (0.5*(lenL[1] + lenR[1])),
           xmid0 = 0.5*(xL + xR),
           ymid0 = 0.5*(yL + yR),
           ds = sqrt((xmid0 - lag(xmid0))^2 + (ymid0 - lag(ymid0))^2))
  
  outline_even$ds[1] <- 0
  
  # calculate the arc length along the midline
  outline_even <- outline_even |> 
    mutate(smid = cumsum(ds),
           smid = smid / last(smid),
           w = case_when(s == 0  ~  headtailweight,
                         s == 1  ~  headtailweight,
                         .default = 1))

  # and make the splines to interpolate along the midline
  splinesM <- with(outline_even,
                   list(
                     xsp = stats::smooth.spline(x = smid, y = xmid0,
                                                spar = 0, cv = NA),
                     ysp = stats::smooth.spline(x = smid, y = ymid0,
                                                spar = 0, cv = NA)))
  
  # to generate the corresponding outline points, we have to figure out
  # how to map the midline arc length coordinate to the original point numbers
  # for the outlines. These splines do that
  ptL <- with(outline_even,
              spline(x = smid, y = outptL, xout = s)$y)
  ptR <- with(outline_even,
              spline(x = smid, y = outptR, xout = s)$y)
  
  # build up the evenly spaced midline and corresponding outline
  outline_even <- outline_even |> 
    rename(xL0 = xL, yL0 = yL, xR0 = xR, yR0 = yR) |> 
    mutate(xmid = predict(splinesM$xsp, x = s)$y,
           ymid = predict(splinesM$ysp, x = s)$y,
           xL = predict(splinesL$splines$xsp, x = ptL)$y,
           yL = predict(splinesL$splines$ysp, x = ptL)$y,
           xR = predict(splinesR$splines$xsp, x = ptR)$y,
           yR = predict(splinesR$splines$ysp, x = ptR)$y)
  
  if (debug) {
    outline_even
  } else {
    outline_even |> 
      select(s, xmid,ymid, xL,yL, xR,yR, lendiff)
  }
}

#' Smooths and evens spacing for one side of the outline
#' 
#' Smooths the raw outline points, then calculates the arc length along the
#' smoothed curve. Uses that arc length to interpolate the location of points
#' that are evenly spaced in terms of arc length.
#' 
#' @param df Data frame with columns `x`, `y`, and `w`
#' @param spar Smoothing parameter for `smooth.spline`
#' @param npt Number of points to interpolate
#' 
#' @returns A list with elements
#' * `outlineeven` Data frame with columns 
#'   - `s` arc length
#'   - `outpt` point number, based on the original integer point numbers
#'   - `len` total length of the curve
#'   - `x`, `y` Smoothed and evenly spaced coordinates of the curve
#' * `splines` A list with elements `xsp` and `ysp`, which are the splines that
#'   map from point number to `x` and `y` coordinates, respectively
smooth_left_right <- function(df, spar, npt)
{
  
  if ((sum(!is.na(df$xctr)) < 4) ||
      (sum(!is.na(df$yctr)) < 4)) {
    return(NULL)
  }

  # Run the first smoothing splines
  
  splines <-
    with(df,
       list(
         xsp = stats::smooth.spline(x = pt, y = xctr,
                                    spar = spar, w = w, cv = NA),
         ysp = stats::smooth.spline(x = pt, y = yctr,
                                    spar = spar, w = w, cv = NA)
       )
    )
  
  # Get the smooth x and y coordinates
  outlinesmooth <- 
    with(splines,
         tibble(
           pt = df$pt,
           xsm = predict(xsp)$y,
           ysm = predict(ysp)$y,
         )
    )
  
  # Build the arc length
  outlinesmooth <- outlinesmooth |> 
    mutate(ds = sqrt((xsm - lag(xsm))^2 + (ysm - lag(ysm))^2))
  
  outlinesmooth$ds[1] <- 0
  
  outlinesmooth <- outlinesmooth |> 
    mutate(
      s_act = cumsum(ds),
      len = last(s_act),
      s_act = s_act / len)
  
  s <- seq(0, 1, 1/(npt-1))
  
  # Map arc length back to point numbers
  pt_even <- with(outlinesmooth,
                  spline(s_act, pt, xout = s)$y)
  
  # And interpolate to evenly spaced arc length
  outlineeven <-
    with(splines,
         tibble(s = s,
                outpt = pt_even,
                len = outlinesmooth$len[1],
                x = predict(xsp, x = pt_even)$y,
                y = predict(ysp, x = pt_even)$y))
  
  list(outlineeven = outlineeven,
       splines = splines)
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