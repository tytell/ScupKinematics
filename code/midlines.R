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
  df <-
    df |> 
    distinct({{m}}, .keep_all = TRUE)
  
  m1 <- pull(df, {{m}})
  n1 <- pull(df, {{n}})
  
  m1max <- max(m1, na.rm = TRUE)
  m0[length(m0)] <- m1max
  
  mn <- approx(m1, n1, xout = m0) |> 
    as_tibble() |> 
    rename("{{m}}{names_suffix}" := x,
              "{{n}}{names_suffix}" := y) |> 
    mutate(!!pt := seq_along(m0))
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
