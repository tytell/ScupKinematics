require("R.utils")

## Look through the data folders and find all of the data files
# Store all of the sizes and number of lines in the files

scan_bottomview_files <- function(rootdir, outputfile, nfiles = Inf)
{
  tar_assert_file(rootdir)
  indivdirs <- Sys.glob(file.path(rootdir, '*', 'scup*'))
  if (length(indivdirs) == 0)
    cli::cli_alert_warning("Found 0 folders!")
  else
    cli::cli_alert_success("Found {length(indivdirs)} folders")
  
  allfiles <-
    map(indivdirs, 
        \(d) tibble(fullfilepath = 
                      list.files(d, pattern = 'scup\\d{2}_[0123456789.]+hz_2022Y_.+\\.xlsx?',
                                 recursive = FALSE, full.names = TRUE))) |> 
    list_rbind() 
  
  allfiles <-
    allfiles |> 
    mutate(relpath = getRelativePath(fullfilepath, relativeTo = rootdir),
           filename = basename(fullfilepath)) |> 
    separate_wider_regex(filename, c(id = "scup\\d+", "_",
                                     speed_Hz = "[0123456789.]+", "hz_",
                                     datetime = "2022Y_.+", "\\.xlsx?")) |> 
    mutate(datetime = parse_date_time(datetime, orders = "%YY_%mM_%dD_%Hh_%Mm_%Ss", 
                                      exact = TRUE,
                                      tz = "America/New_York"))
  if (nrow(allfiles) == 0)
    cli::cli_alert_warning("Found 0 files!")
  else {
    cli::cli_alert_success("Found {nrow(allfiles)} files")
    if (!is.infinite(nfiles))
      cli::cli_alert_success("Limiting to {nfiles} files")
  }
  
  allfiles <-
    allfiles |> 
    slice_head(n = nfiles) |> 
    mutate(data3dfile = purrr::map_vec(relpath, make_output_filename),
           filesize = purrr::map_vec(fullfilepath, file.size,
                                     .progress = "Getting file sizes"),
           nrows = purrr::map_vec(fullfilepath, count_file_lines,
                                  .progress = "Getting number of lines"))
  write_csv(allfiles, outputfile)
  outputfile  
}

count_file_lines <- function(filename, chunksize = 65536) {
  f <- file(filename, open="rb")
  
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", chunksize)) > 0) {
    nlines <- nlines + sum(chunk == as.raw(10L))
  }
  close(f)  
  
  nlines
}

make_output_filename <- function(filename)
{
  str_c(tools::file_path_sans_ext(filename), "-3D.csv")
}

## Prompt to run a Matlab script that produces an output file
# If the outputfile is newer than the Matlab script, it succeeds
prompt_run_matlab <- function(matlabscript, outputfile, ...)
{
  if (!file.exists(outputfile) || (file.mtime(matlabscript) > file.mtime(outputfile))) {
    cli::cli_alert_danger("In MATLAB, run {matlabscript}")
    tar_cancel()
  }
  outputfile
}

## Check Matlab processing using a process file
# The Matlab script indicates which files are processed in the data
# file. If more than 80% of them are listed as processed, the target
# succeeds
check_matlab_processing <- function(matlabscript, processfile, ...)
{
  if (!file.exists(processfile) || (file.mtime(matlabscript) > file.mtime(processfile))) {
    cli::cli_alert_danger("In MATLAB, run {matlabscript}")
    tar_cancel()
  } else {
    processdata <- read_csv(processfile, show_col_types = FALSE)
    nprocessed <- sum(processdata$isprocessed)
    pctprocessed <- round(nprocessed / nrow(processdata) * 100)
    cli::cli_alert_info("{pctprocessed}% ({nprocessed} / {nrow(processdata)}) of files processed.")
    if (pctprocessed > 80) {
      cli::cli_alert_success("More than 80% of files processed. We'll call that success!")
    } else {
      cli::cli_alert_info("In MATLAB, run {matlabscript}")
      tar_cancel()
    }
  }
  processfile
}

## Load the outline data file and reorganize the columns so that it's
# easier to deal with in R
read_data_file <- function(datarootpath, filename)
{
  cli::cli_alert_info("Read file {basename(filename)}")
  
  df <- read_csv(file.path(datarootpath, filename), id = "filename",
                 col_types = list(.default = col_double()),
                 show_col_types = FALSE) |> 
    reorganize_outline_data() |> 
    mutate(filename = factor(filename))
  cli::cli_alert_info("  {nrow(df)} row{?s}")
  df
}

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

