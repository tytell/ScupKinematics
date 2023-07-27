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

scan_bottomview_files <- function(rootdir, outputfile)
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
  else
    cli::cli_alert_success("Found {nrow(allfiles)} files")
  
  allfiles <-
    allfiles |> 
    mutate(data3dfile = purrr::map_vec(relpath, make_output_filename),
           filesize = purrr::map_vec(fullfilepath, file.size,
                                     .progress = "Getting file sizes"),
           nrows = purrr::map_vec(fullfilepath, count_file_lines,
                                  .progress = "Getting number of lines"))
  write_csv(allfiles, outputfile)
  outputfile  
}

prompt_run_matlab <- function(matlabscript, outputfile, ...)
{
  if (!file.exists(outputfile) || (file.mtime(matlabscript) > file.mtime(outputfile))) {
    cli::cli_alert_danger("In MATLAB, run {matlabscript}")
    tar_cancel()
  }
  outputfile
}

check_matlab_processing <- function(matlabscript, processfile, ...)
{
  if (!file.exists(processfile) || (file.mtime(matlabscript) > file.mtime(processfile))) {
    cli::cli_alert_danger("In MATLAB, run {matlabscript}")
    tar_cancel()
  } else {
    processdata <- read_csv(processfile, show_col_types = FALSE)
    nprocessed <- round(sum(processdata$isprocessed)*100)
    if (nprocessed / nrow(processdata) > 80) {
      cli::cli_alert_success("More than 80% of files processed. We'll call that success!")
    } else {
      cli::cli_alert_info("{nprocessed}% of files processed. In MATLAB, run {matlabscript}")
      tar_cancel()
    }
  }
  processfile
}
