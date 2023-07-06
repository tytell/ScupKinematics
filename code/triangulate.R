require(tidyverse)
require(R.utils)

get_all_bottomview_files <- function(videodatadir, outputfilename)
{
  if (!file.exists(videodatadir)) {
    cli::cli_alert_danger("Video data dir {videodatadir} does not exist. Maybe need to map a network drive?")
    stop("Raw data dir does not exist")
  }
    
  indivdirs <- Sys.glob(file.path(videodatadir, '*', 'scup*'))

  allfiles <-
    map(indivdirs, 
        \(d) tibble(fullfilepath = 
                      list.files(d, pattern = 'scup\\d{2}_[0123456789.]+hz_2022Y_.+\\.xlsx?',
                                 recursive = FALSE, full.names = TRUE))) |> 
    list_rbind() 

  allfiles <-
    allfiles |> 
    mutate(relpath = getRelativePath(fullfilepath, relativeTo = videodatadir),
           filename = basename(fullfilepath)) |> 
    separate_wider_regex(filename, c(id = "scup\\d+", "_",
                                     speed.Hz = "[0123456789.]+", "hz_",
                                     datetime = "2022Y_.+", "\\.xlsx?")) |> 
    mutate(datetime = parse_date_time(datetime, orders = "%YY_%mM_%dD_%Hh_%Mm_%Ss", 
                                      exact = TRUE,
                                      tz = "America/New_York"))

  write_csv(allfiles, outputfilename)
  outputfilename
}