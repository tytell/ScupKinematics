# require(R.matlab)
require(cli)
require(glue)
require(stringr)
require(readr)
require(lubridate)

run_matlab_command <- function(cmd, scriptname, outputfile, timeout = 100)
{
  write_lines(cmd, scriptname)
  
  if (!file.exists(outputfile))
    oldtime <- NA
  else
    oldtime <- lubridate::ymd_hms(file.info(outputfile)$mtime)
  
  cli_alert_info("Please run {scriptname} in Matlab.")
  
  done <- FALSE
  start <- lubridate::now()
  cli_progress_bar("Waiting for Matlab", 
                   format = "{cli::pb_name} [{cli::pb_elapsed}]")
  while (!done && (lubridate::now() - start < timeout)) {
    if (file.exists(outputfile)) {
      newtime <- lubridate::ymd_hms(file.info(outputfile)$mtime)
      done <- newtime > oldtime
    }
    Sys.sleep(1)
    cli_progress_update()
  }
  
  if (done)
    cli_progress_done()
  else
    cli_progress_done(result = "failed")
  
  tar_cancel(!done)
}

calibrate_bottomview <- function(matlabfile, 
  calibpath, imageFileNames1, imageFileNames2,
                                 squaresize, outputfile)
{
  # put each file name in the image file lists inside single quotes, then
  # separate them with commas
  imageFileNames1str <- map(imageFileNames1, \(str) glue("'{str}'")) |> 
    paste(collapse = ",")
  imageFileNames2str <- map(imageFileNames2, \(str) glue("'{str}'")) |> 
    paste(collapse = ",")
  
  matlabfile <- basename(matlabfile) |> 
    tools::file_path_sans_ext()
  
  run_matlab_command(c(
    glue("calibpath = '{calibpath}';"),
    glue("imageFileNames1 = {{{imageFileNames1str}}};"),
    glue("imageFileNames2 = {{{imageFileNames2str}}};"),
    glue("outputfile = '{outputfile}';"),
    glue("{matlabfile}(calibpath, imageFileNames1, imageFileNames2, {squaresize}, outputfile);")
  ),
  file.path(matlabtempdir, "run_calibrate_bottomview.m"), outputfile
  )
  
  outputfile
}

triangulate_bottomview <- function(matlabfile, filenames, calibfile, outputfile,
                                   matlabdeps)
{
  cli_alert_info("filenames = {filenames}")
  
  filenamestr <- map(filenames, \(str) glue("'{str}'")) |> 
    paste(collapse = ',')
  
  matlabfile <- basename(matlabfile) |> 
    tools::file_path_sans_ext()
  
  run_matlab_command(c(
    glue("calibfile = '{calibfile}';"),
    glue("filenames = {{{filenamestr}}};"),
    glue("outputfile = '{outputfile}';"),
    glue("{matlabfile}(filenames, calibfile, outputfile);")
  ),
  file.path(matlabtempdir, "run_triangulate_bottomview.m"), outputfile
  )
}
