# require(R.matlab)
# require(targets)
# require(tidyverse)
# require(lubridate)

prompt_run_matlab <- function(matlabscript, matlabsym, outputfile)
{
  outputbasename <- basename(outputfile)
  
  if (!file.exists(outputfile) || (file.mtime(matlabscript) > file.mtime(outputfile))) {
    cli::cli_alert_info("{outputbasename} is out of date. In MATLAB run {matlabsym} to update.")
    tar_cancel()
  } else {
    outputfile
  }
}

check_older <- function(script, outputfile) 
{
  if (!file.exists(outputfile) || (file.mtime(script) > file.mtime(outputfile))) {
    cli::cli_alert_info("{outputbasename} is out of date. In MATLAB run {matlabsym} to update.")
    TRUE
  } else {
    FALSE
  }
}

tar_matlab_script <- function(name, matlabscript, outputfile, matlabdeps = NULL)
{
  matlabsym <- basename(matlabscript) |> 
    tools::file_path_sans_ext() |> 
    paste0("_m")
  
  name_read <- deparse(rlang::enexpr(name))
  
  if (is.null(matlabdeps)) {
    deps <- c(matlabsym)
  } else {
    deps <- c(matlabsym, matlabdeps)
  }
  
  list(
    tar_target_raw(matlabsym, matlabscript, format = "file"),
    tar_target_raw(name_read, prompt_run_matlab(matlabscript, matlabsym, outputfile), 
                   format = "file",
                   deps = deps,
                   error = "null")
  )
}

tar_matlab_data <- function(name, matlabscript, outputfile,
                            matlabdeps = NULL)
{
  matlabsym <- basename(matlabscript) |> 
    tools::file_path_sans_ext() |> 
    paste0("_m")
  outputsym <- basename(outputfile) |> 
    make.names()
  name_read <- deparse(rlang::enexpr(name))
  
  if (is.null(matlabdeps)) {
    deps <- c(matlabsym)
  } else {
    deps <- c(matlabsym, matlabdeps)
  }

  list(
    tar_target_raw(matlabsym, matlabscript, format = "file"),
    tar_target_raw(outputsym, 
                   prompt_run_matlab(matlabscript, matlabsym, outputfile), 
                   format = "file",
                   deps = deps,
                   error = "null"),
    tar_target_raw(name_read, read_csv(outputfile), deps = c(outputsym))
  )
}
