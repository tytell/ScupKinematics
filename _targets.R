# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)
library(R.matlab)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "here", "R.matlab"),
  format = "qs", # Optionally set the default storage format. qs is fast.
)

#rawdatapath <- "/Volumes/Data/WHOI-2022/Data from Erik Anderson/TE_WHOI_2022_bottomview"
rawdatapath <- "/Users/etytel01/Documents/2023/Scup kinematics/raw_data/TE_WHOI_2022_bottomview"
processeddatapath <- "processed_data"
matlabtempdir <- "_matlab"

# Run the R scripts in the code/ folder
tar_source(files = "code")

tar_plan(
  calib_images_file = "raw_data/calibration2022_06_16_files.csv", 
  calib_images = read_csv(calib_images_file),
  calibrate_bottomview_m = "code/calibrate_bottomview.m",
  matlabcalibfile = calibrate_bottomview(calibrate_bottomview_m,
                                         rawdatapath,
                                         calib_images$right_images,
                                         calib_images$left_images,
                                         calib_images$square_size_mm[[1]],
                                         here("processed_data", 
                                              "TE_WHOI_2022_bottomview", 
                                              "calibrations", 
                                              "TE_06_16_2022_1655_calibration.mat"))
)
