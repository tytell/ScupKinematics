# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)
library(R.utils)
# library(R.matlab)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "here", "R.matlab"),
  format = "qs", # Optionally set the default storage format. qs is fast.
)

#rawdatapath <- "/Volumes/Data/WHOI-2022/Data from Erik Anderson/TE_WHOI_2022_bottomview"
videodatapath <- "/Users/etytel01/Documents/2023/ScupKinematics-old/raw_data/TE_WHOI_2022_bottomview"
rawdatapath <- "raw_data"
processeddatapath <- "processed_data"
matlabtempdir <- "_matlab"

# Run the R scripts in the code/ folder
tar_source(files = "code")

# list(
#   tar_file_read(calib_images, here("raw_data", "calibration2022_06_16_files.csv"),
#                 read_csv(!!.x)),
#   tar_file(calibrate_bottomview_m, "code/calibrate_bottomview.m"),
#   tar_target(matlabcalibfile,
#              calibrate_bottomview(calibrate_bottomview_m,
#                                   rawdatapath,
#                                   calib_images$right_images,
#                                   calib_images$left_images,
#                                   calib_images$square_size_mm[[1]],
#                                   here("processed_data",
#                                        "TE_WHOI_2022_bottomview",
#                                        "calibrations",
#                                        "TE_06_16_2022_1655_calibration.mat"))),
#   tar_target(bottomview_files_list,
#              get_all_bottomview_files(file.path(videodatapath, "experiments"), 
#                                       here(rawdatapath, "all_bottomview_files.csv"))),
#   tar_target(bottomview_files, read_csv(bottomview_files_list)),
#   tar_file(triangulate_bottomview_m, here("code", "triangulate_bottomview_files.m")),
#   tar_file(import_bottomview_m, here("code", "import_bottomview_files.m")),
#   tar_file(undistort_bottomview_m, here("code", "undistort_bottomview.m")),
#   tar_file(triangulate_head_tail_m, here("code", "triangulate_head_tail.m")),
#   tar_target(data3d,  
#              triangulate_bottomview(triangulate_bottomview_m, 
#                                     bottomview_files$fullfilepath,
#                                     matlabcalibfile,
#                                     here(processeddatapath,
#                                          "TE_WHOI_2022_bottomview",
#                                          "experiments",
#                                          "data3d.csv"),
#                                     c(import_bottomview_m, undistort_bottomview_m, triangulate_head_tail_m)))
#   # tar_target(all_bottomview_files,
#   #            get_all_bottomview_files(rawdatapath, 'all_bottomview_files.csv'),
#   #            type = "file"),
#   # tar_file_read(bottomview_files, all_bottomview_files, read_csv(!!.x)),
# )
  
list(
  tar_plan(
    calib_images_file = here(rawdatapath, "calibration2022_06_16_files.csv"),
    calib_images = read_csv(calib_images_file),
    calibrate_bottomview_m = here("code","calibrate_bottomview.m"),
    matlabcalibfile = calibrate_bottomview(calibrate_bottomview_m,
                                           videodatapath,
                                           calib_images$right_images,
                                           calib_images$left_images,
                                           calib_images$square_size_mm[[1]],
                                           here(processeddatapath,
                                                "TE_WHOI_2022_bottomview",
                                                "calibrations",
                                                "TE_06_16_2022_1655_calibration.mat")),
    
    bottomview_files_list = get_all_bottomview_files(file.path(videodatapath, "experiments"),
                                                here(rawdatapath, "all_bottomview_files.csv")),
    bottomview_files = read_csv(bottomview_files_list),
    
    triangulate_bottomview_m = here("code", "triangulate_bottomview_files.m"),
    import_bottomview_m = here("code", "import_bottomview_files.m"),
    undistort_bottomview_m = here("code", "undistort_bottomview.m"),
    triangulate_head_tail_m = here("code", "triangulate_head_tail.m"),
    # data3d = triangulate_bottomview(triangulate_bottomview_m,
    #                                 bottomview_files$fullfilepath,
    #                                 matlabcalibfile,
    #                                 here(processeddatapath,
    #                                      "TE_WHOI_2022_bottomview",
    #                                      "experiments",
    #                                      "data3d.csv"),
    #                                 c(import_bottomview_m, undistort_bottomview_m, 
    #                                   triangulate_head_tail_m))
    ),
  tar_group_size(triangulate_groups, bottomview_files, size = 2),
  tar_target(data3d, triangulate_bottomview(triangulate_bottomview_m,
                                            triangulate_groups$fullfilepath,
                                            matlabcalibfile,
                                            here(processeddatapath,
                                                 "TE_WHOI_2022_bottomview",
                                                 "experiments",
                                                 "data3d.csv"),
                                            c(import_bottomview_m, undistort_bottomview_m,
                                              triangulate_head_tail_m)),
             pattern = map(triangulate_groups))
)