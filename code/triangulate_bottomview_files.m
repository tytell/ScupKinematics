function triangulate_bottomview_files(filenames, calibfilename, outputfilename)

load(calibfilename, 'stereoParams');

tab = import_bottomview_files(filenames);
tab = undistort_bottomview(tab, stereoParams);

tab = triangulate_head_tail(tab, stereoParams);

writetable(tab, outputfilename);
