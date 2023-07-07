%% Get file list data

% imagedataroot = '/Volumes/Data/WHOI-2022/Data from Erik Anderson/TE_WHOI_2022_bottomview/experiments';
imagedataroot = '/Users/etytel01/Documents/2023/ScupKinematics-old/raw_data/TE_WHOI_2022_bottomview/experiments';
calibFilePath = '/Users/etytel01/Documents/2023/ScupKinematics/processed_data/TE_WHOI_2022_bottomview/calibrations/TE_06_16_2022_1655_calibration.mat';
processeddataroot = '/Users/etytel01/Documents/2023/ScupKinematics/processed_data';

filelistfile = fullfile(processeddataroot, "all_bottomview_files.csv");

calibrationfilename = fullfile(processeddataroot, 'TE_WHOI_2022_bottomview/calibrations', ...
                                    'TE_06_16_2022_1655_calibration.mat');

outputfile = fullfile(processeddataroot, "data3d.csv");

filelist = readtable(filelistfile,...
    "Delimiter",',');

%% Run through the files

load(calibrationfilename, 'stereoParams');

for i = 1:height(filelist)
    filename = fullfile(imagedataroot, filelist.relpath{i});

    data1 = importBottomviewData(filename);
    data1 = undistort_bottomview(data1, stereoParams);

    data1 = triangulate_head_tail(data1, stereoParams);

    if (i == 1)
        opts = {'WriteVariableNames',true};
    else
        opts = {'WriteMode','Append',...
            'WriteVariableNames',false};
    end
    writetable(data1, outputfile, opts{:});        
end
    