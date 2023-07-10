%% Get file list data

% imagedataroot = '/Volumes/Data/WHOI-2022/Data from Erik Anderson/TE_WHOI_2022_bottomview/experiments';
% imagedataroot = '/Users/etytel01/Documents/2023/ScupKinematics-old/raw_data/TE_WHOI_2022_bottomview/experiments';
imagedataroot = 'Z:\WHOI-2022\Data from Erik Anderson\TE_WHOI_2022_bottomview\experiments';
% calibFilePath = '/Users/etytel01/Documents/2023/ScupKinematics/processed_data/TE_WHOI_2022_bottomview/calibrations/TE_06_16_2022_1655_calibration.mat';
calibFilePath = 'D:\ScupKinematics\processed_data\TE_WHOI_2022_bottomview\calibrations\TE_06_16_2022_1655_calibration.mat';

% processeddataroot = '/Users/etytel01/Documents/2023/ScupKinematics/processed_data';
processeddataroot = 'D:\ScupKinematics\processed_data';

calibrationfilename = fullfile(processeddataroot, 'TE_WHOI_2022_bottomview/calibrations', ...
                                    'TE_06_16_2022_1655_calibration.mat');

filelistfile = fullfile(processeddataroot, "all_bottomview_files.csv");
outputfile = fullfile(processeddataroot, "processed_bottomview_files.csv");

dependencies = {which('importBottomviewData.m'), ...
    which('undistort_bottomview.m'), ...
    which('triangulate_head_tail.m')};
myscript = mfilename;
myscripthash_md5 = GetMD5Files([strcat(mfilename("fullpath"), ".m") dependencies]);

if is_file_newer(outputfile, filelistfile)
    filelist = readtable(outputfile, "Delimiter", ",");
else
    filelist = readtable(filelistfile,...
        "Delimiter",',');
end

%% Run through the files

if ~ismember("isprocessed", filelist.Properties.VariableNames)
    filelist.isprocessed = false(height(filelist), 1);
    filelist.myscript = repmat(myscript, [height(filelist) 1]);
    filelist.myscripthash_md5 = repmat(myscripthash_md5, [height(filelist) 1]);
end

writetable(filelist, outputfile, "Delimiter",",");        

todo = find(~filelist.isprocessed | ...
    ~strcmp(filelist.myscripthash_md5, myscripthash_md5))';

load(calibrationfilename, 'stereoParams');

totallines = sum(filelist.nrows(todo));

progress(0, totallines, "Triangulating...", 'show', 'all');
rowsdone = 0;

for i = todo
    filename = fullfile(imagedataroot, filelist.relpath{i});

    outputfile1 = fullfile(processeddataroot, "experiments", filelist.data3dfile{i});
    makedirs(outputfile1);

    [pn, fn] = fileparts(filename);
    fprintf("File %d of %d (%s)\n", i, height(filelist), fn);

    try
        if filelist.nrows(i) > 0
            data1 = importBottomviewData(filename);
            data1 = undistort_bottomview(data1, stereoParams);
        
            data1 = triangulate_head_tail(data1, stereoParams);
        
            writetable(data1, outputfile1, "Delimiter",",");        
        end
    
        filelist.isprocessed(i) = true;
        filelist.myscript{i} = myscript;
        filelist.myscripthash_md5{i} = myscripthash_md5;
        writetable(filelist, outputfile, "Delimiter",",");
    
        rowsdone = rowsdone + filelist.nrows(i);
        progress(rowsdone);
    catch ME
        warning("Caught error %s. Skipping file", ME.message);
        filelist.isprocessed(i) = false;
        writetable(filelist, outputfile, "Delimiter",",");
    
        rowsdone = rowsdone + filelist.nrows(i);
        progress(rowsdone);
    end 
end
    