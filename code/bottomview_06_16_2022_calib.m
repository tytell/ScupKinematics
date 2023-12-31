

%% Define images to process
% imageFilePath = '/Users/etytel01/Documents/2023/ScupKinematics-old/raw_data/TE_WHOI_2022_bottomview/calibrations/TE_06_16_2022_1655_stereo_calib_images';
if ispc()
    imageFilePath = 'Z:\WHOI-2022\Data from Erik Anderson\TE_WHOI_2022_bottomview\calibrations\TE_06_16_2022_1655_stereo_calib_images';
    calibFilePath = 'D:\ScupKinematics\processed_data\TE_WHOI_2022_bottomview\calibrations\TE_06_16_2022_1655_calibration.mat';
else
    imageFilePath = '/Volumes/Data/WHOI-2022/Data from Erik Anderson/TE_WHOI_2022_bottomview/calibrations/TE_06_16_2022_1655_stereo_calib_images';
    calibFilePath = '/Users/etytel01/Documents/2023/ScupKinematics/processed_data/TE_WHOI_2022_bottomview/calibrations/TE_06_16_2022_1655_calibration.mat';
end

imageFileNames1 = {'Right_Cam1.tif',...
    'Right_Cam2.tif',...
    'Right_Cam3.tif',...
    'Right_Cam4.tif',...
    'Right_Cam5.tif',...
    'Right_Cam6.tif',...
    'Right_Cam7.tif',...
    'Right_Cam8.tif',...
    'Right_Cam9.tif',...
    'Right_Cam10.tif',...
    'Right_Cam11.tif',...
    'Right_Cam12.tif',...
    'Right_Cam13.tif',...
    'Right_Cam14.tif',...
    'Right_Cam15.tif',...
    'Right_Cam16.tif',...
    'Right_Cam17.tif',...
    'Right_Cam18.tif',...
    'Right_Cam19.tif',...
    'Right_Cam20.tif',...
    'Right_Cam21.tif',...
    'Right_Cam22.tif',...
    };
imageFileNames2 = {'Left_Cam1.tif',...
    'Left_Cam2.tif',...
    'Left_Cam3.tif',...
    'Left_Cam4.tif',...
    'Left_Cam5.tif',...
    'Left_Cam6.tif',...
    'Left_Cam7.tif',...
    'Left_Cam8.tif',...
    'Left_Cam9.tif',...
    'Left_Cam10.tif',...
    'Left_Cam11.tif',...
    'Left_Cam12.tif',...
    'Left_Cam13.tif',...
    'Left_Cam14.tif',...
    'Left_Cam15.tif',...
    'Left_Cam16.tif',...
    'Left_Cam17.tif',...
    'Left_Cam18.tif',...
    'Left_Cam19.tif',...
    'Left_Cam20.tif',...
    'Left_Cam21.tif',...
    'Left_Cam22.tif',...
    };

for i = 1:length(imageFileNames1)
    imageFileNames1{i} = fullfile(imageFilePath, imageFileNames1{i});
    imageFileNames2{i} = fullfile(imageFilePath, imageFileNames2{i});
end

% Detect calibration pattern in images
detector = vision.calibration.stereo.CheckerboardDetector();
[imagePoints, imagesUsed] = detectPatternPoints(detector, imageFileNames1, imageFileNames2);

% Generate world coordinates for the planar patten keypoints
squareSize = 2.885000e+01;  % in units of 'millimeters'
worldPoints = generateWorldPoints(detector, 'SquareSize', squareSize);

%% Save out detected points

% ix1 = imagePoints(:,1,:,1);
% iy1 = imagePoints(:,2,:,1);
% ix2 = imagePoints(:,1,:,2);
% iy2 = imagePoints(:,2,:,2);
% 
% nimg = sum(imagesUsed);
% 
% img1 = repmat(reshape(imageFileNames1(imagesUsed), [1 nimg]), size(ix1,1), 1);
% img2 = repmat(reshape(imageFileNames2(imagesUsed), [1 nimg]), size(ix1,1), 1);
% 
% wx = repmat(worldPoints(:,1), [1 nimg]);
% wy = repmat(worldPoints(:,2), [1 nimg]);
% 
% tab = table(ix1(:), iy1(:), ix2(:), iy2(:), wx(:), wy(:), img1(:), img2(:), ...
%     'VariableNames',{'ximR', 'yimR', 'ximL', 'yimL', 'xw_mm', 'yw_mm', 'imR', 'imL'});
% tab.myscript = repmat(mfilename, [height(tab) 1]);
% tab.myscriptmd5 = repmat(GetMD5(strcat(mfilename('fullpath'), '.m'), 'File'), [height(tab) 1]);
% 
% [pn, fn, ~] = fileparts(calibFilePath);
% 
% writetable(tab, fullfile(pn, "detectedPoints.csv"));

%% Do the calibration
% Read one of the images from the first stereo pair
I1 = imread(imageFileNames1{1});
[mrows, ncols, ~] = size(I1);

% Calibrate the camera
[stereoParams, pairsUsed, estimationErrors] = estimateCameraParameters(imagePoints, worldPoints, ...
    'EstimateSkew', false, 'EstimateTangentialDistortion', false, ...
    'NumRadialDistortionCoefficients', 2, 'WorldUnits', 'millimeters', ...
    'InitialIntrinsicMatrix', [], 'InitialRadialDistortion', [], ...
    'ImageSize', [mrows, ncols]);

% View reprojection errors
h1=figure; showReprojectionErrors(stereoParams);

% Visualize pattern locations
h2=figure; showExtrinsics(stereoParams, 'CameraCentric');

% Display parameter estimation errors
displayErrors(estimationErrors, stereoParams);

% You can use the calibration data to rectify stereo images.
I2 = imread(imageFileNames2{1});
[J1, J2, reprojectionMatrix] = rectifyStereoImages(I1, I2, stereoParams);

save(calibFilePath, ...
    "stereoParams", "estimationErrors", "imagePoints", "worldPoints", ...
    "imageFileNames1", "imageFileNames2");
