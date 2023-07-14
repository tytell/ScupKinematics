function [tab] = importBottomviewData(filename, varargin)

% The data files are tab separated text files, but have the extension .xslx 
% as if they were Excel files.
% 
% These seem to be the columns:
%   Time (s)
%   Time (s), maybe rounded differently
%   Computer time (s)
%   Computer time (s), rounded
%   Image number
%   Speed of the flow tank (Hz)
%   Temperature (deg C)
% 
% Then we have eight columns that seem to represent the (x, y) corners of the region of interest (ROI) in each camera.
% 
% Then there are three columns I can't figure out.
% 
% Then the (x, y) location of the tail and head in each camera (8 columns).
% 
% Then, the algorithm seems to step along the x axis from one side of the 
% ROI to the other in the steps of 20 pixels, locating the y coordinate of 
% left and right sides of the fish's body (top and bottom in the image).

opt.startcolumns = {'tsec', 'tsec2', ...
    'comptimesec', 'comptimesec2', ...
    'imnum', 'speedHz', 'tempC', ...
    'roiC1x1', 'roiC1y1', 'roiC1x2', 'roiC1y2', ...
    'roiC2x1', 'roiC2y1', 'roiC2x2', 'roiC2y2', ...
    'V1', 'V2', 'V3', ...
    'tailC1x', 'tailC1y', 'headC1x', 'headC1y', ...
    'tailC2x', 'tailC2y', 'headC2x', 'headC2y'};
opt.spacing = 20;

opt = parsevarargin(opt, varargin, 2);

tab = readtable(filename, 'FileType', 'delimitedtext', ...
    'ReadVariableNames',false);

tab.Properties.VariableNames(1:length(opt.startcolumns)) = ...
    opt.startcolumns;

nptC1 = ceil((tab.roiC1x2(1) - tab.roiC1x1(1)) / opt.spacing);
nptC2 = ceil((tab.roiC2x2(1) - tab.roiC2x1(1)) / opt.spacing);

C1data1 = find(table2array(tab(1,:)) == tab.roiC1x1(1));

% pull out the xy data sets for each camera
coorddataC1 = table2array(tab(:,C1data1(2)-1 + (1:nptC1*4)));
coorddataC2 = table2array(tab(:,C1data1(2)-1 + nptC1*4 + (1:nptC2*4)));

tab = tab(:,1:length(opt.startcolumns));

% then the left and right sides of the fish's body in camera 1
xLC1 = coorddataC1(:,1:nptC1);
yLC1 = coorddataC1(:,nptC1 + (1:nptC1));
xRC1 = coorddataC1(:,2*nptC1 + (1:nptC1));
yRC1 = coorddataC1(:,3*nptC1 + (1:nptC1));

yLC1(yLC1 == 0) = NaN;
yRC1(yRC1 == 0) = NaN;

% and the same in camera 2
xLC2 = coorddataC2(:,1:nptC2);
yLC2 = coorddataC2(:,nptC2 + (1:nptC2));
xRC2 = coorddataC2(:,2*nptC2 + (1:nptC2));
yRC2 = coorddataC2(:,3*nptC2 + (1:nptC2));

yLC2(yLC2 == 0) = NaN;
yRC2(yRC2 == 0) = NaN;

outlinecols = table(xLC1, yLC1, xRC1, yRC1, xLC2, yLC2, xRC2, yRC2);
tab = cat(2, tab, outlinecols);

