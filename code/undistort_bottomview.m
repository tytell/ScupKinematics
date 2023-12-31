function tab = undistort_bottomview(tab, stereoParams, varargin)

opt.nstep = 10;
opt = parsevarargin(opt, varargin, 3);

roiC1 = table2array(tab(1,8:11));
roiC2 = table2array(tab(1,12:15));

txyC1 = cat(2, tab.tailC1x, tab.tailC1y);
txyC2 = cat(2, tab.tailC2x, tab.tailC2y);

oxyLC1 = cat(3, tab.xLC1,  tab.yLC1);
oxyRC1 = cat(3, tab.xRC1,  tab.yRC1);
oxyLC2 = cat(3, tab.xLC2,  tab.yLC2);
oxyRC2 = cat(3, tab.xRC2,  tab.yRC2);

hxyC1 = cat(2, tab.headC1x, tab.headC1y);
hxyC2 = cat(2, tab.headC2x, tab.headC2y);

% check that points are inside the ROI
txyC1 = NaNoutsideROI(txyC1, roiC1);
txyC2 = NaNoutsideROI(txyC2, roiC2);
hxyC1 = NaNoutsideROI(hxyC1, roiC1);
hxyC2 = NaNoutsideROI(hxyC2, roiC2);

oxyLC1 = NaNoutlineOutsideROI(oxyLC1, roiC1);
oxyRC1 = NaNoutlineOutsideROI(oxyRC1, roiC1);
oxyLC2 = NaNoutlineOutsideROI(oxyLC2, roiC1);
oxyRC2 = NaNoutlineOutsideROI(oxyRC2, roiC1);

% correct for camera distortion
indgood = find(all(~isnan(hxyC1), 2) & ...
    all(~isnan(hxyC2), 2) & ...
    all(~isnan(txyC1), 2) & ...
    all(~isnan(txyC2), 2));

hxyC1u = NaN(size(hxyC1));
hxyC2u = NaN(size(hxyC2));
txyC1u = NaN(size(txyC1));
txyC2u = NaN(size(txyC2));

oxyLC1u = NaN(size(oxyLC1));
oxyRC1u = NaN(size(oxyRC1));
oxyLC2u = NaN(size(oxyLC2));
oxyRC2u = NaN(size(oxyRC2));

% progress(0, length(indgood), "Undistorting...");
for i = 1:opt.nstep:length(indgood)
    if i+opt.nstep > length(indgood)
        k = indgood(i:end);
    else
        k = indgood(i:i+opt.nstep-1);
    end

    hxyC1u(k,:) = undistortPoints(hxyC1(k,:), stereoParams.CameraParameters1);
    hxyC2u(k,:) = undistortPoints(hxyC2(k,:), stereoParams.CameraParameters2);
    txyC1u(k,:) = undistortPoints(txyC1(k,:), stereoParams.CameraParameters1);
    txyC2u(k,:) = undistortPoints(txyC2(k,:), stereoParams.CameraParameters2);

    oxyLC1u(k,:,:) = undistort_outline1(oxyLC1(k,:,:), stereoParams.CameraParameters1);
    oxyRC1u(k,:,:) = undistort_outline1(oxyRC1(k,:,:), stereoParams.CameraParameters1);
    oxyLC2u(k,:,:) = undistort_outline1(oxyLC2(k,:,:), stereoParams.CameraParameters2);
    oxyRC2u(k,:,:) = undistort_outline1(oxyRC2(k,:,:), stereoParams.CameraParameters2);

%    progress(i);
end

% progress(length(indgood));

newcols = cat(2, txyC1u, hxyC1u, txyC2u, hxyC2u);
newcols = num2cell(newcols,1);
newcols = table(newcols{:}, 'VariableNames', ...
    {'tailC1xu', 'tailC1yu', 'headC1xu', 'headC1yu', ...
    'tailC2xu', 'tailC2yu', 'headC2xu', 'headC2yu'});

outlinecolsC1 = cat(3, oxyLC1u, oxyRC1u);
outlinecolsC2 = cat(3, oxyLC2u, oxyRC2u);
outlinecolsC1 = num2cell(outlinecolsC1,[1 2]);
outlinecolsC2 = num2cell(outlinecolsC2,[1 2]);

outlinecols = cat(4, outlinecolsC1, outlinecolsC2);

outlinecols = table(outlinecols{:}, 'VariableNames', ...
    {'xLC1u','yLC1u','xRC1u','yRC1u',...
    'xLC2u','yLC2u','xRC2u','yRC2u'});

tab = cat(2, tab, newcols, outlinecols);


function oxy1u = undistort_outline1(oxy1, params)

ox1 = oxy1(:,:,1);
oy1 = oxy1(:,:,2);

ox1u = NaN(size(ox1));
oy1u = NaN(size(oy1));

good = ~isnan(ox1) & ~isnan(oy1);

if any(good(:))
    oxygood = zeros(numel(ox1(good)), 2);
    oxygood(:,1) = ox1(good);
    oxygood(:,2) = oy1(good);
    
    oxyu1good = undistortPoints(oxygood, params);
    
    
    ox1u(good) = oxyu1good(:,1);
    oy1u(good) = oxyu1good(:,2);
end

oxy1u = cat(3, ox1u, oy1u);


function xy = NaNoutsideROI(xy, roi)

good = xy(:,1) > roi(1) & xy(:,1) < roi(3) & ...
    xy(:,2) > roi(2) & xy(:,2) < roi(4);
xy(~good,:) = NaN;


function oxy = NaNoutlineOutsideROI(oxy, roi)

good = oxy(:,:,1) > roi(1) & oxy(:,:,1) < roi(3) & ...
    oxy(:,:,2) > roi(2) & oxy(:,:,2) < roi(4);

good = repmat(good, [1 1 2]);
oxy(~good) = NaN;


