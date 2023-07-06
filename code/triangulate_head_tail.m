function tab = triangulate_head_tail(tab, stereoParams, varargin)

opt.nstep = 100;
opt = parsevarargin(opt, varargin, 3);

txyC1u = cat(2, tab.tailC1xu, tab.tailC1yu);
txyC2u = cat(2, tab.tailC2xu, tab.tailC2yu);

hxyC1u = cat(2, tab.headC1xu, tab.headC1yu);
hxyC2u = cat(2, tab.headC2xu, tab.headC2yu);

% triangulate to 3D

indgood = all(isfinite(hxyC1u) & isfinite(hxyC2u), 2);
hxyz = NaN(size(hxyC1u,1), 3);
hxyz(indgood,:) = triangulate(hxyC1u(indgood,:), hxyC2u(indgood,:), stereoParams);

indgood = all(isfinite(txyC1u) & isfinite(txyC2u), 2);
txyz = NaN(size(hxyC1u,1), 3);
txyz(indgood,:) = triangulate(txyC1u(indgood,:), txyC2u(indgood,:), stereoParams);

newcols = cat(2, txyz, hxyz);
newcols = num2cell(newcols, 1);
newcols = table(newcols{:}, 'VariableNames', ...
    {'tailx', 'taily', 'tailz', ...
    'headx', 'heady', 'headz'});

tab = cat(2, tab, newcols);



