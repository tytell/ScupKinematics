function [tab] = import_bottomview_files(filenames)

% filenames = getfilenames(fullfile(datadir, strcat(basefilename, '*.xls')));

tok = regexp(filenames, '_(\d+\.\d+)hz_(\d{4})Y_(\d+)M_(\d+)D_(\d+)h_(\d+)m_(\d+)s\.xls', ...
    'tokens', 'once');
good = cellfun(@(x) ~isempty(x), tok);

filenames = filenames(good);
tok = tok(good);

toknum = cellfun(@str2num, cat(1,tok{:}));
speed = toknum(:,1);

date = datetime(toknum(:,2:end));

for i = 1:length(filenames)
    [tab1] = importBottomviewData(filenames{i});
    tab1.FileDate = repmat(date(i), [height(tab1) 1]);
    
    if i == 1
        tab = tab1;
    else
        tab = cat(1, tab, tab1);
    end
end

function S = catfields(dim, S1, S2)

fn1 = fieldnames(S1);

for i = 1:length(fn1)
    S.(fn1{i}) = cat(dim, S1.(fn1{i}), S2.(fn1{i}));
end
