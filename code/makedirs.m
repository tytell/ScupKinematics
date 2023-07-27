function makedirs(filename)

[pn, ~, ~] = fileparts(filename);
paths = split(pn, filesep);

if (length(paths) >= 1) && isempty(paths{1})
    paths{1} = '/';
end

for i = 1:length(paths)
    pn1 = fullfile(paths{1:i});
    if ~exist(pn1, "dir")
        mkdir(pn1);
    end
end
