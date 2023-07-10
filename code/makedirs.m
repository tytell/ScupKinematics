function makedirs(filename)

[pn, ~, ~] = fileparts(filename);
paths = split(pn, filesep);

for i = 1:length(paths)
    pn1 = fullfile(paths{1:i});
    if ~exist(pn1, "dir")
        mkdir(pn1);
    end
end
