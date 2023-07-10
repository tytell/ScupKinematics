function isnewer = is_file_newer(file2, file1)
% returns whether file2 is newer than file1

e1 = exist(file1, "file");
e2 = exist(file2, "file");

if (e1 && ~e2)
    isnewer = false;
elseif (e2 && ~e1)
    isnewer = true;
else
    f1 = dir(file1);
    f2 = dir(file2);

    isnewer = f2.datenum > f1.datenum;
end
