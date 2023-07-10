function hash = GetMD5Files(files)

S = cell(length(files),1);
for i = 1:length(files)
    FID = fopen(files{i}, 'r');
    S{i} = fread(FID, inf, 'uchar=>char');
    fclose(FID);
end

Sall = cat(1,S{:});
hash = GetMD5(Sall, '8Bit');

    