function convertCSV

for s = [1:12 14:35]
    s
    load(['Raw Data\S',int2str(s)]);
    
    if s < 10
        subName = ['0' int2str(s)];
    else
        subName = int2str(s);
    end
    
    details = DATA.details;
    exportName = ['CSV Data/' subName '_details.txt'];
    fileID = fopen(exportName,'w');
    fprintf(fileID,'%d %s %s %s %s',details{1:5});
    fclose(fileID);
    
    pats = DATA.patterns;
    for p = 1:12
        pSet = squeeze(pats(p,:,:));
        exportName = ['CSV Data/Patterns/' subName '_Ps_Set' int2str(p) '.csv'];
        csvwrite(exportName,pSet);
    end
    
    td = DATA.trial_data;
    exportName = ['CSV Data/' subName '_td.csv'];
    csvwrite(exportName,td);
    
    ad = DATA.awareness_data;
    exportName = ['CSV Data/' subName '_ad.csv'];
    csvwrite(exportName,ad);
    
end