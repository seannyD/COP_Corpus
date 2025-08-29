try(setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/processing/"))

dataFolder = "../data/LEXIS/textsDiachronicClean/"
outFolder = "../data/LEXIS/textsDiachronicCleanSketchEngine/"
for(file in list.files(dataFolder,pattern = "*.csv")){
  d = read.csv(paste0("../data/LEXIS/textsDiachronicClean/",file),
               fileEncoding = "UTF-8",encoding = "UTF-8",stringsAsFactors = F)
  
  lines = paste(
    '<doc ',
    'ID="',d$ID,'" ',
    'COP="',as.character(d$COP),'" ',
    'country="',d$country ,'" ',
    'date="',d$date ,'"',
    'source="',d$source,'" ',
    ">\n",d$text,"</doc>", sep="")
  
  out = paste(lines,collapse="\n")
  
  writeLines(out,paste0(outFolder,gsub("\\.csv",".xml",file)))
  
}