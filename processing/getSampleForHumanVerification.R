try(setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/processing/"))

folder = "../data/LEXIS/textsDiachronicClean/"
d = NULL
for(file in list.files(folder,"*_UK.csv")){
  dx = read.csv(paste0("../data/LEXIS/textsDiachronicClean/",file),stringsAsFactors = F)
  d = rbind(d,dx)
}

d = d[nchar(d$text)<5000,]

# Average of about 400 words per article, say 3 minutes per article, 2 minutes to judge.
#  10 per hour, 3.5 hours = 36
# Everybody sees the same 8 articles and a further 32 unique articles.
# 12 people, so a total of 32*12 + 8 = 392
set.seed(34567)

# Balance samples over the different COP articles
sampleSizePerCop = 66
chosenArticles = unlist(
  tapply(1:nrow(d),
         d$COP,function(X){
           sample(X,sampleSizePerCop)}))
# Randomise order
chosenArticles = sample(chosenArticles)
chosenArticles = chosenArticles[1:((32*12)+8)]
commonArticles = chosenArticles[1:8]
# commonArticles = c("COP21_UK188", "COP21_UK305", "COP21_UK40", "COP22_UK100", "COP23_UK171", "COP23_UK79", "COP25_UK2", "COP26_UK299")
uniqueArticles = chosenArticles[9:length(chosenArticles)]

shOut = ""
for(coder in 1:12){
  startNum = 1+(32*(coder-1))
  subsel = uniqueArticles[startNum:(startNum+31)]
  subsel = c(subsel,commonArticles)
  subsel= sample(subsel)
  txtOut = paste("---\ngeometry: margin=2cm\n---\n# News Articles Set ",coder,"\n\n")
  anum = 1
  for(i in subsel){
    txt = d$text[i]
    txt = gsub("\\\\[a-z]*","",txt)
    txt = gsub("https?://.+?[ ,;?!]"," ",txt)
    tx = data.frame(txt)
    names(tx) = paste(anum,":",d$ID[i])
    anum = anum+1
    txtOut = paste0(txtOut, "\n\n\\clearpage\\newpage\n\n", paste0(knitr::kable(tx),collapse="\n"))
  }
  outfile = paste0("../data/HumanJudgements/stimuli/NewsArticles_",coder,".md")
  cat(txtOut,file = outfile)
  shOut = paste(shOut,"\n",
    (paste0("pandoc ",outfile," -o ",gsub("\\.md",".pdf",outfile))))
  
  write.csv(data.frame(Num=1:length(subsel),
                       ID = d[subsel,]$ID,
                       "Sustainability"="",
                       "Security"="",
                       "Accessibility"=""),
            file = paste0("../data/HumanJudgements/stimuli/NewsArticles_", coder,"_Judgements.csv"),row.names = F)
}
cat(shOut,file="makeHJStimuli.sh")