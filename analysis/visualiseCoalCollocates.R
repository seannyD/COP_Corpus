setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/")


d = read.csv("../data/COCA_Coal_Collocations.csv",stringsAsFactors = F)

d$word = gsub(" ","",d$word)

d$yearMiddle = as.numeric(sapply(strsplit(d$year,"-"),head,n=1)) + 2

cx =table(d$word)
selectedWords = names(cx[cx>=5])
d = d[d$word %in% selectedWords,]

#selectedWords = names(tail(sort(tapply(d$mi,d$word,max)),n=10))

#selectedWords = names(tail(sort(tapply(d$mi,d$word,sd)),n=10))

wx = c()
for(w in d$word){
  coef(lm(d[d$word==w,]$mi~d[d$word==w,]$yearMiddle))[2]
}

d$change = sapply(d$word, function(w){
  coef(lm(d[d$word==w,]$mi~d[d$word==w,]$yearMiddle))[2]
})

d$absChange = abs(d$change)

selectedWords = names(tail(sort(tapply(d$absChange,d$word,head,n=1)),n=10))
d = d[d$word %in% selectedWords,]

library(ggplot2)

ggplot(d,aes(x=yearMiddle,y=mi,color=word)) +
  geom_line() +
  xlab("Year") +
  ylab("Mutual Information") +
  ggtitle("Collocates of 'Coal' in COCA")
