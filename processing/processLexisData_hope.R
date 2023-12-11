try(setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/processing/"))

library(quanteda)
library(stringr)
library(lubridate)
library(ggplot2)
library(lme4)
library(sjPlot)

hopeKeywords = c("hope","hoping", "hoped","hopes","hopeful")
despairKeywords = c("despair","despairing","desperation",
                 "despaired","despairs",
                 "hopeless","hopelessness")

N.HopeKeyword = length(hopeKeywords)
N.DespairKeyword = length(despairKeywords)

processFile = function(d,country,confStart=NA,confEnd=NA){
  # Lower case
  d$text = tolower(d$text)
  
  myDFM = function(tok){
    #allWords = unique(unlist(tok))
    tx = t(sapply(tok,function(t){
      t[!t %in% c(hopeKeywords,despairKeywords)] = "X"
      sapply(c(hopeKeywords,despairKeywords,"X"),
             function(w){sum(t==w)})}))
  }

  corp2Scores = function(d){
    # Create corpus, tokens, freq matrix
    corp = corpus(d, docid_field = "ID",text_field = "text")
    tok = tokens(corp, remove_punct = TRUE)
    #corpDFM = dfm(tok)
    corpDFM = myDFM(tok)
    totalWords = sum(corpDFM)
    
    # Get frequency for one keyword
    getFrequency = function(keyword){
      keyword = tolower(keyword)
      if(grepl(" ",keyword)){
        # Multi-word expression
        return(length(unlist(str_extract_all(d$text, keyword))))
      }
      if(keyword %in% colnames(corpDFM)){
        return(colSums(corpDFM[,keyword]))
      }
      return(0)
    }
    
    # Get score (frequency per 1000000 words)
    getScore = function(keywords){
      #freq = sapply(keywords,getFrequency)
      freq = colSums(corpDFM[,keywords])
      prop = 1000000 * (freq/totalWords)
      return(prop)
    }
    
    #AccFreqs = getScore(accessibilityKeywords)
    #SecFreqs = getScore(securityKeywords)
    #SusFreqs = getScore(sustainabilityKeywords)
    HopeFreqs = getScore(hopeKeywords)
    DespairFreqs = getScore(despairKeywords)
    return(list(#AccFreqs=AccFreqs,SecFreqs=SecFreqs,
                #SusFreqs=SusFreqs,
                #totalWords=totalWords))
                HopeFreqs=HopeFreqs,
                DespairFreqs = DespairFreqs,
                totalWords=totalWords))
  }
  
  # Measure full corpus
  fullScores = corp2Scores(d)
  #AccFreqs = fullScores$AccFreqs
  #SecFreqs = fullScores$SecFreqs
  #SusFreqs = fullScores$SusFreqs
  HopeFreqs = fullScores$HopeFreqs
  DespairFreqs = fullScores$DespairFreqs

  freqData = data.frame(
    COP = d$COP[1],
    country = d$country[1],
    aspect = c(#rep("Accessibility",length(AccFreqs)),
               #rep("Security",length(SecFreqs)),
               #rep("Sustainability",length(SusFreqs))),
                rep("Hope",length(HopeFreqs)),
                rep("Despair",length(DespairFreqs))),
    #keyword = c(accessibilityKeywords,securityKeywords,sustainabilityKeywords),
    keyword = c(hopeKeywords,despairKeywords),
    #freq = c(AccFreqs, SecFreqs, SusFreqs)
    freq = c(HopeFreqs,DespairFreqs)
  )
  #write.csv(freqData, 
  #          file=paste0("../results/LexisFrequencies/",
  #                 d$COP[1],"_",
  #                 gsub(" ","-",country),
  #                 ".csv"))
  
  # Measure before/during/after
  #beforeScores = list(AccFreqs=0,SecFreqs=0,SusFreqs=0)
  #duringScores = list(AccFreqs=0,SecFreqs=0,SusFreqs=0)
  #afterScores = list(AccFreqs=0,SecFreqs=0,SusFreqs=0)
  beforeScores = list(HopeFreqs=0,DespairFreqs=0)
  duringScores = list(HopeFreqs=0,DespairFreqs=0)
  afterScores = list(HopeFreqs=0,DespairFreqs=0)
  
  if(!is.na(confStart)){
    aDates = lubridate::parse_date_time(d$date, c('mdY','mdYHM',"bdYAHMpU"))
    d$phase = cut(aDates,
                  c(parse_date_time("01/01/1900","dmy"),
                    confStart,confEnd,
                    parse_date_time("01/01/2100","dmy")), 
                  labels = c("Before","During","After"))
    if(sum(d$phase=="Before",na.rm=T)>2){
      beforeScores = corp2Scores(d[!is.na(d$phase) & d$phase=="Before",])
    }
    if(sum(d$phase=="During",na.rm=T)>2){
      duringScores = corp2Scores(d[!is.na(d$phase) & d$phase=="During",])
    }
    if(sum(d$phase=="After",na.rm=T)>2){
      afterScores = corp2Scores(d[!is.na(d$phase) & d$phase=="After",])
    }
  }  
  
  ret = data.frame(
    "COP" = d$COP[1],
    "country" = country,
    #"accessibility" = sum(AccFreqs),
    #"security" = sum(SecFreqs),
    #"sustainability" = sum(SusFreqs),
    "hope" = sum(HopeFreqs),
    "despair" = sum(DespairFreqs),
    #"beforeAcc" = sum(beforeScores[[1]]),
    #"duringAcc" = sum(duringScores[[1]]),
    #"afterAcc" = sum(afterScores[[1]]),
    #"beforeSec" = sum(beforeScores[[2]]),
    #"duringSec" = sum(duringScores[[2]]),
    #"afterSec" = sum(afterScores[[2]]),
    #"beforeSus" = sum(beforeScores[[3]]),
    #"duringSus" = sum(duringScores[[3]]),
    #"afterSus" = sum(afterScores[[3]]),
    beforeHope = sum(beforeScores[[1]]),
    duringHope = sum(duringScores[[1]]),
    afterHope = sum(afterScores[[1]]),
    beforeDespair = sum(beforeScores[[2]]),
    duringDespair = sum(duringScores[[2]]),
    afterDespair = sum(afterScores[[2]]),
    "docs" = nrow(d),
    "totalWords" = fullScores$totalWords
  )
  
  return(ret)
  
}

#uk = processFile("../data/LEXIS/COP26_UK.csv")
#usa = processFile("../data/LEXIS/COP26_USA.csv")
#ireland = processFile("../data/LEXIS/COP26_IRELAND.csv")
#rbind(uk,usa,ireland)

confDates = read.delim("../data/copDates.tab",sep="\t")
confDates$start = lubridate::parse_date_time(confDates$start,"dmY")
confDates$end = lubridate::parse_date_time(confDates$end,"dmY")
confDates$end = confDates$end + days(1)

folder = "../data/LEXIS/textsDiachronicClean/"
res = NULL
for(file in list.files(folder,"*.csv")){
  print(file)
  filename = paste0(folder,file)
  country = gsub("\\.csv","",filename)
  country = unlist(strsplit(country,"_"))
  country = country[length(country)]

  # Load data
  d= read.csv(filename,stringsAsFactors = F)
  # Date
  confDt = confDates[match(d$COP[1],confDates$COP),]
  
  # Process data
  newData= processFile(d,country,confDt$start,confDt$end)
  # Add to big data frame
  res = rbind(res,newData)
}

write.csv(res,"../data/LEXIS/TrilemmaScores_byCountryAndYear_HOPE.csv",row.names = F)

# Overall for each country in all years
overallData = NULL
for(country in unique(res$country)){
  print(country)
  dCountry = NULL
  for(file in list.files(folder,paste0(country,"*.csv"))){
    filename = paste0(folder,file)
    dx= read.csv(filename,stringsAsFactors = F)
    dCountry = rbind(dCountry, dx)
  }
  overallCountryData = processFile(dCountry,country)
  overallCountryData$COP = "ALL"
  overallData = rbind(overallData, overallCountryData)
}

write.csv(overallData, "../data/LEXIS/TrilemmaScores_byCountry_HOPE.csv")

sum(overallData$totalWords)
sum(overallData$docs)

################
ggplot(res,aes(x=COP, y=hope, color=country)) +
  geom_point() +
  stat_smooth(aes(group = 1))

ggplot(res,aes(x=COP, y=despair, color=country)) +
  geom_point() +
  stat_smooth(aes(group = 1))

ggplot(res,aes(x=COP, y=despair, color=country)) +
  geom_point() +
  stat_smooth(aes(group = 1))


ggplot(res,aes(x=1,y=beforeHope)) +
  geom_violin() +
  geom_violin(aes(x=2,y=duringHope)) +
  geom_violin(aes(x=3,y=afterHope))

ggplot(res,aes(x=1,y=beforeDespair)) +
  geom_violin() +
  geom_violin(aes(x=2,y=duringDespair)) +
  geom_violin(aes(x=3,y=afterDespair))

dxHope = data.frame(
  freq = c(res$beforeHope,res$duringHope,res$afterHope),
  phase = rep(c("before","during","after"),length.out = nrow(res)*3),
  country = rep(res$country,each=3),
  COP = rep(res$COP,each=3)
)
dxHope$phase = factor(dxHope$phase,levels = c("before","during","after"), ordered = T)
m0 = lmer(freq ~ 1 + (1|country) + (1|COP), data=dxHope)
m1 = lmer(freq ~ phase + (1|country) + (1|COP), data=dxHope)
anova(m0,m1)

hopeChange = plot_model(m1,"pred") + 
  ylab("Frequency") +
  ggtitle("Hope")

dxDespair = data.frame(
  freq = c(res$beforeDespair,res$duringDespair,res$afterDespair),
  phase = rep(c("before","during","after"),length.out = nrow(res)*3),
  country = rep(res$country,each=3),
  COP = rep(res$COP,each=3)
)
dxDespair$phase = factor(dxDespair$phase,levels = c("before","during","after"), ordered = T)
m0D = lmer(freq ~ 1 + (1|country) + (1|COP), data=dxDespair)
m1D = lmer(freq ~ phase + (1|country) + (1|COP), data=dxDespair)
anova(m0D,m1D)

despairChange = plot_model(m1D,"pred") + 
  ylab("Frequency")+
  ggtitle("Despair")

library(ggpubr)
ggarrange(hopeChange,despairChange)
