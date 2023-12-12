# Note: After the initial processing, a new error appeared with 
#  Quanteda's dfm function, which seems to depend on Rstudio version
# It's patched below, but the results are based on the first implementation
try(setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/processing/"))

library(quanteda)
library(stringr)
library(lubridate)

kw = read.csv("../data/LEXIS/TrilemmaKeywords.csv",stringsAsFactors = F)

getKeywords= function(sub){
  kx = unique(unlist(strsplit(kw[kw$Subject==sub,]$concepts,";")))
  names(kx) = kx
  return(kx)
}
accessibilityKeywords = getKeywords("Accessibility")
securityKeywords = getKeywords("Security")
sustainabilityKeywords = getKeywords("Sustainability")

N.AccKeyword = length(accessibilityKeywords)
N.SecKeyword = length(securityKeywords)
N.SusKeyword = length(sustainabilityKeywords)

length(sustainabilityKeywords)
length(securityKeywords)
length(accessibilityKeywords)

processFile = function(d,country,confStart=NA,confEnd=NA){
  # Lower case
  d$text = tolower(d$text)
  
  myDFM = function(tok){
    #allWords = unique(unlist(tok))
    uniqueToks = unique(unlist(tok))
    frq = t(sapply(tok,function(tk){
      tx = table(tk)[uniqueToks]
      tx[is.na(tx)] = 0
      return(tx)
    }))
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
        return(sum(corpDFM[,keyword]))
      }
      return(0)
    }
    
    # Get score (frequency per 1000000 words)
    getScore = function(keywords){
      freq = sapply(keywords,getFrequency)
      prop = 1000000 * (freq/totalWords)
      return(prop)
    }
    
    AccFreqs = getScore(accessibilityKeywords)
    SecFreqs = getScore(securityKeywords)
    SusFreqs = getScore(sustainabilityKeywords)
    return(list(AccFreqs=AccFreqs,SecFreqs=SecFreqs,
                SusFreqs=SusFreqs,totalWords=totalWords))
  }
  
  # Measure full corpus
  fullScores = corp2Scores(d)
  AccFreqs = fullScores$AccFreqs
  SecFreqs = fullScores$SecFreqs
  SusFreqs = fullScores$SusFreqs

  freqData = data.frame(
    COP = d$COP[1],
    country = d$country[1],
    aspect = c(rep("Accessibility",length(AccFreqs)),
               rep("Security",length(SecFreqs)),
               rep("Sustainability",length(SusFreqs))),
    keyword = c(accessibilityKeywords,securityKeywords,sustainabilityKeywords),
    freq = c(AccFreqs, SecFreqs, SusFreqs)
  )
  write.csv(freqData, 
            file=paste0("../results/LexisFrequencies/",
                   d$COP[1],"_",
                   gsub(" ","-",country),
                   ".csv"))
  
  # Measure before/during/after
  beforeScores = list(AccFreqs=0,SecFreqs=0,SusFreqs=0)
  duringScores = list(AccFreqs=0,SecFreqs=0,SusFreqs=0)
  afterScores = list(AccFreqs=0,SecFreqs=0,SusFreqs=0)
  
  if(!is.na(confStart)){
    aDates = lubridate::parse_date_time(d$date, c('mdY','mdYHM',"bdYAHMpU"))
    d$phase = cut(aDates,
                  c(parse_date_time("01/01/1900","dmy"),
                    confStart,confEnd,
                    parse_date_time("01/01/2100","dmy")), labels = c("Before","During","After"))
    
    beforeScores = corp2Scores(d[!is.na(d$phase) & d$phase=="Before",])
    duringScores = corp2Scores(d[!is.na(d$phase) & d$phase=="During",])
    afterScores = corp2Scores(d[!is.na(d$phase) & d$phase=="After",])
  }  
  
  ret = data.frame(
    "COP" = d$COP[1],
    "country" = country,
    "accessibility" = sum(AccFreqs),
    "security" = sum(SecFreqs),
    "sustainability" = sum(SusFreqs),
    "beforeAcc" = sum(beforeScores[[1]]),
    "duringAcc" = sum(duringScores[[1]]),
    "afterAcc" = sum(afterScores[[1]]),
    "beforeSec" = sum(beforeScores[[2]]),
    "duringSec" = sum(duringScores[[2]]),
    "afterSec" = sum(afterScores[[2]]),
    "beforeSus" = sum(beforeScores[[3]]),
    "duringSus" = sum(duringScores[[3]]),
    "afterSus" = sum(afterScores[[3]]),
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

write.csv(res,"../data/LEXIS/TrilemmaScores_byCountryAndYear.csv",row.names = F)

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

write.csv(overallData, "../data/LEXIS/TrilemmaScores_byCountry.csv")

sum(overallData$totalWords)
sum(overallData$docs)
