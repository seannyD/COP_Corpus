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

processFile = function(d,country,confStart=NA,confEnd=NA,writeSummaries = TRUE){
  # Lower case
  d$text = tolower(d$text)
  
  # myDFM = function(tok){
  #   #allWords = unique(unlist(tok))
  #   uniqueToks = unique(unlist(tok))
  #   frq = t(sapply(tok,function(tk){
  #     tx = table(tk)[uniqueToks]
  #     tx[is.na(tx)] = 0
  #     names(tx) = uniqueToks
  #     return(tx)
  #   }))
  # }

  corp2Scores = function(d){
    # Create corpus, tokens, freq matrix
    d$text = tolower(d$text)
    corp = corpus(d, docid_field = "ID",text_field = "text")
    tok = tokens(corp, remove_punct = TRUE)
    #corpDFM = dfm(tok)
    #corpDFM = myDFM(tok)
    totalWords = sum(sapply(tok,length))
    
    # # Get frequency for one keyword
    # getFrequency = function(keyword){
    #   keyword = tolower(keyword)
    #   if(grepl(" ",keyword)){
    #     # Multi-word expression
    #     return(length(unlist(str_extract_all(d$text, keyword))))
    #   }
    #   if(keyword %in% colnames(corpDFM)){
    #     return(sum(corpDFM[,keyword]))
    #   }
    #   return(0)
    # }
    # 
    # # Get score (frequency per 1000000 words)
    # getScore = function(keywords){
    #   freq = sapply(keywords,getFrequency)
    #   prop = 1000000 * (freq/totalWords)
    #   return(prop)
    # }
    
    #AccFreqs = getScore(accessibilityKeywords)
    #SecFreqs = getScore(securityKeywords)
    #SusFreqs = getScore(sustainabilityKeywords)
    
    bydoc = NA
    # Work out frequency for each document, 
    #  so we can see the overlap between topics
    allkw = tolower(c(accessibilityKeywords,securityKeywords,sustainabilityKeywords))
    names(allkw) = tolower(allkw)
    bydoc = sapply(allkw, function(keyword){
      if(grepl(" ",keyword)){
        # Multi-word expression
        mtch = str_extract_all(d$text, keyword)
        return(sapply(mtch,length))
      }
      return(sapply(tok,function(X){sum(X==keyword)}))
    })
    bydoc = cbind(bydoc,NUMWORDS = sapply(tok,length))
  
    
    AccFreqs = 1000000*(colSums(bydoc[,tolower(accessibilityKeywords)])/totalWords)
    SecFreqs = 1000000*(colSums(bydoc[,tolower(securityKeywords)])/totalWords)
    SusFreqs = 1000000*(colSums(bydoc[,tolower(sustainabilityKeywords)])/totalWords)
    
    return(list(AccFreqs=AccFreqs,SecFreqs=SecFreqs,
                SusFreqs=SusFreqs,totalWords=totalWords,
                bydoc=bydoc))
  }
  
  # Measure full corpus
  fullScores = corp2Scores(d)
  AccFreqs = fullScores$AccFreqs
  SecFreqs = fullScores$SecFreqs
  SusFreqs = fullScores$SusFreqs
  bydoc = fullScores$bydoc
  bydoc = cbind(d[,c("COP","country","ID","title","date","source")],bydoc)
  
  # Write per-document stats
  if(writeSummaries){
    write.csv(bydoc,
      file = paste0("../results/LexisFrequencies/byDocument/",
                   country, "_", d$COP[1], "_byDocument.csv"))
  }

  freqData = data.frame(
    COP = d$COP[1],
    country = d$country[1],
    aspect = c(rep("Accessibility",length(AccFreqs)),
               rep("Security",length(SecFreqs)),
               rep("Sustainability",length(SusFreqs))),
    keyword = c(accessibilityKeywords,securityKeywords,sustainabilityKeywords),
    freq = c(AccFreqs, SecFreqs, SusFreqs)
  )
  if(writeSummaries){
    write.csv(freqData, 
              file=paste0("../results/LexisFrequencies/",
                     d$COP[1],"_",
                     gsub(" ","-",country),
                     ".csv"))
  }
  
  # Measure before/during/after
  phaseFreqs = matrix(0,nrow=3,ncol=3)
  rownames(phaseFreqs) = c("ACC","SEC","SUS")
  colnames(phaseFreqs) = c("Before","During","After")
  if(!is.na(confStart)){
    aDates = lubridate::parse_date_time(d$date, c('mdY','mdYHM',"bdYAHMpU"))
    phase = cut(aDates,
                  c(parse_date_time("01/01/1900","dmy"),
                    confStart,confEnd,
                    parse_date_time("01/01/2100","dmy")), labels = c("Before","During","After"))
    
    phaseFreqs = 
      sapply(c("Before","During","After"),function(phx){
      sapply(list(ACC = accessibilityKeywords,
                  SEC = securityKeywords,
                  SUS = sustainabilityKeywords), function(kws){
        sum(bydoc[!is.na(phase) & phase==phx,tolower(kws)])
      })
    })
  
  }  
  
  ret = data.frame(
    "COP" = d$COP[1],
    "country" = country,
    "accessibility" = sum(AccFreqs),
    "security" = sum(SecFreqs),
    "sustainability" = sum(SusFreqs),
    "beforeAcc" = phaseFreqs["ACC","Before"],
    "duringAcc" = phaseFreqs["ACC","During"],
    "afterAcc" = phaseFreqs["ACC","After"],
    "beforeSec" = phaseFreqs["SEC","Before"],
    "duringSec" = phaseFreqs["SEC","During"],
    "afterSec" = phaseFreqs["SEC","After"],
    "beforeSus" = phaseFreqs["SUS","Before"],
    "duringSus" = phaseFreqs["SUS","During"],
    "afterSus" = phaseFreqs["SUS","After"],
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
  overallCountryData = processFile(dCountry,country,writeSummaries = FALSE)
  overallCountryData$COP = "ALL"
  overallData = rbind(overallData, overallCountryData)
}

write.csv(overallData, "../data/LEXIS/TrilemmaScores_byCountry.csv")

sum(overallData$totalWords)
sum(overallData$docs)
