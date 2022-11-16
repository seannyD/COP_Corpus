try(setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/processing/"))

library(quanteda)
library(stringr)

kw = read.csv("../data/LEXIS/TrilemmaKeywords.csv",stringsAsFactors = F)

getKeywords= function(sub){
  kx = unique(unlist(strsplit(kw[kw$Subject==sub,]$concepts,";")))
  names(kx) = kx
  return(kx)
}
accessibilityKeywords = getKeywords("Accessibility")
securityKeywords = getKeywords("Security")
sustainabilityKeywords = getKeywords("Sustainability")


length(sustainabilityKeywords)
length(securityKeywords)
length(accessibilityKeywords)

processFile = function(d,country){
  
  # d$text = gsub("\\\\tab","",d$text)
  # d$text = gsub("\\*","",d$text)
  # d$text = gsub("\\\\u[0-9]+","",d$text)
  # d$text = gsub("^\\s+","",d$text)
  # 
  # # Remove duplicate articles
  # cap = substr(d$text,0,100)
  # #d = d[!duplicated(cap),]
  # dists = adist(cap,ignore.case = T)
  # dists[upper.tri(dists)] = 100
  # diag(dists) = 100
  # dups = which(dists<25,arr.ind = T)
  # d = d[-unique(dups[,1]),]
  
  # create ID
  #d$ID = paste0(country,1:nrow(d))
  
  # Lower case
  d$text = tolower(d$text)
  
  # Create corpus, tokens, freq matrix
  corp = corpus(d, docid_field = "ID",text_field = "text")
  tok = tokens(corp, remove_punct = TRUE)
  corpDFM = dfm(tok)
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
  
  # Get score (frequency per 1000 words)
  getScore = function(keywords){
    freq = sapply(keywords,getFrequency)
    prop = 1000000 * (freq/totalWords)
    return(prop)
  }
  
  AccFreqs = getScore(accessibilityKeywords)
  SecFreqs = getScore(securityKeywords)
  SusFreqs = getScore(sustainabilityKeywords)
  
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
  
  return(data.frame(
    "COP" = d$COP[1],
    "country" = country,
    "accessibility" = sum(AccFreqs),
    "security" = sum(SecFreqs),
    "sustainability" = sum(SusFreqs),
    "docs" = nrow(d),
    "totalWords" = totalWords
  ))
  
}

#uk = processFile("../data/LEXIS/COP26_UK.csv")
#usa = processFile("../data/LEXIS/COP26_USA.csv")
#ireland = processFile("../data/LEXIS/COP26_IRELAND.csv")
#rbind(uk,usa,ireland)

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
  # Process data
  newData= processFile(d,country)
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
