# Read RFT files from Lexis Nexis, 
# extract title, date, and text,
# remove duplicate articles,
# write to textsDiachronicClean

setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/processing/")

isText = function(line){
  startCues = c("^\\{\\\\rtlch","^\\\\ltrch","^\\\\loch");
  return(any(sapply(startCues,function(X){
    grepl(X,line) & grepl("}$",line)
  })))
}
  
parseText = function(fileName){
  
  loadedText = paste(readLines(fileName),collapse="\n")
  
  indexOf = function(txt,seek){
    unlist(gregexpr(seek, txt))[1]
  }
  
  findFromTo = function(txt,from,to){
    fromPos = unlist(gregexpr(from, txt))[1]
    txt = substr(txt,fromPos,nchar(txt))
    txt = sub(from,"",txt)
    toPos = unlist(gregexpr(to, txt))[1]
    txt = substr(txt,0,toPos-nchar(to))
    return(txt)
  }
    
    #function getText(doc){
  getText = function(doc){
      doc = gsub("\\{","\n{",doc)
      lines = strsplit(doc,"\n")[[1]]
      
      lines = lines[sapply(lines,isText)]
      lines = sapply(lines,findFromTo, from=" ",to="}")
      lines = lines[nchar(lines)>0]
      lineText = paste(lines, collapse ="")
      lineText = substr(lineText,0,unlist(gregexpr("\\line", lineText))[1])
      
      lineText = cleanText(lineText)
      
      return(lineText)
    }
    
    cleanText = function(txt){
      txt = gsub('\u8220','"',txt)
      txt = gsub('\u8221','"',txt)
      txt = gsub('\\u8217',"'",txt)
      txt = gsub('\\u8216',"'",txt)
      txt = gsub('\\u8364',"€",txt)
      txt = gsub('\\u8209','-',txt)
      txt = gsub('\\u8211','-',txt)
      txt = gsub("\\'a","£",txt)
      txt = gsub("\n"," ",txt)
      txt = gsub("\\\\XE2s"," ",txt)
      txt = gsub("\\{\\\\rtlch\\\\afs24\\\\ltrch\\\\fs24\\\\par"," ",txt)
      txt = gsub(" +"," ",txt)
      return(txt)
    }
    
    docs = strsplit(loadedText, "End of Document}")[[1]]
    docs = docs[1:(length(docs)-1)]
    docs[1] = substr(docs[1],indexOf(docs[1]," PAGE"),nchar(docs[1]))
    
    d = data.frame()
    
    for(doc in docs){
      
      #{\fldrslt{\rtlch\af2\alang1025\afs24\ltrch\i\fs20\lang1033\langnp1033\langfe1033\langfenp1033\loch\af2\dbch\af2\hich\f2\ul\cf2
      #{\rtlch\af2\alang1025\afs24\ltrch\fs18\lang1033\langnp1033\langfe1033\langfenp1033\loch\af2\dbch\af2\hich\f2\ulnone\cf1  | }{\field{\*\fldinst{\rtlch\af2\alang1025\afs24\ltrch\fs18\lang1033\langnp1033\langfe1033\langfenp1033\loch\af2\dbch\af2\hich\f2\ul\cf2
      
      title = findFromTo(doc,"\n\\\\dbch\\\\af2\\\\hich\\\\f2 ","}")
      if(!grepl("\n\\\\dbch\\\\af2\\\\hich\\\\f2 ",doc)){
        title = findFromTo(doc,"\\\\loch\\\\af2\\\\dbch\\\\af2\\\\hich\\\\f2\\\\ul\\\\cf2","}")
      }
      
      title = gsub("\\\\kerning32 ","",title)
      
      #metaSplitter = "\\\\loch\\\\af2\\\\dbch\\\\af2\\\\hich\\\\f2\\\\cf1";
      metaSplitter = "\\\\hich\\\\f2\\\\cf1 ";
      docEndCue = "\\\\hich\\\\f2\\\\cf1 Body}"
      
      if(!grepl(metaSplitter,doc)){
        metaSplitter = "\\\\loch\\\\af2\\\\dbch\\\\af2\\\\hich\\\\f2\\\\cf2"
        docEndCue = "\\\\hich\\\\f2\\\\cf2 Body}"
      }
      if(!grepl(metaSplitter,doc)){
        metaSplitter = "\\\\hich\\\\f2\\\\ul\\\\cf2\\\\kerning32"
        docEndCue = "\\\\hich\\\\f2\\\\cf1 Body}"
      }
      if(!grepl(metaSplitter,doc)){
        metaSplitter = "\\\\hich\\\\f2\\\\ul\\\\cf2"
        docEndCue = "\\\\hich\\\\f2\\\\cf1 Body}"
      }
      metaParts = strsplit(doc, metaSplitter)[[1]]
      
      metaParts = sapply(metaParts, function(part){
        substr(part,0, indexOf(part,"}")-1)
      })
      metaParts = metaParts[nchar(metaParts)>0]
      source = metaParts[2]
      date = metaParts[3]
      docBody = substr(doc, indexOf(doc,docEndCue)+17,nchar(doc))
      
      txt = getText(docBody);
      d = rbind(d, 
                  data.frame("title" = title, 
                             "date"=date, 
                             "source"=source, 
                             "text"=txt))
    }
    
    d$text = gsub("\\\\tab","",d$text)
    d$text = gsub("\\*","",d$text)
    d$text = gsub("\\\\u[0-9]+","",d$text)
    d$text = gsub("^\\s+","",d$text)
    
    # Remove duplicate articles
    cap = substr(d$text,0,100)
    #d = d[!duplicated(cap),]
    dists = adist(cap,ignore.case = T)
    dists[upper.tri(dists)] = 100
    diag(dists) = 100
    dups = which(dists<25,arr.ind = T)
    uDups = unique(dups[,1])
    if(length(uDups)>0){
      d = d[-unique(dups[,1]),]
    }
    
    fn = tail(strsplit(fileName,"/")[[1]],1)
    fn = gsub("\\.rtf","", fn,ignore.case = T)
    fnBits = strsplit(fn,"_")[[1]]
    d$COP = fnBits[1]
    d$country = fnBits[2]
    d$ID = paste0(fn,1:nrow(d))
    d = d[,c("COP","country","ID","title","date","source","text")]
      
    fnOut = paste0("../data/LEXIS/textsDiachronicClean/",fn,".csv")
    write.csv(d, fnOut, row.names = F)
}


files = list.files("../data/LEXIS/textsDiachronic/")
files = files[grepl("\\.rtf",files,ignore.case = T)]

for(file in files){
  print(file)
  parseText(paste0("../data/LEXIS/textsDiachronic/",file))
}
