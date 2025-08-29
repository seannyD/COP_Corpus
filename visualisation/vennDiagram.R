#devtools::install_github("gaospecial/ggVennDiagram")
library("ggVennDiagram")
library(ggvenn)

try(setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/visualisation/"))

kw = read.csv("../data/LEXIS/TrilemmaKeywords.csv",stringsAsFactors = F)

getKeywords= function(sub){
  kx = unique(unlist(strsplit(kw[kw$Subject==sub,]$concepts,";")))
  names(kx) = kx
  return(kx)
}
accessibilityKeywords = tolower(getKeywords("Accessibility"))
securityKeywords = tolower(getKeywords("Security"))
sustainabilityKeywords = tolower(getKeywords("Sustainability"))

files = list.files("../results/LexisFrequencies/byDocument/","*.csv")
d = data.frame()
for(file in files){
  dx = read.csv(paste0("../results/LexisFrequencies/byDocument/",file),
             stringsAsFactors = F,
             check.names = F)
  freqs = sapply(list(ACC = accessibilityKeywords,
                      SEC = securityKeywords,
                      SUS = sustainabilityKeywords), function(kws){
                        rowSums(dx[,tolower(kws)])
                      })
  freqs = cbind(freqs, dx$COP,dx$country)
  d = rbind(d,freqs)
}

docsWithAcc = which(d$ACC>0,arr.ind = T)
docsWithSec = which(d$SEC>0,arr.ind = T)
docsWithSus = which(d$SUS>0,arr.ind = T)

ggVennDiagram(list(Accessibility = docsWithAcc,
                   Security = docsWithSec,
                   Sustainability = docsWithSus), 
              set_color = c("#619cffff","#f8766dff","#0a9f37ff"),
              label_alpha = 0,
              label_color = "white")

ggvenn(as.data.frame(d>0),c("ACC","SEC","SUS"),
       show_outside = "always",
       fill_color = c("#619cffff","#f8766dff","#0a9f37ff"))

library(rgl)
plot3d(x=d$ACC, y=d$SEC, z=d$SUS)

plot(log10(d$ACC), log10(d$SEC))

ggplot(d, aes(x=(ACC+1), y=(SEC+1)) ) +
  geom_bin2d(bins = 8) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Frequency of Accessibility Keyterms") +
  ylab("Frequency of Security Keyterms")

ggplot(d, aes(x=(ACC+1), y=(SUS+1)) ) +
  geom_bin2d(bins = 8) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Frequency of Accessibility Keyterms") +
  ylab("Frequency of Sustainability Keyterms")

ggplot(d, aes(x=(SEC+1), y=(SUS+1)) ) +
  geom_bin2d(bins = 8) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Frequency of Security Keyterms") +
  ylab("Frequency of Sustainability Keyterms")


cor.test(d$ACC,d$SEC)
cor.test(d$ACC,d$SUS)
cor.test(d$SEC,d$SUS)
