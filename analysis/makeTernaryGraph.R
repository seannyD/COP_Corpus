try(setwd("~/OneDrive - Cardiff University/Research/Cardiff/ClimageChangeAndLanguage/project/analysis/"))

library(ggtern)
library(ggrepel)
library(ggpubr)
library(openxlsx)
library(lme4)

d = read.csv("../data/LEXIS/TrilemmaScores_byCountry.csv",stringsAsFactors = F)

d$country[d$country=="South.Africa"] = "SA"
d$country[d$country=="Hong.Kong"] = "HK"
#d = d[d$country!="HK",]

d$tot = d$accessibility + d$security + d$sustainability

d$Accessibility = d$accessibility / d$tot
d$Security = d$security / d$tot
d$Sustainability = d$sustainability / d$tot

colMeans(d[,c("Sustainability","Security","Accessibility")])
apply(d[,c("Sustainability","Security","Accessibility")],2,sd)

sum(d$totalWords)

# Load trilemma index from Sparjc et al.
ti = read.csv("../data/Sprajc_EnergyTrilemmaIndex.csv",stringsAsFactors = F)

d$trilemmaIndex.sustainability = ti[match(d$country,ti$Country),]$sustainability
d$trilemmaIndex.security = ti[match(d$country,ti$Country),]$security
d$trilemmaIndex.accessibility = ti[match(d$country,ti$Country),]$equity

plot(d$trilemmaIndex.sustainability, d$Sustainability)
plot(d$trilemmaIndex.security, d$Security)
plot(d$trilemmaIndex.accessibility, d$Accessibility)

# Load trilemma index from Heffron
hi = read.csv("../data/Heffron_2018_Metric.csv",stringsAsFactors = F)
d[,c("Hindex.acc","Hindex.sec","Hindex.sus")] = 
  hi[match(d$country,hi$country),c("accessibility","security","sustainability")]

plot(d$Accessibility,d$Hindex.acc)
plot(d$Security,d$Hindex.sec)
plot(d$Sustainability,d$Hindex.sus)

# Overall trilemma
pdf(file="../results/COP_Overall_EnergyTrilemma.pdf")
ggtern(data=d, 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.4,L=0.4,R=0.85) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_text(aes(label=country,colour=country),show.legend = FALSE,
            position = position_nudge_tern(y=0.01),
            alpha=1) +
  geom_point(aes(colour=country),alpha=0.5)
dev.off()


boxplot(d$Accessibility,d$Security, d$Sustainability,
        names = c("Accessibility","Security","Sustainability"),
        ylim=c(0,1))


boxplotData = data.frame(
  Score = c(d$Accessibility,d$Security,d$Sustainability)*100,
  Type = rep(c("Acc.","Sec.","Sus."),each=nrow(d))
)
pdf(file="../results/COP_Overall_EnergyTrilemma_Boxplot.pdf",
    width=1.7,height=3)
ggplot(boxplotData,aes(y=Score,fill=Type,x=Type,color=Type))+
  geom_boxplot() +
  theme(axis.title.x = element_blank(), legend.position = "none") +
  ylim(c(0,100)) + 
  scale_fill_manual(values=c("#619cffff","#f8766dff","#0a9f37ff")) +
  scale_color_manual(values=c("#3b61a1","#a34e48","#05591f"))
dev.off()

# ggtern(data=d, 
#        aes(x=Accessibility,y=Security, z=Sustainability)) + 
#   tern_limit(T=0.35,L=0.35,R=1) + 
#   theme_rgbw() +
#   theme_showarrows() +
#   labs(x="",xarrow="Accessibility",
#        y="",yarrow="Security",
#        z="",zarrow="Sustainability")+
#   geom_text(aes(label=country),show.legend = FALSE,
#              position = position_nudge_tern(z=-0.06,x=-0.01),
#             alpha=1) +
#   geom_point()
# 

####################
# Results are the same if we control for num keywords
# (security is a little higher)

kw = read.csv("../data/LEXIS/TrilemmaKeywords.csv",stringsAsFactors = F)

getKeywords= function(sub){
  kx = unique(unlist(strsplit(kw[kw$Subject==sub,]$concepts,";")))
  names(kx) = kx
  return(kx)
}
accessibilityKeywords = getKeywords("Accessibility")
securityKeywords = getKeywords("Security")
sustainabilityKeywords = getKeywords("Sustainability")

d$sustainability2 = d$sustainability / length(sustainabilityKeywords)
d$security2 = d$security / length(securityKeywords)
d$accessibility2 = d$accessibility / length(accessibilityKeywords)

d$tot2 = d$sustainability2 + d$security2 + d$accessibility2

d$Accessibility2 = d$accessibility2 / d$tot2
d$Security2 = d$security2 / d$tot2
d$Sustainability2 = d$sustainability2 / d$tot2

boxplot(d$Accessibility2,d$Security2, d$Sustainability2,
        names = c("Accessibility","Security","Sustainability"))

boxplotData2 = data.frame(
  Score = c(d$Accessibility2,d$Security2,d$Sustainability2)*100,
  Type = rep(c("Acc.","Sec.","Sus."),each=nrow(d))
)
ggplot(boxplotData2,aes(y=Score,fill=Type,x=Type,color=Type))+
  geom_boxplot() +
  theme(axis.title.x = element_blank(), legend.position = "none") +
  ylim(c(0,100)) + 
  scale_fill_manual(values=c("#619cffff","#f8766dff","#0a9f37ff")) +
  scale_color_manual(values=c("#3b61a1","#a34e48","#05591f"))

ggtern(data=d, 
       aes(x=Accessibility2,y=Security2, z=Sustainability2)) + 
  tern_limit(T=0.6,L=0.6,R=0.85) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_text(aes(label=country,colour=country),show.legend = FALSE,
            position = position_nudge_tern(y=0.01),
            alpha=1) +
  geom_point(aes(colour=country),alpha=0.5)

########################
# CHANGE OVER TIME

dy = read.csv("../data/LEXIS/TrilemmaScores_byCountryAndYear.csv",stringsAsFactors = F)

dy$tot = d$accessibility + dy$security + dy$sustainability

dy$Accessibility = dy$accessibility / dy$tot
dy$Security = dy$security / dy$tot
dy$Sustainability = dy$sustainability / dy$tot

dy$COP = gsub("COP","",dy$COP)

# Load WEC Energy Trilemma Index
wec = read.xlsx("../data/Trielemma_scores.xlsx",sheet = 1,startRow = 3)
names(wec)[1:2] = c("Conference","Year")
names(wec)[3:ncol(wec)] = paste0(rep(names(wec)[3:14]), rep(c(".SEC",".SUS",".ACC"),each=12))

dy[,c("WEC.SEC","WEC.SUS","WEC.ACC")] = NA
for(i in 1:nrow(dy)){
  country= dy[i,]$country
  conf = dy[i,]$COP
  dy[i,c("WEC.SEC","WEC.SUS","WEC.ACC")] = 
      wec[wec$Conference==paste0("COP",conf),paste0(country,".",c("SEC","SUS","ACC"))]
}

cor.test(dy$Sustainability,dy$WEC.SUS)
cor.test(dy$Security,dy$WEC.SEC)
cor.test(dy$Accessibility,dy$WEC.ACC)

xsus = dy[,c("COP","country","Sustainability","WEC.SUS")]
xsec = dy[,c("COP","country","Security","WEC.SEC")]
xacc = dy[,c("COP","country","Accessibility","WEC.ACC")]
names(xsus) = c("COP","country","D","WEC")
names(xsec) = c("COP","country","D","WEC")
names(xacc) = c("COP","country","D","WEC")
xsus$Comp = "Sustainability"
xsec$Comp = "Security"
xacc$Comp = "Accessibility"
dyLong = rbind(xsus,xsec,xacc)

m0 = lmer(WEC ~ D + (1|country) + (1 +D |Comp),
  data=dyLong)

# Graph of change over time

g1 = ggplot(dy,aes(x=COP,y=Accessibility,group=country,color=country)) +
  geom_point() +
  geom_line() +
  ggtitle("Accessibility")+
  ylab("Score")+
  theme(legend.position = "none")

g2 = ggplot(dy,aes(x=COP,y=Security,group=country,color=country)) +
  geom_point() +
  geom_line() +
  ggtitle("Security") +
  theme(legend.position = "none", axis.title.y = element_blank())

g3 = ggplot(dy,aes(x=COP,y=Sustainability,group=country,color=country)) +
  geom_point() +
  geom_line() +
  ggtitle("Sustainability") +
  theme(axis.title.y = element_blank())

pdf("../results/COP_Trilemma_ChangeOverTime.pdf",height=4,width=12)
ggarrange(g1,g2,g3,nrow = 1,widths = c(1.15,1,1.45))
dev.off()


fillColours = rev(hcl.colors(length(unique(dy$COP))))

pdf("../results/COP_Trilemma_byCountry.pdf",width=5,height=5)
ggtern(data=dy[dy$country=="UK",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.25,L=0.15,R=0.85) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("UK")


ggtern(data=dy[dy$country=="USA",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.25,L=0.15,R=0.85) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("USA")


ggtern(data=dy[dy$country=="Bangladesh",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.25,L=0.15,R=0.85) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("Bangladesh")

ggtern(data=dy[dy$country=="Ireland",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.4,L=0.4,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("Ireland")

ggtern(data=dy[dy$country=="Australia",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.3,L=0.3,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("Australia")

ggtern(data=dy[dy$country=="Nigeria",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.5,L=0.5,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("Nigeria")

ggtern(data=dy[dy$country=="China",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.3,L=0.3,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("China")

ggtern(data=dy[dy$country=="South.Africa",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.5,L=0.5,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("South Africa")

ggtern(data=dy[dy$country=="UAE",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.3,L=0.3,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("UAE")

ggtern(data=dy[dy$country=="Canada",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.3,L=0.3,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("Canada")

ggtern(data=dy[dy$country=="India",], 
       aes(x=Accessibility,y=Security, z=Sustainability)) + 
  tern_limit(T=0.3,L=0.3,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") ),color="gray",alpha=1) +
  #geom_text(aes(label=COP,colour=COP),show.legend = FALSE,
  #          position = position_nudge_tern(y=0.01),
  #          alpha=1) +
  scale_color_manual(values = fillColours)+
  geom_point(aes(colour=COP),alpha=0.5) +
  ggtitle("India")


dev.off()  


######
# Change over time


vec = NULL
vecCenter = NULL
for(country in unique(dy$country)){
  dx = dy[dy$country==country,]
  meanA = mean(dx$Accessibility)
  meanSec = mean(dx$Security)
  meanSus = mean(dx$Sustainability)
  
  dA = sum(diff(dx$Accessibility))
  dSec = sum(diff(dx$Security))
  dSus = sum(diff(dx$Sustainability))
  
  vec = rbind(vec,data.frame(
    country = country,
    Accessibility = c(meanA,meanA+dA),
    Security = c(meanSec,meanSec+dSec),
    Sustainability = c(meanSus,meanSus+dSus)
  ))
  centerPoint = 1/3
  vecCenter = rbind(vecCenter,data.frame(
    country = country,
    Accessibility = c(centerPoint,centerPoint+dA*4),
    Security = c(centerPoint,centerPoint+dSec*4),
    Sustainability = c(centerPoint,centerPoint+dSus*4)
  ))
}

pdf("../results/COP_Overall_AvChangeOverTime.pdf",width=6,height=6)
ggtern(data=vec, 
       aes(x=Accessibility,y=Security, z=Sustainability,colour=country)) + 
  tern_limit(T=0.35,L=0.35,R=1) + 
  theme_showarrows() +
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") )) +
  ggtitle("Average change over time")
dev.off()

pdf("../results/COP_Overall_RelChangeOverTime.pdf",width=6,height=6)
ggtern(data=vecCenter, 
       aes(x=Accessibility,y=Security, z=Sustainability,colour=country)) + 
  tern_limit(T=1,L=1,R=1) + 
  theme_showarrows() +
  theme_nolabels()+
  theme_noticks()+
  theme(legend.position = "none")+
  labs(x="",xarrow="Accessibility",
       y="",yarrow="Security",
       z="",zarrow="Sustainability")+
  geom_text(aes(label=country,colour=country),show.legend = FALSE,
            alpha=1) +
  geom_path(arrow=arrow(type = "closed",length =unit(0.1, "inches") )) +
  ggtitle("Average change over time")
dev.off()
