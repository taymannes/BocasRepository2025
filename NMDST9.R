library(vegan)
library(dplyr)
library(ggplot2)
#pairwise adonis
library(devtools)
library(pairwiseAdonis) #for pairwise tests of permanova
library(plyr)

library(RColorBrewer)

dataT9=read.csv("T9.csv")
NMDST9matrix=as.matrix(dataT9[,-c(1,2)])

NMDST9=metaMDS(NMDST9matrix, k=3, trymax=100)

varsT9=envfit(NMDST9, dataT9[,-c(1,2)], permu=999)
scores(varsT9, "vectors")

head(varsT9)
stressplot(NMDST9)

NMDST9$stress

varsT9<-envfit(NMDST9, dataT9[,-c(1,2)], permu=999)

plot.colT9=c("plum2","violetred1","mediumorchid1","salmon","gold","palegreen","turquoise","mediumseagreen","slategray1","deepskyblue1","lightslateblue", "dimgray")

levels(as.factor(dataT9$Site))  

png("T9EllipsesNMDS.png", width = 3000, height = 3000, res = 300)

T9EllipsesNMDS<-ordiplot(NMDST9,type="n",xlim=c(-1,1), ylim=c(-0.75,1)) #plot space
with(dataT9,points(NMDST9, "sites", pch=20,cex=1, col="lightgray", bg=plot.colT9[Site]))
ordiellipse(NMDST9, groups=dataT9$Site, draw="polygon", col=plot.colT9, alpha=150, kind="se", conf=0.95)
plot(varsT9, p.max=0.05, cex=0.5, lwd=1,col='black')
par(xpd=TRUE)
legend(x=0.75,y=1, bty='n', cex = 0.8, xpd=NA, legend = c("Almirante","Caracol", "Caracol Deep", "Cayo Roldan","Coral Cay", "Hospital Point", "Pastores", "Pollito","Popa","Punta Juan","Salt Creek","STRI"), title = "Site", fill=plot.colT9, col=plot.colT9)



# Close the PNG device
dev.off()

