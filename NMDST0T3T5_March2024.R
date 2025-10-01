library(vegan)
library(dplyr)
library(ggplot2)
#pairwise adonis
library(devtools)
library(pairwiseAdonis) #for pairwise tests of permanova
library(plyr)

library(RColorBrewer) ##Install and load this library for better color palettes



# load raw data file that has all site data transposed (taxa as columns pics as rows)
# and c

#T0
##I added Site back to T0 to figure out why labels aren't showing up in your plot
dataT0=read.csv("T0.csv")

##Now, convert to matrix, but don't include the site column in the matrix (column 1)
NMDST0matrix=as.matrix(dataT0[,-1])

NMDST0=metaMDS(NMDST0matrix, k=3, trymax=100)

varsT0=envfit(NMDST0, dataT0[,-1], permu=999)
scores(varsT0, "vectors")

head(varsT0)
stressplot(NMDST0)

#You want stress to be less than 0.2
NMDST0$stress
### Extract functional groups that have significant contribution to community
#extract significant variable responses for plotting vectors
vars<-envfit(NMDST0, dataT0[,-1], permu=999)


#plot.col0=c("lightseagreen", "navy", "purple") 
##The above line gives three colors for your plot
## But where you have more than 3 sites, we'll want a color palette that matches the number of sites
## You can use a colorbrewer palette (package called Rcolorbrewer)

##The below command shows you some color brewer palette options
display.brewer.all()

plot.col=c("dimgray","salmon","gold","plum2","palegreen","turquoise","slategray1","lightslateblue") #8 colors for 8 sites

plot.colT9=c("plum2","violetred1","mediumorchid1","salmon","gold","palegreen","turquoise","mediumseagreen","slategray1","deepskyblue1","lightslateblue", "dimgray")

#Check how many sites and what order they're in
levels(as.factor(dataT0$Site))  

ordiplot(NMDST0,type="n",xlim=c(-1,1.0), ylim=c(-0.75,1)) #plot space
with(dataT0,points(NMDST0, "sites", pch=20,cex=1, col="lightgray", bg=plot.colT9[Site]))
ordiellipse(NMDST0, groups=dataT0$Site, draw="polygon", col=plot.colT9, alpha=150, kind="se", conf=0.95)
plot(vars, p.max=0.05, cex=0.5, lwd=1,col='black')
par(xpd=TRUE)
legend("topright", bty='n', cex = 0.8, xpd=NA, legend = c("Almirante","Caracol","Coral Cay","Pastores","Popa","Punta Juan","Salt Creek","STRI"), title = "Site", fill=plot.colT9, col=plot.colT9)



##Setting this up similar to my own code to make sure it's correct
## This says use dataframe "dataT0" but not the first column- which is Site
permdataT0 <- dataT0[,-1]

#Run the permANOVA
Perm.anovaT0=adonis2(permdataT0~Site, data=dataT0, permutations=1000, method="bray", sqrt.dist = TRUE)
Perm.anovaT0
#Run post-hoc test
posthocT0=pairwise.adonis(dataT0[,2:9], dataT0$Site)
posthocT0



######T3
dataT3=read.csv("T3_nocaracoldeep.csv")

##Now, convert to matrix, but don't include the site column in the matrix (column 1)
NMDST3matrix=as.matrix(dataT3[,-1])

#run NMDS
NMDST3=metaMDS(NMDST3matrix, k=3, trymax=100)

varsT3=envfit(NMDST3, dataT3[,-1], permu=999)
scores(varsT3, "vectors")

head(varsT3)
stressplot(NMDST3)

levels(as.factor(dataT3$Site)) 

#You want stress to be less than 0.2
NMDST3$stress


ordiplot(NMDST3,type="n",xlim=c(-1,1.0), ylim=c(-0.75,1)) #plot space
with(dataT3,points(NMDST3, "sites", pch=20,cex=1, col="lightgray", bg=plot.col[Site]))
ordiellipse(NMDST3, groups=dataT3$Site, draw="polygon", col=plot.col, alpha=150, kind="se", conf=0.95)
plot(varsT3, p.max=0.05, cex=0.5, lwd=1,col='black')
par(xpd=TRUE)
legend("topright", bty='n', cex = 0.8, xpd=NA, legend = c("Almirante","Caracol","Coral Cay","Pastores","Popa","Punta Juan","Salt Creek","STRI"), title = "Site", fill=plot.col, col=plot.col)


##Setting this up similar to my own code to make sure it's correct
## This says use dataframe "dataT0" but not the first column- which is Site
data3 <- dataT3[,-1]

Perm.anovaT3=adonis2(data3~Site, data=dataT3, permutations=1000, method="bray", sqrt.dist = TRUE)
Perm.anovaT3
posthocT3=pairwise.adonis(dataT3[,2:9], dataT3$Site)
posthocT3


#######T5
dataT5=read.csv("T5_nocaracoldeep.csv")        

##Now, convert to matrix, but don't include the site column in the matrix (column 1)
NMDST5matrix=as.matrix(dataT5[,-1])

NMDST5=metaMDS(NMDST5matrix, k=3, trymax=100)

varsT5=envfit(NMDST5, dataT5[,-1], permu=999)
scores(varsT5, "vectors")

head(varsT5)
stressplot(NMDST5)

#You want stress to be less than 0.2
NMDST5$stress


levels(dataT5$Site)  #Check how many sites and what order they're in

ordiplot(NMDST5,type="n",xlim=c(-1,1.0), ylim=c(-0.75,1)) #plot space
with(dataT5,points(NMDST5, "sites", pch=20,cex=1, col="lightgray", bg=plot.col[Site]))
ordiellipse(NMDST5, groups=dataT5$Site, draw="polygon", col=plot.col, alpha=150, kind="se", conf=0.95)
plot(varsT5, p.max=0.05, cex=0.5, lwd=1,col='black')
par(xpd=TRUE)
legend("topright", bty='n', cex = 0.8, xpd=NA, legend = c("Almirante","Caracol","Coral Cay","Pastores","Popa","Punta Juan","Salt Creek","STRI"), title = "Site", fill=plot.col, col=plot.col)


##Setting this up similar to my own code to make sure it's correct
## This says use dataframe "dataT0" but not the first column- which is Site
data5 <- dataT5[,-1]

Perm.anovaT5=adonis2(data5~Site, data=dataT5, permutations=1000, method="bray", sqrt.dist = TRUE)
Perm.anovaT5
posthocT5=pairwise.adonis(dataT5[,2:9], dataT5$Site)
posthocT5