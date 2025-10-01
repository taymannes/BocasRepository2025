#first create a nmds plot to visualize our multivariate compositions.  Then test for differences with permanova
rm(list=ls())

library(dplyr)
library(ggplot2)
library(vegan)
#new library "vegan" for us!

p1<-read.csv("total cover zeros.csv")

#function from package geodev
dimcheckMDS <- function(matrix, distance = "bray", k = 6,  trymax = 20, autotransform = TRUE) {
  if(!is.data.frame(matrix)) {
    matrix <- data.frame(matrix)
  }
  stress <- 0
  
  for (i in 1:k) {
    nmds_i <- metaMDS(matrix, distance = distance, k = i, trymax = trymax, engine = "monoMDS", autotransform = autotransform)
    stress[i] <- nmds_i$stress
  }
  
  plot(seq(1,k,1), stress, main="Stress value in tested dimensions", xlab="Dimension", ylab="Stress",
       ylim=c(0,max(stress)), type="n")
  lines(seq(1,k,1), stress)
  points(seq(1,k,1), stress, pch=21, col="black", bg = "red", cex = 1.2)
  abline(0.2, 0, col="red", lty = 2)
  print(stress)
}
#in vegan, metaMDS that does the NMDS
metaMDS(p1, distance="bray")
#why won't that work???

#extract predictor variables
#preds<-p1[,1:5]
#or
preds<-select(p1, c(rep.id, year, irr, seeds, soil.trt))

#extract the species cover data
#head
#or cover<-p1[,6:132]
cover<-select(p1, Annual.Fimbry:Wild.Onion)

#use metaMDS in package Vegan to run the nmds.  Here, I'm using Binary = TRUE to just use the presence / absence data.  If I don't include this, it will use the values for cover.  k=3 tells the function that I want to reduce the data to 3 dimensions. 

cov.mds<-metaMDS(cover, distance="bray", binary=TRUE)
cov.mds<-metaMDS(cover, distance="bray", binary=TRUE, trymax = 100)
dimcheckMDS(cover)
set.seed(50)
cov.mds<-metaMDS(cover, distance="bray", binary=TRUE, k=3)
stressplot(cov.mds)
plot(cov.mds)
dimcheckMDS(cover)

#extract the scores for plotting
cov.scores<-as.data.frame(scores(cov.mds))
#above line results in an error that was introduced in an update of vegan.  Use the below code instead
cov.scores = as.data.frame(scores(cov.mds)$sites)
#create the data set with the predictors and the scores
mds.plot<-bind_cols(preds, cov.scores)

#make the plot
ggplot(mds.plot, aes(x=NMDS1, y=NMDS2, color=irr, shape = as.factor(year)))+
  geom_point(size = 3)+
  labs(color="Water", shape = "Year")+
  #theme_bw()
  
  theme(panel.background = element_rect(fill=NA, color="black"))

#the function adonis in package vegan will run the perManova.  The syntax is the same as what we've used with lm, but now the response has to be either a distance matrix or our multivariate species data
veg.cover<-adonis(cover~preds$irr*preds$year, method="bray")
veg.cover<-adonis2(cover~preds$irr*preds$year, method="bray")
#the model will return the anova table
veg.cover

adonis2(cover~irr*year, data = preds, method = "bray")
  
#note that the package vegan also has very good functions for plotting nmds plot
