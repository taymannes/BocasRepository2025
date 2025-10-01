library(vegan)
library(dplyr)
require(labdsv)
require(magrittr)
library(tidyverse)
T0<-read.csv("T0.csv")

#Making sure site is listed as a factor
T0$Site<-as.factor(T0$Site)

###Running SIMPER with inner/outer bay designations, T0 
bocasdataT0 <- T0 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

bocasdataT0$BayType<-as.factor(bocasdataT0$BayType)

#Extract site b/c SIMPER must be run on numerical data. Also extracting "Substrate" & "Other" columns to limit analysis to living functional groups. I also eliminated bleached coral here as it's zero across all data points for T0.
T0benthicdata2<-T0[,-c(1,4,7,10,11)]

#Run SIMPER analysis w/ site as group that we're comparing
bocas.simperT02<-simper(T0benthicdata2, bocasdataT0$BayType)

# Check the results
summary(bocas.simperT02)
print(bocas.simperT02)



##PERMANOVA to see differences based on site clusters, T0
# Calculate the Bray-Curtis dissimilarity matrix
innerouterdist_matrixT0 <- vegdist(T0benthicdata2, method = "bray")

#Adding site classifications
bocasdataT0 <- T0 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

# Run PERMANOVA for BayType only (excluding Time.Point)
baytype_permanovaT0 <- adonis2(innerouterdist_matrixT0 ~ BayType, data = bocasdataT0, permutations = 999)

# Print the result
print(baytype_permanovaT0)


##T9
T9<-read.csv("T9.csv")

#Making sure site is listed as a factor
T9$Site<-as.factor(T9$Site)

###Running SIMPER with inner/outer bay designations, T0 
bocasdataT9 <- T9 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

bocasdataT9$BayType<-as.factor(bocasdataT9$BayType)

#Extract site b/c SIMPER must be run on numerical data. Also extracting "Substrate" & "Other" columns to limit analysis to living functional groups.
T9benthicdata2<-T9[,-c(1,2,11)]

#Run SIMPER analysis w/ site as group that we're comparing
bocas.simperT92<-simper(T9benthicdata2, bocasdataT9$BayType)

# Check the results
summary(bocas.simperT92)
print(bocas.simperT92)



##PERMANOVA to see differences based on site clusters, T0
# Calculate the Bray-Curtis dissimilarity matrix
innerouterdist_matrixT9 <- vegdist(T9benthicdata2, method = "bray")

#Adding site classifications
bocasdataT9 <- T9 %>%
  mutate(BayType = case_when(
    Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
    Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))

# Run PERMANOVA for BayType only (excluding Time.Point)
baytype_permanovaT9 <- adonis2(innerouterdist_matrixT9 ~ BayType, data = bocasdataT9, permutations = 999)

# Print the result
print(baytype_permanovaT9)
