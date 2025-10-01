library(vegan)
library(pheatmap)
library(dplyr)
require(labdsv)
require(magrittr)
library(tidyverse)

#T0 ANALYSIS

T0<-read.csv("T0.csv")

#Making sure site is listed as a factor
T0$Site<-as.factor(T0$Site)

#Extract site b/c SIMPER must be run on numerical data. Also extracting "Substrate" & "Other" columns to limit analysis to living functional groups
T0benthicdata<-T0[,-c(1,7,10)]

#Run SIMPER analysis w/ site as group that we're comparing
bocas.simper<-simper(T0benthicdata, T0$Site)

# Check the results
summary(bocas.simper)

#Converting SIMPER results to a data frame

#Listing comparisons for conversion to data frame.
bocascomparisons<-names(bocas.simper)

# Load tidyverse 
require(tidyverse)

# Initialize results as an empty data frame
bocassimper.results <- data.frame()

# Iterate through comparisons
for (i in 1:length(bocascomparisons)) {
  
  # Extract and process the comparison data
  temp <- summary(bocas.simper)[[bocascomparisons[i]]] %>%
    as.data.frame()
  
  # Rename columns to remove the comparison prefix
  colnames(temp) <- gsub(paste(bocascomparisons[i], ".", sep = ""), "", colnames(temp))
  
  # Add comparison and position columns
  temp <- temp %>%
    mutate(Comparison = bocascomparisons[i],
           Position = row_number()) %>%
    rownames_to_column(var = "Species")
  
  # Combine results using bind_rows for efficiency
  bocassimper.results <- bind_rows(bocassimper.results, temp)
}

#Filter results by statistical significance
bocassimper.results %>%
  filter(p <= 0.05) %>%
  select(Species, average, Comparison, Position) 

#Adding up the contributions of all species in each pairwise combination:
 bocassimpercomparisons<- bocassimper.results %>%
  group_by(Comparison) %>%
  summarize(sum.average = sum(average))
 
view(bocassimpercomparisons)

#Comparing to manual calculation of dissimilarity for each pairwise comparisons to make sure the above is correct:
 meandist(dist = vegdist(T0benthicdata),
          grouping = T0$Site)
 
 #Looks good. Values represent dissimilarity, so Coral Cay is most dissimilar to Pastores and so on.
 
 #Graphing results:
 
 p1 <- ggplot(data = bocassimper.results,
              aes(x = Position, y = cumsum)) +
   geom_line(aes(colour = Comparison)) +
   theme_bw()
 
 p2 <- ggplot(data = bocassimper.results,
              aes(x = Position, y = average)) +
   geom_line(aes(colour = Comparison)) +
   theme_bw()
 
 library(ggpubr)
 
 ggarrange(p1, p2, common.legend = TRUE)
 
 ###T9 ANALYSIS
 
 T9<-read.csv("T9.csv")
 
 #Making sure site is listed as a factor
 T9$Site<-as.factor(T9$Site)
 
 #Extract site & time point b/c SIMPER must be run on numerical data. Also extracting "Rubble or Sand" column to limit analysis to living functional groups
 T9benthicdata<-T9[,-c(1,2,11)]
 
 #Run SIMPER analysis w/ site as group that we're comparing
 bocas.simperT9<-simper(T9benthicdata, T9$Site)
 
 # Check the results
 summary(bocas.simperT9)
 
 #Converting SIMPER results to a data frame
 
 #Extracting comparisons for conversion to data frame.
 bocascomparisonsT9<-names(bocas.simperT9)
 
 # Load tidyverse 
 require(tidyverse) 
 
 # Initialize results as an empty data frame
 bocassimper.resultsT9 <- data.frame()
 
 # Iterate through comparisons
 for (i in 1:length(bocascomparisonsT9)) {
   
   # Extract and process the comparison data
   temp1 <- summary(bocas.simperT9)[[bocascomparisonsT9[i]]] %>%
     as.data.frame()
   
   # Rename columns to remove the comparison prefix
   colnames(temp1) <- gsub(paste(bocascomparisonsT9[i], ".", sep = ""), "", colnames(temp1))
   
   # Add comparison and position columns
   temp1 <- temp1 %>%
     mutate(Comparison = bocascomparisonsT9[i],
            Position = row_number()) %>%
     rownames_to_column(var = "Species")
   
   # Combine results using bind_rows for efficiency
   bocassimper.resultsT9 <- bind_rows(bocassimper.resultsT9, temp1)
 }
 
 #Filter results by statistical significance
 bocassimper.resultsT9 %>%
   filter(p <= 0.05) %>%
   select(Species, average, Comparison, Position) 
 
 #Adding up the contributions of all species in each pairwise combination:
 bocassimpercomparisonsT9<- bocassimper.resultsT9 %>%
   group_by(Comparison) %>%
   summarize(sum.average = sum(average))
 
 view(bocassimpercomparisonsT9)
 

 #Graphing results:

 pT91 <- ggplot(data = bocassimper.resultsT9,
              aes(x = Position, y = cumsum)) +
   geom_line(aes(colour = Comparison)) +
   theme_bw()
 
 pT92 <- ggplot(data = bocassimper.resultsT9,
              aes(x = Position, y = average)) +
   geom_line(aes(colour = Comparison)) +
   theme_bw()
 
 library(ggpubr)
 
 ggarrange(pT91, pT92, common.legend = TRUE)
 
 install.packages("openxlsx")
 
 library(openxlsx)
 
 # Save data frame to Excel
 write.xlsx(bocassimper.results, "T0bocas.SIMPER.results.xlsx", rowNames = FALSE)

 write.xlsx(bocassimper.resultsT9, "T9bocas.SIMPER.results.xlsx", rowNames = FALSE)
 
###Running SIMPER with inner/outer bay designations, T0 
 bocasdataT0 <- T0 %>%
   mutate(BayType = case_when(
     Site %in% c("Almirante", "Caracol", "Caracol Deep","STRI","Island Point","Punta Juan") ~ "Inner",
     Site %in% c("Mainland", "Cayo Roldan", "Pastores","Hospital Point","Salt Creek","Pollito","Coral Cay","Popa") ~ "Outer",))
 
 bocasdataT0$BayType<-as.factor(bocasdataT0$BayType)
 
 #Extract site b/c SIMPER must be run on numerical data. Also extracting "Substrate" & "Other" columns to limit analysis to living functional groups
 T0benthicdata2<-T0[,-c(1,7,10,11)]
 
 #Run SIMPER analysis w/ site as group that we're comparing
 bocas.simperT02<-simper(T0benthicdata2, bocasdataT0$BayType)
 
 # Check the results
 summary(bocas.simperT02)
 print(bocas.simperT02)
 
 #Converting SIMPER results to a data frame
 
 #Listing comparisons for conversion to data frame.
 bocasbaycomparisons<-names(bocas.simperT02)
 
 # Load tidyverse 
 require(tidyverse)
 
 # Initialize results as an empty data frame
 bocassimperT0bay <- data.frame()
 
 # Iterate through comparisons
 for (i in 1:length(bocasbaycomparisons)) {
   
   # Extract and process the comparison data
   temp3 <- summary(bocas.simperT02)[[bocasbaycomparisons[i]]] %>%
     as.data.frame()
   
   # Rename columns to remove the comparison prefix
   colnames(temp3) <- gsub(paste(bocasbaycomparisons[i], ".", sep = ""), "", colnames(temp3))
   
   # Add comparison and position columns
   temp3 <- temp3 %>%
     mutate(Comparison = bocasbaycomparisons[i],
            Position = row_number()) %>%
     rownames_to_column(var = "Species")
   
   # Combine results using bind_rows for efficiency
   bocassimperT0bay <- bind_rows(bocassimperT0bay, temp3)
 }
 
 #Filter results by statistical significance
 bocassimper.results %>%
   filter(p <= 0.05) %>%
   select(Species, average, Comparison, Position) 
 
 #Adding up the contributions of all species in each pairwise combination:
 bocassimpercomparisons<- bocassimper.results %>%
   group_by(Comparison) %>%
   summarize(sum.average = sum(average))
 
 view(bocassimpercomparisons)
 
##PERMANOVA to see differences based on site clusters
 # Calculate the Bray-Curtis dissimilarity matrix
 innerouterdist_matrix <- vegdist(community_data, method = "bray")
 
 # Run PERMANOVA for BayType only (excluding Time.Point)
 baytype_permanova <- adonis2(innerouterdist_matrix ~ BayType, data = bocasdata, permutations = 999)
 
 # Print the result
 print(baytype_permanova)
 

 