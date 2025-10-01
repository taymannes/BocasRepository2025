require(vegan)
require(labdsv)
require(magrittr)
library(tidyverse)

# Load data
Oak <- read.csv("Oak_data_47x216.csv", header = TRUE, row.names = 1) 
Oak_spp <- read.csv("Oak_species_189x5.csv", header = TRUE)

# Subset response and explanatory variables
Oak_abund <- Oak[ , colnames(Oak) %in% Oak_spp$SpeciesCode] 
Oak_explan <- Oak[ , ! colnames(Oak) %in% Oak_spp$SpeciesCode]

# Make adjustments to response variables
Oak1 <- Oak_abund %>%
  vegtab(minval = 0.05 * nrow(Oak_abund)) %>% # Remove rare species
  decostand("max") # Relativize by species maxima


#Grazing status
Oak_explan <- Oak_explan %>%
  rownames_to_column(var = "Stand") %>%
  rowid_to_column(var = "ID") %>%
  mutate(GP_GC = paste(GrazPast, GrazCurr, sep = "_")) %>%
  merge(y = data.frame(GP_GC = c("No_No", "Yes_No", "Yes_Yes"),
                       Grazing = c("Never", "Past", "Always"))) %>%
  arrange(ID)

summary(as.factor(Oak_explan$Grazing))

#PERMANOVA
set.seed(42)

adonis2(Oak1 ~ Grazing,
        data = Oak_explan,
        distance = "bray") 

#plotting via NMDA

set.seed(42)

Oak.nmds <- quick.metaMDS(Oak1, 3)

if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("gavinsimpson/ggvegan")


library(ggvegan)

Oak.nmds.gg <- fortify(Oak.nmds) %>%
  filter(score == "sites")

Oak_explan <- Oak_explan %>%
  merge(y = Oak.nmds.gg, by.x = "Stand", by.y = "label")

theme.custom <- theme_bw() +
  theme(axis.line = element_line(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggplot(data = Oak_explan, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = Grazing, shape = Grazing)) +
  theme.custom

adonis2(Oak1 ~ PDIR,
        data = Oak_explan,
        distance = "bray")

library(ggordiplots)

gg_ordisurf(ord = Oak.nmds, env.var = Oak_explan$PDIR)

#SIMPER
#Manual calculation of dissimilarity for each pairwise grouping
meandist(dist = vegdist(Oak1),
         grouping = Oak_explan$Grazing)

simper.Grazing <- simper(Oak1, Oak_explan$Grazing)

summary(simper.Grazing)$Always_Past %>%
  round(3) %>%
  head()

#Converting to a data frame

#have to keep comparisons in order
comparisons <- c("Always_Past", "Always_Never", "Past_Never")

simper.results <- c()

for(i in 1:length(comparisons)) {
  require(tidyverse)
  temp <- summary(simper.Grazing)[as.character(comparisons[i])] %>%
    as.data.frame()
  colnames(temp) <- gsub(
    paste(comparisons[i],".", sep = ""), "", colnames(temp))
  temp <- temp %>%
    mutate(Comparison = comparisons[i],
           Position = row_number()) %>%
    rownames_to_column(var = "Species")
  simper.results <- rbind(simper.results, temp)
}
