
### This script contains the code for all of the analysis done with the finalized
### data set. 

##  !!! important notice before running the code !!!
##  The sections "open libraries", "establish world map" and "data import" should
##  be run before any of the sections below. Missing packages need to be downloaded
##  before the code can be run.

##  The script is divided into topic section. Some sections are further divided
##  into subsections. Sections can be run independent from one another while sub-
##  sections can generally not be run independently from their respective section.

#---------------open libraries--------------------

### packages along with links and uses are listed in the thesis appendix

library(tidyverse)
library(reshape2)
library(lubridate)

library("ggplot2")
theme_set(theme_bw()) # set ggplot theme
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

library("worrms")

library("vegan")

library("gridExtra")
library("grid")

library("abind")

library("geosphere")

library("scales")

library("plotly")

# ---------------establish world map----------------

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# ------------------------data import------------------------------

# set path for working directory
setwd("D:/Studium/Master/Masterarbeit/Thema 2 Stabilität/Daten und Datenverarbeitung")

# import prepared dataset
zoo_b<-read.csv2("zoobenthos_final.csv")

# -----------------overview map HELCOM-subbasins--------------------

# research stations and subbasins
test <- zoo_b %>%
  group_by(StationID, HELCOM_subbasin) %>%
  summarise()

# mean station coordinates
test <- zoo_b %>%
  group_by(StationID, HELCOM_subbasin) %>%
  summarise(lat = mean(Latitude), lon = mean(Longitude))

# export station list
write.csv2(test, "D:/Studium/Master/Masterarbeit/Thema 2 Stabilität/Daten und Datenverarbeitung/stationlist.csv")

# plot research stations onto world map
ggplot(data = world) +
  geom_sf() +
  geom_point(data = test, aes(x = lon, y = lat, colour = factor(HELCOM_subbasin))) +
  coord_sf(xlim = c(10, 25), ylim = c(54, 60)) + # set map boundaries
  labs(title = "Research station overview", y = "Latitude", x = "Longitude", 
       colour = "HELCOM-subbasin")

# ------------total yearly biomass---------------------

# total yearly biomass per station
test <- zoo_b %>%
  group_by(year, StationID, HELCOM_subbasin) %>%
  summarise(biom = sum(Final_value))

# mean total yearly biomass
mtest <- test %>%
  group_by(year) %>%
  summarise(meanbiom = mean(biom))

# plot total biomass
ggplot(data = test) +
  geom_line(aes(x = year, y = biom, 
                group = StationID, colour = factor(StationID))) +
  geom_line(data = mtest, aes(x = year, y = meanbiom), linewidth = 1) +
  scale_y_log10() +
  labs(title = "Total biomass per station per year", y = "Biomass [g/m^2]",
       x = "Year") +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

### -------------total yearly biomass per subbasin:-------------

### yearly station biomass with station subbasin
test <- zoo_b %>%
  group_by(year, StationID, HELCOM_subbasin) %>%
  summarise(biom = sum(Final_value))

### partition yearly station biomass according to subbasin
BIOM <- list()

# Bay of Mecklenburg
BIOM[["meck"]] <- subset(test, HELCOM_subbasin == "Bay of Mecklenburg")

BIOM[["mmeck"]] <- BIOM[["meck"]] %>%                 # subbasin median
  group_by(year)%>%
  summarise(mean = mean(biom), med = median(biom))

# Arkona Basin
BIOM[["ark"]] <- subset(test, HELCOM_subbasin == "Arkona Basin")

BIOM[["mark"]] <- BIOM[["ark"]] %>%                   # subbasin median
  group_by(year)%>%
  summarise(mean = mean(biom), med = median(biom))

# Bornholm Basin
BIOM[["born"]] <- subset(test, HELCOM_subbasin == "Bornholm Basin")

BIOM[["mborn"]] <- BIOM[["born"]] %>%                 # subbasin median
  group_by(year)%>%
  summarise(mean = mean(biom), med = median(biom))

# Eastern Gotland Basin
BIOM[["egot"]] <- subset(test, HELCOM_subbasin == "Eastern Gotland Basin")

BIOM[["megot"]] <- BIOM[["egot"]] %>%                 # subbasin median
  group_by(year)%>%
  summarise(mean = mean(biom), med = median(biom))

# Western Gotland Basin
BIOM[["wgot"]] <- subset(test, HELCOM_subbasin == "Western Gotland Basin")

BIOM[["mwgot"]] <- BIOM[["wgot"]] %>%                 # subbasin median
  group_by(year)%>%
  summarise(mean = mean(biom), med = median(biom))

### generate plot for every subbasin:

# Bay of Mecklenburg
BIOM[["plot_meck"]] <- ggplot(data = BIOM[["meck"]]) +
  geom_line(aes(x = year, y = biom, colour = factor(StationID))) +
  geom_line(data = BIOM[["mmeck"]], aes(x = year, y = mean), linewidth = 1) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_log10(labels = label_number()) +
  labs(y = "Biomass [g/m^2]", title = "Bay of Mecklenburg", x = "Year") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Arkona Basin
BIOM[["plot_ark"]] <- ggplot(data = BIOM[["ark"]]) +
  geom_line(aes(x = year, y = biom, colour = factor(StationID))) +
  geom_line(data = BIOM[["mark"]], aes(x = year, y = mean), linewidth = 1) +
  scale_y_log10(labels = label_number()) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  labs(y = "Biomass [g/m^2]", title = "Arkona Basin", x = "Year") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Bornholm Basin
BIOM[["plot_born"]] <- ggplot(data = BIOM[["born"]]) +
  geom_line(aes(x = year, y = biom, colour = factor(StationID))) +
  geom_line(data = BIOM[["mborn"]], aes(x = year, y = mean), linewidth = 1) +
  scale_y_log10(labels = label_number()) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  labs(y = "Biomass [g/m^2]", title = "Bornholm Basin", x = "Year") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Eastern Gotland Basin
BIOM[["plot_egot"]] <- ggplot(data = BIOM[["egot"]]) +
  geom_line(aes(x = year, y = biom, colour = factor(StationID))) +
  geom_line(data = BIOM[["megot"]], aes(x = year, y = mean), linewidth = 1) +
  scale_y_log10(labels = label_number()) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  labs(y = "Biomass [g/m^2]", title = "Eastern Gotland Basin", x = "Year") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Western Gotland Basin
BIOM[["plot_wgot"]] <- ggplot(data = BIOM[["wgot"]]) +
  geom_line(aes(x = year, y = biom, colour = factor(StationID))) +
  geom_line(data = BIOM[["mwgot"]], aes(x = year, y = mean), linewidth = 1) +
  scale_y_log10(labels = label_number()) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  labs(y = "Biomass [g/m^2]", title = "Western Gotland Basin", x = "Year") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# arrange plots on grid
grid.arrange(BIOM[["plot_meck"]], BIOM[["plot_ark"]], BIOM[["plot_born"]], 
             BIOM[["plot_egot"]], BIOM[["plot_wgot"]], ncol = 3)

# ------------------alpha diversity--------------------

### Shannon Index

# total shannon index
test <- diversity(zoo_b$Final_value, "shannon")

# yearly station shannon index
test <- zoo_b %>%
  group_by(year, StationID, HELCOM_subbasin) %>%
  summarise(div_shan = diversity(Final_value, "shannon"))

# total yearly mean diversity
mtest <- test %>%
  group_by(year) %>%
  summarise(meandiv = mean(div_shan))

# plot yearly station shannon index and yearly mean shannon index
ggplot(data = test) +
  geom_line(aes(x = year, y = div_shan,
                 group = StationID, colour = factor(StationID))) +
  geom_line(data = mtest, aes(x = year, y = meandiv), linewidth = 1) +
  labs(title = "Shannon-Index per year", y = "Shannon-Index") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(2010, 2022, 2))

### Effective Number of Species

ENS <- list()

# yearly effective number of species per station
ENS[["alpha"]] <- zoo_b %>%
  group_by(year, StationID, HELCOM_subbasin) %>%
  summarise(D_0 = length(unique(checked_name)),       # species richness
            D_1 = exp(diversity(Final_value, "shannon")),    # hill number q=1
            D_2 = diversity(Final_value, "invsimpson")       # hill number q=2
            )

# yearly mean effective number of species
ENS[["mean"]] <- ENS[["alpha"]] %>%
  group_by(year) %>%
  summarise(D_0 = mean(D_0),          # species richness
            D_1 = mean(D_1),          # hill number q=1
            D_2 = mean(D_2))          # hill number q=2

# plot species richness
ENS[["Plot_D0"]] <- ggplot(data = ENS[["alpha"]]) +
  geom_line(aes(x = year, y = D_0, 
                group = StationID, colour = factor(StationID))) +
  geom_line(data = ENS[["mean"]], aes(x = year, y = D_0), linewidth = 1) +
  labs(title = "Species Richness (Hill q = 0)", y = "ENS", x = "Year") +
  theme(legend.position = "none",
        axis.text = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(2010, 2022, 2))

# plot hill number q=1
ENS[["Plot_D1"]] <- ggplot(data = ENS[["alpha"]]) +
  geom_line(aes(x = year, y = D_1, 
                group = StationID, colour = factor(StationID))) +
  geom_line(data = ENS[["mean"]], aes(x = year, y = D_1), linewidth = 1) +
  labs(title = "Hill q = 1", y = "ENS", x = "Year") +
  theme(legend.position = "none",
        axis.text = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(2010, 2022, 2))

# plot hill number q=2
ENS[["Plot_D2"]] <- ggplot(data = ENS[["alpha"]]) +
  geom_line(aes(x = year, y = D_2, 
                group = StationID, colour = factor(StationID))) +
  geom_line(data = ENS[["mean"]], aes(x = year, y = D_2), linewidth = 1) +
  labs(title = "Hill q = 2", y = "ENS", x = "Year") +
  theme(legend.position = "none",
        axis.text = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(2010, 2022, 2))

# arrange plots on grid
grid.arrange(ENS[["Plot_D0"]], ENS[["Plot_D1"]], ENS[["Plot_D2"]], ncol = 2)

### ---------------alpha diversity per subbasin-------------------

ENS <-list()

# yearly effective number of species per station
ENS[["alpha"]] <- zoo_b %>%
  group_by(year, HELCOM_subbasin, StationID) %>%
  summarise(D_0 = length(unique(checked_name)),
            D_1 = exp(diversity(Final_value, "shannon")),
            D_2 = diversity(Final_value, "invsimpson"))

### Arkona Basin

ENS[["ark"]] <- subset(ENS[["alpha"]], HELCOM_subbasin == "Arkona Basin")

# mean yearly ENS Arkona Basin
ENS[["mark"]] <- subset(ENS[["alpha"]], 
                                 HELCOM_subbasin == "Arkona Basin") %>%
  group_by(year) %>%
  summarise(D_0 = mean(D_0),           # species richness
            D_1 = mean(D_1),           # hill number q=1
            D_2 = mean(D_2))           # hill number q=2

### Bay of Mecklenburg

ENS[["meck"]] <- subset(ENS[["alpha"]], 
                              HELCOM_subbasin == "Bay of Mecklenburg")

# mean yearly ENS Bay of Mecklenburg
ENS[["mmeck"]] <- subset(ENS[["alpha"]], 
                                  HELCOM_subbasin == "Bay of Mecklenburg") %>%
  group_by(year) %>%
  summarise(D_0 = mean(D_0),           # species richness
            D_1 = mean(D_1),           # hill number q=1
            D_2 = mean(D_2))           # hill number q=2

### Bornholm Basin

ENS[["born"]] <- subset(ENS[["alpha"]], 
                              HELCOM_subbasin == "Bornholm Basin")

# mean yearly ENS Bornholm Basin
ENS[["mborn"]] <- subset(ENS[["alpha"]], 
                                  HELCOM_subbasin == "Bornholm Basin") %>%
  group_by(year) %>%
  summarise(D_0 = mean(D_0),           # species richness
            D_1 = mean(D_1),           # hill number q=1
            D_2 = mean(D_2))           # hill number q=2

### Eastern Gotland Basin

ENS[["egot"]] <- subset(ENS[["alpha"]], 
                              HELCOM_subbasin == "Eastern Gotland Basin")

# mean yearly ENS Eastern Gotland Basin
ENS[["megot"]] <- subset(ENS[["alpha"]], 
                                  HELCOM_subbasin == "Eastern Gotland Basin") %>%
  group_by(year) %>%
  summarise(D_0 = mean(D_0),           # species richness
            D_1 = mean(D_1),           # hill number q=1
            D_2 = mean(D_2))           # hill number q=2

### Western Gotland Basin

ENS[["wgot"]] <- subset(ENS[["alpha"]], 
                              HELCOM_subbasin == "Western Gotland Basin")

# mean yearly ENS Western Gotland Basin
ENS[["mwgot"]] <- subset(ENS[["alpha"]], 
                                  HELCOM_subbasin == "Western Gotland Basin") %>%
  group_by(year) %>%
  summarise(D_0 = mean(D_0),           # species richness
            D_1 = mean(D_1),           # hill number q=1
            D_2 = mean(D_2))           # hill number q=2

# Plot Arkona Basin
ENS[["ark_plot"]] <- ggplot() +
  geom_line(data = ENS[["ark"]], aes(x = year, y = D_2, colour = factor(StationID))) +
  geom_line(data = ENS[["mark"]], aes(x = year, y = D_2), linewidth = 1) +
  labs(title = "Arkona Basin", y = "ENS (Hill q = 2)", x = "Year") +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

# Plot Bay of Mecklenburg
ENS[["meck_plot"]] <- ggplot() +
  geom_line(data = ENS[["meck"]], aes(x = year, y = D_2, colour = factor(StationID))) +
  geom_line(data = ENS[["mmeck"]], aes(x = year, y = D_2), linewidth = 1) +
  labs(title = "Bay of Mecklenburg", y = "ENS (Hill q = 2)", x = "Year") +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

# Plot Bornholm Basin
ENS[["born_plot"]] <- ggplot() +
  geom_line(data = ENS[["born"]], aes(x = year, y = D_2, colour = factor(StationID))) +
  geom_line(data = ENS[["mborn"]], aes(x = year, y = D_2), linewidth = 1) +
  labs(title = "Bornholm Basin", y = "ENS (Hill q = 2)", x = "Year") +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

# Plot Eastern Gotland Basin
ENS[["egot_plot"]] <- ggplot() +
  geom_line(data = ENS[["egot"]], aes(x = year, y = D_2, colour = factor(StationID))) +
  geom_line(data = ENS[["megot"]], aes(x = year, y = D_2), linewidth = 1) +
  labs(title = "Eastern Gotland Basin", y = "ENS (Hill q = 2)", x = "Year") +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

# Plot Western Gotland Basin
ENS[["wgot_plot"]] <- ggplot() +
  geom_line(data = ENS[["wgot"]], aes(x = year, y = D_2, colour = factor(StationID))) +
  geom_line(data = ENS[["mwgot"]], aes(x = year, y = D_2), linewidth = 1) +
  labs(title = "Western Gotland Basin", y = "ENS (Hill q = 2)", x = "Year") +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

# arrange plots on a grid
grid.arrange(ENS[["meck_plot"]], ENS[["ark_plot"]], ENS[["born_plot"]], 
             ENS[["egot_plot"]], ENS[["wgot_plot"]], ncol = 3)

#---------------------community composition------------------------

###------------------manual color scheme---------------------

taxon_colors <- c("Amphitrite figulus" = "magenta4",
                  "Arctica islandica" = "darkslateblue", 
                  "Capitella capitata" = "violetred",
                  "Cerastoderma glaucum" = "royalblue",
                  "Diastylis rathkei" = "mediumorchid3",
                  "Halicryptus spinulosus" = "turquoise2",
                  "Hediste diversicolor" = "lightgreen",
                  "Hydrobia" = "darkolivegreen",
                  "Lagis koreni" = "mediumspringgreen",
                  "Macoma balthica" = "forestgreen",
                  "Marenzelleria" = "olivedrab3",
                  "Marenzelleria neglecta" = "gold1",
                  "Monoporeia affinis" = "darkgoldenrod2",
                  "Mya arenaria" = "darkorange",
                  "Mytilus" = "lightsalmon",
                  "Mytilus edulis" = "tomato3",
                  "Mytilus trossulus" = "firebrick1",
                  "Nephtys caeca" = "red4",
                  "Nephtys ciliata" = "lightpink",
                  "Nephtys hombergii" = "lightpink4",
                  "other" = "gray70",
                  "Pontoporeia femorata" = "lightsteelblue1",
                  "Pygospio elegans" = "darkseagreen",
                  "Saduria entomon" = "hotpink1",
                  "Scoloplos armiger" = "lemonchiffon",
                  "Terebellides stroemii" = "lightsteelblue4",
                  "Tridonta borealis" = "maroon",
                  "Tridonta elliptica" = "peachpuff")

pie(rep(1, 27), col = taxon_colors)

###--------------taxon share of total biomass by subbasin----------------

# list of all taxa
unique(zoo_b$checked_name)

# biomass per taxon per subbasin
test <- zoo_b %>%
  group_by(HELCOM_subbasin, checked_name) %>%
  summarise(biom = sum(Final_value))

COM <- list()

COM[["names"]] <- c("meck", "ark", "born", "gdan", "egot", "wgot")

COM[["titles"]] <- c("Bay of Mecklenburg", "Arkona Basin", "Bornholm Basin",
                     "Gdansk Basin", "Eastern Gotland Basin", 
                     "Western Gotland Basin")

# find the four taxa with highest biomass share in each subbasin and summarize
# all other taxa in category 'other':
for(x in 1:length(COM[["names"]])) {
  
  # sort taxa by biomass in descending order
  COM[[COM[["names"]][x]]] <- subset(test, HELCOM_subbasin == COM[["titles"]][x]) %>%
    arrange(desc(biom))    
  
  # keep first four entries and summarize the rest in category 'other'
  COM[[COM[["names"]][x]]][5:nrow(COM[[COM[["names"]][x]]]),
                           "checked_name"] <- "other"
  
  # group by subbasin and taxon and summarize biomass for each taxon
  COM[[COM[["names"]][x]]] <- COM[[COM[["names"]][x]]] %>% 
    group_by(HELCOM_subbasin, checked_name) %>% 
    summarise(biom = sum(biom))
  
}

# combine dataframes
COM[["total"]] <- rbind(COM[["meck"]], COM[["ark"]], COM[["born"]], 
                        COM[["gdan"]], COM[["egot"]], COM[["wgot"]])

COM[["total"]]$HELCOM_subbasin <- factor(COM[["total"]]$HELCOM_subbasin,
                                         levels = COM[["titles"]])

# see full list of taxa in the final plot
unique(COM[["total"]]$checked_name)

# plot taxon biomass shares
ggplot(data = COM[["total"]], aes(fill = checked_name, 
                                  y = biom, x = HELCOM_subbasin)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = taxon_colors) +
  scale_x_discrete(labels = label_wrap(15)) +
  labs(title = "Taxon share of total biomass", y = "Total biomass [g/m^2]", 
       x = "HELCOM-subbasin", fill = "Taxon name") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12)) 


###-----------------taxon share of total biomass by station--------------------

# biomass per taxon per station
test <- zoo_b %>%
  group_by(HELCOM_subbasin, StationID, checked_name) %>%
  summarise(biom = sum(Final_value))

COM <- list()

COM[["names"]] <- c("meck", "ark", "born", "gdan", "egot", "wgot")

COM[["titles"]] <- c("Bay of Mecklenburg", "Arkona Basin", "Bornholm Basin",
                     "Gdansk Basin", "Eastern Gotland Basin", 
                     "Western Gotland Basin")

# For every subbasin,find the four taxa with highest biomass share at each 
# station and summarize all other taxa in category 'other':
for(x in 1:length(COM[["names"]])) {
  
  COM[["temp_basin"]] <- subset(test, HELCOM_subbasin == COM[["titles"]][x]) %>%
    group_by(StationID, checked_name, biom) %>% summarise()
  
  COM[["stations"]] <- unique(COM[["temp_basin"]]$StationID)
  
  COM[["temp_station"]] <- subset(COM[["temp_basin"]], 
                                  StationID == COM[["stations"]][1]) %>%
    arrange(desc(biom)) 
  
  COM[["temp_station"]][5:nrow(COM[["temp_station"]]), 
                        "checked_name"] <- "other"
  
  COM[["temp_station"]] <- COM[["temp_station"]] %>% 
    group_by(StationID, checked_name) %>% 
    summarise(biom = sum(biom))
  
  COM[[COM[["names"]][x]]] <- COM[["temp_station"]]
  
  if (length(COM[["stations"]]) <= 1) next
  
  for (y in 2:length(COM[["stations"]])) {
  
    COM[["temp_station"]] <- subset(COM[["temp_basin"]], 
                                    StationID == COM[["stations"]][y]) %>%
      arrange(desc(biom)) 
    
    COM[["temp_station"]][5:nrow(COM[["temp_station"]]), 
                          "checked_name"] <- "other"
    
    COM[["temp_station"]] <- COM[["temp_station"]] %>% 
      group_by(StationID, checked_name) %>% 
      summarise(biom = sum(biom))
    
    COM[[COM[["names"]][x]]] <- rbind(COM[[COM[["names"]][x]]], 
                                      COM[["temp_station"]])
  }
  
}

# combine dataframes
taxa <- c(COM[["meck"]]$checked_name, COM[["ark"]]$checked_name,
          COM[["born"]]$checked_name, COM[["egot"]]$checked_name,
          COM[["wgot"]]$checked_name)

# list of all taxa within the final plots
taxa <- unique(taxa)

taxa

# plot Bay of Mecklenburg
COM[["plot_meck"]] <- ggplot(data = COM[["meck"]], aes(fill = checked_name, 
                                               y = biom, x = StationID)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Bay of Mecklenburg", x = "StationID", y = "Total biomass [g/m^2]",
       fill = "Taxon name") +
  theme(legend.key.size = unit(0.1, 'cm'), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6)) +
  scale_fill_manual(values = taxon_colors) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12)) 

# plot Arkona Basin
COM[["plot_ark"]] <- ggplot(data = COM[["ark"]], aes(fill = checked_name, 
                                                       y = biom, x = StationID)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Arkona Basin", x = "StationID", y = "Total biomass [g/m^2]",
       fill = "Taxon name") +
  theme(legend.key.size = unit(0.1, 'cm'), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))+
  scale_fill_manual(values = taxon_colors)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 12))

# plot Bornholm Basin
COM[["plot_born"]] <- ggplot(data = COM[["born"]], aes(fill = checked_name, 
                                                     y = biom, x = StationID)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Bornholm Basin", x = "StationID", y = "Total biomass [g/m^2]",
       fill = "Taxon name") +
  theme(legend.key.size = unit(0.1, 'cm'), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))+
  scale_fill_manual(values = taxon_colors)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12))

# plot Eastern Gotland Basin
COM[["plot_egot"]] <- ggplot(data = COM[["egot"]], aes(fill = checked_name, 
                                                     y = biom, x = StationID)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Eastern Gotland Basin", x = "StationID", y = "Total biomass [g/m^2]",
       fill = "Taxon name") +
  theme(legend.key.size = unit(0.1, 'cm'), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))+
  scale_fill_manual(values = taxon_colors)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12))

# plot Western Gotland Basin
COM[["plot_wgot"]] <- ggplot(data = COM[["wgot"]], aes(fill = checked_name, 
                                                     y = biom, x = StationID)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Western Gotland Basin", x = "StationID", y = "Total biomass [g/m^2]",
       fill = "Taxon name") +
  theme(legend.key.size = unit(0.1, 'cm'), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))+
  scale_fill_manual(values = taxon_colors)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12))

# arrange single plots on grid
grid.arrange(COM[["plot_meck"]], COM[["plot_ark"]], COM[["plot_born"]],
             COM[["plot_egot"]], COM[["plot_wgot"]])
  
#-------------------IAR--------------------

### --------------data preparation-------------------

del_stat <- c() # insert names of stations that should be omitted from the IAR
                # leave vector empty if no stations should be omitted

del_spec <- c() # insert names of taxa that should be omitted from the IAR
                # leave vector empty if no taxa should be omitted

# yearly taxon biomass per station
temp <- subset(zoo_b, ! StationID %in% del_stat & ! checked_name %in% del_spec) %>%
  group_by(StationID, checked_name, year) %>%
  summarise(biomass = sum(Final_value))

# save number of stations
statnumber <- length(unique(temp$StationID))

# convert wide format to long format:
temp <- dcast(temp, StationID + checked_name ~ year, value.var = "biomass") %>%
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
  group_by(StationID) %>%
  summarise(`2010` = sum(`2010`), `2011` = sum(`2011`), `2012` = sum(`2012`),
            `2013` = sum(`2013`), `2014` = sum(`2014`), `2015` = sum(`2015`),
            `2016` = sum(`2016`), `2017` = sum(`2017`), `2018` = sum(`2018`),
            `2020` = sum(`2020`), `2021` = sum(`2021`), `2022` = sum(`2022`))

### ---------------------Random Aggregation---------------------

randselect <- function(x) {         # Function that takes a pool of station
  if(length(x) <= 1) {return(x)}    # names and returns a randomly picked station
  else {
    return(sample(x, 1))
  }
}

# empty matrix for the IAR values
IAR <- matrix(nrow = statnumber, ncol = statnumber) 

IAR <- data.frame(IAR, row.names = temp$StationID)

# calculate one IAR for each station
for(x in 1:length(temp$StationID)) {
  
  station <- temp$StationID[x] # first station of the IAR
  
  rest <- temp$StationID[! temp$StationID == station] # vector of stations that 
  # are yet to be aggregated
  
  biom <- as.numeric(temp[x, 2:13]) # yearly biomass at first station
  
  Inv <- (mean(biom)/sd(biom))^2 # invariability of first station
  
  IAR[x, 1] <- Inv # save invariability at first station into results dataframe
  
  for(y in 2:length(temp$StationID) ) { 
    
    station <- c(station, randselect(rest)) # randomly select next station to be 
                                            # aggregated
    
    index <- match(station[length(station)], temp$StationID) # index of selected
                                                             # station
    
    biom <- biom + as.numeric(temp[index, 2:13]) # pool biomass of aggregated 
                                                 # stations
    
    Inv <- (mean(biom)/sd(biom))^2 # invariability of aggregated stations
    
    IAR[x, y] <- Inv # save invariability to results dataframe
    
    rest <- rest[! rest %in% station]
    
  }
}

# calculate IAR median and confidence interval:

IARplot <- data.frame(
  median <- vector(mode = "numeric", statnumber),
  quantile_25 <- vector(mode = "numeric", statnumber),
  quantile_75 <- vector(mode = "numeric", statnumber)
)

names(IARplot) <- c("median", "25% quantile", "75% quantile")

for(x in 1:statnumber) {
  
  IARplot$median[x] <- median(IAR[,x])
  
  IARplot$`25% quantile`[x] <- quantile(IAR[,x], 0.25)
  
  IARplot$`75% quantile`[x] <- quantile(IAR[,x], 0.75)
  
}

# plot median and confidence interval for the random aggregation
r <- ggplot(data = IARplot) +
  geom_line(aes(x = 1:statnumber, y = median)) +
  geom_line(aes(x = 1:statnumber, y = 1:statnumber), linetype = 2, linewidth = 1) +
  geom_ribbon(aes(x = 1:statnumber, ymin = `25% quantile`, ymax = `75% quantile`), 
              alpha = 0.3) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Random aggregation",
       x = "Number of stations A", y = "Invariability I") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

### ---------------Nearest Neighbor Aggregation:-----------------

# empty matrix for the IAR values
IAR <- matrix(nrow = statnumber, ncol = statnumber) 

# names of aggregated stations 
IAR_names <- data.frame(IAR, row.names = temp$StationID) 

IAR <- data.frame(IAR, row.names = temp$StationID)

# calculate one IAR for each station
for(x in 1:statnumber) {
  
  # first station of the IAR with coordinates
  station_1 <- subset(zoo_b, StationID == temp$StationID[x]) %>%
    group_by(StationID) %>%
    summarise(longitude = mean(Longitude), latitude = mean(Latitude))
  
  # stations yet to be aggregated with coordinates
  rest <- subset(zoo_b, ! StationID == temp$StationID[x] & ! StationID %in% del_stat) %>%
    group_by(StationID) %>%
    summarise(longitude = mean(Longitude), latitude = mean(Latitude))
  
  # distances of the other stations to first station
  rest$distance <- t(distm(station_1[2:3], rest[2:3]))
  
  # sort stations to be aggregated by distance to station 1 in ascending order
  rest <- arrange(rest, distance) 
  
  ###
  
  biom <- as.numeric(temp[x, 2:13]) # yearly biomass at first station
  
  Inv <- (mean(biom)/sd(biom))^2 # invariability at first station
  
  IAR[x, 1] <- Inv # save invariability at first station to results
  
  # aggregate remaining stations
  for(y in 2:statnumber) {
    
    station_n <- rest$StationID[y-1] # name of next station to be aggregated
    
    IAR_names[x, y] <- station_n # save name of aggregated station to dataframe
    
    index <- match(station_n, temp$StationID) # index of selected station in the 
                                              # original list
    
    ###
    
    biom <- biom + as.numeric(temp[index, 2:13]) # pool biomass of aggregated
                                                 # stations
    
    Inv <- (mean(biom)/sd(biom))^2 # invariability of aggregated stations
    
    IAR[x, y] <- Inv # save invariability of aggregated station to results
    
  }
}

# calculate IAR median and confidence interval:

IARplot <- data.frame(
  median <- vector(mode = "numeric", statnumber),
  quantile_25 <- vector(mode = "numeric", statnumber),
  quantile_75 <- vector(mode = "numeric", statnumber)
)

names(IARplot) <- c("median", "25% quantile", "75% quantile")

for(x in 1:statnumber) {
  
  IARplot$median[x] <- median(IAR[,x])
  
  IARplot$`25% quantile`[x] <- quantile(IAR[,x], 0.25)
  
  IARplot$`75% quantile`[x] <- quantile(IAR[,x], 0.75)
  
}

# plot median and confidence interval for the nearest-neighbor aggregation
nn <- ggplot(data = IARplot) +
  geom_line(aes(x = 1:statnumber, y = median)) +
  geom_line(aes(x = 1:statnumber, y = 1:statnumber), linetype = 2, linewidth = 1) +
  geom_ribbon(aes(x = 1:statnumber, ymin = `25% quantile`, ymax = `75% quantile`), 
              alpha = 0.3) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Nearest-neighbor aggregation",
       x = "Number of stations A", y = "Invariability I")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# arrange IAR plots on a grid
grid.arrange(r, nn, ncol = 2)

### ------------------single IARs------------------------

# Make dataframe with single IARs
IARplot <- data.frame(
  A <- vector(mode = "numeric", statnumber^2),
  StationID <- vector(mode = "numeric", statnumber^2),
  I <- vector(mode = "numeric", statnumber^2),
  subbasin <- vector(mode = "numeric", statnumber^2)
)

names(IARplot) <- c("A", "StationID", "I", "subbasin")

for( x in 1:statnumber ) {
  
  basin <- match(rownames(IAR[x,]), zoo_b$StationID)
  basin <- zoo_b$HELCOM_subbasin[basin] 
  # Find HELCOM subbasin of first station
  
  lower <- 1 + (statnumber*(x-1))
  upper <- statnumber + (statnumber*(x-1))
  
  IARplot$A[lower:upper] <- c(1:statnumber)
  # Number of Stations
  
  IARplot$StationID[lower:upper] <- rep(rownames(IAR[x,]), statnumber)
  # StationID
  
  IARplot$I[lower:upper] <- as.numeric(IAR[x,])
  # Invariability
  
  IARplot$subbasin[lower:upper] <- rep(basin, statnumber)
  # HELCOM subbasin
  
}

# plot single IARs with subbasin of first station indicated by color
ggplot(data = IARplot) +
  geom_line(aes(x = A, y = I, group = StationID, colour = factor(subbasin))) +
  labs(x = "Number of stations A", y = "Invariability I", 
       colour =  "Subbasin of first station")

# plot single IARs with StationID of first station indicated by color
ggplot(data = IARplot) +
  geom_line(aes(x = A, y = I, colour = factor(StationID))) +
  labs(x = "Number of stations A", y = "Invariability I", 
       colour =  "StationID of first station")

###------------------station and taxon effects on total I---------------------

### station impacts

# station names
names <- sort(unique(zoo_b$StationID))

# station biomass and invariability
stations <- subset(zoo_b, StationID %in% names) %>%
  group_by(StationID, HELCOM_subbasin, year) %>%
  summarise(biomass = sum(Final_value)) %>%
  group_by(StationID, HELCOM_subbasin) %>%
  summarise(biom = sum(biomass), I = (mean(biomass)/sd(biomass))^2)

stations$effect <- vector(mode = "numeric", length = length(names))

# total yearly biomass
temp <- zoo_b %>%
  group_by(year) %>%
  summarise(biom = sum(Final_value))

# total invariability
I_normal <- (mean(temp$biom)/sd(temp$biom))^2

# calculating station effect on total invariability
for (x in 1:length(names)) {

  temp <- subset(zoo_b, !StationID == names[x]) %>%
    group_by(year) %>%
    summarise(biom = sum(Final_value))
  
  I_omitt <- (mean(temp$biom)/sd(temp$biom))^2
  
  stations$effect[x] <- log(I_normal / I_omitt)
  
}

# plot station impact over station invariability
ggplot(data = stations) +
  geom_point(aes(x = I, y = effect, colour = factor(HELCOM_subbasin)), size = 2) +
  scale_x_log10() +
  labs(title = "Station I impact on total I", x = "Station I",
       y = "Contribution to total I", colour = "StationID")

# plot station impact over total station biomass
ggplot(data = stations) +
  geom_point(aes(x = biom, y = effect, colour = factor(HELCOM_subbasin))) +
  scale_x_log10() +
  labs(title = "Station biomass impact on total I", x = "Total Station biomass",
       y = "Effect of Station on total I", colour = "StationID")

## correlation tests station impacts:

# correlation tests station impact ~ station invariability 
cor.test(stations$I, stations$effect)                         # pearson-test
cor.test(stations$I, stations$effect, method = "spearman")    # spearman-test

# correlation tests station impact ~ station biomass
cor.test(stations$biom, stations$effect)                        # pearson-test
cor.test(stations$biom, stations$effect, method = "spearman")   # spearman-test

# logarithmic model station impact ~ log(station invariability)
linmod <- lm(stations$effect ~ log(stations$I))
summary(linmod)

# multi-model station impact ~ log(station invariability) * total station biomass
multimod <- lm(stations$effect ~ log(stations$I) * stations$biom)
summary(multimod)

# plot station impact over station invariability with total station biomass
# highlighted by color
ggplot(data = stations) +
  geom_point(aes(x = I, y = effect, colour = biom), size = 2) +
  scale_color_gradientn(colours = c("blue3","chartreuse4","goldenrod1",
                                    "red2","deeppink"),
                        rescaler = ~ scales::rescale_mid(.x, mid = 9000)) +
  scale_x_log10() +
  labs(title = "Station impact on total invariability", x = "Station I",
       y = "log(total I/I with station omitted)", colour = "Total station 
biomass [g/m^2]")

### taxon impacts

# taxon names
names <- sort(unique(zoo_b$checked_name))

# list all taxa and the years they were found in
species <- subset(zoo_b, checked_name %in% names) %>%
  group_by(checked_name, year) %>%
  summarise(biomass = sum(Final_value)) 

# Filter out the taxa that were not present in every year

del <- c()

for (x in 1:length(names)) {
  
  temp <- species$checked_name[species$checked_name == names[x]]
  
  if( length(temp) < 12) {
    species <- subset(species, !checked_name == names[x])
    del <- c(del, names[x])
  }
  
}

# names of the taxa present in every year
names <- names[!names %in% del]

# taxon invariability and total taxon biomass
species <- species %>%
  group_by(checked_name) %>%
  summarise(biom = sum(biomass), I = (mean(biomass)/sd(biomass))^2)

# column for taxon impact
species$effect <- vector(mode = "numeric", length = nrow(species))

# total yearly biomass
temp <- zoo_b %>%
  group_by(year) %>%
  summarise(biom = sum(Final_value))

# total invariability
I_normal <- (mean(temp$biom)/sd(temp$biom))^2

# calculating taxon effect on invariability
for (x in 1:nrow(species)) {
  
  temp <- subset(zoo_b, !checked_name == names[x]) %>%
    group_by(year) %>%
    summarise(biom = sum(Final_value))
  
  I_omitt <- (mean(temp$biom)/sd(temp$biom))^2
  
  species$effect[x] <- log(I_normal / I_omitt)
  
}

# plot taxon impact over taxon invariability with total taxon biomass highlighted
ggplot(data = species) +
  geom_point(aes(x = I, y = effect, colour = biom), size = 2) +
  scale_color_gradientn(colours = c("blue3","chartreuse4","goldenrod1",
                                    "red2","deeppink"),
                        rescaler = ~ scales::rescale_mid(.x, mid = 50000)) +
  labs(title = "Taxon impact on total invariability", x = "Taxon I",
       y = "Contribution to total I", colour = "Total taxon 
biomass [g/m^2]")

# plot taxon impact over total taxon biomass
ggplot(data = species) +
  geom_point(aes(x = biom, y = effect)) +
  labs(title = "Biomass - Destabilizing effect", x = "Total taxon biomass",
       y = "Contribution to total I")

## correlation tests taxon impact:

# correlation test taxon impact ~ taxon invariability
cor.test(species$I, species$effect)                             # pearson-test
cor.test(species$I, species$effect, method = "spearman")        # spearman-test

# correlation test taxon impact ~ total taxon biomass
cor.test(species$biom, species$effect)                          # pearson-test
cor.test(species$biom, species$effect, method = "spearman")     # spearman-test

# multi-model taxon impact ~ log(taxon invariability) * total taxon biomass
multimod <- lm(species$effect ~ log(species$I) * species$biom)
summary(multimod)

### ----------------3d-scatterplot station impact multimodel------------------

effect <- stations$effect
I <- stations$I
biom <- stations$biom

multiplot <- plot_ly(x = ~I, y = ~biom, z = ~effect, 
                     type = "scatter3d", mode = "markers", color = ~effect, 
                     colorscale = "viridis", marker = list(size = 3))

### regression surface using the multi-model:

multimod <- lm(effect ~ log(I) * biom)

# Setup Axis:

axis_x <- seq(min(I), max(I), by = 0.5)
axis_y <- seq(min(biom), max(biom), by = 0.5)

# Sample points:

eff_lm_surface <- expand.grid(I = axis_x, biom = axis_y, KEEP.OUT.ATTRS = F)
eff_lm_surface$effect <- predict.lm(multimod, newdata = eff_lm_surface)
eff_lm_surface <- acast(eff_lm_surface, biom ~ I, value.var = "effect") # y ~ x

multiplot <- add_trace(p = multiplot,
                       z = eff_lm_surface,
                       x = axis_x,
                       y = axis_y,
                       type = "surface")

multiplot

### --------station and taxon effect on the nearest-neighbor IAR median---------

### station impacts

## manual color palette

site_palette <- c("(f) none" = "black",
                  "(a) K18" = "#FF6666",
                  "(b) K22" = "#95A900",
                  "(c) OMMVZBB22" = "#00BF7D",
                  "(d) OMMVZBB15" = "#00ABFD",
                  "(e) OMMVZBC20" = "#E76BFD") 

## data preparation

IARs <- list()

# dataframe for results
IARs[["IARplot"]] <- data.frame(
  A <- rep(1:55, 6), I <- rep(NA, 55*6), stat_omit <- rep(NA, 55*6)
)
names(IARs[["IARplot"]]) <- c("A", "I", "Stations omitted")

# establish which stations to omit
IARs[["del"]] <- list()
IARs[["del"]][[1]] <- c()
IARs[["del"]][[2]] <- c("K18")
IARs[["del"]][[3]] <- c("K22")
IARs[["del"]][[4]] <- c("OMMVZBB22")
IARs[["del"]][[5]] <- c("OMMVZBB15")
IARs[["del"]][[6]] <- c("OMMVZBC20")

IARs[["del_names"]] <- c("(f) none", "(a) K18", "(b) K22", "(c) OMMVZBB22",
                         "(d) OMMVZBB15", "(e) OMMVZBC20")

## calculate IARs with stations omitted

for(i in 1:6) {

# data preparation  
  
# data subset with station omitted
IARs[["temp"]] <- subset(zoo_b, ! StationID %in% IARs[["del"]][[i]]) %>%
  group_by(StationID, checked_name, year) %>%
  summarise(biomass = sum(Final_value))

IARs[["statnumber"]] <- length(unique(IARs[["temp"]]$StationID))

# convert wide format to long format:
IARs[["temp"]] <- dcast(IARs[["temp"]], StationID + checked_name ~ year, value.var = "biomass") %>%
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
  group_by(StationID) %>%
  summarise(`2010` = sum(`2010`), `2011` = sum(`2011`), `2012` = sum(`2012`),
            `2013` = sum(`2013`), `2014` = sum(`2014`), `2015` = sum(`2015`),
            `2016` = sum(`2016`), `2017` = sum(`2017`), `2018` = sum(`2018`),
            `2020` = sum(`2020`), `2021` = sum(`2021`), `2022` = sum(`2022`))

# empty matrix for results of the IAR
IARs[["IAR"]] <- matrix(nrow = IARs[["statnumber"]], ncol = IARs[["statnumber"]]) 

# convert to dataframe
IARs[["IAR"]] <- data.frame(IARs[["IAR"]], row.names = IARs[["temp"]]$StationID)

# calculate the IAR with station omitted
for(x in 1:IARs[["statnumber"]]) {
  
  # first station of the IAR with coordinates
  IARs[["station_1"]] <- subset(zoo_b, StationID == IARs[["temp"]]$StationID[x]) %>%
    group_by(StationID) %>%
    summarise(longitude = mean(Longitude), latitude = mean(Latitude))
  
  # stations to be aggregated with coordinates
  IARs[["rest"]] <- subset(zoo_b, ! StationID == IARs[["temp"]]$StationID[x] & ! StationID %in% IARs[["del"]][[i]]) %>%
    group_by(StationID) %>%
    summarise(longitude = mean(Longitude), latitude = mean(Latitude))
  
  # distances of the other stations to first station
  IARs[["rest"]]$distance <- t(distm(IARs[["station_1"]][2:3], IARs[["rest"]][2:3]))
  
  # sort stations to be aggregated by distance to station 1 in ascending order
  IARs[["rest"]] <- arrange(IARs[["rest"]], distance)
  
  ###
  
  IARs[["biom"]] <- as.numeric(IARs[["temp"]][x, 2:13]) # yearly biomass at 
                                                        # first station
  
  IARs[["Inv"]] <- (mean(IARs[["biom"]])/sd(IARs[["biom"]]))^2 # invariability
                                                               # at first station
  
  IARs[["IAR"]][x, 1] <- IARs[["Inv"]] # save invariability at first station to 
                                       # results
  
  # aggregate remaining stations
  for(y in 2:IARs[["statnumber"]]) {
    
    IARs[["station_n"]] <- IARs[["rest"]]$StationID[y-1] # name of next station 
                                                         # to be aggregated
    
    IARs[["index"]] <- match(IARs[["station_n"]], IARs[["temp"]]$StationID) 
    # index of the next station in the original list
    
    ###
    
    IARs[["biom"]] <- IARs[["biom"]] + as.numeric(IARs[["temp"]][IARs[["index"]], 2:13]) 
    # pool biomass of aggregated stations
    
    IARs[["Inv"]] <- (mean(IARs[["biom"]])/sd(IARs[["biom"]]))^2 
    # invariability of aggregated stations
    
    IARs[["IAR"]][x, y] <- IARs[["Inv"]] # save invariability of aggregated
                                         # station to results
    
  }
}

# calculate IAR medians
for (y in 1:6) {

  IARs[["IARplot"]][(1+(55*(i-1))):(55+(55*(i-1))), 1] <- c(1:55) # A
  
  for(x in 1:IARs[["statnumber"]]) { #I
  
    IARs[["IARplot"]][55*(i-1)+x, 2] <- median(IARs[["IAR"]][,x])
  }
  
  IARs[["IARplot"]][(1+(55*(i-1))):(55+(55*(i-1))), 3] <- rep(IARs[["del_names"]][i], 55) 
  # Stations omitted
  
  }

}

# plot IAR medians (stations omitted)
stat_impact <- ggplot(data = IARs[["IARplot"]]) +
  geom_line(aes(x = A, y = I, colour = factor(`Stations omitted`)), linewidth = 1) +
  labs(title = "Station impact on the IAR median", colour = "Stations omitted",
       y = "Median invariability med(I)", x = "Number of stations A") +
  scale_color_manual(values = site_palette) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

### taxon impacts

## manual color palette

taxon_palette <- c("(g) none" = "black",
                   "(a) Arctica islandica" = "#F8766D",
                   "(b) Mya arenaria" = "#ABA300",
                   "(c) Mytilus edulis" = "#00BE67",
                   "(d) Macoma balthica" = "#00BFC4",
                   "(e) Tridonta borealis" = "#8494FF",
                   "(f) Marenzelleria" = "#FF61CC")

## data preparation

IARs <- list()

# dataframe for results
IARs[["IARplot"]] <- data.frame(
  A <- rep(1:55, 5), I <- rep(NA, 55*5), stat_omit <- rep(NA, 55*5)
)
names(IARs[["IARplot"]]) <- c("A", "I", "Taxon omitted")

# establish with taxa to omit
IARs[["del"]] <- list()
IARs[["del"]][[1]] <- c()
IARs[["del"]][[2]] <- c("Arctica islandica")
IARs[["del"]][[3]] <- c("Mya arenaria")
IARs[["del"]][[4]] <- c("Mytilus edulis")
IARs[["del"]][[5]] <- c("Macoma balthica")
IARs[["del"]][[6]] <- c("Tridonta borealis")
IARs[["del"]][[7]] <- c("Marenzelleria")


IARs[["del_names"]] <- c("(g) none", "(a) Arctica islandica", "(b) Mya arenaria", 
                         "(c) Mytilus edulis", "(d) Macoma balthica", "(e) Tridonta borealis",
                         "(f) Marenzelleria")

## calculate IARs with taxa omitted

for(i in 1:7) {
  
  # data preparation
  
  # data subset with taxa omitted
  IARs[["temp"]] <- subset(zoo_b, ! checked_name %in% IARs[["del"]][[i]]) %>%
    group_by(StationID, checked_name, year) %>%
    summarise(biomass = sum(Final_value))
  
  IARs[["statnumber"]] <- length(unique(IARs[["temp"]]$StationID))
  
  # convert wide format to long format:
   IARs[["temp"]] <- dcast(IARs[["temp"]], StationID + checked_name ~ year, value.var = "biomass") %>%
    mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
    group_by(StationID) %>%
    summarise(`2010` = sum(`2010`), `2011` = sum(`2011`), `2012` = sum(`2012`),
              `2013` = sum(`2013`), `2014` = sum(`2014`), `2015` = sum(`2015`),
              `2016` = sum(`2016`), `2017` = sum(`2017`), `2018` = sum(`2018`),
              `2020` = sum(`2020`), `2021` = sum(`2021`), `2022` = sum(`2022`))
  
  # empty matrix for the results of the IAR
  IARs[["IAR"]] <- matrix(nrow = IARs[["statnumber"]], ncol = IARs[["statnumber"]]) 
  
  # convert to dataframe
  IARs[["IAR"]] <- data.frame(IARs[["IAR"]], row.names = IARs[["temp"]]$StationID)
  
  # calculate the IAR with taxon omitted
  for(x in 1:IARs[["statnumber"]]) {
    
    # first station of the IAR with coordinates
    IARs[["station_1"]] <- subset(zoo_b, StationID == IARs[["temp"]]$StationID[x]) %>%
      group_by(StationID) %>%
      summarise(longitude = mean(Longitude), latitude = mean(Latitude))
    
    # stations to be aggregated with coordinates
    IARs[["rest"]] <- subset(zoo_b, ! StationID == IARs[["temp"]]$StationID[x] & ! StationID %in% IARs[["del"]][[i]]) %>%
      group_by(StationID) %>%
      summarise(longitude = mean(Longitude), latitude = mean(Latitude))
    
    # distances of the other stations to first station
    IARs[["rest"]]$distance <- t(distm(IARs[["station_1"]][2:3], IARs[["rest"]][2:3]))
    
    # sort stations to be aggregated by distance to first station in ascending order
    IARs[["rest"]] <- arrange(IARs[["rest"]], distance)
    
    ###
    
    IARs[["biom"]] <- as.numeric(IARs[["temp"]][x, 2:13]) # yearly biomass at
                                                          # first station
    
    IARs[["Inv"]] <- (mean(IARs[["biom"]])/sd(IARs[["biom"]]))^2 # invariability
                                                                 # at first station
    
    IARs[["IAR"]][x, 1] <- IARs[["Inv"]] # save invariability at first station 
                                         # to results
    
    # aggregate remaining stations
    for(y in 2:IARs[["statnumber"]]) {
      
      IARs[["station_n"]] <- IARs[["rest"]]$StationID[y-1] # name of next station
                                                           # to be aggregated
      
      IARs[["index"]] <- match(IARs[["station_n"]], IARs[["temp"]]$StationID) 
      # index of next station in the original list
      
      ###
      
      IARs[["biom"]] <- IARs[["biom"]] + as.numeric(IARs[["temp"]][IARs[["index"]], 2:13]) 
      # pool biomass of aggregated stations
      
      IARs[["Inv"]] <- (mean(IARs[["biom"]])/sd(IARs[["biom"]]))^2
      # invariability of aggregated stations
      
      IARs[["IAR"]][x, y] <- IARs[["Inv"]] # save invariability of aggregated
                                           # stations to results
      
    }
  }
  
  # calculate IAR medians
  for (y in 1:7) {
    
    IARs[["IARplot"]][(1+(55*(i-1))):(55+(55*(i-1))), 1] <- c(1:55) # A
    
    for(x in 1:IARs[["statnumber"]]) { #I
      
      IARs[["IARplot"]][55*(i-1)+x, 2] <- median(IARs[["IAR"]][,x])
    }
    
    IARs[["IARplot"]][(1+(55*(i-1))):(55+(55*(i-1))), 3] <- rep(IARs[["del_names"]][i], 55) 
    # Stations omitted
    
  }
  
}

# plot IAR medians (taxa omitted)
spec_impact <- ggplot(data = IARs[["IARplot"]]) +
  geom_line(aes(x = A, y = I, colour = factor(`Taxon omitted`)), linewidth = 1) +
  labs(title = "Taxon impact on the IAR median", colour = "Taxon omitted",
       y = "Median invariability med(I)", x = "Number of stations A") +
  scale_color_manual(values = taxon_palette) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

# arrange plots on a grid
grid.arrange(stat_impact, spec_impact, nrow = 1)

# ------------------------stability-distance plot----------------------------

### calculating bray-curtis dissimilarity between stations:

## data preparation:

# total taxon biomass per station
temp <- zoo_b %>%
  group_by(StationID, HELCOM_subbasin, checked_name) %>%
  summarise(biomass = mean(Final_value))

# convert long format to wide format
temp <- dcast(temp, StationID + HELCOM_subbasin ~ checked_name, value.var = "biomass") %>%
  mutate(across(where(is.numeric), ~replace_na(.,0))) # N/A mit 0 ersetzen

# remove the 'sample' column to get only the biomass data
biomass_data <- temp[, 3:ncol(temp)] 
rownames(biomass_data) <- temp$StationID

# calculate Bray-Curtis dissimilarity
bray_curtis_dist <- vegdist(biomass_data, method = "bray")
print(bray_curtis_dist) 

bray_curtis_dist <- melt(as.matrix(bray_curtis_dist), varnames = c("stat1", "stat2"))

### stability distance relation:

## data preparation:

del <- c() # Insert names of stations that should be omitted from the stability-
           # distance plot. Leave vector empty if no stations should be omitted.

# taxon biomass per year per station
temp <- subset(zoo_b, !StationID %in% del) %>%
  group_by(StationID, checked_name, year) %>%
  summarise(biomass = sum(Final_value))

# convert wide format to long format:
temp <- dcast(temp, StationID + checked_name ~ year, value.var = "biomass") %>%
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
  group_by(StationID) %>%
  summarise(`2010` = sum(`2010`), `2011` = sum(`2011`), `2012` = sum(`2012`),
            `2013` = sum(`2013`), `2014` = sum(`2014`), `2015` = sum(`2015`),
            `2016` = sum(`2016`), `2017` = sum(`2017`), `2018` = sum(`2018`),
            `2020` = sum(`2020`), `2021` = sum(`2021`), `2022` = sum(`2022`))

statnumber <- nrow(temp)


### create 3d-array for the results:

stab_dist <- numeric(length = statnumber*(statnumber-1)*4)
stab_dist <- array(stab_dist, dim = c(statnumber-1, 4, statnumber))

dimnames(stab_dist) <- list(c(1:(statnumber-1)), c("Distance", "Stability", 
                                                   "Station b", "Dissimilarity"),
                            temp$StationID)


### calculate stability and distance:

for (x in 1:statnumber) {
  
  station_a <- temp$StationID[x] # station a
  
  rest <- subset(zoo_b, !StationID == station_a & !StationID %in% del) %>%     
    group_by(StationID) %>%
    summarise(latitude = mean(Latitude), longitude = mean(Longitude))
  # coordinates of remaining stations
  
  station_a <- subset(zoo_b, StationID == station_a) %>%     
    group_by(StationID) %>%
    summarise(latitude = mean(Latitude), longitude = mean(Longitude))
  # coordinates of station a 
  
  ###
  
  # coordinates of station a:
  cd_a <- as.numeric(station_a[, 2:3])
  
  # biomass per year at station a:
  a <- as.numeric(temp[x, 2:ncol(temp)])

for (y in 1:(statnumber-1)) {
  
  station_b <- rest$StationID[y] 
  # name of station b
  
  index <- match(station_b, temp$StationID) 
  # index of station b in the original list
  
  ###
  
  # coordinates of station b:
  cd_b <- as.numeric(rest[rest$StationID == station_b, 2:3])
  
  # biomass per year at station b:
  b <- as.numeric(temp[index, 2:ncol(temp)])
  ab <- a + b
  
  # invariance for a and b separate
  Inv_a <- (mean(a)/sd(a))^2
  Inv_b <- (mean(b)/sd(b))^2
  mean_Inv <- mean(c(Inv_a, Inv_b))
  
  # invarianve of a and b pooled
  Inv_ab <- (mean(ab)/sd(ab))^2
  
  # save stabilizing effect to results
  stab_dist[y, "Stability", station_a$StationID ] <- log(Inv_ab/mean_Inv)
  
  # save distance in km to results
  stab_dist[y, "Distance", station_a$StationID ] <- (distm(cd_a, cd_b) / 1000)
  
  # save name of station b to results
  stab_dist[y, "Station b", station_a$StationID] <- station_b
  
  # community dissimilarity between stations a and b
  bray_ab <- subset(
    bray_curtis_dist, stat1 == toString(station_a[1]) & stat2 == station_b)
  stab_dist[y, "Dissimilarity", station_a$StationID] <- as.numeric(bray_ab[3])
  
 }
}

### plot stabilizing effect over distance:

temp <- array2DF(stab_dist) # convert results array to dataframe

stab <- as.numeric(temp$Value[temp$Var2 == "Stability"])
dist <- as.numeric(temp$Value[temp$Var2 == "Distance"])
bray <- as.numeric(temp$Value[temp$Var2 == "Dissimilarity"])

# number of stabilizing effects vs. number of destabilizing effects
length(unique(stab[stab > 0]))                 #stabiliting effects
length(unique(stab[stab < 0]))                 #destabilizing effects

# 2d-Plot using distance as predictive variable
ggplot() +
  geom_point(aes(x = dist, y = stab, col = bray)) +
  scale_color_gradientn(colours = c("blue3","chartreuse4","goldenrod1",
                                    "red2","deeppink"),
                        rescaler = ~ scales::rescale_mid(.x, mid = 0.5))+
  geom_line(aes(x = dist, y = -0.0007833*dist + 0.2957385), 
            color = "black", linetype = 4, linewidth = 1.5) +
  labs(x = "Distance [km]", y = "log(pooled I/mean I)",
       title = "Stability-distance relationship", colour = "Bray-curtis dissimilarity") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

### --------statistical analysis of the stability distance relation:-----------

## stabilizing effect ~ distance
cor.test(dist, stab)                        # pearson correlation test 
cor.test(dist, stab, method = "spearman")   # spearman correlation test 

# scatter plot with smoothing line
scatter.smooth(x = dist, y = stab)

## linear model stabilizing effect ~ distance
linmod <- lm(stab ~ dist)
print(linmod)
summary(linmod)

## bray-curtis dissimilarity - stability relation

# plot stabilizing effect over bray-curtis dissimilarity
ggplot() +
  geom_point(aes(x = bray, y = stab)) +
  labs(x = "Bray-curtis dissimilarity", y = "log(pooled I/mean I)",
       title = "Stability-dissimilarity relation") 

## stabilizing effect ~ bray-curtis dissimilarity
cor.test(bray, stab)                            # pearson correlation test 
cor.test(bray, stab, method = "spearman")       # spearman correlation test 

# plot bray-curtis dissimilarity over distance
ggplot() + 
  geom_point(aes(x = dist, y = bray)) +
  labs(x = "Distance [km]", y = "Bray-curtis dissimilarity",
       title = "Dissimilarity-distance relation") 

## bray curtis-dissimilarity ~ distance
cor.test(dist, bray)                            # pearson correlation test
cor.test(dist, bray, method = "spearman")       # spearman correlation test

### multi-linear regression model 
### stabilizing effect ~ distance + bray-curtis dissimilarity:

multimod <- lm(stab ~ dist + bray)
summary(multimod)

# Which variable is the more significant predictor?
# z-standardized model:

multimod <- lm(scale(stab) ~ scale(dist) + scale(bray))
summary(multimod)

### ------3d-Scatterplot using distance and dissimilarity as predictive variables:-----

multiplot <- plot_ly(x = ~dist, y = ~bray, z = ~stab, type = "scatter3d", mode = "markers",
        color = ~dist, colorscale = "viridis", marker = list(size = 3))

multimod <- lm(stab ~ dist + bray)

#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(dist), max(dist), by = graph_reso)
axis_y <- seq(min(bray), max(bray), by = graph_reso)

#Sample points
stab_lm_surface <- expand.grid(dist = axis_x, bray = axis_y,KEEP.OUT.ATTRS = F)
stab_lm_surface$stab <- predict.lm(multimod, newdata = stab_lm_surface)
stab_lm_surface <- acast(stab_lm_surface, bray ~ dist, value.var = "stab") #y ~ x

multiplot <- add_trace(p = multiplot,
                       z = stab_lm_surface,
                       x = axis_x,
                       y = axis_y,
                       type = "surface")

multiplot

# ----------------------stability-synchrony framework------------------------

### partitioning function "var.partition" by Wang et al. (2019)

var.partition <- function(metacomm_tsdata){
  ## The function "var.partition" performs the partitioning of variability
  ## across hierarchical levesl within a metacommunity.
  ## The input array "metacomm_tsdata" is an N*T*M array. The first dimension represents N species,
  ## the second represents time-series observations of length T, and the third represents M local communities.
  ## The output includes four variability and four synchrony metrics as defined in the main text.
  ## Note that, to be able to handle large metacommunities, this code has avoided calculating all covariance.
  ts_metacom <- apply(metacomm_tsdata,2,sum)
  ts_patch <- apply(metacomm_tsdata,c(2,3),sum)
  ts_species <- apply(metacomm_tsdata,c(1,2),sum)
  sd_metacom <- sd(ts_metacom)
  sd_patch_k <- apply(ts_patch,2,sd)
  sd_species_i <- apply(ts_species,1,sd)
  sd_species_patch_ik <- apply(metacomm_tsdata,c(1,3),sd)
  mean_metacom <- mean(ts_metacom)
  CV_S_L <- sum(sd_species_patch_ik)/mean_metacom
  CV_C_L <- sum(sd_patch_k)/mean_metacom
  CV_S_R <- sum(sd_species_i)/mean_metacom
  CV_C_R <- sd_metacom/mean_metacom
  phi_S_L2R <- CV_S_R/CV_S_L
  phi_C_L2R <- CV_C_R/CV_C_L
  phi_S2C_L <- CV_C_L/CV_S_L
  phi_S2C_R <- CV_C_R/CV_S_R
  partition_3level <- c(CV_S_L=CV_S_L, CV_C_L=CV_C_L, CV_S_R=CV_S_R, CV_C_R=CV_C_R,
                        phi_S_L2R=phi_S_L2R, phi_C_L2R=phi_C_L2R, phi_S2C_L=phi_S2C_L,
                        phi_S2C_R=phi_S2C_R)
  return(partition_3level)
}


### data preparation

del <- c() # Insert names of stations that should be omitted when calculating the
           # stability-synchrony framework. Leave vector empty if no stations 
           # should be omitted.

zoo_b_new <- subset(zoo_b, ! HELCOM_subbasin %in% del)

# taxon biomass per station per year
temp <- zoo_b_new %>%
  group_by(StationID, checked_name, year) %>%
  summarise(biomass = sum(Final_value))

# convert wide format to long format:
temp <- dcast(temp, StationID + checked_name ~ year, value.var = "biomass") %>%
  mutate(across(where(is.numeric), ~replace_na(.,0)))       # replace NA with 0

# insert missing taxa at every station and set their biomass as 0:

taxlist <- unique(zoo_b_new$checked_name)

for(x in 1:length(unique(temp$StationID))) {
  
  station <- unique(temp$StationID)[x] # name of current station
  
  section <- subset(temp, StationID == station) # section within the data set
                                                # pertaining to current station
  
  spec <- taxlist[ ! taxlist %in% section$checked_name] # list of missing taxa 
                                                        # at current station
  
  # section to insert into dataset
  insert <- data.frame(
    StationID = rep(station, length(spec)),
    checked_name = spec,
    matrix(0, length(spec), 12)) %>%
    rename(
      `2010` = X1, `2011` = X2, `2012` = X3, `2013` = X4, `2014` = X5, 
      `2015` = X6, `2016` = X7, `2017` = X8, `2018` = X9, `2020` = X10,
      `2021` = X11, `2022` = X12
    )
  
  # insert missing taxa of current station into data set
  temp <- rbind(temp, insert)
}

# sort data by StationID
temp <- temp %>% arrange(StationID)

# convert dataframe to 3d array
arr <- abind(
  split(temp[,3:ncol(temp)], temp$StationID), 
  along = 3)

### calculate stability-synchrony framework using var.partition:

sync_var_spec <- var.partition(arr)

sync_var_spec

# -----------------------cluster-analysis--------------------------

### data preparation:

# total taxon biomass per station
temp <- zoo_b %>%
  group_by(StationID, HELCOM_subbasin, checked_name) %>%
  summarise(biomass = mean(Final_value))

# convert long format to wide format
temp <- dcast(temp, StationID + HELCOM_subbasin ~ checked_name, value.var = "biomass") %>%
  mutate(across(where(is.numeric), ~replace_na(.,0))) # N/A mit 0 ersetzen

# remove the 'sample' column to get only the biomass data
biomass_data <- temp[, 3:ncol(temp)] 
rownames(biomass_data) <- temp$StationID

# calculate bray-curtis dissimilarity
bray_curtis_dist <- vegdist(biomass_data, method = "bray")
print(bray_curtis_dist) 

# cluster-dendrogram
dend <- hclust(bray_curtis_dist, method = "ward.D")

plot(dend, hang = -1)

# nmds plot
nmds <- metaMDS(bray_curtis_dist, k = 2)

# manual color scheme according to subbasin
subbasin <- temp$HELCOM_subbasin
cols <- c()

for(x in 1:length(subbasin)) {
  colx <- switch(
    subbasin[x],
    "Arkona Basin" = "tomato1",
    "Bay of Mecklenburg" = "goldenrod3",
    "Bornholm Basin" = "forestgreen",
    "Eastern Gotland Basin" = "cyan3",
    "Gdansk Basin" = "dodgerblue",
    "Western Gotland Basin" = "magenta1"
  )
  cols <- c(cols, colx)
}

## plot nmds-plot:

# stressplot
stressplot(nmds)

# simple nmds-plot no color scheme
plot(nmds, main = "NMDS-Plot")

# build nmds-plot with color scheme, station names and legend
ordiplot(nmds, type = "n", main = "NMDS-Plot")      
orditorp(nmds, display="sites", col = cols, cex=0.6, air=0.01)   # site names and
                                                                 # colors
legend('topright', legend = unique(subbasin), col = unique(cols),   
       pch = 16, cex = 0.7)                                      #legend

# optional: add polygons to nmds-plot
ordihull(nmds, groups = subbasin, draw = "polygon", col = "grey90", label = F)

