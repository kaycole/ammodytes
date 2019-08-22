
# This is the combined plots for 
# Marine mammal, bird, and fish diet literature


#--------------#
# load packages
#--------------#
require(dplyr)
require(ggplot2)
library(readxl)
require(tidyverse) #may need to use devtools::install_github("hadley/tidyverse") depending on version if not installed yet
#--------------#


#--------------#
# load data
#--------------#
# data is downloaded from google drive full_manuscript/all_tables
dir.in = "C:/Users/kcoleman/Downloads"
#dir.in = "~/Downloads/"
birds <- read_excel(paste(dir.in, "All_Tables.xlsx",sep="/"), sheet = 8, skip = 1)
fish <- read_excel(paste(dir.in, "All_Tables.xlsx",sep="/"), sheet = 7, skip = 1)
mm <- read_excel(paste(dir.in, "All_Tables.xlsx",sep="/"), sheet = 9, skip = 1)
#--------------#


#--------------#
# format data
#--------------#
# fix bird table
adult_birds = birds[3:18,]
chick_birds = birds[21:76,]
combo_birds = birds[79:88,]
birds = rbind(adult_birds, chick_birds, combo_birds)
rm(adult_birds, chick_birds, combo_birds)

# pull out data we need and rename and filter
birds = birds[,c(1,3,4,5)]
names(birds) = c("species","loc","diet","metric")

fish = fish[,c(1,7,4,5)]
names(fish) = c("species","loc","diet","metric")
fish = filter(fish, !diet %in% "P")

mm = mm[,c(2,8,5,6)]
names(mm) = c("species","loc","diet","metric")
mm = filter(mm, !diet %in% "Present")

#### change diet to numeric
mm = mm %>% mutate(diet = as.numeric(diet))

# fix fish
fish = fish %>% 
  mutate(diet = replace(diet, diet %in% "22-52", (22+52)/2),
         diet = replace(diet, diet %in% "3-17", (3+17)/2),
         diet = replace(diet, diet %in% "1-49", (1+49)/2),
         diet = replace(diet, diet %in% "85.4-93.5", (85.4+93.5)/2),
         diet = replace(diet, diet %in% "<1", 1),
         diet = replace(diet, diet %in% "<10", 10))
to.break = fish[grep(",",fish$diet),]
to.leave = fish[!fish$diet %in% fish$diet[grep(",",fish$diet)],] %>% mutate(diet = as.numeric(diet))

to.break1 = to.break %>% mutate(diet = sapply(strsplit(diet,","),head,1))
to.break2 = to.break %>% mutate(diet = sapply(strsplit(diet,","),tail,1))
to.break3 = to.break[c(4,5,9),] %>% mutate(diet = sapply(strsplit(diet,","),tail,2)[1,])
to.break = rbind(to.break1, to.break2, to.break3)
rm(to.break1, to.break2, to.break3)
to.break = mutate(to.break, 
                  metric = sapply(strsplit(diet,"[(]"), tail, 1),
                  metric = sapply(strsplit(metric,"[)]"), head, 1),
                  diet = sapply(strsplit(diet,"[(]"), head, 1),
                  diet = replace(diet, diet %in% c("<1 "," <1 "), 1),
                  diet = replace(diet, diet %in% "27-31 ", (27+31)/2),
                  diet = replace(diet, diet %in% "4-42 ", (4+42)/2),
                  diet = replace(diet, diet %in% "21-56 ", (21+56)/2),
                  diet = replace(diet, diet %in% "33-82 ", (33+82)/2),
                  diet = replace(diet, diet %in% " 3-80 ", (3+80)/2),
                  diet = replace(diet, diet %in% " 51-57 ", (51+57)/2),
                  diet = replace(diet, diet %in% " 2-34 ", (2+34)/2),
                  diet = replace(diet, diet %in% " 23-30 ", (23+30)/2),
                  diet = replace(diet, diet %in% " 49-60 ", (49+60)/2),
                  diet = replace(diet, diet %in% " 59-65 ", (59+65)/2),
                  diet = replace(diet, diet %in% " 4-50 ", (4+50)/2),
                  diet = replace(diet, diet %in% "2-69 ", (2+69)/2),
                  diet = replace(diet, diet %in% " 7-13 ", (7+13)/2),
                  diet = as.numeric(diet))    
fish = rbind(to.break, to.leave)
rm(to.break, to.leave)

# fix birds
birds = birds %>% 
  mutate(diet = replace(diet, diet %in% "43.7-67.0", (43.7+67.0)/2),
         diet = replace(diet, diet %in% "25.5-72.6", (25.5+72.6)/2),
         diet = replace(diet, diet %in% "2-21", (2+21)/2),
         diet = replace(diet, diet %in% "8-18", (8+18)/2),
         diet = replace(diet, diet %in% "25.5-72.6", (25.5+72.6)/2),
         diet = replace(diet, diet %in% "8.0-39.0", (8.0+39.0)/2),
         diet = replace(diet, diet %in% "2-3", (2+3)/2),
         diet = replace(diet, diet %in% "1-3", (1+3)/2),
         diet = replace(diet, diet %in% "4.0-69.2", (4.0+69.2)/2),
         diet = replace(diet, diet %in% "0-47", (0+47)/2)) #check with Michelle about space

to.break = birds[c(grep(",",birds$diet),grep(";",birds$diet)),]
to.leave = birds[!birds$diet %in% birds$diet[c(grep(",",birds$diet),grep(";",birds$diet))],] %>% 
  mutate(diet = sapply(strsplit(diet,"±"),head,1),
         diet = replace(diet, diet %in% "36.8 3 ",36.83),
         diet = replace(diet, diet %in% "73.75 3 ",73.753),
         diet = as.numeric(diet))

to.break1 = to.break[1:5,] %>% mutate(diet = sapply(strsplit(diet,","),head,1))
to.break2 = to.break[1:5,] %>% mutate(diet = sapply(strsplit(diet,","),tail,1))
to.break3 = to.break[6:8,] %>% mutate(diet = sapply(strsplit(diet,";"),head,1))
to.break4 = to.break[6:8,] %>% mutate(diet = sapply(strsplit(diet,";"),tail,1))
to.break = rbind(to.break1, to.break2, to.break3, to.break4)
rm(to.break1, to.break2, to.break3, to.break4)

to.break = mutate(to.break, 
                  metric = sapply(strsplit(diet,"[(]"), tail, 1),
                  metric = sapply(strsplit(metric,"[)]"), head, 1),
                  diet = sapply(strsplit(diet,"[(]"), head, 1),
                  diet = sapply(strsplit(diet,"±"),head,1),
                  diet = as.numeric(diet))
birds = rbind(to.break, to.leave)
rm(to.break, to.leave)

##### combine all data
fish$type = "Fish"
birds$type = "Birds"
mm$type = "Marine Mammals"
all_data = bind_rows(birds, fish, mm)

# change location to only first few letters to be the same (if there is a comma)
#all_data = mutate(all_data, loc = replace(loc, grep(loc, ","), sapply(strsplit(loc, ","), head, 1)))
all_data = mutate(all_data, 
                  loc = replace(loc, loc %in% c("\nSmall Island, Newfoundland, Canada",
                                                "Baccalieu Island, Newfoundland, Canada",
                                                "Funk Island, Newfoundland, Canada",
                                                "Small Island, Newfoundland, Canada",
                                                "Great Island, Newfoundland, Canada",
                                                "Newfoundland, Canada",
                                                "Gull Island, Labrador, Canada",
                                                "Gannet Islands, Labrador, Canada",
                                                "Labrador, Newfoundland",
                                                "Newfoundland and Labrador, Canada",
                                                "Newfoundland, Labrador, Canada",
                                                "Newfoundland and Labrador"), 
                                "Newfoundland and Labrador"),
                  loc = replace(loc, loc %in% c("Bay of Fundy, Canada",
                                                "Bonaventure Island, Bay of Fundy, Canada"), 
                                "Bay of Fundy"),
                  loc = replace(loc, loc %in% c("Country Island, Nova Scotia, Canada",
                                                "Nova Scotia, Canada",
                                                "ScS","ScS, Bay of Fundy, Canada",
                                                "Nova Scotia"), 
                                "Nova Scotia and Scotian Shelf" ),
                  loc = replace(loc, loc %in% c("Brandypot and St Mary's Islands, Gulf of St. Lawrence, Quebec, Canada",
                                                "Gulf of St. Lawrence, Canada",
                                                "Gulf St. Lawrence, Canada",
                                                
                                                "Gulf of St. Lawrence, Quebec, Canada"), 
                                "Gulf of St. Lawrence"),
                  loc = replace(loc, loc %in% "Hudson Bay, Canada", 
                                "Hudson Bay"),
                  loc = replace(loc, loc %in% c("Stratton, Jenny, Eastern Egg Rock, Seal Islands, Gulf of Maine, USA",
                                                "Seal Island, Matinicus\nRock, Eastern Egg Rock Islands, Gulf of Maine, USA",
                                                "Maine, USA",
                                                "GoM, Saco River Estuary",
                                                "GoM",
                                                "GoM, Stellwagen Bank"), 
                                "Gulf of Maine"),
                  loc = replace(loc, loc %in% c("SNE","Massachusetts, USA",
                                                "Penikese, Bird, and Ram Islands, Massachusetts, USA",
                                                "New England",
                                                "Connecticut, USA"), 
                                "Southern New England"),
                  loc = replace(loc, loc %in% c("New York, USA","MAB"),
                                "Mid-Atlantic Bight"),
                  loc = replace(loc, loc %in% c("Grand Banks, Canada"), 
                                "Grand Banks"),
                  loc = replace(loc, loc %in% c("GB"), 
                                "Georges Banks"),
                  loc = replace(loc, loc %in% c("Eastern Canada",
                                                "Grand Banks, Labrador, Newfoundland",
                                                "Akpatok Island, Baffin, Canada",
                                                "Quebec, Canada",
                                                "New Brunswick, Canada",
                                                "GoM, GB",
                                                "SNE, MAB"), 
                                "North Atlantic"),
                  loc = replace(loc, loc %in% "Nunavut, Canada",
                                "Canadian Arctic"))

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

all_data = mutate(all_data, species = sapply(species, simpleCap),
                  species = replace(species, species %in% "Roseate Tern (kleptoparasitic)", "Roseate Tern"),
                  metric = replace(metric, is.na(metric), "Not described"),
                  loc = replace(loc, is.na(loc), "North Atlantic"),
                  metric = replace(metric, metric %in% "otoliths", "FO"))

# add order to spacial
all_data$loc = factor(all_data$loc, levels = c("Canadian Arctic","Hudson Bay","Greenland",
                                               "Newfoundland and Labrador","Gulf of St. Lawrence",
                                               "Grand Banks","Nova Scotia and Scotian Shelf",
                                               "Bay of Fundy","Gulf of Maine","Georges Banks",
                                               "Southern New England","Mid-Atlantic Bight",
                                               "North Atlantic"))
#--------------#


#--------------#
# plot
#--------------#
# p = ggplot()+
#   geom_point(data = all_data, aes(x = reorder(species,diet,na.rm=TRUE), y = diet, shape = metric, col = loc), 
#              size=5, lwd=5)+
#   coord_flip()+
#   labs(x="Species", y="Percent of sand lance in diet")+
#   theme(text = element_text(size=20))+
#   scale_shape_manual(values=1:10) +
#   theme_bw()#+
# #theme(legend.position = "none")
# p 
# ggsave(paste(dir.in, "Fig8_option1_all_together.png", sep="/"), p)

p = ggplot()+
  geom_point(data = all_data, aes(x = reorder(species,diet,na.rm=TRUE), y = diet, shape = metric, col = loc), 
             size=4)+
  coord_flip()+
  labs(x="Species", y="Percent of sand lance in diet",
       col = "Location of Study", shape = "Diet Metric")+
  theme(text = element_text(size=20))+
  scale_shape_manual(values=1:11) +
  facet_grid(type~., scales = "free", space = "free")+
  theme_bw()#+
p 
ggsave(paste(dir.in, "Fig8_facet_grid.png", sep="/"), p)
#--------------#
