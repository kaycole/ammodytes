
# This is the combined plots for 
# Marine mammal, bird, and fish diet literature


#--------------#
# load packages
#--------------#
require(dplyr)
require(ggplot2)
library(readxl)
#--------------#


#--------------#
# load data
#--------------#
# data is downloaded from google drive full_manuscript/all_tables
dir.in = "C:/Users/kcoleman/Downloads"
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
all_data = mutate(all_data, loc = replace(loc, grep(loc, ","), sapply(strsplit(loc, ","), head, 1)))
#--------------#


#--------------#
# plot
#--------------#
ggplot()+
  geom_point(data = all_data, aes(x = species, y = diet, shape = metric, col = loc))+
  coord_flip()+
  facet_wrap(~type, ncol=1)+
  theme_bw()+
  theme(legend.position = "none")
#--------------#
