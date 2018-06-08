# -------------------- #
# marine mammals diet data from review
# https://docs.google.com/spreadsheets/d/16jHUG_2HJPysvlgs0VULyLlg57S83CzixJ4lLH97hJM/edit#gid=0
# -------------------- #

# -------------------- #
# load packages
# -------------------- #
library(readxl)
require(dplyr)
# -------------------- #


# -------------------- #
# load data
# -------------------- #
diets <- read_excel("~/SL/marine_mammals/marine_mammal_diets.xlsx")
diets = diets[,1:14] # it read it the NA columns
names(diets)=c("species","ref","location","detailedlocation","dietMetric",
               "percentDiet","variation","age",
               "season","dates","spp","preySize","altEnergy","comments")
diets = filter(diets, !is.na(species)) %>% 
  mutate(percentDiet=as.numeric(percentDiet),
         location = replace(location, location %in% c("ME","New England ","New England",
                                                      "Stellwagen Bank","GOM"), "Gulf of Maine"),
         location = replace(location, location %in% c("Nova Scotia","Nova Scotia, Scotian Shelf"), "Scotian Shelf"),
         location = replace(location, location %in% c("Labrador, Newfoundland","Grand Banks, Labrador, Newfoundland"), "Newfoundland and Labrador"),
         location = replace(location, location %in% c("Gulf of St Lawrence "), "Gulf of St. Lawrence"),
         location = replace(location, location %in% c(),"Northwest Atlantic"),
         new.var = NA,
         new.var = replace(new.var, location %in% "Mid-Atlantic/ Southern New England",1),
         new.var = replace(new.var, location %in% "Gulf of Maine",2),
         new.var = replace(new.var, location %in% "George's Bank",3),
         new.var = replace(new.var, location %in% "Scotian Shelf",4),
         new.var = replace(new.var, location %in% "Gulf of St. Lawrence",5),
         new.var = replace(new.var, location %in% "Newfoundland and Labrador",6),
         new.var = replace(new.var, location %in% "Eastern Canada",7),
         new.var = replace(new.var, location %in% "Hudson Bay",8),
         new.var = replace(new.var, location %in% "Canadian Arctic",9),
         new.var = replace(new.var, location %in% "Greenland",10),
         new.var = replace(new.var, location %in% "Northwest Atlantic",11),
         method = NA,
         method = replace(method, dietMetric %in% "W","Mass"),
         method = replace(method, dietMetric %in% c("O","P"),"Frequency of Occurrence"),
         method = replace(method, dietMetric %in% NA, "Unidentifed"))
# -------------------- #


# -------------------- #
# plot
# -------------------- #
# means with color = location, shape = metric
cbPalette <- c("#CC79A7", 
  "#D55E00", "#E69F00", 
  "#F0E442", 
  "#009E73", "#56B4E9",   
  "#0072B2", 
  "darkmagenta", 
  "#000000",
  "#999999","teal")
p = ggplot()+
  geom_point(data = filter(diets,!is.na(percentDiet)), 
             aes(y = percentDiet, x = reorder(species,percentDiet,na.rm=TRUE), 
                 col = reorder(location,new.var) 
                 shape = method),
             stroke=2)+
  coord_flip()+
  theme_bw()+
  ggtitle('Proportion of ammodytes in diet from the literature review')+
  ylab('mean proportion of ammodytes in diet')+
  xlab('species')+ 
  guides(col=guide_legend(title="location"),
         shape=guide_legend(title="Diet Metric (%)"))+
  scale_colour_manual(values=cbPalette)
p
ggsave(file = "~/SL/marine_mammals/prop_ammo_in_lit.png",plot=p)  
