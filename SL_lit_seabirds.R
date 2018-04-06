# -------------------- #
# seabird diet data from review
# https://docs.google.com/spreadsheets/d/1n-Ji8tZa_N5fLWGRqUrB-sTi-33npr4P3YrtGi3xiZY/edit#gid=0
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
diets <- read_excel("~/SL/seabirds/NWAtlantic_seabird_diets.xlsx", skip=1)
# -------------------- #


# -------------------- #
# formatting
# -------------------- #
names(diets)<-c("Species",names(diets)[2:6],"Av","method",names(diets)[9:16],"comments") # spaces after name
diets = diets %>% mutate(Species = tolower(Species),
                         Av = replace(Av, Av %in% "bulk of diet", NA),
                         Av = as.numeric(Av),
                         Species = replace(Species, Species %in% "alcidae - uriu, aka and cepphus", "alcids"),
                         Species = replace(Species, Species %in% "roseate tern (kleptoparasitic)","roseate tern"),
                         Location = replace(Location, Location %in% c("ME"), "Gulf of Maine"),
                         Location = replace(Location, Location %in% c("MA","NY","VA","CT","Fire Island Inlet, NY"), "Mid-Atlantic/ Southern New England"),
                         Location = replace(Location, Location %in% c("Bay of Fundy","Country Island, Nova Scotia",
                                                                      "eastern Nova Scotia","Nova Scotia, Canada",
                                                                      "Nova Scotia" ), "Scotian Shelf"),
                         Location = replace(Location, Location %in% c("Gannet Islands Labrador","Gannet Islands, Labrador",
                                                                      "Newfoundland","Newfoundland, Labrador",
                                                                      "Labrador"), "Newfoundland and Labrador"),
                         Location = replace(Location, Location %in% c("Churchill, Canada","Nunavut, Canada"),"Hudson Bay"),
                         Location = replace(Location, Location %in% c("Bonaventure Island","Gulf St Lawrence","NB",
                                                                      "Quebec","Quebec      ","Quebec, Gulf of St Lawrence"), "Gulf of St. Lawrence"),
                         Location = replace(Location, Location %in% c("Georges Bank, SW Gulf of Maine",
                                                                      "N. Atlantic","northern hemisphere?",NA),"Northwest Atlantic"),
                         Location = replace(Location, Location %in% c("eastern Canadia"),"Eastern Canada"),
                         new.var = NA,
                         new.var = replace(new.var, Location %in% "Mid-Atlantic/ Southern New England",1),
                         new.var = replace(new.var, Location %in% "Gulf of Maine",2),
                         new.var = replace(new.var, Location %in% "George's Bank",3),
                         new.var = replace(new.var, Location %in% "Scotian Shelf",4),
                         new.var = replace(new.var, Location %in% "Gulf of St. Lawrence",5),
                         new.var = replace(new.var, Location %in% "Newfoundland and Labrador",6),
                         new.var = replace(new.var, Location %in% "Eastern Canada",7),
                         new.var = replace(new.var, Location %in% "Hudson Bay",8),
                         new.var = replace(new.var, Location %in% "Canadian Arctic",9),
                         new.var = replace(new.var, Location %in% "Northwest Atlantic",10),
                         method = replace(method, method %in% c("W","M"),"Mass"),
                         method = replace(method, method %in% "N","Number"),
                         method = replace(method, method %in% "OF","Frequency of Occurrence"),
                         method = replace(method, method %in% NA, "Unidentifed"))

sum.diets = diets %>% group_by(Species) %>% 
  summarize(av = mean(Av,na.rm=TRUE),
            sd = sd(Av,na.rm=TRUE)) 

# -------------------- #


# -------------------- #
# plot
# -------------------- #
# means with color = location, shape = metric
cbPalette <- c(#"#CC79A7", 
  "#D55E00", "#E69F00", 
               #"#F0E442", 
  "#009E73", "#56B4E9",   
               #"#0072B2", 
  "darkmagenta", 
               "#000000",
  "#999999")
p = ggplot()+
  geom_point(data = filter(diets,!is.na(Av)), 
           aes(y = Av, x = reorder(Species,Av,na.rm=TRUE), 
               col = reorder(Location,new.var), 
               shape = method),
           stroke=2)+
  coord_flip()+
  theme_bw()+
  ggtitle('Proportion of ammodytes in diet from the literature review')+
  ylab('mean proportion of ammodytes in diet')+
  xlab('species')+ 
  guides(col=guide_legend(title="Location"),
         shape=guide_legend(title="Diet Metric (%)"))+
  scale_colour_manual(values=cbPalette)
p
ggsave(file = "~/SL/seabirds/prop_ammo_in_lit.png",plot=p)  
