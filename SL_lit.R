# ------------ #
# SL lit review plot
# ------------ #


# ------------ #
# load packages
# ------------ #
library(readxl)
require(dplyr)
require(ggplot2)
# ------------ #


# ------------ #
# load data
# ------------ #
sl <- read_excel("~/SL/Sandlance_predator_Litsearch_3.15.18.xlsx", 
                 sheet = "data table",col_names = TRUE, skip=1)
names(sl) = c("comname","lesser","mid","greater","diet.metric","prey.size","location.region",
              "location.local","years","seasons","ref","notes")
sl = as.data.frame(sl[1:(which(is.na(sl$comname))[1]-1),1:12]) # issues with NA column names and rows
sl[sl=="N/A"] = NA

# add record number before spitting
sl$id = seq(1:length(sl$comname))
  
# break rows with more than one diet metric
# scup
to.add = sl[sl$comname %in% "Scup",]
to.add = mutate(to.add, seasons = "annual", mid=NA)
sl = mutate(sl, seasons = ifelse(comname %in% "Scup","summer",seasons),
            lesser = ifelse(comname %in% "Scup",NA,lesser)) %>%
  rbind(.,to.add)
rm(to.add)
# Northern shortfin squid
to.add = sl[sl$comname %in% "Northern shortfin squid",]
to.add = mutate(to.add, mid=49, lesser=NA)
sl = mutate(sl, lesser = ifelse(comname %in% "Northern shortfin squid",1,lesser),
            mid = ifelse(comname %in% "Northern shortfin squid",NA,mid)) %>%
  rbind(.,to.add)
rm(to.add)
# lesser +/-
to.add = sl[grep("+/-",sl$lesser),]
to.add1 = to.add %>% mutate(lesser = abs(apply(cbind(as.numeric(sapply(strsplit(lesser,"+/-",fixed=TRUE),head,1)), 
                                                 as.numeric(sapply(strsplit(lesser,"+/-",fixed=TRUE),tail,1))), 1, sum)))
to.add2 = to.add %>% mutate(lesser = abs(apply(cbind(as.numeric(sapply(strsplit(lesser,"+/-",fixed=TRUE),head,1)), 
                                                 as.numeric(sapply(strsplit(lesser,"+/-",fixed=TRUE),tail,1))), 1, diff)))
sl = rbind(sl,to.add1,to.add2)
rm(to.add,to.add1,to.add2)
# mid +/-
to.add = sl[grep("+/-",sl$mid),]
to.add1 = to.add %>% mutate(mid = abs(apply(cbind(as.numeric(sapply(strsplit(mid,"+/-",fixed=TRUE),head,1)), 
                                                 as.numeric(sapply(strsplit(mid,"+/-",fixed=TRUE),tail,1))), 1, sum)))
to.add2 = to.add %>% mutate(mid = abs(apply(cbind(as.numeric(sapply(strsplit(mid,"+/-",fixed=TRUE),head,1)), 
                                                 as.numeric(sapply(strsplit(mid,"+/-",fixed=TRUE),tail,1))), 1, diff)))
sl = rbind(sl,to.add1,to.add2)
rm(to.add,to.add1,to.add2)
# greater +/-
to.add = sl[grep("+/-",sl$greater),]
to.add1 = to.add %>% mutate(greater = abs(apply(cbind(as.numeric(sapply(strsplit(greater,"+/-",fixed=TRUE),head,1)), 
                                              as.numeric(sapply(strsplit(greater,"+/-",fixed=TRUE),tail,1))), 1, sum)))
to.add2 = to.add %>% mutate(greater = abs(apply(cbind(as.numeric(sapply(strsplit(greater,"+/-",fixed=TRUE),head,1)), 
                                              as.numeric(sapply(strsplit(greater,"+/-",fixed=TRUE),tail,1))), 1, diff)))
sl = rbind(sl,to.add1,to.add2)
rm(to.add,to.add1,to.add2)
# mutate
sl = mutate(sl,
            lesser = gsub("<","",lesser),
            lesser = gsub("% (annual)","",lesser,fixed=TRUE),
            mid = gsub("% (summer)","",mid,fixed=TRUE),
            lesser = sapply(strsplit(lesser,"+/-",fixed=TRUE),head,1),
            greater = sapply(strsplit(greater,"+/-",fixed=TRUE),head,1),
            mid = sapply(strsplit(mid,"+/-",fixed=TRUE),head,1))
# sturgeon
to.add = sl[grep("-",sl$greater),]
to.add = mutate(to.add, greater = as.numeric(sapply(strsplit(greater,"-"),tail,1)))
sl = mutate(sl, greater = as.numeric(sapply(strsplit(greater,"-"),head,1))) %>%
  rbind(.,to.add)
rm(to.add)
 
# format
sl = mutate(sl, diet = NA, 
            sciname = comname,
            sciname = replace(sciname, sciname %in% c("American plaice","american plaice"),"Pleuronectes platessa"),  
            sciname = replace(sciname, sciname %in% c("Atlantic cod","atlantic cod","atlantic cod "),"Gadus morhua"),         
            sciname = replace(sciname, sciname %in% c("Atlantic sturgeon (adult)","Atlantic sturgeon (juvenile)"),"Acipenser oxyrhynchus oxyrhynchus"),
            sciname = replace(sciname, sciname %in% "Black sea bass","Centropristis striata"),               
            sciname = replace(sciname, sciname %in% c("Bluefin tuna","Bluefin tuna (<185cm)","Bluefin tuna (>185cm)","Bluefin tuna age 1-3","Bluefin tuna age 4-5"),"Thunnus thynnus"),
            sciname = replace(sciname, sciname %in% "Bluefish","Pomatomus saltatrix"), 
            sciname = replace(sciname, sciname %in% "Goosefish","Lophiidae"),        
            sciname = replace(sciname, sciname %in% "Little Skate","Leucoraja erinacea"),
            sciname = replace(sciname, sciname %in% "Northern shortfin squid","Illex illecebrosus"),    
            sciname = replace(sciname, sciname %in% "Offshore hake","Merluccius albidus"), 
            sciname = replace(sciname, sciname %in% "Pollock","Pollachius"),  
            sciname = replace(sciname, sciname %in% "Red hake","Urophycis chuss"), 
            sciname = replace(sciname, sciname %in% "Scup","Stenotomus chrysops"), 
            sciname = replace(sciname, sciname %in% "Silver hake","Merluccius bilinearis"),
            sciname = replace(sciname, sciname %in% "Smooth Dogfish","Mustelus canis"),    
            sciname = replace(sciname, sciname %in% "Summer Flounder","Paralichthys dentatus"), 
            sciname = replace(sciname, sciname %in% "Weakfish","Cynoscion regalis"),           
            sciname = replace(sciname, sciname %in% "White hake","Urophycis tenuis"),  
            sciname = replace(sciname, sciname %in% "Winter Skate","Leucoraja ocellata"),
            sciname = replace(sciname, sciname %in% "Yellowfin tuna","Thunnus albacares"),
            sciname = replace(sciname, sciname %in% c("Yellowtail flounder","yellowtail flounder"),"Pleuronectes ferruginea"),
            sciname = replace(sciname, sciname %in% "haddock","Melanogrammus aeglefinus"),
            sciname = replace(sciname, sciname %in% c("Atlantic salmon","atlantic salmon"),"Salmo salar"),
            sciname = replace(sciname, sciname %in% "silver hake","Merluccius bilinearis"),
            sciname = replace(sciname, sciname %in% "Arctic charr","Salvelinus alpinus"),
            sciname = replace(sciname, sciname %in% c("atlantic sea herring","herring"),"Clupea harengus"),
            sciname = replace(sciname, sciname %in% "atlantic mackerel ","Scomber scombrus"),
            sciname = replace(sciname, sciname %in% "Greenland halibut","Reinhardtius hippoglossoides"),
            lesser = as.numeric(lesser),
            mid= as.numeric(mid),
            greater=as.numeric(greater),
            diet = apply(cbind(lesser, mid, greater), 1, mean, na.rm=TRUE),
            diet.metric = replace(diet.metric, diet.metric %in% "?","undefined"),
            # location.region = replace(location.region, location.region %in% c("Gulf of Maine","NWA/ GOM ","NWA/ GOM"), "GOM"),
            # location.region = replace(location.region, location.region %in% c("NJ Coast","NWA/MAB"), "MAB"),
            # location.region = replace(location.region, location.region %in% c("NWA/ SNE","Southern NE"), "SNE"),
            # location.region = replace(location.region, location.region %in% c("Nova Scotian Shelf","Scotian Shelf","Nova Scotia"), "ScS"),
            # location.region = replace(location.region, location.region %in% c("Newfoundland","newfoundland","Newfoundland and Labrador "), "NFL"),
            # location.region = replace(location.region, location.region %in% c("Grand Bank","Grand Banks"), "GrB"), 
            # location.region = replace(location.region, location.region %in% "Gulf of St. Lawrence", "GSL"),
            # location.region = replace(location.region, location.region %in% "western greenland", "WG"),
            # location.region = replace(location.region, location.region %in% c("NWA/ GOM and GB","NWA/ GOM and SNE"),"NWA"),
            location.region = replace(location.region, location.region %in% c("NWA/ GOM ","NWA/ GOM"), "Gulf of Maine"),
            location.region = replace(location.region, location.region %in% c("NJ Coast","NWA/MAB"), "Mid-Atlantic Bight"),
            location.region = replace(location.region, location.region %in% c("NWA/ SNE","Southern NE"), "Southern New England"),
            location.region = replace(location.region, location.region %in% c("Nova Scotian Shelf","Nova Scotia"), "Scotian Shelf"),
            location.region = replace(location.region, location.region %in% c("newfoundland","Newfoundland",
                                                                              "Newfoundland and Labrador ","Labrador "), "Newfoundland and Labrador"),
            location.region = replace(location.region, location.region %in% c("Grand Bank"), "Grand Banks"), 
            location.region = replace(location.region, location.region %in% "western greenland", "Western Greenland"),
            location.region = replace(location.region, location.region %in% c("NWA/ GOM and GB","NWA/ GOM and SNE","NWA"),"Northwest Atlantic"),
            diet.metric = replace(diet.metric, diet.metric %in% c("O","otoliths"),"FO"),
            diet.metric = replace(diet.metric, diet.metric %in% "C:N",NA),
            diet.metric.formal = diet.metric,
            diet.metric.formal = replace(diet.metric.formal,diet.metric.formal %in% "FO","Frequency of occurrence"),
            diet.metric.formal = replace(diet.metric.formal,diet.metric.formal %in% "IRI","Index of relative importance"),
            diet.metric.formal = replace(diet.metric.formal,diet.metric.formal %in% "M","Mass"),
            diet.metric.formal = replace(diet.metric.formal,diet.metric.formal %in% "N","Number"),
            diet.metric.formal = replace(diet.metric.formal,diet.metric.formal %in% "V","Volume"),
            diet.metric.formal = replace(diet.metric.formal,diet.metric.formal %in% "W","Weight")) #,
            #location.region2 = location.region,
            #location.region2 = replace(location.region2, location.region2 %in% c("GSL","GrB","ScS"),"Canada"))
            #diet.metric = replace(diet.metric, diet.metric %in% "otoliths","O"))
# indicate if the study measured %mass/volume, % frequency of occurrence, or %number, 
# %Index of Relative Importance (IRI) 
# M = mass
# V = volume
# W = weight
# FO = frequency of occurrence
# IRI = Index of relative importance
# N = number?
# C:N = carbon to nitrogren isotopes
# otoliths = might also be chemistry like C:N
# O = occurrence?



mean.sl.by.paper = sl %>% group_by(sciname,id) %>% 
  summarize(mean.diet = mean(diet, na.rm=TRUE),
            sd = sd(diet, na.rm=TRUE),
            n = n(),
            diet.metric = first(diet.metric),
            diet.metric.formal = first(diet.metric.formal),
            location.region = first(location.region)) %>%
  mutate(error = qnorm(0.95)*sd/sqrt(n),
         new.var = NA,
         new.var = replace(new.var, location.region %in% "Mid-Atlantic Bight",1),
         new.var = replace(new.var, location.region %in% "Southern New England",2),
         new.var = replace(new.var, location.region %in% "Gulf of Maine",3),
         new.var = replace(new.var, location.region %in% "Georges Bank",4),
         new.var = replace(new.var, location.region %in% "Scotian Shelf",5),
         new.var = replace(new.var, location.region %in% "Gulf of St. Lawrence",6),
         new.var = replace(new.var, location.region %in% "Grand Banks",7),
         new.var = replace(new.var, location.region %in% "Newfoundland and Labrador",8),
         new.var = replace(new.var, location.region %in% "Western Greenland",9),
         new.var = replace(new.var, location.region %in% "Northwest Atlantic",10)) %>% 
  arrange(new.var)

#mean.sl = sl %>% group_by(sciname) %>% summarize(mean.diet = mean(diet, na.rm=TRUE))

#summer - june, july, aug
#spring - march, april, may
#fall - sept, oct, nov
#winter - dec, jan, feb
sl = sl %>% mutate(seasons = tolower(seasons),
                   seasons = replace(seasons, seasons %in% c("year round","summer, spring, fall, winter","jan, feb, ap, june, jul, aug, oct, nov, dec"), "annual"),
                   seasons = replace(seasons, seasons %in% c("spring, summer"), "spring and summer"),
                   seasons = replace(seasons, seasons %in% c("july/ august"), "summer"),
                   seasons = replace(seasons, seasons %in% c("jul, aug, sept, nov","july-nov.","july-oct.","june-october",
                                                             "summer - fall","summer, fall"),"summer and fall"))
# ------------ #


# ------------ #
# plot
# ------------ #
#season
ggplot()+
  geom_point(data = sl, aes(x=seasons, y=sciname, col=seasons))+
  scale_fill_hue(l=98, c=100)+
  theme_bw()+
  labs(title = "Diet literature per species by season", subtitle = "from the literature review")+ 
  ylab("Percent of diet") + 
  xlab("Species")+
  theme(legend.position = "none", axis.text.x = element_text(angle =20, hjust = 1))

#mean
ggplot(mean.sl, aes(mean.diet,reorder(sciname,mean.diet),col=sciname))+geom_point()+theme_bw()+
  ggtitle("Mean percent of ammodytes in diet\nfrom the literature review")+ xlab("Mean percent of diet") + ylab("Species")+
  theme(legend.position = "none")

# box means
ggplot()+
  geom_boxplot(data = sl, aes(x = reorder(sciname, diet, fun=mean, na.rm=TRUE), y = diet, 
                       col=sciname, fill=sciname, alpha= 0.1))+
  geom_point(data = sl, aes(x=sciname, y=diet, col=sciname))+
  scale_fill_hue(l=98, c=100)+
  theme_bw()+
  labs(title="Percent of ammodytes in diet",subtitle="from the literature review")+ 
  ylab("Percent of diet") + 
  xlab("Species")+
  theme(legend.position = "none")+
  coord_flip()
# ------------ #

# bluefin age
bf = sl %>% filter(sciname %in% 'Thunnus thynnus') %>%
  mutate(age = ifelse(comname %in% c("Bluefin tuna (<185cm)","Bluefin tuna age 1-3"), "Juvenile (1-3 years old or <185cm)","Adult (4-5 years old or >185cm)"),
         age = ifelse(comname %in% c("Bluefin tuna"), "Undefined", age)) %>% arrange(age)

ggplot()+
  geom_boxplot(data = bf, aes(x = reorder(age, diet, fun=mean, na.rm=TRUE), y = diet, 
                              col=age, fill=age, alpha= 0.1))+
  geom_point(data = bf, aes(x=age, y=diet, col=age))+
  scale_fill_hue(l=98, c=100)+
  theme_bw()+
  labs(title="Percent of ammodytes in diet for Thunnus thynnus",subtitle="from the literature review")+ 
  ylab("Percent of diet") + 
  xlab("Age/Size range")+
  theme(legend.position = "none")+
  coord_flip()
#
#
bf = sl %>% filter(sciname %in% 'Thunnus thynnus') %>% 
  mutate(age = "Undefined",
         age = ifelse(comname %in% c("Bluefin tuna age 1-3"), "Age 1-3 yo", age),
         age = ifelse(comname %in% c("Bluefin tuna age 4-5"), "Age 4-5 yo", age),
         sizeclass = "Undefined",
         sizeclass = ifelse(comname %in% c("Bluefin tuna (<185cm)"), "Less than 185 cm", sizeclass),
         sizeclass = ifelse(comname %in% c("Bluefin tuna (>185cm)"), "Greater than 185 cm", sizeclass)) %>% 
  arrange(age)

ggplot()+
  geom_boxplot(data = bf, aes(x = reorder(age, diet, fun=mean, na.rm=TRUE), y = diet, 
                              col=age, fill=age, alpha= 0.1))+
  geom_point(data = bf, aes(x=age, y=diet, col=age))+
  scale_fill_hue(l=98, c=100)+
  theme_bw()+
  labs(title="Percent of ammodytes in diet for Thunnus thynnus by Age group",subtitle="from the literature review")+ 
  ylab("Percent of diet") + 
  xlab("Age range")+
  theme(legend.position = "none")+
  coord_flip()

ggplot()+
  geom_boxplot(data = bf, aes(x = reorder(sizeclass, diet, fun=mean, na.rm=TRUE), y = diet, 
                              col=sizeclass, fill=sizeclass, alpha= 0.1))+
  geom_point(data = bf, aes(x=sizeclass, y=diet, col=sizeclass))+
  scale_fill_hue(l=98, c=100)+
  theme_bw()+
  labs(title="Percent of ammodytes in diet for Thunnus thynnus by size group",subtitle="from the literature review")+ 
  ylab("Percent of diet") + 
  xlab("Size range")+
  theme(legend.position = "none")+
  coord_flip()
  
# ----------------- #
# means with color = location, shape = metric
cbPalette <- c("#CC79A7", "#D55E00", "#E69F00", 
               "#F0E442", "#009E73", "#56B4E9",   
               "#0072B2", "darkmagenta", 
               "#000000","#999999")
p = ggplot(data = filter(mean.sl.by.paper, !diet.metric.formal %in% c("undefined",NA)), 
       aes(y = mean.diet, x = reorder(sciname, mean.diet, na.rm=TRUE), 
           col = reorder(location.region,new.var), 
           shape = diet.metric.formal, 
           stroke = 2))+
  geom_point()+
  scale_shape_manual(values=1:length(unique(mean.sl.by.paper$diet.metric))) +
  coord_flip()+
  #geom_errorbar(data = mean.sl.by.paper, aes(ymin = (mean.diet - error), ymax = (mean.diet + error)), 
  #              width=.2, size = 1)+
  theme_bw()+
  ggtitle('Proportion of ammodytes in diet from the literature review')+
  ylab('mean proportion of ammodytes in diet')+
  xlab('species')+ 
  guides(col=guide_legend(title="Location"),
         shape=guide_legend(title="Diet Metric (%)"))+
  scale_colour_manual(values=cbPalette)
p
ggsave(file = "~/SL/prop_ammo_in_lit.png",plot=p)  
