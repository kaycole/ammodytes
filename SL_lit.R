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
sl <- read_excel("~/SL/Sandlance_predator_Litsearch_6.28.17.xlsx", col_names = TRUE, skip=1)
names(sl) = c("comname","lesser","mid","greater","diet.metric","prey.size","location.region",
              "location.local","years","seasons","ref","notes")
sl = as.data.frame(sl[1:(which(is.na(sl$comname))[1]-1),1:12]) # issues with NA column names and rows
sl[sl=="N/A"] = NA

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
            sciname = replace(sciname, sciname %in% "American plaice","Pleuronectes platessa"),  
            sciname = replace(sciname, sciname %in% "Atlantic cod","Gadus morhua"),         
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
            lesser = as.numeric(lesser),
            mid= as.numeric(mid),
            greater=as.numeric(greater),
            diet = apply(cbind(lesser, mid, greater), 1, mean, na.rm=TRUE))

mean.sl = sl %>% group_by(sciname) %>% summarize(mean.diet = mean(diet, na.rm=TRUE))

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
  
