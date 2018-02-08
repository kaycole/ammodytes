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
            lesser = gsub("<","",lesser),
            lesser = sapply(strsplit(lesser,"+/-",fixed="TRUE"),head,1),
            greater = sapply(strsplit(greater,"+/-",fixed="TRUE"),head,1),
            mid = sapply(strsplit(mid,"+/-",fixed="TRUE"),head,1),
            lesser = apply(cbind(as.numeric(sapply(strsplit(lesser,"-"),head,1)), 
                                 as.numeric(sapply(strsplit(lesser,"-"),tail,1))), 1, mean, na.rm=TRUE),
            mid = apply(cbind(as.numeric(sapply(strsplit(mid,"-"),head,1)), 
                                 as.numeric(sapply(strsplit(mid,"-"),tail,1))), 1, mean, na.rm=TRUE),
            greater = apply(cbind(as.numeric(sapply(strsplit(greater,"-"),head,1)), 
                                 as.numeric(sapply(strsplit(greater,"-"),tail,1))), 1, mean, na.rm=TRUE),
            diet = apply(cbind(lesser, mid, greater),1,mean, na.rm=TRUE))

mean.sl = sl %>% group_by(sciname) %>% summarize(mean.diet = mean(diet, na.rm=TRUE))

#summer - june, july, aug
#spring - march, april, may
#fall - sept, oct, nov
#winter - dec, jan, feb
# ------------ #


# ------------ #
# plot
# ------------ #
ggplot(mean.sl, aes(mean.diet,reorder(sciname,mean.diet),col=sciname))+geom_point()+theme_bw()+
  ggtitle("Mean diet in literature review")+ xlab("Mean diet") + ylab("Species")+
  theme(legend.position = "none")
# ------------ #
