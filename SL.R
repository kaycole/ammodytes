# ------------------------ #
# sandlance data
# ------------------------ #


# ------------------------ #
# load packages
# ------------------------ #
library(dplyr)
library(readxl)
require(ggplot2)
require(Hmisc)
require(ggrepel)
# ------------------------ #


# ------------------------ #
# set paths
# ------------------------ #
#dir = "//ifw-hqfs1/MB SeaDuck/seabird_database/Kaycee_Working_Folder/sandlance/BrianSmith_data/"
dir = "C:/Users/kecoleman/Downloads"
dir.out = "~/SL/"
setwd(dir)
# ------------------------ #


# ------------------------ #
# import data
# ------------------------ #
# ------------------------ #
# ABOUT THE DATA:
#
# The files "AMMO_....csv' provide % ammo by mass by the various factors we discussed: predator (base), decade, geoarea, 10-cm length bin, and season.  
# Column names: 
#   meansw = weighted average mass of ammo per stomach and factor (grams), 
#   num_tows = number of tows, 
#   variance= variance of meansw, 
#   cv= cv of meansw, 
#   totwt = weighted average total mass of all prey per stomach and factor (grams), 
#   relmsw = % mass of ammo by factor, 
#   ci = 95% ci for meansw, 
#   relci = 95% ci for relmsw.
#
# "ammopylen.xlsx" provides ammo prey length means, sd, and n by the various factors (excel tabs). 
#
# "allfhlen2.csv" provides individual predator and prey lengths.  
#   Pdlen: predator length is in cm, 
#   pylen: prey length is in mm.  
#   Keep in mind each row represents an individual prey length so there will be some predator lengths with multiple prey lengths.
#
#
# NOTES:
#
# relmsw is the best metric to gauge predator use since it considers the mass of prey relative to other prey (% or proportion of diet).  
# meansw is average mass of prey per stomach, so hard to directly compare this among predators or 
# factors unless it's standardized in some way.  
#
# Number of tows is already accounted for in meansw and relmsw. 
# it's more of a sample size estimate since we weight the prey data by 
# numbers of predator individuals per tow.  This is used in the variance and ci estimates.
# ------------------------ #
# ------------------------ #
allfhlen2 = read.csv("allfhlen2.csv", header=TRUE)    
allfhlen2 = allfhlen2 %>% 
  mutate(geoarea = as.character(geoarea),
         geoarea = factor(geoarea, levels=c("SAB","MAB","SNE","GB","GoM","ScS")),
         season = as.character(season),
         season = factor(season, levels=c("FALL","SUMMER","SPRING","WINTER")))
AMMO_base = read.csv("AMMO_base.csv", header=TRUE)    
AMMO_decade = read.csv("AMMO_decade.csv", header=TRUE)      
AMMO_geoarea = read.csv("AMMO_geoarea.csv", header=TRUE)     
AMMO_len10 = read.csv("AMMO_len10.csv", header=TRUE)       
AMMO_season = read.csv("AMMO_season.csv", header=TRUE)      
ammopylen = read_excel("ammopylen.xlsx")      
# ------------------------ #


# ------------------------ #
# use AMMO pred prey length file to get summary stats, 
# e.g. points per decade, by species etc. 
# to get sample sizes per species to find which species are best candidates for pred-prey analysis
# ------------------------ #
# by species
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam) %>% 
  group_by(pdscinam, pdcomnam) %>% summarise(n=n()) %>% 
  arrange(desc(n))
write.csv(x,paste(dir.out,"number_by_species.csv",sep=""))

# by decade
x = allfhlen2 %>% dplyr::select(decade) %>% group_by(decade) %>% 
  summarise(n=n()) %>% arrange(desc(n))
write.csv(x,paste(dir.out,"number_by_decade.csv",sep=""))

x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade) %>% 
  group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(n=n()) %>% arrange(pdscinam,decade)
write.csv(x,paste(dir.out,"number_by_decade_by_species.csv",sep=""))

# by season
x = allfhlen2 %>% dplyr::select(season) %>% group_by(season) %>% 
  summarise(n=n()) %>% arrange(desc(n))
write.csv(x,paste(dir.out,"number_by_season.csv",sep=""))

x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season) %>% 
  group_by(pdscinam, pdcomnam, season) %>% 
  summarise(n=n()) %>% arrange(pdscinam,season)
write.csv(x,paste(dir.out,"number_by_season_by_species.csv",sep=""))

# by geoarea
x = allfhlen2 %>% dplyr::select(geoarea) %>% group_by(geoarea) %>% 
  summarise(n=n()) %>% arrange(desc(n))
write.csv(x,paste(dir.out,"number_by_geoarea.csv",sep=""))


x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea) %>% 
  group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(n=n()) %>% arrange(pdscinam,geoarea)
write.csv(x,paste(dir.out,"number_by_geoarea_species.csv",sep=""))
# ------------------------ #


# ------------------------ #
# Make summary tables  of mean SL body size by species, 
# decade, season, geographic area 
# ------------------------ #
# by species
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, pylen) %>% 
  group_by(pdscinam, pdcomnam) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% 
  arrange(mean_pylen)
write.csv(x,paste(dir.out,"pylength_by_species.csv",sep=""))

p = ggplot(allfhlen2, aes(reorder(pdcomnam, pylen, mean),pylen,fill=reorder(pdcomnam, pylen, mean)))+
  geom_boxplot()+
  #geom_jitter(width=0.1,size=0.1)+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Sandlance length (mm)")+
  xlab("common name of predator")+
  ggtitle("mean prey length by predator")
p
ggsave(filename=paste(dir.out,"pylength_by_species.png",sep=""), plot=p)

# by species by decade
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade, pylen) %>% 
  group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% 
  arrange(pdscinam,mean_pylen)
write.csv(x,paste(dir.out,"pylength_by_decade_by_species.csv",sep=""))

p = ggplot(allfhlen2, aes(decade,pylen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator by decade")
p
ggsave(filename=paste(dir.out,"pylength_by_decade_by_species.png",sep=""), plot=p)


# by species by season
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season, pylen) %>% 
  group_by(pdscinam, pdcomnam, season) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% 
  arrange(pdscinam,mean_pylen) 
write.csv(x,paste(dir.out,"pylength_by_season_by_species.csv",sep=""))

p = ggplot(allfhlen2, aes(season,pylen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator by season")
p
ggsave(filename=paste(dir.out,"pylength_by_season_by_species.png",sep=""), plot=p)


# by species by geoarea
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea, pylen) %>% 
  group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% 
  arrange(pdscinam,mean_pylen)
write.csv(x,paste(dir.out,"pylength_by_geoarea_by_species.csv",sep=""))

p = ggplot(allfhlen2, aes(geoarea,pylen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator by geoarea")
p
ggsave(filename=paste(dir.out,"pylength_by_geoarea_by_species.png",sep=""), plot=p)
# ------------------------ #


# ------------------------ #
# create tables with all predators min, av., and max size, 
# then for seasonal, decadal, geographic for size 
# ------------------------ #
# by species
p = ggplot(allfhlen2, aes(reorder(pdcomnam, pdlen, mean),pdlen,fill=reorder(pdcomnam, pdlen, mean)))+
  geom_boxplot()+
  #geom_jitter(width=0.1,size=0.1)+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length")
p
ggsave(filename=paste(dir.out,"pdlength_by_species.png",sep=""), plot=p)

# by decade
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade, pdlen) %>% 
  group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(mean_pdlen=mean(pdlen), var_pdlen = var(pdlen), sd_pdlen = sd(pdlen), n=n()) %>% 
  arrange(mean_pdlen)
write.csv(x,paste(dir.out,"pdlength_by_decade_by_species.csv",sep=""))

p = ggplot(allfhlen2, aes(decade,pdlen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length by predator by decade")
p
ggsave(filename=paste(dir.out,"pdlength_by_decade_by_species.png",sep=""), plot=p)

# by season
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season, pdlen) %>% 
  group_by(pdscinam, pdcomnam, season) %>% 
  summarise(mean_pdlen=mean(pdlen), var_pdlen = var(pdlen), sd_pdlen = sd(pdlen), n=n()) %>% 
  arrange(mean_pdlen) 
write.csv(x,paste(dir.out,"pdlength_by_season_by_species.csv",sep=""))

p = ggplot(allfhlen2, aes(season,y=pdlen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length by predator by season")
p
ggsave(filename=paste(dir.out,"pdlength_by_season_by_species.png",sep=""), plot=p)

# by geoarea
x = allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea, pdlen) %>% group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(mean_pdlen=mean(pdlen), var_pdlen = var(pdlen), sd_pdlen = sd(pdlen), n=n()) %>% 
  arrange(mean_pdlen) 
write.csv(x,paste(dir.out,"pdlength_by_geoarea_by_species.csv",sep=""))

p = ggplot(allfhlen2, aes(geoarea,pdlen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length by predator by geoarea")
p
ggsave(filename=paste(dir.out,"pdlength_by_geoarea_by_species.png",sep=""), plot=p)
# ------------------------ #


# ------------------------ #
# Make plots (bar or box and whisker or other?) 
# showing mean and SD of SL body sizes consumed 
# by each predator ordered from smallest mean prey size to largest
# ------------------------ #

#need to get the size cut-offs for life stages (Gary Nelson's work; stock assessment; Brian?) 
# group by life stage 
#   larvae = 3-4 mm
#   planktonic stage (2-3 months) = 5-35 mm (Grosslein and Azarovi tz 1982).
#   schooling behavior (90 days after hatching) and burrowing (133 days) = 35-40 mm  (Smigielski et al. 1984). 
#   1-3 year old fish can grow to 37 mm (Scott 1968) -> growth rate increases from the New York Bight to the Nova Scotia banks (Grosslein and Azarovitz 1982)

#line at 35mm
p = ggplot(allfhlen2, aes(reorder(pdcomnam, pylen, mean),pylen,fill=reorder(pdcomnam, pylen, mean)))+
  geom_boxplot()+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator 35mm line ")+
  geom_hline(yintercept=35, size=1, linetype = 3)
p
ggsave(filename=paste(dir.out,"pylength_by_pd_35mmline.png",sep=""), plot=p)

# split at 35mm
p = allfhlen2 = allfhlen2 %>% mutate(age = ifelse(pylen<=34,"planktonic","semidimersal")) 
ggplot(allfhlen2, aes(reorder(pdcomnam, pylen, mean),pylen,fill=reorder(pdcomnam, pylen, mean)))+
  geom_boxplot()+
  guides(fill=FALSE)+
  coord_flip()+
  facet_wrap(~age, nrow=1, scales = "free_x")+
  ylab("Sandlance length (mm) divided at 35 mm")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator separated by life stages")
p
ggsave(filename=paste(dir.out,"pylength_by_pd_35mmsplit.png",sep=""), plot=p)

#word plot
x = allfhlen2 %>% dplyr::group_by(pdscinam, pdcomnam) %>% summarise(mean.pylen = mean(pylen))
p = ggplot(x,aes(mean.pylen, rep(0,35), label=pdcomnam)) + geom_point(color="red")+
  coord_flip()+
  geom_text_repel(aes(label=pdcomnam), angle=0, size=4,
                  point.padding = unit(0.1, 'lines'),
                  box.padding = unit(0.3, 'lines'),
                  segment.color = "grey",
                  segment.size = 0.3,
                  force=0.1,
                  arrow = arrow(length = unit(0.01, 'npc')))+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())+
  scale_y_continuous(limits = c(0, 5))+
  geom_vline(xintercept=35, size=1, linetype = 3)+
  ylab("")+xlab("sandlance length (mm) ") + 
  ggtitle("mean SL length per species")
p
ggsave(filename=paste(dir.out,"pylength_by_pd_35mmline_names.png",sep=""), plot=p)
# ------------------------ #


# ------------------------ #
# Make line or box and whisker plots of mean SL body size consumed 
# by each predator by decade. Not all predators will have enough data 
# for all decades to show robust trends. Will need to look at # of records 
# by pred x decade; we can touch base and decide which ones to select before proceeding. 
# Regression analyses to be conducted to determine if the mean size 
# of SL has changed over time within and across predators. 
# ------------------------ #

# using only predators that have enough points (n>70)***
ggplot(allfhlen2, aes(reorder(factor(decade), pylen, mean),pylen,fill=reorder(factor(decade), pylen, mean)))+
  geom_boxplot()+
  #geom_jitter(width=0.1,size=0.1)+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Sandlance length (mm)")+
  xlab("Decade")
# ------------------------ #


# ------------------------ #
# determine which predators rely most 
# on SL as prey vs least: will be a relative analysis to total predator diet and 
# across all predators that consume SL. 
#
# Generate an ordered list of most reliant to least reliant in 
# overall diets this will likely be a table and a figure.
# ------------------------ #
p = ggplot(AMMO_base, aes(reorder(comname, relmsw), relmsw, color=reorder(comname, relmsw)))+
  geom_point()+
  coord_flip()+
  ylab("% mass of ammodytes by factor (grams)")+
  xlab("predator name")+
  ggtitle("predator use")+
  theme(legend.position="none")
p
ggsave(filename=paste(dir.out,"pd_use_percent_ammo.png",sep=""), plot=p)

# ------------------------ #


# ------------------------ #
# Create scatter plots of major predators (8-10) 
# exploratory analysis for predator size (x axis) vs. prey size (y-axis), 
# quantile (e.g., 5th & 95th quantiles) and linear (mean) regressions
# ------------------------ #
filter(allfhlen2,pdcomnam %in% c("LONGHORN SCULPIN","WINTER SKATE","ALEWIFE",
                                 "ATLANTIC COD","HADDOCK","WINDOWPANE",
                                 "STRIPED SEAROBIN","NORTHERN KINGFISH",
                                 "CLEARNOSE SKATE","BLUEFISH","YELLOWTAIL FLOUNDER",
                                 "POLLOCK","STRIPED BASS","SPINY DOGFISH","SILVER HAKE")) %>%
  group_by(pdcomnam) %>% summarise(n=n()) %>% arrange(n) %>% 
  left_join(.,dplyr::select(AMMO_base,comname,relmsw),by=c("pdcomnam"="comname"))

# removed because n<70 (ALEWIFE, HADDOCK, STRIPED SEAROBIN, NORTHERN KINGFISH, CLEARNOSE SKATE,YELLOWTAIL FLOUNDER, STRIPED BASS)

# high users with enough data
library(plyr)
r2<-ddply(allfhlen2,.(pdcomnam),function(x) summary(lm(x$pylen ~ x$pdlen))$r.squared)
names(r2)<-c("pdcomnam","r2")

# 1) LONGHORN SCULPIN (base n = 142, relmsw = 22.060416)
data = filter(allfhlen2, pdcomnam %in% "LONGHORN SCULPIN")
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle("LONGHORN SCULPIN")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+2, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% "LONGHORN SCULPIN"], sep=""))
p
ggsave(filename=paste(dir.out,"LONGHORN SCULPIN",sep=""), plot=p)

# 2) WINTER SKATE (base n = 1525, relmsw = 19.804057)
data = filter(allfhlen2, pdcomnam %in% "WINTER SKATE")
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle("WINTER SKATE")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+6, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% "WINTER SKATE"], sep=""))
p
ggsave(filename=paste(dir.out,"WINTER SKATE",sep=""), plot=p)


# 3) ATLANTIC COD (base n = 530, relmsw = 17.009978)
spp = "ATLANTIC COD"
data = filter(allfhlen2, pdcomnam %in% spp)
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle(spp)+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+6, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% spp], sep=""))
p
ggsave(filename=paste(dir.out,spp,".png",sep=""), plot=p)

# 4) WINDOWPANE (base n = 177,  relmsw = 6.196971)
spp = "WINDOWPANE"
data = filter(allfhlen2, pdcomnam %in% spp)
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle(spp)+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+3, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% spp], sep=""))
p
ggsave(filename=paste(dir.out,spp,".png",sep=""), plot=p)

# 5) BLUEFISH  (base n = 79, relmsw = 4.657543)
spp = "BLUEFISH"
data = filter(allfhlen2, pdcomnam %in% spp)
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle(spp)+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+3, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% spp], sep=""))
p
ggsave(filename=paste(dir.out,spp,".png",sep=""), plot=p)

# 6) POLLOCK (base n = 89,  relmsw = 3.696919)
spp = "POLLOCK"
data = filter(allfhlen2, pdcomnam %in% spp)
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle(spp)+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+4, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% spp], sep=""))
p
ggsave(filename=paste(dir.out,spp,".png",sep=""), plot=p)

# 7) SPINY DOGFISH (base n = 798,  relmsw = 2.307634)
spp = "SPINY DOGFISH"
data = filter(allfhlen2, pdcomnam %in% spp)
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle(spp)+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+4, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% spp], sep=""))
p
ggsave(filename=paste(dir.out,spp,".png",sep=""), plot=p)

# 8) SILVER HAKE (base n = 501,  relmsw = 2.095593)
spp = "SILVER HAKE"
data = filter(allfhlen2, pdcomnam %in% spp)
p = ggplot(data, aes(pdlen, pylen))+geom_point()+
  xlab("predator size (cm)")+
  ylab("prey size (mm)")+
  ggtitle(spp)+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  annotate("text", x = min(data$pdlen)+4, y = max(data$pylen)+100, label = paste("R^2:  ", r2$r2[r2$pdcomnam %in% spp], sep=""))
p
ggsave(filename=paste(dir.out,spp,".png",sep=""), plot=p)


# all
data = filter(allfhlen2, pdcomnam %in% c("LONGHORN SCULPIN","WINTER SKATE",
                                         "ATLANTIC COD","WINDOWPANE",
                                         "BLUEFISH","POLLOCK",
                                         "SPINY DOGFISH","SILVER HAKE"))
p = ggplot(data, aes(pdlen,pylen,fill=pdcomnam))+
  geom_point()+
  facet_wrap(~as.factor(pdcomnam), nrow=4)+
  guides(fill=FALSE)+
  geom_smooth(method='lm',formula=y~x)+
  ylab("prey length (mm)")+
  xlab("predator length (cm)")+
  ggtitle("predator size vs. prey size")
p
ggsave(filename=paste("predator size vs. prey size.png",sep=""), plot=p)

# ------------------------ #


