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
dir = "//ifw-hqfs1/MB SeaDuck/seabird_database/Kaycee_Working_Folder/sandlance/BrianSmith_data/"
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
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam) %>% group_by(pdscinam, pdcomnam) %>% summarise(n=n()) %>% arrange(desc(n))

# by decade
allfhlen2 %>% dplyr::select(decade) %>% group_by(decade) %>% summarise(n=n()) %>% arrange(desc(n))

allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade) %>% group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(n=n()) %>% arrange(pdscinam,decade)

# by season
allfhlen2 %>% dplyr::select(season) %>% group_by(season) %>% summarise(n=n()) %>% arrange(desc(n))

allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season) %>% group_by(pdscinam, pdcomnam, season) %>% 
  summarise(n=n()) %>% arrange(pdscinam,season)

# by geoarea
allfhlen2 %>% dplyr::select(geoarea) %>% group_by(geoarea) %>% summarise(n=n()) %>% arrange(desc(n))

allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea) %>% group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(n=n()) %>% arrange(pdscinam,geoarea)
# ------------------------ #


# ------------------------ #
# Make summary tables  of mean SL body size by species, 
# decade, season, geographic area 
# ------------------------ #
# by species
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, pylen) %>% group_by(pdscinam, pdcomnam) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)

ggplot(allfhlen2, aes(reorder(pdcomnam, pylen, mean),pylen,fill=reorder(pdcomnam, pylen, mean)))+
  geom_boxplot()+
  #geom_jitter(width=0.1,size=0.1)+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator")

# by species by decade
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade, pylen) %>% group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(pdscinam,mean_pylen)

ggplot(allfhlen2, aes(decade,pylen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator by decade")

# by species by season
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season, pylen) %>% group_by(pdscinam, pdcomnam, season) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(pdscinam,mean_pylen)

ggplot(allfhlen2, aes(season,pylen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator by season")

# by species by geoarea
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea, pylen) %>% group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(pdscinam,mean_pylen)

ggplot(allfhlen2, aes(geoarea,pylen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator by geoarea")
# ------------------------ #


# ------------------------ #
# create tables with all predators min, av., and max size, 
# then for seasonal, decadal, geographic for size 
# ------------------------ #
# by species

ggplot(allfhlen2, aes(reorder(pdcomnam, pdlen, mean),pdlen,fill=reorder(pdcomnam, pdlen, mean)))+
  geom_boxplot()+
  #geom_jitter(width=0.1,size=0.1)+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length")

# by decade
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade, pdlen) %>% group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(mean_pdlen=mean(pdlen), var_pdlen = var(pdlen), sd_pdlen = sd(pdlen), n=n()) %>% arrange(mean_pdlen)

ggplot(allfhlen2, aes(decade,pdlen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length by predator by decade")

# by season
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season, pdlen) %>% group_by(pdscinam, pdcomnam, season) %>% 
  summarise(mean_pdlen=mean(pdlen), var_pdlen = var(pdlen), sd_pdlen = sd(pdlen), n=n()) %>% arrange(mean_pdlen)

ggplot(allfhlen2, aes(season,pdlen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length by predator by season")

# by geoarea
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea, pdlen) %>% group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(mean_pdlen=mean(pdlen), var_pdlen = var(pdlen), sd_pdlen = sd(pdlen), n=n()) %>% arrange(mean_pdlen)

ggplot(allfhlen2, aes(geoarea,pdlen,fill=pdcomnam))+
  geom_boxplot()+
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  theme(strip.text = element_text(size=5),
        axis.text.x = element_text(size=5))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length by predator by geoarea")
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
ggplot(allfhlen2, aes(reorder(pdcomnam, pylen, mean),pylen,fill=reorder(pdcomnam, pylen, mean)))+
  geom_boxplot()+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator")+
  geom_hline(yintercept=35, size=1, linetype = 3)
  

# split at 35mm
allfhlen2 = allfhlen2 %>% mutate(age = ifelse(pylen<=34,"planktonic","semidimersal")) 
ggplot(allfhlen2, aes(reorder(pdcomnam, pylen, mean),pylen,fill=reorder(pdcomnam, pylen, mean)))+
  geom_boxplot()+
  guides(fill=FALSE)+
  coord_flip()+
  facet_wrap(~age, nrow=1, scales = "free_x")+
  ylab("Sandlance length (mm) divided at 35 mm")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator separated by life stages")

#word plot
ggplot(data,aes(mpylen,rep(0,35), label=pdcomnam)) + geom_point(color="red")+
  geom_text_repel(aes(label=pdcomnam), angle=90, size=3,
                  point.padding = unit(0.5, 'lines'),
                  box.padding = unit(0.4, 'lines'),
                  segment.color = "darkgrey",
                  segment.size = 0.5,
                  force=2,
                  arrow = arrow(length = unit(0.01, 'npc')))+
  theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())+
  scale_y_continuous(limits = c(0, 5))+
  geom_vline(xintercept=35, size=1, linetype = 3)+
  ylab("")+ggtitle("mean SL length per species")
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
ggplot(AMMO_base, aes(reorder(comname, relmsw), relmsw, color=reorder(comname, relmsw)))+
  geom_point()+
  coord_flip()+
  ylab("% mass of ammodytes by factor (grams)")+
  xlab("predator name")+
  ggtitle("predator use")+
  theme(legend.position="none")
# ------------------------ #


# ------------------------ #
# Create scatter plots of major predators (8-10) 
# exploratory analysis for predator size (x axis) vs. prey size (y-axis), 
# quantile (e.g., 5th & 95th quantiles) and linear (mean) regressions
# ------------------------ #
filter(allfhlen2,pdcomnam %in% c("LONGHORN SCULPIN","WINTER SKATE","ALEWIFE","ATLANTIC COD","HADDOCK",
                                 "WINDOWPANE","STRIPED SEAROBIN","NORTHERN KINGFISH",
                                 "CLEARNOSE SKATE","BLUEFISH","YELLOWTAIL FLOUNDER",
                                 "POLLOCK","STRIPED BASS","SPINY DOGFISH","SILVER HAKE")) %>%
  group_by(pdcomnam) %>% summarise(n=n()) %>% arrange(n) %>% 
  left_join(.,dplyr::select(AMMO_base,comname,relmsw),by=c("pdcomnam"="comname"))

# removed because n<70 (ALEWIFE, HADDOCK, STRIPED SEAROBIN, NORTHERN KINGFISH, CLEARNOSE SKATE,YELLOWTAIL FLOUNDER, STRIPED BASS)

# high users with enough data

# 1) LONGHORN SCULPIN (base n = 142, relmsw = 22.060416)
data = filter(allfhlen2, pdcomnam %in% "LONGHORN SCULPIN")

r2<-ddply(data,.(cat),function(x) summary(lm(x$yval ~ x$xval))$r.squared)
names(r2)<-c("cat","r2")

ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("LONGHORN SCULPIN")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)+
  geom_text(r2,aes(color=cat, label = paste("R^2: ", r2, sep="")), x = 25, y = 300, parse = TRUE)

# 2) WINTER SKATE (base n = 1525, relmsw = 19.804057)
data = filter(allfhlen2, pdcomnam %in% "WINTER SKATE")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("WINTER SKATE")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 3) ATLANTIC COD (base n = 530, relmsw = 17.009978)
data = filter(allfhlen2, pdcomnam %in% "ATLANTIC COD")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("ATLANTIC COD")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 4) WINDOWPANE (base n = 177,  relmsw = 6.196971)
data = filter(allfhlen2, pdcomnam %in% "WINDOWPANE")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("WINDOWPANE")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 5) BLUEFISH  (base n = 79, relmsw = 4.657543)
data = filter(allfhlen2, pdcomnam %in% "BLUEFISH")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("BLUEFISH")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 6) POLLOCK (base n = 89,  relmsw = 3.696919)
data = filter(allfhlen2, pdcomnam %in% "POLLOCK")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("POLLOCK")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 7) SPINY DOGFISH (base n = 798,  relmsw = 2.307634)
data = filter(allfhlen2, pdcomnam %in% "SPINY DOGFISH")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("SPINY DOGFISH")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 8) SILVER HAKE (base n = 501,  relmsw = 2.095593)
data = filter(allfhlen2, pdcomnam %in% "SILVER HAKE")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("SILVER HAKE")+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# all
data = filter(allfhlen2, pdcomnam %in% c("LONGHORN SCULPIN","WINTER SKATE","ATLANTIC COD",
                                         "WINDOWPANE","BLUEFISH","POLLOCK","SPINY DOGFISH",
                                         "SILVER HAKE"))
ggplot(data, aes(pdlen,pylen,fill=pdcomnam))+
  geom_point()+
  facet_wrap(~as.factor(pdcomnam), nrow=4)+
  guides(fill=FALSE)+
  geom_smooth(method='lm',formula=y~x)+
  ylab("prey length (mm)")+
  xlab("predator length (cm)")+
  ggtitle("predator size vs. prey size")
# ------------------------ #


