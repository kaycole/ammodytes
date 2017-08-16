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
#   num_tows = number of tows, variance= variance of meansw, cv= cv of meansw, 
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

allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade) %>% group_by(pdscinam, pdcomnam, decade) %>% summarise(n=n()) %>% arrange(desc(n))

# by season
allfhlen2 %>% dplyr::select(season) %>% group_by(season) %>% summarise(n=n()) %>% arrange(desc(n))

allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season) %>% group_by(pdscinam, pdcomnam, season) %>% summarise(n=n()) %>% arrange(desc(n))

# by geoarea
allfhlen2 %>% dplyr::select(geoarea) %>% group_by(geoarea) %>% summarise(n=n()) %>% arrange(desc(n))

allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea) %>% group_by(pdscinam, pdcomnam, geoarea) %>% summarise(n=n()) %>% arrange(desc(n))
# ------------------------ #


# ------------------------ #
# Make summary tables  of mean SL body size by species, 
# decade, season, geographic area 
# ------------------------ #
# by species
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, pylen) %>% group_by(pdscinam, pdcomnam) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)

# by decade
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade, pylen) %>% group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)

# by season
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season, pylen) %>% group_by(pdscinam, pdcomnam, season) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)

# by geoarea
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea, pylen) %>% group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)
# ------------------------ #


# ------------------------ #
# create tables with all predators min, av., and max size, 
# then for seasonal, decadal, geographic for size 
# ------------------------ #
# by species
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, pdlen) %>% group_by(pdscinam, pdcomnam) %>% 
  summarise(mean_pdlen=mean(pdlen), var_pdlen = var(pdlen), sd_pdlen = sd(pdlen), n=n()) %>% arrange(mean_pdlen)

ggplot(allfhlen2, aes(reorder(pdcomnam, pdlen, mean),pdlen,fill=reorder(pdcomnam, pdlen, mean)))+
  geom_boxplot()+
  #geom_jitter(width=0.1,size=0.1)+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Predator length (cm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean predator length")

# by decade
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, decade, pylen) %>% group_by(pdscinam, pdcomnam, decade) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)

# by season
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, season, pylen) %>% group_by(pdscinam, pdcomnam, season) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)

# by geoarea
allfhlen2 %>% dplyr::select(pdscinam, pdcomnam, geoarea, pylen) %>% group_by(pdscinam, pdcomnam, geoarea) %>% 
  summarise(mean_pylen=mean(pylen), var_pylen = var(pylen), sd_pylen = sd(pylen), n=n()) %>% arrange(mean_pylen)
# ------------------------ #


# ------------------------ #
# Make plots (bar or box and whisker or other?) 
# showing mean and SD of SL body sizes consumed 
# by each predator ordered from smallest mean prey size to largest
# ------------------------ #
ggplot(allfhlen2, aes(reorder(pdcomnam, pylen, mean),pylen,fill=reorder(pdcomnam, pylen, mean)))+
  geom_boxplot()+
  #geom_jitter(width=0.1,size=0.1)+
  guides(fill=FALSE)+
  coord_flip()+
  ylab("Sandlance length (mm)")+
  xlab("Scientific name of predator")+
  ggtitle("mean prey length by predator")

#need to get the size cut-offs for life stages (Gary Nelson’s work; stock assessment; Brian?) 
# ------------------------ #


# ------------------------ #
# Make line or box and whisker plots of mean SL body size consumed 
# by each predator by decade. Not all predators will have enough data 
# for all decades to show robust trends. Will need to look at # of records 
# by pred x decade; we can touch base and decide which ones to select before proceeding. 
# Regression analyses to be conducted to determine if the mean size 
# of SL has changed over time within and across predators. 
# ------------------------ #
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
  group_by(pdcomnam) %>% summarise(n=n()) %>% arrange(n)

# removed because n<70 (ALEWIFE, HADDOCK, STRIPED SEAROBIN, NORTHERN KINGFISH, CLEARNOSE SKATE,YELLOWTAIL FLOUNDER, STRIPED BASS)

# high users with enough data

# 1) LONGHORN SCULPIN
data = filter(allfhlen2, pdcomnam %in% "LONGHORN SCULPIN")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("LONGHORN SCULPIN")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 2) WINTER SKATE
data = filter(allfhlen2, pdcomnam %in% "WINTER SKATE")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("WINTER SKATE")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 3) ATLANTIC COD
data = filter(allfhlen2, pdcomnam %in% "ATLANTIC COD")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("ATLANTIC COD")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 4) WINDOWPANE
data = filter(allfhlen2, pdcomnam %in% "WINDOWPANE")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("WINDOWPANE")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 5) BLUEFISH
data = filter(allfhlen2, pdcomnam %in% "BLUEFISH")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("BLUEFISH")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 6) POLLOCK
data = filter(allfhlen2, pdcomnam %in% "POLLOCK")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("POLLOCK")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 7) SPINY DOGFISH
data = filter(allfhlen2, pdcomnam %in% "SPINY DOGFISH")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("SPINY DOGFISH")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# 8) SILVER HAKE
data = filter(allfhlen2, pdcomnam %in% "SILVER HAKE")
ggplot(data, aes(pdlen, pylen))+geom_point()+xlab("predator size (cm)")+ylab("prey size (mm)")+ggtitle("SILVER HAKE")+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

# ------------------------ #


