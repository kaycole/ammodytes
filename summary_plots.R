# ----------------- #
# tables and plots for michelle
# summary by species, season, decade, geoarea
# ----------------- # 

# ------------------------ #
# load packages
# ------------------------ #
library(dplyr)
library(readxl)
require(ggplot2)
# ------------------------ #


# ------------------------ #
# set paths
# ------------------------ #
dir = "C:/Users/kecoleman/Documents/SL/"
setwd(dir)
# ------------------------ #


# ------------------------ #
# load data
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
AMMO_season = read.csv("AMMO_season.csv", header=TRUE)      
# ------------------------ #


# ------------------------ #
# plots
# ------------------------ #
# p = ggplot(AMMO_decade, aes(decade,relmsw,fill=comname))+
#   geom_col()+
#   geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
#   facet_wrap(~as.factor(comname), nrow=5)+
#   theme(strip.text = element_text(size=1),
#         axis.text.x = element_text(size=3))+
#   coord_flip()+
#   guides(fill=FALSE)+
#   ylab("Proportion of ammodytes in diet")+
#   xlab("Decade")+
#   ggtitle("Proportion of ammodytes in diet by decade") +
#   theme_bw()
# p
# 
# p = ggplot(AMMO_season, aes(season,relmsw,fill=comname))+
#   geom_col()+
#   geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
#   facet_wrap(~as.factor(comname), nrow=5)+
#   theme(strip.text = element_text(size=1),
#         axis.text.x = element_text(size=3))+
#   coord_flip()+
#   guides(fill=FALSE)+
#   ylab("Proportion of ammodytes in diet")+
#   xlab("Season")+
#   ggtitle("Proportion of ammodytes in diet by season") +
#   theme_bw()
# p
# 
# p = ggplot(AMMO_geoarea, aes(geoarea,relmsw,fill=comname))+
#   geom_col()+
#   geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
#   facet_wrap(~as.factor(comname), nrow=5)+
#   theme(strip.text = element_text(size=1),
#         axis.text.x = element_text(size=3))+
#   coord_flip()+
#   guides(fill=FALSE)+
#   ylab("Proportion of ammodytes in diet")+
#   xlab("Geoarea")+
#   ggtitle("Proportion of ammodytes in diet by geoarea") +
#   theme_bw()
# p
# ########
p = ggplot(AMMO_geoarea, aes(comname,relmsw,fill=geoarea))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  facet_wrap(~as.factor(geoarea), nrow=1)+
  theme(strip.text = element_text(size=1),
        axis.text.x = element_text(size=3))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Geoarea")+
  ggtitle("Proportion of ammodytes in diet by geoarea") +
  theme_bw()
p

p = ggplot(AMMO_season, aes(comname,relmsw,fill=season))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  facet_wrap(~as.factor(season), nrow=1)+
  theme(strip.text = element_text(size=1),
        axis.text.x = element_text(size=3))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("season")+
  ggtitle("Proportion of ammodytes in diet by season") +
  theme_bw()
p

p = ggplot(AMMO_decade, aes(comname,relmsw,fill=decade))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  facet_wrap(~as.factor(decade), nrow=1)+
  theme(strip.text = element_text(size=1),
        axis.text.x = element_text(size=3))+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Decade")+
  ggtitle("Proportion of ammodytes in diet by decade") +
  theme_bw()
p

# reorder by relmsw
# ------------------------ #


# ------------------------ #
# sum tables
# ------------------------ #
select(sciname, comname, relmsw, relci)

# ------------------------ #


# ------------------------ #
# expot
# ------------------------ #

# ------------------------ #



