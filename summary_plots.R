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
p = ggplot(AMMO_geoarea, aes(reorder(comname,relmsw),relmsw,fill=geoarea))+
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
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_by_geoarea.png", plot=p)

p = ggplot(AMMO_season, aes(reorder(comname,relmsw),relmsw,fill=season))+
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
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_by_season.png", plot=p)

p = ggplot(AMMO_decade, aes(reorder(comname,relmsw),relmsw,fill=decade))+
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
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_by_decade.png", plot=p)

# reorder by relmsw for each 
p = ggplot(AMMO_geoarea[AMMO_geoarea$geoarea %in% "MAB",], aes(reorder(comname,relmsw),relmsw,fill=geoarea))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in MAB") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_MAB.png", plot=p)

p = ggplot(AMMO_geoarea[AMMO_geoarea$geoarea %in% "SNE",], aes(reorder(comname,relmsw),relmsw,fill=geoarea))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in SNE") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_SNE.png", plot=p)

p = ggplot(AMMO_geoarea[AMMO_geoarea$geoarea %in% "GB",], aes(reorder(comname,relmsw),relmsw,fill=geoarea))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in GB") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_GB.png", plot=p)

p = ggplot(AMMO_geoarea[AMMO_geoarea$geoarea %in% "GoM",], aes(reorder(comname,relmsw),relmsw,fill=geoarea))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in GOM") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_GOM.png", plot=p)

p = ggplot(AMMO_geoarea[AMMO_geoarea$geoarea %in% "ScS",], aes(reorder(comname,relmsw),relmsw,fill=geoarea))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in ScS") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_ScS.png", plot=p)

p = ggplot(AMMO_season[AMMO_season$season %in% "FALL",], aes(reorder(comname,relmsw),relmsw,fill=season))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in FALL") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_FALL.png", plot=p)

p = ggplot(AMMO_season[AMMO_season$season %in% "SUMMER",], aes(reorder(comname,relmsw),relmsw,fill=season))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in SUMMER") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_SUMMER.png", plot=p)

p = ggplot(AMMO_season[AMMO_season$season %in% "SPRING",], aes(reorder(comname,relmsw),relmsw,fill=season))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in SPRING") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_SPRING.png", plot=p)

p = ggplot(AMMO_season[AMMO_season$season %in% "WINTER",], aes(reorder(comname,relmsw),relmsw,fill=season))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in WINTER") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_WINTER.png", plot=p)

p = ggplot(AMMO_decade[AMMO_decade$decade %in% "1970s",], aes(reorder(comname,relmsw),relmsw,fill=decade))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in 1970s") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_1970s.png", plot=p)

p = ggplot(AMMO_decade[AMMO_decade$decade %in% "1980s",], aes(reorder(comname,relmsw),relmsw,fill=decade))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in 1980s") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_1980s.png", plot=p)

p = ggplot(AMMO_decade[AMMO_decade$decade %in% "1990s",], aes(reorder(comname,relmsw),relmsw,fill=decade))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in 1990s") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_1990s.png", plot=p)

p = ggplot(AMMO_decade[AMMO_decade$decade %in% "2000s",], aes(reorder(comname,relmsw),relmsw,fill=decade))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in 2000s") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_2000s.png", plot=p)

p = ggplot(AMMO_decade[AMMO_decade$decade %in% "2010s",], aes(reorder(comname,relmsw),relmsw,fill=decade))+
  geom_col()+
  geom_errorbar(aes(ymin=relmsw-relci, ymax=relmsw+relci), width=.5)+
  coord_flip()+
  guides(fill=FALSE)+
  ylab("Proportion of ammodytes in diet")+
  xlab("Species")+
  ggtitle("Proportion of ammodytes in diet in 2010s") +
  theme_bw()
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/port_ammo_2010s.png", plot=p)

# ------------------------ #

# ------------------------ #
#bar plot of pred vs. mean prey size with sd, order smallest to largest
# ------------------------ #
sum.allfhlen2 = allfhlen2 %>% group_by(pdcomnam) %>% 
  summarise(mean.pylen = mean(pylen, na.rm = TRUE),
            sd = sd(pylen, na.rm=TRUE))

p = ggplot(sum.allfhlen2, aes(reorder(pdcomnam,-mean.pylen), mean.pylen, fill=pdcomnam))+
  geom_col()+
  geom_errorbar(aes(ymin=mean.pylen-sd, ymax=mean.pylen+sd), width=.5)+
  ylab("Prey length (mm)")+
  xlab("Species")+
  ggtitle("Mean prey size per species") +
  theme_bw()+ 
  coord_flip()+
  theme(#axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = "none")
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/mean_prey_size_by_species.png", plot=p)
# ------------------------ #

# ------------------------ #
# scatter all pred size and all prey size
# with quant. reg at 1 and 99 
# ------------------------ #
taus = c(0.01,0.99)
p = ggplot(allfhlen2, aes(pdlen, pylen, col=pdcomnam))+
  geom_point()+
  geom_smooth(method='lm', col="black")+
  ylab("Prey length (mm)")+
  xlab("Predator length (cm)")+
  ggtitle("Predator size vs. prey size per species") +
  theme_bw() + 
  facet_wrap(~as.factor(pdcomnam), nrow=5)+
  geom_quantile(quantiles = taus, col="black")+
  theme(legend.position = "none",
        strip.text = element_text(size=7))
p
ggsave(filename="C:/Users/kecoleman/Documents/SL/quant_reg_and _lm_of_pred_size_and_prey_size_by_species.png", plot=p)

# ------------------------ #


