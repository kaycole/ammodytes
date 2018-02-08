# ------------------------ #
# sandlance data
# ------------------------ #


# ------------------------ #
# load packages
# ------------------------ #
library(quantreg) ##library for plotting functions for quantile regression
require(dplyr)
require(readxl)
require(ggplot2)
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
ggsave(filename=paste(dir.out,"predator size vs. prey size.png",sep=""), plot=p)


plot(data$pdlen,data$pylen,type="n",las=1, main=NULL, xlab="", ylab="", family="sans")     ##simple plot function gives only axes, not points; las=1 switches font to horizontal; type ?plot to find more about each argument
points(data$pdlen,data$pylen,cex=1,col="black", pch=3)                                    ##add data points
abline(lm(pylen ~ pdlen, data = data), lty=1, col="black")                        ##add OLS line
taus <- c(.01,.99)                                                               #name taus (quantiles); these 4 lines add quantiles

for( i in 1:length(taus)){
  abline(rq(pylen ~ pdlen,tau=taus[i], data = data), lty=2, col="black")
}

text(locator(1),"c)") ##add an identifying letter to plot by clicking on plot with mouse in location you want it to be



#bring in data
setwd("C:/Bluefin Tuna Diet synthesis/Size analysis data")
all<-read.csv('BFT_Ldata.csv')
attach(all)
str(all)
summary(all)
lm(PreyL ~ PredL)


with(all, plot(PredL, PreyL, cex=.25,type="n",las=1, main=NULL, xlab="Bluefin tuna SFL (cm)", ylab="Prey length (cm)"))     ##simple plot function gives only axes, not points; las=1 switches font to horizontal; type ?plot to find more about each argument
points(PredL, PreyL,cex=.5,col="black", pch=3)                                    ##add data points
abline(rq(PreyL~PredL,tau=.5),col="blue")
abline(lm(PreyL ~ PredL), lty=2, col="blue")                               ##add OLS line
taus <- c(.01,.99)                                                                      #name taus (quantiles); these 4 lines add quantiles ##to examine more use taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(PreyL~PredL,tau=taus[i]),col="black")}

#BFT data code
#bring in data
setwd("C:/Bluefin Tuna Diet synthesis/Size analysis data")
all<-read.csv('BFT_Ldata.csv')
attach(all)
str(all)
summary(all)
lm(PreyL ~ PredL)


with(all, plot(PredL, PreyL, cex=.25,type="n",las=1, main=NULL, xlab="Bluefin tuna SFL (cm)", ylab="Prey length (cm)"))     ##simple plot function gives only axes, not points; las=1 switches font to horizontal; type ?plot to find more about each argument
points(PredL, PreyL,cex=.5,col="black", pch=3)                                    ##add data points
abline(rq(PreyL~PredL,tau=.5),col="blue")
abline(lm(PreyL ~ PredL), lty=2, col="blue")                               ##add OLS line
taus <- c(.01,.99)                                                                      #name taus (quantiles); these 4 lines add quantiles 
for( i in 1:length(taus)){
  abline(rq(PreyL~PredL,tau=taus[i]),col="black")}
