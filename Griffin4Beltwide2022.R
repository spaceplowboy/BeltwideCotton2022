
setwd("~/conferences/2022/BeltwideCotton2022")
rm(list = ls())

library(tidyverse)
library(httr)
library(jsonlite)
library(plyr)
library(tmap)
library(strucchange)
library(reshape2)
library(usdarnass) # negates necessity for API
library(extrafont)
library(lubridate)
#font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       


######
## bounds for probability levels in addition to median
min.prob<-0.15
max.prob<-0.85

### acqure your API key from https://quickstats.nass.usda.gov/api
api.key <- '53D17844-D92E-3CF5-8520-6E1E45CA2A49'
nass_set_key(key = '53D17844-D92E-3CF5-8520-6E1E45CA2A49', overwrite = FALSE) #replace XXXX with key 


PLANTprogress5 <- nass_data(
  year="=>2020", agg_level_desc = "STATE", 
  short_desc =  "COTTON, UPLAND - PROGRESS, 5 YEAR AVG, MEASURED IN PCT PLANTED")
BOLLSprogress5 <- nass_data(
  year="=>2020", agg_level_desc = "STATE", 
  short_desc =  "COTTON, UPLAND - PROGRESS, 5 YEAR AVG, MEASURED IN PCT BOLLS OPENING")
DEFOLIATEprogress5 <- nass_data(
  year="=>2020", agg_level_desc = "STATE", 
  short_desc =  "COTTON, UPLAND - PROGRESS, 5 YEAR AVG, MEASURED IN PCT DEFOLIATED")
HARVESTprogress5 <- nass_data(
  year="=>2020", agg_level_desc = "STATE", 
  short_desc =  "COTTON, UPLAND - PROGRESS, 5 YEAR AVG, MEASURED IN PCT HARVESTED")

prog5 <- rbind(PLANTprogress5, BOLLSprogress5, DEFOLIATEprogress5, HARVESTprogress5)
PROG <- prog5 %>%
  as_tibble() %>%
  filter(year == 2020) %>%
  select(unit_desc, state_name, week_ending, begin_code, Value) %>%
  mutate(perc = as.numeric(as.character(Value))) %>%
  filter(state_name == "ARKANSAS") %>%
  mutate(week = ymd(week_ending)) %>%
  mutate(unit_desc = tolower(unit_desc)) %>%
  select(week, perc, unit_desc) %>%
  mutate_if(is.character,
            str_replace_all, patter = "pct ", replacement = "") %>%
  mutate(unit_desc = fct_relevel(unit_desc, 
                            "planted", "bolls opening", "harveted"))

ggplot(PROG, aes(x=week, y= perc, linetype = unit_desc, 
                 group = unit_desc, col=unit_desc)) +
  geom_line(lwd=1.3) +
  xlab(NULL) + ylab("Progress (%)") +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 15, linetype = "dashed") +
  geom_hline(yintercept = 85, linetype = "dashed") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(text=element_text(family="Times New Roman", size=18))
ggsave("cottonProgress.png", height = 6.42, width = 13.33, units = "in", dpi = "retina") 

###
library(tsibble)
bolls <- PROG %>%
  filter(unit_desc == "bolls opening") %>%
  mutate(ready = lag(perc)) %>%
  mutate(now = difference(ready, lag=1, differences = 1)) %>%
  select(week, perc, ready, now)
bolls
write.csv(bolls, "bollsTable.csv")
###

cottonHarvest<-nass_data(year=">=1950", agg_level_desc = "STATE", 
                         short_desc = "COTTON - ACRES HARVESTED",
                         reference_period_desc = "YEAR")

dat <- cottonHarvest %>%
  mutate(acre = (gsub(",","", Value))) %>%
  mutate(acres000 = as.numeric(acre)/1000) %>%
  mutate(acresM = as.numeric(acre)/1000000) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(STATEFP = state_fips_code)

table1 <- dat %>%
  as.tibble() %>%
  filter(year == 2021) %>%
  filter(state_name != "OTHER STATES") %>%
  mutate(percent = round(acres000/sum(acres000, na.rm=T)*100,2)) %>%
  mutate(rank = rank(-acres000)) %>%  
  select(state_name, acres000, percent, rank, STATEFP) 
write.csv(table1,"Beltwide2022table1.csv")

dat4 <-
  table1 %>%
  mutate(State = (state_name))
######

#nass_set_key(key = '53D17844-D92E-3CF5-8520-6E1E45CA2A49', overwrite = FALSE) #replace XXXX with key 

DSFW<-nass_data(year=">=1995", agg_level_desc = "STATE", 
                         short_desc = "FIELDWORK - DAYS SUITABLE, MEASURED IN DAYS / WEEK ")
DSFW<-subset(DSFW, state_name!="US TOTAL")

arkansasDSFW <-
  DSFW %>%
  filter(state_name != "US TOTAL") %>%
  filter(state_name == "ARKANSAS") %>%
  mutate(days = as.numeric(Value)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(woy = as.numeric(end_code)) %>%
  select(year, woy, days) %>%
  filter(woy >= 30) %>%
  filter(woy <= 49) %>%
  dplyr::group_by(woy) %>% 
  dplyr::summarise(DSFW = quantile(days, seq(0.10, 0.55, 0.05)), prob = seq(0.10, 0.55, 0.05)) %>%
  mutate_if(is.numeric, ~round(., 3)) %>%
  pivot_wider(names_from = prob, values_from = DSFW) %>%
  column_to_rownames("woy")
write.csv(arkansasDSFW, "ArkansasDSFW.csv")

hourPerDay <- 9
daysMultipler <- 7
acresPerHour <- 8

arkansasHours <- 
  arkansasDSFW * hourPerDay * daysMultipler/7

write.csv(arkansasHours, "ArkansasHours.csv")

arkansasAcres <-
  arkansasHours * acresPerHour

write.csv(arkansasAcres, "ArkansasAcres.csv")

############

cottonPlantProg<-nass_data(year=">=2005", agg_level_desc = "STATE", 
                short_desc = "COTTON, UPLAND - PROGRESS, MEASURED IN PCT PLANTED")

cottonHarvProg<-nass_data(year=">=2005", agg_level_desc = "STATE", 
                           short_desc = "COTTON, UPLAND - PROGRESS, MEASURED IN PCT HARVESTED")

cottonBollProg<-nass_data(year=">=2005", agg_level_desc = "STATE", 
                          short_desc = "COTTON, UPLAND - PROGRESS, MEASURED IN PCT BOLLS OPENING")

cottonPlantProg<-subset(cottonPlantProg, state_name!="US TOTAL")
cottonHarvProg<-subset(cottonHarvProg, state_name!="US TOTAL")
cottonBollProg<-subset(cottonBollProg, state_name!="US TOTAL")

bollDat<-cottonBollProg
bollDat$value<-as.numeric(gsub(",", "", bollDat$Value))
bollDat$harvPerc<-as.numeric(bollDat$value)
bollDat$year<-as.numeric(as.character(bollDat$year))

tab4desc<-matrix("NA", nrow=nlevels(factor(cottonPlantProg$state_name)), ncol=5)
mat4lm<-matrix(NA, nrow=nlevels(factor(cottonPlantProg$state_name)), ncol = 9)
mat4DSFWp<-matrix(NA, nrow=nlevels(factor(cottonPlantProg$state_name)), ncol = 6)
mat4DSFWh<-matrix(NA, nrow=nlevels(factor(cottonPlantProg$state_name)), ncol = 6)

#dat0 <- table(dat, dat$year, dat$acres)#

cottonAcreage<-nass_data(year=">=1900", agg_level_desc = "NATIONAL", 
                         short_desc = "COTTON - ACRES HARVESTED",
                         reference_period_desc = "YEAR")

UScottonAcres <- cottonAcreage %>%
  mutate(value = as.numeric(gsub(",", "", Value))) %>%
  mutate(acres = value/1000000) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(domain_desc == "TOTAL") %>%
  filter(source_desc == "SURVEY")

ggplot(UScottonAcres) + geom_line(aes(year, acres)) +   ylab("Millions of Acres") + xlab(NULL) +
  scale_x_continuous(breaks = seq(1900, 2020, 10)) + 
  theme_bw() +
  theme(text=element_text(family="Times New Roman", size=14))
ggsave("Beltwide2022UScottonAc.png", width=13.33, height=6.42, units="in", dpi="retina")



for(i in 1:nlevels(factor(cottonPlantProg$state_name))){
#    i=1
  state<-levels(factor(cottonPlantProg$state_name))[i]
  DSFWdat<-subset(DSFW, state_name==state)
  plantdat<-subset(cottonPlantProg, state_name==state)
  last5yearsp<-subset(plantdat, year>=2015) #actually last 3 years
  last5yearsp$value<-as.numeric(as.character(last5yearsp$Value))
  last5yearsp$begin_code1<-as.numeric(as.character(last5yearsp$begin_code))
  dat4graph5yrsp<-aggregate(value~begin_code1, data=last5yearsp, FUN = "mean")

  numRows<-as.numeric(max(levels(factor(DSFWdat$begin_code))))#nlevels(factor(DSFWdat$begin_code))-1
  mat4longterm<-matrix("NA", nrow = numRows, ncol = 4)
  
  for(k in as.numeric(min(levels(factor(DSFWdat$begin_code)))):as.numeric(max(levels(factor(DSFWdat$begin_code)))))      {
      WOY<-k
    j<-k-as.numeric(min(levels(factor(DSFWdat$begin_code))))
    dat4longtermDSFW<-subset(DSFWdat, begin_code==WOY)
    q4chart<-quantile(as.numeric(as.character(dat4longtermDSFW$Value)), probs=c(min.prob, .5, max.prob))

    
    if (length(dat4longtermDSFW[,1])>=4)
    {
    mat4longterm[j,1]<-k
    mat4longterm[j,2]<-q4chart[1]
    mat4longterm[j,3]<-q4chart[2]
    mat4longterm[j,4]<-q4chart[3]
    }
  }

  colnames(mat4longterm)<-c("WOY", "Bad15th", "Median50th", "Good85th")
  mat4LT<-as.data.frame(mat4longterm)
  mat4LT$Bad15th<-   as.numeric(as.character(mat4LT$Bad15th))
  mat4LT$Median50th<-as.numeric(as.character(mat4LT$Median50th))
  mat4LT$Good85th<-  as.numeric(as.character(mat4LT$Good85th)) 
  
  mat4LT2<-mat4LT
  mat4LT<-mat4LT[complete.cases(mat4LT[ , 2]),]

  colnames(mat4LT2)<-c("WOY", "15th", "50th", "85th")
  dat4DSFW<-melt(mat4LT2)
  dat4DSFW$WOY=as.numeric(levels(dat4DSFW$WOY))[dat4DSFW$WOY]
  dat4DSFW<-dat4DSFW[complete.cases(dat4DSFW[ , 3]),]
  
  
  ggplot() + 
    geom_line(aes(x=dat4DSFW$WOY, y= dat4DSFW$value, group=dat4DSFW$variable, 
                  linetype=dat4DSFW$variable, 
                 color=dat4DSFW$variable), size=1.) +
    scale_y_continuous(breaks = round(seq(0, 7, by = 1),1), limits=c(0,7)) +
    xlim(0, 52) + guides(fill=guide_legend(title=NULL)) +
    labs(y="Days per week", x="Week of year", caption="Source: USDA NASS") +
    labs(colour = "Percentile") +
    scale_color_manual("", values=c("darkgreen", "darkred", "black")) +
    scale_linetype_manual("", values=c("dotted", "twodash", "solid"))+
    theme_bw()
  
  ggsave(paste("1DSFW", state, "graph.png", sep=""), width=6, height=4, units="in", dpi = 600)
  
  begin=15
  end=85
  beginPeriodp<-min(which(abs(dat4graph5yrsp$value-begin)==min(abs(dat4graph5yrsp$value-begin))))
  begin15p<-dat4graph5yrsp$begin_code1[beginPeriodp]+1
  endPeriodp<-min(which(abs(dat4graph5yrsp$value-end)==min(abs(dat4graph5yrsp$value-end))))
  end85p<-dat4graph5yrsp$begin_code1[endPeriodp]
  
  tab4desc[i,1]<-state
  tab4desc[i,2]<-begin15p
  tab4desc[i,3]<-end85p
  
  numWeeks<-end85p-begin15p+1

    pDSFWdat<-subset(DSFWdat, begin_code>=begin15p & begin_code<=end85p)
    pDSFWdat$value<-as.numeric(as.character(pDSFWdat$Value))
    pDSFWdat$year<-as.numeric(as.character(pDSFWdat$year))
    DSFWdatptest<-aggregate(value~year, data=pDSFWdat, FUN = "length")
    DSFWdatp<-aggregate(value~year, data=pDSFWdat, FUN = "sum")
    DSFWdat2<-merge(DSFWdatp,DSFWdatptest,by="year")
    DSFWdat3<-subset(DSFWdat2, value.y==numWeeks)
    
    numYearsp<-length(DSFWdat3$year)
    
    mat4lm[i,1]<-state
    mat4lm[i,2:4]<-round(summary(lm(DSFWdat3$value.x~DSFWdat3$year))$coefficients[2,c(1,2,4)],2)
    if (length(DSFWdat3$year)>5)
    {
      chow4JSC<-sctest(formula=value.x~year, data = DSFWdat3, type = "Chow") # chow test
      mat4lm[i,8]<-round(chow4JSC$p.value, 4)
    } 
     
    ggplot(DSFWdat3, aes(x=year, y=value.x)) + geom_point() +
      geom_smooth(method=lm, se=T) + ylim(0,50) +xlim(1995, 2020) +
      ylab("Number days") + xlab(NULL) + theme_bw()
    ggsave(paste("6figure10",state, "4slope.png", sep=""))
    
    hdat<-subset(cottonHarvProg, state_name==state)
    
    hlast5years<-subset(hdat, year>=2015) #last 3 years
    hlast5years$value<-as.numeric(as.character(hlast5years$Value))
    hdat4graph5yrs<-aggregate(value~begin_code, data=hlast5years, FUN = "mean")

  dat4graph5yrsp<-data.frame(dat4graph5yrsp)
  hdat4graph5yrs<-data.frame(hdat4graph5yrs)
  dat4graph5yrsp$begin_code1<-as.numeric(as.character(dat4graph5yrsp$begin_code1))
  hdat4graph5yrs$begin_code<-as.numeric(as.character(hdat4graph5yrs$begin_code))
  
  colnames(dat4graph5yrsp)<-c("WOY", "perc")
  colnames(hdat4graph5yrs)<-c("WOY", "perc")
  dat4graph5yrsp$Progress<-"planting"  
  hdat4graph5yrs$Progress<-"harvest"
  peanutProgress<-rbind(dat4graph5yrsp, hdat4graph5yrs)
  
  ggplot() +
    geom_line(data=peanutProgress, aes(x=WOY, y=perc, group=Progress, color=Progress, linetype=Progress),
              size=1.1) +
    labs(y="Percentile", x="Week of year", caption="Source: USDA NASS") +
    xlim(0, 52) +
    geom_hline(yintercept=begin, linetype="dotted", color="lightgrey", size=.8) +
    geom_hline(yintercept=end, linetype="dotted", color="lightgrey",   size=.8) +
    scale_color_manual("", values=c("darkgoldenrod4", "darkgreen")) +
    scale_linetype_manual("", values=c("twodash", "solid"))+
    theme_bw()
    
  ggsave(paste("2progress", state, "graph.png", sep=""), width=6, height=4, units="in", dpi=600)
  
  beginPeriod<-min(which(abs(hdat4graph5yrs$perc-begin)==min(abs(hdat4graph5yrs$perc-begin))))
  hbegin15<-as.numeric(hdat4graph5yrs$WOY[beginPeriod])+1
  endPeriod<-min(which(abs(hdat4graph5yrs$perc-end)==min(abs(hdat4graph5yrs$perc-end))))
  hend85<-as.numeric(hdat4graph5yrs$WOY[endPeriod])
  
  tab4desc[i,4]<-as.numeric(as.character(hbegin15))
  tab4desc[i,5]<-as.numeric(as.character(hend85))
  
  hDSFWdat<-subset(DSFWdat, begin_code>=hbegin15 & begin_code<=hend85)
  hDSFWdat$value<-as.numeric(as.character(hDSFWdat$Value))
  hDSFWdat$year<-as.numeric(as.character(hDSFWdat$year))
  
  hDSFWdattest<-aggregate(value~year, data=hDSFWdat, FUN = "length")
  
  hDSFWdath<-aggregate(value~year, data=hDSFWdat, FUN = "sum")
  hnumWeeks<-hend85-hbegin15+1
  hDSFWdat2<-merge(hDSFWdath,hDSFWdattest,by="year")
  hDSFWdat3<-subset(hDSFWdat2, value.x==hnumWeeks)
  
  mat4lm[i,5:7]<-round(summary(lm(hDSFWdat2$value.x~hDSFWdat2$year))$coefficients[2,c(1,2,4)],2)
    if (length(hDSFWdat2$year)>5)
  {
    chow4JSC<-sctest(formula=value.x~year, data = hDSFWdat2, type = "Chow") # chow test
    mat4lm[i,9]<-round(chow4JSC$p.value, 4)
  } 
  
  numYearsh<-length(hDSFWdat2$year)
  
  ggplot(DSFWdat3, aes(value.x)) +
    geom_histogram(color="black", fill="darkgrey", binwidth=2) +
    ylab(paste("Count: n= ", numYearsp, " years", sep="")) +
    xlab("Fieldwork days") +
    xlim(0,60) + 
    theme_bw()
  
  ggsave(paste("3hist4", state, "Planting.png", sep=""), width=5, height=4, dpi=600)

    ggplot(hDSFWdat2, aes(value.x)) +
    geom_histogram(color="black", fill="darkgrey", binwidth=2) +
    ylab(paste("Count: n= ", numYearsp, " years", sep="")) +
    xlab("Fieldwork days") +
    xlim(0,60) + 
    theme_bw()
  
  ggsave(paste("4hist4", state, "harvest.png", sep=""), width=5, height=4, dpi=600)
    
   mat4DSFWp[i,1]<-state
   mat4DSFWp[i,2]<-round(min(DSFWdat3$value.x),2)
   mat4DSFWp[i,3]<-round(quantile(as.numeric(as.character(DSFWdat3$value.x)), probs=min.prob),2)
   mat4DSFWp[i,4]<-round(quantile(as.numeric(as.character(DSFWdat3$value.x)), probs=.5),2)
   mat4DSFWp[i,5]<-round(quantile(as.numeric(as.character(DSFWdat3$value.x)), probs=max.prob),2)
   mat4DSFWp[i,6]<-round(max(DSFWdat3$value.x),2)
   
   mat4DSFWh[i,1]<-state
   mat4DSFWh[i,2]<-round(min(hDSFWdat2$value.x),2)
   mat4DSFWh[i,3]<-round(quantile(as.numeric(as.character(hDSFWdat2$value.x)), probs=min.prob),2)
   mat4DSFWh[i,4]<-round(quantile(as.numeric(as.character(hDSFWdat2$value.x)), probs=.5),2)
   mat4DSFWh[i,5]<-round(quantile(as.numeric(as.character(hDSFWdat2$value.x)), probs=max.prob),2)
   mat4DSFWh[i,6]<-round(max(hDSFWdat2$value.x),2)

     }
 
colnames(mat4DSFWp)<-c("state", "minPdays", "days15p", "days50p", "days85p", "maxPdays")
colnames(mat4DSFWh)<-c("state", "minHdays", "days15h", "days50h", "days85h", "maxHdays")
mat4DSFWp<-as.data.frame(mat4DSFWp)
mat4DSFWh<-as.data.frame(mat4DSFWh)

write.csv(mat4DSFWp, "plantingTable.csv")
write.csv(mat4DSFWh, "harvestTable.csv")
colnames(tab4desc)<-c("State", "Begin Plant", "End Plant", "Begin Harvest", "End Harvest")
write.table(tab4desc, "tab4desc.txt", sep=",")
write.csv(tab4desc, "tab4desc.csv")
tab4<-as.data.frame(tab4desc)
table1<-merge(dat4,tab4,by="State")
table1$State<-tolower(table1$State)
write.csv(table1, "table1.csv")

colnames(mat4lm)<-c("State", "Planting Time Slope", "Planting Time SE", "Planting Time p-value","Harvest Time Slope", "Harvest Time SE", "Harvest Time p-value", "ChowPlant", "ChowHarvest")
mat4lm<-as.data.frame(mat4lm)
mat4lm$State<-tolower(mat4lm$State)
write.csv(mat4lm, "slopestats.csv")

#####
library(tigris)

nass      <- 'https://quickstats.nass.usda.gov/api/api_GET/'
nasskey   <- '?key=A4E9267C-8772-39F4-85AF-C869655C1B6C'
fmt_type  <- '&format=CSV'
src_desc  <- '&source_desc=SURVEY'
year      <- '&year__GE=1973'
ref_desc  <- '&reference_period_desc=YEAR'
agg_desc  <- '&agg_level_desc=COUNTY'
comm_desc <- '&commodity_desc=COTTON'
descript  <-
  '&short_desc=COTTON, UPLAND - ACRES HARVESTED'
dom_desc  <- '&domain_desc=TOTAL'
freq_desc <- '&freq_desc=ANNUAL'

http.request <- paste(nass, nasskey, fmt_type, src_desc, year, ref_desc,
                      agg_desc, comm_desc, dom_desc, freq_desc, descript,
                      sep = '')

download.file(http.request, destfile="cottonAcres.csv")
myDat     <- read_csv(file='cottonAcres.csv')

years<-levels(factor(myDat$year))



usaCounties <- counties( cb = T, resolution = "20m")
states <- states(cb = T, resolution = "20m")
states <- states %>% 
  filter(STATEFP < "60") %>% # keep only states omits territories
  filter(!STATEFP %in% c("02", "15")) # AK HI

usaCounties <- usaCounties %>% 
  filter(STATEFP < "60") %>% # keep only states omits territories out
  filter(!STATEFP %in% c("02", "15")) # AK HI
usaCounties <- usaCounties %>%
  mutate(FIPS = paste(STATEFP, COUNTYFP, sep=""))
us_counties_cotton <- myDat %>%
  mutate(FIPS = paste(state_ansi, county_ansi, sep="")) %>%
  mutate(acres = Value/1000) %>%
  # filter(year == 2020) %>%
  mutate(cotton.cut = cut_number(acres, n = 7, 
                                 dig.lab = 0, 
                                 # breaks = c(1, 2, 6, 10, 20,40))) #, 
                                 labels = c("< 1", "1  to 2", "2 to 6", "6 to 10", "10 to 20", "20 to 40", "> 40")))



ggplot() + 
  geom_sf(data = usaCounties, fill = "grey99", colour = "grey74", lwd = 0.01) +
  geom_sf(data = usaCounties %>% 
            left_join(us_counties_cotton, by="FIPS") %>%
            filter(year == 2020) , 
          aes(fill = cotton.cut),
          colour = "grey37",
          lwd = 0) + 
  geom_sf(data = states, fill = NA, color = "grey6", lwd = 0.4) +
  coord_sf(default_crs = sf::st_crs(4326)) +
  #labs(title=paste(' United States harvested cotton acreage,', yr),
  #   labs(title=' United States harvested cotton acreage, 2021',
  #caption='Terry Griffin, Kansas State University. Data source: USDA National Agricultural Statistics Service (NASS)') +
  scale_fill_brewer(palette = "Greens", name = "acres \n(000s)", drop = FALSE) +
  theme_bw() +
  theme(text=element_text(family="Times New Roman", size=16))

ggsave("Fig2map.png", width=13.33, height=6.42, units='in', dpi="retina")

