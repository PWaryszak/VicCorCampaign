#ANALYSIS======
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(nlme)
library(sjmisc)
library(gridExtra)
library(grid)
library(ggpubr) #ggarrange function is here


#cn2019 <- read.csv("CN_2019CoringCampaign.csv")#DATA created in CN_DATA.R
cn2019 <- read.csv("VIC_CN.csv")#LOAD DATA created in CN_DATA.R
names(cn2019)#2215   48

#% samples contained CaCO3 and these underwent acid fumigation=====
names(cn2019)
dim(cn2019)
CaCO3 <- select(cn2019,HCl_Bubbles) %>% filter(HCl_Bubbles == "yes")
No_bubbles <- select(cn2019,HCl_Bubbles) %>% filter( HCl_Bubbles == "no")
dim(CaCO3) + dim(No_bubbles) #478 samples total
dim(CaCO3) / 478 *100   # = 32% contained CaCO3


#DEPTHS CORRECTED Mg/ha of C-stock by habitat in corrected depths===========

#NewDATA <- read.csv("CN_2019CoringCampaign.csv")#Data produced in CN_DATA.R
NewDATA <- filter (cn2019, CPb =="C") # Get only C-cores = (processed for carbon)
NewDATA <- NewDATA [ !is.na(NewDATA$site),] #remove NA-s
NewDATA <- NewDATA [ !is.na(NewDATA$C.percent),] #remove NA-s
NewDATA$C.percent <- ifelse(NewDATA$C.percent <= 0 , 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models
NewDATA$DepthFrom.cm <- as.numeric(as.character(NewDATA$DepthFrom.cm))
NewDATA$DepthTo.cm <- as.numeric(as.character(NewDATA$DepthTo.cm))

NewDATA <- cn2019
NewDATA <- NewDATA [ !is.na(NewDATA$site),] #remove NA-s
NewDATA <- NewDATA [ !is.na(NewDATA$C.percent),] #remove NA-s
NewDATA$C.percent <- ifelse(NewDATA$C.percent <= 0 , 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models
NewDATA$DepthFrom.cm <- as.numeric(as.character(NewDATA$DepthFrom.cm)) #change to numeric
NewDATA$DepthTo.cm <- as.numeric(as.character(NewDATA$DepthTo.cm)) #change to numeric

#Get Field Compaction Correction Value:
NewDATA$Core_in.cm            <- NewDATA$PipeLenght.cm  - NewDATA$CompactionIn #Lenght of core
NewDATA$Pipe_in.cm            <- NewDATA$PipeLenght.cm  - NewDATA$CompactionOut #Length of pipe belowground
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.cm / NewDATA$Pipe_in.cm

#Apply Compaction_Correction_Value to both depths
NewDATA$DepthFromCorrected_cm <- NewDATA$DepthFrom.cm / NewDATA$Compaction_Correction_Value
NewDATA$DepthToCorrected_cm <- NewDATA$DepthTo.cm / NewDATA$Compaction_Correction_Value
NewDATA$SliceLengthCorrected_cm        <- (NewDATA$DepthToCorrected_cm - NewDATA$DepthFromCorrected_cm) #round % to full numbers to run Poisson
table(NewDATA$DepthToCorrected_cm)
NewDATA$CoreLengthCorrected_cm <- NewDATA$FieldCoreLength_cm / NewDATA$Compaction_Correction_Value
table(NewDATA$CoreLengthCorrected_cm)
table(NewDATA$SliceLengthCorrected_cm)

#STON.MG from Maria fucking up the  SliceLengthCorrected_cm CHECK!!!!!


--------------------------------------------






#Russian Core had different volume than PVC pipes:
NewDATA$SampleVolumeCorrected_cm3      <-   ifelse(NewDATA$coring_method == "PlasticPipe",
                                                   (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLengthCorrected_cm,
                                                   (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLengthCorrected_cm/2)
write
#Compute corrected DBH and C-stock
NewDATA$dry_bulk_densityCorrected_gcm3 <- NewDATA$DryWeight.g / NewDATA$SampleVolumeCorrected_cm3 
NewDATA$CarbonDensityCorrected_gcm3    <- NewDATA$dry_bulk_densityCorrected_gcm3 * NewDATA$C.percent/100
NewDATA$CarbonStockCorrected_Mgha      <- (((NewDATA$CarbonDensityCorrected_gcm3  / 1000000 ) *100000000) * NewDATA$SliceLengthCorrected_cm )

#EXtrapolate C-stock to missing slices:
#Apply etrapolation for OC stok as in paper: https://www.nature.com/articles/srep44071
#TRY 50  cm:
Depth50_100 <- filter(NewDATA,DepthFromCorrected_cm >= 50 & coring_method == "PlasticPipe") # subset slices below 50 cm
Depth50_100$Slice50_100_cm <- Depth50_100$DepthToCorrected_cm - Depth50_100$DepthFromCorrected_cm

summary(lm (log(CarbonStockCorrected_Mgha) ~ Slice50_100_cm,  data = Depth50_100 ))
summary(lm (CarbonStockCorrected_Mgha ~ Slice50_100_cm,  data = Depth50_100 ))


#Compute C-stock per core:
core_carbon <- NewDATA %>% 
  #filter(HCl_Bubbles=="no")%>% #If not account for fumigated samples,C-stock in Sg is lowest.
  select(CarbonStockCorrected_Mgha, C.percent, habitat,core,DepthRange.cm)%>%
  group_by(core,habitat) %>%
  summarise(CarbonStock.Mgha.core = sum(CarbonStockCorrected_Mgha, na.rm = T),
            OrganicCarbon.Mean = mean(C.percent, na.rm = T)) %>%
  group_by(habitat) %>%
  summarise(mean.stock = mean (CarbonStock.Mgha.core, na.rm = T),
            means.stock.SD = sd (CarbonStock.Mgha.core, na.rm = T),
            mean.stock.n = length(habitat),
            mean.stock.SE = means.stock.SD / sqrt (mean.stock.n),
            
            mean.percent = mean (OrganicCarbon.Mean, na.rm = T),
            mean.percent.n = length(habitat),
            mean.percent.SD = sd(OrganicCarbon.Mean, na.rm = T),
            means.percent.SE = mean.percent.SD / sqrt (mean.percent.n))

#plot C stock across 3 ecosystems:
NewDATA$habitat <- factor(NewDATA$habitat, levels = c("Saltmarsh", "Mangrove","Seagrass"))

ggplot(core_carbon, aes(habitat, mean.stock))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin = mean.stock + mean.stock.SE,
                     ymax = mean.stock - mean.stock.SE), width=.2)+
  labs(x= "", y = bquote('Carbon Stock  ' (Mg*~ha^-1)))+
  #facet_grid(.~)+
  #ggtitle("Carbon Accretion Rates in WPP & PPB habitats")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

#There might be shell-rich sample that overestimate C-stock:========
core_carbon_site <- NewDATA %>% 
  #filter(HCl_Bubbles=="no")%>% #If not account for fumigated samples,C-stock in Sg is lowest.
  select(CarbonStockCorrected_Mgha, C.percent, habitat,core,DepthRange.cm,site)%>%
  group_by(core,habitat,site) %>%
  summarise(CarbonStock.Mgha.core = sum(CarbonStockCorrected_Mgha, na.rm = T),
            OrganicCarbon.Mean = mean(C.percent, na.rm = T)) %>%
  group_by(habitat,site) %>%
  summarise(mean.stock = mean (CarbonStock.Mgha.core, na.rm = T),
            means.stock.SD = sd (CarbonStock.Mgha.core, na.rm = T),
            mean.stock.n = length(habitat),
            mean.stock.SE = means.stock.SD / sqrt (mean.stock.n),
            
            mean.percent = mean (OrganicCarbon.Mean, na.rm = T),
            mean.percent.n = length(habitat),
            mean.percent.SD = sd(OrganicCarbon.Mean, na.rm = T),
            means.percent.SE = mean.percent.SD / sqrt (mean.percent.n))


ggplot(core_carbon_site, aes(site, mean.stock))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin = mean.stock + mean.stock.SE,
                     ymax = mean.stock - mean.stock.SE), width=.2)+
  labs(x= "", y = bquote('Carbon Stock  ' (Mg*~ha^-1)))+
  #facet_grid(.~)+
  #ggtitle("Carbon Accretion Rates in WPP & PPB habitats")+
  guides(size=F)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))





#Plot Mg/ha of C-stock by habitat and BAY ===========
NewDATA$habitat <- factor(NewDATA$habitat, levels = c("Saltmarsh", "Mangrove","Seagrass"))
NewDATA$bay <- ifelse(NewDATA$site == "Fren"|NewDATA$site == "Rhyl"|NewDATA$site == "Koow"|
                        NewDATA$site == "Warn", "WPB","PPB")

NewDATA_No <- filter(NewDATA, HCl_Bubbles == "no")
dim(NewDATA_No)#293  59

core_carbon_bay1 <- NewDATA_No %>% 
  select(CarbonStockCorrected_Mgha, C.percent, habitat,core,DepthRange.cm,bay)%>%
  group_by(core,habitat,bay) %>%
  
  summarise(CarbonStock.Mgha.core = sum(CarbonStockCorrected_Mgha, na.rm = T),
            OrganicCarbon.Mean = mean(C.percent, na.rm = T)) %>%
  
  group_by(habitat,bay) %>%
  summarise(mean.stock = mean (CarbonStock.Mgha.core, na.rm = T),
            means.stock.SD = sd (CarbonStock.Mgha.core, na.rm = T),
            mean.stock.n = length(habitat),
            mean.stock.SE = means.stock.SD / sqrt (mean.stock.n),
            
            mean.percent = mean (OrganicCarbon.Mean, na.rm = T),
            mean.percent.n = length(habitat),
            mean.percent.SD = sd(OrganicCarbon.Mean, na.rm = T),
            means.percent.SE = mean.percent.SD / sqrt (mean.percent.n)) %>%
  mutate(bay2 = ifelse(bay=="PPB", "Phillip Bay", "Western Bay"))

n1 <- ggplot(core_carbon_bay1, aes(habitat, mean.stock))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin = mean.stock + mean.stock.SE,
                     ymax = mean.stock - mean.stock.SE), width=.2)+
  labs(x= "", y = bquote('Carbon Stock  ' (Mg*~ha^-1)))+
  facet_grid(.~bay2)+
  scale_y_continuous(limits = c(0,400))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))+
  ggtitle("Shell-rich samples removed")

n1

core_carbon_bay2 <- NewDATA %>% 
  select(CarbonStockCorrected_Mgha, C.percent, habitat,core,DepthRange.cm,bay)%>%
  group_by(core,habitat,bay) %>%
  
  summarise(CarbonStock.Mgha.core = sum(CarbonStockCorrected_Mgha, na.rm = T),
            OrganicCarbon.Mean = mean(C.percent, na.rm = T)) %>%
  
  group_by(habitat,bay) %>%
  summarise(mean.stock = mean (CarbonStock.Mgha.core, na.rm = T),
            means.stock.SD = sd (CarbonStock.Mgha.core, na.rm = T),
            mean.stock.n = length(habitat),
            mean.stock.SE = means.stock.SD / sqrt (mean.stock.n),
            
            mean.percent = mean (OrganicCarbon.Mean, na.rm = T),
            mean.percent.n = length(habitat),
            mean.percent.SD = sd(OrganicCarbon.Mean, na.rm = T),
            means.percent.SE = mean.percent.SD / sqrt (mean.percent.n)) %>%
  mutate(bay2 = ifelse(bay=="PPB", "Phillip Bay", "Western Bay"))

n2 <- ggplot(core_carbon_bay2, aes(habitat, mean.stock))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin = mean.stock + mean.stock.SE,
                     ymax = mean.stock - mean.stock.SE), width=.2)+
  labs(x= "", y = bquote('Carbon Stock  ' (Mg*~ha^-1)))+
  facet_grid(.~bay2)+
  scale_y_continuous(limits = c(0,400))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))+
  ggtitle("All samples")

n2

ns <- ggarrange(n2,n1, nrow = 1)
ns