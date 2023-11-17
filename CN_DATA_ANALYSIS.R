#ANALYSIS======
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(nlme)
library(sjmisc)

cn2019 <- read.csv("VIC_CN.csv")       #LOAD DATA created in CN_DATA.R
cn2019$habitat <- factor(cn2019$habitat,
                         levels = c("Seagrass","Saltmarsh","Mangrove"))
dim(cn2019)#2233   49


##Compute Mg/ha and OC(%)==========
NewDATA <- read.csv("CN_2019CoringCampaign.csv")#Data produced in CN_DATA.R
NewDATA <- NewDATA [ !is.na(NewDATA$site),] #remove NA-s
NewDATA <- NewDATA [ !is.na(NewDATA$C.percent),] #remove NA-s
NewDATA$C.percent <- ifelse(NewDATA$C.percent <= 0 , 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models

RussianCore                       <- filter (NewDATA, coring_method == "RussianPick5cm" | coring_method == "RussianPick8cm")
RussianCore$SliceLength.cm        <- (RussianCore$DepthTo.cm - RussianCore$DepthFrom.cm) #round % to full numbers to run Poisson
RussianCore$SampleVolume.cm3      <- ((pi*(RussianCore$PipeDiameter.cm/2)^2)*RussianCore$SliceLength.cm)/2  #slice volume by two (half cylinder formula for volume)
RussianCore$dry_bulk_density.gcm3 <-  RussianCore$DryWeight.g / RussianCore$SampleVolume.cm3 
RussianCore$CarbonDensity.gcm3    <- RussianCore$dry_bulk_density.gcm3 * RussianCore$C.percent/100

RussianCore$CarbonStock.Mgha <- (((RussianCore$CarbonDensity.gcm3  / 1000000 ) *100000000) * RussianCore$SliceLength.cm )
RussianCore$Core_in.cm <- RussianCore$PipeLenght.cm  - RussianCore$CompactionIn #Compaction in cm
RussianCore$Pipe_in.cm <- RussianCore$PipeLenght.cm  - RussianCore$CompactionOut
RussianCore$Compaction_Correction_Value<- RussianCore$Core_in.cm / RussianCore$Pipe_in.cm
RussianCore$CarbonStock.Mgha_CORRECTED <- RussianCore$CarbonStock.Mgha * RussianCore$Compaction_Correction_Value


PlasticCore <- NewDATA %>%  #keep all samples but those collected with Russian Pick Core
  filter( ! CNCode %in% RussianCore$CNCode)

PlasticCore$SliceLength.cm        <- (PlasticCore$DepthTo.cm - PlasticCore$DepthFrom.cm) #round % to full numbers to run Poisson
PlasticCore$SampleVolume.cm3      <- (pi*(PlasticCore$PipeDiameter.cm/2)^2)*PlasticCore$SliceLength.cm  #slice volume
PlasticCore$dry_bulk_density.gcm3 <- PlasticCore$DryWeight.g / PlasticCore$SampleVolume.cm3 
PlasticCore$CarbonDensity.gcm3    <- PlasticCore$dry_bulk_density.gcm3 * PlasticCore$C.percent/100
PlasticCore$CarbonStock.Mgha      <- (((PlasticCore$CarbonDensity.gcm3  / 1000000 ) *100000000) * PlasticCore$SliceLength.cm )
PlasticCore$Core_in.cm            <- PlasticCore$PipeLenght.cm  - PlasticCore$CompactionIn #Compaction in cm
PlasticCore$Pipe_in.cm            <- PlasticCore$PipeLenght.cm  - PlasticCore$CompactionOut
PlasticCore$Compaction_Correction_Value<- PlasticCore$Core_in.cm / PlasticCore$Pipe_in.cm
PlasticCore$CarbonStock.Mgha_CORRECTED <- PlasticCore$CarbonStock.Mgha * PlasticCore$Compaction_Correction_Value


VicCorData <- rbind(PlasticCore,RussianCore)
VicCorData $habitat <- factor(VicCorData $habitat, levels = c("Saltmarsh", "Mangrove","Seagrass"))
#write.csv(VicCorData, file = "VicCoreData.csv", row.names = F)
range(VicCorData$C.percent)#  0.001 33.295



#Plot Mg/ha of C-stock===========
VicCorData <- read.csv("VicCoreData.csv")
VicCorData $habitat <- factor(VicCorData $habitat, levels = c("Saltmarsh", "Mangrove","Seagrass"))

core_carbon <- VicCorData %>% 
  select(CarbonStock.Mgha_CORRECTED, C.percent, habitat,core)%>%
  group_by(core,habitat) %>%
  summarise(CarbonStock.Mgha.core = sum(CarbonStock.Mgha_CORRECTED, na.rm = T),
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

ggplot(core_carbon, aes(habitat, mean.stock))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin = mean.stock + mean.stock.SE,
                     ymax = mean.stock - mean.stock.SE), width=.2)+
  labs(x= "", y = bquote('Carbon Stock  ' (Mg*~ha^-1 ~y^-1)))+
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


#BoxPlot OC(%) By habitat:=======
cn2019$habitat <- factor(cn2019$habitat, levels = c("Saltmarsh", "Mangrove","Seagrass"))

ggplot(cn2019, aes(x = habitat, y = C.percent, color=habitat)) +
  geom_boxplot() +
  #facet_grid(.~site)+
  ylab("Organic Carbon (%)") + xlab("") +
  theme_bw() + geom_jitter(alpha = 0.4)+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white"))



#Plot Fumigated vs non-fumigated==========
cn<- cn2019
cn$habitat <- factor(cn$habitat,levels = c("Saltmarsh","Mangrove","Seagrass"))

c1 <- ggplot(cn, aes(x = CPb, y = C.percent)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(DepthRange.cm ~ habitat)+ geom_jitter(alpha = 0.4,aes(color=HCl_Bubbles))+
  ylab("Organic Carbon (%)") + xlab("") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 16),
        legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=10),
        strip.background =  element_rect(fill = "white"))

c1
#ggsave(c1, filename = "c1.png", height = 7, width = 3)


