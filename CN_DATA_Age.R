#Compute CAR (Carbon Acrretion Rate)=====
library(tidyverse)

#VicCorCampaign Age Data:
#Create MAR (Mass Accretion Rate) dataset
#off Pere's spreadsheet. MAR's units = g/cm2/y
MAR <- data.frame( site_habitat = c("Fren_Mangrove","Fren_Seagrass","Fren_Saltmarsh",
                                    "Jawb_Seagrass ","Jawb_Saltmarsh","Koow_Mangrove",
                                    "Rhyl_Mangrove","Rhyl_Seagrass","Rhyl_Saltmarsh",
                                    "Swan_Seagrass","Swan_Saltmarsh",
                                    "Warn_Mangrove","Warn_Seagrass","Warn_Saltmarsh"),
                   MAR = c(0.09, 0.12, NA,
                           0.104, NA, 0.046,
                           0.07, 0.13, 0.053,
                           0.046,0.103,
                           0.029, NA, 0.071))

cn2019 <- read.csv("VicCoreData.csv")#LOAD DATA created in CN_DATA.R - Containes fumigated samples as well
#Create variable bay (WPB = Western Port Bay sites, PPB = Port Phillip Bay sites)
cn2019$bay <- ifelse(cn2019$site == "Fren"|cn2019$site == "Rhyl"|cn2019$site == "Koow"|
                       cn2019$site == "Warn",
                     "WPB","PPB")

cn2019 <- cn2019 %>%  unite( "site_habitat", c("site","habitat"), sep = "_", remove = F)

#CAR (Carbon Accretion Rate)======
#Join  above MAR data to Master data of cn2019:
cars <-  left_join(cn2019, MAR, by = "site_habitat") %>%
  mutate(CAR_gcm2y = C.percent/100 * MAR)%>% #Convert % into fraction by "/100"
  mutate (Stock = "Belowground")

cars30<- filter(cars, DepthTo.cm <= 30 ) %>%  #till 30 cm deep because that is
  #how deep the Age-dating was done to.
  group_by(habitat) %>%
  summarise(AV = weighted.mean(CAR_gcm2y*100, na.rm = T), #1 g/cm2 = 100 tonnes per hectare
            SD = sd(CAR_gcm2y*100, na.rm = T), 
            N  = n(), 
            SE = SD / sqrt(N))
cars30
cars30$habitat <- factor(cars30$habitat, levels = c("Saltmarsh", "Mangrove","Seagrass"))

ggplot(cars30, aes(habitat, AV))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.2)+
  labs(x= "", y = bquote('Carbon Accretion Rate  ' (Mg*~ha^-1 ~y^-1)))+
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

#bquote('Assimilation  ('*mu~ 'mol' ~t[2]~ cm^-2~y^-1*')') #nice function for special characters!

ggsave(filename = "VIC_Age_600DPI.png", 
       width = 8, 
       height = 5,
       dpi = 600)

#CAR for Reiss, split by two bays======
cars30<- filter(cars, DepthTo.cm <= 30 ) %>%  #till 30 cm deep because that is
  #how deep the Age-dating was done to.
  group_by(habitat, bay) %>%
  summarise(AV = weighted.mean(CAR_gcm2y*100, na.rm = T), #1 g/cm2 = 100 tonnes per hectare
            SD = sd(CAR_gcm2y*100, na.rm = T),
            N  = n(), 
            SE = SD / sqrt(N))

ggplot(cars30, aes(habitat, AV))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.2)+
  labs(x= "", y = bquote('Carbon Accretion Rate  ' (Mg*~ha^-1 ~y^-1)))+
  facet_grid(.~bay)+
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

#Nitrogen=======
#Join Pere's above data to Master data of cn2019 for Reiss:
nars <-  left_join(cn2019, MAR, by = "site_habitat") %>%
  mutate(NAR_gcm2y = N.percent/100 * MAR)%>% #Convert % into fraction by "/100"
  mutate (Stock = "Belowground")

nars30<- filter(nars, DepthTo.cm <= 30 ) %>%  #till 30 cm deep because that is
  #how deep the Age-dating was done to.
  group_by(habitat,bay) %>%
  summarise(AV = weighted.mean(NAR_gcm2y*100, na.rm = T), #1 g/cm2 = 100 tonnes per hectare
            SD = sd(NAR_gcm2y*100, na.rm = T), 
            N  = n(), 
            SE = SD / sqrt(N))
nars30
nars30$habitat <- factor(nars30$habitat, levels = c("Saltmarsh", "Mangrove","Seagrass"))

ggplot(nars30, aes(habitat, AV))  +
  geom_point(aes(color = habitat, size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.2)+
  labs(x= "", y = bquote('Nitrogen Accretion Rate  ' (Mg*~ha^-1 ~y^-1)))+
  facet_grid(.~bay)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

#bquote('Assimilation  ('*mu~ 'mol' ~t[2]~ cm^-2~y^-1*')') #nice function for special characters!

ggsave(filename = "VIC_NAR_600DPI.png", 
       width = 8, 
       height = 5,
       dpi = 600)

