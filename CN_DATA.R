#Functions======
#Load these packages into your working environment
library("readxl")
library("tidyverse")
#IMPORTANT:YOUR SAMPLE_ID (I named it CNCode) should be consistent across all your spreadsheets.

#LOAD RAW FILES:
#Specify path to folder where you store youre CN spreadsheets and list them all:
files <- list.files(path = "C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/BCL/VicCorCampaign/VicCorCampaign/CN_DATA_12jun19",
                    pattern = "*.xls", full.names = T)

#Create function to export data from sheet = 1 (Sample Table)
ReadCN_smp <- function(x) read_xlsx (path = x,
                                     sheet = 1,
                                     skip = 7,
                                     range = cell_cols("C:E"))

#Export "Sample Table" data from all files in your folder:
tbl1 <- sapply(files, ReadCN_smp, simplify=FALSE) %>%
         bind_rows(.id = "id1")

tbl1
#Create function to export data from sheet = 2 (Element%)
ReadCN <- function(x) read_xlsx (path = x,sheet = 2,skip = 7, range = cell_cols("C:D"))

#Export "Element%" data from all files in your folder using sapply:
tbl2 <- sapply(files, 
               ReadCN, simplify=FALSE) %>% 
  bind_rows(.id = "id2")

tbl2

#Bind sample (tbl1) and CN (tbl2) data together
CN_DATA <- cbind(tbl1,tbl2)#bind smp and CN data

#Double check if data alligns well:
all.equal(tbl1$id1,tbl2$id2) #should be TRUE!

#Clean up the file to keep data you need in a form you/R likes (no special signs):
CN_DATA_clean <- CN_DATA %>%
                 filter(Type == "Smp") %>%
  select("id1", "Sample Name","Weight", "(N) %", "(C) %" ) %>%
  rename(file = "id1", CNCode = "Sample Name", Weight.mg = "Weight",
         N.percent = "(N) %", C.percent = "(C) %") %>%
  
  #Samples with shells were CN-assessed before fumigation, so we need to get them out:
  mutate( fumigated = ifelse(grepl("Fumigated",CN_DATA_clean$file),'yes','no')) 
dim(CN_DATA_clean) #537   6

with_shells <- CN_DATA_clean %>%
             filter (fumigated == "yes" )
dim(with_shells)# 145   6 that many samples contained shells (inorganic carbon)

cut_out_with_shells <- with_shells$CNCode #

no_shells <- CN_DATA_clean %>% 
  filter( ! CNCode %in% cut_out_with_shells)

dim(no_shells) #251

CN_DATA_clean_shells <- rbind(no_shells, with_shells)
#Check for duplicates:
anyDuplicated(CN_DATA_clean_shells$CNCode)#Should be 0!!! #If any, most likely samples were run more than once

#If not 0, some samples are dupliacted,
#You have to decide what to do with duplicates (e.g., average them, remove them)
#Draw meam of all samples:
#CN_DATA_clean2 <- CN_DATA_clean_shells %>% 
  select(C.percent, N.percent, file, CNCode) %>%
  group_by(CNCode) %>%
  summarise(C.percent = mean (C.percent),
            N.percent = mean (N.percent))
#Check for duplicates again:
#anyDuplicated(CN_DATA_clean2$CNCode)


#Merge new CN data with your MASTER file:
MASTER_DATA <- read.csv("CN.csv")
NewDATA     <- left_join(MASTER_DATA, CN_DATA_clean_shells, by = "CNCode")

dim(MASTER_DATA)#2233 rows   44 cols
dim(NewDATA)#2233 rows  49 cols
#write.csv(NewDATA, file = "CN_2019CoringCampaign.csv", row.names = F)