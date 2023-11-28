#--------------------------
# R scripts to quality control ERMN Vegetation Monitoring Data
# Scripts identify missing values, check for logical inconsistencies within and between sampling events, and
# build a species list for each plot based on a range of cycles specified by the user, then compare current event's species 
# list for each plot to find potential identification errors and/or new invasive detections.
# Scripts  output several .csv files that identify errors in the data.
#--------------------------


#-----------------------------
options("scipen"=100, "digits"=10) # Change options to show full TSN number, instead of scientific notation
#-----------------------------
# Load packages used for data scrubbing/queries
library(tidyverse)
library(devtools)
devtools::install_github("sjperles/ERMNforest")  #If this doesn't work, try getting off the VPN!
library(ERMNforest)


rm(list = ls())

#-----------------------------
setwd('C:/ERMN/ERMN_QC_Code')  
#++++++++ NOTE ++++++++ need to add a folder called network_output in your working directory where all files will write to.


# Connect to backed forest database, import data and lookup tables containing species data for the queries that follow
# NOTE - Must use ODBC Administrator 64 bit to Add the target database to the User DSN tab, even if the database has the same name as previous version in same folder.
importData(type='DSN', odbc="ERMNVeg20231018_64m")


# Set up the main table of plots and visits that will be used for a left join at the end of nearly every query to ensure that all plots and visits are included.
park.plots <- joinLocEvent(park='all', QAQC=TRUE, retired=TRUE,
                           anrevisit=TRUE, years=c(2007:2023), output='short')

table(park.plots$Year,park.plots$Unit_Code) # numbers of events match what they should for each year

##### ADD ANY NEW TEST PLOTS TO LIST BELOW #####
park.plots.X1 <- park.plots %>% mutate (Unit_Code1=case_when(Plot_Name == "TEST-188" ~ "DEWA",
                                                                    Plot_Name == "TEST-121" ~ "DEWA",
                                                                    Plot_Name == "TEST-016" ~ "NERI",
                                                                    Plot_Name == "TEST-027" ~ "DEWA",
                                                                    Plot_Name == "TEST-035" ~ "GARI",
                                                                    Plot_Name == "TEST-045" ~ "DEWA",
                                                                    Plot_Name == "TEST-115" ~ "DEWA",
                                                                    Plot_Name == "TEST-134" ~ "NERI",
                                                                    Plot_Name == "TEST-147" ~ "BLUE",
                                                                    Plot_Name == "TEST-159" ~ "NERI",
                                                                    Plot_Name == "TEST-167" ~ "DEWA",
                                                                    Plot_Name == "TEST-211" ~ "NERI",
                                                                    Plot_Name == "TEST-137" ~ "DEWA",
                                                                    Plot_Name == "TEST-319" ~ "DEWA",
                                                                    Plot_Name == "TEST-260" ~ "DEWA",
                                                                    Plot_Name == "TEST-019" ~ "DEWA",
                                                                    Plot_Name == "TEST-063" ~ "BLUE",
                                                                    Plot_Name == "TEST-228" ~ "DEWA",
                                                                    Plot_Name == "TEST-117" ~ "DEWA",
                                                                    Plot_Name == "TEST-198" ~ "GARI",
                                                                    Plot_Name == "TEST-058" ~ "DEWA",
                                                                    Plot_Name == "TEST-148" ~ "NERI",
                                                                    Plot_Name == "TEST-006" ~ "DEWA",
                                                                    Plot_Name == "TEST-054" ~ "DEWA",
                                                                    Plot_Name == "TEST-113" ~ "DEWA",
                                                                    Plot_Name == "TEST-048" ~ "DEWA",
                                                                    Plot_Name == "TEST-191" ~ "DEWA",
                                                                    Plot_Name == "TEST-287" ~ "NERI",
                                                                    Plot_Name == "TEST-298" ~ "DEWA",
                                                                    Plot_Name == "TEST-299" ~ "NERI",
                                                                    Plot_Name == "TEST-305" ~ "DEWA",
                                                                    Plot_Name == "TEST-001" ~ "NONE",
                                                                    Plot_Name == "TEST-002" ~ "NONE"))

park.plots.X2 <- park.plots.X1 %>% mutate (Unit_Code=if_else(!is.na(park.plots.X1$Unit_Code1), Unit_Code1, Unit_Code))

table(park.plots.X2$Year,park.plots.X2$Unit_Code) # numbers of events match what they should for each year


#### TREE DATA ####
tree<-merge(trees,treedata,by="Tree_ID",all.y=T,all.x=T)
tree<-tree[,c("Tree_ID","Tree_Data_ID","Plant_ID","Tree_Number","Event_ID","DBH","Status_ID","Crown_Class_ID","Diameter_Check_ID","Calipers_Used","Notes","QC_DBH_ID")]
notrees<-anti_join(park.plots, tree, by="Event_ID") # Identifies records for plot visits that have no trees (n=13 in 2023)
tree2<-merge(tree,plants[,c("Plant_ID","Latin_name","Common")], by="Plant_ID",all.x=T)
tree3<-merge(park.plots,tree2, all.x=T, by="Event_ID")


#### SAPLING DATA ####
saps1<-merge(micro,saps[,c("Microplot_Sapling_Data_ID", "Microplot_Characterization_Data_ID","Plant_ID","DBH")],by="Microplot_Characterization_Data_ID") #merge tbl_microplot_characterization_data and sapling data tables
saps2<-merge(saps1,plants[,c("Plant_ID","Latin_name","Common")],by="Plant_ID",all.x=T)
saps3<-merge(park.plots,saps2, by="Event_ID",all.x=T) # merge with events


#### SEEDLING DATA ####
seed<-merge(micro,sdlg, by="Microplot_Characterization_Data_ID",all.y=T,all.x=T)
seed2<-merge(seed,plants[,c("Plant_ID","Latin_name","Common")],by="Plant_ID",all.x=T)
seed3<-merge(park.plots[,c("Event_ID","Unit_Code","Plot_Name","Plot_Number","Year")],
             seed2[,c("Plant_ID","Latin_name","Microplot_Characterization_Data_ID","Microplot_ID","Event_ID","Num_Seedlings_5_15cm","Num_Seedlings_15_30cm","Num_Seedlings_30_100cm","Num_Seedlings_100_150cm","Num_Seedlings_Above_150cm")], by="Event_ID",all.x=T)

#### SHRUB DATA ####
shr<-merge(micro,shrub, by="Microplot_Characterization_Data_ID",all.y=T)
shr2<-merge(shr,plants[,c("Plant_ID","Latin_name","Common")],by="Plant_ID",all.x=T)
shr3<-merge(park.plots,shr2[,c("Plant_ID","Latin_name","Common","Microplot_Characterization_Data_ID","Microplot_ID","Event_ID","Cover_Class_ID")],by="Event_ID",all.x=T)


#### QUADRAT DATA ####
herb0<-merge(quads,plants[,c("Plant_ID","Latin_name","Common","Tree","Herbaceous","Vine","Shrub","Graminoid","Fern",
                              "NJ_Pd_Nativ","PA_Glac_Nativ","PA_Mt_Nativ","WV_Mt_Nativ","Invasive")], by="Plant_ID",all.x=T)
herb1<-merge(park.plots,herb0,by="Event_ID",all.x=T)


#### ADDITIONAL PLOT SPECIES ####
spp<-merge(addspp[,c("Event_ID","Plant_ID")],plants[,c("Plant_ID","Latin_name","Common")],by="Plant_ID", all.x=T)
spp1<-merge(park.plots,spp,by="Event_ID",all.x=T)


#### CREATE ALL SPECIES LIST BY PLOT COMBINING TREES, SAPLINGS, SEEDLINGS, SHRUBS, QUADRATS, ADD SPECIES####
#+++++++++++++++++++++++++++++++++++
allsplist1<-tree2[,c("Event_ID","Latin_name","Plant_ID")]
allsplist2<-saps2[,c("Event_ID","Latin_name","Plant_ID")]
allsplist3<-seed2[,c("Event_ID","Latin_name","Plant_ID")]
allsplist4<-shr2[,c("Event_ID","Latin_name","Plant_ID")]
allsplist5<-herb0[,c("Event_ID","Latin_name","Plant_ID")]
allsplist6<-spp[,c("Event_ID","Latin_name","Plant_ID")]
allsplist<-rbind(allsplist1,allsplist2,allsplist3,allsplist4,allsplist5,allsplist6)

unique.splist<-allsplist %>% distinct(Event_ID, Latin_name, .keep_all = TRUE)

plot.all.splist<-merge(park.plots.X2,unique.splist,by="Event_ID",all.x=T)

park.splist_ALL<-plot.all.splist %>% distinct(Unit_Code, Latin_name, .keep_all = FALSE) %>% arrange(Unit_Code,Latin_name)
write.csv(park.splist_ALL,'./Park_Sp_List_ALL.csv', row.names = FALSE) #Complete species list by PARK for ALL events.

