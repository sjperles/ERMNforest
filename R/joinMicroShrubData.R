#' @include joinLocEvent.R
#' @title joinMicroShrubData: compiles shrub data collected in microplots
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by
#' @importFrom magrittr %>%
#'
#' @description This function combines shrub percent cover data from microplots. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return returns a dataframe with shrub data collected in microplots
#'
#' @examples
#' importData()
#' # native shrubs in NERI all years
#' native_shrubs <- joinMicroShrubData(park ='NERI', speciesType = 'native')
#'
#' # all parks with exotic shrubs in most recent survey
#' exotic_shrubs <- joinMicroShrubData(years = c(2014,2018), speciesType = 'exotic')
#'
#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinMicroShrubData<-function(speciesType = c('all', 'native','exotic'), park='all',
                             years=2007:2023, QAQC=FALSE, rejected=FALSE, anrevisit=FALSE, output, ...){

  speciesType<-match.arg(speciesType)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,rejected = rejected,
                                 anrevisit = anrevisit, output = 'short'))

  # Prepare the shrub data
  shrub1 <- merge(micro,shrub, by="Microplot_Characterization_Data_ID",all.y=T,all.x=T)
  shrub2 <- merge(park.plots,shrub1,by='Event_ID',all.x=T)
  shrub3 <- merge(shrub2,plants[,c("Plant_ID","Latin_name","Common","Canopy","PA_Glac_Nativ",
                                   "PA_Mt_Nativ","WV_Mt_Nativ","NJ_Pd_Nativ")], by='Plant_ID',all.x=T)

  # Change cover classes to midpoint
  shrub4 <- shrub3 %>% mutate(cover=
      case_when(Cover_Class_ID == 1 ~ 0.1,
        Cover_Class_ID == 2 ~ 1.5,
        Cover_Class_ID == 3 ~ 3.5,
        Cover_Class_ID == 4 ~ 7.5,
        Cover_Class_ID == 5 ~ 17.5,
        Cover_Class_ID == 6 ~ 37.5,
        Cover_Class_ID == 7 ~ 62.5,
        Cover_Class_ID == 8 ~ 85,
        Cover_Class_ID == 9 ~ 97.5,
        Cover_Class_ID == 0 ~ 0))

  # Ensure not sampled and no species observed have cover of zero and presence of zero.
  shrub5 <- shrub4 %>% mutate(cover=ifelse(Plant_ID>9990,0,cover),
                            present=ifelse(cover>0,1,0))

  # Deal with Nativity
  shrub6 <- shrub5 %>% mutate (Nativity1 = if_else(Unit_Code == "DEWA",PA_Glac_Nativ,
                                                   if_else(Unit_Code == "ALPO",PA_Mt_Nativ,
                                                           if_else(Unit_Code == "JOFL",PA_Mt_Nativ,
                                                                   if_else(Unit_Code == "FONE",PA_Mt_Nativ,
                                                                           if_else(Unit_Code == "FRHI",PA_Mt_Nativ,
                                                                                   if_else(Unit_Code == "NERI", WV_Mt_Nativ,
                                                                                           if_else(Unit_Code == "BLUE", WV_Mt_Nativ,
                                                                                                   if_else(Unit_Code == "GARI", WV_Mt_Nativ, "NONE")))))))))

  shrub7 <- shrub6 %>% mutate (Nativity2 = if_else(Unit_Code == "DEWA" & is.na(Nativity1),NJ_Pd_Nativ,Nativity1))
  shrub8 <- shrub7 %>% mutate (Nativity3 = if_else(Unit_Code == "DEWA" & is.na(Nativity2),PA_Mt_Nativ,Nativity2))

  shrub8$Nativity3[is.na(shrub8$Nativity3)] <- "Unknown"
  shrub9 <- shrub8 %>% mutate (Nativity = if_else(Nativity3 == "maybe exotic","exotic",
                                                  if_else(Nativity3 == "maybe native", "native",Nativity3)))

  shrub10 <- if (speciesType=='native'){filter(shrub9,Nativity == "native")
  } else if (speciesType=='exotic'){filter(shrub9,Nativity == "exotic")
  } else if (speciesType=='all'){(shrub9)
  }

  #Summarize by plot
  shrub11 <- shrub10 %>% group_by(Event_ID) %>%
    summarise(m.freq = sum(present), tot.cover = sum(cover))

  # Determine number of microplots sampled at each event
  plot.micro<-merge(park.plots, micro, by="Event_ID", all.x=T)

  micro.samp1 <- plot.micro %>% mutate(MSamp = ifelse(Nonvascular_Cover_Class_ID==999999 & Vine_Cover_Class_ID==999999 & Graminoid_Cover_Class_ID==999999
                                                      & Fern_Cover_Class_ID==999999 & Herbaceous_Cover_Class_ID==999999,0,1))

  micro.samp<-micro.samp1 %>% group_by(Event_ID,Unit_Code,Plot_Name,Cycle,Year) %>% summarise(MSamp=sum(MSamp)) #NUMBER OF MICROPLOTS SAMPLED PER EVENT!


  # Merge selected data with micro.samp
  shrub12 <- merge(micro.samp,shrub11,by="Event_ID",all.x=T, all.y=T)
  shrub12[,8:9][is.na(shrub12[,8:9])]<-0
  shrub13 <- shrub12 %>% mutate(ave.sp.rich = (m.freq/MSamp),
                                ave.cover = (tot.cover/MSamp))

  shrub13 <- shrub13 %>% arrange(Plot_Name, Year)

  return(data.frame(shrub13))
} # end of function

