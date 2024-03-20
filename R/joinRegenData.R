#' @include joinLocEvent.R
#' @title joinRegenData: compiles seedling and sapling data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub str_pad
#'
#' @description This function combines seedling and sapling data, and calculates stocking index. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only, must select canopyForm='all'}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#' @param canopyForm Allows you to filter on only canopy-forming species or include all species.
#' \describe{
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns native canopy-forming species only}
#'}
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"micro"}{Default. Returns seedling and sapling densities per microplot.}
#' \item{"ha"}{Returns seedling and sapling densities per hectare}
#' \item{"acres"}{Returns densities per acre}
#'}
#'
#'
#' @return returns a dataframe with seedling and sapling densities, and stocking index
#'
#' @examples
#' # compile seedling and sapling data for all parks and all species in most recent cycle
#' regen_data <- joinRegenData(canopyForm = 'all', years = c(2017:2022))
#'
#' # compile regen data for only canopy-forming (default) and native species in FONE for all years
#' FONE_regen <- joinRegenData(park = 'FONE', speciesType = 'native')
#'

#' @export
#'
#------------------------
# Joins microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData<-function(speciesType=c('all', 'native','exotic','invasive'), canopyForm=c('canopy','all'),
  units=c('micro','ha','acres'), park='all',years=2007:2023, QAQC=FALSE, rejected=FALSE, anrevisit=FALSE, output, ...){

  speciesType<-match.arg(speciesType)
  canopyForm<-match.arg(canopyForm)
  units<-match.arg(units)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,rejected = rejected,
                                 anrevisit = anrevisit, output = 'short'))

# Prepare the seedling data
  seed1 <- merge(micro,sdlg, by="Microplot_Characterization_Data_ID",all.y=T,all.x=T)
  seed <- seed1 %>% mutate(seedht0=Num_Seedlings_5_15cm,
                 seedht1=Num_Seedlings_15_30cm,
                 seedht2=Num_Seedlings_30_100cm,
                 seedht3=Num_Seedlings_100_150cm,
                 seedht4=Num_Seedlings_Above_150cm)

  # Prepare the sapling data
  sap1 <- merge(micro,saps, by="Microplot_Characterization_Data_ID", all.x=T)
  sap2 <- sap1 %>% mutate (pres.sap = ifelse(DBH>0,1,0), BA_cm2 = (((DBH / 2)^2)*3.1415926535))

  sap <-sap2 %>% group_by(Event_ID,Plant_ID, Microplot_ID) %>%
    summarise(sap.stems=sum(pres.sap, na.rm=T),tot.sap.ba.cm2=sum(BA_cm2, na.rm=T))

  # Combine seedling and sapling data
  regen1<-merge(seed[,c("Event_ID","Microplot_ID","Plant_ID",
                        "seedht0","seedht1","seedht2","seedht3","seedht4" )],
                sap[,c("Event_ID","Plant_ID","Microplot_ID","sap.stems","tot.sap.ba.cm2")],
                by=c("Event_ID","Plant_ID", "Microplot_ID"),all.x=T,all.y=T)
  regen1[,4:10][is.na(regen1[,4:10])]<-0

  regen2<-merge(park.plots,regen1,by='Event_ID', all.x=T,all.y=F)

  regen3<-merge(regen2,plants[,c("Plant_ID","Latin_name","Common","Canopy","PA_Glac_Nativ",
                                 "PA_Mt_Nativ","WV_Mt_Nativ","NJ_Pd_Nativ","Invasive")], by='Plant_ID',all.x=T)

  # Deal with Nativity
  regen4 <- regen3 %>% mutate (Nativity1 = if_else(Unit_Code == "DEWA",PA_Glac_Nativ,
                                                   if_else(Unit_Code == "ALPO",PA_Mt_Nativ,
                                                           if_else(Unit_Code == "JOFL",PA_Mt_Nativ,
                                                                   if_else(Unit_Code == "FONE",PA_Mt_Nativ,
                                                                           if_else(Unit_Code == "FRHI",PA_Mt_Nativ,
                                                                                   if_else(Unit_Code == "NERI", WV_Mt_Nativ,
                                                                                           if_else(Unit_Code == "BLUE", WV_Mt_Nativ,
                                                                                                   if_else(Unit_Code == "GARI", WV_Mt_Nativ, "NONE")))))))))

  regen5 <- regen4 %>% mutate (Nativity2 = if_else(Unit_Code == "DEWA" & is.na(Nativity1),NJ_Pd_Nativ,Nativity1))
  regen6 <- regen5 %>% mutate (Nativity3 = if_else(Unit_Code == "DEWA" & is.na(Nativity2),PA_Mt_Nativ,Nativity2))

  regen6$Nativity3[is.na(regen6$Nativity3)] <- "Unknown"
  regen7 <- regen6 %>% mutate (Nativity = if_else(Nativity3 == "maybe exotic","exotic",
                                                  if_else(Nativity3 == "maybe native", "native",Nativity3)))


  regen8<-if(canopyForm=='canopy'){filter(regen7, Canopy==TRUE)
  } else if(canopyForm=='all'){(regen7)
  }

  regen9<- if (speciesType=='native') {filter(regen8, Nativity == "native")
  } else if (speciesType=='exotic') {filter(regen8, Nativity == "exotic")
  } else if (speciesType=='invasive'){filter(regen8,Invasive==TRUE)
  } else if (speciesType=='all'){(regen8)
  }


  # Summarise data at plot level
  regen10 <- regen9 %>% mutate (total = (seedht0+seedht1+seedht2+seedht3+seedht4),
                                totU15 = (seedht1+seedht2+seedht3+seedht4),
                                totU30 = (seedht2+seedht3+seedht4),
                                stock5u = (seedht0+seedht1+(seedht2*2)+(seedht3*20)+(seedht4*50)+(sap.stems*50)),
                                stock15u = (seedht1+(seedht2*2)+(seedht3*20)+(seedht4*50)+(sap.stems*50)))

  regen11 <- regen10 %>% group_by(Event_ID,Unit_Code,Plot_Name,Cycle,Year) %>% summarise (plot.seedht0 = sum(seedht0),
                                                                                          plot.seedht1 = sum(seedht1),
                                                                                          plot.seedht2 = sum(seedht2),
                                                                                          plot.seedht3 = sum(seedht3),
                                                                                          plot.seedht4 = sum(seedht4),
                                                                                          plot.total = sum(total),
                                                                                          plot.U15 = sum(totU15),
                                                                                          plot.U30 = sum(totU30),
                                                                                          plot.stock5u = sum(stock5u),
                                                                                          plot.stock15u = sum(stock15u),
                                                                                          plot.sapstems = sum(sap.stems),
                                                                                          plot.sapBAcm2 = sum(tot.sap.ba.cm2))


  # Determine number of microplots sampled at each event
  plot.micro<-merge(park.plots, micro, by="Event_ID", all.x=T)

  micro.samp1 <- plot.micro %>% mutate(MSamp = ifelse(Nonvascular_Cover_Class_ID==999999 & Vine_Cover_Class_ID==999999 & Graminoid_Cover_Class_ID==999999
                                                      & Fern_Cover_Class_ID==999999 & Herbaceous_Cover_Class_ID==999999,0,1))

  micro.samp2<-micro.samp1 %>% group_by(Event_ID,Unit_Code,Plot_Name,Cycle,Year) %>% summarise(MSamp=sum(MSamp)) #NUMBER OF MICROPLOTS SAMPLED PER EVENT!
  micro.samp<-micro.samp2 %>% filter(MSamp>0)


  # Calculate plot-average among microplots sampled
  regen12 <- merge(micro.samp, regen11, by=c("Event_ID","Unit_Code","Plot_Name","Cycle","Year"), all.x=T)

  regen13 <- regen12 %>% mutate (ave.seedht0 = (plot.seedht0/MSamp),
                                 ave.seedht1 = (plot.seedht1/MSamp),
                                 ave.seedht2 = (plot.seedht2/MSamp),
                                 ave.seedht3 = (plot.seedht3/MSamp),
                                 ave.seedht4 = (plot.seedht4/MSamp),
                                 ave.total = (plot.total/MSamp),
                                 ave.U15 = (plot.U15/MSamp),
                                 ave.U30 = (plot.U30/MSamp),
                                 ave.stock5u = (plot.stock5u/MSamp),
                                 ave.stock15u = (plot.stock15u/MSamp),
                                 ave.sapstems = (plot.sapstems/MSamp),
                                 ave.sapBAcm2 = (plot.sapBAcm2/MSamp))
  regen13[,6:20][is.na(regen13[,6:20])]<-0


  regen14<-if (units=='ha'){
    regen13 %>%
      mutate(seedht0.dens = (ave.seedht0*10000)/(pi*4),
             seedht1.dens = (ave.seedht1*10000)/(pi*4),
             seedht2.dens = (ave.seedht2*10000)/(pi*4),
             seedht3.dens = (ave.seedht3*10000)/(pi*4),
             seedht4.dens = (ave.seedht4*10000)/(pi*4),
            tot.seed.dens = (ave.total*10000)/(pi*4),
            U15.seed.dens = (ave.U15*10000)/(pi*4),
            U30.seed.dens = (ave.U30*10000)/(pi*4),
            sap.dens = (ave.sapstems*10000)/(pi*4),
            sap.BA.m2 = ave.sapBAcm2/(pi*4),
            ave.stock5u = ave.stock5u,
            ave.stock15u = ave.stock5u)
  } else if (units=='acres'){
    regen13 %>%
      mutate(seedht0.dens = (ave.seedht0*4046.856)/(pi*4),
             seedht1.dens = (ave.seedht1*4046.856)/(pi*4),
             seedht2.dens = (ave.seedht2*4046.856)/(pi*4),
             seedht3.dens = (ave.seedht3*4046.856)/(pi*4),
             seedht4.dens = (ave.seedht4*4046.856)/(pi*4),
             tot.seed.dens = (ave.total*4046.856)/(pi*4),
             U15.seed.dens = (ave.U15*4046.856)/(pi*4),
             U30.seed.dens = (ave.U30*4046.856)/(pi*4),
             sap.dens = (ave.sapstems*4046.856)/(pi*4),
             sap.BA.ft2 = (ave.sapBAcm2*4.356)/(pi*4),
             ave.stock5u = ave.stock5u,
             ave.stock15u = ave.stock5u)
  } else if (units=='micro'){regen13
  }

  regen14<-regen14 %>% arrange(Plot_Name,Year)
  return(data.frame(regen14))
} # end of function

