#' @include joinLocEvent.R
#' @title joinAddPlotSp: compiles additional plot species data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by rename_at
#' @importFrom magrittr %>%
#'
#' @description This function combines species data collected during plot walk-around with species names and allows you to filter on species types, park, and years.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"unknown"}{Returns only species of unknown nativity, plants identified only to genus or family}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#'
#' @param GrowthForm Allows you to filter by tree, shrub, herb, fern, graminoid, vine, or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"tree"}{Returns tree species only}
#' \item{"shrub"}{Returns shrub species only}
#' \item{"herb"}{Returns herbaceous species only}
#' \item{"gram"}{Returns graminoid species only}
#' \item{"fern"}{Returns fern species only}
#' \item{"vine"}{Returns vine species only}
#' }
#'
#' @return Returns a dataframe with species richness from plot walk around based on inputs.
#'
#' @examples
#' importData()
#' # compile quadrat data for invasive species in ALPO for all years
#' SARA_quads <- joinQuadData(park = 'ALPO', speciesType = 'invasive')
#'
#' # compile native species only for all parks in most recent survey
#' native_quads <- joinQuadData(speciesType = 'native', years = c(2014:2018))
#'
#' @export
#'
#------------------------
# Joins additional plot species table with plant look-up and filters by park, year, and plot/visit type
# Should not select data from 2007 since quadrat protocol was different and not compatible with later years
#------------------------
joinAddPlotSp<-function(speciesType=c('all', 'native', 'exotic', 'unknown', 'invasive'),
                       GrowthForm=c('all', 'tree', 'shrub', 'herb', 'gram', 'fern', 'vine'),
                       park='all',years=2008:2023,
                       QAQC=FALSE, rejected=FALSE, anrevisit=FALSE, output, ...){

  speciesType<-match.arg(speciesType)
  GrowthForm<-match.arg(GrowthForm)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,rejected = rejected,
                                 anrevisit = anrevisit, output = 'short'))


  addspp1a<-merge(addspp,plants[,c("Plant_ID","Latin_name","Tree","Herbaceous","Vine","Shrub","Graminoid","Fern",
                                   "NJ_Pd_Nativ","PA_Glac_Nativ","PA_Mt_Nativ","WV_Mt_Nativ","Invasive")], by="Plant_ID",all.x=T)

  addsp1<-subset(addspp1a, Plant_ID<9990)
  addspp1$Pres <- 1
  addspp2<-merge(park.plots,addspp1, by="Event_ID", all.y=T)
  addspp3<-subset(addspp2,!is.na(Unit_Code))

  # Create nativity by park based on ecoregion. DEWA using PA Glaciated by default
  addspp4 <- addspp3 %>% mutate (Nativity1 = if_else(Unit_Code == "DEWA",PA_Glac_Nativ,
                                                     if_else(Unit_Code == "ALPO",PA_Mt_Nativ,
                                                             if_else(Unit_Code == "JOFL",PA_Mt_Nativ,
                                                                     if_else(Unit_Code == "FONE",PA_Mt_Nativ,
                                                                             if_else(Unit_Code == "FRHI",PA_Mt_Nativ,
                                                                                     if_else(Unit_Code == "NERI", WV_Mt_Nativ,
                                                                                             if_else(Unit_Code == "BLUE", WV_Mt_Nativ,
                                                                                                     if_else(Unit_Code == "GARI", WV_Mt_Nativ, "NONE")))))))))

  addspp5 <- addspp4 %>% mutate (Nativity2 = if_else(Unit_Code == "DEWA" & is.na(Nativity1),NJ_Pd_Nativ,Nativity1))
  addspp6 <- addspp5 %>% mutate (Nativity3 = if_else(Unit_Code == "DEWA" & is.na(Nativity2),PA_Mt_Nativ,Nativity2))

  addspp6$Nativity3[is.na(addspp6$Nativity3)] <- "Unknown"
  addspp7 <- addspp6 %>% mutate (Nativity = if_else(Nativity3 == "maybe exotic","exotic",
                                                    if_else(Nativity3 == "maybe native", "native",Nativity3)))


  # Create single column for growth form
  addspp8 <- addspp7 %>% mutate (GrowthForm = if_else(Tree == TRUE,"tree",
                                                      if_else(Shrub == TRUE,"shrub",
                                                              if_else(Herbaceous == TRUE, "herb",
                                                                      if_else(Vine == TRUE, "vine",
                                                                              if_else(Graminoid == TRUE, "gram",
                                                                                      if_else(Fern == TRUE, "fern","Unknown")))))))

  # Create final file for selecting and summarizing
  plot.addspp <- addspp8[,c("Event_ID","Location_ID", "Unit_Code","Plot_Number","Plot_Name","Panel","Year",
                            "Pres", "Plant_ID", "Latin_name", "Nativity","Invasive","GrowthForm")]


    # Summarize additional plot species data
  plot.addspp<-if (speciesType=='native'){filter(plot.addspp,Nativity=="native")
  } else if (speciesType=='exotic'){filter(plot.addspp,Nativity=="exotic")
  } else if (speciesType=='unknown'){filter(plot.addspp,Nativity=="Unknown")
  } else if (speciesType=='invasive'){filter(plot.addspp,Invasive==TRUE)
  } else if (speciesType=='all'){(plot.addspp)
  }

  plot.addspp<-if (GrowthForm=='tree'){filter(plot.addspp,GrowthForm=="tree")
  } else if (GrowthForm=='shrub'){filter(plot.addspp,GrowthForm=="shrub")
  } else if (GrowthForm=='herb'){filter(plot.addspp,GrowthForm=="herb")
  } else if (GrowthForm=='gram'){filter(plot.addspp,GrowthForm=="gram")
  } else if (GrowthForm=='vine'){filter(plot.addspp,GrowthForm=="vine")
  } else if (GrowthForm=='fern'){filter(plot.addspp,GrowthForm=="fern")
  } else if (GrowthForm=='all'){(plot.addspp)
  }


  # Summarizing additional plot species data
  plot.addsp.rich <- plot.addspp %>% group_by (Unit_Code,Plot_Number,Plot_Name,Panel,Year) %>%
    summarise(plot.add.rich = sum(Pres)) %>% arrange(Plot_Name, Year)

  return(data.frame(plot.addsp.rich))

} # end of function
