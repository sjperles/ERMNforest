#' @include joinLocEvent.R
#' @title joinQuadData: compiles quadrat species data by plot
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by rename_at
#' @importFrom magrittr %>%
#'
#' @description This function combines quadrat species data with species names and allows you to filter on species types, park, and years. ???Note that the Shrub guild also includes woody vine species.???
#'
#' @param park Combine data from all parks or one park at a time. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"NERI"}{New River Gorge NPP only}
#' \item{"GARI"}{Gauley River NRA NHP only}
#' \item{"BLUE"}{Bluestone NSR only}
#' \item{"WV"}{NERI, GARI, and BLUE only}
#' \item{"ALPO"}{Allegheny Portage Railroad NHS only}
#' \item{"FONE"}{Fort Necessity NB only}
#' \item{"FRHI"}{Friendship Hill NHS only}
#' \item{"JOFL"}{Johnstown Flood NM only}
#' \item{"WEPA"}{ALPO, JOFL, FONE, and FRHI only}
#' \item{"FLNI"}{Flight 93 NM only}
#' \item{"DEWA"}{Delaware Water Gap NRA only}}
#'
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#'
#' @param retired Allows you to remove (FALSE) or include (TRUE) retired plots.
#' \describe{
#' \item{FALSE}{Only returns plots that are active}
#' \item{TRUE}{Default. returns all active and retired plots}}
#'
#' @param anrevisit Allows you to remove (FALSE) or include (TRUE) annual revisits from 2008 - 2011.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were sampled on 4 year cycle, does not include annual revisits.}
#' \item{TRUE}{returns all records}}
#'
#' @param years Allows you to select individual years from 2007 to 2023. Default is all years.
#' If more than one year is selected, specify by c(2007:2018), for example.
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
#' @return Returns a dataframe with average cover and species richness across quadrats and total plot species richness.
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
# Joins quadrat tables and filters by park, year, and plot/visit type
# Should not select data from 2007 since quadrat protocol was different and not compatible with later years
#------------------------
joinQuadData<-function(speciesType=c('all', 'native', 'exotic', 'unknown', 'invasive'),
                       GrowthForm=c('all', 'tree', 'shrub', 'herb', 'gram', 'fern', 'vine'),
                       park='all',years=2008:2024,
                       QAQC=FALSE, retired=TRUE, anrevisit=FALSE, output, ...){

  park <- match.arg(park)
  speciesType<-match.arg(speciesType)
  GrowthForm<-match.arg(GrowthForm)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC, retired = retired,
                                 anrevisit = anrevisit, output = 'short'))


  # Creates number of quadrats sampled per event
  quadsamp<-quadchr[,c(1,2,3,18)]
  quadsamp2<-aggregate(Quad_Sp_Sample ~ Event_ID, quadsamp, sum)
  quadsamp3<-merge(park.plots,quadsamp2, by="Event_ID", all.x=T)
  quadsamp4 <- subset(quadsamp3, Quad_Sp_Sample>0)

  # Format quadrat data
  herb1 <- quads
  herb1[,4:15][herb1[,4:15]==1] <-0.1 # convert cover classes to midpoints for all 12 quadrats
  herb1[,4:15][herb1[,4:15]==2] <-1.5
  herb1[,4:15][herb1[,4:15]==3] <-3.5
  herb1[,4:15][herb1[,4:15]==4] <-7.5
  herb1[,4:15][herb1[,4:15]==5] <-17.5
  herb1[,4:15][herb1[,4:15]==6] <-37.5
  herb1[,4:15][herb1[,4:15]==7] <-62.5
  herb1[,4:15][herb1[,4:15]==8] <-85
  herb1[,4:15][herb1[,4:15]==9] <-97.5
  herb1[,4:15][herb1[,4:15]==999999] <-0

  herb3 <- herb1[,2:17]

  herb4 <- herb3 %>% pivot_longer(cols = starts_with("q"),
                                  names_to = "QuadratID",
                                  values_to = "Cover",
                                  values_drop_na = FALSE)

  herb4a <- herb4[,c("Event_ID","Plant_ID","QuadratID","Cover")]
  herb4b <- herb4a %>% mutate (Pres = if_else(Cover>0,1,0))


  herb11<-merge(herb4b,plants[,c("Plant_ID","Latin_name","Tree","Herbaceous","Vine","Shrub","Graminoid","Fern",
                                 "NJ_Pd_Nativ","PA_Glac_Nativ","PA_Mt_Nativ","WV_Mt_Nativ","Invasive")], by="Plant_ID",all.x=T)
  park.herb1<-merge(park.plots,herb11, by="Event_ID", all.y=T)
  park.herb2<-subset(park.herb1,!is.na(Unit_Code))

  # Create nativity by park based on ecoregion. DEWA using PA Glaciated by default
  park.herb3 <- park.herb2 %>% mutate (Nativity1 = if_else(Unit_Code == "DEWA",PA_Glac_Nativ,
                                                           if_else(Unit_Code == "ALPO",PA_Mt_Nativ,
                                                                   if_else(Unit_Code == "JOFL",PA_Mt_Nativ,
                                                                           if_else(Unit_Code == "FONE",PA_Mt_Nativ,
                                                                                   if_else(Unit_Code == "FRHI",PA_Mt_Nativ,
                                                                                           if_else(Unit_Code == "FLNI",PA_Mt_Nativ,
                                                                                              if_else(Unit_Code == "NERI", WV_Mt_Nativ,
                                                                                                   if_else(Unit_Code == "BLUE", WV_Mt_Nativ,
                                                                                                           if_else(Unit_Code == "GARI", WV_Mt_Nativ, "NONE"))))))))))

  park.herb3a <- park.herb3 %>% mutate (Nativity2 = if_else(Unit_Code == "DEWA" & is.na(Nativity1),NJ_Pd_Nativ,Nativity1))
  park.herb4 <- park.herb3a %>% mutate (Nativity3 = if_else(Unit_Code == "DEWA" & is.na(Nativity2),PA_Mt_Nativ,Nativity2))

  park.herb4$Nativity3[is.na(park.herb4$Nativity3)] <- "unknown"
  park.herb5 <- park.herb4 %>% mutate (Nativity = if_else(Nativity3 == "maybe exotic","exotic",
                                                          if_else(Nativity3 == "mixed","unknown",
                                                              if_else(Nativity3 == "maybe native", "native",Nativity3))))


  # Create single column for growth form
  park.herb7 <- park.herb5 %>% mutate (GrowthForm = if_else(Tree == TRUE,"tree",
                                                            if_else(Shrub == TRUE,"shrub",
                                                                    if_else(Herbaceous == TRUE, "herb",
                                                                            if_else(Vine == TRUE, "vine",
                                                                                    if_else(Graminoid == TRUE, "gram",
                                                                                            if_else(Fern == TRUE, "fern","unknown")))))))


  # Create final file for selecting and summarizing
  park.herb8 <- park.herb7[,c("Event_ID","Location_ID", "Unit_Code","Plot_Number","Panel","Year",
                          "Plot_Name", "Plant_ID", "QuadratID", "Cover", "Pres", "Latin_name", "Nativity","Invasive","GrowthForm")]


  # Summarize quadrat data
  park.herb9<-if (speciesType=='native'){filter(park.herb8,Nativity=="native")
  } else if (speciesType=='exotic'){filter(park.herb8,Nativity=="exotic")
  } else if (speciesType=='unknown'){filter(park.herb8,Nativity=="unknown")
  } else if (speciesType=='invasive'){filter(park.herb8,Invasive==TRUE)
  } else if (speciesType=='all'){(park.herb8)
  }

  park.herb10<-if (GrowthForm=='tree'){filter(park.herb9,GrowthForm=="tree")
  } else if (GrowthForm=='shrub'){filter(park.herb9,GrowthForm=="shrub")
  } else if (GrowthForm=='herb'){filter(park.herb9,GrowthForm=="herb")
  } else if (GrowthForm=='gram'){filter(park.herb9,GrowthForm=="gram")
  } else if (GrowthForm=='vine'){filter(park.herb9,GrowthForm=="vine")
  } else if (GrowthForm=='fern'){filter(park.herb9,GrowthForm=="fern")
  } else if (GrowthForm=='all'){(park.herb9)
  }


  # Summarizing quadrat data
  plot.herb1 <- subset(park.herb10, Pres > 0)
  plot.herb2 <- plot.herb1 %>% distinct(Event_ID, Latin_name, .keep_all = TRUE)
  plot.sp.rich <- plot.herb2 %>% group_by (Event_ID) %>% summarise(plot.sp.rich = sum(Pres))
  # Total species richness in plot from quadrats

  herb5 <- park.herb10 %>% group_by (Event_ID, QuadratID) %>% summarise(q.tot.cov = sum(Cover),
                                                                      q.sp.rich = sum(Pres)) %>% ungroup()
  # Total quadrat cover and richness in herb5


  herb6 <- herb5 %>% group_by (Event_ID) %>% summarise (sum.q.cov = sum(q.tot.cov),
                                                        sum.q.rich = sum(q.sp.rich))
  herb7 <- merge (herb6, quadsamp4, by="Event_ID", all.y=T)
  herb7$ave.q.cov = (herb7$sum.q.cov/herb7$Quad_Sp_Sample)
  herb7$ave.q.rich = (herb7$sum.q.rich/herb7$Quad_Sp_Sample)
  # Plot-wide Average quadrat cover and richness

  herb8 <- merge(herb7, plot.sp.rich, by="Event_ID", all.x=T)
  herb8[is.na(herb8)] <- 0

  quads.final <- herb8[,c("Location_ID", "Unit_Code", "Plot_Name", "Plot_Number", "X_Coord", "Y_Coord", "Panel",
                          "Year", "Event_ID", "Event_QAQC", "Cycle", "Vegetation_Domain", "Quad_Sp_Sample", "ave.q.cov", "ave.q.rich",
                          "sum.q.cov", "sum.q.rich", "plot.sp.rich")] %>% arrange(Plot_Name, Year)

  return(data.frame(quads.final))

  } # end of function

