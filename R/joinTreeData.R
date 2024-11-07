#' @include joinLocEvent.R
#' @title joinTreeData: compiles tree data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by
#' @importFrom magrittr %>%
#'
#' @description This function combines location and event-level Tree data. Must run importData first.
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
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all standing trees}
#' \item{"live"}{live trees only}
#' \item{"dead"}{dead trees only}
#' }
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' \item{"invasive"}{Returns species on the Indicator Invasive List}
#' }
#'
#'@param canopyPosition Allows you to filter on tree crown class
#' \describe{
#' \item{"all"}{Returns all canopy positions}
#' \item{"canopy"}{Returns only species that are considered native canopy-forming trees as defined in Canopy in tlu_Plants.}
#' }
#'
#'@param canopyForm Allows you to filter native trees that are canopy-forming species
#' \describe{
#' \item{"all"}{Returns all canopy positions}
#' \item{"canopy"}{Returns only species that are considered native canopy-forming trees as defined in Canopy in tlu_Plants.}
#' }
#'
#' @return returns a dataframe with plot-level and visit-level tree data
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile tree data for live trees only in most recent survey in all parks
#' live_trees <- joinTreeData(status = 'live', years = 2016:2022)
#'
#' # compile dead trees in FRHI in all years
#' FRHI_dead <- joinTreeData(park = 'FRHI', status = 'dead')
#'
#' # compile exotic trees in NERI in all years
#' NERI_exotic <- joinTreeData(park = 'NERI', speciesType = 'exotic')
#' }
#'
#' @export
#'
#------------------------
# Joins tbl_Trees and tbl_Tree_Data tables and filters by park, year, and plot/visit type
#------------------------
joinTreeData<-function(status=c('all', 'live','dead'), speciesType=c('all', 'native','exotic', 'invasive'),
                       canopyPosition = c("all", "canopy"), canopyForm = c("all", "canopy"), park='all',
                       years=2007:2023, QAQC=FALSE, retired=TRUE, anrevisit=FALSE, output, ...){

  park <- match.arg(park, several.ok = TRUE,
                    c("all", "NERI", "GARI", "BLUE", "WV", "ALPO","FONE", "FRHI", "JOFL", "WEPA", "FLNI", "DEWA"))
  status <- match.arg(status)
  speciesType <- match.arg(speciesType)
  canopyPosition <- match.arg(canopyPosition)
  canopyForm <- match.arg(canopyForm)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,retired = retired,
                                 anrevisit = anrevisit, output = 'short'))


 # Prepare tree data

  tree <- merge(trees, treedata, by="Tree_ID", all.y=T, all.x=T)
  tree1 <- tree %>% select(c("Tree_ID", "Tree_Data_ID", "Plant_ID", "Tree_Number", "Event_ID",
                            "DBH", "Status_ID", "Crown_Class_ID", "Diameter_Check_ID", "Calipers_Used", "Decay_Class_ID", "Notes", "QC_DBH_ID"))

  tree2 <- merge(park.plots, tree1, all.x=F, by="Event_ID")
  tree3 <- merge(tree2, plants[,c("Plant_ID","Latin_name","NJ_Pd_Nativ","PA_Glac_Nativ","PA_Mt_Nativ","WV_Mt_Nativ","Invasive","Canopy")],
                 by="Plant_ID", all.x=T)

  tree3n1 <- tree3 %>% mutate (Nativity1 = if_else(Unit_Code == "DEWA",PA_Glac_Nativ,
                                                   if_else(Unit_Code == "ALPO",PA_Mt_Nativ,
                                                           if_else(Unit_Code == "JOFL",PA_Mt_Nativ,
                                                                   if_else(Unit_Code == "FONE",PA_Mt_Nativ,
                                                                           if_else(Unit_Code == "FRHI",PA_Mt_Nativ,
                                                                                   if_else(Unit_Code == "FLNI",PA_Mt_Nativ,
                                                                                      if_else(Unit_Code == "NERI", WV_Mt_Nativ,
                                                                                           if_else(Unit_Code == "BLUE", WV_Mt_Nativ,
                                                                                                   if_else(Unit_Code == "GARI", WV_Mt_Nativ, "NONE"))))))))))

  tree3n2 <- tree3n1 %>% mutate (Nativity2 = if_else(Unit_Code == "DEWA" & is.na(Nativity1),NJ_Pd_Nativ,Nativity1))
  tree3n3 <- tree3n2 %>% mutate (Nativity3 = if_else(Unit_Code == "DEWA" & is.na(Nativity2),PA_Mt_Nativ,Nativity2))

  tree3n3$Nativity3[is.na(tree3n3$Nativity3)] <- "unknown"
  tree3N <- tree3n3 %>% mutate (Nativity = if_else(Nativity3 == "maybe exotic","exotic",
                                                   if_else(Nativity3 == "mixed","unknown",
                                                      if_else(Nativity3 == "maybe native", "native",Nativity3))))

  tree4a <- tree3N[,c("Event_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord","Panel","Year","Event_QAQC","Cycle",
                      "Tree_Number","DBH","Status_ID","Crown_Class_ID","Decay_Class_ID","Latin_name", "Invasive","Nativity","Canopy")]

  tree4 <- tree4a %>% mutate(BA_cm2 = ifelse(DBH<999997.0,(((DBH / 2)^2)*3.1415926535),0),
                              pres = ifelse((Status_ID==1 | Status_ID==2),1,0))


  tree5<- if (status=='live') {filter(tree4,Status_ID==1)
  } else if (status=='dead') {filter(tree4,Status_ID==2)
  } else if (status=='all') {filter(tree4,Status_ID==1 | Status_ID==2)
  }

  tree6<- if (speciesType=='native'){filter(tree5,Nativity=="native")
  } else if (speciesType=='exotic'){filter(tree5,Nativity=="exotic")
  } else if (speciesType=='invasive'){filter(tree5,Invasive==TRUE)
  } else if (speciesType=='all'){(tree5)
  }

  tree7<- if (canopyPosition=='canopy'){filter(tree6,Crown_Class_ID==2 | Crown_Class_ID==3 | Crown_Class_ID==4)
  } else if (canopyPosition=='all'){(tree6)
  }

  tree8<- if (canopyForm=='canopy'){filter(tree7,Canopy==TRUE)
  } else if (canopyForm=='all'){(tree7)
  }

  return(data.frame(tree8))
} # end of function

