#' @include joinTreeData.R
#' @title joinTreeConditions: compiles live and dead tree conditions
#'
#' @importFrom dplyr arrange case_when filter full_join group_by left_join mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @description This function compiles tree condition data into a wide format with
#' one row per tree visit and a column for each foliage condition type. Must run importData first.
#' Retired plots can be  excluded from function.
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
#' @param retired Allows you to remove (FALSE) or include (TRUE) retired plots.
#' \describe{
#' \item{FALSE}{Only returns plots that are active}
#' \item{TRUE}{Default. returns all active and retired plots}}
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
#' @param canopyPosition Allows you to filter on tree crown class
#' \describe{
#' \item{"all"}{Returns all canopy positions}
#' \item{"canopy"}{Returns only dominant, codominant, and intermediate crown classes}
#' }
#'
#'@param canopyForming Allows you to filter native trees that are canopy-forming species
#' \describe{
#' \item{"all"}{Returns all canopy positions}
#' \item{"canopy"}{Returns only species that are considered native canopy-forming trees as defined in Canopy in tlu_Plants.}
#' }
#'
#'
#' @return returns a wide data frame with one row for each tree visit and tree conditions as columns.
#' Note that vines in the crown and on the bole return the number of species in each condition. Remaining
#' conditions are either 0 for absent or 1 for present. Will return NA for years they were not assessed,
#' otherwise codes are 0/1.
#'
#' @examples
#' \dontrun{
#' importData()
#' # compile tree condition data for live trees in all parks in cycle 3, excluding QAQC visits
#' trcond_c3 <- joinTreeConditions(from = 2014, to = 2017, status = 'live', QAQC = FALSE)
#'
#' # compile tree condition for ROVA in 2019, including QAQC visits for active trees
#' ROVA_trees <- joinTreeConditions(park = "ROVA", from = 2019, to = 2019, status = 'active',
#'                                  QAQC = TRUE)
#' }
#' @export
#'
#------------------------
# Joins tree and foliage data and filters by plot, event, and tree types
#------------------------
joinTreeConditions <- function(park='all', years=2007:2024, QAQC=FALSE, retired=TRUE, anrevisit=FALSE,
                               status = c('all', 'live', 'dead'),
                               speciesType = c('all', 'native','exotic', 'invasive'),
                               canopyPosition = c("all", "canopy"),
                               canopyForming = c("all", "canopy"), ){

  # Match args and class
  park <- match.arg(park)
  status <- match.arg(status)
  speciesType <- match.arg(speciesType)
  canopyPosition <- match.arg(canopyPosition)
  canopyForming <- match.arg(canopyForming)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,retired = retired,
                                 anrevisit = anrevisit, output = 'short'))



  treecond0 <- merge(treecond, tlutreecond, by="Tree_Condition_ID", all.x=T)
  treecond1 <- merge(treedata, treecond0, by="Tree_Data_ID", all.y=T)  %>%
    select (Tree_Data_ID, Description)

  treefcond0 <- merge(treefcond, tlufolcond, by="Foliage_Condition_ID", all.x=T)
  treefcond1 <- merge(treedata, treefcond0, by="Tree_Data_ID", all.y=T) %>%
    select (Tree_Data_ID, Description)

  condition <- rbind (treecond1,treefcond1)

  treeC <- merge(treedata, condition, by="Tree_Data_ID", all.x=T, all.y=T)
  treeC0 <- merge(trees, treeC, by="Tree_ID", all.x=T, all.y=T) |>
    select(Event_ID, Tree_ID, Tree_Number, Plant_ID, DBH, Status_ID, Crown_Class_ID, Decay_Class_ID, Description)
  treeC1 <- merge(treeC0, plants[,c("Plant_ID","Latin_name")], by="Plant_ID", all.x=T)
  treeC2 <- merge(park.plots, treeC1, all.x=T, by="Event_ID")













  # subset with EventID from tree_events to make tree data as small as possible to speed up function
  tree_events <- force(joinTreeData(park = park, years = years, QAQC = QAQC,retired = retired,
                                    anrevisit = anrevisit, status = status, speciesType = speciesType,
                                    canopyPosition = canopyPosition, canopyForming = canopyForming, output = 'verbose'))
   %>%
                 select(Plot_Name, Network, Unit_Code,
                        PlotCode, PlotID, EventID, IsQAQC, SampleYear, SampleDate, cycle,
                        TSN, ScientificName, TagCode, TreeStatusCode) %>%
                 filter(ScientificName != "None present") # drop plot-events without trees that match
                                                          # the specified speciesType and/or status
  if(nrow(tree_events) == 0){stop("Function returned 0 rows. Check that park and years specified contain visits.")}

  te_list <- unique(tree_events$EventID)

  trcond_evs <- filter(trcond_vw, EventID %in% te_list)
  vine_evs <- filter(vine_vw, EventID %in% te_list)

  # Reshape vines to wide
  vine_wide <- vine_evs %>% mutate(present = 1) %>%
                            group_by(Plot_Name, PlotID, EventID, TagCode, VinePositionCode) %>%
                            summarize(present = sum(present), .groups = 'drop') %>%
                            pivot_wider(names_from = VinePositionCode,
                                        values_from = present,
                                        values_fill = 0,
                                        names_glue = "VIN_{VinePositionCode}")

  # Another left join to drop unwanted trees early (previous step was unwanted events)
  trcond_evs2 <- left_join(tree_events, trcond_evs, by = intersect(names(tree_events), names(trcond_evs)))

  vine_evs2 <- left_join(tree_events %>% select(Plot_Name, ParkUnit, ParkSubUnit, PlotCode, PlotID, EventID,
                                                IsQAQC, SampleYear, TagCode, TreeStatusCode),
                         vine_evs,
                         by = c("Plot_Name", "PlotID", "EventID", "TagCode"))
     # had to drop Tree TSN/Scientific name is different from Vine TSN/ScientificName

  # Preparing vine data to join with rest of the tree conditions
  # In case the filtering above drops one of the vine positions

  # Combine tree condition and vine data
  tree_comb <- left_join(trcond_evs2, vine_wide, by = intersect(names(trcond_evs2), names(vine_wide)))
  if(!"VIN_B" %in% names(tree_comb)){tree_comb$VIN_B <- NA_real_}
  if(!"VIN_C" %in% names(tree_comb)){tree_comb$VIN_C <- 0} # If VIN_C isn't in trcond_evs2, it hasn't
  # been recorded for trees with the specified arguments. Vines in crown have always been in the protocol,
  # so 0 is better than NA for VIN_C.

  # head(tree_comb)
  # table(tree_comb$VIN_B, tree_comb$SampleYear, useNA = 'always')
  # table(tree_comb$VIN_C, tree_comb$SampleYear, useNA = 'always')

  tree_comb$VIN_C <- ifelse(tree_comb$VINE == 0, 0, tree_comb$VIN_C)
  tree_comb$VIN_B <- case_when(tree_comb$VINE == 0 & tree_comb$SampleYear >= 2019 ~ 0,
                               tree_comb$SampleYear < 2019 ~ NA_real_,
                               TRUE ~ tree_comb$VIN_B)


  # Create list of live and dead tree conditions besides H and NO to count number of conditions
  live_cond_cnt <- c('AD',	'ALB',	'BBD', 'BLD',	'BC',	'BWA',	'CAVL',	'CAVS',	'CW',
                     'DBT',	'DOG', 'EAB',	'EB',	'EHS',	'G',	'GM',	'HWA',	'ID',	'OTH',	'RPS',
                     'SB',	'SLF', 'SOD',	'SPB',	'SW',	'VIN_B', 'VIN_C')

  dead_cond_cnt <- c('CAVL', 'CAVS')

  # List of columns to sum across based on status specified
  cond_sum <- if(status == 'dead'){dead_cond_cnt} else {live_cond_cnt}

  tree_comb$num_cond <- rowSums(tree_comb[, cond_sum], na.rm = T) # num of conditions recorded

  req_cols <- c("Plot_Name", "Network", "ParkUnit", "ParkSubUnit", "PlotTypeCode", "PanelCode",
                "PlotCode", "PlotID", "EventID", "IsQAQC", "SampleYear", "SampleDate", "cycle",
                "TSN", "ScientificName", "TagCode", "TreeStatusCode", "BBDCode", "HWACode")

  tree_comb2 <-
    if(status == 'dead'){
      tree_comb[!tree_comb$TreeStatusCode %in% c("DF", "DC"), c(req_cols, "num_cond", "NO", dead_cond_cnt)]
    } else if(status == 'live'){tree_comb[, c(req_cols, "num_cond", "H", live_cond_cnt)]
    } else {tree_comb[, c(req_cols, "num_cond", "H", "NO", live_cond_cnt)]} # note live_cols contains all dead_cols

  # Convert 0 to NA for status codes added later
  if(status != 'live'){
      tree_comb2$NO[tree_comb2$SampleYear < 2012] <- NA}

  tree_comb2$CAVL[tree_comb2$SampleYear < 2012] <- NA
  tree_comb2$CAVS[tree_comb2$SampleYear < 2012] <- NA

  if(status == 'dead'){
  tree_comb2$num_cond[tree_comb2$SampleYear < 2012] <- NA
  }

  trcond_final <- tree_comb2 %>% filter(!is.na(Plot_Name)) %>%
    arrange(Plot_Name, SampleYear, IsQAQC, TagCode)# drops trees that are not the selected status

  return(data.frame(trcond_final))
} # end of function

