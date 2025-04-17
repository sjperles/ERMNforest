#' @include joinLocEvent.R
#'
#' @importFrom dplyr across contains everything group_by filter inner_join left_join mutate rename select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @title joinSoilLabData: compile and QC soil chemistry data by horizon.
#'
#' @description This function summarizes soil chemistry variables by horizon, average across three sample frames within
#' a plot. Also handles NAs in soil chemistry variables. Must run importData first.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
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
#' @param years Allows you to select individual years from 2007 to 2024. Default is all years.
#' If more than one year is selected, specify by c(2007:2018), for example.#'
#'
#' @param layer Allows you to filter on soil horizons
#' \describe{
#' \item{"all"}{Default. Includes O and A horizons.}
#' \item{"O"}{Return only samples from the O horizon.}
#' \item{"A"}{Return only samples from the A horizon.}}
#'
#' @return returns a dataframe containing each plot and visit with soil chemistry data for each horizon on a plot
#' Plots that weren't sampled during a given cycle are not returned.
#'
#' @examples
#' \dontrun{
#' importData() #imports using default odbc
#'# join only O horizon data for most recent cycle in ACAD.
#' soil_DEWA_O <- joinSoilLabData(park = 'DEWA', years=c(2007:2010), layer = 'O')
#'}
#' @export
#'
#------------------------
# Join soil lab data
#------------------------
joinSoilLabData <- function( park=c('all', 'NERI', 'GARI','BLUE','WV','ALPO','FONE','FRHI','FONE','FLNI','JOFL','WEPA','DEWA'),
                             years=2007:2024,QAQC=FALSE, retired=TRUE, anrevisit=FALSE, layer = c("all", "O", "A")){

  # Match args and class
  park <- match.arg(park)
  layer <- match.arg(layer)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,retired = retired,
                                 anrevisit = anrevisit, output = 'short'))



  # Identify plots where soil was sampled
  soil.plots1 <- merge(park.plots, event[,c("Event_ID","WasSoilSampled")], by="Event_ID", all.x=T)
  soil.plots <- subset(soil.plots1, WasSoilSampled == TRUE)

  # Soil Chemistry Variables
  slhoriz1 <- subset(slhoriz, Horizon_Collected == TRUE)
  slhoriz1a <- slhoriz1[!is.na(slhoriz1$pH), ]
  slhoriz1b <- slhoriz1a %>% mutate (pH = ifelse(pH == "no sample", NA_real_, pH),
                                     LOI = ifelse(LOI == "no sample", NA_real_, LOI),
                                     TN = ifelse(TN == "no sample", NA_real_, TN),
                                     TC = ifelse(TC == "no sample", NA_real_, TC),
                                     Ca = ifelse(Ca == "no sample", NA_real_, Ca),
                                     K = ifelse(K == "no sample", NA_real_, K),
                                     Mg = ifelse(Mg == "no sample", NA_real_, Mg),
                                     P = ifelse(P == "no sample", NA_real_, P),
                                     Al = ifelse(Al == "no sample", NA_real_, Al),
                                     Fe = ifelse(Fe == "no sample", NA_real_, Fe),
                                     Mn = ifelse(Mn == "no sample", NA_real_, Mn),
                                     Na = ifelse(Na == "no sample", NA_real_, Na),
                                     Zn = ifelse(Zn == "no sample", NA_real_, Zn),
                                     acidity = ifelse(acidity == "no sample", NA_real_, acidity),
                                     ECEC = ifelse(ECEC == "no sample", NA_real_, ECEC))


  slhoriz2 <- slhoriz1b %>% mutate_at(c('pH', 'LOI', 'TN', 'TC', 'Ca', 'K', 'Mg', 'P', 'Al',
                                        'Fe', 'Mn', 'Na', 'Zn', 'acidity','ECEC'), as.factor)

  slhoriz3 <- slhoriz2 %>% mutate_at(c('pH', 'LOI', 'TN', 'TC', 'Ca', 'K', 'Mg', 'P', 'Al',
                                       'Fe', 'Mn', 'Na', 'Zn', 'acidity','ECEC'), as.numeric)

  schem1 <- merge(slframe[,c("Event_ID","Soil_Frame_ID","Frame_Number")],slhoriz3, by="Soil_Frame_ID", all.x = T, all.y=T)
  schem2 <- subset(schem1, !is.na(Soil_Horizon_ID)) # remove three soil frames where no soil was collected
  schem2$Soil_Collect <- 1
  schem3 <- schem2 %>% mutate (Analysis_Horiz = case_when(Horizon=="B" ~ "B",
                                                          Horizon=="E" ~ "E",
                                                          Horizon !="B"~ Anal_Horiz))

  schem4 <- schem3 %>% group_by(Event_ID, Analysis_Horiz) %>% summarise(pH.ave = mean(pH,na.rm = TRUE),
                                                                        LOI.ave = mean(LOI, na.rm = TRUE),
                                                                        TN.ave = mean(TN, na.rm = TRUE),
                                                                        TC.ave = mean(TC, na.rm = TRUE),
                                                                        Ca.ave = mean(Ca, na.rm = TRUE),
                                                                        K.ave = mean(K, na.rm = TRUE),
                                                                        Mg.ave = mean(Mg, na.rm = TRUE),
                                                                        P.ave = mean(P, na.rm = TRUE),
                                                                        Al.ave = mean(Al, na.rm = TRUE),
                                                                        Fe.ave = mean(Fe, na.rm = TRUE),
                                                                        Mn.ave = mean(Mn, na.rm = TRUE),
                                                                        Na.ave = mean(Na, na.rm = TRUE),
                                                                        Zn.ave = mean(Zn, na.rm = TRUE),
                                                                        acidity.ave = mean(acidity, na.rm = TRUE),
                                                                        ECEC.ave = mean(ECEC, na.rm = TRUE),
                                                                        num_samples = sum(Soil_Collect))


  soil_chem <- schem4 %>% mutate(Ca_Al = (Ca.ave/40.078)/(Al.ave/26.981),
                                 C_N   = TC.ave/TN.ave,
                                 Ca_meq = Ca.ave/((40.08/2)*10),
                                 K_meq = K.ave/(39.1*10),
                                 Mg_meq = Mg.ave/((24.31/2)*10),
                                 Na_meq = Na.ave/((22.29)*10),
                                 P_meq = P.ave/((31.97/2)*10),
                                 Al_meq = Al.ave/((26.98/3)*10),
                                 Fe_meq = Fe.ave/((55.85/2)*10),
                                 Mn_meq = Mn.ave/((54.94/2)*10),
                                 Zn_meq = Zn.ave/((65.39/2)*10),
                                 BaseSat = ifelse(is.na(ECEC.ave), NA_real_, ((Ca_meq + K_meq + Mg_meq + Na_meq)/ECEC.ave)*100),
                                 CaSat = ifelse(is.na(ECEC.ave), NA_real_, ((Ca_meq)/ECEC.ave)*100),
                                 AlSat = ifelse(is.na(ECEC.ave), NA_real_, ((Al_meq)/ECEC.ave)*100)) %>%
    select (-c(Ca.ave, K.ave, Mg.ave, P.ave, Al.ave, Fe.ave, Mn.ave, Na.ave, Zn.ave))

  plot.soil1 <- merge(soil.plots,soil_chem, by= "Event_ID", all.x=T, all.y=T) %>% arrange(Plot_Name,Analysis_Horiz)
  plot.soil <- subset(plot.soil1, !is.na(Location_ID))


  soil.final<-if (layer=='O'){filter(plot.soil,Analysis_Horiz=="O")
  } else if (layer=='A'){filter(plot.soil,Analysis_Horiz=="A")
  } else if (layer=='all'){(plot.soil)
  }

 return(soil.final)
 }
