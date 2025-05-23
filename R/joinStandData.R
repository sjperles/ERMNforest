#' @include joinLocEvent.R
#'
#' @importFrom dplyr select mutate_at arrange
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @title joinStandData: compile stand data
#'
#' @description This function combines stand-level data for each plot, including tree heights,
#'    canopy closure, deer browse index, etc. Must run importData first.
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
#' @return returns a dataframe with stand data attached to location and event data. Field names starting with "Pct" are midpoints
#' between cover class ranges (e.g., 62.5 is the midpoint for 50-75%).
#'
#' @examples
#' importData() #imports using default odbc
#' stand_BLUE_cycle3 <- joinStandData(park = 'BLUE', years = c(2015:2018))
#'
#'
#' @export
#'
#------------------------
# Join stand table
#------------------------
joinStandData<-function(park=c('all', 'NERI', 'GARI','BLUE','WV','ALPO','FONE','FRHI','FONE','FLNI','JOFL','WEPA','DEWA'),
                        years=2007:2024, QAQC=FALSE,
                        retired=TRUE, anrevisit=FALSE, output, ...){

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,retired = retired,
                                   anrevisit = anrevisit, output = 'short'))

  stand2<-stand %>% select(Event_ID, Stand_Structure_ID, Crown_Closure_ID,  Deer_Browse_Line_ID,
                           Subcanopy_Height_1, Subcanopy_Height_2, Subcanopy_Height_3,
                           Canopy_Height_1, Canopy_Height_2, Canopy_Height_3, Densiometer_0,
                           Densiometer_90, Densiometer_180, Densiometer_270)

  stand_df<-merge(park.plots, stand2, by='Event_ID', all.x=T)

  stand_df2<-merge(stand_df, stdtlu, by='Stand_Structure_ID', all.x=T)
  names(stand_df2)[names(stand_df2)=='Description']<-"Stand_Structure"

  stand_df3 <- stand_df2 %>% mutate(Crown_Closure_Class= case_when(Crown_Closure_ID==1 ~ 5,
                                                                   Crown_Closure_ID==2 ~ 17.5,
                                                                   Crown_Closure_ID==3 ~ 37.5,
                                                                   Crown_Closure_ID==4 ~ 62.5,
                                                                   Crown_Closure_ID==5 ~ 87.5),
                                    Browse_Index = if_else(Deer_Browse_Line_ID==90,2,
                                                           if_else(Deer_Browse_Line_ID==91,4,
                                                                   if_else(Deer_Browse_Line_ID==99,9,Deer_Browse_Line_ID))))

  stand_df4 <- stand_df3 %>% mutate (Ave_Subcanopy_Height = if_else((Subcanopy_Height_1>9997 & Subcanopy_Height_2>9997 & Subcanopy_Height_3>9997),NA,
                                                                    if_else((Subcanopy_Height_1<9997 & Subcanopy_Height_2>9997 & Subcanopy_Height_3>9997),Subcanopy_Height_1,
                                                                            if_else((Subcanopy_Height_1<9997 & Subcanopy_Height_2<9997 & Subcanopy_Height_3>9997),((Subcanopy_Height_1+Subcanopy_Height_2)/2),
                                                                                    ((Subcanopy_Height_1+Subcanopy_Height_2+Subcanopy_Height_3)/3)))),
                                     Ave_Canopy_Height = if_else((Canopy_Height_1>9997 & Canopy_Height_2>9997 & Canopy_Height_3>9997),NA,
                                                                 if_else((Canopy_Height_1<9997 & Canopy_Height_2>9997 & Canopy_Height_3>9997),Canopy_Height_1,
                                                                         if_else((Canopy_Height_1<9997 & Canopy_Height_2<9997 & Canopy_Height_3>9997),((Canopy_Height_1+Canopy_Height_2)/2),
                                                                                 ((Canopy_Height_1+Canopy_Height_2+Canopy_Height_3)/3)))))

  stand_df5 <- stand_df4 %>% mutate (Densiometer_0 = if_else(Densiometer_0 == 9999, NA, Densiometer_0),
                                     Densiometer_90 = if_else(Densiometer_90 == 9999, NA, Densiometer_90),
                                     Densiometer_180 = if_else(Densiometer_180 == 9999, NA, Densiometer_180),
                                     Densiometer_270 = if_else(Densiometer_270 == 9999, NA, Densiometer_270))

  stand_df6 <- stand_df5 %>% mutate (Crown_Closure_Pct = ((((96-Densiometer_0)*1.04)+((96-Densiometer_90)*1.04)+
                                                             ((96-Densiometer_180)*1.04)+((96-Densiometer_270)*1.04))/4))

  stand_data <- stand_df6 %>% select(Event_ID, Unit_Code, Plot_Name, Year, Panel, Cycle,
                                     Stand_Structure, Crown_Closure_Class, Crown_Closure_Pct, Browse_Index,
                                     Ave_Subcanopy_Height, Ave_Canopy_Height) %>% arrange(Plot_Name, Year)


  return(stand_data)
}
