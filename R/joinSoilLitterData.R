#' @include joinLocEvent.R
#'
#' @importFrom dplyr filter group_by left_join mutate select summarize
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @title joinLitterData: compile soil litter data.
#'
#' @description This function compiles data on leaf litter collected during soil sampling. Must run importData first.
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
#' @return returns a dataframe containing each plot and visit with data on leaf litter collected during soil sampling.
#'
#' @examples
#' \dontrun{
#' importData() #default imports
#' litter_NERI <- joinLitterData(park = 'NERI', QAQC=FALSE, retired=FALSE, anrevisit=FALSE, years=c(2007:2010))
#'}
#' @export
#'
#------------------------
# Join litter data
#------------------------
joinLitterData <- function(park=c('all', 'NERI', 'GARI','BLUE','WV','ALPO','FONE','FRHI','FONE','FLNI','JOFL','WEPA','DEWA'),
                               years=2007:2024,QAQC=FALSE, retired=TRUE, anrevisit=FALSE){

    # Match args and class
    park <- match.arg(park)

    park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,retired = retired,
                                   anrevisit = anrevisit, output = 'short'))
    # Identify plots where soil was sampled
    soil.plots1 <- merge(park.plots, event[,c("Event_ID","WasSoilSampled")], by="Event_ID", all.x=T)
    soil.plots <- subset(soil.plots1, WasSoilSampled == TRUE)

    ###### Average soil data across three soil frames.  Address NAs and zeros in soil data ######
    # Litter Thickness
    frame1 <- slframe[,c("Event_ID", "Litter_Sample_Collected", "Litter_Thickness_UpSlope", "Litter_Thickness_Downslope", "Litter_Weight")] #keep only wanted columns
    frame2 <- frame1 %>% group_by(Event_ID) %>% reframe(Litter_Up_Thick_Ave = mean(Litter_Thickness_UpSlope),
                                                        Litter_Down_Thick_Ave = mean(Litter_Thickness_Downslope))
    plot.frame1 <- merge(soil.plots,frame2, by= "Event_ID", all.x=T)

    # Litter Weight
    frame3 <- subset(frame1, Litter_Sample_Collected == TRUE)
    frame3$Litter_Collected <- 1
    frame4 <- frame3 %>% group_by(Event_ID) %>% reframe(Litter_Weight_Ave = mean(Litter_Weight),
                                                        Litter_Collected_n = sum(Litter_Collected))
    plot.frame2 <- merge(soil.plots,frame4, by= "Event_ID", all.x=T)

    plot.litter <- merge(plot.frame1, plot.frame2[,c("Event_ID","Litter_Weight_Ave","Litter_Collected_n")], all.x=T, all.y=T) %>%
      arrange(Plot_Name)

  return(data.frame(plot.litter))

}
