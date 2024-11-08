#' @include joinLocEvent.R
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by case_when
#' @importFrom magrittr %>%
#'
#' @title joinCWDData: compile coarse woody debris data.
#'
#' @description This function combines and calculates CWD volume for each plot. Must run importData first.
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
#' @param units Calculates CWD Volume based on different units.
#' \describe{
#' \item{"ha"}{Default. Returns CWD volume as cubic m/hectare}
#' \item{"acres"}{Returns CWD volume as cubic ft/ acre}
#'}
#'
#' @return returns a dataframe with CWD volume for each plot, one with cubic m/ha and cubic ft/acre
#'
#' @examples
#' importData() #imports using default odbc
#' # Compile CWD data for GARI for most recent survey and return in ft^3/acre
#' cwd_data <- joinCWDData(park = 'GARI', years = c(2014:2018), units = 'acres')
#'
#'
#' @export
#'
#------------------------
# Join CWD table and filters by park, year, and plot/visit type
#------------------------
joinCWDData<-function(units=c('ha','acres'), park='all',years=2007:2023, QAQC=FALSE, retired=TRUE, anrevisit=FALSE, output, ...){

  park <- match.arg(park)
  QAQC <- match.arg(QAQC)
  retired <- match.arg(retired)
  anrevisit <- match.arg(anrevisit)
  output <- match.arg(output)
  units<-match.arg(units)

  park.plots<-force(joinLocEvent(park = park, years = years, QAQC = QAQC,retired = retired,
                                 anrevisit = anrevisit, output = 'short'))


  # Prepare the CWD data
  cwd2<-merge(cwd[,c("Event_ID","Degrees","Plant_ID","Diameter","Decay_Class_ID", "Hollow")],
              plants[,c('Plant_ID','Latin_name','Common')], by='Plant_ID',all.x=T)
  cwd2a<-subset(cwd2,Diameter<9000)

  cwd.std<-merge(cwd2a,cwdts[,c("Event_ID","Slope_0","Slope_60","Slope_120","Slope_180","Slope_240","Slope_300")],by="Event_ID",all.x=T,all.y=F)
  cwd.std2<-cwd.std %>% mutate(pct.slope=case_when(Degrees==0 ~ Slope_0, Degrees==60 ~ Slope_60, Degrees==120 ~ Slope_120,
                                                   Degrees==180 ~ Slope_180, Degrees==240 ~ Slope_240, Degrees==300 ~ Slope_300),
                               hdist=((((pct.slope/100)^2)+1)^0.5)*((pi^2)/(8*15)),
                               diam2=Diameter^2)
  cwd3<-cwd.std2 %>% group_by(Event_ID,Degrees, hdist, Latin_name, Decay_Class_ID) %>% summarise(diam=sum(diam2)) %>% ungroup()

  cwd3a<-cwd.std2 %>% group_by(Event_ID,Degrees) %>% summarise(trdiam=sum(diam2)) %>% ungroup()
  cwd3a$transamp<-1
  cwd3b<-cwd3a %>% group_by(Event_ID) %>% summarise(transamp=sum(transamp)) # transamp = number of CWD transects sampled in event
  cwd3d<-merge(cwd3,cwd3b,by="Event_ID", all.x=T)

  cwd4a<-cwd3d %>% group_by(Event_ID, transamp, Latin_name, Decay_Class_ID) %>% summarise(Tot_CWD_Vol=ifelse(is.na(sum(diam)),0,sum(hdist*diam))) %>% ungroup()
  cwd4 <- cwd4a %>% mutate (CWD_Vol = Tot_CWD_Vol/transamp)

  cwd5<-if (units=='acres'){
    cwd4 %>% mutate(CWD_Vol=CWD_Vol*35.314667/2.4710538)
    # 35.314667 is the # cubic feet in a cubic meter. 2.4710538 is # acres in 1 hectare.)
  } else if (units=='ha'){cwd4}

  cwd6<-merge(park.plots,cwd5[,c("Event_ID","CWD_Vol", "Latin_name","Decay_Class_ID")],by="Event_ID",all.x=T)

  cwddata<-cwd6 %>% filter(Latin_name !='No species observed')


  #  mutate(Latin_Name=Latin_name, Decay_Class=Decay_Class_ID,Tag=NA, Density=NA) %>%
  #  select(Unit_Code, Plot_Name, Sample_Year, Date, Cycle, CWD_Vol, Tag, Latin_Name, Decay_Class, Density)


  return(data.frame(cwddata))
} # end of function
