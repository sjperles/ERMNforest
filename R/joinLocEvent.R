#' @title joinLocEvent: merges Location and Event level data with options for filtering.
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by between
#' @importFrom magrittr %>%
#' @importFrom lubridate year
#' @importFrom stringr str_pad str_sub
#'
#' @description This function combines location and event data. Must run importData first.
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
#' \item{"DEWA"}{Delaware Water Gap NRA only}}
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#' @param retired Allows you to remove (FALSE) or include (TRUE) retired plots.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that are active}
#' \item{TRUE}{returns all active and retired plots}}
#' @param anrevisit Allows you to remove (FALSE) or include (TRUE) annual revisits from 2008 - 2011.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were sampled on 4 year cycle, does not include annual revisits.}
#' \item{TRUE}{returns all records}}
#' }
#' @param years Allows you to select individual years from 2007 to 2023. Default is all years.
#' If more than one year is selected, specify by c(2007:2018), for example.
#'
#' @return returns a dataframe with location and visit events
#'
#' @examples
#' importCSV('./forest_csvs')
#' # Select most recent survey of data from DEWA
#' DEWA_data <- joinLocEvent(park = 'DEWA', years = c(2007:2018))
#'
#
#'
#' @export
#'

#------------------------
# Joins tbl_Locations and tbl_Events tables and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park="all", QAQC=FALSE, retired=FALSE, anrevisit=FALSE,
                       years=2007:2023, output='short', ...){

  loc2<-loc %>% mutate(Unit_Code=as.factor(str_sub(Unit_Code,1,4)))
  loc3<-droplevels(loc2[,c("Location_ID","Unit_Code","X_Coord","Y_Coord","Plot_Number","Status")])
  loc3$Plot_Number<-str_pad(loc3$Plot_Number,width=3,side="left",pad=0) #Pad plot number so retains 3-digits
  loc3$Plot_Name<-paste(loc3$Unit_Code, loc3$Plot_Number, sep="-")

  loc4<- if (QAQC==FALSE) {filter(loc3, Unit_Code != "TEST")
  } else if (QAQC==TRUE) {(loc3)
  } else {stop("QAQC must be TRUE or FALSE")}

  loc5<- if (retired==FALSE) {filter(loc4, Status == "Active")
  } else if (retired==TRUE) {filter(loc4, Status != "Rejected")
  } else {stop("retired must be TRUE or FALSE")}

  loc6<- if (park=='all') {(loc5)
  } else if (park %in% levels(loc5$Unit_Code)){filter(loc5,Unit_Code==park)
  } else if (park=='WV'){filter(loc5,Unit_Code=="NERI" | Unit_Code=="GARI" | Unit_Code=="BLUE")
  } else if (park=='WEPA'){filter(loc5,Unit_Code=="ALPO" | Unit_Code=="JOFL" | Unit_Code=="FONE" | Unit_Code=="FRHI")
  } else {stop("park must be one of the factor levels of Unit_Code")}

  park.ev1<-merge(loc6,event,by="Location_ID",all.x=T)

  park.ev3<- if (anrevisit==FALSE) {filter(park.ev1, Panel != "X")
  } else if (anrevisit==TRUE) {(park.ev1)
  } else {stop("QAQC must be TRUE or FALSE")}

  park.ev4 <- park.ev3 %>% filter(Year %in% years) %>% droplevels()
  park.ev4$Year <- as.numeric(park.ev4$Year)

  park.plots<- if (output=='short') {park.ev4 %>% select(Location_ID,Event_ID,Unit_Code,
    Plot_Name, Plot_Number, X_Coord, Y_Coord, Panel, Year, Event_QAQC, Cycle)
  } else if (output=='verbose') {park.ev4}

  return(data.frame(park.plots))
} # end of function

