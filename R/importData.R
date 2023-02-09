#' @title importData: Import tables directly from ERMN forest database
#'
#' @description This function imports database tables from a named ODBC datasource from
#' ERMN forest backend databases. Each table is assigned to the global environment with names
#' that functions in this package depend on. You must use the 32-bit version of R to work.
#'
#' @param type Select whether to use the default DSN to import data or a different database
#' \describe{
#' \item{"DSN"}{Default. DSN database. If not specified, will use "ERMNVeg9999" .}
#' \item{"file"}{A different database than default DSN}
#' }
#' @param path Quoted path of database backend file, including the name of the backend.
#' @return Assigns database tables to global environment
#'
#' @param import_tables Specify whether to import all tables or individual tables
#' \describe{
#' \item{"all"}{Default. Imports all relevant tables from the backend database.}
#' \item{"single"}{Imports an individual table from the backend database. If single is specified, must then specify the
#' exact table name in quotes from the backend database. The table will be given the same object name as if all tables
#' were imported so that package functions are still useable.}
#' }
#'
#' @param table_name Specify a quoted list of one or more table names
#'
#' @examples
#' # Import database in specific folder:
#' importData(type='file', path='./ERMN/ERMNVeg20220528.mdb')
#'
#' # Import ODBC named database
#' importData(type='DSN', odbc="ERMNVeg20220528.mdb")
#'
#' # Import individual tables
#' importData(type='DSN',odbc='ERMNVeg20220528.mdb',import_tables='single',table_name=c('tlu_Plants','tbl_Trees'))
#'
#' @export

importData<- function(type=c('DSN','file'), odbc='ERMNVeg', path=NA,
                      import_tables=c('all','single'), table_name=NA){
  type<-match.arg(type)
  import_tables<-match.arg(import_tables)

  # set up dataframe linking object name for global environment and table name in access
  objectnames<-c("loc", "parktbl", "event", "treedata", "trees", "treecond", "treefcond","tlutreecond", "tlufolcond",
                 "cwd", "cwdts", "plants", "saps", "micro", "sdlg", "shrub", "quadchr", "quadchrtlu", "quads", "quadind",
                 "addspp", "stand", "stdtlu", "disturb", "disttlu", "disttlutc")

  tablenames<-c("tbl_Locations", "tlu_Parks", "tbl_Events", "tbl_Tree_Data", "tbl_Trees", "tbl_Tree_Data_Tree_Conditions",
                "tbl_Tree_Data_Foliage_Conditions","tlu_Tree_Conditions", "tlu_Foliage_Conditions",
                "tbl_CWD_Transect_Data", "tbl_CWD_Slope_Data", "tlu_Plants", "tbl_Microplot_Sapling_Data",
                "tbl_Microplot_Characterization_Data", "tbl_Microplot_Seedling_Data", "tbl_Microplot_Shrub_Data",
                "tbl_Quadrat_Character_Data", "tlu_Quadrats", "tbl_Quadrat_Species_Data", "tbl_Quadrat_Indicator_Data",
                "tbl_Plot_Additional_Species", "tbl_Stand_Data", "tlu_Stand_Structures", "tbl_Disturbances",
                "tlu_Disturbance_Codes", "tlu_Disturbance_Threshold_Codes")

  namesdf<-data.frame(cbind(objectnames,tablenames))

  if(any(!(is.na(table_name)) & !(table_name %in% tablenames))){
    stop('At least one table_name does not match table name listed in the backend.')
  }

  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }
  # set up progress bar
  pb = txtProgressBar(min = 0, max = 31, style = 3)

  # set up type of database file
  db<- if (type=='DSN'){
    db<- DBI::dbConnect(drv=odbc::odbc(),dsn=odbc)
  }
  else if (type=='file'){
    db<- DBI::dbConnect(drv=odbc::odbc(),
                        .connection_string=paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path))
  }

  # import all important tables
  if(import_tables=='all'){
  assign("loc", DBI::dbReadTable(db, "tbl_Locations"), envir=.GlobalEnv)
  setTxtProgressBar(pb,1)
  assign("parktbl",DBI::dbReadTable(db, "tlu_Parks"),envir=.GlobalEnv)
  setTxtProgressBar(pb,2)
  assign("event",DBI::dbReadTable(db, "tbl_Events"),envir=.GlobalEnv)
  setTxtProgressBar(pb,3)
  assign("treedata",DBI::dbReadTable(db, "tbl_Tree_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,4)
  assign("trees", DBI::dbReadTable(db, "tbl_Trees"),envir=.GlobalEnv)
  setTxtProgressBar(pb,5)
  assign("treecond",DBI::dbReadTable(db,"tbl_Tree_Data_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,6)
  assign("treefcond",DBI::dbReadTable(db,"tbl_Tree_Data_Foliage_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,7)
  assign("tlutreecond",DBI::dbReadTable(db,"tlu_Tree_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,8)
  assign("tlufolcond",DBI::dbReadTable(db,"tlu_Foliage_Conditions"),envir=.GlobalEnv)
  setTxtProgressBar(pb,9)
  assign("cwd",DBI::dbReadTable(db,"tbl_CWD_Transect_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,10)
  assign("cwdts",DBI::dbReadTable(db,"tbl_CWD_Slope_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,11)
  assign("plants", DBI::dbReadTable(db, "tlu_Plants"),envir=.GlobalEnv)
  setTxtProgressBar(pb,12)
  assign("saps", DBI::dbReadTable(db,"tbl_Microplot_Sapling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,13)
  assign("micro",DBI::dbReadTable(db,"tbl_Microplot_Characterization_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,14)
  assign("sdlg",DBI::dbReadTable(db,"tbl_Microplot_Seedling_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,15)
  assign("shrub",DBI::dbReadTable(db,"tbl_Microplot_Shrub_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,16)
  assign("quadchr", DBI::dbReadTable(db,"tbl_Quadrat_Character_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,17)
  assign("quadchrtlu", DBI::dbReadTable(db,"tlu_Quadrats"),envir=.GlobalEnv)
  setTxtProgressBar(pb,18)
  assign("quads", DBI::dbReadTable(db,"tbl_Quadrat_Species_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,19)
  assign("quadind", DBI::dbReadTable(db,"tbl_Quadrat_Indicator_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,20)
  assign("addspp", DBI::dbReadTable(db,"tbl_Plot_Additional_Species"),envir=.GlobalEnv)
  setTxtProgressBar(pb,21)
  assign("stand", DBI::dbReadTable(db,"tbl_Stand_Data"),envir=.GlobalEnv)
  setTxtProgressBar(pb,22)
  assign("stdtlu", DBI::dbReadTable(db,"tlu_Stand_Structures"),envir=.GlobalEnv)
  setTxtProgressBar(pb,23)
  assign("disturb", DBI::dbReadTable(db,"tbl_Disturbances"),envir=.GlobalEnv)
  setTxtProgressBar(pb,24)
  assign("disttlu", DBI::dbReadTable(db,"tlu_Disturbance_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,25)
  assign("disttlutc", DBI::dbReadTable(db,"tlu_Disturbance_Threshold_Codes"),envir=.GlobalEnv)
  setTxtProgressBar(pb,26)
  }

  if(import_tables=='single'){
    table_select<-namesdf %>% filter(tablenames %in% table_name) %>% data.frame() %>% droplevels()

    lapply(seq_along(table_select), function(i){
      pb = txtProgressBar(min = 0, max = length(table_select), style = 3)
      assign(as.character(table_select[i,1]),
             DBI::dbReadTable(db, as.character(table_select[i,2])), envir=.GlobalEnv)
      setTxtProgressBar(pb,i)
    })

    }

  DBI::dbDisconnect(db)
  close(pb)
  noquote('database import complete')
}


