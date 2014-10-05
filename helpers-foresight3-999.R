





# posneg - higher values ... higher ntiles
hdntile  <- function(  x  = NULL                     # required: 'subset (grouped)'  dataframe
                       , measurecol = NULL             # required:  measure column name string 
                       , buckets = 100                  # divisions integer OR "NROW_distinct_measurecol" OR "NROW_distinct_measurecol_or_max100" strings
                       , posneg = 1                     # 1  - higher values = higher measure, -1 - higher values lower measure  integer
                       , new_suffix_type = "functmeas"  # new column suffix typically 'functmeas' _NTILE100 string                        , mutually exclusive of ( new_suffix_name, new_word_explicit_name )
                       , new_suffix_expl_name = NULL    # explicity name the suffix e.g. "_MYNTILEGREAT" string                          , mutually exclusive of ( new_suffix_type, new_word_explicit_name )
                       , new_word_explicit_name = NULL  # explicity name the the entire new column e.g. "MYQUANTILEGREAT_OF_TODAY" string , mutually exclusive of ( new_suffix_type, new_suffix_name )
                       , monopolyhdntile  = "buckets"    # if only one firm, how does it compare against itself? 
                         # NA - hdquantile default                  literal NA
                         # "buckets" - use buckets hdntile default  string
                         # "bucketsDIVtwo" use trunc(buckets/2)     string
                         # 1,2,50,100 or any hard coded             literal integer
                       , measurename = "NTILE"           # name the 'funct' part of 'functmeas' ( of 'measurecol'_functmeas' )            
) {
  
  
  require(Hmisc)
  require(dplyr)
  
  # desire sequential integers
  
  if(buckets == "NROW_distinct_measurecol") {
    buckets <- as.integer(length(unique( x[,measurecol] )))
  }
  
  if(buckets == "NROW_distinct_measurecol_or_max100") {
    buckets <- min( as.integer(length(unique( x[,measurecol] ))), 100L )
  }
  
  newcolname <- ""
  
  # choose output column name 
  
  if(new_suffix_type == "functmeas") {
    newcolname <- paste0(measurecol,"_",measurename,buckets) 
  }
  
  if(!is.null(new_suffix_expl_name)) {
    newcolname <- paste0(measurecol,"_",new_suffix_expl_name) 
  }
  
  if(!is.null(new_word_explicit_name)) {
    newcolname <- new_word_explicit_name  
  }
  
  # calculate

  x[[newcolname]] <-  cut((as.numeric(posneg))*x[[measurecol]], hdquantile((as.numeric(posneg))*x[[measurecol]], seq(0, 1, as.numeric(1.0/as.numeric(buckets))) , na.rm = TRUE ), labels=FALSE , include.lowest=T) 
  
  # pass-through  ( will be NA - hdquantile default )
  # monopolyntilevalue <- NA
  
  # adjustment if only one monopoly firm exists how does it compare against itself?
  
  if( !is.na(monopolyhdntile) & (length(which(!is.na(x[,measurecol]))) == 1) ) {
    
    # browser()
    if(  monopolyhdntile == "buckets"   ) {
      x[,newcolname] <- ifelse(!is.na(x[,measurecol]), as.integer(buckets), NA )
    }
    
    if(  monopolyhdntile == "bucketsDIVtwo"   ) {
      x[,newcolname] <- ifelse(!is.na(x[,measurecol]), as.integer(trunc(buckets/2.0)), NA ) # pessimistic
    }
    
    if(  is.numeric(monopolyhdntile)  ) {
      x[,newcolname] <- ifelse(!is.na(x[,measurecol]), as.integer(monopolyhdntile), NA )
    }
    

  }
  
  # because I do math between hdntiles
  x[,newcolname] <- as.numeric(x[,newcolname])
  
  # return the data.frame
  x
}





# posneg   higher values ... lower hdrank 

hdrank  <- function(  x  = NULL                      # required: 'subset (grouped)'  dataframe
                      , measurecol = NULL              # required:  measure column name string 
                      , buckets = 100                  # divisions integer OR "NROW_distinct_measurecol" OR "NROW_distinct_measurecol_or_max100" strings
                      , posneg = -1                    # 1  - higher values = higher measure, -1 - higher values lower measure  integer
                      , new_suffix_type = "functmeas"  # new column suffix typically 'functmeas' _RANK100 string                        , mutually exclusive of ( new_suffix_name, new_word_explicit_name )
                      , new_suffix_expl_name = NULL    # explicity name the suffix e.g. "_MYNTILEGREAT" string                          , mutually exclusive of ( new_suffix_type, new_word_explicit_name )
                      , new_word_explicit_name = NULL  # explicity name the the entire new column e.g. "MYQUANTILEGREAT_OF_TODAY" string , mutually exclusive of ( new_suffix_type, new_suffix_name )
                      , monopolyhdntile  = 1           # if only one firm, how does it compare against itself? 
                                                          # NA - hdquantile default                  literal NA
                                                          # "buckets" - use buckets                   string
                                                          # "bucketsDIVtwo" use trunc(buckets/2)      string
                                                          #  1 (  hdrank default  ), 2,50, 100 or any hard coded  literal integer                      
                      , measurename = "RANK"            # name the 'funct' part of 'functmeas' ( of 'measurecol'_functmeas' )               
) {
  
  hdntile(            
      x  = x                      
    , measurecol = measurecol              
    , buckets = buckets                  
    , posneg = posneg                   
    , new_suffix_type = new_suffix_type                  
    , new_suffix_expl_name = new_suffix_expl_name    
    , new_word_explicit_name = new_word_explicit_name 
    , monopolyhdntile  = monopolyhdntile 
    , measurename = measurename                                                      
  )
  
}


as.no_worse_than_NA <- function(x) {
    
  # NO JOB!
  # x <- ifelse(!is.na(x),
  #   x
  #   , NA
  # )
  
  # specials
  
  x <- ifelse(!is.na(x) &  !is.infinite(x),
    x
    , NA
  )
  
  x <- ifelse(!is.na(x)  &  !is.nan(x),
    x
    , NA
  )
  
}



eliminate_all_duplicates <- function( df_itself, key_to_fix_name ) {

    # R language 'get out of jail free card.'

    eval(parse(text=paste0("
    
      key_dup              <- df_itself[duplicated(df_itself[,'",key_to_fix_name,"']),,drop=FALSE]
      new_df_no_duplicates <- df_itself[!(df_itself$",key_to_fix_name," %in% as.matrix(key_dup)),,drop=FALSE]
    
    ")))

}


get_from_disk <- function(fnombre, filesoptionlist) { 
   
  require(foreign)
 
  if(is.null(fnombre) )         stop ("fnombre file name      is missing")
  if(is.null(filesoptionlist) ) stop ("filesoptionlist        is missing")
 
  
  if(getLocalOption("FileStoreStyle", optionlist = filesoptionlist) == "Optimized") {
    # NOTE: file.access: case INsenstive ( better built for Windows )
    if( file.access(getLocalOption(paste0("AAIISIPro40PathFileOptim_",fnombre), optionlist = filesoptionlist), mode = 0) == 0) {
      load(file = getLocalOption(paste0("AAIISIPro40PathFileOptim_",fnombre), optionlist = filesoptionlist), envir = environment()) # , verbose = TRUE ... Loading objects:
      # load file ( assign - only takes a string)
      assign("fnombre_data",eval(parse(text=fnombre)))
      # SHOULD ACTUALLY DO rm(list = c(fnombre))
    } else {
      # load file ( assign - only takes a string)
      assign(fnombre,suppressWarnings(suppressMessages(read.dbf(file=getLocalOption(paste0("AAIISIPro40PathFileNotOptim_",fnombre), optionlist = filesoptionlist), as.is = TRUE))))     
      save(list = c(fnombre),file = getLocalOption(paste0("AAIISIPro40PathFileOptim_",fnombre), optionlist = filesoptionlist),envir = environment())
      assign("fnombre_data",eval(parse(text=fnombre)))
    }
  } else {
    # load file
    fnombre_data <- suppressWarnings(suppressMessages(read.dbf(file=getLocalOption(paste0("AAIISIPro40PathFileNotOptim_",fnombre), optionlist = filesoptionlist), as.is = TRUE)))
  }
  return(fnombre_data)
  
 
}



initLocalOptions <- function() {
  return(list())
}

# assign local options 
localoptions <- function(..., optionlist) {
  
  if(is.null(optionlist)) stop("optionlist is NULL")

  # the item name is not ""
  (names(as.list(substitute(list(...)))[-1L]) !=  "")     -> dots_have_values
  list(...)[dots_have_values]                             -> dots_parameters_values
  
  # assign where 'item name is not ""'
  for (var in names(dots_parameters_values)) {
    list(...)[[var]] -> optionlist[[var]]
  }
  
  return(optionlist)
}
# OPTIONLIST <- localoptions(Temperature = 85, season = "Fall", optionlist = OPTIONLIST)


# retrieve local options
getLocalOption <- function(x, default = NULL, optionlist) {
  
  # get from the passthough parameter
  if(is.null(x))          stop("option is NULL")
  if(is.null(optionlist)) stop("optionlist is NULL")

  # to test ( maybe 'exists()' is a better test?)
  if(is.null(optionlist[[x]])) {
    default -> optionlist[[x]] 
  } 
  
  return(optionlist[[x]])
  
}
# MyTemperature <- getLocalOption("Temperature", optionlist = OPTIONLIST)



browseAAIISIPrometa <- function(searchstring = NULL ) {
  
  require(foreign)
  require(sqldf)
  require(dplyr)
  
  # read.dbf
  #   as.is=TRUE do not convert character vectors' to factors  
  
  # begin connection
  sqldf(drv = "SQLite") 
  
  # read the data dictionary DATADICT 
  DATADICT <- tbl_df(suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional140926/Datadict/datadict.dbf", as.is = TRUE))))         
  DATADICT <- tbl_df(suppressWarnings(sqldf("SELECT * FROM DATADICT")))
  
  # read the file master FILEMAST 
  FILEMAST <- tbl_df(suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional140926/Datadict/filemast.dbf", as.is = TRUE))))                                           
  FILEMAST <- tbl_df(suppressWarnings(sqldf("SELECT * FROM FILEMAST")))

  
  META_FIELD_DESC <- tbl_df(sqldf(paste0("SELECT DD.FILE DD_FILE, FIELD_NUM, FIELD_NAME, FIELD_TYPE , FIELD_DESC, DESCRIP, FM.FILE  FM_FILE, DIRECTORY 
    FROM main.DATADICT DD, main.FILEMAST FM 
      WHERE DD.FILE = FM.FILE AND ( 
        FIELD_DESC LIKE '%",searchstring,"%' OR 
        FIELD_NAME LIKE '%",searchstring,"%'
        ) 
  ")))


  # end connection
  sqldf(drv = "SQLite") 

  META_FIELD_DESC <- t(META_FIELD_DESC)
  
  the_end_debug_bookmark_here <- 1
  
  # View(META_FIELD_DESC)
  
}
# browseAAIISIPrometa("PRCHG_13W") # best deal
