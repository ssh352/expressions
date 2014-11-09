
 
# HARD NOTE: dplyr IS FROM github NOT FROM CRAN ( should make no difference )
 
 
options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width
options(sqldf.driver = "SQLite")

# library(Hmisc)

# library(testthat)

# library(tcltk)
# library(Rgraphviz)

# library(sqldf)

# library(lubridate)

# library(plyr)
# library(dplyr)
# library(data.table)

# library(foreign)

# library(TTR)



# IN YELLOW window2 only
# library(testthat)
# auto_test("./R", "./tests/testthat") 


main_foresight3_999 <- function(pauseat=NULL) {

  oldwd <- getwd()
  # "current working directory of the R process"
  setwd("N:\\MyVMWareSharedFolder\\foresight3\\R") 

  # slight less 'SAFE' rstudio debugSource will no longer parse these functions
  # source( paste0(getwd(),"/sql.select.R"))
  # may override as.data.frame ( BE CAREFUL )
  source( paste0(getwd(),"/helpers-foresight3-999.R"), local = TRUE) # do not load into .Global

  # Check if R is running in RStudio
  # http://stackoverflow.com/questions/12389158/check-if-r-is-running-in-rstudio

  # UNFORTUNATLY LOADS INTO THE PARENT/GLOBAL ENVIRONMENT
  # if (!any(search() %in% "tools:rstudio")) {
    # source( paste0(getwd(),"/helpers-foresight3-999.R"), local = TRUE) # do not load into .Global
  # } else { 
    # debugSource(paste0(getwd(),"/helpers-foresight3-999.R"), echo=TRUE) 
  # }


  
  # if(pauseat=="HERE") {}


  OPTIONLIST <- initLocalOptions()
  
#   options(RepositoryStyle = "Installed")      # OR    "InstalledTest" OR "Installed" # OR "Dated"
#   options(FileStoreStyle  = "Optimized" )     # OR    "NotOptimized " OR "Optimized" 
  # Optimized: ( will try to find fast loading binary  file if not found then load then save 'fast loading' )
  
  OPTIONLIST <- 
  localoptions( RepositoryStyle = "Installed"
              , FileStoreStyle  = "Optimized"
  , optionlist = OPTIONLIST)
  
#   if(getOption("RepositoryStyle") == "InstalledTest")  {
#     
#     options(AAIIBase = "N:/MyVMWareSharedFolder/ProfessionalTest") 
#     
#   }
#   
#   if(getOption("RepositoryStyle") == "Installed")  {
#     
#     options(AAIIBase = "N:/MyVMWareSharedFolder/Professional141031") 
#     
#   }
  

  
  if(getLocalOption("RepositoryStyle", optionlist = OPTIONLIST) == "InstalledTest")  {
    
    OPTIONLIST <- localoptions(AAIIBase = "N:/MyVMWareSharedFolder/ProfessionalTest", optionlist = OPTIONLIST)
    
  }
  
  if(getLocalOption("RepositoryStyle", optionlist = OPTIONLIST) == "Installed")  {
    
    OPTIONLIST <- localoptions(AAIIBase = "N:/MyVMWareSharedFolder/Professional141031", optionlist = OPTIONLIST)
    
  }
  

  # default .dbf file locations
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SETUP =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Setup.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SETUP         =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SETUP", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_CI =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/si_ci.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_CI         =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_CI", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_MGDSC =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_MGDSC.DBF"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_MGDSC         =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_MGDSC", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_EXCHG =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_EXCHG.DBF"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_EXCHG         =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_EXCHG", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_PSD   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/si_psd.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_PSD           =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_PSD", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  # NEW_KID 
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_PSDD  =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/si_psdd.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_PSDD          =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_PSDD", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)

  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_PSDC   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/si_psdc.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_PSDC           =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_PSDC", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_DATE  =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/si_date.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_DATE          =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_DATE", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_ISQ   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/si_isq.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_ISQ           =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_ISQ", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_BSQ   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/si_bsq.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_BSQ           =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_BSQ", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_CFQ   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/si_cfq.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_CFQ           =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_CFQ", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  OPTIONLIST <- localoptions(AAIISIPro40PathFileNotOptim_SI_MLT   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/si_mlt.dbf"), optionlist = OPTIONLIST)
  OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_MLT           =  paste0(getLocalOption("AAIISIPro40PathFileNotOptim_SI_MLT", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
  
  # end of default .dbf file locations
  
  if(getLocalOption("FileStoreStyle", optionlist = OPTIONLIST) == "Optimized") {
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SETUP =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/SETUP.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SETUP         =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SETUP", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_CI =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_CI.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_CI         =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_CI", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_MGDSC =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_MGDSC.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_MGDSC         =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_MGDSC", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_EXCHG =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_EXCHG.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_EXCHG         =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_EXCHG", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_PSD   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/SI_PSD.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_PSD           =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_PSD", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    # NEW_KID ( POTENTIAL: BUT DO I USE )
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_PSDD =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/SI_PSDD.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_PSDD          =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_PSDD", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)

    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_PSDC   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/SI_PSDC.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_PSDC           =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_PSDC", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_DATE  =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_DATE.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_DATE          =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_DATE", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_ISQ   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_ISQ.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_ISQ           =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_ISQ", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_BSQ   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_BSQ.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_BSQ           =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_BSQ", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_CFQ   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Static/SI_CFQ.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_CFQ           =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_CFQ", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
    OPTIONLIST <- localoptions(AAIISIPro40PathFileOptim_SI_MLT   =  paste0(getLocalOption("AAIIBase", optionlist = OPTIONLIST),"/Dbfs/SI_MLT.RData"), optionlist = OPTIONLIST)
    OPTIONLIST <- localoptions(AAIISIPro40PathFile_SI_MLT           =  paste0(getLocalOption("AAIISIPro40PathFileOptim_SI_MLT", optionlist = OPTIONLIST)), optionlist = OPTIONLIST)
    
  }
  

  
  require(Hmisc)

  require(testthat)

  require(tcltk)
  require(Rgraphviz)

  require(sqldf)

  require(lubridate) # verify/revisite later: why am I not using TimeWarp?

  require(plyr)
  require(dplyr)
  require(data.table)

  require(foreign)

  require(TTR)
  
  require(gdata)
  require(tidyr)
  require(stringr)
  require(gdata)

  # if( Sys.getenv("ISRTESTING") == "TRUE") { if(NROW(PAYLOAD) == 7000) print(paste0("","")) }

  # stop() stopifnot(), warning(), message()
  # tryCatch(code
  #   , error = function(c) "error"
  #   , warning = function(c) "warning"
  #   , message = function(c) "message"
  #  )
  # browser()
  


  # 2014 - e.g.   The value composite
  # 2014 - if a value for a factor is missing the factor is ignored
  #        but at least 3 factors are required 'in a 'value' [composite]

  #        ( Therefore, I have to keep track of a 'multiplier' )

  # OLD 2010 method - missing a factor then assign to the factor a 'median score'
  # NEW 2014 -      - missing a factor then 'keep it missing'
  #                 - must have at least 2 of 4 FINANCIAL factors, ( 2 OF 4 EARN, 3 OF 5 VALUE_TWO )
  #                     if not it is elminated from the median table 


  # 2014 -  once a rank is assigned to all the factors            
  #          and the stocks are averaged [ within composite ] 
  #          and the stocks are assigned to deciles and  ( I AM KEEPING NTIL100 IS HIGH(BEST) )
  #            lowest score (best) are assigned to 1     ( I AM KEEPING NTIL100 IS HIGH(BEST) )  


  # need a median                _MEDIAN_PASSED
  # 2012 p. 569 - must be in the 'upper 50%' of the combined composites of
  #               FIN, EARN, VAL2  
  #               ( means I add per stock 
  #               _FIN_ + _EARN_ + _VAL2_
                                      
  # Buy the 25 stocks with the 'best Value Composite 2 VAL2' scores
  #                               WINNERS25

  
  
  
  # big AAII data loading will go here  
  
  
  # read.dbf 
  #   as.is=TRUE do not convert character vectors' to factors
  
  # begin load minimal data
  
  # begin connection
  dpsqllconn <- src_sqlite(":memory:", create = T)
  
  # SI Pro 4.0 information
  
  # one record: this will be a cartsian product
  
  # FIELD_NAME  MONTHDATE
  # FIELD_TYPE  D
  # FIELD_DESC  Monthly data date
  # DESCRIP     Stock Investor version information
  # FM_FILE     SETUP
  
  # FIELD_NAME  WEEKDATE
  # FIELD_TYPE  D
  # FIELD_DESC  Weekly data date
  # DESCRIP     Stock Investor version information
  # FM_FILE     SETUP
  
  # FIELD_NAME  SPLITDATE                       
  # FIELD_TYPE  D                                
  # FIELD_DESC  Split date                       
  # DESCRIP     Stock Investor version information  
  # FM_FILE     SETUP   
  
  # SI Pro 4.0 setup
  
   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SETUP"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SETUP"))
    # } else {
      # # load file
      # SETUP <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SETUP"), as.is = TRUE)))
      # save("SETUP",file = getOption("AAIISIPro40PathFileOptim_SETUP"))
    # }
  # } else {
    # # load file
    # SETUP <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SETUP"), as.is = TRUE)))
  # }
  
  SETUP <- get_from_disk("SETUP", filesoptionlist = OPTIONLIST)
  #  BUT: REPLACE IN CODE SETUP <- 
  #                    TO SETUP <- 
  
  SETUP_tbl_sqlite <- copy_to(dpsqllconn, SETUP, temporary = FALSE
    , indexes = list(
    )
  )
  SETUP <- tbl_df(SETUP)
  
  
  # SI_CI ( TYPICALLY BIG: 7 thousand )
  #
  # company information 

   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_CI"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_CI"))
    # } else {
      # # load file
      # SI_CI <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_CI"), as.is = TRUE)))
      # save("SI_CI",file = getOption("AAIISIPro40PathFileOptim_SI_CI"))
    # }
  # } else {
    # # load file
    # SI_CI <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_CI"), as.is = TRUE)))
  # }

  SI_CI <- get_from_disk("SI_CI", filesoptionlist = OPTIONLIST)
  
    # primary_key_dup <- SI_CI[duplicated(SI_CI[,'TICKER']),,drop=FALSE]
    # new_df_no_duplicates <- SI_CI[!(SI_CI$TICKER %in% as.matrix(primary_key_dup)),,drop=FALSE]
    # SI_CI <- new_df_no_duplicates
  
    # primary_key_dup <- SI_CI[duplicated(SI_CI[,'COMPANY_ID']),,drop=FALSE]
    # new_df_no_duplicates <- SI_CI[!(SI_CI$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    # SI_CI <- new_df_no_duplicates
  
    # rm(primary_key_dup,new_df_no_duplicates)
 
    SI_CI <- SI_CI # wierd performance bug ( program runs faster than can it access its variables )
    SI_CI <- eliminate_all_duplicates( SI_CI, "COMPANY_ID" ) 
  
  SI_CI_tbl_sqlite <- copy_to(dpsqllconn, SI_CI, temporary = FALSE
    , indexes = list(
        c("TICKER")
      , c("COMPANY_ID")
      , c("EXCHANGE")  # see UNIVERSE
      , c("IND_2_DIG") # see UNIVERSE
    )
  )
  SI_CI <- tbl_df(SI_CI)

  
  # small LOOK-UP tables
  # read 'sectors and industries'     
  # SI_MGDSC.MG_CODE '2 char: industry'     
  # SI_MGDSC.MG_DESC '4 char' description'
  
   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_MGDSC"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_MGDSC"))
    # } else {
      # # load file
      # SI_MGDSC <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_MGDSC"), as.is = TRUE)))
      # save("SI_MGDSC",file = getOption("AAIISIPro40PathFileOptim_SI_MGDSC"))
    # }
  # } else {
    # # load file
    # SI_MGDSC <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_MGDSC"), as.is = TRUE)))
  # }

  SI_MGDSC <- get_from_disk("SI_MGDSC", filesoptionlist = OPTIONLIST)
  
    # primary_key_dup <- SI_MGDSC[duplicated(SI_MGDSC[,'MG_CODE']),,drop=FALSE]
    # new_df_no_duplicates <- SI_MGDSC[!(SI_MGDSC$MG_CODE %in% as.matrix(primary_key_dup)),,drop=FALSE]
    # SI_MGDSC <- new_df_no_duplicates
    # rm(primary_key_dup,new_df_no_duplicates)
  
    SI_MGDSC <- SI_MGDSC # wierd performance bug ( program runs faster than can it access its variables )
    SI_MGDSC <- eliminate_all_duplicates( SI_MGDSC, "MG_CODE" ) 
  
  SI_MGDSC_tbl_sqlite <- copy_to(dpsqllconn, SI_MGDSC, temporary = FALSE
    , indexes = list(
        c("MG_CODE")
    )
  )
  
  SI_MGDSC <- tbl_df(SI_MGDSC)

  # SI_EXCHG.EXCHG_DESC
  #
  # 3 WAY JOIN
  # SI_CI.EXCHANGE  = SI_EXCHG.EXCHG_CODE AND
  # SI_CI.IND_2_DIG = SI_MGDSC.MG_CODE
  
  # Major stock exchange information 'long descriptions'
  
   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_EXCHG"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_EXCHG"))
    # } else {
      # # load file
      # SI_EXCHG <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_EXCHG"), as.is = TRUE)))
      # save("SI_EXCHG",file = getOption("AAIISIPro40PathFileOptim_SI_EXCHG"))
    # }
  # } else {
    # # load file
    # SI_EXCHG <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_EXCHG"), as.is = TRUE)))
  # }
  
  SI_EXCHG <- get_from_disk("SI_EXCHG", filesoptionlist = OPTIONLIST)
  
    # primary_key_dup <- SI_EXCHG[duplicated(SI_EXCHG[,'EXCHG_CODE']),,drop=FALSE]
    # new_df_no_duplicates <- SI_EXCHG[!(SI_EXCHG$EXCHG_CODE %in% as.matrix(primary_key_dup)),,drop=FALSE]
    # SI_EXCHG <- new_df_no_duplicates
    # rm(primary_key_dup,new_df_no_duplicates)
  
    SI_EXCHG <- SI_EXCHG # wierd performance bug ( program runs faster than can it access its variables )
    SI_EXCHG <- eliminate_all_duplicates( SI_EXCHG, "EXCHG_CODE" ) 
  
  SI_EXCHG_tbl_sqlite <- copy_to(dpsqllconn, SI_EXCHG, temporary = FALSE
    , indexes = list(
        c("EXCHG_CODE")
    )
  )
  
  SI_EXCHG <- tbl_df(SI_EXCHG)


  # SI_PSD.MKTCAP
  #
  # 4 WAY JOIN
  # SI_CI.EXCHANGE = SI_EXCHG.EXCHG_CODE AND
  # SI_CI.IND_2_DIG = SI_MGDSC.MG_CODE AND
  # SI_CI.COMPANY_ID = SI_PSD.COMPANY_ID
  
  # Price and Share Staticsics - Market Capitalization
  
   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_PSD"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_PSD"))
    # } else {
      # # load file
      # SI_PSD <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_PSD"), as.is = TRUE)))
      # save("SI_PSD",file = getOption("AAIISIPro40PathFileOptim_SI_PSD"))
    # }
  # } else {
    # # load file
    # SI_PSD <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_PSD"), as.is = TRUE)))
  # }

  SI_PSD <- get_from_disk("SI_PSD", filesoptionlist = OPTIONLIST)
  
  # primary_key_dup <- SI_PSD[duplicated(SI_PSD[,'COMPANY_ID']),,drop=FALSE]
  # new_df_no_duplicates <- SI_PSD[!(SI_PSD$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
  # SI_PSD <- new_df_no_duplicates
  # rm(primary_key_dup,new_df_no_duplicates)

  SI_PSD <- SI_PSD # wierd performance bug ( program runs faster than can it access its variables )
  SI_PSD <- eliminate_all_duplicates( SI_PSD, "COMPANY_ID" ) 

  # all math must be numeric
  SI_PSD <- mutate(SI_PSD, MKTCAP = as.numeric(MKTCAP) )
  
  SI_PSD_tbl_sqlite <- copy_to(dpsqllconn, SI_PSD, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_PSD <- tbl_df(SI_PSD)

  # 4–4–5 calendar
  # he 4–4–5 calendar is a method of managing accounting periods. 
  # It is a common calendar structure for some industries such as retail, manufacturing and parking industry.

  # The 4–4–5 calendar divides a year into 4 quarters. 
  # Each quarter has 13 weeks, which are grouped into 
   # two 4-week "months" and 
   # one 5-week "month". 
  # The grouping of 13 weeks may also be set up as 5–4–4 weeks or 4–5–4 weeks, 
  # but the 4–4–5 seems to be the most common arrangement.

  # you can still compare a period to the same period in the prior year

  # end date of the period is always the same day of the week

    # BUT
    # has 364 days (7 days * 52 weeks), so that 
    # approximately every 5.6 years there will be a 53-week year, 
    # which can make year-on-year comparison difficult. ( NEEDS OPTIMISTIC ADJUSTMENT ( 365.0/364.0 (X)OR 366.0/364.0 )

  # VARIANTS: Last Saturday of the month at fiscal year end
  # VARIANTS: Saturday nearest the end of month

  # http://en.wikipedia.org/wiki/4%E2%80%934%E2%80%935_calendar


  # FIELD_NAME  PEREND_Q1 (str(UNIVERSE) shows loaded "num" )
  # FIELD_TYPE  D
  # FIELD_DESC  Ending date Q1
  # DESCRIP     Dates and Periods
  # FM_FILE     SI_DATE

  # FIELD_NAME  PERLEN_Q1
  # FIELD_TYPE  C
  # FIELD_DESC  Length of period Q1 ( SIPro 4.0: Field Type: Months )
  # DESCRIP     Dates and Periods
  # FM_FILE     SI_DATE
  
  # FIELD_NAME  PERTYP_Q1
  # FIELD_TYPE  C
  # FIELD_DESC  Period type Q1 ( SIPro 4.0: Field Type: Character (M, W) ) quarterly period [time] length 
  # DESCRIP     Dates and Periods
  # FM_FILE     SI_DATE
  
  # FIELD_NAME  PEREND_Q1
  # FIELD_TYPE  D
  # FIELD_DESC  Ending date Q1
  # DESCRIP     Dates and Periods
  # FM_FILE     SI_DATE
  
  # FIELD_NAME  SPLIT_DATE
  # FIELD_TYPE  D
  # FIELD_DESC  Split Date
  # DESCRIP     Price and Share Statistics
  # FM_FILE     SI_PSD
  
  # FIELD_NAME  SPLIT_FACT
  # FIELD_TYPE  C
  # FIELD_DESC  Split Factor
  # DESCRIP     Price and Share Statistics
  # FM_FILE     SI_PSD
  
  # NEW_KID
  
  SI_PSDD <- get_from_disk("SI_PSDD", filesoptionlist = OPTIONLIST)
  
  # SI_PSDD <- SI_PSDD 
  SI_PSDD <- eliminate_all_duplicates( SI_PSDD, "COMPANY_ID" ) 

  SI_PSDD_tbl_sqlite <- copy_to(dpsqllconn, SI_PSDD, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_PSDD <- tbl_df(SI_PSDD)
  
  
  # Prices - Dates (Close) Price-Date M001-M120

   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_PSDC"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_PSDC"))
    # } else {
      # # load file
      # SI_PSDC <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_PSDC"), as.is = TRUE)))
      # save("SI_PSDC",file = getOption("AAIISIPro40PathFileOptim_SI_PSDC"))
    # }
  # } else {
    # # load file
    # SI_PSDC <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_PSDC"), as.is = TRUE)))
  # }

  SI_PSDC <- get_from_disk("SI_PSDC", filesoptionlist = OPTIONLIST)
  
  # primary_key_dup <- SI_PSDC[duplicated(SI_PSDC[,'COMPANY_ID']),,drop=FALSE]
  # new_df_no_duplicates <- SI_PSDC[!(SI_PSDC$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
  # SI_PSDC <- new_df_no_duplicates
  # rm(primary_key_dup,new_df_no_duplicates)

  SI_PSDC <- SI_PSDC # wierd performance bug ( program runs faster than can it access its variables )
  SI_PSDC <- eliminate_all_duplicates( SI_PSDC, "COMPANY_ID" ) 

  SI_PSDC_tbl_sqlite <- copy_to(dpsqllconn, SI_PSDC, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_PSDC <- tbl_df(SI_PSDC)
  
  # DD_FILE     SI_PSDC
  # FIELD_NAME  PRICE_M001
  # FIELD_TYPE  C
  # FIELD_DESC  Price M001
  # DESCRIP     Prices - Monthly Close
  # FM_FILE     SI_PSDC 
  
  
  # Dates and Periods - Ending date Q1
  
   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_DATE"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_DATE"))
    # } else {
      # # load file
      # SI_DATE <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_DATE"), as.is = TRUE)))
      # save("SI_DATE",file = getOption("AAIISIPro40PathFileOptim_SI_DATE"))
    # }
  # } else {
    # # load file
    # SI_DATE <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_DATE"), as.is = TRUE)))
  # }

  SI_DATE <- get_from_disk("SI_DATE", filesoptionlist = OPTIONLIST)
  
    # primary_key_dup <- SI_DATE[duplicated(SI_DATE[,'COMPANY_ID']),,drop=FALSE]
    # new_df_no_duplicates <- SI_DATE[!(SI_DATE$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    # SI_DATE <- new_df_no_duplicates
    # rm(primary_key_dup,new_df_no_duplicates)
  
    SI_DATE <- SI_DATE # wierd performance bug ( program runs faster than can it access its variables )
    SI_DATE <- eliminate_all_duplicates( SI_DATE, "COMPANY_ID" ) 
  
  SI_DATE_tbl_sqlite <- copy_to(dpsqllconn, SI_DATE, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_DATE <- tbl_df(SI_DATE)

  mydate <- function(numdate) { as.character(dates(numdate)) }
  
  # end load minimal data
  
  # begin elimintate un-investibles ( keep investibles) ( All Stocks)
 
  # later row_number get biased high values/ntiles  # early row_number get biased low/ntiles
  # ntile ( value == same ) problem
  # 'all stocks' to do better than 'large stocks' ( in this era!? )
  # sort by MKTCAP DESC
  # NOTE: no change in final top 25 seen 
  # ( but because of the nature of ntiles AND ties and row_number, THIS makes me 'feel' better )
  
  # one record (main.SETUP SETUP): this will be a cartsian product
  
           # -- STP.MONTHDATE AS MONTHDATEUNX, STP.WEEKDATE AS WEEKDATEUNX, STP.SPLITDATE AS SPLITDATEUNX, 
           # -- main.SETUP STP, 
  
  UNIVERSE <- sqldf(" 
    SELECT STP.MONTHDATE AS MONTHDATEUNX, STP.WEEKDATE AS WEEKDATEUNX, STP.SPLITDATE AS SPLITDATEUNX, 
           CI.TICKER, CI.COMPANY, CI.COMPANY_ID, CI.SIC, CI.EXCHANGE, EXCHG.EXCHG_DESC 
         , CI.IND_2_DIG, MGDSC.MG_DESC, CI.COUNTRY, CI.ADR 
                           , DTE.PEREND_Q1 AS PERENDUNX_Q1, DTE.PEREND_Q2 AS PERENDUNX_Q2, DTE.PEREND_Q3 AS PERENDUNX_Q3, DTE.PEREND_Q4 AS PERENDUNX_Q4 
                           , DTE.PEREND_Q5 AS PERENDUNX_Q5, DTE.PEREND_Q6 AS PERENDUNX_Q6, DTE.PEREND_Q7 AS PERENDUNX_Q7, DTE.PEREND_Q8 AS PERENDUNX_Q8 
                           , DTE.PERLEN_Q1 AS PERLEN_Q1__integer, PERTYP_Q1 
                           , PSD.MKTCAP AS MKTCAP__numeric, PSD.PRICE AS PRICE__numeric, PSD.SPLIT_DATE AS PSD_SPLITUNX_DATE, PSD.SPLIT_FACT AS SPLIT_FACT__numeric 
                           , PSDC.PRICE_M023 AS PRICE_M023__numeric, PSDC.PRICE_M022 AS PRICE_M022__numeric, PSDC.PRICE_M021 AS PRICE_M021__numeric, PSDC.PRICE_M020 AS PRICE_M020__numeric
                           , PSDC.PRICE_M019 AS PRICE_M019__numeric, PSDC.PRICE_M018 AS PRICE_M018__numeric, PSDC.PRICE_M017 AS PRICE_M017__numeric, PSDC.PRICE_M016 AS PRICE_M016__numeric
                           , PSDC.PRICE_M015 AS PRICE_M015__numeric, PSDC.PRICE_M014 AS PRICE_M014__numeric, PSDC.PRICE_M013 AS PRICE_M013__numeric, PSDC.PRICE_M012 AS PRICE_M012__numeric 
                           , PSDC.PRICE_M011 AS PRICE_M011__numeric, PSDC.PRICE_M010 AS PRICE_M010__numeric, PSDC.PRICE_M009 AS PRICE_M009__numeric, PSDC.PRICE_M008 AS PRICE_M008__numeric 
                           , PSDC.PRICE_M007 AS PRICE_M007__numeric, PSDC.PRICE_M006 AS PRICE_M006__numeric, PSDC.PRICE_M005 AS PRICE_M005__numeric, PSDC.PRICE_M004 AS PRICE_M004__numeric 
                           , PSDC.PRICE_M003 AS PRICE_M003__numeric, PSDC.PRICE_M002 AS PRICE_M002__numeric, PSDC.PRICE_M001 AS PRICE_M001__numeric  
                            , PRICEDM001 AS PRICEDM001UNX, PRICEDM002 AS PRICEDM002UNX, PRICEDM003 AS PRICEDM003UNX, PRICEDM004 AS PRICEDM004UNX 
                            , PRICEDM005 AS PRICEDM005UNX, PRICEDM006 AS PRICEDM006UNX, PRICEDM007 AS PRICEDM007UNX, PRICEDM008 AS PRICEDM008UNX 
                            , PRICEDM009 AS PRICEDM009UNX, PRICEDM010 AS PRICEDM010UNX, PRICEDM011 AS PRICEDM011UNX, PRICEDM012 AS PRICEDM012UNX 
                            , PRICEDM013 AS PRICEDM013UNX, PRICEDM014 AS PRICEDM014UNX, PRICEDM015 AS PRICEDM015UNX, PRICEDM016 AS PRICEDM016UNX 
                          ,SHR_AQ1 AS SHR_AQ1__numeric, SHR_AQ2 AS SHR_AQ2__numeric, SHR_AQ3 AS SHR_AQ3__numeric, SHR_AQ4 AS SHR_AQ4__numeric 
                          ,SHR_AQ5 AS SHR_AQ5__numeric, SHR_AQ6 AS SHR_AQ6__numeric, SHR_AQ7 AS SHR_AQ7__numeric, SHR_AQ8 AS SHR_AQ8__numeric 
                           FROM 
                           main.SETUP STP, 
                           main.SI_CI CI, main.SI_EXCHG EXCHG, main.SI_MGDSC MGDSC, main.SI_PSD PSD, main.SI_DATE DTE, main.SI_PSDC PSDC, main.SI_PSDD PSDD
                           WHERE CI.EXCHANGE = EXCHG.EXCHG_CODE AND 
                           CI.IND_2_DIG = MGDSC.MG_CODE AND 
                           CI.COMPANY_ID = PSD.COMPANY_ID AND 
                           CI.COMPANY_ID = DTE.COMPANY_ID AND
                           CI.COMPANY_ID = PSDC.COMPANY_ID AND
                           CI.COMPANY_ID = PSDD.COMPANY_ID 
    ORDER BY MKTCAP DESC 
                           "  , connection = dpsqllconn$con, method="name__class")
  
  # preserve the original ordering
  
  # UNIVERSE[,"ORIG_ORDER"]   <- 1:NROW(UNIVERSE) # str() - shows integer , Rstudio GUI shows numeric
  UNIVERSE <- mutate(UNIVERSE, ORIG_ORDER = 1:NROW(UNIVERSE) )
  
  # 0.4 seconds - a work in progress
  # library(compiler)
  # lubridate:::add_period_to_date
  # add_period_to_date_comp <- cmpfun(lubridate:::add_period_to_date)
  # UNIVERSE[,"PERENDDT_Q1"]  <- as.character( add_period_to_date_comp( dmy("1/1/1970",tz = "EST"), days(UNIVERSE[,"PERENDUNX_Q1"]) ))
  # Error in as.POSIXlt.numeric(date) : 'origin' must be supplied 
  
  
  # 0.4 seconds
  # from
  # dmy("1/1/1970",tz = "EST")
  # to
  # 0.16 seconds
  # EDT is 4 hours behind of Coordinated Universal Time (UTC)
  # http://www.timeanddate.com/library/abbreviations/timezones/na/edt.html
  # ymd_hms(c("2013-01-24 16:00:00.880-0400")) 
  # [1] "2013-01-24 20:00:00 UTC"
  
  # lineprof & shine RE-FUTURE TO TEST PERFORMANCE
   

  UNIVERSE <- mutate(UNIVERSE, 
                                                                     # ##UNX: renamed column, already loaded as num    
      MONTHDATEDT = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(MONTHDATEUNX))  # sqldf I MADE this NUMERIC                                                     
    , WEEKDATEDT  = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(WEEKDATEUNX))                                                                            
    , SPLITDATEDT = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(SPLITDATEUNX))                                                                 
    , PERENDDT_Q2 = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(PERENDUNX_Q2))                                                        
    , PERENDDT_Q1 = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(PERENDUNX_Q1))
    , PSD_SPLITDT_DATE = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(PSD_SPLITUNX_DATE))
  )
 
  
  # library(compiler)
  # addboth <-function(a,b) a + b
  # addboth_comp <- cmpfun(addboth)
  # UNIVERSE[,"PERENDDT_Q1"]  <- as.character(addboth_comp(dmy("1/1/1970",tz = "EST"),days(UNIVERSE[,"PERENDUNX_Q1"])))
  
  # this PERENDDT_Q0 ( almost future) is not useful ( I am removing )
  ## not NA
  # UNIVERSE[,"PERENDDT_Q0"]  <- as.character(dmy("1/1/1970",tz = "EST") + days(UNIVERSE[,"PERENDUNX_Q1"])  ) 
  
  ## months
  
  # UNIVERSE[,"PERENDDT_Q0"]  <- ifelse(UNIVERSE[,"PERTYP_Q1"] == "M",
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <- ifelse(UNIVERSE[,"PERTYP_Q1"] == "M" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <- ifelse(UNIVERSE[,"PERTYP_Q1"] == "M" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(3) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  ## weeks
  
  # UNIVERSE[,"PERENDDT_Q0"]  <- ifelse(UNIVERSE[,"PERTYP_Q1"] == "W",
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <- ifelse(UNIVERSE[,"PERTYP_Q1"] == "W" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <- ifelse(UNIVERSE[,"PERTYP_Q1"] == "W" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(3) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )
 
 

 
 
 
  # end of compute the SMA price per sector ( weighted by sector element 'last known market cap' )


    # 'all stocks' universe
  
  # NOT loaded into SQLite YET
  # sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # NOT dplyrized YET
  # strip off
  # UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
        c("ADR","MKTCAP","COUNTRY","EXCHG_DESC")
    )
  )
  
  UNIVERSE  <- sqldf("SELECT * FROM main.UNIVERSE UNIV WHERE 
                           UNIV.ADR == 0 AND 
                           UNIV.MKTCAP > 220.0 AND 
                           UNIV.COUNTRY == 'United States' AND 
                           UNIV.EXCHG_DESC NOT IN ('Over the counter') 
                           ", connection = dpsqllconn$con)
  
  UNIVERSE <- tbl_df(UNIVERSE)
  

  # end elimintate un-investibles ( keep investibles) ( All Stocks)
  

  # Dividend-Ex Date
  # Data Table Name: DIVNQXDT
  # Data Category: Income Statement ? Quarterly
  # Field Type: Date (MM/DD/YYYY)
  
  # DD_FILE     SI_ISQ
  # FIELD_NAME  DIVNQXDT
  # FIELD_TYPE  D
  # FIELD_DESC  Dividend-Ex Date
  # DESCRIP     Income Statement - Quarterly
  # FM_FILE     SI_ISQ
  
  # Dividend-Pmt Date
  # Data Table Name: DIVNQPDT
  # Data Category: Income Statement ? Quarterly
  # Field Type: Date (MM/DD/YYYY)
  
  # DD_FILE     SI_ISQ
  # FIELD_NAME  DIVNQPDT
  # FIELD_TYPE  D
  # FIELD_DESC  Dividend-Pmt Date
  # DESCRIP     Income Statement - Quarterly
  # FM_FILE     SI_ISQ
  
  
  # have an 'annual' 'EPS change' greater than zero (0)'
  # LAST of growth expose
  
  # FIELD_NAME  EPS_Q1
  # FIELD_DESC  EPS Q1
  # DESCRIP     Income Statement - Quarterly
  # FM_FILE     SI_ISQ
  
  # if(getOption("FileStoreStyle") == "Optimized") {
  # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_ISQ"))) {
  # load(file = getOption("AAIISIPro40PathFileOptim_SI_ISQ"))
  # } else {
  # # load file
  # SI_ISQ <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_ISQ"), as.is = TRUE)))
  # save("SI_ISQ",file = getOption("AAIISIPro40PathFileOptim_SI_ISQ"))
  # }
  # } else {
  # # load file
  # SI_ISQ <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_ISQ"), as.is = TRUE)))
  # }
  
  SI_ISQ <- get_from_disk("SI_ISQ", filesoptionlist = OPTIONLIST)
  
  # primary_key_dup <- SI_ISQ[duplicated(SI_ISQ[,'COMPANY_ID']),,drop=FALSE]
  # new_df_no_duplicates <- SI_ISQ[!(SI_ISQ$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
  # SI_ISQ <- new_df_no_duplicates
  # rm(primary_key_dup,new_df_no_duplicates)
  
  SI_ISQ <- SI_ISQ # wierd performance bug ( program runs faster than can it access its variables )
  SI_ISQ <- eliminate_all_duplicates( SI_ISQ, "COMPANY_ID" ) 
  
  SI_ISQ_tbl_sqlite <- copy_to(dpsqllconn, SI_ISQ, temporary = FALSE
                               , indexes = list(
                                 c("COMPANY_ID")
                               )
  )
  SI_ISQ <- tbl_df(SI_ISQ)
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)
  
  # strip off
  UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
                                 , indexes = list(
                                   #    c("TICKER")
                                   # ,  
                                   c("COMPANY_ID")
                                   # ,  c("ORIG_ORDER")
                                   
                                 )
  )
  
  UNIVERSE <- sqldf("SELECT UNIV.*, ISQ.EPS_Q1 AS EPS_Q1__numeric, ISQ.EPS_Q2 AS EPS_Q2__numeric, ISQ.EPS_Q3 AS EPS_Q3__numeric, ISQ.EPS_Q4 AS EPS_Q4__numeric,         -- comment
                    ISQ.EPS_Q5 AS EPS_Q5__numeric, ISQ.EPS_Q6 AS EPS_Q6__numeric, ISQ.EPS_Q7 AS EPS_Q7__numeric, ISQ.EPS_Q8 AS EPS_Q8__numeric, 
                    ISQ.EPSD_Q1 AS EPSD_Q1__numeric, ISQ.EPSD_Q2 AS EPSD_Q2__numeric, ISQ.EPSD_Q3 AS EPSD_Q3__numeric, ISQ.EPSD_Q4 AS EPSD_Q4__numeric, -- comment
                    ISQ.EPSD_Q5 AS EPSD_Q5__numeric, ISQ.EPSD_Q6 AS EPSD_Q6__numeric, ISQ.EPSD_Q7 AS EPSD_Q7__numeric, ISQ.EPSD_Q8 AS EPSD_Q8__numeric, 
                    ISQ.DIVNQXDT AS DIVNQXDTUNX, ISQ.DIVNQPDT AS DIVNQPDTUNX 
                    FROM 
                    main.UNIVERSE UNIV, main.SI_ISQ ISQ WHERE 
                    UNIV.COMPANY_ID = ISQ.COMPANY_ID 
                    ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <- tbl_df(UNIVERSE)




  
  ###########################################################################################
  ################  VISUALLY SEE THE 10-M MA AT LAST KNOWN MONTH END ########################

  # caluclate after 3000 chosen
  # compute the SMA price per sector ( weighted by sector element 'last known market cap' )

  # 2008 book Stocks for the Long Run 5/E: The Definitive Guide to 
  # Financial Market Returns & Long-Term Investment Strategies, 
  # Jeremy Siegel investigates the use of the 200-day SMA in timing the Dow Jones Industrial Average 
  # (DJIA) from 1886 to 2006. His test bought the DJIA when it closed at least 1 percent above the 
  # 200-day moving average, and sold the DJIA and invested in Treasury bills when it closed 
  # at least 1 percent below the 200-day moving average.
  
  # A_Quantative_Approach_to_Tactical_Asset_Allocation(2006)(2009)(2013)(Faber)_Ryan_Article_Example_blotter_quantstrat.pdf
  # Relative_Strength_Strategies_for_Investing(2013)(Faber).pdf
  
  # # SIPro Adjusts splits for me: I just need PRICE_M001 
  # S&P 500 index is .. cap-weighted
  # components are weighted according to the total market value of their outstanding shares.
  # http://en.wikipedia.org/wiki/Capitalization-weighted_index

  # not fully-correct ( but 'maybe' good enough)
  # I would have to know the 'per month market capitalization
  # that is, #_shares_outstanding_at_the end_of_that_month
  # I could/may use Q# MKTCAP_ Q1, and locf ( last observation carried forward for a better approx
  # TTR::SMA
  # stats::weighted.mean
  
  #  Berkshire Hathaway BRK.A cuases my Financial Sector moving average to be above 10,000
  #  500 financial companies of 3000
  
  # dbGetQuery(conn = dpsqllconn$con, "SELECT MG_DESC, TICKER, MKTCAP, PRICE  FROM UNIVMA WHERE MG_DESC = 'Financial' ORDER BY PRICE DESC")
        # MG_DESC TICKER                     MKTCAP                        PRICE
  # 1   Financial  BRK.A 337727.4000000000232830644 205635.000000000000000000000
  # 2   Financial    MKL   8936.5000000000000000000    639.649999999999977262632
  # 3   Financial    WTM   3881.3000000000001818989    632.059999999999945430318
  # 4   Financial      Y   6977.6999999999998181011    426.180000000000006821210

  # ...
  # dbGetQuery(conn = dpsqllconn$con, "SELECT MG_DESC, TICKER, MKTCAP, PRICE  FROM UNIVMA WHERE MG_DESC = 'Financial' ORDER BY MKTCAP DESC")

  # dbGetQuery(conn = dpsqllconn$con, "SELECT MG_DESC, TICKER, MKTCAP, PRICE  FROM UNIVMA WHERE MG_DESC = 'Financial' ORDER BY MKTCAP DESC")
        # MG_DESC TICKER                     MKTCAP                        PRICE
  # 1   Financial  BRK.A 337727.4000000000232830644 205635.000000000000000000000
  # 2   Financial    WFC 269895.0000000000000000000     51.700000000000002842171
  # 3   Financial    JPM 225789.7000000000116415322     60.030000000000001136868
  # 4   Financial    BAC 176561.2999999999883584678     16.789999999999999147349
  # 5   Financial      C 158804.2999999999883584678     52.380000000000002557954

  #  O SHAUNNESSY NO-NON NORMAL DISTRIBUTION 'NO-BURN RULE' ( USE QUANTILES INSTEAD OF AVERAGES )
  #  I SUSPECT THAT A 'MOVING MEDIAN' MAY BE BETTER ( I WOULD HAVE TO QUANSTRAT BACKTEST THAT )
  #  I WOULD HAVE TO RESEARCH THIS LATER

  # JUST ELIMINATE Berkshire Hathaway BRK.A

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 

  # NOTE IF I ADD MORE OR LESS is.na THEN OUTPUTS WILL SIGHTLY CHANGE
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, # NOTE IF I ADD MORE is.na THE OUTPUTS WILL SIGHTLY CHANGE
      is.na(MKTCAP)      == FALSE
    , is.na(PRICE)       == FALSE
    # , is.na(PRICE_M023)  == FALSE
    # , is.na(PRICE_M022)  == FALSE
    # , is.na(PRICE_M021)  == FALSE
    # , is.na(PRICE_M020)  == FALSE
    # , is.na(PRICE_M019)  == FALSE
    # , is.na(PRICE_M018)  == FALSE
    # , is.na(PRICE_M017)  == FALSE
    # , is.na(PRICE_M016)  == FALSE
    # , is.na(PRICE_M015)  == FALSE
    # , is.na(PRICE_M014)  == FALSE
    # , is.na(PRICE_M013)  == FALSE
    # , is.na(PRICE_M012)  == FALSE
    # , is.na(PRICE_M011)  == FALSE
    , is.na(PRICE_M010)  == FALSE
    , is.na(PRICE_M009)  == FALSE
    , is.na(PRICE_M008)  == FALSE
    , is.na(PRICE_M007)  == FALSE
    , is.na(PRICE_M006)  == FALSE
    , is.na(PRICE_M005)  == FALSE
    , is.na(PRICE_M004)  == FALSE
    , is.na(PRICE_M003)  == FALSE
    , is.na(PRICE_M002)  == FALSE
    , is.na(PRICE_M001)  == FALSE
    ,       TICKER       != 'BRK.A'
  )
 
 
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA, PRICE_M001_TO_M011_NO_NA = as.numeric(  
    1.0
  ))
 
  # FUTURE ( COULD/SHOULD/WOULD USE THIS FOR TRENDY DATA ) 
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA, PRICE_WGHT_MEAN_SMA_10_M_SECTOR = as.numeric(SMA(c(
                                       #    weighted.mean(PRICE_M011, MKTCAP), 
       weighted.mean(PRICE_M010, MKTCAP),  weighted.mean(PRICE_M009, MKTCAP), 
       weighted.mean(PRICE_M008, MKTCAP),  weighted.mean(PRICE_M007, MKTCAP), 
       weighted.mean(PRICE_M006, MKTCAP),  weighted.mean(PRICE_M005, MKTCAP), 
       weighted.mean(PRICE_M004, MKTCAP),  weighted.mean(PRICE_M003, MKTCAP), 
       weighted.mean(PRICE_M002, MKTCAP),  weighted.mean(PRICE_M001, MKTCAP) 
     ), 10)[10]
  ))
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA, PRICE_WGHT_MEAN_SECTOR = as.numeric(
    weighted.mean(PRICE, MKTCAP) 
  ))
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA, PRICE_WGHT_MEAN_SMA_10_M_SECTOR_SVVR = as.numeric(
    ifelse( PRICE_WGHT_MEAN_SECTOR > PRICE_WGHT_MEAN_SMA_10_M_SECTOR , 1.0, 0.0 )
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # View(data.frame(UNIVERSE)[,c("MG_DESC","PRICE_M001_TO_M011_NO_NA","PRICE_WGHT_MEAN_SECTOR","PRICE_WGHT_MEAN_SMA_10_M_SECTOR","PRICE_WGHT_MEAN_SMA_10_M_SECTOR_SVVR")])
  
  # OUTPUT IS RETURNED TO UNIVERSE_FMA[,c("SCT_MN_PRCE_10M_SMA","SCT_MN_PRCE")]
  
  ######## END OF VISUALLY SEE THE 10-M MA AT LAST KNOWN MONTH END ############
  #############################################################################

  
  #############################################################################
  ################  VISUALLY SEE THE 10-M MA PER MONTH ########################

  PRICE_WGHT_MEAN_UNIVERSE_NOT_NA  <- 
    UNIVERSE[,c("MG_DESC", "MKTCAP", "PRICE", "TICKER"
      , "PRICE_M001", "PRICE_M002", "PRICE_M003"
      , "PRICE_M004", "PRICE_M005", "PRICE_M006" 
      , "PRICE_M007", "PRICE_M008", "PRICE_M009"
      , "PRICE_M010", "PRICE_M011", "PRICE_M012"
      , "PRICE_M013", "PRICE_M014", "PRICE_M015"
      , "PRICE_M016", "PRICE_M017", "PRICE_M018"
      , "PRICE_M019"                                                  
  ),drop=FALSE] 
  # 023 ( actually)
                                                                        # "MG_DESC", "MKTCAP", "PRICE", "TICKER"
  PRICE_WGHT_MEAN_UNIVERSE_NOT_NA  <- PRICE_WGHT_MEAN_UNIVERSE_NOT_NA[,c(1,2,3,4, rev(5:length(colnames(PRICE_WGHT_MEAN_UNIVERSE_NOT_NA)))) ]  

  PRICE_WGHT_MEAN_UNIVERSE_NOT_NA  <- group_by(PRICE_WGHT_MEAN_UNIVERSE_NOT_NA,MG_DESC)

  # NOTE IF I ADD MORE OR LESS is.na THEN OUTPUTS WILL SIGHTLY CHANGE
  PRICE_WGHT_MEAN_UNIVERSE_NOT_NA <- filter(PRICE_WGHT_MEAN_UNIVERSE_NOT_NA, 
      is.na(MKTCAP)      == FALSE
    , is.na(PRICE)       == FALSE
    ,       TICKER       != 'BRK.A'
  #     , is.na(PRICE_M023)  == FALSE
  #     , is.na(PRICE_M022)  == FALSE
  #     , is.na(PRICE_M021)  == FALSE
  #     , is.na(PRICE_M020)  == FALSE
    , is.na(PRICE_M019)  == FALSE
    , is.na(PRICE_M018)  == FALSE
    , is.na(PRICE_M017)  == FALSE
    , is.na(PRICE_M016)  == FALSE
    , is.na(PRICE_M015)  == FALSE
    , is.na(PRICE_M014)  == FALSE
    , is.na(PRICE_M013)  == FALSE
    , is.na(PRICE_M012)  == FALSE
    , is.na(PRICE_M011)  == FALSE
    , is.na(PRICE_M010)  == FALSE
    , is.na(PRICE_M009)  == FALSE
    , is.na(PRICE_M008)  == FALSE
    , is.na(PRICE_M007)  == FALSE
    , is.na(PRICE_M006)  == FALSE
    , is.na(PRICE_M005)  == FALSE
    , is.na(PRICE_M004)  == FALSE
    , is.na(PRICE_M003)  == FALSE
    , is.na(PRICE_M002)  == FALSE
    , is.na(PRICE_M001)  == FALSE

  )

  wght_mn_price_grid_do <- function(x) {
    
    data.frame( 
                                                             weighted.mean(x[["PRICE_M019"]], x[["MKTCAP"]]),
           weighted.mean(x[["PRICE_M018"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M017"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M016"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M015"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M014"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M013"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M012"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M011"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M010"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M009"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M008"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M007"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M006"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M005"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M004"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M003"]], x[["MKTCAP"]]), 
           weighted.mean(x[["PRICE_M002"]], x[["MKTCAP"]]),  weighted.mean(x[["PRICE_M001"]], x[["MKTCAP"]])
    ) -> weighted.means_price_mo_sequence # JUST ONE LONG RECORD

    c(     
                             "WGHT_MN_PRICE_M019", 
      "WGHT_MN_PRICE_M018",  "WGHT_MN_PRICE_M017",
      "WGHT_MN_PRICE_M016",  "WGHT_MN_PRICE_M015",
      "WGHT_MN_PRICE_M014",  "WGHT_MN_PRICE_M013",
      "WGHT_MN_PRICE_M012",  "WGHT_MN_PRICE_M011", 
      "WGHT_MN_PRICE_M010",  "WGHT_MN_PRICE_M009",
      "WGHT_MN_PRICE_M008",  "WGHT_MN_PRICE_M007",
      "WGHT_MN_PRICE_M006",  "WGHT_MN_PRICE_M005",
      "WGHT_MN_PRICE_M004",  "WGHT_MN_PRICE_M003",       
      "WGHT_MN_PRICE_M002",  "WGHT_MN_PRICE_M001"
    ) -> colnames(weighted.means_price_mo_sequence)
    
    data.frame( WGHT_MN_PRICE = weighted.mean(x[["PRICE"]], x[["MKTCAP"]]))  -> weighted.means_price_now
    
    c("WGHT_MN_PRICE") -> colnames(weighted.means_price_now)

    data.frame(as.list( SMA(t(as.matrix(weighted.means_price_mo_sequence)),10))) -> weighted.means_price_mo_sequence_sma_10

    paste0("PRICE_SMA_10_AT_M",19:1) -> colnames(weighted.means_price_mo_sequence_sma_10)
    
    cbind(  weighted.means_price_mo_sequence_sma_10  , weighted.means_price_now , x[["MG_DESC"]][1] ) -> y

    c("MG_DESC") -> colnames(y)[length(y)]
    
    # get rid of excess NA columns for MA that I do not have enough data
    y[,-1*c(1:(19 - 10))] -> y
    
    return(y)

  }

  WGHT_MN_PRICE_GRID <- suppressWarnings(do(PRICE_WGHT_MEAN_UNIVERSE_NOT_NA, wght_mn_price_grid_do(.))) 
  
  # In rbind_all(out[[1]]) : Unequal factor levels: coercing to character    
  # http://stackoverflow.com/questions/24609112/how-to-convert-a-list-of-lists-to-a-dataframe-non-identical-lists
  
  # new variable
  # View(WGHT_MN_PRICE_GRID)

  PRICE_WGHT_MEAN_UNIVERSE_NOT_NA <- ungroup(PRICE_WGHT_MEAN_UNIVERSE_NOT_NA) 
  rm(PRICE_WGHT_MEAN_UNIVERSE_NOT_NA)
    
  WGHT_MN_PRICE_GRID <- ungroup(WGHT_MN_PRICE_GRID) 
  
  # OUTPUT IS RETURNED TO    WGHT_MN_PRICE_GRID
    
  ############### END OF VISUALLY SEE THE 10-M MA PER MONTH ###################
  #############################################################################


  #############################################################################
  ################  VISUALLY SEE THE RETURN_PER_DOLLAR ########################
                                      
  # TO_DO [ ] END: NEED TO 'REALLY' MAKE 'NA' ... complete.cases, zoo::na.trim


  # TO_DO [ ] END: NEED TO 'REALLY' MAKE 'NA' ... complete.cases, zoo::na.trim

  RET_DOLLAR_PRICE_UNIVERSE_NOT_NA <- 
  UNIVERSE[,c("MG_DESC", "MKTCAP", "PRICE", "TICKER", "COMPANY_ID" 
              , "PRICEDM001UNX", "PRICEDM002UNX", "PRICEDM003UNX"  # 
              , "PRICEDM004UNX", "PRICEDM005UNX", "PRICEDM006UNX" 
              , "PRICEDM007UNX", "PRICEDM008UNX", "PRICEDM009UNX"
              , "PRICEDM010UNX", "PRICEDM011UNX", "PRICEDM012UNX"
              , "PRICEDM013UNX", "PRICEDM014UNX", "PRICEDM015UNX"
              , "PRICEDM016UNX"   
              , "PRICE_M001", "PRICE_M002", "PRICE_M003"        # PRICE_M001__numeric
              , "PRICE_M004", "PRICE_M005", "PRICE_M006" 
              , "PRICE_M007", "PRICE_M008", "PRICE_M009"
              , "PRICE_M010", "PRICE_M011", "PRICE_M012"
              , "PRICE_M013", "PRICE_M014", "PRICE_M015"
              , "PRICE_M016"   
              , "PERENDUNX_Q1", "PERENDUNX_Q2", "PERENDUNX_Q3"  # PERENDUNX_Q1
              , "PERENDUNX_Q4", "PERENDUNX_Q5", "PERENDUNX_Q6" 
              , "PERENDUNX_Q7", "PERENDUNX_Q8"  
              , "SHR_AQ1", "SHR_AQ2", "SHR_AQ3"                 # SHR_AQ1__numeric         
              , "SHR_AQ4", "SHR_AQ5", "SHR_AQ6" 
              , "SHR_AQ7", "SHR_AQ8"
              , "EPSD_Q1", "EPSD_Q2", "EPSD_Q3"                 # EPSD_Q1__numeric         
              , "EPSD_Q4", "EPSD_Q5", "EPSD_Q6" 
              , "EPSD_Q7", "EPSD_Q8"
              
  ),drop=FALSE] 
              
  RET_DOLLAR_PRICE_UNIVERSE_NOT_NA  <- group_by(RET_DOLLAR_PRICE_UNIVERSE_NOT_NA,MG_DESC)
             
  # TO_DO [ ] END: NEED TO 'REALLY' MAKE 'NA' ... complete.cases, zoo::na.trim
             
  # RET_DOLLAR_PRICE_UNIVERSE_NOT_NA <- filter(RET_DOLLAR_PRICE_UNIVERSE_NOT_NA, 
  # )  
             
  ret_dollar_price_grid_do <- function(x) {

    # for zoo::coredata needing to be a numeric matrix
    # transform the (old-ticker) 'company identifer(hexadecimal) into something more 
    # 1. 'numerically base 10 flexible' AND 
    # 2. 'data.frame column name flexible'
    # help from stringr and base
    
    # SUPRISE - AAII
    # NOT HEXADIMAL:  "9396N" "2091N" 
    # ( but "N" is the only character out of the hexadecimal range - WHAT does "N" mean? )
    # is it just a 'space' padding?
    # (currently) just use NROW instead
    
    # safekeeping in case I have any use later
    data.frame( 
      TICKER             = x[,c("TICKER")]
      , COMPANY_ID       = x[,c("COMPANY_ID")]
      , COMPANY_ID_CID10 = str_join("CID", 1:NROW(x))  
      , stringsAsFactors = FALSE
    ) -> COMPANY_INFO
    
    # add a column at the end to x
    COMPANY_INFO[,"COMPANY_ID_CID10",drop=FALSE] -> x[,"COMPANY_ID_CID10"]

    # ? zoo::as.Date as.Date(x, frac = 1)
    
    # should HAVE BEEN unique and the END OF THE MONTH found out NOT TO BE SO
    # garantee that I AM AT month-end
    # NOTE: side affect 'this current month-time' is rounded up to the 'end of this month'
    # maybe I want to ROUND DOWN?! ( WOULD ALSO HAVE TO TO 'min' and zoo as.Date 'frac = 0')
    # hard note: some companies do not have recent prices  so M001 or M004
    #   can be found shifted back several months
    # THIS AREA ONLY GARANTEES that any 'monthly date' is shift to be an 'END OF MONTH date'
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM001UNX)) , frac = 1)) -> x$PRICEDM001UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM002UNX)) , frac = 1)) -> x$PRICEDM002UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM003UNX)) , frac = 1)) -> x$PRICEDM003UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM004UNX)) , frac = 1)) -> x$PRICEDM004UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM005UNX)) , frac = 1)) -> x$PRICEDM005UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM006UNX)) , frac = 1)) -> x$PRICEDM006UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM007UNX)) , frac = 1)) -> x$PRICEDM007UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM008UNX)) , frac = 1)) -> x$PRICEDM008UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM009UNX)) , frac = 1)) -> x$PRICEDM009UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM010UNX)) , frac = 1)) -> x$PRICEDM010UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM011UNX)) , frac = 1)) -> x$PRICEDM011UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM012UNX)) , frac = 1)) -> x$PRICEDM012UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM013UNX)) , frac = 1)) -> x$PRICEDM013UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM014UNX)) , frac = 1)) -> x$PRICEDM014UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM015UNX)) , frac = 1)) -> x$PRICEDM015UNX
    as.numeric(as.Date(as.yearmon(as.Date(x$PRICEDM016UNX)) , frac = 1)) -> x$PRICEDM016UNX
    
    # others need to round-up the end of the month I CAN do THAT here
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q1)) , frac = 1)) -> x$PERENDUNX_Q1
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q2)) , frac = 1)) -> x$PERENDUNX_Q2
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q3)) , frac = 1)) -> x$PERENDUNX_Q3
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q4)) , frac = 1)) -> x$PERENDUNX_Q4
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q5)) , frac = 1)) -> x$PERENDUNX_Q5
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q6)) , frac = 1)) -> x$PERENDUNX_Q6
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q7)) , frac = 1)) -> x$PERENDUNX_Q7
    as.numeric(as.Date(as.yearmon(as.Date(x$PERENDUNX_Q8)) , frac = 1)) -> x$PERENDUNX_Q8
    
    
    # market information 
    PRIZED  <- 
      x[,c("COMPANY_ID_CID10"                                #, "TICKER",
        , "PRICEDM001UNX", "PRICEDM002UNX", "PRICEDM003UNX"  # AS PRICEDM001UNX
        , "PRICEDM004UNX", "PRICEDM005UNX", "PRICEDM006UNX" 
        , "PRICEDM007UNX", "PRICEDM008UNX", "PRICEDM009UNX"
        , "PRICEDM010UNX", "PRICEDM011UNX", "PRICEDM012UNX"
        , "PRICEDM013UNX", "PRICEDM014UNX", "PRICEDM015UNX"
        , "PRICEDM016UNX"                                               
    ),drop=FALSE] 
    
    
    PRIZED[,1]  -> row.names(PRIZED)
    PRIZED[,-1] -> PRIZED
    PRIZED[,rev(1:length(colnames(PRIZED)))] -> PRIZED 
    t(PRIZED) -> PRIZED 
    
    # keep useful ( used below)
    # (except for NAs) as.yearmon ABOVE 
    #  garanties this this is unique ( if one non-NA found then 'value' else NA )
    apply(PRIZED, MARGIN = 1, FUN = max , na.rm = TRUE)  -> PRIZED_INDEX_NAMED_VECTOR
    
    # SM - static  month 
    # DQ - dymanic quarter
    
    # add index
    cbind( 16:1, PRIZED ) -> PRIZED
    c("INDEXSM") -> dimnames(PRIZED)[[2]][1]
    
    as.character(ymd(c("1970-01-01")) + days(PRIZED_INDEX_NAMED_VECTOR)) -> rownames(PRIZED)
    # as.xts(PRIZED) -> PRIZED ## ( BUT WILL NEED LATER!!!!)
    
    # save 'PRIZED' no index
    # INDEXSM
    # append the 'month index' to the row.names
    PRIZED -> PRIZED_NO_INDEXSM
    # (keep hide ) that static, monthly index appended to the 'row.names of YYYY-MM-DD__N' dates
    str_join(row.names(PRIZED_NO_INDEXSM), "__", PRIZED_NO_INDEXSM[,"INDEXSM",drop=FALSE]) -> row.names(PRIZED_NO_INDEXSM)
    # remove INDEXSM 
    PRIZED_NO_INDEXSM[,-1,drop=FALSE] -> PRIZED_NO_INDEXSM

    
    # company information
    PERENDUNX  <- 
      x[,c("COMPANY_ID_CID10"                                # "TICKER"
           , "PERENDUNX_Q1", "PERENDUNX_Q2", "PERENDUNX_Q3"  # PERENDUNX_Q1
           , "PERENDUNX_Q4", "PERENDUNX_Q5", "PERENDUNX_Q6" 
           , "PERENDUNX_Q7", "PERENDUNX_Q8"                                
      ),drop=FALSE] 
    
    PERENDUNX[,1]  -> row.names(PERENDUNX)
    PERENDUNX[,-1] -> PERENDUNX
    PERENDUNX[,rev(1:length(colnames(PERENDUNX)))] -> PERENDUNX 
    t(PERENDUNX) -> PERENDUNX

    # save 'PERENDUNX no index'( for 'use' below )
    PERENDUNX -> PERENDUNX_NO_INDEXDQ
    
    # add index
    cbind( 8:1, PERENDUNX ) -> PERENDUNX 
    c("INDEXDQ") -> dimnames(PERENDUNX)[[2]][1]
    
    # market information
    PRIZE  <- 
      x[,c("COMPANY_ID_CID10"                       # "TICKER" 
        , "PRICE_M001", "PRICE_M002", "PRICE_M003"  # PRICE_M001__numeric
        , "PRICE_M004", "PRICE_M005", "PRICE_M006" 
        , "PRICE_M007", "PRICE_M008", "PRICE_M009"
        , "PRICE_M010", "PRICE_M011", "PRICE_M012"
        , "PRICE_M013", "PRICE_M014", "PRICE_M015"
        , "PRICE_M016"                                               
    ),drop=FALSE] 

    PRIZE[,1]  -> row.names(PRIZE)
    PRIZE[,-1] -> PRIZE
    PRIZE[,rev(1:length(colnames(PRIZE)))] -> PRIZE 
    t(PRIZE) -> PRIZE
    
    # add index
    cbind( 16:1, PRIZE ) -> PRIZE
    c("INDEXSM") -> dimnames(PRIZE)[[2]][1]    

                                             # from far above
    as.character(ymd(c("1970-01-01")) + days(PRIZED_INDEX_NAMED_VECTOR)) -> rownames(PRIZE)
    # as.xts(PRIZE) -> PRIZE ## ( BUT WILL NEED LATER!!!!)
    
    # save 'PRIZE' no index 
    # INDEXSM
    # append the 'month index' to the row.names
    PRIZE -> PRIZE_NO_INDEXSM
    # (keep hide ) that static, monthly index appended to the 'row.names of YYYY-MM-DD__N' dates
    str_join(row.names(PRIZE_NO_INDEXSM), "__", PRIZE_NO_INDEXSM[,"INDEXSM",drop=FALSE]) -> row.names(PRIZE_NO_INDEXSM)
    # remove INDEXSM 
    PRIZE_NO_INDEXSM[,-1,drop=FALSE] -> PRIZE_NO_INDEXSM
    
    # company information
    SHR_AQ  <- 
      x[,c("COMPANY_ID_CID10"                       # "TICKER" 
        , "SHR_AQ1", "SHR_AQ2", "SHR_AQ3"
        , "SHR_AQ4", "SHR_AQ5", "SHR_AQ6" 
        , "SHR_AQ7", "SHR_AQ8"                                
    ),drop=FALSE] 
  
    SHR_AQ[,1]  -> row.names(SHR_AQ)
    SHR_AQ[,-1] -> SHR_AQ
    SHR_AQ[,rev(1:length(colnames(SHR_AQ)))] -> SHR_AQ 
    t(SHR_AQ) -> SHR_AQ
    
    # save 'SHR_AQ no index'( for 'use' below ) 
    SHR_AQ -> SHR_AQ_NO_INDEXDQ
    
    # add index
    cbind( 8:1, SHR_AQ ) -> SHR_AQ
    c("INDEXDQ") -> dimnames(SHR_AQ)[[2]][1]  
    
    
    # company information
    EPSD_Q  <- 
      x[,c("COMPANY_ID_CID10"                       # "TICKER" 
           , "EPSD_Q1", "EPSD_Q2", "EPSD_Q3"
           , "EPSD_Q4", "EPSD_Q5", "EPSD_Q6" 
           , "EPSD_Q7", "EPSD_Q8"                                
      ),drop=FALSE] 
    
    EPSD_Q[,1]  -> row.names(EPSD_Q)
    EPSD_Q[,-1] -> EPSD_Q
    EPSD_Q[,rev(1:length(colnames(EPSD_Q)))] -> EPSD_Q 
    t(EPSD_Q) -> EPSD_Q
    
    # save 'EPSD_Q no index'( for 'use' below ) 
    EPSD_Q -> EPSD_Q_NO_INDEXDQ
    
    # add index
    cbind( 8:1, EPSD_Q ) -> EPSD_Q
    c("INDEXDQ") -> dimnames(EPSD_Q)[[2]][1] 
    
    # print("LEFT_OFF line 1171")
  
    bookmarkhere <- 1
    
    # want to get into the form ...
    # PRIZED ... PRICE
                  
    # PRIZED                                     <ticker>
    # <everymonth-index> | <periodendunx-#.0>   <everymo-price>
    #                      <'         '-locf> 
    # 

    ### BEGIN TEMP_PERENDUNX ###
    
    # first 2x2 dim to 1:many_columns ( help from gdata ) # old [1:4,1:4]
    unmatrix(PERENDUNX_NO_INDEXDQ) -> TEMP_PERENDUNX

    # swap those ":" to be "."
    gsub( ":", ".", names(TEMP_PERENDUNX) ) -> names(TEMP_PERENDUNX)

    # make those vector names  to be the first  column of a data.frame
    # make those vector values to be the second column of a data.frame
    data.frame( variable=names(TEMP_PERENDUNX)
                 , value=TEMP_PERENDUNX
                 , row.names = NULL  # required for XTS S-LOGIC
                 , stringsAsFactors = FALSE) -> TEMP_PERENDUNX 

    # round to the 'end of the month' 
    # help from How to get last day of a month? 
    # http://r.789695.n4.nabble.com/How-to-get-last-day-of-a-month-td890694.html
    # seems this 'company information' is end of month anyways
    # but WILL 'safely' push it to the end of month, just in case some RARE  
    # 'companies' are not THERE ( in the middle of the month somewhere)
    # company information !! NOW AT THE 'END OF MONTH DATE' ... -> same as market information!! #s
    as.numeric(as.Date(as.yearmon(as.Date(TEMP_PERENDUNX$value)) , frac = 1)) -> TEMP_PERENDUNX$value

    # create an index : period 1 throug 8
    as.numeric(
      str_sub(TEMP_PERENDUNX$variable 
              , str_locate(TEMP_PERENDUNX$variable, "[0-9]+")[,"start"]
              , str_locate(TEMP_PERENDUNX$variable, "[0-9]+")[,"end"  ]
      )
    ) -> TEMP_PERENDUNX$index
    
    # isolate the 'variable' in be the CID____ 
    # need numeric to be zoo coredata compatible
    as.numeric(
      str_sub(TEMP_PERENDUNX$variable 
              , str_locate(TEMP_PERENDUNX$variable, "\\.CID[0-9]+")[,"start"] + 4
              , str_locate(TEMP_PERENDUNX$variable, "\\.CID[0-9]+")[,"end"  ]
      ) 
    )-> TEMP_PERENDUNX$variable
    
    # TEMP_PERENDUNX$variable
    # (COMPANY) ( DAY_END_OF_PERIOD) (PERID_INDEX)
    # variable  value           index
    # 1 15613.00000000000000000 8
    # 1	15705.00000000000000000	7
    # 1	15795.00000000000000000	6
    # 1	15886.00000000000000000	5
    # 2	14790.00000000000000000	8
    
    ### END TEMP_PERENDUNX ###
    
    bookmarkhere <- 1   
    
    ### BEGIN TEMP_PRIZED ###
    
    # first 2x2 dim to 1:many_columns ( help from gdata ) # old [1:4,1:4]
    unmatrix(PRIZED_NO_INDEXSM) -> TEMP_PRIZED
    
    # swap those ":" to be "."
    gsub( ":", ".", names(TEMP_PRIZED) ) -> names(TEMP_PRIZED)
    
    # make those vector names to be the first  column of a data.frame
    # make those vector values to be the second column of a data.frame
    data.frame( variable=names(TEMP_PRIZED)
                , value=TEMP_PRIZED
                , row.names = NULL  # required for XTS S-LOGIC
                , stringsAsFactors = FALSE) -> TEMP_PRIZED
    
    # market information ( always at the end of month) # but I garantee it to be at the end of the month
    as.numeric(as.Date(as.yearmon(as.Date(TEMP_PRIZED$value)) , frac = 1)) -> TEMP_PRIZED$value
    
    # create an index : period 1 through 8
    as.numeric(
      str_sub(TEMP_PRIZED$variable 
              , str_locate(TEMP_PRIZED$variable, "__[0-9]+")[,"start"] + 2
              , str_locate(TEMP_PRIZED$variable, "__[0-9]+")[,"end"  ]
      )
    ) -> TEMP_PRIZED$index
    
    # isolate the 'variable' in be the CID____ 
    # need numeric to be zoo coredata compatible
    as.numeric(
      str_sub(TEMP_PRIZED$variable 
              , str_locate(TEMP_PRIZED$variable, "\\.CID[0-9]+")[,"start"] + 4
              , str_locate(TEMP_PRIZED$variable, "\\.CID[0-9]+")[,"end"  ]
      ) 
    )-> TEMP_PRIZED$variable
    
    ### END TEMP_PRIZED ###
    
    bookmarkhere <- 1   
    
    ### END TEMP_PRIZED ###
    
    ### BEGIN TEMP_PRIZE ###
    
    # first 2x2 dim to 1:many_columns ( help from gdata ) # old [1:4,1:4]
    unmatrix(PRIZE_NO_INDEXSM) -> TEMP_PRIZE
    
    # swap those ":" to be "."
    gsub( ":", ".", names(TEMP_PRIZE) ) -> names(TEMP_PRIZE)
    
    # make those vector names to be the first  column of a data.frame
    # make those vector values to be the second column of a data.frame
    data.frame( variable=names(TEMP_PRIZE)
                , value=TEMP_PRIZE
                , row.names = NULL  # required for XTS S-LOGIC
                , stringsAsFactors = FALSE) -> TEMP_PRIZE
    
    # market information ( always at the end of month) # but I garantee it to be at the end of the month
    ## 'not a date' ( actually is a 'value at a date')
    ## as.numeric(as.Date(as.yearmon(as.Date(TEMP_PRIZE$value)) , frac = 1)) -> TEMP_PRIZE$value
    
    # create an index : period 1 through 8
    as.numeric(
      str_sub(TEMP_PRIZE$variable 
              , str_locate(TEMP_PRIZE$variable, "__[0-9]+")[,"start"] + 2
              , str_locate(TEMP_PRIZE$variable, "__[0-9]+")[,"end"  ]
      )
    ) -> TEMP_PRIZE$index
    
    # isolate the 'variable' in be the CID____ 
    # need numeric to be zoo coredata compatible
    as.numeric(
      str_sub(TEMP_PRIZE$variable 
              , str_locate(TEMP_PRIZE$variable, "\\.CID[0-9]+")[,"start"] + 4
              , str_locate(TEMP_PRIZE$variable, "\\.CID[0-9]+")[,"end"  ]
      ) 
    )-> TEMP_PRIZE$variable
    
    ### END TEMP_PRIZE ###

    ### BEGIN TEMP_SHR_AQ ### 
    
    # first 2x2 dim to 1:many_columns ( help from gdata ) # old [1:4,1:4]
    unmatrix(SHR_AQ_NO_INDEXDQ) -> TEMP_SHR_AQ
    
    # swap those ":" to be "."
    gsub( ":", ".", names(TEMP_SHR_AQ) ) -> names(TEMP_SHR_AQ)
    
    # make those vector names  to be the first  column of a data.frame
    # make those vector values to be the second column of a data.frame
    data.frame( variable=names(TEMP_SHR_AQ)
                , value=TEMP_SHR_AQ
                , row.names = NULL  # required for XTS S-LOGIC
                , stringsAsFactors = FALSE) -> TEMP_SHR_AQ 
    
    # round to the 'end of the month' 
    # help from How to get last day of a month? 
    # http://r.789695.n4.nabble.com/How-to-get-last-day-of-a-month-td890694.html
    # seems this 'company information' is end of month anyways
    # but WILL 'safely' push it to the end of month, just in case some RARE  
    # 'companies' are not THERE ( in the middle of the month somewhere)
    # company information !! NOW AT THE 'END OF MONTH DATE' ... -> same as market information!! #s
    ## 'not a date' ( actually is a 'value at a date')
    ## as.numeric(as.Date(as.yearmon(as.Date(TEMP_SHR_AQ$value)) , frac = 1)) -> TEMP_SHR_AQ$value
    
    # create an index : period 1 throug 8
    as.numeric(
      str_sub(TEMP_SHR_AQ$variable 
              , str_locate(TEMP_SHR_AQ$variable, "[0-9]+")[,"start"]
              , str_locate(TEMP_SHR_AQ$variable, "[0-9]+")[,"end"  ]
      )
    ) -> TEMP_SHR_AQ$index
    
    # isolate the 'variable' in be the CID____ 
    # need numeric to be zoo coredata compatible
    as.numeric(
      str_sub(TEMP_SHR_AQ$variable 
              , str_locate(TEMP_SHR_AQ$variable, "\\.CID[0-9]+")[,"start"] + 4
              , str_locate(TEMP_SHR_AQ$variable, "\\.CID[0-9]+")[,"end"  ]
      ) 
    )-> TEMP_SHR_AQ$variable
    
    ### END TEMP_SHR_AQ ###
    
    ### BEGIN TEMP_EPSD_Q ### 
    
    # first 2x2 dim to 1:many_columns ( help from gdata ) # old: [1:4,1:4]
    unmatrix(EPSD_Q_NO_INDEXDQ) -> TEMP_EPSD_Q
    
    # swap those ":" to be "."
    gsub( ":", ".", names(TEMP_EPSD_Q) ) -> names(TEMP_EPSD_Q)
    
    # make those vector names  to be the first  column of a data.frame
    # make those vector values to be the second column of a data.frame
    data.frame( variable=names(TEMP_EPSD_Q)
                , value=TEMP_EPSD_Q
                , row.names = NULL  # required for XTS S-LOGIC
                , stringsAsFactors = FALSE) -> TEMP_EPSD_Q 
    
    # round to the 'end of the month' 
    # help from How to get last day of a month? 
    # http://r.789695.n4.nabble.com/How-to-get-last-day-of-a-month-td890694.html
    # seems this 'company information' is end of month anyways
    # but WILL 'safely' push it to the end of month, just in case some RARE  
    # 'companies' are not THERE ( in the middle of the month somewhere)
    # company information !! NOW AT THE 'END OF MONTH DATE' ... -> same as market information!! #s
    ## 'not a date' ( actually is a 'value at a date')
    ## as.numeric(as.Date(as.yearmon(as.Date(TEMP_EPSD_Q$value)) , frac = 1)) -> TEMP_EPSD_Q$value
    
    # create an index : period 1 throug 8
    as.numeric(
      str_sub(TEMP_EPSD_Q$variable 
              , str_locate(TEMP_EPSD_Q$variable, "[0-9]+")[,"start"]
              , str_locate(TEMP_EPSD_Q$variable, "[0-9]+")[,"end"  ]
      )
    ) -> TEMP_EPSD_Q$index
    
    # isolate the 'variable' in be the CID____ 
    # need numeric to be zoo coredata compatible
    as.numeric(
      str_sub(TEMP_EPSD_Q$variable 
              , str_locate(TEMP_EPSD_Q$variable, "\\.CID[0-9]+")[,"start"] + 4
              , str_locate(TEMP_EPSD_Q$variable, "\\.CID[0-9]+")[,"end"  ]
      ) 
    )-> TEMP_EPSD_Q$variable
    
    ### END TEMP_EPSD_Q ###
    
    # LEFT_OFF ( see temporary NOTES )
    bookmarkhere <- 1
    
    ### SQL ZONE BEGIN ###
    ### SQL ZONE END ###
    
    # xts TICKERS...across(7000 of them) - coredata
    # dates down left: ( xts::to.monthlthy) 
    #                  (  as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(. . . UNX))- index

    # tools
    # t()
    # xts.to.monthly(PRICEDM001)     -> row.names -> xts( , index(.))
    # xts.to.monthly(PEREND_Q(1...8)
    # xts::merge() X.rbind
    # zoo::na.locf, apply, zoo::rollapply, Recall
  
    # some logic like
    # if ( PEREND_Q1  <=  PRICE_DATE  )                   EPSD_Q1 * SHR_AQ1 * ( 1.0 /  DILUTION_MULT_Q1 ) / PRICE_DATE -> M000_RET_PER_DOLLAR
    # if ( PEREND_Q2  <=  PRICEDM001  <=  PEREND_Q1  )    EPSD_Q2 * SHR_AQ2 * ( 1.0 /  DILUTION_MULT_Q2 ) / PRICE_M001 -> M001_RET_PER_DOLLAR
  
    return(x)

  }

  RET_DOLLAR_PRICE_GRID <- suppressWarnings(do(RET_DOLLAR_PRICE_UNIVERSE_NOT_NA, ret_dollar_price_grid_do(.))) 

  RET_DOLLAR_PRICE_UNIVERSE_NOT_NA <- ungroup(RET_DOLLAR_PRICE_UNIVERSE_NOT_NA) 
  rm(RET_DOLLAR_PRICE_UNIVERSE_NOT_NA)
    
  RET_DOLLAR_PRICE_GRID <- ungroup(RET_DOLLAR_PRICE_GRID) 
  
  # OUTPUT IS RETURNED TO    RET_DOLLAR_PRICE_GRID
             
  bookmarkhere <- 1
                    
  ################  VISUALLY SEE THE RETURN_PER_DOLLAR ########################
  #############################################################################
 
 
  

  # update companies with missing sector MA information
  
  # begin SQL
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # strip off
  UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
   #    c("TICKER")
   # ,  
        c("COMPANY_ID")
   # ,  c("ORIG_ORDER")
      
    )
  )
  
  UNIVERSE <- tbl_df(UNIVERSE)
  
  sqldf("
    CREATE TABLE LOOKUP AS 
    SELECT DISTINCT 
          SMA.MG_DESC 
        , SMA.PRICE_WGHT_MEAN_SECTOR 
        , SMA.PRICE_WGHT_MEAN_SMA_10_M_SECTOR 
        , SMA.PRICE_WGHT_MEAN_SMA_10_M_SECTOR_SVVR 
        FROM main.UNIVERSE SMA WHERE SMA.PRICE_WGHT_MEAN_SECTOR IS NOT NULL 
  ", connection = dpsqllconn$con
  )

  # doing this '2 part way' becauuse 
  #   SQLite' correlated subquery update is not working ( or I can not figure it out )
  #   ( but I do not care . . . 'good enough' )
  # TO_DO FIX [ ] only 'where-in' the cells that I need to UPDATE
  # update NA cells ( actually all cells ) using other cells that do not contain NA ( from the lookup table )
  sqldf("
      UPDATE main.UNIVERSE 
          SET PRICE_WGHT_MEAN_SECTOR               = ( SELECT PRICE_WGHT_MEAN_SECTOR               FROM main.LOOKUP WHERE MG_DESC = main.UNIVERSE.MG_DESC), 
              PRICE_WGHT_MEAN_SMA_10_M_SECTOR      = ( SELECT PRICE_WGHT_MEAN_SMA_10_M_SECTOR      FROM main.LOOKUP WHERE MG_DESC = main.UNIVERSE.MG_DESC), 
              PRICE_WGHT_MEAN_SMA_10_M_SECTOR_SVVR = ( SELECT PRICE_WGHT_MEAN_SMA_10_M_SECTOR_SVVR FROM main.LOOKUP WHERE MG_DESC = main.UNIVERSE.MG_DESC)        
    ", connection = dpsqllconn$con
  )

  sqldf("
    DROP TABLE main.LOOKUP 
  ", connection = dpsqllconn$con 
  )
  
  
  UNIVERSE <- sqldf("SELECT UNIV.* FROM main.UNIVERSE UNIV 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <- tbl_df(UNIVERSE)
  
  
  # end SQL
  
  # end of compute the SMA price per sector ( weighted by sector element 'last known market cap' )
  

  
  
  
  # begin growth expose 
  # The 'growth part of 'marrying growth and value' 
  # uses 'All stocks' universal median 
  
  
  # require 3-month price appreciation greater than the universal 'All stocks' median (UNIVERSE)
  # from SI_PSD  add back PRCHG_13W
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # strip off
  UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
   #    c("TICKER")
   # ,  
        c("COMPANY_ID")
   # ,  c("ORIG_ORDER")
      
    )
  )
  
  UNIVERSE <- tbl_df(UNIVERSE)
  
  UNIVERSE <- sqldf("SELECT UNIV.*, PSD.PRCHG_13W AS PRCHG_13W__numeric FROM 
                                   main.UNIVERSE UNIV, main.SI_PSD PSD WHERE 
                                   UNIV.COMPANY_ID = PSD.COMPANY_ID 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <- tbl_df(UNIVERSE)
  
  
  # all math must be numeric
  UNIVERSE <- mutate(UNIVERSE, PRCHG_13W = as.numeric(as.no_worse_than_NA(
    PRCHG_13W
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(PRCHG_13W) == FALSE)
  
  # ntile 'higher value' is 'higher ntile'
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,PRCHG_13W_NTILE2 = as.numeric(
  #   ntile(PRCHG_13W,2)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"PRCHG_13W", buckets = 2 ) 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
  # surviVor of '3-month price appreciation greater than the universal 'All stocks' median (UNIVERSE)'
  UNIVERSE <- mutate(UNIVERSE, GROWTH_EXPOSE_PRCHG_13W_NTILE2_SRVVR = ifelse(PRCHG_13W_NTILE2 == 2, 1, 0)  )
  
  # require 6-month price appreciation greater than the universal median (ALLSTOCKS)
  # from SI_PSD  add back PRCHG_26W
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # strip off
  UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
        c("TICKER")
     ,  c("COMPANY_ID")
     ,  c("ORIG_ORDER")
      
    )
  )
  
  UNIVERSE <- sqldf("SELECT UNIV.*, PSD.PRCHG_26W AS PRCHG_26W__numeric FROM 
                                   main.UNIVERSE UNIV, main.SI_PSD PSD WHERE 
                                   UNIV.COMPANY_ID = PSD.COMPANY_ID 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <- tbl_df(UNIVERSE)
  
  
  # all math must be numeric
  UNIVERSE <- mutate(UNIVERSE, PRCHG_26W = as.numeric(as.no_worse_than_NA(
    PRCHG_26W
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(PRCHG_26W) == FALSE)
  
  # ntile 'higher value' is 'higher ntile'
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,PRCHG_26W_NTILE2 = as.numeric(
  #   ntile(PRCHG_26W,2)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"PRCHG_26W", buckets = 2 ) 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
  # surviVor of '6-month price appreciation greater than the universal 'All stocks' median (UNIVERSE)'
  UNIVERSE <- mutate(UNIVERSE, GROWTH_EXPOSE_PRCHG_26W_NTILE2_SRVVR = ifelse(PRCHG_26W_NTILE2 == 2, 1, 0)  )
  
 

   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_BSQ"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_BSQ"))
    # } else {
      # # load file
      # SI_BSQ <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_BSQ"), as.is = TRUE)))
      # save("SI_BSQ",file = getOption("AAIISIPro40PathFileOptim_SI_BSQ"))
    # }
  # } else {
    # # load file
    # SI_BSQ <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_BSQ"), as.is = TRUE)))
  # }
  
  SI_BSQ <- get_from_disk("SI_BSQ", filesoptionlist = OPTIONLIST)
  
  # primary_key_dup <- SI_BSQ[duplicated(SI_BSQ[,'COMPANY_ID']),,drop=FALSE]
  # new_df_no_duplicates <- SI_BSQ[!(SI_BSQ$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
  # SI_BSQ <- new_df_no_duplicates
  # rm(primary_key_dup,new_df_no_duplicates)
  
  SI_BSQ <- SI_BSQ # wierd performance bug ( program runs faster than can it access its variables )
  SI_BSQ <- eliminate_all_duplicates( SI_BSQ, "COMPANY_ID" ) 
  
  SI_BSQ_tbl_sqlite <- copy_to(dpsqllconn, SI_BSQ, temporary = FALSE
                               , indexes = list(
                                 c("COMPANY_ID")
                               )
  )
  SI_BSQ <- tbl_df(SI_BSQ)
  

   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_CFQ"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_CFQ"))
    # } else {
      # # load file
      # SI_CFQ <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_CFQ"), as.is = TRUE)))
      # save("SI_CFQ",file = getOption("AAIISIPro40PathFileOptim_SI_CFQ"))
    # }
  # } else {
    # # load file
    # SI_CFQ <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_CFQ"), as.is = TRUE)))
  # }
  
  SI_CFQ <- get_from_disk("SI_CFQ", filesoptionlist = OPTIONLIST)
  
  # primary_key_dup <- SI_CFQ[duplicated(SI_CFQ[,'COMPANY_ID']),,drop=FALSE]
  # new_df_no_duplicates <- SI_CFQ[!(SI_CFQ$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
  # SI_CFQ <- new_df_no_duplicates
  # rm(primary_key_dup,new_df_no_duplicates)
  
  SI_CFQ <- SI_CFQ # wierd performance bug ( program runs faster than can it access its variables )
  SI_CFQ <- eliminate_all_duplicates( SI_CFQ, "COMPANY_ID" ) 
  
  SI_CFQ_tbl_sqlite <- copy_to(dpsqllconn, SI_CFQ, temporary = FALSE
                               , indexes = list(
                                 c("COMPANY_ID") 
                               )
  )
  SI_CFQ <- tbl_df(SI_CFQ)
  
  
  # begin joins
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)
  
  # strip off
  UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
                                 , indexes = list(
                                   #    c("TICKER")
                                   # ,  
                                   c("COMPANY_ID")
                                   # ,  c("ORIG_ORDER")
                                   
                                 )
  )
  
  
  UNIVERSE <- sqldf("SELECT UNIV.* 
                      , BSQ.LIAB_Q1 AS LIAB_Q1__numeric, BSQ.LIAB_Q2 AS LIAB_Q2__numeric                                  -- %change in debt ( and debt / equity ratio )
                      , BSQ.LIAB_Q3 AS LIAB_Q3__numeric, BSQ.LIAB_Q4 AS LIAB_Q4__numeric, BSQ.LIAB_Q5 AS LIAB_Q5__numeric -- %change in debt
                      , BSQ.EQUITY_Q1 AS EQUITY_Q1__numeric                                                               -- debt / equity ratio   -- book / price ratio
                      , BSQ.GWI_Q1 AS GWI_Q1__numeric                                                                                              -- book / price ratio
                      , CFQ.TCF_Q1 AS TCF_Q1__numeric, CFQ.TCF_Q2 AS TCF_Q2__numeric, CFQ.TCF_Q3 AS TCF_Q3__numeric       -- cash flow from financing' / average assets
                      , CFQ.TCF_Q4 AS TCF_Q4__numeric, CFQ.TCF_Q5 AS TCF_Q5__numeric
                      , CFQ.TCF_Q6 AS TCF_Q6__numeric, CFQ.TCF_Q7 AS TCF_Q7__numeric, CFQ.TCF_Q8 AS TCF_Q8__numeric       -- earnings composite: percent change in NOA
                      , BSQ.ASSETS_Q1 AS ASSETS_Q1__numeric, BSQ.ASSETS_Q2 AS ASSETS_Q2__numeric                          -- cash flow from financing' / average assets
                      , BSQ.ASSETS_Q3 AS ASSETS_Q3__numeric, BSQ.ASSETS_Q4 AS ASSETS_Q4__numeric 
                      , BSQ.ASSETS_Q5 AS ASSETS_Q5__numeric                                                               -- ( MAY MAY be used in CATA )
                      , CFQ.NCC_Q1 AS NCC_Q1__numeric, CFQ.NCC_Q2 AS NCC_Q2__numeric, CFQ.NCC_Q3 AS NCC_Q3__numeric       -- annual cash flow / debt 
                      , CFQ.NCC_Q4 AS NCC_Q4__numeric, CFQ.NCC_Q5 AS NCC_Q5__numeric
                                  FROM 
                                   main.UNIVERSE UNIV, main.SI_BSQ BSQ, main.SI_CFQ CFQ WHERE 
                                   UNIV.COMPANY_ID = BSQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = CFQ.COMPANY_ID 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <- tbl_df(UNIVERSE)
  
  
  # end joins
  
  
  # begin joins
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)
  
  # strip off
  UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
                                 , indexes = list(
                                   #    c("TICKER")
                                   # ,  
                                   c("COMPANY_ID")
                                   # ,  c("ORIG_ORDER")
                                   
                                 )
  )
  
  
  UNIVERSE <- sqldf("SELECT UNIV.* 
                      , CFQ.TCO_Q1 AS TCO_Q1__numeric, CFQ.TCO_Q2 AS TCO_Q2__numeric                                -- percent change in net operationg assets ( NOA ) AND current accruals to total assets ( CATA ) 
                      , CFQ.TCO_Q3 AS TCO_Q3__numeric, CFQ.TCO_Q4 AS TCO_Q4__numeric, CFQ.TCO_Q5 AS TCO_Q5__numeric -- percent change in net operationg assets ( NOA ) AND current accruals to total assets ( CATA ) 
                      , CFQ.TCO_Q6 AS TCO_Q6__numeric, CFQ.TCO_Q7 AS TCO_Q7__numeric, CFQ.TCO_Q8 AS TCO_Q8__numeric -- percent change in net operationg assets ( NOA ) AND current accruals to total assets ( CATA )
                      , BSQ.WORK_Q1 AS WORK_Q1__numeric, BSQ.WORK_Q2 AS WORK_Q2__numeric                                  -- total accruals over total assets ( TATA ) 
                      , BSQ.WORK_Q3 AS WORK_Q3__numeric, BSQ.WORK_Q4 AS WORK_Q4__numeric, BSQ.WORK_Q5 AS WORK_Q5__numeric -- total accruals over total assets ( TATA )  
                      , BSQ.CASH_Q1 AS CASH_Q1__numeric, BSQ.CASH_Q2 AS CASH_Q2__numeric                                  -- total accruals over total assets ( TATA ) 
                      , BSQ.CASH_Q3 AS CASH_Q3__numeric, BSQ.CASH_Q4 AS CASH_Q4__numeric, BSQ.CASH_Q5 AS CASH_Q5__numeric -- total accruals over total assets ( TATA ) 
                      , BSQ.AP_Q1 AS AP_Q1__numeric, BSQ.AP_Q2 AS AP_Q2__numeric                              -- current accruals to total assets ( CATA )
                      , BSQ.AP_Q3 AS AP_Q3__numeric, BSQ.AP_Q4 AS AP_Q4__numeric, BSQ.AP_Q5 AS AP_Q5__numeric -- current accruals to total assets ( CATA )
                      , BSQ.AP_Q6 AS AP_Q6__numeric, BSQ.AP_Q7 AS AP_Q7__numeric, BSQ.AP_Q8 AS AP_Q8__numeric -- current accruals to total assets ( CATA )
                      , BSQ.AR_Q1 AS AR_Q1__numeric, BSQ.AR_Q2 AS AR_Q2__numeric                              -- OLD: current accruals to total assets ( CATA )
                      , BSQ.AR_Q3 AS AR_Q3__numeric, BSQ.AR_Q4 AS AR_Q4__numeric, BSQ.AR_Q5 AS AR_Q5__numeric -- OLD: current accruals to total assets ( CATA )
                      , ISQ.EPSCON_Q1 AS EPSCON_Q1__numeric, ISQ.EPSCON_Q2 AS EPSCON_Q2__numeric                                      -- current accruals to total assets ( CATA ) 
                      , ISQ.EPSCON_Q3 AS EPSCON_Q3__numeric, ISQ.EPSCON_Q4 AS EPSCON_Q4__numeric, ISQ.EPSCON_Q5 AS EPSCON_Q5__numeric -- current accruals to total assets ( CATA ) 
                      , ISQ.EPSCON_Q6 AS EPSCON_Q6__numeric, ISQ.EPSCON_Q7 AS EPSCON_Q7__numeric, ISQ.EPSCON_Q8 AS EPSCON_Q8__numeric -- current accruals to total assets ( CATA ) 
                      , ISQ.EPSDC_Q1 AS EPSDC_Q1__numeric, ISQ.EPSDC_Q2 AS EPSDC_Q2__numeric                                    -- EPS-Diluted Cont - Used in DILUTION_MULT_ 
                      , ISQ.EPSDC_Q3 AS EPSDC_Q3__numeric, ISQ.EPSDC_Q4 AS EPSDC_Q4__numeric, ISQ.EPSDC_Q5 AS EPSDC_Q5__numeric -- EPS-Diluted Cont - Used in DILUTION_MULT_
                      , ISQ.EPSDC_Q6 AS EPSDC_Q6__numeric, ISQ.EPSDC_Q7 AS EPSDC_Q7__numeric, ISQ.EPSDC_Q8 AS EPSDC_Q8__numeric -- EPS-Diluted Cont - Used in DILUTION_MULT_
                      , CFQ.TCI_Q1 AS TCI_Q1__numeric, CFQ.TCI_Q2 AS TCI_Q2__numeric                                -- percent change in net operationg assets ( NOA ) AND current accruals to total assets ( CATA ) 
                      , CFQ.TCI_Q3 AS TCI_Q3__numeric, CFQ.TCI_Q4 AS TCI_Q4__numeric, CFQ.TCI_Q5 AS TCI_Q5__numeric -- percent change in net operationg assets ( NOA ) AND current accruals to total assets ( CATA ) 
                      , CFQ.TCI_Q6 AS TCI_Q6__numeric, CFQ.TCI_Q7 AS TCI_Q7__numeric, CFQ.TCI_Q8 AS TCI_Q8__numeric -- percent change in net operationg assets ( NOA ) 
                      , ISQ.DEP_Q1 AS DEP_Q1__numeric, ISQ.DEP_Q2 AS DEP_Q2__numeric                                      -- depreciation  expense to captital expenditures 
                      , ISQ.DEP_Q3 AS DEP_Q3__numeric, ISQ.DEP_Q4 AS DEP_Q4__numeric, ISQ.DEP_Q5 AS DEP_Q5__numeric       -- depreciation  expense to captital expenditures 
                      , CFQ.DEP_CF_Q1 AS DEP_CF_Q1__numeric, CFQ.DEP_CF_Q2 AS DEP_CF_Q2__numeric                                            -- SIPro 4.0: EBITDA = EBIT + Depreciation and Amortization 
                      , CFQ.DEP_CF_Q3 AS DEP_CF_Q3__numeric, CFQ.DEP_CF_Q4 AS DEP_CF_Q4__numeric, CFQ.DEP_CF_Q5 AS DEP_CF_Q5__numeric       -- SIPro 4.0: EBITDA = EBIT + Depreciation and Amortization 
                      , CFQ.CE_Q1 AS CE_Q1__numeric, CFQ.CE_Q2 AS CE_Q2__numeric                              -- depreciation expense to captital expenditures 
                      , CFQ.CE_Q3 AS CE_Q3__numeric, CFQ.CE_Q4 AS CE_Q4__numeric, CFQ.CE_Q5 AS CE_Q5__numeric -- depreciation expense to captital expenditures 
                      , ISQ.NETINC_Q1 AS NETINC_Q1__numeric, ISQ.NETINC_Q2 AS NETINC_Q2__numeric                                            -- Difference between Operating Cash Flow and Net Income and scales the figure to Market Cap 
                      , ISQ.NETINC_Q3 AS NETINC_Q3__numeric, ISQ.NETINC_Q4 AS NETINC_Q4__numeric, ISQ.NETINC_Q5 AS NETINC_Q5__numeric       -- Difference between Operating Cash Flow and Net Income and scales the figure to Market Cap 
                      , ISQ.NETINC_Q6 AS NETINC_Q6__numeric, ISQ.NETINC_Q7 AS NETINC_Q7__numeric, ISQ.NETINC_Q8 AS NETINC_Q8__numeric       -- Difference between Operating Cash Flow and Net Income and scales the figure to Market Cap 
                                  FROM 
                                   main.UNIVERSE UNIV, main.SI_BSQ BSQ, main.SI_CFQ CFQ, main.SI_ISQ ISQ WHERE 
                                   UNIV.COMPANY_ID = BSQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = CFQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = ISQ.COMPANY_ID 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <- tbl_df(UNIVERSE)

  # end joins


  ##############
  
  # 'diluation multiple' = commonEPSOPS/share // dilutedEPSOPS/share ... >= 1.0
  
  # possibly used with LIAB_Q1 ( or ( common ) Shares Average Q1 SHR_AQ1 ) for some usefulness
  # TO BE INVESTIGATED [ ] : READ THE BROWN BOOK ( BUT DEFINITELY DO )!!!
  
  # Consider ( PE ratio FIX ) # MAY? MAKE A GOOD 'custom field in 'SI Pro' 'IN or OUT' = SHR_DQ1

  # EPS-Diluted Continuing  Q1
  # Data Table Name: , Q2, Q3, Q4, Q5, Q6, Q7, Q8
  # fully diluted earnings from continuing operations 
  # A company will only report diluted earnings if it has potentially dilutive securities 
  #  therfore, need ... ifelse(!is.na(EPSDC_Q1), EPSDC_Q1, EPSCON_Q1)
  # ISQ

  # = EARNOPS/AVE_DILUTED_SHARES

  # EPS-Continuing 
  # Data Table Name: EPSCON_Q1
  # dividing earnings from continuing operations by the average number of shares outstanding during the same period
  # ISQ

  # = EARNOPS/AVE_COMMON_SHARES

  # ( 1.0 / ( EARNOPS/AVE_DILUTED_SHARES ) ) * EARNOPS/AVE_COMMON_SHARES = AVE_DILUTED_SHARES / AVE_COMMON_SHARES

  # Shares Average Q1
  # Data Table Name: SHR_AQ1
  # average number of shares of common stock outstanding 
  #   This is the number of shares issued minus the shares held in treasury (Redeemable shares?-Co buyable back from issuer)?.
  # PSD

  # = AVE_COMMON_SHARES

  # Therefore

  # AVE_DILUTED_SHARES / AVE_COMMON_SHARES * AVE_COMMON_SHARES = AVE_DILUTED_SHARES  = Andre Custom
  
  # DD_FILE	   SI_ISQ
  # FIELD_NAME EPSDC_Q1
  # FIELD_TYPE C
  # FIELD_DESC EPS-Diluted Continuing Q1
  # DESCRIP    Income Statement - Quarterly
  # FM_FILE    SI_ISQ
  
# SEEMS SIGNIFICANT ( some as high as 20%)  DILUTION_MULT_ 
# ( STRANGE NO CHANGE FROM QUARTER TO QUARTER )
  
  UNIVERSE <- mutate(UNIVERSE,                                   
      DILUTION_MULT_Q1 =ifelse( !is.na(EPSCON_Q1) == TRUE & !is.na(EPSDC_Q1) == TRUE  & EPSCON_Q1 != 0.0 & EPSDC_Q1 != 0.0, as.numeric(EPSCON_Q1/EPSDC_Q1), 1.0)
    , DILUTION_MULT_Q2 =ifelse( !is.na(EPSCON_Q2) == TRUE & !is.na(EPSDC_Q2) == TRUE  & EPSCON_Q2 != 0.0 & EPSDC_Q2 != 0.0, as.numeric(EPSCON_Q2/EPSDC_Q2), 1.0)                                                                         
    , DILUTION_MULT_Q3 =ifelse( !is.na(EPSCON_Q3) == TRUE & !is.na(EPSDC_Q3) == TRUE  & EPSCON_Q3 != 0.0 & EPSDC_Q3 != 0.0, as.numeric(EPSCON_Q3/EPSDC_Q3), 1.0)                                                              
    , DILUTION_MULT_Q4 =ifelse( !is.na(EPSCON_Q4) == TRUE & !is.na(EPSDC_Q4) == TRUE  & EPSCON_Q4 != 0.0 & EPSDC_Q4 != 0.0, as.numeric(EPSCON_Q4/EPSDC_Q4), 1.0)                                          
    , DILUTION_MULT_Q5 =ifelse( !is.na(EPSCON_Q5) == TRUE & !is.na(EPSDC_Q5) == TRUE  & EPSCON_Q5 != 0.0 & EPSDC_Q5 != 0.0, as.numeric(EPSCON_Q5/EPSDC_Q5), 1.0)
    , DILUTION_MULT_Q6 =ifelse( !is.na(EPSCON_Q6) == TRUE & !is.na(EPSDC_Q6) == TRUE  & EPSCON_Q6 != 0.0 & EPSDC_Q6 != 0.0, as.numeric(EPSCON_Q6/EPSDC_Q6), 1.0)
    , DILUTION_MULT_Q7 =ifelse( !is.na(EPSCON_Q7) == TRUE & !is.na(EPSDC_Q7) == TRUE  & EPSCON_Q7 != 0.0 & EPSDC_Q7 != 0.0, as.numeric(EPSCON_Q7/EPSDC_Q7), 1.0)
    , DILUTION_MULT_Q8 =ifelse( !is.na(EPSCON_Q8) == TRUE & !is.na(EPSDC_Q8) == TRUE  & EPSCON_Q8 != 0.0 & EPSDC_Q8 != 0.0, as.numeric(EPSCON_Q8/EPSDC_Q8), 1.0)
  )

  # SO I DO NOT ACCIDENTALLY 'BUY A STOCK' AT THE WRONG TIME

  # Dividend-Ex Date
  # Data Table Name: DIVNQXDT

  # Dividend-Pmt Date
  # Data Table Name: DIVNQPDT
  
   UNIVERSE <- mutate(UNIVERSE,                                                                                                               
      DIVNQXDT = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(DIVNQXDTUNX))                                                        
    , DIVNQPDT = as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(DIVNQPDTUNX))
  )
  
  ################
  
  
  # have an 'annual' 'EPS change' greater than zero (0)'
  # LAST of growth expose

  # 'not need 'relative' , need 'absolute 'higher is better )
  # relative   -6 - ( -3 ) /    ( -3 )   = -3 / -3 =  1
  # absolute   -6 - ( -3 ) / abs( -3 )   = -3 /  3 = -1 ( WANT THIS )
  # ( ( EPS_Q1 + EPS_Q2 + EPS_Q3 + EPS_Q4 ) - ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 + 0.0000001 ) ) / abs( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 + 0.0000001 ) * 100.0 = GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH
  
  # FIELD_NAME  EPS_Q1
  # FIELD_DESC  EPS Q1
  # DESCRIP     Income Statement - Quarterly
 	# FM_FILE     SI_ISQ
  
  # ALTERNATIVE ( if a company has small 'earnings' 
  #   then a tiny insignificant increase in 'earnings' can produce a 
  #   'bad logic' 'big swing' in ( NUM - DEN ) / abs(DEN + 0.0000001) * 100
  # THEREFORE, I may want to weight something ( NOTE: end result approx: ( ( EPS_Q1 + EPS_Q2 + EPS_Q3 + EPS_Q4 ) - ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 + 0.0000001 ) ) / ( ASSETS_Q5 + 0.0000001 ) * 100.0 = GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH
  # ( ( EPS_Q1 + EPS_Q2 + EPS_Q3 + EPS_Q4 ) - ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 + 0.0000001 ) ) / abs( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 + 0.0000001 )  * ( ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 + 0.0000001 ) / ( ASSETS_Q5 + 0.0000001 ) ) * 100.0 = GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH
  
  # NOTE: THE ALTERNATIVE DOES NOT AFFECT ...
  # have an 'annual' 'EPS change' greater than zero (0)'
  # BUT IT MAKES ME 'feel better'
  
  # DOES NOT BREAK
  #  ( ( EPS_Q1 + EPS_Q2 + EPS_Q3 + EPS_Q4 ) - ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 + 0.0000001 ) ) / ( ASSETS_Q5 + 0.0000001 ) * 100.0
  
  UNIVERSE <- mutate(UNIVERSE, GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH = as.numeric(as.no_worse_than_NA( 
    ( ( EPS_Q1 + EPS_Q2 + EPS_Q3 + EPS_Q4 ) - ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8  ) ) / ( ASSETS_Q5  ) * 100.0
  ) ) )
  
  UNIVERSE <- mutate(UNIVERSE, GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH_GR_THAN_0_SRVVR = ifelse(   
    GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH > 0.0, 1, 0
  ) )
  
  # growth expose winners so far
  
  UNIVERSE <- mutate(UNIVERSE, GROWTH_EXPOSE_SRVVR = 
    ifelse(   
              GROWTH_EXPOSE_PRCHG_13W_NTILE2_SRVVR                      == 1 & 
              GROWTH_EXPOSE_PRCHG_26W_NTILE2_SRVVR                      == 1 & 
              GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH_GR_THAN_0_SRVVR    == 1 
    , 1, 0
    )
  )
  
  # end growth expose
  
  # begin value expose
  
  # want only GROWTH_EXPOSE_SRVVR == 1
  
  # UNIVERSE <- data.table(UNIVERSE)
  # setkeyv(UNIVERSE,c("GROWTH_EXPOSE_SRVVR"))
  # UNIVERSE <- UNIVERSE[GROWTH_EXPOSE_SRVVR==1]
  # UNIVERSE <- as.data.frame(UNIVERSE, stringsAsFactors = FALSE)
  
  UNIVERSE <- filter(UNIVERSE, GROWTH_EXPOSE_SRVVR == 1)
  
  # begin financial composite ( defensive posture )
  
  
  # ( 1 of 4 )
  # begin - financial composite - % change in debt
  # # %change in debt ( balance sheet item ) from a year ago (Q5) to the last quarter (Q1)" 
  # debt(denominator) is always (zero or positive) ( but CAN BE small: + 0.0000001 ( ten cents ) )
  
  # lower value is BETTER 
  
  # DD_FILE	SI_BSQ
  # 2	FIELD_NUM	140
  # 3	FIELD_NAME	LIAB_Q1
  # 4	FIELD_TYPE	C
  # 5	FIELD_DESC	Total liabilities Q1
  # 6	DESCRIP	Balance Sheet - Quarterly
  # 7	FM_FILE	SI_BSQ
  # 8	DIRECTORY	..\Data Files\Static\
  
  # 'absolute '%change in debt ( balance sheet item ) from a year ago (Q5) to the last quarter (Q1)'
  # ( LIAB_Q1 - LIAB_Q5 ) / abs(LIAB_Q5 + 0.0000001) * 100 = VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT
  
  # NOTE: ( LIAB_ and ASSETS_ each are always positive AND  ( ASSETS_ - LIAB_ = 'equity'(some cases negative )
  #  BUT: ( LIAB_ can be 'small'!! )
  
  # ALTERNATIVE ( if a company has small 'liab' 
  #   then a tiny insignificant increase in 'liab' can produce a 
  #   'bad logic' 'big swing' in ( LIAB_Q1 - LIAB_Q5 ) / abs(LIAB_Q5 + 0.0000001) * 100
  # THEREFORE, I may want to weight something ( NOTE: end result approx:  ( LIAB_Q1 - LIAB_Q5 ) / ( ASSETS_Q5 + 0.0000001 ) * 100.0 = VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT
  # ( LIAB_Q1 - LIAB_Q5 ) / abs(LIAB_Q5 + 0.0000001) * ( LIAB_Q5 / ASSETS_Q5 + 0.0000001 )  * 100.0 = VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT
  
  # Piotroski Price-to-Book Screen
  # Financial Leverage
  # Piotroski defined debt to total assets as 
  # total long-term debt plus the current portion of long-term debt divided by average total assets. 
  
  ## group_by MG_DESC  ( sector )
  # lower value is BETTER ' reverse of ntile'
  
  # DOES NOT BREAK
  # ( LIAB_Q1 - LIAB_Q5 ) / ( ASSETS_Q5 + 0.0000001 ) * 100.0
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT = as.numeric(as.no_worse_than_NA(   
    ( LIAB_Q1 - LIAB_Q5 ) / ( ASSETS_Q5 ) * 100.0
  ) ) )

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100 = as.numeric(
  #   ntile((-1)*VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT", posneg = -1 ) 
  ) 

  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
  # end - financial composite - % change in debt
  
  # ( 2 of 4 )
  # begin - financial composite - debt to equity ratio
  # debt to equity ratio
  # equity(denominator) is always positive ( and big )
  # lower value is BETTER 
  # debt / equity
  
  # FIELD_NAME LIAB_Q1
  # FIELD_DESC Total liabilities Q1
  # DESCRIP    Balance Sheet - Quarterly
  # FM_FILE    SI_BSQ ( already have )
  
  # FIELD_NAME EQUITY_Q1
  # FIELD_DESC Equity (common) Q1
  # DESCRIP    Balance Sheet - Quarterly
  # FM_FILE    SI_BSQ ( already have )
  
  # crazy data received - eliminate an Inf
  # LIAB_Q1 / (EQUITY_Q1 + 0.0000001) = VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY 
  
  # lower value is BETTER
  
  # DOES NOT BREAK
  # LIAB_Q1 / (EQUITY_Q1 + 0.0000001)
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY = as.numeric(as.no_worse_than_NA(   
    LIAB_Q1 / (EQUITY_Q1 + 0.0000001)
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY) == FALSE)
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100 = as.numeric(
  #   ntile((-1)*VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY", posneg = -1 ) 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # end - financial composite - debt to equity ratio
  
  
  # ( 3 of 4 )
  # begin - financial composite - external financing
  # total assets(denominator) is always positive ( and big )
  # lower value is BETTER 
  
  # DEF 1  
  # external financing: 'cash flow from financing' / average assets ( Shaughnessy 2012 book )
  # DEF 2
  # external financing: 'cash flow from financing' / total assets   ( March 2014 AAII article )
  # AUTHOR MAY HAVE DONE A 'SHORT CUT (WRONG) CHEAT'
  # % Rank-Total liab/assets Q1        RTL_TA_Q1
  
  # THIS IS A FIX ( will use DEF 1( Shaughnessy 2012 book ): seems to make more 'sense' )
  
  # NOTE: AAII missing MUCH data here: maybe an adjust2; ( TCF_Q2 + TCF_Q3 + TCF_Q4 + TCF_Q5) 

  # NOTE: Data problem: $ TCF_Q1 : num  -1e+08  as.integer(-1e+08)   -100000000
  # read.dbf CORRECTLY loads as 'NA' s
  # DATA MISSING 
  
  # FIELD_NAME  TCF_Q1
  # FIELD_DESC  Cash from financing Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ ( new ) 
  
  # FIELD_NAME ASSETS_Q1
  # FIELD_DESC Total assets Q1
  # DESCRIP    Balance Sheet - Quarterly
  # FM_FILE    SI_BSQ ( already have )
  
  # will NOT use ( but do SQL load TCF_Q1 and ASSETS_Q1 )
  # ( TCF_Q2 + TCF_Q3 + TCF_Q4 + TCF_Q5 ) / ( ( ASSETS_Q2 + ASSETS_Q3 + ASSETS_Q4 + ASSETS_Q5 + 0.0000001) / 4.0 ) = VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING

  # will use ( but do SQL load TCF_Q1 and ASSETS_Q1 ) BUT WILL USE 'AAII method of March 2014'
  # ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4) / ( ( ASSETS_Q1 + ASSETS_Q2 + ASSETS_Q3 + ASSETS_Q4 + 0.0000001) / 4.0 ) = VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING
  
  # lower value is BETTER 
  
  # DOES NOT BREAK 
  # ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4) / ( ( ASSETS_Q1 + ASSETS_Q2 + ASSETS_Q3 + ASSETS_Q4 + 0.0000001) / 4.0 )
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING = as.numeric(as.no_worse_than_NA(   
    ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4) / ( ( ASSETS_Q1 + ASSETS_Q2 + ASSETS_Q3 + ASSETS_Q4 + 0.0000001) / 4.0 )
  ) ) )
  

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100 = as.numeric(
  #   ntile((-1)*VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING", posneg = -1 ) 
  ) 

  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # edit(sqldf("SELECT TICKER, MG_DESC, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100 
                # FROM UNIVERSE 
              # ORDER BY MG_DESC, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING DESC, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100
  # "))
  # look at Transportation
  
  # TO_DO ### LEFT_OFF ###
  # 1. DONE(EVERYWHERE) REPLACE 'ALL NTILE' CALCULATIONS WITH ONE THAT 'DOES NOT INCLUDE NAS' IN THE CALULATION ( see just above )
  # 2. DONE.  ELIMINATE 'FINANCE' FROM THAT ONE MEASURE ... ( same method as above left_join ( LEFT OUTER ) )
  # 3. DONE.  COMPOSITE REBALANCING  ... at least 2/3 ... ntile left_join
  # 4. DONE. ENTIRE COMPOSITE NTILE?!
  # NEXT
  # BIG 5. EARNINGS COMPOSITE
  
  # end - financial composite - external financing
  
  # ( 4 of 4 ) ( 'AAII March 2014 - 'only non-financial companies use THIS')
  # begin - financial composite - annual cash flow / debt 
  # annual cash flow / debt 
  # debt(denominator) is always (zero or positive) ( but CAN BE small: + 0.0000001 ( ten cents ) )
  # higher value is BETTER 
  
  # http://www.financeformulas.net/Debt-Coverage-Ratio.html  debt payments for the same period 
  
  # some -1.00e+08 ( ONLY A PROBLEM OF Q1 )
  # NUMERATOR SHOULD BE: NCC_Q1 + NCC_Q2 + NCC_Q3 + NCC_Q4 
  # BUT I AM USING:               NCC_Q2 + NCC_Q3 + NCC_Q4 + NCC_Q5
  
  # FIELD_NAME NCC_Q1
  # FIELD_DESC Cash flow Q1
  # DESCRIP    Cash Flow - Quarterly
  # FM_FILE    SI_CFQ ( from above )
  
  # will NOT use ( but do SQL load NCC_Q1 and LIAB_Q1 )
  # ( NCC_Q2 + NCC_Q3 + NCC_Q4 + NCC_Q5) / ( LIAB_Q2 + LIAB_Q3 + LIAB_Q4 + LIAB_Q5 + 0.0000001 ) = VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT
  
  # will use ( but do SQL load NCC_Q1 and LIAB_Q1 )  BUT WILL USE 'AAII method of March 2014'
  # ( NCC_Q1 + NCC_Q2 + NCC_Q3 + NCC_Q4) / ( LIAB_Q1 + LIAB_Q2 + LIAB_Q3 + LIAB_Q4 + 0.0000001 ) = VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT

  # higher value is BETTER 
  
  # DOES NOT BREAK
  # ( NCC_Q1 + NCC_Q2 + NCC_Q3 + NCC_Q4) / ( LIAB_Q1 + LIAB_Q2 + LIAB_Q3 + LIAB_Q4 + 0.0000001 ) 
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT = as.numeric(as.no_worse_than_NA(   
    ( NCC_Q1 + NCC_Q2 + NCC_Q3 + NCC_Q4) / ( LIAB_Q1 + LIAB_Q2 + LIAB_Q3 + LIAB_Q4  ) 
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT) == FALSE)
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT") 
  ) 

  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  ### IMPORTANT ###
  # 'only non-financial companies use THIS' ( just _NTILE 'NA out" the "Financial" companies )
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100 = ifelse(MG_DESC == "Financial", NA, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100)  )
  
  # end - financial composite - annual cash flow / debt 
  
  # begin - financial composite - rebalance and scoring
  # (USE) March 2014:  at least 2 of 4 factors (financial composite case), then rebalance  ( technique seems fairer )
  # 2012 book: if NA, then assign 50 (of ntile 100 ) ( seems to unfairly punish companies for slow/non-exist reporting?)
  
  # count up of non-NA ntiles
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_UNIQUE_SCORES_CNT = as.numeric(
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100)            == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100)         == TRUE, 1.0, 0.0) + 
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100)     == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100) == TRUE, 1.0, 0.0)  
  ))
  
  # minimum two factors are required
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_SCORES_SUMM = as.numeric(
    ifelse( VAL_EXPOSE_FIN_CMPST_UNIQUE_SCORES_CNT >= 2.0, 
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100)            == TRUE, VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100,            0.0) +
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100)         == TRUE, VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100,         0.0) + 
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100)     == TRUE, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100,     0.0) +
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100) == TRUE, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100, 0.0) 
    , NA) 
  ))
  
  # four factors total possible
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL = as.numeric(
     VAL_EXPOSE_FIN_CMPST_SCORES_SUMM * 4.0 / VAL_EXPOSE_FIN_CMPST_UNIQUE_SCORES_CNT
  ))
  
  # higher value is BETTER 
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL) == FALSE)
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL") 
  ) 

  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # end - financial composite - rebalance and scoring
  
  # end financial composite
  
  
  
  # begin earnings composite ( defensive posture )

  # OShaughnessy Earnings Composite
  # --------------------------------
  # AAII JOURNAL > October 2013
  # Read Comments (43)
  # STOCK STRATEGIES
  # ?What Works?: Key New Findings on Stock Selection
  # by James O'Shaughnessy
    # http://www.aaii.com/files/journal/OSAMdisclosures.pdf **** ( LEFT_OFF: FINE TUNE YETAGAIN ?? ) ****
      # Definitions of Earnings Quality Factors
        # Total Accruals to Total Asset
        # Current Accruals to Total
  # http://www.aaii.com/journal/article/what-works-key-new-findings-on-stock-selection

  # OShaughnessy Value Blend ( Seen: August 2014 )
    # O?Shaughnessy Value Blend 
    # ( RENAMED? FROM O?Shaughnessy Asset Management?s (OSAM) value composite ) *
    # http://www.osam.com/value_blend.aspx
  # http://www.osam.com/strategies.aspx

  
  # see END of 'earnings composite'
  # ( If I were to use a 'single measure for the earnings composite ...')
  # March 2014 custom: Difference between "Operating Cash Flow" and "Net Income" 
  #                                   and scaled to "MarketCap"
  
  
  
  
  # ( 1 of 4 )
  # begin - percent change in net operating assets ( NOA )
  #  BOOK 278: PERCENTAGE CHANGE IN NET OPERATING ASSETS   ( TREND )
  # higher value is better
  
  # separate operating activities from financiing activities
  # isolate the gains from operating performance of the company from gains from finanical performance
  
  # can not do '% change in net operating assets' ( not the right items in SI_PRO )

  # best: I can do is ratio of ( 'Cash from operations' - 'Cash from financing' ) / 'market cap'

  # Cash from operations TCO_Q1

  # Net Income + Depreciation + Amortization 
  # +(-) Increase (Decrease) in Accounts Payable 
  # +(-) Decrease (Increase) in Accounts Receivable 
  # +(-) Increase (Decrease) in Deferred Taxes 
  # +(-) Decrease (Increase) in Inventories 
  # +(-) Decrease (Increase) in Pre-Paid Expenses.

  # FIELD_NAME  TCO_Q1 ( new column )
  # FIELD_DESC  Cash from operations Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ

  # Cash from financing TCF_Q1

  # Cash flows from financing for each of the last eight fiscal quarters. 
  # Inflows from additional borrowing, repayment of debt, dividend payments, 
  # and equity financing are all components of financing cash flow.

  # FIELD_NAME  TCF_Q1 ( old column )
  # FIELD_DESC  Cash from financing Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ

  # A positive number for cash flow from financing activities means 
  # more money is flowing into the company than flowing out, which increases the company?s assets(BAD WAY). 

  # Negative numbers can mean the company is servicing debt(GOOD WAY)

  # http://www.investopedia.com/terms/c/cashflowfromfinancing.asp

  # ( desperate people sell off thier assets )
  
  # FIELD_NAME  TCI_Q1 ( new column )
  # FIELD_DESC  Cash from investing Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ
  
  # if a company sells off old equipment or sells a division of its operations to another firm, 
  # these activities are also captured on paper as income from investing. 
  # http://www.investopedia.com/articles/financial-theory/11/cash-flow-from-investing.asp
  # http://www.readyratios.com/reference/accounting/cash_flows_from_investing_activities.html

  
  # higher value is better ( TREND )

  # all these cashes TC* are assets

  # ( ( ( TCO_Q1 + TCO_Q2 + TCO_Q3 + TCO_Q4 ) - ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4 )  - ( TCI_Q1 + TCI_Q2 + TCI_Q3 + TCI_Q4  ) ) - ( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  ) ) ) / abs( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  ) + 0.0000001 ) * 100  = VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA

  # higher value is better

  # about 75% will make an NTILE ( the culprit: some data is missing from the first quarter )
  
  # DOES NOT BREAK
  #  ( ( ( TCO_Q1 + TCO_Q2 + TCO_Q3 + TCO_Q4 ) - ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4 )  - ( TCI_Q1 + TCI_Q2 + TCI_Q3 + TCI_Q4  ) ) - ( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  ) ) ) / abs( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  ) + 0.0000001 ) * 100 
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA = as.numeric(as.no_worse_than_NA(   
    ( ( ( TCO_Q1 + TCO_Q2 + TCO_Q3 + TCO_Q4 ) - ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4 )  - ( TCI_Q1 + TCI_Q2 + TCI_Q3 + TCI_Q4  ) ) - ( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  ) ) ) / abs( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  )  ) * 100 
  ) ) )

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA") 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
  # end - percent change in net operationg assets ( NOA )

  
  
  # ( 2 of 4 )
  # begin - total accruals over total assets ( TATA ) 
  # book p. 284 - change in total acruals to total assets
  
  # lower value is better (  uses % change in working capital ( BELOW ) )
  
  # Note: March 2014: Authtor mentioned using the replacement numerator "Change in Working Capital"
  # I will use "Change in Working Capital"  ( "Working Capital" is a balance sheet item )
  # I will will use 
  # change_in_working_capital / total_assets

  # stock investor pro 4.0 help
  # Working Capital Q1
  # TREND: A declining working capital ratio over a longer time period could be a red flag. 
  # if a company is not operating in the most efficient manner (slow collection), 
  # it will show up as an ***increase in the working capital***
  # slow collection may signal an underlying problem in the company's operations.

  # Liquidity
  # To judge liquidity, a company earns one point if 
  # its current ratio at the end of its most recent fiscal year increased compared to the prior fiscal year. 
  # Too high a ratio may point to 
  #   unnecessary investment in current assets or 
  #   failure to collect receivables or a bloated inventory, 
  # all negatively affecting earnings.
  
  # FIELD_NAME  WORK_Q1 ( new column )
  # FIELD_DESC  Working Capital Q1
  # DESCRIP     Balance Sheet - Quarterly
  # FM_FILE     SI_BSQ
  
  # FIELD_NAME  CASH_Q1
  # FIELD_DESC  Cash Q1
  # DESCRIP     Balance Sheet - Quarterly
  # FM_FILE     SI_BSQ
  
  # Working Capital: difference between a company's current assets and its current liabilities
  
  # JUNK : March 2014: TATA: Change in working capital(balance sheet:(Q1 - Q5)/abs(Q5)*100.0
  #                                Change in current assets - Change in current liabilities - Change in cash
  #                                (All in the SI help file: letter "C" ) ( "All in the Balance Sheet Quarterly")
  #                                ( ( CA_Q1 - CL_Q1 - CASH_Q1 ) - ( CA_Q5 - CL_Q5 - CASH_Q5 ) ) / abs( CA_Q5 - CL_Q5 - CASH_Q5 ) * 100.0 

  # I will still call it TATA
  # But use
  # And 'since a 'change'

  # TATA 
  # (  ( ( WORK_Q1 - CASH_Q1 ) / ( ASSETS_Q1 + 0.0000001 ) )  - ( ( WORK_Q5 - CASH_Q5 ) / ( ASSETS_Q5 + 0.0000001 ) )  ) / abs( ( ( WORK_Q5 - CASH_Q5 + 0.0000001 ) /  ( ASSETS_Q5 + 0.0000001 ) )  ) * 100.0  = VAL_EXPOSE_EARN_CMPST_TATA

  # ALTERNATE 
  # ( as 'working capital WORK_ goes up' LAIB_ seem to be fixed'? )
  # I think that the  original way meant to use in the denominator: ASSETS_Q5 
  # ( but I think that the way THAT would be written WORK_(up) / ASSETS_(up) : not as usefule as LIAB_(fixed) )
  
  # (  ( ( WORK_Q1 - CASH_Q1 ) / ( LIAB_Q1 + 0.0000001 ) )  - ( ( WORK_Q5 - CASH_Q5 ) / ( LIAB_Q5 + 0.0000001 ) )  ) / abs( ( ( WORK_Q5 - CASH_Q5 + 0.0000001 ) /  ( LIAB_Q5 + 0.0000001 ) )  ) * 100.0  = VAL_EXPOSE_EARN_CMPST_TATA

  # lower value is better  (  uses % change in working capital ( BELOW ) )

  # current assets(accounts receivable) increases then 'assets' increase ( 'liabilites remains fixed' )
  # cash                                increases then 'assets' increase                                   
  # then just 'assets' in the denominator
  
  # DOES NOT BREAK
  # (  ( ( WORK_Q1 - CASH_Q1 ) / ( ASSETS_Q1 + 0.0000001 ) )  - ( ( WORK_Q5 - CASH_Q5 ) / ( ASSETS_Q5 + 0.0000001 ) )  ) / abs( ( ( LIAB_Q5 - CASH_Q5 + 0.0000001 ) /  ( ASSETS_Q5 + 0.0000001 ) )  ) * 100.0 
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_TATA = as.numeric(as.no_worse_than_NA(   
    (  ( ( WORK_Q1 - CASH_Q1 ) / ( ASSETS_Q1  ) )  - ( ( WORK_Q5 - CASH_Q5 ) / ( ASSETS_Q5  ) )  ) / abs( ( ( LIAB_Q5 - CASH_Q5  ) /  ( ASSETS_Q5  ) )  ) * 100.0 
  ) ) )

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_TATA) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_TATA_NTILE100 = as.numeric(
  #    ntile((-1)*VAL_EXPOSE_EARN_CMPST_TATA,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_EARN_CMPST_TATA", posneg = -1) 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # end - total accruals over total assets ( TATA ) 


  # ( 3 of 4 )
  # begin - current accruals to total assets ( CATA ) # ( replacement of TAAA )
  # since TATA is a TREND, then CATA/TAAA is also a trend ?!
  # TATA: book p. 284 - change in total acruals to total assets
  
  # lower value is better

  # Oct 2013, March 2014  -- Current Acruals / Assets  replaces 2010 "Total Accruals to Average Assets"


  # March 2014 custom: "Difference in Accruals to Earnings over the last 12 months 
  #                                   minus cash earnings over the last 12 months

  # lower is better CASE
  # (  ( acc_rec_accr_1 - acc_rec_accr_5 ) - 12mo_net_income - 12mo_cash_flow ) / mktcap_q1

  # FIELD_NAME  AP_Q1 ( new column )
  # FIELD_DESC  Accounts payable Q1
  # DESCRIP     Balance Sheet - Quarterly
  # FM_FILE     SI_BSQ
  
  # accounts payable entry is found on a balance sheet under the heading current liabilities.
  # http://www.investopedia.com/terms/a/accountspayable.asp
  
  ## OLD
  ## FIELD_NAME	AR_Q1 ( new column )    
  ## FIELD_DESC	Accounts receivable Q1
  ## DESCRIP	Balance Sheet - Quarterly
  ## FM_FILE	SI_BSQ
  
  # On a public company's balance sheet, accounts receivable is often recorded as an asset 
  # because this represents a legal obligation for the customer to remit 
  # http://www.investopedia.com/terms/a/accountsreceivable.asp
  
  # FIELD_NAME  EPSCON_Q1
  # FIELD_DESC  EPS-Continuing Q1
  # DESCRIP     Income Statement - Quarterly
  # FM_FILE     SI_ISQ
  
  # FIELD_NAME  TCO_Q1 ( 'will be old column' )
  # FIELD_DESC  Cash from operations Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ
  
  # ( desperate people sell off thier assets )
  
  # FIELD_NAME  TCI_Q1 ( new column )
  # FIELD_DESC  Cash from investing Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ
  
  # if a company sells off old equipment or sells a division of its operations to another firm, 
  # these activities are also captured on paper as income from investing. 
  # http://www.investopedia.com/articles/financial-theory/11/cash-flow-from-investing.asp
  # http://www.readyratios.com/reference/accounting/cash_flows_from_investing_activities.html
  
  # call it CATA
  # lower is better
  # Difference in Accruals to Earnings over the last 12 months ( 'Accruals raises liabilities')
  #   minus cash earnings over the last 12 months 
  # CATA
  ##  OLD: (  ( AP_Q1 - AP_Q5 ) - ( AR_Q1 - AR_Q5 ) - ( TCO_Q1 + TCO_Q2 + TCO_Q3 + TCO_Q4 ) - ( TCI_Q1 + TCI_Q2 + TCI_Q3 + TCI_Q4 )  ) / ( LIAB_Q1 + 0.0000001 ) = VAL_EXPOSE_EARN_CMPST_CATA

  # ALTERNATIVE
  # SIPro 4.0 help
  # Piotroski Price-to-Book Screen
  # Return on Assets
  # Piotroski defined return on assets (ROA) as 
  # net income before extraordinary items for the fiscal year preceding the analysis 
  #   divided by 
  # total assets at the beginning of the fiscal year. 
  # Stock Investor deviates by using net income after extraordinary items in its calculation
  # Piotroski did not look for high levels, only a positive figure. ( MY MATH IS OPPOSITE )
  # ALTERNATIVE : ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4 ) / ( ASSETS_Q5 + 0.0000001 ) = VAL_EXPOSE_EARN_CMPST_CATA
  
  # POSSIBLE IF A 'trend' 
  # ( ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4 ) - ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 ) ) / abs( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 + 0.0000001 ) * 100  = VAL_EXPOSE_EARN_CMPST_CATA
  
  # POSSILBLE IF A 'ratio'
  # ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4 ) / ( ASSETS_Q5 + 0.0000001 )
  
   # Therefore, 'trend' and 'ratio'
   # OSAMdisclosure.pdf "size and direction"
   # POSSIBLE IF A 'trend' and a 'ratio'
   # ( ( ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4 ) / ( ASSETS_Q1 + 0.0000001 ) )   - ( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 ) / ( ASSETS_Q5 + 0.0000001 ) ) ) /  abs( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 ) / ( ASSETS_Q5 + 0.0000001 ) )
 
  
  # HARD NOTE: ( DENOMINATOR + 0.0000001  ) : Removes R 'cut' function error: Error in View : 'breaks' are not unique ( using hdntile and ALTERNATIVE )
  
  # lower is better

  # DOES NOT BREAK FORM ( COMMON )
  #  ( ( ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4 + 0.0000001 ) / ( ASSETS_Q1 + 0.0000001 ) )   - ( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 + 0.0000001 ) / ( ASSETS_Q5 + 0.0000001 ) ) ) /  abs( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 + 0.0000001 ) / ( ASSETS_Q5 + 0.0000001 ) )
 
  # DOES     BREAK FORM
  #  ( ( ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4             ) / ( ASSETS_Q1             ) )   - ( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8             ) / ( ASSETS_Q5              ) ) ) /  abs( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8            ) / ( ASSETS_Q5             ) )
 
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_CATA = as.numeric(as.no_worse_than_NA(
      # USED  
    ( ( ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4             ) / ( ASSETS_Q1             ) )   - ( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8             ) / ( ASSETS_Q5              ) ) ) /  abs( ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8            ) / ( ASSETS_Q5             ) )
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_CATA) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_CATA_NTILE100 = as.numeric(
    # Error in View : 'breaks' are not unique ( using ALTERNATIVE hdntile AND DENOMINATOR WITHOUT!! 0.0000001 )
    # ntile((-1)*VAL_EXPOSE_EARN_CMPST_CATA,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_EARN_CMPST_CATA", posneg = -1) 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
  # end - current accruals to total assets ( CATA )

  
  # ( 4 of 4 )
  # begin - depreciation expense to captital expenditures
  # BOOK p. 273 depreciation to capital expenditures ratio ( RATIO )
  # higher value is better ( read explanation below )

  # Book p 273-74 "stocks with the lowest deprecation expense to capital expense do poorly"
  # if management is aggressive it might over-estimate the usefulness of equipement
  # and therefore write down asset values more slowly, thus negatively affecting future earnings
  # NOTE ...
  # 'depreciation write down' problem was seen in the Browne book

  # seems to be a 'social behaviour measure'

  # FIELD_NAME  DEP_Q1 ( new column )
  # FIELD_DESC  Depreciation Q1
  # DESCRIP     Income Statement - Quarterly
  # FM_FILE     SI_ISQ

  # FIELD_NAME  DEP_CF_Q1
  # FIELD_DESC  Depreciation and Amortization - Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ
  
  # ISQ.DEP_Q# ( USED ) - BUT SINCE IT IS A SOCIAL MEASURE, AND EVERYONE HAS CHAIRS - is.na(DEP_Q#) ...-> 0.0
  # Depreciation
  # Data Category: Income Statement - Quarterly
  # Many companies do not break out depreciation and amortization on their income statement, 
  # choosing to lump it in with operating expenses. 
  # Instead, they report these line items it on their cash flow statement.

  # CFQ.DEP_CF_Q# ( NOT USED )
  # Depreciation and Amortization 
  # Data Category: Cash Flow Statement - Quarterly
  # Many companies opt to only report depreciation and amortization expense items on their cash flow statement 
  # (and not on the income statement).
  # [ cash flow statement ] typically reflecting a more comprehensive reporting of these non-cash items.
  
  # FIELD_NAME  CE_Q1 ( new column )
  # FIELD_DESC	Capital expenditures Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ

  # View(UNIVERSE[UNIVERSE$MG_DESC == "Financial",c(A:A,B:B)])  
  # still heavily biased against financial companies ( 50%+ STILL do not report )
  # Pray: min required components per composite SAVES "Financial" companies
  # PERHAPS FAR FUTUTURE: DEP_(perhaps: ifelse(!is.na(DEP_Q#) == TRUE, DEP_Q#, 0.0 ) 
  
  # INCOME STATEMENT
  # still SOME problem with 0.0 # later row_number get biased high values/ntiles  # early row_number get biased low/ntiles
  #                               but LESS problems than (tried) and (tried2) ( especially in "Finance" )
  # ( DEP_Q1 + DEP_Q2 + DEP_Q3 + DEP_Q4 ) / ( CE_Q1 + CE_Q2 + CE_Q3 + CE_Q4 + 0.0000001 ) = VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND
  
  # (could not use): too_many_ties of 'sum of DEP_#' == 0.0 produces large and volitile NTILE results 
  # later row_number get biased high values/ntiles  # early row_number get biased low/ntiles
  # (tried) : ( ifelse(!is.na(DEP_Q1) == TRUE, DEP_Q1, 0.0 ) + ifelse(!is.na(DEP_Q2) == TRUE, DEP_Q2, 0.0 ) + ifelse(!is.na(DEP_Q3) == TRUE, DEP_Q3, 0.0 ) + ifelse(!is.na(DEP_Q4) == TRUE, DEP_Q4, 0.0 ) ) / ( CE_Q1 + CE_Q2 + CE_Q3 + CE_Q4 + 0.0000001 )  = VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND

  # 0.0000001 / 0.0000001 == 1.0  # later row_number get biased high values/ntiles  # early row_number get biased low/ntiles
  # (tried2): ( ifelse(!is.na(DEP_Q1) == TRUE, DEP_Q1, 0.0 ) + ifelse(!is.na(DEP_Q2) == TRUE, DEP_Q2, 0.0 ) + ifelse(!is.na(DEP_Q3) == TRUE, DEP_Q3, 0.0 ) + ifelse(!is.na(DEP_Q4) == TRUE, DEP_Q4, 0.0 ) + 0.0000001 ) / ( CE_Q1 + CE_Q2 + CE_Q3 + CE_Q4 + 0.0000001 )  = VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND

  # View(UNIVERSE[,c("DEP_Q1","DEP_Q2","DEP_Q3","DEP_Q4","CE_Q1","CE_Q2","CE_Q3","CE_Q4","VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND","VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100")])
  # View(UNIVERSE[UNIVERSE$MG_DESC == "Financial",c("DEP_Q1","DEP_Q2","DEP_Q3","DEP_Q4","CE_Q1","CE_Q2","CE_Q3","CE_Q4","VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND","VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100")])
  
  # higher value is better ( NOTE: Brown Book: This seems to be a 'social measure' )

  # Does NOT BREAK
  # ( DEP_Q1 + DEP_Q2 + DEP_Q3 + DEP_Q4 ) / ( CE_Q1 + CE_Q2 + CE_Q3 + CE_Q4 + 0.0000001 ) 
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND = as.numeric(as.no_worse_than_NA(   
    ( DEP_Q1 + DEP_Q2 + DEP_Q3 + DEP_Q4 ) / ( CE_Q1 + CE_Q2 + CE_Q3 + CE_Q4  ) 
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND") 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # end - depreciation expense to captital expenditures
  
  # ( special_EARN of special_EARN )
  # begin - diff operating cash flow and net income and scales that figure to market cap
  # higher value is better 
  
  # March 2014: SIPro 4.0: Earnings Composite: One Single Easiest Way
  # Difference between Operating Cash Flow and Net Income and scales the figure to Market Cap

  # (similar to) SIPro 4.0 - Piotroski Price-to-Book Screen
  # Operating Cash Flow
  #   Piotroski awarded one point if a firm had positive operating cash flow. 
  # Accrual Accounting Check - A point is awarded if 
  #   cash from operations exceeded net income before extraordinary items.
  
  # FIELD_NAME  TCO_Q1 
  # FIELD_DESC  Cash from operations Q1
  # DESCRIP     Cash Flow - Quarterly
  # FM_FILE     SI_CFQ

  # FIELD_NAME  NETINC_Q1
  # FIELD_DESC  Net income Q1
  # DESCRIP     Income Statement - Quarterly
  # FM_FILE     SI_ISQ

  # FIELD_NAME  MKTCAP
  # FIELD_DESC  Market Cap Q1 ( not actually 'Market Cap Q1' ( MKTCAP_Q1 is Market Cap Hist. Q1 ) )
  # DESCRIP     Price and Share Statistics
  # FM_FILE     SI_PSD
  
  # 87% overall data found
  # View(UNIVERSE[,c("VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP","VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP_NTILE100")])
  # 68% "Financial" data found
  # View(UNIVERSE[UNIVERSE$MG_DESC == "Financial",c("VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP","VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP_NTILE100")])

  # higher value is better 
  
  # ( ( TCO_Q1 - NETINC_Q1 ) + ( TCO_Q2 - NETINC_Q2 ) + ( TCO_Q3 - NETINC_Q3 ) + ( TCO_Q4 - NETINC_Q4 ) ) / MKTCAP = VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP

  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP = as.numeric(as.no_worse_than_NA(   
    ( ( TCO_Q1 - NETINC_Q1 ) + ( TCO_Q2 - NETINC_Q2 ) + ( TCO_Q3 - NETINC_Q3 ) + ( TCO_Q4 - NETINC_Q4 ) ) / MKTCAP
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP") 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # end - diff operating cash flow and net income and scales that figure to market cap
  
  
  
  # begin - earnings composite - rebalance and scoring
  # (USE) March 2014:  at least 2 of 4 factors (HOPEFULLY earnings composite case), then rebalance  ( technique seems fairer )
  # 2012 book: if NA, then assign 50 (of ntile 100 ) ( seems to unfairly punish companies for slow/non-exist reporting?)
  
  # count up of non-NA ntiles
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_UNIQUE_SCORES_CNT = as.numeric(
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100)             == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_TATA_NTILE100)                      == TRUE, 1.0, 0.0) + 
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_CATA_NTILE100)                      == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100) == TRUE, 1.0, 0.0)  
  ))
  
  # minimum two factors are required
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_SCORES_SUMM = as.numeric(
    ifelse( VAL_EXPOSE_EARN_CMPST_UNIQUE_SCORES_CNT >= 2.0, 
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100)             == TRUE, VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100,             0.0) +
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_TATA_NTILE100)                      == TRUE, VAL_EXPOSE_EARN_CMPST_TATA_NTILE100,                      0.0) + 
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_CATA_NTILE100)                      == TRUE, VAL_EXPOSE_EARN_CMPST_CATA_NTILE100,                      0.0) +
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100) == TRUE, VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100, 0.0) 
    , NA) 
  ))
  
  # four factors total possible
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL = as.numeric(
     VAL_EXPOSE_EARN_CMPST_SCORES_SUMM * 4.0 / VAL_EXPOSE_EARN_CMPST_UNIQUE_SCORES_CNT
  ))
  
  # higher value is BETTER 
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL) == FALSE)
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL") 
  ) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  

  # begin - IF DOING # ( special_EARN of special_EARN )
  #         THEN remove this area 'entire' comment out
  # March 2014: SIPro 4.0: Earnings Composite: One Single Easiest Way
  # Difference between Operating Cash Flow and Net Income and scales the figure to Market Cap
  
  # overwrite!! # note: above 'is.na' ... has already been done
  
  # UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
  #   VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP_NTILE100
  # ))
  
  # end - IF DOING # ( special_EARN of special_EARN )
  
  
  # end - earnings composite - rebalance and scoring

  
  # end earnings composite
  
  # begin value_two composite ( offensive posture ) 


  # Multiples
  
   # if(getOption("FileStoreStyle") == "Optimized") {
    # if( file.exists(getOption("AAIISIPro40PathFileOptim_SI_MLT"))) {
      # load(file = getOption("AAIISIPro40PathFileOptim_SI_MLT"))
    # } else {
      # # load file
      # SI_MLT <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_MLT"), as.is = TRUE)))
      # save("SI_MLT",file = getOption("AAIISIPro40PathFileOptim_SI_MLT"))
    # }
  # } else {
    # # load file
    # SI_MLT <- suppressWarnings(suppressMessages(read.dbf(file=getOption("AAIISIPro40PathFileNotOptim_SI_MLT"), as.is = TRUE)))
  # }

  SI_MLT <- get_from_disk("SI_MLT", filesoptionlist = OPTIONLIST)
  
    # primary_key_dup <- SI_MLT[duplicated(SI_MLT[,'COMPANY_ID']),,drop=FALSE]
    # new_df_no_duplicates <- SI_MLT[!(SI_MLT$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    # SI_MLT <- new_df_no_duplicates
    # rm(primary_key_dup,new_df_no_duplicates)
  
  SI_MLT <- SI_MLT # wierd performance bug ( program runs faster than can it access its variables )
  SI_MLT <- eliminate_all_duplicates( SI_MLT, "COMPANY_ID" ) 
  
  SI_MLT_tbl_sqlite <- copy_to(dpsqllconn, SI_MLT, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_MLT <- tbl_df(SI_MLT)


  # begin joins
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)
  
  # strip off
  UNIVERSE <- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
                                 , indexes = list(
                                   #    c("TICKER")
                                   # ,  
                                   c("COMPANY_ID")
                                   # ,  c("ORIG_ORDER")
                                   
                                 )
  )



  UNIVERSE <- sqldf("SELECT UNIV.* 
                      , ISQ.EPSDC_Q1 AS EPSDC_Q1__numeric, ISQ.EPSDC_Q2 AS EPSDC_Q2__numeric                                    -- earnings / price ratio
                      , ISQ.EPSDC_Q3 AS EPSDC_Q3__numeric, ISQ.EPSDC_Q4 AS EPSDC_Q4__numeric, ISQ.EPSDC_Q5 AS EPSDC_Q5__numeric -- earnings / price ratio
                      , ISQ.SALES_Q1 AS SALES_Q1__numeric, ISQ.SALES_Q2 AS SALES_Q2__numeric                                    -- sales / price ratio
                      , ISQ.SALES_Q3 AS SALES_Q3__numeric, ISQ.SALES_Q4 AS SALES_Q4__numeric, ISQ.SALES_Q5 AS SALES_Q5__numeric -- sales / price ratio
                      , CFQ.FCFPS_Q1 AS FCFPS_Q1__numeric, CFQ.FCFPS_Q2 AS FCFPS_Q2__numeric                                    -- free cash flow / price ratio
                      , CFQ.FCFPS_Q3 AS FCFPS_Q3__numeric, CFQ.FCFPS_Q4 AS FCFPS_Q4__numeric, CFQ.FCFPS_Q5 AS FCFPS_Q5__numeric -- free cash flow / price ratio
                      , ISQ.EBITDA_Q1 AS EBITDA_Q1__numeric, ISQ.EBITDA_Q2 AS EBITDA_Q2__numeric                                      -- EBITDA / enterprise value ratio ( NOT USED - too much missing data ) 
                      , ISQ.EBITDA_Q3 AS EBITDA_Q3__numeric, ISQ.EBITDA_Q4 AS EBITDA_Q4__numeric, ISQ.EBITDA_Q5 AS EBITDA_Q5__numeric -- EBITDA / enterprise value ratio ( NOT USED - too much missing data ) 
                      , ISQ.EBIT_Q1 AS EBIT_Q1__numeric, ISQ.EBIT_Q2 AS EBIT_Q2__numeric                                              -- EBITDA / enterprise value ratio -- SIPro 4.0: EBITDA = EBIT + Depreciation and Amortization
                      , ISQ.EBIT_Q3 AS EBIT_Q3__numeric, ISQ.EBIT_Q4 AS EBIT_Q4__numeric, ISQ.EBIT_Q5 AS EBIT_Q5__numeric             -- EBITDA / enterprise value ratio -- SIPro 4.0: EBITDA = EBIT + Depreciation and Amortization
                      , BSQ.ENTVAL_Q1 AS ENTVAL_Q1__numeric                                                                           -- EBITDA / enterprise value ratio 
                      , MLT.SHY AS SHY__numeric                                                                                       -- shareholder yield
                                  FROM 
                                   main.UNIVERSE UNIV, main.SI_BSQ BSQ, main.SI_CFQ CFQ, main.SI_ISQ ISQ, main.SI_MLT MLT WHERE 
                                   UNIV.COMPANY_ID = BSQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = CFQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = ISQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = MLT.COMPANY_ID 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <- tbl_df(UNIVERSE)

  # end joins
  
  # ( FIGURE ALL 'WHAT COLUMNS TO ADD FOR THE 'ENTIRE COMPOSITE FIRST)
  # ( NEXT ADD COLUMNS USING 'SQL')
  # ( FIGURE OUT THE COMPONENT NTILES )
  # ( FIGURE OUT THE COMPOSITE NTILE )

  # Oct 2013 ( known as "Asset Management Value Composite" )

  # Oct 2013 & March 2014 "Book_Value" is removed 
  # ( and in OShaun2010 was not thrilled about it # Therefore, Book_Value is removed here)  

  # 2010 used                     "Price to Cash Flow"
  # Oct 2013: He replaces it with "Free Cash Flow/Enterprise Value"

  # 2010 used                       "Price to Cash Flow"
  # March 2014: He replaces it with "Price to Free Cash Flow" ( WILL USE THIS ONE )
      
  # below: see "multiples" note: keep 'sector groupings'
      
  # ( 1 of 5 ) ( RATIO )
  # begin - earnings / price ratio ( price is always large and positive )
  
  # higher is better 

  # MANUAL CALCULATION 
  
  # SI Pro 4.0
  # Data Table Name: PE
  # Data Category: Multiples
  # Field Type: Decimal (0.1 to 999.9) ( AAII will make NA if 'real world negative' (I do not want this) )
  # dividing the current stock price by diluted earnings per share from continuing operations 
                                      # for the last four quarters (trailing 12 months). 
  
  #	FIELD_NAME  EPSDC_Q1
  #	FIELD_DESC  EPS-Diluted Continuing Q1
  #	DESCRIP     Income Statement - Quarterly
  #	FM_FILE     SI_ISQ
  
  # PSD.PRICE ( added to the "elimintate un-investibles query") [DONE]
  
  # PSD.PRICE  
  # SI Pro 4.0
  # Data Table Name: PRICE  
  # Data Category: Price and Share Statistics
  # Field Type: Dollars per share (0.01 to 99999.99)
  # For NYSE, AMEX, and NASDAQ-traded companies, the Price is the closing price for the date of the program update release. 

  ### CORRECTED OCT 24, 2014 ###
  # EPS-Diluted Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8
  # Data Table Name: EPSD_Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8
  # Data Category: Income Statement - Quarterly
  # Field Type: Dollars per share
  # The amount of fully diluted earnings from total operations reported by a company for ( see WALMART sheet )
  # each of the last eight fiscal quarters.
  # Stock Investor Pro 4.0 Help

  # FIELD_NAME  EPSD_Q1
  # FIELD_TYPE  C
  # FIELD_DESC  EPS-Diluted Q1
  # DESCRIP Income Statement - Quarterly
  # FM_FILE SI_ISQ
  
  # higher is better ( already Diluted! )
  
  # NOTED NOV 8, 2014
  # 1
  # EPS-Diluted Q1
  # EPSD_Q1
  # Field Type: Dollars per share

  # earn/share//price/share = earn/price O.K
  
  # ( EPSDC_Q1 + EPSDC_Q2 + EPSDC_Q3 + EPSDC_Q4  ) / PRICE = VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO = as.numeric(as.no_worse_than_NA(  
    ( EPSD_Q1  + EPSD_Q2  + EPSD_Q3  + EPSD_Q4  ) / PRICE
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100 = as.numeric(
  #  ntile(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO") 
  ) 

  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # end  - earnings / price ratio 
      
  # ( 2 of 5 )
  # begin - sales / price ratio ( price is always large and positive )
  # higher is better 
  
  #	FIELD_NAME  SALES_Q1
  #	FIELD_DESC  Sales Q1
  #	DESCRIP     Income Statement - Quarterly
  #	FM_FILE	SI_ISQ
  
  # higher is better ( Andre put in dilutions )
  
  # CORRECTED NOV 8, 2014
  # 2
  # Sales Q1
  # SALES_Q1
  # Field Type: Dollars in millions (0.0 to 999999.9)

  # sales/1//price/share = share*sales//price ... # to get rid of shares in numerator do "/ (SHR_AQ1 )"

  # ( SALES_Q1 + SALES_Q2 + SALES_Q3 + SALES_Q4  ) / PRICE = VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO = as.numeric(as.no_worse_than_NA(   
    ( SALES_Q1  * ( 1.0 / DILUTION_MULT_Q1 ) / (SHR_AQ1) + SALES_Q2  * ( 1.0 / DILUTION_MULT_Q2 ) / (SHR_AQ2) + SALES_Q3  * ( 1.0 / DILUTION_MULT_Q3 ) / (SHR_AQ3) + SALES_Q4  * ( 1.0 / DILUTION_MULT_Q4 ) / (SHR_AQ4)  ) / PRICE 
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100 = as.numeric(
  #  ntile(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO,100)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO") 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
      
  # end  - sales / price ratio 
      
  # ( 3 of 5 )
  # begin - 'free cash flow' XOR 'book' / price ratio ( price is always large and positive )
  # higher is better 
   
  # 3 definitions and non seem to be the one that I am using
  # http://bizfinance.about.com/od/cashflowanalysis/f/free-cash-flow-calculation.htm
      
  # book: p 143: free cash flow: net cash flow minus capital expenditures, dividends, and preferred dividends
  # good enough in SI Pro - use THIS
      
  #	FIELD_NAME  FCFPS_Q1
  #	FIELD_DESC  Free cash flow/share Q1
  #	DESCRIP     Cash Flow - Quarterly
  #	FM_FILE     SI_CFQ
      
  # Free cash flow/share ( NOTE: COULD BE 'lousy math if a 'stock split/repurchase' is in there? )
  # SI Pro 4.0
  # Free cash flow divided by average shares outstanding.
  # Free cash flow per share = (Cash from operations ? Capital expenditures ? Dividends paid) / Average Shares Outstanding
  # Data Category: Cash Flow - Quarterly
  # Field Type: Dollars per share (0.00 to 9999.99)
  
  # ?
  # HACK #39
  # From eight quarters starting in October 1998, 
  # PP&E ranged from 80 to 400 percent of net income. 
  # Stuffing expenses into this area enabled WorldCom to report net income as positive, 
  # Spot Hanky Panky with Cash Flow Analysis ( CASH FLOW ANALYSIS )
  # Online Investing Hacks ( Buy the book! )
  # http://oreilly.com/pub/h/1868

  # MANUAL CALCULATION 
  
  # higher is better ( Andre dilution is included )
  
  # NOTED NOV 8, 2014
  # 3
  # Free cash flow/share Q1
  # FCFPS_Q1
  # Field Type: Dollars per share (0.00 to 9999.99)

  # fcf/share//price/share = fcf/price O.K
  
  # ( FCFPS_Q1 + FCFPS_Q2 + FCFPS_Q3 + FCFPS_Q4  ) / PRICE = VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO = as.numeric(as.no_worse_than_NA( 
    ( FCFPS_Q1  * ( 1.0 / DILUTION_MULT_Q1 ) + FCFPS_Q2  * ( 1.0 / DILUTION_MULT_Q2 ) + FCFPS_Q3  * ( 1.0 / DILUTION_MULT_Q3 ) + FCFPS_Q4  * ( 1.0 / DILUTION_MULT_Q4 )  ) / PRICE 
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO") 
  ) 

  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  

  ### IMPORTANT ###
  # 'only non-financial companies use sales / price' ( just _NTILE 'NA out" the "Financial" companies )
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO_NTILE100 = ifelse(MG_DESC == "Financial", NA, VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO_NTILE100 ) )

  ### IMPORTANT ###
  ### INSTEAD 'financial companies use 'book / price'
  
  # OShaughnessy2012 p 165 used instead of Book "Common Equity Liquidating Value"
  # AAII April 2014 p 18: Selecting a Validation Method to Determine a Stocks Worth

  # Johnson, Robinson, and Horan 
  # "Book - Commonly used for Financial companies"

  # higher is better 
  
  # Equity (common) Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8
  # Data Table Name: EQUITY_Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8
  # Data Category: Balance Sheet - Quarterly
  # Field Type: Dollars in millions (0.0 to 999999.9)
  # Percent Rank: No
  # Industry/Sector Median: No
   
  # Common stock equity is the total equity minus preferred stock and redeemable preferred stock. 
  # This is also known as book value.

  # Goodwill and intangibles Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8  ( Brown Book )
  # Data Table Name: GWI_Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8
  # Data Category: Balance Sheet - Quarterly
  # Field Type: Dollars in millions (0.0 to 999999.9)
  # Percent Rank: No
  # Industry/Sector Median: No
  
  # ( NOTE: THIS NUMBER CAN TIPICALLY BE 25% OF equity or more ) ( FOR NOW I WILL NOT USE IT )
  # When intangible assets and goodwill are explicitly excluded, 
  # the metric is often specified to be "tangible book value".[4]
  # http://en.wikipedia.org/wiki/Book_value
  # ( EQUITY_Q1 - GWI_Q1 ) / PRICE
  
  # higher is better ( Andre dilution is included )
  
  # CORRECTED NOV 8, 2014
  # (BOOK VALUE)
  # Equity (common) Q1
  # EQUITY_Q1
  # Field Type: Dollars in millions (0.0 to 999999.9)

  # equity/1//price/share = share*equity//price ... # to get rid of shares in numerator do "/ (SHR_AQ1 )"
    
  # EQUITY_Q1 / PRICE = VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO) == FALSE)
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA,       MG_DESC == "Financial"                                          )

  # since still kept for 'non-Financial' just re-calculate(re-overwrite)  on top of 'Financial'
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA, VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO = as.numeric(as.no_worse_than_NA(   
    ( EQUITY_Q1 * ( 1.0 / DILUTION_MULT_Q1 ) / (SHR_AQ1 ) )/ PRICE 
  ) ) )
  
  # since still kept for 'non-Financial' just re-calculate(re-overwrite)  on top of 'Financial'
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO") 
  ) 

  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # end  - 'free cash flow' XOR 'book' / price ratio 
      
      
      
  # ( 4 of 5 ) ( meant to do over 4 quarters )
  # begin - EBITDA / 'enterprise value' ratio ( 'enterprise value' (hopefully) is often large and positive )
  # higher is better 
      
  # Enterprise Value/EBITDA
  # SI Pro 4.0
  # it's important to compare the multiple to other companies or to the industry in general. 
  # Expect higher enterprise multiples in high growth industries (like biotech) and 
  # lower multiples in industries with slow growth (like railways).

  #	FIELD_NAME  EBITDA_Q1
  #	FIELD_DESC  EBITDA Q1
  #	DESCRIP     Income Statement - Quarterly
  #	FM_FILE     SI_ISQ
      
  #	FIELD_NAME  EBIT_Q1   -- SIPro 4.0: EBITDA = EBIT + Depreciation and Amortization
  #	FIELD_DESC  EBIT Q1
  #	DESCRIP     Income Statement - Quarterly
  #	FM_FILE     SI_ISQ
      
  #	FIELD_NAME  DEP_CF_Q1
  #	FIELD_DESC  Depreciation and Amortization - Q1
  #	DESCRIP     Cash Flow - Quarterly
  #	FM_FILE     SI_CFQ
      
  #	FIELD_NAME  ENTVAL_Q1
  #	FIELD_DESC  Enterprise Value Q1
  #	DESCRIP     Balance Sheet - Quarterly
  #	FM_FILE     SI_BSQ
      
  # SLIGHTLY MANUAL CALCULATION 
  # ( theirs: Enterprise Value/EBITDA ( Decimal (-99999.9 to 99999.9)  )
  # their ratio can make BAD values
      
  # higher is better 
  
  # lousy: over 50% data not found an 'EBITDA'
  # ( EBITDA_Q1 + EBITDA_Q2 + EBITDA_Q3 + EBITDA_Q4  ) / ENTVAL_Q1 = VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO
  
  # View(UNIVERSE[,c("MG_DESC","EBITDA_Q1","EBITDA_Q2","EBITDA_Q3","EBITDA_Q4","ENTVAL_Q1","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100")])
  # View(UNIVERSE[UNIVERSE$MG_DESC == "Financial",c("EBITDA_Q1","EBITDA_Q2","EBITDA_Q3","EBITDA_Q4","ENTVAL_Q1","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100")])
  
  # more EBITs found: add back Depreciation and Amortization
  # 97 % overall 99 % "Financial"
  # (  EBIT_Q1 + EBIT_Q2 + EBIT_Q3 + EBIT_Q4 + ifelse(!is.na(DEP_CF_Q1) == TRUE, DEP_CF_Q1, 0.0 ) + ifelse(!is.na(DEP_CF_Q2) == TRUE, DEP_CF_Q2, 0.0 ) + ifelse(!is.na(DEP_CF_Q3) == TRUE, DEP_CF_Q3, 0.0 ) + ifelse(!is.na(DEP_CF_Q4) == TRUE, DEP_CF_Q4, 0.0 ) ) / ENTVAL_Q1 = VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO
  # View(UNIVERSE[,c("EBIT_Q1","EBIT_Q2","EBIT_Q3","EBIT_Q4","ENTVAL_Q1","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100")])
  # View(UNIVERSE[UNIVERSE$MG_DESC == "Financial",c("EBIT_Q1","EBIT_Q2","EBIT_Q3","EBIT_Q4","ENTVAL_Q1","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO","VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100")])
  
  # higher is better ( Andre added dilutions )
  
  # NOTED NOV 8, 2014
  # 4
  # Depreciation Q1
  # DEP_Q1
  # Field Type: Dollars in millions

  # Earnings before interest and taxes (EBIT) Q1
  # EBIT_Q1
  # Field Type: Dollars in millions (-99999.9 to 99999.9)

  # Enterprise Value Q1
  # ENTVAL_Q1
  # Field Type: Dollars in millions (-999990.9 to 999999.9)

  # ebitda//ent_value O.K.
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO = as.numeric(as.no_worse_than_NA(   
    (  EBIT_Q1 * ( 1.0 / DILUTION_MULT_Q1 ) + EBIT_Q2 * ( 1.0 / DILUTION_MULT_Q2 ) + 
       EBIT_Q3 * ( 1.0 / DILUTION_MULT_Q3 ) + EBIT_Q4 * ( 1.0 / DILUTION_MULT_Q4 ) + 
      ifelse(!is.na(DEP_CF_Q1) == TRUE, DEP_CF_Q1 * ( 1.0 / DILUTION_MULT_Q1 ) , 0.0 ) + 
      ifelse(!is.na(DEP_CF_Q2) == TRUE, DEP_CF_Q2 * ( 1.0 / DILUTION_MULT_Q2 ) , 0.0 ) + 
      ifelse(!is.na(DEP_CF_Q3) == TRUE, DEP_CF_Q3 * ( 1.0 / DILUTION_MULT_Q3 ) , 0.0 ) + 
      ifelse(!is.na(DEP_CF_Q4) == TRUE, DEP_CF_Q4 * ( 1.0 / DILUTION_MULT_Q4 ) , 0.0 ) ) / ENTVAL_Q1 
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100 = as.numeric(
  #    ntile(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO") 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
      
  # end  - EBITDA / 'enterprise value' 
      
  # ( 5 of 5 )
  # begin -  shareholder yield ( funky logic )
  # higher is better 
      
  # Shareholder Yield
  # SI Pro 4.0
  # Field Type: Percentage (-999.9 to 99.9)
  # sum of its 
  #  buyback yield and - comparing the average shares Q1 from Q5 ( Percentage (-999.9 to 99.9) )
  #  dividend yield    - called Yield - dividing the indicated dividend by the current stock price. 
    #  Dividend Yield = [Dividend, indicated] / [Price] 
    #    Dividend, indicated - cumulative per-share dividend a company expects to pay over the next four quarters. 
    #                          Typically calculated by multiplying the latest per-share dividend paid by four.
  # and shows what percentage of total cash the company is paying out to shareholders,
 
  #### SI Pro 4.0
  #### Dividend, indicated - multiplying the latest per-share dividend paid by four
      
  # Shareholder Yield
  # OShaugnnessy p 225
  # Percentage of total cash the company is paying out to its shareholders
  #   either in the form of a cash dividend or
  #   as expended cash to re-purchase its shares on the open market  
      
  # FIELD_NAME  SHY
  # FIELD_DESC  Shareholder Yield
  # DESCRIP     Multiples
  # FM_FILE     SI_MLT ( primary key: COMPANY_ID )
      
  # SINCE (NON-QUARTER cyclical, I will use their AUTOMATIC )
      
  # higher is better ( Andre added dilutions )
  
  # SHY = VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD
  
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD = as.numeric(as.no_worse_than_NA(   
    SHY  * ( 1.0 / DILUTION_MULT_Q1 ) 
  ) ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD") 
  ) 
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # end  - shareholder yield
      
                      
  # begin - value two composite - rebalance and scoring
  # (USE) March 2014:  at least 3 of 5 factors, then rebalance  ( technique seems fairer )
  # 2012 book: if NA, then assign 50 (of ntile 100 ) ( seems to unfairly punish companies for slow/non-exist reporting?)
  
  # count up of non-NA ntiles
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_UNIQUE_SCORES_CNT = as.numeric(
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100)                      == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100)                     == TRUE, 1.0, 0.0) + 
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO_NTILE100)             == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100)                   == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100)                                == TRUE, 1.0, 0.0)  
  ))
  
  # minimum two factors are required
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM = as.numeric(
    ifelse( VAL_EXPOSE_VAL_TWO_CMPST_UNIQUE_SCORES_CNT >= 3.0, 
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100)                      == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100,    0.0) +
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100)                     == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100,   0.0) + 
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO_NTILE100)             == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_XOR_BOOK_TO_PRCE_RATIO_NTILE100,   0.0) +
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100)                   == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100, 0.0) +
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100)                                == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100,              0.0) 
    , NA) 
  ))
  
  # five factors total possible
  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL = as.numeric(
     VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM * 5.0 / VAL_EXPOSE_VAL_TWO_CMPST_UNIQUE_SCORES_CNT
  ))
                      
  # higher value is BETTER 
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL) == FALSE)
  
  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
  #   ntile(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL,100)
  # ))
  
  UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL") 
  ) 
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
                      
  # end value_two_composite
  
  # end value expose
  
  
  # begin conclusion
  
  
  # need a median                _MEDIAN_PASSED
  # 2012 p. 569 - must be in the 'upper 50%' of the combined composites of
  #               FIN, EARN, VAL_TWO_
  #               ( means I add per stock 
  #               _FIN_ + _EARN_ + _VAL_TWO_
                              
  # higher value is BETTER 
   
  # Buy the 25 stocks with the 'best Value Composite Two' scores
  #                               WINNERS25
   
  # need a median   
  
  # higher value is BETTER 
   
  # VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100 = VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM 

  UNIVERSE <- mutate(UNIVERSE, VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM  = as.numeric(   
    VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100
  ) )
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM ) == FALSE)

  # UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2 = as.numeric(
  #   ntile(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM ,2)
  # ))
  
 UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    hdntile(.,"VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM", buckets = 2) 
  ) 
  
  # A LEFT_OFF ( FIX hdrank 'NOT LOOSE ITS GROUPING WHEN A PARAMETER IS SET' )
  
  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # 2012 p. 569 - must be in the 'upper 50%' of the combined composites of
  #               FIN, EARN, VAL_TWO_
  #               ( means I add per stock 
  #               _FIN_ + _EARN_ + _VAL_TWO_
                              
  # higher value is BETTER 
  
    # 2012 p. 569
  # Buy the 25 stocks with the 'best Value Composite Two' scores
  #                               TOP25
  
  # "Who is ranked number 1?" 
  # ( lower 'ranks' are higher 'values' )  
  # too many ties on _SUMM_REBAL_NTILE100 ( 600 - 700 total )  instead do _SUMM_REBAL and _SUMM_REBAL_NTILE100_SUMM
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  # UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2 )  == FALSE)

  # UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA,       VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2    == 2     )
  
  # UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL )                     == FALSE) 
  
  # UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM )     == FALSE) 
  
  
  # Note: I was too in a hurry to finish the program ( may be 'too much NA removals': need to review ( someday ) )
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, 
      is.na(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2)  == FALSE
    ,       VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2   == 2 
    , is.na(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL )                        == FALSE
    , is.na(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM )        == FALSE
  )

  UNIVERSE_NOT_NA <- arrange(UNIVERSE_NOT_NA
    , desc(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL) 
    , desc(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM)
  )

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN = as.numeric(
    1:NROW(UNIVERSE_NOT_NA)
  ))

  UNIVERSE <- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
 


  # 2012 p. 569
  # Buy the 25 stocks with the 'best Value Composite Two' scores
  #                               TOP25
  

  # good ALL_CMBND_CMPST(better than half) and great VAL_TWO_CMPST(best to less best)
  # note: magritter: FMA ( A - can not see columns created in M ) # I SHOULD REPORT THIS ERROR
  
  UNIVERSE_F <- filter(UNIVERSE, VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN <= 25 ) 
  
  UNIVERSE_FM <-
    mutate( UNIVERSE_F
            , VAL_TWO_CMPST_SUMM_REBAL_NTILE100   = VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100
            , ALL_CMBND_NTILE100_SUMM             = VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM
            , ALL_CMBND_SUMM_NTILE2               = VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2
            , SCT_MN_PRCE_10M_SMA      = PRICE_WGHT_MEAN_SMA_10_M_SECTOR
            , SCT_MN_PRCE              = PRICE_WGHT_MEAN_SECTOR
            , SCT_MN_PRCE_10M_SMA_SVVR = PRICE_WGHT_MEAN_SMA_10_M_SECTOR_SVVR
          ) 
          

  UNIVERSE_FMA <- arrange( UNIVERSE_FM
  ,      VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN
  , desc(VAL_TWO_CMPST_SUMM_REBAL_NTILE100)
  , desc(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL)
  , desc(ALL_CMBND_SUMM_NTILE2)
  , desc(ALL_CMBND_NTILE100_SUMM)
  )[,c( 
  "MONTHDATEDT", "WEEKDATEDT","SPLITDATEDT", 
  "TICKER","COMPANY","DIVNQXDT","DIVNQPDT","MG_DESC","SCT_MN_PRCE_10M_SMA_SVVR","MKTCAP","PSD_SPLITDT_DATE","SPLIT_FACT","PERENDDT_Q2", "PERLEN_Q1", "PERTYP_Q1", "PERENDDT_Q1"
  # ,"PERENDDT_Q0" ( NOT USEFUL )
  ,"VAL_TWO_CMPST_SUMM_REBAL_NTILE100", "VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL", "ALL_CMBND_NTILE100_SUMM","ALL_CMBND_SUMM_NTILE2","VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN"
  ,"SCT_MN_PRCE_10M_SMA","SCT_MN_PRCE")
    ]
          
  setwd(oldwd)

  

  the_end_debug_bookmark_here <- 1
  # rm(list=ls(all.names=TRUE))
  # source('N:/MyVMWareSharedFolder/foresight3/R/main-foresight3-999.R', echo=TRUE)

  # View(UNIVERSE_FMA)
  # View(WGHT_MN_PRICE_GRID)

  # list( UNIVERSE_FMA = UNIVERSE_FMA, WGHT_MN_PRICE_GRID = WGHT_MN_PRICE_GRID )

  ##  View(main_foresight3_999())

  return(list( UNIVERSE_FMA = UNIVERSE_FMA, WGHT_MN_PRICE_GRID = WGHT_MN_PRICE_GRID ))      
  # return(UNIVERSE_FMA)

  # end of feb,may,aug,nov :buy 4 stocks
  #        other months    :buy 2 stocks
  # maybe a good idea to only invest in sectors with a 10 month Mebane Faber 'uptrend'
  # darn good idea to BAIL when FED/PIGER/other 3-month recession indicators flair UP
  
  # end conclusion
  
}


# BEGIN SUNDAY



# higest: swap out 'ntile' with 'do(  hgquantile )'
# [x] big effort

  # [X] DONE ( NOTE 669 observations returned instead of 668 )
  # UNIVERSE_NOT_NA <- do(UNIVERSE_NOT_NA, 
    # hdntile(.,"PRCHG_13W", buckets = 2 ) 
  # ) 

# BEGIN THURSDAY ( DAY OF LEAVE )
  
  # [ ] MISSING 'unit test for "posneg = -1"' but super highly unlikely to fail
  #     ( note: all hdntile [ ] needs testhat testing )
  
  # [x] !!!! ***** SWAP OUT THE REMANDER ntile --> hdntile ****

   # [ ] ADJUST (PROGRAM) hdntile/ndrank SO "casecount" IS AN OPTION (ndrank WOULD like)
   # [-] [ALSO  ADD trunc(a/b) + 1 option (hdrank CASE)
  
# fix that report date + 'Add Periods' shift see TimeWarp ( if I go back to future end_of_Q preduction )
# [ ] ( PROB - NOT WORK THE EFFORT )

# style and conversion ( sqldf to work for me ) 
# EASIER OUTPUT OF DATE                         [ ] MAY BE TOO CUMBERSOME
# If you name the output column the same name as an input column which has "Date" class 
# then it will correctly infer that the output is to be of class "Date" as well.
#   "select max(sale_date) sale_date from test1"
# http://code.google.com/p/sqldf/ FAQ #4

# note ( right answer is returned as date)
# sqldf(sprintf("select * from DF where a >= %d", Sys.Date() + 2))
# 2009-08-01 2   
# http://code.google.com/p/sqldf/ FAQ #4


# CLEAN SOME CODE UP ( SIMPLIFY SOME 'REPEATS T FUNCTIONS' )
# (1) eliminate duplicates [x]
# (2) repeating 'nite' tests [x] 

# INTEGRATE VJAYS ( CENTRALIZED DIRECTORY FUNCTION )
# [ ] YYYY-MM-DD directories  ( also scan .txt for copyDirectory ) ( IIF - I DO "REAL" BACKTESTING )

# FILE STARTUP SPEED
# [x] if !file.exists(...) ... load file .... ( once only ) ... save file
    
# [ ] of top 600 SOME MISSING 'not there yet date' ( maybe a lower LONGER term thing )
#       [ ]--- quantmod: 'Google Finance' ( is it there )
#       [ ]    XML and EDGAR(fed gov), ( is it there? )    
    
# TO_DO ( NOTE DONE YET ) : END OF DAY. "Put Up on GitHub" [X] August 13, 2014
# main-foresight-999.R [ ] 
# helpers.R rename   helpers-foresight-999.R [x]    [ ] THIS ONE IS NEW
    
# END SUNDAY 
    
# SOON ( MAKE SURE PRICES AND SPLITS ARE HOPEFULLY SMOOTH ) ( SHORT TERM - JUST INSPECT AND ELIM )
# YES - AAPL 7 TO 1 SPLIT CORRECTLY [X]

# [ ]
#   RARE DATA CASE: FAR FUTURE: NOT A PRIORITY
# percent ch in debt ( STILL NEAR ZERO PROBLEMS?: need some weight with assets - liabilies = equity? )
# weight START OF THOUGHT: abs( (assets - liabilites)/assets )   -> 1 good    -> 0 bad   ->  "> 1" very bad

# START LOOKING AT (TO START) FED & PIGER & RANDOMFOREST
    
# NEXT, REVIEW THAT "YELLOW FOLDER" CLEAN_UP/CENTRALIZE 'SOME' REPEATING CODE
 
  # NOTE: View(cbind(UNIVERSE[,"ASSETS_Q1"]-UNIVERSE[,"LIAB_Q1"],UNIVERSE[,"EQUITY_Q1"]))
  #       ASSETS_Q1 & LIAB_Q1 are always large and positive
  #       ASSETS_Q1 - LIAB_Q1 = EQUITY_Q1(sometimes negative)

  
# rm(list=ls(all.names=TRUE))
# source('N:/MyVMWareSharedFolder/foresight3/R/main-foresight3-999.R', echo=TRUE)

# [ ] LEFT_OFF ret_dollar_price_grid_do() ... need to create those xts OBJECTS
# [ ] SAVE TO github

