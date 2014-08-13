
     
 
options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width
options(sqldf.driver = "SQLite")

library(testthat)

library(tcltk)
library(Rgraphviz)

library(sqldf)

library(lubridate)

library(plyr)
library(dplyr)
library(data.table)

library(foreign)

setwd("N:\\MyVMWareSharedFolder\\foresight3\\R")

# source( paste0(getwd(),"/sql.select.R"))
# may override as.data.frame ( BE CAREFUL )

source( paste0(getwd(),"/helpers.R"))

# IN YELLOW window2 only
# library(testthat)
# auto_test("./R", "./tests/testthat") 


main_foresight3_999 <- function(pauseat=NULL) {


  # if(pauseat=="HERE") {}

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
  SETUP <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Setup.dbf", as.is = TRUE)))
  
  SETUP_tbl_sqlite <- copy_to(dpsqllconn, SETUP, temporary = FALSE
    , indexes = list(
    )
  )
  SETUP <<- tbl_df(SETUP)
  
  
  # SI_CI ( TYPICALLY BIG: 7 thousand )
  #
  # company information 
  SI_CI <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Static/si_ci.dbf", as.is = TRUE)))
  
    primary_key_dup <- SI_CI[duplicated(SI_CI[,'TICKER']),,drop=FALSE]
    new_df_no_duplicates <- SI_CI[!(SI_CI$TICKER %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_CI <<- new_df_no_duplicates
  
    primary_key_dup <- SI_CI[duplicated(SI_CI[,'COMPANY_ID']),,drop=FALSE]
    new_df_no_duplicates <- SI_CI[!(SI_CI$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_CI <<- new_df_no_duplicates
  
    rm(primary_key_dup,new_df_no_duplicates)
  
  SI_CI_tbl_sqlite <- copy_to(dpsqllconn, SI_CI, temporary = FALSE
    , indexes = list(
        c("TICKER")
      , c("COMPANY_ID")
      , c("EXCHANGE")  # see UNIVERSE
      , c("IND_2_DIG") # see UNIVERSE
    )
  )
  SI_CI <<- tbl_df(SI_CI)

  
  
  # small LOOK-UP tables
  # read 'sectors and industries'     
  # SI_MGDSC.MG_CODE '2 char: industry'     
  # SI_MGDSC.MG_DESC '4 char' description'
  SI_MGDSC <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Static/SI_MGDSC.DBF", as.is = TRUE)))
    primary_key_dup <- SI_MGDSC[duplicated(SI_MGDSC[,'MG_CODE']),,drop=FALSE]
    new_df_no_duplicates <- SI_MGDSC[!(SI_MGDSC$MG_CODE %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_MGDSC <<- new_df_no_duplicates
    rm(primary_key_dup,new_df_no_duplicates)
  
  SI_MGDSC_tbl_sqlite <- copy_to(dpsqllconn, SI_MGDSC, temporary = FALSE
    , indexes = list(
        c("MG_CODE")
    )
  )
  
  SI_MGDSC <<- tbl_df(SI_MGDSC)

  # SI_EXCHG.EXCHG_DESC
  #
  # 3 WAY JOIN
  # SI_CI.EXCHANGE  = SI_EXCHG.EXCHG_CODE AND
  # SI_CI.IND_2_DIG = SI_MGDSC.MG_CODE
  
  # Major stock exchange information 'long descriptions'
  SI_EXCHG <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Static/SI_EXCHG.DBF", as.is = TRUE)))
    primary_key_dup <- SI_EXCHG[duplicated(SI_EXCHG[,'EXCHG_CODE']),,drop=FALSE]
    new_df_no_duplicates <- SI_EXCHG[!(SI_EXCHG$EXCHG_CODE %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_EXCHG <<- new_df_no_duplicates
    rm(primary_key_dup,new_df_no_duplicates)
  
  SI_EXCHG_tbl_sqlite <- copy_to(dpsqllconn, SI_EXCHG, temporary = FALSE
    , indexes = list(
        c("EXCHG_CODE")
    )
  )
  
  SI_EXCHG <<- tbl_df(SI_EXCHG)


  # SI_PSD.MKTCAP
  #
  # 4 WAY JOIN
  # SI_CI.EXCHANGE = SI_EXCHG.EXCHG_CODE AND
  # SI_CI.IND_2_DIG = SI_MGDSC.MG_CODE AND
  # SI_CI.COMPANY_ID = SI_PSD.COMPANY_ID
  
  # Price and Share Staticsics - Market Capitalization
  SI_PSD <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Dbfs/si_psd.dbf", as.is = TRUE)))
    primary_key_dup <- SI_PSD[duplicated(SI_PSD[,'COMPANY_ID']),,drop=FALSE]
    new_df_no_duplicates <- SI_PSD[!(SI_PSD$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_PSD <<- new_df_no_duplicates
    rm(primary_key_dup,new_df_no_duplicates)
  
    # all math must be numeric
    SI_PSD <<- mutate(SI_PSD, MKTCAP = as.numeric(MKTCAP) )
  
  SI_PSD_tbl_sqlite <- copy_to(dpsqllconn, SI_PSD, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_PSD <<- tbl_df(SI_PSD)

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
  
  
  # Dates and Periods - Ending date Q1
  SI_DATE <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Static/si_date.dbf", as.is = TRUE)))
    primary_key_dup <- SI_DATE[duplicated(SI_DATE[,'COMPANY_ID']),,drop=FALSE]
    new_df_no_duplicates <- SI_DATE[!(SI_DATE$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_DATE <<- new_df_no_duplicates
    rm(primary_key_dup,new_df_no_duplicates)
  
  SI_DATE_tbl_sqlite <- copy_to(dpsqllconn, SI_DATE, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_DATE <<- tbl_df(SI_DATE)

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
  
  UNIVERSE <<- sqldf(" 
    SELECT STP.MONTHDATE AS MONTHDATEUNX, STP.WEEKDATE AS WEEKDATEUNX, STP.SPLITDATE AS SPLITDATEUNX, 
           CI.TICKER, CI.COMPANY, CI.COMPANY_ID, CI.SIC, CI.EXCHANGE, EXCHG.EXCHG_DESC 
         , CI.IND_2_DIG, MGDSC.MG_DESC, CI.COUNTRY, CI.ADR, DTE.PEREND_Q2 AS PERENDUNX_Q2, DTE.PEREND_Q1 AS PERENDUNX_Q1, DTE.PERLEN_Q1 AS PERLEN_Q1__integer, PERTYP_Q1 
                           , PSD.MKTCAP, PSD.PRICE PRICE__numeric, PSD.SPLIT_DATE AS PSD_SPLITUNX_DATE, PSD.SPLIT_FACT AS SPLIT_FACT__numeric
                           FROM 
                           main.SETUP STP, 
                           main.SI_CI CI, main.SI_EXCHG EXCHG, main.SI_MGDSC MGDSC, main.SI_PSD PSD, main.SI_DATE DTE
                           WHERE CI.EXCHANGE = EXCHG.EXCHG_CODE AND 
                           CI.IND_2_DIG = MGDSC.MG_CODE AND 
                           CI.COMPANY_ID = PSD.COMPANY_ID AND 
                           CI.COMPANY_ID = DTE.COMPANY_ID 
  ORDER BY MKTCAP DESC 
                           "  , connection = dpsqllconn$con, method="name__class")
  
  # preserve the original ordering
  UNIVERSE[,"ORIG_ORDER"]   <<- 1:NROW(UNIVERSE) # str() - shows integer , Rstudio GUI shows numeric


  
  # 0.4 seconds - a work in progress
  # library(compiler)
  # lubridate:::add_period_to_date
  # add_period_to_date_comp <- cmpfun(lubridate:::add_period_to_date)
  # UNIVERSE[,"PERENDDT_Q1"]  <<- as.character( add_period_to_date_comp( dmy("1/1/1970",tz = "EST"), days(UNIVERSE[,"PERENDUNX_Q1"]) ))
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
  
  # lineprof & shine 0.4 ...> 0.16 BETTER
  
                                                                          # renamed column, already loaded as num                                    # sqldf I MADE this NUMERIC
  UNIVERSE[,"MONTHDATEDT"]       <<- as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(UNIVERSE[,"MONTHDATEUNX"]))

                                                                          # renamed column, already loaded as num                                    # sqldf I MADE this NUMERIC
  UNIVERSE[,"WEEKDATEDT"]       <<- as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(UNIVERSE[,"WEEKDATEUNX"]))

                                                                            # renamed column, already loaded as num                                    # sqldf I MADE this NUMERIC
  UNIVERSE[,"SPLITDATEDT"]       <<- as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(UNIVERSE[,"SPLITDATEUNX"]))


                                                                          # renamed column, already loaded as num                                    # sqldf I MADE this NUMERIC
  UNIVERSE[,"PERENDDT_Q2"]       <<- as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(UNIVERSE[,"PERENDUNX_Q2"]))

  
  
                                                                          # renamed column, already loaded as num                                    # sqldf I MADE this NUMERIC
  UNIVERSE[,"PERENDDT_Q1"]       <<- as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(UNIVERSE[,"PERENDUNX_Q1"]))
  
  UNIVERSE[,"PSD_SPLITDT_DATE"]  <<- as.character(ymd_hms(c("1970-01-01 16:00:00.880-0400")) + days(UNIVERSE[,"PSD_SPLITUNX_DATE"]))
 
  
  
  
  # library(compiler)
  # addboth <-function(a,b) a + b
  # addboth_comp <- cmpfun(addboth)
  # UNIVERSE[,"PERENDDT_Q1"]  <<- as.character(addboth_comp(dmy("1/1/1970",tz = "EST"),days(UNIVERSE[,"PERENDUNX_Q1"])))
  
  # this PERENDDT_Q0 ( almost future) is not useful ( I am removing )
  ## not NA
  # UNIVERSE[,"PERENDDT_Q0"]  <<- as.character(dmy("1/1/1970",tz = "EST") + days(UNIVERSE[,"PERENDUNX_Q1"])  ) 
  
  ## months
  
  # UNIVERSE[,"PERENDDT_Q0"]  <<- ifelse(UNIVERSE[,"PERTYP_Q1"] == "M",
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <<- ifelse(UNIVERSE[,"PERTYP_Q1"] == "M" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <<- ifelse(UNIVERSE[,"PERTYP_Q1"] == "M" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(3) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + months(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  ## weeks
  
  # UNIVERSE[,"PERENDDT_Q0"]  <<- ifelse(UNIVERSE[,"PERTYP_Q1"] == "W",
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(0) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <<- ifelse(UNIVERSE[,"PERTYP_Q1"] == "W" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(1) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )

  # UNIVERSE[,"PERENDDT_Q0"]  <<- ifelse(UNIVERSE[,"PERTYP_Q1"] == "W" & is.na(UNIVERSE[,"PERENDDT_Q0"]),
    # ifelse(is.na(as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))),
                 # as.character(dmy("1/1/1970",tz = "EST") - days(3) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # ,
                 # as.character(dmy("1/1/1970",tz = "EST") - days(2) + days(UNIVERSE[,"PERENDUNX_Q1"]) + weeks(UNIVERSE[,"PERLEN_Q1"]))
    # )
  # , UNIVERSE[,"PERENDDT_Q0"]
  # )
 
 


    # 'all stocks' universe
  
  # NOT loaded into SQLite YET
  # sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # NOT dplyrized YET
  # strip off
  # UNIVERSE <<- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
        c("ADR","MKTCAP","COUNTRY","EXCHG_DESC")
    )
  )
  
  UNIVERSE  <<- sqldf("SELECT * FROM main.UNIVERSE UNIV WHERE 
                           UNIV.ADR == 0 AND 
                           UNIV.MKTCAP > 220.0 AND 
                           UNIV.COUNTRY == 'United States' AND 
                           UNIV.EXCHG_DESC NOT IN ('Over the counter') 
                           ", connection = dpsqllconn$con)
  
  UNIVERSE <<- tbl_df(UNIVERSE)
  

  # end elimintate un-investibles ( keep investibles) ( All Stocks)
  
  
  # begin growth expose 
  # The 'growth part of 'marrying growth and value' 
  # uses 'All stocks' universal median 
  
  
  # require 3-month price appreciation greater than the universal 'All stocks' median (UNIVERSE)
  # from SI_PSD  add back PRCHG_13W
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # strip off
  UNIVERSE <<- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
   #    c("TICKER")
   # ,  
        c("COMPANY_ID")
   # ,  c("ORIG_ORDER")
      
    )
  )
  
  UNIVERSE <<- tbl_df(UNIVERSE)
  
  UNIVERSE <<- sqldf("SELECT UNIV.*, PSD.PRCHG_13W FROM 
                                   main.UNIVERSE UNIV, main.SI_PSD PSD WHERE 
                                   UNIV.COMPANY_ID = PSD.COMPANY_ID 
                                   ", connection = dpsqllconn$con)
  
  UNIVERSE <<- tbl_df(UNIVERSE)
  
  
  # all math must be numeric
  UNIVERSE <<- mutate(UNIVERSE, PRCHG_13W = as.numeric(PRCHG_13W) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(PRCHG_13W) == FALSE)
  
  # ntile 'higher value' is 'higher ntile'
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,PRCHG_13W_NTILE2 = as.numeric(
    ntile(PRCHG_13W,2)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
  # surviVor of '3-month price appreciation greater than the universal 'All stocks' median (UNIVERSE)'
  UNIVERSE <<- mutate(UNIVERSE, GROWTH_EXPOSE_PRCHG_13W_NTILE2_SRVVR = ifelse(PRCHG_13W_NTILE2 == 2, 1, 0)  )
  
  # require 6-month price appreciation greater than the universal median (ALLSTOCKS)
  # from SI_PSD  add back PRCHG_26W
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # strip off
  UNIVERSE <<- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
        c("TICKER")
     ,  c("COMPANY_ID")
     ,  c("ORIG_ORDER")
      
    )
  )
  
  UNIVERSE <<- sqldf("SELECT UNIV.*, PSD.PRCHG_26W FROM 
                                   main.UNIVERSE UNIV, main.SI_PSD PSD WHERE 
                                   UNIV.COMPANY_ID = PSD.COMPANY_ID 
                                   ", connection = dpsqllconn$con)
  
  UNIVERSE <<- tbl_df(UNIVERSE)
  
  
  # all math must be numeric
  UNIVERSE <<- mutate(UNIVERSE, PRCHG_26W = as.numeric(PRCHG_26W) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(PRCHG_26W) == FALSE)
  
  # ntile 'higher value' is 'higher ntile'
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,PRCHG_26W_NTILE2 = as.numeric(
    ntile(PRCHG_26W,2)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
  # surviVor of '6-month price appreciation greater than the universal 'All stocks' median (UNIVERSE)'
  UNIVERSE <<- mutate(UNIVERSE, GROWTH_EXPOSE_PRCHG_26W_NTILE2_SRVVR = ifelse(PRCHG_26W_NTILE2 == 2, 1, 0)  )
  
  
  
  # have an 'annual' 'EPS change' greater than zero (0)'
  # LAST of growth expose
  

  # 'not need 'relative' , need 'absolute 'higher is better )
  # relative   -6 - ( -3 ) /    ( -3 )   = -3 / -3 =  1
  # absolute   -6 - ( -3 ) / abs( -3 )   = -3 /  3 = -1 ( WANT THIS )
  # ( ( EPS_Q1 + EPS_Q2 + EPS_Q3 + EPS_Q4 ) - ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 ) ) / abs( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 ) * 100.0
  
  # FIELD_NAME  EPS_Q1
  # FIELD_DESC  EPS Q1
  # DESCRIP     Income Statement - Quarterly
 	# FM_FILE     SI_ISQ
  
  SI_ISQ <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Static/si_isq.dbf", as.is = TRUE)))
  
    primary_key_dup <- SI_ISQ[duplicated(SI_ISQ[,'COMPANY_ID']),,drop=FALSE]
    new_df_no_duplicates <- SI_ISQ[!(SI_ISQ$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_ISQ <<- new_df_no_duplicates
  
    rm(primary_key_dup,new_df_no_duplicates)
  
  SI_ISQ_tbl_sqlite <- copy_to(dpsqllconn, SI_ISQ, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  SI_ISQ <<- tbl_df(SI_ISQ)

  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)

  # strip off
  UNIVERSE <<- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
    , indexes = list(
   #    c("TICKER")
   # ,  
        c("COMPANY_ID")
   # ,  c("ORIG_ORDER")
      
    )
  )
  
  UNIVERSE <<- sqldf("SELECT UNIV.*, ISQ.EPS_Q1, ISQ.EPS_Q2, ISQ.EPS_Q3, ISQ.EPS_Q4, -- comment
                                      ISQ.EPS_Q5, ISQ.EPS_Q6, ISQ.EPS_Q7, ISQ.EPS_Q8 
                                  FROM 
                                   main.UNIVERSE UNIV, main.SI_ISQ ISQ WHERE 
                                   UNIV.COMPANY_ID = ISQ.COMPANY_ID 
                                   ", connection = dpsqllconn$con)
  
  UNIVERSE <<- tbl_df(UNIVERSE)
  
  # all math must be numeric
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q1 = as.numeric(EPS_Q1) )
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q2 = as.numeric(EPS_Q2) )
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q3 = as.numeric(EPS_Q3) )
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q4 = as.numeric(EPS_Q4) )
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q5 = as.numeric(EPS_Q5) )
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q6 = as.numeric(EPS_Q6) )
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q7 = as.numeric(EPS_Q7) )
  UNIVERSE <<- mutate(UNIVERSE, EPS_Q8 = as.numeric(EPS_Q8) )
  
  UNIVERSE <<- mutate(UNIVERSE, GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH = as.numeric(   
    ( ( EPS_Q1 + EPS_Q2 + EPS_Q3 + EPS_Q4 ) - ( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 ) ) / abs( EPS_Q5 + EPS_Q6 + EPS_Q7 + EPS_Q8 ) * 100.0
  ) )
  
  UNIVERSE <<- mutate(UNIVERSE, GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH_GR_THAN_0_SRVVR = ifelse(   
    GROWTH_EXPOSE_ANNUAL_EPS_CH_PCT_GROWTH > 0.0, 1, 0
  ) )
  
  # growth expose winners so far
  
  UNIVERSE <<- mutate(UNIVERSE, GROWTH_EXPOSE_SRVVR = 
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
  
  UNIVERSE <<- data.table(UNIVERSE)
  setkeyv(UNIVERSE,c("GROWTH_EXPOSE_SRVVR"))
  UNIVERSE <<- UNIVERSE[GROWTH_EXPOSE_SRVVR==1]
  UNIVERSE <<- as.data.frame(UNIVERSE, stringsAsFactors = FALSE)
  
  # begin financial composite ( defensive posture )
  
  SI_BSQ <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Static/si_bsq.dbf", as.is = TRUE)))
  
  primary_key_dup <- SI_BSQ[duplicated(SI_BSQ[,'COMPANY_ID']),,drop=FALSE]
  new_df_no_duplicates <- SI_BSQ[!(SI_BSQ$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
  SI_BSQ <<- new_df_no_duplicates
  
  rm(primary_key_dup,new_df_no_duplicates)
  
  SI_BSQ_tbl_sqlite <- copy_to(dpsqllconn, SI_BSQ, temporary = FALSE
                               , indexes = list(
                                 c("COMPANY_ID")
                               )
  )
  SI_BSQ <<- tbl_df(SI_BSQ)
  

  SI_CFQ <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Static/si_cfq.dbf", as.is = TRUE)))
  
  primary_key_dup <- SI_CFQ[duplicated(SI_CFQ[,'COMPANY_ID']),,drop=FALSE]
  new_df_no_duplicates <- SI_CFQ[!(SI_CFQ$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
  SI_CFQ <<- new_df_no_duplicates
  
  rm(primary_key_dup,new_df_no_duplicates)
  
  SI_CFQ_tbl_sqlite <- copy_to(dpsqllconn, SI_CFQ, temporary = FALSE
                               , indexes = list(
                                 c("COMPANY_ID") 
                               )
  )
  SI_CFQ <<- tbl_df(SI_CFQ)
  
  
  # begin joins
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)
  
  # strip off
  UNIVERSE <<- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
                                 , indexes = list(
                                   #    c("TICKER")
                                   # ,  
                                   c("COMPANY_ID")
                                   # ,  c("ORIG_ORDER")
                                   
                                 )
  )
  
  
  UNIVERSE <<- sqldf("SELECT UNIV.* 
                      , BSQ.LIAB_Q1 AS LIAB_Q1__numeric, BSQ.LIAB_Q2 AS LIAB_Q2__numeric                                  -- %change in debt ( and debt / equity ratio )
                      , BSQ.LIAB_Q3 AS LIAB_Q3__numeric, BSQ.LIAB_Q4 AS LIAB_Q4__numeric, BSQ.LIAB_Q5 AS LIAB_Q5__numeric -- %change in debt
                      , BSQ.EQUITY_Q1 AS EQUITY_Q1__numeric                                                               -- debt / equity ratio
                      , CFQ.TCF_Q1 AS TCF_Q1__numeric, CFQ.TCF_Q2 AS TCF_Q2__numeric, CFQ.TCF_Q3 AS TCF_Q3__numeric       -- cash flow from financing' / average assets
                      , CFQ.TCF_Q4 AS TCF_Q4__numeric, CFQ.TCF_Q5 AS TCF_Q5__numeric
                      , CFQ.TCF_Q6 AS TCF_Q6__numeric, CFQ.TCF_Q7 AS TCF_Q7__numeric, CFQ.TCF_Q8 AS TCF_Q8__numeric       -- earnings composite: percent change in NOA
                      , BSQ.ASSETS_Q1 AS ASSETS_Q1__numeric, BSQ.ASSETS_Q2 AS ASSETS_Q2__numeric                          -- cash flow from financing' / average assets
                      , BSQ.ASSETS_Q3 AS ASSETS_Q3__numeric, BSQ.ASSETS_Q4 AS ASSETS_Q4__numeric 
                      , BSQ.ASSETS_Q5 AS ASSETS_Q5__numeric 
                      , CFQ.NCC_Q1 AS NCC_Q1__numeric, CFQ.NCC_Q2 AS NCC_Q2__numeric, CFQ.NCC_Q3 AS NCC_Q3__numeric       -- annual cash flow / debt 
                      , CFQ.NCC_Q4 AS NCC_Q4__numeric, CFQ.NCC_Q5 AS NCC_Q5__numeric
                                  FROM 
                                   main.UNIVERSE UNIV, main.SI_BSQ BSQ, main.SI_CFQ CFQ WHERE 
                                   UNIV.COMPANY_ID = BSQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = CFQ.COMPANY_ID 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <<- tbl_df(UNIVERSE)
  
  
  # end joins
  
  
  
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
  
  ## group_by MG_DESC  ( sector )
  # lower value is BETTER ' reverse of ntile'
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT = as.numeric(   
    ( LIAB_Q1 - LIAB_Q5 ) / abs(LIAB_Q5 + 0.0000001) * 100.0
  ) )

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100 = as.numeric(
    ntile((-1)*VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
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
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY = as.numeric(   
    LIAB_Q1 / (EQUITY_Q1 + 0.0000001)
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY) == FALSE)
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100 = as.numeric(
    ntile((-1)*VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
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
  # ( TCF_Q2 + TCF_Q3 + TCF_Q4 + TCF_Q5 ) / ( ( ASSETS_Q2 + ASSETS_Q3 + ASSETS_Q4 + ASSETS_Q5 ) / 4.0 ) = VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING

  # will use ( but do SQL load TCF_Q1 and ASSETS_Q1 ) BUT WILL USE 'AAII method of March 2014'
  # ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4) / ( ( ASSETS_Q1 + ASSETS_Q2 + ASSETS_Q3 + ASSETS_Q4 ) / 4.0 ) = VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING = as.numeric(   
    ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4) / ( ( ASSETS_Q1 + ASSETS_Q2 + ASSETS_Q3 + ASSETS_Q4 ) / 4.0 )
  ) )
  

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100 = as.numeric(
    ntile((-1)*VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
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

  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT = as.numeric(   
    ( NCC_Q1 + NCC_Q2 + NCC_Q3 + NCC_Q4) / ( LIAB_Q1 + LIAB_Q2 + LIAB_Q3 + LIAB_Q4 + 0.0000001 ) 
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT) == FALSE)
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  ### IMPORTANT ###
  # 'only non-financial companies use THIS' ( just _NTILE 'NA out" the "Financial" companies )
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100 = ifelse(MG_DESC == "Financial", NA, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100)  )
  
  # end - financial composite - annual cash flow / debt 
  
  # begin - financial composite - rebalance and scoring
  # (USE) March 2014:  at least 2 of 4 factors (financial composite case), then rebalance  ( technique seems fairer )
  # 2012 book: if NA, then assign 50 (of ntile 100 ) ( seems to unfairly punish companies for slow/non-exist reporting?)
  
  # count up of non-NA ntiles
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_UNIQUE_SCORES_CNT = as.numeric(
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100)            == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100)         == TRUE, 1.0, 0.0) + 
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100)     == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100) == TRUE, 1.0, 0.0)  
  ))
  
  # minimum two factors are required
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_SCORES_SUMM = as.numeric(
    ifelse( VAL_EXPOSE_FIN_CMPST_UNIQUE_SCORES_CNT >= 2.0, 
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100)            == TRUE, VAL_EXPOSE_FIN_CMPST_PCT_CH_DEBT_NTILE100,            0.0) +
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100)         == TRUE, VAL_EXPOSE_FIN_CMPST_DEBT_TO_EQUITY_NTILE100,         0.0) + 
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100)     == TRUE, VAL_EXPOSE_FIN_CMPST_EXTERNAL_FINANCING_NTILE100,     0.0) +
      ifelse( !is.na(VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100) == TRUE, VAL_EXPOSE_FIN_CMPST_ANN_CSH_FLOW_OVER_DEBT_NTILE100, 0.0) 
    , NA) 
  ))
  
  # four factors total possible
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL = as.numeric(
     VAL_EXPOSE_FIN_CMPST_SCORES_SUMM * 4.0 / VAL_EXPOSE_FIN_CMPST_UNIQUE_SCORES_CNT
  ))
  
  # higher value is BETTER 
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL) == FALSE)
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL,100)
  ))
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # end - financial composite - rebalance and scoring
  
  # end financial composite
  
  
  
  # begin earnings composite ( defensive posture )

  # ( If I were to use a 'single measure for the earnings composite ...')
  # March 2014 custom: Difference between "Operating Cash Flow" and "Net Income" 
  #                                   and scaled to "MarketCap"
  
  
  # begin joins
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)
  
  # strip off
  UNIVERSE <<- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
                                 , indexes = list(
                                   #    c("TICKER")
                                   # ,  
                                   c("COMPANY_ID")
                                   # ,  c("ORIG_ORDER")
                                   
                                 )
  )
  
  
  UNIVERSE <<- sqldf("SELECT UNIV.* 
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
                                  FROM 
                                   main.UNIVERSE UNIV, main.SI_BSQ BSQ, main.SI_CFQ CFQ, main.SI_ISQ ISQ WHERE 
                                   UNIV.COMPANY_ID = BSQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = CFQ.COMPANY_ID AND
                                   UNIV.COMPANY_ID = ISQ.COMPANY_ID 
                                   ", connection = dpsqllconn$con, method="name__class")
  
  UNIVERSE <<- tbl_df(UNIVERSE)

  # end joins
  
  
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
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA = as.numeric(   
    ( ( ( TCO_Q1 + TCO_Q2 + TCO_Q3 + TCO_Q4 ) - ( TCF_Q1 + TCF_Q2 + TCF_Q3 + TCF_Q4 )  - ( TCI_Q1 + TCI_Q2 + TCI_Q3 + TCI_Q4  ) ) - ( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  ) ) ) / abs( ( TCO_Q5 + TCO_Q6 + TCO_Q7 + TCO_Q8 ) - ( TCF_Q5 + TCF_Q6 + TCF_Q7 + TCF_Q8 )  - ( TCI_Q5 + TCI_Q6 + TCI_Q7 + TCI_Q8  ) + 0.0000001 ) * 100 
  ) )

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
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
  # (  ( ( WORK_Q1 - CASH_Q1 ) / ( LIAB_Q1 + 0.0000001 ) )  - ( ( WORK_Q5 - CASH_Q5 ) / ( LIAB_Q1 + 0.0000001 ) )  ) / abs( ( ( WORK_Q5 - CASH_Q5 + 0.0000001 ) /  ( LIAB_Q1 + 0.0000001 ) )  ) * 100.0  = VAL_EXPOSE_EARN_CMPST_TATA

  # lower value is better  (  uses % change in working capital ( BELOW ) )

  # current assets(accounts receivable) increases then 'assets' increase
  # cash                                increases then 'assets' increase                                   
  # then just 'assets' in the denominator
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_TATA = as.numeric(   
    (  ( ( WORK_Q1 - CASH_Q1 ) / ( ASSETS_Q1 + 0.0000001 ) )  - ( ( WORK_Q5 - CASH_Q5 ) / ( ASSETS_Q1 + 0.0000001 ) )  ) / abs( ( ( WORK_Q5 - CASH_Q5 + 0.0000001 ) /  ( ASSETS_Q1 + 0.0000001 ) )  ) * 100.0 
  ) )

  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_TATA) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_TATA_NTILE100 = as.numeric(
    ntile((-1)*VAL_EXPOSE_EARN_CMPST_TATA,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

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

  # ( ( AP_Q1 - NETINC_Q1 - TCO_Q1  - NETINC_Q2 - TCO_Q2 - NETINC_Q3 - TCO_Q3 - NETINC_Q4 - TCO_Q4 ) - ( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 ) ) / abs( AP_Q5 - NETINC_Q5 - TCO_Q5  - NETINC_Q6 - TCO_Q6 - NETINC_Q7 - TCO_Q7 - NETINC_Q8 - TCO_Q8 + 0.0000001 ) * 100  = VAL_EXPOSE_EARN_CMPST_CATA
  
  # lower is better

  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_CATA = as.numeric(   
    ( ( AP_Q1 - EPSCON_Q1 - TCO_Q1  - EPSCON_Q2 - TCO_Q2 - EPSCON_Q3 - TCO_Q3 - EPSCON_Q4 - TCO_Q4 ) - ( AP_Q5 - EPSCON_Q5 - TCO_Q5  - EPSCON_Q6 - TCO_Q6 - EPSCON_Q7 - TCO_Q7 - EPSCON_Q8 - TCO_Q8 ) ) / abs( AP_Q5 - EPSCON_Q5 - TCO_Q5  - EPSCON_Q6 - TCO_Q6 - EPSCON_Q7 - TCO_Q7 - EPSCON_Q8 - TCO_Q8 + 0.0000001 ) * 100 
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_CATA) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_CATA_NTILE100 = as.numeric(
    ntile((-1)*VAL_EXPOSE_EARN_CMPST_CATA,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  
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
  
  # higher value is better 

  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND = as.numeric(   
    ( DEP_Q1 + DEP_Q2 + DEP_Q3 + DEP_Q4 ) / ( CE_Q1 + CE_Q2 + CE_Q3 + CE_Q4 + 0.0000001 ) 
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # end - depreciation expense to captital expenditures
  
  
  # ( special_EARN of special_EARN )
  # begin - diff operating cash flow and net income and scales that figure to market cap
  # higher value is better 
  
  # March 2014: SIPro 4.0: Earnings Composite: One Single Easiest Way
  # Difference between Operating Cash Flow and Net Income and scales the figure to Market Cap

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

  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP = as.numeric(   
    ( ( TCO_Q1 - NETINC_Q1 ) + ( TCO_Q2 - NETINC_Q2 ) + ( TCO_Q3 - NETINC_Q3 ) + ( TCO_Q4 - NETINC_Q4 ) ) / MKTCAP
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # end - diff operating cash flow and net income and scales that figure to market cap
  
  
  
  # begin - earnings composite - rebalance and scoring
  # (USE) March 2014:  at least 2 of 4 factors (HOPEFULLY earnings composite case), then rebalance  ( technique seems fairer )
  # 2012 book: if NA, then assign 50 (of ntile 100 ) ( seems to unfairly punish companies for slow/non-exist reporting?)
  
  # count up of non-NA ntiles
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_UNIQUE_SCORES_CNT = as.numeric(
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100)             == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_TATA_NTILE100)                      == TRUE, 1.0, 0.0) + 
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_CATA_NTILE100)                      == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100) == TRUE, 1.0, 0.0)  
  ))
  
  # minimum two factors are required
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_SCORES_SUMM = as.numeric(
    ifelse( VAL_EXPOSE_EARN_CMPST_UNIQUE_SCORES_CNT >= 2.0, 
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100)             == TRUE, VAL_EXPOSE_EARN_CMPST_PCT_CH_IN_NOA_NTILE100,             0.0) +
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_TATA_NTILE100)                      == TRUE, VAL_EXPOSE_EARN_CMPST_TATA_NTILE100,                      0.0) + 
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_CATA_NTILE100)                      == TRUE, VAL_EXPOSE_EARN_CMPST_CATA_NTILE100,                      0.0) +
      ifelse( !is.na(VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100) == TRUE, VAL_EXPOSE_EARN_CMPST_DPRCT_EXPND_TO_CAPT_EXPND_NTILE100, 0.0) 
    , NA) 
  ))
  
  # four factors total possible
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL = as.numeric(
     VAL_EXPOSE_EARN_CMPST_SCORES_SUMM * 4.0 / VAL_EXPOSE_EARN_CMPST_UNIQUE_SCORES_CNT
  ))
  
  # higher value is BETTER 
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL) == FALSE)
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL,100)
  ))
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  

  # begin - IF DOING # ( special_EARN of special_EARN )
  #         THEN remove this area 'entire' comment out
  # March 2014: SIPro 4.0: Earnings Composite: One Single Easiest Way
  # Difference between Operating Cash Flow and Net Income and scales the figure to Market Cap
  
  # overwrite!! # note: above 'is.na' ... has already been done
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
    VAL_EXPOSE_EARN_CMPST_DIFF_OP_CSH_FLW_AND_NET_INC_SCALED_TO_MKTCAP_NTILE100
  ))
  
  # end - IF DOING # ( special_EARN of special_EARN )
  
  
  # end - earnings composite - rebalance and scoring

  
  # end earnings composite
  
  # begin value_two composite ( offensive posture ) 



  # Multiples
  SI_MLT <<- suppressWarnings(suppressMessages(read.dbf(file="N:/MyVMWareSharedFolder/Professional/Dbfs/si_mlt.dbf", as.is = TRUE)))
    primary_key_dup <- SI_MLT[duplicated(SI_MLT[,'COMPANY_ID']),,drop=FALSE]
    new_df_no_duplicates <- SI_MLT[!(SI_MLT$COMPANY_ID %in% as.matrix(primary_key_dup)),,drop=FALSE]
    SI_MLT <<- new_df_no_duplicates
    rm(primary_key_dup,new_df_no_duplicates)
  

  SI_MLT_tbl_sqlite <- copy_to(dpsqllconn, SI_MLT, temporary = FALSE
    , indexes = list(
        c("COMPANY_ID")
    )
  )
  
  SI_MLT <<- tbl_df(SI_MLT)


  # begin joins
  
  sqldf("DROP TABLE main.UNIVERSE", connection = dpsqllconn$con)
  
  # strip off
  UNIVERSE <<- as.data.frame(UNIVERSE)
  UNIVERSE_tbl_sqlite <- copy_to(dpsqllconn, UNIVERSE, temporary = FALSE
                                 , indexes = list(
                                   #    c("TICKER")
                                   # ,  
                                   c("COMPANY_ID")
                                   # ,  c("ORIG_ORDER")
                                   
                                 )
  )



  UNIVERSE <<- sqldf("SELECT UNIV.* 
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
  
  UNIVERSE <<- tbl_df(UNIVERSE)

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

  # higher is better 
  
  # ( EPSDC_Q1 + EPSDC_Q2 + EPSDC_Q3 + EPSDC_Q4  ) / PRICE = VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO = as.numeric(   
    ( EPSDC_Q1 + EPSDC_Q2 + EPSDC_Q3 + EPSDC_Q4  ) / PRICE 
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # end  - earnings / price ratio 
      
  # ( 2 of 5 )
  # begin - sales / price ratio ( price is always large and positive )
  # higher is better 
  
  #	FIELD_NAME  SALES_Q1
  #	FIELD_DESC  Sales Q1
  #	DESCRIP     Income Statement - Quarterly
  #	FM_FILE	SI_ISQ
  
  # higher is better 
  
  # ( SALES_Q1 + SALES_Q2 + SALES_Q3 + SALES_Q4  ) / PRICE = VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO = as.numeric(   
    ( SALES_Q1 + SALES_Q2 + SALES_Q3 + SALES_Q4  ) / PRICE 
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
      
  # end  - sales / price ratio 
      
  # ( 3 of 5 )
  # begin - 'free cash flow' / price ratio ( price is always large and positive )
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
  
  # higher is better 
  
  # ( FCFPS_Q1 + FCFPS_Q2 + FCFPS_Q3 + FCFPS_Q4  ) / PRICE = VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO = as.numeric(   
    ( FCFPS_Q1 + FCFPS_Q2 + FCFPS_Q3 + FCFPS_Q4  ) / PRICE 
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
  
  # end  - 'free cash flow' / price ratio 
      
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
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO = as.numeric(   
    (  EBIT_Q1 + EBIT_Q2 + EBIT_Q3 + EBIT_Q4 + ifelse(!is.na(DEP_CF_Q1) == TRUE, DEP_CF_Q1, 0.0 ) + ifelse(!is.na(DEP_CF_Q2) == TRUE, DEP_CF_Q2, 0.0 ) + ifelse(!is.na(DEP_CF_Q3) == TRUE, DEP_CF_Q3, 0.0 ) + ifelse(!is.na(DEP_CF_Q4) == TRUE, DEP_CF_Q4, 0.0 ) ) / ENTVAL_Q1 
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
      
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
      
  # FIELD_NAME  SHY
  # FIELD_DESC  Shareholder Yield
  # DESCRIP     Multiples
  # FM_FILE     SI_MLT ( primary key: COMPANY_ID )
      
  # SINCE (NON-QUARTER cyclical, I will use their AUTOMATIC )
      
  # higher is better 
  
  # SHY = VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD
  
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD = as.numeric(   
    SHY
  ) )
  
  UNIVERSE_NOT_NA <- group_by(UNIVERSE,MG_DESC) 
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD,100)
  ))
  
  UNIVERSE_NOT_NA <- ungroup(UNIVERSE_NOT_NA) 
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

  # end  - shareholder yield
      
                      
  # begin - value two composite - rebalance and scoring
  # (USE) March 2014:  at least 3 of 5 factors, then rebalance  ( technique seems fairer )
  # 2012 book: if NA, then assign 50 (of ntile 100 ) ( seems to unfairly punish companies for slow/non-exist reporting?)
  
  # count up of non-NA ntiles
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_UNIQUE_SCORES_CNT = as.numeric(
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100)    == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100)   == TRUE, 1.0, 0.0) + 
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO_NTILE100)   == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100) == TRUE, 1.0, 0.0) +
    ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100)              == TRUE, 1.0, 0.0)  
  ))
  
  # minimum two factors are required
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM = as.numeric(
    ifelse( VAL_EXPOSE_VAL_TWO_CMPST_UNIQUE_SCORES_CNT >= 3.0, 
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100)    == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_EARN_TO_PRICE_RATIO_NTILE100,    0.0) +
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100)   == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_SALES_TO_PRICE_RATIO_NTILE100,   0.0) + 
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO_NTILE100)   == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_FCFPS_TO_PRICE_RATIO_NTILE100,   0.0) +
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100) == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_EBITDA_TO_ENTVAL_RATIO_NTILE100, 0.0) +
      ifelse( !is.na(VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100)              == TRUE, VAL_EXPOSE_VAL_TWO_CMPST_SHY_YIELD_NTILE100,              0.0) 
    , NA) 
  ))
  
  # four factors total possible
  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL = as.numeric(
     VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM * 5.0 / VAL_EXPOSE_VAL_TWO_CMPST_UNIQUE_SCORES_CNT
  ))
                      
  # higher value is BETTER 
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL) == FALSE)
  
  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100 = as.numeric(
    ntile(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL,100)
  ))
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
                      
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
   
  # VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100 = VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM 

  UNIVERSE <<- mutate(UNIVERSE, VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM  = as.numeric(   
    VAL_EXPOSE_FIN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_EARN_CMPST_SCORES_SUMM_REBAL_NTILE100 + VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100
  ) )
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM ) == FALSE)

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2 = as.numeric(
    ntile(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM ,2)
  ))
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN

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
  #  rank(desc(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100), ties.method = "min" ) = VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN
  
  UNIVERSE_NOT_NA <- UNIVERSE
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2 )  == FALSE)

  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA,       VAL_EXPOSE_ALL_CMBND_CMPST_SCORES_SUMM_REBAL_NTILE100_SUMM_NTILE2    == 2     )
  
  UNIVERSE_NOT_NA <- filter(UNIVERSE_NOT_NA, is.na(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100 )                == FALSE) 

  UNIVERSE_NOT_NA <- mutate(UNIVERSE_NOT_NA,VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN = as.numeric(
    rank(desc(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL_NTILE100), ties.method = "min" ) 
  ))
  
  UNIVERSE <<- suppressMessages(left_join(UNIVERSE, UNIVERSE_NOT_NA)) # LEFT OUTER JOIN
 

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

          ) 
          

  UNIVERSE_FMA <- arrange( UNIVERSE_FM
  ,      VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN
  , desc(VAL_TWO_CMPST_SUMM_REBAL_NTILE100)
  , desc(VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL)
  , desc(ALL_CMBND_SUMM_NTILE2)
  , desc(ALL_CMBND_NTILE100_SUMM)
  )[,c( 
  "MONTHDATEDT", "WEEKDATEDT","SPLITDATEDT", 
  "TICKER","COMPANY","MG_DESC","MKTCAP","PSD_SPLITDT_DATE","SPLIT_FACT","PERENDDT_Q2", "PERLEN_Q1", "PERTYP_Q1", "PERENDDT_Q1"
  # ,"PERENDDT_Q0" ( NOT USEFUL )
  ,"VAL_TWO_CMPST_SUMM_REBAL_NTILE100", "VAL_EXPOSE_VAL_TWO_CMPST_SCORES_SUMM_REBAL", "ALL_CMBND_NTILE100_SUMM","ALL_CMBND_SUMM_NTILE2","VAL_EXPOSE_ALL_CMBND_CMPST_VAL_TWO_CMPST_SCORES_TOPN")
    ]
        
        
  return(UNIVERSE_FMA)

  # end of feb,may,aug,nov :buy 4 stocks
  #        other months    :buy 2 stocks
  # maybe a good idea to only invest in sectors with a 10 month Mebane Faber 'uptrend'
  # darn good idea to BAIL when FED/PIGER/other 3-month recession indicators flair UP
  
  # end conclusion
  
}


# TO_DO ( NOTE DONE YET ) : END OF DAY. "Put Up on GitHub" [ ] August 13, 2014


# NEXT ( MAKE SURE PRICES AND SPLITS ARE HOPEFULLY SMOOTH )


# CLEAN SOME CODE UP ( SIMPLIFY SOME 'REPEATS T FUNCTIONS' )
# INTEGRATE VJAYS ( CENTRALIZED DIRECTORY FUNCTION )
    
    
# START LOOKING AT (TO START) FED & PIGER & RANDOMFOREST
    
# NEXT, REVIEW THAT "YELLOW FOLDER" CLEAN_UP/CENTRALIZE 'SOME' REPEATING CODE
 
  # NOTE: View(cbind(UNIVERSE[,"ASSETS_Q1"]-UNIVERSE[,"LIAB_Q1"],UNIVERSE[,"EQUITY_Q1"]))
  #       ASSETS_Q1 & LIAB_Q1 are always large and positive
  #       ASSETS_Q1 - LIAB_Q1 = EQUITY_Q1(sometimes negative)

# TOGGLE-ABLE
# main_foresight3_999()
# 
# rm(list=ls(all.names=TRUE))
# source('N:/MyVMWareSharedFolder/foresight3/R/main-foresight3-999.R', echo=TRUE)
# colnames(UNIVERSE)
# View(UNIVERSE[,98:length(UNIVERSE)])
# View(main_foresight3_999())
# as.matrix(UNIVERSE[558:569,172:173])
# View(UNIVERSE[UNIVERSE$MG_DESC == "Financial",c(8:8,88:92,104:105)])
