
# RECENT HELP FROM
# N:\FUND_DATA_FIXES_2\scratchFaberRecessionINDQuantstrat.txt


# install.packages("PerformanceAnalytics")

# install.packages("iterators")
# install.packages("foreach")

# install.packages("zoo")
# install.packages("xts")
# install.packages("quantmod")

# install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org", dep = TRUE)
# install.packages("blotter", repos="http://R-Forge.R-project.org", dep = TRUE)

# install.packages("xtsExtra", repos="http://R-Forge.R-project.org", dep = TRUE)

# install.packages("PortfolioAnalytics", repos="http://R-Forge.R-project.org", dep = TRUE)
# install.packages("rgl", repos="http://R-Forge.R-project.org", dep = TRUE)
# install.packages("quantstrat", repos="http://R-Forge.R-project.org", dep = TRUE)

# install.packages("pander")
# install.packages("shiny")
# install.packages("qmao",repos = "http://R-Forge.R-project.org")

# tO be useful I need the source ( if I am debugging within a PACKAGE )
# options(error = recover)
# options(error = NULL)
# options(error = dump.frames)
# NEXT time I get an error without a line # ( maybe? this can help )
# options(error = browser)

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width
## options(sqldf.driver = "PostgreSQL") ### OLD SQLite"

# options(TSrepresentation="xts")

# CAN NOT GET TO WORK
# options(TSvintage=TRUE)
# options(TSpanel=TRUE)
# TSreplace(x=GSPC, serIDs="GSPCSeries1", con = contspgr, panel="GSPCPanel1",TSdescription="S and P", TSdoc="S and P 500 from Yahoo")
# Error: object 'rP' not found

# options(TSconnection=conn)

# library(Hmisc)

# library(testthat)

# library(tcltk)
# library(Rgraphviz)

# library(sqldf)

# library(TimeWarp)
# library(lubridate)

# library(plyr)
# library(dplyr)
# library(data.table)

# library(foreign)
# options("getSymbols.warning4.0"=FALSE) # LATER ( WITH quantstrat )
# library(quantstrat)
# library(TimeWarp)
# library(qmao)  # alignSymbols  # Deletes rows that not all Symbols have in common

# library(qmao)
# library(xtsExtra)

# library(randomForest)

# library(TSdbi)
# library(TSsql)
# library(TShistQuote)
# library(TSgetSymbol)
# library(RSQLite) # loads DBI
# library(RPostgreSQL)
# library(TSSQLite)
# library(TSPostgreSQL)
# library(tframe)   
# library(tframePlus)
# library(TScompare)
# library(tfplot)


setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R")

# LATER
# source( paste0(getwd(),"/helpers-rcsnsight1-999.R"))

# IN YELLOW window2 only
# library(testthat)
# auto_test("./R", "./tests/testthat")  



symbol_OHLC_lag_k_periods <- function(symbol,new_symbol_name, llagk = c(1), qmOHLC = "Cl") {  
   require(quantmod)
   if (is.null(symbol))          stop("symbol_OHLC_lag_k_periods is missing symbol") 
   if (is.null(new_symbol_name)) stop("symbol_OHLC_lag_k_periods is missing new_symbol_name") 
    # Lag(Cl(eval(parse(text=symbol))[,c(paste0(symbol,".",OHLCi))]),k=llagk) -> L; paste0(symbol,".",OHLCi,".","Lag",".",c(llagk)) -> colnames(L); L 
      if ( !any(qmOHLC %in% c("Cl","Lowz"))) stop ( "symbol_OHLC_lag_k_periods qmOHLC should be Cl or Lowz")
      if ( qmOHLC == "Lowz") {
        Lag(Lo(symbol),k=llagk) -> L; paste0(new_symbol_name,".MinOf.Lag.",c(llagk)) -> colnames(L)
      } 
      if ( qmOHLC == "Cl") { 
        Lag(Cl(symbol),k=llagk) -> L; paste0(new_symbol_name,".EndOf.Lag.",c(llagk)) -> colnames(L)
      }
      L
}
# KEEP - testing
# first( symbol_OHLC_lag_k_periods(IBM , "IBM"    )  , 6 )
# first( symbol_OHLC_lag_k_periods(GSPC,"GSPC",1:2)  , 6 )



symbol_OHLC_range_k_periods <- function(symbol,new_symbol_name, llagk = c(1), qmOHLC, llagk_zone_back = 0, findthat, findthat_name, findthat_range_name ) {  
  require(quantmod)
  if (is.null(symbol))          stop("symbol_OHLC_range_k_periods is missing symbol") 
  if (is.null(new_symbol_name)) stop("symbol_OHLC_range_k_periods is missing new_symbol_name") 
  # if ( !any(qmOHLC %in% c("Cl","Lowz"))) stop ( "symbol_OHLC_range_k_periods qmOHLC should be Cl or Lowz")
  if( length(llagk) > 0 ) {
    for( i in 1:length(llagk) )  {
      # only one column is retured: the Lowz across lags
      # NOTE: if llagk is NOT ( a 'one sep' sequence of integers') then the 'naming of the column would break
      apply(Lag(eval(parse(text=paste0(qmOHLC,"(symbol)"))),(llagk[i]):(llagk[i]+llagk_zone_back)), MARGIN = 1, eval(parse(text=findthat)) ) -> L ; xts( L, as.Date(names(L), tz = "UTC") )  -> L ; paste0(new_symbol_name,".",findthat_name,".Lag.",(llagk[i]),".",findthat_range_name,".", (llagk[i]),".",(llagk[i]+llagk_zone_back) )  -> colnames(L)
      if ( i == 1 ) {
        L -> BULK_L
      } else {
        suppressWarnings(mergge(BULK_L, L, join='outer',tz='UTC')) -> BULK_L
      }
    }
  }
  return(BULK_L)
}



max_drawdown_pct_chnge <- function(xtsdata, col_0 = "", col_1 = "") {
  # subset of data
  xtsdata[,c(col_0,col_1)] -> xtsdata_subset
  # calculate the metric    
  (xtsdata_subset[,col_0] - xtsdata_subset[,col_1] ) / abs(xtsdata_subset[,col_1] + 0.0000001) -> xts_data_interest_new
  # rename the new column - grab those numbers at the end   
  ( last(gregexpr("\\.",col_0)[[1]],2) + 1 )[1]      -> col_0_extract_start_pos
  ( last(gregexpr("\\.",col_0)[[1]],2) + 0 )[2] - 1  -> col_0_extract_stop_pos
  substr(col_0,start=col_0_extract_start_pos, stop=col_0_extract_stop_pos) -> col_0_nbr_suffix
  # rename the new column - grab those numbers at the end   
  ( last(gregexpr("\\.",col_1)[[1]]) + 1 ) -> col_1_extract_start_pos
  nchar(col_1)                              -> col_1_extract_stop_pos
  substr(col_1,start=col_1_extract_start_pos, stop=col_1_extract_stop_pos) -> col_1_nbr_suffix
  # remove the ticker from col_1 ( since it is already in col_0 )
  col_1 -> col_1_new_base
  ( first(gregexpr("\\.",col_1_new_base)[[1]]) + 1 )            -> col_1_new_base_extract_start_pos
  nchar(col_1_new_base)                                          -> col_1_new_base_extract_stop_pos
  substr(col_1_new_base,start=col_1_new_base_extract_start_pos, stop=col_1_new_base_extract_stop_pos) -> col_1_new_base
  # actual new column name
  paste0(col_0,".",col_1_new_base,".PctChnge.",col_0_nbr_suffix,".",col_1_nbr_suffix) -> colnames(xts_data_interest_new)
  return(xts_data_interest_new) 
}


pct_chnge <- function(xtsdata, col_0 = "", col_1 = "") {
  
  # subset of data
  xtsdata[,c(col_0,col_1)] -> xtsdata_subset
  # calculate the metric    
  (xtsdata_subset[,col_0] - xtsdata_subset[,col_1] ) / abs(xtsdata_subset[,col_1] + 0.0000001) -> xts_data_interest_new
  # rename the new column - grab those numbers at the end   
  ( last(gregexpr("\\.",col_0)[[1]]) + 1 ) -> col_0_extract_start_pos
  nchar(col_0)                              -> col_0_extract_stop_pos
  substr(col_0,start=col_0_extract_start_pos, stop=col_0_extract_stop_pos) -> col_0_nbr_suffix
  # rename the new column - grab those numbers at the end   
  ( last(gregexpr("\\.",col_1)[[1]]) + 1 ) -> col_1_extract_start_pos
  nchar(col_1)                              -> col_1_extract_stop_pos
  substr(col_1,start=col_1_extract_start_pos, stop=col_1_extract_stop_pos) -> col_1_nbr_suffix
  # actual new column name
  paste0(col_0,".PctChnge.",col_0_nbr_suffix,".",col_1_nbr_suffix) -> colnames(xts_data_interest_new)
  return(xts_data_interest_new) 
 
}

  # # how I would call it

  # pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.0","RECPROUSM156N.EndOf.Lag.1") -> GALAXY_L_CHNGES 
  # # ...: one or more xts objects, or objects coercible to class xts
  # suppressWarnings(merge(GALAXY_L,GALAXY_L_CHNGES,join='outer',tz='UTC')) -> GALAXY_L

# # OLD
# pct_chnge_pct_chnge <- function(xtsdata, col_01 = "", col_12 = "") {
  
  # # subset of data
  # xtsdata[,c(col_01,col_12)] -> xtsdata_subset
  # # calculate the metric    
  # (xtsdata_subset[,col_01] - xtsdata_subset[,col_12] ) / abs(xtsdata_subset[,col_12] + 0.0000001) -> xts_data_interest_new
  # # rename the new column - grab those numbers at the end   
  # ( first(last(gregexpr("\\.",col_01)[[1]],2)) + 1 ) -> col_01_extract_start_pos
  # nchar(col_01)                              -> col_01_extract_stop_pos
  # substr(col_01,start=col_01_extract_start_pos, stop=col_01_extract_stop_pos) -> col_01_nbr_suffix
  # # rename the new column - grab those numbers at the end   
  # ( first(last(gregexpr("\\.",col_12)[[1]],2)) + 1 ) -> col_12_extract_start_pos
  # nchar(col_12)                              -> col_12_extract_stop_pos
  # substr(col_12,start=col_12_extract_start_pos, stop=col_12_extract_stop_pos) -> col_12_nbr_suffix
  # # actual new column name
  # paste0(col_01,".PctChngePctChnge.",col_01_nbr_suffix,".",col_12_nbr_suffix) -> colnames(xts_data_interest_new)
  # return(xts_data_interest_new) 
  
# }


pct_chnge_pct_chnge <- function(xtsdata, col_0 = "", col_1 = "") {
  # subset of data
  xtsdata[,c(col_0,col_1)] -> xtsdata_subset
  # calculate the metric    
  (xtsdata_subset[,col_0] - xtsdata_subset[,col_1] ) / abs(xtsdata_subset[,col_1] + 0.0000001) -> xts_data_interest_new
  # find the number of soltutions
  n_matches <- length(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]])
  # get the left argument: from its end; the second number position
  fst_end_number_pos <- gregexpr("\\.[0-9]?[0-9]",col_0)[[1]][(n_matches-1)] 
  # get its range including the period
  fst_end_number_pos_lngth <- attr(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]],"match.length")[(n_matches-1)] 
  # get the right argument: from its end; the first number position
  snd_end_number_pos <- gregexpr("\\.[0-9]?[0-9]",col_1)[[1]][(n_matches  )] 
  # get its range including the period
  snd_end_number_pos_lngth <- attr(gregexpr("\\.[0-9]?[0-9]",col_1)[[1]],"match.length")[(n_matches  )]  
  # get the left arg: actual number; mo
  fst_end_number <- substr(col_0, fst_end_number_pos + 1 , fst_end_number_pos + 1 + fst_end_number_pos_lngth - 2) 
  # get the right arg: actual number; mo
  snd_end_number <- substr(col_1, snd_end_number_pos + 1 , snd_end_number_pos + 1 + snd_end_number_pos_lngth - 2) 
  # get the 'base name': get the beginnin of the col_0 name up to the end of the first number
  fst_begin_number <- gregexpr("\\.[0-9]?[0-9]",col_0)[[1]][(1)] - 1  + attr(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]],"match.length")[(1)] 
  # the actual 'base name'
  fst_begin <- substr(col_0, 1, fst_begin_number )
  # the actual new full column name ( including the base name )
  col_new <- paste0(fst_begin,".PctChngePctChnge.", fst_end_number, ".", snd_end_number)
  # give the new data column a new string name
  paste0(col_new) -> colnames(xts_data_interest_new)
  # return that new data column ( with its new name )
  return(xts_data_interest_new) 
}


  # # how I would call it
  # pct_chnge_pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.0.PctChnge.0.1","RECPROUSM156N.EndOf.Lag.1.PctChnge.1.2") -> GALAXY_L_CHNGES 

pct_chnge_pct_chnge_pct_chnge <- function(xtsdata, col_0 = "", col_1 = "") {
  # subset of data
  xtsdata[,c(col_0,col_1)] -> xtsdata_subset
  # calculate the metric    
  (xtsdata_subset[,col_0] - xtsdata_subset[,col_1] ) / abs(xtsdata_subset[,col_1] + 0.0000001) -> xts_data_interest_new
  # find the number of soltutions
  n_matches <- length(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]])
  # get the left argument: from its end; the second number position
  fst_end_number_pos <- gregexpr("\\.[0-9]?[0-9]",col_0)[[1]][(n_matches-1)] 
  # get its range including the period
  fst_end_number_pos_lngth <- attr(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]],"match.length")[(n_matches-1)] 
  # get the right argument: from its end; the first number position
  snd_end_number_pos <- gregexpr("\\.[0-9]?[0-9]",col_1)[[1]][(n_matches  )] 
  # get its range including the period
  snd_end_number_pos_lngth <- attr(gregexpr("\\.[0-9]?[0-9]",col_1)[[1]],"match.length")[(n_matches  )]  
  # get the left arg: actual number; mo
  fst_end_number <- substr(col_0, fst_end_number_pos + 1 , fst_end_number_pos + 1 + fst_end_number_pos_lngth - 2) 
  # get the right arg: actual number; mo
  snd_end_number <- substr(col_1, snd_end_number_pos + 1 , snd_end_number_pos + 1 + snd_end_number_pos_lngth - 2) 
  # get the 'base name': get the beginnin of the col_0 name up to the end of the first number
  fst_begin_number <- gregexpr("\\.[0-9]?[0-9]",col_0)[[1]][(1)] - 1  + attr(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]],"match.length")[(1)] 
  # the actual 'base name'
  fst_begin <- substr(col_0, 1, fst_begin_number )
  # the actual new full column name ( including the base name )
  col_new <- paste0(fst_begin,".PctChngePctChngePctChnge.", fst_end_number, ".", snd_end_number)
  # give the new data column a new string name
  paste0(col_new) -> colnames(xts_data_interest_new)
  # return that new data column ( with its new name )
  return(xts_data_interest_new) 
}



pct_chnge_pct_chnge_pct_chnge_pct_chnge <- function(xtsdata, col_0 = "", col_1 = "") {
  # subset of data
  xtsdata[,c(col_0,col_1)] -> xtsdata_subset
  # calculate the metric    
  (xtsdata_subset[,col_0] - xtsdata_subset[,col_1] ) / abs(xtsdata_subset[,col_1] + 0.0000001) -> xts_data_interest_new
  # find the number of soltutions
  n_matches <- length(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]])
  # get the left argument: from its end; the second number position
  fst_end_number_pos <- gregexpr("\\.[0-9]?[0-9]",col_0)[[1]][(n_matches-1)] 
  # get its range including the period
  fst_end_number_pos_lngth <- attr(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]],"match.length")[(n_matches-1)] 
  # get the right argument: from its end; the first number position
  snd_end_number_pos <- gregexpr("\\.[0-9]?[0-9]",col_1)[[1]][(n_matches  )] 
  # get its range including the period
  snd_end_number_pos_lngth <- attr(gregexpr("\\.[0-9]?[0-9]",col_1)[[1]],"match.length")[(n_matches  )]  
  # get the left arg: actual number; mo
  fst_end_number <- substr(col_0, fst_end_number_pos + 1 , fst_end_number_pos + 1 + fst_end_number_pos_lngth - 2) 
  # get the right arg: actual number; mo
  snd_end_number <- substr(col_1, snd_end_number_pos + 1 , snd_end_number_pos + 1 + snd_end_number_pos_lngth - 2) 
  # get the 'base name': get the beginnin of the col_0 name up to the end of the first number
  fst_begin_number <- gregexpr("\\.[0-9]?[0-9]",col_0)[[1]][(1)] - 1  + attr(gregexpr("\\.[0-9]?[0-9]",col_0)[[1]],"match.length")[(1)] 
  # the actual 'base name'
  fst_begin <- substr(col_0, 1, fst_begin_number )
  # the actual new full column name ( including the base name )
  col_new <- paste0(fst_begin,".PctChngePctChngePctChngePctChnge.", fst_end_number, ".", snd_end_number)
  # give the new data column a new string name
  paste0(col_new) -> colnames(xts_data_interest_new)
  # return that new data column ( with its new name )
  return(xts_data_interest_new) 
}


# get rid of that "tz" column
# must 'not be lexical' ( I THINK? NO - CAN BE LEXICAL. MAYBE )
mergge <- function(..., all = TRUE, fill = NA, suffixes = NULL, join = "outer",
                          retside = TRUE, retclass = "xts", tzone = NULL, drop = NULL,
                          check.names = NULL) {

  require(xts)
 
  merge(..., all = all, fill = fill, suffixes = suffixes, join = join,
                            retside = retside, retclass = retclass, tzone = tzone, drop = drop,
                            check.names = check.names) -> merge_result
                            
  merge_result[,!grepl("^tz$",colnames(merge_result))] -> merge_result
  
                            
  return(merge_result)
}



main_rcsnsight1_999 <- function(THESEED = 1) {

  # require(Hmisc)  # NOT USED YET ( BUT WILL BE SOON )

  # require(testthat) # NOT USED HERE

  # require(tcltk)     # dyanically called by gsubfn
  # require(Rgraphviz) # NOT 'really' NEEDED for gsubfn

  require(sqldf)

  # require(lubridate) # NOT
  require(Holidays)
  require(TimeWarp)

  # require(plyr) # NOT USED
  
  require(data.table) # dplyr may/not? dynamically call this?
  require(dplyr)

  # require(foreign)

  # # DECIDED - to have my columns to be named 'EndOf' 
  # # fix quantmod::has.Cl so it only looks for the STOCK.Close ( not my appended .Lag. columns )
  # assignInNamespace(x="has.Cl", value= 
    # function (x, which = FALSE) {
      # colAttr <- attr(x, "Cl")
      # if (!is.null(colAttr))
        # return(if (which) colAttr else TRUE)
      # loc <- grep(".*\\.Close$", colnames(x), ignore.case = TRUE) # ORIGINAL was just "Close"
      # if (!identical(loc, integer(0))) {
        # return(if (which) loc else TRUE)
      # }
        # else FALSE
      # }
  # , ns=asNamespace("quantmod") )
  
  # required quantmod will now load
  require(quantstrat)  # calls required functions ( IN USE ) ???
  
  require(qmao) # get information from briefing.com


  # require(qmao)  # alignSymbols  # Deletes rows that not all Symbols have in common

  # require(qmao)      # NOT USED YET ( IF EVER )
  # require(xtsExtra)  # NOT USED YET ( IF EVER 0

  # require(randomForest)  ## NOT USED YET, BUT WILL BE SOON

  # require(TSdbi)
  # require(TSsql)
  # require(TShistQuote)
  # require(TSgetSymbol)
  #   require(RSQLite)     # loads DBI # dyamically called by sqldf
  #   require(RPostgreSQL) # loads DBI # dyamically called by sqldf
  # require(TSSQLite)
  # require(TSPostgreSQL)
  # require(tframe)   
  # require(tframePlus)
  # require(TScompare)
  # require(tfplot)
  
  require(caret)
  
  ## ended up not using
  ## require(unbalanced)
  
  setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R")


  # FROM MY EMAIL AND 'WORK OUTLOOK'
    
  # --------- VERY IMPORTANT ( EVERY 2 WEEKS ) -----------

   
  # FRB: Press Release - Monetary Policy

  # http://www.federalreserve.gov/feeds/press_monetary.xml

  # FRB: Chair Yellen: Speeches and Testimony

  # http://www.federalreserve.gov/feeds/s_t_yellen.xml

  # --

  # Chauvet/Piger

  # http://research.stlouisfed.org/fred2/data/RECPROUSM156N.txt

  # Smoothed U.S. Recession Probabilities

  # Monthly, Not Seasonally Adjusted, RECPROUSM156N, Updated: 2014-06-02 10:01 AM CDT

  # Source: Marcelle Chauvet and Jeremy Piger

  # http://research.stlouisfed.org/fred2/series/RECPROUSM156N

  # --

  # NBER

  # http://research.stlouisfed.org/fred2/data/USRECM.txt

  # (Midpoint ?)

  # NBER based Recession Indicators for the United States from the Peak through the Trough ( 2ND MOST POP )

  # Monthly, Not Seasonally Adjusted, USRECM

  # http://research.stlouisfed.org/fred2/series/USRECM

   
  # http://research.stlouisfed.org/fred2/data/USREC.txt

  # NBER based Recession Indicators for the United States from the Period following the Peak through the Trough ( MOST POP )

  # Monthly, Not Seasonally Adjusted, USREC

  # http://research.stlouisfed.org/fred2/series/USREC


  # http://research.stlouisfed.org/fred2/data/USRECP.txt

  # NBER based Recession Indicators for the United States from the Peak through the Period preceding the Trough

  # Monthly, Not Seasonally Adjusted, USRECP

  # http://research.stlouisfed.org/fred2/series/USRECP

  # ---

  # Sornette

  # FINANCIAL CRISIS OBSERVATOR - Market Bubble Watch Overview

  # Choose

  # Indicators -> Early Bubble Warning Short Time Scale

  # Settings -> 3 Months

  # http://risikopedia.ethz.ch:2375/

  # ---

  # OECD

  # http://research.stlouisfed.org/fred2/data/USARECM.txt

  # OECD based Recession Indicators for the United States from the Peak through the Trough

  # Monthly, Not Seasonally Adjusted, USARECM

  # http://research.stlouisfed.org/fred2/series/USARECM

   
  # http://research.stlouisfed.org/fred2/data/USAREC.txt

  # OECD based Recession Indicators for the United States from the Period following the Peak through the Trough

  # Monthly, Not Seasonally Adjusted, USAREC

  # http://research.stlouisfed.org/fred2/series/USAREC


  # http://research.stlouisfed.org/fred2/data/USARECP.txt

  # OECD based Recession Indicators for the United States from the Peak through the Period preceding the Trough

  # Monthly, Not Seasonally Adjusted, USARECP

  # http://research.stlouisfed.org/fred2/series/USARECP
    
  # IF NOT ALREAY THERE

  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  # drvsqll <- dbDriver("SQLite")

  # # LATER: STORE IN A REAL FILE ) # sqlltsdbname <- "(R_PORTABLE)"
  # sqlltsdbname   <-  tempfile()

  # condbsqll <- dbConnect(drvsqll, 
    # dbname = sqlltsdbname #,
    # # max.con  =  999, 
    # # force.reload=TRUE
  # )
  
  # removeTSdbTables(condbsqll, yesIknowWhatIamDoing=TRUE)
  # createTSdbTables(condbsqll, index=TRUE)

  # need installed TS database ( will not be doing that here )
  # contssqll <- TSconnect(drvsqll, 
       # dbname = sqlltsdbname #,
       # # max.con  =  999, 
       # # force.reload=TRUE
   # )

  # drvpgr <- dbDriver("PostgreSQL")

  # condbpgr <- dbConnect(drvpgr, 
       # dbname   = "ets", 
       # user     = "Administrator", 
       # password = "postgre", 
       # host     = "localhost",
       # port     = 5432 #,
       # # max.con  =  999 , 
       # # force.reload=TRUE
  # ) 

  # begin connection
  dpsqllconn <- src_sqlite(":memory:", create = T)
  
  # contspgr <- TSconnect(drvpgr, 
       # dbname   = "ets", 
       # user     = "Administrator", 
       # password = "postgre", 
       # host     = "localhost",
       # port     = 5432 #,
       # # max.con  =  999, 
       # # force.reload=TRUE
  # ) 
  
  # PostgreSQL specific error - I do not get this in SQLite
  # z <- ts(rnorm(10), start=c(1990,1), frequency=1)
  # seriesNames(z) <- "vec" 
  # Browse[2]> TSexists("vec", contspgr)
    # [1] FALSE
  # Browse[2]> TSdelete("vec", contspgr)
    # Error in realPanel(con, panel) : panel must be specified
  # Browse[2]> TSput( z, contspgr)
    # Error in realPanel(con, panel) : panel must be specified
  
  #   dppgrconn <- src_postgres(
  #        dbname   = "ets"
  #     ,  user     = "Administrator"
  #     ,  password = "postgre"
  #     ,  host     = "localhost"
  #     ,  port     = 5432 
  #   )
  
  
  # MINE DIFFERENT ( HARD CODED : earliest of USRECM )
  initDate='1950-03-01' # some minor testing 1980 
  
  initDateSafelyBackOneMo <- as.character(dateWarp(as.Date(initDate), -1, by='months'))

  # SURE 100 thousand
  # initEq=100000

  #  HIGHLY DEEPLY CONSIDER ( FOR MY TESTS 'THE BAD 2008-09' RECESSION )
  #  use test dates
  #  initDate="2007-01-01" 
  #  endDate ="2010-12-31" 

  # FOR NOW: pretend each is a stock : TICKER.Close
  # ( to be comptible? with quantstrat *model* functions 
  # AND it's call to randomForest )
  
  # S & P 500 ( 0 month lag )                   SUBTRACT OFF ZERO DAY [x]
  
  if( file.exists(paste0(getwd(),"/GSPC.Rdata"))){
    load(file = paste0(getwd(),"/GSPC.Rdata"))
  } else {
    # by luck always the last calendar day of the month
    suppressWarnings(suppressMessages(getSymbols("^GSPC",from=initDateSafelyBackOneMo,src='yahoo', index.class=c("POSIXt","POSIXct"))))
    symbols <- c("GSPC")
    # register to be like a stock
    currency("USD")
    stock(symbols[1], currency="USD",multiplier=1)  # FinancialInstrument ( NOT REALLY A STOCK )
    symbol <- symbols
    x <- get(symbol)
    index(x) <- dateWarp(date=index(x),spec=0,by="days") # subtract off zero days
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE) # faber.R
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x) <- '%Y-%m-%d'
    # benchmark specific: I actually want the low ( different from non-benchmarks )
    x <- x[,c("x.Low","x.Close")] # remove quantmod o/h
    x[,"x.Low"]   <- as.numeric(x[,"x.Low"])   # garantee numeric
    x[,"x.Close"] <- as.numeric(x[,"x.Close"]) # garantee numeric
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
    save("GSPC",file = paste0(getwd(),"/GSPC.Rdata"))
    rm("x","symbols","symbol")
  }
  # GSPC.Low
  # GSPC.Close
  
  L_GSPC <- cbind( 1:NROW(GSPC),rev(1:NROW(GSPC)) ,as.numeric(index(GSPC)), coredata(GSPC))
  colnames(L_GSPC)[c(1,2,3)] <- c("NROW_IDX","REV_NROW_IDX","UNXDTE") # SHOULD KEEP JUST ONE!!
  colnames(L_GSPC)<-gsub("\\.","_",colnames(L_GSPC))
  L_GSPC <- tbl_dt( L_GSPC )
  # str(L_GSPC)
  # rowse[2]> str(L_GSPC)
  # Classes â€˜tbl_dtâ€™, â€˜tblâ€™, â€˜data.tableâ€™ and 'data.frame':	775 obs. of  4 variables:
   # $ NROW_IDX    : num  1 2 3 4 5 6 7 8 9 10 ...
   # $ REV_NROW_IDX: num  775 774 773 772 771 770 769 768 767 766 ...
   # $ UNXDTE      : num  -7216 -7186 -7155 -7125 -7094 ...
   # $ GSPC_Close  : num  17.3 18 18.8 17.7 17.8 ...
   # - attr(*, ".internal.selfref")=<externalptr> 

  # head(L_GSPC,2)
    # NROW_IDX            REV_NROW_IDX                   UNXDTE              GSPC_Close
  # 1        1 775.0000000000000000000 -7216.000000000000000000 17.28999999999999914735
  # 2        2 774.0000000000000000000 -7186.000000000000000000 17.96000000000000085265

  # sqldf - works - 
  # because I am not selecting/ins/upt/del from/on the table
  #   suppressWarnings(suppressMessages(sqldf('DROP TABLE IF EXISTS "GSPC"', connection = dppgrconn$con)))
  # 
  #   GSPC_tbl_postgres <- copy_to(dppgrconn, df = L_GSPC , name = "GSPC", temporary = FALSE
  #     , indexes = list(
  #           "NROW_IDX"
  #         , "REV_NROW_IDX"
  #         , "UNXDTE"
  #       )
  #   )

  # May? have to put a slight Pause here before the query?

  # sqldf - works - everytime
  # str(sqldf('SELECT * FROM "GSPC"', connection = dppgrconn$con))
  # Browse[2]> str(sqldf('SELECT * FROM "GSPC"', connection = dppgrconn$con))
  # 'data.frame':	775 obs. of  4 variables:
   # $ NROW_IDX    : num  1 2 3 4 5 6 7 8 9 10 ...
   # $ REV_NROW_IDX: num  775 774 773 772 771 770 769 768 767 766 ...
   # $ UNXDTE      : num  -7216 -7186 -7155 -7125 -7094 ...
   # $ GSPC_Close  : num  17.3 18 18.8 17.7 17.8 ...
  
  
  # NBER ( x3x 4 month lag )  VERIFIED NEW!  USRECMFIX : FIX : SUBTRACT OFF ONE DAY [x]
  
  if( file.exists(paste0(getwd(),"/USRECM.Rdata"))){
    load(file = paste0(getwd(),"/USRECM.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("USRECM",from=initDateSafelyBackOneMo, src = "FRED", index.class=c("POSIXt","POSIXct") )))
    # USRECM <- USRECM[paste0(substr(initDate,1,7),'::')]
    symbols <- c("USRECM")
    symbol <- symbols
    x <- get(symbol)
    index(x) <- dateWarp(date=index(x),spec=-1,by="days") # subtract off one day
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,"x.Close"] # remove quantmod o/h/l 
    x[,"x.Close"] <- as.numeric(x[,"x.Close"]) # garantee numeric
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
    save("USRECM",file = paste0(getwd(),"/USRECM.Rdata"))
    rm("x","symbols","symbol")
  }
  # USRECM.Close
  
  # Chauvet/Piger ( 3 month lag )    RECPROUSM156N: FIX : SUBTRACT OFF ONE DAY [x]
 
   if( file.exists(paste0(getwd(),"/RECPROUSM156N.Rdata"))){
    load(file = paste0(getwd(),"/RECPROUSM156N.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("RECPROUSM156N",from=initDateSafelyBackOneMo, src = "FRED", index.class=c("POSIXt","POSIXct") )))
    # RECPROUSM156N <- RECPROUSM156N[paste0(substr(initDate,1,7),'::')]
    symbols <- c("RECPROUSM156N")
    symbol <- symbols
    x <- get(symbol)
    index(x) <- dateWarp(date=index(x),spec=-1,by="days") # subtract off one day
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,"x.Close"] # remove quantmod o/h/l 
    x[,"x.Close"] <- as.numeric(x[,"x.Close"]) # garantee numeric
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
    save("RECPROUSM156N",file = paste0(getwd(),"/RECPROUSM156N.Rdata"))
    rm("x","symbols","symbol")
  }
  # RECPROUSM156N.Close
  
  # Applied Mutlivariate Statical Analysis 6th ( Blue Book ) Johnson and Wichern p. 635
  # logit - DUMPED - 'postive 0 to 100 is just the prob itself;  ( 0 TO 100 ) USE hdquantile TO HELP
  
  # NEXT: SAVE ON GITHUB [x] ( FORGOT LAST TIME)     ( FRI NIGHT PLAY )
  #        DEEPLY CONSIDER ( DATA MINING BOOK ... use ? Next, 'Next(Next(',Lag, Delt, Cl put into quantmod::model* function
  #          quantmod: Lag, Next( and cbind ), and Delt
  # NEXT: PUT 'REST OF 3 SYMBOLS UP ON POSTGRE [ ]
  #   SQL 'NATURAL JOIN' THEM [ ]
  #     works(tested): SELECT * FROM "GSPC" NATURAL JOIN "GSPC2"
  
  # *** TO START **** 
  # Database-ize: https://r-forge.r-project.org/scm/viewvc.php/pkg/TSdata/vignettes/Guide.Stex?view=markup&root=tsdbi ??? ]
  # [x] TRIED library(TSdbi) and library(TSsql) # POSTGRE specific failures 
  # LATER [ ] quandmod getSymbols re-try style tryCatch
  # SOON ABOVE [ ] need a logit() 'green book'to convert 'recession probabilites in to better numbers
  #                need new_var/all_variables 'green book' method
  
  # RECENT HELP ( some integration ) FROM
  # N:\FUND_DATA_FIXES_2\scratchFaberRecessionINDQuantstrat.txt
  # *** MAIN DEAL **** ( NEED 'LOGICALLY CORRECT' LEAD/LAGS )  ( If want dplyr, PostgreSQL to help SO_BE_IT ) ( see bottom SQLite)
  # need LAGS 3 + 8 + 1 = 12
  # *** PUT all indicators into JUST ONE xts(matrix(numbers)) **** ( ***** THE MOST MAJOR DEAL: GALAXY **** )
  #    data adjustment: > index(RECPROUSM156N[,"RECPROUSM156N"]) -1
  #   qmao help? xtsExtra help?
  # [*** rolling SOMETHING ***]
  # [*** hdnile ( put that into help file), ... need size 8 quantiles(major deal) ]
  # *** 1,2,4,8 lead/lag 1,2,3 velocity,accelleration, jerk
  # randomForest ( and/or Others ) 
  #  ( that Cross-validation function )
  # [ ] consider that DATA MINING book help ( create an rf through a model* function )
  
  # other material ( it would be better if I had the 'stock market' ( and est of dividends ) insted of S & P 500
  # Bernard Bool Book / Sornette Book(fed behavior) / AAII survey ( and other s)
  # that Random Forest pdf article 'stock minus 'bond'' ( as indicator )
  
  # [ ] consider DEEPLY faber 8-mo(10-mo) per sector moving averages ( SEE foresight CODE [x] )
  
  # OECD ( x3x 4 month lag )  VERIFIED NEW!      USARECM: FIX : SUBTRACT OFF ONE DAY [x]

   if( file.exists(paste0(getwd(),"/USARECM.Rdata"))){
    load(file = paste0(getwd(),"/USARECM.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("USARECM",from=initDateSafelyBackOneMo, src = "FRED", index.class=c("POSIXt","POSIXct") )))
    # USARECM <- USARECM[paste0(substr(initDate,1,7),'::')]
    symbols <- c("USARECM")
    symbol <- symbols
    x <- get(symbol)
    index(x) <- dateWarp(date=index(x),spec=-1,by="days")
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,"x.Close"] # remove quantmod o/h/l 
    x[,"x.Close"] <- as.numeric(x[,"x.Close"]) # garantee numeric
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
    save("USARECM",file = paste0(getwd(),"/USARECM.Rdata"))
    rm("x","symbols","symbol")
  }
  # USARECM.Close
  
  # Michigan Sentiment ( A WORK IN PROGRESS )
  #     
  # FRED getSymbols("UMCSENT1" ... 1952-11-01 ... 1977-11-01 # EVERY 3 MONTHS 
  # http://research.stlouisfed.org/fred2/series/UMCSENT1/

   if( file.exists(paste0(getwd(),"/UMCSENT1.Rdata"))){
    load(file = paste0(getwd(),"/UMCSENT1.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("UMCSENT1",from=initDateSafelyBackOneMo, src = "FRED", index.class=c("POSIXt","POSIXct") )))
    # UMCSENT1 <- UMCSENT1[paste0(substr(initDate,1,7),'::')]
    symbols <- c("UMCSENT1")
    symbol <- symbols
    x <- get(symbol)
    index(x) <- dateWarp(date=index(x),spec=-1,by="days") # subtract off one day
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,"x.Close"] # remove quantmod o/h/l 
    x[,"x.Close"] <- as.numeric(x[,"x.Close"]) # garantee numeric
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
    save("UMCSENT1",file = paste0(getwd(),"/UMCSENT1.Rdata"))
    rm("x","symbols","symbol")
  }
  # UMCSENT1.Close

  # http://research.stlouisfed.org/fred2/data/UMCSENT1.txt
  # DATE        VALUE
  # 1952-11-01   86.2
  # 1953-02-01   90.7
  # 1953-08-01   80.8
  
  # Browse[2]> first( UMCSENT1,3)
  #                   UMCSENT1.Close
  # 1952-10-31 86.20000000000000284217
  # 1953-01-31 90.70000000000000284217
  # 1953-07-31 80.79999999999999715783
  
  # FRED getSymbols("UMCSENT"  ... 1978-01-01 ... 2014-03-01
  # http://research.stlouisfed.org/fred2/series/UMCSENT/

  if( file.exists(paste0(getwd(),"/UMCSENT.Rdata"))){
    load(file = paste0(getwd(),"/UMCSENT.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("UMCSENT",from=initDateSafelyBackOneMo, src = "FRED", index.class=c("POSIXt","POSIXct") )))
    # UMCSENT <- UMCSENT[paste0(substr(initDate,1,7),'::')]
    symbols <- c("UMCSENT")
    symbol <- symbols
    x <- get(symbol)
    index(x) <- dateWarp(date=index(x),spec=-1,by="days") # subtract off one day
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,"x.Close"] # remove quantmod o/h/l 
    x[,"x.Close"] <- as.numeric(x[,"x.Close"]) # garantee numeric
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)
    save("UMCSENT",file = paste0(getwd(),"/UMCSENT.Rdata"))
    rm("x","symbols","symbol")
  }
  # UMCSENT.Close

  # http://research.stlouisfed.org/fred2/data/UMCSENT.txt
  # DATE        VALUE
  # 1978-01-01   83.7
  # 1978-02-01   84.3
  # 1978-03-01   78.8
  
  # Browse[2]> first( UMCSENT,3)
  #                    UMCSENT.Close
  # 1977-12-31 83.70000000000000284217
  # 1978-01-31 84.29999999999999715783
  # 1978-02-28 78.79999999999999715783

  # combine (rbind the 'pre-1978 series(1)' with theh post-'pre-1978 series()')
  
  # AsMuchAsPossible ( the old 'up to 1977' series )
  c("UMCSENTAMAP.Close") -> colnames(UMCSENT1)[1]
  c("UMCSENTAMAP.Close") -> colnames(UMCSENT )[1]
  # apropos("bind") ? rbind.xts # Implemented in C
  rbind(UMCSENT1,UMCSENT) -> UMCSENTAMAP
  c("UMCSENT1.Close") -> colnames(UMCSENT1)[1]
  c("UMCSENT.Close")  -> colnames(UMCSENT )[1]
  
  # I want the 'index of the outcome'
  suppressWarnings(mergge(GSPC,UMCSENTAMAP,join='left',tz="UTC")) -> UMCSENTAMAP
  
  # keep just the last column: "UMCSENTAMAP.Close"
  UMCSENTAMAP[,NCOL(UMCSENTAMAP),drop=FALSE] -> UMCSENTAMAP
  
  # prepare for imputation
  # still have to trim the bottom(early)(left) 
  # ( GSPC starts earlier, earliest UMSENT is not until 1952-10-31 )
  # (                      late NA messup imputation?!
  # zoo::na.trim
  na.trim(UMCSENTAMAP, sides=c("both")) -> UMCSENTAMAP
  # 1952-10-31 ... 7 months back == 2014-02-28
  # "UMCSENTAMAP.Close" 
  
  # impute UMCSENT1('pre-1978 series(1)') intermediate months: some kind of impute
  
  # want all GSPC entries ( and for UMCSENTAMAP imputation: trim the early and late )
  na.trim(suppressWarnings(mergge(GSPC[,"GSPC.Close"],UMCSENTAMAP,join='left',tz="UTC")) , sides=c("both")) -> UMCSENTAMAP_IMPUTED
  
  # BEGIN zoo 'linear approximation interpolation'
  # replaced by linear interpolation via 'approx' ( ? zoo:::na.approx.zoo ) # VERY RIGHT ANSWER
  # simple testing: works better than  ( ? zoo:::na.spline.zoo ) # WRONG ANSWER
  
  # I only CARE about
  UMCSENTAMAP_IMPUTED[,"UMCSENTAMAP.Close"] -> UMCSENTAMAP_IMPUTED
  
  # 'Doug Short Method'
  # zoo:::na.approx.zoo 
  # zoo:::na.approx.default
  #   LATER ( IF I CARE FOR SOME CONTROL )
  #   n: If 'xout' is not specified, interpolation takes place at 'n'
  #           equally spaced points spanning the interval ['min(x)','max(x)'].
  
  #   method: specifies the interpolation method to be used.  Choices are '"linear"'(default) or '"constant"'.
  #     .Call(C_ApproxTest, x, y, method, f)
  
  # CORRECT ANSWER!!
  na.approx(UMCSENTAMAP_IMPUTED) -> UMCSENTAMAP_IMPUTED
  
  # done with imputation
  UMCSENTAMAP_IMPUTED -> UMCSENTAMAP
  
  ## JUNK AREA
  # replace NA missing values with something useful
  # I do not want "GSPC.Close" in my math
  # UMCSENTAMAP_IMPUTED[,"UMCSENTAMAP.Close"] -> UMCSENTAMAP_IMPUTED
  # realized xts::lag 'probabally not the 'correct direction' .. did not continue trying or experimenting
  # lag(UMCSENTAMAP_IMPUTED, k = 1)  -> UMCSENTAMAP_IMPUTED[,"UMCSENTAMAP.Close.lag_1"]
  # lead 1
  # lag(UMCSENTAMAP_IMPUTED, k = -1) -> UMCSENTAMAP_IMPUTED[,"UMCSENTAMAP.Close.lead_1"]
  ## END OF JUNK AREA

  # require(imputation) TRY - wrong direction
  
  # wrong answers: ( DOES NOT KNOW THAT THIS IS IS A 'TIME SERIES')
  # forever - never finished
  # invisible(gbmImpute(coredata(UMCSENTAMAP_IMPUTED), max.iters = 2, cv.fold = 2, n.trees = 100, verbose = T) -> result) # forever ... never
  
  # wrong answer ( DOES NOT KNOW THAT THIS IS IS A 'TIME SERIES')
  # quicks: seems like caret likes '5' wRONG ANSWER ( '2' is also the wrong answer)
  # invisible(kNNImpute(coredata(UMCSENTAMAP_IMPUTED), k = 5, verbose = FALSE) -> result) 
  
  # wrong answer ( DOES NOT KNOW THAT THIS IS IS A 'TIME SERIES')
  # invisible(meanImpute(coredata(UMCSENTAMAP_IMPUTED)) -> result) 
  
  # result$x[,"UMCSENTAMAP.Close"] -> UMCSENTAMAP_IMPUTED[,"UMCSENTAMAP.Close"]
  # View(  result )
  
  # just keep UMCSENTAMAP.Close"  
  # UMCSENTAMAP_IMPUTED[,"UMCSENTAMAP.Close"] -> UMCSENTAMAP_IMPUTED
  
  # rm(result)
  # end of require(imputation) TRY - wrong direction
  
  bookmarkhere <- 1
  
  # later ... big mergge ... SPC,USRECM,RECPROUSM156N,USARECM, UMCSENTAMAP_IMPUTED
  
  # "Michigan Sentiment - Final" Economic Calender
  # NEED briefing.com
  # library(qmao)
  # df1 <- getEconomicCalendar(from='YYY-MM-DD', to='YYYY-MM-DD') # form makes the source: briefing.com
  # subset(df1, Statistic == "Michigan Sentiment - Final")
  # NEED 2 MONTHS BACK ( THIS MONTH 'IN' AND 'LAST MONTH BACK TO THE 1ST')
  # NEED 8 MONTHS BACK

  # NEED 
  # minus('7 MONTHS BACK'-'2 MONTHS BACK') + '2 MONTHS BACK
  
  
  ## require(timeDate)

  
  # now
  as.character(Sys.Date()) -> RecentNow
  
  # of last month its first day 
  as.character(as.Date(timeFirstDayInMonth(dateWarp(date=RecentNow,spec=-1,by="months")))) -> RecentFirst 
  
  # of last month its first day the day before
  dateWarp(date=RecentFirst,spec=-1,by="days") -> RecentFirstDayBefore 
  
  # ( of last month its first day the day before ) seven months before ... its first day
  as.character(as.Date(timeFirstDayInMonth(dateWarp(date=RecentFirstDayBefore,spec=-8,by="months")))) -> MediumFirstDay 
  
  if( file.exists(paste0(getwd(),"/BRIEFINGCALMEDIUM.Rdata"))){
    load(file = paste0(getwd(),"/BRIEFINGCALMEDIUM.Rdata"))
  } else {
    print("Beginning ... Running .. getEconomicCalendar ...")
    getEconomicCalendar(from=MediumFirstDay, to=RecentFirstDayBefore) -> BRIEFINGCALMEDIUM
    print("Done ... Running .. getEconomicCalendar ...")
    save("BRIEFINGCALMEDIUM",file = paste0(getwd(),"/BRIEFINGCALMEDIUM.Rdata"))
  }
  
  subset(
    BRIEFINGCALMEDIUM
    , Statistic == "Michigan Sentiment - Final"
  ) -> UMCSENTMEDIUM
  
  if( file.exists(paste0(getwd(),"/BRIEFINGCALRECENT.Rdata"))){
    load(file = paste0(getwd(),"/BRIEFINGCALRECENT.Rdata"))
  } else {
    print("Beginning ... Running .. getEconomicCalendar ...")
    getEconomicCalendar(from=RecentFirst, to=RecentNow) -> BRIEFINGCALRECENT
    print("Done ... Running .. getEconomicCalendar ...")
    save("BRIEFINGCALRECENT",file = paste0(getwd(),"/BRIEFINGCALRECENT.Rdata"))
  }
  
  subset(
    BRIEFINGCALRECENT
    , Statistic == "Michigan Sentiment - Final"
  ) -> UMCSENTRECENT
  
  
  as.character(as.Date(UMCSENTMEDIUM$Time))        -> row.names(UMCSENTMEDIUM)     
  as.numeric(UMCSENTMEDIUM$Actual)                 -> UMCSENTMEDIUM[,"x"]
  UMCSENTMEDIUM[,"x",drop=FALSE]                   -> UMCSENTMEDIUM
  x <- as.xts(UMCSENTMEDIUM,tzone = "UTC")
  
  x <- to.monthly(x,indexAt='lastof',drop.time=TRUE) 
  x <- x[paste0(substr(initDate,1,7),'::')]
  indexFormat(x)<-'%Y-%m-%d'
  x <- x[,"x.Close"] # remove quantmod o/h/l 
  colnames(x)<-gsub("x","UMCSENTMEDIUM",colnames(x))
  x -> UMCSENTMEDIUM
  rm(x)
  
  
  as.character(as.Date(UMCSENTRECENT$Time))        -> row.names(UMCSENTRECENT)     
  as.numeric(UMCSENTRECENT$Actual)                 -> UMCSENTRECENT[,"x"]
  UMCSENTRECENT[,"x",drop=FALSE]                   -> UMCSENTRECENT
  x <- as.xts(UMCSENTRECENT,tzone = "UTC")
  
  x <- to.monthly(x,indexAt='lastof',drop.time=TRUE) 
  x <- x[paste0(substr(initDate,1,7),'::')]
  indexFormat(x)<-'%Y-%m-%d'
  x <- x[,"x.Close"] # remove quantmod o/h/l 
  colnames(x)<-gsub("x","UMCSENTRECENT",colnames(x))
  x -> UMCSENTRECENT
  rm(x)
  
  
  bookmarkhere <- 1
  
  
  # combine the "Michigan Sentiment - Final" AMAP
  #    with the "Michigan Sentiment - Final" MEDIUM and
  #    with the "Michigan Sentiment - Final" RECENT
  
  c("UMCSENTAMAP.Close") -> colnames(UMCSENTMEDIUM)[1]
  # apropos("bind") ? rbind.xts # Implemented in C
  rbind(UMCSENTAMAP,UMCSENTMEDIUM) -> UMCSENTAMAP
  # Typically duplicates are eliminated
  # fix it up by taking last in each set of duplicates
  # zoo FAQ
  aggregate(UMCSENTAMAP, identity, tail, 1) -> UMCSENTAMAP
  # return to be an xts, fix lost column name
  xts(UMCSENTAMAP) -> UMCSENTAMAP; c("UMCSENTAMAP.Close") ->colnames(UMCSENTAMAP)
  c("UMCSENTMEDIUM.Close") -> colnames(UMCSENTMEDIUM)[1]
  
  # I want the 'index of the outcome'
  suppressWarnings(mergge(GSPC,UMCSENTAMAP,join='left',tz="UTC")) -> UMCSENTAMAP
  
  # keep just the last column: "UMCSENTAMAP.Close"
  UMCSENTAMAP[,NCOL(UMCSENTAMAP),drop=FALSE] -> UMCSENTAMAP
  
  
  c("UMCSENTAMAP.Close") -> colnames(UMCSENTRECENT)[1]
  # apropos("bind") ? rbind.xts # Implemented in C
  rbind(UMCSENTAMAP,UMCSENTRECENT) -> UMCSENTAMAP
  # Typically duplicates are eliminated
  # fix it up by taking last in each set of duplicates
  # zoo FAQ
  aggregate(UMCSENTAMAP, identity, tail, 1) -> UMCSENTAMAP
  # return to be an xts, fix lost column name
  xts(UMCSENTAMAP) -> UMCSENTAMAP; c("UMCSENTAMAP.Close") ->colnames(UMCSENTAMAP)
  c("UMCSENTRECENT.Close") -> colnames(UMCSENTRECENT)[1]
  
  # I want the 'index of the outcome'
  suppressWarnings(mergge(GSPC,UMCSENTAMAP,join='left',tz="UTC")) -> UMCSENTAMAP
  
  # keep just the last column: "UMCSENTAMAP.Close"
  UMCSENTAMAP[,NCOL(UMCSENTAMAP),drop=FALSE] -> UMCSENTAMAP
  
  bookmarkhere <- 1 
  
  # because a missing value in 2014-07-31 "UMCSENTMEDIUM"  
  #  I am required to do this na.trim and na.approx NOW!
  
  # zoo::na.trim
  na.trim(UMCSENTAMAP, sides=c("both")) -> UMCSENTAMAP
  
  # time series impute missing values ( SHOULD BE JUST ONE COL ANYWAYS )
  na.approx(UMCSENTAMAP[,"UMCSENTAMAP.Close"]) -> UMCSENTAMAP[,"UMCSENTAMAP.Close"]
  
  #  LEFT_OFF  [ ] average_out or velocitize UMICH data
  #            [ ] add to merge data below
  #  FAR NEXT  [ ] consider VIX volitity or 'similar' data / or OTHERS?
  
  
  # ### MAIN MERGE BEGINS HERE ######
  #                                 #
  # left join .External("mergeXts"? - complains about 'left join meant for 2' 'localtime is not UTC' 
  suppressWarnings(mergge(GSPC,USRECM,RECPROUSM156N,USARECM,join='left',tz="UTC")) -> GALAXY_L   
  
  # TOO EARLY - have to do below anyways
  # keep complete.cases ( zoo::na.trim is equivalent )
  # na.trim(GALAXY_L[,!(colnames(GALAXY_L) %in% "tz")])  -> GALAXY_L 
  
  # CURRENTLY DO NOT CHANGE THE 'ORDER OF COLUMN GENERATION'
  
  # the benchmark
  
  # regular end of month closes for the benchmark
  symbol_OHLC_lag_k_periods(GSPC,"GSPC",0:6)                         -> GSPC_LAGS 
  suppressWarnings(mergge(GALAXY_L,GSPC_LAGS,join='outer',tz='UTC')) -> GALAXY_L 
  
  # Generated
  # GSPC.EndOf.Lag.0  ... GSPC.EndOf.Lag.6
  
  # the 'lowest daily found in a day in that month'
  symbol_OHLC_lag_k_periods(GSPC,"GSPC",0:5, qmOHLC = "Lowz")            -> GSPC_LAGS_LOW 
  suppressWarnings(mergge(GALAXY_L,GSPC_LAGS_LOW,join='outer',tz='UTC')) -> GALAXY_L 
  
  # Generated
  # GSPC.MinOf.Lag.0 ... GSPC.MinOf.Lag.5

  # all others ( not the benchmark )
  
  # COME BACK NEED THESE LAGS HERE
  
  # NBER: 4 month lag ( TO BE DONE [X] ) # old 0:1 ( NOW: 2 month ave, pred 1-6)
  symbol_OHLC_lag_k_periods(USRECM,"USRECM",5:11)                      -> USRECM_LAGS 
  suppressWarnings(mergge(GALAXY_L,USRECM_LAGS,join='outer',tz='UTC')) -> GALAXY_L
  
  # Generated
  # USRECM.EndOf.Lag.5 ... USRECM.EndOf.Lag.11
  
  # Chavet Piger 3 month lag 
  # ( maybe a 'safe 4 month lag' ( currenlty using 3 month lag ) old: 0:11 
  # ( TO HAVE: 2 month ave, change, changechange, changechangechange, changechangechangechange 
  # 2 month ave    4:10   
  # pred 1: changechangechangechange 4:12
  # pred 6: changechangechangechange 9:17  therefore  4:17
  # ( TO BE DONE - IN PROGRESS [ ] )
  symbol_OHLC_lag_k_periods(RECPROUSM156N,"RECPROUSM156N",4:12)               -> RECPROUSM156N_LAGS 
  suppressWarnings(mergge(GALAXY_L,RECPROUSM156N_LAGS,join='outer',tz='UTC')) -> GALAXY_L
  
  # Generated
  # RECPROUSM156N.EndOf.Lag.4 ... RECPROUSM156N.EndOf.Lag.17
  
  # OECD 4 month lag ( TO BE DONE [X] ) # old 0:1 ( NOW: 2 month ave, pred 1-6)
  symbol_OHLC_lag_k_periods(USARECM,"USARECM",5:11)                      -> USARECM_LAGS 
  suppressWarnings(mergge(GALAXY_L,USARECM_LAGS,join='outer',tz='UTC'))  -> GALAXY_L
  
  # Generated
  # USARECM.EndOf.Lag.5 ... USARECM.EndOf.Lag.11
  
  # Need the 'min ever seen' over the zone 'llagk_zone_back'
  # the benchmark merge.xts? - outer ( old: 0:2 ) ( TO BE DONE [ ] )
  

  
  # symbol_OHLC_range_k_periods(GSPC,"GSPC",0:0
    # , qmOHLC = "Lo", llagk_zone_back = 0
    # , findthat = "min", findthat_name = "MinOf", findthat_range_name = "RangeLwz" )  -> GSPC_RANGE_LOWZ 
  # suppressWarnings(mergge(GALAXY_L,GSPC_RANGE_LOWZ ,join='outer',tz='UTC'))          -> GALAXY_L 
  
  # columns input:  ( GSPC Lag(Lo(GSPC) )
  
  suppressWarnings(mergge(GALAXY_L 
    , symbol_OHLC_range_k_periods(GSPC,"GSPC",0:0, qmOHLC = "Lo", llagk_zone_back = 0, findthat = "min", findthat_name = "MinOf", findthat_range_name = "RangeLwz" )
    , symbol_OHLC_range_k_periods(GSPC,"GSPC",0:0, qmOHLC = "Lo", llagk_zone_back = 1, findthat = "min", findthat_name = "MinOf", findthat_range_name = "RangeLwz" )
    , symbol_OHLC_range_k_periods(GSPC,"GSPC",0:0, qmOHLC = "Lo", llagk_zone_back = 2, findthat = "min", findthat_name = "MinOf", findthat_range_name = "RangeLwz" )
    , symbol_OHLC_range_k_periods(GSPC,"GSPC",0:0, qmOHLC = "Lo", llagk_zone_back = 3, findthat = "min", findthat_name = "MinOf", findthat_range_name = "RangeLwz" )
    , symbol_OHLC_range_k_periods(GSPC,"GSPC",0:0, qmOHLC = "Lo", llagk_zone_back = 4, findthat = "min", findthat_name = "MinOf", findthat_range_name = "RangeLwz" )
    , symbol_OHLC_range_k_periods(GSPC,"GSPC",0:0, qmOHLC = "Lo", llagk_zone_back = 5, findthat = "min", findthat_name = "MinOf", findthat_range_name = "RangeLwz" )
  ,join='outer',tz='UTC'))  -> GALAXY_L 
  
  # Generated
  # GSPC.MinOf.Lag.0.RangeLwz.0.0 ... GSPC.MinOf.Lag.0.RangeLwz.0.5 ( NEXT [ ] DIVE IN ABOVE: AND NEED COLUMN INPUTS )
  
  # Need the average at the 'point' minus 2 months ( range )  
  
  symbol_OHLC_range_k_periods(RECPROUSM156N,"RECPROUSM156N",4:9
    , qmOHLC = "Cl", llagk_zone_back = 1
    , findthat = "mean", findthat_name = "AveOf", findthat_range_name = "RangeAvgz" )   -> RECPROUSM156N_RANGE_AVGZ 
  suppressWarnings(mergge(GALAXY_L,RECPROUSM156N_RANGE_AVGZ ,join='outer',tz='UTC'))    -> GALAXY_L 

  # Generated
  # RECPROUSM156N.AveOf.Lag.4.RangeAvgz.4.5  RECPROUSM156N.AveOf.Lag.9.RangeAvgz.9.10
  
  symbol_OHLC_range_k_periods(USRECM,"USRECM",5:10
    , qmOHLC = "Cl", llagk_zone_back = 1
    , findthat = "mean", findthat_name = "AveOf", findthat_range_name = "RangeAvgz" )   -> USRECM_RANGE_AVGZ
  suppressWarnings(mergge(GALAXY_L,USRECM_RANGE_AVGZ ,join='outer',tz='UTC'))           -> GALAXY_L 
  
  # Generated
  # USRECM.AveOf.Lag.5.RangeAvgz.5.6         USRECM.AveOf.Lag.10.RangeAvgz.10.11
  
  symbol_OHLC_range_k_periods(USARECM,"USARECM",5:10
    , qmOHLC = "Cl", llagk_zone_back = 1
    , findthat = "mean", findthat_name = "AveOf", findthat_range_name = "RangeAvgz" )   -> USARECM_RANGE_AVGZ
  suppressWarnings(mergge(GALAXY_L,USARECM_RANGE_AVGZ ,join='outer',tz='UTC'))          -> GALAXY_L 
  
  # Generated
  # USARECM.AveOf.Lag.5.RangeAvgz.5.6        USARECM.AveOf.Lag.10.RangeAvgz.10.11
  
  # need maximum drawdown
  suppressWarnings(mergge(GALAXY_L
    ,  max_drawdown_pct_chnge(GALAXY_L,"GSPC.MinOf.Lag.0.RangeLwz.0.0","GSPC.EndOf.Lag.1") 
    ,  max_drawdown_pct_chnge(GALAXY_L,"GSPC.MinOf.Lag.0.RangeLwz.0.1","GSPC.EndOf.Lag.2") 
    ,  max_drawdown_pct_chnge(GALAXY_L,"GSPC.MinOf.Lag.0.RangeLwz.0.2","GSPC.EndOf.Lag.3") 
    ,  max_drawdown_pct_chnge(GALAXY_L,"GSPC.MinOf.Lag.0.RangeLwz.0.3","GSPC.EndOf.Lag.4") 
    ,  max_drawdown_pct_chnge(GALAXY_L,"GSPC.MinOf.Lag.0.RangeLwz.0.4","GSPC.EndOf.Lag.5") 
    ,  max_drawdown_pct_chnge(GALAXY_L,"GSPC.MinOf.Lag.0.RangeLwz.0.5","GSPC.EndOf.Lag.6") 
  ,join='outer',tz='UTC')) -> GALAXY_L

  # OUTPUT 'maximum drawdown pct change' 
  # of the minimum of the range of months 0,1,2 compared to th end of lag 3
  # Generated
  # 
  
  # ( Note: Later may want to keep 'and create an "articial Open"' for quantstrat )
  # remove those .Close ( and .Low ) columns
  # so not to break quantmod::Cl()
  GALAXY_L[,!grepl(".Close",colnames(GALAXY_L))] -> GALAXY_L
  GALAXY_L[,!grepl(".Low",colnames(GALAXY_L))]   -> GALAXY_L
  
  # currently - just predict 1 # ( LEFT_OFF: RE-VERIFY UPWARDS [ ] )
  suppressWarnings(mergge(GALAXY_L
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.4","RECPROUSM156N.EndOf.Lag.5")
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.5","RECPROUSM156N.EndOf.Lag.6")
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.6","RECPROUSM156N.EndOf.Lag.7")
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.7","RECPROUSM156N.EndOf.Lag.8")
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.8","RECPROUSM156N.EndOf.Lag.9") 
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.9","RECPROUSM156N.EndOf.Lag.10") 
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.10","RECPROUSM156N.EndOf.Lag.11") 
    ,  pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.11","RECPROUSM156N.EndOf.Lag.12") 
  ,join='outer',tz='UTC')) -> GALAXY_L
  
  # Generated
  # "RECPROUSM156N.EndOf.Lag.4.PctChnge.4.5" ... "RECPROUSM156N.EndOf.Lag.11.PctChnge.11.12"
  
  
  # # OLD
  # suppressWarnings(mergge(GALAXY_L
    # ,  pct_chnge_pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.0.PctChnge.0.1","RECPROUSM156N.EndOf.Lag.1.PctChnge.1.2")
  # ,join='outer',tz='UTC')) -> GALAXY_L
  
  # currently - just predict 1
  suppressWarnings(mergge(GALAXY_L
    ,  pct_chnge_pct_chnge(GALAXY_L, "RECPROUSM156N.EndOf.Lag.4.PctChnge.4.5","RECPROUSM156N.EndOf.Lag.5.PctChnge.5.6")
    ,  pct_chnge_pct_chnge(GALAXY_L, "RECPROUSM156N.EndOf.Lag.6.PctChnge.6.7","RECPROUSM156N.EndOf.Lag.7.PctChnge.7.8")
    ,  pct_chnge_pct_chnge(GALAXY_L, "RECPROUSM156N.EndOf.Lag.8.PctChnge.8.9","RECPROUSM156N.EndOf.Lag.9.PctChnge.9.10")
    ,  pct_chnge_pct_chnge(GALAXY_L, "RECPROUSM156N.EndOf.Lag.10.PctChnge.10.11","RECPROUSM156N.EndOf.Lag.11.PctChnge.11.12")
  ,join='outer',tz='UTC')) -> GALAXY_L
  
  # generates output
  # RECPROUSM156N.EndOf.Lag.4.PctChngePctChnge.4.6  ... RECPROUSM156N.EndOf.Lag.10.PctChngePctChnge.10.12
  
  # currently - just predict 1
  suppressWarnings(mergge(GALAXY_L
  ,  pct_chnge_pct_chnge_pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.4.PctChngePctChnge.4.6","RECPROUSM156N.EndOf.Lag.6.PctChngePctChnge.6.8")
  ,  pct_chnge_pct_chnge_pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.8.PctChngePctChnge.8.10","RECPROUSM156N.EndOf.Lag.10.PctChngePctChnge.10.12")
  ,join='outer',tz='UTC')) -> GALAXY_L
  
  # Generates exactly 
  # RECPROUSM156N.EndOf.Lag.4.PctChngePctChngePctChnge.4.8  RECPROUSM156N.EndOf.Lag.8.PctChngePctChngePctChnge.8.12

  # currently - just predict 1
  suppressWarnings(mergge(GALAXY_L
  ,  pct_chnge_pct_chnge_pct_chnge_pct_chnge(GALAXY_L,"RECPROUSM156N.EndOf.Lag.4.PctChngePctChngePctChnge.4.8","RECPROUSM156N.EndOf.Lag.8.PctChngePctChngePctChnge.8.12")
  ,join='outer',tz='UTC')) -> GALAXY_L
  
  # Generates exactly 
  # RECPROUSM156N.EndOf.Lag.4.PctChngePctChngePctChngePctChnge.4.12
  
  # remove those uselss 'tz.#' columns ( aesthetics )
  # will leave just "tz" behind ( NOTE: SHOULD! remove "tz" from GALAXY_L just after each merge )
  # GALAXY_L[,!grepl("tz.",colnames(GALAXY_L))] -> GALAXY_L
  
  
  # AGAIN remove the 'extra records that have crept in because of the 
  #  should ACTUALLY be removed after each 'merge' ( currenly does not break anything )
  # 'outer' join to the benchmark
  # keep complete.cases ( zoo::na.trim is equivalent )
  # na.trim(GALAXY_L[,!(colnames(GALAXY_L) %in% "tz")])  -> GALAXY_L 
  
  # KEEP
  # if I want to just work with a SAMPLE
  #   as in 'pointless having everything: as sample of 300 as good as 3000' 
  #      'to much memory' OR 'get the same results' after 300'
  # GALAXY_L[sample(1:NROW(GALAXY_L),300,replace=FALSE),] -> GALAXY_L
  
  # KEEP
  # make sure I look at current result of long running results 'on the fly'
  # capture.output(print(paste0(as.character(Sys.Date())))
    # , file = "./tests/main-rcsnsight1-999_runRESULTS.txt"  
    # , append=TRUE
  # )
  
 # ROUND 1 - one month prediction 
 # one month prediction - data
  #             RESPONSE
  #               PREDICTORS
  # GALAXY_L[, c("GSPC.MinOf.Lag.0.RangeLwz.0.0.EndOf.Lag.1.PctChnge.0.1"   # caret prefers the first col to be the outcome
                # , "RECPROUSM156N.EndOf.Lag.4", "RECPROUSM156N.EndOf.Lag.5"
                  # , "RECPROUSM156N.AveOf.Lag.4.RangeAvgz.4.5"
                  # , "RECPROUSM156N.EndOf.Lag.4.PctChnge.4.5"
                # , "USRECM.EndOf.Lag.5", "USRECM.EndOf.Lag.6"
                  # , "USRECM.AveOf.Lag.5.RangeAvgz.5.6"
                # , "USARECM.EndOf.Lag.5", "USARECM.EndOf.Lag.6"
                  # , "USARECM.AveOf.Lag.5.RangeAvgz.5.6"
              # )] -> GALAXY_L   # NOTE: SHOULD JUST ( SEARCH AND REPLACE ON THE DOT.NUMBER PREDICTORS -> 2 month look ahead )
                  
 # ROUND 2 - one month prediction 
 # one month prediction - data
  #             RESPONSE
  #               PREDICTORS
  # GALAXY_L[, c("GSPC.MinOf.Lag.0.RangeLwz.0.0.EndOf.Lag.1.PctChnge.0.1"   # caret prefers the first col to be the outcome
                # , "RECPROUSM156N.EndOf.Lag.4"
                  # , "RECPROUSM156N.EndOf.Lag.4.PctChnge.4.5"
                    # , "RECPROUSM156N.EndOf.Lag.4.PctChngePctChnge.4.6"
                      # , "RECPROUSM156N.EndOf.Lag.4.PctChngePctChngePctChnge.4.8"
                        # , "RECPROUSM156N.EndOf.Lag.4.PctChngePctChngePctChngePctChnge.4.12"
              # )] -> GALAXY_L   

  # one mo pred
  # TargetObservation < -0.05
  # C5.0
            # Reference
  # Prediction one two
         # one   7  14
         # two  54 201
    
  # GALAXY_L[, c("GSPC.MinOf.Lag.0.RangeLwz.0.0.EndOf.Lag.1.PctChnge.0.1"   # caret prefers the first col to be the outcome
                # , "RECPROUSM156N.EndOf.Lag.4"
                  # , "RECPROUSM156N.AveOf.Lag.4.RangeAvgz.4.5"
              # )] -> GALAXY_L  
    
  # one mo pred
  # TargetObservation < -0.05
  # rf

            # Reference
  # Prediction one two
         # one  10  14
         # two  52 204
 
  GALAXY_L[, c("GSPC.MinOf.Lag.0.RangeLwz.0.2.EndOf.Lag.3.PctChnge.0.3"   # caret prefers the first col to be the outcome
                , "RECPROUSM156N.EndOf.Lag.6"
                  , "RECPROUSM156N.AveOf.Lag.6.RangeAvgz.6.7"
              )] -> GALAXY_L  
             
  # three mo pred
  # TargetObservation < -0.05
 
  # rf
              
            # Reference
  # Prediction one two
         # one  32  36
         # two  83 128
 
  # C5.0

            # Reference
  # Prediction one two
         # one  48  48
         # two  67 116

  # gbm

            # Reference
  # Prediction one two
         # one  37  38
         # two  78 126
 
 
  # VERY LAST STEP 
  # ABOVE be sure A HAVE managed NA's
  # be SURE this is what I want
  # zoo::na.trim
  na.trim(GALAXY_L) -> GALAXY_L 
 
  # ALTERNATIVE ( weaker )
  # In general, the functions in the caret package assume that there are no missing values in the
  # data or that these values have been handled via imputation or other means.
  # 2008 pdf 12
  # > apropos("impu")
  # [1] "rfImpute" ( rfImpute package:randomForest )
  # > ? rfImpute
       # Impute missing values in predictor data using proximity from ( SO 'PROXIMIY' useful? )
       # randomForest.

       # The algorithm starts by imputing 'NA's using 'na.roughfix'.  Then
       # 'randomForest' [with proximity=TRUE(non-default)] is called with the completed data. 

       # Value:
         # A data frame or matrix containing the completed data matrix 
         # ( NOTE: I would have to RE-SAVE the *new* matrix, then re-run through caret )
    # ? na.roughfix ( weak ) AND  Andy Liaw ONLY
 
  # Building_Predictive_Models_in_R_Using_the_caret_package(1startarticle)(Kuhn)(2008).pdf page and pdf 5 ( of 26 pages )

  # stats::cor
  # tendancy to move 'up and down together'
  # each predictor is weakly correlated with the response
  # but most all predictors seem to be highly correlated with each other
  # RECPROUSM156N.AveOf.Lag.4.RangeAvgz.4.5 and RECPROUSM156N.EndOf.Lag.4.PctChnge.4.5 ARE 'weakly' correlated with each other
  # View(cor(coredata(GALAXY_L)))
  
  # x: A correlation matrix
  #   Value: A vector of indices denoting the columns to remove.
  # findCorrelation(x, cutoff = .90, verbose = FALSE) # .90 is HIGH CORRELATION ALLOWED ( many variables kept )
  # ( do not remove my outcome )

  # 0.95 it chose to remove [1:3] 7 3 10
  # RECPROUSM156N.AveOf.Lag.4.RangeAvgz.4.5 USRECM.AveOf.Lag.5.RangeAvgz.5.6  USARECM.AveOf.Lag.5.RangeAvgz.5.6

  # 0.90 ( default) it chose to remove int [1:6] 7 3 5 2 10 9 
  # RECPROUSM156N.AveOf.Lag.4.RangeAvgz.4.5 USRECM.AveOf.Lag.5.RangeAvgz.5.6  USARECM.AveOf.Lag.5.RangeAvgz.5.6
  # added 5, 2, 9
  # USRECM.EndOf.Lag.5 RECPROUSM156N.EndOf.Lag.5 USARECM.EndOf.Lag.6

  # BUT Note: Linoff and B; perform math within the correlations
  # O'Shannessy           ; quantiles
  # Perhaps I will do: hdntile(RECPROUSM156N*AVG*,8) + hdntile(USRECM*AVG*,8) + hdntile(USARECM*AVG*,8) ... hdquantile(combined) -> one good indicator
  # #                        OR JUST KEEP THE 'binary ones USREC + USAREC: and 'ADD THEM UP' = new predictor 
  
  # I will stick with 0.95 ( Perhaps Temporary )
  
  print("Possibly correlated observation and predictors columns")
  print(data.frame(colnames(GALAXY_L)))
  
  colnames(GALAXY_L)[1] <- "TargetObservation"
  
  ###### IF I JUST WANT TO WORK WITH SMALL DATA
  ### # is my 'data bad' # -0.10 28 obs in 553 ( ROUGHLY 24 IN 500 ) ( -0.07444 56 observations in 56 )
  data.frame(GALAXY_L) -> GALAXY_L_df 
  conn <- sqldf(drv = "SQLite")
  # artificial small data
    # GALAXY_L_df <- sqldf("select * from GALAXY_L_df where TargetObservation < -0.07444", connection = conn, row.names = TRUE)
  # the real deal big data
     GALAXY_L_df <- sqldf("select * from GALAXY_L_df where TargetObservation < 99999.9", connection = conn, row.names = TRUE)
  conn <- NULL
  xts(GALAXY_L_df, as.Date(row.names(GALAXY_L_df), tz = "UTC" )) -> GALAXY_L
  rm(GALAXY_L_df)
  ###
  #####
 
  # NOTE: sqldf over SQLite  changed "." to "_"
 
  ## Default S3 method:
  #  confusionMatrix(data, reference, positive = NULL, 
  #                  dnn = c("Prediction", "Reference"), 
  #                  prevalence = NULL, ...)
  #  
  #  reference: a factor of classes to be used as the true results
  #  
  #  positive: an optional character string for the factor level that
  #  corresponds to a "positive" result (if that makes sense for
  #                                      your data). If there are only two factor levels, the first
  #  level will be used as the "positive" result.
  #  
  #  > ? confusionMatrix
  #  
  #  library(balance)
  #  ubBalance
  #  Y the response variable of the unbalanced dataset. It must be a 
  #  binary factor 
  #  where the 
  #    majority class is coded as 0 and the minority as 1.
  #  http://cran.r-project.org/web/packages/unbalanced/unbalanced.pdf
 
  # means: labels=(c(1,0)) # (first factor, second factor) # ( dput CODED AS 1L,2L )
 
  # Try to setup for classification ( could be twoClassSummary ) 
  #
  # as.matrix(within(data.frame(coredata(GALAXY_L)),{ # xts  # SMALL SAMPLE I USED -0.10
  within(data.frame(coredata(GALAXY_L)),{
   factor(ifelse( TargetObservation < -0.05,
           1L, 
           2L), levels = c(1L,2L),labels=c("one","two")) -> TargetObservation
  } 
  ) -> GALAXY_L
  # )) -> coredata(GALAXY_L) # xts ( ALSO remove as.factor() PLEASE )
 
  # HARD NOTE: FOR library(unbalanced) I WILL HAVE TO RECODE "one" as "1" and "zero" as "0"
  #       THEN FOR library(caret)           I WILL HAVE TO RECODE BACK "1" as "one" and "two" as "0"
 
  # classification only 
  # unbalanced to 'more' BALANCED zone ON /OFF 
 
  # ? ubSMOTE ? ubBalance
  # library(unbalanced) # data(ubIonosphere) $ Class: Factor w/ 2 levels "0","1"
  # data   $ Class: Factor w/ 2 levels "0","1"
   
  within(data.frame(coredata(GALAXY_L)),{
    factor(ifelse( TargetObservation != "one",
                  0L, 
                  1L), levels = c(0L,1L)) -> TargetObservation
  } 
  ) -> GALAXY_L #  GALAXY_L$TargetObservation : Factor w/ 2 levels "0","1": 2 1
 
  # ? ubSMOTE ? ubBalance
  ubBalance(X = GALAXY_L[,-1*c(1),drop=FALSE], Y = unlist(GALAXY_L[,1,drop=FALSE])
    , type="ubSMOTE") -> GALAXY_L_ubSMOTE
 
 # from 577 to 1610 obs. of  2 variables
 # Browse[2]> table(GALAXY_L_ubSMOTE$Y  )
 # 0   1 
 # 920 690 
 
  cbind(GALAXY_L_ubSMOTE$Y,GALAXY_L_ubSMOTE$X) -> GALAXY_L
  "TargetObservation" -> colnames(GALAXY_L)[1] #  GALAXY_L$TargetObservation: Factor w/ 2 levels "0","1": 1 2 2 1 
 
  # now change the labels back to the values: "one" (minority),"two" (majority)
  # TO DO
 
   within(data.frame(coredata(GALAXY_L)),{  # LEFT_OFF
     factor(ifelse( TargetObservation == 1L,
                    1L, 
                    2L), levels = c(1L,2L),labels=c("one","two")) -> TargetObservation
   } 
   ) -> GALAXY_L #  GALAXY_L$TargetObservation : Factor w/ 2 levels 
   
  bookmarkhere <- 1;
 
  # end of 'unbalanced to 'more' BALANCED zone ON /OFF'
  
  ## CORRELATION removal zone ON / OFF
 
  # xts case 
  # cor(coredata(GALAXY_L)[,-1*c(1),drop=FALSE])                          -> GALAXY_L_Corr_matrix
  # non-xts case
  ###cor(GALAXY_L[,-1*c(1),drop=FALSE])                                   -> GALAXY_L_Corr_matrix
  ###findCorrelation(GALAXY_L_Corr_matrix, cutoff = .95, verbose = FALSE) -> GALAXY_L_highCorr_colnames_vector
  # # just keep one per list item: 1st in list item is the violator
  # # findLinearCombos(coredata(GALAXY_L)[,-1*c(1),drop=FALSE])
  # xts - eliminate columns
  ###GALAXY_L[,-1*(GALAXY_L_highCorr_colnames_vector + 1)] -> GALAXY_L

  ###print("Should be UN-correlated observation and predictors columns")
  ###print(data.frame(colnames(GALAXY_L)))
  
 ## END OF CORRELATION removal zone ON / OFF
 
  # Gentle learning set
 
  # ( 37 SLIDES )
  # caret_Package_A_Unifed_Interface_for_Predictive_Models(201402)(Kuhn).pdf
  
  # A Short Introduction to the caret Package ( AUG 2014 - JUST 10 PAGES )
  # http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
  
  if(exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed",envir=.GlobalEnv)
  set.seed(THESEED) 
  # p = 0.5 ( default )  ( p: the percentage of data that goes to training  )
  # These random 0-dates are no longer good for xts ... now need matrix
  # createTimeSlices : may have been good; but I started without doing IT this way
  # xts case
  # createDataPartition(coredata(GALAXY_L[,1,drop=FALSE]), p = .50, list = FALSE) -> GALAXY_L_inTrainingSet_rowid_vector 
  # non-xts case
  createDataPartition(unlist(GALAXY_L[,1,drop=FALSE]), p = 0.50, list = FALSE) -> GALAXY_L_inTrainingSet_rowid_vector
  
  # xts would have semi-random dates ... no longer good to me ... matrix
  coredata(GALAXY_L[ GALAXY_L_inTrainingSet_rowid_vector,,drop = FALSE])  -> GALAXY_L_Train
  coredata(GALAXY_L[-GALAXY_L_inTrainingSet_rowid_vector,,drop = FALSE ]) -> GALAXY_L_Test
  
  # preProcess - I DID MY OWN
  # method = c("center", "scale"), 
                # thresh = 0.95,
                # pcaComp = NULL,
  # BUT HEAVY "pca" ORIENTATED    
  #                               method = "pca" pdf 6  method generates values with column names "PC1", "PC2"
  # ? preProcess # also read:     Building_Predictive_Models_in_R_Using_the_caret_package(1startarticle)(Kuhn)(2008).pdf
  
  
  # ( seems very little effect (if not worse)? )
  # **Preprocess zone (if any)** #
  
  procValues <- preProcess(GALAXY_L_Train[,-1*c(1),drop=FALSE]
  # , method =  c("pca")
    # , thresh = 90
    # , pcaComp = 2
    , method =  c("ica")
    # fastICA::fastICA(x, ...); argument "n.comp" is missing, with no default
    # n.comp number of components to be extracted
      , n.comp = 2
    # http://cran.r-project.org/web/packages/fastICA/fastICA.pdf
  )
  # Use the predict methods to do the adjustments ( LEFT_OFF ) AFTER BREAK
  GALAXY_L_trainScaled <- predict(procValues, GALAXY_L_Train[,-1*c(1),drop=FALSE])
  cbind(GALAXY_L_Train[,1,drop=FALSE],GALAXY_L_trainScaled) -> GALAXY_L_Train
  
  #  "pca"
  # GALAXY_L_trainScaled
  # 'data.frame':  279 obs. of  2 variables:
  # $ PC1: num  -0.582 -0.591 -0.59 -0.545 -0.284 ...
  # $ PC2: num  0.00143 -0.0039 -0.00443 -0.01162 -0.08386 ..

  # "ica"
  # colnames.GALAXY_L_Train.
  # 1        TargetObservation
  # 2                     ICA1
  # 3                     ICA2
 
  # "pca"
  # GALAXY_L_Train
  # 'data.frame':  279 obs. of  3 variables:
  #   $ TargetObservation: Factor w/ 2 levels "one","two": 1 1 1 1 1 1 1 1 1 1 ...
  # $ PC1              : num  -0.582 -0.591 -0.59 -0.545 -0.284 ...
  # $ PC2              : num  0.00143 -0.0039 -0.00443 -0.01162 -0.08386 ...

  GALAXY_L_testScaled  <- predict(procValues, GALAXY_L_Test[,-1*c(1),drop=FALSE])
  cbind(GALAXY_L_Test[,1,drop=FALSE],GALAXY_L_testScaled) -> GALAXY_L_Test
  
  print("After preProcess; new (ifany) 'processed' predictors columns")
  print(data.frame(colnames(GALAXY_L_Train)))
  
  # "pca"
  # colnames.GALAXY_L_Train.
  # 1        TargetObservation
  # 2                      PC1
  # 3                      PC2


  # caret_Package_A_Unifed_Interface_for_Predictive_Models(201402)(Kuhn).pdf pdf 9
  # Building_Predictive_Models_in_R_Using_the_caret_package(1startarticle)(Kuhn)(2008)_DEEPER_INSTRUCTIONS.pdf pdf 5
  # ? preProcess
  
  # **End of Preprocess zone (if any)** #
  
  # next 'trainControl'
  # http://topepo.github.io/caret/training.html
  # I BELIEVE? Brieman liked 10-fold: number = 10.  WHY 10 repeats? 
  
  #'twoClassSummary' computes sensitivity, specificity and the area
  #   under the ROC curve. To use this function, the 'classProbs'
  #   argument of 'trainControl' should be 'TRUE'.
  # ? twoClassSummary
  
  # caret_Package_A_Unifed_Interface_for_Predictive_Models(201402)(Kuhn).pdf
  
  trainControl(## 10-fold CV
    method = "repeatedcv"  # randomForest ( could have been: 'oob' )
    , number = 10, # ? trainControl ( "repeatedcv" default seems to be '10' anyways ) # QUICK 5
     ## repeated ten times    
    , repeats = 1 # CHANGE FROM 10 ( OR 5 ) DOWN TO 1 ( speed )                      # QUICK 1
    # , summaryFunction = defaultSummary
    , classProbs = TRUE                 # classification
    , summaryFunction = twoClassSummary # classification ( sp case ) TRYING
      # ( caret.pdf): = twoClassSummary. The latter will compute measures specific to two-class problems, 
      # such as the area under the ROC curve, the sensitivity and specificity
    # A list of options to pass to 'preProcess'
    # , preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5),   
       # http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
       # compute measures specific to two “class
       # problems, such as the area under the ROC curve, the sensitivity and specificity
       # SINCE I AM DOING 'regression' ( FOR NOW ) ( and not a classifiction ( SPECIFICALLY '2-class' CLASSIFICATION )
       # , classProbs = TRUE, summaryFunction = twoClassSummary
    , allowParallel = FALSE # ( I do not have a parallel setup ( and parallel is not so great on Windows )
  ) -> GALAXY_L_fitControl
  
 
  # train :function has the following arguments
    # ... : the three dots can be used to pass additional arguments to the functions listed in Table 1.
  # table1: 
    # Random forests rf(THISONE) randomForest mtry
  # Building_Predictive_Models_in_R_Using_the_caret_package(1startarticle)(Kuhn)(2008).pdf pdf 6 and pdf 8 (table 1)
  
  #  'train' ( default : method = "rf" )
  # http://topepo.github.io/caret/Random_Forest.html
  # Random Forest
  # method = 'rf'
  # Type: Classification, Regression
  # Tuning Parameters: mtry (#Randomly Selected Predictors)
  
  # y: a numeric or factor vector containing the outcome for each
          # sample.
  
  # 2008 pdf
  # y: a numeric or factor vector of outcomes. The function determines the type of problem
    # (classifcation or regression) from the type of the response given in this argument.

  # predict() # ( from TRAIN )
  # Error in `[.data.frame`(out, , obsLevels, drop = FALSE) : 
  #  undefined columns selected 
  # In addition: Warning message:
  #  In train.default(x = GALAXY_L_Train[, -1 * c(1), drop = FALSE],  :
  #                     At least one of the class levels are not valid R variables names; 
  #                   This may cause errors if class probabilities are generated 
  #                   because the variables names will be converted to: X1, X0
   
  train( x = GALAXY_L_Train[,-1*c(1),drop=FALSE] 
    # xts case
    # , y = GALAXY_L_Train[,1,drop=FALSE]
    # non-xts case                                         ( 2 classes; also twoClassSummary )               
    , y = { unlist(GALAXY_L_Train[,1,drop=FALSE]) -> TEMP; NULL -> attr(TEMP, "names"); TEMP } # ;c("one","two") -> levels(TEMP) # ALREADY CARRIED
    , trControl = GALAXY_L_fitControl
  # , method = "gbm" # GOOD TRAIN, TERRIBLE TEST ( NO GOOD FOR SMALL MODELS)
      # tuneGrid <- gbm_grid_smalldata <- expand.grid( shrinkage=0.1, interaction.depth=3, n.trees=150)
      # The dataset size is too small or subsampling rate is too large: nTrain*bag.fraction <= n.minobsinnode
      #   tough here
      #     http://cran.r-project.org/web/packages/gbm/gbm.pdf
  # , method = "rf" # default (DID NOT WORK) TERRIBLE TRAIN, TERRIBLE TEST
    , method = "gbm" # "C5.0" # ( switch to classification only)
    , metric = "ROC" # classification ( stop printing that annoying: The metric "Accuracy" was not in the result set. ROC will be used instead.)
    , verbose = FALSE # gbm TOO MUCH # default is TRUE? anyway?
    # tuneGrid = NULL ( "rf" default?! )
    # tuneGrid = grid # grid <- expand.grid()     # Note: "rf": Tuning Parameters: mtry (#Randomly Selected Predictors)
    # , tuneGrid = expand.grid( trials = 7, model = 'tree', winnow = TRUE ) # C5.0
      , tuneGrid = expand.grid(interaction.depth = seq(1, 7, by = 2),n.trees = seq(100, 1000, by = 50),shrinkage = c(0.01, 0.1)) # gbm
    #  2008 pdf pdf 10: seems if I EXPLICITY define a tuneGrid, then it DOES NOT use TuneLength
    
    # TuneLength ?: By default, train uses a minimal search grid: 3 values for each tuning parameter. 
    # ...: arguments passed to the classification or regression routine (such as 'randomForest'). 
    #      Errors will occur if values for tuning parameters are passed here.
    # , control = C5.0Control(earlyStopping = FALSE) # C5.0
  ) -> GALAXY_L_FitterTune
  
 
  print(GALAXY_L_FitterTune$finalModel)
 
  # (possibly of intest)
  #  GALAXY_L_FitterTune
  #    $ bestTune
  #  
  #  GALAXY_L_FitterTune
  #    $ control
  #    ? $ control
  #      $ call 
  #    ? $ problemType
  #    ? $ tuneValue
 
 
 
  pausepoint <- 1
 
  # on its 'call' ( classification: 2 classes )
  # randomForest 4.6-10
  # Type rfNews() to see new features/changes/bug fixes.
  # note: only 2 unique complexity parameters in default grid. Truncating the grid to 2 .
 
  # NOTE: Heavy "Value" output
  
  # the optimal"model is selected ( 2008 pdf - 8 )
  # print(GALAXY_L_FitterTune$finalModel)
 

  #####################
 
  # ? update.train ...
  
  ## S3 method for class 'train'
  
  # update(object, param = NULL, ...)
    # param: a data frame or named list of all tuning parameters
    # ...: not currently used
    # Value:
       # a new 'train' object
       
  ####################
 
 
  # 'predict' ( use some *statitics* on a date from the past ...  )
 
  # caret_Package_A_Unifed_Interface_for_Predictive_Models(201402)(Kuhn).pdf
  # Prediction and Performance Assessment ( slide 27 of 37 )
  
  # ? predict.randomForest
  
  # > randomForest:::predict.randomForest

  # library(randomForest) predict.randomForest
       ## S3 method for class 'randomForest'
     # predict(object, newdata, type="response",
       # norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE,
       # cutoff, ...)

  ##   newdata ?  ? predict.randomForest
   #     (Note: If not given, the out-of-bag prediction in 'object' is returned.
 
   # predict.all 
       # Should the predictions of all trees be kept?

   # Value:
   # If 'object$type' is 'regression', a vector of predicted values is
   # returned.  If 'predict.all=TRUE', then the returned object is a
   # list of two components: 'aggregate', which is the vector of
   # predicted values by the forest, and 'individual', which is a
   # matrix where each column contains prediction by a tree in the
   # forest.
   
   # if I need to make an xts  ( NOTE ( xts1 - xts2 )/ abs(xts1) MATH DOES/(SHOULD) work
   # 
   # index(GALAXY_L)[GALAXY_L_inTrainingSet_rowid_vector]
   # with Y
   # coredata(GALAXY_L[GALAXY_L_inTrainingSet_rowid_vector])
   # without Y
   # coredata(GALAXY_L[GALAXY_L_inTrainingSet_rowid_vector,-1*c(1)])
 
  # FORM 1 ( 2008 pdf 13)  GALAXY_L_FitterTune$finalModel
  # FORM 2                 GALAXY_L_FitterTune
  # 2008 note: train/test:Descr is the 'matrix of predictors'
  #            train/test:Class is the 'long vector of the outcome'
 
  # ONLY ? extractPrediction DOCS
  ## S3 method for class 'list'
  # predict(object, ...)   # SEE ? predict !!!!
 
  # For 'predict.list', a list results. Each element is produced by
  #     'predict.train'. ( just below ...)
 
  ## S3 method for class 'train' ( classification "raw" or "prob", for the number/class predictions)
  # predict(object, newdata = NULL, type = "raw", na.action = na.omit, ...)
 
  pausepoint <- 1
 
 
 
  # 2008 : he never stores the output for anything
  GALAXY_L_Predict_Test_vector <- predict(GALAXY_L_FitterTune, newdata = GALAXY_L_Test[,-1*c(1),drop=FALSE]) 
  
  # non-xts case classification
  GALAXY_L_Predict_Test_vector_Probs <- predict(GALAXY_L_FitterTune, newdata = GALAXY_L_Test[,-1*c(1),drop=FALSE], type = "prob") 
 
  #   If *want* to compare the outcomes of multiple models 2008 pdf - 13
  #   R> models <- list(svm = svmFit, gbm = gbmFit)
  #   R> testPred <- predict(models, newdata = testDescr)
  #   R> lapply(testPred, function(x) x[1:5]) # FIRST FIVE
  #   $svm
  #   [1] mutagen nonmutagen nonmutagen nonmutagen mutagen
  #   Levels: mutagen nonmutagen
  #   $gbm
  #   [1] mutagen mutagen mutagen nonmutagen mutagen
  #   Levels: mutagen nonmutagen

  ### ###
 
  # ? extractPrediction  ? extractProb ? 2008 pdf 13
  # obtain predictions NOTE: 'model' SHOULD!/must be a list
  # pre-required of ? extractPrediction
  #'fitBest = FALSE'
  # returnData =TRUE 
  # trainControl(. . ., returnData = TRUE(required and default)  
  #                    selectionFunction = "best"(???)) ? trainControl
 
  pausepoint <- 1
 
  extractPrediction(models = list(GFitterTuneITEM1 = GALAXY_L_FitterTune)
    , testX = GALAXY_L_Test[,-1*c(1),drop=FALSE]
    # xts case
    # , testY = GALAXY_L_Test[, 1     ,drop=FALSE]
    # non-xts case                                         ( 2 classes; also twoClassSummary ) 
    , testY = { unlist(GALAXY_L_Test[,1,drop=FALSE]) -> TEMP; NULL -> attr(TEMP, "names"); TEMP } # ;c("one","two") -> levels(TEMP) # ALREADY CARRIED
  ) -> GALAXY_L_Test_predValues 
 
  # only non-xts case    classification
  # caret? S-logic ( of the observation, require at least 2 predictors)
  # otherwise: error ( and others? ) gbm: Error in 1:cCols : argument of length 0
  if (length(colnames(GALAXY_L_Test[,-1*c(1),drop=FALSE])) > 1) {
    extractProb(models = list(GFitterTuneITEM1 = GALAXY_L_FitterTune)
      , testX = GALAXY_L_Test[,-1*c(1),drop=FALSE]
      # xts case
      # , testY = GALAXY_L_Test[, 1     ,drop=FALSE]
      # non-xts case                                         ( 2 classes; also twoClassSummary ) 
      , testY = { unlist(GALAXY_L_Test[,1,drop=FALSE]) -> TEMP; NULL -> attr(TEMP, "names"); TEMP } # ;c("one","two") -> levels(TEMP) # ALREADY CARRIED
    ) -> GALAXY_L_Test_probValues 
  }
 
  # ? extractPrediction: un-named list, the values of 'object' will be "Object1" ... "Object2" and so on
  # 2008 13  : data type (i.e., training, test or unknown). 
  #  UNKNOWN??? unkown? : if unkX (w/wo unkOnly ) is used? ( SEE EXAMPLE: BOTTOM OF ? plotObsVsPred )
  # NOTE: output is sorted "Training" before "Test"
 
  # dataType == "Test" ?? what data has not been run through "train()" ??? ( 2008 pdf 13)
 
  #  str(GALAXY_L_Test_predValues) 
  #  'data.frame':  553 obs. of  5 variables:
  #  $ obs     : num  -0.0696 -0.0548 -0.0392 -0.0502 -0.0553 ...
  #  $ pred    : num  -0.0519 -0.0493 -0.0342 -0.0451 -0.0468 ...
  #  $ model   : Factor w/ 1 level "rf": 1 1 1 1 1 1 1 1 1 1 ...
  #  $ dataType: Factor w/ 2 levels "Test","Training": 2 2 2 2 2 2 2 2 2 2 ...
  #  $ object  : Factor w/ 1 level "GFitterTuneITEM1": 1 1 1 1 1 1 1 1 1 1 ...
   
  # subset of interest  2008 pdf 14 ( base::subset )
 
  subset(GALAXY_L_Test_predValues, dataType == "Test"
  ) -> GALAXY_L_Test_predValues_DataType_Test
 

  # only non-xts case    classification
  # caret? S-logic ( of the observation, require at least 2 predictors)
  # otherwise: error ( and others? ) gbm: Error in 1:cCols : argument of length 0
  if (length(colnames(GALAXY_L_Test[,-1*c(1),drop=FALSE])) > 1) {
    subset(GALAXY_L_Test_probValues, dataType == "Test"
    ) -> GALAXY_L_Test_probValues_DataType_Test
  }
 
  # kept: dataType: Factor w/ 2 levels "Test","Training 
  # AS PART OF the str() definition

 
  # ? extractPrediction 
  # objects can then be passes to 
  #'plotObsVsPred' 
  # plotClassProbs' 2008 pdf 14: plotClassProbs(testProbs). 
  #   # 2008 pdf   : classification models, the function plotClassProbs function
  
  # ? extractPrediction   
  # plotObsVsPred(object, equalRanges = TRUE, ...)
  #   object (preferably from the function 'extractPrediction'
  # For factor outcomes, a dotplot plot is produced
  #  ...: parameters to pass to 'xyplot' or 'dotplot', such as 'auto.key'
 
  # ? plotObsVsPred
  # with dataType == "Test" data on graphic ( default )
 
  ## graphics_bookmark_here <- 1
 
  # rm(list=ls(all.names=TRUE))
  # debugSource('N:/MyVMWareSharedFolder/rcsnsight1/R/main-rcsnsight1-999.R', echo=TRUE)
  # main_rcsnsight1_999()
 
  # rf and ( RECPROUSM156N with pct_changes )
  # disappointingly miss a 15 percent drawdown and a 30 percent drawdown
  # nature of 'randomForest' elminating THAT which does NOT matter ???
  # FIX: work in 'smaller zone' e.g. ( +- a few months of a recession)
 
  #### really? xts case ( regression case )
  ### plotObsVsPred(GALAXY_L_Test_predValues) 
 
  # seems useless in 
  # # non-xts case  classification   ( 2 classes )

  # just Type == "Test"
  # xts case
  # Browse[2]> View(sqldf("select obs, pred, (pred - obs) as abs_less_worse from GALAXY_L_Test_predValues_DataType_Test order by obs" , drv = "RSQLite" ))
 
  # click on zoom
  # Browse[2]> plotObsVsPred(GALAXY_L_Test_predValues) # All?
  # Browse[2]> plotObsVsPred(GALAXY_L_Test_predValues_DataType_Test) # just my Test
  
  # plotObsVsPred(GALAXY_L_Test_predValues_DataType_Test)
  # Browse[2]> plotObsVsPred(GALAXY_L_Test_predValues_DataType_Test)
 
  ##  classification only zone ##
  
  # none seem useful  # classification only
  
  
  
  # only non-xts case    classification
  # caret? S-logic ( of the observation, require at least 2 predictors)
  # otherwise: error ( and others? ) gbm: Error in 1:cCols : argument of length 0
  # if (length(colnames(GALAXY_L_Test[,-1*c(1),drop=FALSE])) > 2) {
    # plotClassProbs(GALAXY_L_Test_probValues_DataType_Test)
    # plotClassProbs(GALAXY_L_Test_probValues_DataType_Test, useObjects = TRUE) # in this case ( SAME )
    # plotClassProbs(GALAXY_L_Test_probValues_DataType_Test,useObjects = TRUE, plotType = "densityplot", auto.key = list(columns = 2))
  # }
  
  # 2008 Kuhn 6. Characterizing performance
  
  # For classifcation models, the functions 
  #  sensitivity(), 
  #    sensitivity is defined as the 
  #    proportion of positive results ( which were actually positive )
  #    out of the number of samples 
  #    ? caret::sensitivity
  #  specificity(),
  #  posPredValue() and 
  #  negPredValue() can be used to characterize performance where there are
  #  two classes. By default, the first level of the outcome factor is used to define the positive"
  #  result, although this can be changed. 
  # ( I currently have '< -0.10' as first level AND"c("one","two") -> levels"  I may want to change this
  
  graphics_bookmark_here_again <- 1
  
  ## Default S3 method:
  #  confusionMatrix(data, reference, positive = NULL, 
  #                  dnn = c("Prediction", "Reference"), 
  #                  prevalence = NULL, ...)
  #  
  #  reference: a factor of classes to be used as the true results
  #  
  #  positive: an optional character string for the factor level that
  #  corresponds to a "positive" result (if that makes sense for
  #                                      your data). If there are only two factor levels, the first
  #  level will be used as the "positive" result.
  #  
  #  > ? confusionMatrix
  #  
  #  library(balance)
  #    majority class is coded as 0 and the minority as 1.
  #  http://cran.r-project.org/web/packages/unbalanced/unbalanced.pdf
  
  # means: levels=c("one","zero") OR 'levels=c(1,0),labels=("one","zero") 
  # (first factor, second factor) # ( dput CODED AS 1L,2L )
 

  print("Percent Correct predictions ...")
  print(with(GALAXY_L_Test_predValues_DataType_Test,{ sum(pred == obs) }) / NROW(GALAXY_L_Test_predValues_DataType_Test))

  # cross validation 10,5    C5.0
  # maybe an expected output?
  #            Reference
  #  Prediction one two
  #  one   7  14
  #  two  54 201
  #  
  #  Accuracy : 0.753623188405797    
 
  # SEE HERE ON HOW TO READ A CONFUSION MATRIX
  # ? caret::sensitivity
 
  # non-xts case classification ONLY  
  print(confusionMatrix(GALAXY_L_Test_predValues_DataType_Test$pred, GALAXY_L_Test_predValues_DataType_Test$obs))
  
  # data.frame(GALAXY_L_Test_predValues_DataType_Test) -> GALAXY_L_Test_predValues_DataType_Test_df 
  #   conn <- sqldf(drv = "SQLite")
  #   sqldf("select * from GALAXY_L_Test_predValues_DataType_Test", connection = conn, row.names = TRUE)
  #   sqldf(
  #     "select *  
  #        from main.GALAXY_L_Test_predValues_DataType_Test 
  #          where obs = 'one'"
  #   , connection = conn, row.names = TRUE)
  #   conn <- NULL
 
  # xts case ( would have to convert back to an xts)
  # xts(GALAXY_L_Test_predValues_DataType_Test, as.Date(row.names(GALAXY_L_Test_predValues_DataType_Test), tz = "UTC" )) -> GALAXY_L_Test_predValues_DataType_Test

  
  # 
  # 'Positive' Class : . . . 
  #  Accuracy : 0.945652173913044  # Therefore 95% failure to predict using "rf" and 577 samples
  #  Accuracy : 0.285714285714286  # Therefore 29% failure to predict using "rf" and 57  samples
 
  ##  end of classification only zone ## LEFT_OFF
 
 #############
 
 ## comparisons 'model .v.s. model - resamples ( see my caret .txt notes )
 
 ###############
 
  save(GALAXY_L, file = "GALAXY_L_0_4.RData")
  
  # main_rcsnsight1_999()
  the_end_debug_bookmark_here <- 1
  # View(last(GALAXY_L,10))
  
  # dbDisconnect(contspgr)
  ## dbDisconnect(condbpgr)
  # dbDisconnect(contssqll)
  ## dbDisconnect(condbsqll)
  ### dbDisconnect(dppgrconn$con)
  # be clean ( if nothing else )
  dbDisconnect(dpsqllconn$con)
  
  Sys.setenv(TZ=oldtz)

  # lineprof ( and any other unclosed connections from prev runs ) will hang
  # 1. close out R-Studio and any other connections
  # 2. NULL out those drivers
  # drvsqll <- NULL
  # drvpgr  <- NULL
  ## dbUnloadDriver(drvsqll)
  ## dbUnloadDriver(drvpgr)
  
  ## drvsqll <- NULL
  ## drvpgr  <- NULL
  # Error in sqliteCloseDriver(drv, ...) : 
    # RS-DBI driver: (there are opened connections -- close them first)
  
  # JUST LET THE 'END OF PROGRAM' CLEAN UP THE CONNECTIONS AND DRIVERS
  
  print("Done with main_rcsnsight1_999()")
    
}
# 
# rm(list=ls(all.names=TRUE))
# source('N:/MyVMWareSharedFolder/rcsnsight1/R/main-rcsnsight1-999.R', echo=TRUE)
# main_rcsnsight1_999()
# View
# View(last(GALAXY_L,10))

# save(GALAXY_L, file = "GALAXY_L_0_4.RData")
# save(GALAXY_L, file = "GALAXY_L_0_4.RData")

# testing: WAY 1 ( will re-load all the libraries each time )
# rcsnsight1\R\tests\testthat>R --file=test-main-rcsnsight1-999.R

# testing: WAY 2 ( BETTER) ( libraries are only loaded once )
# rcsnsight1\R\tests\testthat> R
# > getwd()
# [1] "N:/MyVMWareSharedFolder/rcsnsight1/R/tests/testthat"
# > library(testthat)
# > test_file(path="test-main-rcsnsight1-999.R") 
# LATER DO -helpers.R alone

# performance testing:
# in R Studio: library(lineprof)

  # drvsqll <- NULL
  # drvpgr  <- NULL
# AND
# > rm(list=ls(all.names=TRUE))
# > source('N:/MyVMWareSharedFolder/rcsnsight1/R/main-rcsnsight1-999.R', echo=TRUE)

# > library(lineprof)
# > xsaved <- lineprof(main_rcsnsight1_999(), torture = FALSE) # not memory ( USE THIS )
# > shine(xsaved)

# MOVING MINIMUM (almost) DONE [x] quantmod::lag, apply, and min

# [3/4] FIX 3 4 4 ( only Chavet/Piger lags 3 months ) ( just a 'comment' for right now )

# [X] FIX lapply -> as.xts ( WRONG DEFAULT DATE TIME Posix#, SHOULD BE DATE )
# [X] VERIFY [x] min_over_range ARE correct_numbers

# [X] AVE(like_min) over the LAST 2 [X] Chauvet/Piger %  [FUTURE] OECD 1/0  [FUTURE] NBER 1/0  
# AFTER SLEEP
# [X] CSPC (1,2,3,4,5,6)  %change max drawdown (maxdraw(GSPC)-Cl(GSCPC))/abs(CSPC)
# [X] slightly redone: remove second ticker

# LEFT_OFF
# NEXT: NEED STRUCTURES ( OR PRINTED ZONES ) TO GENERATE 6 OUTCOME 1 MO 2MO ... 6MO ( of maxdrawdawn )
# [X] SOME THINKING IS REQUIRED ( fixed RANGE bug )
# LEFT_OFF
# worst drawdown the next 6 mo
# GSPC                  lag 0 through 6  ( outcome )
# RECESSION INDICATORS:           lag 9(9-10 min ranges)   ... PIGER
# RECESSION INDICATORS:           lag 10(10-11 min ranges) ... NBER OECD

# *** worst drawdown the next 5 mo ( SHIFT ALL NUMBER TO THE LEFT BY 1 UNIT ) ***

# NEXT TIME ( have what I need for a 1 month ahead prediction)
# piger lag 3, back 2 month average
# piger lag 3, back 2 month change
# NBER  lag 4, back 2 month average
# OECD  lag 4, back 2 month change

# NEXT TIME, ... pick columns [ ] ... formuala: if not too difficult [ ]
# send to librar(caret) [ ] xor/or librar(randomForest) [ ]

# MAYBE LATER: NEXT: need hdquantiles [ ] ( THIS WILL BE *ROOT* TICKERS: GSPCQ8 'higher is 'relatively/logically' better ) 
# MAYBE LATER: Harrell-Davis hdquantile [ ] ( OR SOMETING ELSE  ( tis::colQuantiles ) )

# MAYBE LATER: [ ] 8 quantiles of ( CSPC (1,2,3,4,5,6)  %change max drawdown maxdraw(GSPC)-Cl(GSCPC)/abs(CSPC) )  

# MAYBE LATER: [ need YET MORE 'caret' practice on randomForest [ IN PROGRESS] ]

# SAVE TO GITHUB!! ASAP [ ]

# VERIFY MY DATA IS CORRECT ( YES - AGAIN ) [ ]
# CHANGE PREDICT TEST TO 80 % [ ]  
#  HOW WELL PREDICT? GALAXY_L_Predict_vector[1,,drop=FALSE] - GALAXY_L_Test[1,,drop=FALSE] [ ]
### HIGEST - LEFT_OFF_ COMPARE
  # ADD 'NOTSMART' COLUMN 'UNXDATE'  SO I CAN 'sqdf SQL JOIN or xts/zoo merge  ACROSS OR ( OTHER ) [ ] )
# ADD IN hdntile/hdquantile [ ] : See the Ave + Ave + Ave problem

# 'variable importance' ? [ ]
# ( WHAT CAN I KEEP OR ELIMATATE ( IN ADDITION TO CORRELATION: DOES 'PRINCIPAL COMPONENTS HELP? )
# NOTE: Most correleated Ave + Ave + Ave ( see ABOVE: should hdntile ( SUM ndntile(EACH) ) = new predictor
#                        OR JUST KEEP THE 'binary ones USREC + USAREC: and 'ADD THEM UP' = new predictor
# 'findLinearCombos() [ ] My Ave problem'

# Another possibilty 
# if USARECM always covers USAREC ( or I FORCE to cover ) then 
#   the case of 0 + 1 never exists and 1 + 1 = 2 has TRUE value

# NEED 2 ... 6 month LOOK AHEADS ( HIGH LEFT_OFF: STICK IN CODE ) [ ] SOME THINKING IS REQUIRED

# need zoo:rollapply to fix/replace pct_change [ ]
### rcsnsight(2)?
# zoo::rollapply() and quantmod ClCl() Cl() and Recall() could SOLVE my 'Change Problems'
###

# Possibly WRONG data ( my input is for RECESSION ( not MARKET_CRASH_FROM_HISTERIA )
# Probally need to add some some "consumer sentiment surveys instead", manufacture survey, VIX
# Possibly need a model that can do outliers well ( see my pdf on 'randomForest and lasso' )

# 201402 pdf ( JUST REPLACED 'method = "rf"' WITH method = "gbm" )
# method = "gbm" TERRIBLE TRAIN, TERRIBLE TEST 
# method = "rf" GOOD TRAIN, TERRIBLE TEST

# instead of graphs need sqldf() where graph output would be


# pick the 20 worst months over the last 46 years and just try to predict those
# more (social import) variables 
#    importance, fscaret (feature selection)
# and library(caretEnsemble) - run many!

# SEE *LEFT_OFF* IN THE SOURCE CODE
# * TRY A [X]"gbm"/[ ]"C5.0" LOWER/RAISE THOSE PARAMETERS 
# [X] Change Prediction Criteria to 2-mo ave/3-mo ave ... 6-mo ave
# [X] Put all 3 factors in there and run throu "pca" and "isa"(sp)


# [x]does library(qmao) have it? getEconomicCalendarBriefing
# 
# https://github.com/gsee/qmao/blob/master/R/getCalendar.R

# [ ] caretensemlble ?
# [ ] fscaret ?



