
# main-rcsnsight2-999.R

# R version 3.4.0 (2017-04-21)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# I ACTUALLY *RAN THESE THREE LINES IN *R TERM*
# install.packages("checkpoint") 
# library(checkpoint) 
# ? checkpoint  

# checkpoint(snapshotDate, project = getwd(), R.version, 
#           scanForPackages = TRUE, checkpointLocation = "~/", verbose = TRUE,
#           use.knitr = system.file(package = "knitr") != "") 

# install.packages("quantmod")  
# curl
# quantmod
# TTR # direct call
# xts # direct call
# zoo # direct call
# 
# install.packages("FinancialInstrument") # actually NEED
# 
# install.packages("gbm") # direct call ( through caret )
# install.packages("mondate") # direct call
# install.packages("Holidays") # actually NEED
# install.packages("TimeWarp") # direct call
# install.packages("XML") # direct call ???
# install.packages("rvest") # direct call
# 
# stringi
# Rcpp
# BH
# jsonlite
# mime
# openssl
# R6
# stringr
# xml2
# httr
# selectr
# magrittr
# 
# install.packages("functional") # Curry # direct call
# 
# install.packages("caret") # direct call
# 
# colorspace
# minqa
# nloptr
# RcppEigen
# RColorBrewer
# dichromat
# munsell
# labeling
# rlang
# lme4
# SparseM
# MatrixModels
# digest
# gtable
# scales
# tibble
# lazyeval
# pbkrtest
# quantreg
# iterators
# ggplot2
# car
# foreach
# plyr
# ModelMetrics
# reshape2
# 
# install.packages("matrixStats")
# 
# install.packages("dplyr")
# assertthat
# DBI

# ( NOTE: REORGANIZE THESE 'checkpoint' NOTES )

# https://github.com/RevolutionAnalytics/checkpoint

# Introducing the Reproducible R Toolkit and the checkpoint package
# October 13, 2014
# http://blog.revolutionanalytics.com/2014/10/introducing-rrt.html
# 
# VIDEO ( HE DOES IT INTERACTIVELY )
# Reproducibility with Revolution R Open and the Checkpoint Package
# March 24, 2015
# http://www.revolutionanalytics.com/webinars/reproducibility-revolution-r-open-and-checkpoint-package

# MAKE SURE getwd() IS IN THE CORRECT DIRECTORY with the .R file(s)

# INSTALLING
# checkpoint("2015-04-25", R.version = "3.2.0")
# COMMON EVERYDAY DEBUGGING
# I do not want it to scan every time
### checkpoint("2015-04-25", R.version = "3.2.0", scanForPackages = FALSE)

# SCANS THIS DIRECTORY FOR .R files

# note: make sure that THIS FILE is closed by ALL applications
# else RTERM may get this error
#   > checkpoint("2015-04-25")
#   Scanning for packages used in this project
#   |======================================================================| 100%
#   - Discovered 0 packages
#   Unable to parse 1 files:
#     - main-rcsnsight2-999.R
#   No packages found to install
#   checkpoint process complete
#   ---

# if an error, close THIS FILE
# rerun
# checkpoint("2015-04-25", R.version = "3.2.0")

# NOTE : WAIT A WHILE, IT IS 'NOT HUNG'
#  IT DOWNLOADS ALL PACKAGES, HANGS A WHILE,  THEN IT INSTALLS THEM

# EVERYTHING GOES HERE ( R IN 64 BIT MODE )
#   W:\New_Economics\rcsnsight1.320_assistance\RDebug\Home\.checkpoint\2015-04-25\lib\x86_64-w64-mingw32\3.2.0
# EXCEPT compiler package
#   W:\New_Economics\rcsnsight1.320_assistance\RDebug\Home\.checkpoint\R-3.2.0\compiler

# DONE WHEN
# checkpoint process complete


# > getOption("repos")
# [1] "http://mran.revolutionanalytics.com/snapshot/2015-04-25"

# > .libPaths()
# [1] "W:/New_Economics/rcsnsight1.320_assistance/RDebug/Home/.checkpoint/2015-04-25/lib/x86_64-w64-mingw32/3.2.0"
# [2] "W:/New_Economics/rcsnsight1.320_assistance/RDebug/Home/.checkpoint/R-3.2.0"

# Note, does not have any problem finding 'library' 'base and recommended'
# > library(MASS)
# OK

# but '.checkpoint' is in '.libPaths' but 'library' is NOT
# gbm needs 'survival' but it is already a 'base package'
# but checkpoint re-downloads it into its 'checkpoint' directory

# 1 package is needed for this model and is not installed. (gbm). Would you like to try to install it now?
# 1: yes
# 2: no
# no - I chose, I manually wrote require(gbm)
#      and let checkpoint() automatically 
# install 1. survival(base) and 2. gbm(dynically installed by caret) 
# it into the '.checkpoint' directory

# ---

# main-rcsnsight2-999.R 

# ###########################   
# IMPORTANT
#
# GOOD ENOUGH FOR NOW
#
# (THIS) Works well gbm - 7,500,0.01
#
# FUTURE try gbm combos
# trees 500/1000
# interaction depth = 7
# learn rate 0.05,0.20
#
#############################  

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width 

# setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R") # TO BE CHANGED LATER 
### I AM ALREADY THERE ### setwd("W:/New_Economics/rcsnsight1.340")

bookmark_here <- 1


# put this early before xts is added to the search 
# JUST 'pulls old dates forward!!'
pullAheadZOOData <- function(zooobj,monthsPullAhead) {
  
  require(mondate)
  require(zoo)
  
  index(zooobj) -> dates
  
  # zoo::as.Date  # pull 'past date' 'monthsPullAhead positions' into the 'current position' 
  as.Date(as.character(as.mondate(dates, displayFormat="%Y-%m-%d",timeunits="months") + monthsPullAhead)) -> index(zooobj) 
  return(zooobj)
  
}

# WARNING
# NOTE:  subtractOffDaysSpec = # is persistently stored on .RData
# THEREFORE, if I change this value I MUST delete the .Rdata files 
#            then get BRAND NEW data from the external source

retrieveSymbolsQuantmodRdata_OLD <- function(
  # "^GSPC" 
  finSymbol # financial Symbol 
  # "1950-03-01"
  , finSymbolRemoteSource = "Quantmod_yahoo"
  , finSymbolAttributes = c("Close")
  , initDate 
  # some minor testing 1980 
  , subtractOffDaysSpec        # if the date received is the 'first of the month'       
  #   then I want it to be the last of the previous month: (0 or -1) days           
  # index ^GSPC
  , intlCurrency = "USD"
  , interpolate = FALSE 
) {
  require("xts")
  require("Holidays")
  require("TimeWarp")
  require("quantmod")
  require("FinancialInstrument")
  # just in case is an index
  finSymbolNOIndex <- sub("\\^","", finSymbol)
  if( file.exists(paste0(getwd(),"/",finSymbolNOIndex,".Rdata"))){
    load(file = paste0(getwd(),"/",finSymbolNOIndex,".Rdata"), envir = environment())
    return(get(finSymbolNOIndex))
  } else {
    initDateSafelyBackOneMo <- as.character(dateWarp(as.Date(initDate), -1, by='months'))
    if(finSymbolRemoteSource == "Quantmod_yahoo") {
      # YAHOO delay requirement
      Sys.sleep(0.80)
      suppressWarnings(suppressMessages(getSymbols(finSymbol,from=initDateSafelyBackOneMo,src='yahoo', index.class=c("POSIXt","POSIXct"))))
    } 
    if(finSymbolRemoteSource == "Quantmod_FRED") {
      # NO KNOWN delay requirement # NO Sys.sleep
      suppressWarnings(suppressMessages(getSymbols(finSymbol,from=initDateSafelyBackOneMo, src = "FRED", index.class=c("POSIXt","POSIXct") )))
    } 
    if( !(finSymbolRemoteSource %in% c("Quantmod_yahoo","Quantmod_FRED")) ) {
      stop(paste0("retrieveRdata does not know finSymbolRemoteSource = ", finSymbolRemoteSource))
    }
    # remove 'index-ing' symbols, then save a raw file
    save(list = c(finSymbolNOIndex),file = paste0(getwd(),"/",finSymbolNOIndex,"_RAW.Rdata"))
    symbols <- c(finSymbolNOIndex)
    # register to be like a stock
    currency(intlCurrency)
    stock(symbols[1], currency=intlCurrency,multiplier=1)  # FinancialInstrument ( NOT REALLY A STOCK )
    symbol <- symbols
    x <- get(symbol)
    # SOMETIMES by luck always the last calendar day of the month
    index(x) <- dateWarp(date=index(x),spec=subtractOffDaysSpec,by="days") # subtract off DaysSpec (0 or -1) days
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE) # faber.R
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x) <- '%Y-%m-%d'
    # benchmark specific
    x <- x[,paste0("x.",finSymbolAttributes)]  # remove quantmod o/h ( remove(keep) non-desired attributes )
    for(X in paste0("x.",finSymbolAttributes)) {
      x[,X] <- as.numeric(x[,X])              # garantee numeric
    }
    colnames(x)<-gsub("x",symbol,colnames(x))
    
    # fill in missing values
    if( interpolate == TRUE ) {
      
      # create month-end calendar dates from the first through the last
      xts( ,order.by = as.Date(c(dateSeq(from = index(first(x)), to = index(last(x)) , by="months", k.by = 1 ) - 1,index(last(x)))) ) -> monthsxts
      
      # make lots of NAs
      merge.xts(monthsxts,x, join="left")            -> x
      
      # merge.xt will prefix column(s)? with an "X".  Remove this "X"
      sub("^X","",colnames(x)) -> colnames(x)
      
      # interpolate(na.approx) to locf(na.locf) 
      na.locf(merge.xts(monthsxts,x, join="left")) -> x
      
      # merge.xt will prefix column(s)? with an "X".  Remove this "X"
      sub("^X","",colnames(x)) -> colnames(x)
      
    }
    
    assign(symbol,x)
    save(list = c(finSymbolNOIndex),file = paste0(getwd(),"/",finSymbolNOIndex,".Rdata"))
    return(get(symbol))
  }
}

# WARNING
# NOTE:  subtractOffDaysSpec = # is persistently stored on .RData
# THEREFORE, if I change this value I MUST delete the .Rdata files 
#            then get BRAND NEW data from the external source


retrieveSymbolsQuantmodRdata <- function(
  # "^GSPC" 
  finSymbol # financial Symbol 
  # "1950-03-01"
  , finSymbolRemoteSource = "Quantmod_yahoo"
  # NEW
  , finSymbolRemoteSourcePath = NULL
  , finSymbolNewCoreDatum = NULL
  , finSymbolNewIndexStr  = NULL
  # RESUME
  , finSymbolAttributes = c("Close")
  , initDate 
  # some minor testing 1980 
  , subtractOffDaysSpec        # if the date received is the 'first of the month'       
  #   then I want it to be the last of the previous month: (0 or -1) days           
  # index ^GSPC
  , intlCurrency = "USD"
  , interpolate = FALSE 
) {
  require("xts")
  require("Holidays")
  require("TimeWarp")
  require("quantmod")
  require("FinancialInstrument")
  # just in case is an index
  finSymbolNOIndex <- sub("\\^","", finSymbol)
  if( file.exists(paste0(getwd(),"/",finSymbolNOIndex,".Rdata"))){
    load(file = paste0(getwd(),"/",finSymbolNOIndex,".Rdata"), envir = environment())
    return(get(finSymbolNOIndex))
  } else {
    initDateSafelyBackOneMo <- as.character(dateWarp(as.Date(initDate), -1, by='months'))
    if(finSymbolRemoteSource == "Quantmod_yahoo") {
      # YAHOO delay requirement
      Sys.sleep(0.80)
      suppressWarnings(suppressMessages(getSymbols(finSymbol,from=initDateSafelyBackOneMo,src='yahoo', index.class=c("POSIXt","POSIXct"))))
    } 
    if(finSymbolRemoteSource == "Quantmod_FRED") {
      # NO KNOWN delay requirement # NO Sys.sleep
      suppressWarnings(suppressMessages(getSymbols(finSymbol,from=initDateSafelyBackOneMo, src = "FRED", index.class=c("POSIXt","POSIXct") )))
    } 
    if(finSymbolRemoteSource == "Quantmod_FRED_RData") {
      # NO KNOWN delay requirement # NO Sys.sleep
      load(file = finSymbolRemoteSourcePath)
      assign(finSymbol, rbind(get(finSymbol),xts(finSymbolNewCoreDatum,zoo::as.Date(finSymbolNewIndexStr))) )
    } 
    if( !(finSymbolRemoteSource %in% c("Quantmod_yahoo","Quantmod_FRED","Quantmod_FRED_RData")) ) {
      stop(paste0("retrieveRdata does not know finSymbolRemoteSource = ", finSymbolRemoteSource))
    }
    # remove 'index-ing' symbols, then save a raw file
    save(list = c(finSymbolNOIndex),file = paste0(getwd(),"/",finSymbolNOIndex,"_RAW.Rdata"))
    symbols <- c(finSymbolNOIndex)
    # register to be like a stock
    currency(intlCurrency)
    stock(symbols[1], currency=intlCurrency,multiplier=1)  # FinancialInstrument ( NOT REALLY A STOCK )
    symbol <- symbols
    x <- get(symbol)
    # SOMETIMES by luck always the last calendar day of the month
    index(x) <- dateWarp(date=index(x),spec=subtractOffDaysSpec,by="days") # subtract off DaysSpec (0 or -1) days
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE) # faber.R
    x <- x[paste0(substr(initDate,1,7),'::')]
    indexFormat(x) <- '%Y-%m-%d'
    # benchmark specific
    x <- x[,paste0("x.",finSymbolAttributes)]  # remove quantmod o/h ( remove(keep) non-desired attributes )
    for(X in paste0("x.",finSymbolAttributes)) {
      x[,X] <- as.numeric(x[,X])              # garantee numeric
    }
    colnames(x)<-gsub("x",symbol,colnames(x))
    
    # fill in missing values
    if( interpolate == TRUE ) {
      
      # create month-end calendar dates from the first through the last
      xts( ,order.by = as.Date(c(dateSeq(from = index(first(x)), to = index(last(x)) , by="months", k.by = 1 ) - 1,index(last(x)))) ) -> monthsxts
      
      # make lots of NAs
      merge.xts(monthsxts,x, join="left")            -> x
      
      # merge.xt will prefix column(s)? with an "X".  Remove this "X"
      sub("^X","",colnames(x)) -> colnames(x)
      
      # interpolate(na.approx) to locf(na.locf) 
      na.locf(merge.xts(monthsxts,x, join="left")) -> x
      
      # merge.xt will prefix column(s)? with an "X".  Remove this "X"
      sub("^X","",colnames(x)) -> colnames(x)
      
    }
    
    assign(symbol,x)
    save(list = c(finSymbolNOIndex),file = paste0(getwd(),"/",finSymbolNOIndex,".Rdata"))
    return(get(symbol))
  }
}



bookmark_here <- 1



# # OLD CODE AND NEW CODE ( BOTH SAME # SCRAPE ( HTML HAS CHANGED ) #
# # THIS IS 'OBSOLETE' # I CAN GET THIS DIRECTLY FROM QUANDL(THE AUTHOR POSTS THERE)
# #
# # REMOVED: BECAUSE (NOT USED) # DOES NOT HELP MY 'PREDICTIONS': SUPER POOR INFLUENCE #
# #
# # rvest_0.2.0  # 'html' is deprecated. Use 'read_html' instead. # xml2_1.1.1 #  read_html
# # XML_3.98-1.1 # readHTMLTable ...                              # rvest_0.3.2  # html_table
# 
# 
# getSymbols.multpl("SandP.500.12.month.EPS"
#                   , finSymbolAttribute = "Close" # finSymbolAttribute 
#                   , interpolate = TRUE 
# ) -> x
# > str(x)
# An 'xts' object on 1871-01-31/2016-12-31 containing:
#   Data: num [1:1752, 1] 7.85 7.62 7.51 7.79 7.97 8.1 8.1 8.23 8.03 7.91 ...
# - attr(*, "dimnames")=List of 2
# ..$ : NULL
# ..$ : chr "Value......................Value.Close"
# Indexed by objects of class: [Date] TZ: UTC
# xts Attributes:  
#   NULL

# # TO BE 'EDITED,DEBUGGED and TESTED' in ( main_rcsnsight1_999_miniplay5  )

# getSymbols.multpl <- function(
#     symbolText
#   , finSymbolAttribute = "Close" # if only ONE column
#   , interpolate = FALSE
# ) {
#   
#   # Main page
#   # http://www.multpl.com/sitemap
#   
#   # More pages
#   # http://www.multpl.com/sitemap/world-economic-stats
#   
#   #   (SEEMS RECESSION SENSITIVE)
#   
#   #   S&P 500 Earnings Per Share. ( "12.month.EPS" )
#   #   http://www.multpl.com/s-p-500-earnings/table?f=m
#   #   
#   #   S&P 500 Real Earnings Growth by Quarter ( "SandP.500.Real.Earnings.Growth.Pct" )
#   #   http://www.multpl.com/s-p-500-real-earnings-growth/table/by-quarter
#   #   
#   #   S&P 500 PE Ratio by Month ( MATH) ( SandP.500.PE.Ratio )
#   #   Price to earnings ratio, based on trailing twelve month __AS_REPORTED__
#   #   http://www.multpl.com/table?f=m
#   
#   #   S&P 500 Book Value Per Share by Quarter ( "SandP.500.BV.Per.Share" )
#   #   S&P 500 book value per share ___ non-inflation adjusted current dollars. 
#   #   http://www.multpl.com/s-p-500-book-value/table/by-quarter
#   
#   # web site and owner
#   # multpl.com  JOSHSTAIGER@GMAIL.COM ( in California )
#   # https://www.easywhois.com/
#   
#   # After I signed up for email, the following was returned
#   # multpl.com
#   # 1130 Shoreline Dr
#   # San Mateo, California 94404
#   
#   if(is.null(symbolText)) stop("missing arg symbolText in function getSymbols.multp")
#   
#   # create, later will be 'tested for null'
#   NULL -> url
#   
#   # which web page data do I want?
#   if(symbolText == "SandP.500.12.month.EPS")              "http://www.multpl.com/s-p-500-earnings/table?f=m" -> url
#   
#   # Annual percentage change in 12 month
#   if(symbolText == "SandP.500.Real.Earnings.Growth.Pct")  "http://www.multpl.com/s-p-500-real-earnings-growth/table/by-quarter" -> url
#   if(symbolText == "SandP.500.PE.Ratio")                  "http://www.multpl.com/table?f=m" -> url
#   if(symbolText == "SandP.500.BV.Per.Share")              "http://www.multpl.com/s-p-500-book-value/table/by-quarter" -> url
#   # OTHERS ( in future follow ) ..
#   # if
#   # if
#   
#   if(is.null(url)) stop(paste0("symbolText: ", symbolText ," url not found by function getSymbols.multp"))
#   
#   require(XML)     # NEED readHTMLTable
#   # Hadley Wickham # web scraping 
#   require(rvest)   # imports XML  masked from __PACKAGE_XML_xml__
#   # IF uncommented : require(XML), USE: XML::xml to access XML::xml
#   require(xts)     # as.xts STUFF
#   
#   require(TimeWarp) # create month-end calendar dates from the first through the last
#   
#   # NO KNOWN requirement ( but I do not want to be pest and be kicked OUT)
#   Sys.sleep(1.0)
#   
#   # # rvest_0.2.0  # 'html' is deprecated. Use 'read_html' instead. ... # xml2_1.1.1 #  read_html
#   ## html(url) -> found 
#   read_html(url) -> found
#   
#   # content="multpl" expected
#   if(!grepl("multpl",html_text(found))) stop(paste0("seems internal url not(no_longer) correct for ", symbolText," in function getSymbols.multpl"))
#   
#   # get that data
#   
#   # # XML_3.98-1.1 # readHTMLTable ... #  rvest_0.3.2  # html_table
#   ## readHTMLTable(html_node(found,"#datatable")
#   ##               , stringsAsFactors = FALSE
#   ## ) -> prexts 
#   html_table(found)[[1]] -> prexts
#   
#   # NOTE: SOME other TABLES have
#   #   some other non-numeric characters "commas", "slashes" 
#   # TO DO: clean_out AS NEEDED
#   
#   # if a first cell of a non-Date column has the text '\n estimate(red)' remove this
#   for(cn in colnames(prexts)) { 
#     if(cn != "Date") 
#       if(grepl("estimate",prexts[1,cn])) { 
#         gsub("estimate","",prexts[1,cn]) -> prexts[1,cn]
#         gsub("\n"      ,"",prexts[1,cn]) -> prexts[1,cn]
#         gsub(" "       ,"",prexts[1,cn]) -> prexts[1,cn]
#       }
#   }
#   
#   # check if this is a "Percent table" if so create a suffix
#   for(cn in colnames(prexts)) { 
#     if(cn != "Date") 
#       if(any(grepl("%",prexts[,cn]))) { 
#         paste0( colnames(prexts)[colnames(prexts) %in% cn],".Pct") -> colnames(prexts)[colnames(prexts) %in% cn]
#       }
#   }
#   
#   # in the 'not the Date' columns, if a 'percent sign' exists remove it'
#   for(cn in colnames(prexts)) { 
#     if(cn != "Date") gsub("%","",prexts[,cn]) -> prexts[,cn] 
#   }
#   
#   # make columns other than 'Date' to be numeric
#   for(cn in colnames(prexts)) { if(cn != "Date") as.numeric(prexts[,cn]) -> prexts[,cn] }
#   
#   # convert 'Jan 31, 1872' to friendly general date format YYYY-MM-DD
#   as.character(strptime(prexts[,"Date"],format='%b %d, %Y')) -> prexts[,"Date"]
#   
#   # create 'data.frame to xts' expected row.names
#   prexts[,"Date"] -> row.names(prexts)
#   
#   # remove NOT useful Date column ( safer form )
#   prexts[,which(!(colnames(prexts) %in% "Date")), drop = FALSE] -> prexts
#   
#   # make R-like column names (somthing character to dots)
#   gsub(" ", "."  , colnames(prexts)) -> colnames(prexts)
#   gsub("&", "and", colnames(prexts)) -> colnames(prexts)
#   
#   # when only ONE column, add a named " finSymbolAttribute"
#   if(NCOL(prexts) == 1) {
#     paste0(colnames(prexts)[1],".",finSymbolAttribute) -> colnames(prexts)[1]
#   }
#   
#   # convert to xts # SEE ? as.xts.methods
#   as.xts(prexts, dateFormat="Date") -> nowxts
#   
#   # fill in missing values
#   if( interpolate == TRUE ) {
#     
#     # create month-end calendar dates from the first through the last
#     xts( ,order.by = as.Date(c(dateSeq(from = index(first(nowxts)), to = index(last(nowxts)) , by="months", k.by = 1 ) - 1,index(last(nowxts)))) ) -> monthsxts
#     
#     # make lots of NAs
#     merge.xts(monthsxts,nowxts, join="left")            -> nowxts
#     
#       # merge.xt will prefix column(s)? with an "X".  Remove this "X"
#       sub("^X","",colnames(nowxts)) -> colnames(nowxts)
#     
#     # interpolate(na.approx) to locf(na.locf)
#     na.locf(merge.xts(monthsxts,nowxts, join="left")) -> nowxts
#     
#       # merge.xt will prefix column(s)? with an "X".  Remove this "X"
#       sub("^X","",colnames(nowxts)) -> colnames(nowxts)
#     
#   }
#   
#   # TEMPORARILY hardcoded until I find a more elegant solution
#   # REALLY 'should' have BEEN put through the QUAUNTSTRAT to.monthly fixer method
#   if(symbolText == "SandP.500.PE.Ratio") {
#     
#     # these dates are the '1st of the month'
#     # these dates also include an extra 'last observation within-month partial estimate'
#     
#     # remove THAT 'last observation within-month partial estimate'
#     # but I REALLY should DETECT this
#     nowxts[1:(NROW(nowxts)-1)] -> nowxts
#     
#     # since at the '1st of the month' subtract off one day
#     dateWarp(date=index(nowxts),spec=-1,by="days") -> index(nowxts) 
#     
#     return(nowxts)
#   }
#   
#   return(nowxts)
# }
# 
# 
# retrieveSymbolsmultplRdata <- function(
#     finSymbol
#   , finSymbolAttribute 
# ) {
#   
#   if( file.exists(paste0(getwd(),"/",finSymbol,".",finSymbolAttribute,".Rdata"))){
#     load(file = paste0(getwd(),"/",finSymbol,".",finSymbolAttribute,".Rdata"), envir = environment())
#     return(get(paste0(finSymbol,".",finSymbolAttribute)))
#   } else {
#     
#     if ( finSymbol == "SP500.12M.EPS" ) {
#       getSymbols.multpl("SandP.500.12.month.EPS"
#                         , finSymbolAttribute = finSymbolAttribute 
#                         , interpolate = TRUE 
#       ) -> x
#     }
#     
#     if ( finSymbol == "SP500.REAL.EARN.GR.PCT" ) {
#       getSymbols.multpl("SandP.500.Real.Earnings.Growth.Pct"
#                         , finSymbolAttribute = finSymbolAttribute 
#                         , interpolate = TRUE 
#       ) -> x
#     }
#     
#     if ( finSymbol == "SP500.PE.RATIO" ) {
#       getSymbols.multpl("SandP.500.PE.Ratio"
#                         , finSymbolAttribute = finSymbolAttribute 
#                         , interpolate = FALSE 
#       ) -> x
#       
#     }
#       
#     if ( finSymbol == "SP500.BV.PER.SHARE" ) {
#       getSymbols.multpl("SandP.500.BV.Per.Share"
#                           , finSymbolAttribute = finSymbolAttribute 
#                           , interpolate = TRUE 
#       ) -> x
#     }
# 
#     
#     symbol <- paste0(finSymbol,".",finSymbolAttribute)
#     
#     assign(symbol,x)
#     save(list = c(symbol),file = paste0(getwd(),"/",finSymbol,".",finSymbolAttribute,".Rdata"))
#     return(get(symbol))
#     
#   }
#   
# }



main_rcsnsight2_999 <- function(THESEED = 1,pauseat=NULL) {
  
  main_rcsnsight2_999_inner <- function(...) {
  
    set.seed(THESEED)
    
    bookmarkhere <- 1
    
    ### I AM ALREADY THERE ### setwd("W:/New_Economics/rcsnsight1.340")
    
    oldtz <- Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    
    ## require(DMwR) NOT YET
    # require(performanceEstimation) LATER ?!!
    # require(caret) ## CLOSE FUTURE

    require(functional) # Curry
    
    require(mondate) # diff.mondate to calculate pullAheadZOODataMonthShiftAmount
    
    require(Holidays)
    require(TimeWarp)
    
    ## DANGEROUSLY commented out because checkpoint CAN NOT find quantstrat(R-forge)
    ## require(quantstrat) # and added to search() path: 
    #  blotter, PerformanceAnalytics, FinancialInstrument,
    #  quantmod, TTR, xts, zoo
    # IN PLACE OF require(quantstrat)
    require(zoo)
    require(xts)
    require(TTR)
    require(quantmod)
    
    # findLinearCombos
    require(caret) # and added to search() path:
    #  ggplot2, lattice 
    
    # require(require(PerformanceAnalytics) # POSSIBLE FUT ( and through require("quantstrat") )
    
    # every function
    list() -> ALL.OBSERVEES
    list() -> ALL.OBSERVEESFUNCTIONS
    list() -> ALL.OVER.OBSERVEESFUNCTIONS
    list() -> ALL.PREDICTEESFUNCTIONS
    list() -> ALL.PREDICTEES.HARD.CODED
    
    
    # NOTE: PROGRAM (MAY) NOT (REALISTICALLY) WORK IF DATE IS BEFORE "1950-03-01/1950-03-31"
    # NOTE: "1950-03-01" IS STILL HARD_CODED in PLACES
    initData.TestTrain.Global.Earliest <- "1950-03-31"
    
    # HARD NOTE
    # NOTE: this IS NEVER the CURRENT MONTH 
    #  1. 'all zoo shifts expect end of prev month' AND 
    #  2. incomplete data exists of current month 
    # THEREFORE
    # this IS       the end of the PREVIOUS MONTH
    # finDate.TestTrain.Global.Latest    <- "2017-11-30"  # 2014-12-31(perfect) 
                                                          # march 21, 2015 run: "2015-01-31": Warning message: In to.period(x, "months", indexAt = indexAt, name = name, ...) : missing values removed from data
                                                          # march 21, 2015 run: "2015-02-28": Warning message: In to.period(x, "months", indexAt = indexAt, name = name, ...) : missing values removed from data
                                                          # march 21, 2015 run: "2015-03-31": Warning message: In to.period(x, "months", indexAt = indexAt, name = name, ...) : missing values removed from data
    finDate.TestTrain.Global.Latest      <- "2017-11-30"  # april  6, 2015 run: "2015-03-31": Warning message: In to.period(x, "months", indexAt = indexAt, name = name, ...) : missing values removed from data
    
    # training and TRUE tests
    list(Test2001 = list(Train=list(initDate = initData.TestTrain.Global.Earliest,finDate ="1998-12-31"),
                         Test=list(initDate = "1999-01-01",finDate = finDate.TestTrain.Global.Latest),
                         ListName="Test2001"
    ),
    Test2008 = list(Train=list(initDate = initData.TestTrain.Global.Earliest,finDate ="2003-12-31"),
                    Test=list(initDate="2004-01-31",finDate = finDate.TestTrain.Global.Latest),
                    ListName="Test2008"
    )
    ) -> TestTrainDates
    
    # max range : total ; training and TRUE tests initData.TestTrain.Global.Earliest to finDate.TestTrain.Global.Latest
    xts( 
      , order.by = as.Date(c(dateSeq(
        from = index(xts(,as.Date((initData.TestTrain.Global.Earliest))))
        , to = index(xts(,as.Date((finDate.TestTrain.Global.Latest)))) 
        , by="months", k.by = 1 ) - 1 
        ,index(  xts(,as.Date((finDate.TestTrain.Global.Latest)))   ))) 
    ) -> MaxAllTestTrainMonthEnds
    
    MaxAllTestTrainMonthEndsRange <- paste0(initData.TestTrain.Global.Earliest,"::",finDate.TestTrain.Global.Latest)
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "UMCSENT1"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
      , interpolate = TRUE
    ) -> UMCSENT1
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "UMCSENT"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
      , interpolate = TRUE
    ) -> UMCSENT
    
    "UMSENT.HIST.FRED" -> colnames(UMCSENT1) 
    "UMSENT.HIST.FRED" -> colnames(UMCSENT) 
    
    # zoo no.locf
    # cbind.xts
    # one month patch
    xts(as.numeric(NA),as.Date("1977-11-30")) -> xtspatch
    "UMSENT.HIST.FRED" -> colnames(xtspatch)
    na.locf(rbind(UMCSENT1, xtspatch, UMCSENT))  -> UMSENT.HIST.FRED 
    
    bookmark_here <- 1
    
    # LEFT_OFF above TRYING TO WORK IN UMICH SENTIMENT SURVEY
    # 
    # ### CURRENTLY - NOT USED 'UMICH SENTIMENT SURVEY' ###
    #
    
    ## SUN JUN 04 2017 # old quantmod "# but yahoo URLS/method has changed (LATE MAY/JUN 2017 ) # no longer loading # ^GSPC
    ## SUN JUN 04 2017 # INDEXES ^GSPC do not seem to work any more ( compLaints from Quantmod issues )
    
    #     # S&P500 from yahoo  
    #     
    #     retrieveSymbolsQuantmodRdata(
    #         finSymbol = "^GSPC"
    #       , finSymbolAttributes = c("Close","Low")
    #       , initDate = "1950-03-01"
    #       , subtractOffDaysSpec = 0
    #     ) -> GSPC.DELAYZERO.ABS     # head "1950-03-31"
    #     
    #     # really meant for a monthly
    #     as.integer(diff.mondate(c(
    #       as.mondate(tail(index(GSPC.DELAYZERO.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
    #       as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    #     ))) -> pullAheadZOODataMonthShiftAmount
    #     
    #     merge.xts(MaxAllTestTrainMonthEnds,GSPC.DELAYZERO.ABS) -> GSPC.DELAYZERO.ABS
    #     GSPC.DELAYZERO.ABS[MaxAllTestTrainMonthEndsRange] -> GSPC.DELAYZERO.ABS
    #     
    #     assign("GSPC.DELAYZERO.ABS", value=GSPC.DELAYZERO.ABS, envir = .GlobalEnv) 
    #     
    #     # if HAD BEEN a Daily
    #     max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    #     
    #     pullAheadZOOData(GSPC.DELAYZERO.ABS,pullAheadZOODataMonthShiftAmount) ->   GSPC.DELAYZERO.ABS.ADJUSTNOW
    #     print(paste0("GSPC pullAheadZOOData should be DELAYZERO"," Actual: ",pullAheadZOODataMonthShiftAmount))
    #     
    #     merge.xts(MaxAllTestTrainMonthEnds,GSPC.DELAYZERO.ABS.ADJUSTNOW) -> GSPC.DELAYZERO.ABS.ADJUSTNOW
    #     GSPC.DELAYZERO.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> GSPC.DELAYZERO.ABS.ADJUSTNOW
    #     
    #     # specifics 
    #     Cl(GSPC.DELAYZERO.ABS.ADJUSTNOW) -> GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE
    #     Lo(GSPC.DELAYZERO.ABS.ADJUSTNOW) -> GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW
    #     assign("GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE", value=GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE, envir = .GlobalEnv)
    #     assign("GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW"  , value=GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW  , envir = .GlobalEnv)
    #     
    #     "GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE" -> ALL.OBSERVEES["GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE"] 
    #     "GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW"   -> ALL.OBSERVEES["GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW"] 
    
    bookmark_here <- 1
    
    # NBER ( x5 month 'behind' )
    
    # 1 in a recession, 0 not in a recession
    
    # NO LONGER UPDATING
    # first interpretation, known as the midpoint method ( USRECP )
    # http://research.stlouisfed.org/fred2/series/USRECM
    
    # trough method
    # 5 month lag ( would have seen 2008 rececession ( 2008-01-01 1) at ( 2008-06-01) 2008-05-31
    # http://research.stlouisfed.org/fred2/data/USREC.txt
    
    # peak method ( EARLIEST WARNING )
    # 5 month lag ( would have seen 2008 rececession ( 2007-12-01 1) at ( 2008-05-01) 2008-04-30
    # http://research.stlouisfed.org/fred2/data/USRECP.txt
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "USRECP"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> USRECP.DELAYSEVEN.ABS      # head "1854-12-01" 
                                    # monthly
                                    # ( Last Updated:  2014-09-18 - random date - typically 7 mo late WITH 7 mo old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(USRECP.DELAYSEVEN.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,USRECP.DELAYSEVEN.ABS) -> USRECP.DELAYSEVEN.ABS
    
    assign("USRECP.DELAYSEVEN.ABS", value=USRECP.DELAYSEVEN.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(USRECP.DELAYSEVEN.ABS,pullAheadZOODataMonthShiftAmount) -> USRECP.DELAYSEVEN.ABS.ADJUSTNOW
    print(paste0("USRECP pullAheadZOOData should be DELAYSEVEN"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,USRECP.DELAYSEVEN.ABS.ADJUSTNOW) -> USRECP.DELAYSEVEN.ABS.ADJUSTNOW
    USRECP.DELAYSEVEN.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> USRECP.DELAYSEVEN.ABS.ADJUSTNOW
    
    assign("USRECP.DELAYSEVEN.ABS.ADJUSTNOW", value=USRECP.DELAYSEVEN.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "USRECP.DELAYSEVEN.ABS.ADJUSTNOW" -> ALL.OBSERVEES["USRECP.DELAYSEVEN.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    # Chauvet/Piger ( 3 month delay ) ( sometimes 2 month delay )  
    
    # http://research.stlouisfed.org/fred2/data/RECPROUSM156N.txt 
    
    # 'probability of a recession'  
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "RECPROUSM156N"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> RECPROUSM156N.DELAYTHREE.ABS    # head "1967-06-01" ( Last Updated: 2015-04-01 12:53 PM CDT)
                                         # monthly
                                         # ( Last Updated:   2015-04-01 12:53 PM CDT - ??? date - typically 4 mo late WITH 4 mo old date )
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(RECPROUSM156N.DELAYTHREE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    
    merge.xts(MaxAllTestTrainMonthEnds,RECPROUSM156N.DELAYTHREE.ABS) -> RECPROUSM156N.DELAYTHREE.ABS
    
    assign("RECPROUSM156N.DELAYTHREE.ABS", value=RECPROUSM156N.DELAYTHREE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(RECPROUSM156N.DELAYTHREE.ABS,pullAheadZOODataMonthShiftAmount) -> RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW
    print(paste0("RECPROUSM156N pullAheadZOOData should be DELAYTHREE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW) -> RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW
    RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW
    
    assign("RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW", value=RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW"]
    
    #     Title:               Gross Domestic Product
    #     Series ID:           GDP
    #     Source:              US. Bureau of Economic Analysis
    #     Release:             Gross Domestic Product
    #     Seasonal Adjustment: Seasonally Adjusted Annual Rate
    #     Frequency:           Quarterly
    #     http://research.stlouisfed.org/fred2/data/GDP.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "GDP"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
      , interpolate = TRUE 
    ) -> GDP.DELAYSIX.ABS      # head "1947-01-01"
                               # Quarterly
                               # ( Last Updated: 2015-03-27 8:01 AM CDT - ??? date - typically 7 mo late WITH 7 mo old date in 3 mo chunks )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(GDP.DELAYSIX.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,GDP.DELAYSIX.ABS) -> GDP.DELAYSIX.ABS

    assign("GDP.DELAYSIX.ABS", value=GDP.DELAYSIX.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(GDP.DELAYSIX.ABS,pullAheadZOODataMonthShiftAmount) -> GDP.DELAYSIX.ABS.ADJUSTNOW
    print(paste0("GDP pullAheadZOOData should be DELAYSIX"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,GDP.DELAYSIX.ABS.ADJUSTNOW) -> GDP.DELAYSIX.ABS.ADJUSTNOW
    GDP.DELAYSIX.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> GDP.DELAYSIX.ABS.ADJUSTNOW
    
    assign("GDP.DELAYSIX.ABS.ADJUSTNOW", value=GDP.DELAYSIX.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "GDP.DELAYSIX.ABS.ADJUSTNOW" -> ALL.OBSERVEES["GDP.DELAYSIX.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #     Title:               10-Year Treasury Constant Maturity Rate
    #     Series ID:           GS10
    #     Source:              Board of Governors of the Federal Reserve System
    #     Release:             H.15 Selected Interest Rates
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Monthly
    #     http://research.stlouisfed.org/fred2/data/GS10.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "GS10"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> GS10.DELAYONE.ABS        # head  "1953-04-01"
                                  # Monthly
                                  # ( Last Updated: 2015-04-06 3:41 PM CDT - ??? date - typically 1 mo late WITH 1 mo old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(GS10.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,GS10.DELAYONE.ABS) -> GS10.DELAYONE.ABS

    assign("GS10.DELAYONE.ABS", value=GS10.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(GS10.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> GS10.DELAYONE.ABS.ADJUSTNOW
    print(paste0("GS10 pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,GS10.DELAYONE.ABS.ADJUSTNOW) -> GS10.DELAYONE.ABS.ADJUSTNOW
    GS10.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> GS10.DELAYONE.ABS.ADJUSTNOW
    
    assign("GS10.DELAYONE.ABS.ADJUSTNOW", value=GS10.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "GS10.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["GS10.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    
    #     Title:               3-Month Treasury Constant Maturity Rate
    #     Series ID:           DGS3MO
    #     Source:              Board of Governors of the Federal Reserve System (US)
    #     Release:             H.15 Selected Interest Rates
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Daily
    #     Units:               Percent
    #     http://research.stlouisfed.org/fred2/data/DGS3MO.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "DGS3MO"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = 0
    ) -> DGS3MO.DELAYZERO.ABS      # head "1982-01-04" 
                                   # Daily
                                   # ( Last Updated:  015-04-17 3:31 PM CDT - ??? date - typically 1 day late WITH 1 day old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(DGS3MO.DELAYZERO.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount

    merge.xts(MaxAllTestTrainMonthEnds,DGS3MO.DELAYZERO.ABS) -> DGS3MO.DELAYZERO.ABS
    DGS3MO.DELAYZERO.ABS[MaxAllTestTrainMonthEndsRange] -> DGS3MO.DELAYZERO.ABS
    
    assign("DGS3MO.DELAYZERO.ABS", value=DGS3MO.DELAYZERO.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(DGS3MO.DELAYZERO.ABS,pullAheadZOODataMonthShiftAmount) ->  DGS3MO.DELAYZERO.ABS.ADJUSTNOW
    print(paste0("DGS3MO pullAheadZOOData should be DELAYZERO"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,DGS3MO.DELAYZERO.ABS.ADJUSTNOW) -> DGS3MO.DELAYZERO.ABS.ADJUSTNOW
    DGS3MO.DELAYZERO.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> DGS3MO.DELAYZERO.ABS.ADJUSTNOW
    
    assign("DGS3MO.DELAYZERO.ABS.ADJUSTNOW", value=DGS3MO.DELAYZERO.ABS.ADJUSTNOW, envir = .GlobalEnv)
    "DGS3MO.DELAYZERO.ABS.ADJUSTNOW" -> ALL.OBSERVEES["DGS3MO.DELAYZERO.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    # TO_DO [ ]: slope of the yield curve ( {T}_{t}) corresponds to the 
    #   difference between the 10-year bond rate and the 3-month T-bill rate. 
    # The slope of the curve is considered the best predictor of recessions among the yield curve components. 
    # yield curve has inverted before five out of six recessions 
    
    # A Dynamic Factor Model of the Yield Curve as a Predictor of the Economy*
    # Marcelle Chauvet1
    # University of California Riverside
    # Zeynep Senyuz2
    # Federal Reserve Board
    # March 2012
    # http://www.federalreserve.gov/pubs/feds/2012/201232/index.html
    
    # (SPREAD) difference between yields on 10-year Treasury bonds and 3-month Treasury bills
    # David S. Matteson
    # Cornell University
    # http://www.rinfinance.com/agenda/2014/talk/DavidMatteson.pdf
    
    #
    # relies on ( just above ) GS10.DELAYONE.ABS.ADJUSTNOW and DGS3MO.DELAYZERO.ABS
    
    GS10.DELAYONE.ABS.ADJUSTNOW - DGS3MO.DELAYZERO.ABS -> DIFF.GS10.DGS3MO.DELAYONE.ABS.ADJUSTNOW
    
    "DIFF.GS10.DGS3MO.Close"                           ->   colnames(DIFF.GS10.DGS3MO.DELAYONE.ABS.ADJUSTNOW)[1]
    
    assign("DIFF.GS10.DGS3MO.DELAYONE.ABS.ADJUSTNOW", value= DIFF.GS10.DGS3MO.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "DIFF.GS10.DGS3MO.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["DIFF.GS10.DGS3MO.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #     Title:               Moody's Seasoned Baa Corporate Bond Minus Federal Funds Rate
    #     Series ID:           BAAFFM
    #     Source:              Federal Reserve Bank of St. Louis
    #     Release:             Interest Rate Spreads (Not a Press Release)
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Percent
    #     https://research.stlouisfed.org/fred2/data/BAAFFM.txt
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "BAAFFM"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> BAAFFM.DELAYONE.ABS      # head "1954-07-01"                          
                                  # Monthly
                                  # ( Last Updated:  2015-04-06 3:56 PM CDT - ??? date - typically 1 month late WITH 1 month old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(BAAFFM.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,BAAFFM.DELAYONE.ABS) -> BAAFFM.DELAYONE.ABS
    BAAFFM.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> BAAFFM.DELAYONE.ABS
    
    assign("BAAFFM.DELAYONE.ABS", value=BAAFFM.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(BAAFFM.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) ->  BAAFFM.DELAYONE.ABS.ADJUSTNOW
    print(paste0("BAAFFM pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,BAAFFM.DELAYONE.ABS.ADJUSTNOW) -> BAAFFM.DELAYONE.ABS.ADJUSTNOW
    BAAFFM.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> BAAFFM.DELAYONE.ABS.ADJUSTNOW
    
    assign("BAAFFM.DELAYONE.ABS.ADJUSTNOW", value=BAAFFM.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)

    "BAAFFM.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["BAAFFM.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #     Title:               Moody's Seasoned Aaa Corporate Bond Minus Federal Funds Rate
    #     Series ID:           AAAFFM
    #     Source:              Federal Reserve Bank of St. Louis
    #     Release:             Interest Rate Spreads (Not a Press Release)
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Percent
    #     https://research.stlouisfed.org/fred2/data/AAAFFM.txt
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "AAAFFM"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> AAAFFM.DELAYONE.ABS      # head "1954-07-01"                           
                                  # Monthly
                                  # ( Last Updated:  2015-04-06 3:56 PM CDT - ??? date - typically 1 month late WITH 1 month old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(AAAFFM.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,AAAFFM.DELAYONE.ABS) -> AAAFFM.DELAYONE.ABS
    AAAFFM.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> AAAFFM.DELAYONE.ABS
    
    assign("AAAFFM.DELAYONE.ABS", value=AAAFFM.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(AAAFFM.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) ->  AAAFFM.DELAYONE.ABS.ADJUSTNOW
    print(paste0("AAAFFM pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,AAAFFM.DELAYONE.ABS.ADJUSTNOW) -> AAAFFM.DELAYONE.ABS.ADJUSTNOW
    AAAFFM.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> AAAFFM.DELAYONE.ABS.ADJUSTNOW
    
    assign("AAAFFM.DELAYONE.ABS.ADJUSTNOW", value=AAAFFM.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "AAAFFM.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["AAAFFM.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    # Andre Also found that this SWUNG hard before the 2008 recession
    
    BAAFFM.DELAYONE.ABS.ADJUSTNOW - AAAFFM.DELAYONE.ABS.ADJUSTNOW  -> DIFF.BAAFFM.AAAFFM.DELAYONE.ABS.ADJUSTNOW
    
    "DIFF.BAAFFM.AAAFFM.Close"                            -> colnames(DIFF.BAAFFM.AAAFFM.DELAYONE.ABS.ADJUSTNOW)[1]
    
    assign("DIFF.BAAFFM.AAAFFM.DELAYONE.ABS.ADJUSTNOW", value= DIFF.BAAFFM.AAAFFM.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "DIFF.BAAFFM.AAAFFM.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["DIFF.BAAFFM.AAAFFM.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    
    #     Title:               Effective Federal Funds Rate
    #     Series ID:           DFF
    #     Source:              Board of Governors of the Federal Reserve System (US)
    #     Release:             H.15 Selected Interest Rates
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Daily, Seven Day
    #     Units:               Percent
    #     
    #     interest rate at which depository
    #     institutions trade federal funds (balances held at Federal Reserve
    #                                       Banks) with each other overnight.
    #     
    #     http://research.stlouisfed.org/fred2/data/DFF.txt
    
    # ( Discount Rate ) ( Federal Funds Rate )
    # Historical Changes of the Target Federal Funds and Discount Rates
    # http://www.newyorkfed.org/markets/statistics/dlyrates/fedrate.html
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "DFF"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = 0
    ) -> DFF.DELAYZERO.ABS          # head "1954-07-01" 
                                    # Daily  
                                    # ( Last Updated:  2015-04-17 3:31 PM CDT - ??? date - typically 1 day late WITH 1 day old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(DFF.DELAYZERO.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,DFF.DELAYZERO.ABS) -> DFF.DELAYZERO.ABS
    DFF.DELAYZERO.ABS[MaxAllTestTrainMonthEndsRange] -> DFF.DELAYZERO.ABS
    
    assign("DFF.DELAYZERO.ABS", value=DFF.DELAYZERO.ABS, envir = .GlobalEnv)

    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(DFF.DELAYZERO.ABS,pullAheadZOODataMonthShiftAmount) ->   DFF.DELAYZERO.ABS.ADJUSTNOW
    print(paste0("DFF pullAheadZOOData should be DELAYZERO"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,DFF.DELAYZERO.ABS.ADJUSTNOW) -> DFF.DELAYZERO.ABS.ADJUSTNOW
    DFF.DELAYZERO.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> DFF.DELAYZERO.ABS.ADJUSTNOW
    
    assign("DFF.DELAYZERO.ABS.ADJUSTNOW", value=DFF.DELAYZERO.ABS.ADJUSTNOW, envir = .GlobalEnv)
    "DFF.DELAYZERO.ABS.ADJUSTNOW" -> ALL.OBSERVEES["DFF.DELAYZERO.ABS.ADJUSTNOW"]

    bookmark_here <- 1
    
    # maybe useful for some math somewhere
    
    #     Title:               Unemployed
    #     Series ID:           UNEMPLOY
    #     Source:              US. Bureau of Labor Statistics
    #     Release:             Employment Situation
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Thousands of Persons
    #     Date Range:          1948-01-01 to 2014-12-01
    #     
    #     http://research.stlouisfed.org/fred2/data/UNEMPLOY.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "UNEMPLOY"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> UNEMPLOY.DELAYONE.ABS       # head "1948-01-01" 
                                     # Monthly
                                     # ( Last Updated:  2015-04-03 7:56 AM CDT- ??? date - typically 1 month late WITH 1 month old date )

    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(UNEMPLOY.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,UNEMPLOY.DELAYONE.ABS) -> UNEMPLOY.DELAYONE.ABS
    UNEMPLOY.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> UNEMPLOY.DELAYONE.ABS
    
    assign("UNEMPLOY.DELAYONE.ABS", value=UNEMPLOY.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(UNEMPLOY.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> UNEMPLOY.DELAYONE.ABS.ADJUSTNOW
    print(paste0("UNEMPLOY pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,UNEMPLOY.DELAYONE.ABS.ADJUSTNOW) -> UNEMPLOY.DELAYONE.ABS.ADJUSTNOW
    UNEMPLOY.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> UNEMPLOY.DELAYONE.ABS.ADJUSTNOW
    
    assign("UNEMPLOY.DELAYONE.ABS.ADJUSTNOW", value=UNEMPLOY.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "UNEMPLOY.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["UNEMPLOY.DELAYONE.ABS.ADJUSTNOW"]
    
    # pattern: flattens out before a recession
    
    #     Title:               Median Duration of Unemployment
    #     Series ID:           UEMPMED
    #     Source:              US. Bureau of Labor Statistics
    #     Release:             Employment Situation
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Weeks
    #     
    #     http://research.stlouisfed.org/fred2/data/UEMPMED.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "UEMPMED"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> UEMPMED.DELAYONE.ABS        # head "1967-07-01"
                                     # Monthly
                                     # ( Last Updated: 2015-04-03 7:56 AM CDT  - ??? date - typically 1 month late WITH 1 month old date )

    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(UEMPMED.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,UEMPMED.DELAYONE.ABS) -> UEMPMED.DELAYONE.ABS
    UEMPMED.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> UEMPMED.DELAYONE.ABS
    
    assign("UEMPMED.DELAYONE.ABS", value=UEMPMED.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(UEMPMED.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> UEMPMED.DELAYONE.ABS.ADJUSTNOW
    print(paste0("UEMPMED pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,UEMPMED.DELAYONE.ABS.ADJUSTNOW) -> UEMPMED.DELAYONE.ABS.ADJUSTNOW
    UEMPMED.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> UEMPMED.DELAYONE.ABS.ADJUSTNOW
    
    assign("UEMPMED.DELAYONE.ABS.ADJUSTNOW", value=UEMPMED.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "UEMPMED.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["UEMPMED.DELAYONE.ABS.ADJUSTNOW"]
    
    # maybe useful for some math somewhere
    
    #     Title:               Total Population: All Ages including Armed Forces Overseas
    #     Series ID:           POP
    #     Source:              US. Bureau of the Census
    #     Release:             Monthly National Population Estimates (Not a Press Release)
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Thousands
    #     Date Range:          1952-01-01 to 2014-11-01
    #     
    #     http://research.stlouisfed.org/fred2/data/POP.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "POP"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> POP.DELAYTWO.ABS         # head "1952-01-01"
                                  # Monthly
                                  # ( Last Updated: 2015-04-08 8:51 AM CDT - ??? date - typically 1 month late WITH 1 month old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(POP.DELAYTWO.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,POP.DELAYTWO.ABS) -> POP.DELAYTWO.ABS
    POP.DELAYTWO.ABS[MaxAllTestTrainMonthEndsRange] -> POP.DELAYTWO.ABS
    
    assign("POP.DELAYTWO.ABS", value=POP.DELAYTWO.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(POP.DELAYTWO.ABS,pullAheadZOODataMonthShiftAmount) -> POP.DELAYTWO.ABS.ADJUSTNOW
    print(paste0("POP pullAheadZOOData should be DELAYTWO"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,POP.DELAYTWO.ABS.ADJUSTNOW) -> POP.DELAYTWO.ABS.ADJUSTNOW
    POP.DELAYTWO.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> POP.DELAYTWO.ABS.ADJUSTNOW
    
    assign("POP.DELAYTWO.ABS.ADJUSTNOW", value=POP.DELAYTWO.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "POP.DELAYTWO.ABS.ADJUSTNOW" -> ALL.OBSERVEES["POP.DELAYTWO.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #  to make useful math 
    
    # FROM ABOVE
    
    #     Title:               Unemployed
    #     Series ID:           UNEMPLOY
    #     Source:              US. Bureau of Labor Statistics
    #     Release:             Employment Situation
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Thousands of Persons
    #     Date Range:          1948-01-01 to 2014-12-01
    #     
    #     http://research.stlouisfed.org/fred2/data/UNEMPLOY.txt
    
    # FROM ABOVE
    
    #     Title:               Total Population: All Ages including Armed Forces Overseas
    #     Series ID:           POP
    #     Source:              US. Bureau of the Census
    #     Release:             Monthly National Population Estimates (Not a Press Release)
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Thousands
    #     Date Range:          1952-01-01 to 2014-11-01
    #     
    #     http://research.stlouisfed.org/fred2/data/POP.txt
    
    UNEMPLOY.DELAYONE.ABS.ADJUSTNOW / POP.DELAYTWO.ABS.ADJUSTNOW * 100 -> CALC.UNEMPLOY.OVER.POP.ADJUSTNOW
    
    "CALC.UNEMPLOY.OVER.POP.ADJUSTNOW.Close"                          -> colnames(CALC.UNEMPLOY.OVER.POP.ADJUSTNOW)[1]
    
    assign("CALC.UNEMPLOY.OVER.POP.ADJUSTNOW", value=CALC.UNEMPLOY.OVER.POP.ADJUSTNOW, envir = .GlobalEnv)
    
    "CALC.UNEMPLOY.OVER.POP.ADJUSTNOW" -> ALL.OBSERVEES["CALC.UNEMPLOY.OVER.POP.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #     Title:               Civilian Unemployment Rate
    #     Series ID:           UNRATE
    #     Source:              US. Bureau of Labor Statistics
    #     Release:             Employment Situation
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Percent
    #     
    #     http://research.stlouisfed.org/fred2/data/UNRATE.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "UNRATE"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> UNRATE.DELAYONE.ABS         # head "1948-01-01"
                                     # Monthly
                                     # ( Last Updated: 2015-04-03 7:56 AM CDT - ??? date - typically 1 month late WITH 1 month old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(UNRATE.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,UNRATE.DELAYONE.ABS) -> UNRATE.DELAYONE.ABS
    UNRATE.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> UNRATE.DELAYONE.ABS
    
    assign("UNRATE.DELAYONE.ABS", value=UNRATE.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(UNRATE.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> UNRATE.DELAYONE.ABS.ADJUSTNOW
    print(paste0("UNRATE pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,UNRATE.DELAYONE.ABS.ADJUSTNOW) -> UNRATE.DELAYONE.ABS.ADJUSTNOW
    UNRATE.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> UNRATE.DELAYONE.ABS.ADJUSTNOW
    
    assign("UNRATE.DELAYONE.ABS.ADJUSTNOW", value=UNRATE.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "UNRATE.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["UNRATE.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #     Of Total Unemployed, Percent Unemployed 27 Weeks and Over
    #     Monthly, Seasonally Adjusted, LNS13025703, Updated: 2015-04-03 8:38 AM CDT 
    #     http://research.stlouisfed.org/fred2/series/LNS13025703
    #     
    #     Title:               Of Total Unemployed, Percent Unemployed 27 Weeks and Over
    #     Series ID:           LNS13025703
    #     Source:              US. Bureau of Labor Statistics
    #     Release:             Employment Situation
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Percent
    #     Date Range:          1948-01-01 to 2015-04-01
    #     Last Updated:        2015-05-08 8:57 AM CDT
    #     Notes:               The series comes from the 'Current Population Survey (Household Survey)'
    #     
    #     http://research.stlouisfed.org/fred2/data/LNS13025703.txt
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "LNS13025703"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> LNS13025703.DELAYONE.ABS         # head "1948-01-01"
                                          # Monthly
                                          # ( Last Updated: 2015-05-08 - ??? date - typically 1 month late WITH 1 month old date )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(LNS13025703.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,LNS13025703.DELAYONE.ABS) -> LNS13025703.DELAYONE.ABS
    LNS13025703.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> LNS13025703.DELAYONE.ABS
    
    assign("LNS13025703.DELAYONE.ABS", value=LNS13025703.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(LNS13025703.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> LNS13025703.DELAYONE.ABS.ADJUSTNOW
    print(paste0("LNS13025703 pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,LNS13025703.DELAYONE.ABS.ADJUSTNOW) -> LNS13025703.DELAYONE.ABS.ADJUSTNOW
    LNS13025703.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> LNS13025703.DELAYONE.ABS.ADJUSTNOW
    
    assign("LNS13025703.DELAYONE.ABS.ADJUSTNOW", value=LNS13025703.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "LNS13025703.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["LNS13025703.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #     Title:               Consumer Price Index for All Urban Consumers: All Items Less Food & Energy
    #     Series ID:           CPILFESL
    #     Source:              US. Bureau of Labor Statistics
    #     Release:             Consumer Price Index
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     
    #     http://research.stlouisfed.org/fred2/data/CPILFESL.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "CPILFESL"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> CPILFESL.DELAYONE.ABS       # head "1957-01-01"
                                     # Monthly
                                     # ( Last Updated: 2015-04-17 9:20 AM CDT - ??? date - typically 1 month late WITH 1 month old date )
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(CPILFESL.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,CPILFESL.DELAYONE.ABS) -> CPILFESL.DELAYONE.ABS
    CPILFESL.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> CPILFESL.DELAYONE.ABS
    
    assign("CPILFESL.DELAYONE.ABS", value=CPILFESL.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(CPILFESL.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> CPILFESL.DELAYONE.ABS.ADJUSTNOW
    print(paste0("CPILFESL pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,CPILFESL.DELAYONE.ABS.ADJUSTNOW) -> CPILFESL.DELAYONE.ABS.ADJUSTNOW
    CPILFESL.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> CPILFESL.DELAYONE.ABS.ADJUSTNOW
    
    assign("CPILFESL.DELAYONE.ABS.ADJUSTNOW", value=CPILFESL.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "CPILFESL.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["CPILFESL.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    ### prob not useful: would need TOTAL_PRIV_DEBT TOTAL_PUBLIC_DEBT ###
    
    # abrupt start in 2002( and does not go back that far )
    # but still 'numerically useful' best to left join with OTHERS
    
    #     Title:               U.S. Treasury securities held by the Federal Reserve: All Maturities
    #     Series ID:           TREAST
    #     Source:              Board of Governors of the Federal Reserve System (US)
    #     Release:             H.4.1 Factors Affecting Reserve Balances
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Weekly, As of Wednesday
    #     Units:               Millions of Dollars
    #     Date Range:          2002-12-18 to 2015-01-14
    #     
    #     http://research.stlouisfed.org/fred2/data/TREAST.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "TREAST"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = 0
    ) -> TREAST.DELAYZERO.ABS     # head "2002-12-18" 
                                  # Weekly
                                  # ( Last Updated: 2015-04-16 3:46 PM CDT - ??? date - typically 2 days late WITH 2 days old date in 7 day chunks )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(TREAST.DELAYZERO.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,TREAST.DELAYZERO.ABS) -> TREAST.DELAYZERO.ABS
    TREAST.DELAYZERO.ABS[MaxAllTestTrainMonthEndsRange] -> TREAST.DELAYZERO.ABS
    
    assign("TREAST.DELAYZERO.ABS", value=TREAST.DELAYZERO.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(TREAST.DELAYZERO.ABS,pullAheadZOODataMonthShiftAmount) ->  TREAST.DELAYZERO.ABS.ADJUSTNOW
    print(paste0("TREAST pullAheadZOOData should be DELAYZERO"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,TREAST.DELAYZERO.ABS.ADJUSTNOW) -> TREAST.DELAYZERO.ABS.ADJUSTNOW
    TREAST.DELAYZERO.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> TREAST.DELAYZERO.ABS.ADJUSTNOW
    
    assign("TREAST.DELAYZERO.ABS.ADJUSTNOW", value=TREAST.DELAYZERO.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "TREAST.DELAYZERO.ABS.ADJUSTNOW" -> ALL.OBSERVEES["TREAST.DELAYZERO.ABS.ADJUSTNOW"]
    
    
    # smooth start in 2002 ( otherwise zero before that )
    
    #     Title:               Mortgage-backed securities held by the Federal Reserve: All Maturities
    #     Series ID:           MBST
    #     Source:              Board of Governors of the Federal Reserve System (US)
    #     Release:             H.4.1 Factors Affecting Reserve Balances
    #     Seasonal Adjustment: Not Seasonally Adjusted
    #     Frequency:           Weekly, As of Wednesday
    #     Units:               Millions of Dollars
    #     Date Range:          2002-12-18 to 2015-01-14
    #     
    #     http://research.stlouisfed.org/fred2/data/MBST.txt
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "MBST"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = 0
    ) -> MBST.DELAYZERO.ABS          # head "2002-12-18" 
                                     # Weekly
                                     # ( Last Updated: 2015-04-16 3:46 PM CDT - ??? date - typically 2 days late WITH 2 days old date in 7 day chunks )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(MBST.DELAYZERO.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,MBST.DELAYZERO.ABS) -> MBST.DELAYZERO.ABS
    MBST.DELAYZERO.ABS[MaxAllTestTrainMonthEndsRange] -> MBST.DELAYZERO.ABS
    
    assign("MBST.DELAYZERO.ABS", value=MBST.DELAYZERO.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(MBST.DELAYZERO.ABS,pullAheadZOODataMonthShiftAmount) ->  MBST.DELAYZERO.ABS.ADJUSTNOW
    print(paste0("MBST pullAheadZOOData should be DELAYZERO"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,MBST.DELAYZERO.ABS.ADJUSTNOW) -> MBST.DELAYZERO.ABS.ADJUSTNOW
    MBST.DELAYZERO.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> MBST.DELAYZERO.ABS.ADJUSTNOW
    
    assign("MBST.DELAYZERO.ABS.ADJUSTNOW", value=MBST.DELAYZERO.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "MBST.DELAYZERO.ABS.ADJUSTNOW" -> ALL.OBSERVEES["MBST.DELAYZERO.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    ### END OF prob not useful: would need TOTAL_PRIV_DEBT TOTAL_PUBLIC_DEBT ###
    
    # most recessions, there is a dip before theh recession
    
    #     Title:               Industrial Production Index                          
    #     Series ID:           INDPRO
    #     Source:              Board of Governors of the Federal Reserve System
    #     Release:             G.17 Industrial Production and Capacity Utilization
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Index 2007=100
    #     Date Range:          1919-01-01 to 2014-10-01
    #     Last Updated:        2014-11-17 8:26 AM CST
    
    #     http://research.stlouisfed.org/fred2/data/INDPRO.txt
    
    # real output for all facilities located in the United States
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "INDPRO"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> INDPRO.DELAYONE.ABS        # head "1919-01-01"
                                    # Monthly
                                    # ( Last Updated: 2015-04-15 11:28 AM CDT - ??? date - typically 1 month late WITH 1 month old date  )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(INDPRO.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,INDPRO.DELAYONE.ABS) -> INDPRO.DELAYONE.ABS
    INDPRO.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> INDPRO.DELAYONE.ABS
    
    assign("INDPRO.DELAYONE.ABS", value=INDPRO.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(INDPRO.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> INDPRO.DELAYONE.ABS.ADJUSTNOW
    print(paste0("INDPRO pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,INDPRO.DELAYONE.ABS.ADJUSTNOW) -> INDPRO.DELAYONE.ABS.ADJUSTNOW
    INDPRO.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> INDPRO.DELAYONE.ABS.ADJUSTNOW
    
    assign("INDPRO.DELAYONE.ABS.ADJUSTNOW", value=INDPRO.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "INDPRO.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["INDPRO.DELAYONE.ABS.ADJUSTNOW"]
    
    
    # ISM Manufacturing Index
    # -----------------------
    #   
    #   Institute for Supply Management Data To Be Removed from FRED
    # Posted on June 16, 2016
    # 
    # On June 24, FRED will no longer include data from the Institute for Supply Management. 
    # All 22 series from the Manufacturing ISM Report on Business and the 
    # Non-Manufacturing ISM Report on Business will 
    # be deleted from the FRED database, 
    # https://news.research.stlouisfed.org/2016/06/institute-for-supply-management-data-to-be-removed-from-fred/
    #   
    #   Posted in FRED Announcements
    # https://news.research.stlouisfed.org/category/fred-announcements/
    #   
    #   ( BEST PAGE - JUST ONE MONTH )
    # ISM Manufacturing Index: Continuing Expansion in June
    # July 1, 2016
    # by Jill Mislinski
    # http://www.advisorperspectives.com/dshort/updates/ISM-Manufacturing
    # 
    # "The June _PMI_ registered 53.2 percent, an increase of 1.9 percentage points 
    # from the May reading of 51.3 percent.
    # 
    # Today the Institute for Supply Management published its 
    # 
    # monthly Manufacturing Report for June. 
    # registered 53.2
    # 
    # http://www.ism.ws/ISMReport/MfgROB.cfm?navItemNumber=12942
    # 
    # REDIRECTS TO ( ORIGINAL )
    # GOOGLE SEARCH ( TOP LINK ): 'June 2016 Manufacturing ISM Report On Business'
    # '          Manufacturing ISM Report On Business'
    # https://www.instituteforsupplymanagement.org/ISMReport/MfgROB.cfm?navItemNumber=12942
    # June 2016 Manufacturing ISM Report On Business
    # FOR RELEASE: July 1, 2016
    # 
    # PMI registered 53.2 percent
    # 
    # Apr 2016	 50.8
    # May 2016	 51.3
    # Jun 2016	 53.2
    # 
    # The latest headline PMI was 53.2 percent, ( JUL 01 2016 )
    # an increase of 1.9 percent from the previous month and above the Investing.com forecast of 51.4.
    # 
    # 52.6 ( AUG 01 2016 ) REPORTS  X.Y.Z ON DAY MONTH 01, YEAR FOR 'MONTH-1': OLD_FRED AS YEAR-'MONTH-1'-01
    #
    # ISM Manufacturing Index: Continuing Expansion in June
    # July 1, 2016
    # by Jill Mislinski
    # http://www.advisorperspectives.com/dshort/updates/ISM-Manufacturing # SHOWS ONLY THE MOST RECENT
    #    LINK TO THE real report
    #  GOOGLE SEARCH: "Latest Manufacturing ROB" ( HAS IT HERE )
    #  of ISM Report On Business / Latest Manufacturing ROB
    # 
    # # OLD FRED
    # > load(file="W:\\New_Economics\\rcsnsight1.320\\Data160603\\NAPM_RAW.Rdata")
    # 
    # # THIS IS NOT THE
    # ISM Manufacturing: PMI Composite Index
    # https://www.quandl.com/data/FRED/NAPM-ISM-Manufacturing-PMI-Composite-Index # FRED/NAPM
    
    # THIS IS THE DEFAULT *OVERALL* PMI  ( not 'Production' SUBITEM )
    #                PMI Composite Index ( SEEN NOV 03 2016)
    # > tail(NAPM) # https://www.quandl.com/data/ISM/MAN_PMI-PMI-Composite-Index # ISM/MAN_PMI
    # NAPM
    # 2015-12-01 48.0
    # 2016-01-01 48.2
    # 2016-02-01 49.5
    # 2016-03-01 51.8
    # 2016-04-01 50.8
    # 2016-05-01 51.3
    # 
    # SO 2016-06-01   53.2    REPORTED ON  2016-07-01
    
    #     Title:               ISM Manufacturing: PMI Composite Index
    #     Series ID:           NAPM
    #     Source:              Institute for Supply Management
    #     Release:             Manufacturing ISM Report on Business
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Index
    #     Date Range:          1948-01-01 to 2014-12-01
    #     
    #     http://research.stlouisfed.org/fred2/data/NAPM.txt
    
    # A PMI reading above 50 percent indicates that the manufacturing
    # economy is generally expanding; below 50 percent that it is generally
    # declining.  
    
    # OLD
    # retrieveSymbolsQuantmodRdata(
    #     finSymbol = "NAPM"
    #   , finSymbolRemoteSource = "Quantmod_FRED"
    #   , finSymbolAttributes = c("Close")
    #   , initDate = "1950-03-01"
    #   , subtractOffDaysSpec = -1
    # ) -> NAPM.DELAYONE.ABS             # head "1948-01-01"
    #                                    # Monthly
    #                                    # ( Last Updated: 2015-04-01 9:06 AM CDT - ??? date - typically 1 month late WITH 1 month old date  )

    #     I SCREWED UP
    #     Maybe Quandl has the wrong data
    #     REMOVE NAPM - not a siqnficant indicator ( not worth the headache )
    #     
    #     > load(file=".\\Data161005\\NAPM_RAW.Rdata")
    #     > ls()
    #     > library(quantmod)
    #     
    #     > tail(NAPM) NOT: 
    #       NAPM
    #     2016-04-01 50.8 quandle-ISM/MAN_PMI
    #     2016-05-01 51.3 quandle-ISM/MAN_PMI
    #     2016-06-01 53.2 quandle-ISM/MAN_PMI
    #     2016-07-01 52.6 quandle-ISM/MAN_PMI
    #     2016-08-01 49.4 quandle-ISM/MAN_PMI
    #     2016-09-01 52.8 WRONG - NO WHERE:? 
    #     https://www.instituteforsupplymanagement.org/ismreport/mfgrob.cfm?SSO=1
    #     PMI  Sep 51.5
    #     PMI  Oct 51.9
    #     Prod Sep 52.8 ** SO SCREWED UP: SINCE NAPM is SO low IN the INDICATOR ... remove it ( not worthe headache )
    #     Prod Oct 54.6
    
    #     # NEW
    #     
    #     retrieveSymbolsQuantmodRdata(
    #       finSymbol = "NAPM"
    #       , finSymbolRemoteSource = "Quantmod_FRED_RData"
    #       , finSymbolRemoteSourcePath = "./Data160903/NAPM_RAW.Rdata"
    #       , finSymbolNewCoreDatum = 52.8
    #       , finSymbolNewIndexStr  = "2016-09-01" # "2016-06-01" would have been recorded on "July 1st 2016 news as FOR_JUNE"
    #       , finSymbolAttributes = c("Close")
    #       , initDate = "1950-03-01"
    #       , subtractOffDaysSpec = -1
    #     ) -> NAPM.DELAYONE.ABS             # head "1948-01-01"
    #     # Monthly
    #     # ( Last Updated: 2015-04-01 9:06 AM CDT - ??? date - typically 1 month late WITH 1 month old date  )
    #     
    #     # really meant for a monthly
    #     as.integer(diff.mondate(c(
    #       as.mondate(tail(index(NAPM.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
    #       as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    #     ))) -> pullAheadZOODataMonthShiftAmount
    #     
    #     merge.xts(MaxAllTestTrainMonthEnds,NAPM.DELAYONE.ABS) -> NAPM.DELAYONE.ABS
    #     NAPM.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> NAPM.DELAYONE.ABS
    #     
    #     assign("NAPM.DELAYONE.ABS", value=NAPM.DELAYONE.ABS, envir = .GlobalEnv)
    #     
    #     # if HAD BEEN a Daily
    #     max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    #     
    #     pullAheadZOOData(NAPM.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> NAPM.DELAYONE.ABS.ADJUSTNOW
    #     print(paste0("NAPM pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    #     
    #     merge.xts(MaxAllTestTrainMonthEnds,NAPM.DELAYONE.ABS.ADJUSTNOW) -> NAPM.DELAYONE.ABS.ADJUSTNOW
    #     NAPM.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> NAPM.DELAYONE.ABS.ADJUSTNOW
    #     
    #     assign("NAPM.DELAYONE.ABS.ADJUSTNOW", value=NAPM.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    #     
    #     "NAPM.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["NAPM.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    # 3 month lag 
    
    # real manufacturing and trade sales ( used by Chavet/Piger?! )
    # http://research.stlouisfed.org/fred2/data/CMRMTSPL.txt
    # http://pages.uoregon.edu/jpiger/us_recession_probs.htm/
    
    # Title:               Real Manufacturing and Trade Industries Sales
    # Series ID:           CMRMTSPL
    # Source:              Federal Reserve Bank of St. Louis
    # Release:             Supplemental Estimates, Underlying Detail Tables, Spliced Series (Not a Press Release)
    # Seasonal Adjustment: Seasonally Adjusted
    
    # HARD NOTE ( MAY? DO MAY REVISIONS ) ( NEWER DATA DOES NOT LOOK ANYTING LIKE MY OLD DATA)
    # https://alfred.stlouisfed.org/series/downloaddata?seid=CMRMTSPL
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "CMRMTSPL"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> CMRMTSPL.DELAYTHREE.ABS             # head "1967-01-01"
                                             # Monthly
                                             # ( Last Updated: 2015-04-07 1:41 PM CDT - ??? date - typically 4 months late WITH 4 months old date  )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(CMRMTSPL.DELAYTHREE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,CMRMTSPL.DELAYTHREE.ABS) -> CMRMTSPL.DELAYTHREE.ABS
    CMRMTSPL.DELAYTHREE.ABS[MaxAllTestTrainMonthEndsRange] -> CMRMTSPL.DELAYTHREE.ABS
    
    assign("CMRMTSPL.DELAYTHREE.ABS", value=CMRMTSPL.DELAYTHREE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(CMRMTSPL.DELAYTHREE.ABS,pullAheadZOODataMonthShiftAmount) -> CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW
    print(paste0("CMRMTSPL pullAheadZOOData should be DELAYTHREE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW) -> CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW
    CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW
    
    assign("CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW", value=CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    # some recessions: flat or diving before the recession
    
    #     Title:               Capacity Utilization: Total Industry
    #     Series ID:           TCU
    #     Source:              Board of Governors of the Federal Reserve System
    #     Release:             G.17 Industrial Production and Capacity Utilization
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Percent of Capacity
    #     Date Range:          1967-01-01 to 2014-10-01
    #     Last Updated:        2014-11-17 8:26 AM CST
    #     http://research.stlouisfed.org/fred2/data/TCU.txt
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "TCU"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> TCU.DELAYONE.ABS              # head "1967-01-01"
                                       # Monthly
                                       # ( Last Updated: 2015-04-15 11:26 AM CDT - ??? date - typically 1 month late WITH 1 month old date  )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(TCU.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,TCU.DELAYONE.ABS) -> TCU.DELAYONE.ABS
    TCU.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> TCU.DELAYONE.ABS
    
    assign("TCU.DELAYONE.ABS", value=TCU.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(TCU.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> TCU.DELAYONE.ABS.ADJUSTNOW
    print(paste0("TCU pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,TCU.DELAYONE.ABS.ADJUSTNOW) -> TCU.DELAYONE.ABS.ADJUSTNOW
    TCU.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> TCU.DELAYONE.ABS.ADJUSTNOW
    
    assign("TCU.DELAYONE.ABS.ADJUSTNOW", value=TCU.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "TCU.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["TCU.DELAYONE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    #     Industrial Production: Durable consumer goods 
    #     Monthly, Seasonally Adjusted 1947-01 to 2015-02 (Mar 16) 
    #     http://research.stlouisfed.org/fred2/series/IPDCONGD
    #     
    #     Title:               Industrial Production: Durable Consumer Goods
    #     Series ID:           IPDCONGD
    #     Source:              Board of Governors of the Federal Reserve System (US)
    #     Release:             G.17 Industrial Production and Capacity Utilization
    #     Seasonal Adjustment: Seasonally Adjusted
    #     Frequency:           Monthly
    #     Units:               Index 2007=100
    #     Date Range:          1947-01-01 to 2015-03-01
    #     Last Updated:        2015-04-15 11:29 AM CDT           
    #     
    #     http://research.stlouisfed.org/fred2/data/IPDCONGD.txt
    

    retrieveSymbolsQuantmodRdata(
      finSymbol = "IPDCONGD"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> IPDCONGD.DELAYONE.ABS         # head "1947-01-01"
                                       # Monthly
                                       # ( Last Updated: 2015-04-15 - ??? date - typically 1 month late WITH 1 month old date  )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(IPDCONGD.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,IPDCONGD.DELAYONE.ABS) -> IPDCONGD.DELAYONE.ABS
    IPDCONGD.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> IPDCONGD.DELAYONE.ABS
    
    assign("IPDCONGD.DELAYONE.ABS", value=IPDCONGD.DELAYONE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(IPDCONGD.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> IPDCONGD.DELAYONE.ABS.ADJUSTNOW
    print(paste0("IPDCONGD pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,IPDCONGD.DELAYONE.ABS.ADJUSTNOW) -> IPDCONGD.DELAYONE.ABS.ADJUSTNOW
    IPDCONGD.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> IPDCONGD.DELAYONE.ABS.ADJUSTNOW
    
    assign("IPDCONGD.DELAYONE.ABS.ADJUSTNOW", value=IPDCONGD.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "IPDCONGD.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["IPDCONGD.DELAYONE.ABS.ADJUSTNOW"]
    
    
    bookmark_here <- 1
    
    #     Title:               Real Gross Domestic Income
    #     Series ID:           A261RL1Q225SBEA
    #     Source:              US. Bureau of Economic Analysis
    #     Release:             Gross Domestic Product
    #     Seasonal Adjustment: Seasonally Adjusted Annual Rate
    #     Frequency:           Quarterly
    #     Units:               Percent Change from Preceding Period
    #     Date Range:          1947-04-01 to 2014-07-01
    #     Last Updated:        2014-12-24 9:19 AM CST
    #     Notes:               BEA Account Code: A261RL1
    #     
    #     http://research.stlouisfed.org/fred2/data/A261RL1Q225SBEA.txt
    
    # Percent Change from Preceding Period
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "A261RL1Q225SBEA"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"        
      , subtractOffDaysSpec = -1
      , interpolate = TRUE
    ) ->  A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO  # head "1947-04-01"
                                                   # Quarterly
                                                   # ( Last Updated: 2015-03-27 8:49 AM CDT - ??? date - typically 7 month late WITH 7 month old date in 3 month chunks )

    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO) -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO
    A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO[MaxAllTestTrainMonthEndsRange] -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO
    
    assign("A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO", value=A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO,pullAheadZOODataMonthShiftAmount) -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW
    print(paste0("261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO pullAheadZOOData should be DELAYSIX"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW) -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW
    A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW
    
    assign("A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW", value=A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW, envir = .GlobalEnv)
    
    "A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW" -> ALL.OBSERVEES["A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    # [1] "A576RC1Q027SBEA pullAheadZOOData should be DELAYFIVE Actual: 24"
    # SEEN APR 03 2017
    # Compensation of employees: Wages and salaries (DISCONTINUED) (A576RC1Q027SBEA)
    # Series was replaced with https://fred.stlouisfed.org/series/WASCUR
    # Compensation of Employees: Wages and Salary Accruals (WASCUR) - Quarterly
    # https://fred.stlouisfed.org/series/A576RC1Q027SBEA
    
    #     # change rate seems to DIP right before a RECESSION
    #     
    #     #     Title:               Compensation of employees: Wages and salaries       
    #     #     Series ID:           A576RC1Q027SBEA
    #     #     Source:              US. Bureau of Economic Analysis
    #     #     Release:             Gross Domestic Product
    #     #     Seasonal Adjustment: Seasonally Adjusted Annual Rate
    #     #     Frequency:           Quarterly
    #     #     Units:               Billions of Dollars
    #     #     Date Range:          1947-01-01 to 2014-07-01
    #     #     Last Updated:        2014-11-25 8:34 AM CST
    #     #     Notes:               BEA Account Code: A576RC1
    #     #     
    #     #     http://research.stlouisfed.org/fred2/data/A576RC1Q027SBEA.txt
    #     
    #     retrieveSymbolsQuantmodRdata(
    #       finSymbol = "A576RC1Q027SBEA"
    #       , finSymbolRemoteSource = "Quantmod_FRED"
    #       , finSymbolAttributes = c("Close")
    #       , initDate = "1950-03-01"
    #       , subtractOffDaysSpec = -1
    #       , interpolate = TRUE
    #     ) -> A576RC1Q027SBEA.DELAYFIVE.ABS     # head "1947-01-01"
    #                                            # Quarterly
    #                                            # ( Last Updated: 2015-03-27 8:48 AM CDT - ??? date - typically 7 month late WITH 7 month old date in 3 month chunks )
    # 
    #     # really meant for a monthly
    #     as.integer(diff.mondate(c(
    #       as.mondate(tail(index(A576RC1Q027SBEA.DELAYFIVE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
    #       as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    #     ))) -> pullAheadZOODataMonthShiftAmount
    #     
    #     merge.xts(MaxAllTestTrainMonthEnds,A576RC1Q027SBEA.DELAYFIVE.ABS) -> A576RC1Q027SBEA.DELAYFIVE.ABS
    #     A576RC1Q027SBEA.DELAYFIVE.ABS[MaxAllTestTrainMonthEndsRange] -> A576RC1Q027SBEA.DELAYFIVE.ABS
    #     
    #     assign("A576RC1Q027SBEA.DELAYFIVE.ABS", value=A576RC1Q027SBEA.DELAYFIVE.ABS, envir = .GlobalEnv)
    #     
    #     # if HAD BEEN a Daily
    #     max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    #     
    #     pullAheadZOOData(A576RC1Q027SBEA.DELAYFIVE.ABS,pullAheadZOODataMonthShiftAmount) -> A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW
    #     print(paste0("A576RC1Q027SBEA pullAheadZOOData should be DELAYFIVE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    #     
    #     merge.xts(MaxAllTestTrainMonthEnds,A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW) -> A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW
    #     A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW
    #     
    #     assign("A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW", value=A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    #     
    #     "A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW"]
    
    # DIDIER SORNETE? ( VOLITILITY BEFORE THE EARTHQUAKE? )
    #
    # ( FUTURE [ ]?: ^GSPC : CONSIDER ADDING CORRELATIONS )
    # January 21, 2015
    # By Ilya Kipnis   library(ecp); library(BreakoutDetection)
    # http://www.r-bloggers.com/an-introduction-to-change-points-packages-ecp-and-breakoutdetection/
    # https://quantstrattrader.wordpress.com/2015/01/21/an-introduction-to-change-points-packages-ecp-and-breakoutdetection/
    
    # Historical / Future Volatility Correlation Stability
    # runSD -> lag -> na.omit -> runCor -> plot.zoo
      # also see Torgo data mining book
      # specifyModel( . . . + runSD(Cl(GSPC)) )
    # April 11, 2010
    # By Joshua Ulrich
    # http://www.r-bloggers.com/historical-future-volatility-correlation-stability/
    # http://blog.fosstrading.com/2010/04/historical-future-volatility.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+FossTrading+%28FOSS+Trading%29
    
    
    
    # [1] "A576RC1Q027SBEA pullAheadZOOData should be DELAYFIVE Actual: 24"
    # SEEN APR 03 2017
    # Compensation of employees: Wages and salaries (DISCONTINUED) (A576RC1Q027SBEA)
    # Series was replaced with https://fred.stlouisfed.org/series/WASCUR
    # Compensation of Employees: Wages and Salary Accruals (WASCUR) - Quarterly
    # https://fred.stlouisfed.org/series/A576RC1Q027SBEA
    
    # change rate seems to DIP right before a RECESSION
    
    #     Title:               Compensation of Employees: Wages and Salary Accruals
    #     Series ID:           WASCUR
    #     Source:              U.S. Bureau of Economic Analysis
    #     Release:             Gross Domestic Product
    #     Seasonal Adjustment: Seasonally Adjusted Annual Rate
    #     Frequency:           Quarterly
    #     Units:               Billions of Dollars
    #     Date Range:          1947-01-01 to 2016-10-01
    #     Last Updated:        2017-03-30 7:51 AM CDT
    #     Notes:               BEA Account Code: A034RC1
    #     
    #     A Guide to the National Income and Product Accounts of the United
    #     States (NIPA) - (http://www.bea.gov/national/pdf/nipaguid.pdf)
    #     
    #     http://research.stlouisfed.org/fred2/data/WASCUR.txt
    
    # NOTE: SEEMS like just the *NAME* changed
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "WASCUR"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
      , interpolate = TRUE
    ) -> WASCUR.DELAYFIVE.ABS     # head "1947-01-01"
    # Quarterly
    # ( Last Updated: 2015-03-27 8:48 AM CDT - ??? date - typically 7 month late WITH 7 month old date in 3 month chunks )
    
    # really meant for a monthly
    as.integer(diff.mondate(c(
      as.mondate(tail(index(WASCUR.DELAYFIVE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
      as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    ))) -> pullAheadZOODataMonthShiftAmount
    
    merge.xts(MaxAllTestTrainMonthEnds,WASCUR.DELAYFIVE.ABS) -> WASCUR.DELAYFIVE.ABS
    WASCUR.DELAYFIVE.ABS[MaxAllTestTrainMonthEndsRange] -> WASCUR.DELAYFIVE.ABS
    
    assign("WASCUR.DELAYFIVE.ABS", value=WASCUR.DELAYFIVE.ABS, envir = .GlobalEnv)
    
    # if HAD BEEN a Daily
    max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    
    pullAheadZOOData(WASCUR.DELAYFIVE.ABS,pullAheadZOODataMonthShiftAmount) -> WASCUR.DELAYFIVE.ABS.ADJUSTNOW
    print(paste0("WASCUR pullAheadZOOData should be DELAYFIVE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    
    merge.xts(MaxAllTestTrainMonthEnds,WASCUR.DELAYFIVE.ABS.ADJUSTNOW) -> WASCUR.DELAYFIVE.ABS.ADJUSTNOW
    WASCUR.DELAYFIVE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> WASCUR.DELAYFIVE.ABS.ADJUSTNOW
    
    assign("WASCUR.DELAYFIVE.ABS.ADJUSTNOW", value=WASCUR.DELAYFIVE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "WASCUR.DELAYFIVE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["WASCUR.DELAYFIVE.ABS.ADJUSTNOW"]    
    
    
    bookmark_here <- 1
    
    # retrieveSymbolsmultplRdata(         # "12.month.EPS.Close" ( web table header) 
    #   finSymbol = "SP500.12M.EPS"
    #   , finSymbolAttribute = "Close"
    # )  -> SP500.12M.EPS.DELAYTHREE.ABS  # head "Jan 31, 1871"
    #                                     # Monthly
    #                                     # ( Last Known Date: Dec 31, 2014 - ??? date - typically 4 month late WITH 4 month old date )
    
    # # really meant for a monthly
    # as.integer(diff.mondate(c(
    #   as.mondate(tail(index(SP500.12M.EPS.DELAYTHREE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
    #   as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    # ))) -> pullAheadZOODataMonthShiftAmount
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.12M.EPS.DELAYTHREE.ABS) -> SP500.12M.EPS.DELAYTHREE.ABS
    # SP500.12M.EPS.DELAYTHREE.ABS[MaxAllTestTrainMonthEndsRange] -> SP500.12M.EPS.DELAYTHREE.ABS
    # 
    # assign("SP500.12M.EPS.DELAYTHREE.ABS", value=SP500.12M.EPS.DELAYTHREE.ABS, envir = .GlobalEnv)
    # 
    # # if HAD BEEN a Daily
    # max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    # 
    # pullAheadZOOData(SP500.12M.EPS.DELAYTHREE.ABS,pullAheadZOODataMonthShiftAmount) -> SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW
    # print(paste0("SP500.12M.EPS pullAheadZOOData should be DELAYTHREE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW) -> SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW
    # SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW
    # 
    # assign("SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW", value=SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    # 
    # "SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW"]
    
    # TO DO ABOVE: FIX THAT COLNAME
    # ...........12.month.EPS...........Value..........Close
    
    # retrieveSymbolsmultplRdata(                      # "SandP.500.Real.Earnings.Growth.Pct.Close"
    #   finSymbol = "SP500.REAL.EARN.GR.PCT"           # Annual percentage change in 12 month
    #   , finSymbolAttribute = "Close"
    # )  -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO   # head "Dec 31, 1989"
    #                                                       #  Quarterly
    #                                                       # ( Last Known Date: Dec 31, 2014 - ??? date - typically 4 month late WITH 4 month old date )

    # # really meant for a monthly
    # as.integer(diff.mondate(c(
    #   as.mondate(tail(index(SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO),1), displayFormat="%Y-%m-%d",timeunits="months"),
    #   as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    # ))) -> pullAheadZOODataMonthShiftAmount
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO) -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO
    # SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO[MaxAllTestTrainMonthEndsRange] -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO
    # 
    # assign("SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO", value=SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO, envir = .GlobalEnv)
    # 
    # # if HAD BEEN a Daily
    # max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    # 
    # pullAheadZOOData(SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO,pullAheadZOODataMonthShiftAmount) -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW
    # print(paste0("SP500.REAL.EARN.GR pullAheadZOOData should be DELAYTHREE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW) -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW
    # SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW
    # 
    # assign("SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW", value=SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW, envir = .GlobalEnv)
    # 
    # "SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW" -> ALL.OBSERVEES["SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW"]
    
    # retrieveSymbolsmultplRdata(          # "SandP.500.PE.Ratio.Close" ( web table header) 
    #     finSymbol = "SP500.PE.RATIO"
    #   , finSymbolAttribute = "Close"
    # )  -> SP500.PE.RATIO.DELAYONE.ABS    # head "Jan 1, 1871"
    #                                      #  Monthly ( with in-month estimate )
    #                                      # ( Last Known Date: Mar 1, 2015 - ??? date - typically 1 month late WITH 1 month old date )
    
    # # really meant for a monthly
    # as.integer(diff.mondate(c(
    #   as.mondate(tail(index(SP500.PE.RATIO.DELAYONE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
    #   as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    # ))) -> pullAheadZOODataMonthShiftAmount
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.PE.RATIO.DELAYONE.ABS) -> SP500.PE.RATIO.DELAYONE.ABS
    # SP500.PE.RATIO.DELAYONE.ABS[MaxAllTestTrainMonthEndsRange] -> SP500.PE.RATIO.DELAYONE.ABS
    # 
    # assign("SP500.PE.RATIO.DELAYONE.ABS", value=SP500.PE.RATIO.DELAYONE.ABS, envir = .GlobalEnv)
    # 
    # # if HAD BEEN a Daily
    # max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    # 
    # pullAheadZOOData(SP500.PE.RATIO.DELAYONE.ABS,pullAheadZOODataMonthShiftAmount) -> SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW
    # print(paste0("SP500.PE.RATIO pullAheadZOOData should be DELAYONE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW) -> SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW
    # SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW
    # 
    # assign("SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW", value=SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    # 
    # "SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["SP500.PE.RATIO.DELAYONE.ABS.ADJUSTNOW"]
    
    # retrieveSymbolsmultplRdata(               # "SandP.500.Book.Value.Close"
    #     finSymbol = "SP500.BV.PER.SHARE"
    #   , finSymbolAttribute = "Close"
    # )  -> SP500.BV.PER.SHARE.DELAYTHREE.ABS   
    #                                            # head "Dec 31, 1999"
    #                                            #  Quarterly 
    #                                            # ( Last Known Date:Sep 30, 2014 - ??? date - typically 6 month late WITH 6 month old date )
    # 
    # # really meant for a monthly
    # as.integer(diff.mondate(c(
    #   as.mondate(tail(index(SP500.BV.PER.SHARE.DELAYTHREE.ABS),1), displayFormat="%Y-%m-%d",timeunits="months"),
    #   as.mondate(finDate.TestTrain.Global.Latest, displayFormat="%Y-%m-%d",timeunits="months")
    # ))) -> pullAheadZOODataMonthShiftAmount
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.BV.PER.SHARE.DELAYTHREE.ABS) -> SP500.BV.PER.SHARE.DELAYTHREE.ABS
    # SP500.BV.PER.SHARE.DELAYTHREE.ABS[MaxAllTestTrainMonthEndsRange] -> SP500.BV.PER.SHARE.DELAYTHREE.ABS
    # 
    # assign("SP500.BV.PER.SHARE.DELAYTHREE.ABS", value=SP500.BV.PER.SHARE.DELAYTHREE.ABS, envir = .GlobalEnv)
    # 
    # # if HAD BEEN a Daily
    # max(pullAheadZOODataMonthShiftAmount,0) -> pullAheadZOODataMonthShiftAmount
    # 
    # pullAheadZOOData(SP500.BV.PER.SHARE.DELAYTHREE.ABS,pullAheadZOODataMonthShiftAmount) -> SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW
    # print(paste0("SP500.BV.PER.SHARE pullAheadZOOData should be DELAYTHREE"," Actual: ",pullAheadZOODataMonthShiftAmount))
    # 
    # merge.xts(MaxAllTestTrainMonthEnds,SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW) -> SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW
    # SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW[MaxAllTestTrainMonthEndsRange] -> SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW 
    # 
    # assign("SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW", value=SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    # 
    # "SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW"]
    
    bookmark_here <- 1 
    
    # predictees area   
    
    # x source ; y destination
    NEXT.PCTDRAWDOWN.OVER <- function(x,y,over) {
      require(quantmod)     # Next     # search path 'xts' # cbind 
      require(matrixStats)  # rowMins
      # get a list of "Next" 1 . . . over
      y <- lapply(1:over, FUN = function(x,target) {
        xts(Next(target, x)) 
      }, target = y
      )
      # make into one data.frame
      do.call("cbind",y) -> y
      # minimum of all of those data.frame columns
      rowMins(y)         -> y
      # absolute percentage change
      ( y - x )/ abs(x) * 100 -> y
      y -> coredata(x)
      paste0(colnames(x),".NEXT.PCTDRAWDOWN.OVER.",over,"MO") -> colnames(x)
      return(x)
    }
    # require(functional)
    Curry(NEXT.PCTDRAWDOWN.OVER,over=3) -> NEXT.PCTDRAWDOWN.OVER.3MO 
    Curry(NEXT.PCTDRAWDOWN.OVER,over=2) -> NEXT.PCTDRAWDOWN.OVER.2MO 
    Curry(NEXT.PCTDRAWDOWN.OVER,over=1) -> NEXT.PCTDRAWDOWN.OVER.1MO 
    
    sink("NULL")
    assign("NEXT.PCTDRAWDOWN.OVER.3MO", value = dput(NEXT.PCTDRAWDOWN.OVER.3MO), envir = .GlobalEnv)
    assign("NEXT.PCTDRAWDOWN.OVER.2MO", value = dput(NEXT.PCTDRAWDOWN.OVER.2MO), envir = .GlobalEnv)
    assign("NEXT.PCTDRAWDOWN.OVER.1MO", value = dput(NEXT.PCTDRAWDOWN.OVER.1MO), envir = .GlobalEnv)
    sink()
    
    # test
    # test1 <- NEXT.PCTDRAWDOWN.OVER.3MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE,GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW)
    
    "NEXT.PCTDRAWDOWN.OVER.3MO" -> ALL.PREDICTEESFUNCTIONS[["NEXT.PCTDRAWDOWN.OVER.3MO"]]
    "NEXT.PCTDRAWDOWN.OVER.2MO" -> ALL.PREDICTEESFUNCTIONS[["NEXT.PCTDRAWDOWN.OVER.2MO"]]
    "NEXT.PCTDRAWDOWN.OVER.1MO" -> ALL.PREDICTEESFUNCTIONS[["NEXT.PCTDRAWDOWN.OVER.1MO"]]
    
    bookmark_here <- 1 
 
    NEXT.PCTCHG.OVER <- function(x,over) {
      require(quantmod)     # Next     # search path 'xts' # cbind  
      # absolute percent change    
      ( xts(Next(x,over)) - x )/ abs(x) * 100 -> y
      y -> coredata(x)
      paste0(colnames(x),".NEXT.PCTCHG.OVER.",over,"MO") -> colnames(x)
      return(x)
    }
    Curry(NEXT.PCTCHG.OVER,over=3) -> NEXT.PCTCHG.OVER.3MO 
    Curry(NEXT.PCTCHG.OVER,over=2) -> NEXT.PCTCHG.OVER.2MO 
    Curry(NEXT.PCTCHG.OVER,over=1) -> NEXT.PCTCHG.OVER.1MO 
  
    sink("NULL")
    assign("NEXT.PCTCHG.OVER.3MO", value = dput(NEXT.PCTCHG.OVER.3MO), envir = .GlobalEnv)
    assign("NEXT.PCTCHG.OVER.2MO", value = dput(NEXT.PCTCHG.OVER.2MO), envir = .GlobalEnv)
    assign("NEXT.PCTCHG.OVER.1MO", value = dput(NEXT.PCTCHG.OVER.1MO), envir = .GlobalEnv)
    sink()
    
    # test  
    # test2 <- NEXT.PCTCHG.OVER.1MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE)
    
    "NEXT.PCTCHG.OVER.3MO" -> ALL.PREDICTEESFUNCTIONS[["NEXT.PCTCHG.OVER.3MO"]]
    "NEXT.PCTCHG.OVER.2MO" -> ALL.PREDICTEESFUNCTIONS[["NEXT.PCTCHG.OVER.2MO"]]
    "NEXT.PCTCHG.OVER.1MO" -> ALL.PREDICTEESFUNCTIONS[["NEXT.PCTCHG.OVER.1MO"]]
    
    bookmark_here <- 1 
    
    LAG.PCTCHG.OVER <- function(x,over) {
      require(quantmod)     # Lag     # search path 'xts' # cbind
      # absolute percent change 
      ( x - xts(Lag(x,over)))/ abs(xts(Lag(x,over))) * 100 -> y
      y -> coredata(x)
      paste0(colnames(x),".LAG.PCTCHG.OVER.",over,"MO") -> colnames(x)
      return(x)
    }
    
    bookmark_here <- 1
    
    # WORKS ( BUT I AM CURRENLY *NOT USING* ) 

    #     assignManyOverCurries <- function(funct,overvalues) {
    #       require(functional) # Curry 
    #       
    #       match.fun(funct)                      -> match_fun_funct
    #       
    #       as.character(substitute(funct))       -> funct_name   
    #       
    #       for(var in overvalues) { 
    #         assign(paste0(funct_name,".",var,"MO"   ), value = dput(Curry(match_fun_funct,over=var)), envir = parent.frame())  
    #         assign(paste0(funct_name,".",var,"MO"   ), value = dput(Curry(match_fun_funct,over=var)), envir = .GlobalEnv)  
    #       }
    #       
    #     }
    #     
    #     # test   
    #     test2a <-  assignManyOverCurries(LAG.PCTCHG.OVER, overvalues= c(1,2,3,6,9,12) )   
    
    bookmark_here <- 1

    Curry(LAG.PCTCHG.OVER,over=1) -> LAG.PCTCHG.OVER.1MO 
    Curry(LAG.PCTCHG.OVER,over=2) -> LAG.PCTCHG.OVER.2MO 
    Curry(LAG.PCTCHG.OVER,over=3) -> LAG.PCTCHG.OVER.3MO 
    Curry(LAG.PCTCHG.OVER,over=4) -> LAG.PCTCHG.OVER.4MO 
    Curry(LAG.PCTCHG.OVER,over=5) -> LAG.PCTCHG.OVER.5MO 
    Curry(LAG.PCTCHG.OVER,over=6) -> LAG.PCTCHG.OVER.6MO 
    Curry(LAG.PCTCHG.OVER,over=7) -> LAG.PCTCHG.OVER.7MO 
    Curry(LAG.PCTCHG.OVER,over=8) -> LAG.PCTCHG.OVER.8MO 
    Curry(LAG.PCTCHG.OVER,over=9) -> LAG.PCTCHG.OVER.9MO 
    Curry(LAG.PCTCHG.OVER,over=10) -> LAG.PCTCHG.OVER.10MO 
    Curry(LAG.PCTCHG.OVER,over=11) -> LAG.PCTCHG.OVER.11MO 
    Curry(LAG.PCTCHG.OVER,over=12) -> LAG.PCTCHG.OVER.12MO 
    Curry(LAG.PCTCHG.OVER,over=13) -> LAG.PCTCHG.OVER.13MO 
    Curry(LAG.PCTCHG.OVER,over=14) -> LAG.PCTCHG.OVER.14MO 
    Curry(LAG.PCTCHG.OVER,over=15) -> LAG.PCTCHG.OVER.15MO 
    
    sink("NULL")
    assign("LAG.PCTCHG.OVER.1MO", value = dput(LAG.PCTCHG.OVER.1MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.2MO", value = dput(LAG.PCTCHG.OVER.2MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.3MO", value = dput(LAG.PCTCHG.OVER.3MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.4MO", value = dput(LAG.PCTCHG.OVER.4MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.5MO", value = dput(LAG.PCTCHG.OVER.5MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.6MO", value = dput(LAG.PCTCHG.OVER.6MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.7MO", value = dput(LAG.PCTCHG.OVER.7MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.8MO", value = dput(LAG.PCTCHG.OVER.8MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.9MO", value = dput(LAG.PCTCHG.OVER.9MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.10MO", value = dput(LAG.PCTCHG.OVER.10MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.11MO", value = dput(LAG.PCTCHG.OVER.11MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.12MO", value = dput(LAG.PCTCHG.OVER.12MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.13MO", value = dput(LAG.PCTCHG.OVER.13MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.14MO", value = dput(LAG.PCTCHG.OVER.14MO), envir = .GlobalEnv)
    assign("LAG.PCTCHG.OVER.15MO", value = dput(LAG.PCTCHG.OVER.15MO), envir = .GlobalEnv)
    sink()
    
    # test
    # test2b <- LAG.PCTCHG.OVER.1MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE)

    "LAG.PCTCHG.OVER.1MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.1MO"]]
    "LAG.PCTCHG.OVER.2MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.2MO"]]
    "LAG.PCTCHG.OVER.3MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.3MO"]]
    "LAG.PCTCHG.OVER.4MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.4MO"]]
    "LAG.PCTCHG.OVER.5MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.5MO"]]
    "LAG.PCTCHG.OVER.6MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.6MO"]]
    "LAG.PCTCHG.OVER.7MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.7MO"]]
    "LAG.PCTCHG.OVER.8MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.8MO"]]
    "LAG.PCTCHG.OVER.9MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.9MO"]]
    "LAG.PCTCHG.OVER.10MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.10MO"]]
    "LAG.PCTCHG.OVER.11MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.11MO"]]
    "LAG.PCTCHG.OVER.12MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.12MO"]]
    "LAG.PCTCHG.OVER.13MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.13MO"]]
    "LAG.PCTCHG.OVER.14MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.14MO"]]
    "LAG.PCTCHG.OVER.15MO" -> ALL.OBSERVEESFUNCTIONS[["LAG.PCTCHG.OVER.15MO"]]
    
    bookmark_here <- 1 
    
    TTR.MATH.OVER <- function(x,math,over) {
      require(TTR)  # technical trading rules 
      # technical trading rule 
      
      # note TTR functions WILL remove NA head() data EXCEPT one NA
      # save and merge.xts BACK later
      x_original_index <- xts(,index(x))
      
      # TTR rule: required 'x' data cannot have NAs in the tail()
      # Error in runSum(x, n) : Series contains non-leading NAs
      na.trim(x,sides="right") -> x
      
      # TTR rule: required 'x' data cannot have 'NA gaps' between the head and the tail
      # Error in runSum(x, n) : Series contains non-leading NAs
      # interpolate(na.approx) to locf(na.locf) 
      na.locf(x) -> x
      
      do.call(math,list(x = x, n = over)) -> y # note TTR functions WILL remove NA head data EXCEPT one NA
      y -> coredata(x)
      
      # merge.xts BACK the head() NA dates that TTR removed
      merge.xts(x_original_index,x) -> x
      
      paste(colnames(x),"TTR.MATH.OVER",math,over,"MO", sep='.') -> colnames(x)
      return(x)
    }
    Curry(TTR.MATH.OVER,math="SMA",over=2) -> TTR.MATH.OVER.SMA.2MO
    Curry(TTR.MATH.OVER,math="SMA",over=3) -> TTR.MATH.OVER.SMA.3MO
    Curry(TTR.MATH.OVER,math="SMA",over=4) -> TTR.MATH.OVER.SMA.4MO
    Curry(TTR.MATH.OVER,math="SMA",over=5) -> TTR.MATH.OVER.SMA.5MO
    Curry(TTR.MATH.OVER,math="SMA",over=6) -> TTR.MATH.OVER.SMA.6MO
    Curry(TTR.MATH.OVER,math="SMA",over=7) -> TTR.MATH.OVER.SMA.7MO
    Curry(TTR.MATH.OVER,math="SMA",over=8) -> TTR.MATH.OVER.SMA.8MO
    Curry(TTR.MATH.OVER,math="SMA",over=9) -> TTR.MATH.OVER.SMA.9MO
    Curry(TTR.MATH.OVER,math="SMA",over=10) -> TTR.MATH.OVER.SMA.10MO
    Curry(TTR.MATH.OVER,math="SMA",over=11) -> TTR.MATH.OVER.SMA.11MO
    Curry(TTR.MATH.OVER,math="SMA",over=12) -> TTR.MATH.OVER.SMA.12MO
    Curry(TTR.MATH.OVER,math="SMA",over=13) -> TTR.MATH.OVER.SMA.13MO
    Curry(TTR.MATH.OVER,math="SMA",over=14) -> TTR.MATH.OVER.SMA.14MO
    Curry(TTR.MATH.OVER,math="SMA",over=15) -> TTR.MATH.OVER.SMA.15MO

    sink("NULL")
    assign("TTR.MATH.OVER.SMA.2MO", value = dput(TTR.MATH.OVER.SMA.2MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.3MO", value = dput(TTR.MATH.OVER.SMA.3MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.4MO", value = dput(TTR.MATH.OVER.SMA.4MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.5MO", value = dput(TTR.MATH.OVER.SMA.5MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.6MO", value = dput(TTR.MATH.OVER.SMA.6MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.7MO", value = dput(TTR.MATH.OVER.SMA.7MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.8MO", value = dput(TTR.MATH.OVER.SMA.8MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.9MO", value = dput(TTR.MATH.OVER.SMA.9MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.10MO", value = dput(TTR.MATH.OVER.SMA.10MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.11MO", value = dput(TTR.MATH.OVER.SMA.11MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.12MO", value = dput(TTR.MATH.OVER.SMA.12MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.13MO", value = dput(TTR.MATH.OVER.SMA.13MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.14MO", value = dput(TTR.MATH.OVER.SMA.14MO), envir = .GlobalEnv)
    assign("TTR.MATH.OVER.SMA.15MO", value = dput(TTR.MATH.OVER.SMA.15MO), envir = .GlobalEnv)
    sink()
    
    # test
    # test3 <- TTR.MATH.OVER.SMA.2MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE)
    
    "TTR.MATH.OVER.SMA.2MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.2MO"]]
    "TTR.MATH.OVER.SMA.3MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.3MO"]]
    "TTR.MATH.OVER.SMA.4MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.4MO"]]
    "TTR.MATH.OVER.SMA.5MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.5MO"]]
    "TTR.MATH.OVER.SMA.6MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.6MO"]]
    "TTR.MATH.OVER.SMA.7MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.7MO"]]
    "TTR.MATH.OVER.SMA.8MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.8MO"]]
    "TTR.MATH.OVER.SMA.9MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.9MO"]]
    "TTR.MATH.OVER.SMA.10MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.10MO"]]
    "TTR.MATH.OVER.SMA.11MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.11MO"]]
    "TTR.MATH.OVER.SMA.12MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.12MO"]]
    "TTR.MATH.OVER.SMA.13MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.13MO"]]
    "TTR.MATH.OVER.SMA.14MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.14MO"]]
    "TTR.MATH.OVER.SMA.15MO" -> ALL.OBSERVEESFUNCTIONS[["TTR.MATH.OVER.SMA.15MO"]]

    bookmark_here <- 1 
    
    # x source ; y destination
    COMPARE.ABOVE.PCT <- function(x,y) {
      require(zoo)   # search path 'zoo' # coredata
      # absolute percent change 
      ( y - x )/ abs(x) * 100 -> y
      y -> coredata(x)
      paste0(colnames(x),".COMPARE.ABOVE.PCT") -> colnames(x)
      return(x)
    }
    sink("NULL")
    assign("COMPARE.ABOVE.PCT", value = dput(COMPARE.ABOVE.PCT), envir = .GlobalEnv)
    sink()
    
    # test 
    # test4 <- COMPARE.ABOVE.PCT(TTR.MATH.OVER.SMA.2MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE),GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE)
    
    # see 'test' above NEEDS a CLEVER use
    "COMPARE.ABOVE.PCT" ->  ALL.OVER.OBSERVEESFUNCTIONS[["COMPARE.ABOVE.PCT"]]
    
    bookmark_here <- 1 
    
    # observees area
    
    list() -> CURR.OBSERVEES 
    for(var.obs in ALL.OBSERVEES) {
      
      for(var.obsfun in ALL.OBSERVEESFUNCTIONS) {

        # relative 
                    
        if( grepl("LAG\\.PCTCHG\\.OVER\\..*MO",var.obsfun) ) {   
          
          # browser(text = paste0("loop starting: ", var.obsfun), expr = {  var.obs == "USRECP.DELAYSEVEN.ABS.ADJUSTNOW" } )
          
          # 1/0 0/0 0/1 on/off switches: no sense; skip this
          # Chauvet/Piger - recession probabilites - wrong math - too extreme to be useful
          
          if( var.obs == "USRECP.DELAYSEVEN.ABS.ADJUSTNOW" ||
              var.obs == "RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW" 
          ) { 
            # do nothing
          }
          else {
            paste0(var.obsfun,"(",var.obs,")") -> CURR.OBSERVEES[[paste0(var.obsfun,"(",var.obs,")")]]
          }
          
        }
        
        # absolute  
        
        if( grepl("TTR\\.MATH\\.OVER\\.SMA\\..*MO",var.obsfun) ) {  
          
          for( var.all.obsfun in ALL.OVER.OBSERVEESFUNCTIONS) {
            
            # browser()
            # browser(text = paste0("loop starting: ", var.all.obsfun), expr = {  var.obs == "USRECP.DELAYSEVEN.ABS.ADJUSTNOW" } )
            
            # USRECP.DELAYSEVEN.ABS.ADJUSTNOW 1/0 0/0 0/1 on/off switches: no sense; use the simpler form instead
            if(ALL.OVER.OBSERVEESFUNCTIONS == "COMPARE.ABOVE.PCT"             &&
                                  var.obs  == "USRECP.DELAYSEVEN.ABS.ADJUSTNOW"
            ) {
               # USRECP.DELAYSEVEN.ABS.ADJUSTNOW 1/0 0/0 0/1 on/off switches: no sense; use the simpler form instead
               paste0(var.obsfun,"(",var.obs,")")  -> CURR.OBSERVEES[[paste0(var.obsfun,"(",var.obs,")")]]
            } else {
               paste0(var.all.obsfun,"(",var.obsfun,"(",var.obs,"),",var.obs,")") -> CURR.OBSERVEES[[paste0(var.all.obsfun,"(",var.obsfun,"(",var.obs,"),",var.obs,")")]]
            }
            
          }
          
        }
        
      }
    }
    
    bookmark_here <- 1  
    
    # predictees area     
    
    # non-dynamic solution ( would HAVE preferred to get this from ALL.PREDICTEESFUNCTIONS and ALL.PREDICTEES
    
    # *** On Predictee ( ONE of TWO code chanages are here ) *** #  
    # ALL.PREDICTEES.HARD.CODED
    # main thing I try to predict 
    # "NEXT.PCTCHG.OVER.3MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE)"                              -> ALL.PREDICTEES.HARD.CODED[["NEXT.PCTCHG.OVER.3MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE)"]]
    # "NEXT.PCTDRAWDOWN.OVER.3MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE,GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW)"  -> ALL.PREDICTEES.HARD.CODED[["NEXT.PCTDRAWDOWN.OVER.3MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE,GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW)"]]
    "NEXT.PCTCHG.OVER.3MO(GDP.DELAYSIX.ABS.ADJUSTNOW)"                            -> ALL.PREDICTEES.HARD.CODED[["NEXT.PCTCHG.OVER.3MO(GDP.DELAYSIX.ABS.ADJUSTNOW)"]]
    
    bookmark_here <- 1 
    
    for(var.all.pred.hard in ALL.PREDICTEES.HARD.CODED) {
      
      for(var.testtraindates in TestTrainDates) {
        
        # for right now ( just this one )
        if( var.testtraindates[["ListName"]] == "Test2008" ) {
        
          # *** On Predictee ( TWO of TWO code changes are here ) *** # 
          # for right now ( just this one ) 
          # if( var.all.pred.hard == "NEXT.PCTCHG.OVER.3MO(GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE)" ) {  
          if( var.all.pred.hard == "NEXT.PCTCHG.OVER.3MO(GDP.DELAYSIX.ABS.ADJUSTNOW)" ) {
            
            # 'right now'   
            var.all.pred.hard -> CURR.PREDICTEE 
            
            # 'right now' directly from the loop above
            CURR.OBSERVEES    ->  CURR.OBSERVEES
            
            # KEEP - STRANGLY - IN COMBINATION WITH MOODY'S BONDS - GETTING SLIGHTLY BETTER RESULTS KEEPING THE *BORING GOVERNMENT BONDS"
            # Remove these GS10, DGS3MO, DIFF.GS10.DGS3MO ( 160 )
            # because     BAAFFM, AAAFFM, and 'BAAFFM - AAAFFM' seem better ( 148 )
            ##CURR.OBSERVEES <- CURR.OBSERVEES[!grepl("\\(GS10\\.",CURR.OBSERVEES)]
            ##CURR.OBSERVEES <- CURR.OBSERVEES[!grepl("\\(DGS3MO\\.",CURR.OBSERVEES)]
            ##CURR.OBSERVEES <- CURR.OBSERVEES[!grepl("\\(DIFF\\.GS10\\.DGS3MO\\.",CURR.OBSERVEES)]
            
            CURR.FORMULA <- as.formula(paste0(CURR.PREDICTEE," ~ ",paste0(unlist(CURR.OBSERVEES), collapse =" + ")))
            
            data.model <- specifyModel(CURR.FORMULA, na.rm = FALSE) # na.rm = TRUE # default # I MAY WANT DIFFERENT
    
            # Error in total.columns[j] <- ncol(m) : replacement has length zero
            # if error, to find the error, RUN THIS
            #         sapply(X = c(CURR.PREDICTEE,CURR.OBSERVEES)
            #                , FUN = function(x) { print(x); eval(parse(text=x)) }
            #                , simplify = FALSE
            #         ) -> newlist
            
            # all data   
            

            # Train_initDate through Test_finDate 
            model.data.ALL <- modelData(data.model, data.window = c(var.testtraindates[["Train"]][["initDate"]] ,var.testtraindates[["Test"]][["finDate"]]))
            
            # DEBUG(BOTH STMTS)
            model.data.ALL_before_Inf_Fix <- model.data.ALL
            save(list = c("model.data.ALL_before_Inf_Fix"), file = "DATA_model.data.ALL_before_Inf_Fix.RData")
            
            # needed for caret::findLinearCombos 
            
            model.data.ALL[model.data.ALL == -Inf] <- .Machine[["double.xmin"]]
            model.data.ALL[model.data.ALL ==  Inf] <- .Machine[["double.xmax"]]
            data.model@model.data <- model.data.ALL
            
            # all except the 'predictee column'
            model.data.OBSERVEES.CURR <- model.data.ALL[,setdiff(colnames(model.data.ALL),data.model@model.target)]
            
            # MAGIC NUMBER ( SHOULD BE STORED ELSEWHERE )    
            MinObserveeDate <- "1969-01-31"
            
            # choose only those observee columns that have the 'minimum date of interest'
            model.data.OBSERVEES.CURR <- model.data.OBSERVEES.CURR[,which(!is.na(model.data.OBSERVEES.CURR[MinObserveeDate,colnames(model.data.OBSERVEES.CURR)] ))]
            # test     
            # anyNA(model.data.OBSERVEES.CURR["1969-01-31::",]) == FALSE
               
            # choose only the observee columns that have the 'minimum date of interest'    
            # remove the single predectee's COLUMN most recent few NEXT NA elements ( if any )
            # remove the single predectee's COLUMN trailing NAs ( if possible ) 
            # remove observees                     trailing NAs  
            #  will create a SMOOTH head 
            # xts:::na.omit.xts ( tested: will remove !(complete.cases(any single NA in row) )
            model.data.CURR <-  na.omit(model.data.ALL[,c(data.model@model.target,colnames(model.data.OBSERVEES.CURR))])
            
            bookmarkhere <- 1
            
            # note: A261RL1Q225SBEA seem to be a nightmare ( consider REMOVING )   
            
            # caret - remove linear combinations ( do not check the predictee column )
            
            # find the predictee column
            model.data.CURR.PREDICTEE.ORIG.COL.INDEX <- which(colnames(model.data.CURR) == data.model@model.target)
            
            # remove observee linear combinations ( do not check the predictee column )
            model.data.CURR.OBSERVEES.LINEARCOMBOS.INDEXES  <- findLinearCombos(coredata(model.data.CURR[,-model.data.CURR.PREDICTEE.ORIG.COL.INDEX]))[["remove"]] 
            #### TEMPORARILIY TURN OFF 'REMOVE LINEAR COMBINATIONS ###
            print("### TEMPORARILIY TURN OFF 'REMOVE LINEAR COMBINATIONS '###")
            NULL -> model.data.CURR.OBSERVEES.LINEARCOMBOS.INDEXES
            
            model.data.CURR.OBSERVEES.COLNAMES.REMOVED <- c()
            
            if(!is.null(model.data.CURR.OBSERVEES.LINEARCOMBOS.INDEXES)) {
            
              print("Linear combination columns removed.")
              model.data.CURR.OBSERVEES.COLNAMES.REMOVED <- colnames(model.data.CURR[,-model.data.CURR.PREDICTEE.ORIG.COL.INDEX])[model.data.CURR.OBSERVEES.LINEARCOMBOS.INDEXES]
              print(model.data.CURR.OBSERVEES.COLNAMES.REMOVED)
              
              # keep remaining columns and the  predictee
              model.data.CURR <- model.data.CURR[,!(colnames(model.data.CURR) %in% model.data.CURR.OBSERVEES.COLNAMES.REMOVED)]
            
            }
            
            bookmarkhere <- 1
            
            # caret - remove correlation ( do not check the predictee column )
            
            # find the predictee column
            model.data.CURR.PREDICTEE.ORIG.COL.INDEX <- which(colnames(model.data.CURR) == data.model@model.target)
            
            # remove observee correlations ( do not check the predictee column ) 
            # stats::cor
            # method="spearman" does not return: nan na Inf -Inf
            model.data.CURR.OBSERVEES.CORRELATIONS.MATRIX   <- cor(coredata(model.data.CURR[,-model.data.CURR.PREDICTEE.ORIG.COL.INDEX]), method="spearman")
            model.data.CURR.OBSERVEES.CORRELATIONS.INDEXES  <- findCorrelation(model.data.CURR.OBSERVEES.CORRELATIONS.MATRIX, cutoff = 1.00) 
            #          cutoff = 1.0 NO COLUMNS REMOVED
            # default: cutoff = 0.9  will produce 61 observees ( some long term data )
            #                   0.25 will product 9 points ( all 2MO data ) 
            
            model.data.CURR.OBSERVEES.COLNAMES.REMOVED <- c()
            # handle zero correctly
            if(length(model.data.CURR.OBSERVEES.CORRELATIONS.INDEXES) != 0) {
              
              print("Correlation columns removed.")
              model.data.CURR.OBSERVEES.COLNAMES.REMOVED <- colnames(model.data.CURR[,-model.data.CURR.PREDICTEE.ORIG.COL.INDEX])[model.data.CURR.OBSERVEES.CORRELATIONS.INDEXES]
              print(model.data.CURR.OBSERVEES.COLNAMES.REMOVED)
              
              # keep remaining columns and the  predictee
              model.data.CURR <- model.data.CURR[,!(colnames(model.data.CURR) %in% model.data.CURR.OBSERVEES.COLNAMES.REMOVED)]
              
            }
            
            bookmarkhere <- 1
            
            # ** LEFT_OFF ** "FEATURE SELECTION"
            # **  (follow)    ICA
            
            # earliest date HEAVY_LIKELY BE a 'before' MinObserveeDate("1969-01-31")
            # e.g. "1968-10-31"
            # OPTIONAL step TO KEEP exactly ONLY the  MinObserveeDate and LATER 
            model.data.CURR[paste0(MinObserveeDate,"::"),] -> model.data.CURR
            
            # real training data
            model.data.CURR[paste0(var.testtraindates[["Train"]][["initDate"]],"::",var.testtraindates[["Train"]][["finDate"]]),] -> model.data.train.CURR
            
            # the 'real world test' data to be tried LATER
            model.data.CURR[paste0(var.testtraindates[["Test"]][["initDate"]],"::",var.testtraindates[["Test"]][["finDate"]]),,] -> model.data.test.CURR
          
            bookmark_here <- 1 
            
            ## *** LEFT_OFF  ***
            ## review fscaret ( any remove corr )( random forest feature selection )
            ## review caret ( remove correlation, ica )
            ## review DMwR ( a little bit)
            ## # review performanceEstimation ( custom workflow for caret )
            ##  
            
            # later 
            # as.data.frame(modelData, stringsAsFactors = FALSE
          }
        }
      }
    } 
    
    bookmark_here <- 1 # View(data.frame(t(tail(model.data.CURR,10))))
    
    trainControl(## 10-fold CV
        method = "repeatedcv"  # randomForest ( could have been: 'oob' )
      , number = 5  # 10 ? trainControl ( "repeatedcv" default seems to be '10' anyways ) # QUICK 5
      ## repeated ten times    
      , repeats = 1 # 3 CHANGE FROM 10 ( OR 5 ) DOWN TO 1 ( speed )                      # QUICK 1
    ) -> fitControl
    
    bookmark_here <- 1 
    
    print("")
    print("Begin train to determine variable importance")
    print("")
    
    # checkpoint can find ( else caret will try to dynamically install
    #   along with dependency survial(base) )
    require(gbm)
    
    train( 
        x = data.frame(model.data.train.CURR[,setdiff(colnames(model.data.train.CURR),data.model@model.target)],stringsAsFactors=FALSE)            
      , y = as.vector(model.data.train.CURR[,data.model@model.target])
      , trControl = fitControl
      , method = "gbm" 
      , verbose = FALSE # gbm TOO MUCH # default is TRUE? anyway?
      # , tuneGrid = expand.grid(interaction.depth = seq(1, 7, by = 2),n.trees = seq(500, 1000, by = 250),shrinkage = c(0.1)) 
      , tuneGrid = expand.grid(interaction.depth = 7, n.trees = seq(10,500,1), shrinkage = c(0.01), n.minobsinnode = 10)
    ) -> FitterTune
    
    # ntreesOPTIMAL <-  gbm.perf(FitterTune$finalModel, method="OOB", plot.it = FALSE)
    # print(paste0("Collected FitterTune with Optimal(OOB) Number of Trees: ", ntreesOPTIMAL))
    # [1] 355
    
    ntreesOPTIMAL <-FitterTune$finalModel$tuneValue
    print("")
    print(paste0("Train VarImp: Collected FitterTune with Optimal caret repeatedcv Number of Trees. . ."))
    print(ntreesOPTIMAL)
    print("")
    
    bookmark_here <- 1 
    
    # gbm::summary(method=relative.influence) ?
    #   # > caret:::varImp.gbm
    #   # function (object, numTrees = NULL, ...)
    varImpFound <- varImp(FitterTune,numTrees = ntreesOPTIMAL$n.trees) # numTrees number must equal a number found in train(n.trees)
    
    # small data.frame of variable importance
    model.data.CURR.OBSERVEES.IMPORTANCES <- dplyr::filter(dplyr::arrange( cbind(row.names(varImpFound$importance),varImpFound$importance), desc(Overall)), Overall >= 10.0)
    
    print("Most Important Observees kept")
    print(model.data.CURR.OBSERVEES.IMPORTANCES)
    
    # Important Observees
    model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES <- model.data.CURR.OBSERVEES.IMPORTANCES[,"row.names(varImpFound$importance)"]
    
    ### TEMPORARILIY TURN OFF 'IMPORTANT VARIABLES' #####
    print("### TEMPORARILIY TURN OFF 'IMPORTANT VARIABLES' #####")
    model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES <- setdiff(colnames(model.data.train.CURR),data.model@model.target)
    
    print("")
    print("End train to determine variable importance")
    print("")
    
    print("")
    print("Begin train to determine predictions")
    print("")
    
    # trainControl ( SHOULD BE BETTER ALTERNATE)
    #   method='timeslice' # ( Zachary Mayer )
    #   initialWindow
    #   horizon
    #   fixedWindow
    
    # THIS IS 'GOOD'
    trainControl(## 10-fold CV
      method = "repeatedcv"  # randomForest ( could have been: 'oob' )
      , number = 5  # 10 ? trainControl ( "repeatedcv" default seems to be '10' anyways ) # QUICK 5
      ## repeated ten times    
      , repeats = 1 # 3 CHANGE FROM 10 ( OR 5 ) DOWN TO 1 ( speed ) # QUICK 1
      # , preProcOptions = list(ICAcomp = 2)
    ) -> fitControl
    
#   # Zachary Mayer

    # gbm WITH timeslices
    # Error in train.default(x = data.frame(model.data.train.CURR[, model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES],  : 
    # final tuning parameters could not be determined 

    # 50: In eval(expr, envir, enclos) :
    # model fit failed for Training050: interaction.depth=7, n.trees=500, shrinkage=0.01 
    # Error in gbm.fit(x = structure(list(LAG.PCTCHG.OVER.7MO.NAPM.DELAYONE.ABS.ADJUSTNOW = c(15.8069883527454,  :                                                                                                                                                                         
    # The dataset size is too small or subsampling rate is too large: nTrain*bag.fraction <= n.minobsinnode
    #
    #  the number of iterations,T(n.trees)
    #  the depth of each tree,K(interaction.depth)
    #  the shrinkage (or learning rate) parameter,Î»(shrinkage)
    #  the subsampling rate,p(bag.fraction)
    #
    #  Generalized Boosted Models: A guide to the gbm package Greg Ridgeway August 3, 2007

    # defaults
    # n.trees = 100,
    # interaction.depth = 1,
    # n.minobsinnode = 10,
    # shrinkage = 0.001,
    # bag.fraction = 0.5,

    #     trainControl(
    #         method = "timeslice"  
    #       , initialWindow=15
    #       , fixedWindow=TRUE
    #       , horizon=15
    #       # , preProcOptions = list(ICAcomp = 2)
    #     ) -> fitControl
    
    # DEBUG(BOTH STMTS)
    train_x <- data.frame(model.data.train.CURR[,model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES],stringsAsFactors=FALSE)
    save(list = c("train_x"), file = "DATA_MOST.IMPORTANT_train_x.RData")


    # FOR NOW: USING THE OLD ntreesOPTIMAL WITHOUT recalculating IT
    # print(paste0("USING FitterTune with Optimal(OOB) Numeber of Trees: ", ntreesOPTIMAL))
    print(paste0("USING Train VarImp with Optimal caret repeatedcv Number of Trees..."))
    print(ntreesOPTIMAL)

    # almost the same as above but I added ica
    train( 
      x = data.frame(model.data.train.CURR[,model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES],stringsAsFactors=FALSE)            
      , y = as.vector(model.data.train.CURR[,data.model@model.target])
      , trControl = fitControl
      , method = "gbm" 
      , verbose = FALSE # gbm TOO MUCH # default is TRUE? anyway?
      # , preProcess = "ica"
      # , tuneGrid = expand.grid(interaction.depth = seq(1, 7, by = 2),n.trees = seq(500, 1000, by = 250),shrinkage = c(0.1)) 
      , tuneGrid = expand.grid(interaction.depth = 7, n.trees = ntreesOPTIMAL$n.trees, shrinkage = c(0.01), n.minobsinnode = 10) 
    ) -> FitterTune

    # DEBUG(BOTH STMTS)
    train_FitterTune <- FitterTune
    save(list = c("train_FitterTune"), file = "DATA_MOST.IMPORTANT_train_FitterTune.RData")

    # caret
    newpred <- predict(FitterTune, newdata=data.frame(model.data.test.CURR[,model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES],stringsAsFactors=FALSE) )
    
    # DEBUG(BOTH STMTS)
    predict_newdata <- data.frame(model.data.test.CURR[,model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES],stringsAsFactors=FALSE)
    save(list = c("predict_newdata"), file = "DATA_MOST.IMPORTANT_predict_newdata.RData")

    # cbind.xts wrapper over merge.xts
    model.data.test.CURR.PRED <- cbind(model.data.test.CURR[,data.model@model.target], CURR.PRED=newpred )
    "CURR.VALUE" -> colnames(model.data.test.CURR.PRED)[1]
    
    bookmark_here <- 1
    
    # zoo:::with.zoo
    print("Ave pct that the CURR.PRED is away from the CURR.VALUE")
    print(sum(with(model.data.test.CURR.PRED,{ abs((CURR.PRED - CURR.VALUE)/abs(CURR.VALUE) * 100)}))/NROW(model.data.test.CURR.PRED))
    
    bookmark_here <- 1
        
    print("")
    print("End train to determine predictions")
    print("")
    
    # predict on the 'most very recent data'
    
    # DEBUG # DEBUG
    save(list = c("data.model"), file = "DATA_data.model.RData")
    save(list = c("model.data.ALL"), file = "DATA_model.data.ALL.RData")
    
    # do not carry forward what I am trying to predict - does not make sense
    # locf BECAUSE some data may not be perfectly right aligned
    # please 1. avoid mis-aligned data
    # please 2. no locf
    model.data.ALL.na.locf <- na.locf(model.data.ALL[,setdiff(colnames(model.data.ALL),data.model@model.target)])
    model.data.ALL.na.locf <- cbind.xts(model.data.ALL[,data.model@model.target], model.data.ALL.na.locf)

    # DEBUG
    save(list = c("model.data.test.CURR"), file = "DATA_model.data.test.CURR.RData")

    # just the last few months  length( colnames(model.data.CURR)  ) = 580
    model.data.ALL.na.locf.NEWTAIL <- model.data.ALL.na.locf[as.Date(max(index(model.data.test.CURR))) < as.Date(index(model.data.ALL.na.locf)),]

    # inherit same columns as ... identical(colnames(model.data.train.CURR),colnames(model.data.test.CURR)) and model.data.CURR
    model.data.ALL.na.locf.NEWTAIL <- model.data.ALL.na.locf.NEWTAIL[,colnames(model.data.test.CURR)]

    # DEBUG # DEBUG
    save(list = c("model.data.test.CURR"), file = "DATA_model.data.test.CURR.RData")
    save(list = c("model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES"), file = "DATA_model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES.RData")

    # just the 'meaningful columns'
    newpred.NEWTAIL <- predict(FitterTune, newdata=data.frame(rbind.xts(model.data.test.CURR, model.data.ALL.na.locf.NEWTAIL)[,model.data.CURR.OBSERVEES.MOST.IMPORTANT.VARIABLES],stringsAsFactors=FALSE) )
    # str(newpred.NEWTAIL) num [1:132] 
    
    model.data.test.CURR.NEWTAIL <- cbind(rbind.xts(model.data.test.CURR, model.data.ALL.na.locf.NEWTAIL)[,data.model@model.target], CURR.PRED=newpred.NEWTAIL )
    "CURR.VALUE" -> colnames(model.data.test.CURR.NEWTAIL)[1] 
    # DEBUG
    save(list=c("model.data.test.CURR.NEWTAIL"), file = "DATA_model.data.test.CURR.NEWTAIL.RData")

    # works 1.34 1.39 1.41
    # View(model.data.test.CURR.NEWTAIL)

    # DEBUG
    save.image(file = "DATA_image.RData")
    
    bookmark_here <- 1

    # View(model.data.test.CURR.PRED)
    # plot.zoo(model.data.test.CURR.PRED,n=1,main=data.model@model.target)
    
    # useful KEEP
    # View(data.frame(t(tail(model.data.CURR,10)))) # MAKE SURE DATA IS ALIGNED RIGHT AS MUCH AS POSIBLE
    # View(data.frame(t(head(model.data.CURR,10))))
    # View(t(data.frame(ALL.OBSERVEES)))
    # View(t(model.data.ALL[tail(index(model.data.ALL),4),])) 

    # see greater than 10 importance ( gbm influence hard coded )
    # View(print(model.data.CURR.OBSERVEES.IMPORTANCES))
    # colnames that are important
    # colnames(model.data.ALL)[colnames(model.data.ALL) %in% as.vector(model.data.CURR.OBSERVEES.IMPORTANCES[,"row.names(varImpFound$importance)"])]
    # positions of column names that are important
    # which(colnames(model.data.ALL) %in% as.vector(model.data.CURR.OBSERVEES.IMPORTANCES[,"row.names(varImpFound$importance)"]))
    # see colnames
    # View(data.frame(colnames(model.data.ALL)))
    
    # View(model.data.test.CURR.PRED)    # test zone predictions
    # View(model.data.test.CURR.NEWTAIL) # test zone predictions + NEWTAIL

    # View(data.frame(t(tail(model.data.ALL,10))))
    
    # after through filters na.omit(GARANTEED), linear combinations(OPTIONAL),
    # correlations(OPTIONAL), columns manually removed because no sense'  
    # model.data.CURR

    # BEGIN INSTRUCTIONS
    # BEGIN INSTRUCTIONS

    # NOTE: OCCASIONAL ERROR 
    # [1] ""
    # [1] "Train VarImp: Collected FitterTune with Optimal caret repeatedcv Number of Trees. . ."
    #     n.trees interaction.depth shrinkage
    # 490     499                 7      0.01
    # [1] ""
    # AVOID (THAT) ERROR (ABOVE) BY THE FOLLOWING
    #
    # *** HIGH RECOMMENDED ***
    # ( MAKE SURE THE RSTUDIO_VERSION_IS_JUST_GREATER_AND_CLOSE_TO_THE_R_VERSION )
    #
    # ** HIGH RECOMMENDED ***
    # ( ONLY ONE ONE R/RSTUDIO PROGRAM AT A TIME, 
    # IF 2 RUN AT A TIME OCASSIONAL DLL CLASHING MAY HAPPEN!? )
    
    # 
    # MANAGE THE OLD
    # In W:\New_Economics\rcsnsight1.340
    # Create folder DataYYMMDD  *** where YYMMDD is the date on the .RData files ***
    #   Cut/Move all .Rdata files into DataYYMMDD
    # Copy     all .R     files into DataYYMMDD
    # 
    
    # 
    # DON'T FORGET THAT in R Studio
    # setwd("W:/New_Economics/rcsnsight1.340") # getwd()
    # 
    # CHECK THAT THE LOADED .R FILE IN THE TAB (hover over) 
    # HAS THE SAME PATH AS THE 'setwd'
    
    ## ## NO LONG USE NAPM ## ##
    ## modify the NAPM entry to point to the last months(last_time) entry
    ##
    ## retrieveSymbolsQuantmodRdata(
    ##   finSymbol = "NAPM"
    ##   , finSymbolRemoteSource = "Quantmod_FRED_RData"
    ##   , finSymbolRemoteSourcePath = "./Data160603/NAPM_RAW.Rdata" # ( UPDATE THIS: 1 OF 3)
    ##   , finSymbolNewCoreDatum = 53.2 # ( UPDATE THIS: 2 OF 3)
    ##   , finSymbolNewIndexStr  = "2016-06-01" # "2016-06-01" would have been recorded on "July 1st 2016 news as FOR_JUNE"" # ( UPDATE THIS: 3 OF 3)
    ##   , finSymbolAttributes = c("Close")
    ##   , initDate = "1950-03-01"
    ##   , subtractOffDaysSpec = -1
    ## ) -> NAPM.DELAYONE.ABS             # head "1948-01-01"
    ## Monthly
    ## ( Last Updated: 2015-04-01 9:06 AM CDT - ??? date - typically 1 month late WITH 1 month old date  )
    ## ##
    
    # HARD NOTE: Any modification of 'data loading' and 'massaging'
    # then I HAVE TO RE-GET the DATA from the SOURCE ( ** IMPORTANT ** )

    # 
    # *** VERY VERY VERY IMPORTANT  ***
    # CHANGE "finDate.TestTrain.Global.Latest      <- "YYYY-MM-DD"
    # TO BE the 'date of the 'end of the latest month' before running the program TODAY'
    # **** SEARCH FOR * finDate.TestTrain.Global.Latest * SEARCH FOR *****
    # 
    # rm(list=ls(all.names=TRUE))
    # 
    # BEST HAVE THE 'ABSOLUTE PATH' 
    # debugSource('W:/New_Economics/rcsnsight1.340/main-rcsnsight2-999.R')
    # 
    # PLACE DOWN BREAKPOINT AT "bookmark_here <- 1" ( ABOVE )
    # 
    # main_rcsnsight2_999(THESEED = 2)
    # 
    # View(model.data.test.CURR.NEWTAIL)
    # BUT ( BETTER ) MAY WANT TO DO : model.data.test.CURR.NEWTAIL -> model.data.test.CURR.NEWTAIL_YYMMDD
    # View(model.data.test.CURR.NEWTAIL_YYMMDD)
    # 
    # END INSTRUCTIONS
    # END INSTRUCTIONS

    # begin all variable importance area #
    # begin all variable importance area #
    
    # model.data.CURR.OBSERVEES.IMPORTANCES.ALL <- dplyr::arrange( cbind(row.names(varImpFound$importance),varImpFound$importance), desc(Overall))
    
    # View(model.data.CURR.OBSERVEES.IMPORTANCES.ALL)
    
    # model.data.CURR.OBSERVEES.IMPORTANCES.ALL.VECTOR <- model.data.CURR.OBSERVEES.IMPORTANCES.ALL[,"Overall"]
    # names(model.data.CURR.OBSERVEES.IMPORTANCES.ALL.VECTOR) <- model.data.CURR.OBSERVEES.IMPORTANCES.ALL[,1]
    
    # where(indicies) does it occur
    # grep("INDPRO", names(model.data.CURR.OBSERVEES.IMPORTANCES.ALL.VECTOR))
    # grep("INDPRO\\.", names(model.data.CURR.OBSERVEES.IMPORTANCES.ALL.VECTOR))
    # [1]   1  18  19  51  70  94 102
    
    # View(data.frame(grep("INDPRO", names(model.data.CURR.OBSERVEES.IMPORTANCES.ALL.VECTOR), value = TRUE)))
    
    # end all variable importance area #
    # end all variable importance area #

    bookmark_here <- 1
    
    # TOO MUCH TIME
    # LEFT_OFF ( skip VARIMP:  RESUME with ICA )
    

    ## LEFT_OFF
    
    # [x] ADD NUMBER ratio: unumployment/population
    # DONE
    
    # [x] ADJUST removeCorr 1.00
    # DONE
    
    # [ ] PRINT gbm Variable Importance
    
    # follow rest rcnsight1 
    # ICA
    # [SMOTE]
    # gbm
    # predict
    
    
    # *** LEFT_OFF ***
    # [X]  !!!! VERIFY THAT THE DATA IS ALIGNED TO 14-12-31: Browse[2]> View(data.frame(t(tail(model.data.test,10))))
    #   solve mystery of NAs [ ] and NaNs [ ]
    # [LATER] better variable NAMES
    # [LATER] fscaret ( find variable importance) means(reduce CURR.OBSERVEES)
    
    # predictees area    
    
    ## "NEXT.RANGE.3(GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW)" -> ALL.OBSERVEESPREDICTEES[["NEXT.RANGE.3(GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW)"]] 
    ## CURR.PREDICTEE <- "NEXT.RANGE.3(GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW)" 
    # end predictees area
    
    # begin observees area ( remove CURR.PREDICTEE )   
    
    # include all
    ## ALL.OBSERVEESPREDICTEES -> CURR.OBSERVEES 
    
    # obviously remove the PREDICTEE 
    ## NULL -> CURR.OBSERVEES[["NEXT.RANGE.3(GSPC.DELAYZERO.ABS.ADJUSTNOW.LOW)"]] 

    # XOR
    # exclude all
    ## list(   
    ##   GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE = "GSPC.DELAYZERO.ABS.ADJUSTNOW.CLOSE"
    ## ) -> CURR.OBSERVEES 
    
    # end observees area 
    
    ## CURR.FORMULA <- as.formula(paste0(CURR.PREDICTEE," ~ ",paste0(unlist(CURR.OBSERVEES), collapse =" + ")))
    
    # SOON (feature selection: perhaps use package fscaret )
 
    
    the_end_debug_bookmark_here <- 1
    
    
    ## LEFT_OFF
    ## [x] fix the below ( delay(if_any), save )   
    ## 
    ## [x]               SP500.12M#   
    ## [x]               PE.RATIO
    ## 
    ## [x]               USRECM   
    ## [x]               RECPROUSM156N 
    
    ## [X] wed/th - get FROM FRED  
    ## [x]   GDP garanteed
    ## [x]   10  year bond 
    ## [x]    3  year bill
    ## [x]   effective funds rate
    ##   [X] (TO DO) 'real earnings growth or similar' = 'EPS - 10 year treasury yield'
    ## [x]   unemployment
    ## [x]   inflation
    ## [SORTOF]   actions of the fed  
    
    ## LEFT_OFF
    ## [x] LEFT_OFF complete pullAheadZOOData's ( MANY many)
      # LEFT_OFF ( repeat the FOLLOWING below FOR EACH ONE where NECESSARY)
      # pullAheadZOOData(,) -> .ADJUSTNOW
      # assign(".ADJUSTNOW", value=.ADJUSTNOW, envir = .GlobalEnv)  
    
    ## [X] PUT .CLOSE/.LOW in ONE place WHERE appropriate
    ## [X] GET other FRED data RECM[#] that has NOT BEEN stopped DELIVERED
    ###  [LATER] remove .RData files ( [ ] generate NEW data )
    ## after quantmodmodel [ ] TO simulate a train,test/predict HAVE limits
    ##  [X] 2nd VERIFY/REFINE each pullAheadZOOData result IS CORRECT 'land on date' [X]
    ## [X] put 'ONLY' the variable name in a OBSPRED list()
    ##  [X] # as.formula(") -> quanmod::specifyModel  -> @
    ## [X] (SPREAD) difference between yields on 10-year Treasury bonds and 3-month Treasury bills
    
    ## ******* LEFT_OFF *************** 
    
    # TO ADD [x]
    # real manufacturing and trade sales
    # http://research.stlouisfed.org/fred2/data/CMRMTSPL.txt
    # http://pages.uoregon.edu/jpiger/us_recession_probs.htm/
    
    ## [x] new function function NEXT.RANGE.3.PCT.CHANGE 
    ## [x] DO                SMA 1,2,3,...12, and # ( see elegant MINIPLAYs )
    ## [X] INDICATOR(NOW PERCENT ABOVE) SMA ( *** LEFT_OFF *** )
    
    ## [X]   PCTCHANGE(WHERE APPROPRIATE)  
    ## [x] consider renaming prog variables to .ABS )
    ## [X] tails - what is useful and what is not  

    ## [ ]   re-UMCENT ???
    
    ## also consider
    ## UMICH sent survey ( copy OVER from 1 )
    ## IDENTITY() - ChavetPiger,NBER(Maybe Needs to be a factor),ManufactExpansionGr50
    
    ## HIGH
    ## TRANSSFORM  ManufactExpansionGr50 +-(50-ManufactExpansionGr50)
    
    ## review fscaret ( any remove corr )( random forest feature selection )
    ## review caret ( remove correlation, ica )
    ## review DMwR ( a little bit)
    ## review performanceEstimation ( custom workflow for caret )
    
    ## DYNAMIC INPUT
    
    # GET THE DATAFRAME FROM *THAT* MODEL
    
    # no.omit
    # handle MISSING values Impute? 'zoo interpolate' ( MAYBE I wanted to HANDLE earlier )
    # correlation remove
  
    # ** LEFT_OFF ** "FEATURE SELECTION" [ ]
    # **  (follow)    ICA                [ ]
    
    # variable importance
    # ICA
    
    # ( create factor response (SMOTE) )
    
    # DMwR, performanceEstimation 
    
    # gbm boosting
    
    
    the_end_debug_bookmark_here <- 1
    
    
    ### I AM ALREADY THERE ### setwd("W:/New_Economics/rcsnsight1.340") # TO BE CHANGED LATER
    
    
    the_end_debug_bookmark_here <- 1
    
    Sys.setenv(TZ=oldtz)
    
    print("Done with main_rcsnsight2_999()")
  
  }  
  main_rcsnsight2_999_inner(pauseat) # should have been do.call

  
}
# rm(list=ls(all.names=TRUE))
# library(checkpoint)
# debugSource('W:/New_Economics/rcsnsight1.320/main-rcsnsight2-999.R')
# PLACE DOWN BREAKPOINT
# main_rcsnsight2_999(THESEED = 2)

# REMEMBER TO SET TO THE LATEST END OF MONTH DATE
# finDate.TestTrain.Global.Latest      <- "2015-03-31"
# NOTE IF MY: "Ave pct that the CURR.PRED is away from the CURR.VALUE"
#  is far greater than 160 ( e.g.  213 )then I forgot to set this variable(?)

# SEE PAST PRICTIONS, PAST RESULTS, AND 
# View(model.data.test.CURR.NEWTAIL) 


########################      
# 
#          


