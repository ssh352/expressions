
## search for "LEFT_OFF"

# main-rcsnsight2-999.R 

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width 

setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R") # TO BE CHANGED LATER 

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

retrieveSymbolsQuantmodRdata <- function(
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
      
      # interpolate
      na.approx(merge.xts(monthsxts,x, join="left")) -> x
      
      # merge.xt will prefix column(s)? with an "X".  Remove this "X"
      sub("^X","",colnames(x)) -> colnames(x)
      
    }
    
    assign(symbol,x)
    save(list = c(finSymbolNOIndex),file = paste0(getwd(),"/",finSymbolNOIndex,".Rdata"))
    return(get(symbol))
  }
}

bookmark_here <- 1


# TO BE 'EDITED,DEBUGGED and TESTED' in ( main_rcsnsight1_999_miniplay5  )

getSymbols.multpl <- function(
    symbolText
  , finSymbolAttribute = "Close" # if only ONE column
  , interpolate = FALSE
) {
  
  # Main page
  # http://www.multpl.com/sitemap
  
  # More pages
  # http://www.multpl.com/sitemap/world-economic-stats
  
  #   (SEEMS RECESSION SENSITIVE)
  
  #   S&P 500 Earnings Per Share. ( "12.month.EPS" )
  #   http://www.multpl.com/s-p-500-earnings/table?f=m
  #   
  #   S&P 500 Real Earnings Growth by Quarter ( "SandP.500.Real.Earnings.Growth.Pct" )
  #   http://www.multpl.com/s-p-500-real-earnings-growth/table/by-quarter
  #   
  #   S&P 500 PE Ratio by Month ( MATH) ( SandP.500.PE.Ratio )
  #   Price to earnings ratio, based on trailing twelve month “as reported”
  #   http://www.multpl.com/table?f=m
  
  # S&P 500 Book Value Per Share by Quarter ( "SandP.500.BV.Per.Share" )
  # S&P 500 book value per share — non-inflation adjusted current dollars. 
  # http://www.multpl.com/s-p-500-book-value/table/by-quarter
  
  # web site and owner
  # multpl.com  JOSHSTAIGER@GMAIL.COM ( in California )
  # https://www.easywhois.com/
  
  # After I signed up for email, the following was returned
  # multpl.com
  # 1130 Shoreline Dr
  # San Mateo, California 94404
  
  if(is.null(symbolText)) stop("missing arg symbolText in function getSymbols.multp")
  
  # create, later will be 'tested for null'
  NULL -> url
  
  # which web page data do I want?
  if(symbolText == "SandP.500.12.month.EPS")              "http://www.multpl.com/s-p-500-earnings/table?f=m" -> url
  
  # Annual percentage change in 12 month
  if(symbolText == "SandP.500.Real.Earnings.Growth.Pct")  "http://www.multpl.com/s-p-500-real-earnings-growth/table/by-quarter" -> url
  if(symbolText == "SandP.500.PE.Ratio")                  "http://www.multpl.com/table?f=m" -> url
  if(symbolText == "SandP.500.BV.Per.Share")              "http://www.multpl.com/s-p-500-book-value/table/by-quarter" -> url
  # OTHERS ( in future follow ) ..
  # if
  # if
  
  if(is.null(url)) stop(paste0("symbolText: ", symbolText ," url not found by function getSymbols.multp"))
  
  require(XML)     # NEED readHTMLTable
  # Hadley Wickham # web scraping 
  require(rvest)   # imports XML  masked from ‘package:XML’: xml
  # IF uncommented : require(XML), USE: XML::xml to access XML::xml
  require(xts)     # as.xts STUFF
  
  require(TimeWarp) # create month-end calendar dates from the first through the last
  
  # NO KNOWN requirement ( but I do not want to be pest and be kicked OUT)
  Sys.sleep(1.0)
  html(url) -> found 
  
  # content="multpl" expected
  if(!grepl("multpl",html_text(found))) stop(paste0("seems internal url not(no_longer) correct for ", symbolText," in function getSymbols.multpl"))
  
  # get that data
  readHTMLTable(html_node(found,"#datatable")
                , stringsAsFactors = FALSE
  ) -> prexts 
  
  # NOTE: SOME other TABLES have
  #   some other non-numeric characters "commas", "slashes" 
  # TO DO: clean_out AS NEEDED
  
  # if a first cell of a non-Date column has the text '\n estimate(red)' remove this
  for(cn in colnames(prexts)) { 
    if(cn != "Date") 
      if(grepl("estimate",prexts[1,cn])) { 
        gsub("estimate","",prexts[1,cn]) -> prexts[1,cn]
        gsub("\n"      ,"",prexts[1,cn]) -> prexts[1,cn]
        gsub(" "       ,"",prexts[1,cn]) -> prexts[1,cn]
      }
  }
  
  # check if this is a "Percent table" if so create a suffix
  for(cn in colnames(prexts)) { 
    if(cn != "Date") 
      if(any(grepl("%",prexts[,cn]))) { 
        paste0( colnames(prexts)[colnames(prexts) %in% cn],".Pct") -> colnames(prexts)[colnames(prexts) %in% cn]
      }
  }
  
  # in the 'not the Date' columns, if a 'percent sign' exists remove it'
  for(cn in colnames(prexts)) { 
    if(cn != "Date") gsub("%","",prexts[,cn]) -> prexts[,cn] 
  }
  
  # make columns other than 'Date' to be numeric
  for(cn in colnames(prexts)) { if(cn != "Date") as.numeric(prexts[,cn]) -> prexts[,cn] }
  
  # convert 'Jan 31, 1872' to friendly general date format YYYY-MM-DD
  as.character(strptime(prexts[,"Date"],format='%b %d, %Y')) -> prexts[,"Date"]
  
  # create 'data.frame to xts' expected row.names
  prexts[,"Date"] -> row.names(prexts)
  
  # remove NOT useful Date column ( safer form )
  prexts[,which(!(colnames(prexts) %in% "Date")), drop = FALSE] -> prexts
  
  # make R-like column names (somthing character to dots)
  gsub(" ", "."  , colnames(prexts)) -> colnames(prexts)
  gsub("&", "and", colnames(prexts)) -> colnames(prexts)
  
  # when only ONE column, add a named " finSymbolAttribute"
  if(NCOL(prexts) == 1) {
    paste0(colnames(prexts)[1],".",finSymbolAttribute) -> colnames(prexts)[1]
  }
  
  # convert to xts # SEE ? as.xts.methods
  as.xts(prexts, dateFormat="Date") -> nowxts
  
  # fill in missing values
  if( interpolate == TRUE ) {
    
    # create month-end calendar dates from the first through the last
    xts( ,order.by = as.Date(c(dateSeq(from = index(first(nowxts)), to = index(last(nowxts)) , by="months", k.by = 1 ) - 1,index(last(nowxts)))) ) -> monthsxts
    
    # make lots of NAs
    merge.xts(monthsxts,nowxts, join="left")            -> nowxts
    
      # merge.xt will prefix column(s)? with an "X".  Remove this "X"
      sub("^X","",colnames(nowxts)) -> colnames(nowxts)
    
    # interpolate
    na.approx(merge.xts(monthsxts,nowxts, join="left")) -> nowxts
    
      # merge.xt will prefix column(s)? with an "X".  Remove this "X"
      sub("^X","",colnames(nowxts)) -> colnames(nowxts)
    
  }
  
  # TEMPORARILY hardcoded until I find a more elegant solution
  # REALLY 'should' have BEEN put through the QUAUNTSTRAT to.monthly fixer method
  if(symbolText == "SandP.500.PE.Ratio") {
    
    # these dates are the '1st of the month'
    # these dates also include an extra 'last observation within-month partial estimate'
    
    # remove THAT 'last observation within-month partial estimate'
    # but I REALLY should DETECT this
    nowxts[1:(NROW(nowxts)-1)] -> nowxts
    
    # since at the '1st of the month' subtract off one day
    dateWarp(date=index(nowxts),spec=-1,by="days") -> index(nowxts) 
    
    return(nowxts)
  }
  
  return(nowxts)
}


retrieveSymbolsmultplRdata <- function(
    finSymbol
  , finSymbolAttribute 
) {
  
  if( file.exists(paste0(getwd(),"/",finSymbol,".",finSymbolAttribute,".Rdata"))){
    load(file = paste0(getwd(),"/",finSymbol,".",finSymbolAttribute,".Rdata"), envir = environment())
    return(get(paste0(finSymbol,".",finSymbolAttribute)))
  } else {
    
    if ( finSymbol == "SP500.12M.EPS" ) {
      getSymbols.multpl("SandP.500.12.month.EPS"
                        , finSymbolAttribute = finSymbolAttribute 
                        , interpolate = TRUE 
      ) -> x
    }
    
    if ( finSymbol == "SP500.REAL.EARN.GR.PCT" ) {
      getSymbols.multpl("SandP.500.Real.Earnings.Growth.Pct"
                        , finSymbolAttribute = finSymbolAttribute 
                        , interpolate = TRUE 
      ) -> x
    }
    
    if ( finSymbol == "SP500.PE.RATIO" ) {
      getSymbols.multpl("SandP.500.PE.Ratio"
                        , finSymbolAttribute = finSymbolAttribute 
                        , interpolate = FALSE 
      ) -> x
      
    }
      
    if ( finSymbol == "SP500.BV.PER.SHARE" ) {
      getSymbols.multpl("SandP.500.BV.Per.Share"
                          , finSymbolAttribute = finSymbolAttribute 
                          , interpolate = TRUE 
      ) -> x
    }

    
    symbol <- paste0(finSymbol,".",finSymbolAttribute)
    
    assign(symbol,x)
    save(list = c(symbol),file = paste0(getwd(),"/",finSymbol,".",finSymbolAttribute,".Rdata"))
    return(get(symbol))
    
  }
  
}



main_rcsnsight2_999 <- function(pauseat=NULL) {
  
  main_rcsnsight2_999_inner <- function(...) {
  
    bookmarkhere <- 1
    
    setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R")
    
    oldtz <- Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    
    ## require(DMwR) NOT YET
    # require(performanceEstimation) LATER ?!!
    # require(caret) ## CLOSE FUTURE

    require(functional) # Curry
    
    require(Holidays)
    require(TimeWarp)
    require(quantstrat) # and added to search() path: 
    #  blotter, PerformanceAnalytics, FinancialInstrument,
    #  quantmod, TTR, xts, zoo
    
    # findLinearCombos
    require(caret) # and added to search() path:
    #  ggplot2, lattice 
    
    # require(require(PerformanceAnalytics) # POSSIBLE FUT ( and through require("quantstrat") )
    
    # every function
    list() -> ALL.OBSERVEES
    
    list() -> ALL.OBSERVEESFUNCTIONS
    list() -> ALL.OVER.OBSERVEESFUNCTIONS
    
    list() -> ALL.PREDICTEESFUNCTIONS
    
    list() -> ALL.PREDICTEES             # seems NO LONGER USED ( I could eliminate )
    
    list() -> ALL.PREDICTEES.HARD.CODED
    
    # training and TRUE tests
    list(Test2001 = list(Train=list(initDate="1950-03-31",finDate ="1998-12-31"),
                          Test=list(initDate="1999-01-01",finDate ="2014-12-31"),
                          ListName="Test2001"
        ),
          Test2008 = list(Train=list(initDate="1950-03-31",finDate ="2003-12-31"),
                           Test=list(initDate="2004-01-31",finDate ="2014-12-31"),
                           ListName="Test2008"
        )
    ) -> TestTrainDates
    
    # max range : total ; training and TRUE tests "1950-03-31" to "2014-12-31"
    xts( 
      , order.by = as.Date(c(dateSeq(
        from = index(xts(,as.Date(("1950-03-31"))))
        , to = index(xts(,as.Date(("2014-12-31")))) 
        , by="months", k.by = 1 ) - 1 
        ,index(  xts(,as.Date(("2014-12-31")))   ))) 
    ) -> MaxAllTestTrainMonthEnds
    
    # S&P500 from yahoo
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "^GSPC"
      , finSymbolAttributes = c("Close","Low")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = 0
    ) -> GSPC.DELAYZERO.ABS     # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,GSPC.DELAYZERO.ABS) -> GSPC.DELAYZERO.ABS
    GSPC.DELAYZERO.ABS["1950-03-31::2014-12-31"] -> GSPC.DELAYZERO.ABS
    
    assign("GSPC.DELAYZERO.ABS", value=GSPC.DELAYZERO.ABS, envir = .GlobalEnv)
    
    # specifics
    Cl(GSPC.DELAYZERO.ABS) -> GSPC.DELAYZERO.ABS.CLOSE
    Lo(GSPC.DELAYZERO.ABS) -> GSPC.DELAYZERO.ABS.LOW
    assign("GSPC.DELAYZERO.ABS.CLOSE", value=GSPC.DELAYZERO.ABS.CLOSE, envir = .GlobalEnv)
    assign("GSPC.DELAYZERO.ABS.LOW"  , value=GSPC.DELAYZERO.ABS.LOW  , envir = .GlobalEnv)
    
    "GSPC.DELAYZERO.ABS.CLOSE" -> ALL.OBSERVEES["GSPC.DELAYZERO.ABS.CLOSE"] 
    "GSPC.DELAYZERO.ABS.LOW"   -> ALL.OBSERVEES["GSPC.DELAYZERO.ABS.LOW"] 
    
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
    ) -> USRECP.DELAYFIVE.ABS      # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,USRECP.DELAYFIVE.ABS) -> USRECP.DELAYFIVE.ABS

    assign("USRECP.DELAYFIVE.ABS", value=USRECP.DELAYFIVE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(USRECP.DELAYFIVE.ABS,5) -> USRECP.DELAYFIVE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,USRECP.DELAYFIVE.ABS.ADJUSTNOW) -> USRECP.DELAYFIVE.ABS.ADJUSTNOW
    USRECP.DELAYFIVE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> USRECP.DELAYFIVE.ABS.ADJUSTNOW
    
    assign("USRECP.DELAYFIVE.ABS.ADJUSTNOW", value=USRECP.DELAYFIVE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "USRECP.DELAYFIVE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["USRECP.DELAYFIVE.ABS.ADJUSTNOW"]
    
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
    ) -> RECPROUSM156N.DELAYTHREE.ABS    # head "1967-05-31"
    merge.xts(MaxAllTestTrainMonthEnds,RECPROUSM156N.DELAYTHREE.ABS) -> RECPROUSM156N.DELAYTHREE.ABS
    
    assign("RECPROUSM156N.DELAYTHREE.ABS", value=RECPROUSM156N.DELAYTHREE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(RECPROUSM156N.DELAYTHREE.ABS,3) -> RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW) -> RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW
    RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW
    
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
    ) -> GDP.DELAYSIX.ABS      # head"1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,GDP.DELAYSIX.ABS) -> GDP.DELAYSIX.ABS

    assign("GDP.DELAYSIX.ABS", value=GDP.DELAYSIX.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(GDP.DELAYSIX.ABS,6) -> GDP.DELAYSIX.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,GDP.DELAYSIX.ABS.ADJUSTNOW) -> GDP.DELAYSIX.ABS.ADJUSTNOW
    GDP.DELAYSIX.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> GDP.DELAYSIX.ABS.ADJUSTNOW
    
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
    ) -> GS10.DELAYONE.ABS        # head  "1953-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,GS10.DELAYONE.ABS) -> GS10.DELAYONE.ABS

    assign("GS10.DELAYONE.ABS", value=GS10.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(GS10.DELAYONE.ABS,1) -> GS10.DELAYONE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,GS10.DELAYONE.ABS.ADJUSTNOW) -> GS10.DELAYONE.ABS.ADJUSTNOW
    GS10.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> GS10.DELAYONE.ABS.ADJUSTNOW
    
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
    ) -> DGS3MO.DELAYZERO.ABS      # head "1982-01-31"
    merge.xts(MaxAllTestTrainMonthEnds,DGS3MO.DELAYZERO.ABS) -> DGS3MO.DELAYZERO.ABS
    DGS3MO.DELAYZERO.ABS["1950-03-31::2014-12-31"] -> DGS3MO.DELAYZERO.ABS
    
    assign("DGS3MO.DELAYZERO.ABS", value=DGS3MO.DELAYZERO.ABS, envir = .GlobalEnv)
    
    "DGS3MO.DELAYZERO.ABS" -> ALL.OBSERVEES["DGS3MO.DELAYZERO.ABS"]
    
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
    ) -> DFF.DELAYZERO.ABS          # head "1954-07-31"
    merge.xts(MaxAllTestTrainMonthEnds,DFF.DELAYZERO.ABS) -> DFF.DELAYZERO.ABS
    DFF.DELAYZERO.ABS["1950-03-31::2014-12-31"] -> DFF.DELAYZERO.ABS
    
    assign("DFF.DELAYZERO.ABS", value=DFF.DELAYZERO.ABS, envir = .GlobalEnv)

    "DFF.DELAYZERO.ABS" -> ALL.OBSERVEES["DFF.DELAYZERO.ABS"]
    
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
    ) -> UNEMPLOY.DELAYONE.ABS       # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,UNEMPLOY.DELAYONE.ABS) -> UNEMPLOY.DELAYONE.ABS
    
    assign("UNEMPLOY.DELAYONE.ABS", value=UNEMPLOY.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(UNEMPLOY.DELAYONE.ABS,1) -> UNEMPLOY.DELAYONE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,UNEMPLOY.DELAYONE.ABS.ADJUSTNOW) -> UNEMPLOY.DELAYONE.ABS.ADJUSTNOW
    UNEMPLOY.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> UNEMPLOY.DELAYONE.ABS.ADJUSTNOW
    
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
    ) -> UEMPMED.DELAYONE.ABS        # head "1967-06-30"
    merge.xts(MaxAllTestTrainMonthEnds,UEMPMED.DELAYONE.ABS) -> UEMPMED.DELAYONE.ABS
    
    assign("UEMPMED.DELAYONE.ABS", value=UEMPMED.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(UEMPMED.DELAYONE.ABS,1) -> UEMPMED.DELAYONE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,UEMPMED.DELAYONE.ABS.ADJUSTNOW) -> UEMPMED.DELAYONE.ABS.ADJUSTNOW
    UEMPMED.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> UEMPMED.DELAYONE.ABS.ADJUSTNOW
    
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
    ) -> POP.DELAYTWO.ABS         # head "1951-12-31"
    merge.xts(MaxAllTestTrainMonthEnds,POP.DELAYTWO.ABS) -> POP.DELAYTWO.ABS
    
    assign("POP.DELAYTWO.ABS", value=POP.DELAYTWO.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(POP.DELAYTWO.ABS,2) -> POP.DELAYTWO.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,POP.DELAYTWO.ABS.ADJUSTNOW) -> POP.DELAYTWO.ABS.ADJUSTNOW
    POP.DELAYTWO.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> POP.DELAYTWO.ABS.ADJUSTNOW
    
    assign("POP.DELAYTWO.ABS.ADJUSTNOW", value=POP.DELAYTWO.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "POP.DELAYTWO.ABS.ADJUSTNOW" -> ALL.OBSERVEES["POP.DELAYTWO.ABS.ADJUSTNOW"]
    
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
    ) -> UNRATE.DELAYONE.ABS         # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,UNRATE.DELAYONE.ABS) -> UNRATE.DELAYONE.ABS

    assign("UNRATE.DELAYONE.ABS", value=UNRATE.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(UNRATE.DELAYONE.ABS,1) -> UNRATE.DELAYONE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,UNRATE.DELAYONE.ABS.ADJUSTNOW) -> UNRATE.DELAYONE.ABS.ADJUSTNOW
    UNRATE.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> UNRATE.DELAYONE.ABS.ADJUSTNOW
    
    assign("UNRATE.DELAYONE.ABS.ADJUSTNOW", value=UNRATE.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "UNRATE.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["UNRATE.DELAYONE.ABS.ADJUSTNOW"]
    
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
    ) -> CPILFESL.DELAYONE.ABS       # head "1956-12-31"
    merge.xts(MaxAllTestTrainMonthEnds,CPILFESL.DELAYONE.ABS) -> CPILFESL.DELAYONE.ABS

    assign("CPILFESL.DELAYONE.ABS", value=CPILFESL.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(CPILFESL.DELAYONE.ABS,1) -> CPILFESL.DELAYONE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,CPILFESL.DELAYONE.ABS.ADJUSTNOW) -> CPILFESL.DELAYONE.ABS.ADJUSTNOW
    CPILFESL.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> CPILFESL.DELAYONE.ABS.ADJUSTNOW
    
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
    ) -> TREAST.DELAYZERO.ABS     # head "2002-12-31"
    merge.xts(MaxAllTestTrainMonthEnds,TREAST.DELAYZERO.ABS) -> TREAST.DELAYZERO.ABS
    TREAST.DELAYZERO.ABS["1950-03-31::2014-12-31"] -> TREAST.DELAYZERO.ABS
    
    assign("TREAST.DELAYZERO.ABS", value=TREAST.DELAYZERO.ABS, envir = .GlobalEnv)
    
    "TREAST.DELAYZERO.ABS" -> ALL.OBSERVEES["TREAST.DELAYZERO.ABS"]
    
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
    ) -> MBST.DELAYZERO.ABS          # head "2002-12-31"
    merge.xts(MaxAllTestTrainMonthEnds,MBST.DELAYZERO.ABS) -> MBST.DELAYZERO.ABS
    MBST.DELAYZERO.ABS["1950-03-31::2014-12-31"] -> MBST.DELAYZERO.ABS
    
    assign("MBST.DELAYZERO.ABS", value=MBST.DELAYZERO.ABS, envir = .GlobalEnv)
    
    "MBST.DELAYZERO.ABS" -> ALL.OBSERVEES["MBST.DELAYZERO.ABS"]
    
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
    ) -> INDPRO.DELAYONE.ABS        # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,INDPRO.DELAYONE.ABS) -> INDPRO.DELAYONE.ABS

    assign("INDPRO.DELAYONE.ABS", value=INDPRO.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(INDPRO.DELAYONE.ABS,1) -> INDPRO.DELAYONE.ABS.ADJUSTNOW
 
    merge.xts(MaxAllTestTrainMonthEnds,INDPRO.DELAYONE.ABS.ADJUSTNOW) -> INDPRO.DELAYONE.ABS.ADJUSTNOW
    INDPRO.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> INDPRO.DELAYONE.ABS.ADJUSTNOW
    
    assign("INDPRO.DELAYONE.ABS.ADJUSTNOW", value=INDPRO.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "INDPRO.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["INDPRO.DELAYONE.ABS.ADJUSTNOW"]
    
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
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "NAPM"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> NAPM.DELAYONE.ABS             # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,NAPM.DELAYONE.ABS) -> NAPM.DELAYONE.ABS

    assign("NAPM.DELAYONE.ABS", value=NAPM.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(NAPM.DELAYONE.ABS,1) -> NAPM.DELAYONE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,NAPM.DELAYONE.ABS.ADJUSTNOW) -> NAPM.DELAYONE.ABS.ADJUSTNOW
    NAPM.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> NAPM.DELAYONE.ABS.ADJUSTNOW
    
    assign("NAPM.DELAYONE.ABS.ADJUSTNOW", value=NAPM.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "NAPM.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["NAPM.DELAYONE.ABS.ADJUSTNOW"]
    
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
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "CMRMTSPL"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> CMRMTSPL.DELAYTHREE.ABS             # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,CMRMTSPL.DELAYTHREE.ABS) -> CMRMTSPL.DELAYTHREE.ABS
    
    assign("CMRMTSPL.DELAYTHREE.ABS", value=CMRMTSPL.DELAYTHREE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(CMRMTSPL.DELAYTHREE.ABS,3) -> CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW

    merge.xts(MaxAllTestTrainMonthEnds,CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW) -> CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW
    CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> CMRMTSPL.DELAYTHREE.ABS.ADJUSTNOW
    
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
    ) -> TCU.DELAYONE.ABS              # head "1966-12-31"
    merge.xts(MaxAllTestTrainMonthEnds,TCU.DELAYONE.ABS) -> TCU.DELAYONE.ABS
    
    assign("TCU.DELAYONE.ABS", value=TCU.DELAYONE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(TCU.DELAYONE.ABS,1) -> TCU.DELAYONE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,TCU.DELAYONE.ABS.ADJUSTNOW) -> TCU.DELAYONE.ABS.ADJUSTNOW
    TCU.DELAYONE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> TCU.DELAYONE.ABS.ADJUSTNOW
    
    assign("TCU.DELAYONE.ABS.ADJUSTNOW", value=TCU.DELAYONE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "TCU.DELAYONE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["TCU.DELAYONE.ABS.ADJUSTNOW"]
    
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
    ) ->  A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO  # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO) -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO
    
    assign("A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO", value=A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO, envir = .GlobalEnv)
    
    pullAheadZOOData(A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO,6) -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW) -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW
    A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW["1950-03-31::2014-12-31"] -> A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW
    
    assign("A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW", value=A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW, envir = .GlobalEnv)
    
    "A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW" -> ALL.OBSERVEES["A261RL1Q225SBEA.DELAYSIX.PCTCHG.OVER3MO.ADJUSTNOW"]
    
    bookmark_here <- 1
    
    # change rate seems to DIP right before a RECESSION
    
    #     Title:               Compensation of employees: Wages and salaries       
    #     Series ID:           A576RC1Q027SBEA
    #     Source:              US. Bureau of Economic Analysis
    #     Release:             Gross Domestic Product
    #     Seasonal Adjustment: Seasonally Adjusted Annual Rate
    #     Frequency:           Quarterly
    #     Units:               Billions of Dollars
    #     Date Range:          1947-01-01 to 2014-07-01
    #     Last Updated:        2014-11-25 8:34 AM CST
    #     Notes:               BEA Account Code: A576RC1
    #     
    #     http://research.stlouisfed.org/fred2/data/A576RC1Q027SBEA.txt
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "A576RC1Q027SBEA"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
      , interpolate = TRUE
    ) -> A576RC1Q027SBEA.DELAYFIVE.ABS     # head "1950-03-31"
    merge.xts(MaxAllTestTrainMonthEnds,A576RC1Q027SBEA.DELAYFIVE.ABS) -> A576RC1Q027SBEA.DELAYFIVE.ABS

    assign("A576RC1Q027SBEA.DELAYFIVE.ABS", value=A576RC1Q027SBEA.DELAYFIVE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(A576RC1Q027SBEA.DELAYFIVE.ABS,5) -> A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW) -> A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW
    A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW
    
    assign("A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW", value=A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["A576RC1Q027SBEA.DELAYFIVE.ABS.ADJUSTNOW"]
    
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
    
    bookmark_here <- 1
    
    retrieveSymbolsmultplRdata(         # head "1871-01-31"
      finSymbol = "SP500.12M.EPS"
      , finSymbolAttribute = "Close"
    )  -> SP500.12M.EPS.DELAYTHREE.ABS  # "12.month.EPS.Close" ( web table header) 
    merge.xts(MaxAllTestTrainMonthEnds,SP500.12M.EPS.DELAYTHREE.ABS) -> SP500.12M.EPS.DELAYTHREE.ABS
    
    assign("SP500.12M.EPS.DELAYTHREE.ABS", value=SP500.12M.EPS.DELAYTHREE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(SP500.12M.EPS.DELAYTHREE.ABS,3) -> SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW) -> SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW
    SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW
    
    assign("SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW", value=SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["SP500.12M.EPS.DELAYTHREE.ABS.ADJUSTNOW"]
    
    retrieveSymbolsmultplRdata(                      # head "1871-01-31"
      finSymbol = "SP500.REAL.EARN.GR.PCT"           # Annual percentage change in 12 month
      , finSymbolAttribute = "Close"
    )  -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO # "SandP.500.Real.Earnings.Growth.Pct.Close" 
    merge.xts(MaxAllTestTrainMonthEnds,SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO) -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO
    
    assign("SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO", value=SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO, envir = .GlobalEnv)
    
    pullAheadZOOData(SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO,3) -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW) -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW
    SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW["1950-03-31::2014-12-31"] -> SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW
    
    assign("SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW", value=SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW, envir = .GlobalEnv)
    
    "SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW" -> ALL.OBSERVEES["SP500.REAL.EARN.GR.DELAYTHREE.PCTCHG.OVER12MO.ADJUSTNOW"]
    
    retrieveSymbolsmultplRdata(              # head "1870-12-31"
        finSymbol = "SP500.PE.RATIO"
      , finSymbolAttribute = "Close"
    )  -> SP500.PE.RATIO.DELAYZERO.ABS # "SandP.500.PE.Ratio.Close" ( web table header) 
    merge.xts(MaxAllTestTrainMonthEnds,SP500.PE.RATIO.DELAYZERO.ABS) -> SP500.PE.RATIO.DELAYZERO.ABS
    SP500.PE.RATIO.DELAYZERO.ABS["1950-03-31::2014-12-31"] -> SP500.PE.RATIO.DELAYZERO.ABS
    
    assign("SP500.PE.RATIO.DELAYZERO.ABS", value=SP500.PE.RATIO.DELAYZERO.ABS, envir = .GlobalEnv)

    "SP500.PE.RATIO.DELAYZERO.ABS" -> ALL.OBSERVEES["SP500.PE.RATIO.DELAYZERO.ABS"]
    
    retrieveSymbolsmultplRdata(          #  head "head 1999-12-31"
        finSymbol = "SP500.BV.PER.SHARE"
      , finSymbolAttribute = "Close"
    )  -> SP500.BV.PER.SHARE.DELAYTHREE.ABS  # "SandP.500.Book.Value.Close" 
    merge.xts(MaxAllTestTrainMonthEnds,SP500.BV.PER.SHARE.DELAYTHREE.ABS) -> SP500.BV.PER.SHARE.DELAYTHREE.ABS

    assign("SP500.BV.PER.SHARE.DELAYTHREE.ABS", value=SP500.BV.PER.SHARE.DELAYTHREE.ABS, envir = .GlobalEnv)
    
    pullAheadZOOData(SP500.BV.PER.SHARE.DELAYTHREE.ABS,3) -> SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW
    merge.xts(MaxAllTestTrainMonthEnds,SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW) -> SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW
    SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW["1950-03-31::2014-12-31"] -> SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW 
    
    assign("SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW", value=SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW, envir = .GlobalEnv)
    
    "SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW" -> ALL.OBSERVEES["SP500.BV.PER.SHARE.DELAYTHREE.ABS.ADJUSTNOW"]
    
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
    test1 <- NEXT.PCTDRAWDOWN.OVER.3MO(GSPC.DELAYZERO.ABS.CLOSE,GSPC.DELAYZERO.ABS.LOW)
    
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
    test2 <- NEXT.PCTCHG.OVER.1MO(GSPC.DELAYZERO.ABS.CLOSE)
    
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
    test2b <- LAG.PCTCHG.OVER.1MO(GSPC.DELAYZERO.ABS.CLOSE)

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
      na.approx(x) -> x
      
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
    test3 <- TTR.MATH.OVER.SMA.2MO(GSPC.DELAYZERO.ABS.CLOSE)
    
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
    test4 <- COMPARE.ABOVE.PCT(TTR.MATH.OVER.SMA.2MO(GSPC.DELAYZERO.ABS.CLOSE),GSPC.DELAYZERO.ABS.CLOSE)
    
    # see 'test' above NEEDS a CLEVER use
    "COMPARE.ABOVE.PCT" ->  ALL.OVER.OBSERVEESFUNCTIONS[["COMPARE.ABOVE.PCT"]]
    
    bookmark_here <- 1 
    
    # observees area
    
    list() -> CURR.OBSERVEES 
    for(var.obs in ALL.OBSERVEES) {
      
      for(var.obsfun in ALL.OBSERVEESFUNCTIONS) {

        # relative 
            
        if( grepl("LAG\\.PCTCHG\\.OVER\\..*MO",var.obsfun) ) {   
          
          # 1/0 0/0 0/1 on/off switches: no sense; skip this
          if( var.obs == "USRECP.DELAYFIVE.ABS.ADJUSTNOW" ) { 
            next 
          }
          
          # Chauvet/Piger - recession probabilites - wrong math - too extreme to be useful
          if( var.obs == "RECPROUSM156N.DELAYTHREE.ABS.ADJUSTNOW" ) { 
            next 
          }

          paste0(var.obsfun,"(",var.obs,")") -> CURR.OBSERVEES[[paste0(var.obsfun,"(",var.obs,")")]]
        }
        
        # absolute  
        
        if( grepl("TTR\\.MATH\\.OVER\\.SMA\\..*MO",var.obsfun) ) {  
          
          for( var.all.obsfun in ALL.OVER.OBSERVEESFUNCTIONS) {
            
            # 1/0 0/0 0/1 on/off switches: no sense; skip this
            if(ALL.OVER.OBSERVEESFUNCTIONS == "COMPARE.ABOVE.PCT"             &&
                                  var.obs  == "USRECP.DELAYFIVE.ABS.ADJUSTNOW"
               ) {next}
            
            paste0(var.all.obsfun,"(",var.obsfun,"(",var.obs,"),",var.obs,")") -> CURR.OBSERVEES[[paste0(var.all.obsfun,"(",var.obsfun,"(",var.obs,"),",var.obs,")")]]
          }
          
        }
        
      }
    }
    
    bookmark_here <- 1  
    
    # predictees area      
    
    # non-dynamic solution ( would HAVE preferred to get this from ALL.PREDICTEESFUNCTIONS and ALL.PREDICTEES
    
    # ALL.PREDICTEES.HARD.CODED 
    "NEXT.PCTCHG.OVER.3MO(GSPC.DELAYZERO.ABS.CLOSE)"                              -> ALL.PREDICTEES.HARD.CODED[["NEXT.PCTCHG.OVER.3MO(GSPC.DELAYZERO.ABS.CLOSE)"]]
    # "NEXT.PCTDRAWDOWN.OVER.3MO(GSPC.DELAYZERO.ABS.CLOSE,GSPC.DELAYZERO.ABS.LOW)"  -> ALL.PREDICTEES.HARD.CODED[["NEXT.PCTDRAWDOWN.OVER.3MO(GSPC.DELAYZERO.ABS.CLOSE,GSPC.DELAYZERO.ABS.LOW)"]]
    # "NEXT.PCTCHG.OVER.3MO(GDP.DELAYSIX.ABS.ADJUSTNOW)"                            -> ALL.PREDICTEES.HARD.CODED[["NEXT.PCTCHG.OVER.3MO(GDP.DELAYSIX.ABS.ADJUSTNOW)"]]
    
    bookmark_here <- 1 
    
    for(var.all.pred.hard in ALL.PREDICTEES.HARD.CODED) {
      
      for(var.testtraindates in TestTrainDates) {
        
        # for right now ( just this one )
        if( var.testtraindates[["ListName"]] == "Test2008" ) {
        
          # for right now ( just this one )
          if( var.all.pred.hard == "NEXT.PCTCHG.OVER.3MO(GSPC.DELAYZERO.ABS.CLOSE)" ) {
            
            # 'right now' 
            var.all.pred.hard -> CURR.PREDICTEE 
            
            # 'right now' directly from the loop above 
            CURR.OBSERVEES    ->  CURR.OBSERVEES
            
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
            model.data.CURR.OBSERVEES.CORRELATIONS.INDEXES  <- findCorrelation(model.data.CURR.OBSERVEES.CORRELATIONS.MATRIX) 
            # default: cutoff = 0.9  will produce 61 observees ( some long term data )
            #                   0.25 will product 9 points ( all 2MO data )
            
            model.data.CURR.OBSERVEES.COLNAMES.REMOVED <- c()
            # handle zero correctly
            if(model.data.CURR.OBSERVEES.CORRELATIONS.INDEXES[1] != 0) {
              
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
    
    # *** LEFT_OFF ***
    # [X]  !!!! VERIFY THAT THE DATA IS ALIGNED TO 14-12-31: Browse[2]> View(data.frame(t(tail(model.data.test,10))))
    #   solve mystery of NAs [ ] and NaNs [ ]
    # [LATER] better variable NAMES
    # [LATER] fscaret ( find variable importance) means(reduce CURR.OBSERVEES)
    
    # predictees area    
    
    ## "NEXT.RANGE.3(GSPC.DELAYZERO.ABS.LOW)" -> ALL.OBSERVEESPREDICTEES[["NEXT.RANGE.3(GSPC.DELAYZERO.ABS.LOW)"]] 
    ## CURR.PREDICTEE <- "NEXT.RANGE.3(GSPC.DELAYZERO.ABS.LOW)" 
    # end predictees area
    
    # begin observees area ( remove CURR.PREDICTEE )   
    
    # include all
    ## ALL.OBSERVEESPREDICTEES -> CURR.OBSERVEES 
    
    # obviously remove the PREDICTEE 
    ## NULL -> CURR.OBSERVEES[["NEXT.RANGE.3(GSPC.DELAYZERO.ABS.LOW)"]] 

    # XOR
    # exclude all
    ## list(   
    ##   GSPC.DELAYZERO.ABS.CLOSE = "GSPC.DELAYZERO.ABS.CLOSE"
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
    
    
    list() -> PREDICTORS
    
    list() -> OBSERVATIONS

    
    setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R") # TO BE CHANGED LATER
    
    the_end_debug_bookmark_here <- 1
    
    Sys.setenv(TZ=oldtz)
    
    print("Done with main_rcsnsight2_999()")
  
  }  
  main_rcsnsight2_999_inner(pauseat) # should have been do.call
  
}
# main_rcsnsight2_999()
# debugSource('N:/MyVMWareSharedFolder/rcsnsight1/R/main-rcsnsight2-999.R') 
# rm(list=ls(all.names=TRUE))

########################       


