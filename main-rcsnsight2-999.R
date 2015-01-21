
## search for "LEFT_OFF"

# main-rcsnsight2-999.R 

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width 

setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R") # TO BE CHANGED LATER 

bookmark_here <- 1

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

    require(Holidays)
    require(TimeWarp)
    require(quantstrat) # require(quantmod) # path: xts zoo
    
    # require(require(PerformanceAnalytics) # POSSIBLE FUT ( and through require("quantstrat") )
    
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "^GSPC"
      , finSymbolAttributes = c("Close","Low")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = 0
    ) -> GSPC.DELAYZERO
    
    bookmark_here <- 1
    
    # NBER ( x3 x4x6 month lag )
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "USRECM"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> USRECM.DELAYSIX
    
    bookmark_here <- 1
    
    # Chauvet/Piger ( 3 month lag ) ( sometimes 2 month lag) 
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "RECPROUSM156N"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> RECPROUSM156N.DELAYTHREE
    
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
    ) -> GDP.DELAYSIX
    
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
    ) -> GS10.DELAYZERO
    
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
    ) -> DGS3MO.DELAYZERO
    
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
    ) -> DFF.DELAYZERO
    
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
    ) -> UNEMPLOY.DELAYONE
    
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
    ) -> UEMPMED.DELAYZERO
    
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
    ) -> POP.DELAYTWO
    
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
    ) -> UNRATE.DELAYONE
    
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
    ) -> CPILFESL.DELAYONE
    
    bookmark_here <- 1
    
    ### prob not useful: would need TOTAL_PRIV_DEBT TOTAL_PUBLIC_DEBT ###
    
    # abrupt start in 2002( and does not go back that far )
    
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
    ) -> TREAST.DELAYZERO
    
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
    ) -> MBST.DELAYZERO
    
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
    
    retrieveSymbolsQuantmodRdata(
      finSymbol = "INDPRO"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> INDPRO.DELAYONE
    
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
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "NAPM"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) -> NAPM.DELAYONE
    
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
    ) -> TCU.DELAYONE
    
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
    
    retrieveSymbolsQuantmodRdata(
        finSymbol = "A261RL1Q225SBEA"
      , finSymbolRemoteSource = "Quantmod_FRED"
      , finSymbolAttributes = c("Close")
      , initDate = "1950-03-01"
      , subtractOffDaysSpec = -1
    ) ->  A261RL1Q225SBEA.DELAYSIX
    
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
    ) -> A576RC1Q027SBEA.DELAYFIVE
    
    
    
    bookmark_here <- 1
    
    retrieveSymbolsmultplRdata(
        finSymbol = "SP500.12M.EPS"
      , finSymbolAttribute = "Close"
    )  -> SP500.12M.EPS.DELAYTWO  # "12.month.EPS.Close" ( web table header)
    
    retrieveSymbolsmultplRdata(
        finSymbol = "SP500.REAL.EARN.GR.PCT"
      , finSymbolAttribute = "Close"
    )  -> SP500.REAL.EARN.GR.PCT.DELAYTWO # "SandP.500.Real.Earnings.Growth.Pct.Close"
    
    retrieveSymbolsmultplRdata(
        finSymbol = "SP500.PE.RATIO"
      , finSymbolAttribute = "Close"
    )  -> SP500.PE.RATIO.DELAYZERO # "SandP.500.PE.Ratio.Close" ( web table header)
    
    retrieveSymbolsmultplRdata(
        finSymbol = "SP500.BV.PER.SHARE"
      , finSymbolAttribute = "Close"
    )  -> SP500.BV.PER.SHARE.DELAYTHREE  # "SandP.500.Book.Value.Close"
    
    
    bookmark_here <- 1 
    
    the_end_debug_bookmark_here <- 1
    
    
    ## LEFT_OFF
    ## [x] fix the below ( delay(if_any), save )
    ## 
    ## [x]               SP500.12M#  
    ## [x]               PE.RATIO
    ## 
    ## [x]               USRECM 
    ## [x]               RECPROUSM156N
    
    ## [ ] wed/th - get FROM FRED
    ## [x]   GDP garanteed
    ## [x]   10  year bond
    ## [x]    3  year bill
    ## [x]   effective funds rate
    ##   [ ] (TO DO) 'real earnings growth or similar' = 'EPS - 10 year treasury yield'
    ## [x]   unemployment
    ## [x]   inflation
    ## [SORTOF]   actions of the fed
    
    ## LEFT_OFF
    ## [ ] (SPREAD) difference between yields on 10-year Treasury bonds and 3-month Treasury bills
    ## [ ] PCTCHANGE(WHERE APPROPRIATE)  ( [ ] consider renaming prog variables to .ABS )
    ## [ ] DO                SMA 1,2,3,...12, and
    ## [ ] INDICATOR(NOW PERCENT ABOVE) SMA
    
    ## [ ]   re-UMCENT ???

    ## 
    ## [ ] DURING THE WEEK: ABSVAL VELOCITY ACCEL JERK;  DOUBLE TRIPLE
    ## [ ]                  MOVING AVE(1:8), FLAG_ABOVE_MA
    
    ## DYNAMIC INPUT
    
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

