
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
options(error = browser)

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width
options(sqldf.driver = "PostgreSQL") ### OLD SQLite"

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




main_rcsnsight1_999 <- function(pauseat=NULL) {

  # require(Hmisc)  # NOT USED YET ( BUT WILL BE SOON )

  # require(testthat) # NOT USED HERE

  # require(tcltk)     # dyanically called by gsubfn
  # require(Rgraphviz) # NOT 'really' NEEDED for gsubfn

  require(sqldf)

  # require(lubridate) # NOT
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
  require(quantstrat)  # calls required functions ( IN USE )
  require(TimeWarp)
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
  
  dppgrconn <- src_postgres(
       dbname   = "ets"
    ,  user     = "Administrator"
    ,  password = "postgre"
    ,  host     = "localhost"
    ,  port     = 5432 
  )
  
  
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

  L_GSPC <- cbind( 1:NROW(GSPC),rev(1:NROW(GSPC)) ,as.numeric(index(GSPC)), coredata(GSPC))
  colnames(L_GSPC)[c(1,2,3)] <- c("NROW_IDX","REV_NROW_IDX","UNXDTE") # SHOULD KEEP JUST ONE!!
  colnames(L_GSPC)<-gsub("\\.","_",colnames(L_GSPC))
  L_GSPC <- tbl_dt( L_GSPC )
  # str(L_GSPC)
  # rowse[2]> str(L_GSPC)
  # Classes ‘tbl_dt’, ‘tbl’, ‘data.table’ and 'data.frame':	775 obs. of  4 variables:
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
  suppressWarnings(suppressMessages(sqldf('DROP TABLE IF EXISTS "GSPC"', connection = dppgrconn$con)))

  GSPC_tbl_postgres <- copy_to(dppgrconn, df = L_GSPC , name = "GSPC", temporary = FALSE
    , indexes = list(
          "NROW_IDX"
        , "REV_NROW_IDX"
        , "UNXDTE"
      )
  )

  # May? have to put a slight Pause here before the query?

  # sqldf - works - everytime
  # str(sqldf('SELECT * FROM "GSPC"', connection = dppgrconn$con))
  # Browse[2]> str(sqldf('SELECT * FROM "GSPC"', connection = dppgrconn$con))
  # 'data.frame':	775 obs. of  4 variables:
   # $ NROW_IDX    : num  1 2 3 4 5 6 7 8 9 10 ...
   # $ REV_NROW_IDX: num  775 774 773 772 771 770 769 768 767 766 ...
   # $ UNXDTE      : num  -7216 -7186 -7155 -7125 -7094 ...
   # $ GSPC_Close  : num  17.3 18 18.8 17.7 17.8 ...
  
  
  # NBER ( 3 month lag ) VERIFIED NEW!  USRECMFIX : FIX : SUBTRACT OFF ONE DAY [x]
  
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
  
  # OECD ( 3 month lag )  VERIFIED NEW!      USARECM: FIX : SUBTRACT OFF ONE DAY [x]

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
  
  # left join .External("mergeXts"? - complains about 'left join meant for 2' 'localtime is not UTC' 
  suppressWarnings(merge(GSPC,USRECM,RECPROUSM156N,USARECM,join='left',tz="UTC")) -> GALAXY_L  

  # TOO EARLY - have to do below anyways
  # keep complete.cases ( zoo::na.trim is equivalent )
  # na.trim(GALAXY_L[,!(colnames(GALAXY_L) %in% "tz")])  -> GALAXY_L 
  
  # the benchmark merge.xts? - outer
  symbol_OHLC_lag_k_periods(GSPC,"GSPC",0:1, qmOHLC = "Lowz")           -> GSPC_LAGS_LOW 
  suppressWarnings(merge(GALAXY_L,GSPC_LAGS_LOW,join='outer',tz='UTC')) -> GALAXY_L 
  symbol_OHLC_lag_k_periods(GSPC,"GSPC",0:1)                        -> GSPC_LAGS 
  suppressWarnings(merge(GALAXY_L,GSPC_LAGS,join='outer',tz='UTC')) -> GALAXY_L 

  
  
  # all others ( not the benchmark )
  symbol_OHLC_lag_k_periods(USRECM,"USRECM",0:1)                      -> USRECM_LAGS 
  suppressWarnings(merge(GALAXY_L,USRECM_LAGS,join='outer',tz='UTC')) -> GALAXY_L
  symbol_OHLC_lag_k_periods(RECPROUSM156N,"RECPROUSM156N",0:1)               -> RECPROUSM156N_LAGS 
  suppressWarnings(merge(GALAXY_L,RECPROUSM156N_LAGS,join='outer',tz='UTC')) -> GALAXY_L
  symbol_OHLC_lag_k_periods(USARECM,"USARECM",0:1)                     -> USARECM_LAGS 
  suppressWarnings(merge(GALAXY_L,USARECM_LAGS,join='outer',tz='UTC')) -> GALAXY_L
  
  # remove those uselss 'tz.#' columns ( aesthetics )
  # will leave just "tz" behind
  GALAXY_L[,!grepl("tz.",colnames(GALAXY_L))] -> GALAXY_L
  
  # remove those .Close ( and .Low ) columns
  # so not to break quantmod::Cl()
  GALAXY_L[,!grepl(".Close",colnames(GALAXY_L))] -> GALAXY_L
  GALAXY_L[,!grepl(".Low",colnames(GALAXY_L))] -> GALAXY_L
  
  
  # AGAIN remove the 'extra records that have crept in because of the 
  #  should ACTUALLY be removed after each 'merge' ( currenly does not break anything )
  # 'outer' join to the benchmark
  # keep complete.cases ( zoo::na.trim is equivalent )
  na.trim(GALAXY_L[,!(colnames(GALAXY_L) %in% "tz")])  -> GALAXY_L 
  
  # KEEP
  # if I want to just work with a SAMPLE
  #   as in 'pointless having everything: as sample of 300 as good as 3000' 
  #      'to much memory' OR 'get the same results' after 300'
  # GALAXY_L[sample(1:NROW(GALAXY_L),300,replace=FALSE),] -> GALAXY_L
  
  # make sure I look at current result of long running results 'on the fly'
  capture.output(print(paste0(as.character(Sys.Date())))
    , file = "./tests/main-rcsnsight1-999_runRESULTS.txt"
    , append=TRUE
  )
  
  the_end_debug_bookmark_here <- 1
  
  # dbDisconnect(contspgr)
  ## dbDisconnect(condbpgr)
  # dbDisconnect(contssqll)
  ## dbDisconnect(condbsqll)
  dbDisconnect(dppgrconn$con)
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
# 

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

