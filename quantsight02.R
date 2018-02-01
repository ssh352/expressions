
# quantsight02.R
# R 3.4.3 commonly debugged with RStudio-1.1.383

# 100% NEVER-TESTED

# slide 38
# Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
# Data integrity check
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
  text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
    print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
    print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
    print("duplicate timestamps in account summary")
  }
  return(ok)
}
# Data integrity check
# 
# slide 38
# Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
# checkBlotterUpdate("will5000ind_f_drblacbs", "will5000ind_f_drblacbs")

# FOMC Cycle Trading Strategy in Quantstrat
# March 14, 2015
# By Peter Chan
# http://www.returnandrisk.com/2015/03/fomc-cycle-trading-strategy-in.html
# http://www.r-bloggers.com/fomc-cycle-trading-strategy-in-quantstrat/
# 

GeneralFee <- function(TxnQty, TxnPrice, Symbol, ...) {
  
  # list(...) # does NOT pass through add.rule( arguments = 
  # if(hasArg(ExecutionCost)) ExecutionCost=match.call(expand.dots=TRUE)$ExecutionCost

  ExecutionCost <- 0 # CAREFUL: ALSO HARDCODED ELSEWHERE
  
  return(abs(TxnQty) * TxnPrice * - ExecutionCost)
  
}

# We will use a bet size of 100% of equity for all trades. 
# This may not be optimal in developing trading systems but will allow for 
# easy comparison with the buy and hold passive strategy, which is 100% allocated
# 
# https://www.r-bloggers.com/fomc-cycle-trading-strategy-in-quantstrat/
# https://github.com/returnandrisk/r-code/blob/master/FOMC%20Cycle%20Trading%20Strategy%20in%20Quantstrat.R

# AUTHOR BUYS AT CLOSE 
################################################################################
# run strategy backtest                                                        #
################################################################################
# applyStrategy(strategy = qs.strategy, portfolios = qs.portfolio)

# ANDRE ADDED BACK MISSING 2018 ARGUMENTS
################################################################################
# custom order sizing function to allocate 100% of equity to a trade           #
################################################################################

osAllIn <- function(data, timestamp, orderqty, ordertype, orderside, portfolio, symbol, ruletype, digits, 
                    roundqty = FALSE, ...) {
  
  # global variables
  # , initDate = initDate, initEq = initEq, ExecutionCost = ExecutionCost
  if(hasArg(initDate)) initDate=match.call(expand.dots=TRUE)$initDate
  if(hasArg(initEq))     initEq=match.call(expand.dots=TRUE)$initEq
  
  if(hasArg(initEq))  ExecutionCost=match.call(expand.dots=TRUE)$ ExecutionCost


  # NOTE: to manage "prefer"(Open,High,Low,Close) pass "prefer" variable to add.rule
  # if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
  
  # SEE
  # https://github.com/returnandrisk/r-code/blob/master/FOMC%20Cycle%20Trading%20Strategy%20in%20Quantstrat.R
 
  # ANDRE                                                                             # do not know how to get
  if(!periodicity(mktdata)$scale %in% c("yearly", "quarterly", "monthly", "daily WITH allowMagicalThinking == T")) {
    # ANDRE
    # next bar Close
    # AUTHOR
    # authors original date
    # hack to get correct index for trading on today's close  
    idx <- which(index(mktdata) == as.Date(timestamp)) + 1
  } else { 
    # ANDRE
    # this bar Close
    # see   ruleOrderProc.R > ruleOrderProc -> switch(freq$scale
    idx <- which(index(mktdata) == as.Date(timestamp)) 
  }
  
  close <- as.numeric(Cl(mktdata[idx, ]))
  txns <- getTxns(portfolio, symbol, paste0(initDate, "::", timestamp))
  # calculate unrealised pnl
  tmp <- getPos(portfolio, symbol, timestamp)
  unreal.pl <- (close - as.numeric(tmp$Pos.Avg.Cost)) * as.numeric(tmp$Pos.Qty)
  # round qty down or not
  if (roundqty) {
      orderqty <- floor((initEq + sum(txns$Net.Txn.Realized.PL) + unreal.pl) / 
                            (close * (1 + ExecutionCost))) * sign(orderqty)
  } else {
      orderqty <- (initEq + sum(txns$Net.Txn.Realized.PL) + unreal.pl) / 
          (close * (1 + ExecutionCost)) * sign(orderqty)
  } 
  return(orderqty[1])
}
# function is LOCATED HERE because R studio 
# would not let me place a BREAKPOINT inside OF non-Global osAllIn 
# REQUIRED to be IN THE GLOBAL ENV so QUANTSTRAT functions CAN see
# already in the global env
# assign("osAllIn", osAllIn, envir = .GlobalEnv)
    
quantstrat02 <- function() {

# quantsight01.R
# R 3.4.3 commonly debugged with RStudio-1.1.383

# As of 2013-08-12, apply.paramset does not appear to run properly in parallel on
# Windows. To run on a Windows platform, load the doParallel package but do not call
# the registerDoParallel function; apply.paramset will then be able to run in sequential
# rather than parallel mode.
# slide 40
# Quantstrat_IV(08_2013))(GuyYollin)_quantstrat(blotter).pdf

# # I HAVE 'OTHER' NOTES THAT ARE BETTER THAN THIS
# # distributions, constraints, and paramsets
# if( Sys.info()['sysname'] == "Windows" ) {
#   require(doParallel)
#   registerDoParallel(cores=detectCores()/2)
# } else {
#   require(doMC)
#   registerDoMC(cores=detectCores()/2)
# }
# # slide 34
# # Quantstrat_IV(08_2013))(GuyYollin)_quantstrat(blotter).pdf
  
  
  quanstrat02_inner <- function() {
  
    # "function form" based loosely on "https://github.com/AndreMikulec/expressions/blob/master/faber.R"
    # based on                         "https://github.com/braverock/quantstrat/blob/master/demo/faber.R" JAN 2018

    # BASED ON 
    # # View of /pkg/quantstrat/demo/faber.R
    # # Parent Directory Parent Directory | Revision Log Revision Log
    # # Revision 1594 - (download) (annotate) 
    # # Sat Mar 29 20:39:45 2014 UTC (5 months, 1 week ago) by braverock 
    # # File size: 7592 byte(s)
    # # - fix rebalancing and rebalancing demos
    # # - update roxygen docs
    # # - bump version
  

    # Load required libraries
    ### require(quantstrat) 
    # can! use valuesight function get_bankruptcy_filing_counts_eoq_xts ( see function for info ) 
    
    # required ( If I am running in a function )
    if(!is.null(sys.call(sys.parent()))) {
      if (!exists('.blotter' , envir=.GlobalEnv))  { .GlobalEnv$.blotter   <- new.env() } 
      if (!exists('.strategy', envir=.GlobalEnv))  { .GlobalEnv$.strategy  <- new.env() }
    }
    # NOTE: .instrument is stored here: FinancialInstrument:::.instrument
    # NOTE: .getSymbols is stored here: .GlobalEnv$.getSymbols
    # CHECK THIS [ ] # COME BACK
    
    ops <- options()
    
    options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
    options(digits = 2) 
    options(max.print=99999)
    options(scipen=255) # Try these = width
    
    #correct for TZ issues if they crop up
    oldtz<-Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    # Try to clean up in case the demo was run previously
    suppressWarnings(rm("account.will5000ind_f_drblacbs"   ,"portfolio.will5000ind_f_drblacbs", pos=.blotter))
    suppressWarnings(rm("order_book.will5000ind_f_drblacbs",                                    pos=.strategy))
  
    ##### PLACE DEMO AND TEST DATES HERE #################
    #
    #if(isTRUE(options('in_test')$in_test))
    #  # use test dates
    #  {initDate="2011-01-01" 
    #  endDate="2012-12-31"   
    #  } else
    #  # use demo defaults
    #  {initDate="1999-12-31"
    #  endDate=Sys.Date()}
  
    # # Set initial values
    # initDate='1997-12-31'
    # initEq=100000
    # 
    # # Set up instruments with FinancialInstruments package
    # currency("USD")
    # # ANDRE JUST SIMPLER TO DEBUG
    # symbols = "XLE" #  c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
    # for(symbol in symbols){ # establish tradable instruments
    #     stock(symbol, currency="USD",multiplier=1)
    # }
    # 
    # # Load data with quantmod
    # #getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1998-01-01')
    # ### Download monthly data instead?
    # ### GSPC=to.monthly(GSPC, indexAt='endof')
    # getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01')
    
    suppressWarnings(rm("WILL5000IND", "DRBLACBS", envir = .GlobalEnv))
    suppressWarnings(rm("get_DRBLACBS", envir = .GlobalEnv)) 
  
    suppressWarnings(rm("BUSCH11BNKR", envir = .GlobalEnv))
    
    # (lone) symbol that I test against
    # FRED: Wilshire 5000 Total Market Index (WILL5000IND)
    # updated daily
    # https://fred.stlouisfed.org/data/WILL5000IND.txt
    # 
    # Title:               Wilshire 5000 Total Market Index
    # Series ID:           WILL5000IND
    # Source:              Wilshire Associates
    # Release:             Wilshire Indexes (Not a Press Release)
    # Seasonal Adjustment: Not Seasonally Adjusted
    # Frequency:           Daily, Close
    # Units:               Index
    # Date Range:          1970-12-31 to 2018-01-23
    # Last Updated:        2018-01-24 7:41 AM CST
    # Notes:               The total market indexes are total market returns, which do include
    #                      reinvested dividends. Copyright, 2016, Wilshire Associates
    #                      Incorporated. Reprinted with permission. More information about the
    #                      various indexes from Wilshire Associates can be found at
    #                      http://www.wilshire.com/Indexes/.
    # 
    # DATE         VALUE
    # 1970-12-31    1.00
    # ...
    
    # indicator
    # FRED: Delinquency Rate on Commercial and Industrial Loans, All Commercial Banks (DRBLACBS) 
    # seasonally adjusted: updated every 3 months on the first of day month 
    # with a "just less than" 5 month delay 
    # between 'index date' and 'release date' ('luckily' near the end(27th) of the month)
    # (BUT VERY ACCURATE and GOOD)
    # https://fred.stlouisfed.org/data/DRBLACBS.txt
    # 
    # Title:               Delinquency Rate on Commercial and Industrial Loans, All Commercial Banks
    # Series ID:           DRBLACBS
    # Source:              Board of Governors of the Federal Reserve System (US)
    # Release:             Charge-Off and Delinquency Rates on Loans and Leases at Commercial Banks
    # Seasonal Adjustment: Seasonally Adjusted
    # Frequency:           Quarterly, End of Period
    # Units:               Percent
    # Date Range:          1987-01-01 to 2017-07-01
    # Last Updated:        2017-11-27 3:21 PM CST
    # Notes:               
    # 
    # DATE       VALUE
    # 1987-01-01  6.75
    # ...
    

    # Set up instruments with FinancialInstruments package
    currency("USD")
    symbols = c("WILL5000IND") # indicator DRBLACBS/BUSCH11BNKR is(are) not as a 'tradable'
    # establish tradable instruments
    stock("WILL5000IND", currency="USD",multiplier=1)
  
    # load the stock market
    
    # do not attack
    if(!file.exists("WILL5000IND.RData")) {
      # all data # currently quantmod::getSymbols.FRED gets *all* of the data
      WILL5000IND <- getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE)
      save(WILL5000IND, file = "WILL5000IND.RData", envir = environment())
    } else {
      load(file = "WILL5000IND.RData", envir = environment())
    }
  
    # SEEMS NO[T] [LONGER] REQUIRED FOR monthly/Date
    # # 'quantstrat is 'seconds' based,  so required
    # # "POSIXt","POSIXct"
    # # required 
    # # getSymbols(. . . , index.class=c("POSIXt","POSIXct")) # convert to early morning
    # # but index.class is NOT a parameter in getSymbols.FRED
    # 
    # # adjust and format
    # indexClass(WILL5000IND)  <- c("POSIXt","POSIXct")
    # index(WILL5000IND) <- index(WILL5000IND) + ( 3600 * 23 ) # 6 p.m. Eastern / 11 p.m. UTC # 'American' markets are closed
    # indexFormat(WILL5000IND) <- "%Y-%m-%d"

    
    # format the stock market
    WILL5000IND <- suppressWarnings(to.monthly(WILL5000IND, indexAt="lastof", drop.time=TRUE)) # default OHLC = TRUE # created
    # suppressWarnings: missing values removed from data
    
    # SEEMS NO[T] [LONGER] REQUIRED FOR monthly/Date
    # # This is a 'daily' BUT ...
    # # warning # some earlier data only has month-end data
    # # my index had been convertd to a class of Date
    # # I convert it back
    # indexClass(WILL5000IND) <- c("POSIXt","POSIXct")
    # index(WILL5000IND) <- index(WILL5000IND)       + ( 3600 * 23 ) # 6 p.m. Eastern / 11 p.m. UTC # 'American' markets are closed
    # indexFormat(WILL5000IND)<-"%Y-%m-%d"
    # 
    # # > head(WILL5000IND,1)
    # #          WILL5000IND.Open WILL5000IND.High WILL5000IND.Low WILL5000IND.Close
    # # 1970-12-31 
    
    # load the indicator
    if(!file.exists("DRBLACBS.RData")) {
    DRBLACBS   <- getSymbols("DRBLACBS",  src = "FRED", auto.assign = FALSE) 
      save(DRBLACBS, file = "DRBLACBS.RData", envir = environment())
    } else {
      load(file = "DRBLACBS.RData", envir = environment()) 
    }
    # for convenience subtract off a day ( so I have the end of the month ) 
    index(DRBLACBS) <- index(DRBLACBS) - 1
    require(lubridate)
    index(DRBLACBS) <- index(DRBLACBS) %m+% months(5) # note if/when Machine Learning data model requires SOME work
  
    # SEEMS NO[T] [LONGER] REQUIRED FOR monthly/Date
    # # adjust and format
    # indexClass(DRBLACBS) <- c("POSIXt","POSIXct")
    # index(DRBLACBS) <- index(DRBLACBS)       + ( 3600 * 23 )  # 6 p.m. Eastern / 11 p.m. UTC # 'American' markets are closed
    # indexFormat(DRBLACBS)<-"%Y-%m-%d"

    # can load the OTHER indicator
    # want (later) SMA2
    BUSCH11BNKR <- get_bankruptcy_filing_counts_eoq_xts(pub_dates = NA)[,"bus_ch_11"]
    colnames(BUSCH11BNKR) <- "BUSCH11BNKR"
    
    # since, work is done per instrument(symbol) 'in a LOOP'
    # and ONLY using the data of mktdata that is the 'per instrument(symbol) single xts object'
    # than I have to add other external data into the 'per instrument(symbol) single xts object'
    # S3 dispatch merge.xts
    #
    ## WILL5000IND <- merge(WILL5000IND, DRBLACBS)
    WILL5000IND <- merge(WILL5000IND, DRBLACBS, BUSCH11BNKR) # note: early dates limited very much by BUSCH11BNKR

    # earliest date of WILL5000IND is 1970-12-31
    # earliest date of DRBLACBS    is 1987-01-01" # subtract by 1 day ... 1986-12-31 then add 5 months ... 1987-05-31
    # earliest date of BUSCH11BNKR is 2001-03-31 (already account for 'publishing delay: see function definition)
    
    # since DRBLACBS only occurs ever three months, then copy it forward
    # so, I have a 'value' per month
    # S3 dispatch na.locf.xts # read help ? na.locf.xts
    WILL5000IND <- na.locf(WILL5000IND)
    # need to trim off early records of which the column DRBLACBS only has NA values
    WILL5000IND <- na.trim(WILL5000IND, sides = "left") # earlier observations ar removed
    # copy back to the global environment, so the quantstat functions can find the instrument(symbol) mktdata
    assign("WILL5000IND", WILL5000IND, envir = .GlobalEnv)
  
    # indicator
    # needed for add.indicator and quantstrat programs
    get_DRBLACBS <- function(x) { return(WILL5000IND[,"DRBLACBS"]) }
    # place so that quanstrat programs can find it
    assign("get_DRBLACBS", get_DRBLACBS, envir = .GlobalEnv)
  
    # indicator
    # needed for add.indicator and quantstrat programs
    get_BUSCH11BNKR <- function(x) { return(WILL5000IND[,"BUSCH11BNKR"]) }
    # place so that quanstrat programs can find it
    assign("get_BUSCH11BNKR", get_BUSCH11BNKR, envir = .GlobalEnv)
    
    # # SKIP THIS . . . instead use SMA 4 in add.indicator # #
    #
    # # MOVE only on a change above/belwo
    # get_DRBLACBS_SIG <- function(x) {
    # 
    # require(xts)
    #   
    # # testing
    # # x <- head(WILL5000IND[,"DRBLACBS"],11)
    # # lag.xts ...
    # # ifelse(lag.xts(x,4) > x, -1, 1)
    #   
    # # S3 dispatch ifelse.zoo
    # # increasing/decreasing/same
    # 
    # # same becomes > increasing/decreasing
    #  
    # #  1 increasing slope (not-desireable ..  stay out of the market)
    # # -1 decreasing slope (desirable      ... stay in the market)
    # return(xout)
    # 
    # }
    # # place so that quanstrat programs can find it
    # assign("get_DRBLACBS_SIG", get_DRBLACBS_SIG, envir = .GlobalEnv)
    

    
    # Yollin: (SHOULD HAVE BEEN) BEFORE! earliest date (required)
    # DOES NOT MATTER ANYMORE
    # LINE IS THE SAME ... SO JUST USE THE first INDEX value
    # Txn.Qty Txn.Price Txn.Fees   Txn.Value Txn.Avg.Cost Net.Txn.Realized.PL
    # 1987-05-31      0.000      0.00        0        0.00         0.00               0.000
    initDate=as.character(zoo::as.Date(head(index(WILL5000IND),1)))  # ,1)) -1) # BEFORE!

    # PROBLEMS? xDATE BEING GREATER THAN TODAY?x, +NOT A REAL MARKET DATE!
    # updatePortf(Portfolio="will5000ind_f_drblacbs",Dates=paste('::',as.Date(Sys.time()),sep=''))
    # see applyStrategy
    if(tail(index(WILL5000IND),1) > Sys.Date()) {
      finalDate = paste('::',as.character(zoo::as.Date(as.yearmon(tail(index(WILL5000IND),1))) - 1),sep='')
    } else {
      finalDate = paste('::', as.Date(Sys.time()),sep='')
    } 
    # finalDate VARIABLE currently NOT USED
    
    # optional: ( If I am running in a function )
    # does not affect the program ( but for consistency if nothing else )
    if(!is.null(sys.call(sys.parent()))) {
      if(exists(".getSymbols")){
        .GlobalEnv$.getSymbols <- .getSymbols 
      }
    }
    # 
    # for(symbol in symbols) {
    #     x<-get(symbol)
    #     x<-to.monthly(x,indexAt='lastof',drop.time=TRUE)
    #     indexFormat(x)<-'%Y-%m-%d'
    #     colnames(x)<-gsub("x",symbol,colnames(x))
    #     assign(symbol,x)
    #     # required ( If I am running in a function )
    #     if(!is.null(sys.call(sys.parent()))) {
    #       assign(symbol,x, envir = .GlobalEnv)
    #     }
    # }
  
    # Set initial values 
    initEq=100000.00 
    # Initialize portfolio and account
    initPortf("will5000ind_f_drblacbs", symbols=symbols, initDate=initDate)
    initAcct("will5000ind_f_drblacbs", portfolios="will5000ind_f_drblacbs", initDate=initDate, initEq=initEq)
    initOrders(portfolio="will5000ind_f_drblacbs", initDate=initDate)
    
    # osMaxPos
    # for(symbol in symbols){
    #   addPosLimit("will5000ind_f_drblacbs", symbol, initDate, maxpos=Inf, minpos=-Inf)
    # }
    
    message("setup completed")
  
    # Initialize a strategy object
    strategy("will5000ind_f_drblacbs", store=TRUE)
  
    # faber indicators NOT USED
    # # Add an indicator
    # add.indicator("will5000ind_f_drblacbs", name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
    
    # Add an indicator
    
    # strategy                              # function    # function arguments ( could be NOT here and be in applyStrategy parameters )
    # eventually, in mktdata, label "SMA4" becomes column SMA.SMA4 of values (1,NA)
    # sigCrossover/ruleSignal/sigval=TRUE( SMA.SMA4 = 1 )
    add.indicator("will5000ind_f_drblacbs", name = "SMA", arguments = list(x = quote(get_DRBLACBS(mktdata)),    n=4), label="SMA4DR") 

    add.indicator("will5000ind_f_drblacbs", name = "SMA", arguments = list(x = quote(get_BUSCH11BNKR(mktdata)), n=7), label="SMA7BCH11")
    
    # faber signals NOT USED
    # # There are two signals:
    # # The first is when monthly price crosses over the 10-month SMA
    # add.signal("will5000ind_f_drblacbs",name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="gte"),label="Cl.gt.SMA")
    # # The second is when the monthly price crosses under the 10-month SMA
    # add.signal("will5000ind_f_drblacbs",name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="lt"),label="Cl.lt.SMA")
  
    # There are two signals
  
    # strategy
    # The first is when monthly DRBLACBS crosses over the 4-month SMA of DRBLACBS ( MORE Delinquencies)
    add.signal("will5000ind_f_drblacbs", name="sigCrossover",arguments = list(columns=c("DRBLACBS","SMA4DR"),relationship="gt"),label="DR.gt.SMA4DR")
    
    # The second is when the monthly DRBLACBS crosses under the 4-month of SMA DRBLACBS (LESS Delinquencies)
    add.signal("will5000ind_f_drblacbs", name="sigCrossover",arguments = list(columns=c("DRBLACBS","SMA4DR"),relationship="lt"),label="DR.lt.SMA4DR")
    
    # strategy
    # The first is when the monthly BUSCH11BNKR crosses over the 7-month SMA of BUSCH11BNKR ( MORE Delinquencies)
    add.signal("will5000ind_f_drblacbs", name="sigCrossover",arguments = list(columns=c("BUSCH11BNKR","SMA7BCH11"),relationship="gt"),label="BCH11.gt.SMA7BCH11")
    
    # The second is when the monthly BUSCH11BNKR crosses under the 7-month of SMA BUSCH11BNKR (LESS Delinquencies)
    add.signal("will5000ind_f_drblacbs", name="sigCrossover",arguments = list(columns=c("BUSCH11BNKR","SMA7BCH11"),relationship="lt"),label="BCH11.lt.SMA7BCH11")
    
    # faber rules NOT used
    # There are two rules:
    # The first is to buy when the price crosses above the SMA
    #   add.rule("will5000ind_f_drblacbs", name='ruleSignal', arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=1,        osFUN='osAllIn', ordertype='market', orderside='long', pricemethod='market',TxnFees=0), type='enter', path.dep=TRUE)
    # The second is to sell when the price crosses below the SMA
    #   add.rule("will5000ind_f_drblacbs", name='ruleSignal', arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all'                    , ordertype='market', orderside='long', pricemethod='market',TxnFees=0), type='exit',  path.dep=TRUE)
  
    # There are two rules

    # Note function exists: enable.rule(. . . 
    
    # HARD NOTE: ruleSignal HAS A 'prefer' # COME_BACK # VERIFY THIS
    # 'prefer' passed to 'getPrice'(value) and 'addOrder'(string:'Open')
    
    # recommend that you 
    #   prefer="Open" on all of your rules, otherwise you see the signal at day t, 
    #                                       but only buy on the close of day t+1, and so, miss an entire day's worth of edge.
    # 
    #  add.rule(. . .,  prefer="Open", . . . )
    # 
    # https://stat.ethz.ch/pipermail/r-sig-finance/attachments/20140403/74e51eb7/attachment.pl
    # 
    # Ilya Kipnis ilya.kipnis at gmail.com 
    # Thu Apr 3 11:01:43 CEST 2014
    # R-SIG-Finance] Fwd: quantstrat - stochastic oscillator overbought-oversold (OBOS) strategy
    # https://stat.ethz.ch/pipermail/r-sig-finance/2014q2/012336.html
    
    # strategy ( note: my buy/sell rules are REVERSED compared to faber.R )
    # 
    # ANDRE DEBUG: monthly(garanteed) periodicity(mktdata) 
    # does 'signalling and buying and selling on the same bar(that month)' # So I only want to buy/sell on a Close ( because THAT is how the program works )
    # 
    # add.rule( prefer = "????" ) # overrides applyStrategy
    #

    ExecutionCost <- 0 # CAREFUL: ALSO HARDCODED ELSEWHERE
    
    # The first is to SELL when the DRBLACBS crosses above the SMA of DRBLACBS(MORE Delinquencies) # 
    add.rule("will5000ind_f_drblacbs", name="ruleSignal", arguments = list(sigcol="DR.gt.SMA4DR", sigval=TRUE, orderqty="all"                , ordertype="market", TxnFees = "GeneralFee", orderside="long", pricemethod="market"),                                                                      type="exit", path.dep=TRUE) # , prefer = "High"
  
    # if eq        ... no change ... 
    # if SMA is NA ... no change ...

    # The second is to BUY when the DRBLACBS crosses below the SMA of DRBLACBS (LESS Delinquencies)                                                                                                                                # global variables can not be seen
    add.rule("will5000ind_f_drblacbs", name="ruleSignal", arguments = list(sigcol="DR.lt.SMA4DR", sigval=TRUE, orderqty=1,    osFUN="osAllIn", ordertype="market", TxnFees = "GeneralFee", orderside="long", pricemethod="market", initDate = initDate, initEq = initEq, ExecutionCost = ExecutionCost), type="enter", path.dep=TRUE) # , prefer = "High"

    
    # enable.rule(. . . 
    
    # The first is to SELL when the BUSCH11BNKR crosses above the SMA of BUSCH11BNKR(MORE Delinquencies) # 
    add.rule("will5000ind_f_drblacbs", name="ruleSignal", arguments = list(sigcol="BCH11.gt.SMA7BCH11", sigval=TRUE, orderqty="all"                , ordertype="market", TxnFees = "GeneralFee", orderside="long", pricemethod="market"),                                                                      type="exit", path.dep=TRUE) # , prefer = "High"
  
    # if eq        ... no change ... 
    # if SMA is NA ... no change ...

    # The second is to BUY when the BUSCH11BNKR crosses below the SMA of BUSCH11BNKR (LESS Delinquencies)                                                                                                                                # global variables can not be seen
    add.rule("will5000ind_f_drblacbs", name="ruleSignal", arguments = list(sigcol="BCH11.lt.SMA7BCH11", sigval=TRUE, orderqty=1,    osFUN="osAllIn", ordertype="market", TxnFees = "GeneralFee", orderside="long", pricemethod="market", initDate = initDate, initEq = initEq, ExecutionCost = ExecutionCost), type="enter", path.dep=TRUE) # , prefer = "High"

    
    
    # Process the indicators and generate trades
    # I want to buy on the "Close"(default) of the month
    # from Guy Yollin pdfs 
    # add.indicator ( function 'name' arguments = ) can be skipped here 
    #   and instead 'late bound' sent to applyStrategy(  parameters = )
    out <- applyStrategy(strategy="will5000ind_f_drblacbs", portfolios="will5000ind_f_drblacbs") # add.rule = "" # BUT add.rule( prefer = "High" ) overrides
    print("Strategy Loop:")
    # debug in ruleOrderProc, set a breakpoint at "switch(orderType"
    # print(orderbook[[portfolio]][[symbol]])
    # get the begin and end dates
    # print(mktdata["begin::end"])
    # outside debugging: compare to 
    # output of applyStrategy(above)
    # output of getTxns(below)

    # !CHANGE!
    # fixInNamespace(x = ".updatePosPL", ns = "blotter")
    # change: 
    #         wants to be specific: !is.timeBased(Dates): Dates = index(prices)
    # xor
    #                    all dates: is.null(Dates):     : Dates = index(prices)
    message("Begin trade blotter portfolio update")
    updatePortf(Portfolio="will5000ind_f_drblacbs") # , Dates= finalDate # NOT ANY GOOD ANYMORE
    updateAcct("will5000ind_f_drblacbs")
    updateEndEq("will5000ind_f_drblacbs")
    message("End trade blotter portfolio update")
    
    message("begin portfolio info")
    # blotter portfolio object
    # slide 57
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
                                # Portfolio
    thePortfolio = getPortfolio("will5000ind_f_drblacbs")
    print(data.frame(names(thePortfolio)))
    print(data.frame(names(thePortfolio$symbols)))
    print(data.frame(names(thePortfolio$symbols$WILL5000IND)))
    print(data.frame(names(thePortfolio$summary)))
    # portfolio transactions # slide 58
    # too much data
    # print(thePortfolio$symbols$WILL5000IND$txn)
    message("end portfolio info")
    
    # bivariate scatterplots or time-series plots 
    require(lattice) #  good for multi-panel plots
    # Plot of instrument P&L
    xyplot(thePortfolio$symbols$WILL5000IND$posPL.USD,type="h",col=4)
    # Plot of portfolio summary time series object
    xyplot(thePortfolio$summary,type="h",col=4)
    # slide 61-63
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    message("begin account info")
    # blotter account object
    theAccount = getAccount("will5000ind_f_drblacbs")
    print(data.frame(names(theAccount)))
    print(data.frame(names(theAccount$portfolios)))
    print(data.frame(names(theAccount$portfolios$will5000ind_f_drblacbs)))
    print(data.frame(names(theAccount$summary)))
    # by month, PL and End.Eq
    # slide 66
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    message("end account info")
    
    require(lattice)
    xyplot(theAccount$summary,type="h",col=4)
    # slide 52
    # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  
    message("begin strategy info")
    # slide 20
    # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    theStrategy <- getStrategy("will5000ind_f_drblacbs")
    print(data.frame(summary(theStrategy)))
    # indicators, signals, rules, constraints
    message("end strategy info")
    
    message("begin orderbook info")
    # slide 45
    # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    theOrderBook <- getOrderBook("will5000ind_f_drblacbs")
    print(data.frame(class(theOrderBook)))
    print(data.frame(names(theOrderBook)))
    print(data.frame(names(theOrderBook$will5000ind_f_drblacbs)))
    print(data.frame(names(theOrderBook$will5000ind_f_drblacbs$WILL5000IND)))
    print(data.frame(theOrderBook$will5000ind_f_drblacbs$WILL5000IND[,1:5]))
    message("end orderbook info")
    
    message("begin transactions")
    # Transactions
    # slide 34
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    print(getTxns(Portfolio="will5000ind_f_drblacbs", Symbol="WILL5000IND"))
    # per date
    # Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost Net.Txn.Realized.PL
    message("end transactions")
    
    message("begin View of portfolio returns")
    # of each portfolio or the entire account
                        # account    # Portfolios = NULL(default NULL (all portfolios))
                                     # Dates: xts style ISO 8601 date subset to retrieve, default NULL (alldates)
    portfolio_returns       <- PortfReturns("will5000ind_f_drblacbs")
    # slide 42
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    # rownames(returns) <- NULL # someimes to avoid errors using table.Arbitrary
    portfolio_returns       <- to.monthly(portfolio_returns)
    portfolio_returns$total <- rowSums(portfolio_returns)
    # print(portfolio_returns) # OHLC
    message("end View of portfolio returns")
    
    # # Cumulative returns by asset
    # # Plot individual asset returns ( of a multi-asset portfolio)
    # rets.multi <- PortfReturns(multi.asset)
    # colnames(rets.multi) <- symbols
    # rets.multi$TOTAL<-rowSums(rets.multi)
    # rets.multi <- rets.multi[,c("TOTAL",symbols)]
    # chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft", main="WILL5000IND Cumulative Returns")
    # # slide 66
    # # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    # # Return distribution analysis
    # chart.Boxplot(rets.multi, main = "WILL5000IND Returns", colorset= rich10equal)
    # # slide 68
    # # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  
    # # Risk-return scatter plot
    # # Annulized risk and return
    # (ar.tab <- table.AnnualizedReturns(rets.multi))
    # max.risk <- max(ar.tab["Annualized Std Dev",])
    # max.return <- max(ar.tab["Annualized Return",])
    # chart.RiskReturnScatter(rets.multi, main = "WILL5000IND Performance", colorset = rich10equal, xlim=c(0,max.risk*1.1),ylim=c(0,max.return))
    # # slide 69
    # # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  

    
    # hack for new quantmod graphics, remove later
    themelist<-chart_theme()
    themelist$col$up.col<-'lightgreen'
    themelist$col$dn.col<-'pink'
  
    # Only one RStudio graphics device is permitted
    if (!any(search() %in% "tools:rstudio")) {
      dev.new()
    }
    if (!any(search() %in% "tools:rstudio")) {
      layout(mat=matrix(1:(length(symbols)+1),ncol=2))
      for(symbol in symbols){
        # R Studio: Error in plot.new() : figure margins too large
        if (!any(search() %in% "tools:rstudio")) {
          chart.Posn(Portfolio="will5000ind_f_drblacbs",Symbol=symbol,theme=themelist,TA="add_SMA(n=10,col='darkgreen')")
        }
      }
    }
      
    # One vs The Other Performance
    # slide 42
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    if(!exists("WILL5000IND.ret")) WILL5000IND.ret <- Return.calculate(WILL5000IND$WILL5000IND.Close)
    # xor
    #   WILL5000IND.ret <- Return.calculate(WILL5000IND$WILL5000IND.Close, method = "log")
    returns.ret <- Return.calculate(getAccount("will5000ind_f_drblacbs")$summary$End.Eq)
    # xor
    #  returns.ret <- Return.calculate(getAccount("will5000ind_f_drblacbs")$summary$End.Eq, method = "log")
    bothreturns <- cbind(returns.ret,WILL5000IND.ret)
    colnames(bothreturns) <- c("drblacbs","BuyHold")
    print(bothreturns["2011"])
    # Rstudio?!
    # Error in par(op) : invalid value specified for graphical parameter "pin"
    if (!any(search() %in% "tools:rstudio")) {
      charts.PerformanceSummary(bothreturns, geometric=FALSE, wealth.index=TRUE)
    }
  	# if after Return.calculate(. . . method = "log" . . .)
  	# xor                                                              # default
  	#  charts.PerformanceSummary(bothreturns, geometric=TRUE, wealth.index=TRUE)
  	
    # # Consolidated equity curve ( of a multi-asset portfolio )
    # plot(theAccount$summary$End.Eq, main="Consolidated SPDR Equity Curve")
    # charts.PerformanceSummary(Return.calculate(theAccount$summary$End.Eq), geometric=FALSE, wealth.index=TRUE, main="Consolidated SPDR Performance")
    # # slide 71
    # # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    message("begin of both returns")
    # slide 45
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    # table.Arbitrary creates a table of statistics from a passed vector
    # of functions and vector of labels
    # 
    # # Compute performance statistics
    tab.perf <- table.Arbitrary(bothreturns,
    metrics=c(
      "Return.cumulative",
      "Return.annualized",
      "SharpeRatio.annualized",
      "CalmarRatio"),
    metricsNames=c(
      "Cumulative Return",
      "Annualized Return",
      "Annualized Sharpe Ratio",
      "Calmar Ratio"))
    print(tab.perf)
    
    # Compute risk statistics
    tab.risk <- table.Arbitrary(bothreturns,
    metrics=c(
      "StdDev.annualized",
      "maxDrawdown",
      "VaR",
      "ES"),
    metricsNames=c(
      "Annualized StdDev",
      "Max DrawDown",
      "Value-at-Risk",
      "Conditional VaR"))
    print(tab.risk)
  
    performance.stats.tab <- data.frame(
    rownames(tab.perf),tab.perf[,1],
    rownames(tab.risk),tab.risk[,1])
    print(performance.stats.tab)
    message("end of both returns")
    
    # Return and risk comparison
    # slide 52
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    print(table.AnnualizedReturns(bothreturns)) # Std Dev and Sharp
    if (!any(search() %in% "tools:rstudio")) {
      chart.RiskReturnScatter(bothreturns, Rf = 0, add.sharpe = c(1, 2), xlim=c(0,0.25), main = "Return versus Risk", colorset = c("red","blue"))
    }
    # Return stats and relative performance
    # quartiles, means, mins, maxes
    print(table.Stats(bothreturns))
    if (!any(search() %in% "tools:rstudio")) {
      chart.RelativePerformance(bothreturns[,1],bothreturns[,2], colorset = c("red","blue"), lwd = 2, legend.loc = "topleft")
    }
    # tradeStats(Portfolios, Symbols . . .
    # Single symbol portolio: 'Net.Trading.PL'
    # in Multisymbol portolio, the NAME will be different: "Total.Net.Profit"
    will5000ind_f_drblacbs.stats<-tradeStats("will5000ind_f_drblacbs")[,c('Net.Trading.PL','Max.Drawdown','Num.Trades','Profit.Factor','Std.Dev.Trade.PL','Largest.Winner','Largest.Loser','Max.Equity','Min.Equity')]
    View(will5000ind_f_drblacbs.stats) # per symbol, summary statistics
    # slide 37
    # trade related
    # profit related
    # averages
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    # Trade stats by instrument
    # tradeStats(multi.asset)
    # # slide 65
    # # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    # perTradeStats(Portfolio, Symbol 
    View(perTradeStats("will5000ind_f_drblacbs"))
    # per 24hours# ticks # PL, MAE, MFE
    # slide 48
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    # ONE Symbol of many Symbols in a portfolio
    # perTradeStats("multi.macd","XLV")
    # slide 48
    # Quantstrat_III(08_2013))(GuyYollin)_quantstrat(blotter)_osFixedDollar(CUSTOM)(32)(41)(42)_sigThreshold(40)_osMaxPos(53)_rulePctEquity(62_66).pdf
  
    # text plot results
    # too much data
    # PerformanceAnalytics:::textplot(t(tradeStats("will5000ind_f_drblacbs", "WILL5000IND")))
    # slide 23
    # Quantstrat_IV(08_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    # Plot monthly WILL5000IND and 10-month SMA
    # slide 27
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    # create custom theme
    myTheme<-chart_theme()
    myTheme$col$dn.col    <- "lightblue"
    myTheme$col$dn.border <- "lightgray"
    myTheme$col$up.border <- "lightgray"
    # plot OHLC series
    chart_Series( # quantmod
      x=Cl(WILL5000IND),
      theme=myTheme,
      name="WILL5000IND",
      TA="add_SMA(n=10,col=4)"
    )
  
    # blotter Performance plot
    # slide 34
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    # blotter function chart.Posn charts trades against market data, position through time, and cumulative P&L
    # botter function: trades against market data, position through time, and cumulative P&L
    # strategy                           # [first] Portfolio symbol
    if (!any(search() %in% "tools:rstudio")) {
      chart.Posn("will5000ind_f_drblacbs", Symbol = "WILL5000IND",theme=myTheme)
      plot(add_SMA(n=10,col=4, on=1))
    }
    # # if MANY Symbols ( a mult-asset porfolio )
    # # e.g. 9 Symbols in a specific portfolio
    # par(mfrow=c(3,3))
    # for(symbol in symbols) {
    #   chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
    #   TA="add_SMA(n=10,col='blue')")
    # }
    # par(mfrow=c(1,1))
    # # slide 63
    # # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
    
    # blotter maximum adverse excursion (MAE) and maximum favorable excursion (MFE) charts
    if (!any(search() %in% "tools:rstudio")) {
      chart.ME(Portfolio="will5000ind_f_drblacbs", Symbol="WILL5000IND", type="MAE", scale="percent")
      chart.ME(Portfolio="will5000ind_f_drblacbs", Symbol="WILL5000IND", type="MFE", scale="percent")
    }
    # slide 49
    # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf

    # 
    # NOTE: more GOOD stuff (headmaps, 3-D renderings) in HERE that I did not summarize  
    # Quantstrat_IV(08_2013))(GuyYollin)_quantstrat(blotter).pdf
    # 
    
    # TradeStation Performance Report 
    # Gross Profit   Sum of all winning trades during the tested period.
    # Gross Loss     Sum of all losing trades during the tested period.
    # Profit Factor  Gross Profit / Gross Loss                       quantstrat
    # Average Trade  Total Realized Return / Number of Trades.       quantstrat
    # http://help.tradestation.com/09_01/tsportfolio/reports/about_performance_report.htm
    # https://community.tradestation.com/discussions/
    
    # An unrealized gain 
    #   is a profit that exists on paper, resulting from an investment. 
    #   It is a profitable position that has yet to be sold in return for cash, 
    #   such as a stock position that has increased in capital gains but still remains open. 
    # 
    # A gain becomes realized 
    #   once the position is closed for a profit.
    # 
    # Unrealized Gain - Investopedia
    # https://www.investopedia.com/terms/u/unrealizedgain.asp
    # 
    # realized  P&L (from closed transaction) 
    # and 
    # Unrealized P&L (from your open  position)
    # 
    # Brian G. Peterson brian at braverock.com 
    # Tue Jan 14 15:26:28 CET 2014
    # https://stat.ethz.ch/pipermail/r-sig-finance/2014q1/012192.html
    
    Sys.setenv(TZ=oldtz)
    options(ops)
    
  }
  return(quanstrat02_inner())

}
# quantstrat02() 

# quantsight02.R

