 
# quantsight01.R

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



do_quantstrat <- function() {

  # "function form" based loosely on "https://github.com/AndreMikulec/expressions/blob/master/faber.R"
  # based on                         "https://github.com/braverock/quantstrat/blob/master/demo/faber.R" JAN 2018

  ops <- options()
  
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv("TZ")
  if(oldtz=="") {
    Sys.setenv(TZ="UTC")
  }

  require(quantstrat)
  require(lubridate) # to later push foward index dates by 5 months
  
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

  
  message("Begin function do_quantstrat_returns.")
  
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
  
  # Try to clean up in case the demo was run previously
  rm.strat("will5000ind_f_drblacbs")
  suppressWarnings(rm("WILL5000IND", "DRBLACBS"         , envir = .GlobalEnv))
  suppressWarnings(rm("get_DRBLACBS", envir = .GlobalEnv)) 
  # Note: not using (unprogrammed): get_DRBLACBS_SIG
   
  # required ( If I am running in a function )
  if(!is.null(sys.call(sys.parent()))) {
    if (!exists(".blotter" , envir=.GlobalEnv))  { .GlobalEnv$.blotter   <- new.env() } 
    if (!exists(".strategy", envir=.GlobalEnv))  { .GlobalEnv$.strategy  <- new.env() }
  }
  # NOTE: .instrument is stored here: FinancialInstrument:::.instrument
  # NOTE: .getSymbols is stored here: .GlobalEnv$.getSymbols
  # CHECK THIS [ ] # COME BACK
  
  # Set up instruments with FinancialInstruments package
  currency("USD")
  # stock market
  symbols = c("WILL5000IND") # indicator DRBLACBS is not as a stock
  # establish tradable instruments
  stock("WILL5000IND", currency="USD",multiplier=1)

  # load the stock market
  
  # 'quantstrat is 'seconds' based,  so required
  # "POSIXt","POSIXct"
  
  # required 
  # getSymbols(. . . , index.class=c("POSIXt","POSIXct")) # convert to early morning
  # but index.class is NOT a parameter in getSymbols.FRED

  # all data # currently quantmod::getSymbols.FRED gets *all* of the data
  WILL5000IND <- getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE) 
  # adjust and format
  indexClass(WILL5000IND)  <- c("POSIXt","POSIXct")
  index(WILL5000IND) <- index(WILL5000IND) + ( 3600 * 18 ) # 6 p.m. Eastern ... Time # 'American' markets are closed
  indexFormat(WILL5000IND) <- "%Y-%m-%d"
  
  # format the stock market
  WILL5000IND <- to.monthly(WILL5000IND, indexAt="lastof", drop.time=TRUE) # default OHLC = TRUE # created
  # This is a 'daily' BUT ...
  # warning # some earlier data only has month-end data
  # my index had been convertd to a class of Date
  # I convert it back
  indexClass(WILL5000IND) <- c("POSIXt","POSIXct")
  index(WILL5000IND) <- index(WILL5000IND)       + ( 3600 * 23 )   # 6 p.m. Eastern ... Time # 'American' markets are closed
  indexFormat(WILL5000IND)<-"%Y-%m-%d"
  
  # > head(WILL5000IND,1)
  #          WILL5000IND.Open WILL5000IND.High WILL5000IND.Low WILL5000IND.Close
  # 1970-12-31      
  
  # load the indicator
  DRBLACBS   <- getSymbols("DRBLACBS",  src = "FRED", auto.assign = FALSE) 
  # class(index(DRBLACBS)) # "Date"
  
  # always on the 1st of the month
  # 
  # format the indicator
  # always falls aon the first of the month and every three months 
  # and 'press release time' is 5 months later than 'index time'
  # 
  # for convenience subtract off a day ( so I have the end of the month )
  index(DRBLACBS) <- index(DRBLACBS) - 1
  index(DRBLACBS) <- index(DRBLACBS) %m+% months(5) # note if/when Machine Learning data model requires SOME work

  # adjust and format
  indexClass(DRBLACBS) <- c("POSIXt","POSIXct")
  index(DRBLACBS) <- index(DRBLACBS)       + ( 3600 * 23 )   # 6 p.m. Eastern ... Time # 'American' markets are closed
  indexFormat(DRBLACBS)<-"%Y-%m-%d"
  
  # since, work is done per instrument(symbol) 'in a LOOP'
  # and ONLY using the data of mktdata that is the 'per instrument(symbol) single xts object'
  # than I have to add other external data into the 'per instrument(symbol) single xts object'
  # S3 dispatch merge.xts
  WILL5000IND <- merge(WILL5000IND, DRBLACBS)

  # earliest date of WILL5000IND is 1970-12-31
  # earliest date of DRBLACBS    is 1987-01-01" # subtract by 1 day ... 1986-12-31 then add 5 months ... 1987-05-31
  
  # since DRBLACBS only occurs ever three months, then copy it forward
  # so, I have a 'value' per month
  # S3 dispatch na.locf.xts # read help ? na.locf.xts
  WILL5000IND <- na.locf(WILL5000IND)
  
  # need to trim off early records of which the column DRBLACBS only has NA values
  WILL5000IND <- na.trim(WILL5000IND, sides = "left") # earlier observations ar removed
  
  # copy back to the global environment, so the quantstat functions can find the instrument(symbol) mktdata
  assign("WILL5000IND", WILL5000IND, envir = .GlobalEnv)
  
  # need a signal of a slope of increasing/decreasing/same
  # currenly, same will be 'no-action', so I have to dig further back in history
  # to see if I was increasing/decreasing ( and keep the decision to be the same (current plan)
  
  # indicator
  # needed for add.indicator and quantstrat programs
  get_DRBLACBS <- function(x) { return(WILL5000IND[,"DRBLACBS"]) }
  # place so that quanstrat programs can find it
  assign("get_DRBLACBS", get_DRBLACBS, envir = .GlobalEnv)
  
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
  
  # Set initial values 
  # BEFORE! earliest date (required)
  initDate=as.character(zoo::as.Date(head(index(WILL5000IND),1)) -1) 
  initEq=100000.00
  
  # # optional: ( If I am running in a function )
  # # does not affect the program ( but for consistency if nothing else )
  #  if(!is.null(sys.call(sys.parent()))) {
  #     .GlobalEnv$.getSymbols <- .getSymbols
  #  }
  
  # Initialize portfolio and account
  initPortf("will5000ind_f_drblacbs", symbols=symbols, initDate=initDate)
  initAcct("will5000ind_f_drblacbs", portfolios="will5000ind_f_drblacbs", initDate=initDate, initEq=initEq)
  initOrders(portfolio="will5000ind_f_drblacbs", initDate=initDate)

  print("setup completed")

  # Initialize a strategy object
  strategy("will5000ind_f_drblacbs", store=TRUE)
  
  # Add an indicator
  
  # strategy                              # function    # function arguments ( could be NOT here and be in applyStrategy parameters )
  # eventually, in mktdata, label "SMA4" becomes column SMA.SMA4 of values (1,NA)
  # sigCrossover/ruleSignal/sigval=TRUE( SMA.SMA4 = 1 )
  add.indicator("will5000ind_f_drblacbs", name = "SMA", arguments = list(x = quote(get_DRBLACBS(mktdata)), n=4), label="SMA4") 
  
   # There are two signals
  
  # strategy
  # The first is when monthly DRBLACBS crosses over the 4-month SMA of DRBLACBS ( MORE Delinquencies)
  add.signal("will5000ind_f_drblacbs",name="sigCrossover",arguments = list(columns=c("DRBLACBS","SMA4"),relationship="gte"),label="DR.gt.SMA4")
  
  # The second is when the monthly DRBLACBS crosses under the 4-month of SMA DRBLACBS (LESS Delinquencies)
  add.signal("will5000ind_f_drblacbs",name="sigCrossover",arguments = list(columns=c("DRBLACBS","SMA4"),relationship="lt"),label="DR.lt.SMA4")
  
  # There are two rules
  
  # Note function exists: enable.rule(. . . 
  
  # strategy ( note: my buy/sell rules are REVERSED compared to faber.R )
  # 
  # The first is to SELL when the DRBLACBS crosses above the SMA of DRBLACBS(MORE Delinquencies)
  add.rule("will5000ind_f_drblacbs", name="ruleSignal", arguments = list(sigcol="DR.gt.SMA4", sigval=TRUE, orderqty="all", ordertype="market", orderside="long", pricemethod="market",TxnFees=0), type="exit", path.dep=TRUE)

  # The second is to BUY when the DRBLACBS crosses below the SMA of DRBLACBS (LESS Delinquencies)
  add.rule("will5000ind_f_drblacbs", name="ruleSignal", arguments = list(sigcol="DR.lt.SMA4", sigval=TRUE, orderqty="all", ordertype="market", orderside="long", pricemethod="market",TxnFees=0), type="enter", path.dep=TRUE)
 
  # Process the indicators and generate trades
  # from Guy Yollin pdfs 
  # add.indicator ( function 'name' arguments = ) can be skipped here and instead 'late bound' sent to 
  # applyStrategy(  parameters = )
  out <- applyStrategy(strategy="will5000ind_f_drblacbs", portfolios="will5000ind_f_drblacbs", prefer = "Open") # ANDRE debug # prefer defaults to Cl, close, Close

  # Do trade statistics
  # getting UGLY WARNINGS on   Dates=paste("::",as.Date(Sys.time()),sep="") # as of today
  # I will instead use 'the last day of the previous month'
  # BUT Date =  default NULL (will use times from Prices)
  message("Begin trade blotter portfolio update")
  updatePortf(Portfolio="will5000ind_f_drblacbs", Dates=paste("::",as.character(zoo::as.Date(tail(index(WILL5000IND),1)) %m+% months(-1) ), sep="")) 
  updateAcct("will5000ind_f_drblacbs")
  # account
  updateEndEq("will5000ind_f_drblacbs")
  message("End trade blotter portfolio update")
  
  # # hack for new quantmod graphics, remove later
  # themelist<-chart_theme()
  # themelist$col$up.col<-'lightgreen'
  # themelist$col$dn.col<-'pink'
  # 
  # dev.new()
  # layout(mat=matrix(1:(length(symbols)+1),ncol=2))
  # for(symbol in symbols){
  #     chart.Posn(Portfolio="will5000ind_f_drblacbs",Symbol=symbol,theme=themelist,TA="add_SMA(n=10,col='darkgreen')")
  # }
  
  # blotter portfolio object
  # slide 57
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
                              # Portfolio
  thePortfolio = getPortfolio("will5000ind_f_drblacbs")
  print(names(thePortfolio))
  print(names(thePortfolio$symbols))
  print(names(thePortfolio$symbols$WILL5000IND))
  print(names(thePortfolio$summary))
  # portfolio transactions # slide 58
  print(thePortfolio$symbols$WILL5000IND$txn)
  
  # bivariate scatterplots or time-series plots 
  require(lattice) #  good for multi-panel plots
  # Plot of instrument P&L
  xyplot(thePortfolio$symbols$WILL5000IND$posPL.USD,type="h",col=4)
  # Plot of portfolio summary time series object
  xyplot(thePortfolio$summary,type="h",col=4)
  # slide 61-63
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  
  # blotter account object
  theAccount = getAccount(b.strategy)
  print(names(theAccount))
  print(names(theAccount$portfolios))
  print(names(theAccount$portfolios$bFaber))
  print(names(theAccount$summary))
  # by month, PL and End.Eq
  # slide 66
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  
  require(lattice)
  xyplot(theAccount$summary,type="h",col=4)
  # slide 52
  # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf

  # slide 20
  # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  theStrategy <- getStrategy("will5000ind_f_drblacbs")
  summary(theStrategy)
  # indicators, signals, rules, constraints
  
  # slide 45
  # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  theOrderBook <- getOrderBook("will5000ind_f_drblacbs")
  print(class(theOrderBook))
  print(names(theOrderBook))
  print(names(theOrderBook$will5000ind_f_drblacbs))
  print(names(theOrderBook$will5000ind_f_drblacbs$WILL5000IND))
  print(theOrderBook$will5000ind_f_drblacbs$WILL5000IND[,1:5])
  
  # Transactions
  # slide 34
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  getTxns(Portfolio="will5000ind_f_drblacbs", Symbol="WILL5000IND")
  # per date
  # Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost Net.Txn.Realized.PL
  
  # of each portfolio or the entire account
                      # account    # Portfolios = NULL(default NULL (all portfolios))
                                   # Dates: xts style ISO 8601 date subset to retrieve, default NULL (alldates)
  returns <- PortfReturns("will5000ind_f_drblacbs")
  # slide 42
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  # rownames(returns) <- NULL # someimes to avoid errors using table.Arbitrary
  returns$total <- rowSums(returns)

  print(returns)
  
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

  
  # slide 45
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  # table.Arbitrary creates a table of statistics from a passed vector
  # of functions and vector of labels
  
  # # Compute performance statistics
  tab.perf <- table.Arbitrary(returns,
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
  tab.risk <- table.Arbitrary(returns,
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

  if("package:PerformanceAnalytics" %in% search() || require("PerformanceAnalytics",quietly=TRUE)){ 
  	WILL5000IND.ret <- Return.calculate(WILL5000IND$WILL5000IND.Close)
  	# xor
  	# 	WILL5000IND.ret <- Return.calculate(WILL5000IND$WILL5000IND.Close, method = "log")
  	dev.new()
  	# Plot cumulative return and drawdown(NICE ONE)
  	# slide 42
    # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  	charts.PerformanceSummary(cbind(returns$total,WILL5000IND.ret), geometric=FALSE, wealth.index=TRUE)
  	# if after Return.calculate(. . . method = "log" . . .)
  	# xor                                                              # default
  	#  charts.PerformanceSummary(cbind(returns$total,WILL5000IND.ret), geometric=TRUE, wealth.index=TRUE)
  	
    # # Consolidated equity curve ( of a multi-asset portfolio )
    # plot(theAccount$summary$End.Eq, main="Consolidated SPDR Equity Curve")
    # charts.PerformanceSummary(Return.calculate(theAccount$summary$End.Eq), geometric=FALSE, wealth.index=TRUE, main="Consolidated SPDR Performance")
    # # slide 71
    # # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf

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
  charts.PerformanceSummary(bothreturns, geometric=FALSE, wealth.index=TRUE)
  
  # Return and risk comparison
  # slide 52
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  table.AnnualizedReturns(bothreturns) # Std Dev and Sharp
  chart.RiskReturnScatter(bothreturns, Rf = 0, add.sharpe = c(1, 2), xlim=c(0,0.25), main = "Return versus Risk", colorset = c("red","blue"))

    # Return stats and relative performance
  # quartiles, means, mins, maxes
  table.Stats(bothreturns)
  chart.RelativePerformance(returns[,1],returns[,2], colorset = c("red","blue"), lwd = 2, legend.loc = "topleft")
  
  
  # tradeStats(Portfolios, Symbols . . .
  faber.stats <- tradeStats("will5000ind_f_drblacbs")[,c("Net.Trading.PL","Max.Drawdown","Num.Trades","Profit.Factor","Std.Dev.Trade.PL","Largest.Winner","Largest.Loser","Max.Equity","Min.Equity")]
  print(faber.stats) # per symbol, summary statistics
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
  perTradeStats("will5000ind_f_drblacbs")
  # per 24hours# ticks # PL, MAE, MFE
  # slide 48
  # Quantstrat_I(12_2013))(GuyYollin)_quantstrat(blotter).pdf
  
  # ONE Symbol of many Symbols in a portfolio
  # perTradeStats("multi.macd","XLV")
  # slide 48
  # Quantstrat_III(08_2013))(GuyYollin)_quantstrat(blotter)_osFixedDollar(CUSTOM)(32)(41)(42)_sigThreshold(40)_osMaxPos(53)_rulePctEquity(62_66).pdf

  # text plot results
  PerformanceAnalytics:::textplot(t(tradeStats("will5000ind_f_drblacbs", "WILL5000IND")))
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
  chart.Posn("will5000ind_f_drblacbs", Symbol = "WILL5000IND",theme=myTheme)
  plot(add_SMA(n=10,col=4, on=1))
  
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
  chart.ME(Portfolio="will5000ind_f_drblacbs", Symbol="WILL5000IND", type="MAE", scale="percent")
  chart.ME(Portfolio="will5000ind_f_drblacbs", Symbol="WILL5000IND", type="MFE", scale="percent")
  # slide 49
  # Quantstrat_II(12_2013))(GuyYollin)_quantstrat(blotter).pdf

  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function do_quantstrat_returns.")

  return(NULL)

  # NOTE: more GOOD stuff (headmaps, 3-D renderings) in HERE that I did not summarize  
  # Quantstrat_IV(08_2013))(GuyYollin)_quantstrat(blotter).pdf


}
# do_quantstrat() 

# quantsight01.R
