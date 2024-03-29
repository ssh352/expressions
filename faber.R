
lexical_faber_R <- function() {

  faber_R <- function() {
  
    # # View of /pkg/quantstrat/demo/faber.R
    # # Parent Directory Parent Directory | Revision Log Revision Log
    # # Revision 1594 - (download) (annotate) 
    # # Sat Mar 29 20:39:45 2014 UTC (5 months, 1 week ago) by braverock 
    # # File size: 7592 byte(s)
    # # - fix rebalancing and rebalancing demos
    # # - update roxygen docs
    # # - bump version
  
    # This is a very simple trend following strategy for testing the results of:
    # Faber, Mebane T., "A Quantitative Approach to Tactical Asset Allocation." 
    # Journal of Risk Management (Spring 2007).
    # The article proposes a very simple quantitative market-timing model.  They 
    # test the model in sample on the US stock market since 1900 before testing
    # it out-of-sample in twenty other markets.
  
    # The article discusses a 200-day simple moving average, which is proposed
    # in Jeremy Seigel's book "Stocks for the Long Run" for timing the DJIA.  He 
    # concludes that a simple market timing strategy improves the absolute and
    # risk adjusted returns over a buy-and-hold strategy.  After all transaction
    # costs are included, the timing strategy falls short on the absolute return,
    # but still provides a better risk-adjusted return.  Siegel also tests timing on  
    # the Nasdaq composite since 1972 and finds better absolute and risk adjusted
    # returns.
  
    # The article implements a simpler version of the 200-day SMA, opting for a
    # 10-month SMA.  Monthly data is more easily available for long periods of time,
    # and the lower granularity should translate to lower transaction costs.  
  
    # The rules of the system are relatively simple:
    # - Buy when monthly price > 10-month SMA
    # - Sell and move to cash when monthly price < 10-month SMA
  
    # 1. All entry and exit prices are on the day of the signal at the close.
    # 2. All data series are total return series including dividends, updated monthly. 
    #    For the purposes of this demo, we only use price returns.
    # 3. Cash returns are estimated with 90-day commercial paper.  Margin rates for
    #    leveraged models are estimated with the broker call rate.  Again, for the
    #    purposes of this demo, we ignore interest and leverage.
    # 4. Taxes, commissions, and slippage are excluded.
  
    # This simple strategy is different from well-known trend-following systems in
    # three respects.  First, there's no shorting.  Positions are converted to cash on
    # a 'sell' signal, rather than taking a short position. Second, the entire position
    # is put on at trade inception.  No assumptions are made about increasing position
    # size as the trend progresses.  Third, there are no stops.  If the trend reverts
    # quickly, this system will wait for a sell signal before selling the position.
  
    # Data
    # Instead of using total returns data, this demo uses monthly data for the SP500
    # downloaded from Yahoo Finance.  We'll use about 10 years of data, starting at 
    # the beginning of 1998.
  
    # Load required libraries
    require(quantstrat) 
    
    # required ( If I am running in a function )
    if(!is.null(sys.call(sys.parent()))) {
      if (!exists('.blotter' , envir=.GlobalEnv))  { .GlobalEnv$.blotter   <- new.env() } 
      if (!exists('.strategy', envir=.GlobalEnv))  { .GlobalEnv$.strategy  <- new.env() }
    }
      
    #correct for TZ issues if they crop up
    oldtz<-Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="GMT")
    }
    # Try to clean up in case the demo was run previously
    suppressWarnings(rm("account.faber","portfolio.faber",pos=.blotter))
    suppressWarnings(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "equity", 
             "GSPC", "stratFaber", "initDate", "initEq", "Posn", "UnitSize", "verbose"))
     suppressWarnings(rm("order_book.faber",pos=.strategy))
  
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
  
    # Set initial values
    initDate='1997-12-31'
    initEq=100000
  
    # Set up instruments with FinancialInstruments package
    currency("USD")
    symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
    for(symbol in symbols){ # establish tradable instruments
        stock(symbol, currency="USD",multiplier=1)
    }
  
    # Load data with quantmod
    #getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1998-01-01')
    ### Download monthly data instead?
    ### GSPC=to.monthly(GSPC, indexAt='endof')
    getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01')
    # optional: ( If I am running in a function )
    # does not affect the program ( but for consistency if nothing else )
    if(!is.null(sys.call(sys.parent()))) {
      .GlobalEnv$.getSymbols <- .getSymbols
    }
    
    for(symbol in symbols) {
        x<-get(symbol)
        x<-to.monthly(x,indexAt='lastof',drop.time=TRUE)
        indexFormat(x)<-'%Y-%m-%d'
        colnames(x)<-gsub("x",symbol,colnames(x))
        assign(symbol,x)
        # required ( If I am running in a function )
        if(!is.null(sys.call(sys.parent()))) {
          assign(symbol,x, envir = .GlobalEnv)
        }
    }
  
    # Initialize portfolio and account
    initPortf('faber', symbols=symbols, initDate=initDate)
    initAcct('faber', portfolios='faber', initDate=initDate, initEq=100000)
    initOrders(portfolio='faber', initDate=initDate)
  
    print("setup completed")
  
    # Initialize a strategy object
    strategy("faber", store=TRUE)
  
    # Add an indicator
    add.indicator('faber', name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
  
    # There are two signals:
    # The first is when monthly price crosses over the 10-month SMA
    add.signal('faber',name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="gte"),label="Cl.gt.SMA")
    # The second is when the monthly price crosses under the 10-month SMA
    add.signal('faber',name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="lt"),label="Cl.lt.SMA")
  
    # There are two rules:
    # The first is to buy when the price crosses above the SMA
    add.rule('faber', name='ruleSignal', arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=500, ordertype='market', orderside='long', pricemethod='market',TxnFees=-5), type='enter', path.dep=TRUE)
    # The second is to sell when the price crosses below the SMA
    add.rule('faber', name='ruleSignal', arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all', ordertype='market', orderside='long', pricemethod='market',TxnFees=-5), type='exit', path.dep=TRUE)
  
    # Process the indicators and generate trades
    start_t<-Sys.time()
    out<-try(applyStrategy(strategy='faber' , portfolios='faber'))
    end_t<-Sys.time()
    print("Strategy Loop:")
    print(end_t-start_t)
  
    # look at the order book
    #print(getOrderBook('faber'))
  
    start_t<-Sys.time()
    updatePortf(Portfolio='faber',Dates=paste('::',as.Date(Sys.time()),sep=''))
    updateAcct('faber')
    updateEndEq('faber')
    end_t<-Sys.time()
    print("trade blotter portfolio update:")
    print(end_t-start_t)
  
    # hack for new quantmod graphics, remove later
    themelist<-chart_theme()
    themelist$col$up.col<-'lightgreen'
    themelist$col$dn.col<-'pink'
  
    # Only one RStudio graphics device is permitted
    if (!any(search() %in% "tools:rstudio")) {
      dev.new()
    }
    layout(mat=matrix(1:(length(symbols)+1),ncol=2))
    for(symbol in symbols){
      # R Studio: Error in plot.new() : figure margins too large
      if (!any(search() %in% "tools:rstudio")) {
        chart.Posn(Portfolio='faber',Symbol=symbol,theme=themelist,TA="add_SMA(n=10,col='darkgreen')")
      }
    }
  
    ret1 <- PortfReturns('faber')
    ret1 <- to.monthly(ret1)
    ret1$total <- rowSums(ret1)
  
    View(ret1)
  
    if("package:PerformanceAnalytics" %in% search() || require("PerformanceAnalytics",quietly=TRUE)){
      getSymbols("SPY", src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01')
      SPY<-to.monthly(SPY)
      SPY.ret<-Return.calculate(SPY$SPY.Close)
      # Only one RStudio graphics device is permitted
      if (!any(search() %in% "tools:rstudio")) {
        dev.new()
      }
      # R Studio: Error in plot.new() : figure margins too large
      if (!any(search() %in% "tools:rstudio")) {
        charts.PerformanceSummary(cbind(ret1$total,SPY.ret), geometric=FALSE, wealth.index=TRUE)
      }
    }
  
    faber.stats<-tradeStats('faber')[,c('Net.Trading.PL','Max.Drawdown','Num.Trades','Profit.Factor','Std.Dev.Trade.PL','Largest.Winner','Largest.Loser','Max.Equity','Min.Equity')]
    View(faber.stats)
  
    Sys.setenv(TZ=oldtz)
    
    the_end_debug_bookmark_here <- 1
    
    ###############################################################################
    # R (http://r-project.org/) Quantitative Strategy Model Framework
    #
    # Copyright (c) 2009-2012
    # Peter Carl, Dirk Eddelbuettel, Brian G. Peterson,
    # Jeffrey Ryan, Joshua Ulrich, and Garrett See
    #
    # This library is distributed under the terms of the GNU Public License (GPL)
    # for full details see the file COPYING
    #
    # $Id$
    #
    ###############################################################################
  
    ##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
    # book  = getOrderBook(port)
    # stats = tradeStats(port)
    # rets  = PortfReturns(acct)
    ################################################################
  
  }
  faber_R()

}
  
# faber_R()

run_faber_R <- function() {
  lexical_faber_R()
}
# run_faber_R() 
