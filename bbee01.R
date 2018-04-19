 
# bbee01.R


# my old work
#
# safely make an xts into a data.frame
dfize <- function(xtso) {
  require(xts)
  df <- as.data.frame(xtso)  # zoo::as.data.frame.zoo # xts:::as.data.frame.xts
  # rn <- as.numeric( zoo::as.Date( row.names(df) ) )
  rn <- index(xtso)
  cb <- cbind(rn,df)
  colnames(cb)[1] <- "datetime"
  # attributes(cb) <- list(tindex = index)
  return(cb)
} 
# see test/example below

# safely make a df into an xts
xtsize <- function(dfo) {
  require(xts)
  id <-  dfo[["datetime"]] 
  # drop column
  dfo[,"datetime"] <- NULL
  cd <- coredata(as.matrix(dfo))
  xts(cd,id)  # could be 'zoo'
}
# library(xts)
# sample_xts <- as.xts(sample_matrix)
# str(head(xtsize(dfize(sample_xts))))
#
# edge case tests
# 
# xts1 <- xts(NA_real_, zoo::as.Date(0))
# colnames(xts1) <- "col1"
# xts1 <- xts1[0,]
# 
# exiting from: dfize(xts1)
# 'data.frame':   0 obs. of  2 variables:
#  $ datetime:Class 'Date'  num(0)
#  $ col1    : num
# 
# > xtsize(dfize(xts1))
#      col1
# > str(xtsize(dfize(xts1)))
# An 'xts' object of zero-width
# > index(xtsize(dfize(xts1)))
# [1] "Date of length 0"


# unstable structure to debug inside: holds for 2 seconds then bounces out
# within.xts IS TOO VOLITILE: CAN NOT browser()/rstudio debug inside: SOMETHING IS NOT RIGHT
within.xts <- function (data, expr, ...) {
  data <- dfize(data)
  # tindex <- attrib(data, "tindex")
  parent <- environment()    # JUST CHANGED parent.frame() to environment()
  e <- evalq(environment(), data, parent)
  eval(substitute(expr), e)
  l <- as.list(e, all.names = TRUE)
  l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
  nD <- length(del <- setdiff(names(data), (nl <- names(l))))
  data[nl] <- l
  if (nD)
    data[del] <- if (nD == 1)
      NULL
  else
    vector("list", nD)
  
  xtsize(data)
}



ifelse         <- function(test, yes, no) UseMethod("ifelse")
ifelse.default <- function(test, yes, no) base::ifelse(test, yes, no )

ifelse.xts  <- function(test, yes, no) {

  require(xts)

  if(!is.xts(yes)) {
    if(NROW(yes) == 1) yes <- rep(yes,NROW(test))
    yes <- xts(yes, index(test))
  }
  if(!is.xts(no)) {
    if(NROW(no) == 1) no <- rep(no,NROW(test))
    no <- xts(no, index(test))
  }
  test.yes.no <- merge(test,yes,no)
  colnames(test.yes.no) <- c("test", "yes", "no" )
  res <- within( test.yes.no, { res <- ifelse(test,yes,no);rm(test,yes,no);return(res) } )
  colnames(res) <-"result"
  return(res)
  
}
# debug(ifelse.xts)
# PURPOSE OF THIS: 
# SO I CAN DO ifelse( . . . ) WITHOUT having TO DO ( within.xts( xts, { ifelse . . .} ) )
# SIMILAR TO zoo in quantmod::tradeModel
# library(xts)
# sample_xts <- as.xts(sample_matrix)
# head( ifelse( sample_xts[,"Open"] > sample_xts[,"Close"], -1, sample_xts[,"Open"] ) )
# except index class is wrong because, I did not keep the index: within, dfize, xtsize
#              result
# 2007-01-02 50.03978
# 2007-01-03 50.23050
# 2007-01-04 -1.00000
# 2007-01-05 -1.00000
# 2007-01-06 -1.00000
# 2007-01-07 -1.00000
# str( ifelse( sample_xts[,"Open"] > sample_xts[,"Close"], -1, sample_xts[,"Open"] ) )
# An 'xts' object on 2007-01-02/2007-06-30 containing:
#   Data: num [1:180, 1] 50 50.2 -1 -1 -1 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr "result"
#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#   xts Attributes:
#  NULL



get_up_side_down_side <- function(){
  
  ops <- options()
  
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  # Inspired by ...
  # 
  # How to backtest a strategy in R:
  # 
  # Step 1: Get the data
  # Step 2: Create your indicator
  # Step 3: Construct your trading rule
  # Step 4: The trading rules/equity curve ( means 'actually trade' )
  # Step 5: Evaluate strategy performance
  # 
  # How to backtest a strategy in R
  # Saturday, March 26, 2011
  # http://blog.fosstrading.com/2011/03/how-to-backtest-strategy-in-r.html
  # 
  # Generic Signal-Based Strategy Modeling:
  # 
  # Instruments -> Indicators -> Signals -> Rules -> Orders -> Transactions
  # 
  # Instruments 
  #   contain market data
  # Indicators 
  #   are quantitative values derived from market data 
  #   [ or with a simple liberalization: force my own data source ]
  # Interaction 
  #   between indicators and market data are used to generate
  #     signals (e.g. crossovers, thresholds)
  # Rules 
  #   use market data, indicators, signals, and 
  #   current account/portfolio characteristics 
  #    to generate orders
  # Interaction 
  #   between orders and market data generates transactions
  # 
  # Quantstrat object model
  # Slide 8
  # quantstrat (updated 9/2014)
  # http://www.r-programming.org/files/quantstrat.pdf
  # Guy Yollin papers
  # http://www.r-programming.org/papers
  # 
  # R-forge/github R package quantstrat ( with blotter and FinancialInstrument )
  # https://github.com/braverock/quantstrat
  # https://r-forge.r-project.org/scm/viewvc.php/pkg/quantstrat/?root=blotter
  
  require(xts)      # merge.xts
  require(TTR)      # ROC, SMA
  require(quantmod) # monthlyReturn
  require(PerformanceAnalytics) # Return.portfolio # table.CalendarReturns
  # uses R.utils    hpaste
  # uses lubridate  `%m+%`
  `%m+%` <- lubridate::`%m+%`
  
  message("begin get_up_side_down_side")
  
  # begin gather data
  
  ### ### ### ### ### ###
  ### BEGIN INSTRUMENTS ###
  
  # 1st empty xts
  all_possible_instrument_log_rets <- xts(, zoo::as.Date(0)[0])
  all_possible_instrument_log_rets_dates    <- xts(, index(all_possible_instrument_log_rets))
  
  ### ### 
  ### WHAT I AM TRYING TO OPTIMIZE 
  # 
  # Wilshire 5000 Total Market Index
  # back through 1970
  # Daily, Close
  # total market returns, which do include reinvested dividends
  # https://fred.stlouisfed.org/data/WILL5000IND.txt
  # 
  # will5000ind <- get_fred_wilshire5000_eom_xts()
  # head(will5000ind,1)
  # 
  #           will5000ind
  # 1970-12-31        1.00
  # 
  # today is MAR 05 2018
  # to.monthy ROUNDS FORWARD ( FORWARD: KEEP FOR NOW; I MAY WANT TO CHANGE LATER )
  # > tail(index(fred_wilshire5000_eom_xts))
  # [1] "2017-10-31" "2017-11-30" "2017-12-31" "2018-01-31" "2018-02-28" "2018-03-31"
  will5000ind          <- get_fred_wilshire5000_eom_xts()
  will5000ind_log_rets <- ROC(will5000ind)               # which(is.na(will5000ind_log_rets)) # logrithmic
  will5000ind_log_rets[is.na(will5000ind_log_rets)] <- 0 # usually just the 1st observation
  
  # will5000ind                       # 1st empty xts
  all_possible_instrument_log_rets <- merge.xts(all_possible_instrument_log_rets, will5000ind_log_rets)
  all_possible_instrument_log_rets_dates  <- xts(, index(all_possible_instrument_log_rets))
  
  # "cash"             # no returns good/bad
  cash_log_rets <- xts(rep(0,NROW(all_possible_instrument_log_rets)),index(all_possible_instrument_log_rets))
  colnames(cash_log_rets) <- "cash"
  cash_log_rets_dates <- index(cash_log_rets)

  # colnames                                   "cash"    +     "will5000ind"
  all_possible_instrument_log_rets <- merge.xts(cash_log_rets, all_possible_instrument_log_rets)
  all_possible_instrument_log_rets_dates    <- xts(, index( all_possible_instrument_log_rets))
  
# like quantmod::tradeModel(. . ., signal.threshold = c(0, 0), . .  )
# Barclays Capital U.S. Aggregate Bond Index(AGG) in yahoo finance only goes back to 2003.
# In nasdaq ( https://www.nasdaq.com/symbol/agg/historical ) only goes back 10 years
#   Lehman Aggregate Bond Index was co-created in 1973 by Art Lipson and John Roundtree
#   Bloomberg Barclays US Aggregate Bond Index
#   https://en.wikipedia.org/wiki/Bloomberg_Barclays_US_Aggregate_Bond_Index
#   was created in 1986 with backdated history going back to 1976
#   Bloomberg Barclays US Aggregate Bond Index
#   https://www.bogleheads.org/wiki/Bloomberg_Barclays_US_Aggregate_Bond_Index
# Consider? instead/with using a 'short of the Wilshire5000/WILL5000IND(St.Louis.FRED)'?
# Also a 'short of the Wilshire5000' may modernly correct mimic the behavior of the 
# AGG/stock_market up/down,down/up
# (When did AGG/market change to up/down,down/up compared to the historical up/up,down/down?)
  
  ### ### ### ### ### ###
  ### BEGIN INDICATORS ###
  
  # as as I gather data, place the value in my indicators xts
  all_possible_indicators <- xts(, zoo::as.Date(0)[0])
  all_possible_indicators_dates <- index(all_possible_indicators)
  
  # Order is mostly from 
  # RECESSION TO DRAWDOWN
  # BROAD TO NARROW
  # "MORE RELIABLE" TO "LESS RELIABLE"
  
  # PRE/POST RECESSION ONLY
  # MOST BROAD STATISTIC
  #
  # Civilian Unemployment Rate
  # back through 1948
  # updated the 'first or second' friday of each month ( variable )
  # ** ASSUME I run just after the RELEASE of the month ***  *** IMPORTANT ***
  # just a one month delay (Last Updated - max(Date Range))
  ## Schedule of Releases for the Employment Situation
  ## FIRST OR SECOND FRI OF THE MONTH ( IRREGULAR )
  ## https://www.bls.gov/schedule/news_release/empsit.htm
  # # just four month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/UNRATE.txt
  # contains: unrate[["monthly"]] column named "unrate"
  unrate_indicator <- get_symbols_xts_eox("UNRATE", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  unrate_indicator <- unrate_indicator[["monthly"]]
  
  #                                                             "unrate"
  all_possible_indicators <- merge.xts(all_possible_indicators, unrate_indicator)
  all_possible_indicators_dates <- index(all_possible_indicators)
  
  # PRE/POST RECESSION ONLY
  # PART OF BROAD STATISTIC (UNRATE(ABOVE))
  # SNEAK PEAK INTO THE FUTURE OF THE "MOST CONSERVATIVE(OUTSIDE) STATISTIC"
  #
  # Employment Level: Part-Time for Economic Reasons, All Industries
  # back through 1955
  # updated the 'first or second' friday of each month ( variable: same as UNRATE )
  # ** ASSUME I run just after the RELEASE of the month ***  *** IMPORTANT ***
  # just a one month delay (Last Updated - max(Date Range))
  # Schedule of Releases for the Employment Situation ( SEEMS same day as UNRATE but 19 minutes later ) 
  # https://www.bls.gov/schedule/news_release/empsit.htm
  # # just four month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/LNS12032194.txt
  ####lns12032194 <- get_symbols_xts_eox("LNS12032194", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  
  
  
  # RECESSION ONLY
  # BROAD STATISTIC
  # # GOOD EXAMPLE ( notece: EXACTLY when BEFORE/AFTER going in/out OF MAJOR recessions )
  ###ret <- get_phil_survey_of_prof_forecasters_eom_xts(file_data_loc = "DISK", surveys_of_interest_regex = "^(unemp__).*(3|4)$", future_dates_regex = "(3|4)$")
  ###require(quantmod)
  ###quantmod::getSymbols("UNRATE", src = "FRED", from = "1940-01-01")
  # # to make eom
  # # keep at "forcast_target" date ( NOTE: UNRATE data is published one month later))
  ###index(UNRATE) <- index(UNRATE) - 1 # FRED 1st day shift
  # NOTE: FORECASTERS overshoot downtrends and undershoot up trends
  # ONLY GOOD CONCLUSION: during UNRATE downtrend ( or flat )
  #   when the unemp3 suprisingly undershoots the UNRATE then A RECESSION BEGINS
  ###dygraphs::dygraph(merge.xts(UNRATE,ret[,"unemp__unemp3__median"])) # 4.5 months into the future
  # NEED SOME SLOPE TO PROPERLY FORM modellable data
  # COME BACK
  
  
  
  # RECESSION
  # BROAD STATISTIC
  # TREND OF PRINTING MORE MONEY(RISING SLOPE) IS A GOOD THING
  # [FED] balance sheet with its components broadly divided into these categories
  # https://www.clevelandfed.org/our-research/indicators-and-data/credit-easing.aspx
  # INSPIRED FROM
  # https://github.com/cran/easingr
  # GRAND TOTAL LOOKS SIMILAR TO
  # Categories > Money, Banking, & Finance > Monetary Data > Factors Affecting Reserve Balances
  # All Federal Reserve Banks: Total Assets
  # since 2002
  # https://fred.stlouisfed.org/data/WALCL.txt
  ####clev_easing_balances_eom_xts <- get_clev_easing_balances_eom_xts()
  ####clev_easing_balances <- get_symbols_xts_eox(symbol_raw = clev_easing_balances_eom_xts[,"clev_easing_balances"], returns = "monthly", OHLC = FALSE, indexAt = "lastof")
  # NEED SOME SLOPE TO PROPERLY FORM modellable data
  # COME BACK
  
  
  
  # RECESSION
  # BROAD STATISTIC
  # TREND OF LOWERING INTEREST RATES IS A GOOD THING
  # Effective Federal Funds Rate
  # monthly, published 
  # back through 1954
  # updated delay is one month ( published on the 1st )
  # one month dely (Last Updated - max(Date Range))
  # AFTER THIS IS ACCELERATED UP TO A *HIGH MOUNTAIN* AND
  #   MUST BE *HIGH MOUNTAIN* AND THEN REQUIRED! THE 
  #   FED STARTS LOWERING INTEREST RATES, THEN THE GAME IS OVER!
  # https://fred.stlouisfed.org/data/FEDFUNDS.txt
  ####fedfunds <- get_symbols_xts_eox("FEDFUNDS", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")

  
  
  # RECESSION
  # BROAD STATISTIC
  # TREND OF LOWERING INTEREST RATES IS A GOOD THING
  # Interest Rates, Discount Rate for United States
  # monthly, published at change on the 1st of the month
  # back through 1950
  # updated at time of change: NA on the month ( on the 1st )
  # NA: (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/INTDSRUSM193N.txt
  ####intdsrusm193n <- get_symbols_xts_eox("INTDSRUSM193N", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 0, OHLC = FALSE, indexAt = "lastof")
  
  
  
  # RECESSION AND ECONOMIC DOWNTURN ( NOTE: TODO: verify ON ALFRED )
  # MEDIUM STATISTICS
  # OECD based Recession Indicators for the United States from the Peak through the Trough
  # back through 1947
  # updated 'near' the middle of each month
  # just four month delay (Last Updated - max(Date Range))
  # http://research.stlouisfed.org/fred2/data/usarecm.txt
  ####usarecm <- get_symbols_xts_eox("USARECM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 4, OHLC = FALSE, indexAt = "lastof")

  
  
  # OFFICIAL RECESSION ONLY ( author in journal articles claims to be faster than USRECM(NBER))
  # NARROW STATISTIC (MAY BE LATE)
  # Piger, Jeremy Max, Chauvet, Marcelle
  # back through 1967
  # updated 'at' or 'just after' the 1st of each month
  # just three month delay (Last Updated - max(Date Range))
  # http://research.stlouisfed.org/fred2/data/RECPROUSM156N.txt
  # NOTE:Piger screwed up in 2015/2016 # but only alfred shows
  # NOTE: R package alfred # SOMETIMES # can get the 'actual day reported'
  ####recprousm156n <- get_symbols_xts_eox("RECPROUSM156N", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 3, OHLC = FALSE, indexAt = "lastof")

  
  
  # OFFICIAL RECESSION ONLY
  # (MOST) NARROW STATISTIC, (MAY BE EVEN 'TOO' LATE)
  # 2018
  # NBER based Recession Indicators for the United States from the Peak through the Trough
  # updated algorithm? 
  # back through 1954
  # updated 'at' or 'just after' the 1st of each month
  # just one month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/USRECM.txt
  ####usrecm <- get_symbols_xts_eox("USRECM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  
  
  
  # RECESSION AND ECONOMIC DOWNTURN 
  # MEDIUM STATISTIC
  # somewhat bouncy
  # bus_ch_13 is the most reliable! bus_ch_11 is next
  # maybe a LITTLE TOO LATE going into the false recession of 2015-2016
  # maybe a 2-quarter '2 element' min/max/ave may be a good idea?
  # suprise (incorrect) down BUMP at 2015-12-31
  # near equivalents
  # Note: FRED has DRBLACBS    (very good): Delinquency Rate on Commercial and Industrial Loans, All Commercial Banks
  # Note: FRED has DALLCIACBEP (good)     : Delinquencies on All Loans and Leases, Commercial and Industrial, All Commercial Banks
  # ALSO CLOSE are the responses from the "Senior Load Officer Opinion Survey"
  # net_income of SP500 of banks ( and mktcap/net_income ) should be considered
  # Overall markets are *much more succeptible* to the performance of the "BANKS"
  # 
  ####bankruptcy_filing_counts_eoq_xts <- get_bankruptcy_filing_counts_eoq_xts(pub_dates = Sys.Date(), updating_file = "bankruptcy_filing_counts_eoq_xts.RData")
  # seq.Date by = "unit" (to be accurate) needs to start on the 1st of the month
  ####bus_ch_11 <- get_symbols_xts_eox(symbol_raw = bankruptcy_filing_counts_eoq_xts[ , "bus_ch_11"], returns = "monthly", raise_to_returns_frequency = TRUE, OHLC = FALSE, indexAt = "lastof")
  ####bus_ch_13 <- get_symbols_xts_eox(symbol_raw = bankruptcy_filing_counts_eoq_xts[ , "bus_ch_13"], returns = "monthly", raise_to_returns_frequency = TRUE, OHLC = FALSE, indexAt = "lastof")

  
  
  # RECESSION ONLY
  # NARROW STATISTIC (AND GREAT TIMING)
  # *ANY* DOWNSLOPING, then the PARTY is OVER
  # updated weekly (published on a THU, refers back to the previous FRI)
  # HELPFUL ( but just helpful - very smooth and *peaks* just before a recession )
  # Chicago Feds National Financial Conditions Index (NFCI) NFCINONFINLEVERAGE
  #   and HAS 3 components ( but only 'always!' useful component is NFCICREDIT )
  # DOWTNURNVERY VERY SENSITIVE # where the SLOPE # STARTS! dipping DOWNWARD matters
  # https://fred.stlouisfed.org/data/NFCINONFINLEVERAGE.txt
  ####nfcinonfinleverage <- get_symbols_xts_eox("NFCINONFINLEVERAGE", src = "FRED", returns = "monthly", day_delay = 6, OHLC = FALSE, indexAt = "lastof")
  
  
  
  # RECESSION AND ECONOMIC DOWNTURN
  # NARROW STATISTIC (AND GREAT TIMING)
  # updated quarterly. Publish date is later: 'one month and one day (the 2nd)'
  # just one month + one day delay: Last Updated - max(Date Range)) ** IMPORTANT **
  # ** "invest below zero" and/or "invest after seeing a 'very' steep downward slope" **
  # The January 2018 Senior Loan Officer Opinion Survey on Bank Lending Practices 
  # https://www.federalreserve.gov/data/sloos/sloos-201802.htm
  # Senior Loan Officer Opinion Survey on Bank Lending Practices
  # https://www.federalreserve.gov/data/sloos.htm
  # Senior Loan Officer Survey
  # Categories > Money, Banking, & Finance > Banking
  # https://fred.stlouisfed.org/categories/32239
  # Net Percentage of Domestic Banks Tightening Standards for Commercial and Industrial Loans to Large and Middle-Market Firms (DRTSCILM)
  # https://fred.stlouisfed.org/data/DRTSCILM.txt
  ####drtscilm <- get_symbols_xts_eox("DRTSCILM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")

  
  
  # OFFICIAL RECESSION AND  DRAWDOWN
  # NARROW STATISTIC : five month delay hurts performance
  # *ANY* DOWNSLOPING, then the PARTY is OVER
  # TOO LATE IN REC 1990
  # EARLY IN REC 2000 (GOOD)
  # TOO LATE IN REC 2008
  # EARLY IN 2016 DRAWDOWN (GOOD)
  # Delinquency Rate on Commercial and Industrial Loans, All Commercial Banks
  # quarterly, at the end of the period
  # back through 1987
  # updated 'just near' the end of the fifth month ( on the 22nd )
  # nearly five month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/DRBLACBS.txt
  # this one seems right
  ####drblacbs <- get_symbols_xts_eox("DRBLACBS", src ="FRED", returns = "monthly", raise_to_returns_frequency = TRUE, month_delay = 4, OHLC = FALSE, indexAt = "lastof")

  
  
  # OFFICIAL RECESSION AND DRAWDOWN
  # NARROW STATISTIC : five month delay hurts performance
  # *ANY* UPSLOPING*, then the PARTY is OVER ( BUT HAS YEAR OF HANGING JAGGEDNEWSS )
  # TOO LATE IN REC 1990
  # EARLY IN REC 2000 (GOOD)
  # TOO LATE IN REC 2008
  # EARLY IN 2016 DRAWDOWN (GOOD)
  # Delinquencies on All Loans and Leases, Commercial and Industrial, All Commercial Banks
  # quarterly, at the end of the period
  # back through 1987
  # updated 'just near' the end of the fifth month ( on the 22nd )
  # nearly five month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/DALLCIACBEP.txt
  # this one seems right
  ####dallciacbep <- get_symbols_xts_eox("DALLCIACBEP", src ="FRED", returns = "monthly", raise_to_returns_frequency = TRUE, month_delay = 4, OHLC = FALSE, indexAt = "lastof")
  
  
  
  # RECESSION AND ECONOMIC DOWNTURN
  # ANOTHER NARROW STATISTIC (AND GREAT TIMING)
  # updated quarterly. Publish date is later: 'one month and one day (the 2nd)'
  # just one month + one day delay: Last Updated - max(Date Range)) ** IMPORTANT **
  # ** "invest below zero" and/or "invest after seeing a 'very' steep downward slope" **
  # The January 2018 Senior Loan Officer Opinion Survey on Bank Lending Practices 
  # https://www.federalreserve.gov/data/sloos/sloos-201802.htm
  # Senior Loan Officer Opinion Survey on Bank Lending Practices
  # https://www.federalreserve.gov/data/sloos.htm
  # Senior Loan Officer Survey
  # Categories > Money, Banking, & Finance > Banking
  # https://fred.stlouisfed.org/categories/32239
  # NOT IN FRED
  # Net percentage of domestic banks increasing premiums charged on riskier loans for large and middle-market firms
  # https://www.quandl.com/data/FED/SUBLPDCILTR_N_Q-Net-percentage-of-domestic-banks-increasing-premiums-charged-on-riskier-loans-for-large-and-middle-market-firms-Quarterly
  ####fed_sublpdciltr_n_q <- get_symbols_xts_eox("FED/SUBLPDCILTR_N_Q", src = "Quandl", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  # or just
  # fed_sublpdciltr_n_q <- get_symbols_xts_eox("FED/SUBLPDCILTR_N_Q",               , returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  
  
  
  # competition/fear
  # RECESSION AND ECONOMIC DOWNTURN
  # ANOTHER NARROW STATISTIC
  # EARLY BEFORE RECESSION, JUST LATE-ISH BEFORE DOWNTURN
  # ORIG FROM ( INSPIRED BY )
  # The equity premium
  # https://fredblog.stlouisfed.org/2016/07/the-equity-premium/
  ####willshire_less_agg_equity_premium <- get_willshire_less_agg_equity_premium_eom_xts()
  # downtrending and below 5% means RECESSION or DRAWDOWN
  # equity_prem_p06m_ann <- willshire_less_agg_equity_premium[,c("equity_prem_p06m_ann")]
  # ALREADY IN FORM ( NOTHING TO DO )
  # XOR
  # FOR COMPLETENESS
  ####equity_prem_p06m_ann2 <- get_symbols_xts_eox(symbol_raw = willshire_less_agg_equity_premium[,c("equity_prem_p06m_ann")], returns = "monthly", OHLC = FALSE, indexAt = "lastof")
  
  
  
  # internal performance ( from PostgreSQL database )
  # all/banks: median/mean/weighted net_income, mktcap
  #
  # NOTE BUT THINK ABOUT USING INSTEAD "FRED BROAD STATISTICS (ABOVE)" ( THEY TEND TO BE BETTER )
  #
  # FAST DRAWDOWN NARROW STATISTIC 
  # BEFORE A FAST_DRAWDOWN(MINICRASH) ONE SEES A SHARP DROP IN NON-MARGINAL                                   every_pchg_netinc_q1q2
  # BEFORE A FAST_DRAWDOWN(MINICRASH) ONE SEES A SHARP DROP IN     MARGINAL                                   every_pchg_netinc_q1q2
  # BEFORE A FAST DRAWDOWN ONE SEES A MARGINAL *HISTORICALLY TOO HIGH*                                        every_rat_mktcap_o_netinc_q1_x_4
  # 
  # DOWNTURN NARROW-TO-BROAD STATISTIC 
  # BEFORE A DOWNTURN ONE SEES A GRADULE MONTH-BY-MONTH (AND NEGATIVE) GETTING WORSE NON-MARGINAL              banks_pchg_netinc_q1q2
  # BEFORE A DOWNTURN ONE SEES A GRADULE MONTH-BY-MONTH (AND NEGATIVE AND FAST ) GETTING WORSE MARGINAL        banks_pchg_netinc_q1q2
  # 
  # RECESSION BROAD STATISTICS
  # BEFORE A RECESSION ONE SEES A GRADULE MONTH-BY-MONTH (AND NEGATIVE AND SLOWLY ) GETTING WORSE NON-MARGINAL every_pchg_netinc_q1q2
  # BEFORE A RECESSION ONE SEES A GRADULE MONTH-BY-MONTH SLOWLY GETTING BIGGER MARGINAL                        every_rat_mktcap_o_netinc_q1_x_4
  #  NOTE: THIS IS FULLY RELIABLE: THIS CAN PEAK 6-12 MONTHS BEFORE A RECESSION
  # BEFORE A RECESSION ONE SEES A GRADULE MONTH-BY-MONTH AND VERY NEGATIVE                                     banks_rat_mktcap_o_netinc_q1_x_4
  # 
  ####sipro_inbnd_netinc_any_marginals_eom_xts           <- get_sipro_inbnd_netinc_any_marginals_eom_xts()
  ####sipro_inbnd_netinc_any_marginals_eom_xts_marginals <- get_sipro_inbnd_netinc_any_marginals_eom_xts(marginals = TRUE)

  # # PARTIAL view
  # # NOT a GOOD view of the 'ENTIRE'
  # # instantaneous look-at: sp rolling last 10 trillion
  #####sipro_rolling_finstats <- get_sipro_rolling_finstats()
  
  # # PARTIAL view
  # # NOT A GOOD view of the 'ENTIRE'
  # # large financial instititutions
  # ####sipro_rolling_finstats <- get_sipro_rolling_finstats(
  #        dateindex_criteria = "now.dateindex in ( select distinct dateindex from fe_data_store.si_finecon2 order by dateindex desc offset 0 limit 6 )"
  #      , other_criteria     = "now.sp in ('500') and now.industry_desc in ('Money Center Banks', 'Regional Banks', 'Consumer Financial Services', 'S&Ls/Savings Banks')"
  #      , rolling_limit     = "600000.00"
  #      )
  
  # HIGHLY recommend 
  #   something on VOLUME of trading
  # HIGHLY recommend
  #   something on VOLITILITY of 'willshire 5000 ind'(maybe VIX?)
  #
  # out-of-sample validators for my volatility trading research
  # Creating a Table of Monthly Returns With R and a Volatility Trading Interview
  # February 20, 2018
  # By Ilya Kipnis
  # https://www.r-bloggers.com/creating-a-table-of-monthly-returns-with-r-and-a-volatility-trading-interview/
  # https://quantstrattrader.wordpress.com/2018/02/20/creating-a-table-of-monthly-returns-with-r-and-a-volatility-trading-interview/
  # Some FAQs about the strategy
  # Ilya Kipnis is creating Quantitative Trading Strategies
  # https://www.patreon.com/quantstrattrader
  # ONE HOUR AND 37 MINUTES
  # based around VXV/VXMT ratios and other inputs.  His system successful avoided the most recent spike
  # Short Vix System Avoids huge Spike #13 Ilya Kipnis Short Vix trading
  # https://www.youtube.com/watch?v=Whx66_w78Cs
  
  # GOOD ARTICLE
  # What History Says About Low Volatility
  # The VIX is based on the amount investors are willing to pay for protection against all market moves
  # https://www.bloomberg.com/view/articles/2017-07-03/what-history-says-about-low-volatility
  #
  # SEE ( MY NOTES )
  # WHICH ASSET ALLOCATION WEIGHTS WORK THE BEST?
  # October 27, 2015
  # http://blog.alphaarchitect.com/2015/10/27/which-asset-allocation-weights-work-the-best/#gs.Xq5taBI

  # NOVEL CASE: 
  # NARROW STATISTIC: SOMEWHERE AT AN UPTURN TRYING TO GET OUT OF A MAJOR RECESSION: 
  # A NEW SHARP UP 'INCREASE IN SIZE OF BALANCE SHEET' ONLY MATTERS
  # 
  # In addition, after careful planning and public communication, 
  # last October [2017] the FOMC began to gradually and predictably reduce the size of the Fed's balance sheet. 
  # Reducing our securities holdings is another way to move the stance of monetary policy toward neutral. 
  # The balance sheet reduction process is going smoothly and is expected to contribute over time to a gradual tightening of financial conditions. 
  # Over the next few years, the size of our balance sheet is expected to shrink significantly.
  # https://www.federalreserve.gov/newsevents/speech/powell20180406a.htm
  # 
  # U.S. Treasury securities held by the Federal Reserve: All Maturities (TREAST)
  # https://fred.stlouisfed.org/series/TREAST
  # (SEE: https://github.com/AndreMikulec/expressions/blob/master/main-rcsnsight2-999.R)
  
  # BAZZAR
  # Assets: Central Bank Liquidity Swaps (WACBS)
  # https://fred.stlouisfed.org/series/WACBS
  #
  # (GIFT OF *FREE MONEY* TO BANKS)
  # BAZAAR - ** MASSIVE DROP **
  # Capital: Total Capital (WCTCL)
  #   MASSIVE DROP DEC2015/JAN2016
  # https://fred.stlouisfed.org/series/WCTCL
  # BAZAAR
  # Capital: Surplus (WCSL)
  #     MASSIVE DROP DEC2015/JAN2016
  # https://fred.stlouisfed.org/series/WCSL
  # BAZAAR
  # Factors Affecting Reserve Balances of Depository Institutions: Reverse Repurchase Agreements: Foreign Official and International Accounts (WREPOFOR)
  # https://fred.stlouisfed.org/series/WREPOFOR
  # BAZARR
  # ANOTHER DEC/JAN 2015/2016 mass drop
  # Factors Affecting Reserve Balances of Depository Institutions: Other Liabilities and Capital (WOTHLIAB)
  # https://fred.stlouisfed.org/series/WOTHLIAB
  
  # end gather data
  
  ### #### ### ### ### ### ### ### ###
  ### ALL POSSIBLE RULES (STRATEGIES)
  ### 
  
  # strategies 
  # 
  #  Return.porfolio # COLUMN order DOES matter
  #  "weights" must have the same number of columns as "R"
  #  "weights" colnames are ignored! 
  #  (so the 'weights' column order (names) 
  #  must physically match to the corresponding Rs column order names)
  
  # begin buy and hold weights
  
  # default benchmark: zero percent(0.00) to one hundred percent(1.00) weights
                                      # 100%
  buyandhold_will5000ind_rules_wts <- xts(rep(1,NROW(all_possible_instrument_log_rets)),index(all_possible_instrument_log_rets))
  # "will5000ind"
  colnames(buyandhold_will5000ind_rules_wts) <- "will5000ind"
  buyandhold_will5000ind_rules_wts_dates <- index(buyandhold_will5000ind_rules_wts)
  # end buy and hold weights
  
  # begin portfolio weights
  
  # unstable structure to debug inside: holds for 2 seconds then bounces out
  # within.xts IS TOO VOLITILE: CAN NOT browser()/rstudio debug inside: SOMETHING IS NOT RIGHT
  # 
  # TODO [ ] REPLACE BELOW WITH
  # 
  # # what xts objects ( single column xts objects  ) where specifyModel ( getModelData ( exists ) )  MAY try to see
  # xs.not.exist <- c(sapply( colnames(x), function(x) { if(!exists(x)) { x } else { NULL }  } ))
  
  # # assign where specifyModel ( getModelData ( exists ) ) can find
  # for(xi in xs.not.exist) {
  #   assign(xi, x[,xi], envir = .GlobalEnv)
  # }
  
  # specifyModel ( getModelData ( exists ) ) 
  # quantmod:::buildModel.hardened(custom)
  # #  quantmod class ->  vtreat ->  as.quantmod -> quandmod class                # LEFT_OFF note
  # #  quantmod class ->  Boruta(getImpXgboost) ->  as.quantmod -> quantmod class # LEFT_OFF note
  # #  quantmod class ->  UBL::SmoteRegress( rel = matrix/"auto", C.perc = "balance"/"extreme") # SEE my R function/notes  

  # Handle Imbalanced Dataset
  # 
  # For common cases such as ads clickthrough log, 
  # the dataset is extremely imbalanced. 
  # This can affect the training of xgboost model, and there are two ways to improve it.
  # 
  # If you care only about the ranking order (AUC) of your prediction
  #   Balance the positive and negative weights, via 
  #                                           scale_pos_weight
  #   Use AUC for evaluation
  # If you care about predicting the right probability
  #   In such a case, you cannot re-balance the dataset
  #   In such a case, set parameter 
  #      max_delta_step to a finite number (say 1) will help convergence
  # 
  # http://xgboost.readthedocs.io/en/latest/how_to/param_tuning.html
  # 
  #   scale_pos_weight, [default=1]
  #     Control the balance of positive and negative weights, useful for unbalanced classes.
  #     Also see Higgs Kaggle competition demo for examples: R, py1, py2, py3
  # 
  #       testsize <- 550000
  #       weight <- as.numeric(dtrain[[32]]) * testsize / length(label)
  #       sumwpos <- sum(weight * (label==1.0))
  #       sumwneg <- sum(weight * (label==0.0))
  #       print(paste("weight statistics: wpos=", sumwpos, "wneg=", sumwneg, "ratio=", sumwneg / sumwpos))
  #       "scale_pos_weight" = sumwneg / sumwpos
  # 
  #       https://github.com/dmlc/xgboost/blob/master/demo/kaggle-higgs/higgs-train.R
  # 
  #   max_delta_step [default=0]
  #     Maximum delta step we allow each tree’s weight estimation to be.
  #     it might help in logistic regression when class is extremely imbalanced. 
  #     Set it to value of 1-10 might help control the update
  # 
  # http://xgboost.readthedocs.io/en/latest/parameter.html
  
  # #    also SEE my notes: Hugh Miller gbm optimization techniques

  # some of the most predictive attributes were not linearly correlated with the targets, ( REALLY! ) 
  # 
  #   using single numerical attributes
  #     we build shallow decision trees (2-4 levels deep)
  #       and used their predictions as extra features. 
  # 
  #   using two features at a time 
  #     We also build shallow decision trees           'IN THE HOPE' - DID IT WORK?
  #       and used their prediction as an extra feature 
  #         (in the hope of capturing some non-additive interactions among features.)

  # SEE other NOTES
  # quantmod:::buildModel.randomForest
  #   quantmod:::buildModel.caret(xgboost, weights = ) # roll my own
  #       simple multi variable testing: 
  #         could split ors(|) SEE FAR BELOW 
  #         into threes and/or two pair combos and test using quantmod:::buildModel.OTHER
  #  single variable testing: make myself a quantmod:::buildModel.lm
  # tradeModel   ( getModelData ( exists ) ): if the data is already in the passed in "quantmod" object
  #                                           the just use modelData to get the data
  #                                           instead of re-recalling getModelData
  # tradeModel.unrate <- f(. . .) Return.porfolio
  # tradeModel.unrate <- f(. . ., signal.threshold = c(0, 0), . .  )
  #   uses a 'short of the 'willshire 5000 index: WILL5000IND(St.Louis.Fred)

  # > methods(class = "quantmod")
  #  [1] anova         coef          coefficients  fitted        fitted.values
  #  [6] fittedModel<- formula       logLik        plot          resid
  # [11] residuals     show          summary       vcov
  # 
  # > methods("fittedModel<-")  # NAMESPACE # exportMethods(show,summary,'fittedModel<-')
  # [1] fittedModel<-,quantmod-method
  # 
  # R/zzz.R
  # setMethod("show","quantmod"
  # setMethod("summary","quantmod"
  # "fittedModel"<-function(object) {object@fitted.model}
  # setReplaceMethod("fittedModel","quantmod", function(object,value)
  # {
  #     object@fitted.model <- value
  #     
  # }
  # 
  # > methods(class = "xgb.Booster")
  # [1] predict print
  # 
  # [105] predict.train                predict.train.recipe*
  # [107]                              predict.xgb.Booster*
  # [109] predict.xgb.Booster.handle* 
  # 
  # > methods(class = "train")
  #  [1] confusionMatrix densityplot     fitted          ggplot
  #  [5] histogram       levels          plot            predict
  #  [9] predictors      print           residuals       stripplot
  # [13] summary         update          varImp          xyplot
  # see '?methods' for accessing help and source code
  
  #   modelData(. . . continued from above in 'tradeModel' . . .)
  #   quantmod:::predictModel.randomForest
  #   quantmod:::predictModel.caret(xgboost) # roll my own # LEFT_OFF note
  #       single variable testing: make myself a quantmod:::predictModel.lm
  #       multiple variable testing: SEE ABOVE
  #     retuned object becomes a zoo object ( so the object can work in zoo:::ifelse.zoo )
  #     (I made ifelse.xts(SEE BELOW): I an try this)
  # 
  #   # eventually
  #   quantmodResults <- list(model = quantmod, signal = signal.zoo)
  #   model.returns <- modelReturn(quantmodResults, trade.dates = trade.dates, leverage = leverage, ret.type = ret.type)
  #     modelReturn
  #       # eventually ( but I just need to calculate the xts weights ( to be inputed into Return.portfolio ) )
  #       model.results <- trade.signal[, 1] * leverage * trade.signal[, 2]
  #       model.results[which(is.na(model.results))] <- 0
  #       model.cumret <- cumprod(1 + model.results)
  
  # # remove anything new that was placed
  # for(xi in xs.not.exist) {
  #   rm(list = xi, envir = .GlobalEnv)
  # }
  
  # unstable structure to debug inside: holds for 2 seconds then bounces out
  # will5000ind cash            "datetime", "unrate"
  unrate_will5000ind_rules_wts <- within.xts( all_possible_indicators, { 

    # register before using
    wts_vars <- "unrate"
    wts_vars_missing <- character()
    for(wts_var in wts_vars) {
      if(!exists(wts_var, envir = environment()))  wts_vars_missing <- c(wts_vars_missing, wts_var)
    }
    if(length(wts_vars_missing)) {
      stop(paste0("  portfolio_wts within is missing variables . . . \n  ", R.utils::hpaste(wts_vars_missing,  maxHead = Inf, lastCollapse=" and ") ))
    }
    rm(wts_var, wts_vars_missing, wts_vars)

    # Return.portfolio requirement: rets columns to  weights columns matchups: same # of columns 
    #                                                                          weights column names are ignored
  
    # 2007-09-30 + lag(+-1) IS THE time of SWITCH
    # performanceAnalytics Return.portfolio weights(xts)
    # SMOOTHER HACK(OVERFIT)
    # SHOULD WORK TO FIND SOMETHING *MORE* PRECISE
    
    # # rough eyball check
    # # months of 4% loss or more
    # 1978: OCT(very very bad)
    # 1998: AUG(very very bad)
    # 1981: AUG
    # 1987: OCT(very very bad)
    # 1994: MAR
    # 1997: MAR
    # 1998: AUG(very very bad)
    # 2000: JAN, APR
    # 2010: JUN, AUG
    # 2011: MAY, JUN, JUL, SEP(very bad)
    # 2016: JAN(bad)
    will5000ind <- ifelse( ((SMA(unrate,2)        - SMA(    unrate   ,6)) <= 0)              | 
                           ((SMA(lag(unrate),2)   - SMA(lag(unrate  ),6)) <= 0)              | 
                           ((SMA(lag(unrate,2),2) - SMA(lag(unrate,2),6)) <= 0), 1.00, 0.00) 
    
    # some early time before the indictator 
    # has enough information to make a decison
    # then just simply "buy and hold" in "cash"
    will5000ind[is.na(will5000ind)] <- 1 # 100% allocated
    
    # what is "left over"
    # creates column
    cash <- 1 - will5000ind
    
    # what happened last month affects next months decision
    # APROPRIATE # 1st of the NEXT MONTH # all DECISIONS happening in that NEXT MONTH
    # Return.portfolio requirement
    datetime <- datetime + 1; rm(unrate) # required "rm position"
  
  })
  unrate_will5000ind_rules_wts_dates <- index(unrate_will5000ind_rules_wts)
  
  bookmark <- 1

  # end portfolio wieghts 

  ### ### ### ### ### ### ### 
  ### BEGIN INTERACTIONS 
  ### 

  # begin portfolio

  # 100,000 dollars to start
  initial_value <- 100000
  
  # Return.porfolio
  # rebalance_on: Ignored if 'weights' is an xts object that specifies the rebalancing dates
  # verbose is TRUE, return a list of intermediary calculations
  
  # Return.porfolio(R, weights)
  #   COLUMN order/number_of_columns DOES matter
  #   Return.porfolio(R, weights)
  #   "weights" must             have the same number of columns as "R"
  #   "weights" does not need to have the same column names as "R"
  #   "weights" columns must    have the same order as the columns of "R"
  
  message("")
  message("    unrate_will5000ind_portf   ")
  print(tail(unrate_will5000ind_rules_wts))

  # see caroline::dbWriteTable2
  # match: returns a vector of the positions of (first) matches of its first argument in its second.
  # c("b","d","c")[match(c("c","b"),c("b","d","c"))]
  # [1] "c" "b"
  # garantee 

  # NEED a Return.portfolio.X safe R/weights form: errors out if many exact common columns do not exist in each

  # very few instruments # very many weights
  
  unrate_will5000ind_portf <- Return.portfolio(R = all_possible_instrument_log_rets[,match(colnames(unrate_will5000ind_rules_wts), colnames(all_possible_instrument_log_rets))], weights =  unrate_will5000ind_rules_wts, value = initial_value, verbose = TRUE)
  # "portfolio.returns"
  unrate_will5000ind_portf_log_rets <- unrate_will5000ind_portf$returns

  # default class yearmon # type="arithmetic"
  unrate_will5000ind_portf_nonlog_monthly_rets <- monthlyReturn(exp(cumsum(unrate_will5000ind_portf_log_rets)) * initial_value)
  
  # extra portfolio information
  
  # alternative method
  # 
  # CalendarReturnTable
  # 
  # Creating a Table of Monthly Returns With R and a Volatility Trading Interview
  # February 20, 2018
  # By Ilya Kipnis
  # https://quantstrattrader.wordpress.com/2018/02/20/creating-a-table-of-monthly-returns-with-r-and-a-volatility-trading-interview/
  # https://www.r-bloggers.com/creating-a-table-of-monthly-returns-with-r-and-a-volatility-trading-interview/
  
  # geometric: only used for the cumulative return for each year
  # geometric: utilize geometric chaining (TRUE) or simple/arithmetic
  #            chaining (FALSE) to aggregate returns, default TRUE

  # other table values: just displays whatever is input
  View(table.CalendarReturns(unrate_will5000ind_portf_nonlog_monthly_rets, digits = 1, as.perc = TRUE, geometric = TRUE))   
  # WORKS
  
  # end portfolio
  
  # begin default benchmark

  ### CURRENTLY NOT USEFUL
  ### message("")
  ### message("    buyandhold_will5000ind_rules_wts   ")
  ### print(tail(buyandhold_will5000ind_rules_wts))

  buyandhold_will5000ind_portf <- Return.portfolio(R = all_possible_instrument_log_rets[,match(colnames(buyandhold_will5000ind_rules_wts), colnames(all_possible_instrument_log_rets))], weights =  buyandhold_will5000ind_rules_wts, value = initial_value, verbose = TRUE)
  # "portfolio.returns"
  buyandhold_will5000ind_portf_log_rets <- buyandhold_will5000ind_portf$returns

  # default class yearmon # type="arithmetic"
  buyandhold_will5000ind_portfolio_nonlog_monthly_rets <- monthlyReturn(exp(cumsum(buyandhold_will5000ind_portf_log_rets)) * initial_value) 

  # end default benchmark

  ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### BEGIN EVALUATION STRATEGY(RULES) PERFORMANCE
  
  # <anything< v.s buyandhold_will5000ind_portf_nonlog_monthly_rets
  
  ### ### ### ### 
  ### TODO AREA
  ### 
  
  # NEXT return of stocks verses return of bonds
  #      willshire_less_agg_equity_premium (6 months) 5% rule
  # NEXT NEXT rolling percentile rank of price /earnings ratio
  #
  # weighted ave ( by mktcap ) of the last three months + a SMA over the last three monhts
  # NOTE NEED TO inver the query results so net_income is in the denominator 
  # every_rat_mktcap_o_netinc_q1_x_4
  # sipro_inbnd_netinc_any_marginals_eom_xts_marginals <- get_sipro_inbnd_netinc_any_marginals_eom_xts(marginals = TRUE)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("  end get_up_side_down_side")
  
  up_side_down_side <- NULL
  return(up_side_down_side)

}
# up_side_down_side <- get_up_side_down_side()


# bbee01.R
