 
# bbee01.R

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
  
  require(xts) # cbind.xts
  require(TTR) # ROC
  # uses lubridate `%m+%`
  `%m+%` <- lubridate::`%m+%`
  
  # WHAT I AM TRYING TO OPTIMIZE
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
  will5000ind <- get_fred_wilshire5000_eom_xts()
  will5000ind_log_rets <- ROC(will5000ind)               # which(is.na(will5000ind_log_rets))
  will5000ind_log_rets[is.na(will5000ind_log_rets)] <- 0 # usually just the 1st observation
  
  
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
  unrate <- get_symbols_xts_eox("UNRATE", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  
  bm <- 1
  
  # 2007-09-30 + lag(-1) IS THE time of SWITCH
  # performanceAnalytics Return.portfolio weights(xts)
  # SMOOTHER HACK(OVERFIT)
  # SHOULD WORK TO FIND SOMETHING *MORE* PRECISE
  w <- with( unrate[["monthly"]], { ifelse( ((SMA(unrate,2) - SMA(unrate,6)) <= 0) | ((SMA(lag(unrate),2) - SMA(lag(unrate),6)) <= 0) | ((SMA(lag(unrate,2),2) - SMA(lag(unrate,2),6)) <= 0), 1.00, 0.00) } )
  # what happened last month affects next months decision
  # PE R.p requirement
  # # APROPRIATE # FIRST of the MONTH # all DECISIONS affecting THAT month
  index(w) <- index(w) + 1 
  # PE R.p requirement
  colnames(w) <- "will5000ind"
  
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
  ####bus_ch_13 <- get_symbols_xts_eox(symbol_raw = bankruptcy_filing_counts_eoq_xts[ , "bus_ch_13"], returns = "monthly", raise_to_returns_frequency = TRUE, OHLC = FALSE, indexAt = "lastof")
  ####bus_ch_11 <- get_symbols_xts_eox(symbol_raw = bankruptcy_filing_counts_eoq_xts[ , "bus_ch_11"], returns = "monthly", raise_to_returns_frequency = TRUE, OHLC = FALSE, indexAt = "lastof")
  
  
  
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

  # BAZZAR
  # Assets: Central Bank Liquidity Swaps (WACBS)
  # https://fred.stlouisfed.org/series/WACBS
  # BAZAAR - ** MASIVE DROP **
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
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  up_side_down_side <- NULL
  return(up_side_down_side)

}
# up_side_down_side <- get_up_side_down_side()


# bbee01.R
