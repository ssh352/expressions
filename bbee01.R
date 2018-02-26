 
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
  
  require(xts)
  # uses lubridate `%m+%`
  `%m+%` <- lubridate::`%m+%`
  
  
  # PRE/POST RECESSION ONLY
  # MOST BROAD STATISTIC
  #
  # Civilian Unemployment Rate
  # back through 1948
  # updated the 'first or second' friday of each month ( variable )
  # ** ASSUME I run just after the RELEASE of the month ***  *** IMPORTANT ***
  # just a one month delay (Last Updated - max(Date Range))
  # Schedule of Releases for the Employment Situation
  # https://www.bls.gov/schedule/news_release/empsit.htm
  # # just four month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/UNRATE.txt
  unrate <- get_symbols_xts_eox("UNRATE", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  
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
  lns12032194 <- get_symbols_xts_eox("LNS12032194", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  
  # RECESSION AND ECONOMIC DOWNTURN ( NOTE: TODO: verify ON ALFRED )
  # MEDIUM STATISTICS
  # OECD based Recession Indicators for the United States from the Peak through the Trough
  # back through 1947
  # updated 'near' the middle of each month
  # just four month delay (Last Updated - max(Date Range))
  # http://research.stlouisfed.org/fred2/data/usarecm.txt
  usarecm <- get_symbols_xts_eox("USARECM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 4, OHLC = FALSE, indexAt = "lastof")
    
  # OFFICIAL RECESSION ONLY ( author in journal articles claims to be faster than USRECM(NBER))
  # NARROW STATISTIC (MAY BE LATE)
  # Piger, Jeremy Max, Chauvet, Marcelle
  # back through 1967
  # updated 'at' or 'just after' the 1st of each month
  # just three month delay (Last Updated - max(Date Range))
  # http://research.stlouisfed.org/fred2/data/RECPROUSM156N.txt
  # NOTE:Piger screwed up in 2015/2016 # but only alfred shows
  # NOTE: R package alfred # SOMETIMES # can get the 'actual day reported'
  recprousm156n <- get_symbols_xts_eox("RECPROUSM156N", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 3, OHLC = FALSE, indexAt = "lastof")

  # OFFICIAL RECESSION ONLY
  # (MOST) NARROW STATISTIC, (MAY BE EVEN 'TOO' LATE)
  # 2018
  # NBER based Recession Indicators for the United States from the Peak through the Trough
  # updated algorithm? 
  # back through 1954
  # updated 'at' or 'just after' the 1st of each month
  # just one month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/USRECM.txt
  usrecm <- get_symbols_xts_eox("USRECM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")

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
  bankruptcy_filing_counts_eoq_xts <- get_bankruptcy_filing_counts_eoq_xts(pub_dates = Sys.Date(), updating_file = "bankruptcy_filing_counts_eoq_xts.RData")
  # seq.Date by = "unit" (to be accurate) needs to start on the 1st of the month
  bus_ch_13 <- get_symbols_xts_eox(symbol_raw = bankruptcy_filing_counts_eoq_xts[ , "bus_ch_13"], returns = "monthly", raise_to_returns_frequency = TRUE, OHLC = FALSE, indexAt = "lastof")
  bus_ch_11 <- get_symbols_xts_eox(symbol_raw = bankruptcy_filing_counts_eoq_xts[ , "bus_ch_11"], returns = "monthly", raise_to_returns_frequency = TRUE, OHLC = FALSE, indexAt = "lastof")
  
  # RECESSION ONLY
  # NARROW STATISTIC (AND GREAT TIMING)
  # *ANY* DOWNSLOPING, then the PARTY is OVER
  # updated weekly (published on a THU, refers back to the previous FRI)
  # HELPFUL ( but just helpful - very smooth and *peaks* just before a recession )
  # Chicago Feds National Financial Conditions Index (NFCI) NFCINONFINLEVERAGE
  #   and HAS 3 components ( but only 'always!' useful component is NFCICREDIT )
  # DOWTNURNVERY VERY SENSITIVE # where the SLOPE # STARTS! dipping DOWNWARD matters
  # https://fred.stlouisfed.org/data/NFCINONFINLEVERAGE.txt
  nfcinonfinleverage <- get_symbols_xts_eox("NFCINONFINLEVERAGE", src = "FRED", returns = "monthly", day_delay = 6, OHLC = FALSE, indexAt = "lastof")
  
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
  drtscilm <- get_quantmod_xts_eox("DRTSCILM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")

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
  fed_sublpdciltr_n_q <- get_symbols_xts_eox("FED/SUBLPDCILTR_N_Q", src = "Quandl", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  # or just
  # fed_sublpdciltr_n_q <- get_symbols_xts_eox("FED/SUBLPDCILTR_N_Q",               , returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
  
  # vote on performance
  # (more banks)
  # # All/banks: mktcap / net_income
  # other strengthening/weakening
  # DRBLACBS, DALLCIACBEP,
  
  # gov support
  #
  # discount rate
  # INTDSRUSM193N
  # 
  # federal funds rate
  # FEDFUNDS
  # 
  # get_clev_easing_balances_eom_xts
  
  # competition/pessimism
  #
  # zimmerman equity index
  # LEFT OFF: 
  # competition ( something better )
  # 6 months seems to work best
  # get_willshire_less_agg_equity_premium_eom_xts

  # internal performance ( from PostgreSQL database )
  # # All/banks: median net_income, mktcap net_income
  # # mktcap / net_income

  Sys.setenv(TZ=oldtz)
  options(ops)
  
  up_side_down_side <- NULL
  return(up_side_down_side)

}
# get_up_side_down_side()

# bbee01.R
