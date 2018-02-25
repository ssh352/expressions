 
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
  unrate <- get_quantmod_xts_eox("UNRATE", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1)
  
  # PRE/POST RECESSION ONLY
  # PART OF BROAD STATISTIC (UNRATE(ABOVE))
  # SNEAK PEAK INTO THE FUTURE OF THE "MOST CONSERVATIVE(OUTSIDE) STATISTIC"
  #
  #  Employment Level: Part-Time for Economic Reasons, All Industries
  # back through 1955
  # updated the 'first or second' friday of each month ( variable: same as UNRATE )
  # ** ASSUME I run just after the RELEASE of the month ***  *** IMPORTANT ***
  # just a one month delay (Last Updated - max(Date Range))
  # Schedule of Releases for the Employment Situation ( SEEMS same day as UNRATE but 19 minutes later ) 
  # https://www.bls.gov/schedule/news_release/empsit.htm
  # # just four month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/LNS12032194.txt
  lns12032194 <- get_quantmod_xts_eox("LNS12032194", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1)
  
  # RECESSION AND ECONOMIC DOWNTURN ( NOTE: TODO: verify ON ALFRED )
  # MEDIUM STATISTICS
  # OECD based Recession Indicators for the United States from the Peak through the Trough
  # back through 1947
  # updated 'near' the middle of each month
  # just four month delay (Last Updated - max(Date Range))
  # http://research.stlouisfed.org/fred2/data/usarecm.txt
  usarecm <- get_quantmod_xts_eox("USARECM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 4)
    
  # OFFICIAL RECESSION ONLY ( author in journal articles claims to be faster than USRECM(NBER))
  # NARROW STATISTIC (MAY BE LATE)
  # Piger, Jeremy Max, Chauvet, Marcelle
  # back through 1967
  # updated 'at' or 'just after' the 1st of each month
  # just three month delay (Last Updated - max(Date Range))
  # http://research.stlouisfed.org/fred2/data/RECPROUSM156N.txt
  # NOTE:Piger screwed up in 2015/2016 # but only alfred shows
  # NOTE: R package alfred # SOMETIMES # can get the 'actual day reported'
  recprousm156n <- get_quantmod_xts_eox("RECPROUSM156N", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 3)

  # OFFICIAL RECESSION ONLY
  # (MOST) NARROW STATISTIC, (MAY BE EVEN 'TOO' LATE)
  # 2018
  # NBER based Recession Indicators for the United States from the Peak through the Trough
  # updated algorithm? 
  # back through 1954
  # updated 'at' or 'just after' the 1st of each month
  # just one month delay (Last Updated - max(Date Range))
  # https://fred.stlouisfed.org/data/USRECM.txt
  usrecm <- get_quantmod_xts_eox("USRECM", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1)

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
  all_dates <- seq.Date(min(index(bankruptcy_filing_counts_eoq_xts) + 1L), Sys.Date(), by = "month") - 1L
  bankruptcy_filing_counts_eom_xts <- na.locf(merge.xts(bankruptcy_filing_counts_eoq_xts, xts(, all_dates) ))
  rm(all_dates)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  up_side_down_side <- NULL
  return(up_side_down_side)
  
  # zimmerman
  # LEFT OFF: 
  # competition ( something better )
  # 6 months seems to work best
  # get_willshire_less_agg_equity_premium_eom_xts
  # 
  # internal performance
  # # All/banks: median net_income, mktcap net_income
  # 
  # vote on performance
  # # All/banks: mktcap / net_income
  # other strengthening/weakening
  # DRBLACBS, DALLCIACBEP, Senior Load Officer Opinion Survey , mktcap / net_income

}
# get_up_side_down_side()

# bbee01.R

