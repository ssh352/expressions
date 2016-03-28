# 


# Inspirations
###############

# Charles Givens
# Wealth without Risk:Bonds, Stocks, Money Markets, and the Fed Interest Rates
#
# Stocks_For_The_Long_Run_2nd(1994)(Seigel).pdf
# 
# A_Quantitative_Approach_to_Tactical_Asset_Allocation(2006)(Faber)(Seigel)_ORIGINAL.pdf
# How to Get Data | An Introduction into quantmod
# November 27, 2014   
# http://statmath.wu.ac.at/~hornik/QFS1/quantmod-vignette.pdf
# 
# AAII article and BOOK
# our classification scheme is to look at both interest rates to classify the overall monetary environment. 
# 
# If both are restrictive, then we classify the overall monetary environment as restrictive. 
# Likewise, 
# if both are expansive, we classify the overall monetary environment as expansive. 
# 
# If one of the rates is expansive and the other is restrictive, 
# we classify the overall monetary environment as indeterminate. 
# 
# AAII Journal 2015 April
# Follow the Fed, but Be Smart About It
# by Gerald R. Jensen , Robert R. Johnson and Luis Garcia-Feijoo
# http://www.aaii.com/journal/article/follow-the-fed-but-be-smart-about-it
# 
# Invest with the Fed: Maximizing Portfolio Performance by Following Federal Reserve Policy Hardcover  – March 23, 2015 
# by Robert R. Johnson (Author),    Gerald R. Jensen (Author), & 1 more 
# http://www.amazon.com/Invest-Fed-Maximizing-Portfolio-Performance/dp/0071834400


# Dat references

# found_that OLD NOT_UPDATED
# Discount Rate      - SINCE 1970 # through February 2010
# Federal Funds Rate - SINCE 1970 # through February 2010
# Historical Changes of the Target Federal Funds and Discount Rates
# UPDATED 2/19/10 ( bottom of the page )
# https://www.newyorkfed.org/markets/statistics/dlyrates/fedrate.html
# 
# Selected Interest Rates (Daily) - H.15 ( HELP OR NOT ) - YES!!
# http://www.federalreserve.gov/releases/h15/data.htm

# 
# Historical Discount Rates ( LEFT MOST COLUMN - *Prmary_Credit - ROUGHLY THE LAST 10 YEARS )
# Primary and Secondary credit [MS Excel; 30K] # OPENS EXCEL
# https://www.frbdiscountwindow.org/~/media/Documents/primarysecondary.ashx?la=en # OPENS UP EXCEL 
# https://www.frbdiscountwindow.org/en/Pages/Discount-Rates/Historical-Discount-Rates.aspx
# 
# Federal funds (effective) 1 2 3 - Daily - ( SINCE 1997   : 4 day delay ) # HERE #
# http://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=c5025f4bbbed155a6f17c587772ed69e&lastObs=&from=&to=&filetype=csv&label=include&layout=seriescolumn
# 
# Discount window primary credit 2 9 - Daily - (SINCE 2003 : 4 day delay ) : # HERE #
# http://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=e048853c9a3f0734b8538f508828f298&lastObs=&from=&to=&filetype=csv&label=include&layout=seriescolumn
# 
# Selected Interest Rates (Daily) - H.15 ( HELP OR NOT )
# http://www.federalreserve.gov/releases/h15/update/
#   

  
# Cleveland Fed has the Asset Purchase dates
#   
# rightish columns
# QE DATES   MortgageBackedSecurity     long term treasury purchases
# 
# -- begin begin qe1 -- # MY GITHUB NOTES
#   01-04-2009 
#   03-18-2009
# -- end begin qe1 --
#   
# -- begin end qe1 --   # MY GITHUB NOTES
#   03-31-2010          # LOOKS FLATISH
# -- end end qe1 --
#   
# -- begin begin qe2 --   # MY GITHUB NOTES
#   2010-11-03 # LOOKS INCREASING NUMBERS
# -- end begin qe2 --
#   
# -- begin end qe2 --   # MY GITHUB NOTES
#   2014-10-29 # LOOKS FLATISH # ( OR 2014-10-31 from my github NOTES )
# -- end end qe2 --
#   
# C:\Users\NFF397N\ANDREBIGPDFS\CreditEasingBalanceSheet_CLEVELAND_FED.xls # ( SEE )
# 
# https://cran.r-project.org/web/packages/easingr/index.html # MARCH 10, 2016
  

# Downloads FRB credit easing policy tools 
# federal agency debt 
# and 
# mortgage-backed securities data. ( mortgage backed ) 
# -----------------------------------------------------
#   
#   library(easingr)
# 
# # download includes easing tool weekly time series data starting January 2007
# 
# Lending to financial institutions
# Providing liquidity to key credit markets
# Purchasing longer-term securities
# 
# # http://www.rdocumentation.org/packages/easingr/functions/getEasingSummary
# 
# long-term treasury purchases weekly time series
# 
# getEasingLongTermPurchases(startDate = "012007", endDate = "122100")
# 
# lt <- getEasingLongTermPurchases()
# head(lt$df)
# 
# federal agency debt mortgage backed securities purchases
# 
# getEasingAgencyDebt(startDate = "012007", endDate = "122100")
# 
# ad <- getEasingAgencyDebt()
# head(ad$df)
# 
# rdocumenation.org SEARCH on  (Description: federal)
# Matt Barry 
# http://www.rdocumentation.org/packages/easingr/functions/getEasingAgencyDebt
# AND
# https://www.clevelandfed.org/our-research/indicators-and-data/credit-easing.aspx
# BUT
# Package ‘easingr’ was removed from the CRAN repository.
# Formerly available versions can be obtained from the archive. 
# Archived on 2014-12-29 as the service it used is no longer available. 
# https://cran.r-project.org/src/contrib/Archive/easingr/easingr_1.0.1.tar.gz
# 
# cfurl <- paste("http://www.clevelandfed.org/research/data/credit_easing/chart_csv.cfm?id="
# ,id,
# "&start=",
# startDate,
# "&end=",
# endDate,
# sep='')
# 
# C:\Users\NFF397N\ANDREEXTRA\easingr_1.0.1\R\easingr.R # SEAMS # THIS # CAN # BE FIXED TO READ THE xls
# 
# CLEVELAND FED - SEARCH   easing
# page shows the balance sheet with its components 
#   broadly divided into these categories, 
# plus the base of traditional Federal Reserve assets. 
# https://www.clevelandfed.org/en/our-research/indicators-and-data/credit-easing.aspx
# 
# MortgageBackedSecurity 
# AND MAYBE (this and OTHER) far RIGHT columns: HARD to TELL: 
#   best (ABOLUTE CHANGE in values) + inflation_index?
# Download Dataset 
# https://www.clevelandfed.org/~/media/files/charting/crediteasingbalancesheet.xls?la=en   # HERE #






# safely make an xts into a data.frame
dfize <- function(xtso) {
  df <- as.data.frame(xtso)  # zoo::as.data.frame.zoo # xts:::as.data.frame.xts
  rn <- as.numeric( zoo::as.Date( row.names(df) ) )
  cb <- cbind(rn,df)
  colnames(cb)[1] <- "tindex"
  return(cb)
} 


# safely make an xts into a data.frame
xtsize <- function(dfo) {
  id <- zoo::as.Date(as.numeric(  as.vector(unlist(dfo[,"tindex"]))   ))   
  # drop column
  dfo[,"tindex"] <- NULL
  cd <- coredata(as.matrix(dfo))
  xts(cd,id)  # could be 'zoo'
}
# 


within.xts <- function (data, expr, ...) {
  data <- dfize(data)
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


# # EXISTS # WORKS # I HAVE TO MANUALLY RENAME THE OUTPUT COLUMN MYSELF
# zoo:::with.zoo
# function (data, expr, ...)
# {
#   stopifnot(length(dim(data)) == 2)
#   eval(substitute(expr), as.list(data), enclos = parent.frame())
# }
# <environment: namespace:zoo>
#   


zoo_slope_change_num <- function(vec_interval =  c(5.2,6.8,8.4,10.0,  10.6,10.6,10.5,10.55, 11.0,11.3),
                                 max_width = 30 ) {
  zoo::rollapply(data = vec_interval
                 , width = max_width
                 , FUN = function(x) { 
                   last_x <- rev(x)[1]
                   all_other_xs <- rev(x)[-1]
                   all_other_xs <- all_other_xs[all_other_xs != last_x]
                   uniq_all_other_xs <- unique(all_other_xs)
                   # if at any time within max_width a point was greater than the last point
                   #   then a slope change did occur
                   if(length(all_other_xs) == 0)   return(2)
                   if(all(uniq_all_other_xs >  last_x)) return(1)
                   if(all(uniq_all_other_xs == last_x)) return(2)
                   if(all(uniq_all_other_xs <= last_x)) return(3)
                   return(2)
                 }
                 , partial = TRUE, align = "right") -> z
  
  return(z)
  
}

# > zoo_slope_change_num()
# [1] 2 3 3 3 3 2 1 1 3 3
# > zoo_slope_change_num(max_width = 2)
# [1] 2 3 3 3 3 2 1 3 3 3
# > zoo_slope_change_num(max_width = 3)
# [1] 2 3 3 3 3 2 1 1 3 3


# oftenish would be e.g. a 'fred monthy'   #  dailyish would be e.g. GSPC
zoo_delay <- function(oftenish = xts::xts(100 *seq(7,100,14), zoo::as.Date(seq(7,100,14))),
                      dailyish = xts::xts(10 * seq(9,107,1),  zoo::as.Date(seq(9,107,1))),
                      max_width = 30) { # observations back # e.g. days # possibly 2 observations found ( one every 14 days )
  
  
  dailyish_index <- zoo::index(dailyish) 
  oftenish_index <- zoo::index(oftenish)
  zoo::rollapply(data = dailyish_index
                 , width = max_width + 1 
                 , FUN = function(x,y) { 
                   any_back_found <- rev(y[y <= max(x)])[1]
                   if(is.na(any_back_found)) return(NA) # all ys younger than 'any x'
                   close_back_found <- if( as.numeric( zoo::as.Date(max(x)) ) - as.numeric( zoo::as.Date(any_back_found) ) <=  max_width  ) { any_back_found  } else { NA }
                   return(close_back_found)
                 }
                 , partial = TRUE,  align = "right", y = oftenish_index ) -> # [1]  7 21 35 49 63 77 91
    out
  
  z <-  as.numeric( zoo::index(dailyish) ) - as.numeric( zoo::as.Date(out) )
  return(z)
  
}

# > zoo_delay(xts::xts(NULL,zoo::as.Date(1:10)),xts::xts(NULL,zoo::as.Date(6:15)))
# [1] 0 0 0 0 0 1 2 3 4 5
# 
# > zoo_delay(xts::xts(NULL,zoo::as.Date(6:15)),xts::xts(NULL,zoo::as.Date(1:10)))
# [1] NA NA NA NA NA  0  0  0  0  0
# 
# > zoo_delay(max_width = 1) # only today or only yesterday
# [1] NA NA NA NA NA NA NA NA NA NA NA NA  0  1 NA NA NA NA NA NA NA NA NA NA NA NA  0  1 NA NA NA NA NA NA NA
# [36] NA NA NA NA NA  0  1 NA NA NA NA NA NA NA NA NA NA NA NA  0  1 NA NA NA NA NA NA NA NA NA NA NA NA  0  1
# [71] NA NA NA NA NA NA NA NA NA NA NA NA  0  1 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# 
# > zoo_delay(max_width = 0) # only today
# [1] NA NA NA NA NA NA NA NA NA NA NA NA  0 NA NA NA NA NA NA NA NA NA NA NA NA NA  0 NA NA NA NA NA NA NA NA
# [36] NA NA NA NA NA  0 NA NA NA NA NA NA NA NA NA NA NA NA NA  0 NA NA NA NA NA NA NA NA NA NA NA NA NA  0 NA
# [71] NA NA NA NA NA NA NA NA NA NA NA NA  0 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# 
# > zoo_delay()
# [1]  2  3  4  5  6  7  8  9 10 11 12 13  0  1  2  3  4  5  6  7  8  9 10 11 12 13  0  1  2  3  4  5  6  7  8
# [36]  9 10 11 12 13  0  1  2  3  4  5  6  7  8  9 10 11 12 13  0  1  2  3  4  5  6  7  8  9 10 11 12 13  0  1
# [71]  2  3  4  5  6  7  8  9 10 11 12 13  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
# 
# > zoo_delay(max_width = 5)
# [1]  2  3  4  5 NA NA NA NA NA NA NA NA  0  1  2  3  4  5 NA NA NA NA NA NA NA NA  0  1  2  3  4  5 NA NA NA
# [36] NA NA NA NA NA  0  1  2  3  4  5 NA NA NA NA NA NA NA NA  0  1  2  3  4  5 NA NA NA NA NA NA NA NA  0  1
# [71]  2  3  4  5 NA NA NA NA NA NA NA NA  0  1  2  3  4  5 NA NA NA NA NA NA NA NA NA NA NA


# VERSION 2 - VARIANT 2

# INTDSRUSM193N # LAST KNOWN GOOD TO BE USED WITH FRED ( NON-SOCASTIC )

xts_treat_na_all_methods_slope <- function(X, NAdelayed_max_width = 57, slope_change_col = "NAedLOCF", slope_change_width = 57) {
  
  X_colname <- colnames(X)[1] # SHOULD BE JUST ONE COLUMN
  
  # seq # ## S3 method for class 'Date'
  X__NAed <- xts::merge.xts( X, xts::xts(NULL, seq( from = zoo::as.Date(head(index(X),1)), to = zoo::as.Date(tail(index(X),1)), by = 1 ) ) ) 
  colnames(X__NAed) <- paste0(X_colname,'__NAed')
  
  v__isNA_flag <- ifelse(is.na(coredata(X__NAed)[,1]), 2.0, 1.0 ) # . . . convert to a factor later ( just before sending to MODELER ) . . .
  X__isNA_flag <- xts(v__isNA_flag, index(X__NAed))
  colnames(X__isNA_flag) <- paste0(X_colname,'__isNA_flag')
  
  # in zoo test 'out' is much better than 'not out' BUT exact data same data is returned with the NA gap is ONLY 2
  # cbind( X__NAedApproxed  = head( zoo::na.approx( X__NAed ), 30 ), head( zoo::na.approx( X__NAed, xout = zoo::as.Date(index(X__NAed))), 30 ) , head( X__NAed, 30 )  )
  
  # so, from the comment above, do this one
  X__NAedApproxed <- zoo::na.approx( X__NAed, xout = zoo::as.Date(index(X__NAed)), na.rm = FALSE ) 
  colnames(X__NAedApproxed) <- paste0(X_colname,'__NAedApproxed')
  
  X__NAedLOCF     <- zoo::na.locf  ( X__NAed,                                      na.rm = FALSE ) 
  colnames(X__NAedLOCF)     <- paste0(X_colname,'__NAedLOCF'    )
  
  # originally I programmed X to be sparse and X__NAed to be detailed
  # but since I pretreated X by merging it with every date in many decades
  #  then X is no longer sparse
  # the 'looking for 'not nulls' below, returns the original intention of the function
  
  v__NAdelayed <- zoo_delay(X[!is.na(coredata(X)),1], 
                            X__NAed, 
                            max_width = NAdelayed_max_width )
  # v__NAdelayed <- zoo_delay(X, X__NAed, max_width = NAdelayed_max_width )
  X__NAdelayed <- xts( v__NAdelayed, index(X__NAed))
  colnames(X__NAdelayed) <- paste0(X_colname,'__NAdelayed')
  
  # X_to_be_slope_change_col <- get(paste0("X__", slope_change_col))
  # v_slope_changed <- zoo_slope_change_num(coredata(X_to_be_slope_change_col)[,1],  max_width = slope_change_width) 
  # X_slope_changed <- xts(v_slope_changed, index(X__NAed))
  # colnames(X_slope_changed) <- paste0(X_colname,'__',slope_change_col,'__','slope_changed')
  #
  # # them all
  # z <- xts::merge.xts(X__NAed, X__isNA_flag, X__NAedApproxed, X__NAedLOCF, X_slope_changed, X__NAdelayed)
  
  X__to_be_slope_change_col <- get(paste0("X__", slope_change_col))
  v__slope_changed <- zoo_slope_change_num(coredata(X__to_be_slope_change_col)[,1],  max_width = slope_change_width) 
  X__slope_changed <- xts(v__slope_changed, index(X__NAed))
  colnames(X__slope_changed) <- paste0(X_colname,'__',slope_change_col,'__','slope_changed')
  
  # them all
  z <- xts::merge.xts(X__NAed, X__isNA_flag, X__NAedApproxed, X__NAedLOCF, X__slope_changed, X__NAdelayed)
  
  return(z)
  
}



# VERSION 2 - VARIANT 4 - DONE - LAST KNOW WORKING VARIANT FOR 'GSPC' [ AND MARKET DATA ]

# X would be e.g.  GSPC of 'FRED MONTHLY'                                # ONLY USES NAedApproxed
xts_treat_na_all_methods_lagsma <- function(X, NAdelayed_max_width = 57, i_X_micro_change = 1, lagsma_change_col = "NAedApproxed", lagsma_change_width = 200) {
  
  X_colname <- colnames(X)[1] # SHOULD BE JUST ONE COLUMN
  
  # seq # ## S3 method for class 'Date'
  X__NAed <- xts::merge.xts( X, xts::xts(NULL, seq( from = zoo::as.Date(head(index(X),1)), to = zoo::as.Date(tail(index(X),1)), by = 1 ) ) ) 
  colnames(X__NAed) <- paste0(X_colname,'__NAed')
  
  v__isNA_flag <- ifelse(is.na(coredata(X__NAed)[,1]), 2.0, 1.0 ) # . . . convert to a factor later ( just before sending to MODELER ) . . .
  X__isNA_flag <- xts(v__isNA_flag, index(X__NAed))
  colnames(X__isNA_flag) <- paste0(X_colname,'__isNA_flag')
  
  # in zoo test 'out' is much better than 'not out' BUT exact data same data is returned with the NA gap is ONLY 2
  # cbind( X__NAedApproxed  = head( zoo::na.approx( X__NAed ), 30 ), head( zoo::na.approx( X__NAed, xout = zoo::as.Date(index(X__NAed))), 30 ) , head( X__NAed, 30 )  )
  
  # so, from the comment above, do this one
  X__NAedApproxed <- zoo::na.approx( X__NAed, xout = zoo::as.Date(index(X__NAed)), na.rm = FALSE ) 
  colnames(X__NAedApproxed) <- paste0(X_colname,'__NAedApproxed')
  
  # zoo::na.approx does not do backard/forward( tail ( non-leadings) ) approximations 
  #   that should have replaced NA there.
  # but TTR::SMA requires non-NAs in the tail( non-leadings)
  #   so I just have to estimate them.
  X__NAedApproxed <- zoo::na.locf(X__NAedApproxed,                                 na.rm = FALSE )
  
  # price returns # ( future - today ) / abs( today )
  X__i_X_micro_change <- ( lag(X__NAedApproxed, -1 * i_X_micro_change ) - lag(X__NAedApproxed,0) ) / abs( lag(X__NAedApproxed,0) )
  colnames(X__i_X_micro_change) <- paste0(X_colname,'__i_X_micro_change',i_X_micro_change)
  
  X__i_X_micro_change_isgain <- ifelse(  X__i_X_micro_change > 0, 2.0, 1.0 )
  colnames(X__i_X_micro_change_isgain) <- paste0(X_colname,'__i_X_micro_change',i_X_micro_change, '_isgain')
  
  X__NAedLOCF     <- zoo::na.locf  ( X__NAed,                                      na.rm = FALSE ) 
  colnames(X__NAedLOCF)     <- paste0(X_colname,'__NAedLOCF'    )
  
  # originally I programmed X to be sparse and X__NAed to be detailed
  # but since I pretreated X by merging it with every date in many decades
  #  then X is no longer sparse
  # the 'looking for 'not nulls' below, returns the original intention of the function

  v__NAdelayed <- zoo_delay(X[!is.na(coredata(X)),1], 
                            X__NAed, 
                            max_width = NAdelayed_max_width )
  # v__NAdelayed <- zoo_delay(X, X__NAed, max_width = NAdelayed_max_width )
  X__NAdelayed <- xts( v__NAdelayed, index(X__NAed))
  colnames(X__NAdelayed) <- paste0(X_colname,'__NAdelayed')
  
  X__to_be_sma_col <- get(paste0("X__", lagsma_change_col))
  
  v__sma2 <- SMA(coredata(X__to_be_sma_col)[,1], n = 2)
  X__sma2 <- xts(v__sma2, index(X__NAed))
  colnames(X__sma2)   <- paste0(X_colname,'__',lagsma_change_col,'__','sma',2)
  # NOT PRINTED TO OUTPUT ( USED IN MATH BELOW )
  
  v__smaN  <- SMA(coredata(X__to_be_sma_col)[,1], n = lagsma_change_width) # could have done on a '1 col xts object'
  X__smaN  <- xts(v__smaN, index(X__NAed))
  colnames(X__smaN)   <- paste0(X_colname,'__',lagsma_change_col,'__','sma',lagsma_change_width)
  
  X__lagsma <- ifelse(X__sma2 - X__smaN > 0, 2.0, 1.0 ) # to be made into a factor later
  colnames(X__lagsma) <- paste0(X_colname,'__',lagsma_change_col,'__','sma',2,                  '_above_lagsma',lagsma_change_width)
  
  # them all
  z <- xts::merge.xts(X__NAed, X__isNA_flag, X__NAedApproxed, X__i_X_micro_change, X__i_X_micro_change_isgain, X__NAedLOCF, X__smaN, X__lagsma, X__NAdelayed)
  
  return(z)
  
}





Givens_Siegel_Faber_Johnson <- function(new_data = FALSE, new_derived_data = FALSE, make_new_model = FALSE, final_date_str = "2015-01-01") {
  
  ops <- options()
  options(error = recover)
  Sys.setenv(TZ="UTC")
  
  require(TTR)
  require(quantmod)
  
  # ^GSPC: Summary for S&P 500- Yahoo! Finance
  # https://r-forge.r-project.org/scm/viewvc.php/pkg/quantstrat/demo/faber.R?view=markup&root=blotter
  
  if(new_data == TRUE ) {
    
    # if I overshOot the past on Yahoo, thin I only return 7 years of data
    Sys.sleep(0.67)
    GSPC <-  getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE , 
                        from = as.Date("1960-01-04"), to = as.Date(final_date_str))
    save("GSPC", file = "GSPC.RData")
  
    # discount rate    
    Sys.sleep(0.67)
    INTDSRUSM193N <- getSymbols('INTDSRUSM193N',src='FRED', auto.assign = FALSE)
    save("INTDSRUSM193N", file = "INTDSRUSM193N.RData")
    
    # federal funds rate ( CURRENLY NOT USED )  
    # Sys.sleep(0.67)
    # FEDFUNDS <- getSymbols('FEDFUNDS',src='FRED')
    # save("FEDFUNDS", file = "FEDFUNDS.RData", auto.assign = FALSE)

    # manufacturing expansion/contraction
    Sys.sleep(0.67)
    NAPM <- getSymbols('NAPM',src='FRED', auto.assign = FALSE)
    save("NAPM", file = "NAPM.RData")
    
    # total capacity utilization
    Sys.sleep(0.67)
    TCU <- getSymbols('TCU',src='FRED', auto.assign = FALSE)  
    save("TCU", file = "TCU.RData")
    
    # NOTE, I could have 10 year bonds
    # I would also need to treat it by tracking the 5 year SMA(correl) to GSPC
    
    # What does Bernard Boule Say?
  
  }
  
  if(new_data == FALSE ) {
    
    load(file = "GSPC.RData")
      
    # discount rate
    load(file = 'INTDSRUSM193N.RData')
    
    # federal funds rate ( CURRENLY NOT USED )   
    # load(file = "FEDFUNDS.RData")
    
    # manufacturing expansion/contraction
    load(file = 'NAPM.RData')
    
    # total capacity utilization
    load(file = 'TCU.RData')
    
    # NOTE, I could have 10 year bonds
    # I would also need to treat it by tracking the 5 year SMA(correl) to GSPC
    
    # What does Bernard Boule Say?
    
  }
  
  if( new_derived_data == TRUE) {
    # master Date index
    MSTRIDX <- xts(NULL, seq(as.Date("1950-01-01"),as.Date(final_date_str), by = 1))
    save("MSTRIDX", file = "MSTRIDX.RData")
    
    # FRED ( not volitile )  
    INTDSRUSM193N_PLUS <- merge(MSTRIDX,INTDSRUSM193N, join = "left")
    NTDSRUSM193N_PLUS <- xts_treat_na_all_methods_slope( 
      INTDSRUSM193N_PLUS, slope_change_width = 57 ) # peopls reacion time
    
    # FRED ( approximately known )
    NAPM_PLUS          <- merge(MSTRIDX,NAPM         , join = "left")
    NAPM_PLUS <- xts_treat_na_all_methods_lagsma(
      NAPM_PLUS, lagsma_change_width = 57) # peopls reacion time
    save("NAPM_PLUS", file = "NAPM_PLUS.RData")
    
    TCU_PLUS           <- merge(MSTRIDX,TCU          , join = "left")
    TCU_PLUS <- xts_treat_na_all_methods_lagsma(
      TCU_PLUS, lagsma_change_width = 57) # peopls reacion time
    save("TCU_PLUS", file = "TCU_PLUS.RData")
    
    # Market data ( volitile ( but exactly known ) )
    GSPC_PLUS                  <- merge(MSTRIDX,GSPC, join = "left")
    GSPC_PLUS_CLOSE            <- GSPC_PLUS[,"GSPC.Close"]
    colnames(GSPC_PLUS_CLOSE)  <- 'GSPC_CLOSE'
    
    GSPC_PLUS_CLOSE <- xts_treat_na_all_methods_lagsma(
      GSPC_PLUS_CLOSE, lagsma_change_width = 270) # Faber 9 months SMA
    save("GSPC_PLUS_CLOSE", file = "GSPC_PLUS_CLOSE.RData")
    
    # quantitive easing ( not volitle )
    
    rbind( 
      
      # QE1
      
      xts(  seq(3,3,length.out = zoo::as.Date('2010-03-31') - zoo::as.Date('2009-01-14') + 1 ) , 
            seq( zoo::as.Date('2009-01-14'), zoo::as.Date('2010-03-31'), by = 1)   
      ),
      
      # QE2
      
      xts(  seq(3,3,length.out = zoo::as.Date('2014-10-29') - zoo::as.Date('2010-11-03') + 1 ) , 
            seq( zoo::as.Date('2010-11-03'), zoo::as.Date('2014-10-29'), by = 1)   
      )
      
    ) -> X__qe_in_action 
    colnames(X__qe_in_action) <- "USMONPOL__qe_in_action"
    merge( xts(NULL,index(MSTRIDX) ), X__qe_in_action ) ->  X__qe_in_action
    save("X__qe_in_action", file = "X__qe_in_action.RData")
    
    # discount rate will also hold the fed QE actions
    NTDSRUSM193N_PLUS <- merge(NTDSRUSM193N_PLUS, X__qe_in_action)
  
    # if QE is in progress, the fed_direction is always EXPANDING (3)
    NTDSRUSM193N_PLUS <- within.xts( NTDSRUSM193N_PLUS, { USMONPOL__fed_direction <- ifelse( USMONPOL__qe_in_action == 3, 3, NA) } ) 
    # If the fed directin is not in progress, the direction is determined by the slope of the discount rate (3,2,1)
    NTDSRUSM193N_PLUS <- within.xts( NTDSRUSM193N_PLUS, { USMONPOL__fed_direction <- ifelse( is.na(USMONPOL__fed_direction),  INTDSRUSM193N__NAedLOCF__slope_changed, USMONPOL__fed_direction) } ) 
    # otherwise, the fed direction ( no expansion ) is just neutral (2)
    NTDSRUSM193N_PLUS <- within.xts( NTDSRUSM193N_PLUS, { USMONPOL__qe_in_action  <- ifelse( is.na(USMONPOL__qe_in_action ),                                      2 , USMONPOL__qe_in_action ) } ) 
    save("NTDSRUSM193N_PLUS", file = "NTDSRUSM193N_PLUS.RData")
    
    WORLD <- merge(GSPC_PLUS_CLOSE,NTDSRUSM193N_PLUS,NAPM_PLUS, TCU_PLUS)
    save("WORLD", file = "WORLD.RData")
    
    bookmarkhere <- 1
    
  }
  
  if( new_derived_data == FALSE) {
   
    load(file = "MSTRIDX.RData")
    load(file = "X__qe_in_action.RData")
    
    load(file = "GSPC_PLUS_CLOSE.RData")
    load(file = "NTDSRUSM193N_PLUS.RData")
    
    load(file = "NAPM_PLUS.RData")
    load(file = "TCU_PLUS.RData")
    
    load(file = "TCU_PLUS.RData")
    
    load(file = "WORLD.RData")
    
  }
  
  if( make_new_model == TRUE ) {
  
    # MAY!? want to put last
    PERFECTWORLD <- WORLD[,!grepl(".*__NAed$", colnames(WORLD)),drop = FALSE]
    PERFECTWORLD <- na.trim( PERFECTWORLD )
    
    within( data.frame(cbind( timeindex = as.numeric(zoo::as.Date(index(PERFECTWORLD))), PERFECTWORLD ), stringsAsFactors = FALSE), {
      
      GSPC_CLOSE__i_X_micro_change_isgain_f2                <- factor( GSPC_CLOSE__i_X_micro_change1_isgain, levels = c('1','2'), labels = c('terrible','great'))
      
      
      GSPC_CLOSE__isNA_flag_f2                              <- factor( GSPC_CLOSE__isNA_flag , levels = c('1','2'), labels = c('not_na','is_na'))
      GSPC_CLOSE__NAedApproxed__sma2_above_lagsma270_f2     <- factor( GSPC_CLOSE__NAedApproxed__sma2_above_lagsma270 , levels = c('1','2'), labels = c('terrible','great'))
      
      INTDSRUSM193N__isNA_flag_f2                           <- factor( INTDSRUSM193N__isNA_flag , levels = c('1','2'), labels = c('not_na','is_na'))
      INTDSRUSM193N__NAedLOCF__slope_changed_f3             <- factor( INTDSRUSM193N__NAedLOCF__slope_changed , levels = c('1','2','3'), labels = c('bad','ok','good'))
      
      USMONPOL__fed_direction_f3                            <- factor( USMONPOL__fed_direction , levels = c('1','2','3'), labels = c('bad','ok','good'))
      
      TCU__isNA_flag_f2                                     <- factor( TCU__isNA_flag , levels = c('1','2'), labels = c('not_na','is_na'))
      TCU__NAedApproxed__sma2_above_lagsma57_f2             <- factor( TCU__NAedApproxed__sma2_above_lagsma57, levels = c('1','2'), labels = c('terrible','great'))
      
      
      NAPM__isNA_flag_f2                                    <- factor( NAPM__isNA_flag , levels = c('1','2'), labels = c('not_na','is_na'))
      NAPM__NAedApproxed_above_50pct_f2                     <- factor( ifelse( NAPM__NAedApproxed > 50, 2, 1 ), levels = c('1','2'), labels = c('terrible','great')) 
      NAPM__NAedApproxed__sma2_above_lagsma57_f2            <- factor( NAPM__NAedApproxed__sma2_above_lagsma57, levels = c('1','2'), labels = c('terrible','great')) 
      
    }) ->  PERFECTWORLDFACTORED
    
    
    PERFECTWORLDFACTORED[, c(
      'GSPC_CLOSE__i_X_micro_change_isgain_f2',
      
      'GSPC_CLOSE__NAdelayed',
      'GSPC_CLOSE__isNA_flag_f2',
      'GSPC_CLOSE__NAedApproxed__sma2_above_lagsma270_f2',
      
      'INTDSRUSM193N__NAdelayed',
      'INTDSRUSM193N__isNA_flag_f2',
      'INTDSRUSM193N__NAedLOCF__slope_changed_f3',
      
      'USMONPOL__fed_direction_f3',
      
      'TCU__NAdelayed',
      'TCU__isNA_flag_f2',
      'TCU__NAedApproxed',
      'TCU__NAedApproxed__sma57',
      'TCU__NAedApproxed__sma2_above_lagsma57_f2',
      
      'NAPM__NAdelayed',
      'NAPM__isNA_flag_f2',
      'NAPM__NAedApproxed',
      'NAPM__NAedApproxed_above_50pct_f2', 
      'NAPM__NAedApproxed__sma57',
      'NAPM__NAedApproxed__sma2_above_lagsma57_f2' 
      
    ) ] -> PERFECTWORLDFACTOREDMODEL
    
    save(PERFECTWORLDFACTOREDMODEL, file = "PERFECTWORLDFACTOREDMODEL.RData")
    
  }
  
  if( make_new_model == FALSE ) {
    
    load( file = "PERFECTWORLDFACTOREDMODEL.RData")
    
  }
  
  options(ops)
  
  return(1)
  
}
# 


# rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
# debugSource(paste0(getwd(),'/','Givens_Siegel_Faber_Johnson.R'))
# Givens_Siegel_Faber_Johnson()
##  Givens_Siegel_Faber_Johnson(new_data = TRUE, final_date_str = as.character( Sys.Date()))
# Givens_Siegel_Faber_Johnson(new_data = TRUE, new_derived_data = TRUE)
# Givens_Siegel_Faber_Johnson(                 new_derived_data = TRUE ) 
# Givens_Siegel_Faber_Johnson(                                         make_new_model = TRUE)


