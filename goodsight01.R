

# goodsight01.R

# Note: sinew::makeOxygen helped generate documentation template

# DESCRIPTION
# 
# Package: expandxts
# Version: 0.0.0.9000
# Title: Real World Xts Assistance functions
# Description: xts helper functions.
#   In general, unless an exception is noted otherwise,
#   input x meant to be an xts object with index class of Date
# Authors@R: Andre Mikulec <Andre_Mikulec@Hotmail.com> [aut, cre]
# License: MIT + file LICENSE
# Encoding: UTF-8
# LazyData: true
# ByteCompile: true
# Suggests: 
#     testthat
# Roxygen: list(markdown = TRUE)
# RoxygenNote: 6.0.1

# NAMESPACE
# exportPattern("^[^\\.]")

# example data
# 
require(quantmod) # puts in path zoo, then xts
data(sample_matrix)  # from package xts

#' @title investment data
#' @description OHLC data
#' @format A data frame with 180 rows and 4 variables:
#' \describe{
#'   \item{\code{Open}}{double }
#'   \item{\code{High}}{double }
#'   \item{\code{Low}}{double }
#'   \item{\code{Close}}{double}
#'}
#' @details see help("sample_matrix", package = "xts")
#' @examples
#' \dontrun{
#' # require(xts)
#' # data(sample_matrix)
#' # str(sample_matrix)
#' #  num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
#' #  - attr(*, "dimnames")=List of 2
#' #   ..$ : chr [1:180] "2007-01-02" "2007-01-03" "2007-01-04" "2007-01-05" ...
#' #   ..$ : chr [1:4] "Open" "High" "Low" "Close"
#' # sample_xts <- as.xts(sample_matrix)
#' # str(sample_xts)
#' # An 'xts' object on 2007-01-02/2007-06-30 containing:
#' #   Data: num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
#' #  - attr(*, "dimnames")=List of 2
#' #   ..$ : NULL
#' #   ..$ : chr [1:4] "Open" "High" "Low" "Close"
#' #   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#' #   xts Attributes:
#' #  NULL
#' }
#' @rdname sample_xts
#' @export
"sample_xts"
sample_xts <- as.xts(sample_matrix) 
# rm(sample_matrix) # TEMP for creation


#' @title unemployment rate
#' @description U.S. unemployment rate from the St. Louis FRED
#' @format A data frame with 3 rows and 1 variables:
#' \describe{
#'   \item{\code{UNRATE}}{double percent of workers w/o jobs}
#'}
#' @details percent expressed
#' @examples
#' \dontrun{
#' # require(quantmod)
#' # unrate <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1948-03-01"]
#' # An 'xts' object on 1948-01-01/1948-03-01 containing:
#' # str(unrate)
#' # Data: num [1:3, 1] 3.4 3.8 4
#' #  - attr(*, "dimnames")=List of 2
#' #   ..$ : NULL
#' #   ..$ : chr "UNRATE"
#' #   Indexed by objects of class: [Date] TZ: UTC
#' #   xts Attributes:
#' # List of 2
#' #  $ src    : chr "FRED"
#' #  $ updated: POSIXct[1:1], format: "2017-12-16 12:48:06"
#' }
#' @rdname unrate
#' @export
"unrate"
unrate <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1948-03-01"]



#' @title unemployment rate in 1948 and 1949
#' @description U.S. unemployment rate from the St.Louis FRED
#' @format A data frame with 3 rows and 1 variables:
#' \describe{
#'   \item{\code{UNRATE}}{double percent of workers w/o jobs}
#'}
#' @details percent expressed
#' @examples
#' \dontrun{
#' # require(quantmod)
#' # unrate_40s <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1949-03-01"]
#' # An 'xts' object on 1948-01-01/1949-03-01 containing:
#' #   Data: num [1:15, 1] 3.4 3.8 4 3.9 3.5 3.6 3.6 3.9 3.8 3.7 ...
#' #  - attr(*, "dimnames")=List of 2
#' #   ..$ : NULL
#' #   ..$ : chr "UNRATE"
#' #   Indexed by objects of class: [Date] TZ: UTC
#' #   xts Attributes:
#' # List of 2
#' #  $ src    : chr "FRED"
#' #  $ updated: POSIXct[1:1], format: "2017-12-16 12:48:07"
#' }
#' @rdname (unrate_40s
#' @export
"unrate_40s"
unrate_40s <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1949-03-01"]


#' @title international business machines stock price data
#' @description data from St. Louis FRED
#' @format A data frame with 7 rows and 6 variables:
#' \describe{
#'   \item{\code{IBM.Open}}{double }
#'   \item{\code{IBM.High}}{double }
#'   \item{\code{IBM.Low}}{double }
#'   \item{\code{IBM.Close}}{double }
#'   \item{\code{IBM.Volume}}{double }
#'   \item{\code{IBM.Adjusted}}{double }
#'}
#' @details price
#' @examples
#' \dontrun{
#' # require(quantmod)
#' # ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#' # An 'xts' object on 1970-01-02/1970-01-12 containing:
#' #   Data: num [1:7, 1:6] 18.2 18.3 18.4 18.4 18.4 ...
#' #  - attr(*, "dimnames")=List of 2
#' #   ..$ : NULL
#' #   ..$ : chr [1:6] "IBM.Open" "IBM.High" "IBM.Low" "IBM.Close" ...
#' #   Indexed by objects of class: [Date] TZ: UTC
#' #   xts Attributes:
#' # List of 2
#' #  $ src    : chr "yahoo"
#' #  $ updated: POSIXct[1:1], format: "2017-12-16 12:48:09"
#' }
#' @rdname ibm 
#' @export
"ibm"
ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)


#' @title international business machines stock price data
#' @description data from St. Louis FRED
#' @format A data frame with 7 rows and 6 variables:
#' \describe{
#'   \item{\code{IBM.Open}}{double }
#'   \item{\code{IBM.High}}{double }
#'   \item{\code{IBM.Low}}{double }
#'   \item{\code{IBM.Close}}{double }
#'   \item{\code{IBM.Volume}}{double }
#'   \item{\code{IBM.Adjusted}}{double }
#'}
#' @details price, has two values(cells) of data removed
#' @examples
#' \dontrun{
#' # require(quantmod)
#' # ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#' # ibm_missing_data <- local({ t <- ibm; t[2:3,1] <- NA_real_; t})
#' # str(ibm_missing_data)
#' # An 'xts' object on 1970-01-02/1970-01-12 containing:
#' #   Data: num [1:7, 1:6] 18.2 NA NA 18.4 18.4 ...
#' #  - attr(*, "dimnames")=List of 2
#' #   ..$ : NULL
#' #   ..$ : chr [1:6] "IBM.Open" "IBM.High" "IBM.Low" "IBM.Close" ...
#' #   Indexed by objects of class: [Date] TZ: UTC
#' #   xts Attributes:
#' # List of 2
#' #  $ src    : chr "yahoo"
#' #  $ updated: POSIXct[1:1], format: "2017-12-16 12:48:09"
#' }
#' @rdname ibm_missing_data
#' @export
"ibm_missing_data"
ibm_missing_data <- local({ t <- ibm; t[2:3,1] <- NA_real_; t})



#' @title last observation carried forward
#' @description carry forward up through 'n' observations
#' @param x xts observations
#' @param n number of observations to carry forward through, Default: NULL
#' @return modified xts object
#' @details input is one single column xts only or an unclassed numeric vector
#' @examples
#' \dontrun{
#' #  vector input
#' #  get_na_locfl( c(101,NA,NA,NA,102,NA,NA), n = 2)
#' #  [1] 101 101 101  NA 102 102 102
#' # 
#' #  require(xts)
#' #  xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7)))
#' #             [,1]
#' # 1970-01-11  101
#' # 1970-01-21   NA
#' # 1970-01-31   NA
#' # 1970-02-10   NA
#' # 1970-02-20  102
#' # 1970-03-02   NA
#' # 1970-03-12   NA
#' # 
#' # get_na_locfl(xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7))), n = 2)
#' #         na_locfl
#' # 1970-01-11   101
#' # 1970-01-21   101
#' # 1970-01-31   101
#' # 1970-02-10    NA
#' # 1970-02-20   102
#' # 1970-03-02   102
#' # 1970-03-12   102
#' }
#' @rdname get_na_locfl
#' @export
get_na_locfl <- function(x, n = NULL) {

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_na_locfl_xts, only ONE column is allowed.")
  
  require(xts)
  # uses package zoo function rollapply.zoo, 
  # uses package DescTools function DoCall

  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(  xts::try.xts(x_orig) , silent = T)

  x_try_zoo_success <- FALSE
  if(any(class(x_try.xts) %in% "try-error")) { 
    x_try_zoo <- try(zoo::as.zoo(x), silent = T)
    if(any(class(x_try_zoo) %in% "try-error"  )) {
      # x_orig # ASSUMING I CAN *STILL* DO SOMETHING WITH THIS
      stop("get_na_locfl: can not make a zoo object")
    } else {
      x_try_zoo_success <- TRUE
      x_try_zoo
    }
  } else { 
    x_try.xts_success <- TRUE
    x_try.xts 
  } -> x
  
  # na.locf from the 'company information' last quarterly report
  # but only carry forward a max of '2 periods'(THAT). XOR '4 periods'
  # After THAT. NAs follow 

  # NOT USING xts:::rollapply.xts 
  # NEED "partial = TRUE" support ( to handle early smaller windows )
  #   so using zoo:::rollapply.zoo
      
  # I WANT to USE the ZOO method ( no dispatch )
  zoo:::rollapply.zoo(zoo::as.zoo(x), width = list(seq(-1*n, 0)),  FUN = function(xx) {
    
      # if the 'element of interest'(last) is 'NA'
      # and in the width range, there exists at least one other element that  is 'not NA'
      # then about the range 'last observation carry forward'
      #   to return the 'new element of interest' ( that will now have a "non-NA' value)
      # othewise ( the entire range stays all 'NA's )
      #   return just the element of interest ( will be 'NA' )   
      
      if( is.na(xx[NROW(xx)]) && (max(as.integer(!is.na(xx))) > 0) ) { 
        zoo::na.locf(xx) -> y
        return(y[NROW(y)]) 
      } else {
        return(xx[NROW(xx)])
      }
    } , partial = 1 # min window size for partial computations 
  )  -> x_result
  
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } else {
    if(exists(paste0("as.", c_orig))) {
      DescTools::DoCall(paste0("as.", c_orig), list(x_result)) 
    } else {
      x_result
    }
  } -> x_result
  
  if(!x_try.xts_success && x_try_zoo_success) {
    if(exists(paste0("as.", c_orig))) {
      DescTools::DoCall(paste0("as.", c_orig), list(x_result)) 
    } else {
      x_result # THE BEST THAT I CAN DO
    }
  } -> x_result
  
  if(!is.null(dim(x_result))) colnames(x_result) <- "na_locfl"
  
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))

  locfl <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(locfl)
  
  # ORIG FROM
  # https://github.com/AndreMikulec/expressions/blob/8a910454ea590a30878e97b18d5e9dbe45a9d4fb/main-foresight3-999.R#L2287

}



#' @title get percent change
#' @description get xts percent change between two different times
#' @param x xts objects
#' @param n change in time from observations from the pasts
#'          percent change from the past through NOW
#'          only ONE lag is allowed: so "n" must be a vector of size: 1
#' @param to_future TRUE/FALSE change from the future(FALSE) instead of the past(NULL/FALSE), Default: NULL(FALSE)
#' @return modifed xts objects
#' @details input is one single column xts object only
#' @examples
#' \dontrun{
#' # require(xts)
#' # data(sample_matrix)
#' # sample_xts <- as.xts(sample_matrix)
#' # head(sample_xts[,"Open"],4) # x
#' #                          Open
#' # 2007-01-02 50.039781911546299
#' # 2007-01-03 50.230496197795397
#' # 2007-01-04 50.420955209067003
#' # 2007-01-05 50.373468054328498
#' # 
#' # get_pctchg_xts(head(sample_xts[,"Open"],4), n = 1)
#' #                           pctchg
#' # 2007-01-02                    NA
#' # 2007-01-03  0.381125334611203126 # res
#' # 2007-01-04  0.379170077320410137
#' # 2007-01-05 -0.094181386571521211
#' # 
#' # #( 50.230496197795397 - 50.039781911546299 ) / abs(50.039781911546299) * 100
#' # [1] 0.38112533461120313
#' # 
#' # get_pctchg_xts(head(sample_xts[,"Open"],4), n = 1, to_future = TRUE )
#' #                           pctchg
#' # 2007-01-02 -0.381125334611203126
#' # 2007-01-03 -0.379170077320410137 # res
#' # 2007-01-04  0.094181386571521211
#' # 2007-01-05                    NA
#' # 
#' # ( 50.420955209067003 -50.230496197795397) / abs( 50.230496197795397 ) * 100
#' # [1] 0.37917007732041014
#' # 
#' # # NOT(which <= NROW(x))
#' # get_pctchg_xts(head(sample_xts[,"Open"],4), n = 5, to_future = TRUE )
#' # 
#' #          pctchg
#' # 2007-01-02   NA
#' # 2007-01-03   NA
#' # 2007-01-04   NA
#' # 2007-01-05   NA
#' }
#' @rdname get_pctchg_xts
#' @export
get_pctchg_xts <- function(x, n, to_future = NULL) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_pctchg_xts, only ONE column is allowed.")
  
  require(xts) 
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_pctchg_xts can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

   # normal backwards
  if(is.null(to_future) || to_future == FALSE) { to_future = FALSE;   lag_direction <- 1 }

  if(to_future == TRUE) lag_direction <- -1
  
  # xts::lag.xts rules
  if( n <= NROW(x) ) {
    # from past
    if(!to_future) x_result <- ( x - xts::lag.xts(x, n * lag_direction) )/ abs(xts::lag.xts(x, n * lag_direction)) * 100 * lag_direction
    # to future
    if( to_future) x_result <- ( xts::lag.xts(x, n * lag_direction) - x )/ abs(                                    x) * 100 * lag_direction
  } else {
    x[,] <- NA_real_
    x_result <- x
  }

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "pctchg"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  pctchg_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(pctchg_xts)
} 



#' @title get a recent xts observation
#' @description get a recent xts observation deterimined by the higher of values(max)
#' @param x xts object
#' @param n number of 'backwards in time xts values' at observations to compare
#' @return modfiied xts object
#' @details input is one single column xts object only   
#'          function is meant to smooth data
#' @examples
#' \dontrun{
#' # require(xts)
#' # data(sample_matrix)
#' # sample_xts <- as.xts(sample_matrix)
#' # 
#' # head(sample_xts[,"Open"],4)
#' #                          Open
#' # 2007-01-02 50.039781911546299
#' # 2007-01-03 50.230496197795397
#' # 2007-01-04 50.420955209067003
#' # 2007-01-05 50.373468054328498
#' # 
#' # get_recent_max_xts(head(sample_xts[,"Open"],4), 2)
#' #                    recent_max
#' # 2007-01-02 50.039781911546299
#' # 2007-01-03 50.230496197795397
#' # 2007-01-04 50.420955209067003
#' # 2007-01-05 50.420955209067003
#' }
#' @rdname get_recent_max_xts
#' @export
get_recent_max_xts <- function(x, n) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_recent_max, only ONE column is allowed.")
  
  require(xts) 
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_pctchg_xts can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

  zoo::rollapply(as.zoo(x), width = n, partial = TRUE, align = "right", FUN = function(x, n) { 
    max(x, na.rm = TRUE)
  }, n = n) -> x_result

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "recent_max"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  pctchg_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(pctchg_xts)
} 



#' @title get a recent xts observation
#' @description get a recent xts observation deterimined by the higher of values(min)
#' @param x xts object
#' @param n number of 'backwards in time xts values' at observations to compare
#' @return modfiied xts object
#' @details input is one single column xts object only    
#'          function is meant to smooth data
#' @examples
#' \dontrun{
#' # require(xts)
#' # data(sample_matrix)
#' # sample_xts <- as.xts(sample_matrix)
#' # 
#' # head(sample_xts[,"Open"],4)
#' #                          Open
#' # 2007-01-02 50.039781911546299
#' # 2007-01-03 50.230496197795397
#' # 2007-01-04 50.420955209067003
#' # 2007-01-05 50.373468054328498
#' # 
#' # get_recent_min_xts(head(sample_xts[,"Open"],4), 2)
#' #                    recent_min
#' # 2007-01-02 50.039781911546299
#' # 2007-01-03 50.039781911546299
#' # 2007-01-04 50.230496197795397
#' # 2007-01-05 50.373468054328498
#' }
#' @rdname get_recent_min_xts
#' @export
get_recent_min_xts <- function(x, n) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(min.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_recent_min, only ONE column is allowed.")
  
  require(xts) 
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_pctchg_xts can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

  zoo::rollapply(as.zoo(x), width = n, partial = TRUE, align = "right", FUN = function(x, n) { 
    min(x, na.rm = TRUE)
  }, n = n) -> x_result

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "recent_min"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  pctchg_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(pctchg_xts)
} 



#' @title adjust as if held using a full year of time
#' @description multiple xts object by value
#' @param x xts object
#' @param n multiple ( e.g monthly = 12, daily = 260 )
#' @return modified xts object
#' @details input is one single column xts object only
#' @examples
#' \dontrun{
#' # head(sample_xts[,"Open"],4)
#' #                          Open
#' # 2007-01-02 50.039781911546299
#' # 2007-01-03 50.230496197795397
#' # 2007-01-04 50.420955209067003
#' # 2007-01-05 50.373468054328498
#' # 
#' # get_annualized_xts(head(sample_xts[,"Open"],4), 2)
#' #                    annualized
#' # 2007-01-02 100.07956382309260
#' # 2007-01-03 100.46099239559079
#' # 2007-01-04 100.84191041813401
#' # 2007-01-05 100.74693610865700
#' }
#' @rdname get_annualized_xts
#' @export
get_annualized_xts <- function(x, n) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(min.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_annualized_xts, only ONE column is allowed.")
  
  require(xts) 
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_pctchg_xts can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

  x_result <- x * n

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "annualized"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))

  annualized_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(annualized_xts)
} 



#' @title get the simple moving average
#' @description sma of xts observation
#' @param x xts object
#' @param n how many values to 'average over' 
#'          logical mininum is n = 2
#' @return modified xts object
#' @details input is one single column xts object only
#' @examples
#' \dontrun{
#' # head(sample_xts[,"Open"],4)
#' #                          Open
#' # 2007-01-02 50.039781911546299
#' # 2007-01-03 50.230496197795397
#' # 2007-01-04 50.420955209067003
#' # 2007-01-05 50.373468054328498
#' # 
#' # get_sma_xts(head(sample_xts[,"Open"],4), 2)
#' #                           sma
#' # 2007-01-02                 NA
#' # 2007-01-03 50.135139054670844
#' # 2007-01-04 50.325725703431203
#' # 2007-01-05 50.397211631697751
#' }
#' @rdname get_sma_xts
#' @export
get_sma_xts <- function(x, n) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(min.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_sma_xts, only ONE column is allowed.")
  
  require(xts) 
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_sma_xts can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

  zoo::rollapply(as.zoo(x), width = n, partial = TRUE, align = "right", FUN = function(x, n) { 
    # if not too short
    if(n <= length(x)) { mean(x, na.rm = FALSE) } else { NA_real_ }
  }, n = n) -> x_result

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "sma"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  sma_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(sma_xts)
} 



# single column xts only
#
# simple moving (true)sortino
# Sortino Ratio: Are you calculating it wrong?
# https://www.rcmalternatives.com/2013/09/sortino-ratio-are-you-calculating-it-wrong/
# 
# n - number of obs

#' @title get the moving (true)sortino ratio
#' @description (true)sortino of xts observations
#' @param x xts object
#' @param n how many values to 'calculate over' 
#'          logical mininum is n = 2
#' @return modified xts object
#' @details input is one single column xts object only
#' 
#'          # (true)sortino ratio
#'          # Sortino Ratio: Are you calculating it wrong?
#'          # https://www.rcmalternatives.com/2013/09/sortino-ratio-are-you-calculating-it-wrong/
#' 
#' @examples
#' \dontrun{
#' # get_pctchg_xts(head(sample_xts[,"Open"],6), n = 1)
#' #                           pctchg
#' # 2007-01-02                    NA
#' # 2007-01-03  0.381125334611203126
#' # 2007-01-04  0.379170077320410137
#' # 2007-01-05 -0.094181386571521211
#' # 2007-01-06 -0.256370148090095062
#' # 2007-01-07 -0.223335115582047300
#' # 
#' # get_smtsortino_xts(get_pctchg_xts(head(sample_xts[,"Open"],6), n = 1), 3)
#' #                       smtsortino
#' # 2007-01-02                    NA
#' # 2007-01-03                    NA
#' # 2007-01-04                    NA
#' # 2007-01-05  4.083408896943601540
#' # 2007-01-06  0.073562112260411788
#' # 2007-01-07 -2.231893800843009146
#' }
#' @rdname get_smtsortino_xts
#' @export
get_smtsortino_xts <- function(x, n) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(min.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_smtsortino_xts, only ONE column is allowed.")
  
  require(xts) 
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_smsortino_xts can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

  # (true)sortino ratio
  # Sortino Ratio: Are you calculating it wrong?
  # https://www.rcmalternatives.com/2013/09/sortino-ratio-are-you-calculating-it-wrong/
                                   # any NA, then entire thing returns NA
  tsortino <- function(x, rf = 0.0, na.rm = FALSE) {  (mean(x, na.rm = na.rm) - rf )/sd( local({x[x > 0] <- 0; x } ), na.rm = na.rm) }
  
  zoo::rollapply(as.zoo(x), width = n, partial = TRUE, align = "right", FUN = function(x, n) { 
    # if not too short
    if(n <= length(x)) { tsortino(x, rf = 0.0, na.rm = FALSE) } else { NA_real_ }
  }, n = n) -> x_result

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "smtsortino"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  smtsortino_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(smtsortino_xts)
} 



#' @title get the simple moving pessimistic ranks
#' @description pessimistic ranks of xts observations
#' @param x xts object
#' @param n how many values to 'calculate over' 
#'          logical mininum is n = 2
#'          pessimistic means that if a 'few observations' exist
#'            then the higher value numbers will not generate as low(better) ranks
#'            compared to the case of 'many observations'
#' @return modified xts object
#' @details input is one single column xts object only
#' @examples
#' \dontrun{
#' # get_smtsortino_xts(get_pctchg_xts(head(sample_xts[,"Open"],11), n = 1), 3)
#' #                       smtsortino
#' # 2007-01-02                    NA
#' # 2007-01-03                    NA
#' # 2007-01-04                    NA
#' # 2007-01-05  4.083408896943601540
#' # 2007-01-06  0.073562112260411788
#' # 2007-01-07 -2.231893800843009146
#' # 2007-01-08 -7.029534326268571220
#' # 2007-01-09 -2.217163448868932907
#' # 2007-01-10 -2.522576361771222242
#' # 2007-01-11 -1.729327001953728127
#' # 2007-01-12  1.728401658544285180
#' # 
#' # amoung the trailing 4, position(a 'higher' value = 1, a 'lower' value = 2)
#' # 
#' # get_smrank_xts(get_smtsortino_xts(get_pctchg_xts(head(sample_xts[,"Open"],11), n = 1), 3), n = 4, n_ranks = 2)
#' # 
#' #            smrank
#' # 2007-01-02     NA
#' # 2007-01-03     NA
#' # 2007-01-04     NA
#' # 2007-01-05     NA
#' # 2007-01-06     NA
#' # 2007-01-07     NA
#' # 2007-01-08      2
#' # 2007-01-09      1
#' # 2007-01-10      2
#' # 2007-01-11      1
#' # 2007-01-12      1
#' }
#' @rdname get_smrank_xts
#' @export
get_smrank_xts <- function(x, n, n_ranks) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(min.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_smrank_xts, only ONE column is allowed.")
  
  require(xts) 
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_smsortino_xts can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

  env <- environment()
  
  zoo::rollapply(as.zoo(x), width = n, partial = TRUE, align = "right", FUN = function(x, n, n_ranks, e) { 
    # if not too short
    if(n <= length(x)) {  
      # if zero NAs found
      if(!any(is.na(x))) { 
        # tail(_, 1): I only care about the value furthest value to the right
        # tested: pessimistic 
        # higher x values produce lower(better) 'sports' rank numbers: keep for now.  I may later change my mind.
                                                                                       # if any NA, then then 'error'
        tail(findInterval(-1 * x, tail(head(   quantile(-1 * x, seq(0, 1, 1/n_ranks) , na.rm = FALSE ),-1),-1)) + 1, 1)
      } else { NA_real_ } } else { NA_real_ } 
  }, n = n, n_ranks = n_ranks, e = env) -> x_result

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "smrank"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  smrank_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  # if n = 4
  # NEEDS to return smrank_lag0, smrank_lag1, smrank_lag2, smrank_lag3 
  
  return(smrank_xts)
} 




# single column xts only

#' @title fill in the gaps 
#' @description of an xts object create missing daily observations
#' @param x xts object
#' @return modified xts object
#' @details input is one single column xts object only
#'          missing daily observations in an xts series of daily values are inserted
#' @examples
#' \dontrun{
#' # require(xts)
#' # xts(c(11,13,15),zoo::as.Date(c(1,3,5))) 
#' #            [,1]
#' # 1970-01-02   11
#' # 1970-01-04   13
#' # 1970-01-06   15
#' # 
#' # get_collofdays2daily_xts(xts(c(11,13,15),zoo::as.Date(c(1,3,5))))
#' #            collofdays2daily
#' # 1970-01-02               11
#' # 1970-01-03               NA
#' # 1970-01-04               13
#' # 1970-01-05               NA
#' # 1970-01-06               15
#' #
#' # Note: the following is not part of the package tests
#' #
#' # as.POSIXct(c(1,10000,200000,400000), origin = "1970-01-01")
#' # [1] "1970-01-01 00:00:01 UTC" "1970-01-01 02:46:40 UTC" "1970-01-03 07:33:20 UTC" "1970-01-05 15:06:40 UTC"
#' # xts(11:14, as.POSIXct(c(1,10000,200000,400000), origin = "1970-01-01"))
#' # 
#' #                     [,1]
#' # 1970-01-01 00:00:01   11
#' # 1970-01-01 02:46:40   12
#' # 1970-01-03 07:33:20   13
#' # 1970-01-05 15:06:40   14
#' # 
#' # get_collofdays2daily_xts(xp)
#' #          [,1]
#' # 1970-01-01 12
#' # 1970-01-02 NA
#' # 1970-01-03 13
#' # 1970-01-04 NA
#' # 1970-01-05 14
#' }
#' @rdname get_collofdays2daily_xts
#' @export
get_collofdays2daily_xts <- function(x) {
  
  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  if(NCOL(x) > 1) stop("In get_collofdays2daily_xts, only ONE column is allowed.")
  
  require(xts) 
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_collofdays2daily_xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }
  # EXPECTED TO CHANGE THE NUMBER OF ROWS  SO CAN NOT DO 'reclass'
  
  # note: index(first/last(x, '1 day')) expected to return ONE single element
  # reduce a a 'set of first/last '1 day' to ret ONE single element 
  # also will covert a non-Date index to a Date index
  # 
  # zoo::as.Date garantees that a non-Date will become a Date
  # saved NOW because FUTURE xts::to.daily WILL trim off dates
  earliest_idx_x <- zoo::as.Date(zoo:::head.zoo(index(x),1))
  latest_idx_x   <- zoo::as.Date(zoo:::tail.zoo(index(x),1))
  
  # trims off dates THAT have a PAYLOAD of NA
  # Warning in to.period(x, "days", name = name, ...) :
  # missing values removed from data
  x <- suppressWarnings(xts::to.daily(x, OHLC = F))
  
  # dispatch on xts:::merge.xts, seq.Date, xts:::index.xts, xts:::first/last.xts
  x_days <- xts(,seq(earliest_idx_x, latest_idx_x, by = 1))
  
  # dispach on xts:::merge.xts
  merged <- merge(x_days, x)
  
  colnames(merged) <- "collofdays2daily"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(class(merged) %in% c("xts","zoo")) index(merged) <- zoo::as.Date(index(merged))
  
  collofdays2daily_xts <- merged
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(collofdays2daily_xts)
  
}



#' @title get the number of NA values
#' @description get the number of NA values since the last non-NA observations
#' @param x numeric vector
#' @return numeric vector of distances from the last non-NA observation
#' @details no details
#' @examples
#' \dontrun{
#' # get_delay_since_last_obs(c(101,NA,NA,NA,102,NA,NA))
#' # [1] 0 1 2 3 0 1 2
#' }
#' @rdname get_delay_since_last_obs
#' @export
#' @importFrom rowr rowApply
get_delay_since_last_obs <- function(x) {

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  # help from  StreamMetabolism::contiguous.zoo
  # uses       rowr::rowApply

  StreamMetabolism__noncontiguous.zoo <- function(x)   {
      z.rle <- rle(is.na(rowSums(coredata(x))))  # CAN BE EXTENDED HERE
      ends <- cumsum(z.rle$lengths)
      starts <- ends - z.rle$lengths + 1
      indexes <- with(z.rle, data.frame(starts, ends, lengths,
          values))
      indexes.sort <- indexes[order(-indexes$lengths), ]
      indexes.sort[indexes.sort$values, ]
  }

  vec <- x   #  vec <- c(101,NA,NA,NA,102,NA,NA)
  new_vec <- rep(0,length(vec)) # output of all values found ( optimistic ): zeros : no delays found of elements found
  
  # 100% opposite of the below ( delays found )

  snc <- StreamMetabolism__noncontiguous.zoo(data.frame(vec))
  if(NROW(snc)>0) {
    rowr::rowApply(snc, fun = function(x) { 
      with( x, { new_vec[starts:ends] <- seq(1,lengths,1)
                 assign("new_vec", new_vec , envir= parent.frame(8))  
               } ) -> discard ; NULL
    }) -> discard
  }
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(new_vec)
 
}



#' @title get the number of NA values
#' @description get the number of NA values since the last non-NA observations
#' @param x xts object
#' @return modified xts object
#' @details input is one single column xts object only
#'          payload NA-gaps matter ( NOT index time gaps )
#' @examples
#' \dontrun{
#' # get_delay_since_last_obs_xts(xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
#' #            delay_since_last_obs
#' # 1970-01-11                    0
#' # 1970-01-21                    1
#' # 1970-01-31                    2
#' # 1970-02-10                    3
#' # 1970-02-20                    0
#' # 1970-03-02                    1
#' # 1970-03-12                    2
#' }
#' @rdname get_delay_since_last_obs_xts
#' @export
get_delay_since_last_obs_xts <-function(x) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_delay_since_last_obs_xts, only ONE column is allowed.")
  
  require(xts) 
  # uses function get_delay_since_last_obs
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_delay_since_last_obs_xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  x_core  <- as.vector(coredata(x))
  x_index <- index(x)
  
  x_core_new <- get_delay_since_last_obs(x_core)
  
  x_result <- xts(x_core_new,x_index)
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "delay_since_last_obs"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  delay_since_last_obs_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(delay_since_last_obs_xts)

} 



# single column xts only
# add a record for each day

#' @title get a filled object of observations and delay times since the last non-na observation
#' @description get a filled xts object of the 
#'              days and number days since the last xts non-na observation
#' @param x xts object
#' @return modified xts object
#' @details input is one single column xts object only
#'          payload NA-gaps matter ( NOT index time gaps )
#' @examples
#' \dontrun{
#' # require(xts)
#' # get_delay_since_last_day_xts(xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
#' #            delay_since_last_day
#' # 1970-01-11                    0
#' # 1970-01-12                    1
#' # 1970-01-13                    2
#' # 1970-01-14                    3
#' # 1970-01-15                    4
#' # 1970-01-16                    5
#' # 1970-01-17                    6
#' # 1970-01-18                    7
#' # 1970-01-19                    8
#' # 1970-01-20                    9
#' # 1970-01-21                   10
#' # 1970-01-22                   11
#' # 1970-01-23                   12
#' # 1970-01-24                   13
#' # 1970-01-25                   14
#' # 1970-01-26                   15
#' # 1970-01-27                   16
#' # 1970-01-28                   17
#' # 1970-01-29                   18
#' # 1970-01-30                   19
#' # 1970-01-31                   20
#' # 1970-02-01                   21
#' # 1970-02-02                   22
#' # 1970-02-03                   23
#' # 1970-02-04                   24
#' # 1970-02-05                   25
#' # 1970-02-06                   26
#' # 1970-02-07                   27
#' # 1970-02-08                   28
#' # 1970-02-09                   29
#' # 1970-02-10                   30
#' # 1970-02-11                   31
#' # 1970-02-12                   32
#' # 1970-02-13                   33
#' # 1970-02-14                   34
#' # 1970-02-15                   35
#' # 1970-02-16                   36
#' # 1970-02-17                   37
#' # 1970-02-18                   38
#' # 1970-02-19                   39
#' # 1970-02-20                    0
#' # 1970-02-21                    1
#' # 1970-02-22                    2
#' # 1970-02-23                    3
#' # 1970-02-24                    4
#' # 1970-02-25                    5
#' # 1970-02-26                    6
#' # 1970-02-27                    7
#' # 1970-02-28                    8
#' # 1970-03-01                    9
#' # 1970-03-02                   10
#' # 1970-03-03                   11
#' # 1970-03-04                   12
#' # 1970-03-05                   13
#' # 1970-03-06                   14
#' # 1970-03-07                   15
#' # 1970-03-08                   16
#' # 1970-03-09                   17
#' # 1970-03-10                   18
#' # 1970-03-11                   19
#' # 1970-03-12                   20
#' }
#' @rdname get_delay_since_last_day_xts
#' @export
get_delay_since_last_day_xts <-function(x) { 

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In get_delay_since_last_day_xts, only ONE column is allowed.")
  
  require(xts) 
  # uses function get_delay_since_last_obs
  # uses package xts merge.xts
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("get_delay_since_last_day_xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # more dates - create temporary rows
  x_nonsparse <- get_collofdays2daily_xts(x)
  
  # find delays(0 - no delay over NA, 1 - one delay 'at' NA)
  x_nonsparse_delays <- get_delay_since_last_obs_xts(x_nonsparse)
  
  x_result <- x_nonsparse_delays
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "delay_since_last_day"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  delay_since_last_day_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(delay_since_last_day_xts)

} 



#' @title determine if  observations have the value of NA
#' @description find whether or not an xts value is NA
#' @param x xts object
#' @return modified xts object
#' @details input is one single column xts object only
#' @examples
#' \dontrun{
#' # require(xts)
#' # is_na_xts(xts(c(11,NA,NA,14,NA),zoo::as.Date(1:5)))
#' #              na
#' # 1970-01-02    2
#' # 1970-01-03    1
#' # 1970-01-04    1
#' # 1970-01-05    2
#' # 1970-01-06    1
#' }
#' @rdname is_na_xts
#' @export
is_na_xts <- function(x) {
  
  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In is_na_xts, only ONE column is allowed.")
  
  require(xts) 
  # uses ojUtils::ifelseC
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("is.na.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # expecting a 'single' column xts

  x_vector     <- as.vector(coredata(x))
  x_vector_len <- length(x_vector)

  coredata_new <- ojUtils::ifelseC(is.na(x_vector), rep(1,x_vector_len), rep(2,x_vector_len))
  coredata(x)  <- coredata_new
  x_result     <- x

  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "na"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  na_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
   
  return(na_xts)

} 



#' @title remove observations
#' @description remove day(Date) 'day of week' observations from an xts object
#' @param x 
#' @param rm_what typical entry rm_what = c("Saturday", "Sunday", "BIZHOLIDAYS" )
#' @return modified xts object
#' @details input is one single column xts object only
#' @examples
#' \dontrun{
#' # weekends removed
#' # about 
#' # 
#' # RQuantLib::isHoliday(. . ."UnitedStates/NYSE". . .) considers 'weekends' to be holidays
#' # # JAN 01 2007 to JAN 17 2007
#' # 
#' # # weekends     removed
#' # 
#' # # Holiday 1st  removed - New Year's Day
#' # # Holiday 2nd  removed - Day Of Mourning - Gerald Ford ( SUPRISING )
#' # # Holiday 15th removed - Martin Luther King Day
#' # 
#' # require(xts)
#' # x <- xts(1:17, zoo::as.Date("2007-01-01") -1 + 1:17)
#' # df <- data.frame( index(x), RQuantLib::isHoliday("UnitedStates/NYSE", index(x)), weekdays(index(x)))
#' # colnames(df) <- c("index", "is_nyse_holiday", "weekday")
#' # df
#' #         index is_nyse_holiday   weekday
#' # 1  2007-01-01            TRUE    Monday
#' # 2  2007-01-02            TRUE   Tuesday
#' # 3  2007-01-03           FALSE Wednesday
#' # 4  2007-01-04           FALSE  Thursday
#' # 5  2007-01-05           FALSE    Friday
#' # 6  2007-01-06            TRUE  Saturday
#' # 7  2007-01-07            TRUE    Sunday
#' # 8  2007-01-08           FALSE    Monday
#' # 9  2007-01-09           FALSE   Tuesday
#' # 10 2007-01-10           FALSE Wednesday
#' # 11 2007-01-11           FALSE  Thursday
#' # 12 2007-01-12           FALSE    Friday
#' # 13 2007-01-13            TRUE  Saturday
#' # 14 2007-01-14            TRUE    Sunday
#' # 15 2007-01-15            TRUE    Monday
#' # 16 2007-01-16           FALSE   Tuesday
#' # 17 2007-01-17           FALSE Wednesday             
#' # 
#' # rm_days_xts(xts(1:17,zoo::as.Date("2007-01-01") -1 + 1:17), rm_what = c("Saturday", "Sunday", "BIZHOLIDAYS"))
#' #            days
#' # 2007-01-03    3
#' # 2007-01-04    4
#' # 2007-01-05    5
#' # 2007-01-08    8
#' # 2007-01-09    9
#' # 2007-01-10   10
#' # 2007-01-11   11
#' # 2007-01-12   12
#' # 2007-01-16   16
#' # 2007-01-17   17
#' }
#' @rdname rm_days_xts
#' @export
rm_days_xts <- function(x, rm_what = NULL) {

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In rm_days_xts, only ONE column is allowed.")
  
  # JUN 2017
  # .indexwday
  # http://joshuaulrich.github.io/xts/xts_faq.html

  require(xts) 
  # uses package RQuantLib function isHoliday
  # uses package stringr   function str_detect
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("rm_days_xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  if( !is.null(rm_what)     && 
     ( length(rm_what) > 0) && 
      any(stringr::str_detect(rm_what,"Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|HOLIDAYS$"))
    ) { 

    # 0 - Sunday ... 6 - Saturday  
    
    # .indexwday(xts(,zoo::as.Date("2017-06-18"))) # Sunday
    # [1] 0
    # 
    # weekdays(index(xts(,zoo::as.Date("2017-06-18")))) # format(zoo::as.Date("2017-06-18"), "%A")
    # [1] "Sunday"

    weekdays_index <- c(Sunday = 0,  Monday = 1, Tuesday = 2, Wednesday = 3, Thursday = 4, Friday = 5, Saturday = 6)

    rm_what_holidays_not <-  rm_what[!stringr::str_detect(rm_what,"HOLIDAYS$")]
    if(length(rm_what_holidays_not)) {
    
      rm_what_weekdays_index   <-  weekdays_index[match(rm_what_holidays_not, names(weekdays_index))]
      if(length(rm_what_weekdays_index)) {
      
        keep_what_weekdays_index <- weekdays_index[-match(names(rm_what_weekdays_index), names(weekdays_index))]
        x <- x[.indexwday(x) %in% keep_what_weekdays_index]
      
      }
    
    }
      
    rm_what_holidays <-  rm_what[stringr::str_detect(rm_what,"BIZHOLIDAYS$")]
    if(length(rm_what_holidays)) {
    
      if(length(match("BIZHOLIDAYS",rm_what_holidays))) {
      
        # "UnitedStates/GovernmentBond"
        # 2007 Federal Holidays
        # Monday, January 1   New Year's Day
        # Monday, January 15  Birthday of Martin Luther King, Jr.
        # https://archive.opm.gov/Operating_Status_Schedules/fedhol/2007.asp
        
        # "UnitedStates/NYSE"
        # NYSE Holidays from 2000-2010
        # 01 Jan 2007 Monday  New Years Day
        # 02 Jan 2007 Tuesday Day Of Mourning - Gerald Ford ( SUPRISING )
        # 15 Jan 2007 Monday  Martin Luther King Day
        # http://nyseholidays.blogspot.com/2012/11/nyse-holidays-from-2000-2010.html

        # NYSE Holidays: Market Closings for 2017 - Stock Market Holidays Schedule
        # Thursday, February 23, 2017
        # https://mrtopstep.com/nyse-holidays-market-closings-2017-stock-market-holidays-schedule/
        
        # NOTE does INCLUDE WEEKENDS
        x <- x[!RQuantLib::isHoliday("UnitedStates/NYSE", zoo::as.Date(index(x)))] # CHANGED index(x) TO zoo::as.Date(index(x)) # UNTESTED but POSIX__ NOT WORK
      
      }
    }
  }
  
  x_result <- x

  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "days"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  days_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(days_xts)
}



#' @title determine if a FRED Holiday is nearby
#' @description find out whether or not a nearby FRED xts day is a holiday or not
#' @param x xts object
#' @param d numeric vector of past days: -1 yesterday, c(-1,-2) yesterday AND the 'day before yesterday' etc ( both must be true )
#'           e.g. if TRUE and d = c(-1,-2), then 
#'           BOTH yesterday and the 'day before yesterday' NON-working days of the U.S. Federal Government.
#'           holdays are based on RQuantlib UnitedStates/GovernmentBond calendar
#' @return modified xts object
#' @details input is one single column xts object only
#'          meant to be a utility function ( input to OTHERS )
#' @examples
#' \dontrun{
#' # require(xts)
#' # data(sample_matrix)
#' # sample_xts <- as.xts(sample_matrix)
#' # 
#' # are_nearby_fred_holidays_xts(sample_xts[2:16,"Open"], d = c(-1,-2))
#' #            nearby_fred_holidays
#' # 2007-01-03                FALSE
#' # 2007-01-04                FALSE
#' # 2007-01-05                FALSE
#' # 2007-01-06                FALSE
#' # 2007-01-07                FALSE
#' # 2007-01-08                 TRUE # is TRUE because yesterday and the day before were holidays
#' # 2007-01-09                FALSE
#' # 2007-01-10                FALSE
#' # 2007-01-11                FALSE
#' # 2007-01-12                FALSE
#' # 2007-01-13                FALSE
#' # 2007-01-14                FALSE
#' # 2007-01-15                 TRUE # is TRUE because SUN(yesterday) and SAT were holidays
#' # 2007-01-16                 TRUE # is TRUE because MON(yesterday) and SUN were holidays
#' # 2007-01-17                FALSE
#' }
#' @rdname are_nearby_fred_holidays_xts
#' @export
are_nearby_fred_holidays_xts <- function(x, d = NULL) {
 
  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(NCOL(x) > 1) stop("In are_nearby_fred_holidays_xts, only ONE column is allowed.")
  
  # "UnitedStates/GovernmentBond"
  # 2007 Federal Holidays
  # Monday, January 1   New Year's Day
  # Monday, January 15  Birthday of Martin Luther King, Jr.
  # https://archive.opm.gov/Operating_Status_Schedules/fedhol/2007.asp
  
  require(xts) 
  # uses package RQuantLib function isHoliday, 
  # uses package rlist     function list.zip
  # uses package lubridate function `%m+%`
  # uses package lubridate function days
  
  `%M+%` <- lubridate::`%m+%`
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("xxx could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # for ONE single day returns multiple days
  FUN  <- function(x,d) { index(x) %M+% lubridate::days(c(d)) }
  PARALLEL_LISTS <- Vectorize(FUN, vectorize.args = "d", SIMPLIFY = FALSE)(x, d)
  TOGETHER_LISTS <- do.call(rlist::list.zip, PARALLEL_LISTS)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  # if BOTH of those 'multiple days' is a "UnitedStates/GovernmentBond" then return TRUE
  sapply(TOGETHER_LISTS, function(x) { 
    xx <- x
    all(sapply(xx, function(xx) { 
      RQuantLib::isHoliday("UnitedStates/GovernmentBond",zoo::as.Date(xx)) 
    })) 
  }) -> x_result
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "nearby_fred_holidays"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  nearby_fred_holidays_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(nearby_fred_holidays_xts)
  
}



#' @title replace an xts object date attribute
#' @description swap out an XTS object index USING my OWN custom INDEX
#' @param x xts object
#' @param x_index_new new index to replace the index of the xts(x) object
#'                    anything one dimensional with a length/NROW(x_index_new) == NROW(x) == length(index(x))
#'                    numeric values are day(Date) values
#' @return modified xts object
#' @details one 'many columned' xts is allowed as input
#'          columns are not RENAMED
#' @examples
#' \dontrun{
#' # require(quantmod)
#' # unrate <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1948-03-01"]
#' #            UNRATE
#' # 1948-01-01    3.4
#' # 1948-02-01    3.8
#' # 1948-03-01    4.0
#' # 
#' # do_reindex_xts(unrate, 0:2)
#' #                        UNRATE
#' # 1970-01-01 3.3999999999999999
#' # 1970-01-02 3.7999999999999998
#' # 1970-01-03 4.0000000000000000
#' # 
#' # do_reindex_xts(merge(unrate,unrate), 0:2)
#' #                        UNRATE           UNRATE.1
#' # 1970-01-01 3.3999999999999999 3.3999999999999999
#' # 1970-01-02 3.7999999999999998 3.7999999999999998
#' # 1970-01-03 4.0000000000000000 4.0000000000000000
#' }
#' @rdname do_reindex_xts
#' @export
do_reindex_xts <- function(x,  x_index_new ) {

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  # many columns are allowed
  
  require(xts) 

  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("do_reindex_xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  x_tclass <- tclass(x)
  x_tzone  <- tzone(x)
  # INTERNAL STORAGE IS ALWAYS POSIXCT
  # as.vector(unlist: anything one dimensional # HACK: x_tclass == "Date": SEE index:::index.xts
  # .index(x) # DESTROYS entire attr(x,"index")
  .index(x) <- as.vector(unlist(x_index_new)) * if( x_tclass == "Date") { 3600 * 24 } else { 1 }
  x <- x[order(index(x)),]
  tclass(x) <- x_tclass
  tzone(x)  <- x_tzone # get for free( redundant ) == "UTC" if x_tclass == "Date"
  
  x_result <- x
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  do_reindex <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(do_reindex)
}



#' @title adjust a date to be an earlier data
#' @description change an xts object index value become an earlier date
#' @param x xts object
#' @return modified xts object
#' @details meant REALY only for St.Louis FRED
#'          adjust dates that start on the 1st( sometimes 4th, 3rd, or 2nd ) to be the 31st
#'          be aware of landings on weekend and long holiday weekends and after a Tuesday or Thursday holiday
#'          one 'many columned' xts is allowed as input
#'          columns are not RENAMED
#'          slow: 170 observations per second
#' @examples
#' \dontrun{
#' # require(quantmod)
#' # unrate <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1948-03-01"]
#' #            UNRATE
#' # 1948-01-01    3.4
#' # 1948-02-01    3.8
#' # 1948-03-01    4.0
#' # 
#' # pushback_fred_1st_days_xts(unrate)
#' #                        UNRATE
#' # 1947-12-31 3.3999999999999999
#' # 1948-01-31 3.7999999999999998
#' # 1948-02-29 4.0000000000000000
#' # 
#' # pushback_fred_1st_days_xts(merge(unrate,unrate))
#' #                        UNRATE           UNRATE.1
#' # 1947-12-31 3.3999999999999999 3.3999999999999999
#' # 1948-01-31 3.7999999999999998 3.7999999999999998
#' # 1948-02-29 4.0000000000000000 4.0000000000000000
#' }
#' @rdname pushback_fred_1st_days_xts
#' @export
pushback_fred_1st_days_xts <- function(x) {

  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  # many columns are allowed
  
  require(xts)
  # uses package lubridate function `%m+%`
  # uses package lubridate function day
  # uses package lubridate function days
  # uses package xts function apply.daily
  # uses function are_nearby_fred_holidays_xts  
  # uses function do_reindex_xts

  `%M+%` <- lubridate::`%m+%`
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("pushback_fred_1st_days_xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # If Mutiple POSIXct per day then I just need ONE of them 
  x_s        <- xts::split.xts( x, f = "days" )
  x_s_l_last <- lapply( x_s , function(x) { xts::last(x) } ) # xts::last remaining of all the elements
  # unsplit
  x <- do.call(xts::rbind.xts, x_s_l_last )
  
  # if the 4th is Today and the last 3 days were holidays then shift the index 4 # then done  
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(lubridate::day(index(xx)) == 4 && are_nearby_fred_holidays_xts(xx, c(-1,-2,-3))) {
      index(xx) %M+% lubridate::days(-4)
    } else {
      index(xx)
    }
  }) -> x_4th
  x <- do_reindex_xts(x, x_4th)

  # if the 3rd is Today and the last 2 days were holidays then shift the index 3 # then done  
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(lubridate::day(index(xx)) == 3 && are_nearby_fred_holidays_xts(xx, c(-1,-2))) {
      index(xx) %M+% lubridate::days(-3)
    } else {
      index(xx)
    }
  }) -> x_3rd
  x <- do_reindex_xts(x, x_3rd)

  # Tuesday and Thursday(Thanksgiving) holidays
  # if the 2nd is Today and the last 1 day was a holiday then shift the index 2 # then done  
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(lubridate::day(index(xx)) == 2 && are_nearby_fred_holidays_xts(xx, c(-1))) {
      index(xx) %M+% lubridate::days(-2)
    } else {
      index(xx)
    }
  }) -> x_2nd
  x <- do_reindex_xts(x, x_2nd)

  # if the 1st is Today then shift the index 1  # then done
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(lubridate::day(index(xx)) ==  1) {
      index(xx) %M+% lubridate::days(-1)
    } else {
      index(xx)
    }
  }) -> x_1st
  x <- do_reindex_xts(x, x_1st)
  x_result <- x

  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  # columns are not renamed
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  fred_1st_days_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(fred_1st_days_xts)

}



#' @title determine if a year value is less than or equal to a specific value
#' @description finds if an xts index value is less than a specific value
#' @param x xts object
#' @param n meant to pass just the index year numeric YYYY
#' @return modified xts object with ONE column is returned
#'         1 - yes # 2 - no
#' @details input is one 'multiple columned' xts object only
#'          But, the style would be better to just input one single columned xts object only
#' @examples
#' \dontrun{
#'# require(quantmod)
#'# unrate_40s <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1949-03-01"]
#'# 
#'# is_year_less_than_or_equal_xts(unrate_40s, 1948)
#'#            year_less_than_or_equal_xts
#'# 1948-01-01                           1
#'# 1948-02-01                           1
#'# 1948-03-01                           1
#'# 1948-04-01                           1
#'# 1948-05-01                           1
#'# 1948-06-01                           1
#'# 1948-07-01                           1
#'# 1948-08-01                           1
#'# 1948-09-01                           1
#'# 1948-10-01                           1
#'# 1948-11-01                           1
#'# 1948-12-01                           1
#'# 1949-01-01                           2
#'# 1949-02-01                           2
#'# 1949-03-01                           2
#'# 
#'# is_year_less_than_or_equal_xts(merge(unrate_40s,unrate_40s), 1948)
#'#            year_less_than_or_equal_xts
#'# 1948-01-01                           1
#'# 1948-02-01                           1
#' }
#' @rdname is_year_less_than_or_equal_xts
#' @export
is_year_less_than_or_equal_xts <- function(x, n = NULL) {
  
  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  # multiple columns are allowed
  
  require(xts) 
  # uses package lubridate function year
  # uses package ojUtils   function ifelseC
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("is_year_less_than_or_equal_xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # only the index is important
  
  x_index_len <- length(index(x))

  coredata_new <- ojUtils::ifelseC(lubridate::year(index(x)) <= n, rep(1,x_index_len), rep(2,x_index_len))
  
  # coredata(x)  <- coredata_new
  # safer method if no corredata is passed
  x <- xts(coredata_new, index(x))
  x_result <- x
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "year_less_than_or_equal_xts"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
  
  year_less_than_or_equal_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
   
  return(year_less_than_or_equal_xts)

}



#' @title apply a function across all columns and all 2nd paramter values
#' @description travers the columns of an xts object an apply functions
#'              also do a traversal for each velue of 'whiches' 
#' @param x xts object
#' @param fnct function to apply to each column in the xts input and to each velue of 'whiches'
#'             can be for the forms 
#'               literal closure functions 
#'                 FUNCTION
#'               access of interals functions
#'                 package::function package:::private_function
#'               literal and accessor of functions sent in the form of strings (anonymous)
#'                 "FUNCTION"    "package::function"    "package:::private_function"
#'               literal anonymous functions
#'                 function(x, n) { . . . }
#'               anonymous functions in the form of strings (anonymous)
#'                 "function(x, n) { . . . }"  
#' @param whiches numeric parameter sent to the 'fnct' second argument
#'                will be expecting a vector of numeric values
#' @param alt_name of output xts, of new columns generated, the new 'root' name
#' @param o_args other arguments
#'               o_args is a named vector of arguments ( but user should really should use a Curry )
#'               if o_args IS OF A MIXED DATA.TAPE,  use a list INSTEAD ( of a vector )
#'               e.g. = list(indexAt = 'lastof', OHLC = FALSE)
#' @param prefix , to the root, use a prefix(TRUE) instead of a postfix(NULL/FALSE): NULL(FALSE) postfix
#' @param fixed_sep in output column divider between the root and the post/pre/fix
#' @return modified xts object
#' @details Based on the concept of 'automatatic column and column name generation' 
#'          seen the article of Zachary Mayer
#'          Time series cross-validation 5
#'          January 24, 2013
#'          By Zachary Mayer ( http://www.r-bloggers.com/author/zachary-mayer/ )
#'          http://www.r-bloggers.com/time-series-cross-validation-5/
#'          https://gist.github.com/zachmayer/4630129#file-1-load-data-r
#'          http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
#' @examples
#' \dontrun{
#' # require(quantmod) 
#' # ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#' # 
#' #                      IBM.Open           IBM.High   IBM.Low          IBM.Close IBM.Volume       IBM.Adjusted
#' # 1970-01-02 18.225000000000001 18.287500000000001 18.200001 18.237499000000000     315200 5.3213109999999997
#' # 1970-01-05 18.299999000000000 18.412500000000001 18.299999 18.412500000000001     424000 5.3723760000000000
#' # 1970-01-06 18.412500000000001 18.450001000000000 18.312500 18.424999000000000     488000 5.3760190000000003
#' # 1970-01-07 18.424999000000000 18.437500000000000 18.312500 18.437500000000000     457600 5.3796629999999999
#' # 1970-01-08 18.437500000000000 18.475000000000001 18.375000 18.475000000000001     707200 5.3906080000000003
#' # 1970-01-09 18.475000000000001 18.524999999999999 18.424999 18.450001000000000     585600 5.3833140000000004
#' # 1970-01-12 18.450001000000000 18.487499000000000 18.387501 18.387501000000000     379200 5.3650779999999996
#' # 
#' # changed prefix = TRUE
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = TRUE) # NOT default
#' #            TTR_SMA.2.IBM.Open TTR_SMA.2.IBM.Close TTR_SMA.3.IBM.Open TTR_SMA.3.IBM.Close
#' # 1970-01-02                 NA                  NA                 NA                  NA
#' # 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
#' # 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
#' # 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
#' # 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
#' # 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
#' # 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665
#' # 
#' # changed    prefix = FALSE  ( default )
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = FALSE)  # default
#' #            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
#' # 1970-01-02                 NA                  NA                 NA                  NA
#' # 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
#' # 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
#' # 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
#' # 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
#' # 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
#' # 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665
#' # 
#' # changed   fixed_sep = "_"
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, fixed_sep = "_")
#' #            IBM.Open_TTR_SMA_2 IBM.Close_TTR_SMA_2 IBM.Open_TTR_SMA_3 IBM.Close_TTR_SMA_3
#' # 1970-01-02                 NA                  NA                 NA                  NA
#' # 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
#' # 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
#' # 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
#' # 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
#' # 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
#' # 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3)
#' #            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
#' # 1970-01-02                 NA                  NA                 NA                  NA
#' # 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
#' # 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
#' # 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
#' # 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
#' # 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
#' # 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665
#' # 
#' # changed to hard-coded function call   fnct = TTR::SMA
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = TTR::SMA, whiches = 2:3)
#' #            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
#' # 1970-01-02                 NA                  NA                 NA                  NA
#' # 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
#' # 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
#' # 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
#' # 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
#' # 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
#' # 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665
#' # 
#' # changed to function sent as a string   fnct = "function(x,n){ TTR::SMA(x,n) }"
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "function(x,n){ TTR::SMA(x,n) }", whiches = 2:3)
#' # 
#' #               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
#' # 1970-01-02                 NA                 NA                 NA                 NA
#' # 1970-01-05 18.262499500000001 18.324999500000001                 NA                 NA
#' # 1970-01-06 18.356249500000001 18.418749500000001 18.312499666666668 18.358332666666666
#' # 1970-01-07 18.418749500000001 18.431249500000000 18.379166000000001 18.424999666666665
#' # 1970-01-08 18.431249500000000 18.456250000000001 18.424999666666665 18.445833000000000
#' # 1970-01-09 18.456250000000001 18.462500500000001 18.445833000000000 18.454166999999998
#' # 1970-01-12 18.462500500000001 18.418751000000000 18.454166999999998 18.437500666666665
#' # 
#' # change to function sent as a closure   fnct = function(x,n){ TTR::SMA(x,n) }
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = function(x,n){ TTR::SMA(x,n) }, whiches = 2:3)
#' #               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
#' # 1970-01-02                 NA                 NA                 NA                 NA
#' # 1970-01-05 18.262499500000001 18.324999500000001                 NA                 NA
#' # 1970-01-06 18.356249500000001 18.418749500000001 18.312499666666668 18.358332666666666
#' # 1970-01-07 18.418749500000001 18.431249500000000 18.379166000000001 18.424999666666665
#' # 1970-01-08 18.431249500000000 18.456250000000001 18.424999666666665 18.445833000000000
#' # 1970-01-09 18.456250000000001 18.462500500000001 18.445833000000000 18.454166999999998
#' # 1970-01-12 18.462500500000001 18.418751000000000 18.454166999999998 18.437500666666665
#' # 
#' # check that a specific function works
#' # 
#' # ibm_missing_data <- local({ t <- ibm; t[2:3,1] <- NA_real_; t})
#' # expand_xts(ibm_missing_data[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3)
#' #               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
#' # 1970-01-02 18.225000000000001 18.237499000000000 18.225000000000001 18.237499000000000
#' # 1970-01-05 18.225000000000001 18.412500000000001 18.225000000000001 18.412500000000001
#' # 1970-01-06 18.225000000000001 18.424999000000000 18.225000000000001 18.424999000000000
#' # 1970-01-07 18.424999000000000 18.437500000000000 18.424999000000000 18.437500000000000
#' # 1970-01-08 18.437500000000000 18.475000000000001 18.437500000000000 18.475000000000001
#' # 1970-01-09 18.475000000000001 18.450001000000000 18.475000000000001 18.450001000000000
#' # 1970-01-12 18.450001000000000 18.387501000000000 18.450001000000000 18.387501000000000
#' # 
#' # give the ouput columns and alternate pre/post(append) name
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3, alt_name = "NALOCF")
#' #             IBM.Open.NALOCF.2 IBM.Close.NALOCF.2  IBM.Open.NALOCF.3 IBM.Close.NALOCF.3
#' # 1970-01-02 18.225000000000001 18.237499000000000 18.225000000000001 18.237499000000000
#' # 1970-01-05 18.225000000000001 18.412500000000001 18.225000000000001 18.412500000000001
#' # 1970-01-06 18.225000000000001 18.424999000000000 18.225000000000001 18.424999000000000
#' # 1970-01-07 18.424999000000000 18.437500000000000 18.424999000000000 18.437500000000000
#' # 1970-01-08 18.437500000000000 18.475000000000001 18.437500000000000 18.475000000000001
#' # 1970-01-09 18.475000000000001 18.450001000000000 18.475000000000001 18.450001000000000
#' # 1970-01-12 18.450001000000000 18.387501000000000 18.450001000000000 18.387501000000000
#' # 
#' # send extra arguements to a function
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "get_pctchg_xts", whiches = 2:3, alt_name = "futPCTCHG" , o_args = c(to_future = TRUE ))
#' #             IBM.Open.futPCTCHG.2 IBM.Close.futPCTCHG.2 IBM.Open.futPCTCHG.3 IBM.Close.futPCTCHG.3
#' # 1970-01-02 -1.028806584362139898 -1.028101495714955238 -1.09738820301782303  -1.09664707863726441
#' # 1970-01-05 -0.683060146615308561 -0.135777325186686088 -0.75137162575801408  -0.33944331296673452
#' # 1970-01-06 -0.135777325186686088 -0.271375862761250308 -0.33944331296673452  -0.13569607249368446
#' # 1970-01-07 -0.271375862761250308 -0.067802033898306802 -0.13569607249368446   0.27118101694915081
#' # 1970-01-08 -0.067802033898306802  0.473607577807854341                   NA                    NA
#' # 1970-01-09                    NA                    NA                   NA                    NA
#' # 1970-01-12                    NA                    NA                   NA                    NA
#' # 
#' # check that the xts function 'to.monthly' works
#' # 
#' # expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "to.monthly", alt_name = "MONTHLY", o_args = list(indexAt= 'lastof', OHLC = FALSE))
#' #            IBM.Open.MONTHLY IBM.Close.MONTHLY
#' # 1970-01-31        18.450001         18.387501
#' # 
#' # check that a specific function 'is_year_less_than_or_equal_xts' works
#' # 
#' # expand_xts(xts(,index(ibm)), fnct = "is_year_less_than_or_equal_xts", whiches =  seq(lubridate::year(min(index(ibm))), lubridate::year(max(index(ibm))),by = 1), alt_name = "y_lth_or_eq_to_fact")
#' # 
#' #            y_lth_or_eq_to_fact.1970
#' # 1970-01-02                        1
#' # 1970-01-05                        1
#' # 1970-01-06                        1
#' # 1970-01-07                        1
#' # 1970-01-08                        1
#' # 1970-01-09                        1
#' # 1970-01-12                        1
#' }
#' @rdname expand_xts
#' @export
expand_xts <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL, fixed_sep = NULL) {
  
  # concept (Found by RSEEK) BASED ON 
  #   Time series cross-validation 5
  #   January 24, 2013
  #   By Zachary Mayer ( OTHERS BY THIS AUTHOR: http://www.r-bloggers.com/author/zachary-mayer/ )
  #   Zachary Deane-Mayer  
  #   CARET GUY
  #     http://www.r-bloggers.com/time-series-cross-validation-5/
  #     http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
  #     GIST OF THIS ON GITHUB
  #     https://gist.github.com/zachmayer/4630129#file-1-load-data-r
  #   AUTHOR OF R CRAN package caretEnsemble and R github package cv.ts 
  
  fnct_text   <- as.character(deparse(substitute(fnct)))
  
  matched_call <- capture.output(str(match.call()))
  
  ops <- options()
  
  options(warn = 1)
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  expand_xts_inner <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL, fixed_sep = NULL) {

    require(xts) 
    # uses package zoo       functions is.zoo, as.zoo, na.locf
    # uses package xts       functions is.xts, as.xts
    # uses package xts       fuctiions na.locf.xts(S3dispatch), merge.xts(S3dispatch)
    # uses package DescTools function  DoCall
    # uses package plyr      functions join_all, mutate
    # uses package stringr   functions str_replace_all, str_detect, str_c
    # uses package rlist     function  list.flatten, list.ungroup 

    if(is.null(     x))  stop("run-time user must provide input data")
    if(is.null(   fnct)) stop("run-time user must provide a function 'fnct'")
    if(is.null(prefix))  prefix  <- FALSE # do  I of name fnct/alt_name do a preprend(TRUE) or append(FALSE)(default) 
    if(is.null(fixed_sep)) fixed_sep = "." # between the root and the pre/post(append) fix of the new column(s) names
    
    has_whiches <- TRUE     # patch # if does not have a non-null wiches argument, then give it one so it can LASTER do ONE loop
    if(is.null(whiches)) { has_whiches <- FALSE ; whiches = -Inf }  
  
    have_alt_name <- FALSE
    if(!is.null(alt_name)) have_alt_name <- TRUE # patch # but REALLY the user should use a Curry
  
    fnct_pass_type_determined <- FALSE
    
    isFunctionSyntax <- function(x) { 
       stringr::str_detect(x,"^[a-zA-Z][a-zA-Z0-9]*(::|:::)[a-zA-Z]([a-zA-Z0-9])*$") || stringr::str_detect(x,"^[a-zA-Z][a-zA-Z0-9]*$")
    }
    
    # pass by   function_name
    # pass by   function(x,n){  }
    if(is.function(fnct) && !fnct_pass_type_determined) { 
      
      # because :: and ::: themselves are functions that return function content 
      # exists("NS::FUNCT", mode = "function")
      # then return FALSE
      
      # pass by   function(x,n){  }
      if(!isFunctionSyntax(stringr::str_c(fnct_text, collapse = "")) && !fnct_pass_type_determined) { 
        fnct_pass_type_determined <- TRUE
        assign( "anon", fnct )
        fnct <- "anon"
      }
      
      # pass by   function_name   namespace::function_name
      # NOTE: fnct_text SHOULD HAVE already been collapsed
      if( isFunctionSyntax(stringr::str_c(fnct_text, collapse = "")) && !fnct_pass_type_determined) { 
        fnct_pass_type_determined <- TRUE
        assign(fnct_text, fnct )
        fnct <- fnct_text
      }
      
    }
    
    if(!is.function(fnct) && !fnct_pass_type_determined) {

      # pass by   "function_name"
      if(is.character(fnct) && is.function(eval(parse(text=fnct))) &&  isFunctionSyntax(fnct) && !fnct_pass_type_determined) {
        fnct_pass_type_determined <- TRUE
        # default
        # fnct = "function_name"
      }
      
      # pass by   "function(x,n){  }"
      if(is.character(fnct) && is.function(eval(parse(text=fnct))) && !isFunctionSyntax(fnct) && !fnct_pass_type_determined) {
        fnct_pass_type_determined <- TRUE
        assign( "anon", eval(parse(text=fnct)))
        fnct <- "anon"
      }
    
    }
      
    x_orig <- x
    c_orig <- class(x)[1] # original class
    
    ## VERY BASIC attemped CLASS conversion ##
    x_try.xts_success <- FALSE
    x_try.xts <- try(xts::try.xts(x_orig), silent = T)
    #
    x         <- if(any("try-error" %in% class(x_try.xts))) { x_orig } else { x_try.xts_success <- TRUE; x_try.xts }
    
    x -> INPUT  
  
    # NOTE: INPUT class is typically "xts" "zoo"
    RETs <- lapply(whiches, function(x) {
      lapply( INPUT, function(x,whiches) { 
            # global
        if(!has_whiches) whiches = NULL # send nothing             # global
        as.data.frame( DescTools::DoCall(fnct, c(list(x), whiches, o_args, list())), stringsAsFactors = FALSE) -> z # extra list(), so c garanteed to upsize to a list
        colnames(z) <- NULL
        return(z) 
      }, whiches = x ) -> out
  
      # Override 
      if(have_alt_name) fnct <- alt_name
      
      if(has_whiches) {
        if(prefix) { 
          paste(            stringr::str_replace_all(fnct,"[.]|::","_"), x, names(out), sep= fixed_sep) -> names(out)
        } else { 
          paste(names(out), stringr::str_replace_all(fnct,"[.]|::","_"), x            , sep= fixed_sep) -> names(out)
        }   
      } else { # no whiches argument e.g. na.locf
        if(prefix) { 
          paste(            stringr::str_replace_all(fnct,"[.]|::","_"),    names(out), sep= fixed_sep) -> names(out)
        } else { 
          paste(names(out), stringr::str_replace_all(fnct,"[.]|::","_")               , sep= fixed_sep) -> names(out)
        }   
      } 
      # edge case: a column name is null: possible! allowed!: typical case send: xts(,index(<something>))
      stringr::str_replace_all(names(out),"^[.]","") -> names(out)
      
      return(out)
    })
    
    # GREAT ( BUT )
    # works well in the situation that the row.names(indexes) are ... 
    # EXACTLY the same as INPUT ( and the same as EACH other )
    # e.g. to.monthy will reduce the indexes
    # 
    # RETs <- data.frame(rlist::list.flatten(RETs))
    
    # make mergeable ( upgraded to data type )
    # 2nd layer of list is promoted to the first level
    RETsUGtDT <- lapply(rlist::list.ungroup(RETs), function(x) { 
     
       # zoo, xts
      if(!is.null(indexClass(INPUT))) {
        if(indexClass(INPUT) == "Date") {                                        # Global
           ret <- as.xts(x, DescTools::DoCall(paste0("as.",indexClass(INPUT)),list(row.names(x)))                       )
        } else { # could be? POSIXct, POSIXlt
           ret <- as.xts(x, DescTools::DoCall(paste0("as.",indexClass(INPUT)),list(row.names(x))), tzone = indexTZ(INPUT))
        }
      } else { # something else - prepare to merge by "index"
        ret <- plyr::mutate(x, index = row.names(x))
      }
      
      return(ret)
    
    })
    
    # S3 dispatch xts:::merge.xts
    RETs <- DescTools::DoCall("merge", RETsUGtDT )
    
    x_result <- RETs
    
    # Should have always made it here
    if(x_try.xts_success) { 
      xts::reclass(x_result, x_orig) 
    } -> x_result
    
    # if I did not do this earlier / failsafe / Really should have every only been xts/Date
    if(inherits(x_result,"zoo")) index(x_result) <- zoo::as.Date(index(x_result))
    
    xts_inner <- x_result
    
    Sys.setenv(TZ=oldtz)
    options(ops)
     
    return(xts_inner)
    
  }
  
  expand_xts_ <- expand_xts_inner(x = x, fnct =  fnct, whiches = whiches, alt_name = alt_name, o_args = o_args, prefix = prefix, fixed_sep = fixed_sep)
    
  Sys.setenv(TZ=oldtz)
  options(ops)

  return(expand_xts_)

}


# goodsight01.R
# expand_xts

# 

tests <- function() {

rm(list=setdiff(ls(all.names=TRUE),c("sample_xts","unrate","unrate_40s", "ibm", "ibm_missing_data"))); source(paste0(getwd(),"/","goodsight01.R"))

  if(!exists("sample_xts")) {
    require(quantmod) # puts in path zoo, then xts
    data(sample_matrix)  # from package xts
    # sample_xts <- as.xts(sample_matrix) 
    sample_xts <- structure(c(50.0397819115463, 50.2304961977954, 50.420955209067, 
                      50.3734680543285, 50.2443255196795, 50.1321122972067, 50.0355467742705, 
                      49.9948860954217, 49.9122834457642, 49.8852887132391, 50.2125821224916, 
                      50.3238453485025, 50.4635862266585, 50.6172355897254, 50.620241173435, 
                      50.7414981135498, 50.4805101188755, 50.4138129108681, 50.3532310036568, 
                      50.1618813949374, 50.3600836896748, 50.0396626712588, 50.1095259574076, 
                      50.2073807897632, 50.1600826681477, 50.0604060861532, 49.9658575219043, 
                      49.8562442241656, 49.8547727815966, 50.0704896508826, 50.2244845229791, 
                      50.4450306721515, 50.3721857137319, 50.4818267518265, 50.523893828191, 
                      50.7166067461433, 50.4932188302243, 50.5853098172462, 50.8333117106257, 
                      50.6892288304174, 50.6784905984176, 50.8898983760854, 50.9005579809156, 
                      50.9528277211339, 51.0633014723722, 51.1287929285077, 50.9772173544539, 
                      51.1841370498561, 51.295021248152, 51.1372463340331, 50.9293966227967, 
                      50.7211101780551, 50.843921143575, 50.7835976504561, 50.7896013042288, 
                      50.8816812795241, 50.7433266638814, 50.6943485629562, 50.8162030132845, 
                      50.6097967963997, 50.7324080081653, 50.3927259270933, 50.2650069647869, 
                      50.2746373900041, 50.1445762543741, 49.9314929048729, 49.9237734835486, 
                      49.7936997386099, 49.8306222453665, 49.8276255602479, 49.6962809106445, 
                      49.3626991517068, 49.5737411878039, 49.4489956783527, 49.5566612908277, 
                      49.2977796860981, 49.6274672147247, 49.5952884965034, 49.4976451429029, 
                      49.4230642938896, 49.2728079548362, 48.8663474774338, 48.5064888202386, 
                      48.3421024422286, 48.2524825106404, 48.3309000601732, 48.5923613368859, 
                      48.7456190083389, 48.9561619747141, 48.9440668539277, 48.9048836271181, 
                      49.0607100610899, 49.2257899841075, 49.4143540745659, 49.3362124203641, 
                      49.4517023124334, 49.5433780166351, 49.444289050952, 49.55703548735, 
                      49.7454978974624, 49.7507941077393, 49.7070824171387, 49.7415394908976, 
                      49.7470725636594, 49.7491522227102, 49.846975262063, 49.9379395474831, 
                      50.0244086177575, 49.7604232552535, 49.989541304844, 50.3120333961183, 
                      50.3200947473636, 49.8734026465211, 49.7338525835356, 49.8906358262349, 
                      49.8053048313893, 49.5468840391121, 49.3028935291798, 49.1382464427998, 
                      49.3457214583033, 49.4706167078959, 49.4632789128586, 49.5996297158354, 
                      49.3842789199786, 49.1660587256141, 49.4918780475782, 49.1328177423567, 
                      49.1773905424881, 48.8347914561581, 48.2545634885324, 47.968125028333, 
                      48.0554956281431, 47.644692571948, 47.6064724380648, 47.7206509536914, 
                      47.7942981789561, 47.6501318131713, 47.6555190311968, 47.5621026686154, 
                      47.9658214031213, 47.8183040302612, 47.9359302489862, 47.8904133721512, 
                      47.9823372258737, 48.1452112358212, 48.0101843888937, 47.9014160029841, 
                      47.6566546513431, 47.7886603971671, 47.8284532662533, 47.7443173931521, 
                      47.6022347047868, 47.7121494165901, 47.5151648819698, 47.4109038423752, 
                      47.3658120027045, 47.420985190709, 47.4844872933481, 47.3866882554947, 
                      47.7489862263065, 47.2780696035328, 47.194106912618, 47.4613546517325, 
                      47.4327917992174, 47.3330584316802, 47.3645229105516, 47.2478260931866, 
                      47.4346993610725, 47.4605472647125, 47.7112586675401, 47.7101167781098, 
                      47.5684886049305, 47.2287315871627, 47.2399558001892, 47.2047065698809, 
                      47.4430011919908, 47.6232337843679, 47.6760351295041, 47.6362885503127, 
                      47.6746805308509, 50.1177772350145, 50.421876002125, 50.420955209067, 
                      50.3734680543285, 50.2443255196795, 50.2156114607262, 50.1036323973342, 
                      49.9948860954217, 50.130525854306, 50.239103712694, 50.359801769449, 
                      50.4799955346125, 50.62395479761, 50.6858336400418, 50.7373052873552, 
                      50.7733564529701, 50.6071189691962, 50.5562734759594, 50.3532310036568, 
                      50.4208972428836, 50.4387454378126, 50.1696064309279, 50.2694187981265, 
                      50.2826849790742, 50.1600826681477, 50.0977658473744, 50.0021694838495, 
                      49.930381203448, 50.0218043126519, 50.2257790634706, 50.4137614515346, 
                      50.5348977915646, 50.4691169535685, 50.5550923533772, 50.6978278150252, 
                      50.7166067461433, 50.696932712043, 50.8473366461113, 50.896827622917, 
                      50.726959655694, 50.91776061924, 50.9665340950417, 51.0029854532602, 
                      51.0469861312818, 51.114008680003, 51.1287929285077, 51.1365256784907, 
                      51.3209005779334, 51.323424806756, 51.1494018613584, 50.9293966227967, 
                      50.8659669412615, 50.9694609683283, 50.8645263373624, 50.9318744899078, 
                      50.8816812795241, 50.7890893188517, 50.770905453827, 50.8162030132845, 
                      50.7206096320916, 50.7324080081653, 50.4088124653177, 50.3404988869915, 
                      50.3201916131904, 50.2027764867291, 50.0036413740678, 49.9237734835486, 
                      49.8898404553457, 49.8829457681315, 49.9031130358205, 49.7086250880523, 
                      49.5373488863704, 49.6231039012457, 49.6528522333934, 49.5566612908277, 
                      49.6785736159034, 49.6540715276625, 49.6200349582181, 49.5396073979361, 
                      49.4230642938896, 49.2728079548362, 48.8663474774338, 48.5064888202386, 
                      48.4463705919766, 48.4157189260319, 48.5359461762133, 48.6998847605209, 
                      49.0021812307077, 49.097283039653, 48.9781600566663, 49.0839954270775, 
                      49.245253833598, 49.3733496216003, 49.4143540745659, 49.4189994859189, 
                      49.6095030005936, 49.5896809020894, 49.5023417359146, 49.7877576550563, 
                      49.8192485602815, 49.7546976892052, 49.853322329816, 49.7733970805046, 
                      49.7934102661747, 49.8628865946235, 49.954558677343, 50.0720779787385, 
                      50.0299098586477, 49.9284741862485, 50.2012292399203, 50.337807096784, 
                      50.3200947473636, 49.901838155481, 49.8862227667758, 49.8906358262349, 
                      49.8053048313893, 49.5549664935449, 49.3028935291798, 49.3397448231728, 
                      49.5263478188825, 49.4706167078959, 49.6909716597571, 49.5996297158354, 
                      49.4026580314052, 49.4599885580201, 49.4918780475782, 49.2550693689544, 
                      49.1773905424881, 48.8454913822302, 48.2545634885324, 48.032858055938, 
                      48.0554956281431, 47.7250498159334, 47.7405271861058, 47.9071682801885, 
                      47.7942981789561, 47.7511652769296, 47.7798557548305, 47.9308458230391, 
                      48.029033942613, 47.9482531441853, 48.0824246165765, 48.0307726129326, 
                      48.1754323314596, 48.1452112358212, 48.0216564098931, 47.9339798198289, 
                      47.8934158707544, 47.9326701523078, 47.8404415298449, 47.7443173931521, 
                      47.7454243122342, 47.7121494165901, 47.5354500689903, 47.482170679888, 
                      47.4123313828473, 47.5063692973413, 47.5308912760639, 47.7476950731972, 
                      47.7489862263065, 47.3088378144923, 47.4183383185878, 47.5200446580673, 
                      47.4327917992174, 47.4049049093854, 47.4046284008614, 47.4724863618123, 
                      47.5633569665608, 47.7335250775606, 47.8175924066148, 47.7101167781098, 
                      47.5926598454947, 47.2477128773392, 47.3028651846619, 47.4277219359799, 
                      47.6161097730372, 47.7167299732728, 47.7046027771086, 47.775634540576, 
                      47.9412667206186, 49.9504146442813, 50.2304961977954, 50.2641365663597, 
                      50.2210338242788, 50.1112075487643, 49.9918544993091, 49.9697097184701, 
                      49.8045358090165, 49.9122834457642, 49.8852887132391, 50.1717574121646, 
                      50.3238453485025, 50.4635862266585, 50.4735909825374, 50.5662652106609, 
                      50.449321357316, 50.4026921292911, 50.4127813388677, 50.0214153820363, 
                      50.1604405513317, 50.2112913039383, 50.0366991631639, 50.063868338926, 
                      50.1291318545355, 49.9405191078187, 49.9726670534334, 49.8746804186122, 
                      49.7630844122028, 49.7724223081099, 50.0704896508826, 50.191012164208, 
                      50.3606408837506, 50.298799757062, 50.4020264816621, 50.459767560449, 
                      50.4986480384182, 50.4932188302243, 50.5853098172462, 50.6768581674052, 
                      50.6070733856372, 50.6784905984176, 50.8360446014901, 50.8793493835529, 
                      50.803171907551, 50.9468108768572, 51.0061335218936, 50.952600190595, 
                      51.1371299879095, 51.1352419149073, 50.9352261443787, 50.6987973947919, 
                      50.6571814189883, 50.7305961336348, 50.7669213141213, 50.7896013042288, 
                      50.7548084168209, 50.6187434734297, 50.5988137418271, 50.5645087290095, 
                      50.5080789844205, 50.4092945808094, 50.2492157707179, 50.2650069647869, 
                      50.1638044184635, 49.9138089296028, 49.8489346112531, 49.7424150559387, 
                      49.7038514071734, 49.760310812525, 49.670494239676, 49.3792415099661, 
                      49.3074568226063, 49.3987553413956, 49.4241619504068, 49.3356413900106, 
                      49.2977796860981, 49.5160420086702, 49.4232068118256, 49.4160965263844, 
                      49.3118358542017, 48.9309517223527, 48.5268371665895, 48.3340927379523, 
                      48.2896926777253, 48.2364771063669, 48.3309000601732, 48.5743179732927, 
                      48.7456190083389, 48.9561619747141, 48.8096186482138, 48.9048836271181, 
                      48.9692750768163, 49.1991258503583, 49.3064111174296, 49.3362124203641, 
                      49.4517023124334, 49.4180628219513, 49.3382782378181, 49.55703548735, 
                      49.7454978974624, 49.6173171733521, 49.6924492458175, 49.7015885221842, 
                      49.662988150532, 49.7109114979342, 49.7775439894587, 49.9248422973257, 
                      49.8394501870147, 49.6980794321414, 49.989541304844, 50.2478765013235, 
                      49.8757386019976, 49.7276899923422, 49.7338525835356, 49.7489871076424, 
                      49.5081397146114, 49.2918633073164, 49.0567561810582, 49.1149971640139, 
                      49.3457214583033, 49.3426131232972, 49.4632789128586, 49.4137530585047, 
                      49.1031043568417, 49.1660587256141, 49.1357230365042, 49.1328177423567, 
                      48.7270762056073, 48.3800124679673, 47.9690434191124, 47.8926179087975, 
                      47.6620855703505, 47.5821240912087, 47.5179560866739, 47.7091322313329, 
                      47.5513987007966, 47.6501318131713, 47.6053615081546, 47.5621026686154, 
                      47.7807235394881, 47.811546625574, 47.8876326500158, 47.8841269246251, 
                      47.9450716031323, 47.9264941691766, 47.9019342884565, 47.6471764517754, 
                      47.6544603143575, 47.7886603971671, 47.7377986168352, 47.5482040904643, 
                      47.5679622276754, 47.501976670636, 47.3234230750255, 47.211163022771, 
                      47.2330580059869, 47.3532002325846, 47.4281355831578, 47.3866882554947, 
                      47.2868487885989, 47.1466018260288, 47.1815348543221, 47.4308266385453, 
                      47.3348965212827, 47.2615736095983, 47.2605645709109, 47.2478260931866, 
                      47.3642401566334, 47.4605472647125, 47.668434910213, 47.6110642732936, 
                      47.3254940958526, 47.0914419882576, 47.2093194619751, 47.1340495076106, 
                      47.4430011919908, 47.6001511393257, 47.5724060421453, 47.6173290583887, 
                      47.6746805308509, 50.1177772350145, 50.3976663383861, 50.3323571013133, 
                      50.3345948961691, 50.1811210391819, 49.9918544993091, 49.988063256042, 
                      49.913329335608, 49.9724600550638, 50.239103712694, 50.2851940807548, 
                      50.4128602347002, 50.6014452374366, 50.4891214813319, 50.6783465243683, 
                      50.4864408302735, 50.5763196086684, 50.4127813388677, 50.0214153820363, 
                      50.4208972428836, 50.2112913039383, 50.1696064309279, 50.2314483010655, 
                      50.2433369933702, 50.070242540723, 50.0109145160256, 49.8809633637625, 
                      49.9187495029809, 50.0218043126519, 50.2257790634706, 50.3578435161537, 
                      50.3692756978625, 50.4310899203875, 50.5550923533772, 50.6978278150252, 
                      50.4986480384182, 50.6061055576287, 50.8138255864256, 50.6768581674052, 
                      50.695622055169, 50.9115985194411, 50.9665340950417, 50.9010550008819, 
                      51.0469861312818, 51.0518465248395, 51.0216357806733, 51.1365256784907, 
                      51.1515142172957, 51.1789930735369, 50.9352261443787, 50.7732543242554, 
                      50.8659669412615, 50.7649826990875, 50.7953382903315, 50.8477621406292, 
                      50.7548084168209, 50.6920604718023, 50.770905453827, 50.570749692632, 
                      50.6155918666066, 50.4103286523541, 50.3263588179421, 50.2956702271079, 
                      50.1638044184635, 49.9138089296028, 49.9183871340213, 49.8071204530403, 
                      49.8869833529875, 49.78805814558, 49.7403315550003, 49.3792415099661, 
                      49.5373488863704, 49.496003986535, 49.5950011884777, 49.3471431527284, 
                      49.6546261076859, 49.5458954022922, 49.5068963631111, 49.5180739210179, 
                      49.3968738599656, 48.9309517223527, 48.5268371665895, 48.3397338277091, 
                      48.2896926777253, 48.30850892202, 48.5359461762133, 48.6998847605209, 
                      48.9354567345693, 48.974901416968, 48.8703195118646, 49.0631647734845, 
                      49.245253833598, 49.3473606787059, 49.3377557822211, 49.4189994859189, 
                      49.5381883056603, 49.4180628219513, 49.5023417359146, 49.7698448660165, 
                      49.7462345946245, 49.7299649322951, 49.7333931653161, 49.7555215783115, 
                      49.7094151808194, 49.8388590726778, 49.954558677343, 50.0720779787385, 
                      49.8394501870147, 49.9110274504956, 50.2012292399203, 50.3255614801967, 
                      49.8853882678616, 49.7276899923422, 49.8847151942606, 49.7920112943787, 
                      49.5081397146114, 49.2918633073164, 49.1352904471748, 49.3397448231728, 
                      49.4713813499697, 49.3852052435403, 49.5867696788652, 49.4137530585047, 
                      49.1031043568417, 49.4599885580201, 49.1357230365042, 49.1893012571989, 
                      48.7270762056073, 48.3800124679673, 47.9690434191124, 48.0193547288843, 
                      47.6620855703505, 47.6592988835332, 47.726862406458, 47.8668317346629, 
                      47.6293786041021, 47.6842280532209, 47.6053615081546, 47.9308458230391, 
                      47.7807235394881, 47.8294556467246, 47.9006826430557, 48.0113049442773, 
                      48.1605755467172, 47.9961340046721, 47.9019342884565, 47.6471764517754, 
                      47.8725213360129, 47.8329083126244, 47.7377986168352, 47.6512305894323, 
                      47.7256907018078, 47.501976670636, 47.3764237289667, 47.2292952802724, 
                      47.4004820908595, 47.4526174659862, 47.4835950385865, 47.7476950731972, 
                      47.2868487885989, 47.1466018260288, 47.4183383185878, 47.4308266385453, 
                      47.3488381779215, 47.3677901566039, 47.2605645709109, 47.3952135245457, 
                      47.3642401566334, 47.6722018616947, 47.668434910213, 47.6292144787367, 
                      47.3254940958526, 47.2477128773392, 47.2276437542817, 47.4277219359799, 
                      47.6161097730372, 47.6276905335225, 47.6071583256498, 47.6647123051268, 
                      47.7671937777376), .Dim = c(180L, 4L), .Dimnames = list(NULL, 
                          c("Open", "High", "Low", "Close")), index = structure(c(1167717600, 
                      1167804000, 1167890400, 1167976800, 1168063200, 1168149600, 1168236000, 
                      1168322400, 1168408800, 1168495200, 1168581600, 1168668000, 1168754400, 
                      1168840800, 1168927200, 1169013600, 1169100000, 1169186400, 1169272800, 
                      1169359200, 1169445600, 1169532000, 1169618400, 1169704800, 1169791200, 
                      1169877600, 1169964000, 1170050400, 1170136800, 1170223200, 1170309600, 
                      1170396000, 1170482400, 1170568800, 1170655200, 1170741600, 1170828000, 
                      1170914400, 1171000800, 1171087200, 1171173600, 1171260000, 1171346400, 
                      1171432800, 1171519200, 1171605600, 1171692000, 1171778400, 1171864800, 
                      1171951200, 1172037600, 1172124000, 1172210400, 1172296800, 1172383200, 
                      1172469600, 1172556000, 1172642400, 1172728800, 1172815200, 1172901600, 
                      1172988000, 1173074400, 1173160800, 1173247200, 1173333600, 1173420000, 
                      1173506400, 1173592800, 1173675600, 1173762000, 1173848400, 1173934800, 
                      1174021200, 1174107600, 1174194000, 1174280400, 1174366800, 1174453200, 
                      1174539600, 1174626000, 1174712400, 1174798800, 1174885200, 1174971600, 
                      1175058000, 1175144400, 1175230800, 1175317200, 1175403600, 1175490000, 
                      1175576400, 1175662800, 1175749200, 1175835600, 1175922000, 1176008400, 
                      1176094800, 1176181200, 1176267600, 1176354000, 1176440400, 1176526800, 
                      1176613200, 1176699600, 1176786000, 1176872400, 1176958800, 1177045200, 
                      1177131600, 1177218000, 1177304400, 1177390800, 1177477200, 1177563600, 
                      1177650000, 1177736400, 1177822800, 1177909200, 1177995600, 1178082000, 
                      1178168400, 1178254800, 1178341200, 1178427600, 1178514000, 1178600400, 
                      1178686800, 1178773200, 1178859600, 1178946000, 1179032400, 1179118800, 
                      1179205200, 1179291600, 1179378000, 1179464400, 1179550800, 1179637200, 
                      1179723600, 1179810000, 1179896400, 1179982800, 1180069200, 1180155600, 
                      1180242000, 1180328400, 1180414800, 1180501200, 1180587600, 1180674000, 
                      1180760400, 1180846800, 1180933200, 1181019600, 1181106000, 1181192400, 
                      1181278800, 1181365200, 1181451600, 1181538000, 1181624400, 1181710800, 
                      1181797200, 1181883600, 1181970000, 1182056400, 1182142800, 1182229200, 
                      1182315600, 1182402000, 1182488400, 1182574800, 1182661200, 1182747600, 
                      1182834000, 1182920400, 1183006800, 1183093200, 1183179600), tzone = "", tclass = c("POSIXct", 
                      "POSIXt")), .indexCLASS = c("POSIXct", "POSIXt"), tclass = c("POSIXct", 
                      "POSIXt"), .indexTZ = "", tzone = "", class = c("xts", "zoo"))
                          
    rm(sample_matrix) # TEMP for creation
  }
  
  if(!exists("unrate")) {
      # unrate <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1948-03-01"]
      unrate <- structure(c(3.4, 3.8, 4), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", src = "FRED", updated = structure(1513478863.11855, class = c("POSIXct", 
                    "POSIXt")), class = c("xts", "zoo"), index = structure(c(-694310400, 
                    -691632000, -689126400), tzone = "UTC", tclass = "Date"), .Dim = c(3L, 
                    1L), .Dimnames = list(NULL, "UNRATE"))
        }
  if(!exists("unrate_40s")) {
    # unrate_40s <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1949-03-01"]
    unrate_40s <- structure(c(3.4, 3.8, 4, 3.9, 3.5, 3.6, 3.6, 3.9, 3.8, 3.7, 3.8, 
                      4, 4.3, 4.7, 5), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", src = "FRED", updated = structure(1513478863.19573, class = c("POSIXct", 
                      "POSIXt")), class = c("xts", "zoo"), index = structure(c(-694310400, 
                      -691632000, -689126400, -686448000, -683856000, -681177600, -678585600, 
                      -675907200, -673228800, -670636800, -667958400, -665366400, -662688000, 
                      -660009600, -657590400), tzone = "UTC", tclass = "Date"), .Dim = c(15L, 
                      1L), .Dimnames = list(NULL, "UNRATE"))
  }
    
  if(!exists("ibm") || !exists("ibm_missing_data")) {
    # ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
    ibm <- structure(c(18.225, 18.299999, 18.4125, 18.424999, 18.4375, 18.475, 
                18.450001, 18.2875, 18.4125, 18.450001, 18.4375, 18.475, 18.525, 
                18.487499, 18.200001, 18.299999, 18.3125, 18.3125, 18.375, 18.424999, 
                18.387501, 18.237499, 18.4125, 18.424999, 18.4375, 18.475, 18.450001, 
                18.387501, 315200, 424000, 488000, 457600, 707200, 585600, 379200, 
                5.321311, 5.372376, 5.376019, 5.379663, 5.390608, 5.383314, 5.365078
                ), index = structure(c(86400, 345600, 432000, 518400, 604800, 
                691200, 950400), tzone = "UTC", tclass = "Date"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", src = "yahoo", updated = structure(1513478863.4307, class = c("POSIXct", 
                "POSIXt")), .Dim = c(7L, 6L), .Dimnames = list(NULL, c("IBM.Open", 
                "IBM.High", "IBM.Low", "IBM.Close", "IBM.Volume", "IBM.Adjusted"
                )), class = c("xts", "zoo"))
    ibm_missing_data <- local({ t <- ibm; t[2:3,1] <- NA_real_; t})
  }

  require(testthat)
  context("expanded xts function versions")
  
  test_that("package version tests", {
    
    expect_equal(R.Version()$version.string, "R version 3.4.3 (2017-11-30)")

  })

  test_that("package function get_na_locfl", {
    
    expect_equal(
      get_na_locfl( c(101,NA,NA,NA,102,NA,NA), n = 2)
    , c(101, 101, 101, NA, 102, 102, 102)
    )
    
    expect_equal(
        get_na_locfl(xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7))), n = 2)
      , structure(c(101, 101, 101, NA, 102, 102, 102), index = structure(c(10, 
        20, 30, 40, 50, 60, 70), class = "Date"), class = "zoo", .Dim = c(7L, 
        1L), .Dimnames = list(NULL, "na_locfl"))
    )


  })
  
  test_that("package function get_pctchg_xts", {
  
    expect_equal(
        get_pctchg_xts(head(sample_xts[,"Open"],4), n = 1)
      , structure(c(NA, 0.381125334611203, 0.37917007732041, -0.0941813865715212
        ), .indexCLASS = "Date", tclass = c("POSIXct", "POSIXt"), .indexTZ = "", tzone = "", index = structure(c(1167696000, 
        1167782400, 1167868800, 1167955200), tclass = "Date", tzone = "UTC"), .Dim = c(4L, 
        1L), .Dimnames = list(NULL, "pctchg"), class = c("xts", "zoo"))
    )
    
    expect_equal(
        get_pctchg_xts(head(sample_xts[,"Open"],4), n = 1, to_future = TRUE )
      , structure(c(-0.381125334611203, -0.37917007732041, 0.0941813865715212, 
        NA), .indexCLASS = "Date", tclass = c("POSIXct", "POSIXt"), .indexTZ = "", tzone = "", index = structure(c(1167696000, 
        1167782400, 1167868800, 1167955200), tclass = "Date", tzone = "UTC"), .Dim = c(4L, 
        1L), .Dimnames = list(NULL, "pctchg"), class = c("xts", "zoo"))
    )
  
  })
  
  test_that("package function get_recent_min/max_xts", {
    
    expect_equal(
        get_recent_min_xts(head(sample_xts[,"Open"],4), 2)
      , structure(c(50.0397819115463, 50.0397819115463, 50.2304961977954, 
        50.3734680543285), .Dim = c(4L, 1L), .Dimnames = list(NULL, "recent_min"), index = structure(c(1167696000, 
        1167782400, 1167868800, 1167955200), tclass = "Date", tzone = "UTC"), .indexCLASS = "Date", .indexTZ = "", tclass = c("POSIXct", 
        "POSIXt"), tzone = "", class = c("xts", "zoo"))
    )
    
    expect_equal(
        get_recent_min_xts(head(sample_xts[,"Open"],4), 2)
      , structure(c(50.0397819115463, 50.0397819115463, 50.2304961977954, 
        50.3734680543285), .Dim = c(4L, 1L), .Dimnames = list(NULL, "recent_min"), index = structure(c(1167696000, 
        1167782400, 1167868800, 1167955200), tclass = "Date", tzone = "UTC"), .indexCLASS = "Date", .indexTZ = "", tclass = c("POSIXct", 
        "POSIXt"), tzone = "", class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function get_annualized_xts", {
    
    expect_equal(
        get_annualized_xts(head(sample_xts[,"Open"],4), 2)
      , structure(c(100.079563823093, 100.460992395591, 100.841910418134, 
        100.746936108657), .indexCLASS = "Date", tclass = c("POSIXct", 
        "POSIXt"), .indexTZ = "", tzone = "", index = structure(c(1167696000, 
        1167782400, 1167868800, 1167955200), tclass = "Date", tzone = "UTC"), .Dim = c(4L, 
        1L), .Dimnames = list(NULL, "annualized"), class = c("xts", "zoo"))
    )
    
  })
    
  test_that("package function get_sma_xts", {

    expect_equal(
      get_sma_xts(head(sample_xts[,"Open"],4), 2)
    , structure(c(NA, 50.1351390546708, 50.3257257034312, 50.3972116316978
      ), .Dim = c(4L, 1L), .Dimnames = list(NULL, "sma"), index = structure(c(1167696000, 
      1167782400, 1167868800, 1167955200), tclass = "Date", tzone = "UTC"), .indexCLASS = "Date", .indexTZ = "", tclass = c("POSIXct", 
      "POSIXt"), tzone = "", class = c("xts", "zoo"))
    )
    
  })

  test_that("package function get_pctchg_xts", {

    expect_equal(
        get_pctchg_xts(head(sample_xts[,"Open"],6), n = 1)
      , structure(c(NA, 0.381125334611203, 0.37917007732041, -0.0941813865715212, 
        -0.256370148090095, -0.223335115582047), .indexCLASS = "Date", tclass = c("POSIXct", 
        "POSIXt"), .indexTZ = "", tzone = "", index = structure(c(1167696000, 
        1167782400, 1167868800, 1167955200, 1168041600, 1168128000), tclass = "Date", tzone = "UTC"), .Dim = c(6L, 
        1L), .Dimnames = list(NULL, "pctchg"), class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function get_smtsortino_xts", {

    expect_equal(
        get_smtsortino_xts(get_pctchg_xts(head(sample_xts[,"Open"],6), n = 1), 3)
      , structure(c(NA, NA, NA, 4.0834088969436, 0.0735621122604118, 
        -2.23189380084301), .Dim = c(6L, 1L), .Dimnames = list(NULL, 
        "smtsortino"), index = structure(c(1167696000, 1167782400, 
        1167868800, 1167955200, 1168041600, 1168128000), tclass = "Date", tzone = "UTC"), .indexCLASS = "Date", .indexTZ = "UTC", tclass = "Date", tzone = "UTC", 
        class = c("xts", "zoo"))
    )
    
    expect_equal(
        get_smtsortino_xts(get_pctchg_xts(head(sample_xts[,"Open"],11), n = 1), 3)
      , structure(c(NA, NA, NA, 4.0834088969436, 0.0735621122604118, 
        -2.23189380084301, -7.02953432626857, -2.21716344886893, -2.52257636177122, 
        -1.72932700195373, 1.72840165854429), .Dim = c(11L, 1L), .Dimnames = list(
        NULL, "smtsortino"), index = structure(c(1167696000, 1167782400, 
        1167868800, 1167955200, 1168041600, 1168128000, 1168214400, 1168300800, 
        1168387200, 1168473600, 1168560000), tclass = "Date", tzone = "UTC"), .indexCLASS = "Date", .indexTZ = "UTC", tclass = "Date", tzone = "UTC", 
        class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function get_smrank_xts", {

    expect_equal(
        get_smrank_xts(get_smtsortino_xts(get_pctchg_xts(head(sample_xts[,"Open"],11), n = 1), 3), n = 4, n_ranks = 2)
      , structure(c(NA, NA, NA, NA, NA, NA, 2, 1, 2, 1, 1), .Dim = c(11L, 
        1L), .Dimnames = list(NULL, "smrank"), index = structure(c(1167696000, 
        1167782400, 1167868800, 1167955200, 1168041600, 1168128000, 1168214400, 
        1168300800, 1168387200, 1168473600, 1168560000), tclass = "Date", tzone = "UTC"), .indexCLASS = "Date", .indexTZ = "UTC", tclass = "Date", tzone = "UTC", 
        class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function get_collofdays2daily_xts", {

    expect_equal(
        get_collofdays2daily_xts(xts(c(11,13,15),zoo::as.Date(c(1,3,5))))
      , structure(c(11, NA, 13, NA, 15), .Dim = c(5L, 1L), .Dimnames = list(
        NULL, "collofdays2daily"), index = structure(c(86400, 172800, 
        259200, 345600, 432000), tzone = "UTC", tclass = "Date"), .indexCLASS = "Date", .indexTZ = "UTC", tclass = "Date", tzone = "UTC", 
        class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function get_delay_since_last_obs", {

    expect_equal(
        get_delay_since_last_obs(c(101,NA,NA,NA,102,NA,NA))
      , c(0, 1, 2, 3, 0, 1, 2)
    )
    
  })
  
  test_that("package function get_delay_since_last_obs_xts", {

    expect_equal(
        get_delay_since_last_obs_xts(xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
      , structure(c(0, 1, 2, 3, 0, 1, 2), .Dim = c(7L, 1L), index = structure(c(864000, 
        1728000, 2592000, 3456000, 4320000, 5184000, 6048000), tzone = "UTC", tclass = "Date"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", .Dimnames = list(
        NULL, "delay_since_last_obs"), class = c("xts", "zoo"))
    )
    
  })
  
  # Warning in if (class(merged) %in% c("xts", "zoo")) index(merged) <- zoo::as.Date(index(merged)) :
  #   the condition has length > 1 and only the first element will be used
  test_that("package function get_delay_since_last_day_xts", {

    expect_equal(
          suppressWarnings(get_delay_since_last_day_xts(xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10)))))
        , structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
          15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 
          31, 32, 33, 34, 35, 36, 37, 38, 39, 0, 1, 2, 3, 4, 5, 6, 7, 8, 
          9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), .Dim = c(61L, 
          1L), index = structure(c(864000, 950400, 1036800, 1123200, 1209600, 
          1296000, 1382400, 1468800, 1555200, 1641600, 1728000, 1814400, 
          1900800, 1987200, 2073600, 2160000, 2246400, 2332800, 2419200, 
          2505600, 2592000, 2678400, 2764800, 2851200, 2937600, 3024000, 
          3110400, 3196800, 3283200, 3369600, 3456000, 3542400, 3628800, 
          3715200, 3801600, 3888000, 3974400, 4060800, 4147200, 4233600, 
          4320000, 4406400, 4492800, 4579200, 4665600, 4752000, 4838400, 
          4924800, 5011200, 5097600, 5184000, 5270400, 5356800, 5443200, 
          5529600, 5616000, 5702400, 5788800, 5875200, 5961600, 6048000
          ), tzone = "UTC", tclass = "Date"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", .Dimnames = list(
              NULL, "delay_since_last_day"), class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function is_na_xts", {

    expect_equal(
      is_na_xts(xts(c(11,NA,NA,14,NA),zoo::as.Date(1:5)))
      , structure(c(2, 1, 1, 2, 1), .Dim = c(5L, 1L), index = structure(c(86400, 
      172800, 259200, 345600, 432000), tzone = "UTC", tclass = "Date"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", .Dimnames = list(
          NULL, "na"), class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function rm_days_xts", {

    expect_equal(
          rm_days_xts(xts(1:17,zoo::as.Date("2007-01-01") -1 + 1:17), rm_what = c("Saturday", "Sunday", "BIZHOLIDAYS"))
       , structure(c(3L, 4L, 5L, 8L, 9L, 10L, 11L, 12L, 16L, 17L), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", index = structure(c(1167782400, 
         1167868800, 1167955200, 1168214400, 1168300800, 1168387200, 1168473600, 
         1168560000, 1168905600, 1168992000), tzone = "UTC", tclass = "Date"), .Dim = c(10L, 
         1L), .Dimnames = list(NULL, "days"), class = c("xts", "zoo"))
    )
    
  })
  
  test_that("package function are_nearby_fred_holidays_xts", {

    expect_equal(
        are_nearby_fred_holidays_xts(sample_xts[2:16,"Open"], d = c(-1,-2))
      , structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
        FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE), .Dim = c(15L, 
        1L), index = structure(c(1167782400, 1167868800, 1167955200, 
        1168041600, 1168128000, 1168214400, 1168300800, 1168387200, 1168473600, 
        1168560000, 1168646400, 1168732800, 1168819200, 1168905600, 1168992000
        ), tclass = "Date", tzone = "UTC"), .indexCLASS = "Date", .indexTZ = "", tclass = c("POSIXct", 
        "POSIXt"), tzone = "", .Dimnames = list(NULL, "nearby_fred_holidays"), class = c("xts", "zoo"))
    )
    
  })
  
  # PROBLEMS (LOOKS LIKE ROUNDING)
  test_that("package function do_reindex_xts", {

    # expect_equal(
    #     do_reindex_xts(unrate, 0:2)
    #   , structure(c(3.4, 3.8, 4), .indexCLASS = "Date", tclass = "Date", .indexTZ = structure("UTC", .Names = "TZ"), tzone = structure("UTC", .Names = "TZ"), src = "FRED", updated = structure(1513499226.82177, class = c("POSIXct", 
    #     "POSIXt")), class = c("xts", "zoo"), index = structure(c(0, 86400, 
    #     172800), tzone = structure("UTC", .Names = "TZ"), tclass = "Date"), .Dim = c(3L, 
    #     1L), .Dimnames = list(NULL, "UNRATE"))
    # )
    
    # expect_equal(
    #     do_reindex_xts(merge(unrate,unrate), 0:2)
    #   , structure(c(3.4, 3.8, 4, 3.4, 3.8, 4), class = c("xts", "zoo"
    #     ), .indexCLASS = "Date", .indexTZ = structure("UTC", .Names = "TZ"), tclass = "Date", tzone = structure("UTC", .Names = "TZ"), src = "FRED", updated = structure(1513494805.43047, class = c("POSIXct", 
    #     "POSIXt")), index = structure(c(0, 86400, 172800), tzone = structure("UTC", .Names = "TZ"), tclass = "Date"), .Dim = c(3L, 
    #     2L), .Dimnames = list(NULL, c("UNRATE", "UNRATE.1")))
    # )
    
  })
  
  # PROBLEMS (LOOKS LIKE ROUNDING)
  test_that("package function pushback_fred_1st_days_xts", {

    # expect_equal(
    #   pushback_fred_1st_days_xts(unrate)
    # , structure(c(3.4, 3.8, 4), class = c("xts", "zoo"), .indexCLASS = "Date", .indexTZ = structure("UTC", .Names = "TZ"), tclass = "Date", tzone = structure("UTC", .Names = "TZ"), src = "FRED", updated = structure(1513494805.43047, class = c("POSIXct", 
    #   "POSIXt")), index = structure(c(-694396800, -691718400, -689212800
    #   ), tzone = "UTC", tclass = "Date"), .Dim = c(3L, 1L), .Dimnames = list(
    #   NULL, "UNRATE"))
    # )
    
    # expect_equal(
    #   pushback_fred_1st_days_xts(merge(unrate,unrate)) 
    #   , structure(c(3.4, 3.8, 4, 3.4, 3.8, 4), class = c("xts", "zoo"
    #   ), .indexCLASS = "Date", .indexTZ = structure("UTC", .Names = "TZ"), tclass = "Date", tzone = structure("UTC", .Names = "TZ"), src = "FRED", updated = structure(1513494805.43047, class = c("POSIXct", 
    #   "POSIXt")), index = structure(c(-694396800, -691718400, -689212800
    #   ), tzone = "UTC", tclass = "Date"), .Dim = c(3L, 2L), .Dimnames = list(
    #   NULL, c("UNRATE", "UNRATE.1")))
    # )
    
  })
  
  # PROBLEM( BUT I DO NOT KNOW WHAT )
  test_that("package function is_year_less_than_or_equal_xts", {

    # expect_equal(
    #     is_year_less_than_or_equal_xts(unrate_40s, 1948)
    #   , structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2), .Dim = c(15L, 
    #     1L), index = structure(c(-694310400, -691632000, -689126400, 
    #     -686448000, -683856000, -681177600, -678585600, -675907200, -673228800, 
    #     -670636800, -667958400, -665366400, -662688000, -660009600, -657590400
    #     ), tzone = "UTC", tclass = "Date"), .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", src = "FRED", updated = structure(1513494805.5264, class = c("POSIXct", 
    #     "POSIXt")), .Dimnames = list(NULL, "year_less_than_or_equal_xts"), 
    #     class = c("xts", "zoo"))
    # )
    
  })
  

  test_that("package function expand_xts", {

    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = TRUE)  )
      , c("TTR_SMA.2.IBM.Open", "TTR_SMA.2.IBM.Close", "TTR_SMA.3.IBM.Open", "TTR_SMA.3.IBM.Close")
    )
    
    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = FALSE) )
      , c("IBM.Open.TTR_SMA.2", "IBM.Close.TTR_SMA.2", "IBM.Open.TTR_SMA.3", "IBM.Close.TTR_SMA.3")
    )

    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, fixed_sep = "_") )
      , c("IBM.Open_TTR_SMA_2", "IBM.Close_TTR_SMA_2", "IBM.Open_TTR_SMA_3", "IBM.Close_TTR_SMA_3")
    )

    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3) )
      , c("IBM.Open.TTR_SMA.2", "IBM.Close.TTR_SMA.2", "IBM.Open.TTR_SMA.3", "IBM.Close.TTR_SMA.3")
    )

    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = TTR::SMA, whiches = 2:3) )
      , c("IBM.Open.TTR_SMA.2", "IBM.Close.TTR_SMA.2", "IBM.Open.TTR_SMA.3", "IBM.Close.TTR_SMA.3")
    )

    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "function(x,n){ TTR::SMA(x,n) }", whiches = 2:3) )
      , c("IBM.Open.anon.2", "IBM.Close.anon.2", "IBM.Open.anon.3", "IBM.Close.anon.3")
    )

    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = function(x,n){ TTR::SMA(x,n) }, whiches = 2:3) )
      , c("IBM.Open.anon.2", "IBM.Close.anon.2", "IBM.Open.anon.3", "IBM.Close.anon.3")
    )

    expect_equal(
        colnames( expand_xts(ibm_missing_data[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3) )
      , c("IBM.Open.anon.2", "IBM.Close.anon.2", "IBM.Open.anon.3", "IBM.Close.anon.3")
    )

    expect_equal(
        colnames(  expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3, alt_name = "NALOCF") )
      , c("IBM.Open.NALOCF.2", "IBM.Close.NALOCF.2", "IBM.Open.NALOCF.3", "IBM.Close.NALOCF.3")
    )

    expect_equal(
        colnames( expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "get_pctchg_xts", whiches = 2:3, alt_name = "futPCTCHG" , o_args = c(to_future = TRUE )) )
      , c("IBM.Open.futPCTCHG.2", "IBM.Close.futPCTCHG.2", "IBM.Open.futPCTCHG.3", "IBM.Close.futPCTCHG.3")
    )

    expect_equal(
        expand_xts(ibm[,c("IBM.Open","IBM.Close")], fnct = "to.monthly", alt_name = "MONTHLY", o_args = list(indexAt= 'lastof', OHLC = FALSE))
      , structure(c(18.450001, 18.387501), .Dim = 1:2, .Dimnames = list(
        NULL, c("IBM.Open.MONTHLY", "IBM.Close.MONTHLY")), index = structure(2592000, tzone = "UTC", tclass = "Date"), class = c("xts", 
        "zoo"), .indexCLASS = "Date", .indexTZ = "UTC", tclass = "Date", tzone = "UTC")
    )

    expect_equal(
        colnames( expand_xts(xts(,index(ibm)), fnct = "is_year_less_than_or_equal_xts", whiches =  seq(lubridate::year(min(index(ibm))), lubridate::year(max(index(ibm))),by = 1), alt_name = "y_lth_or_eq_to_fact") )
      , "y_lth_or_eq_to_fact.1970"
    )
    
  })
  
}

# rm(list=setdiff(ls(all.names=TRUE),c("con","cid", "sample_xts","unrate","unrate_40s", "ibm", "ibm_missing_data"))); debugSource('W:/R-3.4._/finecon01.R');debugSource('W:/R-3.4._/goodsight01.R');debugSource('W:/R-3.4._/valuesight01.R');verify_connection();options(upsert_temp_is_temporary=Inf);Quandl::Quandl.api_key(api_key= "36igkU9Tthi6cGozFTgh");setDefaults(getSymbols.av, api.key="WN6SS6MSDDVU79RZ")
# tests()

# goodsight01.R
