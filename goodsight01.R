

# goodsight01.R

# in general, unless an exception is noted otherwise,
#   input x meant to be an xt object with index class of Date

# single column xts only (currently)
# multi  column xts (untried)

# last observation carried forard limited
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
  # x         <- if(any(class(x_try.xts) %in% "try-error")) { x_orig } else { x_try.xts_success <- TRUE; x_try.xts }

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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))

  locfl <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(locfl)
  
  # ORIG FROM
  # https://github.com/AndreMikulec/expressions/blob/8a910454ea590a30878e97b18d5e9dbe45a9d4fb/main-foresight3-999.R#L2287

}
# vector input
# get_na_locfl( c(101,NA,NA,NA,102,NA,NA), n = 2)
# [1] 101 101 101  NA 102 102 102
#
# require(xts)
# xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7)))
#            [,1]
# 1970-01-11  101
# 1970-01-21   NA
# 1970-01-31   NA
# 1970-02-10   NA
# 1970-02-20  102
# 1970-03-02   NA
# 1970-03-12   NA
#
# get_na_locfl( xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7))), n = 2)
#         na_locfl
# 1970-01-11   101
# 1970-01-21   101
# 1970-01-31   101
# 1970-02-10    NA
# 1970-02-20   102
# 1970-03-02   102
# 1970-03-12   102


# PROB WILL NOT USE
# 
# uses <- function(programmed_in_R_version, explicit_package_function_calls, matched_call = NULL) {
# 
#   running_in_R_version <- paste(R.Version()$major,R.Version()$minor, sep = ".")
#   # programmed_in_R_version # "3.4.0"
# 
#   if(running_in_R_version  != programmed_in_R_version) {
#     warning(paste0("Running R is ", running_in_R_version, " but Programmed in R is ", programmed_in_R_version ) )
#   }
# 
#   # EVERY ONE that I explicitly CALL
#   # 
#   uses_packages <- explicit_package_function_calls
#   packages_not_found <- uses_packages[!uses_packages %in% installed.packages()]
#   
#   if(length(packages_not_found)) warning(paste0("Missing package(s) not found in .libPaths: ",paste0(.libPaths(), collapse = ", ")))
#   for(packages_not_found_i in seq_along(packages_not_found)) { 
#     warning(paste0("Package ", packages_not_found[packages_not_found_i]," is not found.")) 
#     if(length(packages_not_found) == packages_not_found_i) stop(paste0("STOPPED in", matched_call))
#   }
#   
#   return(TRUE)
# 
# }
# # f <- function() {
# #   matched_call <- capture.output(str(match.call()))
# #   uses("3.4.1",c("zoo","xts","rlist","stringr","DescTools","plyr","DataCombine"), matched_call)
# # }
# # f()
# # Warning message:
# # In uses("3.4.1", c("zoo", "xts", "rlist", "stringr", "DescTools",  :
# #   Running R is 3.4.3 but Programmed in R is 3.4.1
# # f <- function() {
# #   matched_call <- capture.output(str(match.call()))
# #   uses("3.4.3",c("zoo","xts","rlist","stringr","DescTools","plyr","DataCombine"), matched_call)
# # }
# # f()
# # [1] TRUE


# single column xts only
# percent change from the past through NOW 
# ( if to_future == TRUE, then from NOW to the FUTURE )
# only ONE lag is allowed: so "n" must be a vector of size: 1.
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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  pctchg_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(pctchg_xts)
} 

# require(xts)
# data(sample_matrix)
# sample_xts <- as.xts(sample_matrix)
# head(sample_xts[,"Open"],4) # x
#                          Open
# 2007-01-02 50.039781911546299
# 2007-01-03 50.230496197795397
# 2007-01-04 50.420955209067003
# 2007-01-05 50.373468054328498
# 
# get_pctchg_xts(head(sample_xts[,"Open"],4), n = 1)
#                           pctchg
# 2007-01-02                    NA
# 2007-01-03  0.381125334611203126 # res
# 2007-01-04  0.379170077320410137
# 2007-01-05 -0.094181386571521211
# 
# #( 50.230496197795397 - 50.039781911546299 ) / abs(50.039781911546299) * 100
# [1] 0.38112533461120313
# 
# get_pctchg_xts(head(sample_xts[,"Open"],4), n = 1, to_future = TRUE )
#                           pctchg
# 2007-01-02 -0.381125334611203126
# 2007-01-03 -0.379170077320410137 # res
# 2007-01-04  0.094181386571521211
# 2007-01-05                    NA
# 
# ( 50.420955209067003 -50.230496197795397) / abs( 50.230496197795397 ) * 100
# [1] 0.37917007732041014
# 
# # NOT(which <= NROW(x))
# get_pctchg_xts(head(sample_xts[,"Open"],4), n = 5, to_future = TRUE )
# 
#          pctchg
# 2007-01-02   NA
# 2007-01-03   NA
# 2007-01-04   NA
# 2007-01-05   NA



# single column xts only
#
# n - number of obs
# smoother over previous obs
get_recent_max <- function(x, n) { 

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
    # if(n <= length(x)) { max(x, na.rm = FALSE) } else { NA_real_ }
    max(x, na.rm = TRUE)
  }, n = n) -> x_result

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "recent_max"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  pctchg_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(pctchg_xts)
} 

# require(xts)
# data(sample_matrix)
# sample_xts <- as.xts(sample_matrix)
# 
# head(sample_xts[,"Open"],4)
#                          Open
# 2007-01-02 50.039781911546299
# 2007-01-03 50.230496197795397
# 2007-01-04 50.420955209067003
# 2007-01-05 50.373468054328498
# 
# get_recent_max(head(sample_xts[,"Open"],4), 2)
#                    recent_max
# 2007-01-02 50.039781911546299
# 2007-01-03 50.230496197795397
# 2007-01-04 50.420955209067003
# 2007-01-05 50.420955209067003


# single column xts only
#
# n - number of obs
# smoother over previous obs
# single column xts only
#
# n - number of obs
# smoother over previous obs
get_recent_min <- function(x, n) { 

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
    # if(n <= length(x)) { min(x, na.rm = FALSE) } else { NA_real_ }
    min(x, na.rm = TRUE)
  }, n = n) -> x_result

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "recent_min"
  # if I did not do this earlier / failsafe / Really should have every only been xts/Date
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  pctchg_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(pctchg_xts)
} 

# require(xts)
# data(sample_matrix)
# sample_xts <- as.xts(sample_matrix)
# 
# head(sample_xts[,"Open"],4)
#                          Open
# 2007-01-02 50.039781911546299
# 2007-01-03 50.230496197795397
# 2007-01-04 50.420955209067003
# 2007-01-05 50.373468054328498
# 
# get_recent_min(head(sample_xts[,"Open"],4), 2)
#                    recent_min
# 2007-01-02 50.039781911546299
# 2007-01-03 50.039781911546299
# 2007-01-04 50.230496197795397
# 2007-01-05 50.373468054328498



# single column xts only
#
# adjust as if held using a full year of time
# n multiple ( e.g monthly = 12, daily = 260 )
# 
get_annualized_xts <- function(x, n) {

}



# single column xts only
#
# simple moving average
# n - number of obs
get_sma_xts <- function(x, n) {


}

# single column xts only
#
# simple moving sortino
# n - number of obs
get_smsortino_xts <- function(x, n) {



}

# single column xts only
#
# m = number of ranks ( pessimistic )
# n - number of obs
# simple moving ranks
get_smrank_xts <- function(x, n, m) {


}



# single column xts only
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
  
  get_collofdays2daily <- merged
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(get_collofdays2daily)
  
}
# require(xts)
# xts(c(11,13,15),zoo::as.Date(c(1,3,5))) 
#            [,1]
# 1970-01-02   11
# 1970-01-04   13
# 1970-01-06   15
# 
# get_collofdays2daily_xts(xts(c(11,13,15),zoo::as.Date(c(1,3,5))))
#            collofdays2daily
# 1970-01-02               11
# 1970-01-03               NA
# 1970-01-04               13
# 1970-01-05               NA
# 1970-01-06               15

# as.POSIXct(c(1,10000,200000,400000), origin = "1970-01-01")
# [1] "1970-01-01 00:00:01 UTC" "1970-01-01 02:46:40 UTC" "1970-01-03 07:33:20 UTC" "1970-01-05 15:06:40 UTC"
# xts(11:14, as.POSIXct(c(1,10000,200000,400000), origin = "1970-01-01"))
# 
#                     [,1]
# 1970-01-01 00:00:01   11
# 1970-01-01 02:46:40   12
# 1970-01-03 07:33:20   13
# 1970-01-05 15:06:40   14
# 
# get_collofdays2daily_xts(xp)
#          [,1]
# 1970-01-01 12
# 1970-01-02 NA
# 1970-01-03 13
# 1970-01-04 NA
# 1970-01-05 14



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
# get_delay_since_last_obs(c(101,NA,NA,NA,102,NA,NA))
# [1] 0 1 2 3 0 1 2



# single column xts only

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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  delay_since_last_obs_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(delay_since_last_obs_xts)

} 
# payload NA-gaps matter ( NOT index time gaps ) 
# 
# get_delay_since_last_obs_xts(xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
#            delay_since_last_obs
# 1970-01-11                    0
# 1970-01-21                    1
# 1970-01-31                    2
# 1970-02-10                    3
# 1970-02-20                    0
# 1970-03-02                    1
# 1970-03-12                    2



# single column xts only
# add a record for each day

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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  delay_since_last_day_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(delay_since_last_day_xts)

} 
# require(xts)
# get_delay_since_last_day_xts(xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
#            delay_since_last_day
# 1970-01-11                    0
# 1970-01-12                    1
# 1970-01-13                    2
# 1970-01-14                    3
# 1970-01-15                    4
# 1970-01-16                    5
# 1970-01-17                    6
# 1970-01-18                    7
# 1970-01-19                    8
# 1970-01-20                    9
# 1970-01-21                   10
# 1970-01-22                   11
# 1970-01-23                   12
# 1970-01-24                   13
# 1970-01-25                   14
# 1970-01-26                   15
# 1970-01-27                   16
# 1970-01-28                   17
# 1970-01-29                   18
# 1970-01-30                   19
# 1970-01-31                   20
# 1970-02-01                   21
# 1970-02-02                   22
# 1970-02-03                   23
# 1970-02-04                   24
# 1970-02-05                   25
# 1970-02-06                   26
# 1970-02-07                   27
# 1970-02-08                   28
# 1970-02-09                   29
# 1970-02-10                   30
# 1970-02-11                   31
# 1970-02-12                   32
# 1970-02-13                   33
# 1970-02-14                   34
# 1970-02-15                   35
# 1970-02-16                   36
# 1970-02-17                   37
# 1970-02-18                   38
# 1970-02-19                   39
# 1970-02-20                    0
# 1970-02-21                    1
# 1970-02-22                    2
# 1970-02-23                    3
# 1970-02-24                    4
# 1970-02-25                    5
# 1970-02-26                    6
# 1970-02-27                    7
# 1970-02-28                    8
# 1970-03-01                    9
# 1970-03-02                   10
# 1970-03-03                   11
# 1970-03-04                   12
# 1970-03-05                   13
# 1970-03-06                   14
# 1970-03-07                   15
# 1970-03-08                   16
# 1970-03-09                   17
# 1970-03-10                   18
# 1970-03-11                   19
# 1970-03-12                   20



# single column xts only

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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  na_xts <- x_result 
  
  Sys.setenv(TZ=oldtz)
  options(ops)
   
  return(na_xts)

} 
# require(xts)
# is_na_xts(xts(c(11,NA,NA,14,NA),zoo::as.Date(1:5)))
#              na
# 1970-01-02    2
# 1970-01-03    1
# 1970-01-04    1
# 1970-01-05    2
# 1970-01-06    1


# single column xts only
#
# typical entry rm_what = c("Saturday", "Sunday", "BIZHOLIDAYS" )
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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  days_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(days_xts)
}
# weekends removed
# about 
# 
# RQuantLib::isHoliday(. . ."UnitedStates/NYSE". . .) considers 'weekends' to be holidays
  # JAN 01 2007 - JAN 17 2007
  # weekends     removed
  # Holiday 1st  removed - New Year's Day
  # Holiday 2nd  removed - Day Of Mourning - Gerald Ford ( SUPRISING )
  # Holiday 15th removed - Martin Luther King Day
# require(xts)
# x <- xts(1:17, zoo::as.Date("2007-01-01") -1 + 1:17)
# df <- data.frame( index(x), RQuantLib::isHoliday("UnitedStates/NYSE", index(x)), weekdays(index(x)))
# colnames(df) <- c("index", "is_nyse_holiday", "weekday")
# df
#         index is_nyse_holiday   weekday
# 1  2007-01-01            TRUE    Monday
# 2  2007-01-02            TRUE   Tuesday
# 3  2007-01-03           FALSE Wednesday
# 4  2007-01-04           FALSE  Thursday
# 5  2007-01-05           FALSE    Friday
# 6  2007-01-06            TRUE  Saturday
# 7  2007-01-07            TRUE    Sunday
# 8  2007-01-08           FALSE    Monday
# 9  2007-01-09           FALSE   Tuesday
# 10 2007-01-10           FALSE Wednesday
# 11 2007-01-11           FALSE  Thursday
# 12 2007-01-12           FALSE    Friday
# 13 2007-01-13            TRUE  Saturday
# 14 2007-01-14            TRUE    Sunday
# 15 2007-01-15            TRUE    Monday
# 16 2007-01-16           FALSE   Tuesday
# 17 2007-01-17           FALSE Wednesday             
# 
# rm_days_xts(xts(1:17,zoo::as.Date("2007-01-01") -1 + 1:17), rm_what = c("Saturday", "Sunday", "BIZHOLIDAYS"))
#            days
# 2007-01-03    3
# 2007-01-04    4
# 2007-01-05    5
# 2007-01-08    8
# 2007-01-09    9
# 2007-01-10   10
# 2007-01-11   11
# 2007-01-12   12
# 2007-01-16   16
# 2007-01-17   17



# utility function ( input to OTHERS )
# 
# xts object: x, 
# d: numeric vector of past days: -1 yesterday, c(-1,-2) yesterday AND the 'day before yesterday' etc ( both must be true )
# 
# e.g. if TRUE and d = c(-1,-2), then 
#      BOTH yesterday and the 'day before yesterday' NON-working days of the U.S. Federal Government.
#
are_nearby_fred_holidays_xts <- function(x = NULL, d = NULL) {
 
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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  nearby_fred_holidays_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(nearby_fred_holidays_xts)
  
}
# require(xts)
# data(sample_matrix)
# sample_xts <- as.xts(sample_matrix)
#
# are_nearby_fred_holidays_xts(sample_xts[2:16,"Open"], d = c(-1,-2))
#            nearby_fred_holidays
# 2007-01-03                FALSE
# 2007-01-04                FALSE
# 2007-01-05                FALSE
# 2007-01-06                FALSE
# 2007-01-07                FALSE
# 2007-01-08                 TRUE
# 2007-01-09                FALSE
# 2007-01-10                FALSE
# 2007-01-11                FALSE
# 2007-01-12                FALSE
# 2007-01-13                FALSE
# 2007-01-14                FALSE
# 2007-01-15                 TRUE
# 2007-01-16                 TRUE
# 2007-01-17                FALSE


# many columns are allowed
# columns are not RENAMED
# 
# swap out an XTS object index USING my OWN custom INDEX
# 
# x 
#   xts object
# 
# x_index_new
#  anything one dimensional with a length/NROW(x_index_new) == NROW(x) == length(index(x))
# 
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
# require(quantmod)
# getSymbols("GDP", src = "FRED") 
# x <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1948-03-01"]
#            UNRATE
# 1948-01-01    3.4
# 1948-02-01    3.8
# 1948-03-01    4.0
# 
# do_reindex_xts(x, 0:2)
#                        UNRATE
# 1970-01-01 3.3999999999999999
# 1970-01-02 3.7999999999999998
# 1970-01-03 4.0000000000000000
#
# do_reindex_xts(merge(x,x), 0:2)
#                        UNRATE           UNRATE.1
# 1970-01-01 3.3999999999999999 3.3999999999999999
# 1970-01-02 3.7999999999999998 3.7999999999999998
# 1970-01-03 4.0000000000000000 4.0000000000000000



# many columns are allowed
# columns are not renamed
#
# meant REALY only for St.Louis FRED
# adjust dates that start on the 1st( sometimes 4th, 3rd, or 2nd) to be the 31st
# be aware of landings on weekend and long holiday weekends and after a Tuesday or Thursday holiday

# slow: 170 observations per second
# 
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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  fred_1st_days_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(fred_1st_days_xts)

}
# require(quantmod)
# x <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1948-03-01"]
#            UNRATE
# 1948-01-01    3.4
# 1948-02-01    3.8
# 1948-03-01    4.0

# pushback_fred_1st_days_xts(x)
#                        UNRATE
# 1947-12-31 3.3999999999999999
# 1948-01-31 3.7999999999999998
# 1948-02-29 4.0000000000000000

# 
# pushback_fred_1st_days_xts(merge(x,x))
#                        UNRATE           UNRATE.1
# 1947-12-31 3.3999999999999999 3.3999999999999999
# 1948-01-31 3.7999999999999998 3.7999999999999998
# 1948-02-29 4.0000000000000000 4.0000000000000000



# many columns are allowed
# 
# n
#   meant to pass just the index year numeric YYYY
# 
# value
#   1 - yes # 2 - no
# 
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
  if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
  
  year_less_than_or_equal_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
   
  return(year_less_than_or_equal_xts)

}
# require(quantmod)
# x2 <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1949-03-01"]
# 
# is_year_less_than_or_equal_xts(x2, 1948)
#            year_less_than_or_equal_xts
# 1948-01-01                           1
# 1948-02-01                           1
# 1948-03-01                           1
# 1948-04-01                           1
# 1948-05-01                           1
# 1948-06-01                           1
# 1948-07-01                           1
# 1948-08-01                           1
# 1948-09-01                           1
# 1948-10-01                           1
# 1948-11-01                           1
# 1948-12-01                           1
# 1949-01-01                           2
# 1949-02-01                           2
# 1949-03-01                           2



# o_args is a named vector of arguments ( but user should really should use a Curry )
# 
#   IF o_args IS OF A MIXED DATA.TAPE,  use a list INSTEAD ( of a vector ) 
#    e.g. = list(indexAt= 'lastof', OHLC = FALSE)
# 
expand_xts <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL, fixed_sep = NULL) {
  
  # BASED ON 
  #   Found by RSEEK
  #   Time series cross-validation 5
  #   January 24, 2013
  #   By Zachary Mayer ( OTHERS BY THIS AUTHOR: http://www.r-bloggers.com/author/zachary-mayer/ )
  #   Zachary Deane-Mayer  
  #   ( CARET GUY: AND: Author of library(caretEnsemble)
  #     http://www.r-bloggers.com/time-series-cross-validation-5/
  #     http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
  #     GIST OF THIS ON GITHUB
  #     https://gist.github.com/zachmayer/4630129#file-1-load-data-r
  #     ALSO THE AUTHOR OF R CRAN package caretEnsemble and R github package cv.ts 
  
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
    # uses package stringr   functions str_replace_all, str_detect
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
      if(!isFunctionSyntax(str_c(fnct_text, collapse = "")) && !fnct_pass_type_determined) { 
        fnct_pass_type_determined <- TRUE
        assign( "anon", fnct )
        fnct <- "anon"
      }
      
      # pass by   function_name   namespace::function_name
      # NOTE: fnct_text SHOULD HAVE already been collapsed
      if( isFunctionSyntax(str_c(fnct_text, collapse = "")) && !fnct_pass_type_determined) { 
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
    if(any(class(x_result) %in% c("xts","zoo"))) index(x_result) <- zoo::as.Date(index(x_result))
    
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
# require(quantmod); 
# x3 <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
# 
#                      IBM.Open           IBM.High   IBM.Low          IBM.Close IBM.Volume       IBM.Adjusted
# 1970-01-02 18.225000000000001 18.287500000000001 18.200001 18.237499000000000     315200 5.3213109999999997
# 1970-01-05 18.299999000000000 18.412500000000001 18.299999 18.412500000000001     424000 5.3723760000000000
# 1970-01-06 18.412500000000001 18.450001000000000 18.312500 18.424999000000000     488000 5.3760190000000003
# 1970-01-07 18.424999000000000 18.437500000000000 18.312500 18.437500000000000     457600 5.3796629999999999
# 1970-01-08 18.437500000000000 18.475000000000001 18.375000 18.475000000000001     707200 5.3906080000000003
# 1970-01-09 18.475000000000001 18.524999999999999 18.424999 18.450001000000000     585600 5.3833140000000004
# 1970-01-12 18.450001000000000 18.487499000000000 18.387501 18.387501000000000     379200 5.3650779999999996

# changed prefix = TRUE
#
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = TRUE) # NOT default
#            TTR_SMA.2.IBM.Open TTR_SMA.2.IBM.Close TTR_SMA.3.IBM.Open TTR_SMA.3.IBM.Close
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# changed    prefix = FALSE  ( default )
#
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = FALSE)  # default
#            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# changed   fixed_sep = "_"
# 
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, fixed_sep = "_")
#            IBM.Open_TTR_SMA_2 IBM.Close_TTR_SMA_2 IBM.Open_TTR_SMA_3 IBM.Close_TTR_SMA_3
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3)
#            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# changed to hard-coded function call   fnct = TTR::SMA
#
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = TTR::SMA, whiches = 2:3)
#            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# changed to function sent as a string   fnct = "function(x,n){ TTR::SMA(x,n) }"
#
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = "function(x,n){ TTR::SMA(x,n) }", whiches = 2:3)

#               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
# 1970-01-02                 NA                 NA                 NA                 NA
# 1970-01-05 18.262499500000001 18.324999500000001                 NA                 NA
# 1970-01-06 18.356249500000001 18.418749500000001 18.312499666666668 18.358332666666666
# 1970-01-07 18.418749500000001 18.431249500000000 18.379166000000001 18.424999666666665
# 1970-01-08 18.431249500000000 18.456250000000001 18.424999666666665 18.445833000000000
# 1970-01-09 18.456250000000001 18.462500500000001 18.445833000000000 18.454166999999998
# 1970-01-12 18.462500500000001 18.418751000000000 18.454166999999998 18.437500666666665

# change to function sent as a closure   fnct = function(x,n){ TTR::SMA(x,n) }
#
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = function(x,n){ TTR::SMA(x,n) }, whiches = 2:3)
#               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
# 1970-01-02                 NA                 NA                 NA                 NA
# 1970-01-05 18.262499500000001 18.324999500000001                 NA                 NA
# 1970-01-06 18.356249500000001 18.418749500000001 18.312499666666668 18.358332666666666
# 1970-01-07 18.418749500000001 18.431249500000000 18.379166000000001 18.424999666666665
# 1970-01-08 18.431249500000000 18.456250000000001 18.424999666666665 18.445833000000000
# 1970-01-09 18.456250000000001 18.462500500000001 18.445833000000000 18.454166999999998
# 1970-01-12 18.462500500000001 18.418751000000000 18.454166999999998 18.437500666666665

# check that a specific function works
#
# x3a <- { t <- x3; t[2:3,1] <- NA_real_; t}
# expand_xts(x3a[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3)
#               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
# 1970-01-02 18.225000000000001 18.237499000000000 18.225000000000001 18.237499000000000
# 1970-01-05 18.225000000000001 18.412500000000001 18.225000000000001 18.412500000000001
# 1970-01-06 18.225000000000001 18.424999000000000 18.225000000000001 18.424999000000000
# 1970-01-07 18.424999000000000 18.437500000000000 18.424999000000000 18.437500000000000
# 1970-01-08 18.437500000000000 18.475000000000001 18.437500000000000 18.475000000000001
# 1970-01-09 18.475000000000001 18.450001000000000 18.475000000000001 18.450001000000000
# 1970-01-12 18.450001000000000 18.387501000000000 18.450001000000000 18.387501000000000

# give the ouput columns and alternate pre/post(append) name
#
# expand_xts(x3a[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3, alt_name = "NALOCF")
#             IBM.Open.NALOCF.2 IBM.Close.NALOCF.2  IBM.Open.NALOCF.3 IBM.Close.NALOCF.3
# 1970-01-02 18.225000000000001 18.237499000000000 18.225000000000001 18.237499000000000
# 1970-01-05 18.225000000000001 18.412500000000001 18.225000000000001 18.412500000000001
# 1970-01-06 18.225000000000001 18.424999000000000 18.225000000000001 18.424999000000000
# 1970-01-07 18.424999000000000 18.437500000000000 18.424999000000000 18.437500000000000
# 1970-01-08 18.437500000000000 18.475000000000001 18.437500000000000 18.475000000000001
# 1970-01-09 18.475000000000001 18.450001000000000 18.475000000000001 18.450001000000000
# 1970-01-12 18.450001000000000 18.387501000000000 18.450001000000000 18.387501000000000

# send extra arguements to a function
#
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = "get_pctchg_xts", whiches = 2:3, alt_name = "futPCTCHG" , o_args = c(to_future = TRUE ))
#             IBM.Open.futPCTCHG.2 IBM.Close.futPCTCHG.2 IBM.Open.futPCTCHG.3 IBM.Close.futPCTCHG.3
# 1970-01-02 -1.028806584362139898 -1.028101495714955238 -1.09738820301782303  -1.09664707863726441
# 1970-01-05 -0.683060146615308561 -0.135777325186686088 -0.75137162575801408  -0.33944331296673452
# 1970-01-06 -0.135777325186686088 -0.271375862761250308 -0.33944331296673452  -0.13569607249368446
# 1970-01-07 -0.271375862761250308 -0.067802033898306802 -0.13569607249368446   0.27118101694915081
# 1970-01-08 -0.067802033898306802  0.473607577807854341                   NA                    NA
# 1970-01-09                    NA                    NA                   NA                    NA
# 1970-01-12                    NA                    NA                   NA                    NA

# check that i specific function works
#
# expand_xts(x3[,c("IBM.Open","IBM.Close")], fnct = "to.monthly", alt_name = "MONTHLY", o_args = list(indexAt= 'lastof', OHLC = FALSE))
#            IBM.Open.MONTHLY IBM.Close.MONTHLY
# 1970-01-31        18.450001         18.387501

# check that a specific function works
#
# expand_xts(xts(,index(x3)), fnct = "is_year_less_than_or_equal_xts", whiches =  seq(lubridate::year(min(index(x3))), lubridate::year(max(index(x3))),by = 1), alt_name = "y_lth_or_eq_to_fact")
# 
#            y_lth_or_eq_to_fact.1970
# 1970-01-02                        1
# 1970-01-05                        1
# 1970-01-06                        1
# 1970-01-07                        1
# 1970-01-08                        1
# 1970-01-09                        1
# 1970-01-12                        1


# goodsight01.R
