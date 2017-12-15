

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
  
  colnames(x_result) <- "na_locfl"
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



uses <- function(programmed_in_R_version, explicit_package_function_calls, matched_call = NULL) {

  running_in_R_version <- paste(R.Version()$major,R.Version()$minor, sep = ".")
  # programmed_in_R_version # "3.4.0"

  if(running_in_R_version  != programmed_in_R_version) {
    warning(paste0("Running R is ", running_in_R_version, " but Programmed in R is ", programmed_in_R_version ) )
  }

  # EVERY ONE that I explicitly CALL
  # 
  uses_packages <- explicit_package_function_calls
  packages_not_found <- uses_packages[!uses_packages %in% installed.packages()]
  
  if(length(packages_not_found)) warning(paste0("Missing package(s) not found in .libPaths: ",paste0(.libPaths(), collapse = ", ")))
  for(packages_not_found_i in seq_along(packages_not_found)) { 
    warning(paste0("Package ", packages_not_found[packages_not_found_i]," is not found.")) 
    if(length(packages_not_found) == packages_not_found_i) stop(paste0("STOPPED in", matched_call))
  }
  
  return(TRUE)

}
# f <- function() {
#   matched_call <- capture.output(str(match.call()))
#   uses("3.4.1",c("zoo","xts","rlist","stringr","DescTools","plyr","DataCombine"), matched_call)
# }
# f()
# Warning message:
# In uses("3.4.1", c("zoo", "xts", "rlist", "stringr", "DescTools",  :
#   Running R is 3.4.3 but Programmed in R is 3.4.1
# f <- function() {
#   matched_call <- capture.output(str(match.call()))
#   uses("3.4.3",c("zoo","xts","rlist","stringr","DescTools","plyr","DataCombine"), matched_call)
# }
# f()
# [1] TRUE


# single column xts only
# percent change from the past through NOW 
# ( if to_future == TRUE, then from NOW to the FUTURE )
# only ONE lag is allowed: so "which" must be a vector of size: 1.
get_pctchg_xts <- function(x, which, to_future = NULL) { 

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
  if( which <= NROW(x) ) {
    # from past
    if(!to_future) x_result <- ( x - xts::lag.xts(x, which * lag_direction) )/ abs(xts::lag.xts(x, which * lag_direction)) * 100 * lag_direction
    # to future
    if( to_future) x_result <- ( xts::lag.xts(x, which * lag_direction) - x )/ abs(                                    x) * 100 * lag_direction
  } else {
    x[,] <- NA_real_
    x_result <- x
  }

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  colnames(x_result) <- "pctchg"
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
# get_pctchg_xts(head(sample_xts[,"Open"],4), which = 1)
#                           pctchg
# 2007-01-02                    NA
# 2007-01-03  0.381125334611203126 # res
# 2007-01-04  0.379170077320410137
# 2007-01-05 -0.094181386571521211
# 
# #( 50.230496197795397 - 50.039781911546299 ) / abs(50.039781911546299) * 100
# [1] 0.38112533461120313
# 
# get_pctchg_xts(head(sample_xts[,"Open"],4), which = 1, to_future = TRUE )
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
# get_pctchg_xts(head(sample_xts[,"Open"],4), which = 5, to_future = TRUE )
# 
#          pctchg
# 2007-01-02   NA
# 2007-01-03   NA
# 2007-01-04   NA
# 2007-01-05   NA

# [ ] NEED get_sma_xts



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
  
  # # 100% opposite of the above ( no  delays found ): REDUNDANT: I CAN REMOVE THIS CODE
  # 
  # sc <- StreamMetabolism::contiguous.zoo(data.frame(vec))
  # 
  # if(NROW(snc)>0) {
  #   rowr::rowApply(sc, fun = function(x) { 
  #     with( x, { new_vec[starts:ends] <- rep(0,lengths)
  #                assign("new_vec", new_vec , envir= parent.frame(8))  
  #              } ) -> discard ; NULL
  #   }) -> discard
  # }
   
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
# 
#              GDP
# 1946-12-31 243.1
# 1947-03-31 246.3
# 1947-06-30 250.1
# 1947-09-30 260.3
# 1947-12-31 266.2
# 1948-03-31 272.9
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
  year_less_than_or_equal_xts <- x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
   
  return(year_less_than_or_equal_xts)

}
# require(quantmod)
# x2 <- getSymbols("UNRATE", src = "FRED",  auto.assign = FALSE)["1948-01-01/1949-03-01"]
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
expand.xts <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL) {
  
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
  #     ALSO THE AUTHOR OF R package  cv.ts) on github
  
  # not used
  fnct_text_items <- as.character(substitute(fnct))
  
  # used
  fnct_text       <- as.character(deparse(substitute(fnct)))
  
  # fnct_text <- as.character(substitute(fnct))
  # 
  # if((length(fnct_text) == 3) && eval(parse(text=deparse(fnct_text[1]))) %in% c("::",":::")) {
  #   fnct_text <- paste0( eval(parse(text=deparse(fnct_text[2]))), eval(parse(text=deparse(fnct_text[1]))), eval(parse(text=deparse(fnct_text[3]))), collapse = "")
  # }
  # 
  # # not encountered case
  # if((length(fnct_text) == 2) && eval(parse(text=deparse(fnct_text[1]))) %in% c("::",":::")) {
  #   fnct_text <- paste0( eval(parse(text=deparse(fnct_text[2]))),                                          eval(parse(text=deparse(fnct_text[3]))), collapse = "")
  # }
  # 
  # # not encountered case
  # if((length(fnct_text)  > 0)                                                              ) {
  #   fnct_text <- paste0(sapply(fnct_text, function(xx) { eval(parse(text=deparse(xx))) }), collapse = "")
  # }
  
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
  
  expand.xts_inner <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL) {

        # uses zoo::is.zoo, zoo::as.zoo, zoo::na.locf, DescTools::DoCall, 
    # xts:::na.locf.xts(dispatch), xts:::merge.xts(dispatch), plyr::join_all,  DataCombine::VarDrop, stringr::str_replace_all
    # xts::is.xts, xts::as.xts, rlist::list.flatten(X?X),  rlist::list.ungroup, stringr::str_replace_all, plyr::mutate, stringr::str_detect
  
    uses("3.4.3",c("zoo","xts","rlist","stringr","DescTools","plyr","DataCombine"), matched_call)
    
    require(xts) # # Attaching package: 'zoo'
    # IF NOT Error in try.xts(element1) : could not find function "try.xts"
    
    if(is.null(     x))  stop("run-time user must provide input data")
    if(is.null(   fnct)) stop("run-time user must provide a function 'fnct'")
    # if(is.null(whiches)) stop("run-time user must provide a function 'fnct' arguments: e.g. 0:4")
    if(is.null(prefix))  prefix  <- FALSE # do  I of name fnct/alt_name do a preprend(TRUE) or append(FALSE)(default) 
    
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
        assign("anon", fnct )
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
        assign("anon", eval(parse(text=fnct)))
        fnct <- "anon"
      }
    
    }
      
    require(xts) 
    
    x_orig <- x
    c_orig <- class(x)[1] # original class
    
    ## VERY BASIC attemped CLASS conversion ##
    x_try.xts_success <- FALSE
    x_try.xts <- try(xts::try.xts(x_orig), silent = T)
    #
    x         <- if(any("try-error" %in% class(x_try.xts))) { x_orig } else { x_try.xts_success <- TRUE; x_try.xts }
    
    x -> INPUT  
  
    # clINPUT <-  class(INPUT)[1] # typically "xts" "zoo"
    # rINPUT <-  if(xts::is.xts(INPUT) || zoo::is.zoo(INPUT)) { index(INPUT) }  else { rownames(INPUT) }
    RETs <- lapply(whiches, function(x) {
      lapply( INPUT, function(x,whiches) { 
        # as.data.frame(  eval(parse(text = fnct))(x, whiches), stringsAsFactors = FALSE) -> z
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
          paste(            stringr::str_replace_all(fnct,"[.]|::","_"), x, names(out), sep='.') -> names(out)
        } else { 
          paste(names(out), stringr::str_replace_all(fnct,"[.]|::","_"), x            , sep='.') -> names(out)
        }   
      } else { # no whiches argument e.g. na.locf
        if(prefix) { 
          paste(            stringr::str_replace_all(fnct,"[.]|::","_"),    names(out), sep='.') -> names(out)
        } else { 
          paste(names(out), stringr::str_replace_all(fnct,"[.]|::","_")               , sep='.') -> names(out)
        }   
      } 
      # edge case: a column name is null: possible! allowed!: typical case send: xts(, index(<something>))
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
    
    # xts:::merge.xts(dispatch)
    RETs <- DescTools::DoCall("merge", RETsUGtDT )
    
    # if(any(clINPUT %in% c("xts","zoo"))) { 
    #   # assign("RETs",eval(parse(text=paste0(clINPUT[1],"::","as.",clINPUT[1],"(RETs,rINPUT)")))) # xts/zoo get an extra index
    #   # xts:::merge.xts(dispatch)
    #   RETs <- DescTools::DoCall("merge", RETsUGtDT )
    # } else { 
    #   # assign("RETs",eval(parse(text=paste0(                "as.",clINPUT[1],"(RETs)"))))
    #   # rownames(RETs) <- rINPUT
    #   RETs <- plyr::join_all(RETsUGtDT, by = "index", type = "full")
    #   row.names(RETs) <- RETs$index
    #   RETs <- RETs[!"index" %in% columns(RETs),,drop = FALSE] 
    # } 
    
    # return(RETs)
    
    x_result <- RETs
    
    # Should have always made it here
    if(x_try.xts_success) { 
      xts::reclass(x_result, x_orig) 
    } -> x_result
    
    Sys.setenv(TZ=oldtz)
    options(ops)
     
    return(x_result)
    
  }
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(expand.xts_inner(x = x, fnct =  fnct, whiches = whiches, alt_name = alt_name, o_args = o_args, prefix = prefix))

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

# prefix = TRUE
# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = TRUE) # NOT default
#            TTR_SMA.2.IBM.Open TTR_SMA.2.IBM.Close TTR_SMA.3.IBM.Open TTR_SMA.3.IBM.Close
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3, prefix = FALSE)  # default
#            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = "TTR::SMA", whiches = 2:3)
#            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = TTR::SMA, whiches = 2:3)
#            IBM.Open.TTR_SMA.2 IBM.Close.TTR_SMA.2 IBM.Open.TTR_SMA.3 IBM.Close.TTR_SMA.3
# 1970-01-02                 NA                  NA                 NA                  NA
# 1970-01-05 18.262499500000001  18.324999500000001                 NA                  NA
# 1970-01-06 18.356249500000001  18.418749500000001 18.312499666666668  18.358332666666666
# 1970-01-07 18.418749500000001  18.431249500000000 18.379166000000001  18.424999666666665
# 1970-01-08 18.431249500000000  18.456250000000001 18.424999666666665  18.445833000000000
# 1970-01-09 18.456250000000001  18.462500500000001 18.445833000000000  18.454166999999998
# 1970-01-12 18.462500500000001  18.418751000000000 18.454166999999998  18.437500666666665

# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = "function(x,n){ TTR::SMA(x,n) }", whiches = 2:3)

#               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
# 1970-01-02                 NA                 NA                 NA                 NA
# 1970-01-05 18.262499500000001 18.324999500000001                 NA                 NA
# 1970-01-06 18.356249500000001 18.418749500000001 18.312499666666668 18.358332666666666
# 1970-01-07 18.418749500000001 18.431249500000000 18.379166000000001 18.424999666666665
# 1970-01-08 18.431249500000000 18.456250000000001 18.424999666666665 18.445833000000000
# 1970-01-09 18.456250000000001 18.462500500000001 18.445833000000000 18.454166999999998
# 1970-01-12 18.462500500000001 18.418751000000000 18.454166999999998 18.437500666666665

# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = function(x,n){ TTR::SMA(x,n) }, whiches = 2:3)
#               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
# 1970-01-02                 NA                 NA                 NA                 NA
# 1970-01-05 18.262499500000001 18.324999500000001                 NA                 NA
# 1970-01-06 18.356249500000001 18.418749500000001 18.312499666666668 18.358332666666666
# 1970-01-07 18.418749500000001 18.431249500000000 18.379166000000001 18.424999666666665
# 1970-01-08 18.431249500000000 18.456250000000001 18.424999666666665 18.445833000000000
# 1970-01-09 18.456250000000001 18.462500500000001 18.445833000000000 18.454166999999998
# 1970-01-12 18.462500500000001 18.418751000000000 18.454166999999998 18.437500666666665

# x3a <- { t <- x3; t[2:3,1] <- NA_real_; t}
# expand.xts(x3a[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3)
#               IBM.Open.anon.2   IBM.Close.anon.2    IBM.Open.anon.3   IBM.Close.anon.3
# 1970-01-02 18.225000000000001 18.237499000000000 18.225000000000001 18.237499000000000
# 1970-01-05 18.225000000000001 18.412500000000001 18.225000000000001 18.412500000000001
# 1970-01-06 18.225000000000001 18.424999000000000 18.225000000000001 18.424999000000000
# 1970-01-07 18.424999000000000 18.437500000000000 18.424999000000000 18.437500000000000
# 1970-01-08 18.437500000000000 18.475000000000001 18.437500000000000 18.475000000000001
# 1970-01-09 18.475000000000001 18.450001000000000 18.475000000000001 18.450001000000000
# 1970-01-12 18.450001000000000 18.387501000000000 18.450001000000000 18.387501000000000

# expand.xts(x3a[,c("IBM.Open","IBM.Close")], fnct = "na.locf", whiches = 2:3, alt_name = "NALOCF")
#             IBM.Open.NALOCF.2 IBM.Close.NALOCF.2  IBM.Open.NALOCF.3 IBM.Close.NALOCF.3
# 1970-01-02 18.225000000000001 18.237499000000000 18.225000000000001 18.237499000000000
# 1970-01-05 18.225000000000001 18.412500000000001 18.225000000000001 18.412500000000001
# 1970-01-06 18.225000000000001 18.424999000000000 18.225000000000001 18.424999000000000
# 1970-01-07 18.424999000000000 18.437500000000000 18.424999000000000 18.437500000000000
# 1970-01-08 18.437500000000000 18.475000000000001 18.437500000000000 18.475000000000001
# 1970-01-09 18.475000000000001 18.450001000000000 18.475000000000001 18.450001000000000
# 1970-01-12 18.450001000000000 18.387501000000000 18.450001000000000 18.387501000000000

# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = "get_pctchg_xts", whiches = 2:3, alt_name = "futPCTCHG" , o_args = c(to_future = TRUE ))
#             IBM.Open.futPCTCHG.2 IBM.Close.futPCTCHG.2 IBM.Open.futPCTCHG.3 IBM.Close.futPCTCHG.3
# 1970-01-02 -1.028806584362139898 -1.028101495714955238 -1.09738820301782303  -1.09664707863726441
# 1970-01-05 -0.683060146615308561 -0.135777325186686088 -0.75137162575801408  -0.33944331296673452
# 1970-01-06 -0.135777325186686088 -0.271375862761250308 -0.33944331296673452  -0.13569607249368446
# 1970-01-07 -0.271375862761250308 -0.067802033898306802 -0.13569607249368446   0.27118101694915081
# 1970-01-08 -0.067802033898306802  0.473607577807854341                   NA                    NA
# 1970-01-09                    NA                    NA                   NA                    NA
# 1970-01-12                    NA                    NA                   NA                    NA

# expand.xts(x3[,c("IBM.Open","IBM.Close")], fnct = "to.monthly", alt_name = "MONTHLY", o_args = list(indexAt= 'lastof', OHLC = FALSE))
#            IBM.Open.MONTHLY IBM.Close.MONTHLY
# 1970-01-31        18.450001         18.387501

# expand.xts(xts(,index(x3)), fnct = "is_year_less_than_or_equal_xts", whiches =  seq(lubridate::year(min(index(x3))), lubridate::year(max(index(x3))),by = 1), alt_name = "y_lth_or_eq_to_fact")
# 
#            y_lth_or_eq_to_fact.1970
# 1970-01-02                        1
# 1970-01-05                        1
# 1970-01-06                        1
# 1970-01-07                        1
# 1970-01-08                        1
# 1970-01-09                        1
# 1970-01-12                        1



# perform function maxx over previous observation and currenct observation ( in THAT order )
#
# expects(inherits) an 'xts' or 'zoo' object
# n # the number of function maxx to 'perform over' #
# n = 2 means previous observation and current observation ( in THAT order )
# note: earliest(first) value is ALWAYS the result of "function max to 'perform over' #'
#
Maxx <- function(x, n = 2) {

  orig_zoo_class <- class(x)[1]
  orig_zoo_index <- index(x)
  
  zoo::rollapply(as.zoo(x), n, function(x) { 
    max(x, na.rm = TRUE)
    print(x)
  }, partial = TRUE, align = "right") -> res

  res <- eval(parse(text=paste0("as.", orig_zoo_class, "(res)"))) 
  index(res) <- orig_zoo_index
  return(res)
}
# data(sample_matrix)
# sample.xts <- as.xts(sample_matrix)
# sample.xts[1:4,c("Low","Close")]
#                 Low    Close
# 2007-01-02 49.95041 50.11778
# 2007-01-03 50.23050 50.39767
# 2007-01-04 50.26414 50.33236
# 2007-01-05 50.22103 50.33459
# 
# > Maxx(sample.xts[1:4,c("Low","Close")])
#                 Low    Close
# 2007-01-02 49.95041 50.11778
# 2007-01-03 50.23050 50.39767
# 2007-01-04 50.26414 50.39767
# 2007-01-05 50.26414 50.33459




get_large_nationals_yearly_gdp_weights_by_month <- function(keep_eom_date_since = "2003-01-01") {

  message("Begin function: get_large_nationals_yearly_gdp_weights_by_month")
  
  # R version 3.4.2 (2017-09-28)
  # NOV 2017
  
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  # Loading required package: RJSONIO
  # requires package WDI ( otherwise WDI::WDI produces the error: "object 'WDI_data' not found" )
  require(WDI)
  
  # uses package tidyr function spread           # tidyr::spread
  # uses package DataCombine function MoveFront  # DataCombine::MoveFront
  # uses package tidyr function fill_            # tidyr::fill_
  
  # countries of interest
  # largest/most-affectioning countries that I would actually care about
  #
  # since 1960(NY.GDP.MKTP.CD)
  large_nationals <- 
  c(
     "united_states"
   , "china"
   , "japan"
   , "germany"            # since 1970(NY.GDP.MKTP.CD)
   , "united_kingdom"
   , "india"
   , "france"
   , "brazil"
   , "italy"
   , "canada"
   , "russian_federation" # since 1989(NY.GDP.MKTP.CD)
   , "korea_rep"
   , "australia"
   , "spain"
  )
  
  # weightings 
  # GDP (current US$)
  # World Bank national accounts data, and OECD National Accounts data files.
  # GDP (current US$) ... scale it ...
  # https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
  # Download XLS (  API_NY.GDP.MKTP.CD_DS2_en_excel_v2.xls )

  gross_domestic_product <- WDI(country = "all", start="1900", end = "2100", indicator = "NY.GDP.MKTP.CD")
  
  # clean
  gross_domestic_product<- data.frame(lapply(gross_domestic_product, function(x) { 
    if(is.character(x)) {
      # prevent 'drop to factor ... all 1s'
      # so I can do tidyr and PostgreSQL
      x <- tolower(x)
      x <- gsub("(,|[.])","", x)
      x <- gsub("(&|'| |-)","_", x)
    }
    return(x)
  }), stringsAsFactors = FALSE)

  # rename columns
  names(gross_domestic_product) <- tolower(names(gross_domestic_product))
  # rename NY.GDP.MKTP.CD" ... -> ... "measure"
  names(gross_domestic_product)[3] <- "measure"

  # add a 'real' date
  gross_domestic_product[["dateindex_dt"]] <- zoo::as.Date(paste0(gross_domestic_product[["year"]], "-12-31"))
  # change column name 
  names(gross_domestic_product)[match("year",names(gross_domestic_product))] <- "dateindex" 
  # add an 'integer' date
  gross_domestic_product[["dateindex"]] <- as.integer(zoo::as.Date(gross_domestic_product[["dateindex_dt"]]))

  # position columns
  gross_domestic_product <- DataCombine::MoveFront(gross_domestic_product, c("dateindex", "dateindex_dt"))
  # sort rows
  gross_domestic_product <- dplyr::arrange_(gross_domestic_product, "dateindex")

  # reduce to my data of interest
  gross_domestic_product <- gross_domestic_product[which(gross_domestic_product$country %in% 
    large_nationals
      )
  ,, drop = FALSE]
  
  # drop column ( because will produce a 'unique column value, and 'spread' will not work correctly )
  gross_domestic_product <- gross_domestic_product[ , !names(gross_domestic_product) %in% "iso2c",drop = FALSE]

  # reshape long to wide
  gross_domestic_product_spreaded_country_measure <- tidyr::spread(gross_domestic_product, key = "country", value = "measure")

  # rename columns to "<country>_gdp"
  names(gross_domestic_product_spreaded_country_measure) <- gsub("(^.{1,8}$|^.{9}(?<!dateindex).*)", "\\1__gdp", names(gross_domestic_product_spreaded_country_measure ), perl = TRUE )

  # combine dates: observation dates + end of month dates ( since Jan 2003 )
  gross_domestic_product_spreaded_country_measure_only_dateindex_dt_plus_eom_dates <-
  data.frame(dateindex_dt = unique(sort(c(gross_domestic_product_spreaded_country_measure$dateindex_dt, zoo::as.Date(zoo::as.yearmon(seq(as.Date(keep_eom_date_since), Sys.Date(), by = "month")), frac = 1)))))

  # put end of month dates into the data.frame
  gross_domestic_product_spreaded_country_measure_plus_eom_dates <- 
  merge(gross_domestic_product_spreaded_country_measure, gross_domestic_product_spreaded_country_measure_only_dateindex_dt_plus_eom_dates, all = TRUE)
  # calculate (for mostly new dateindex_dts) 'dateindex'
  gross_domestic_product_spreaded_country_measure_plus_eom_dates[["dateindex"]] <- as.integer(zoo::as.Date(gross_domestic_product_spreaded_country_measure_plus_eom_dates[["dateindex_dt"]]))
  
  # garantee column order
  gross_domestic_product_spreaded_country_measure_plus_eom_dates <- DataCombine::MoveFront(gross_domestic_product_spreaded_country_measure_plus_eom_dates, c("dateindex","dateindex_dt"))

  # last observation carried forward
  gross_domestic_product_spreaded_country_measure_plus_eom_dates_w_locf <-
  tidyr::fill_(gross_domestic_product_spreaded_country_measure_plus_eom_dates, colnames(gross_domestic_product_spreaded_country_measure_plus_eom_dates))

  # (from 2003 onward) keep dates that are 'end of month dates' 
  # many rows are removed
  gross_domestic_product_spreaded_country_measure_eom <- 
  gross_domestic_product_spreaded_country_measure_plus_eom_dates_w_locf[
    gross_domestic_product_spreaded_country_measure_plus_eom_dates_w_locf$dateindex_dt %in% zoo::as.Date(zoo::as.yearmon(seq(as.Date(keep_eom_date_since), Sys.Date(), by = "month")), frac = 1)
  ,, drop = FALSE]
  
  # because many rows are removed, re-number
  row.names(gross_domestic_product_spreaded_country_measure_eom) <- seq_along(row.names(gross_domestic_product_spreaded_country_measure_eom))
  
  # prepare 
  gross_domestic_product_spreaded_country_measure_weighted_eom <- gross_domestic_product_spreaded_country_measure_eom
  # rename columns from "<country>_gdp" to "<country>_gdp_wtd"
  names(gross_domestic_product_spreaded_country_measure_weighted_eom) <- gsub("(^.{1,8}$|^.{9}(?<!dateindex).*)", "\\1_wdt", names(gross_domestic_product_spreaded_country_measure_weighted_eom), perl = TRUE )

  # Weighted percentage contribution by row in R
  # https://stackoverflow.com/questions/36086376/weighted-percentage-contribution-by-row-in-r
  # rebalance
  # na.rm = TRUE
  # "germany"            # since 1970(NY.GDP.MKTP.CD)
  # "russian_federation" # since 1989(NY.GDP.MKTP.CD)
  gross_domestic_product_spreaded_country_measure_weighted_eom[,grep("(^.{1,8}$|^.{9}(?<!dateindex).*)", names(gross_domestic_product_spreaded_country_measure_weighted_eom), perl = TRUE, value = TRUE)] <-
  sweep(
                as.matrix(gross_domestic_product_spreaded_country_measure_weighted_eom[,grep("(^.{1,8}$|^.{9}(?<!dateindex).*)", names(gross_domestic_product_spreaded_country_measure_weighted_eom), perl = TRUE, value = TRUE)]) 
    , 1
    , rowSums(  as.matrix(gross_domestic_product_spreaded_country_measure_weighted_eom[,grep("(^.{1,8}$|^.{9}(?<!dateindex).*)", names(gross_domestic_product_spreaded_country_measure_weighted_eom), perl = TRUE, value = TRUE)]), na.rm = TRUE  )
    , FUN="/"
  )
  
  on.exit({Sys.setenv(TZ=oldtz)})
  
  message("End function: get_large_nationals_yearly_gdp_weights_by_month")
  
  return(gross_domestic_product_spreaded_country_measure_weighted_eom)
  
}
# NOV 12 2017
# NOV 12 2017
# res <- get_large_nationals_yearly_gdp_weights_by_month()
# head(res[, colnames(res) %in% c("dateindex","dateindex_dt","china__gdp_wdt","united_states__gdp_wdt")])
# tail(res[, colnames(res) %in% c("dateindex","dateindex_dt","china__gdp_wdt","united_states__gdp_wdt")])
# 
# # >  head(res[, colnames(res) %in% c("dateindex","dateindex_dt","china__gdp_wdt","united_states__gdp_wdt")])
#   dateindex dateindex_dt china__gdp_wdt united_states__gdp_wdt
# 1     12083   2003-01-31     0.05447564              0.4066555
# 2     12111   2003-02-28     0.05447564              0.4066555
# 3     12142   2003-03-31     0.05447564              0.4066555
# 4     12172   2003-04-30     0.05447564              0.4066555
# 5     12203   2003-05-31     0.05447564              0.4066555
# 6     12233   2003-06-30     0.05447564              0.4066555
# 
# # > tail(res[, colnames(res) %in% c("dateindex","dateindex_dt","china__gdp_wdt","united_states__gdp_wdt")])
#     dateindex dateindex_dt china__gdp_wdt united_states__gdp_wdt
# 174     17347   2017-06-30      0.2005963              0.3326051
# 175     17378   2017-07-31      0.2005963              0.3326051
# 176     17409   2017-08-31      0.2005963              0.3326051
# 177     17439   2017-09-30      0.2005963              0.3326051
# 178     17470   2017-10-31      0.2005963              0.3326051
# 179     17500   2017-11-30      0.2005963              0.3326051
# 
# # > str(res)
# > str(res)
# 'data.frame':	179 obs. of  16 variables:
#  $ dateindex                  : int  12083 12111 12142 12172 12203 12233 12264 12295 12325 12356 ...
#  $ dateindex_dt               : Date, format: "2003-01-31" "2003-02-28" "2003-03-31" "2003-04-30" ...
#  $ australia__gdp_wdt         : num  0.0146 0.0146 0.0146 0.0146 0.0146 ...
#  $ brazil__gdp_wdt            : num  0.0188 0.0188 0.0188 0.0188 0.0188 ...
#  $ canada__gdp_wdt            : num  0.0281 0.0281 0.0281 0.0281 0.0281 ...
#  $ china__gdp_wdt             : num  0.0545 0.0545 0.0545 0.0545 0.0545 ...
#  $ france__gdp_wdt            : num  0.0556 0.0556 0.0556 0.0556 0.0556 ...
#  $ germany__gdp_wdt           : num  0.077 0.077 0.077 0.077 0.077 ...
#  $ india__gdp_wdt             : num  0.0188 0.0188 0.0188 0.0188 0.0188 ...
#  $ italy__gdp_wdt             : num  0.0469 0.0469 0.0469 0.0469 0.0469 ...
#  $ japan__gdp_wdt             : num  0.152 0.152 0.152 0.152 0.152 ...
#  $ korea_rep__gdp_wdt         : num  0.0226 0.0226 0.0226 0.0226 0.0226 ...
#  $ russian_federation__gdp_wdt: num  0.0128 0.0128 0.0128 0.0128 0.0128 ...
#  $ spain__gdp_wdt             : num  0.0261 0.0261 0.0261 0.0261 0.0261 ...
#  $ united_kingdom__gdp_wdt    : num  0.0651 0.0651 0.0651 0.0651 0.0651 ...
#  $ united_states__gdp_wdt     : num  0.407 0.407 0.407 0.407 0.407 ...
# 
# # rebalance on 
# # "germany"            # since 1970(NY.GDP.MKTP.CD)
# # "russian_federation" # since 1989(NY.GDP.MKTP.CD)
# # res2 <- get_large_nationals_yearly_gdp_weights_by_month(keep_eom_date_since = "1980-01-01")
# # 
# # res2[, colnames(res2) %in% c("dateindex","dateindex_dt","china__gdp_wdt","united_states__gdp_wdt","germany__gdp_wdt","russian_federation__gdp_wdt")]
#     dateindex dateindex_dt china__gdp_wdt germany__gdp_wdt russian_federation__gdp_wdt united_states__gdp_wdt
# 1        3682   1980-01-31     0.02469427       0.12161493                          NA              0.3645832
# 2        3711   1980-02-29     0.02469427       0.12161493                          NA              0.3645832



get_large_nationals_last_know_bond_ratings_by_month <- function(keep_eom_date_since = "2003-01-01") {

    message("Begin function: get_large_nationals_last_know_bond_ratings_by_month")
  
  # R version 3.4.2 (2017-09-28)
  # NOV 2017
  
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  # uses package htmltab function htmltab        # htmltab::htmltab
  # uses package tidyr function spread           # tidyr::spread
  # uses package DataCombine function MoveFront  # DataCombine::MoveFront
  # uses package tidyr function fill_            # tidyr::fill_
  # uses package plyr function join_all          # plyr::join_all
  
  # https://tradingeconomics.com/countries
  #                   # name in WDI and start-of-record comments
  large_nationals <-
  c(
     "united-states"
   , "china"
   , "japan"
   , "germany"        # "germany" # since 1970(NY.GDP.MKTP.CD)
   , "united-kingdom"
   , "india"
   , "france"
   , "brazil"
   , "italy"
   , "canada"
   , "russia"       # "russian_federation" # since 1989(NY.GDP.MKTP.CD)
   , "south-korea"  # "korea_rep"
   , "australia"
   , "spain"
  )
  
  ## testing
  # large_nationals <-
  # c(
  #    "united-states"
  #  , "china"
  # )
  
  all_countries <- list()
  for(large_nationals_i in large_nationals) {
  
    message(paste0("Beginning: ", large_nationals_i))
    # NOTE: may? timeout (curl 10 seconds )
    # 
    this_country_historical_ratings <- try( 
        htmltab::htmltab(doc = paste0("https://tradingeconomics.com/", large_nationals_i, "/rating"), which = 1)
      , silent = TRUE
    )
    # try once more
    if(inherits(this_country_historical_ratings, 'try-error')) { 
      this_country_historical_ratings <- try( 
        htmltab::htmltab(doc = paste0("https://tradingeconomics.com/", large_nationals_i, "/rating"), which = 1)
      , silent = TRUE
      )
      if(inherits(this_country_historical_ratings, 'try-error')) { stop(paste0("counld not browse: ", paste0("https://tradingeconomics.com/", large_nationals_i, "/rating"))) }
    }

    # remove web-site error repeats ( united-kingdom: Sep 21, 2000 )
    this_country_historical_ratings <- this_country_historical_ratings[!duplicated(this_country_historical_ratings),, drop = FALSE]
    
    # I do not track this one
    this_country_historical_ratings <- this_country_historical_ratings[this_country_historical_ratings$Agency != "DBRS",,drop = FALSE]
    
    # store dates (save for later)
    this_country_historical_ratings_only_dateindex_dts <- as.Date(this_country_historical_ratings[["Date"]], "%b %d %Y")

    # drop column "Date"
    this_country_historical_ratings <- this_country_historical_ratings[ , !names(this_country_historical_ratings) %in% "Date",drop = FALSE]
    
    # clean ( Rstudio 'unknown reason' parsing error: work around: x[is.na(lapply(x,utf8ToInt))] )
    this_country_historical_ratings <- data.frame(lapply(this_country_historical_ratings, function(x) { 
      x[x == "N/A"] <- NA_character_
      # not-ascii (latin1) A (A is 'nothing')
      # if ... RStudio parser chokes ... detect
      x[is.na(lapply(x,utf8ToInt))] <- NA_character_ 
      # really only 'terating' ( because MY 'agency ratings description table' does not have a ZERO )
      x[x == "0"]   <- "1"
      # prevent 'drop to factor ... all 1s'
      # so I can do tidyr and PostgreSQL
      x <- tolower(x)
      x <- gsub("(&|'| )","_", x)
      return(x)
    }), stringsAsFactors = FALSE)

    # rename columns
    names(this_country_historical_ratings) <- tolower(names(this_country_historical_ratings))
    # add a 'real' date                             # from 'saved'
    this_country_historical_ratings[["dateindex_dt"]] <- this_country_historical_ratings_only_dateindex_dts 
    # add an integer date
    this_country_historical_ratings[["dateindex"]] <- as.integer(zoo::as.Date(this_country_historical_ratings[["dateindex_dt"]]))
    
    # garantee column order
    this_country_historical_ratings <- DataCombine::MoveFront(this_country_historical_ratings, c("dateindex", "dateindex_dt"))
    ### 'not sort now' because will interfere with the 'order-sensitive' duplicated record removal from below
    ### # sort
    ### this_country_historical_ratings <- dplyr::arrange_(this_country_historical_ratings, "dateindex")
    
    # begin reshapes
    #
    
    # begin agency-rating reshape
    
    # drop column not involved in the reshape(some extra safety)
    this_country_historical_ratings_spreaded_agency_rating <- this_country_historical_ratings
    this_country_historical_ratings_spreaded_agency_rating <- this_country_historical_ratings_spreaded_agency_rating[,!names(this_country_historical_ratings_spreaded_agency_rating) %in% "outlook", drop = FALSE]

    # top of the HTML page is the lastest date ( and latest decision )
    # if two(2)+ decisions in one day ( italy 1996-05-01 ), so eliminate the earliest date
    # # remove rating agency business repeats (change mind from earlier decision on the SAME day: italy May 01, 1996 )
    this_country_historical_ratings_spreaded_agency_rating <- this_country_historical_ratings_spreaded_agency_rating[ !duplicated(this_country_historical_ratings_spreaded_agency_rating[, c("dateindex","dateindex_dt", "agency"), drop = FALSE]), , drop = FALSE]

    # reshape long to wide: agency-rating
    this_country_historical_ratings_spreaded_agency_rating <- tidyr::spread(this_country_historical_ratings_spreaded_agency_rating, key = "agency", value = "rating")
    
    ### # reshape long to wide: agency-rating
    ### this_country_historical_ratings_spreaded_agency_rating <- tidyr::spread(this_country_historical_ratings, key = "agency", value = "rating")
    
    # in long form, it was always was a 'shared' character column
    # in wide form(result of tidyr::spread), it will have its own(alone) data type
    this_country_historical_ratings_spreaded_agency_rating[["te"]] <- as.numeric(this_country_historical_ratings_spreaded_agency_rating[["te"]])

    # rename columns
    names(this_country_historical_ratings_spreaded_agency_rating) <- gsub("(^fitch$|^moody_s$|^s_p$|^te$)", "\\1_rating", names(this_country_historical_ratings_spreaded_agency_rating) )
    
    ### # drop column
    ### this_country_historical_ratings_spreaded_agency_rating_less_outlook <- this_country_historical_ratings_spreaded_agency_rating[ , !names(this_country_historical_ratings_spreaded_agency_rating) %in% "outlook",drop = FALSE]
    
    # after order-sensitive 'duplicatated' was done
    this_country_historical_ratings_spreaded_agency_rating <- dplyr::arrange_(this_country_historical_ratings_spreaded_agency_rating, "dateindex")

    # end agency-rating reshape
    # begin agency-outlook reshape
    
    # drop column not involved in the reshape(some extra safety)
    this_country_historical_ratings_spreaded_agency_outlook <- this_country_historical_ratings
    this_country_historical_ratings_spreaded_agency_outlook <- this_country_historical_ratings_spreaded_agency_outlook[,!names(this_country_historical_ratings_spreaded_agency_outlook) %in% "rating", drop = FALSE]

    # top of the HTML page is the lastest date ( and latest decision )
    # if two(2)+ decisions in one day ( italy 1996-05-01 ), so eliminate the earliest date
    # # remove rating agency business repeats (change mind from earlier decision on the SAME day: italy May 01, 1996 )
    this_country_historical_ratings_spreaded_agency_outlook <- this_country_historical_ratings_spreaded_agency_outlook[ !duplicated(this_country_historical_ratings_spreaded_agency_outlook[, c("dateindex","dateindex_dt", "agency"), drop = FALSE]), , drop = FALSE]

    # reshape long to wide: agency-rating
    this_country_historical_ratings_spreaded_agency_outlook <- tidyr::spread(this_country_historical_ratings_spreaded_agency_outlook, key = "agency", value = "outlook")
    
    ### # reshape long to wide: agency-outlook
    ### this_country_historical_ratings_spreaded_agency_outlook <- tidyr::spread(this_country_historical_ratings, key = "agency", value = "outlook")
    
    # rename columns
    names(this_country_historical_ratings_spreaded_agency_outlook) <- gsub("(^fitch$|^moody_s$|^s_p$|^te$)", "\\1_outlook", names(this_country_historical_ratings_spreaded_agency_outlook) )
    
    ### # drop column
    ### this_country_historical_ratings_spreaded_agency_outlook_less_rating <- this_country_historical_ratings_spreaded_agency_outlook[ , !names(this_country_historical_ratings_spreaded_agency_outlook) %in% "rating",drop = FALSE] 
    
    # after order-sensitive 'duplicatated' was done
    this_country_historical_ratings_spreaded_agency_outlook <- dplyr::arrange_(this_country_historical_ratings_spreaded_agency_outlook, "dateindex")
    
    # end agency-outlook reshape
   
    #
    # end reshapes
    
    # bring together:  agency-rating and agency-outlook
    this_country_historical_ratings_spreaded <- 
    merge( this_country_historical_ratings_spreaded_agency_rating, 
           this_country_historical_ratings_spreaded_agency_outlook
      , by = c("dateindex", "dateindex_dt")
      , all = TRUE 
    )
    
    # combine dates: observation dates + end of month dates
    this_country_historical_ratings_spreaded_only_dateindex_dt_plus_eom_dates <-
    data.frame(dateindex_dt = unique(sort(c(this_country_historical_ratings_spreaded$dateindex_dt, zoo::as.Date(zoo::as.yearmon(seq(as.Date(keep_eom_date_since), Sys.Date(), by = "month")), frac = 1)))))
    
    # put end of month dates into the data.frame
    this_country_historical_ratings_spreaded_plus_eom_dates <- 
    merge(this_country_historical_ratings_spreaded, this_country_historical_ratings_spreaded_only_dateindex_dt_plus_eom_dates, all = TRUE)
    
    # calculate (for mostly new dateindex_dts) 'dateindex'
    this_country_historical_ratings_spreaded_plus_eom_dates[["dateindex"]] <- as.integer(zoo::as.Date(this_country_historical_ratings_spreaded_plus_eom_dates[["dateindex_dt"]]))
      
    # garantee column order
    this_country_historical_ratings_spreaded_plus_eom_dates <- DataCombine::MoveFront(this_country_historical_ratings_spreaded_plus_eom_dates, c("dateindex","dateindex_dt"))
    
    # last observation carried forward
    this_country_historical_ratings_spreaded_plus_eom_dates_w_locf <-
    tidyr::fill_(this_country_historical_ratings_spreaded_plus_eom_dates, colnames(this_country_historical_ratings_spreaded_plus_eom_dates))
    
    # (from 2003 onward) keep dates that are 'end of month dates' 
    this_country_historical_ratings_spreaded_eom <- 
    this_country_historical_ratings_spreaded_plus_eom_dates_w_locf[
      this_country_historical_ratings_spreaded_plus_eom_dates_w_locf$dateindex_dt %in% zoo::as.Date(zoo::as.yearmon(seq(as.Date(keep_eom_date_since), Sys.Date(), by = "month")), frac = 1)
      , 
      , drop = FALSE
    ]
    
    # because many rows are removed, re-number
    row.names(this_country_historical_ratings_spreaded_eom) <- seq_along(row.names(this_country_historical_ratings_spreaded_eom))

    # rename columns to "<country>__item"
    names(this_country_historical_ratings_spreaded_eom) <- gsub("(^.{1,8}$|^.{9}(?<!dateindex).*)", paste0(gsub("-","_", large_nationals_i), "__\\1"), names(this_country_historical_ratings_spreaded_eom), perl = TRUE )
    
    # add df to list of data.frames
         all_countries[[gsub("-","_", large_nationals_i)]] <- this_country_historical_ratings_spreaded_eom
    attr(all_countries[[gsub("-","_", large_nationals_i)]],"label") <- gsub("-","_", large_nationals_i)
    
    message(paste0("Ending: ", large_nationals_i))
    Sys.sleep(1.0)
  }
  
  # browser()
  
  # combine all data.frames
  ### all_countries <- do.call(merge, c(list(), all_countries, by = c("dateindex", "dateindex_dt"), all = TRUE))
  
  all_countries <- plyr::join_all(all_countries, by = c("dateindex", "dateindex_dt"), type = "full")
  
  on.exit({Sys.setenv(TZ=oldtz)})

  message("End function: get_large_nationals_last_know_bond_ratings_by_month")
  
  return(all_countries)
  
}
# res <- get_large_nationals_last_know_bond_ratings_by_month()
# str(res, list.len = 999)
# res[1:4, grep("dateindex|dateindex_dt|^italy.*", names(ret), perl = TRUE, value = TRUE)[1:7] , drop = FALSE]
# colnames(res)
# 
# >  res[1:4, grep("dateindex|dateindex_dt|^italy.*", names(res), perl = TRUE, value = TRUE)[1:7] , drop = FALSE]
#   dateindex dateindex_dt italy__fitch_rating italy__moody_s_rating italy__s_p_rating italy__te_rating italy__fitch_outlook
# 1     12083   2003-01-31                  aa                   aa2                aa             <NA>               stable
# 2     12111   2003-02-28                  aa                   aa2                aa             <NA>               stable
# 3     12142   2003-03-31                  aa                   aa2                aa             <NA>               stable
# 4     12172   2003-04-30                  aa                   aa2                aa             <NA>               stable
# > colnames(res)
#   [1] "dateindex"                       "dateindex_dt"                    "united_states__fitch_rating"     "united_states__moody_s_rating"  
#   [5] "united_states__s_p_rating"       "united_states__te_rating"        "united_states__fitch_outlook"    "united_states__moody_s_outlook" 
#   [9] "united_states__s_p_outlook"      "united_states__te_outlook"       "china__fitch_rating"             "china__moody_s_rating"          
#  [13] "china__s_p_rating"               "china__te_rating"                "china__fitch_outlook"            "china__moody_s_outlook"         
#  [17] "china__s_p_outlook"              "china__te_outlook"               "japan__fitch_rating"             "japan__moody_s_rating"          
#  [21] "japan__s_p_rating"               "japan__te_rating"                "japan__fitch_outlook"            "japan__moody_s_outlook"         
#  [25] "japan__s_p_outlook"              "japan__te_outlook"               "germany__fitch_rating"           "germany__moody_s_rating"        
#  [29] "germany__s_p_rating"             "germany__te_rating"              "germany__fitch_outlook"          "germany__moody_s_outlook"       
#  [33] "germany__s_p_outlook"            "germany__te_outlook"             "united_kingdom__fitch_rating"    "united_kingdom__moody_s_rating" 
#  [37] "united_kingdom__s_p_rating"      "united_kingdom__te_rating"       "united_kingdom__fitch_outlook"   "united_kingdom__moody_s_outlook"
#  [41] "united_kingdom__s_p_outlook"     "united_kingdom__te_outlook"      "india__fitch_rating"             "india__moody_s_rating"          
#  [45] "india__s_p_rating"               "india__te_rating"                "india__fitch_outlook"            "india__moody_s_outlook"         
#  [49] "india__s_p_outlook"              "india__te_outlook"               "france__fitch_rating"            "france__moody_s_rating"         
#  [53] "france__s_p_rating"              "france__te_rating"               "france__fitch_outlook"           "france__moody_s_outlook"        
#  [57] "france__s_p_outlook"             "france__te_outlook"              "brazil__fitch_rating"            "brazil__moody_s_rating"         
#  [61] "brazil__s_p_rating"              "brazil__te_rating"               "brazil__fitch_outlook"           "brazil__moody_s_outlook"        
#  [65] "brazil__s_p_outlook"             "brazil__te_outlook"              "italy__fitch_rating"             "italy__moody_s_rating"          
#  [69] "italy__s_p_rating"               "italy__te_rating"                "italy__fitch_outlook"            "italy__moody_s_outlook"         
#  [73] "italy__s_p_outlook"              "italy__te_outlook"               "canada__fitch_rating"            "canada__moody_s_rating"         
#  [77] "canada__s_p_rating"              "canada__te_rating"               "canada__fitch_outlook"           "canada__moody_s_outlook"        
#  [81] "canada__s_p_outlook"             "canada__te_outlook"              "russia__fitch_rating"            "russia__moody_s_rating"         
#  [85] "russia__s_p_rating"              "russia__te_rating"               "russia__fitch_outlook"           "russia__moody_s_outlook"        
#  [89] "russia__s_p_outlook"             "russia__te_outlook"              "south_korea__fitch_rating"       "south_korea__moody_s_rating"    
#  [93] "south_korea__s_p_rating"         "south_korea__te_rating"          "south_korea__fitch_outlook"      "south_korea__moody_s_outlook"   
#  [97] "south_korea__s_p_outlook"        "south_korea__te_outlook"         "australia__fitch_rating"         "australia__moody_s_rating"      
# [101] "australia__s_p_rating"           "australia__te_rating"            "australia__fitch_outlook"        "australia__moody_s_outlook"     
# [105] "australia__s_p_outlook"          "australia__te_outlook"           "spain__fitch_rating"             "spain__moody_s_rating"          
# [109] "spain__s_p_rating"               "spain__te_rating"                "spain__fitch_outlook"            "spain__moody_s_outlook"         
# [113] "spain__s_p_outlook"              "spain__te_outlook"              
#  
# debugging of italy on May 01, 1996 
# res2 <- get_large_nationals_last_know_bond_ratings_by_month(keep_eom_date_since = "1990-01-01")
# res2[, grep("dateindex|dateindex_dt|^italy.*", names(res2), perl = TRUE, value = TRUE)[c(1:2, 3:6)] , drop = FALSE]
# res2[, grep("dateindex|dateindex_dt|^italy.*", names(res2), perl = TRUE, value = TRUE)[c(1:2,7:10)] , drop = FALSE]



credit_rating_descs <- function() {
  
  message("Begin function: credit_rating_descs")
  
  # R version 3.4.2 (2017-09-28)
  # NOV 2017
  
  # uses package htmltab function htmltab        # htmltab::htmltab
  # uses package tidyr   function fill           # tidyr::fill
  
  # NOTE: may? timeout (curl 10 seconds )
  #                         # SAME on EVERY PAGE
  credit_rating_descs <- try( 
      htmltab::htmltab(doc = "https://tradingeconomics.com/israel/rating", which = 3)
    , silent = TRUE
  )
  # try once more
  if(inherits(credit_rating_descs, 'try-error')) { 
    Sys.sleep(1.0)
    credit_rating_descs <- try( 
      htmltab::htmltab(doc = "https://tradingeconomics.com/israel/rating", which = 3)
    , silent = TRUE
    )
    if(inherits(credit_rating_descs, 'try-error')) { stop(paste0("counld not browse: ","https://tradingeconomics.com/israel/rating")) }
  }

  # clean
  credit_rating_descs<- data.frame(lapply(credit_rating_descs, function(x) { 
    if(is.character(x)) {
      # prevent 'drop to factor ... all 1s'
      # so I can do tidyr and PostgreSQL
      x <- tolower(x)
      x <- gsub("(,|[.])","", x)
      x <- gsub("(&|'| )","_", x)
    }
    return(x)
  }), stringsAsFactors = FALSE)
  
  # names(credit_rating_descs)    <- tolower(names(credit_rating_descs))
  # names(credit_rating_descs)[5] <- "credit_rating_long_desc"
  
  # But for now, I will just hard code
  names(credit_rating_descs) <- c("te_rating", "s_p_rating", "moody_s_rating", "fitch_rating", "credit_rating_long_desc")
  
  # last observation carried forward
  credit_rating_descs <- tidyr::fill(credit_rating_descs, te_rating, s_p_rating, moody_s_rating, fitch_rating, credit_rating_long_desc)

  # logical fix at 'very low ratings'
  credit_rating_descs[20:24,"te_rating"] <- c("7","5","3","2","1")
  
  # gives a math rating
  credit_rating_descs[["te_rating"]]     <- as.numeric(credit_rating_descs[["te_rating"]])

  # CORRECT htmltab::htmltab, definitely an error on row.names (problem: starts at "2", fix: starts at "1")
  # re-number
  row.names(credit_rating_descs) <- seq_along(row.names(credit_rating_descs))
  
  # SAVE (just in case this 'table of values' changes, then I can refer back)
  # print(credit_rating_descs)
  
  #    te_rating s_p_rating moody_s_rating fitch_rating                       credit_rating_long_desc
  # 1        100        aaa            aaa          aaa                                         prime
  # 2         95        aa+            aa1          aa+                                    high_grade
  # 3         90         aa            aa2           aa                                    high_grade
  # 4         85        aa-            aa3          aa-                                    high_grade
  # 5         80         a+             a1           a+                            upper_medium_grade
  # 6         75          a             a2            a                            upper_medium_grade
  # 7         70         a-             a3           a-                            upper_medium_grade
  # 8         65       bbb+           baa1         bbb+                            lower_medium_grade
  # 9         60        bbb           baa2          bbb                            lower_medium_grade
  # 10        55       bbb-           baa3         bbb-                            lower_medium_grade
  # 11        50        bb+            ba1          bb+             non-investment_grade__speculative
  # 12        45         bb            ba2           bb             non-investment_grade__speculative
  # 13        40        bb-            ba3          bb-             non-investment_grade__speculative
  # 14        35         b+             b1           b+                            highly_speculative
  # 15        30          b             b2            b                            highly_speculative
  # 16        25         b-             b3           b-                            highly_speculative
  # 17        20       ccc+           caa1          ccc                             substantial_risks
  # 18        15        ccc           caa2          ccc                         extremely_speculative
  # 19        10       ccc-           caa3          ccc in_default_with_little__prospect_for_recovery
  # 20         7         cc             ca          ccc in_default_with_little__prospect_for_recovery
  # 21         5          c              c          ccc in_default_with_little__prospect_for_recovery
  # 22         3          d                        ddd                                    in_default
  # 23         2          d              //           dd                                    in_default
  # 24         1          d              /            d                                    in_default
  
  message("End function: credit_rating_descs")
  
  return(credit_rating_descs)
  
}
# res <- credit_rating_descs()
# > str(res)
# 'data.frame':	24 obs. of  5 variables:
#  $ te_rating              : num  100 95 90 85 80 75 70 65 60 55 ...
#  $ s_p_rating             : chr  "aaa" "aa+" "aa" "aa-" ...
#  $ moody_s_rating         : chr  "aaa" "aa1" "aa2" "aa3" ...
#  $ fitch_rating           : chr  "aaa" "aa+" "aa" "aa-" ...
#  $ credit_rating_long_desc: chr  "prime" "high_grade" "high_grade" "high_grade" ...
# > res
#    te_rating s_p_rating moody_s_rating fitch_rating                       credit_rating_long_desc
# 1        100        aaa            aaa          aaa                                         prime
# 2         95        aa+            aa1          aa+                                    high_grade
# 3         90         aa            aa2           aa                                    high_grade
# 4         85        aa-            aa3          aa-                                    high_grade
# 5         80         a+             a1           a+                            upper_medium_grade
# 6         75          a             a2            a                            upper_medium_grade
# 7         70         a-             a3           a-                            upper_medium_grade
# 8         65       bbb+           baa1         bbb+                            lower_medium_grade
# 9         60        bbb           baa2          bbb                            lower_medium_grade
# 10        55       bbb-           baa3         bbb-                            lower_medium_grade
# 11        50        bb+            ba1          bb+             non-investment_grade__speculative
# 12        45         bb            ba2           bb             non-investment_grade__speculative
# 13        40        bb-            ba3          bb-             non-investment_grade__speculative
# 14        35         b+             b1           b+                            highly_speculative
# 15        30          b             b2            b                            highly_speculative
# 16        25         b-             b3           b-                            highly_speculative
# 17        20       ccc+           caa1          ccc                             substantial_risks
# 18        15        ccc           caa2          ccc                         extremely_speculative
# 19        10       ccc-           caa3          ccc in_default_with_little__prospect_for_recovery
# 20         7         cc             ca          ccc in_default_with_little__prospect_for_recovery
# 21         5          c              c          ccc in_default_with_little__prospect_for_recovery
# 22         3          d                        ddd                                    in_default
# 23         2          d              //           dd                                    in_default
# 24         1          d              /            d                                    in_default

                                                                        # default in internal funcions "2003-01-01"
get_large_nationals_last_know_bond_ratings_by_month_numeric <- function(keep_eom_date_since = NULL) {

  message("Begin function: get_large_nationals_last_know_bond_ratings_by_month_numeric")
  
  # R version 3.4.2 (2017-09-28)
  # NOV 2017
  
  # uses function get_large_nationals_last_know_bond_ratings_by_month
  # uses function credit_rating_descs
  
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  if(!is.null(keep_eom_date_since)) {
    country_bond_rats_by_month  <- get_large_nationals_last_know_bond_ratings_by_month(keep_eom_date_since = keep_eom_date_since)
  } else {
    # default in funcions "2003-01-01"
    country_bond_rats_by_month <- get_large_nationals_last_know_bond_ratings_by_month()
  }
  credit_ratings <- credit_rating_descs()
  
  # note: FOR RIGHT NOW, I AM NOT DOING "outlook"
  
  # trick on unique: end trailing repeated items are reduced to just 'one ' item
  # e.g. c("/", "/", "/") -> unique -> c("\")
  
  moody_s_rating        <- credit_ratings[seq_along(unique(credit_ratings[["moody_s_rating"]])),"te_rating"]
  names(moody_s_rating) <-                          unique(credit_ratings[["moody_s_rating"]])
  
  fitch_rating        <- credit_ratings[seq_along(unique(credit_ratings[["fitch_rating"]])),"te_rating"]
  names(fitch_rating) <-                          unique(credit_ratings[["fitch_rating"]])
  
  s_p_rating         <- credit_ratings[seq_along(unique(credit_ratings[["s_p_rating"]])),"te_rating"]
  names(s_p_rating)  <-                          unique(credit_ratings[["s_p_rating"]])
  
  # prepare
  country_bond_rats_by_month_numeric  <- country_bond_rats_by_month
  
  
  for(country_col_i in setdiff(gsub("^(.*)(__)(.*)$", "\\1", names(country_bond_rats_by_month), perl = TRUE), c("dateindex","dateindex_dt"))) {
  
    for(rating_i in c("s_p_rating", "moody_s_rating", "fitch_rating")) {
  
      # assign
      # country_bond_rats_by_month_numeric[["italy__moody_s_rating"]] <- moody_s_rating[match(country_bond_rats_by_month_numeric[["italy__moody_s_rating"]], names(moody_s_rating))]
      # country_bond_rats_by_month_numeric[[paste0("italy", "__", "moody_s_rating")]] <- get(paste0("moody_s_rating"))[match(country_bond_rats_by_month_numeric[[paste0("italy", "__", "moody_s_rating")]], names(get(paste0("moody_s_rating"))))]
      # 
      # country_col_i <- "italy"
      # rating_i      <- "moody_s_rating"
      country_bond_rats_by_month_numeric[[paste0(country_col_i, "__", rating_i)]] <- get(paste0(rating_i))[match(country_bond_rats_by_month_numeric[[paste0(country_col_i, "__", rating_i)]], names(get(paste0(rating_i))))]
      
    }
  
  }

  on.exit({Sys.setenv(TZ=oldtz)})
  
  message("End function: get_large_nationals_last_know_bond_ratings_by_month_numeric")
  
  return(country_bond_rats_by_month_numeric)

}
# res <- get_large_nationals_last_know_bond_ratings_by_month_numeric()
# > str(res[, c("dateindex","dateindex_dt", grep("^italy.*rating$",names(res), perl = TRUE, value = TRUE))], list.len = 999L)
# 'data.frame':	179 obs. of  6 variables:
#  $ dateindex            : int  12083 12111 12142 12172 12203 12233 12264 12295 12325 12356 ...
#  $ dateindex_dt         : Date, format: "2003-01-31" "2003-02-28" "2003-03-31" "2003-04-30" ...
#  $ italy__fitch_rating  : num  90 90 90 90 90 90 90 90 90 90 ...
#  $ italy__moody_s_rating: num  90 90 90 90 90 90 90 90 90 90 ...
#  $ italy__s_p_rating    : num  90 90 90 90 90 90 90 90 90 90 ...
#  $ italy__te_rating     : num  NA NA NA NA NA NA NA NA NA NA ...


                                                                        # default in internal funcions "2003-01-01"
get_one_large_nationals_bond_bond_ratings_wtd_by_month  <- function(keep_eom_date_since = NULL) {

  message("Begin function: get_one_large_nationals_bond_bond_ratings_wtd_by_month")

  # R version 3.4.2 (2017-09-28)
  # NOV 2017
  
  # uses function get_large_nationals_yearly_gdp_weights_by_month
  # uses function get_large_nationals_last_know_bond_ratings_by_month_numeric

  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  # limit to month ends of interest
  if(!is.null(keep_eom_date_since)) {
    large_nationals_yearly_gdp_weights_by_month       <- get_large_nationals_yearly_gdp_weights_by_month(keep_eom_date_since = keep_eom_date_since)
    large_nationals_last_know_bond_ratings_by_month_numeric <- get_large_nationals_last_know_bond_ratings_by_month_numeric(keep_eom_date_since = keep_eom_date_since)
  } else {
    # default in funcions "2003-01-01"
    large_nationals_yearly_gdp_weights_by_month       <- get_large_nationals_yearly_gdp_weights_by_month()
    large_nationals_last_know_bond_ratings_by_month_numeric <- get_large_nationals_last_know_bond_ratings_by_month_numeric()
  }
  
  # adjustments
  names(large_nationals_last_know_bond_ratings_by_month_numeric) <- 
    gsub("(^south_korea__)", "korea_rep__" ,     names(large_nationals_last_know_bond_ratings_by_month_numeric))
    
  names(large_nationals_last_know_bond_ratings_by_month_numeric) <-
    gsub("(^russia__)", "russian_federation__" , names(large_nationals_last_know_bond_ratings_by_month_numeric))
  
  countries_sorted <- sort(setdiff(gsub("^(.*)(__)(.*)$", "\\1", names(large_nationals_last_know_bond_ratings_by_month_numeric), perl = TRUE), c("dateindex","dateindex_dt")))

  for(country_col_i in setdiff(gsub("^(.*)(__)(.*)$", "\\1", names(large_nationals_last_know_bond_ratings_by_month_numeric), perl = TRUE), c("dateindex","dateindex_dt"))) {
  
    # I am not doing __te_rating
  
    # average rating among the two or three rating agencies
    large_nationals_last_know_bond_ratings_by_month_numeric[[paste0(country_col_i, "__", "rating_mean")]] <- 
      rowMeans(large_nationals_last_know_bond_ratings_by_month_numeric[,c(paste0(country_col_i, "__", "fitch_rating"),paste0(country_col_i, "__", "moody_s_rating"),paste0(country_col_i, "__", "s_p_rating")),drop = FALSE], na.rm = TRUE)
  }
  

  # join
  large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights <- 
  merge(large_nationals_last_know_bond_ratings_by_month_numeric, large_nationals_yearly_gdp_weights_by_month, all = TRUE)

  # limit to month-ends of interest ( most likely redundant: already done above in arg in inbound functions )
  if(!is.null(keep_eom_date_since)){
    large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights <- 
      large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights[
        large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights$dateindex_dt %in% zoo::as.Date(zoo::as.yearmon(seq(as.Date(keep_eom_date_since), Sys.Date(), by = "month")), frac = 1)
      ,, drop = FALSE]
    
  }
  
  # ( FUTURE: COME_BACK ?)
  # setdiff # garantee columns ( both 'measures' and 'measure weights' )
  # large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights[,  col_vector_column not_found] <- NA_real_
  # 
  # REST OVERSIMPLIFIED (because I did NOT garantee columns) ... ( but good-enough for right now )
  # 

  # possibles
  #
  # sweep
  # apply
  # rowr::rowApply
  # matrixStats::rowWeightedMeans
  
  # final_result ( uses 'countries_sorted' )
  large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights[["all_ratings_mean_gdp_wtd"]] <- 
  rowSums( as.matrix(large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights[, paste0(countries_sorted, "__rating_mean"),drop = FALSE]) * as.matrix(large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights[, paste0(countries_sorted, "__gdp_wdt"),drop = FALSE]), na.rm = TRUE)

  
         large_nationals_bond_bond_ratings_wtd_by_month <- large_nationals_last_know_bond_ratings_by_month_numeric_plus_gdp_weights

  on.exit({Sys.setenv(TZ=oldtz)})
         
  message("End function: get_one_large_nationals_bond_bond_ratings_wtd_by_month")
         
  return(large_nationals_bond_bond_ratings_wtd_by_month)

}
# res <- get_one_large_nationals_bond_bond_ratings_wtd_by_month()
# > str(res, list.len = 999)
# 'data.frame':	179 obs. of  143 variables:
#  $ dateindex                          : int  12083 12111 12142 12172 12203 12233 12264 12295 12325 12356 ...
#  $ dateindex_dt                       : Date, format: "2003-01-31" "2003-02-28" "2003-03-31" "2003-04-30" ...
#  $ united_states__fitch_rating        : num  100 100 100 100 100 100 100 100 100 100 ...
#  $ united_states__moody_s_rating      : num  100 100 100 100 100 100 100 100 100 100 ...
#  $ united_states__s_p_rating          : num  NA NA NA NA NA NA NA NA NA NA ...
#  $ united_states__te_rating           : num  NA NA NA NA NA NA NA NA NA NA ...
#  $ united_states__fitch_outlook       : chr  "stable" "stable" "stable" "stable" ...
#  $ united_states__moody_s_outlook     : chr  "stable" "stable" "stable" "stable" ...
#  $ united_states__s_p_outlook         : chr  NA NA NA NA ...
#  $ united_states__te_outlook          : chr  NA NA NA NA ...
# ...
#  $ united_states__rating_mean         : num  100 100 100 100 100 100 100 100 100 100 ...
#  $ china__rating_mean                 : num  66.7 66.7 66.7 66.7 66.7 ...
#  $ japan__rating_mean                 : num  90 90 90 90 90 90 90 90 90 90 ...
#  $ germany__rating_mean               : num  100 100 100 100 100 100 100 100 100 100 ...
#  $ united_kingdom__rating_mean        : num  100 100 100 100 100 100 100 100 100 100 ...
#  $ india__rating_mean                 : num  45 46.7 46.7 46.7 46.7 ...
#  $ france__rating_mean                : num  100 100 100 100 100 100 100 100 100 100 ...
#  $ brazil__rating_mean                : num  31.7 31.7 31.7 31.7 31.7 ...
#  $ italy__rating_mean                 : num  90 90 90 90 90 90 90 90 90 90 ...
#  $ canada__rating_mean                : num  98.3 98.3 98.3 98.3 98.3 ...
#  $ russian_federation__rating_mean    : num  43.3 43.3 43.3 43.3 46.7 ...
#  $ korea_rep__rating_mean             : num  71.7 71.7 71.7 71.7 71.7 ...
#  $ australia__rating_mean             : num  95 98.3 98.3 98.3 98.3 ...
#  $ spain__rating_mean                 : num  96.7 96.7 96.7 96.7 96.7 ...
#  $ australia__gdp_wdt                 : num  0.0146 0.0146 0.0146 0.0146 0.0146 ...
#  $ brazil__gdp_wdt                    : num  0.0188 0.0188 0.0188 0.0188 0.0188 ...
#  $ canada__gdp_wdt                    : num  0.0281 0.0281 0.0281 0.0281 0.0281 ...
#  $ china__gdp_wdt                     : num  0.0545 0.0545 0.0545 0.0545 0.0545 ...
#  $ france__gdp_wdt                    : num  0.0556 0.0556 0.0556 0.0556 0.0556 ...
#  $ germany__gdp_wdt                   : num  0.077 0.077 0.077 0.077 0.077 ...
#  $ india__gdp_wdt                     : num  0.0188 0.0188 0.0188 0.0188 0.0188 ...
#  $ italy__gdp_wdt                     : num  0.0469 0.0469 0.0469 0.0469 0.0469 ...
#  $ japan__gdp_wdt                     : num  0.152 0.152 0.152 0.152 0.152 ...
#  $ korea_rep__gdp_wdt                 : num  0.0226 0.0226 0.0226 0.0226 0.0226 ...
#  $ russian_federation__gdp_wdt        : num  0.0128 0.0128 0.0128 0.0128 0.0128 ...
#  $ spain__gdp_wdt                     : num  0.0261 0.0261 0.0261 0.0261 0.0261 ...
#  $ united_kingdom__gdp_wdt            : num  0.0651 0.0651 0.0651 0.0651 0.0651 ...
#  $ united_states__gdp_wdt             : num  0.407 0.407 0.407 0.407 0.407 ...
#  $ all_ratings_mean_gdp_wtd           : num  92.3 92.4 92.4 92.4 92.4 ...
# > 
# PROB NOT HELPFUL ( GOES UP BEFORE A RECESSION )
# res[,c("dateindex", "dateindex_dt","all_ratings_mean_gdp_wtd")]
# 
# 49      13544   2007-01-31                 92.17384
# 50      13572   2007-02-28                 92.17384
# 51      13603   2007-03-31                 92.17384
# 52      13633   2007-04-30                 92.37025
# 53      13664   2007-05-31                 92.46629
# 54      13694   2007-06-30                 92.46629
# 55      13725   2007-07-31                 92.62947
# 56      13756   2007-08-31                 92.67748
# 57      13786   2007-09-30                 92.67748
# 58      13817   2007-10-31                 92.67748
# 59      13847   2007-11-30                 92.79680
# 60      13878   2007-12-31                 92.06020
# 61      13909   2008-01-31                 92.06020
# 62      13938   2008-02-29                 92.06020
# 63      13969   2008-03-31                 92.06020
# 64      13999   2008-04-30                 92.11469
# 65      14030   2008-05-31                 92.16918
# 66      14060   2008-06-30                 92.16918
# 67      14091   2008-07-31                 92.35841
# 68      14122   2008-08-31                 92.35841
# 69      14152   2008-09-30                 92.35841
# 70      14183   2008-10-31                 92.35841
# 71      14213   2008-11-30                 92.35841
# 72      14244   2008-12-31                 91.75374
# 
# 103     15186   2011-07-31                 90.37297
# 104     15217   2011-08-31                 89.65630
# 105     15247   2011-09-30                 89.58260
# 106     15278   2011-10-31                 89.03956
# 107     15308   2011-11-30                 89.15580
# 108     15339   2011-12-31                 88.64830
# 109     15370   2012-01-31                 88.08578
# 110     15399   2012-02-29                 87.83104
# 111     15430   2012-03-31                 87.83104
# 112     15460   2012-04-30                 87.73762
# 113     15491   2012-05-31                 87.35105
# 114     15521   2012-06-30                 87.07079
# 115     15552   2012-07-31                 86.92788
# 116     15583   2012-08-31                 86.96562
# 
# 150     16616   2015-06-30                 86.18115
# 151     16647   2015-07-31                 86.18115
# 152     16678   2015-08-31                 86.10958
# 153     16708   2015-09-30                 85.85483
# 154     16739   2015-10-31                 85.82336
# 155     16769   2015-11-30                 85.82336
# 156     16800   2015-12-31                 86.72017 # credit rating jump ( but eoy gdp refresh )
# 157     16831   2016-01-31                 86.72017
# 158     16860   2016-02-29                 86.55538
# 159     16891   2016-03-31                 86.55538



# given 
# two dataframes from two different sources that are close to each other ( but not exactly the same )
#
# generate 
# replacement data.frame(s)
# (replacement data.frame(s) are meant to be uses with the R package 'unjoin' and 
# function ( unjoin::unjoin )
# 
# NOTE: also, TODO [ ] add a metadata fix ( e.g. column name: table/start_column/end_column  ) (EASY)
# 
# NOTE: I did combinations of '1 variable' and 'all variables'
# I did not (yet!) do combinations ( e.g. combn ) of other variable subsets ( NOT SURE where THIS value GOES )
# (QUITE DIFFICULT)
#
bridge_dfs <- function( dbfs, adjs = list() ) {

  
  `%like%` <- function(x, pattern) {
  
      if (is.factor(x)) {
          as.integer(x) %in% grep(pattern, levels(x))
      }
      else {
          grepl(pattern, x)
      }
  }
  
  # one-off (practical/realistic) data adjustments
  for(adjs_i in adjs) { 
    
    temp <-  dbfs[[adjs_i[1]]]
    # replace column element values
    temp[[adjs_i[2]]] <- blockmodeling::recode(temp[[adjs_i[2]]], adjs_i[3], adjs_i[4]) 
    dbfs[[adjs_i[1]]] <- temp
    rm("temp")
    
  }
    
  # NEED one-off (practical/realistic) meta-data adjustements
  # TODO[ ]
  
  # all values in all columns
  dbfs[[paste0(names(dbfs), collapse = "__")]] <- plyr::join_all(dbfs, type = "full")
  
  dbfs_new <- list()
  
  dbfs_index <- 0L
  for(dbfs_i in dbfs) {
    
    dbfs_index  <- dbfs_index + 1L
    # specific data.frame name
    dbfs_i_name <- names(dbfs)[dbfs_index] 
    
    # LEFT_OFF ( NOTHING SPECIAL: JUST 'MORE' 'single column values
    # browser( expr = { dbfs_i_name == "house__zoo"  } )
    
    # > combn(colnames(dbfs_i), 1, simplify = FALSE)
    # [[1]]
    # [1] "animal"
    
    # [[2]]
    # [1] "color"
    
    # LEFT_OFF: need to handle the multiple case
    # > combn(colnames(dbfs_i), 2, simplify = FALSE)
    # [[1]]
    # [1] "animal" "color" 
    
    # single column ONLY 
    for(columns_i_name in colnames(dbfs_i)){
      
      # in the original data.frames constuct columns of non-unique record identifiers
      #
      # column values vector
      temp <- dbfs[[dbfs_i_name]][[columns_i_name]]
      # new column
      dbfs[[dbfs_i_name]][[paste0(dbfs_i_name, "__", columns_i_name, "__id")]] <- seq_along(temp)
      rm("temp")
      dbfs[[dbfs_i_name]] <- DataCombine::MoveFront(dbfs[[dbfs_i_name]], paste0(dbfs_i_name, "__", columns_i_name, "__id") )
      
      # construct new data.frames of unique values
      # 
      # unique column values
      unique_temp <- sort(unique(dbfs[[dbfs_i_name]][[columns_i_name]]))
      temp <- data.frame(id = seq_along(unique_temp), value = unique_temp, stringsAsFactors = FALSE)
      rm("unique_temp")
      # column names
      names(temp) <- paste0(paste0(dbfs_i_name, "__", columns_i_name), c("__unique__id", "__unique__value"))
      temp <- list(temp)
      # the data.frame name itself
      names(temp)[1] <- paste0(dbfs_i_name, "__", columns_i_name, "__unique")
      dbfs_new    <- c(dbfs_new, temp) 
      rm("temp")
      
      # from the data.frames of unique values
      # assign those unique ids into the original data.frames
      # 
      dbfs[[dbfs_i_name]][[paste0(dbfs_i_name, "__", columns_i_name, "__unique__id")]] <-
      as.integer(
        blockmodeling::recode(
            dbfs[[dbfs_i_name]][[columns_i_name]]
          , dbfs_new[[paste0(dbfs_i_name, "__", columns_i_name, "__unique")]][[paste0(dbfs_i_name, "__", columns_i_name, "__unique__value")]]
          , dbfs_new[[paste0(dbfs_i_name, "__", columns_i_name, "__unique")]][[paste0(dbfs_i_name, "__", columns_i_name, "__unique__id")]]
        )
      )
      dbfs[[dbfs_i_name]] <- DataCombine::MoveFront(dbfs[[dbfs_i_name]], paste0(dbfs_i_name, "__", columns_i_name, "__unique__id") )
      
    }
  }
  
  ## # all single data.frame results
  ## dbfs_new[[paste(names(dbfs), collapse = "__")]] <- plyr::join_all(dbfs, type = "full")
  
  ## 
  ## # begin multiple data.frame results
  ## temp <- dbfs_new[[paste(names(dbfs), collapse = "__")]][,!colnames(get(dbfs_i)) %like% "__id$", drop = FALSE]
  
  return(list(dbfs=dbfs,dbfs_new=dbfs_new))
}
# call-ish
# bridge_dfs( Hmisc_llist_of_dfs, list( c(table, col,  from_value, to_value ), c(table, col,  from_value, to_value ) )

# # BEGIN TEST
# house  <- data.frame(animal = c("cat2", "dog","cat2"), color = c("fire", "blue", "blue"), stringsAsFactors = FALSE)
# 
# zoo    <- data.frame(animal = c("monkey","cat","cat"), color = c("yellow", "red", "red"), stringsAsFactors = FALSE)
# 
# res <- bridge_dfs( 
#      dbfs = list(house = house, zoo = zoo) # pass a named list
#    , adjs = list( 
#        c("house", "animal",  "cat2", "cat" ) # table/column/start_value/replaced_value
#      , c("house", "color" ,  "fire", "red" )
#    ) 
# ) 
#
# > str(res$dbfs, vec.len = 999)
# List of 3
#  $ house     :'data.frame':	3 obs. of  6 variables:
#   ..$ house__color__unique__id : int [1:3] 2 1 1
#   ..$ house__color__id         : int [1:3] 1 2 3
#   ..$ house__animal__unique__id: int [1:3] 1 2 1
#   ..$ house__animal__id        : int [1:3] 1 2 3
#   ..$ animal                   : chr [1:3] "cat" "dog" "cat"
#   ..$ color                    : chr [1:3] "red" "blue" "blue"
#  $ zoo       :'data.frame':	3 obs. of  6 variables:
#   ..$ zoo__color__unique__id : int [1:3] 2 1 1
#   ..$ zoo__color__id         : int [1:3] 1 2 3
#   ..$ zoo__animal__unique__id: int [1:3] 2 1 1
#   ..$ zoo__animal__id        : int [1:3] 1 2 3
#   ..$ animal                 : chr [1:3] "monkey" "cat" "cat"
#   ..$ color                  : chr [1:3] "yellow" "red" "red"
#  $ house__zoo:'data.frame':	5 obs. of  6 variables:
#   ..$ house__zoo__color__unique__id : int [1:5] 2 2 1 1 3
#   ..$ house__zoo__color__id         : int [1:5] 1 2 3 4 5
#   ..$ house__zoo__animal__unique__id: int [1:5] 1 1 2 1 3
#   ..$ house__zoo__animal__id        : int [1:5] 1 2 3 4 5
#   ..$ animal                        : chr [1:5] "cat" "cat" "dog" "cat" "monkey"
#   ..$ color                         : chr [1:5] "red" "red" "blue" "blue" "yellow"
#   
# > str(res$dbfs_new, vec.len = 999)
# List of 6
#  $ house__animal__unique     :'data.frame':	2 obs. of  2 variables:
#   ..$ house__animal__unique__id   : int [1:2] 1 2
#   ..$ house__animal__unique__value: chr [1:2] "cat" "dog"
#  $ house__color__unique      :'data.frame':	2 obs. of  2 variables:
#   ..$ house__color__unique__id   : int [1:2] 1 2
#   ..$ house__color__unique__value: chr [1:2] "blue" "red"
#  $ zoo__animal__unique       :'data.frame':	2 obs. of  2 variables:
#   ..$ zoo__animal__unique__id   : int [1:2] 1 2
#   ..$ zoo__animal__unique__value: chr [1:2] "cat" "monkey"
#  $ zoo__color__unique        :'data.frame':	2 obs. of  2 variables:
#   ..$ zoo__color__unique__id   : int [1:2] 1 2
#   ..$ zoo__color__unique__value: chr [1:2] "red" "yellow"
#  $ house__zoo__animal__unique:'data.frame':	3 obs. of  2 variables:
#   ..$ house__zoo__animal__unique__id   : int [1:3] 1 2 3
#   ..$ house__zoo__animal__unique__value: chr [1:3] "cat" "dog" "monkey"
#  $ house__zoo__color__unique :'data.frame':	3 obs. of  2 variables:
#   ..$ house__zoo__color__unique__id   : int [1:3] 1 2 3
#   ..$ house__zoo__color__unique__value: chr [1:3] "blue" "red" "yellow"
# 
# # END TEST



get_av_agg_eom_xts <- function() {

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
  
  require(quantmod)

  message("Begin function get_av_agg_eom_xts.")

  # setDefaults(getSymbols.av, api.key="YOURAPIKEY")
    
  AGG <- getSymbols("AGG", src = "av", output.size = "full", auto.assign = FALSE)
  temp <- AGG[,"AGG.Close"]
  colnames(temp)[1] <- "agg"
  temp <- to.monthly(temp, OHLC = FALSE, indexAt = "lastof") 
  av_agg_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_av_agg_eom_xts.")

  return(av_agg_eom_xts)

}
# ret <- get_av_agg_eom_xts()
# > str(ret)
# An 'xts' object on 2003-09-30/2017-12-31 containing:
#   Data: num [1:172, 1] 103 102 102 102 103 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr "agg"
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:
# List of 2
#  $ src    : chr "alphavantage"
#  $ updated: Date[1:1], format: "2017-12-01"
# > head(ret)
#                           agg
# 2003-09-30 102.70000000000000
# 2003-10-31 101.73999999999999
# 2003-11-30 101.72000000000000
# 2003-12-31 102.15000000000001
# 2004-01-31 102.59999999999999
# 2004-02-29 103.48999999999999
# > tail(ret)
#                           agg
# 2017-07-31 109.65000000000001
# 2017-08-31 110.45000000000000
# 2017-09-30 109.59000000000000
# 2017-10-31 109.47000000000000
# 2017-11-30 109.08000000000000
# 2017-12-31 109.16000000000000



get_fred_wilshire5000_eom_xts <- function() {

  # The total market indexes are total market returns, which do include reinvested dividends. 
  # https://fred.stlouisfed.org/series/WILL5000IND

  # ORIG FROM ( INSPIRED BY )
  # The equity premium
  # https://fredblog.stlouisfed.org/2016/07/the-equity-premium/
  
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
  
  require(quantmod)

  message("Begin function get_fred_wilshire5000_eom_xts.")

  WILL5000IND <- getSymbols("WILL5000IND", src = "FRED", from = "1950-01-01", auto.assign = FALSE) # SINCE DEC 1970
  temp <- WILL5000IND
  colnames(temp)[1] <- tolower(colnames(temp)[1])
  temp <- to.monthly(temp, OHLC = FALSE, indexAt = "lastof") 
  fred_wilshire5000_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_fred_wilshire5000_eom_xts.")

  return(fred_wilshire5000_eom_xts)

}
# > ret <- get_fred_wilshire5000_eom_xts()
# Begin function get_fred_wilshire5000_eom_xts.
# End   function get_fred_wilshire5000_eom_xts.
# > str(ret)
# An 'xts' object on 1970-12-31/2017-11-30 containing:
#   Data: num [1:564, 1] 1 1.05 1.07 1.12 1.16 1.12 1.13 1.09 1.13 1.12 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr "will5000ind"
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:
# List of 3
#  $ src      : chr "FRED"
#  $ updated  : POSIXct[1:1], format: "2017-12-03 22:20:50"
#  $ na.action:Class 'omit'  atomic [1:2550] 2 3 4 5 6 7 8 9 10 11 ...
#   .. ..- attr(*, "index")= num [1:2550] 31536000 31795200 31881600 31968000 32054400 ...
# > head(ret)
#                   will5000ind
# 1970-12-31 1.0000000000000000
# 1971-01-31 1.0500000000000000
# 1971-02-28 1.0700000000000001
# 1971-03-31 1.1200000000000001
# 1971-04-30 1.1599999999999999
# 1971-05-31 1.1200000000000001
# > tail(ret)
#                   will5000ind
# 2017-06-30 111.14000000000000
# 2017-07-31 113.23000000000000
# 2017-08-31 113.50000000000000
# 2017-09-30 116.23999999999999
# 2017-10-31 118.73999999999999
# 2017-11-30 122.34999999999999



# competiton from bonds ( any reason )
get_willshire_less_agg_equity_premium_eom_xts <- function() {

  # ORIG FROM ( INSPIRED BY )
  # The equity premium
  # https://fredblog.stlouisfed.org/2016/07/the-equity-premium/
  
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
  
  require(xts) # lag.xts
  # uses get_av_agg_eom_xts
  # uses get_fred_wilshire5000_eom_xts

  message("Begin function get_willshire_less_agg_equity_premium_eom_xts.")

  fred_wilshire5000_eom_xts <- get_fred_wilshire5000_eom_xts()  # will5000ind
  av_agg_eom_xts            <- get_av_agg_eom_xts()             # agg
  
  tempw01_ann <- ( fred_wilshire5000_eom_xts - lag.xts(fred_wilshire5000_eom_xts, 1) ) / abs(lag.xts(fred_wilshire5000_eom_xts, 1)) * 12.0/ 1.0 * 100.00
  tempw02_ann <- ( fred_wilshire5000_eom_xts - lag.xts(fred_wilshire5000_eom_xts, 2) ) / abs(lag.xts(fred_wilshire5000_eom_xts, 2)) * 12.0/ 2.0 * 100.00
  tempw03_ann <- ( fred_wilshire5000_eom_xts - lag.xts(fred_wilshire5000_eom_xts, 3) ) / abs(lag.xts(fred_wilshire5000_eom_xts, 3)) * 12.0/ 3.0 * 100.00
  tempw06_ann <- ( fred_wilshire5000_eom_xts - lag.xts(fred_wilshire5000_eom_xts, 6) ) / abs(lag.xts(fred_wilshire5000_eom_xts, 6)) * 12.0/ 6.0 * 100.00
  tempw09_ann <- ( fred_wilshire5000_eom_xts - lag.xts(fred_wilshire5000_eom_xts, 9) ) / abs(lag.xts(fred_wilshire5000_eom_xts, 9)) * 12.0/ 9.0 * 100.00
  tempw12_ann <- ( fred_wilshire5000_eom_xts - lag.xts(fred_wilshire5000_eom_xts,12) ) / abs(lag.xts(fred_wilshire5000_eom_xts,12)) * 12.0/12.0 * 100.00
  
  tempa01_ann <- ( av_agg_eom_xts - lag.xts(av_agg_eom_xts, 1) ) / abs(lag.xts(av_agg_eom_xts, 1)) * 12.0/ 1.0 * 100.00
  tempa02_ann <- ( av_agg_eom_xts - lag.xts(av_agg_eom_xts, 2) ) / abs(lag.xts(av_agg_eom_xts, 2)) * 12.0/ 2.0 * 100.00
  tempa03_ann <- ( av_agg_eom_xts - lag.xts(av_agg_eom_xts, 3) ) / abs(lag.xts(av_agg_eom_xts, 3)) * 12.0/ 3.0 * 100.00
  tempa06_ann <- ( av_agg_eom_xts - lag.xts(av_agg_eom_xts, 6) ) / abs(lag.xts(av_agg_eom_xts, 6)) * 12.0/ 6.0 * 100.00
  tempa09_ann <- ( av_agg_eom_xts - lag.xts(av_agg_eom_xts, 9) ) / abs(lag.xts(av_agg_eom_xts, 9)) * 12.0/ 9.0 * 100.00
  tempa12_ann <- ( av_agg_eom_xts - lag.xts(av_agg_eom_xts,12) ) / abs(lag.xts(av_agg_eom_xts,12)) * 12.0/12.0 * 100.00
  
  temp <- merge.xts( 
      tempw01_ann - tempa01_ann
    , tempw02_ann - tempa02_ann
    , tempw03_ann - tempa03_ann
    , tempw06_ann - tempa06_ann
    , tempw09_ann - tempa09_ann
    , tempw12_ann - tempa12_ann
    )
    
  # equity premium past(p) XY months
  colnames(temp) <- c(
      "equity_prem_p01m_ann"
    , "equity_prem_p02m_ann"
    , "equity_prem_p03m_ann"
    , "equity_prem_p06m_ann"
    , "equity_prem_p09m_ann"
    , "equity_prem_p12m_ann"
    )
  
  willshire_less_agg_equity_premium_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_willshire_less_agg_equity_premium_eom_xts.")

  return(willshire_less_agg_equity_premium_eom_xts)

}
# ret <- get_willshire_less_agg_equity_premium_eom_xts()
# 3-MONTH IS TOO VOLITILE TO BE TRUSTED
# I DID NOT SEE ANY DIFFERENCE BETWEEN 6,9.12-MONTH
# dygraphs::dygraph(ret[,c("equity_prem_p03m_ann","equity_prem_p06m_ann","equity_prem_p09m_ann","equity_prem_p12m_ann")])
# 
# > str(ret)
# An 'xts' object on 2003-09-30/2017-11-30 containing:
#   Data: num [1:171, 1:6] NA 84.3 17.1 48.9 21.4 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:6] "equity_prem_p01m_ann" "equity_prem_p02m_ann" "equity_prem_p03m_ann" "equity_prem_p06m_ann" ...
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:
# List of 3
#  $ src      : chr "FRED"
#  $ updated  : POSIXct[1:1], format: "2017-12-03 22:56:15"
#  $ na.action:Class 'omit'  atomic [1:2550] 2 3 4 5 6 7 8 9 10 11 ...
#   .. ..- attr(*, "index")= num [1:2550] 31536000 31795200 31881600 31968000 32054400 ...
#   
# > head(ret)
#            equity_prem_p01m_ann equity_prem_p02m_ann equity_prem_p03m_ann equity_prem_p06m_ann equity_prem_p09m_ann equity_prem_p12m_ann
# 2003-09-30                   NA                   NA                   NA                   NA                   NA                   NA
# 2003-10-31  84.2987450884582472                   NA                   NA                   NA                   NA                   NA
# 2003-11-30  17.1130481177452651   51.218714679298330                   NA                   NA                   NA                   NA
# 2003-12-31  48.9323460982841851   33.402967470490964   51.837654936685105                   NA                   NA                   NA
# 2004-01-31  21.3658774060835412   35.737669848211752   29.913895463481893                   NA                   NA                   NA
# 2004-02-29   7.0787195864594263   14.393577095692896   26.552312397718790                   NA                   NA                   NA
# 
# > tail(ret,12)
#            equity_prem_p01m_ann equity_prem_p02m_ann equity_prem_p03m_ann equity_prem_p06m_ann equity_prem_p09m_ann equity_prem_p12m_ann
# 2016-12-31  26.6785409468942873  57.6792731404528567  33.6783351827045294  26.1460287693127249   19.396159765021100   13.318836650160643
# 2017-01-31  18.8136356428537788  22.9679704390198687  45.2262122134306637  21.9183884075927082   20.889474629676457   23.013005909026440
# 2017-02-28  39.3219182540622683  29.4595654796353159  28.9949591503358377  27.6768256380941722   22.925214476958740   27.842211472730174
# 2017-03-31   3.6451575000364063  21.5007272661133726  20.8718471149072045  27.8160222562867538   25.085154797845274   20.451617949694253
# 2017-04-30   4.1550662765484354   3.9138422275604001  15.8714419300207119  31.4319368317205345   20.390660635441670   20.302472508527266
# 2017-05-31   6.4993794573965795   5.3709383229494136   4.8115608664173877  17.2213815271180231   20.390816491020793   18.739148244610355
# 2017-06-30  13.1888542311096923   9.9032154931740166   8.0595718966889791  14.7901341485866489   21.684611732823850   21.310832179009061
# 2017-07-31  21.0320263311760236  17.2104969234017631  13.7536575930667055  15.1754316734198973   26.251964207042814   19.198544152585530
# 2017-08-31  -5.8936974764027141   7.5904728289958010   9.5384269155111507   7.2908943998708722   15.050994584742723   18.079013826104507
# 2017-09-30  38.3127573821949312  16.2781539891988558  18.0630168838048810  13.3311258156569021   16.414348216283905   21.408621110395330
# 2017-10-31  27.1226602162964525  33.0241164000693956  20.1214408937029035  17.3090251816943663   17.400646991173964   25.602864419556575
# 2017-11-30  40.7582161337507785  34.3304224021625757  36.1509483630243054  23.3301906712948259   17.476032800731414   21.390696660109594
# # RUN OF DEC 03 2017
# 
# # NOTE ( COMPARE TO : https://fredblog.stlouisfed.org/2016/07/the-equity-premium/ )
# > ret["2015/2016"]
#            equity_prem_p01m_ann equity_prem_p02m_ann  equity_prem_p03m_ann  equity_prem_p06m_ann equity_prem_p09m_ann equity_prem_p12m_ann
# 2015-01-31 -57.8567990386709852 -26.5413755239343239 -10.00440168939763552   2.36547709549237428   4.7117603788284619  9.16153232475986812
# 2015-02-28  80.9510484434047441  10.7300704238034079   8.71338104452690487   9.73305556262234539  12.4025926472196097 11.60080700164916578
# 2015-03-31 -16.0413364223876371  32.0817231842582302   1.67299727594256975   9.63858492076925444   6.8873007003371125  8.97285222382645031
# 2015-04-30  12.7317894346522991  -1.6856762308536708  25.71338484011894110   7.86921663226877488  10.4996198198680872 10.37817303103409117
# 2015-05-31  22.9442895041154316  17.8614514804051545   6.48643536279191135   7.64750195124236498   8.7199525365108208 11.01401183025542885
# 2015-06-30  -5.8351614088464032   8.3721959168704920   9.77643481888302368   5.78341837326514607   9.7580635615037554  7.65834232933381820
# 2015-07-31  12.8787999014850953   3.3899506898527330   9.89069658083005265  17.89533395413536354   8.6317661014064146 10.48931055871820917
# 2015-08-31 -65.0168926112570915 -26.6672150485021575 -19.41494740991376489  -6.56855480413585280  -1.6528766363349345  1.28620653590337541
# 2015-09-30 -39.9122732887513152 -51.4746255944353308 -30.61577828607414276 -10.39381546390428390  -6.4922151594906214 -0.81858260693828655
# 2015-10-31  97.5538675664195125  27.5177036318603960  -4.52374192632552763   2.65303848993806746  10.3205817674240912  5.27267701174492665
# 2015-11-30  12.5220911169222102  55.2543234061736328  22.62363796858394593   0.93675803498879295   2.7643022479017980  4.26097393923528145
# 2015-12-31 -14.7936781509440838  -1.2157605464390775  31.21469154050884498  -0.56101723134450809   2.8614859967278492  2.58603347487096480
# 2016-01-31 -80.3710951539452623 -46.8832888920785535 -27.20417039769123235 -15.70184777722573344  -7.3059356769249355  0.58320974925696012
# 2016-02-29  -8.6838292139228823 -44.5624714987857189 -34.14983390501353000  -6.57852760019924787 -10.6642215107928848 -6.43335006049919222
# 2016-03-31  76.7226046806774065  33.9722008189255504  -5.74072679030750876  12.96160095076648489  -2.2784390328552746  0.78021287695523278
# 2016-04-30   6.9802621241255371  42.1188042050102069  25.15038203007103235  -2.08518886573276063  -2.9015228586525401  0.30080769424144049
# 2016-05-31  23.7554380275571226  15.4373916150372459  36.56297429717124459  -0.23375839192358372   7.5077058091240403  0.36053082165282424
# 2016-06-30 -17.0894900474790532   3.3844703901941298   4.63371364852351331  -0.57262641312829121  10.4429189971645826 -0.57596447261033212
# 2016-07-31  42.8418795950987388  12.9123135442783017  16.84830066734769360  21.88920409132333589   4.2146086172883397  1.95594080488325339
# 2016-08-31   7.8570491502432116  25.4183825253064484  11.30420175019935236  24.79036997898009531   3.6706420255988430  8.75726049007443486
# 2016-09-30   2.7046561691669169   5.2790878833225845  17.86235340081689671  11.49162654482293711   5.8101264682378408 12.75750751442655684
# 2016-10-31 -12.3467649887635815  -4.8402151737854195  -0.64562094891989563   7.95333890913508768  14.1247381985226195  2.94107946015766242
# 2016-11-30  87.6113938593773725  36.9149049238328786  25.50064217026994484  18.76072948617356673  25.6365221739902545  9.41805354289089891
# 2016-12-31  26.6785409468942873  57.6792731404528567  33.67833518270452942  26.14602876931272490  19.3961597650210997 13.31883665016064278
# 
# # NOTE ( COMPARE TO : https://fredblog.stlouisfed.org/2016/07/the-equity-premium/ )
# > ret["2008"]
#             equity_prem_p01m_ann  equity_prem_p02m_ann  equity_prem_p03m_ann equity_prem_p06m_ann equity_prem_p09m_ann equity_prem_p12m_ann
# 2008-01-31 -100.4084757860892410  -48.6917771027152924  -54.6455473379572823 -18.1290358804996004 -11.9348124531106556  -6.5234632388542320
# 2008-02-29  -29.4604818257940551  -63.7613354597350650  -41.4488737069382793 -23.5321257863188578 -21.0263145424547808  -6.1178404308165248
# 2008-03-31   -5.3763956078764616  -17.3016022132415763  -44.0371738788008074 -30.1890311778175118 -20.7166451008663444  -8.1610724363520895
# 2008-04-30   60.6034518011223398   27.4062629061172025    7.9312314352738440 -23.5444044537395101  -9.4549529582493079  -7.0068930832134777
# 2008-05-31   43.9766081871344028   52.9049227779607349   33.2638018198465346  -5.2700577607560612  -5.2620972313619889  -8.1502654498142277
# 2008-06-30  -90.7206833410654525  -24.4444444444444891    2.6613781552689755 -20.3256155892188062 -18.9023128301533347 -14.6012865540315815
# 2008-07-31  -10.3460860362331726  -50.1171051856782412  -19.5321637426900274  -5.9928972375873855 -21.2342760855632910 -11.6547029001954190
# 2008-08-31   14.5530118965071686    2.0239350227442059  -29.1061139121883166   1.1064432331824068 -12.8556773573571093 -11.0114988118394130
# 2008-09-30  -85.7510701120621377  -36.4201866686210209  -27.4593912854091933 -12.2094792246333093 -21.4593764465962415 -19.8388758012597606
# 2008-10-31 -178.8215938497030209 -122.8364557314771304  -78.5410258192981416 -45.8479532163742647 -28.4299931934493983 -31.6890774959226533
# 2008-11-30 -127.2174882365168003 -144.1639174259655363 -115.7239776818370842 -67.7406820831996725 -37.2502831568011175 -35.2114417389134857
# 2008-12-31  -49.1979829006260303  -89.9532729102799635 -114.1582891367277171 -66.6426789205679881 -42.8168226380555765 -40.2288045640858485

# NOTE: really(from 'stock for the long run': the 10 month return may MATTER most)



get_fred_good_corp_bond_yearly_yield_eom_xts <- function() {

  # BofA Merrill Lynch US Corporate BBB Effective Yield
  # https://fred.stlouisfed.org/series/BAMLC0A4CBBBEY
  # ORIG FROM ( INSPIRED BY )
  # The equity premium
  # https://fredblog.stlouisfed.org/2016/07/the-equity-premium/
  
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
  
  require(quantmod)

  message("Begin function get_fred_good_corp_bond_yearly_yield_eom_xts.")

  BAMLC0A4CBBBEY <- getSymbols("BAMLC0A4CBBBEY", src = "FRED", from = "1950-01-01", auto.assign = FALSE) # SINCE DEC 1996
  temp <- BAMLC0A4CBBBEY
  colnames(temp)[1] <- tolower(colnames(temp)[1])
  temp <- to.monthly(temp, OHLC = FALSE, indexAt = "lastof") 
  fred_good_corp_bond_yearly_yield_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_fred_good_corp_bond_yearly_yield_eom_xts.")

  return(fred_good_corp_bond_yearly_yield_eom_xts)

}
# ret <- get_fred_good_corp_bond_yearly_yield_eom_xts()





# competiton from bonds ( any reason )
get_fred_zimmermann_equity_premium_eom_xts <- function() {

  # ORIG FROM ( INSPIRED BY )
  # The equity premium
  # https://fredblog.stlouisfed.org/2016/07/the-equity-premium/
  
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
  
  require(xts) # lag.xts
  # uses fred_wilshire5000_eom_xts
  # uses get_fred_good_corp_bond_yearly_yield_eom_xts

  message("Begin function get_fred_zimmermann_equity_premium_eom_xts.")

  fred_wilshire5000_eom_xts                <- get_fred_wilshire5000_eom_xts()
  fred_good_corp_bond_yearly_yield_eom_xts <- get_fred_good_corp_bond_yearly_yield_eom_xts()
 
  tempw12_ann <- ( fred_wilshire5000_eom_xts - lag.xts(fred_wilshire5000_eom_xts,12) ) / abs(lag.xts(fred_wilshire5000_eom_xts,12)) * 12.0/12.0 * 100.00
 
  temp <- tempw12_ann - fred_good_corp_bond_yearly_yield_eom_xts
  colnames(temp)[1] <- "zimmermann_equity_premium"
  fred_zimmermann_equity_premium_eom_xts <- temp
  rm(temp)
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_fred_zimmermann_equity_premium_eom_xts.")

  return(fred_zimmermann_equity_premium_eom_xts)

}
# ret <- get_fred_zimmermann_equity_premium_eom_xts()
# dygraphs::dygraph(ret)
# compare TO https://fredblog.stlouisfed.org/2016/07/the-equity-premium/




get_fred_chicago_fed_nat_fin_cond_idx_nonfin_lev_subidx_eom_xts <- function() {

  # The Chicago Fed’s National Financial Conditions Index (NFCI) provides a 
  # comprehensive weekly update on U.S. financial conditions in 
  #   money markets, debt and equity markets, and the traditional and “shadow” banking systems.
  # 
  # THIS IS IT: ACTUALLY SLOPE AND TREND/DIP MATTER
  # "Positive values of the NFCI indicate financial conditions that are tighter than average, 
  # while negative values indicate financial conditions that are looser than average."
  # The three subindexes of the NFCI (risk, credit and leverage)
  # https://fred.stlouisfed.org/series/NFCINONFINLEVERAGE
  # 
  # NFCI is a weighted average of a large number of variables (105 measures of financial activity)
  # http://www.chicagofed.org/webpages/publications/nfci/index.cfm
  
  # (USEFUL IN ALL RECESSIONS: SLOPE IS MOST USEFUL)
  # Chicago Fed National Financial Conditions Credit Subindex (NFCICREDIT)
  # "Positive values of the NFCI indicate financial conditions that are tighter than average, 
  # while negative values indicate financial conditions that are looser than average."
  # https://fred.stlouisfed.org/series/NFCICREDIT
  
  # (EXCEPT FOR 2001 RECESSION: VERY GOOD)
  # Chicago Fed National Financial Conditions Leverage Subindex (NFCILEVERAGE)
  # "Positive values of the NFCI indicate financial conditions that are tighter than average, 
  # while negative values indicate financial conditions that are looser than average."
  # https://fred.stlouisfed.org/series/NFCILEVERAGE
  
  # (ONLY USEFUL IN 2008 RECESSION)
  # Chicago Fed National Financial Conditions Risk Subindex (NFCIRISK)
  # Positive values of the NFCI indicate financial conditions that are tighter than average, 
  # while negative values indicate financial conditions that are looser than average."
  # https://fred.stlouisfed.org/series/NFCIRISK
  
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
  
  require(quantmod)

  message("Begin function get_fred_chicago_fed_nat_fin_cond_idx_nonfin_lev_subidx_eom_xts.")

  NFCINONFINLEVERAGE <- getSymbols("NFCINONFINLEVERAGE", src = "FRED", from = "1950-01-01", auto.assign = FALSE) # SINCE JAN 1971
  temp <- NFCINONFINLEVERAGE
  colnames(temp)[1] <- tolower(colnames(temp)[1])
  temp <- to.monthly(temp, OHLC = FALSE, indexAt = "lastof") 
  fred_chicago_fed_nat_fin_cond_idx_nonfin_lev_subidx_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_fred_chicago_fed_nat_fin_cond_idx_nonfin_lev_subidx_eom_xts.")

  return(fred_chicago_fed_nat_fin_cond_idx_nonfin_lev_subidx_eom_xts)

}
# ret <- get_fred_chicago_fed_nat_fin_cond_idx_nonfin_lev_subidx_eom_xts()
# dygraphs::dygraph(ret)




get_fred_civil_unemp_rate_eom_xts <- function() {

  # number of unemployed as a percentage of the labor force
  # Seasonally Adjusted
  # https://fred.stlouisfed.org/series/UNRATE

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
  
  require(quantmod)

  message("Begin function get_fred_civil_unemp_rate_eom_xts.")

  UNRATE <- getSymbols("UNRATE", src = "FRED", from = "1940-01-01", auto.assign = FALSE) # SINCE JAN 1948
  index(UNRATE) <- index(UNRATE) - 5L
  temp <- UNRATE
  colnames(temp)[1] <- tolower(colnames(temp)[1])
  temp <- to.monthly(temp, OHLC = FALSE, indexAt = "lastof") 
  fred_civil_unemp_rate_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_fred_civil_unemp_rate_eom_xts.")

  return(fred_civil_unemp_rate_eom_xts)

}
# ret <- get_fred_civil_unemp_rate_eom_xts()
# dygraphs::dygraph(ret)



# see the PRESSURE the FED is applying
get_frbdata_federal_funds_eff_rate_eom_xts <- function() {

  # FF:Federal funds effective rate
  # ? FRBData::GetInterestRates

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
  # uses FRBData function GetInterestRates

  message("Begin function get_frbdata_federal_funds_eff_rate_eom_xts.")

  FF <-FRBData::GetInterestRates("FF") # SINCE JUL 1954
  temp <- FF
  colnames(temp)[1] <- "ff"
  temp <- to.monthly(temp, OHLC = FALSE, indexAt = "lastof") 
  frbdata_federal_funds_eff_rate_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_frbdata_federal_funds_eff_rate_eom_xts.")

  return(frbdata_federal_funds_eff_rate_eom_xts)

}
# see the PRESSURE the FED is applying
# ret <- get_frbdata_federal_funds_eff_rate_eom_xts()
# dygraphs::dygraph(ret)



# see the PRESSURE the FED is applying
get_frbdata_discount_window_primary_credit_rate_eom_xts <- function() {

  # DWPC:Discount window primary credit.The rate charged for primary credit under amendment to the Board's Regulation A
  # ? FRBData::GetInterestRates

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
  # uses FRBData function GetInterestRates

  message("Begin function get_frbdata_discount_window_primary_credit_rate_eom_xts.")

  DWPC <-FRBData::GetInterestRates("DWPC") # SINCE JAN 2003
  temp <- DWPC
  colnames(temp)[1] <- "dwpc"
  temp <- to.monthly(temp, OHLC = FALSE, indexAt = "lastof") 
  frbdata_discount_window_primary_credit_rate_eom_xts <- temp
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_frbdata_discount_window_primary_credit_rate_eom_xts.")

  return(frbdata_discount_window_primary_credit_rate_eom_xts)

}
# see the PRESSURE the FED is applying
# ret <- get_frbdata_discount_window_primary_credit_rate_eom_xts()
# dygraphs::dygraph(ret)



# related to 'equity premium'
get_quandl_sp500_pe_ratio_month_4q_eom_xts <- function() {
  
  # S&P 500 PE Ratio by Month
  # https://www.quandl.com/data/MULTPL/SP500_PE_RATIO_MONTH-S-P-500-PE-Ratio-by-Month

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
  
  message("Begin function get_quandl_sp500_pe_ratio_month_4q_eom_xts.")
  
  require(Quandl)
  # Quandl.api_key(api_key= "YOURKEYHERE")
  temp <- Quandl("MULTPL/SP500_PE_RATIO_MONTH", type = "xts")   # 
  index(temp) <- zoo::as.Date(index(temp), frac= 1) # checked date 'are end month anyways'
  colnames(temp)[1] <- "sp500_pe_ratio_month_4q"
  quandl_sp500_pe_ratio_month_4q_eom_xts <- temp
  rm(temp)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_quandl_sp500_pe_ratio_month_4q_eom_xts.")
  
  return(quandl_sp500_pe_ratio_month_4q_eom_xts)
  
}
# related to equity premium
# SEE THE PRESSURE/RELIEF: easingr
# Cleveland Fed 
#
# see the preductions
# Phildelphia Fed



get_aaii_sentiment_survey_eom_xts <- function() {

  # http://www.aaii.com/sentimentsurvey
  # download file
  # works in FF
  # http://www.aaii.com/files/surveys/sentiment.xls

  ops <- options()
  
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  options(warn=1)
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  message("Begin function get_aaii_sentiment_survey_eom_xts.")
  
  require(xts)
  # uses TTR function  SMA
  # uses plyr function ddply
  
  tmpf <- tempfile(fileext = ".xls")
  # weekly data ( every Friday ) since JAN 03 2017
  download.file(url = "http://www.aaii.com/files/surveys/sentiment.xls"
      , destfile = tmpf
      , mode = "wb"
  )
  spreadsheet <- suppressWarnings(readxl::read_excel(tmpf, sheet = "SENTIMENT", skip = 5, col_names = FALSE))
  # Reported Date: POSIXct
  # % - automatically removed
  
  # end
  # Observations over life of survey summary statistics 
  #  read_excel converted to 'NA' ( not useful(all statistics): will be removed )		
  spreadsheet <- spreadsheet[!is.na(spreadsheet[[1]]), , drop = FALSE]
  
  
  spreadsheet <- as.data.frame(spreadsheet, stringsAsFactors = FALSE)
  
  # names
  spreadsheet_col_names <- readxl::read_excel(tmpf, sheet = "SENTIMENT", skip = 1, n_max = 3, col_names = FALSE) 
  colnames(spreadsheet) <- plyr::laply( spreadsheet_col_names, .fun = function(x) {
  
    # aaii specific 
    temp <- gsub("- ","less ", x)
    temp <- gsub("\\+S","add S", temp)
    
    temp <- gsub("( |&|\\+|-|[.])","_", temp)
    # gsub would have replaced empty row with NA
    temp <- temp[!is.na(temp)] 
    temp <- paste0(temp, collapse = "_")
    temp <- tolower(temp)
    return(temp)
  
  })
  rm(spreadsheet_col_names)
  
  # future xts index
  dates <- zoo::as.Date(spreadsheet[[1]])
  spreadsheet <- spreadsheet[,-1, drop = FALSE]
  
  # convert
  row.names(spreadsheet) <- as.character(dates)
  spreadsheet <- as.xts(as.matrix(spreadsheet))
  index(spreadsheet) <- zoo::as.Date(index(spreadsheet))

  # just month end data ( last weekly of the month matters )
  # smooth all columns
  
  temp <- as.zoo(spreadsheet)
  # MORE user friendly than PACKAGE TTR function SMA
  # note: TTR::SMA/runMean/runSum
  #   runSum only supports univariate
  #   Series contains non-leading NAs
  #   not enough non-NA values
  # mean(x, na.rm = FALSE) # default # required # if NA in data # then return NA
  #                                  # required(partial = TRUE) # to prevent the 'length'(x) from shortening 
  spreadsheet <- rollapply(temp, width = 4, partial = TRUE, align = "right", FUN = function(x, n) { 
    # if not enough data 'length'
    if(n <= length(x)) { mean(x, na.rm = FALSE) } else { NA_real_ }
  }, n = 4)
  spreadsheet <- as.xts(spreadsheet, dates)
  rm(temp)
  # Warning in to.period(x, "months", indexAt = indexAt, name = name, ...) :
  # missing values removed from data
  spreadsheet <- suppressWarnings(to.monthly(spreadsheet, OHLC = FALSE, indexAt = "lastof"))
    
  
  aaii_sentiment_survey_eom_xts <- spreadsheet
                          
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_aaii_sentiment_survey_eom_xts.")
  
  return(aaii_sentiment_survey_eom_xts)

}
# even AFTER SMOOTHING: gives an IDEA how volitile and clueless the individual is.
# ignorance(suprise) of the general population
# ret <- get_aaii_sentiment_survey_eom_xts()
# dygraphs::dygraph(ret[,"bull_bear_spread"])
# generally ACCOMPLISHES THE SAME THING: zimmerman e.q. 'WILL5000INDFC - AGG'
# SO just use ZIMMERMAN





# SEE THE PRESSURE/RELIEF
get_clev_easing_balances_eom_xts <- function() {
  
  # balance sheet with its components broadly divided into these categories
  # https://www.clevelandfed.org/our-research/indicators-and-data/credit-easing.aspx
  # INSPIRED FROM
  # https://github.com/cran/easingr
  
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
  
  message("Begin function get_clev_easing_balances_eom_xts.")
  
  require(xts)
  
  tmpf <- tempfile(fileext = ".xls")
  # weekly data ( every Friday ) since JAN 03 2017
  download.file(url = "https://www.clevelandfed.org/~/media/files/charting/crediteasingbalancesheet.xls"
      , destfile = tmpf
      , mode = "wb"
  )
  spreadsheet <- readxl::read_excel(tmpf, sheet = 1L, skip = 1) 
  spreadsheet <- as.data.frame(spreadsheet, stringsAsFactors = FALSE)
  
  # names
  temp <- spreadsheet 
  temp <- temp[,!colnames(temp) %in% c("X__1","X__2","X__3","X__4")]
  colnames(temp) <- gsub("( |-|[.])","_",colnames(temp))
  colnames(temp) <- tolower(colnames(temp))
  
  # convert
  row.names(temp) <- as.character(zoo::as.Date(temp[[1]]))
  temp <- temp[,-1, drop = FALSE]
  temp2 <- as.xts(as.matrix(temp))
  index(temp2) <- zoo::as.Date(index(temp2))
  rm(temp)
  
  # just month end data ( last weekly of the month matters )
  temp2 <- to.monthly(temp2, OHLC = FALSE, indexAt = "lastof")
  spreadsheet <- temp2
  rm(temp2)
  
  clev_easing_balances_eom_xts <- spreadsheet[, "lending_to_financial_institutions"] +
                          spreadsheet[, "liquidity_to_key_credit_markets"] +
                          spreadsheet[, "traditional_security_holdings"] + 
                          spreadsheet[, "federal_agency_debt_and_mortgage_backed_securities_purchases"] +
                          spreadsheet[, "long_term_treasury_purchases"]
                          
  colnames(clev_easing_balances_eom_xts)[1] <- "clev_easing_balances"
  # flat since NOV 2014, but WITHOUT "clev_easing_balances" ACTUALLY INCREASING!+ since the BEGINNING of 2017          

  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_clev_easing_balances_eom_xts.")
  
  return(clev_easing_balances_eom_xts)
  
}
# PRESSURE or LACK_OF - Federal Gov credit easing (money printing)
# ret <- get_clev_easing_balances_eom_xts()
# dygraphs::dygraph(ret)


# NOTE: I HAVE NOT DONE/DO NOT KNOW HOW TO DO PR*obabilities99
# 
                                       # common place override ( by user )
get_phil_survey_of_prof_forecasters_eom_xts <- function(file_data_loc = NULL, surveys_of_interest_regex = "^(unemp__|cpi__).*(3|4|5|6)$", future_dates_regex = "(3|4|5|6)$") {
  
 # Individual Forecasts for the Survey of Professional Forecasters
 # https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data/individual-forecasts
  
 # doc - read the output
 # https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-documentation.pdf?la=en
  
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
  
  message("Begin function get_phil_survey_of_prof_forecasters_eom_xts.")
  
  require(xts)
  
  # uses readxl function read_excel
  # uses plyr function join_all
  # uses DataCombine function MoveFront
  # uses hydroTSM function smry
  # uses rlist list.zip

  # to save common place (see below)
  save_file_loc <- "phil_survey_of_prof_forecasters__all_files_in_one.RData"
  
  # if not a disk stored file ... then go get them
  if(is.null(file_data_loc)) {
  
    base_url <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data/"
    
    list_of_files <- list()
    for(file_name_i in c("micro1.xlsx", "micro2.xlsx", "micro3.xlsx", "micro4.xlsx", "micro5.xlsx")) {
      
      message(paste0("  Begin file ", file_name_i))
      
      tmpf <- tempfile(fileext = ".xlsx")
      url_file <-  paste0(base_url, file_name_i, "?la=en")
      
      # getting data
      message(paste0("  Starting Downloading url/file ", url_file))
      download.file(destfile = tmpf, url = url_file,  mode = "wb")
      message(paste0("  Finished Downloading url/file ", url_file))
      
      list_of_sheets <- list()
      for(excel_sheet_name_i in readxl::excel_sheets(tmpf)) {
      
        # unfortuately performs a disk i/o upon EACH sheet access
        # currently AUTHOR not motivated/not_worth_the_time, to find a 'faster (entire workbook at/a/time ) way
        message(paste0("    Begin sheet ", excel_sheet_name_i))
        
        spreadsheet_i <- suppressWarnings(readxl::read_excel(tmpf, sheet = excel_sheet_name_i, col_types = "numeric"))
        spreadsheet_i <- as.data.frame(spreadsheet_i, stringsAsFactors = FALSE)
  
        # names
        colnames(spreadsheet_i) <- tolower(colnames(spreadsheet_i))
        colnames(spreadsheet_i)[!colnames(spreadsheet_i) %in% c("year","quarter","id","industry")] <-
          paste0(tolower(excel_sheet_name_i), "__", colnames(spreadsheet_i)[!colnames(spreadsheet_i) %in% c("year","quarter","id","industry")])
        
        message(paste0("    End   sheet ", excel_sheet_name_i))
        
        list_of_sheets[[tolower(excel_sheet_name_i)]] <- spreadsheet_i
        
      }
      
      message(paste0("  End   file ", file_name_i))
      
      # sheet together
      all_sheets_in_one <- plyr::join_all(list_of_sheets, by =c("year","quarter","id","industry"), type = "full")
      list_of_files[[file_name_i]] <- all_sheets_in_one
      
    }
    # files ( of sheets ) together
    # "year","quarter", "id"(forecaster) are the 'unique record indicator'
    # "industry" is the description of the forcaster
    # by LUCK c("year","quarter","id","industry") STILL produce a 'unique record'
    #  so I will lazily simply add 'industry' to the join
    all_files_in_one <- plyr::join_all(list_of_files, by =c("year","quarter","id","industry"), type = "full")
    
    # common place
    message(paste0("  Begin saving file ", save_file_loc))
    save(all_files_in_one, file = save_file_loc, envir = environment())
    message(paste0("  End   saving file ", save_file_loc))
    
  } else { # !is.null(file_data_loc)
    
    # common place override
    if(!is.null(file_data_loc) || (file_data_loc == "DISK")) {
      if(file_data_loc == "DISK") {
        message(paste0("  Begin loading file ", save_file_loc))
        load(file = save_file_loc, envir = environment())
        message(paste0("  End    loading file ", save_file_loc))
      }else {
        # common place override
        load(file = file_data_loc, envir = environment())
      }
    }
     
  }
  
  # note: many faster and/or R-ish_/ply-ish better-ish ways do/may exist to do this.
  # but the method below is just simple, flexible, comprehendable ( and thus possibly better-ly extensible )
  
  # within the same  'survey_i'  I rbind.xts
  # amoung different 'survey_i'  I merge.xts
  
  list_of_xtss <- list()
  grand_list_of_xtss <- list()
  
  survey_i_rbinded_xtss <- xts()
  
                                # entered by user           # entered by user
  for(survey_name_i in sort(grep(surveys_of_interest_regex, colnames(all_files_in_one), value = TRUE, perl = TRUE))) {
    
    message(paste0("  Begin survey ", survey_name_i))
    
                                              # entered by user
    # character position in the survey_i string where the 'forecast time end characters are located.'
    forecast_end_represent_id_loc <- regexpr(future_dates_regex, survey_name_i, perl = TRUE)
    
    # number  "1" represents the "forecast" for the quarter prior
    # number  "2" represents the forecast for the current quarter
    # numbers "3" through "6" represent the forecasts for the  four quarters after the current quarter. 
    # letters "a" and "b" (and somtimes "c") represent annual average forecasts for 
    #   the current year (the year in which the survey is conducted) 
    #     and 
    #   the following year.
    #     (and
    #   the following year following year)
    # 
    # determinte the forcast_end_represent_id
    forecast_end_represent_id <- substr(survey_name_i, start = attr(forecast_end_represent_id_loc, "capture.start")[1], stop = attr(forecast_end_represent_id_loc, "capture.start")[1] + attr(forecast_end_represent_id_loc, "capture.length")[1])
    message(paste0("  forecast_end_represent_id ", forecast_end_represent_id))
    
    # just cols of interest
    survey <- all_files_in_one[ ,c("year", "quarter", "id", "industry", survey_name_i), drop = FALSE]
    
    # add future dates
    # currently not yet defined for c("a","b","c") # COME_BACK
    if(!any(forecast_end_represent_id %in% c("a","b","c"))) {
      forecast_end_represent_id     <- as.numeric(forecast_end_represent_id)
      # ( RETURN the data of the future prediction )
      #                              # now             # next_quarter (#3)
      # zoo::as.Date(zoo::as.yearmon(1968 + (1/4) + ( (3 - 2) * (1/4) )- 0.00001), frac = 1)
      dates <- as.character(zoo::as.Date(zoo::as.yearmon(survey[["year"]] + survey[["quarter"]]/4 + ( (forecast_end_represent_id - 2) * (1/4) )- 0.00001), frac = 1))
      message("  Message max(dates) ", paste0(max(dates)))
    } else {
      stop("future is not defined for 'a','b', or 'c'") 
    }
    survey <- cbind(date = dates, survey , stringsAsFactors = FALSE) 
    rm(dates)
    survey <- DataCombine::MoveFront(survey, "date")


    # column subset on the future date
    survey_summary_by_date_result <- plyr::dlply(survey, "date", function(x) { 
      
      # returns a df
      summary <- hydroTSM::smry(x[survey_name_i], na.rm = TRUE)
      # remove 0/0 math
      summary[is.nan(as.vector(unlist(summary))),] <- NA_real_
      
      # return a 'named vector
      temp <- summary[[survey_name_i]]
      names(temp) <- row.names(summary)
      # "Min."     "1st Qu."  "Median"   "Mean"     "3rd Qu."  "Max."     "IQR"      "sd"      
      # "cv" "Skewness" "Kurtosis" "NA's"     "n"
      
      # rename
      names(temp) <- tolower(names(temp))
      names(temp) <- gsub("([.]|'| )","_",names(temp))
      names(temp) <- gsub("(\\d)","d\\1", names(temp))
      
      x <- temp
      attr(x, "label") <- survey_name_i
      
      return(x)
      
    } )
    rm(survey)
    
    # instead of plyr::dlply returned 'list item names'
    #   attr(*, "split_type")
    #   attr(*, "split_labels")
    survey_summary_by_date_result <- 
      rlist::list.zip(result = survey_summary_by_date_result, result_date = names(survey_summary_by_date_result))
    for(survey_summary_by_date_result_i in survey_summary_by_date_result) {
      
      message(paste0("    Begin survey result ", survey_summary_by_date_result_i[["result_date"]]))
      # create each small xts
      temp <- t(as.matrix(survey_summary_by_date_result_i[["result"]]))
      colnames(temp) <- paste0(attr(survey_summary_by_date_result_i[["result"]],"label", exact = TRUE), "__", colnames(temp)) 
      rownames(temp) <- survey_summary_by_date_result_i[["result_date"]]
      # S3 dispatch as.xts.matrix
      temp <- as.xts(temp)
      # because a non-Date index had been created
      index(temp) <- zoo::as.Date(index(temp))
    
      # combine with the grand list
      list_of_xtss <- c(list(temp), list_of_xtss)
      
      message(paste0("    End   survey result ", survey_summary_by_date_result_i[["result_date"]]))
      
    }
    
    # per survey_i
    survey_i_rbinded_xtss <- do.call(rbind.xts, list_of_xtss)
    
    grand_list_of_xtss <- c(list(survey_i_rbinded_xtss), grand_list_of_xtss)
    # ready for next loop
    list_of_xtss <- list()
    
    message(paste0("  End   survey ", survey_name_i))
    
  }
  
  # bring all xtss together
  many_xtss_in_one <- do.call(merge.xts, grand_list_of_xtss)
  phil_survey_of_prof_forecasters_eom_xts <- many_xtss_in_one
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_phil_survey_of_prof_forecasters_eom_xts.")
  
  return(phil_survey_of_prof_forecasters_eom_xts)

}
# ONLY do 'every so often'. Perhaps, once/month or once/quarter?
# From the internet gets the the data, then saves it to the current working directory
# ret <- get_phil_survey_of_prof_forecasters_eom_xts()
# 
# From the local directory, get the saved data
# ret <- get_phil_survey_of_prof_forecasters_eom_xts(file_data_loc = "DISK")
# 
# From a user custom location, get the data
# ret <- get_phil_survey_of_prof_forecasters_eom_xts(file_data_loc = "phil_survey_of_prof_forecasters__all_files_in_one.RData")
#
# just two future quarters of unemployment
# ret <- get_phil_survey_of_prof_forecasters_eom_xts(file_data_loc = "DISK", surveys_of_interest_regex = "^(unemp__).*(3|4)$", future_dates_regex = "(3|4)$")
#
# # GOOD EXAMPLE ( notece: EXACTLY when BEFORE/AFTER going in/out OF MAJOR recessions )
# ret <- get_phil_survey_of_prof_forecasters_eom_xts(file_data_loc = "DISK", surveys_of_interest_regex = "^(unemp__).*(3|4)$", future_dates_regex = "(3|4)$")
# quantmod::getSymbols("UNRATE", src = "FRED", from = "1940-01-01")
# # to make eom
# zoo::index(UNRATE) <- zoo::index(UNRATE) - 1
# dygraphs::dygraph(merge.xts(UNRATE,ret[,"unemp__unemp3__median"]))
# 

# dygraphs::dygraph(ret)


# goodsight01.R

