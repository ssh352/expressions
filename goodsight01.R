

# goodsight01.R

# last observation carried forard limited

#' Title
#'
#' @return
#' @export
#'
#' @examples
na.locfl <- function(x, n = NULL) {

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
  
  require(xts) # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  # uses zoo:::rollapply.zoo, DescTools::DoCall

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
      stop("na.locfl: can not make a zoo object")
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

  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(x_result)
  
  # ORIG FROM
  # https://github.com/AndreMikulec/expressions/blob/8a910454ea590a30878e97b18d5e9dbe45a9d4fb/main-foresight3-999.R#L2287

}

# # na.locfl( c(101,NA,NA,NA,102,NA,NA), n = 2)
# [1] 101 101 101  NA 102 102 102

# # input
# # xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7)))
#            [,1]
# 1970-01-11  101
# 1970-01-21   NA
# 1970-01-31   NA
# 1970-02-10   NA
# 1970-02-20  102
# 1970-03-02   NA
# 1970-03-12   NA

# # na.locfl( xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7))), 2 )
#            [,1]
# 1970-01-11  101
# 1970-01-21  101
# 1970-01-31  101
# 1970-02-10   NA
# 1970-02-20  102
# 1970-03-02  102




# SHOULD 'RENAME TO 'Uses'
#' Title
#'
#' @return
#' @export
#'
#' @examples
check_uses_packages_available <- function(programmed_in_R_version, explicit_package_function_calls, matched_call = NULL) {

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

# pecent change from the past through NOW 
# ( if to_future == TRUE, then from NOW to the FUTURE )
#' Title
#'
#' @return
#' @export
#'
#' @examples
PCTCHG.xts <- function(x, whiches, to_future = NULL) { 
  
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
  
  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  ## VERY BASIC attemped CLASS conversion ##
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("PCTCHG.xts: can not make an xts object") } else { x_try.xts_success <- TRUE; x_try.xts }

   # normal backwards
  if(is.null(to_future) || to_future == FALSE) { to_future = FALSE;   lag_direction <- 1 }

  if(to_future == TRUE) lag_direction <- -1
  
  # xts::lag.xts rules
  if( whiches <= NROW(x) ) {
    # from past
    if(!to_future) x_result <- ( x - xts::lag.xts(x, whiches * lag_direction) )/ abs(xts::lag.xts(x, whiches * lag_direction)) * 100 * lag_direction
    # to future
    if( to_future) x_result <- ( xts::lag.xts(x, whiches * lag_direction) - x )/ abs(                                       x) * 100 * lag_direction
  } else {
    x[,] <- NA_real_
    x_result <- x
  }

  # would/should always be/been true else I may/have/never ever made it his far
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(x_result)
} 

# > library(xts)
# > data(sample_matrix)
# > sample_xts <- as.xts(sample_matrix)
# > head(sample_xts[,"Open"],4) # x
#                          Open
# 2007-01-02 50.039781911546299
# 2007-01-03 50.230496197795397
# 2007-01-04 50.420955209067003
# 2007-01-05 50.373468054328498
# >
# > PCTCHG.xts(head(sample_xts[,"Open"],4), whiches = 1,                 )
#                             Open
# 2007-01-02                    NA
# 2007-01-03  0.381125334611203126 # res
# 2007-01-04  0.379170077320410137
# 2007-01-05 -0.094181386571521211
# >
# > ( 50.230496197795397 - 50.039781911546299 ) / abs(50.039781911546299) * 100
# [1] 0.38112533461120313
# >
# > PCTCHG.xts(head(sample_xts[,"Open"],4), whiches = 1, to_future = TRUE )
#                             Open
# 2007-01-02 -0.381125334611203126
# 2007-01-03 -0.379170077320410137 # res
# 2007-01-04  0.094181386571521211
# 2007-01-05                    NA
# >
# > ( 50.420955209067003 -50.230496197795397) / abs( 50.230496197795397 ) * 100
# [1] 0.37917007732041014
# 
# # NOT(whiches <= NROW(x))
# > PCTCHG.xts(head(sample_xts[,"Open"],4), whiches = 5, to_future = TRUE )
# >
#            Open
# 2007-01-02   NA
# 2007-01-03   NA
# 2007-01-04   NA
# 2007-01-05   NA



#' Title
#'
#' @return
#' @export
#'
#' @examples
collofdays2daily.xts <- function(x) {
  
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

  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("collofdays2daily.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }
  # EXPECTED TO CHANGE THE NUMBER OF ROWS  SO CAN NOT DO 'reclass'
  
  # note: index(first/last(x, '1 day')) expected to return ONE single element
  # reduce a a 'set of first/last '1 day' to ret ONE single element 
  # also will covert a non-Date index to a Date index
  # 
  #  zoo::as.Date garantees that a non-Date will become a Date
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
  ret <- merge(x_days, x)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(ret)
  
}
# x <- xts(c(11,13,15),zoo::as.Date(c(1,3,5))) 
# x
#            [,1]
# 1970-01-02   11
# 1970-01-04   13
# 1970-01-06   15
# xc <- collofdays2daily.xts(x)
# xc
#              x
# 1970-01-02 11
# 1970-01-03 NA
# 1970-01-04 13
# 1970-01-05 NA
# 1970-01-06 15

# # as.POSIXct(c(1,10000,200000,400000), origin = "1970-01-01")
# # [1] "1970-01-01 00:00:01 UTC" "1970-01-01 02:46:40 UTC" "1970-01-03 07:33:20 UTC" "1970-01-05 15:06:40 UTC"
# xp <- xts(11:14, as.POSIXct(c(1,10000,200000,400000), origin = "1970-01-01"))
# xpc <- collofdays2daily.xts(xp)
# xpc
#             x
# 1970-01-01 12
# 1970-01-02 NA
# 1970-01-03 13
# 1970-01-04 NA
# 1970-01-05 14



# # time since 'end of data'
# TMsinceEOD
# 
# # time since report
# TMsinceREP
# 
# # irregular: time(in future) window(horizon) to prediction
# TMinfutTOPRED
#' Title
#'
#' @return
#' @export
#'
#' @examples
delay_since_last_obs <- function(x) UseMethod("delay_since_last_obs")


delay_since_last_obs.default <- function(x) {

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
 
# delay_since_last_obs.default(c(101,NA,NA,NA,102,NA,NA))
# [1] 0 1 2 3 0 1 2




#' Title
#'
#' @return
#' @export
#'
#' @examples
delay_since_last_obs.xts <-function(x) { 

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
  
  # ONLY works on a single column xts
  # uses   delay_since_last_obs.default
  
  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("delay_since_last_obs.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  x_core  <- as.vector(coredata(x))
  x_index <- index(x)
  
  x_core_new <- delay_since_last_obs.default(x_core)
  
  x_result <- xts(x_core_new,x_index)
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(x_result)

} 

# # payload NA-gaps matter ( NOT index time gaps ) 
# xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10)))
#            [,1]
# 1970-01-11  101
# 1970-01-21   NA
# 1970-01-31   NA
# 1970-02-10   NA
# 1970-02-20  102
# 1970-03-02   NA
# 1970-03-12   NA
# 
# delay_since_last_obs.xts(xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
#            [,1]
# 1970-01-11    0
# 1970-01-21    1
# 1970-01-31    2
# 1970-02-10    3
# 1970-02-20    0
# 1970-03-02    1
# 1970-03-12    2
# 
# # S3 dispatch TEST
# delay_since_last_obs(xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
#            [,1]
# 1970-01-11    0
# 1970-01-21    1
# 1970-01-31    2
# 1970-02-10    3
# 1970-02-20    0
# 1970-03-02    1
# 1970-03-12    2



# ADD A a record for each day is this what I want?
#' Title
#'
#' @return
#' @export
#'
#' @examples
delay_since_last_day.xts <-function(x) { 

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
  
  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("delay_since_last_day.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # ONLY works on a single column xts

  # uses   delay_since_last_obs.default
  # uses xts:::merge.xts
  
  # more dates - create temporary rows
  x_nonsparse <- collofdays2daily.xts(x)
  
  # find delays(0 - no delay over NA, 1 - one delay 'at' NA)
  x_nonsparse_delays <- delay_since_last_obs.xts(x_nonsparse)
  
  x_result <- x_nonsparse_delays
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(x_result)

} 

# > delay_since_last_day.xts(xts::xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10,70,10))))
#            [,1]
# 1970-01-11    0
# 1970-01-12    1
# 1970-01-13    2
# 1970-01-14    3
# 1970-01-15    4
# 1970-01-16    5
# 1970-01-17    6
# 1970-01-18    7
# 1970-01-19    8
# 1970-01-20    9
# 1970-01-21   10
# 1970-01-22   11
# 1970-01-23   12
# 1970-01-24   13
# 1970-01-25   14
# 1970-01-26   15
# 1970-01-27   16
# 1970-01-28   17
# 1970-01-29   18
# 1970-01-30   19
# 1970-01-31   20
# 1970-02-01   21
# 1970-02-02   22
# 1970-02-03   23
# 1970-02-04   24
# 1970-02-05   25
# 1970-02-06   26
# 1970-02-07   27
# 1970-02-08   28
# 1970-02-09   29
# 1970-02-10   30
# 1970-02-11   31
# 1970-02-12   32
# 1970-02-13   33
# 1970-02-14   34
# 1970-02-15   35
# 1970-02-16   36
# 1970-02-17   37
# 1970-02-18   38
# 1970-02-19   39
# 1970-02-20    0
# 1970-02-21    1
# 1970-02-22    2
# 1970-02-23    3
# 1970-02-24    4
# 1970-02-25    5
# 1970-02-26    6
# 1970-02-27    7
# 1970-02-28    8
# 1970-03-01    9
# 1970-03-02   10
# 1970-03-03   11
# 1970-03-04   12
# 1970-03-05   13
# 1970-03-06   14
# 1970-03-07   15
# 1970-03-08   16
# 1970-03-09   17
# 1970-03-10   18
# 1970-03-11   19
# 1970-03-12   20


# renamed is.na.xt -> is.nasig.xts 
# to PREVENT ACCIDENTAL REPLACE dispatch the REAL is.na.xts
  # is the xts observation na? 1 - true  2 - false(regular observation)
#' Title
#'
#' @return
#' @export
#'
#' @examples
is.na_fctr.xts <- function(x) {
  
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
  
  # uses ojUtils::ifelseC
  
  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
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
  
  Sys.setenv(TZ=oldtz)
  options(ops)
   
  return(x_result)

} 
# xts(c(11,NA,NA,14,NA),zoo::as.Date(1:5))
#            [,1]
# 1970-01-02   11
# 1970-01-03   NA
# 1970-01-04   NA
# 1970-01-05   14
# 1970-01-06   NA
# 
# is.na_fctr.xts(xts(c(11,NA,NA,14,NA),zoo::as.Date(1:5)))
#            [,1]
# 1970-01-02    2
# 1970-01-03    1
# 1970-01-04    1
# 1970-01-05    2
# 1970-01-06    1



# typical entry rm_what = c("Saturday", "Sunday", "BIZHOLIDAYS" )
#' Title
#'
#' @return
#' @export
#'
#' @examples
rm.days.xts <- function(x, rm_what = NULL) {

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
  
  # JUN 2017
  # .indexwday
  # http://joshuaulrich.github.io/xts/xts_faq.html

  # uses stringr::stringr::str_detect, RQuantLib::isHoliday

  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("rm.days.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  if( !is.null(rm_what)     && 
     ( length(rm_what) > 0) && 
      any(stringr::str_detect(rm_what,"Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|HOLIDAYS$"))
    ) { 

    # 0 - Sunday ... 6 - Saturday  
    
    # Browse[2]> .indexwday(xts(,zoo::as.Date("2017-06-18"))) # Sunday
    # [1] 0
    # 
    # > weekdays(index(xts(,zoo::as.Date("2017-06-18")))) # format(zoo::as.Date("2017-06-18"), "%A")
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
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(x_result)
}
# weekends removed
# 
# RQuantLib::isHoliday("UnitedStates/NYSE" considers 'weekends' to be holidays
  # Holiday 2nd removed
  # Holiday 16th removed

# > rm.days.xts(xts(1:31,zoo::as.Date("2017-01-01") + 0:30), rm_what = c("Saturday", "Sunday", "BIZHOLIDAYS"))
#            [,1]
# 2017-01-03    3
# 2017-01-04    4
# 2017-01-05    5
# 2017-01-06    6
# 2017-01-09    9
# 2017-01-10   10
# 2017-01-11   11
# 2017-01-12   12
# 2017-01-13   13
# 2017-01-17   17
# 2017-01-18   18
# 2017-01-19   19
# 2017-01-20   20
# 2017-01-23   23
# 2017-01-24   24
# 2017-01-25   25
# 2017-01-26   26
# 2017-01-27   27
# 2017-01-30   30
# 2017-01-31   31

# weekends removed
# BIZHOLIDAYS removed 2017-01-02   2017-01-16   2017-02-20

# Browse[2]> data.frame( index(x), !RQuantLib::isHoliday("UnitedStates/NYSE", index(x)) )
#      index.x. X.RQuantLib..isHoliday..UnitedStates.NYSE...index.x..
# 1  2017-01-02                                                 FALSE
# 2  2017-01-03                                                  TRUE
# 3  2017-01-04                                                  TRUE
# 4  2017-01-05                                                  TRUE
# 5  2017-01-06                                                  TRUE
# 6  2017-01-09                                                  TRUE
# 7  2017-01-10                                                  TRUE
# 8  2017-01-11                                                  TRUE
# 9  2017-01-12                                                  TRUE
# 10 2017-01-13                                                  TRUE
# 11 2017-01-16                                                 FALSE
# 12 2017-01-17                                                  TRUE
# # ... etc ...

# utility function ( input to OTHERS )
# 
# xts object: x, 
# d numeric vector of past days: -1 yesterday, c(-1,-2) yesterday AND the 'day before yesterday' etc ( both must be true )
# 
# e.g. if TRUE and d = c(-1,-2), then 
#      BOTH yesterday and the 'day before yesterday' NON-working days of the U.S. Federal Government.
#

#' Title
#'
#' @return
#' @export
#'
#' @examples
all.nearby.FRED.holidays.xts <- function(x = NULL, d = NULL) {

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
  
  # "UnitedStates/GovernmentBond"
  # 2007 Federal Holidays
  # Monday, January 1   New Year's Day
  # Monday, January 15  Birthday of Martin Luther King, Jr.
  # https://archive.opm.gov/Operating_Status_Schedules/fedhol/2007.asp
  
  # uses  RQuantLib::isHoliday, rlist::list.zip, lubridate::`%m+%`

  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  `%M+%` <- lubridate::`%m+%`
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("all.nearby.FRED.holidays.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # for ONE single day returns multiple days
  FUN  <- function(x,d) { index(x) %M+% days(c(d)) }
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
  
  return(x_result)
  
}
# library(xts)
# data("sample_matrix")
# sample_xts <- as.xts(sample_matrix)
#
# ojUtils::ifelseC(all.nearby.FRED.holidays.xts (sample_xts,c(-1,-2)) , rep(TRUE,length(index(sample_xts))), rep(FALSE,length(index(sample_xts))))
# WORKS
# [169] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE

# 2017

# xe <- xts(1:31,zoo::as.Date("2017-01-01") + 0:30)

# xe
#            [,1]
# 2017-01-01    1
# 2017-01-02    2
# 2017-01-03    3
# 2017-01-04    4
# 2017-01-05    5
# 2017-01-06    6
# 2017-01-07    7
# 2017-01-08    8
# 2017-01-09    9
# 2017-01-10   10
# 2017-01-11   11
# 2017-01-12   12
# 2017-01-13   13
# 2017-01-14   14
# 2017-01-15   15
# 2017-01-16   16
# 2017-01-17   17
# 2017-01-18   18
# 2017-01-19   19
# 2017-01-20   20
# 2017-01-21   21
# 2017-01-22   22
# 2017-01-23   23
# 2017-01-24   24
# 2017-01-25   25
# 2017-01-26   26
# 2017-01-27   27
# 2017-01-28   28
# 2017-01-29   29
# 2017-01-30   30
# 2017-01-31   31

# > ojUtils::ifelseC(all.nearby.FRED.holidays.xts(xe,c(-1,-2)), rep(TRUE,length(index(xe))), rep(FALSE,length(index(xe))))
# [1] FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE
#[22] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE






# SWAP OUT an XTS object index WITH my OWN custom INDEX
# 
# x 
#   xts object
# x_index_new
#  anyting one dimensional with a length/NROW(x_index_new) == NROW(x) == length(index(x))
#' Title
#'
#' @return
#' @export
#'
#' @examples
reindex.xts <- function(x,  x_index_new ) {

  require(xts)

  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("reindex.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

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
  
  Sys.setenv(TZ=oldtz)
  
  return(x_result)
}

# # library(quantmod)
# # getSymbols("GDP", src = "FRED") # ABOVE
# # head(GDP)
#              GDP
# 1947-01-01 243.1
# 1947-04-01 246.3
# 1947-07-01 250.1
# 1947-10-01 260.3
# 1948-01-01 266.2
# 1948-04-01 272.9
# 
# reindex.xts(head(GDP), 0:5)
#              GDP
# 1970-01-01 243.1 # new index starting with the 'birth' of UNIX 0 ... 5
# 1970-01-02 246.3
# 1970-01-03 250.1
# 1970-01-04 260.3
# 1970-01-05 266.2
# 1970-01-06 272.9



# meant REALY only for St.Louis FRED
# adjust dates that start on the 1st( sometimes 4th, 3rd, or 2nd) to be the 31st
# be aware of landings on weekend and long holiday weekends and after a Tuesday or Thursday holiday

# slow: 170 observations per second
# 

#' Title
#'
#' @return
#' @export
#'
#' @examples
pushback.FRED.1st.days.xts <- function(x) {

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
  
  # uses lubridate::`%m+%`
  # uses xts::apply.daily xts::index
  # uses all.nearby.FRED.holidays.xts  reindex.xts

  `%M+%` <- lubridate::`%m+%`
  
  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("pushback.FRED.1st.days.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # If Mutiple POSIXct per day then I just need ONE of them 
  x_s        <- xts::split.xts( x, f = "days" )
  x_s_l_last <- lapply( x_s , function(x) { xts::last(x) } ) # xts::last remaining of all the elements
  # unsplit
  x <- do.call(xts::rbind.xts, x_s_l_last )
  
  # if the 4th is Today and the last 3 days were holidays then shift the index 4 # then done  
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(day(index(xx)) == 4 && all.nearby.FRED.holidays.xts(xx, c(-1,-2,-3))) {
      index(xx) %M+% days(-4)
    } else {
      index(xx)
    }
  }) -> x_4th
  x <- reindex.xts(x, x_4th)

  # if the 3rd is Today and the last 2 days were holidays then shift the index 3 # then done  
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(day(index(xx)) == 3 && all.nearby.FRED.holidays.xts(xx, c(-1,-2))) {
      index(xx) %M+% days(-3)
    } else {
      index(xx)
    }
  }) -> x_3rd
  x <- reindex.xts(x, x_3rd)

  # Tuesday and Thursday(Thanksgiving) holidays
  # if the 2nd is Today and the last 1 day was a holiday then shift the index 2 # then done  
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(day(index(xx)) == 2 && all.nearby.FRED.holidays.xts(xx, c(-1))) {
      index(xx) %M+% days(-2)
    } else {
      index(xx)
    }
  }) -> x_2nd
  x <- reindex.xts(x, x_2nd)

  # if the 1st is Today then shift the index 1  # then done
  apply.daily(x, function(xx) { 
    # only one daily observation in this case so '&&' is O.K.
    if(day(index(xx)) ==  1) {
      index(xx) %M+% days(-1)
    } else {
      index(xx)
    }
  }) -> x_1st
  x <- reindex.xts(x, x_1st)
  x_result <- x

  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(x_result)

}

# # getSymbols("GDP", src = "FRED") # ABOVE
# # head(GDP)
#              GDP
# 1947-01-01 243.1
# 1947-04-01 246.3
# 1947-07-01 250.1
# 1947-10-01 260.3
# 1948-01-01 266.2
# 1948-04-01 272.9
# 
# # head(pushback.FRED.1st.days.xts(GDP))
# 
#              GDP
# 1946-12-31 243.1
# 1947-03-31 246.3
# 1947-06-30 250.1
# 1947-09-30 260.3
# 1947-12-31 266.2
# 1948-03-31 272.9


# meant to pass just the index
# 1 - yes # 2 - new

#' Title
#'
#' @return
#' @export
#'
#' @examples
year.less.then.or.equal.xts <- function(x, n = NULL ) {

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

    # uses lubridate::year
  # uses ojUtils::ifelseC

  require(xts) # # Attaching package: 'zoo'
  # IF NOT Error in try.xts(element1) : could not find function "try.xts"
  
  x_orig <- x
  c_orig <- class(x)[1] # original class
  
  ## VERY BASIC attemped CLASS conversion ##
  x_try.xts_success <- FALSE
  x_try.xts <- try(xts::try.xts(x_orig), silent = T)
  #
  x         <- if(any(class(x_try.xts) %in% "try-error")) { stop("delay_since_last_obs.xts could not make an xts") } else { x_try.xts_success <- TRUE; x_try.xts }

  # only the index is important
  
  x_index_len <- length(index(x))

  coredata_new <- ojUtils::ifelseC(lubridate::year(index(x)) <= n, rep(1,x_index_len), rep(2,x_index_len))
  
  # coredata(x)  <- coredata_new
  # safer methoda if no corredata is passed
  x <- xts(coredata_new, index(x))
  x_result <- x
  
  # Should have always made it here
  if(x_try.xts_success) { 
    xts::reclass(x_result, x_orig) 
  } -> x_result
  
  Sys.setenv(TZ=oldtz)
  options(ops)
   
  return(x_result)

}

# >  year.less.then.or.equal.xts(head(GDP,12), 1948 )
#            [,1]
# 1947-01-01    1
# 1947-04-01    1
# 1947-07-01    1
# 1947-10-01    1
# 1948-01-01    1
# 1948-04-01    1
# 1948-07-01    1
# 1948-10-01    1
# 1949-01-01    2
# 1949-04-01    2
# 1949-07-01    2
# 1949-10-01    2


# morning: expand.xts

## [ ] SKIPPED FOR NOW # WILL COME BACK LATER ##
# o_args is av named vector of arguments ( but user should really should use a Curry )
# IF o_args IS OF A MIXED DATA.TAPE us a list INSTEAD ( of a vector ) =list(indexAt= 'lastof', OHLC = FALSE)

#' Title
#'
#' @return
#' @export
#'
#' @examples
expand.xts <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL) {
  
  # BASED ON 
  #   Found by RSEEK
  #   Time series cross-validation 5
  #   January 24, 2013
  #   By Zachary Mayer ( OTHERS BY THIS AUTHOR: http://www.r-bloggers.com/author/zachary-mayer/ )
  #   Zachary Deane-Mayer  
  #   ( CARET GUY: AND: Author of library(caretEnsemble)
  #     http://www.r-bloggers.com/time-series-cross-validation-5/
  #       http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
  #     GIST OF THIS ON GITHUB
  #     https://gist.github.com/zachmayer/4630129#file-1-load-data-r
  #     ALSO THE AUTHOR OF (cv.ts)(github)
  
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
  
    check_uses_packages_available("3.4.1",c("zoo","xts","rlist","stringr","DescTools","plyr","DataCombine"), matched_call)
    
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
      
    require(xts) # # Attaching package: 'zoo'
    # IF NOT Error in try.xts(element1) : could not find function "try.xts"
    
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
  
# # testing
# library(quantmod); getSymbols("IBM")
# 
# SMA
#
# head(expand.xts(IBM, fnct = "TTR::SMA", whiches = 2:3                ),2) # default

# # expected pass method
# head(expand.xts(IBM, fnct =                "TTR::SMA"       , whiches = 2:3                ),2) # default
# #
# head(expand.xts(IBM, fnct =                 TTR::SMA        , whiches = 2:3                ),2) # default
# #
# head(expand.xts(IBM, fnct = "function(x,n){ TTR::SMA(x,n) }", whiches = 2:3                ),2) # default
# #
# head(expand.xts(IBM, fnct =  function(x,n){ TTR::SMA(x,n) } , whiches = 2:3                ),2) # default


# head(expand.xts(IBM, fnct = "TTR::SMA", whiches = 2:3, prefix = FALSE),2) # default
# head(expand.xts(IBM, fnct = "TTR::SMA", whiches = 2:3, prefix = TRUE ),2) 
# 
#
# # if xts # should dispatch on xts:::na.locf.xts
# head(expand.xts(IBM, fnct = "na.locf"),2)
#
# na.locf
# 
# IBM2 <- IBM
# IBM2[2:3,1] <- NA_real_
# head(IBM2)
# head(expand.xts(IBM2, fnct = "na.locf"),6)
# 
# head(expand.xts(IBM,  fnct = "na.locf", alt_name = "NALOCF"),2)
#
# lag.xts
#
# head(merge(IBM, expand.xts(IBM, fnct = "lag.xts", whiches = 1:2)))
# 
# PCTCHG(lag.xts)
# 
# head(expand.xts(IBM, fnct = "PCTCHG.xts", whiches = 1),6)
#
# head(expand.xts(IBM, fnct = "PCTCHG.xts", whiches = 1:2, alt_name = "pastPCTCHG", o_args = c(to_future = FALSE)),6)
# tail(expand.xts(IBM, fnct = "PCTCHG.xts", whiches = 1:2, alt_name = "futPCTCHG" , o_args = c(to_future = TRUE )),6)
#
# # xts::merge.xts # dispach
# head(merge(IBM, expand.xts(IBM, fnct = "TTR::SMA", whiches = 2:3)))
# 

# REDUCED index size 
# head(expand.xts(IBM,  fnct = "to.monthly", o_args = list(indexAt= 'lastof', OHLC = FALSE)),6)


# # testing 
# library(quantmod); 

# getSymbols("GDP", src = "FRED")

# WEEKENDS WILL SHOW DELAYS 
# WILL INCREASE the number of days
# head(expand.xts(GDP, fnct = "delay_since_last_day.xts", alt_name = "DELAY"),10) 
#            GDP.DELAY
# 1947-01-01         0
# 1947-01-02         1
# 1947-01-03         2
# 1947-01-04         3
# 1947-01-05         4
# 1947-01-06         5
# 1947-01-07         6
# 1947-01-08         7
# 1947-01-09         8
# 1947-01-10         9

# seq ... as long as the characters order correctly ... should work
# 
# ONLY the index is important: so ONLY passing NO coredate:  xts(,index(IBM)
#
# head(expand.xts(xts(,index(IBM)), fnct = "year.less.then.or.equal.xts", whiches =  seq(lubridate::year(min(index(IBM))), lubridate::year(max(index(IBM))),by = 1), alt_name = "y_lth_or_eq_to_fact"),1)
# tail(expand.xts(xts(,index(IBM)), fnct = "year.less.then.or.equal.xts", whiches =  seq(lubridate::year(min(index(IBM))), lubridate::year(max(index(IBM))),by = 1), alt_name = "y_lth_or_eq_to_fact"),1)


# TO DO
# Reproducible Finance with R: Sector Correlations - Jonathan Regenstein
# merged_xts$rolling_cor <- rollapply
# https://www.rstudio.com/rviews/2017/01/18/reproducible-finance-with-r-sector-correlations/
# cut(mtcars$mpg(quantile(mtcars$mpg
# findInterval
# datavis::weighted.quantile

#  What are the ways of treatng missing values in XGboost? #21
#  Internally, XGBoost will automatically learn what is the best direction to go when a value is missing. 
#  For continuous features, a missing(default) direction is learnt for missing value data to go into, so when the data of the speficific value is missing, then it goes to the default direction
#  3.4 Sparsity-aware Split Finding
#  https://arxiv.org/pdf/1603.02754.pdf
#  10 JUN 2016
#  XGBoost: A Scalable Tree Boosting System
#  29 APR 2016
# https://github.com/dmlc/xgboost/issues/21

# debugSource('W:/R-3.4._/goodsight01.R')
# rm(list=setdiff(ls(all.names=TRUE),c()))



# perform function maxx over previous observation and currenct observation ( in THAT order )
#
# expects(inherits) an 'xts' or 'zoo' object
# n # the number of function maxx to 'perform over' #
# n = 2 means previous observation and currenct observation ( in THAT order )
# note: earliest(first) value is ALWAYS the result of "function max to 'perform over' #'
#
maxx <- function(x, n = 2) {

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
# sample.xts <- as.xts(sample_matrix
# sample.xts[1:4,c("Low","Close")]
#                 Low    Close
# 2007-01-02 49.95041 50.11778
# 2007-01-03 50.23050 50.39767
# 2007-01-04 50.26414 50.33236
# 2007-01-05 50.22103 50.33459
# 
# > maxx(sample.xts[1:4,c("Low","Close")])
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



# goodsight01.R


