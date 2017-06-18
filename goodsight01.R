

# goodsight01.R



# last observation carried forard limited
na.locfl <- function(x, n = NULL) {

  # uses zoo:::rollapply.zoo, DescTools::DoCall

  INPUT <- x
  cINPUT <-  class(INPUT)[1]

  # na.locf from the 'company information' last quarterly report
  # but only carry forward a max of '2 periods'(THAT). XOR '4 periods'
  # After THAT. NAs follow 

  # NOT USING xts:::rollapply.xts 
  # NEED "partial = TRUE" support ( to handle early smaller windows )
  #   so using zoo:::rollapply.zoo
      
  # I WANT to USE the ZOO method ( no dispatch )
  zoo:::rollapply.zoo(as.zoo(INPUT), width = list(seq(-1*n, 0)),  FUN = function(x) {
    
      # if the 'element of interest'(last) is 'NA'
      # and in the width range, there exists at least one other element that  is 'not NA'
      # then about the range 'last observation carry forward'
      #   to return the 'new element of interest' ( that will now have a "non-NA' value)
      # othewise ( the entire range stays all 'NA's )
      #   return just the element of interest ( will be 'NA' )   
      
      if( is.na(x[NROW(x)]) && (max(as.integer(!is.na(x))) > 0) ) { 
        na.locf(x) -> y
        return(y[NROW(y)]) 
      } else {
        return(x[NROW(x)])
      }
    } , partial = 1 # min window size for partial computations 
  )  -> RES
  
  if(class(RES)[1] != "zoo") DescTools::DoCall(paste0("as.",cINPUT),list(RES)) -> RES

  return(RES)
  
  # ORIG FROM
  # https://github.com/AndreMikulec/expressions/blob/8a910454ea590a30878e97b18d5e9dbe45a9d4fb/main-foresight3-999.R#L2287

}

# # input
# # xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7)))
#            [,1]
# 1970-01-11  101
# 1970-01-21   NA
# 1970-01-31   NA
# 1970-02-10   NA
# 1970-02-20  102
# 1970-03-02   NA
# 1970-03-12   NA
# # na.locfl( xts(c(101,NA,NA,NA,102,NA,NA),zoo::as.Date(seq(10, 10*7, length.out = 7))), 2 )
#            [,1]
# 1970-01-11  101
# 1970-01-21  101
# 1970-01-31  101
# 1970-02-10   NA
# 1970-02-20  102
# 1970-03-02  102





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
PCTCHG <- function(x, whiches, to_future = NULL) { 
  lag_direction <- 1 # normal backwards
  if(to_future == TRUE) lag_direction <- -1
  ( x - xts::lag.xts(x, whiches * lag_direction) )/ abs(x) * 100 * lag_direction
} 


# o_args is av named vector of arguments ( but user should really should use a Curry )
# IF o_args IS OF A MIXED DATA.TAPE us a list INSTEAD ( of a vector ) =list(indexAt= 'lastof', OHLC = FALSE)
calculate <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL) {
  
  matched_call <- capture.output(str(match.call()))
  
  calculate_inner <- function(x = NULL, fnct = NULL, whiches = NULL, alt_name = NULL, o_args = NULL, prefix = NULL) {
    # uses zoo::is.zoo, zoo::as.zoo, zoo::na.locf, DescTools::DoCall, 
    # xts:::na.locf.xts(dispatch), xts:::merge.xts(dispatch), plyr::join_all,  DataCombine::VarDrop, stringr::str_replace_all
    # xts::is.xts, xts::as.xts, rlist::list.flatten(X?X),  rlist::list.ungroup, stringr::str_replace_all, plyr::mutate
  
    check_uses_packages_available("3.4.0",c("zoo","xts","rlist","stringr","DescTools","plyr","DataCombine"), matched_call)
    
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
    
    if(is.null(     x))  stop("run-time user must provide input data")
    if(is.null(   fnct)) stop("run-time user must provide a function 'fnct'")
    # if(is.null(whiches)) stop("run-time user must provide a function 'fnct' arguments: e.g. 0:4")
    if(is.null(prefix))  prefix  <- FALSE # do  I of name fnct/alt_name do a preprend(TRUE) or append(FALSE)(default) 
    
    has_whiches <- TRUE     # patch # if does not have a non-null wiches argument, then give it one so it can LASTER do ONE loop
    if(is.null(whiches)) { has_whiches <- FALSE ; whiches = -Inf }  
  
    have_alt_name <- FALSE
    if(!is.null(alt_name)) have_alt_name <- TRUE # patch # but REALLY the user should use a Curry
  
    x -> INPUT  
  
    clINPUT <-  class(INPUT)[1] # typically "xts" "zoo"
    rINPUT <-  if(xts::is.xts(INPUT) || zoo::is.zoo(INPUT)) { index(INPUT) }  else { rownames(INPUT) }
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
    
    if(any(clINPUT %in% c("xts","zoo"))) { 
      # assign("RETs",eval(parse(text=paste0(clINPUT[1],"::","as.",clINPUT[1],"(RETs,rINPUT)")))) # xts/zoo get an extra index
      # xts:::merge.xts(dispatch)
      RETs <- DescTools::DoCall("merge", RETsUGtDT )
    } else { 
      # assign("RETs",eval(parse(text=paste0(                "as.",clINPUT[1],"(RETs)"))))
      # rownames(RETs) <- rINPUT
      RETs <- plyr::join_all(RETsUGtDT, by = "index", type = "full")
      row.names(RETs) <- RETs$index
      RETs <- RETs[!"index" %in% columns(RETs),,drop = FALSE] 
    } 
    
    options(ops)
    
    return(RETs)
  }
  return(calculate_inner(x = x, fnct =  fnct, whiches = whiches, alt_name = alt_name, o_args = o_args, prefix = prefix))

}
  
# # testing
# library(quantmod); getSymbols("IBM")
# 
# SMA
#
# head(calculate(IBM, fnct = "TTR::SMA", whiches = 2:3                ),2) # default
# head(calculate(IBM, fnct = "TTR::SMA", whiches = 2:3, prefix = FALSE),2) # default
# head(calculate(IBM, fnct = "TTR::SMA", whiches = 2:3, prefix = TRUE ),2) 
# 
# # if xts # should dispatch on xts:::na.locf.xts
# head(calculate(IBM, fnct = "na.locf"),2)
#
# na.locf
# 
# IBM2 <- IBM
# IBM2[2:3,1] <- NA_real_
# head(IBM2)
# head(calculate(IBM2, fnct = "na.locf"),6)
# 
# head(calculate(IBM,  fnct = "na.locf", alt_name = "NALOCF"),2)
#
# lag.xts
#
# head(merge(IBM, calculate(IBM, fnct = "lag.xts", whiches = 1:2)))
# 
# PCTCHG(lag.xts)
# 
# head(calculate(IBM, fnct = "PCTCHG", whiches = 1),6)
#
# head(calculate(IBM, fnct = "PCTCHG", whiches = 1:2, alt_name = "pastPCTCHG", o_args = c(to_future = FALSE)),6)
# tail(calculate(IBM, fnct = "PCTCHG", whiches = 1:2, alt_name = "futPCTCHG", o_args = c(to_future = TRUE)),6)
#
# # xts::merge.xts # dispach
# head(merge(IBM, calculate(IBM, fnct = "TTR::SMA", whiches = 2:3)))
# 

# REDUCED index size 
# head(calculate(IBM,  fnct = "to.monthly", o_args = list(indexAt= 'lastof', OHLC = FALSE)),6)


# # time since 'end of data'
# TMsinceEOD
# 
# # time since report
# TMsinceREP
# 
# # irregular: time(in future) window(horizon) to prediction
# TMinfutTOPRED



delay_since_last_obs.default <- function(x) {

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
   
  return(new_vec)
 
}
 
# delay_since_last_obs.default(c(101,NA,NA,NA,102,NA,NA))
# [1] 0 1 2 3 0 1 2



# note: index(first/last(x, '1 day')) expected to return ONE single element
# note: consider using period.apply( to reduce a a 'set of first/last '1 day' to ret ONE single element 

collofdays2daily.xts <- function(x) {

  require(xts)

  # dispatch on xts:::merge.xts, seq.Date, xts:::index.xts, xts:::first/last.xts
  x_days <- xts(,seq(index(first(x, '1 day')),index(last(x, '1 day')), by = 1))
  
  # dispach on xts:::merge.xts
  ret <- merge(x,x_days)
  return(ret)
  
}
# x <- xts(c(11,13,15),zoo::as.Date(c(1,3,5))) 
# xc <- collofdays2daily.xts(x)
# xc
#              x
# 1970-01-02 11
# 1970-01-03 NA
# 1970-01-04 13
# 1970-01-05 NA
# 1970-01-06 15



delay_since_last_obs.xts <-function(x) { 

  # ONLY works on a single column xts

  # uses   delay_since_last_obs.default
  
  x_core  <- as.vector(coredata(x))
  x_index <- index(x)
  
  x_core_new <- delay_since_last_obs.default(x_core)
  
  return(xts(x_core_new,x_index))

} 

# ADD A a revord fo each day is this what I want?
delay_since_last_day.xts <-function(x) { 

  # ONLY works on a single column xts

  # uses   delay_since_last_obs.default
  # uses xts:::merge.xts
  
  # more dates - create temporary rows
  x_nonsparse <- collofdays2daily.xts(x)
  
  # find delays(0 - no delay over NA, 1 - one delay 'at' NA)
  x_nonsparse_delays <- delay_since_last_obs.xts(x_nonsparse)
  
  return(x_nonsparse_delays)

} 

# # testing 
# library(quantmod); 


# getSymbols("GDP", src = "FRED")

# WEEKENDS WILL SHOW DELAYS 
# WILL INCREASE the number of days
# head(calculate(GDP, fnct = "delay_since_last_day.xts", alt_name = "DELAY"),6) 

# > head(calculate(GDP, fnct = "delay_since_last_day.xts", alt_name = "DELAY"),10) 
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



# is the xts observation na? 1 - true  2 - false(regular observation)
is.xts.na <- function(x) {

   # uses ojUtils::ifelseC

   # expecting a 'single' column xts

   x_vector     <- as.vector(coredata(x))
   x_vector_len <- length(x_vector)

   coredata_new <- ojUtils::ifelseC(is.na(x_vector), rep(1,x_vector_len), rep(2,x_vector_len))
   coredata(x)  <- coredata_new

   return(x)

} 
# xts(c(11,NA,NA,14,NA),zoo::as.Date(1:5))
#            [,1]
# 1970-01-02   11
# 1970-01-03   NA
# 1970-01-04   NA
# 1970-01-05   14
# 1970-01-06   NA
# 
# is.xts.na(xts(c(11,NA,NA,14,NA),zoo::as.Date(1:5)))
#            [,1]
# 1970-01-02    2
# 1970-01-03    1
# 1970-01-04    1
# 1970-01-05    2
# 1970-01-06    1

# meant later to be flagged "_factor" then future as.factor


# LEFT_oFF 
# BACKWARD ADJUST FRED EOM REPORT DAYS IF LASTS ON THE FIRST/2ND/3RD/4TH/ PULLthe data back tot the FIRST



# TO DO
# Reproducible Finance with R: Sector Correlations - Jonathan Regenstein
# merged_xts$rolling_cor <- rollapply
# https://www.rstudio.com/rviews/2017/01/18/reproducible-finance-with-r-sector-correlations/
# cut(mtcars$mpg(quantile(mtcars$mpg
# findInterval
# datavis::weighted.quantile

## BASED ON 
  #   Found by RSEEK
  #   Time series cross-validation 5
  #   January 24, 2013
  #   By Zachary Mayer ( OTHERS BY THIS AUTHOR: http://www.r-bloggers.com/author/zachary-mayer/ )
  #   ( CARET GUY: AND: Author of library(caretEnsemble)
  #     http://www.r-bloggers.com/time-series-cross-validation-5/
  #       http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
  #     GIST OF THIS ON GITHUB
  #     https://gist.github.com/zachmayer/4630129#file-1-load-data-r
  #     ALSO THE AUTHOR OF (cv.ts)(github)


# debugSource('W:/R-3.4._/goodsight01.R')
# rm(list=setdiff(ls(all.names=TRUE),c()))

