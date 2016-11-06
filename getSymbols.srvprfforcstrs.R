
options(browserNLdisabled=TRUE)

getSymbols.srvprfforcstrs.test <- function() { 


  # NOTE: IS IT ALREADY ON QUANDL - BUT  
  # 1. QUANDLE DOES NOT EXACT RELEASE DATES
  # 2. QANDLE ONLY GOES BACK TO 2005 ( EXCEL GOES BACK TO THE 1960S )
  # https://www.quandl.com/data/FRBP
  
  # R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
  
  # OCT 16 2016
  #
  # I had some problems, so I did
  # devtools::install_github("RcppCore/Rcpp")
  # devtools::install_github("rstats-db/DBI")
  # devtools::install_github("rstats-db/RSQLite")
  
  # Survey of Professional Forecasters
  # from the Philadelphia FED
  # release dates returned in a data.frame of RICH columns
  #
  # usage
  #
  # spf_release_dates_AS_OF_TODAY <- spf_release_dates()
  #
  
  # Run of October 11, 2016
  #
  # > str(spf_release_dates_AS_OF_TODAY)
  # 'data.frame':   106 obs. of  5 variables:
  #  $ year_quarter      :Class 'yearqtr'  num [1:106] 1990 1990 1991 1991 1991 ...
  #  $ year              : int  1990 1990 1990 1991 1991 1991 1991 1992 1992 1992 ...
  #  $ quarter           : int  2 3 4 1 2 3 4 1 2 3 ...
  #  $ true_deadline_date: Date, format: "1990-08-23" "1990-08-23" ...
  #  $ release_date      : Date, format: "1990-08-31" "1990-08-31" ...
  
  # > tail(spf_release_dates_AS_OF_TODAY)
  #     year_quarter year quarter true_deadline_date release_date
  # 101      2015 Q2 2015       2         2015-05-12   2015-05-15
  # 102      2015 Q3 2015       3         2015-08-11   2015-08-14
  # 103      2015 Q4 2015       4         2015-11-10   2015-11-13
  # 104      2016 Q1 2016       1         2016-02-09   2016-02-12
  # 105      2016 Q2 2016       2         2016-05-10   2016-05-13
  # 106      2016 Q3 2016       3         2016-08-09   2016-08-12
  
  #
  ## implementation
  #
  # changes
  #
  # Oct 13 2016
  # remove 1st useless record  DONE
  
  
  spf_release_dates <- function() { 
    
    # R version 3.2.2 (2015-08-14) -- "Fire Safety"
    
    # imports
    # 
    # DescTools::StrTrim - trim the right side of each line
    # tidyr::fill        - put years put down columns
    # zoo::as.yearqtr    - rich type         
    
    # save to put back at the end
    # ERRORs on LATER versions of R, so SKIP
    # inte <- setInternet2(NA)
    # setInternet2(TRUE) # may? by on by default?
    
    oldtz <- Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    
    down_res <- download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-release-dates.txt?la=en', destfile = 'spf-release-dates.txt')  
    # success == 0
    if(down_res != 0) stop("spf_release_dates are not? available at the Philadelphia FED?") 
    
    spf_release_dates_txt <- readLines("spf-release-dates.txt")
    
    # need a column space delimited file
    clean_spf_release_dates_txt <- vector(mode="character")
    line_counter <- 0
    for(line in spf_release_dates_txt) {
      
      line_counter <- line_counter + 1
      clean_line <- line
      
      # skip the first 6 lines
      if(line_counter < 7) next
      
      # skip lines with zero or one character
      if(nchar(line) %in% c(0,1)) next
      
      # skip lines with only whitespace
      if(nchar(DescTools::StrTrim(clean_line)) == 0) next
      
      # skip the line that starts with a tab
      # clean_line <-  gsub("^\t","", clean_line)
      
      # remove asteriskes
      clean_line <-   gsub("*","", clean_line, fixed = TRUE)
      
      # remove trailing whitespace
      clean_line <-  DescTools::StrTrim(clean_line, method = "right")
      
      # replace a tab with 4 spaces
      clean_line <- gsub("\t", "    ", clean_line)
      
      # skip any line that has a first character of a capital letter
      if(grepl("^[A-Z]", clean_line)) next
      
      # skip any line that has a second character of a lowercase letter
      if(grepl("^ [a-z]", clean_line)) next
      
      # make and sequence of 16,15,14,12,or 11 characher lines into a 14 character line
      
      # 16->8
      clean_line <- gsub("                ", "_", clean_line)
      
      # 15->8
      clean_line <- gsub("               ", "_", clean_line)
      
      # 14->8
      clean_line <- gsub("              ", "_", clean_line)
      
      # 13->8
      clean_line <- gsub("             ", "_", clean_line)
      
      # 12->8
      clean_line <- gsub("            " , "_", clean_line)
      
      # 9->8
      clean_line <- gsub("         " , "_", clean_line)
      
      # 8->8
      clean_line <- gsub("        " , "_", clean_line)
      
      # 7->8
      clean_line <- gsub("       " , "_", clean_line)
      
      # _->8
      clean_line <- gsub("_" , "        ", clean_line)
      
      # first four spaces to 0000
      clean_line <- sub("^    " , "0000", clean_line)
      
      # remove Q
      clean_line <- sub("Q" , "", clean_line)
      
      # combine
      clean_spf_release_dates_txt <- c(clean_spf_release_dates_txt,clean_line)
    }
    
    # loop result is . . .
    # clean_spf_release_dates_txt
    
    # COULD use a TEXT CONNECTION instead of WRITING to the OS
    writeLines(clean_spf_release_dates_txt, "clean_spf_release_dates_txt.txt")
    
    # COULD use a TEXT CONNECTION instead of WRITING to the OS
    # text to df
    read.table("clean_spf_release_dates_txt.txt"
               , col.names  = c("year"     ,"quarter"  ,"true_deadline_date","release_date")
               , colClasses = c("character","character","character"         , "character")
               , fill = TRUE # unequal length rows
               , stringsAsFactors = FALSE
    ) -> clean_spf_release_dates_df
    
    # dummy years(0000) to NA
    clean_spf_release_dates_df[clean_spf_release_dates_df$year == "0000","year"] <- NA_character_
    
    # fill years e.g. 1990 ... down the columns
    clean_spf_release_dates_df <- tidyr::fill(clean_spf_release_dates_df,year)
    
    # safe: 1970+
    clean_spf_release_dates_df$true_deadline_date <- zoo::as.Date(clean_spf_release_dates_df$true_deadline_date, format = "%m/%d/%y") 
    clean_spf_release_dates_df$release_date       <- zoo::as.Date(clean_spf_release_dates_df$release_date      , format = "%m/%d/%y") 
    
    clean_spf_release_dates_df$year    <- with(clean_spf_release_dates_df , { as.integer(year)    } )
    clean_spf_release_dates_df$quarter <- with(clean_spf_release_dates_df , { as.integer(quarter) } )
    
    # rich type - Class 'yearqtr'
    year_quarter <- zoo::as.yearqtr( with( clean_spf_release_dates_df, {  paste0(year," Q",quarter) } ) )
    
    # together
    clean_spf_release_dates_df <- cbind( year_quarter, clean_spf_release_dates_df )
    
    # 1990 Q2 was released TOO LATE, so the record is not useful, so it is omitted
    
    # Deadline and Release Dates for the Survey of Professional Forecasters
    # True deadline and news release dates for surveys prior to 1990:Q2 are not known.
    # https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-release-dates.txt?la=en
    
    # Survey         True Deadline Date       News Release Date
    
    # 1990 Q2             8/23/90*            8/31/90*
    #      Q3             8/23/90             8/31/90
    
    # Oct 13 2016
    # remove 1st useless record  DONE
    clean_spf_release_dates_df <- clean_spf_release_dates_df[-1, , drop = FALSE]
    
    # ERRORs on LATER versions of R, so SKIP
    # end function
    Sys.setenv(TZ=oldtz)
    
    # return property
    # err
    # setInternet2(inte)
    
    return(clean_spf_release_dates_df)
    
  }
  
  bookmarkhere <- 1
  
  
  # typical run
  # lean_spf_release_dates_now <- spf_release_dates()
  
  
  # calculate the future percent changes in social and economic measures
  # using forcasts submitted by the participants in the 
  # Survey of Professional Forecasters
  
  # common symbols recommended actual argument
  # c("CPI","NGDP","INDPROD","TBOND","UNEMP","RCONSUM")
  
  # CPI Inflation Rate (CPI)
  # Nominal Gross National Product/Gross Domestic Product (NGDP)
  # Industrial Production Index (INDPROD)
  # 10-Year Treasury Bond Rate (TBOND)
  # Civilian Unemployment Rate (UNEMP)
  # Real Personal Consumption Expenditures (RCONSUM)
  
  # See more choices here
  
  # Historical Data Files for the Survey of Professional Forecasters
  # https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files
  
  # The 'symbol root prefix' with be the symbol name in lowercase.
  # E.g. "CPI" -> "cpi"
  
  # default response(s) unless specified
  # responses = "median"
  
  # Only tested/designed to work with median and mean
  # response can be "mean" or "median" or a combination: c("median","mean")
  # I.e."median" -> "mdn"  "mean" -> "ave"
  # The 'measure root prefix' will be the measure 
  
  # measure = "cpi" ( root prefix )
  # responses = "median" 
  
  # cpi_mdn
  
  # prevous quarter to target of interest
  # currently only applied on quarter forcasts
  
  # (derived) columns of interest ( not implemented yet )

  # pctchg = TRUE # default   
  
  # input
  #
  # typical expect run
  #
  # library(qanantmod)
  # srvprfforcstrs <- getSymbols.srvprfforcstrs(c("CPI","NGDP"), response = "median", pctchg = TRUE, auto.assign = FALSE)
  
  # output
  
  # column mapping
  # from Survey of Professional Forecasters ( Philadelphia FED )
  # to getSymbols.srvprfforcstrs
  
  # column postfix number to 
  
  # This(1) is the ACTUAL result ( NOT forcasters prediction )
  # 1 -> cqp1 # calendar quarter past 1(last) quarter. 
  
  # 2 -> cqf0 # prediction of the forcasters made IN the current calendar quarter
  # who are predicting the result for the END OF the CURRENT calendar quarter
  
  # 3 -> cqf1 # prediction of the forcasters made IN the current calendar quarter
  # who are predicting the result for the END OF the NEXT(1st) calendar quarter
  
  # 4 -> cqf2 # prediction of the forcasters made IN the current calendar quarter
  # who are predicting the result for the END OF the NEXT NEXT (2st) calendar quarter
  
  # etc . . . 5 . . . 6 . . .
  
  # a -> cqf0 # prediction of the forcasters made IN the current calendar year
  # who are predicting the result for the END OF the CURRENT calendar year
  
  # b -> cqf1 # prediction of the forcasters made IN the current calendar year
  # who are predicting the result for the END OF the NEXT(1st) calendar year
  
  # etc . . . c . . . d . . .
  
  # if ( I THINK )
  # pctchg == TRUE
  #
  # requires colunns 1 and 2
  #
  # if cpi and media, then this math is 
  # (cqf2 - cqf1) / abs(cqf1) == cqf_mdn_pch_d1t2 
  
  require(quantmod)
  
  # based on quantmod::getSymbols.FRED

  # getSymbols.srvprfforcstrs {{{
  `getSymbols.srvprfforcstrs` <- function(Symbols,env,
      return.class="xts", ...) {
      importDefaults("getSymbols.srvprfforcstrs")
      
      # R version 3.2.2 (2015-08-14) -- "Fire Safety"
    
      # ON OCT 16 2016 because of RSQLite woes
      #
      # devtools::install_github("RcppCore/Rcpp")
      # devtools::install_github("rstats-db/DBI")
      # devtools::install_github("rstats-db/RSQLite")
      #
      # https://github.com/rstats-db/RSQLite
    
      # imports
      # 
      # readxl::read_excel
      # plyr::arrange plyr::join_all
    
      # uses
      #
      # spf_release_dates  
    
      ops <- options()
      options(warn = 1)
    
      oldtz <- Sys.getenv('TZ')
      if(oldtz=='') {
        Sys.setenv(TZ="UTC")
      }
    
      require(RSQLite) # 

      require(xgboost)
      require(caret) # xgbTree will 'use plyr'
      
      require(dygraphs) # uses dygraphs::dygraph

       this.env <- environment()
       for(var in names(list(...))) {
          # import all named elements that are NON formals
          assign(var, list(...)[[var]], this.env)
       }
       if(!hasArg(verbose)) verbose <- FALSE
       if(!hasArg(auto.assign)) auto.assign <- TRUE

       if(verbose) cat("downloading Survey of Professional Forecasters release dates",".....\n\n")
       
       # spf_release_dates_now <- spf_release_dates()
       # TESTING
       load(file = "spf_release_dates_now.Rdata", envir = environment())
       
       if(verbose) cat("done.\n")
       
       # need columns for SQL joins
       cbind(spf_release_dates_now   
             , release_date_year =  as.integer(format(spf_release_dates_now$release_date,"%Y"))
             , release_date_month = as.integer(format(spf_release_dates_now$release_date,"%m"))
             , release_date_day   = as.integer(format(spf_release_dates_now$release_date,"%d"))
       ) -> spf_release_dates_now
       
       # begin responses
       
       srvprfforcstrs.URL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data"
       if(!hasArg(responses)) responses <- "median"
       
       # tmp <- tempfile()
       
       # many sheets of an excel file
       frs <- list()
       for (responses_i in responses) {
         
         if(responses_i == "median") measure_abbr <- "mdn"
         if(responses_i == "mean")   measure_abbr <- "ave"
         
         if(verbose) cat("downloading ",responses_i,".....\n\n")
         URL <- paste(srvprfforcstrs.URL, "/", responses_i, "Level.xls", sep="")
         # MS Excel requires binary
         
         # TESTING
         # if(responses_i == "median") {
         #    quantmod:::try.download.file(URL, destfile="C:\\Users\\NFF397N\\160802ANDREGRACE\\GRACE\\R_USER_3.3.1\\medianLevel.xls", quiet=!verbose, mode="wb", ...) 
         # } else {
             quantmod:::try.download.file(URL, destfile=paste0(tempdir(),"/",responses_i,".xls"), quiet=!verbose, mode="wb", ...) 
         # }
         
         on.exit(unlink(paste0(tempdir(),"/",responses_i,".xls")))
         

         for(i in seq_along(Symbols)) {
          
           if(verbose) cat("reading ",responses_i,".....\n\n")

           # TESTING
           # one sheet of en excel file
           # needs to read the extension (.xls)
           if(responses_i == "median") {
             fr <- try( readxl::read_excel("C:\\Users\\NFF397N\\160802ANDREGRACE\\GRACE\\R_USER_3.3.1\\medianLevel.xls", sheet = Symbols[[i]], na = "#N/A"), silent = TRUE)
           } else {
             fr <- try( readxl::read_excel(paste0(tempdir(),"/",responses_i,".xls"), sheet = Symbols[[i]], na = "#N/A"), silent = TRUE )
           }
           if(any(class(fr) %in% c("try-error"))) { 
             warning(paste0("Symbol ", Symbols[[i]]," in response ", responses_i, " is not found.")) 
             warning("NOt all combinations of responses and symbols are available.") 
             next
           } 
           
           if(verbose) cat("done reading ",responses_i,".....\n\n")
           
           colnames(fr) <- tolower(colnames(fr))
           
           fr$year    <- with(fr , { as.integer(year)    })
           fr$quarter <- with(fr , { as.integer(quarter) })
           
           # rich type - Class 'yearqtr'
           year_quarter <- zoo::as.yearqtr( with( fr , {  paste0(year," Q",quarter) } ) )
           
           # together ( put at the front )
           fr <- cbind( year_quarter, fr )
           
           # separate ^root with ^root_
           colnames(fr) <-  gsub(tolower(Symbols[[i]]),paste0(tolower(Symbols[[i]]),"_",measure_abbr,"_"),colnames(fr))
           
           # rename for ease of identification
           
           # past,now, and future
           # exception: past is 'actual'
           # calendar quarter (cqf): past quarter (cqp1) , current quarter (cqf0), and future quarters (cqf[1-])
           colnames(fr) <- gsub("_1$","_cqp1",colnames(fr))
           colnames(fr) <- gsub("_2$","_cqf0",colnames(fr))
           colnames(fr) <- gsub("_3$","_cqf1",colnames(fr))
           colnames(fr) <- gsub("_4$","_cqf2",colnames(fr))
           colnames(fr) <- gsub("_5$","_cqf3",colnames(fr))
           colnames(fr) <- gsub("_6$","_cqf4",colnames(fr))
           colnames(fr) <- gsub("_7$","_cqf5",colnames(fr))
           colnames(fr) <- gsub("_8$","_cqf6",colnames(fr))
           
           # past,now, and future
           # exception: past is 'actual'
           # calendar year future (cyf): current year (cyf0), and future 3 years (cyf[1-])
           colnames(fr) <- gsub("_a$","_cyf0",colnames(fr))
           colnames(fr) <- gsub("_b$","_cyf1",colnames(fr))
           colnames(fr) <- gsub("_c$","_cyf2",colnames(fr))
           colnames(fr) <- gsub("_d$","_cyf3",colnames(fr))
           
           # accumulate the files(excel tab pages)
           # list(a): protection against turning a df into list items
           frs <- c(frs,list(fr)); names(frs)[length(frs)] <-  paste0(tolower(Symbols[[i]]),"_",measure_abbr)
         }
       }
           
       if(verbose) cat("done.\n")
           
       # big SQL to creat that ONE big table
         
       con <- dbConnect(RSQLite::SQLite(), ":memory:")
       
       # Dates are converted to integers (UTC days since 1970)
       # upload
       dbWriteTable(con, "spf_release_dates", spf_release_dates_now)
       
       for (frs_names_i in names(frs)) {
         
         assign(frs_names_i,frs[[frs_names_i]])
         # upload each
         dbWriteTable(con, frs_names_i, get(frs_names_i))
         
       }
       
       bookmarkhere <- 1
       
       # CREATE ** A BIG table  **

       if (length(frs) > 0) {
         
         available_measures_remaining <- names(frs)
         
         for(idx in seq_along(frs)) {
           
           # do first time work
           if(idx == 1) bulk_measure_name <- "spf_release_dates"
           # other time work
           if(idx != 1) bulk_measure_name <- paste0("result",(idx-1))
           
           if(idx >1) {
             prev_accum_measure_columns <- dbGetQuery(con, paste0("pragma table_info(",bulk_measure_name,");"))
             prev_accum_measure_columns <- prev_accum_measure_columns[,"name",drop=TRUE]
             prev_accum_measure_columns <- prev_accum_measure_columns[!prev_accum_measure_columns %in% c("year","quarter","release_date_year","release_date_month","release_date_day")]
             old_measure_columns_sql_line <- paste0(sapply(prev_accum_measure_columns, function(x,y) { paste0(", ",y,".",x," ",x) }, y = bulk_measure_name), collapse ="")
           } else { # idx == 1
             old_measure_columns_sql_line <- ""
           }
           
           current_measure_name <- available_measures_remaining[1]
           current_measure_column_names   <- names(frs[[current_measure_name]])[!names(frs[[current_measure_name]]) %in% c("year_quarter","year","quarter")]
           current_measure_columns_sql_line <- paste0(sapply(current_measure_column_names, function(x,y) { paste0(", ",y,".",x," ",x) }, y = current_measure_name), collapse ="") 
           
           next_bulk_measure_name <- paste0("result",(idx))
           # SQL  ... create table result1 as ... 
           
           sql_snippet <-  "
           create table NEXT_TABLE as
           select 
           BULK_TABLE.year year, BULK_TABLE.quarter quarter 
           , BULK_TABLE.release_date_year release_date_year, BULK_TABLE.release_date_month release_date_month, BULK_TABLE.release_date_day release_date_day  
           OLD_TABLE_SQL_LINE
           MEASURE_TABLE_SQL_LINE 
           from 
           BULK_TABLE BULK_TABLE left join MEASURE_TABLE MEASURE_TABLE 
           on 
           BULK_TABLE.year = MEASURE_TABLE.year and BULK_TABLE.quarter = MEASURE_TABLE.quarter                 union
           select 
           MEASURE_TABLE.year year, MEASURE_TABLE.quarter quarter
           , BULK_TABLE.release_date_year release_date_year, BULK_TABLE.release_date_month release_date_month, BULK_TABLE.release_date_day release_date_day  
           OLD_TABLE_SQL_LINE
           MEASURE_TABLE_SQL_LINE 
           from 
           MEASURE_TABLE MEASURE_TABLE left join BULK_TABLE BULK_TABLE 
           on 
           BULK_TABLE.year = MEASURE_TABLE.year and BULK_TABLE.quarter = MEASURE_TABLE.quarter 
           order by year, quarter 
           ;"
           
           sql_snippet <- gsub("NEXT_TABLE", next_bulk_measure_name, sql_snippet, ignore.case = FALSE, fixed = TRUE)
           sql_snippet <- gsub("BULK_TABLE", bulk_measure_name , sql_snippet, ignore.case = FALSE, fixed = TRUE)
           sql_snippet <- gsub("MEASURE_TABLE_SQL_LINE", current_measure_columns_sql_line, sql_snippet, ignore.case = FALSE, fixed = TRUE)
           sql_snippet <- gsub("MEASURE_TABLE", current_measure_name, sql_snippet, ignore.case = FALSE, fixed = TRUE)
           sql_snippet <- gsub("OLD_TABLE_SQL_LINE", old_measure_columns_sql_line, sql_snippet, ignore.case = FALSE, fixed = TRUE)
           
           # writeLines(sql_snippet)
           dbGetQuery(con, sql_snippet)
           
           last_idx <- idx
           # so done 
           
           # just done                     
           available_measures_remaining <- available_measures_remaining[-1] 
           
         }
         
       } else {
         
         warning("User neeeds to select at least one measure.") 
         
       }
       
       # NO LONGER USED ( USING INSTEAD: plyr::join_all )
       # result_all <- dbGetQuery(con, paste0("select * from result", last_idx))  
       rm("idx")
       # WORKS ( OR JUST )
       result_all2 <- plyr::arrange(plyr::join_all(dfs = c(list(spf_release_dates_now),frs), by = c("year","quarter"), type = "full", match = "all"), year, quarter)
   
       fake_and_real_realease_dates <- as.Date(result_all2[,"year_quarter",drop = TRUE], frac = 0.5)
       fake_release_dates_tf          <- is.na(result_all2[,"release_date",drop = TRUE])
       result_all2[fake_release_dates_tf ,"release_date"] <- fake_and_real_realease_dates[fake_release_dates_tf]
       result_all2[fake_release_dates_tf ,"release_date_year"]  <- as.integer(format(fake_and_real_realease_dates[fake_release_dates_tf], "%Y"))
       result_all2[fake_release_dates_tf ,"release_date_month"] <- as.integer(format(fake_and_real_realease_dates[fake_release_dates_tf], "%m"))
       result_all2[fake_release_dates_tf ,"release_date_day"]   <- as.integer(format(fake_and_real_realease_dates[fake_release_dates_tf], "%d"))
       
       # save the future index()
       result_all2_redux_index <- as.character(result_all2$release_date)
       
       # I can not put this into coredata()
       # for 'convert to xts'
       # can not carry into the matrix: year_quarter(yearmon), true_deadline_date(Date), release_date(Date)
       result_all2_redux <- result_all2[,!colnames(result_all2) %in% c("year_quarter","true_deadline_date","release_date")]
       
       # create the coredata()
       result_all2_redux <- as.matrix(result_all2_redux)
       
       # create the index()
       rownames(result_all2_redux) <- result_all2_redux_index
       
       # methods(as.xts)
       # ? xts:::as.xts.matrix
       result_all2_redux <- as.xts(result_all2_redux, dateFormat="Date")
       
       # non-response variables
       
       (result_all2_redux[,grep("_cqf1",colnames(result_all2_redux), value = TRUE)] -
         result_all2_redux[,grep("_cqf0",colnames(result_all2_redux), value = TRUE)]
       ) / abs(
         result_all2_redux[,grep("_cqf0",colnames(result_all2_redux), value = TRUE)]
       ) * 100 -> new_variables
       
       # calendar quarter future relative delta zero(0) to one(1)
       colnames(new_variables) <- sub("_cqf1$","_cqfrd0t1",colnames(new_variables))
       
       # together again
       result_all2_redux <- merge.xts(result_all2_redux, new_variables)
       
       # need 'last working days'(sipro) and 'last days'
       
       # creates an NA column ... remove later
       all_days     <- xts(,seq(from=min(as.Date(year_quarter,frac=0)),to=max(as.Date(year_quarter, frac=1)),by=1))[]
       # creates an NA column ... remove later 
       
       # need 'last working days'(sipro)
       all_days_wd  <- all_days[!weekdays(index(all_days)) %in% c("Saturday","Sunday")]
       all_days_l    <- split.xts(all_days,    f = "months")
       all_days_wd_l <- split.xts(all_days_wd, f = "months")
       
       # prepare to process by time division
       
       all_days_l_last    <- lapply( all_days_l    , function(x) { last(x) } ) 
       all_days_wd_l_last <- lapply( all_days_wd_l , function(x) { last(x) } )
       
       all_days_last    <- do.call("rbind.xts", all_days_l_last  )
       all_days_last    <- xts(,index(all_days_last))
       all_days_wd_last <- do.call("rbind.xts", all_days_wd_l_last  )
       all_days_wd_last    <- xts(,index(all_days_wd_last))
       
       # need 'last working days'(sipro) and 'last days'
       # add important dates - last weekday of the month(wd)(sipro) and the last day of the month
       result_all2_redux__days_wd_days_last <- merge.xts(result_all2_redux, all_days_wd_last, all_days_last)
       
       # need gspc future performance of the 'relative changes': last day of the month
       # Indexed by objects of class: [Date] TZ: UTC 
       
       gspc <- getSymbols("^GSPC", auto.assign = FALSE, from = "1954-01-01" )
       gspc <- gspc[,"GSPC.Close"]
       colnames(gspc) <- "gspc"
       
       gspc_all_dates <- seq(min(index(gspc)),max(index(gspc)),1)
       
       # dummy data required for "outer" of merge.xts
       # if no 'dummy' data, then it will perform an 'inner' (BAD = DECEPTIVE )
       gspc_all_dates_length <- length(gspc_all_dates)
       # all days ... later to garantee that to.monthly produces contiguous yearmons ?
       gspc <- merge.xts(xts(rep(1,gspc_all_dates_length),gspc_all_dates),gspc)
       # remove dummy column 1
       gspc <- gspc[,-1]

       # last record *real observation* ( gets obscured in to.monthly ( I *may*? append later)
       gspc_endof_last_record <- tail(suppressWarnings(to.monthly(gspc, OHLC = FALSE, indexAt = 'endof')),1)
       
       # Warning: missing values removed from data
       # index placed in na.action:Class 'omit' 
       # suppressWarnings
       # keep for lag.xts below
       # Indexed by objects of class: [yearmon] TZ: UTC
       gspc <- suppressWarnings(to.monthly(gspc, OHLC = FALSE)) # yearmon
       # xts::to.monthly will add the this months eom date ( that has not been reached yet)
       
       # adjustment ( fix on xts::to.monthly )
       # garantee that today I read yesterdays data 
       # decided NOT to do this HERE
       # if(as.Date(tail(index(gspc),1),frac = 1) >= Sys.Date()) gspc <- gspc[-NROW(gspc),]
       
       # since to.monthly(  , OHCL = FALSE) # I do not need these lines
       # gspc <- gspc[,"gspc.Close"]
       # colnames(gspc) <- "gspc"     # Indexed by objects of class: [yearmon] TZ: UTC
       
       # yearmon 'time differencing' math does not work, because an unfound index returns nothing instead of NA
       # > gspc[min(index(gspc))-1/12]
       #     gspc
       # so xts::lag.xts_yearmon and quantmod::Lag do not work as expected 
       # THAT is why I had to do all of the xts.merge( all dates ) above
       # so I can garantee a contiguous yearmon
       
       # relative delta calenter quarter future 3 (rdcqf3)
       
       # Indexed by objects of class: [yearmon] TZ: UTC
       # BUT diff.xts WORKS BUT only AGAINST previous VALUE ( not yearmon SLOT )
       gspc_rdcqf3  <- (lag.xts(gspc, k = -3) - lag.xts(gspc, k = 0))/ abs(lag.xts(gspc, k = 0)) * 100
       
       colnames(gspc_rdcqf3) <- "gspc_rdcqf3" 
       
       # to be compatable with results xts index  
       index(gspc_rdcqf3) <- as.Date(index(gspc_rdcqf3), frac = 1) # Date
       
       # if the false to.monthy last record index date is greater than today, then remove it
       if(max(index(gspc_rdcqf3)) > Sys.Date()) gspc_rdcqf3 <- gspc_rdcqf3[-NROW(gspc_rdcqf3),]
       # if the false to.monthy last record index date does not equal to today,
       # then append back the last *real reccord* index date    # NA require for outer join # only want the index, 'rd' has already (past) been calculated
       if(max(index(gspc_rdcqf3)) != Sys.Date()) gspc_rdcqf3 <- rbind.xts(gspc_rdcqf3, xts(rep(NA_real_,NROW(gspc_endof_last_record)),index(gspc_endof_last_record)))
       
       # adjustment ( fix on xts::to.monthly )
       # garantee that today I read yesterday's data
       # if( tail(index(gspc_rdcqf3),1) >= Sys.Date()) gspc_rdcqf3 <- gspc[-NROW(gspc_rdcqf3),]
       # empty record ( used in today's prediction )
       #gspc_rdcqf3 <- merge.xts(gspc_rdcqf3, xts(, Sys.Date())) 
       
       # combine results all and 'gspc future'
       
       # put what I am trying to predict in the first column
       result_all2_redux__days_wd_days_last_gspc_rdcqf3 <- merge.xts(gspc_rdcqf3, result_all2_redux__days_wd_days_last)
       
       prediction <- result_all2_redux__days_wd_days_last_gspc_rdcqf3
       predicton_redux <- prediction[,c(colnames(prediction)[1],c("release_date_year", "release_date_month", "release_date_day"),grep("_cqfrd0t1$",colnames(prediction)[-1], value = TRUE))]  
       
       # FIX: SOMEHOW IN THE LAST TO ELEMENTS, BOTH OF THE *SAME DATE* IN THE INDEX
       # ZOO COMPAINS ... SO FIX ... [ ] LEFT_OFF
       
       # locf on the interior values only
       as.xts(with(as.zoo(predicton_redux), {  
         
         sapply( ls(sort=FALSE) , function(x){  
           
           xx <- as.xts(get(x))
           
           min_idx <- min(which(!is.na(as.vector(coredata(xx)))))
           max_idx <- max(which(!is.na(as.vector(coredata(xx)))))
           
           range_seq <- min_idx:max_idx
           
           ranged_xx_na_locb <- na.locf(xx[range_seq,], na.rm = FALSE, fromLast= TRUE)
           inner_result      <- xx
           inner_result[range_seq,] <- ranged_xx_na_locb  
           inner_result_zoo  <- zoo(coredata(inner_result), index(inner_result))
           
           return(inner_result_zoo)
           
         } ) -> outer_result 
         
         return(outer_result)
         
       }), index(predicton_redux)) -> prediction_inner_zone # NAs remain at the extremes
       
       # real dates when a release occurrs
       release_yyyy_mm_dd <- paste0(prediction_inner_zone$release_date_year,"-",prediction_inner_zone$release_date_month,"-",prediction_inner_zone$release_date_day)
       release_yyyy_mm_dd <- zoo::as.Date(ifelse(release_yyyy_mm_dd == "NA-NA-NA", NA_character_, release_yyyy_mm_dd))
       
                                                               # inner dates 
       prediction_train     <- na.trim(prediction_inner_zone[index(prediction_inner_zone) == release_yyyy_mm_dd,]["::2003-12-31",!colnames(prediction_inner_zone) %in% c("release_date_year","release_date_month","release_date_day")], sides = "left" )  
       
       prediction_true_test <- na.trim(prediction_inner_zone[index(prediction_inner_zone) == release_yyyy_mm_dd,]["2004-01-01::",!colnames(prediction_inner_zone) %in% c("release_date_year","release_date_month","release_date_day")], sides = "both" )  # should never be 'left'
       
       
       # help from GRACE\R_USER_3.3.1\scratchW.txt
       
       # need yet! [ ] LEFT_OFF ( but SEE BELOW )
       #
       # prediction_prediction
       #
       # if the date is greater than the max date in prediction_true_test
       # and
       # the date is on or after the quarters release date
       # and
       # the date is on or before the end of its quarter end date
       # 
       # this is not right (yet)
       # 
       # prediction_idx_tf <- unlist(lapply( index(prediction_inner_zone), function(x) { if( 
       #  (max(index(prediction_true_test)) < x) && 
       #  (index(predicton_redux[(as.yearqtr(index(predicton_redux)) == as.yearqtr(x)) && (!is.na(predicton_redux$release_date_day)),])  <= x ) && 
       #  (x <= as.Date(as.yearmon(x),frac = 1)) ) { TRUE } else { FALSE } 
       #} ))   
       #
       # MORE
       # 
       
       # if the forecasters can not predict the change in the gcpc
       #   (then the sitation 'may not' be worth figuring out the prediction_prediction
       
       # caret xgboost tree with early stopping 
       # train - through December 2003
       # real world test - January 2003 throught the latest that I have full dda
       #   should by(prediction day) loop by mid month date that is not a lwd and not an end of month day
       
       
       xgb.ctrl <- trainControl(method = "repeatedcv",   # 10 fold cross validation
                                number = 5,              # do 5 repititions of cv 
                                repeats =1)              # for k-fold, the number of complete sets   
       
       
       xgb.grid <- expand.grid( nrounds = 500,         #the maximum number of iterations
                                max_depth = c(2,6,10),
                                eta = c(0.01,0.1),     # shrinkage
                                gamma = 0.0, 
                                colsample_bytree = 1.0, 
                                min_child_weight = 1.0
       )
       
       bookmarkhere <- 1 
       
       # Variable importance using the caret package (error); RandomForest algorithm
       #   The importance scores can take a while to compute and 
       #   train won't automatically get randomForest to create them. Add importance = TRUE to the train call and it should work.
       # http://stackoverflow.com/questions/18578861/variable-importance-using-the-caret-package-error-randomforest-algorithm
       
       set.seed(2.0)
       
       rf.tune <-train(as.formula(paste0(colnames(prediction_train)[1]," ~ .")),
         data = prediction_train,
         importance = TRUE )
       writeLines("RandomForest(rf) boot method variable importance")
       print(varImp(rf.tune))
       
       writeLines("")
       writeLines("Total number of tuning(prediction_train) observations.")
       writeLines(as.character(NROW(prediction_train)))
       
       xgb.tune <-train(as.formula(paste0(colnames(prediction_train)[1]," ~ .")), 
         data = prediction_train,
         method    ="xgbTree",   
         trControl = xgb.ctrl,
         tuneGrid = xgb.grid)
       
       bookmarkhere <- 1 
       
       # how much  better or not are other tuning parameters
       if(file.exists("srvprfforcstrs_xgboost.png")) unlink("rvprfforcstrs_xgboost.png")
       ggplot(xgb.tune) + theme(legend.position = "top")
       ggsave('srvprfforcstrs_xgboost.png')
       
       
       xgb.predicted.prediction_train     = predict(xgb.tune, newdata = prediction_train[,-1])
       
       xgb.predicted.prediction_true_test = predict(xgb.tune, newdata = prediction_true_test[,-1])
       
       # super fit ( what I trained on ) # green = actual # red = predicted
       if(file.exists("ssrvprfforcstrs_superfit.png")) unlink("srvprfforcstrs_superfit.png")
       png('srvprfforcstrs_superfit.png')
     # plot( seq_along(              prediction_train[,1]),           prediction_train[,1],  type = "l", col ="green" )
       plot(                       index(prediction_train),           prediction_train[,1],  type = "l", col ="green" )
       title("srvprfforcstrs_superfit")
     # lines(seq_along(xgb.predicted.prediction_train), xgb.predicted.prediction_train,      type = "l", col ="red"   )
       lines(                  index(prediction_train), xgb.predicted.prediction_train,      type = "l", col ="red"   )
       abline(h = 0)
       dev.off()


       # real world test                             # green = actual # red = predicted
       if(file.exists("srvprfforcstrs_realworldtest.png")) unlink("rvprfforcstrs_realworldtest.png")
       png('srvprfforcstrs_realworldtest.png')
     # plot( seq_along(              prediction_true_test[,1]),           prediction_true_test[,1],  type = "l", col ="green" )
       plot(                       index(prediction_true_test),           prediction_true_test[,1],  type = "l", col ="green" )
       title("rvprfforcstrs_realworldtest")
     # lines(seq_along(xgb.predicted.prediction_true_test), xgb.predicted.prediction_true_test,  type = "l", col ="red"   )
       lines(                  index(prediction_true_test), xgb.predicted.prediction_true_test,  type = "l", col ="red"   )
       abline(h = 0)
       dev.off()
       
       # tail 12 - last 3 years predictions
       
       # real world test ( last 12 obs ( 3 years ) ) # green = actual # red = predicted
       if(file.exists("srvprfforcstrs_realworldtest_last12obs.png")) unlink("rvprfforcstrs_realworldtest_last12obs.png")
       png('srvprfforcstrs_realworldtest_last12obs.png')
       last_12_obs_idx <- (NROW(prediction_true_test) - 11):NROW(prediction_true_test)
     # plot( seq_along(              prediction_true_test[last_12_obs_idx,1]),   prediction_true_test[last_12_obs_idx,1],  type = "l", col ="green" )
       plot(                    index(prediction_true_test)[last_12_obs_idx],    prediction_true_test[last_12_obs_idx,1],  type = "l", col ="green" )
       title("rvprfforcstrs_realworldtest_last12obs")
     # lines(seq_along(xgb.predicted.prediction_true_test[last_12_obs_idx]), xgb.predicted.prediction_true_test[last_12_obs_idx],    type = "l", col ="red"   )
       lines(                 index(prediction_true_test)[last_12_obs_idx],  xgb.predicted.prediction_true_test[last_12_obs_idx],    type = "l", col ="red"   )
       abline(h = 0)
       dev.off()
       
       dygraph( cbind.xts(prediction_true_test[,1], predicted = xgb.predicted.prediction_true_test), 
          ylab="GSPC Relative Pct Change", 
          main="Survey of Professional Forecasters")  %>% dyRangeSelector()
       
       prediction_true_test_predicted <- cbind.xts(prediction_true_test[,1], predicted = xgb.predicted.prediction_true_test)
       
       with( prediction_true_test_predicted, {  
         factor(ifelse(0 < predicted,               "Good", "Bad"), levels = c("Good", "Bad")) -> Predictuals
         factor(ifelse(0 < get(ls(sort = TRUE)[1]), "Good", "Bad"), levels = c("Good", "Bad")) -> Actuals
         data.frame(Predictuals,Actuals)
       }) -> prediction_true_test_predicted_diagram
       
       # DEBUGGING
       # print("")
       # print(prediction_true_test_predicted_diagram)
       
       writeLines("")
       print(table(prediction_true_test_predicted_diagram))
       
       writeLines("")
       writeLines("Total number of cases.")
       writeLines(as.character(NROW(prediction_true_test_predicted_diagram)))
       
       writeLines("")
       print(addmargins(prop.table(table(prediction_true_test_predicted_diagram))))
       
       print(confusionMatrix(table(prediction_true_test_predicted_diagram)))
       # caret::confusionMatrix
       
       bookmarkhere <- 1 # left_Off
           # 
       # fr <- xts(as.matrix(fr[,-1]), 
       #           as.Date(fr[,1],origin='1970-01-01'),
       #           src='srvprfforcstrs',updated=Sys.time())
       # dim(fr) <- c(NROW(fr),1)
       # colnames(fr) <- as.character(toupper(Symbols[[i]])) # I WILL NOT UPPER CASE?!
       # fr <- convert.time.series(fr=fr,return.class=return.class)
       # Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       # if(auto.assign)
       #   assign(Symbols[[i]],fr,env)
       
       dbDisconnect(con)
       Sys.setenv(TZ=oldtz)
       
       options(ops)
       
       # if(auto.assign)
       #   return(Symbols)
       # return(fr)
       
       # NOT WHAT REALLY WANT TO RETURN 
       # BUD GOOD ENOUGH FOR RIGHT  NOW
       list(prediction_train=prediction_train, prediction_true_test=prediction_true_test)
       
  } #}}}
  
  # WORKS
  # getSymbols.srvprfforcstrs(c("CPI","NGDP"), responses = c("median","mean"), pctchg = TRUE)
  # + more
  # getSymbols.srvprfforcstrs(c("CPI","NGDP","INDPROD","TBOND","UNEMP","RCONSUM"), responses = c("median","mean"), pctchg = TRUE)
  # + everything else
    getSymbols.srvprfforcstrs(c("CPI","NGDP","INDPROD","TBOND","UNEMP","RCONSUM","CPROF","RNRESIN","REXPORT","TBILL"), responses = c("median","mean"), pctchg = TRUE)
    # Play
  # getSymbols.srvprfforcstrs(c("CPROF","RNRESIN","REXPORT","TBILL"), responses = c("median","mean"), pctchg = TRUE)
  
  
}

# rm(list=ls(all.names= TRUE))
# debugSource(paste0(getwd(),"/","getSymbols.srvprfforcstrs.R"))
# 
# result <- getSymbols.srvprfforcstrs.test()

# TEMPORARY 
# print(head(result[['prediction_train']]))
# print(tail(result[['prediction_train']]))
# print(head(result[['prediction_true_test']]))
# print(tail(result[['prediction_true_test']]))
#                                  
#                                                        
            
