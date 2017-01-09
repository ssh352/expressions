

# finecon01.R   

if(exists("copyDirectoryByPattern.default")) suppressWarnings(rm("copyDirectoryByPattern.default"))

# it DOES not overwrite what is ALREADY there
# uses package R.methodsS3
# depends upon  DESCRIPTION Imports R.utils
R.methodsS3::setMethodS3("copyDirectoryByPattern", "default", function(from, to=".", ..., private=TRUE, recursive=TRUE
                                                                       , pattern = NULL, tolower = FALSE, collapse_to = FALSE) {
  # BACKWARD COMPATIBILITY: file.copy() gained argument copy.mode=TRUE in
  # R (>= 2.13.0) [April 2013].  Due to the default, this means that when
  # previously copying a read-only file, the new file would have write
  # permissions, whereas now it preserved the read-only permissions.
  # This private function silently drop argument 'copy.mode' and 'copy.date'
  # if passed older versions of R.
  .file.copy <- function(...) {
    args <- list(...)
    names <- names(args)
    if (!is.null(names)) {
      known <- names(formals(base::file.copy))
      keep <- (nchar(names) == 0L | is.element(names, known))
      args <- args[keep]
    }
    do.call(base::file.copy, args=args, envir=parent.frame())
  } # .file.copy()
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'from':
  if (!R.utils::isDirectory(from))
    throw("Argument 'from' is not a directory: ", from);
  
  # Argument 'to':
  to <- R.utils::Arguments$getWritablePath(to, mkdirs=TRUE, absolutePath=FALSE);
  
  # Argument 'private':
  private <- R.utils::Arguments$getLogical(private);
  
  # Argument 'recursive':
  recursive <- R.utils::Arguments$getLogical(recursive);
  
  # Use relative pathnames
  files <- list.files(from, all.files=private, pattern = pattern, full.names=FALSE);
  files <- files[!basename(files) %in% c(".", "..")];
  files <- file.path(from, files);
  
  copiedFiles <- c();
  for (file in files) {
    basename <- basename(file);
    if (R.utils::isFile(file)) {
      if (.file.copy(from=file, to=R.utils::filePath(to, if(!tolower) { basename } else { tolower(basename) } ), ...)) {
        copiedFiles <- c(copiedFiles, file);
      }
    } else if (R.utils::isDirectory(file)) {
      if (recursive) {
        copiedFiles <- c(copiedFiles,
                         copyDirectoryByPattern(file, to=R.utils::filePath(to, basename), ..., recursive=TRUE));
      }
    }
  }
  
  invisible(copiedFiles);
})


# uses copyDirectoryByPattern
copyAAIISIProDBFs <- function(from = "C:/Program Files (x86)/Stock Investor/Professional", to = "./Desination" ) {
  
  subdirs <- c("","/Dbfs","/User","/Static","/Temp","/Datadict")
  
  for(subdir in subdirs) {
    
    # it DOES not overwrite what is ALREADY there
    copyDirectoryByPattern(from = paste0(from, subdir)
                           , pattern = "(*\\.dbf$|\\.*DBF$|\\.*DBF$|*.chm$|ReadMe\\.txt)", to=to,  tolower = TRUE
    )
    
  }
  
}

# uses package foreign
# depends upon  DESCRIPTION Imports foreign
getAAIISIProDate <- function(from = "C:/Program Files (x86)/Stock Investor/Professional") {
  
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  setup_file <- suppressWarnings(suppressMessages(foreign::read.dbf(file=paste0(from,"/","Setup.dbf"), as.is = TRUE)))
  
  if(length(unique(
    as.integer(setup_file[,"MONTHDATE"])
    , as.integer(setup_file[,"WEEKDATE"])
    , as.integer(setup_file[,"SPLITDATE"])
  )) != 1) stop("MONTHDATE != WEEKDATE != SPLITDATE")
  
  new_repository_entry_name <- as.character(as.integer(setup_file[,"MONTHDATE"]))
  
  Sys.setenv(TZ=oldtz)
  
  return(new_repository_entry_name)
  
}



verify_connection <- function () {
  
  # R version 3.3.2 (2016-10-31) # sessionInfo()
  
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
  
  verify_connection_inner <- function () {
    
    # # Depends: DBI
    # require(RPostgreSQL)
    # 
    # if(!exists("drv", envir = .GlobalEnv)) { drv <<- dbDriver("PostgreSQL") }
    # if(!exists("con", envir = .GlobalEnv)) { 
    #   con <<- dbConnect(drv, user="postgres", password="postgres", dbname="finance_econ") # RPostgreSQL
    #   dbGetQuery(con, "set search_path to fe_data_store, public;")
    # }
    
    # Depends: Matrix
    # Suggests:	DBI, RPostgreS(on the fly loaded)
    require(PivotalR) #                                     # OSUser
    if(!exists("cid", envir = .GlobalEnv)) {
      cid <<- db.connect(user = "postgres", dbname = "finance_econ", default.schemas = c("fe_data_store","public"))
      con <<- PivotalR:::.localVars$db[[1]]$conn
    }
  }
  
  
  
  verify_connection_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
}
# verify_connection()


# uses verify_connection
verify_si_finecon_exists <- function () {
  
  # R version 3.3.2 (2016-10-31) # sessionInfo()
  
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
  
  verify_si_finecon_exists_inner <- function () {
    
    # create schema fe_data_store
    # authorization postgres;
    # 
    # comment on schema fe_data_store
    # is 'finance and economics data store';
    # 
    # show search_path;
    # --  "$user", public
    # 
    # set search_path to public;
    # 
    # -- if not already done throug some OTHER means
    # create extension intarray schema public;
    # create extension      plr schema public;
    # create extension quantile schema public;
    # create extension tablefunc schema public;
    # create extension  tinyint  schema public;
    # create extension unnest_ordinality  schema public;
    # create extension     weighted_stats schema public;
    # 
    # 
    # set search_path to fe_data_store, public;
    # 
    # 
    # select r_version();
    
    verify_connection()
    db("create table if not exists si_finecon2();", conn.id = cid)
    db("alter table si_finecon2 add if not exists dateindex    int;", conn.id = cid)
    db("alter table si_finecon2 add if not exists dateindexlwd int;", conn.id = cid)
    db("alter table si_finecon2 add if not exists dateindexeom int;", conn.id = cid)
    db("alter table si_finecon2 add if not exists company_id_orig  text;", conn.id = cid)
    db("alter table si_finecon2 add if not exists company_id  text;", conn.id = cid)
    db("alter table si_finecon2 add if not exists ticker  text;", conn.id = cid)
    db("alter table si_finecon2 add if not exists company text;", conn.id = cid)
    
    # create table if not exists si_finecon2();
    

  }
  verify_si_finecon_exists_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
}
# verify_si_finecon_exists()


getsetvar_aaii_sipro_dir <- function (new_dir = NULL) {
  
  # R version 3.3.2 (2016-10-31) # sessionInfo()
  
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
  
  getsetvar_aaii_sipro_dir_inner <- function () {
    if( is.null(new_dir) && !exists("aaii_sipro_dir")) aaii_sipro_dir <<- "W:/AAIISIProDBFs"
    if(!is.null(new_dir)                             ) aaii_sipro_dir <<- new_dir;           
    return(aaii_sipro_dir)
  }
  
  res <- getsetvar_aaii_sipro_dir_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  return(res)
}
# getsetvar_aaii_sipro_dir(new_dir = NULL)



# uses package xts
to.monthly.lwd <- function(x) {
  
  # require(xts)
  x <- xts::xts(zoo::coredata(x),zoo::as.Date(zoo::index(x))) # index MUST be of class Date ( xts::split.xts would not work CORRECTLY otherwise )
  # no weekends
  xwd <- x[!weekdays(zoo::index(x)) %in% c("Saturday","Sunday")]
  # prepare to process by time division
  xwd_l <- xts::split.xts( xwd, f = "months" )
  xwd_l_last <- lapply( xwd_l , function(x) { xts::last(x) } ) # xts::last remaining of all the elements
  # unsplit
  ret <- do.call(xts::rbind.xts, xwd_l_last )
  return(ret)
}

# # Thursday
# zoo::as.Date("2016-12-29")
# [1] "2016-12-29"
# as.integer(zoo::as.Date("2016-12-29"))
# [1] 17164

lwd_of_month <- function(anyday = NULL) {  
  #
  # uses # to.monthly.lwd
  # uses package zoo
  #
  seq(from = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday)), frac = 0)),
      to = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday)), frac = 1)),
      by = 1) -> all_month_days
  as.integer(zoo::index(to.monthly.lwd(xts::xts(rep(NA_real_,length(all_month_days)),zoo::as.Date(all_month_days)))))
}
# lwd_of_month(17164)
# [1] 17165

last_day_of_month <- function(anyday = NULL) {  
  # uses package zoo
  as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday)), frac = 1))
}
#  last_day_of_month(17164)
# [1] 17166




# uses to.monthly.lwd, getsetvar_aaii_sipro_dir, package zoo
getvar_all_load_days_lwd <- function () {
  
  # R version 3.3.2 (2016-10-31) # sessionInfo()
  
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
  
  getvar_all_load_days_lwd_inner <- function () {
    
    as.integer(dir(getsetvar_aaii_sipro_dir())) -> all_load_days
    
    # search range
    min(all_load_days) -> earliest_load_day
    max(all_load_days) -> latest_load_day
    seq(from = earliest_load_day, to = latest_load_day, by = 1) -> range_load_days
    as.integer(zoo::index(to.monthly.lwd(xts::xts(rep(NA_real_,length(range_load_days)),zoo::as.Date(range_load_days))))) -> range_load_days_lwd
    
    # just my lwd in the ranage of lwd
    all_load_days[which(all_load_days %in% range_load_days_lwd)] -> all_load_days_lwd
    
    return(all_load_days_lwd)
    
  }
  ret <- getvar_all_load_days_lwd_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
}
# getvar_all_load_days_lwd()



optimize <- function(tb = NULL, colz = c("dateindex","company_id")) {
  
  # require(data.table)
  # require(dtplyr)        # MUST HAVE data.table
  # require(dplyr)         # MUST HAVE
  # require(bsearchtools)  # DOES NOT DO ARRAYS
  
  data.table::data.table(tb) -> tb
  dtplyr::tbl_dt(tb, copy = FALSE) -> tb
  data.table::setkeyv(tb, colz)
  bsearchtools::DFI(tb,colz) -> tb
  
  return(tb)
}

# airquality -> aq
# aq$company_id <- NA_character_
# aq$dateindex <- NA_integer_
# optimize(aq) -> aq



# version 2 meant for real world data

# takes in a data.frame or a matrix 
#   composed of columns of characters, logicals, integers, numerics, and Dates 
#   but if take in a matrix the 'row names' are expected to be unique
# output is a data.frame that is AAII-ized # financize(df, round_to_decimal_places=1)
# and PostgreSQL-ized
# CURRENTLY, DOES NOT convert row.names to a column VALUE ( POSSIBLE TO DO )
# SEE? data.table

# if char_col_to_numeric = TRUE than those column names that match the pattern *char_col_rexpr*
#   are tried
#     1. as.numeric(x)
#     2. round(x, digits=round_to_decimal_places)
#     3. x[char_col_numeric_limit < x] <- NA_real_ 

financize <- function(df
                      , int_col_rexpr = "^perlen_q.*$"
                      , stringsAsFactors = FALSE       # untested # most likely upsiszed to a database to be an integer?
                      , char_col_rexpr = "^pertyp_q.*$"
                      , num_col_rexpr = "price|mktcap|^.*_q.*$"
                      , round_to_decimal_places = 1
                      , char_col_numeric_limit = 999999.9 # PostgreSQL # exact(actually an integer) # numeric(7,1) # SHOULD fit MOST aaii sipro data
) {
  
  ops <- options()
  options(warn=1)
  
  # uses # xts::xtsible, xts::xts
  
  require(stringi) # stri_flatten
  require(stringr) # str_detect
  
  
  col__names <- colnames(df)
  df_local <- df
  
  # non-xts row.names
  row__names <- rownames(df) # NULL if an xts object
  #     xts row.names
  if(xts::xtsible(xts::xts(,try({ zoo::as.Date(row.names(data.frame(df, stringsAsFactors = FALSE)))}, silent = TRUE)))) {
    row__names <- row.names(data.frame(df, stringsAsFactors = FALSE))
  }
  
  col__names_iter <- 0
  data.frame(
    lapply( as.data.frame(df, stringsAsFactors = FALSE), #head(diamonds), 
            function(x) { 
              print(str_c("Begin: ",col__names[col__names_iter], collapse = " "))
              col__names_iter <<- col__names_iter + 1
              if(xts::xtsible(x)) { # detects Date 
                return(as.integer(x)) # convert Dates to integer
              }
              if(is.integer(x)) {
                return(x)
              }
              if(is.logical(x)) {
                return(as.integer(x)) # TRUE ->  1, FALSE -> 0  # On PostgreSQL ... final .. smallint (or by extension: tinyint)
              }
              if(str_detect(col__names[col__names_iter], int_col_rexpr)) return(as.integer(x))
              #
              if(is.factor(x)) { # package foreign DOES not change characters to factors
                if(stringsAsFactors == FALSE) return(as.character(x))
              }
              if(str_detect(col__names[col__names_iter], char_col_rexpr)) return(as.character(x))
              #
              # ONLY characters are REMAINING
              if(str_detect(col__names[col__names_iter], num_col_rexpr)) {
                out <- try( { as.numeric(x) }, silent = TRUE)
                if ( !"try-error" %in% class(out))  { 
                  x <- out 
                } else { 
                  warning(stri_c("numeric(x) conversion failed for column: " %s+% col__names[col__names_iter])) 
                  return(x)
                }
                if(!is.null(round_to_decimal_places)) {
                  out <- try( { round(x, digits=round_to_decimal_places)  }, silent = TRUE)
                  if ( !"try-error" %in% class(out)) { 
                    x <- out 
                  } else { 
                    warning(stri_c("round(x, digits=round_to_decimal_places) conversion failed for column: " %s+% col__names[col__names_iter])) 
                    return(x)
                  }
                }
                if(!is.null(char_col_numeric_limit)) {
                  over_the_limit_tf <- {char_col_numeric_limit < x}
                  print(stringi::stri_c("  Note, these many NAs found in x: " %s+% sum(is.na(x)), ignore_null = TRUE))
                  if(any(over_the_limit_tf, na.rm = TRUE)) {                           #  NROW(x[!is.na(x)][x[!is.na(x)] > char_col_numeric_limit]) # SAME
                    warning(stri_c("  Note, these many OVER THE LIMIT found in x: " %s+% sum(over_the_limit_tf, na.rm = TRUE), ignore_null = TRUE))
                    print(stri_c("over_the_limit_tf <- x[" %s+% char_col_numeric_limit %s+% " < x] records found for column: " %s+% col__names[col__names_iter]))
                    print(stri_c("Printing those " %s+% sum(over_the_limit_tf, na.rm = TRUE) %s+% " (column_ids)(if any) records Now."))
                    if("company_id" %in% col__names) { print(cbind(df[,"company_id",drop = FALSE],x = x)[!is.na(x) &  { x > 999999.9 },,drop = FALSE])  }
                    out <- try( { x[char_col_numeric_limit < x] <- NA_real_ ; x }, silent = TRUE)
                    if ( !"try-error" %in% class(out)) { 
                      print(stri_c("  SUCCESS for ... over_the_limit_tf <- x[char_col_numeric_limit < x] records found for column: " %s+% col__names[col__names_iter]))
                      x <- out 
                    } else { 
                      warning(stri_c("Conversion ACTUALLY failed for ... x[char_col_numeric_limit < x] <- NA_real_ conversion failed for column: " %s+% col__names[col__names_iter])) 
                      return(x)
                    }
                  }
                }
              }
              print(str_c("End: ",col__names[col__names_iter], collapse = " "))
              return(x) # all done
            } 
    )
    , stringsAsFactors = stringsAsFactors) -> new_df
  row.names(new_df) <- row__names
  options(ops)
  return(new_df)
}

# si_si_bsq_df <- list()
# si_si_bsq_df$x <- foreign::read.dbf(file = "W:/AAIISIProDBFs/15764/si_bsq.dbf", as.is = TRUE)
# colnames(si_si_bsq_df$x) <- tolower(colnames(si_si_bsq_df$x))
# financize(si_si_bsq_df$x) ->  output_df_x 
# str(output_df_x , list.len = 9999)
# str(si_si_bsq_df$x. list.len = 9999)
# View (output_df_x)




verify_company_basics <- function (dateindex = NULL) {
  
  # R version 3.3.2 (2016-10-31) # sessionInfo()
  
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
  
  verify_company_basics_inner <- function (dateindex = NULL) {
    
    verify_si_finecon_exists()
    
    require(magrittr)
    
    require(stringi)
    require(stringr)
    
    # uses # DataCombine::MoveFront
    # uses plyr::join
    
    # uses last_day_of_month
    # uses lwd_day_of_month
    
    # run once
    getvar_all_load_days_lwd_var <- getvar_all_load_days_lwd()
    
    ## LEFT_OFF
    bm <- 1
    
    # some dateindexs in arg not found on disk
    if(any(!dateindex %in% getvar_all_load_days_lwd_var)) { 
      dateindex[!dateindex %in% getvar_all_load_days_lwd_var] -> dateindexes_not_found_on_disk
      warning("one/some arg dateindex not found on disk" %s+% str_c(dateindexes_not_found_on_disk, collapse = "") )
    }
    
    # getsetvar_aaii_sipro_dir()
    if(!any(dateindex %in% getvar_all_load_days_lwd_var)) stop("no arg dateindex was found on disk")
    
    # just the ones on found on disk    
    # spec date(not all dates) IN 'all disk possible aaii si_pro last weekday of the month'
    dateindex_redux <- dateindex[dateindex %in% getvar_all_load_days_lwd_var]
    
    # at least one
    for(dateindex_redux_i in dateindex_redux) { 
    
      # # save for later
      # c("setup","si_ci","si_exchg","si_mgdsc",
      #   "si_bsq","si_isq","si_cfq",
      #   "si_date","si_psd","si_psdc","si_psdd",
      #   "si_mlt","si_rat",
      #   "si_ee")
      
      si_tbl <- c("si_ci","si_exchg","si_mgdsc")
      for(si_tbl_i in si_tbl) {
      
        ## always load
        paste0(getsetvar_aaii_sipro_dir(),"/",dateindex_redux_i,"/") -> part_path_file_name
        si_si_tbl_df <- suppressWarnings(suppressMessages(foreign::read.dbf(file = paste0(part_path_file_name,si_tbl_i,".dbf"), as.is = TRUE)))
        
        # all lower ( PostgreSQL friendly )
        colnames(si_si_tbl_df) <- tolower(colnames(si_si_tbl_df))
        
        # remove useless columns
        # nothing starts with 'x'
        # x_nullflags, x., x, x.1, x.2 ...
        # 
        si_si_tbl_df[, !str_detect(colnames(si_si_tbl_df),"^x\\.?+")   & 
                       !str_detect(colnames(si_si_tbl_df),"^repno$")   & 
                       !str_detect(colnames(si_si_tbl_df),"^lastmod$") &
                       !str_detect(colnames(si_si_tbl_df),"^updated$")
        , drop = FALSE] -> si_si_tbl_df
        
        # unique ids
        
        if(si_tbl_i == "si_ci") {

          within( si_si_tbl_df, { assign("dateindexeom", rep(last_day_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexeom")    -> si_si_tbl_df
          
          within( si_si_tbl_df, { assign("dateindexlwd", rep(     lwd_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexlwd") -> si_si_tbl_df
          
        }
        
        within( si_si_tbl_df, { assign("dateindex", rep(as.integer(dateindex_redux_i),NROW(si_si_tbl_df[,1]))      )  } ) -> si_si_tbl_df
        DataCombine::MoveFront(si_si_tbl_df,   "dateindex")                                  -> si_si_tbl_df
        
        within( si_si_tbl_df, { assign("rn_" %s+% si_tbl_i, seq_along(si_si_tbl_df[,1])) } ) -> si_si_tbl_df
        DataCombine::MoveFront(si_si_tbl_df,   "rn_" %s+% si_tbl_i)                          -> si_si_tbl_df
        
        # remove duplicated company_id, ticker ( will NOT be loaded into the PostgreSQL database)
        
        # "si_exchg","si_mgdsc" will NOT HAVE
        if(any(colnames(si_si_tbl_df) %in% "company_id")) {
          si_si_tbl_df[
            !with(si_si_tbl_df, { company_id == "" | is.na(company_id) | is.null(company_id) | stri_duplicated(company_id) | stri_duplicated(company_id, fromLast = TRUE) 
          }),,drop = FALSE] -> si_si_tbl_df
        }
        
        # extra for si_ci
        if(any(colnames(si_si_tbl_df) %in% "ticker")) {
          si_si_tbl_df[
            !with(si_si_tbl_df, { ticker     == "" | is.na(ticker)     | is.null(ticker)     | stri_duplicated(ticker)     | stri_duplicated(ticker    , fromLast = TRUE) 
            }),,drop = FALSE] -> si_si_tbl_df
        }
          
        # speed

        # keys, DFIs, and more DFI keys: ... speed
        if(any(colnames(si_si_tbl_df) %in% "company_id")) optimize(si_si_tbl_df)               -> si_si_tbl_df
        if(any(colnames(si_si_tbl_df) %in% "exchg_code")) optimize(si_si_tbl_df, "exchg_code") -> si_si_tbl_df
        if(any(colnames(si_si_tbl_df) %in% "mg_code"))    optimize(si_si_tbl_df, "mg_code")     -> si_si_tbl_df
          
        # si_TBL VARIABLES
        assign(si_tbl_i,si_si_tbl_df)
        rm(si_si_tbl_df)
        
      }
      
      # join key of exchange
      si_exchg$x$exchg_code  -> si_exchg$x$exchange
      
      # outer join
      plyr::join_all(list(si_ci$x,si_exchg$x), by = c("dateindex","exchange"), type = "full") -> si_all_df
      
      optimize(si_all_df) -> si_all_df
      
      rm(si_ci)
      rm(si_exchg)
      
      # join key of industry
      si_all_df$x$ind_3_dig -> si_all_df$x$industry_code
       si_mgdsc$x$mg_code  ->   si_mgdsc$x$industry_code
      
      # left join because mg_code has codes that are sectors and not industries ( creates orphans )
      # left join becuase orphan industries are meaningless
      plyr::join_all(list(si_all_df$x,si_mgdsc$x), by = c("dateindex","industry_code"), type = "left") -> si_all_df
      si_all_df$mg_desc -> si_all_df$industry_desc 
      within( si_all_df, { rm("mg_code","mg_desc") }) -> si_all_df
      si_all_df$rn_si_mgdsc -> si_all_df$rn_si_mgdsc_ind; within(si_all_df, { rm("rn_si_mgdsc") } ) -> si_all_df 
      within(  si_mgdsc$x , { rm("industry_code") }) -> si_mgdsc$x

      optimize(si_all_df) -> si_all_df
      
      # join key of sector
      si_all_df$x$ind_2_dig            ->  si_all_df$x$sector_code
      str_sub(si_mgdsc$x$mg_code,1,2)  ->  si_mgdsc$x$sector_code
      
      # get rid of duplicated sectors ( each industry has its sector rementioned )
      si_mgdsc$x -> si_mgdsc  
      # SPECIAL
      si_mgdsc$rn_si_mgdsc -> si_mgdsc$rn_si_mgdsc_sect
      si_mgdsc[with( si_mgdsc, { !stri_duplicated(sector_code) } ),,drop = FALSE] -> si_mgdsc
        
      optimize(si_mgdsc,c("mg_code")) -> si_mgdsc
      
      # left join becuase orphan sectors are meaningless
      plyr::join_all(list(si_all_df$x,si_mgdsc$x), by = c("dateindex","sector_code"), type = "left")  -> si_all_df
      si_all_df$mg_desc -> si_all_df$sector_desc 
      within( si_all_df, { rm("mg_code","mg_desc") }) -> si_all_df
      # SPECIAL 
      within(si_all_df, { rm("rn_si_mgdsc") } ) -> si_all_df
      within(  si_mgdsc$x , { rm("sector_code") }) -> si_mgdsc$x

      optimize(si_all_df) -> si_all_df
      
      rm("si_mgdsc")
      
      bm <- 1 # LEFT_OFF 
              # financize-like TRIM data.types ( and load into the database)
              # ( Caroline-like & SQL: load into the database)
      
      # LEFT_OFF
      
      # look forward to NEXT month and get any NEW COMPANY_IDS by TICKER then company_id by STREET
      

      # WORKS
      # colnames(si_ci)[match("EXCHANGE",colnames(si_ci))] <- "EXCHG_CODE"
      # plyr::join_all(list(si_ci,si_exchg), type = "full") 
      
      # need industry & sectors
      
      # next program ( on demand load)
    } 
    
    
  }
  verify_company_basics_inner(dateindex = dateindex)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
}
# verify_company_basics(dateindex = NULL)
# verify_company_basics(dateindex = c(15764))





finecon01 <- function () {
  
  # R version 3.3.2 (2016-10-31) # sessionInfo()
  
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
  
  finecon01_inner <- function () {
    
  }
  finecon01_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
}
# finecon01()


##  rm(list=ls(all.names=TRUE))
## 
# rm(list=setdiff(ls(all.names=TRUE),c("con","cid")))
# debugSource(paste0(getwd(),'/finecon01.R'))
# PLACE BREAKPOINT
# verify_company_basics(dateindex = c(15764))
# .... finecon01()

# after downloading the 'end of the last friday of the month'
# http://www.aaii.com/stock-investor-pro/archives
# and installing

# per END_OF_MONTH
#
# ** CAN be a Postgresql function call **
# will CREATE the target directory
# copyAAIISIProDBFs(
#    from = "C:/Program Files (x86)/Stock Investor/Professional"
#    , to   = paste0("W:/AAIISIProDBFs/",getAAIISIProDate()) # 
#  )                 
 
 


  
