

#LEFT_OFF see LEFT_OFF.txt unnest(array(...

# finecon01.R   

# QUANTILE FIX ( for pl/r)
# 71906
# committed as  71906  (plus an example)
# Bug 16672 - quantile produces decreasing output
# https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16672
# https://bugs.r-project.org/bugzilla3/attachment.cgi?id=2045&action=diff
# https://bugs.r-project.org/bugzilla3/attachment.cgi?id=2045&action=diff&collapsed=&headers=1&format=raw
# FEB 20, 2017 R-DEV ( MEANT FOR R_NEXT 3.3.3 )
# https://svn.r-project.org/R/trunk/src/library/stats/R/quantile.R
# REPLACE
# stats:::quantile.default


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


# TRY running this EXACTLY - WORKS
# copyAAIISIProDBFs(
#     from = "C:/Program Files (x86)/Stock Investor/Professional"
#   , to   = paste0("W:/AAIISIProDBFs","/",getAAIISIProDate())  
# )



# insert at a postion a new column into a data.frame
# --------------------------------------------------

# insert after POSITION pos
#  df 
# val: vector of values
#  nm: new name in the data.frame
# pos: new position in the data.frame 
insert_df <- function(df = NULL, val = NULL, nm = NULL, pos = 0 ) {
  
  require(magrittr)
  # uses plyr::rename
  
  # put in the last column
  cbind(df[,,drop=FALSE],END = val, stringsAsFactors = FALSE) %>%  
    # change the column order
    .[,append(colnames(.)[-NCOL(.)],"END",pos),drop=FALSE] %>%
    # rename END to desired name (nm)
    { plyr::rename(., c("END" = nm)) } -> ret
  return(ret)
  
}

# insert_df(iris[1:26,],letters,"Let",3)
# DataCombine::MoveFront
# insert_df(iris[1:26,],letters,"Let",0)
##   Let Sepal.Length Sepal.Width Petal.Length Petal.Width Species
##1    a          5.1         3.5          1.4         0.2  setosa
##2    b          4.9         3.0          1.4         0.2  setosa

#   Sepal.Length Sepal.Width Petal.Length Let Petal.Width Species
##1           5.1         3.5          1.4   a         0.2  setosa
##2           4.9         3.0          1.4   b         0.2  setosa

# ANDRE


is_connected_postgresql_con <- function() {  

  tryCatch({  res <- postgresqlExecStatement(get("con", envir = .GlobalEnv), "select 1; ")
                 postgresqlFetch(res)
                 TRUE 
            }
            , error = function(e) { FALSE } )

}
# How to use dbGetQuery in tryCatch with PostgreSQL?
# http://stackoverflow.com/questions/34332769/how-to-use-dbgetquery-in-trycatch-with-postgresql



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
    
    # set search_path to fe_data_store,public;
    # set time zone 'utc';
    # set work_mem to '2047MB';
    # set constraint_exclusion = on;
    # set max_parallel_workers_per_gather to 4;
    
    
    # How to use dbGetQuery in tryCatch with PostgreSQL?
    # http://stackoverflow.com/questions/34332769/how-to-use-dbgetquery-in-trycatch-with-postgresql
    
    #
    require(PivotalR) #                                     # OSUser
    require(stringr)
    if(!exists("cid", envir = .GlobalEnv) || 
       !exists("con", envir = .GlobalEnv) || 
       !is_connected_postgresql_con() 
    ) {
      cid <<- db.connect(user = "postgres", dbname = "finance_econ", default.schemas = "fe_data_store,public")
      # increments up by 1 every time
      con <<- PivotalR:::.localVars$db[[length(PivotalR:::.localVars$db)]]$conn
      
      if(!is_connected_postgresql_con()){
        stop("PostgreSQL database server is not responding.  Is it up/blocked?  Are the client login credentials valid?")
      }
      
      # set search_path to fe_data_store,public;
      # set time zone 'utc';
      # set work_mem to '2047MB';
      # set constraint_exclusion = on;
      # set max_parallel_workers_per_gather to 4;
      
      db.q(str_c("set time zone 'utc';"), nrows =  -1, conn.id = cid)
      # windows LIMIT
      db.q(str_c("set work_mem to '2047MB';"), nrows =  -1, conn.id = cid)
      db.q(str_c("set constraint_exclusion = on;"), nrows =  -1, conn.id = cid)
      # postgresql 9.6
      db.q(str_c("set max_parallel_workers_per_gather to 4;"), nrows =  -1, conn.id = cid)
      
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
    
    # create table if not exists si_finecon2();
    
    verify_connection()
    db.q("create table if not exists si_finecon2();", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindex_company_id_orig text;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindex_company_id text;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindex    int;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindexlwd int;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindexeom int;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists company_id_orig  text;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists company_id  text;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists ticker  text;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists company text;", conn.id = cid)
    # 
    # db.q("create unique index if not exists si_finecon2_dateindex_company_id_both_unqpkidx on si_finecon2(dateindex_company_id);", conn.id = cid)
    # db.q("alter table si_finecon2 add primary key(dateindex_company_id) using index si_finecon2_dateindex_company_id_both_unqpkidx;", conn.id = cid)
    # 
    # db.q("create unique index if not exists si_fincon2_dateindex_company_id_unqkey on si_finecon2(dateindex_company_id);", conn.id = cid)
    # db.q("alter table si_finecon2 drop constraint        if exists si_fincon2_dateindex_company_id_unqkey;", conn.id = cid)
    # db.q("alter table si_finecon2 add  constraint unique(dateindex, company_id) using si_fincon2_dateindex_company_id_unqkey;", conn.id = cid)
    # 
    
    # db.q("drop index if exists si_fincon2_dateindex_company_id_unqidx;", conn.id = cid)
    # db.q("create unique index si_fincon2_dateindex_company_id_unqidx on si_finecon2(dateindex, company_id);", conn.id = cid)
    
    # Re: Are Indices automatically generated for primary keys?
    # PostgreSQL automatically creates an index for each unique constraint and primary key constraint to enforce uniqueness.
    # https://www.postgresql.org/message-id/4C6BA0F6020000250003481C@gw.wicourts.gov
    
    

    try( { db.q("create unique index if not exists si_finecon2_dateindex_company_id_orig_both_key      on si_finecon2(dateindex_company_id_orig);", conn.id = cid) }, silent = TRUE )
    
    # just SIMPLY
    # will ERROR OUT ( will not allow to add a second primary key )
    try( { db.q("alter table if exists si_finecon2 add primary key(dateindex_company_id );", conn.id = cid) }, silent = TRUE ) # only be on
    # si_finecon2_pkey
    # singleton

    # try( { db.q("alter table if exists si_finecon2 add      unique(dateindex, company_id);", conn.id = cid) }, silent = TRUE )
    # si_finecon2_dateindex_company_id_key
    # can be many
    
    # (if unamed ndex?) ... WILL JUST KEEP ADDING MORE ... so I name it
    try( { db.q("create unique index if not exists si_finecon2_dateindex_company_id_key       on si_finecon2(dateindex, company_id);", conn.id = cid) }, silent = TRUE )
    # can be many
    try( { db.q("create unique index if not exists si_finecon2_dateindex_company_id_orig_key  on si_finecon2(dateindex, company_id_orig);", conn.id = cid) }, silent = TRUE )
    # can be many

    # PROTECT against a LOSS of INTEGRITY
    try( { db.q("create unique index if not exists si_finecon2_dateindex_ticker_id_key        on si_finecon2(dateindex, ticker);", conn.id = cid) }, silent = TRUE )
    
    try( { db.q("
      
      -- drop function if exists si_finecon2_bef_row_ins_upd();
      create or replace function si_finecon2_bef_row_ins_upd() returns trigger as 
      $body$
          begin
              new.dateindex_company_id := new.dateindex || '_' || new.company_id;
              return new;
          end;
      $body$ 
      language plpgsql;
      
      
      drop trigger if exists si_finecon2_bef_row_ins_upd on si_finecon2;
      create trigger si_finecon2_bef_row_ins_upd before insert or update of dateindex, company_id on si_finecon2
          for each row 
          execute procedure si_finecon2_bef_row_ins_upd();
      
    ", conn.id = cid) }, silent = TRUE )

    
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



# lwd_of_month <- function(anyday = NULL) {  
#   #
#   # uses # to.monthly.lwd
#   # uses package zoo
#   #
#   seq(from = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday)), frac = 0)),
#       to = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday)), frac = 1)),
#       by = 1) -> all_month_days
#   as.integer(zoo::index(to.monthly.lwd(xts::xts(rep(NA_real_,length(all_month_days)),zoo::as.Date(all_month_days)))))
# }
# # lwd_of_month(17164)
# # [1] 17165



# vectorized
lwd_of_month <- function(anyday = NULL) {  
  #
  # uses # to.monthly.lwd
  # uses package zoo
  #
  logical() -> result
  for(anyday_i in anyday) {
    seq(from = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday_i)), frac = 0)),
        to = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday_i)), frac = 1)),
        by = 1) -> all_month_days
    as.integer(zoo::index(to.monthly.lwd(xts::xts(rep(NA_real_,length(all_month_days)),zoo::as.Date(all_month_days))))) -> result_i
    c(result, result_i) -> result
  }
  return(result)
    
}
# lwd_of_month(c(17164, 17165, 17166))
# [1] 17165 17165 17165



is_lwd_of_month <- function(anyday = NULL) { 
  # uses lwd_of_month
  return(anyday == lwd_of_month(anyday)) 

}
# is_lwd_of_month(c(17164, 17165, 17166))
# [1] FALSE  TRUE FALSE



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
                      , int_col_rexpr = "sic|employees|^perlen_q.*$"
                      , stringsAsFactors = FALSE       # untested # most likely upsiszed to a database to be an integer?
                      , char_col_rexpr = "^pertyp_q.*$"
                      # , num_col_rexpr = "price|mktcap|^.*_q.*$"
                      , num_col_rexpr = "price|mktcap|^.*_q.*$|^prchg_\\d\\dw$"
                      , round_to_decimal_places  = 2
                                               # 8,2
                      , char_col_numeric_limit = 999999.99 # PostgreSQL # exact(actually an integer) # numeric(8,2) # SHOULD fit MOST aaii sipro data
) {                     # also applies to is.numeric general check
  
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
              # ONLY characters are REMAINING from SIPRO dbfS ( but not in DERIVED DATA, see BELOW )
              # 
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
                  print(stringi::stri_c("  Note, these many NA_real_s found in x: " %s+% sum(is.na(x)), ignore_null = TRUE))
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
              # 
              # BUT (anonymous) derived DATA ( e.g weekly returns )
              # e.g. calculated weekly returns (and not sent to this function call's actual params regex names )
              #   then the column values may be NUMERIC
              # 
              if(is.numeric(x)) {
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
                  print(stringi::stri_c("  Note, these many NA_real_s found in x: " %s+% sum(is.na(x)), ignore_null = TRUE))
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

# by df cols [names], removes any rows with bad values or duplicates
# lazy cols
rm_df_dups <- function(df = NULL, cols = NULL) {
  
  # uses stringi::stri_duplicated, stringi::stri_c
  ops <- options()
  options(warn=1)
  
  if(any(colnames(df) %in% cols)) {
    for(col_i in cols) {            # optimistic # user entered only columns that are really there
      if(col_i %in% colnames(df)) { # realistic  # user entered more columns that are really there
        df[
          !with(df[,,drop = FALSE], {   get(col_i) == ""             | 
                                  is.na(get(col_i))                  | 
                                is.null(get(col_i))                  | 
               stringi::stri_duplicated(get(col_i))                  | 
               stringi::stri_duplicated(get(col_i), fromLast = TRUE) 
          }),,drop = FALSE] -> df
      } else {
        warning(stringi::stri_c("In the call to rm_df_dups, parameter element cols[i] '", col_i, "' was entered that does not exist in df"))
      }
    }
  } else {
    warning(stringi::stri_c("In the call to rm_df_dups, no parameter element cols[i] was entered that exists in df"))
  }
  options(ops)
  return(df[,,drop = FALSE])
} 
# rm_df_dups(df,c("column_1","column_2"))
#
# rm_df_dups(rbind(iris,iris),c("Species"))
## [1] Sepal.Length Sepal.Width  Petal.Length Petal.Width  Species
## <0 rows> (or 0-length row.names)
# rm_df_dups(rbind(iris,iris),c("SpeciesX")) -> RESULT; NROW(RESULT) == 300 # TRUE
## Warning message:
##   In rm_df_dups(rbind(iris, iris), c("SpeciesX")) :
##   In the call to rm_df_dups, no parameter element cols[i] was entered that exists in df
# rm_df_dups(rbind(iris,iris),c("SpeciesX","Species"))
## <0 rows> (or 0-length row.names)
## Warning message:
##   In rm_df_dups(rbind(iris, iris), c("SpeciesX", "Species")) :
##   In the call to rm_df_dups, parameter element cols[i] 'SpeciesX' was entered that does not exist in df


lcase_a_remove_useless_columns <- function(df) {

  require(stringr)

  # all lower ( PostgreSQL friendly )
  colnames(df) <- tolower(colnames(df))
  
  # remove useless columns
  # nothing starts with 'x'
  # x_nullflags, x., x, x.1, x.2 ...
  # 
  df[, !str_detect(colnames(df),"^x\\.?+")   & 
                 !str_detect(colnames(df),"^repno$")   & 
                 !str_detect(colnames(df),"^lastmod$") &
                 !str_detect(colnames(df),"^updated$") &
                 !str_detect(colnames(df),"^business$|^analyst_fn$") # UNTESTED - si_ci binary garbage
  , drop = FALSE] -> df
  
  return(df)

}

# not used ( but works )
get_db_data_types <- function(name = NULL) {

  require(stringr) #  0 = 1 normally returnes a zero column data.frame
  rs   <- dbSendQuery(con,str_c("select * from ",name," where 0 = 1;"))
  info <- dbColumnInfo(rs)
  dbClearResult(rs)
  return(info)
}


# # AFTER I GET 'mktcap' LOADED
# jamesos [ ] newer one
# -- drop index sipro_data_store.si_finecon_jamesos_partial_idx3;
# CREATE INDEX si_finecon_jamesos_partial_idx3
#   ON sipro_data_store.si_finecon
#   USING btree
#   (adr, exchange COLLATE pg_catalog."default", mktcap COLLATE pg_catalog."default", company_id_unq COLLATE pg_catalog."default", company COLLATE pg_catalog."default")
#   WHERE adr = false AND exchange <> 'O'::text AND mktcap::numeric(15,2) >= 200.00 AND ((company !~~ '%iShares%'::text) AND (company !~~ '%Vanguard%'::text) AND (company !~~ 'SPDR'::text) AND (company !~~ '%PowerShares%'::text) AND (company !~~ '%Fund%'::text))  
#   AND (company !~~ '%Holding%'::text) AND (mg_desc_ind !~~ '%Investment Service%'::text)
#
#  
#  

verify_finecon_jamesos_partial_idx <- function() {

  require(RPostgreSQL)
  require(PivotalR)
  
  # uses verify_connection, verify_si_finecon_exists
  
  verify_connection()
  
  # verify that si_finecon2 exists 
  if(!dbExistsTable(con, "si_finecon2")) {
    verify_si_finecon_exists()
  }

  # to what is in the database
  db.data.frame("si_finecon2", conn.id = cid, verbose = FALSE) -> ptr_si_finecon2  # class (db.#)
  col.types(ptr_si_finecon2) -> fc_meta # NOT USED
      names(ptr_si_finecon2) -> names(fc_meta) 
      
  if(all(c("dateindex","adr","exchange","mktcap","industry_desc","company") %in% names(fc_meta))) {
  
    # create index if not exists
    # NOTE: the ORIGINAL did NOT include 'dateindex' ( I am adding that here ... )
    #  Note: BEST performance table is sorted by : sort by dateindex, company_id
    # 
    #

    db.q("create index if not exists si_finecon2_finecon_jamesos_partial_idx on 
                  si_finecon2(dateindex,adr,exchange,mktcap,industry_desc,company) where 
                    adr = 0 and exchange != 'O' and mktcap > 200.0 
                    and (company !~~ '%iShares%') and (company !~~ '%Vanguard%') and (company !~~ 'SPDR') 
                    and (company !~~ '%PowerShares%') and (company !~~ '%Fund%') 
                    and (company !~~ '%Holding%') and (industry_desc !~~ '%Investment Service%')
              ;", conn.id = cid)
    
    
    # save space ( if nothing else )
    db.q("create or replace view si_finecon2_jos as select * from  
                  si_finecon2                                                      where 
                    adr = 0 and exchange != 'O' and mktcap > 200.0 
                    and (company !~~ '%iShares%') and (company !~~ '%Vanguard%') and (company !~~ 'SPDR') 
                    and (company !~~ '%PowerShares%') and (company !~~ '%Fund%') 
                    and (company !~~ '%Holding%') and (industry_desc !~~ '%Investment Service%')
              ;", conn.id = cid)
  
  }
      
   
}
# verify_finecon_jamesos_partial_idx() 



# remove at the beginning 
# debug at <text>#30: .base_paste0 <- base::paste0\n\n  
# stop the rstudio debugger OUTPUT from going inside the string
clean_text <- function(x) { 
  require(stringi)
  require(stringr)
  stri_split_lines1(x) -> lines
  str_c(lines[!str_detect(lines,"^debug at <text>")],collapse = "\n") 
}
# writeLines(clean_text(my_string))



## assumes(uses) that input value(data.frame) ALREADY has a column called dateindex
##   (future) SHOULD BE VECTORIZED: INPUT MANY 'values's (data.frames)
# NOTE: keys MUST be entered in lowercase
upsert <-  function(value = NULL, keys = NULL) { # vector of primary key values

  require(magrittr)
  require(RPostgreSQL)
  require(PivotalR)
  require(stringi)
  require(stringr)
  require(R.rsp)
  
  # uses clean_text
  
  options() -> ops
  options(max.print = 10000)
  # RStudio: Options->Code->Dislplay: Console; Limit length of lines displayed in console to: 0
  options(warn=1)
  
  # loading forward ( and backward dates ) that 'do not use company_id'
  if(is.null(keys)) character() -> keys
  
  verify_connection()
  
  # verify that si_finecon2 exists  
  # if(!db.q(str_c("select count(*) from information_schema.tables where table_name = 'si_finecon2';"), conn.id = cid)) {
  if(!dbExistsTable(con, "si_finecon2")) {
    verify_si_finecon_exists()
  }

  # SHOULD HAVE column dateindex, ELSE I can not (later in the function) join 
  if(any(colnames(value) %in% "dateindex")) {

    unique(value$dateindex) -> value_unq_index

    # FAULTY LOGIC ( HAVE TO RETURN TO THIS 'TOPIC' )  
    #    ** DO I NEED A 'SETDIFF INSTEAD?' **
    # what 'value df dateindex' are already in the database
    value_unq_index[
      value_unq_index %in% as.vector(unlist(db.q(str_c("select distinct dateindex from si_finecon2;"), conn.id = cid)))
                   ] ->  same_dateindex_found_in_db

    # FAULTY LOGIC ( HAVE TO RETURN TO THIS 'TOPIC' ) 
    # # the value dateindex was not found in the database
    # if(length(same_dateindex_found_in_db) == 0) {
    #     
    #   # if the dateindex was not found in the database then run verify_company_basics(dateindex)
    #   warning(str_c("no dateindex records of " %s+% ' ' %s+% str_c(value_unq_index, collapse = " ") %s+% ' ' %s+% " found in si_finecon, SO RUNNING verify_company_basics first ")) 
    #   
    #   for(dateindex_i in setdiff(value_unq_index, same_dateindex_found_in_db)){
    #   
    #     warning(str_c("Begin verify_company_basics/update_from_future_new_company_ids at ",dateindex_i))
    #     verify_company_basics(dateindex_i) -> si_all_g_df
    #     update_from_future_new_company_ids(df = si_all_g_df, ref = dateindex_i) -> si_all_g_df # always run
    #     upsert(si_all_g_df, keys = c("company_id"))
    #     warning(str_c("End verify_company_basics/update_from_future_new_company_ids at ",dateindex_i))
    # 
    #   
    #   }
    # 
    # }
    
  }
  
  # cleanup and prepare
                # note: if I send "dateindex", then all records are removed(rm_df_dups)
                # LATER?, I may want to liberalzed this function "to 'all dups per group'"
  # ALL 3 already DONE in the verify_ function
  # { value } %>% rm_df_dups(., keys) %>% lcase_a_remove_useless_columns(.) %>% financize(.) -> value
  
  # compare inbound value dat.frame column names and column types 
  with(value,{sapply(ls(sort=FALSE),function(x){class(get(x))})}) -> value_meta
  
  # to what is in the database
  db.data.frame("si_finecon2", conn.id = cid, verbose = FALSE) -> ptr_si_finecon2  # class (db.#)
  col.types(ptr_si_finecon2) -> fc_meta
      names(ptr_si_finecon2) -> names(fc_meta) 
  
  # names(with types) in value that are 'value only'(setdiff) that need to (soon) be new columns in 'fc'
  value_meta[names(value_meta) %in% setdiff(names(value_meta),names(fc_meta))] -> fc_new_columns

  # plan to change "numeric"(R) to "numeric(8,2)"(PostgreSQL)
  #
  # + other custom column types
  # 
  "text"          -> fc_new_columns[fc_new_columns == "character"] 
  "numeric(8,2)"  -> fc_new_columns[fc_new_columns == "numeric"]  
  "smallint"      -> fc_new_columns[names(fc_new_columns) %in% c("drp_avail","adr")] 
   
  # actually add NEW columns to si_finecon 
  # if any columns exist to add
  # EXPECTED 'IF-THEN' to be extended
  if(length(fc_new_columns) > 0L) {
  
    # to fc, add new columns ( of 'new columns' from 'value' that do not exist in 'fc' )
    str_trim(str_c(rstring('
    alter table if exists si_finecon2
      <% for (i in seq_along(fc_new_columns)) { -%>
          <% sprintf("add %1$s %2$s", 
               names(fc_new_columns)[i], fc_new_columns[i]) -> res
          -%><%= str_c("    ",res) %><%=if(i  < length(fc_new_columns)) ", \n" -%><%=if(i == length(fc_new_columns)) "  \n" -%>
      <% } %>
    ;                     
    ')))  %>% clean_text(.) -> add_columns_sql # 
              # remove at the beginning
              # debug at <text>#30: .base_paste0 <- base::paste0\n\n 
              # stop the rstudio debugger OUTPUT from going inside the string

    db.q(add_columns_sql, conn.id = cid)
    
    # to what is in the database
    db.data.frame("si_finecon2", conn.id = cid, verbose = FALSE) -> ptr_si_finecon2  # class (db.#)
    col.types(ptr_si_finecon2) -> fc_meta
        names(ptr_si_finecon2) -> names(fc_meta) 
    
  } else {
    warning("in call to function upsert, no new columns were found to be needed to be added to si_finecon2.  Is this correct?")
  }
  
  # add a 'dataindex + keys'primary key column to input 'value data.fram ( needed for PivotalR and OTHER things ) 
  # currenly 'really' only tested used with/about company_id
  str_c(c("dateindex",keys), collapse = "_")  -> value_primary_key
  with( value, { eval(parse(text=eval(parse(text=('str_c(c("dateindex",keys), collapse = " %s+% \'_\' %s+% ")'))))) } ) -> value[,value_primary_key]
  DataCombine::MoveFront(value,value_primary_key) -> value
  
  # upload 'value' into the database 
  
  # eventually
  {function() { db.q("drop table if exists upsert_temp", conn.id = cid) }} -> drop_upsert_temp
  #
  drop_upsert_temp()
  # # upsert into the database
  # SEEMS must CREATE A pk THIS WAY
  as.db.data.frame(value, "upsert_temp", conn.id = cid, verbose = FALSE, key = value_primary_key) -> ptr_upsert_temp
  
  # db.q(str_c("create unique index upsert_temp_unqpkidx on upsert_temp(" %s+% value_primary_key %s+% ");"), conn.id = cid)

  # garantee that upsert_Temp values are unique
  try( { db.q(str_c("alter table if exists upsert_temp add primary key(" %s+% value_primary_key %s+% ");"), conn.id = cid) }, silent = TRUE )
  # upsert_temp_<value_primary_key>_pkey ( singleton )
  
  # db.q(str_c("alter table upsert_temp add constraint upsert_temp_unqkey unique(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid)
  # db.q(str_c("create unique index upsert_temp_unqidx on upsert_temp(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid)
  
  # try( { db.q(str_c("alter table if exists upsert_temp add unique(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid) }, silent = TRUE )

  # garantee unqueness 
  try( { db.q(str_c("create unique index if not exists upsert_temp_keyz_key on upsert_temp(" %s+% str_c(c("dateindex",keys), collapse = ", ") %s+% ");"), conn.id = cid) }, silent = TRUE )
  # upsert_temp_keyz_key ( can be many )
  
  bm <- 1
  
  # par1
  # ? db.data.frame HELP ( indirect way of doing it)
  # to 'value in the database' change 'double precision' to 'numeric(8,2)'  
  
  # eventually
  # col.types(ptr_upsert_temp) -> upsert_meta
  #     names(ptr_upsert_temp) -> names(upsert_meta)
  
  # IF I NEED TO ( change ) a data.type here
  # make another table
  # as.list(col.types(ptr_upsert_temp)) -> LL; names(ptr_upsert_temp) -> names(LL); LL
  # as.db.data.frame(ptr_upsert_temp[,], table.name ="upsert_temp_greater", field.types = LL) -> ptr_upsert_temp_gr
  
  # upsert ( these columns have a different data type )
  # of upsert_temp these colums are what I want to change  dataypes to match what is in si_finecon2

  # REQUIRED refresh of meta?!
  # ovewrite
  db.data.frame("upsert_temp", conn.id = cid, verbose = FALSE) -> ptr_upsert_temp  # class (db.#)
  col.types(ptr_upsert_temp) -> upsert_meta
      names(ptr_upsert_temp) -> names(upsert_meta)

  # to what is in the database ( RE-calculate fc_meta: REDONE from above )
  # REQUIRED refresh of meta?!
  db.data.frame("si_finecon2", conn.id = cid, verbose = FALSE) -> ptr_si_finecon2  # class (db.#)
  col.types(ptr_si_finecon2) -> fc_meta
      names(ptr_si_finecon2) -> names(fc_meta) 

  # prepare for together by creating a common join column
  data.frame(upsert_meta, in_common_names  = names(upsert_meta), stringsAsFactors = FALSE) -> upsert_df 
  data.frame(fc_meta    , in_common_names  = names(fc_meta),     stringsAsFactors = FALSE) -> fc_df 

  # outer join together
  plyr::join(upsert_df, fc_df, by = c("in_common_names"), type = "full", match = "all" )-> upsert_fc_df

  # find column date types in table upsert_temp thet need to be changed
  { upsert_fc_df } %>% {
      # non-NA only concerted about matches(in common(f/full)) AND matches(datatypes) that are not equal
     .[(!is.na(.$upsert_meta)) & (.$upsert_meta !=.$fc_meta),,drop = FALSE] } %>% { 
       # convert to an iteratable list
       # as.list( data.frame(t(.), stringsAsFactors = FALSE) ) 
       { split(as.data.frame(., stringsAsFactors = F)[,, drop = F], seq_along(as.data.frame(., stringsAsFactors = F)[,1, drop = T])) }
        } -> upsert_col_type_changes
             #from #in_common #to

  print("upsert_temp from ... to column type changes")
  print(upsert_col_type_changes)
  
  # actually update type in NEW columns to update_temp
  if(length(upsert_col_type_changes) > 0L) {

    # actual change column date types in table upsert_temp
    0 -> i
    str_trim(str_c(rstring('
    alter table if exists upsert_temp
      <% for (change_i in upsert_col_type_changes) { -%>
          <% i + 1 -> i; sprintf("alter column %1$s set data type %2$s", 
               change_i[2], change_i[3]) -> res
          -%><%= str_c("    ",res) %><%=if(i  < length(upsert_col_type_changes)) ", \n" -%><%=if(i == length(upsert_col_type_changes)) "  \n" -%>
      <% } %>
    ;                     
    ')))  %>% clean_text(.) -> upsert_col_type_changes_sql #
  
    db.q(upsert_col_type_changes_sql, conn.id = cid)
    # update meta
    # overwrite
    db.data.frame("upsert_temp", conn.id = cid, verbose = FALSE) -> ptr_upsert_temp  # class (db.#)
    col.types(ptr_upsert_temp) -> upsert_meta
        names(ptr_upsert_temp) -> names(upsert_meta)
  
  } else {
    print("in call to function upsert, no update type were found to be needed to be done in update_temp.  Is this correct?")
  }
      
  # part2
  # to fc, sql_update ON CONFLICT DO UPDATE ( given columns/info from value )  
  
  # -- detect if it has the column *dateindex_company_id_orig* ( si_ci )
  # insert into si_finecon2(dateindex_company_id_orig, dateindex_company_id, dateindex, company_id, company_id_orig, ticker, company) 
  #                  select dateindex_company_id_orig, dateindex_company_id, dateindex, company_id, company_id_orig, ticker, company
  #                    from upsert_temp 
  #                      on conflict (dateindex_company_id_orig) 
  #                        do update set (dateindex_company_id_orig, dateindex, company_id, company_id_orig, ticker, company) = (excluded.dateindex_company_id_orig, excluded.dateindex, excluded.company_id, excluded.company_id_orig, excluded.ticker, excluded.company);
  # 
  # -- detect if (upsert_temp)  does not have the column *dateindex_company_id_orig* ( use dateindex_company_id )
  # -- REPLACE: on conflict (dateindex_company_id_orig)  WITH on conflict (dateindex_company_id)  

  logical()       -> upsert_temp_perform_nothing
  logical()       -> upsert_temp_perform_upsert
  character()     -> conflict_column
  # prepare to upsert
  if( all(c("dateindex_company_id_orig","ticker") %in% names(upsert_meta)) ) {
  
    # detect if si_finecon2.dateindex_company_id_orig exists, then this is th required MIN columns to be updated
    if (!any( !c("dateindex_company_id_orig", "dateindex_company_id", "dateindex", 
                           "company_id",      "company_id_orig", "ticker", "company") %in% names(upsert_meta) )) {
      # si_ci GO EXACTLY HERE
      FALSE           -> upsert_temp_perform_nothing
      TRUE            -> upsert_temp_perform_upsert
      "dateindex_company_id_orig" -> conflict_column
    } else {
      TRUE           -> upsert_temp_perform_nothing # NOTHING IS HERE
      FALSE          -> upsert_temp_perform_upsert
      stop("dateindex_company_id_orig and ticker exist BUT the mininum columns do not")
    }
  } else { 
    if("dateindex_company_id_orig" %in% names(upsert_meta)) { 
      # si_NON_ci GO EXACTLY HERE
      FALSE                           -> upsert_temp_perform_nothing
      FALSE                           -> upsert_temp_perform_upsert # then do update-ish
      "dateindex_company_id_orig"     -> conflict_column # STILL
    } else {
      if("dateindex" %in% names(upsert_meta)) {
        # ** verify_return_dates(dateindex) go HERE ***
        FALSE           -> upsert_temp_perform_nothing
        FALSE           -> upsert_temp_perform_upsert # then do update-ish
        "dateindex"     -> conflict_column 
      } else {
        TRUE            -> upsert_temp_perform_nothing
        FALSE           -> upsert_temp_perform_upsert
        stop(str_c("update_temp dateindex_company_id_orig NOT exist AND ticker NOT exist " %s+% " and primary key is only "  %s+% str_c(c("dateindex",keys), collapse = ", ")))
      }
    }
  }
  if(!length(upsert_temp_perform_nothing)) stop("upsert_temp_perform_nothing is not determined")
  if(!length(upsert_temp_perform_upsert))  stop("upsert_temp_perform_upsert is not determined")
  
  if(upsert_temp_perform_upsert) {
  
    # actually perform the upsert
    str_trim(str_c(rstring('<%= 
    sprintf(
    "insert into si_finecon2(" %s+% str_c(names(upsert_meta), collapse = ", ") %s+% ")" %s+% " \n" %s+% 
    "  select " %s+% str_c(names(upsert_meta), collapse = ", ") %s+% " \n" %s+% 
    "    from upsert_temp" %s+% " \n" %s+% 
    "      on conflict (" %s+% conflict_column %s+% ")" %s+% " \n" %s+%
    "        do update set (" %s+% str_c(names(upsert_meta), collapse = ", ") %s+% ") = (" %s+%  str_c("excluded.", names(upsert_meta), collapse = ", ") %s+% ");" 
    )
    %>')))  %>% clean_text(.) -> fc_col_val_changes_sql 
    
    print("UPSERT")

  }
  
  
  # %s+% if( conflict_column == "dateindex" ) { " and dateindex = " %s+% dateindex %s+%  ";" }  else { ""  %s+% ";" }
  
  # then do update-ish STILL based on company_id_orig  
  # ( and do not UPDATE 
  #   dateindex, company_id, dateindex_company_id, company_id_orig         # at least ONE COLUMN to try to update ( so the SQL DML UPDATE statement is valid ) # BELOW ARE 'non-updatable'
  if(!upsert_temp_perform_nothing && !upsert_temp_perform_upsert && sum(!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")) ) {
    
    # actually perform the update 
    str_trim(str_c(rstring('<%= 
    sprintf(
    "update si_finecon2 s " %s+% " set " %s+% 
     str_c(sprintf("\n  %1$s = t.%1$s", names(upsert_meta)[!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")] ), collapse = ", ") %s+% " \n" %s+%
    "    from upsert_temp t " %s+% " \n" %s+%
    "      where " %s+% "s." %s+% conflict_column %s+% " = " %s+% "t." %s+% conflict_column  %s+% ";" 
    )
    %>')))  %>% clean_text(.) -> fc_col_val_changes_sql

    print("UPDATE")
    
  }
  
  if(!upsert_temp_perform_nothing) {
    
    print(writeLines(fc_col_val_changes_sql))
  
    rs <- dbSendQuery(con, fc_col_val_changes_sql)
    if(dbHasCompleted(rs)) { print(str_c("Rows Affected: ", dbGetRowsAffected(rs)))  }
    
    # for right now. I will just stop
    if(dbGetRowsAffected(rs) == 0) { stop(str_c("Since Rows Affected: ", dbGetRowsAffected(rs)," so PERHAPS something is wrong!"))  }
    
    if(dbHasCompleted(rs)) dbClearResult(rs)
    
    # db.q(fc_col_val_changes_sql, conn.id = cid) 
    
    # if I have the columns for this index, then create the index ( if it does not already exist )
    verify_finecon_jamesos_partial_idx()
    
  }
  
  drop_upsert_temp()
  
  options(ops)
  return(invisible(NULL))
   
}



## (future) BUSINESS LOGIC ERROR HERE: (RETURNS too_early(after first loop)) SHOULD INSTEAD 'ADD TO A 'LIST OF DATA.FRAMES'
# 
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
    
    require(magrittr)
    
    require(stringi)
    require(stringr)
    
    # uses # DataCombine::MoveFront
    # uses plyr::join
    
    # uses last_day_of_month
    # uses lwd_day_of_month
    # uses insert_df
    
    verify_si_finecon_exists()
    
    # verify that si_finecon2 exists 
    # if(!db.q(str_c("select count(*) from information_schema.tables where table_name = 'si_finecon2';"), conn.id = cid)) {
    if(!dbExistsTable(con, "si_finecon2")) {
      verify_si_finecon_exists()
    }
    
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
      # c("setup",                                           # DONE [X]
      #   "si_ci",                                           # DONE [X]
      #   "si_exchg","si_mgdsc",                             # DONE[X]
      #   "si_psd"                       # mktcap(big index) 
      #   "si_bsq","si_isq","si_cfq",
      #   "si_date","si_psdc","si_psdd",
      #   "si_mlt","si_rat",
      #   "si_ee")
      
      si_tbl <- c("si_ci","si_exchg","si_mgdsc")
      for(si_tbl_i in si_tbl) {
      
        ## always load
        paste0(getsetvar_aaii_sipro_dir(),"/",dateindex_redux_i,"/") -> part_path_file_name
        si_si_tbl_df <- suppressWarnings(suppressMessages(foreign::read.dbf(file = paste0(part_path_file_name,si_tbl_i,".dbf"), as.is = TRUE)))
        
        # # all lower ( PostgreSQL friendly )
        # colnames(si_si_tbl_df) <- tolower(colnames(si_si_tbl_df))
        # 
        # # remove useless columns
        # # nothing starts with 'x'
        # # x_nullflags, x., x, x.1, x.2 ...
        # # 
        # si_si_tbl_df[, !str_detect(colnames(si_si_tbl_df),"^x\\.?+")   & 
        #                !str_detect(colnames(si_si_tbl_df),"^repno$")   & 
        #                !str_detect(colnames(si_si_tbl_df),"^lastmod$") &
        #                !str_detect(colnames(si_si_tbl_df),"^updated$")
        # , drop = FALSE] -> si_si_tbl_df 
        
        lcase_a_remove_useless_columns(si_si_tbl_df) -> si_si_tbl_df
        
        
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
        
        # save before updating with new(future) values
        if(si_tbl_i == "si_ci") {
          { si_si_tbl_df } %>%
          insert_df(.,      .$company_id                 , "company_id_orig"          , match("company_id", colnames(.)) - 1 ) -> si_si_tbl_df
          { si_si_tbl_df } %>%
          insert_df(.,str_c(.$dateindex,'_',.$company_id), "dateindex_company_id_orig", match("company_id", colnames(.)) - 1 ) -> si_si_tbl_df
        }
          
        # remove duplicated company_id, ticker ( will NOT be loaded into the PostgreSQL database)
        
        # BEGIN
        # LATER: SHOULD be able to REPLACE with rm_df_dups(si_si_tbl_df, c("company_id","ticker")) -> si_si_tbl_df
        
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
          
        # LATER: SHOULD be able to REPLACE with rm_df_dups(si_si_tbl_df, c("company_id","ticker")) -> si_si_tbl_df
        # END
        
        # speed

        ##  keys, DFIs, and more DFI keys: ... speed
        # if(any(colnames(si_si_tbl_df) %in% "company_id")) optimize(si_si_tbl_df)               -> si_si_tbl_df
        # if(any(colnames(si_si_tbl_df) %in% "exchg_code")) optimize(si_si_tbl_df, "exchg_code") -> si_si_tbl_df
        # if(any(colnames(si_si_tbl_df) %in% "mg_code"))    optimize(si_si_tbl_df, "mg_code")    -> si_si_tbl_df
          
        # create a primary key and move it to the front
        
          # SHOULD *MAKE* THIS INTO A FUNCTION
          # currenly ONLY for tables that have company_id ( and dateindex )

        # generate column: dateindex_company_id on ...
        if("company_id" %in% colnames(si_si_tbl_df)) {

          "company_id" -> keys
          str_c(c("dateindex",keys), collapse = "_") -> si_si_tbl_df_primary_key
          with( si_si_tbl_df, { eval(parse(text=eval(parse(text=('str_c(c("dateindex",keys), collapse = " %s+% \'_\' %s+% ")'))))) } ) -> si_si_tbl_df[,si_si_tbl_df_primary_key]
          DataCombine::MoveFront(si_si_tbl_df,si_si_tbl_df_primary_key) -> si_si_tbl_df

        }
        
        # si_TBL VARIABLES
        assign(si_tbl_i,si_si_tbl_df)
        rm(si_si_tbl_df)
        
      }
      
      # join key of exchange
      si_exchg$exchg_code  -> si_exchg$exchange
      
      # outer join
      plyr::join_all(list(si_ci,si_exchg), by = c("dateindex","exchange"), type = "full") -> si_all_df
      
      # optimize(si_all_df) -> si_all_df
      
      rm(si_ci)
      rm(si_exchg)
      
      # join key of industry
      si_all_df$ind_3_dig -> si_all_df$industry_code
       si_mgdsc$mg_code  ->   si_mgdsc$industry_code
      
      # left join because mg_code has codes that are sectors and not industries ( creates orphans )
      # left join becuase orphan industries are meaningless
      plyr::join_all(list(si_all_df,si_mgdsc), by = c("dateindex","industry_code"), type = "left") -> si_all_df
      si_all_df$mg_desc -> si_all_df$industry_desc 
      within( si_all_df, { rm("mg_code","mg_desc") }) -> si_all_df
      si_all_df$rn_si_mgdsc -> si_all_df$rn_si_mgdsc_ind; within(si_all_df, { rm("rn_si_mgdsc") } ) -> si_all_df 
      within(  si_mgdsc , { rm("industry_code") }) -> si_mgdsc

      # optimize(si_all_df) -> si_all_df
      
      # join key of sector
      si_all_df$ind_2_dig            ->  si_all_df$sector_code
      str_sub(si_mgdsc$mg_code,1,2)  ->  si_mgdsc$sector_code
      
      # get rid of duplicated sectors ( each industry has its sector rementioned )
      si_mgdsc -> si_mgdsc  
      # SPECIAL
      si_mgdsc$rn_si_mgdsc -> si_mgdsc$rn_si_mgdsc_sect
      si_mgdsc[with( si_mgdsc, { !stri_duplicated(sector_code) } ),,drop = FALSE] -> si_mgdsc
        
      # optimize(si_mgdsc,c("mg_code")) -> si_mgdsc
      
      # left join becuase orphan sectors are meaningless
      plyr::join_all(list(si_all_df,si_mgdsc), by = c("dateindex","sector_code"), type = "left")  -> si_all_df
      si_all_df$mg_desc -> si_all_df$sector_desc 
      within( si_all_df, { rm("mg_code","mg_desc") }) -> si_all_df
      # SPECIAL 
      within(si_all_df, { rm("rn_si_mgdsc") } ) -> si_all_df
      within(  si_mgdsc , { rm("sector_code") }) -> si_mgdsc

      financize(si_all_df) -> si_all_df
      # optimize(si_all_df) -> si_all_df
      
      rm("si_mgdsc")
      
      return(si_all_df) ## BUSINESS LOGIC ERROR HERE: (RETURNS too_early(after first loop)) SHOULD INSTEAD 'ADD TO A 'LIST OF DATA.FRAMES'
    } 
    
  }
  ret <- verify_company_basics_inner(dateindex = dateindex)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
}

# verify_company_basics(dateindex = NULL)
# verify_company_basics(dateindex = c(15764)) -> si_all_g_df

# LEFT_OFF 
#   financize-like TRIM data.types ( and load into the database)
#   ( Caroline-like & SQL: load into the database)
# 
# look forward to NEXT month and get any NEW COMPANY_IDS by TICKER then company_id by STREET
# 
# WORKS
# colnames(si_ci)[match("EXCHANGE",colnames(si_ci))] <- "EXCHG_CODE"
# plyr::join_all(list(si_ci,si_exchg), type = "full") 

# next program ( on demand load)




# 'grabbing future' company_id
# (future) SHOULD BE VECTORIZED: should be INPUT MANY 'ref's AND then RETERN 'A 'LIST OF DATA.FRAMES'
# 
update_from_future_new_company_ids <- function(df = NULL, ref = NULL) {
  
  require(magrittr)
  require(lubridate)
  require(stringr)
  require(PivotalR)
  
  # uses zoo::as.Date, foreign::read.dbf, plyr::join
  # uses lwd_of_month, rm_df_dups
  
  # zoo::as.Date(ref) %m+% months(1:13) %>% lwd_of_month(x) %>% zoo::as.Date(.)
  #    "2013-02-28"
  
  #  [1] 15793 15825 15856 15884 15917 15947 15978 16009 16038 16070 16101 16129
  # [13] 16160
  
  #  [1] "2013-03-29" "2013-04-30" "2013-05-31" "2013-06-28" "2013-07-31"
  #  [6] "2013-08-30" "2013-09-30" "2013-10-31" "2013-11-29" "2013-12-31"
  # [11] "2014-01-31" "2014-02-28" "2014-03-31"
  
  # ONLY applicable to dates BEFORE this DATE
  # then need to try to update the 'old company_ids' in the old system to the new company_ids in the 'new system'
  if(ref < 15184 ) {
  
    verify_connection()
    
  
    # just work with the core
    # if("DFI" %in% class(df)) df <- df$x
    
                      # must be a data.frame ALONE
                                                      # temporary table becaue I did not give a name
    trg_db_temp    <- as.db.data.frame(as.data.frame(df), conn.id = cid, key = "company_id", verbose = FALSE)
    trg_db_temp_nm <- trg_db_temp@.content # temporary table
    db.q(str_c("drop table if exists trg"), conn.id = cid)
    db.q(str_c("create table if not exists trg as select * from ", trg_db_temp_nm, collapse = ""), conn.id = cid)
    db.q(str_c("drop index if exists trg_company_id_idx"), conn.id = cid)
    db.q(str_c("create index if not exists trg_company_id_idx on trg(company_id)"), conn.id = cid)
    db.q(str_c("drop index if exists trg_ticker_idx"), conn.id = cid)
    db.q(str_c("create index if not exists trg_ticker_idx on trg(ticker)"), conn.id = cid)
    db.q(str_c("drop index if exists trg_company_idx"), conn.id = cid)
    db.q(str_c("create index if not exists trg_company_idx on trg(company)"), conn.id = cid)
    
    # str( db.q(str_c("select * from trg"), conn.id = cid) )
    
    
    # the_past(whatever) 
    # past data looking forward to grab company_ids, tickers, and company s
    # expect 2 SQL updates per loop iteration
    
    # FIRST(+future+) date that HAS the NEW(earliest of latest) company_id system
    for(month_i in 15184) {
    
    # will downsize into integers ( 1 months ahead is sufficient to find a lost link )
    # ?? loop iterations ( CURRENLY JUST ONE iteration)
    # for(month_i in zoo::as.Date(ref) %m+% months(1:1)) {
      
      lwd_of_month(month_i) -> lwd
      
      print(str_c("Looking in direction at ... ", zoo::as.Date(lwd)," ",lwd," Maybe in "))
      if(file.exists(str_c("W:/AAIISIProDBFs/",lwd))) { print(str_c("  Exists: ","W:/AAIISIProDBFs/",lwd)) }
      
      suppressWarnings(suppressMessages(foreign::read.dbf(file = str_c("W:/AAIISIProDBFs/",lwd,"/si_ci.dbf")
                                                          , as.is = TRUE))) %>% lcase_a_remove_useless_columns(.) %>%
        ## setNames(.,tolower(colnames(.))) %>%
        rm_df_dups(.,c("company_id","ticker"))  -> trg_db
      
                                               # no name ... becomes a temporary
      src_db_temp    <- as.db.data.frame(trg_db, conn.id = cid, key = "company_id", verbose = FALSE)
      src_db_temp_nm <- src_db_temp@.content # temporary table
      db.q(str_c("drop table if exists src"), conn.id = cid)
      db.q(str_c("create table if not exists src as select * from ", src_db_temp_nm, collapse = ""), conn.id = cid)
      db.q(str_c("drop index if exists src_company_id_idx"), conn.id = cid)
      db.q(str_c("create index if not exists src_company_id_idx on src(company_id)"), conn.id = cid)
      db.q(str_c("drop index if exists src_ticker_idx"), conn.id = cid)
      db.q(str_c("create index if not exists src_ticker_idx on src(ticker)"), conn.id = cid)
      db.q(str_c("drop index if exists src_company_idx"), conn.id = cid)
      db.q(str_c("create index if not exists src_company_idx on src(company)"), conn.id = cid)
      
      # not worth my time to write a trigger affecting dateindex_company_id
      # UPDATE 1 - grab future months where the ticker name is the same but the company_id is differrent
      # db.q(str_c("
      #            update trg
      #              set company_id =                     src.company_id,
      #        dateindex_company_id = dateindex || '_' || src.company_id  
      #            from src
      #              where trg.company_id != src.company_id and
      #                    trg.ticker      = src.ticker
      # "), nrows =  -1, conn.id = cid)
      
      db.q(str_c("
                 update trg
                   set company_id =                     src.company_id,
             dateindex_company_id = dateindex || '_' || src.company_id  
                 from src
                   where trg.company_id != src.company_id and
                         trg.ticker      = src.ticker and
                         trg.street      = src.street
      "), nrows =  -1, conn.id = cid)    # SRC.STREET # a little extra safety, hp & hpq
      
      
      ## MAY? WANT TO SHUT OFF DURING DEVELOPMENT ( THIS TAKES 5 SECONDS TO RUN)
      
      # not worth my time to write a trigger affecting dateindex_company_id
      # I DO NOT LIKE THIS ONE.  ... waiting for an error to occur
      # UPDATE 2 ( UPDATE 1 IS REQUIRED ) - grab future months where the company nme is the same ( and elimintate duplicates )
      
      ## MAY BE 'BUGGY'??
      # db.q(str_c("
      #           update trg
      #           set company_id          =                     src.company_id,
      #     dateindex_company_id          = dateindex || '_' || src.company_id
      #                   from src
      #                 where trg.company_id != src.company_id and
      #                       trg.ticker     != src.ticker and
      #                       trg.company     = src.company  -- 117
      #           and trg.company not in -- IN -> NOT_IN -> ELMINTATES DUPLICATED COMPANY NAMES
      #           (select trg.company 
      #           from trg
      #           where trg.company
      #           in ( 
      #             select src.company
      #                from trg, src
      #                 where trg.company_id = src.company_id and
      #                       trg.ticker     = src.ticker 
      #             )
      #           ) -- 99 rows ( 5 second update in PgAdminIII)
      #            "), nrows =  -1, conn.id = cid)
      ## END OF 'MAY BE BUGGY?'
      
      # ORDER is NOT GARANTEED
      ci_tk <- db.q(str_c("select dateindex_company_id, dateindex_company_id_orig, company_id from trg"), nrows =  -1, conn.id = cid)
      df <- plyr::join(ci_tk, df[,!colnames(df) %in% c("dateindex_company_id","company_id"), drop = FALSE], by = "dateindex_company_id_orig" ,type = "inner")
      
      print(str_c("Done looking in direction at ... ", zoo::as.Date(lwd)," ",lwd," Maybe in "))
    }
    
    db.q(str_c("drop table if exists trg"), conn.id = cid)
    db.q(str_c("drop table if exists src"), conn.id = cid)
    
    # optimize(df) -> df
    
  }

  return(df)
  
}




# SEE BELOW (for how to use)
verify_company_details <- function(dateindex = NULL,  table_f = NULL, cnames_e = NULL) {
  
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
  
  verify_company_details_inner <- function (dateindex = NULL, table_f = NULL, cnames_e = NULL) {
    
    verify_si_finecon_exists()
    
    require(magrittr)
    
    require(stringi)
    require(stringr)
    
    # uses # DataCombine::MoveFront
    # uses plyr::join
    
    # uses last_day_of_month
    # uses lwd_day_of_month
    # uses insert_df
    
    # run once
    getvar_all_load_days_lwd_var <- getvar_all_load_days_lwd()
    
    ## LEFT_OFF
    bm <- 1
    
    # dateindex in arg not found on disk
    if(!dateindex %in% getvar_all_load_days_lwd_var) { 
      dateindex[!dateindex %in% getvar_all_load_days_lwd_var] -> dateindex_not_found_on_disk
      stop("arg dateindex not found on disk" %s+% str_c(dateindex_not_found_on_disk, collapse = "") )
    }
    
    ## SINGLE value NOW A REDUNDANT CHECK
    ## getsetvar_aaii_sipro_dir()
    ## if(!dateindex %in% getvar_all_load_days_lwd_var) stop("no arg dateindex was found on disk")
    
    ## SINGLE value NOW A REDUNDANT CHECK
    # just the ones on found on disk    
    ## spec date(not all dates) IN 'all disk possible aaii si_pro last weekday of the month'
    # dateindex_redux <- dateindex[dateindex %in% getvar_all_load_days_lwd_var]
    
    
    # dateindex_redux_i
    # at least one

    # # save for later
    # c("setup",                                           # DONE [X]
    #   "si_ci",                                           # DONE [X]
    #   "si_exchg","si_mgdsc",                             # DONE[X]
    #   "si_psd"                       # mktcap(big index) 
    #   "si_bsq","si_isq","si_cfq",
    #   "si_date","si_psdc","si_psdd",
    #   "si_mlt","si_rat",
    #   "si_ee")
    
    # si_tbl <- c("si_ci","si_exchg","si_mgdsc")
    
    table_f -> si_tbl_i 

    ## always load
    paste0(getsetvar_aaii_sipro_dir(),"/",dateindex,"/") -> part_path_file_name
    si_si_tbl_df <- suppressWarnings(suppressMessages(foreign::read.dbf(file = paste0(part_path_file_name,si_tbl_i,".dbf"), as.is = TRUE)))
    
    # # all lower ( PostgreSQL friendly )
    # colnames(si_si_tbl_df) <- tolower(colnames(si_si_tbl_df))
    # 
    # # remove useless columns
    # # nothing starts with 'x'
    # # x_nullflags, x., x, x.1, x.2 ...
    # # 
    # si_si_tbl_df[, !str_detect(colnames(si_si_tbl_df),"^x\\.?+")   & 
    #                !str_detect(colnames(si_si_tbl_df),"^repno$")   & 
    #                !str_detect(colnames(si_si_tbl_df),"^lastmod$") &
    #                !str_detect(colnames(si_si_tbl_df),"^updated$")
    # , drop = FALSE] -> si_si_tbl_df 
    
    lcase_a_remove_useless_columns(si_si_tbl_df) -> si_si_tbl_df
    
    # keep the ones that I have interest
    si_si_tbl_df[,str_subset(colnames(si_si_tbl_df),str_c("^company_id$","|",cnames_e)),drop = FALSE] -> si_si_tbl_df
    
    # unique ids
    
    # if(si_tbl_i == "si_ci") {
    # 
    #   within( si_si_tbl_df, { assign("dateindexeom", rep(last_day_of_month(dateindex),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
    #   DataCombine::MoveFront(si_si_tbl_df,   "dateindexeom")    -> si_si_tbl_df
    #   
    #   within( si_si_tbl_df, { assign("dateindexlwd", rep(     lwd_of_month(dateindex),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
    #   DataCombine::MoveFront(si_si_tbl_df,   "dateindexlwd") -> si_si_tbl_df
    #   
    # }
    
    within( si_si_tbl_df, { assign("dateindex", rep(as.integer(dateindex),NROW(si_si_tbl_df[,1]))      )  } ) -> si_si_tbl_df
    DataCombine::MoveFront(si_si_tbl_df,   "dateindex")                                  -> si_si_tbl_df
    
    within( si_si_tbl_df, { assign("rn_" %s+% si_tbl_i, seq_along(si_si_tbl_df[,1])) } ) -> si_si_tbl_df
    DataCombine::MoveFront(si_si_tbl_df,   "rn_" %s+% si_tbl_i)                          -> si_si_tbl_df
    
    # save before updating with new(future) values
    # EVERY TABLE WILL END UP NEEDING
    # if(si_tbl_i == "si_ci") {
      { si_si_tbl_df } %>%
      insert_df(.,      .$company_id                 , "company_id_orig"          , match("company_id", colnames(.)) - 1 ) -> si_si_tbl_df
      { si_si_tbl_df } %>%
      insert_df(.,str_c(.$dateindex,'_',.$company_id), "dateindex_company_id_orig", match("company_id", colnames(.)) - 1 ) -> si_si_tbl_df
    # }
      
    # remove duplicated company_id, ticker ( will NOT be loaded into the PostgreSQL database)
    
    # BEGIN
    # LATER: SHOULD be able to REPLACE with rm_df_dups(si_si_tbl_df, c("company_id","ticker")) -> si_si_tbl_df
    
    # "si_exchg","si_mgdsc" will NOT HAVE
    
    # if(any(colnames(si_si_tbl_df) %in% "company_id")) {
    #   si_si_tbl_df[
    #     !with(si_si_tbl_df, { company_id == "" | is.na(company_id) | is.null(company_id) | stri_duplicated(company_id) | stri_duplicated(company_id, fromLast = TRUE) 
    #   }),,drop = FALSE] -> si_si_tbl_df
    # }
    
    if(any(colnames(si_si_tbl_df) %in% "company_id")) {
      rm_df_dups(si_si_tbl_df,"company_id") -> si_si_tbl_df
    }
    
    # # OTHER si_ci TABLES WILL NOT HAVE BUT OK TO LEAVE HERE
    # # extra for si_ci
    # if(any(colnames(si_si_tbl_df) %in% "ticker")) {
    #   si_si_tbl_df[
    #     !with(si_si_tbl_df, { ticker     == "" | is.na(ticker)     | is.null(ticker)     | stri_duplicated(ticker)     | stri_duplicated(ticker    , fromLast = TRUE) 
    #     }),,drop = FALSE] -> si_si_tbl_df
    # }
      
    # LATER: SHOULD be able to REPLACE with rm_df_dups(si_si_tbl_df, c("company_id","ticker")) -> si_si_tbl_df
    # END
    
    # speed

    ##  keys, DFIs, and more DFI keys: ... speed
    # if(any(colnames(si_si_tbl_df) %in% "company_id")) optimize(si_si_tbl_df)               -> si_si_tbl_df
    # if(any(colnames(si_si_tbl_df) %in% "exchg_code")) optimize(si_si_tbl_df, "exchg_code") -> si_si_tbl_df
    # if(any(colnames(si_si_tbl_df) %in% "mg_code"))    optimize(si_si_tbl_df, "mg_code")    -> si_si_tbl_df
      
    # create a primary key and move it to the front
    
      # SHOULD *MAKE* THIS INTO A FUNCTION
      # currenly ONLY for tables that have company_id ( and dateindex )

    # generate column: dateindex_company_id on ...
    if("company_id" %in% colnames(si_si_tbl_df)) {

      "company_id" -> keys
      str_c(c("dateindex",keys), collapse = "_") -> si_si_tbl_df_primary_key
      with( si_si_tbl_df, { eval(parse(text=eval(parse(text=('str_c(c("dateindex",keys), collapse = " %s+% \'_\' %s+% ")'))))) } ) -> si_si_tbl_df[,si_si_tbl_df_primary_key]
      DataCombine::MoveFront(si_si_tbl_df,si_si_tbl_df_primary_key) -> si_si_tbl_df

    }
    
    # si_TBL VARIABLES
    assign(si_tbl_i,si_si_tbl_df)
    rm(si_si_tbl_df)
    
    # NOT APPLICABLE
    # # join key of exchange
    # si_exchg$exchg_code  -> si_exchg$exchange
    
    # NOT APPLICABLE
    # # outer join
    # plyr::join_all(list(si_ci,si_exchg), by = c("dateindex","exchange"), type = "full") -> si_all_df
    
    get(si_tbl_i) -> si_all_df
    
    # optimize(si_all_df) -> si_all_df
    
    # # NOT APPLICABLE
    # rm(si_ci)
    # rm(si_exchg)
    
    # # NOT APPLICABLE
    # # join key of industry
    # si_all_df$ind_3_dig -> si_all_df$industry_code
    # si_mgdsc$mg_code  ->   si_mgdsc$industry_code
    
    # # NOT APPLICABLE
    # # left join because mg_code has codes that are sectors and not industries ( creates orphans )
    # # left join becuase orphan industries are meaningless
    # plyr::join_all(list(si_all_df,si_mgdsc), by = c("dateindex","industry_code"), type = "left") -> si_all_df
    # si_all_df$mg_desc -> si_all_df$industry_desc 
    # within( si_all_df, { rm("mg_code","mg_desc") }) -> si_all_df
    # si_all_df$rn_si_mgdsc -> si_all_df$rn_si_mgdsc_ind; within(si_all_df, { rm("rn_si_mgdsc") } ) -> si_all_df 
    # within(  si_mgdsc , { rm("industry_code") }) -> si_mgdsc
    # 
    # # optimize(si_all_df) -> si_all_df
    # 
    # # join key of sector
    # si_all_df$ind_2_dig            ->  si_all_df$sector_code
    # str_sub(si_mgdsc$mg_code,1,2)  ->  si_mgdsc$sector_code
    # 
    # # get rid of duplicated sectors ( each industry has its sector rementioned )
    # si_mgdsc -> si_mgdsc  
    # # SPECIAL
    # si_mgdsc$rn_si_mgdsc -> si_mgdsc$rn_si_mgdsc_sect
    # si_mgdsc[with( si_mgdsc, { !stri_duplicated(sector_code) } ),,drop = FALSE] -> si_mgdsc
    #   
    # # optimize(si_mgdsc,c("mg_code")) -> si_mgdsc
    # 
    # # left join becuase orphan sectors are meaningless
    # plyr::join_all(list(si_all_df,si_mgdsc), by = c("dateindex","sector_code"), type = "left")  -> si_all_df
    # si_all_df$mg_desc -> si_all_df$sector_desc 
    # within( si_all_df, { rm("mg_code","mg_desc") }) -> si_all_df
    # # SPECIAL 
    # within(si_all_df, { rm("rn_si_mgdsc") } ) -> si_all_df
    # within(  si_mgdsc , { rm("sector_code") }) -> si_mgdsc
    # 
    
    # KEEP
    financize(si_all_df) -> si_all_df
    
    # NOT APPLICABLE
    # # optimize(si_all_df) -> si_all_df
    # 
    # rm("si_mgdsc")   
    
    return(si_all_df) ## (no business error) ## BUSINESS LOGIC ERROR HERE: (RETURNS too_early(after first loop)) SHOULD INSTEAD 'ADD TO A 'LIST OF DATA.FRAMES'

  }
  ret <- verify_company_details_inner(dateindex = dateindex, table_f = table_f, cnames_e = cnames_e)
                                      
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
}

# alter table if exists si_finecon2 drop column if exists dateindexp38lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp38eom;
# alter table if exists si_finecon2 drop column if exists dateindexp37lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp37eom;
# alter table if exists si_finecon2 drop column if exists dateindexp36lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp36eom;
# alter table if exists si_finecon2 drop column if exists dateindexp35lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp35eom;
# alter table if exists si_finecon2 drop column if exists dateindexp34lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp34eom;
# alter table if exists si_finecon2 drop column if exists dateindexp33lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp33eom;
# alter table if exists si_finecon2 drop column if exists dateindexp32lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp32eom;
# alter table if exists si_finecon2 drop column if exists dateindexp31lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp31eom;
# alter table if exists si_finecon2 drop column if exists dateindexp30lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp30eom;
# alter table if exists si_finecon2 drop column if exists dateindexp29lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp29eom;
# alter table if exists si_finecon2 drop column if exists dateindexp28lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp28eom;
# alter table if exists si_finecon2 drop column if exists dateindexp27lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp27eom;
# alter table if exists si_finecon2 drop column if exists dateindexp26lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp26eom;
# alter table if exists si_finecon2 drop column if exists dateindexp25lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp25eom;
# alter table if exists si_finecon2 drop column if exists dateindexp24lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp24eom;
# alter table if exists si_finecon2 drop column if exists dateindexp23lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp23eom;
# alter table if exists si_finecon2 drop column if exists dateindexp22lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp22eom;
# alter table if exists si_finecon2 drop column if exists dateindexp21lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp21eom;
# alter table if exists si_finecon2 drop column if exists dateindexp20lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp20eom;
# alter table if exists si_finecon2 drop column if exists dateindexp19lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp19eom;
# alter table if exists si_finecon2 drop column if exists dateindexp18lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp18eom;
# alter table if exists si_finecon2 drop column if exists dateindexp17lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp17eom;
# alter table if exists si_finecon2 drop column if exists dateindexp16lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp16eom;
# alter table if exists si_finecon2 drop column if exists dateindexp15lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp15eom;
# alter table if exists si_finecon2 drop column if exists dateindexp14lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp14eom;
# alter table if exists si_finecon2 drop column if exists dateindexp13lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp13eom;
# alter table if exists si_finecon2 drop column if exists dateindexp12lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp12eom;
# alter table if exists si_finecon2 drop column if exists dateindexp11lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp11eom;
# alter table if exists si_finecon2 drop column if exists dateindexp10lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp10eom;
# alter table if exists si_finecon2 drop column if exists dateindexp09lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp09eom;
# alter table if exists si_finecon2 drop column if exists dateindexp08lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp08eom;
# alter table if exists si_finecon2 drop column if exists dateindexp07lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp07eom;
# alter table if exists si_finecon2 drop column if exists dateindexp06lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp06eom;
# alter table if exists si_finecon2 drop column if exists dateindexp05lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp05eom;
# alter table if exists si_finecon2 drop column if exists dateindexp04lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp04eom;
# alter table if exists si_finecon2 drop column if exists dateindexp03lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp03eom;
# alter table if exists si_finecon2 drop column if exists dateindexp02lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp02eom;
# alter table if exists si_finecon2 drop column if exists dateindexp01lwd;
# alter table if exists si_finecon2 drop column if exists dateindexp01eom;
# alter table if exists si_finecon2 drop column if exists dateindexf01lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf01eom;
# alter table if exists si_finecon2 drop column if exists dateindexf02lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf02eom;
# alter table if exists si_finecon2 drop column if exists dateindexf03lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf03eom;
# alter table if exists si_finecon2 drop column if exists dateindexf04lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf04eom;
# alter table if exists si_finecon2 drop column if exists dateindexf05lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf05eom;
# alter table if exists si_finecon2 drop column if exists dateindexf06lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf06eom;
# alter table if exists si_finecon2 drop column if exists dateindexf07lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf07eom;
# alter table if exists si_finecon2 drop column if exists dateindexf08lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf08eom;
# alter table if exists si_finecon2 drop column if exists dateindexf09lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf09eom;
# alter table if exists si_finecon2 drop column if exists dateindexf10lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf10eom;
# alter table if exists si_finecon2 drop column if exists dateindexf11lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf11eom;
# alter table if exists si_finecon2 drop column if exists dateindexf12lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf12eom;
# alter table if exists si_finecon2 drop column if exists dateindexf13lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf13eom;
# alter table if exists si_finecon2 drop column if exists dateindexf14lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf14eom;
# alter table if exists si_finecon2 drop column if exists dateindexf15lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf15eom;
# alter table if exists si_finecon2 drop column if exists dateindexf16lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf16eom;
# alter table if exists si_finecon2 drop column if exists dateindexf17lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf17eom;
# alter table if exists si_finecon2 drop column if exists dateindexf18lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf18eom;
# alter table if exists si_finecon2 drop column if exists dateindexf19lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf19eom;
# alter table if exists si_finecon2 drop column if exists dateindexf20lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf20eom;
# alter table if exists si_finecon2 drop column if exists dateindexf21lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf21eom;
# alter table if exists si_finecon2 drop column if exists dateindexf22lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf22eom;
# alter table if exists si_finecon2 drop column if exists dateindexf23lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf23eom;
# alter table if exists si_finecon2 drop column if exists dateindexf24lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf24eom;
# alter table if exists si_finecon2 drop column if exists dateindexf25lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf25eom;
# alter table if exists si_finecon2 drop column if exists dateindexf26lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf26eom;
# alter table if exists si_finecon2 drop column if exists dateindexf27lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf27eom;
# alter table if exists si_finecon2 drop column if exists dateindexf28lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf28eom;
# alter table if exists si_finecon2 drop column if exists dateindexf29lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf29eom;
# alter table if exists si_finecon2 drop column if exists dateindexf30lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf30eom;
# alter table if exists si_finecon2 drop column if exists dateindexf31lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf31eom;
# alter table if exists si_finecon2 drop column if exists dateindexf32lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf32eom;
# alter table if exists si_finecon2 drop column if exists dateindexf33lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf33eom;
# alter table if exists si_finecon2 drop column if exists dateindexf34lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf34eom;
# alter table if exists si_finecon2 drop column if exists dateindexf35lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf35eom;
# alter table if exists si_finecon2 drop column if exists dateindexf36lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf36eom;
# alter table if exists si_finecon2 drop column if exists dateindexf37lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf37eom;
# alter table if exists si_finecon2 drop column if exists dateindexf38lwd;
# alter table if exists si_finecon2 drop column if exists dateindexf38eom;



verify_return_dates <- function(dateindex = NULL, months_limit = NULL) {
  
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
  
  verify_return_dates_inner <- function (dateindex = NULL, months_limit = NULL) {
    
    verify_si_finecon_exists()
    
    require(magrittr)
    require(stringi)
    require(stringr)
    require(lubridate)
    # uses zoo::as.Date.integer, zoo::as.Date.yearmon, zoo::as.yearmon
    # uses Hmisc::Hmisc::trunc.POSIXt
    
    if(is.null(months_limit)) stop("must supply a number of months: e.g. months_limit = 38")

    # create the data.frame
    
    1:months_limit -> both_months_range 

    both_months_range ->   future_months_range
                    length(future_months_range) -> len_direction_months_range

    c(rev(future_months_range),future_months_range) -> past_and_future_months_range
      length(past_and_future_months_range)          -> len_past_and_future_months_range

    rep(c('p','f'), each = len_past_and_future_months_range) %>%
      # TWO_m per month
      str_c(., rep(past_and_future_months_range, each= 2) %>% str_pad(.,2,'left','0') ) %>%
        # suffix 'eom','lwd'
        str_c( .,rep(c('lwd','eom'), times = len_past_and_future_months_range)) %>%
          # prefix 'dateindex'
          str_c('dateindex', .) -> column_names
    
    c("dateindex", "dateindexlwd", "dateindexeom", column_names) -> all_col_names 
    
    # eval(parse(text=str_c("data.frame(dateindex=integer(), dateindexlwd=integer(), dateindexeom=integer()," %s+% str_c(column_names,"=integer()",collapse = ", ") %s+% ")"))) -> si_all_df
    
    # fill the data.frame
    
    dateindex -> now_date
    zoo::as.Date(now_date) -> now_date

    # this month
    now_date %m+% months(1) %>%
      # 1st day of next month # last day of this month
      Hmisc::trunc.POSIXt(., units='months') %m+% days(-1) %>% 
        zoo::as.Date(.)  %>%
          # add in lwd ( Sat or Sun falls back to Fri)
          lapply(.,function(x) { (x - match(weekdays(x), c('Saturday','Sunday'), nomatch = 0)) %>% 
                                  # lwd, eom
                                  c(.,x)
                               } ) %>% 
            # flattened (Date class is stripped)
            unlist(.) %>% zoo::as.Date(.) -> now_lwd_eom_dates

    # past lwd eom dates

    # previous Nth(months_limit) month back
    now_date %m+% months(-months_limit) %>%
      # 1st day of previous Nth month
      Hmisc::trunc.POSIXt(., units='months')  %>%
        # N(months_limit) or so months
        seq(., by = 'month', length.out = months_limit) %m+% 
          # last day of month
          months(1) %m+% days(-1) %>% 
            zoo::as.Date(.) %>% 
              # add in lwd ( Sat or Sun falls back to Fri)
              lapply(.,function(x) { (x - match(weekdays(x), c('Saturday','Sunday'), nomatch = 0)) %>% 
                                     # lwd, eom
                                     c(.,x)
                                   } ) %>% 
                # flattened (Date class is stripped)
                unlist(.) %>% zoo::as.Date(.) -> past_lwd_eom_dates

    # future lwd eom dates

    # next month
    now_date %m+% months(1) %>%
      # 1st day of next month
      Hmisc::trunc.POSIXt(., units='months') %>%
        # N(months_limit) or so months
        seq(., by = 'month', length.out = months_limit) %m+% 
          # last day of month
          months(1) %m+% days(-1) %>% 
            zoo::as.Date(.) %>% 
              # add in lwd ( Sat or Sun falls back to Fri)
              lapply(.,function(x) { (x - match(weekdays(x), c('Saturday','Sunday'), nomatch = 0)) %>% 
                                     # lwd, eom
                                     c(.,x)
                                   } ) %>% 
                # flattened (Date class is stripped)
                unlist(.) %>% zoo::as.Date(.) -> future_lwd_eom_dates


    c(now_date,now_lwd_eom_dates,past_lwd_eom_dates,future_lwd_eom_dates) %>% as.integer(.) -> all_dates

    # actual fill 
    
    eval(parse(text=str_c("data.frame(" %s+% str_c(all_col_names,"=",all_dates, "L" , collapse = ", ") %s+% ")"))) -> si_all_df
    
    return(si_all_df) 

  }
  ret <- verify_return_dates_inner(dateindex = dateindex, months_limit = months_limit)
                                      
  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
}
# verify_return_dates(15155, months_limit = 38)  -> si_all_g_df 
# upsert(si_all_g_df, keys = NULL) # ONLY dateindex is the pk


 
# -- si_date.perend_q1-q8(integer) -- Date ( BUT I think I just need this? )
# -- si_date.perlen_q1-q8(integer)
# -- si_date.pertyp_q1-q8
# 
# requires 
#   dateindexf##lwd, price, prchg_##w, perend_q#, dps_q#
# 
verify_week_often_week_returns <- function(dateindex = NULL) {

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
  
  verify_week_often_week_returns_inner <- function (dateindex = NULL) {
    
    verify_connection()
    if(!dbExistsTable(con, "si_finecon2")) {
      verify_si_finecon_exists()
    }

    require(RPostgreSQL)
    require(PivotalR)
    
    require(stringi)
    require(stringr)
    
    # to what is in the database
    db.data.frame("si_finecon2", conn.id = cid, verbose = FALSE) -> ptr_si_finecon2  # class (db.#)
    col.types(ptr_si_finecon2) -> fc_meta
    names(ptr_si_finecon2)     -> names(fc_meta)
    
    # requires 
    #   dateindexf##lwd, price, prchg_##w, perend_q#, dps_q# 
    # 
    c("dateindexf01lwd","price","prchg_04w","perend_q1","dps_q1") -> sample_columns
    
    if(any(!sample_columns %in% names(fc_meta))) { 
      warning("Minumum columns are missing.") 
      warning("Sample Missing columns are the following:"  %s+% ' ' %s+% str_c(sample_columns[!sample_columns %in% names(fc_meta)], collapse = " "))
      browser()
    } 
    
    "
    select 
    --
    fe.dateindex,
    fe.dateindex_company_id_orig,
    fe.company_id,
    --
    fe_04w_o.pricebck_04w,
    fe_04w_o.prchg_04w_ann,
    fe_04w_o.pct_div_ret_ov_pr_04w_q1_ann,
    fe_04w_o.pradchg_04w_ann,
    fe_04w_o.price_04w,
    --
    fe_13w_o.pricebck_13w,
    fe_13w_o.prchg_13w_ann,
    fe_13w_o.pct_div_ret_ov_pr_13w_q1_ann,
    fe_13w_o.pradchg_13w_ann,
    fe_13w_o.price_13w,
    --
    fe_26w_o.pricebck_26w,
    fe_26w_o.prchg_26w_ann,
    fe_26w_o.pct_div_ret_ov_pr_26w_q12_ann,
    fe_26w_o.pradchg_26w_ann,
    fe_26w_o.price_26w,
    --
    fe_52w_o.pricebck_52w,
    fe_52w_o.prchg_52w_ann,
    fe_52w_o.pct_div_ret_ov_pr_52w_q1234_ann,
    fe_52w_o.pradchg_52w_ann,
    fe_52w_o.price_52w
    from 
      si_finecon2 fe 
    left join lateral ( 
        select 
          fe_04w.dateindex, fe_04w.company_id,
            nullif(fe_04w.price,0)/(nullif(fe_04w.prchg_04w,-100)/100 + 1)  pricebck_04w,
            fe_04w.prchg_04w * 12                                           prchg_04w_ann,
                                    30.5 / ( fe_04w.perend_q1 - fe_04w.perend_q2 ) * ( coalesce(fe_04w.dps_q1,0)    )/(nullif(fe_04w.price,0)/(nullif(fe_04w.prchg_04w,-100)/100 + 1)) * 100 * 12 pct_div_ret_ov_pr_04w_q1_ann,
            fe_04w.prchg_04w * 12 + 30.5 / ( fe_04w.perend_q1 - fe_04w.perend_q2 ) * ( coalesce(fe_04w.dps_q1,0)    )/(nullif(fe_04w.price,0)/(nullif(fe_04w.prchg_04w,-100)/100 + 1)) * 100 * 12 pradchg_04w_ann,
            fe_04w.price price_04w
          from 
              si_finecon2 fe_04w
    ) fe_04w_o on fe.dateindexf01lwd  = fe_04w_o.dateindex and fe.company_id = fe_04w_o.company_id                              
    left join lateral ( 
        select 
          fe_13w.dateindex, fe_13w.company_id,
            nullif(fe_13w.price,0)/(nullif(fe_13w.prchg_13w,-100)/100 + 1)  pricebck_13w,
            fe_13w.prchg_13w * 4                                            prchg_13w_ann,
                                    91.0 / ( fe_13w.perend_q1 - fe_13w.perend_q2 ) * ( coalesce(fe_13w.dps_q1,0)    )/(nullif(fe_13w.price,0)/(nullif(fe_13w.prchg_13w,-100)/100 + 1)) * 100 *  4 pct_div_ret_ov_pr_13w_q1_ann,
            fe_13w.prchg_13w * 4 +  91.0 / ( fe_13w.perend_q1 - fe_13w.perend_q2 ) * ( coalesce(fe_13w.dps_q1,0)    )/(nullif(fe_13w.price,0)/(nullif(fe_13w.prchg_13w,-100)/100 + 1)) * 100 *  4 pradchg_13w_ann,
            fe_13w.price price_13w
          from 
              si_finecon2 fe_13w
    ) fe_13w_o on fe.dateindexf03lwd  = fe_13w_o.dateindex and fe.company_id = fe_13w_o.company_id 
    left join lateral ( 
        select 
          fe_26w.dateindex, fe_26w.company_id,
            nullif(fe_26w.price,0)/(nullif(fe_26w.prchg_26w,-100)/100 + 1)  pricebck_26w,
            fe_26w.prchg_26w * 2                                            prchg_26w_ann,
                                    182.0 / ( fe_26w.perend_q1 - fe_26w.perend_q3 ) * ( coalesce(fe_26w.dps_q1,0) + 
                                                                                        coalesce(fe_26w.dps_q2,0)    )/(nullif(fe_26w.price,0)/(nullif(fe_26w.prchg_26w,-100)/100 + 1)) * 100 *  2 pct_div_ret_ov_pr_26w_q12_ann,
            fe_26w.prchg_26w * 2 +  182.0 / ( fe_26w.perend_q1 - fe_26w.perend_q3 ) * ( coalesce(fe_26w.dps_q1,0) + 
                                                                                        coalesce(fe_26w.dps_q2,0)    )/(nullif(fe_26w.price,0)/(nullif(fe_26w.prchg_26w,-100)/100 + 1)) * 100 *  2 pradchg_26w_ann,
            fe_26w.price price_26w
          from 
              si_finecon2 fe_26w
    ) fe_26w_o on fe.dateindexf06lwd  = fe_26w_o.dateindex and fe.company_id = fe_26w_o.company_id 
    left join lateral ( 
        select 
          fe_52w.dateindex, fe_52w.company_id,
            nullif(fe_52w.price,0)/(nullif(fe_52w.prchg_52w,-100)/100 + 1)  pricebck_52w,
            fe_52w.prchg_52w * 1                                            prchg_52w_ann,
                                    365.0 / ( fe_52w.perend_q1 - fe_52w.perend_q5 ) * ( coalesce(fe_52w.dps_q1,0) + 
                                                                                        coalesce(fe_52w.dps_q2,0) + 
                                                                                        coalesce(fe_52w.dps_q3,0) + 
                                                                                        coalesce(fe_52w.dps_q4,0)    )/(nullif(fe_52w.price,0)/(nullif(fe_52w.prchg_52w,-100)/100 + 1)) * 100 *  1 pct_div_ret_ov_pr_52w_q1234_ann,
            fe_52w.prchg_52w * 1 +  365.0 / ( fe_52w.perend_q1 - fe_52w.perend_q5 ) * ( coalesce(fe_52w.dps_q1,0) + 
                                                                                        coalesce(fe_52w.dps_q2,0) + 
                                                                                        coalesce(fe_52w.dps_q3,0) + 
                                                                                        coalesce(fe_52w.dps_q4,0)    )/(nullif(fe_52w.price,0)/(nullif(fe_52w.prchg_52w,-100)/100 + 1)) * 100 *  1 pradchg_52w_ann,
            fe_52w.price price_52w
          from 
              si_finecon2 fe_52w
    ) fe_52w_o on fe.dateindexf12lwd  = fe_52w_o.dateindex and fe.company_id = fe_52w_o.company_id 
    where fe.dateindex = " %s+% dateindex %s+% ";" -> add_columns_sql

    db.q(add_columns_sql, nrows = -1, conn.id = cid) -> si_all_df

    # KEEP
    financize(si_all_df) -> si_all_df
    
    return(si_all_df) 

  }
  ret <- verify_week_often_week_returns_inner(dateindex = dateindex)

  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
}   



# requires (above)
#    price_m001 through price_m017
verify_month_often_month_past_returns <- function(dateindex = NULL, months_limit = NULL) {

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
  
  verify_month_often_month_past_returns_inner <- function (dateindex = NULL) {
    
    verify_connection()
    if(!dbExistsTable(con, "si_finecon2")) {
      verify_si_finecon_exists()
    }

    require(magrittr)

    require(RPostgreSQL)
    require(PivotalR)
    
    require(stringi)
    require(stringr)
    require(R.rsp)
    
    # uses clean_text
    
    # to what is in the database
    db.data.frame("si_finecon2", conn.id = cid, verbose = FALSE) -> ptr_si_finecon2  # class (db.#)
    col.types(ptr_si_finecon2) -> fc_meta
    names(ptr_si_finecon2)     -> names(fc_meta)
    
    # requires 
    #   dateindexf##lwd, price, prchg_##w, perend_q#, dps_q# 
    # 
    c("price_m001") -> sample_columns
    
    if(any(!sample_columns %in% names(fc_meta))) { 
      warning("Minumum columns are missing.") 
      warning("Sample Missing columns are the following:"  %s+% ' ' %s+% str_c(sample_columns[!sample_columns %in% names(fc_meta)], collapse = " "))
      browser()
    } 
    
    (months_limit-1) -> n 
    
    # NOT USING ( SHOULD HAVE USED instead )
    # verify_company_details(dateindex = c(dir_i),  table_f = "si_psdc", cnames_e = "^price_m00[1-9]$|^price_m01[0-7]$")
    
    # # writeLines({
    # str_trim(str_c(rstring('
    # alter table if exists si_finecon2
    #   <% for (i in 1:n) { -%>
    #       <% sprintf("drop if exists m%1$s_m%2$s_prchg_ann", 
    #            str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
    #       -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
    #   <% } %>
    # ; 
    # '))) %>% clean_text(.) -> remove_columns_sql
    # # })
    # # writeLines(remove_columns_sql)
    # 
    # db.q(remove_columns_sql, conn.id = cid)
    
    # # writeLines({
    # str_trim(str_c(rstring('
    # alter table if exists si_finecon2
    #   <% for (i in 1:n) { -%>
    #       <% sprintf("add if not exists m%1$s_m%2$s_prchg_ann numeric(8,2)", 
    #            str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
    #       -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
    #   <% } %>
    # ; 
    # '))) %>% clean_text(.) -> add_columns_sql 
    # # })
    # # writeLines(add_columns_sql)
    # 
    # db.q(add_columns_sql, conn.id = cid)
    
    # # UNTRIED ( SELF-JOIN - MAY? WORK?) ( COULD BE SIMPLER: JUST *ONE* TABLE)
    #
    # # writeLines({
    # str_trim(str_c(rstring('
    # update si_finecon2 fc set
    #   <% for (i in 1:n) { -%>
    #       <% sprintf("m%1$s_m%2$s_prchg_ann = sq.m%1$s_m%2$s_prchg_ann", 
    #            str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
    #       -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
    #   <% } %>
    # from (
    # select 
    #   <% for (i in 1:n) { -%>
    #       <% sprintf("(price_m%1$s - price_m%2$s) / nullif(abs(price_m%2$s),0) * 100 * 12 m%1$s_m%2$s_prchg_ann", 
    #           str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
    #       -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
    #   <% } %>
    #   from si_finecon2
    # ) sq
    # where fc.dateindex_company_id_orig = sq.dateindex_company_id_orig and fe.dateindex = ' %s+% dateindex %s+%  ';
    # '))) %>% clean_text(.) -> update_columns_sql
    # # })
    # # writeLines(update_columns_sql)
    # 
    # db.q(update_columns_sql, conn.id = cid)) # nrows =  -1, 
    
    # writeLines({
    str_trim(str_c(rstring('
    select fe.dateindex, fe.dateindex_company_id_orig, fe.company_id,
      <% for (i in 1:n) { -%>
          <% sprintf("(price_m%1$s - price_m%2$s) / nullif(abs(price_m%2$s),0) * 100 * 12 m%1$s_m%2$s_prchg_ann", 
              str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
          -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
      <% } %>
      from si_finecon2 fe
    where fe.dateindex = ' %s+% dateindex %s+%  ';
    '))) %>% clean_text(.) -> add_columns_sql
    # })
    # writeLines(add_columns_sql)
    
    db.q(add_columns_sql, nrows = -1, conn.id = cid) -> si_all_df

    # KEEP
    financize(si_all_df) -> si_all_df
    
    return(si_all_df) 

  }
  ret <- verify_month_often_month_past_returns_inner(dateindex = dateindex)

  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
} 



# DECIDED that THESE functions will process ONLY one DATEINDEX at a time
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_psd", cnames_e = "^mktcap$") -> si_all_g_df
# ... X ( does not have a ticker ) update_from_future_new_company_ids(df = si_all_g_df, ref = 15155) -> si_all_g_df  ... x
# ... upsert(si_all_g_df, keys = c("company_id")) 
# 

# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid")))
# debugSource('W:/R-3.3._/finecon01.R')
# 

# #

# verify_company_basics(dateindex = c(15155)) -> si_all_g_df
# update_from_future_new_company_ids(df = si_all_g_df, ref = 15155) -> si_all_g_df # always run after a verify(load)
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_psd", cnames_e = "^price$|^mktcap$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id")) 
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_psd", cnames_e = "^prchg_\\d\\dw$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_return_dates(dateindex = c(15155), months_limit = 38)  -> si_all_g_df 
# upsert(si_all_g_df, keys = NULL) # ONLY dateindex is the pk
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_isq", cnames_e = "^dps_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#
# verify_company_details(dateindex = c(15155),  table_f = "si_date", cnames_e = "^perend_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_isq", cnames_e = "^sales_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#
# verify_company_details(dateindex = c(15155),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#
# verify_week_often_week_returns(15155) -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_psdc", cnames_e = "^price_m00[1-9]$|^price_m01[0-7]$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 

# # prchg_ # > zoo::as.Date(15155) [1] "2011-06-30" > zoo::as.Date(15184) [1] "2011-07-29" > zoo::as.Date(15217) [1] "2011-08-31" > zoo::as.Date(15247) [1] "2011-09-30"

# verify_company_basics(dateindex = c(15184)) -> si_all_g_df
# update_from_future_new_company_ids(df = si_all_g_df, ref = 15184) -> si_all_g_df 
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15184),  table_f = "si_psd", cnames_e = "^price$|^mktcap$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id")) 
# 
# verify_company_details(dateindex = c(15184),  table_f = "si_psd", cnames_e = "^prchg_\\d\\dw$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_return_dates(dateindex = c(15184), months_limit = 38)  -> si_all_g_df 
# upsert(si_all_g_df, keys = NULL) # ONLY dateindex is the pk
# 
# verify_company_details(dateindex = c(15184),  table_f = "si_isq", cnames_e = "^dps_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))  
#
# verify_company_details(dateindex = c(15184),  table_f = "si_date", cnames_e = "^perend_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15184),  table_f = "si_isq", cnames_e = "^sales_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15184),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_week_often_week_returns(15184) -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id")) 
# 
# verify_company_details(dateindex = c(15184),  table_f = "si_psdc", cnames_e = "^price_m00[1-9]$|^price_m01[0-7]$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#

# #

# verify_company_basics(dateindex = c(15217)) -> si_all_g_df
# update_from_future_new_company_ids(df = si_all_g_df, ref = 15217) -> si_all_g_df 
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15217),  table_f = "si_psd", cnames_e = "^price$|^mktcap$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id")) 
# 
# verify_company_details(dateindex = c(15217),  table_f = "si_psd", cnames_e = "^prchg_\\d\\dw$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_return_dates(dateindex = c(15217), months_limit = 38)  -> si_all_g_df 
# upsert(si_all_g_df, keys = NULL) # ONLY dateindex is the pk
# 
# verify_company_details(dateindex = c(15217),  table_f = "si_isq", cnames_e = "^dps_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id")) 
# 
# verify_company_details(dateindex = c(15217),  table_f = "si_date", cnames_e = "^perend_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15217),  table_f = "si_isq", cnames_e = "^sales_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15217),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_week_often_week_returns(15217) -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#
# verify_company_details(dateindex = c(15217),  table_f = "si_psdc", cnames_e = "^price_m00[1-9]$|^price_m01[0-7]$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#

# #

# verify_company_basics(dateindex = c(15247)) -> si_all_g_df
# update_from_future_new_company_ids(df = si_all_g_df, ref = 15247) -> si_all_g_df 
# upsert(si_all_g_df, keys = c("company_id")) # HERE #
# 
# verify_company_details(dateindex = c(15247),  table_f = "si_psd", cnames_e = "^price$|^mktcap$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id")) 
# 
# verify_company_details(dateindex = c(15247),  table_f = "si_psd", cnames_e = "^prchg_\\d\\dw$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_return_dates(dateindex = c(15247), months_limit = 38)  -> si_all_g_df
# upsert(si_all_g_df, keys = NULL) # ONLY dateindex is the pk
# 
# verify_company_details(dateindex = c(15247),  table_f = "si_isq", cnames_e = "^dps_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#
# verify_company_details(dateindex = c(15247),  table_f = "si_date", cnames_e = "^perend_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#
# verify_company_details(dateindex = c(15247),  table_f = "si_isq", cnames_e = "^sales_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15247),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#
# verify_week_often_week_returns(15247) -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
# 
# verify_company_details(dateindex = c(15247),  table_f = "si_psdc", cnames_e = "^price_m00[1-9]$|^price_m01[0-7]$") -> si_all_g_df
# upsert(si_all_g_df, keys = c("company_id"))
#

                                                                                                # NO CHECK: I must verify
                                                                                                # that (1) exists AND (2) lwd
upload_lwd_sipro_dbfs_to_db <- function(from_dir = "W:/AAIISIProDBFs", months_only_back = NULL, exact_lwd_dbf_dirs = NULL) {

  ops <- options()
  options(warn=1) # If 'warn' is one, warnings are printed as they occur. ( Because I can not print colors )
  
  if(is.null(exact_lwd_dbf_dirs)){
    as.integer(dir(from_dir))         ->     all_dbf_dirs
    is_lwd_of_month(all_dbf_dirs)     -> lwd_all_dbf_dirs_tf 
    all_dbf_dirs[lwd_all_dbf_dirs_tf] ->     lwd_dbf_dirs
  } else {
                      exact_lwd_dbf_dirs -> lwd_dbf_dirs
  }
  
  # latest to earliest 
  # NOTE: any *new* month, I have to iterate back (months_only_back = 13) 13 months to calculate any *new* future returns
  # 
  # index of lwd months
  seq_along(lwd_dbf_dirs) -> lwd_months_idx
  if(is.null(months_only_back)) { 
    # everything
    sort(lwd_dbf_dirs, decreasing = TRUE)[lwd_months_idx]  -> lwd_dbf_dirs_ordered
  } else {
    # just the *new* month and the previous 12 months redone (months_only_back = 13)
    sort(lwd_dbf_dirs, decreasing = TRUE)[head(lwd_months_idx,months_only_back)]  -> lwd_dbf_dirs_ordered
  }
  
  for(dir_i in lwd_dbf_dirs_ordered) {
    
    warning(paste0("Beginning disk dbf dir: ",dir_i))
    
    verify_company_basics(dateindex = c(dir_i)) -> si_all_g_df
    update_from_future_new_company_ids(df = si_all_g_df, ref = dir_i) -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id")) # HERE #

    verify_company_details(dateindex = c(dir_i),  table_f = "si_psd", cnames_e = "^price$|^mktcap$") -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))

    verify_company_details(dateindex = c(dir_i),  table_f = "si_psd", cnames_e = "^prchg_\\d\\dw$") -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))

    verify_return_dates(dateindex = c(dir_i), months_limit = 38)  -> si_all_g_df
    upsert(si_all_g_df, keys = NULL) # ONLY dateindex is the pk

    verify_company_details(dateindex = c(dir_i),  table_f = "si_isq", cnames_e = "^dps_q.$") -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))

    verify_company_details(dateindex = c(dir_i),  table_f = "si_date", cnames_e = "^perend_q.$") -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))

    # requires
    #   dateindexf##lwd, price, prchg_##w, perend_q#, dps_q#
    verify_week_often_week_returns(dir_i) -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))

    verify_company_details(dateindex = c(dir_i),  table_f = "si_psdc", cnames_e = "^price_m00[1-9]$|^price_m01[0-7]$") -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))
    
    # requires (above)
    #    price_m001 through price_m017
    verify_month_often_month_past_returns(dir_i,  months_limit = 17) -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))
    
    verify_company_details(dateindex = c(dir_i),  table_f = "si_isq", cnames_e = "^sales_q.$") -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))

    verify_company_details(dateindex = c(dir_i),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
    upsert(si_all_g_df, keys = c("company_id"))
    
    warning(paste0("Ending disk dbf dir: ",dir_i))

  }
  
  options(ops)
  
  return(invisible())
  
}

# upload_lwd_sipro_dbfs_to_db()
# upload_lwd_sipro_dbfs_to_db(months_only_back = 13)
# upload_lwd_sipro_dbfs_to_db(exact_lwd_dbf_dirs = 16678) 

# untried BUT truncate table is BETTER for company_id/ticker SYSTEM change PROBLEMS
# upload_lwd_sipro_dbfs_to_db(exact_lwd_dbf_dirs = sort(all_load_days_lwd[all_load_days_lwd <= (15155 + 400)], decreasing = TRUE))



#### BEGIN WORKFLOW ####

# ANDRE NEW PLAN - INBOUND
# ------------------------
# Inbound Sales/Market,Net_Income/Market,Net_Income/Sales  by (since last time, since last year this time)
# INBOUND DATA  
#   missing data
#     na.locf
#     flag column of na.locf
#     time in days since last measure ( higher values for na.locf ) ( see my Philadelpha/Cleveland ... "Forecasters" )
# OTHER
# Real_Sharp of monthly stock returns ('risk')
# Gold & Silver sector returns ('fear')
#   Real_Sharp of Gold & Silver sector returns('risk')
# Competition('Return on bonds')
#   Real_Sharp of Competition('risk')
# Inflation ('free growth') [ over Competition('Return on bonds')]
# Ratio of Stock_Risk/Bond_Risk
# Ratio of Stock_Return/Bond_Return
# PREDICT on ONE only ... loop predict(1); rbind(1) end loop
# 
# ANDRE NEW PLAN - PROCESSING and OUTBOUND
# ----------------------------------------
# DATA, DATA-CLEANING, PREDICTION
# 
# require(tidyquant); require(timekit); my Zachary Ma
# require(wrapr)
# require(Boruta)
# require(UBL)
# require(vtreat)
# 
# workflow
# --------
# 
# add [my] transforms(<none>/ROLLING/SMA/PCTCHG/LAG[/CORRELATION]) &+ _WEIGHTED .. &+ _12_MO_SEASONAL-> 
#   add mead-adj-factor-vars(e..g. codings) and/or trendish_time-vars ->
#     MANUALLY: eliminate non-[near]future-able data) -> 
#       vtreat(eliminate bad luck)
#         Boruta (variable elimination: eliminate luck: permutes variable values to see if any effect)  
#           SMOTE imbalanced -> balanced ( SMOTE for Regression library(UBL) See Torgo )
#             vtreat(eliminate bad luck) -> CUSTOM resamples -> model [cross] validation -> find 'optimal parameters'
#               COMBO
#               model feature selection (PROB not USE: 'boost'(immune)/'random forest'(immune-never overfit?)
#               observation/class weights (error rates) (model specific)
#                 model prediction(newdata(recent))

#### END WORKFLOW ####




#### MONTHLY METHOD BEGINS ####

# -------------------------------------------------------------------
# ---- begin VERY EARLY MORNING MON APR 03 2017, SUN MAY 29 2017 ----
# 
# Andre_Mikulec@Hotmail.com
# 
# <short_usual>+<code>+<code>WITH RULES(JUST IN TOP OF FILE)
#   Andre_R_AAII_SIPro_rcom_AppActivate_RDCOMClient_SIPro_40_Install.txt
# 
# C:\PG-9.6._
# C:\PG-9.6._\PG-9.6._.bat
# 
# <windows - all programs>
# P
#  PgAdmin III 1.22
#    PgAdmin III
#     postgres_5432__finance_econ(localhost:5432) double_click
#       finance_econ
#         fe_data_store
#           si_finecon2
# 
#     SQL Toolbar Icon
#       File->Recent Files
#           W:\R-Portable.3.2.2\App\R-Portable\bin\x64\RDebug\Home
# scratch.txt
# scratch_queries.txt
# partitions.SCRATCH.sql
# partitions.SCRATCH_finalizing.sql
# TEMPORARY.sql
# 
#         execute paragraph
#           set search_path to sipro_data_store,sipro_stage;
#           set ...
#           ...
#       File->New Window ( repeat 4 more times)
#      
# # old Excel spreadsheet program (*.xls)
# 
# W:\R-Portable.3.2.2\App\R-Portable\bin\x64\
# W:\R-Portable.3.2.2\App\R-Portable\bin\x64\START_R.bat
# > shell("rstudio", wait = FALSE)                           # YELLOW 
# W:\R-Portable.3.2.2\App\R-Portable\bin\x64\RDebug\Home (FILES in HERE)
# RStudio
# > debugSource(paste0(getwd(),'/data_loading_with_Excel_4.R'))
# 
# # new sipro data loader program into PostgreSQL
# 
# W:\R-3.3._
# W:\R-3.3._\R-3.3._.bat
# > shell("rstudio", wait = FALSE)                           # WHITE
# W:\R-3.3._                                             (FILES in HERE) : finecon01.R  finecon01_more_SQL.sql
# > debugSource('W:/R-3.3._/finecon01.R')
# 
# > getAAIISIProDate()
# [1] "17225"
# 
# > zoo::as.Date(as.integer("17225")) # reads directly from # C:/Program Files (x86)/Stock Investor/Professional/Setup.dbf
# [1] "2017-02-28"                                          # MUST BE THE 'last weekday of the lastmonth' (CAN! (and has_been! Christmas))
# 
# Fri Mar 31
# stockinvestorinstall_20170331.exe
# --6465
# 
# > getAAIISIProDate()
# [1] "17256" - new
# 
# # will create the folder
# copyAAIISIProDBFs(
#     from = "C:/Program Files (x86)/Stock Investor/Professional"
#   , to   = paste0("W:/AAIISIProDBFs/",getAAIISIProDate()) # 
# )
# 
# # view last months data
# set search_path to sipro_data_store,sipro_stage;
# set ...
# set ...
# select max(dateindex) from fe_data_store.si_finecon2;
# select count(*) from fe_data_store.si_finecon2 where dateindex = 17225;
# select * from fe_data_store.si_finecon2 where dateindex = 17225;
# 
# W:\R-3.3._
# W:\R-3.3._\R-3.3._.bat
# > shell("rstudio", wait = FALSE)
# RStudio
# > debugSource('W:/R-3.3._/finecon01.R')
# RAN MON MORN APR 03
# > upload_lwd_sipro_dbfs_to_db(months_only_back = 13)
#  ( can take a while: 30 minutes+ ??? )
# DONE
# 
# # relook ( updated future date columns )
# # view last months data
# set search_path to sipro_data_store,sipro_stage;
# set ...
# set ...
# 
# -- begin drill 1
# select max(dateindex) from fe_data_store.si_finecon2;
# 
# select count(*) from fe_data_store.si_finecon2 where dateindex = 17225;
# select * from fe_data_store.si_finecon2 where dateindex = 17225;
# look at 4 colulmns ( should be filled )
# prchg_042_ann, pct_div_ret_ov_pr_04w_q1_ann,pradchg_04w_ann,price_04w
# # new month-data
# select max(dateindex) from fe_data_store.si_finecon2;
# -- end drill 1
# 
# -- begin drill 2
# select max(dateindex) from fe_data_store.si_finecon2;
# -- 17284
# 
# select count(*) from fe_data_store.si_finecon2 where dateindex = 17284;
# 
#                                                                -- early OCT 2016 --
# select distinct dateindex from fe_data_store.si_finecon2 where dateindex > 17074 and dateindexf01lwd is null;
# --NO RECORDS RETURNED
# 
# select distinct dateindex from fe_data_store.si_finecon2;
# --17284 -- latest loaded ( no future data exists )
# 
# -- THIS COULD TAKE much MEMORY
# select * from fe_data_store.si_finecon2 where dateindex = 17284;
# -- 11.8 query + 30 seconds load + 11.3 MB postgresql 
# 
# select * from fe_data_store.si_finecon2 where dateindex = 17256;
# 
# -- current(last) month: just  loaded ( no future data in the database)
# select * from fe_data_store.si_finecon2 where dateindex = 17284;  -- e.g. pricebck_04w IS NULL
# -- previous month ( EXACTLY ONE month of  future data in the database)
# select * from fe_data_store.si_finecon2 where dateindex = 17256;  -- e.g. pricebck_04w IS FILLED
# 
# select * from fe_data_store.si_finecon2 where dateindex in (17256,17284) where ticker = 'AAPL';
# -- end drill 2
# 
# C:\PG-9.6._>"C:\PG-9.6._\bin\pg_ctl" -D "C:\PG-9.6._\data" stop -m fast"
#   any wierd AVG interference? ... then re-cycle PostgreSQL ... make sure
# 
# ---- end VERY EARLY MORNING MON APR 03 2017, SUN MAY 29 2017 ----
# -------------------------------------------------------------

#### MONTHLY METHOD ENDS ####



#### QUERIES BEGIN ####

# -- find out whether the sp500 companies are actually 'doing better' xor 'inflating(bubbling)'
# --
# -- from NOW(17197) and then back one year ( and 3 months ): 16829 - 100
# 
# set search_path to fe_data_store;
# set time zone 'utc';
# --set work_mem to '1200MB';
#   set work_mem to '2047MB';
# set constraint_exclusion = on;
# -- postgresql 9.6
# set max_parallel_workers_per_gather to 4; -- not 'written in docs'
# 
# -- NOTE: (if I want to dig deeper: e.g. per month) per company: perend_q1
# 
# -- dateindex 
# -- dateindex_dt 
# -- sp500_price_mktcap_wtd -- somwhere around 5000
# -- sp500_price_mktcap_wtd_m00_m01_pctchg_ann 
# -- sp500_market_ov_sales 
# -- sp500_market_ov_sales_m00_m01_pctchg_ann 
# -- sp500_market_ov_netinc 
# -- sp500_market_ov_netinc_m00_m01_pctchg_ann 
# -- sp500_sales_ov_netinc 
# -- sp500_sales_ov_netinc_m00_m01_pctchg_ann
# 
# with
# sp500_total_measures as
# (
#   with
#   sp500_total_market as
#   (
#     -- sp500_total_market
#     select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt, sum(coalesce(mktcap,0)) sp500_total_market from 
#     si_finecon2 where dateindex >= 16829 - 100 and company_id in ( select company_id from fe_data_store.si_finecon2 where sp = '500' and dateindex = 16829 )
#     group by dateindex
#   ),
#   sp500_price_mktcap_wtd as (
#     with
#     sp500_total_market as
#     (
#       -- sp500_total_market
#       select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt, sum(coalesce(mktcap,0)) sp500_total_market from 
#       si_finecon2 where dateindex >= 16829 - 100 and company_id in ( select company_id from fe_data_store.si_finecon2 where sp = '500' and dateindex = 16829 )
#       group by dateindex
#     ) 
#     -- sp500_price_mktcap_wtd
#     select fe.dateindex, to_timestamp(fe.dateindex*3600*24)::date dateindex_dt, sum(coalesce(fe.price,0) * coalesce(fe.mktcap,0) / tm.sp500_total_market) sp500_price_mktcap_wtd
#     from 
#       si_finecon2 fe, sp500_total_market tm 
#     where 
#       fe.dateindex >= 16829 - 100 and fe.company_id in ( select company_id from si_finecon2 where sp = '500' and dateindex = 16829 ) and 
#       fe.dateindex = tm.dateindex group by fe.dateindex
#   ),
#   sp500_total_sales as
#   (
#     -- sp500_total_sales
#     select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt, sum(coalesce(sales_q1,0)) sp500_total_sales from 
#     si_finecon2 where dateindex >= 16829 - 100 and company_id in ( select company_id from fe_data_store.si_finecon2 where sp = '500' and dateindex = 16829 )
#     group by dateindex
#   ),
#   sp500_total_netinc as
#   (
#     -- sp500_total_netinc
#     select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt, sum(coalesce(netinc_q1,0)) sp500_total_netinc from 
#     si_finecon2 where dateindex >= 16829 - 100 and company_id in ( select company_id from fe_data_store.si_finecon2 where sp = '500' and dateindex = 16829 )
#     group by dateindex
#   )
#   select tm.dateindex, tm.dateindex_dt
#     , sp500_price_mktcap_wtd
#     , sp500_total_market / sp500_total_sales  sp500_market_ov_sales   -- down-trend is better ( if up-trending, then it is 'over-selling' )
#     , sp500_total_market / sp500_total_netinc sp500_market_ov_netinc  -- down-trend is better ( if up-trending, then it is 'over-selling' )
#     , sp500_total_sales  / sp500_total_netinc sp500_sales_ov_netinc   -- down-trend is better ( if up-trending, them company expenses are relatively increasing )
#   from 
#     sp500_total_market tm,
#     sp500_price_mktcap_wtd wp,
#     sp500_total_sales ts,
#     sp500_total_netinc ni
#   where tm.dateindex = wp.dateindex and wp.dateindex = ts.dateindex and ts.dateindex = ni.dateindex order by tm.dateindex
# ) 
# select te.dateindex, te.dateindex_dt
#   ,      te.sp500_price_mktcap_wtd
#   , (lag(te.sp500_price_mktcap_wtd,0) over te_di - lag(te.sp500_price_mktcap_wtd,1) over te_di) / abs(lag(te.sp500_price_mktcap_wtd,1) over te_di) * 100.00 * 12 sp500_price_mktcap_wtd_m00_m01_pctchg_ann
#   ,      te.sp500_market_ov_sales 
#   , (lag(te.sp500_market_ov_sales,0)  over te_di - lag(te.sp500_market_ov_sales,1)  over te_di) / abs(lag(te.sp500_market_ov_sales,1)  over te_di) * 100.00 * 12 sp500_market_ov_sales_m00_m01_pctchg_ann
#   ,      te.sp500_market_ov_netinc 
#   , (lag(te.sp500_market_ov_netinc,0) over te_di - lag(te.sp500_market_ov_netinc,1) over te_di) / abs(lag(te.sp500_market_ov_netinc,1) over te_di) * 100.00 * 12 sp500_market_ov_netinc_m00_m01_pctchg_ann
#   ,      te.sp500_sales_ov_netinc 
#   , (lag(te.sp500_sales_ov_netinc,0)  over te_di - lag(te.sp500_sales_ov_netinc,1)  over te_di) / abs(lag(te.sp500_sales_ov_netinc,1)  over te_di) * 100.00 * 12 sp500_sales_ov_netinc_m00_m01_pctchg_ann
# from 
#   sp500_total_measures te 
# window 
#   te_di as (order by te.dateindex) 
# order by 
#   te.dateindex
# ;
# --WORKS
# --TODO: [ ] ADD sp500_total_shares mktcap / price ( to demonstrate buyback in action )
# --TODO: [ ] ADD mktvalue weight of those that have reported within the last month
# --          [ ] ADD yoy(q1 v.s. q5) pctchg of those that have reported within the last month
# --TODO: Ratio adjustment for those dateindex that do NOT have exactly 500 firms
# --TODO: [ ] load GetSymbols into the database



# IF I CARE TO expand ... EXCELLENT ARTICLE
# Metrics Maven: Calculating a Moving Average in PostgreSQL
# https://www.compose.com/articles/metrics-maven-calculating-a-moving-average-in-postgresql/



# -- of ALL firms in HISTORY
# -- the 52,26,13,4 week *returns* of *avg* and *sd*
# --   for best sales/mktcap in ranks 1-10 of 1000 ranks
# 
# set search_path to fe_data_store;
# set time zone 'utc';
# --set work_mem to '1200MB';
#   set work_mem to '2047MB';
# set constraint_exclusion = on;
# -- postgresql 9.6
# set max_parallel_workers_per_gather to 4; -- not 'written in docs'
# 
# -- ORIGINAL HAD INSTEAD:  ( sales / shr_aq ) / price   MAY?_HAD_SLIGHTLY_BETTER_RETURNS
# -- ORIGINAL ALSO HAD + : sharpe, sortino, sharp_short, sortino_true, sortino_short, sortino_true_short
# -- ORIGINAL USED 85 SECONDS
# -- 
# select sqe.* from (
#   select 
#       coalesce(sq.dateindex::text,'ALL')                   dateindex  -- aesthetics
#     , count(1)                                             all_ct
#     , count(1)                   filter(where sq.total_sales_ov_total_mktcap is not null) total_sales_ov_total_mktcap_ct
#     , coalesce(sq.total_sales_ov_total_mktcap::text,'ALL')     total_sales_ov_total_mktcap_rnk1000 -- aesthetics
#     , count(sq.pradchg_52w_ann)  filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_52w_ann_ct
#     , avg(sq.pradchg_52w_ann)    filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_52w_ann_avg
#     , stddev_pop(sq.pradchg_52w_ann) filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_52w_ann_sd
#     , count(sq.pradchg_26w_ann)  filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_26w_ann_ct
#     , avg(sq.pradchg_26w_ann)    filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_26w_ann_avg
#     , stddev_pop(sq.pradchg_26w_ann) filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_26w_ann_sd
#     , count(sq.pradchg_13w_ann)  filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_13w_ann_ct
#     , avg(sq.pradchg_13w_ann)    filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_13w_ann_avg
#     , stddev_pop(sq.pradchg_13w_ann) filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_13w_ann_sd
#     , count(sq.pradchg_04w_ann)  filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_04w_ann_ct
#     , avg(sq.pradchg_04w_ann)    filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_04w_ann_avg
#     , stddev_pop(sq.pradchg_04w_ann) filter(where sq.total_sales_ov_total_mktcap is not null) pradchg_04w_ann_sd
#   from ( 
#     select sqi.* from (
#       select 
#           dateindex
#         , to_timestamp(dateindex*3600*24)::date dateindex_dt
#         , ticker 
#         , company
#         , mktcap
#         , industry_desc
#         , sector_desc
#         , trunc(least(case when 
#           (    coalesce(sales_q1,0)  
#              + coalesce(sales_q2,0) 
#           )  / 
#                 nullif( mktcap,0)  
#           is not null then
#           percent_rank() over (partition by dateindex order by ( 
#             ( 
#                 coalesce(sales_q1,0)  
#               + coalesce(sales_q2,0)    
#             )  / 
#                 nullif( mktcap,0)  
#           ) desc nulls last )  
#           else null end,0.99)::numeric * 1000,0)::int + 1 
#           total_sales_ov_total_mktcap
#         , pradchg_04w_ann
#         , pradchg_13w_ann
#         , pradchg_26w_ann
#         , pradchg_52w_ann
#       from si_finecon2_jos
#     ) sqi
#   ) sq group by cube(
#         sq.dateindex
#       , sq.total_sales_ov_total_mktcap
#     )
# ) sqe where sqe.dateindex = 'ALL' 
#       and   sqe.total_sales_ov_total_mktcap_rnk1000 in ('1','2','3','4','5','6','7','8','9','10')
# ;
# WORKS
# 
# --TODO: [ ] ADD/TRY stability of the last 12 months of returns



# -- of ALL firms in HISTORY
# -- UNFINISHED (main QUERY IS DONE) ( meant as to determine the MARGINAL returns of the firms that 
# --  that ONLY have reported WITHIN the last month ( SO now_dateindex >= now_perend_q1 > now_dateindexp01lwd )
# --
# -- determine earlier(erlr): mktcap, price sales_q2, sales_q1, netinc_q1, netinc_q2, erlr_mktcap, erlr_price
# -- print now(now) SAME AS ABOVE and now_perend_q2 and now_perend_q1, now_mktcap, now_price

# set search_path to fe_data_store;
# set time zone 'utc';
# --set work_mem to  '1200MB';
# set work_mem to '2047MB';
# set constraint_exclusion = on;
# -- postgresql 9.6
# set max_parallel_workers_per_gather to 4; -- not 'written in docs'
# 
# select sq.*     -- Index Only Scan
#   from ( select distinct dateindex from si_finecon2 ) curr  
# join lateral  -- for each dateindex,  load in 'current record + previous record', process, then loop to the next dateindex
# (
#   with clump as ( 
#     -- pairs(sets) for performance reasons
#     select * from si_finecon2 now where now.dateindex = curr.dateindex  -- even a WITH statement CAN reference the OUTSIDE of LATERAL
#     union all
#     select * from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp01lwd) dateindexp01lwd from si_finecon2 now where now.dateindex = curr.dateindex ) 
#   )
#   select
#     now.dateindex
#   , now.dateindex now_dateindex
#   , to_timestamp(now.dateindex*3600*24)::date now_dateindex_dt
#   , now.company_id             company_id 
#   , now.company_id             now_company_id 
#   , now.ticker                 now_ticker
#   , now.company                now_company
#   , now.industry_desc          now_industry_desc
#   , now.sector_desc            now_sector_desc
#   , now.perend_q2 now_perend_q2
#   , to_timestamp(now.perend_q2*3600*24)::date now_perend_q2_dt
#   , now.perend_q1 now_perend_q
#   , to_timestamp(now.perend_q1*3600*24)::date now_perend_q1_dt              -- data current(last known of) as of 
#   , now.dateindexp01eom now_dateindexp01eom
#   , to_timestamp(now.dateindexp01eom*3600*24)::date now_dateindexp01eom_dt  -- (later: find the gap of companies that have reported)
#   , erlr.dateindexp01lwd erlr_dateindexp01lwd
#   , to_timestamp(erlr.dateindexp01lwd*3600*24)::date erlr_dateindexp01lwd_dt
#   , coalesce(erlr.mktcap,0)    erlr_mktcap
#   , coalesce(now.mktcap,0)     now_mktcap
#   , coalesce(erlr.price,0)     erlr_price
#   , coalesce(now.price,0)      now_price
#   , coalesce(erlr.sales_q2,0)  erlr_sales_q2
#   , coalesce(erlr.sales_q1,0)  erlr_sales_q1
#   , coalesce(erlr.netinc_q1,0) erlr_netinc_q1
#   , coalesce(erlr.netinc_q2,0) erlr_netinc_q2
#   , coalesce(now.sales_q2,0)   now_sales_q2
#   , coalesce(now.sales_q1,0)   now_sales_q1
#   , coalesce(now.netinc_q2,0)  now_netinc_q2
#   , coalesce(now.netinc_q1,0)  now_netinc_q1
#   from 
#   clump now inner join clump erlr on now.dateindexp01lwd  = erlr.dateindex and now.company_id = erlr.company_id
# ) sq on (true) -- order by dateindex, now_company;
# -- 177 seconds(cold run(first)) most often ( entire database 618,000 rows )
# -- 
# -- 60 seconds ( 13 seconds to query + 47 seconds on data load 

#### QUERIES ENDED ####

#        
#          
#                                                   
