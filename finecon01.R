
# finecon01.R   
# R 3.4.3 commonly debugged with RStudio-1.1.383

# dateindexlwd IS WRONG everywhere ( patch fixed )
# DATA needs left to be fixed [ ] ( but I do not join on it anywhere )


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



# psqlODBC
#   09.06.0500
# PSQLODBC_x64
#   09.06.0500
# https://www.postgresql.org/ftp/odbc/versions/msi/
# ODBC data sources
# ODBC Data Source Administrator (64-bit)
# Drivers
# PostgreSQL ANSI(x64)    9.06.05.00 PostgreSQL Global Development Group PSQLODBC30A.DLL 9/5/2017
# PostgreSQL Unicode(x64) 9.06.05.00 PostgreSQL Global Development Group PSQLODBC30W.DLL 9/5/2017
# ODBC Data Source Administrator (64-bit)
# 
# Drivers
# PostgreSQL ANSI    9.06.05.00 PostgreSQL Global Development Group PSQLODBC30A.DLL 9/5/2017
# PostgreSQL Unicode 9.06.05.00 PostgreSQL Global Development Group PSQLODBC30W.DLL 9/5/2017
# DEC 2017 
# 
verify_channel <- function () {
  
  # R version 3.4.3 (2017-11-30) # sessionInfo()
  
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
  
  require(RODBC) # RODBC_1.3-15
  message("Begin check of (re)connecting PostgreSQL RODBC channel")
  
                                               # see from .Global
  if(!exists("channel", envir = .GlobalEnv) || !RODBC:::odbcValidChannel(channel)) {
  
    message("  Begin (re)connecting PostgreSQL RODBC channel")
  
    channel <-  odbcDriverConnect("DRIVER=PostgreSQL Unicode(x64);Port=5432;Database=finance_econ;Uid=postgres;Pwd=postgres;")
    
    sqlQuery(channel, "set search_path to fe_data_store;", max = 0, as.is = TRUE)
    
    sqlQuery(channel, "set effective_cache_size to '6144MB';",     max = 0, as.is = TRUE)
    sqlQuery(channel, "set work_mem to '2047MB';",                 max = 0, as.is = TRUE)
    sqlQuery(channel, "set maintenance_work_mem to '2047MB';",     max = 0, as.is = TRUE)
    sqlQuery(channel, "set constraint_exclusion = on;",            max = 0, as.is = TRUE)
    sqlQuery(channel, "set max_parallel_workers_per_gather to 4;", max = 0, as.is = TRUE)
    
    message("  End   (re)connecting PostgreSQL RODBC channel")
  
  }
  
  channel <<- channel
  
  message("End   check of (re)connecting PostgreSQL RODBC channel")

  Sys.setenv(TZ=oldtz)
  options(ops)
  
}
# verify_channel()



# It was made with SQLite 3.15.2 and a MinGW cross compiler
#   select sqlite_version();
# sqliteodbc.exe 
# sqliteodbc_w64.exe 
# sqliteodbc-0.9995.tar.gz ( NOTE: NOW 0.9996 has my error correction/fix )
# DEC 2017 - FEB 2018
# http://www.ch-werner.de/sqliteodbc/
# > odbcGetInfo(channel)
#         DBMS_Name          DBMS_Ver   Driver_ODBC_Ver  Data_Source_Name
#          "SQLite"          "3.15.2"           "03.00"                ""
#       Driver_Name        Driver_Ver          ODBC_Ver       Server_Name
# "sqlite3odbc.dll"          "0.9995"      "03.80.0000" "<FILENAME>.sqlite"
# FEB 2018
# 
verify_sqlite3_channel <- function(aaii_sipro_dir = getsetvar_aaii_sipro_dir(), dateindex,  sqlite_file_root) { 

  # R version 3.4.3 (2017-11-30) # sessionInfo()
  
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
  
  require(RODBC) # RODBC_1.3-15
  # uses stringi
  # uses dplyr   dplyr::filter dplyr::inner_join
  
  message("Begin check of (re)connecting SQLite RODBC channel")
  
  # different from postgreSQL verify_channel
  # SQLite case will always close and open
  
  if(exists("sqlite3_channel", envir = .GlobalEnv) && RODBC:::odbcValidChannel(sqlite3_channel)) {
  
    odbcClose(get("sqlite3_channel", envir = .GlobalEnv))
    rm("sqlite3_channel", envir = .GlobalEnv)
  
  }
  
                                                       # see from .Global
  if(!exists("sqlite3_channel", envir = .GlobalEnv) || !RODBC:::odbcValidChannel(sqlite3_channel)) {
  
    message("  Begin (re)connecting SQLite RODBC channel")
  
    sqlite3_channel <- odbcDriverConnect(stringi::stri_c("DRIVER=SQLite3 ODBC Driver;Database=",getsetvar_aaii_sipro_dir(),"/",dateindex,"/",sqlite_file_root,".sqlite"), believeNRows =  FALSE)
    
    # any pragma or OTHER
    # sqlQuery(sqlite3_channel, "",  max = 0, as.is = TRUE)

    message("  End   (re)connecting SQLite RODBC channel")
  
  }
  
  sqlite3_channel <<- sqlite3_channel
  
  message("End   check of (re)connecting SQLite RODBC channel")

  Sys.setenv(TZ=oldtz)
  options(ops)
  
}
# verify_sqlite3_channel(aaii_sipro_dir = getsetvar_aaii_sipro_dir(), dateindex = 17562,  sqlite_file_root = "si_ci")
# odbcGetInfo(sqlite3_channel)



# con, 
#   DBI connectoin
# table.name, 
#   string
# df, 
#   dataframe in insert(append) and updated
#   Be careful column datatypes especially 
#   integer and numeric MATTER.
# fill.null = TRUE,
#   if the local df only has less columns than
#   the remote database, then create dummy columns
#   and fill them with NA. May be required in some
#   datebase where dbWriteTable requires this.
#   PostgreSQL does not require this.
#   SQLite has not been tested(yet) 
# row.names = FALSE
#   passed
# inbnd_snake_case_cols = TRUE
#   upper case letters become lowercase
#   dots(.) become underscores
# skip_insert_recs      = FALSE
#   sometimes not enough information exists in the df
#   to do an insert ( e.g. a multicolumed PK may exist )
#   but the df only contains some columns of the PK,
#   so skip doing inserts
# date_to_int           = TRUE  # pre-dbWriteTable
#   R Date classed datatypes are convertd to integers
# logical_to_int        = TRUE  # pre-dbWriteTable
#   R boolen datatypes are convert to integers
# double_to_numeric     = TRUE  # pre-dbWriteTable # RSQLite # USE # "double_to_numeric = "numeric"
#   R numeric datatypes are converted
#     choices
#       exactly name the datatype ( goodl for SQLite )
#         double_to_numeric = "numeric"
#       precise type ( good for PostgreSQL )
#         double_to_numeric = TRUE ( good for PostgreSQL )
#           numeric becomes "numeric(13,2)"
# index = NULL
#   columns in "where clause" forming a join between the temporary table and the target table
#   that is used in the "UPDATE" statement.  These/this are/is VERY IMPORTANT
# ...
#   passed to dbWriteTable e.g. 'field types'
#   TO DO 'field types' no longer works because data is ONLY appended
#          ADJUST intercept 'field types' and USE the information
# 
dbWriteTableX <- function (con, table.name, df, fill.null = TRUE, row.names = FALSE
  , inbnd_snake_case_cols = TRUE
  , skip_insert_recs      = FALSE
  , date_to_int           = TRUE  # pre-dbWriteTable
  , logical_to_int        = TRUE  # pre-dbWriteTable
  , double_to_numeric     = TRUE  # pre-dbWriteTable # RSQLite # USE # "double_to_numeric = "numeric"
  , index = NULL
  , ...) {
  
  # FUTURE - ADD ALLOW UPDATING - NO WHERE CLAUSE
  # index = character()
  
  # # FUTURE ( IN dots ... INTERCEPT field.types AND USE )
  # field.types 
  #   character vector of named SQL field types where the names are the names of new table's columns
  # https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf
  # field.types 
  #   is a list of named field SQL types where names(field.types) provide the new table's column names 
  # https://cran.r-project.org/web/packages/RPostgreSQL/RPostgreSQL.pdf
  # field.types
  #   character vector of named SQL field types where the names are the names of new table's columns. 
  # https://cran.r-project.org/web/packages/RPostgres/RPostgres.pdf
  # field.types 
  #   Additional field types used to override derived types
  # https://cran.r-project.org/web/packages/odbc/odbc.pdf
  # dbWriteTable signature(conn = "JDBCConnection", ...)
  # https://cran.r-project.org/web/packages/RJDBC/RJDBC.pdf
  
  ops <- options()
  
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  options(warnings = 1)
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  # inspirated heavily by the R CRAN package caroline function dbWriteTable2
  
  ## X require(DBI) X ##
  # #bring your own dbWriteTable ( S3 or S4 dispath on the class of 'con' )
  # expect a passed con of class "RPostgreSQLConnection"(RPostgreSQL package)
  # should be able handle a con of class RSQLiteConnection(RSQLite package)
  
  # uses dplyr inner_join filter
  
  message("Begin dbWriteTableX")

  if(dbExistsTable(con,table.name)) {
    target_table_exists <- TRUE
    # ANDRE DOES table already exist (expected)
    # RPostgreSQL will give an error
    # RSQlite     will give an error
    res <- try({ dbListFields(con, table.name) }, silent = TRUE)
    if(!inherits(res, "try-error")) { 
      fields <- res 
    } else {      
      fields <- character()
    }
  } else {
    # zero column table
    # 
    # works on PostgreSQL.  
    # May NOT work on all databases
    # Does NOT work on SQLite
    # TODO
    #   attr(class(con), "package")        [1] "RPostgreSQL"
    #   attr(class(sqlite_con), "package") [1] "RSQLite"
    # NOTE, BUT DOING THIS, always need, append = TRUE to dbWriteTable
    res <- try({ dbExecute(con, paste("create table ", table.name, "()")) }, silent = TRUE)
    if(!inherits(res, "try-error")) { 
      target_table_exists <- TRUE
      fields <- character() # dbListFields(con, table.name)
    } else {
      # A dummy COLUMN that is DROPPED later
      # RSQLite (WITHOUT column DATA TYPE PROCESSING )
      res <- try({ dbExecute(con, paste("create table ", table.name, "(dummy integer)")) }, silent = TRUE)
      if(!inherits(res, "try-error")) {
        target_table_exists <- TRUE
        fields <- "dummmy"
        dummy_field_exists <- TRUE
      } else {
        stop("dbWriteTableX could not create an ALMOST nothing table.")
      }
    }
    
  }
    
  # SOMTHING 'AUTHOR' SPECIFIC
  fields <- fields[!grepl("\\.\\.pg\\.dropped", fields)]
  
  # ANDRE ADDED if-then
  if(inbnd_snake_case_cols) {
    # AUTHOR NAME CONVERSION (I WILL KEEP FOR NOW)
    names(df) <- tolower(names(df))
    names(df) <- gsub("\\.", "_", names(df))
  }
  
  # FINDS NAMES IN THE DATA.FRAME COLUMNS THAT EXIST IN THE DATABASE FIELDS
  clmn.match <- match(names(df), fields)

  # FINDS NAMES OF DATABASE FIELDS THAT EXIST IN THE DATA.FRAME COLUMNS
  field.match <- match(fields, names(df))
  
  # ALL COLUMNS ( AND THEIR CLASSES )
  all.clmns <- sapply(df, class)
    
  # 'FORMAT' THE DATATYTPLE OF THE COLUMN IN PREPARATION FOR FUTURE PLACEMENT IN THE DB
  
  # TO BE DONE IN R 
  if(date_to_int) {
    # CONVERT THE TYPE 'AT(IN)' THE R LANGUAGE
    # ANY COLUMNS OF class Date
    if(length(which(all.clmns %in% "Date"))) {
       # CONVERT TO INTEGER
      df[, names(all.clmns[which(all.clmns %in% "Date")])] <- lapply( df[, names(all.clmns[which(all.clmns %in% "Date")])], as.integer)
      all.clmns <- sapply(df, class)
    }
  }
  
  # TO BE DONE IN R
  if(logical_to_int) {
    # CONVERT THE TYPE 'AT(IN)' THE R LANGUAGE
    # ANY COLUMNS OF class logical (Boolean)
    if(length(which(all.clmns %in% "logical"))) {
       # CONVERT TO INTEGER
      df[, names(all.clmns[which(all.clmns %in% "logical")])] <- lapply( df[, names(all.clmns[which(all.clmns %in% "logical")])], as.integer)
      all.clmns <- sapply(df, class)
    }
  }
  
  if(is.logical(double_to_numeric) && double_to_numeric) {
    # CONVERT THE TYPE 'AT(IN)' THE DATABASE 
    all.clmns[all.clmns == "numeric"] <- "numeric(13,2)"
  } 
  else if (is.character(double_to_numeric)) { 
    all.clmns[all.clmns == "numeric"] <- double_to_numeric
  }
  
  # CONVERT THE TYPE 'AT(IN)' THE DATABASE 
  all.clmns[all.clmns == "character"] <- "text"

  if (any(is.na(clmn.match))) { 
    message(paste0("  Found '", paste0(names(df)[is.na(clmn.match)], collapse = ", "), 
                 "' not in fields of '", table.name, "' table. Adding to database . . ."))
    
    # NEW NAMES IN THE DATA.FRAME COLUMNS THAT DO NOT EXIST IN THE DATABASE FIELDS
    new.fields <- all.clmns[!names(all.clmns) %in% fields]
    
    # ACTUALLY ADD NEW COLUMNS TO THE DATABASE 
    for(new.fields_idx in seq_along(new.fields)) {
      message(paste0("  Begin adding column ", names(new.fields)[new.fields_idx], "."))
                                                            # field name                       # field type
      dbExecute(con, paste("alter table", table.name, "add", names(new.fields)[new.fields_idx], new.fields[new.fields_idx]))
      message(paste0("  End   adding column ", names(new.fields)[new.fields_idx], "."))
    }
    
    # REDO (FROM ABOVE)  
    fields <- dbListFields(con, table.name)
    # SOMTHING 'AUTHOR' SPECIFIC
    fields <- fields[!grepl("\\.\\.pg\\.dropped", fields)]
    
    # NOT USED (ANYMORE) BELOW ( BUT FOR INTEGRITY)
    clmn.match <- match(names(df), fields)
    
    # FINDS NAMES OF DATABASE FIELDS THAT EXIST IN THE DATA.FRAME COLUMNS
    field.match <- match(fields, names(df))
    
  }
  
  if(!is.null(index)) {
    
    res <- try({ dbListFields(con, table.name) }, silent = TRUE)
    if(!inherits(res, "try-error")) { 
      if(!all(index %in% res)) {
        stop("No index columns are found in the database")
      } 
    } else {      
      stop("No columns in the input data")
    }
    
    # ONLY INSERT(APPEND) NEW RECORDS that are not FOUND in the INDEX
    # 
    # then I want to reduce my df records ( that are going to dbWriteTable( append = TRUE ) )
    # to just those df[, index] records NOT found in the database table.name(index)
    # IN DB, INSERT(APPEND) *SOME* RECORDS
  
    # ( CHECK index COLUMNS EXIST AND ) GET index COLUMNS DATATYPES
    res <- try({ dbSendQuery(con, paste0("select ",paste(index, collapse = ", ")," from ", table.name, " where 0 = 1")) }, silent = TRUE)
    if(!inherits(res, "try-error")) {
    
      db.col.info <- dbColumnInfo(res)
      dbClearResult(res)
      
      # NON-PRECISE KEY ( ALWAYS A BAD IDEA )
      if(is.logical(double_to_numeric) && double_to_numeric) {
        db.col.info$type[db.col.info$type == "DECIMAL"] <- "NUMERIC(13,2)"
      } else if (is.character(double_to_numeric)) {
        db.col.info$type[db.col.info$type == "DECIMAL"] <- double_to_numeric
      }
      
      # temporary table
      dbExecute(con, paste0("create ", if(!is.null(getOption("upsert_temp_is_temporary"))) { "temporary" } else { "" }," table index_temp( ", paste(db.col.info$name, db.col.info$type, collapse = ", "), ", not_in_db INTEGER )") )

      # not_in_db  == 0 expects/shows *new* records to be added
      dbWriteTable(con, "index_temp", cbind(df[, index, drop = F], not_in_db = 0L) , row.names = FALSE, append = TRUE)
      # performance helper
      dbExecute(con, paste0("create index index_temp_idx on index_temp( ", paste(index,  collapse = ", " )," )"))

       # anti join   
      dbExecute(con, paste0(
        "update index_temp te set not_in_db = 1 where not exists ( select 1 from  ", table.name, " where ", paste0(sapply(index, function(x) { paste0("te.",x," = ",x) } ), collapse = " and "), ")"
        ))
      
      # optimistic: everthing is not in the database 
                                              # ORDER is not garanteed
     df.index_temp  <- dbGetQuery(con, paste0("select * from index_temp"))
      
      # FURTHER PROCESSING(below) - INSERT(APPEND)
      df.reduced.not.in.db <- dplyr::filter(dplyr::inner_join(df, df.index_temp, by = index), not_in_db == 1L)
      df.reduced.not.in.db$not_in_db <- NULL
      # FURTHER PROCESSING(below) - UPDATE
      df.reduced.in.db     <- dplyr::filter(dplyr::inner_join(df, df.index_temp, by = index), not_in_db == 0L)
      df.reduced.in.db$not_in_db <- NULL
      
      dbExecute(con, "drop table index_temp")
    } else {
      # [A] COLUMN(S) does(DO) NOT exists
      stop("dbWriteTableX was sent at least one index field that does not exist in the database table")
    }
    
  }
      
  # PRE-INSERT(APPEND) ACTIVITY
  # SWITCH TO 'current working df'
  # (if I have an 'index')
  # 
  if(exists("df.reduced.not.in.db")) {
    df <- df.reduced.not.in.db
  } 
  
  # fill.null == TRUE (KEEP DEFAULT FOR NOW: NOT SURE = NOT USEFUL)
  # 
  # R Package RPostgreSQL dbWriteTable will automatically match column names 
  #  ( so fill.null = FALSE is O.K.)
  #
  # R Package RSQLite? OTHERS,I am not sure? UNTESTED
  # 
  # ORIGINAL df COLUMN NAMES before new (all NULL(NA)) columns (COULD BE) added
  df.nms.orgnl <- names(df)
  if (sum(is.na(field.match)) > 0 && fill.null == TRUE) {
      message("creating NAs/NULLs for for fields of table that are missing in your df")

      # ADD EMPTY COLUMNS 
      for(i in seq_len(sum(is.na(field.match)))) df[[paste0("new",i)]] <- rep(NA, NROW(df))
      names(df) <- c(df.nms.orgnl, fields[is.na(field.match)])
      
    # FINDS NAMES OF DATABASE FIELDS THAT EXIST IN THE DATA.FRAME COLUMNS
    field.match <- match(fields, names(df))
  }
  
  # AUTO-REORDERING ( IF BOTH THE SAME DB FIELDS AND TABLE COLUMNS EXIST IN BOTH, THEN CAN AUTO-REORDER )
  if ((sum(is.na(field.match)) == 0) && !identical(fields, names(df))){
  
    # MAKE THE COLUMN ORDER the same as the database FIELD order
    reordered.names <- names(df)[match(fields, names(df))]
    if (any(is.na(reordered.names))) 
        stop("Too many unmatched columns to database column list. Stopping")
    df <- df[, reordered.names]
    
    # FINDS NAMES OF DATABASE FIELDS THAT EXIST IN THE DATA.FRAME COLUMNS
    field.match <- match(fields, names(df))
  
  }
  
  # BEGIN TO CHECK FOR NULL/NOTNULL, WRONG PRECISION, AND WRONG TYPE ( I WILL NOT KEEP THIS PART)
  # r <- dbSendQuery(con, paste("SELECT * FROM", table.name, "WHERE 0 = 1"))
  # db.col.info <- dbColumnInfo(r)
  # DATABASE FIELD NAMES
  # rownames(db.col.info) <- db.col.info$name
  
  # BEGIN CHECK FOR REQUIRED (NOT NULL FIELDS) (PROBABLY WILL NOT KEEP THIS PART ... TOO DB SPECIFIC)
  # null.OK <- nv(db.col.info, "nullOK")
  # reqd.fields <- names(null.OK[!null.OK])
  # na.cols <- sapply(df, function(x) any(is.na(x)))
  # req.miss <- na.cols[reqd.fields]
  # if (any(req.miss)) 
  #     stop(paste("Didn't load df because required field(s)", 
  #         paste(names(req.miss)[req.miss], collapse = ", "), 
  #         "contained missing values"))
          
  # * PROACTIVE WAY OF CHECKING: I MAY RETURN TO THIS LATER *
  # 
  # BEGIN CHECK FOR PRECISION (float# to/from numeric(x,y) DOES NOT TRANSLATE: ... ERROR ... SO I WILL NOT KEEP)
  # db.precisions <- nv(db.col.info, "precision")
  # df.nchars <- sapply(df, function(c) max(nchar(c)))
  # prec.reqd <- db.precisions > 0
  # too.long <- db.precisions[prec.reqd] < df.nchars[prec.reqd]
  # if (any(too.long)) 
  #     stop(paste("Didn't load df because fields", paste(names(df.nchars)[prec.reqd][too.long], 
  #         collapse = ", "), "were too long"))
  
  # AUTHOR DETECT TYPE MISMATCHES BUT DECIDES NOT TO DO 'ANYTHING ABOUT IT' (I WILL NOT KEEP THIS PART)
  # db.sclasses <- nv(db.col.info, "Sclass")
  # df.classes <- sapply(df, class)
  # type.mismatches <- names(df.classes)[db.sclasses != df.classes & !na.cols]
  # dbClearResult(r)
  
     
  # ANDRE ADDED append = TRUE because (earlier in the code), 
  # if the table DID not exist, I HAD made a 'stub' table
  if(target_table_exists && !skip_insert_recs) {
    message(paste0("  Begin loading ", table.name, " new table data into the target database"))
    db.write <- dbWriteTable(con, table.name, df, row.names = row.names, append = TRUE, ...)
    message(paste0("  End   loading ", table.name, " new table data into the target database"))
  }
  # currently do not have a case where the 'target table does not exist'
  
  # PRE-APPEND ACTIVITY
  # SWITCH TO 'current working df'
  # (if I have an 'index')
  # 
  if(exists("df.reduced.in.db")) {
    df <- df.reduced.in.db
  }
  
  # ONLY UPDATE CURRENT RECORDS that are *FOUND* in the INDEX 
  if(!is.null(index)) {

    # performance helper
    try ({ dbExecute(con, paste0("create index ", table.name, "_", paste0( index, collapse = "_"), "_idx on ", table.name, " ( ", paste0( index, collapse = ", ")  ,  " )" ) ) }, silent = TRUE)
    # discard results
    
    # ONLY the ORIGIAL columns SENT to dbWriteTable
    df <- df[,df.nms.orgnl, drop = F]
    
    df.classes <- sapply(df, class)
    
    df.classes[df.classes == "character"] <- "text"
    
    # can not convert here
    # existe in RPostgreSQL
    # do not exist in RSQLite 
    #  ( from experience: will convert "logical" to "integer": prob O.K. )
    #  ( from experience: will convert "Date" to "numeric": prob not desirable )
    # SHOULD NOT HAVE MADE it this FAR
    # EARLIER, SHOULD have filtered out ( by the call to dbWriteTableX(with paramters) )
    
    df.classes[df.classes == "Date"]      <- "date"
    df.classes[df.classes == "logical"]   <- "boolean"
    
    if(is.logical(double_to_numeric) && double_to_numeric) {
      # PostgreSQL, SQLite does not have this
      df.classes[df.classes == "numeric"]   <- "numeric(13,2)"       #  WRONG FOR #SQLite
    } else if(is.logical(double_to_numeric) && !double_to_numeric) {
      df.classes[df.classes == "numeric"]   <- "float8"              #  WRONG FOR #SQLite
    } else if(is.character(double_to_numeric)){ 
      df.classes[df.classes == "numeric"]   <- double_to_numeric     # SQLite #  double_to_numeric = "numeric"
    } else { }
    # "numeric"  #  correct FOR  SQLite

    # temporary table
    dbExecute( con,
      paste0("create ",if(!is.null(getOption("upsert_temp_is_temporary"))) { "temporary" } else { "" }," table update_temp( ", paste0( names(df.classes), " " , df.classes, collapse = ", "), " )")
    )
    message(paste0("  Begin loading ", table.name, " updated table data into the target database"))
    dbWriteTable(con, "update_temp", df, row.names = FALSE, append = TRUE)
    message(paste0("  End   loading ", table.name, " updated table data into the target database"))
    # performance helper
    dbExecute( con,
      paste0("create index update_temp_idx on update_temp( ", paste0( index, collapse = ", "), " )")
    )
    
    message(paste0("  Begin updating ", table.name, " old table data using new data in the the target database"))
    dbExecute(con, 
    paste0(
        "update ", table.name, " t \n"
      , "  set "  , paste0(sapply(colnames(df)[!colnames(df) %in% index], function(x) { paste0(x," = s.",x) } ), collapse = ", "), " \n"
      , "from update_temp s \n"
      , "  where ", paste0(sapply(index, function(x) { paste0("s.", x," = t.",x) } ), collapse = " and ")
    ))
    message(paste0("  End   updating ", table.name, " old table data using new data in the the target database"))
    
    dbExecute( con, "drop table update_temp")
    
    # uses RODBC
    # RETURN ERROR: would have LIKED this to WORK
    # verify_channel()
    # RODBC::sqlUpdate(channel, dat = df, tablename = table.name, 
    #           index = index,
    #           verbose = FALSE, test = FALSE, nastring = NULL,
    #           fast = TRUE)

  }
  
  # dummy handling HAS NOT BEEN TESTED
  if(exists("dummy_field_exists") && (dummy_field_exists == TRUE)) {
    dbExecute(con, paste0("alter table ",table.name, " drop column dummy") )
  }
  
  message("End   dbWriteTableX")
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  # currently ONLY dbWriteTable result (if ever executed)
  # COULD I choose SOMETHING better
  if(exists("db.write")) { return(db.write) } else { return(NULL)}
  
}
# DEBUGGING HELP
# {
#   mtcars2 <- { cbind(mtcar = rownames(mtcars), mtcars, stringsAsFactors = F) -> t; rownames(t) <- NULL; t }
#   mtcars2      <- DataCombine::MoveFront(mtcars2, "mtcar")
#   mtcars2$vs   <- as.integer(mtcars2$vs )
#   mtcars2$am   <- as.integer(mtcars2$am )
#   mtcars2$gear <- as.integer(mtcars2$gear)
#   mtcars2$carb <- as.integer(mtcars2$carb)
# }
# str(mtcars2)
# {
#   mtcars3 <- mtcars2
#   mtcars3$index2             <- seq_len(NROW(mtcars3)) # integner
#   mtcars3$revindexdetaiiled <- as.numeric(rev(seq_len(NROW(mtcars3)))) 
#   mtcars3$letter_index      <- letters[seq_len(NROW(mtcars3))]
#   mtcars3$dattish           <- zoo::as.Date(seq_len(NROW(mtcars3)))
#   mtcars3$dattishplus       <- zoo::as.Date(seq_len(NROW(mtcars3))) + 365L
# }
# TEST GROUP
# dbGetQuery(con, "drop table mtcars")
# dbWriteTableX(con, "mtcars",  mtcars3[17:32, !colnames(mtcars3) %in% c("wt", "qsec","vs", "am"), drop = F], index = "mtcar" )
# CREATES COLUMNS
# dbWriteTableX(con, "mtcars",  mtcars3[17:32, !colnames(mtcars3) %in% c(      "qsec","vs"),       drop = F], index = "mtcar" )
# dbWriteTableX(con, "mtcars",  mtcars3[ 1:18, !colnames(mtcars3) %in% c("               "),       drop = F], index = "mtcar" )


# con
#   DBI CONNECTION
# all_load_days = getvar_all_load_days()
#   interger Dates since 1970
# file_names    = c("si_ci", "si_isq", "si_cfq", "si_isq", "si_psd", "si_psdc", "si_date", "si_mlt", "si_rat", "si_ee")
#   sipro 'before the dot' file names of the .dbf files
# candidate_columns = NULL 
#   columns (IN UPPERCASE) that I want to create in the database and update records, e.g.
#   candidate_columns = c("LASTMOD","UPDATED")
# col_conversions   = NULL
#   named vector of column names and datatype conversions, e.g.
#   col_conversions   = c("LASTMOD" = "as.integer(zoo::as.Date(<COL>))"
# add_file_name_root_prefix = TRUE
#   add the "si_..." ... part of the name to the beginning of the new column name
#   only useful on columns of the 'same name' that are found in different .dbf files
# 
load_columns_direct <- function(
    con
  , all_load_days = getvar_all_load_days()
  , file_names    = c("si_ci", "si_isq", "si_cfq", "si_isq", "si_psd", "si_psdc", "si_date", "si_mlt", "si_rat", "si_ee")
  , candidate_columns = NULL 
  , col_conversions   = NULL
  , add_file_name_root_prefix = TRUE
  ) {
  
  ops <- options() 
  
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  options(warn = 1)
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  message("Begin load_columns_direct")
  
  if(is.null(all_load_days))        stop("load_columns_direct must have at least one value in the parameter all_load_days")
  if(is.null(file_names))           stop("load_columns_direct must have at least one value in the parameter file_names")
  if(is.null(candidate_columns))    stop("load_columns_direct must have at least one value in the parameter candidate_columns")
  
  candidate_getvar_all_load_days_var <- all_load_days
  candidate_old_columns_to_upload    <- candidate_columns
  
  verify_connection()
  
  # per dateindex
  for(load_days_var_i in  candidate_getvar_all_load_days_var) {
    
    message(paste0("  Begin dateindex: ", load_days_var_i))

    # per si_#
    for(file_name_i in  file_names) {
      
      message(paste0("    Begin file_name: ", file_name_i))
   
      file_name_i_with_path <- paste0(getsetvar_aaii_sipro_dir(),"/",load_days_var_i,"/", file_name_i, ".dbf")
      si_xxx_tbl_df <- suppressWarnings(suppressMessages(foreign::read.dbf(file = file_name_i_with_path, as.is = TRUE)))

      # remove dangerous duplicates
      si_xxx_tbl_df <- si_xxx_tbl_df[!duplicated(si_xxx_tbl_df),,drop = FALSE]
      
      si_xxx_tbl_df <- si_xxx_tbl_df[!duplicated(si_xxx_tbl_df$COMPANY_ID),                 ,drop = FALSE]
      si_xxx_tbl_df <- si_xxx_tbl_df[!duplicated(si_xxx_tbl_df$COMPANY_ID, fromLast = TRUE),,drop = FALSE]
      
      old_columns_to_upload <- colnames(si_xxx_tbl_df)[colnames(si_xxx_tbl_df) %in% candidate_old_columns_to_upload]

      # per column
      new_column_names <- character()
      for( old_columns_to_upload_i in old_columns_to_upload)  {
        
         message(paste0("      Begin old column: ", old_columns_to_upload_i))
      
         new_column_name <- tolower(old_columns_to_upload_i)
         # add decoraton
         if(add_file_name_root_prefix) {
         
           file_name_root_prefix <- sub("^si_","",file_name_i)
           new_column_name       <- paste0(file_name_root_prefix, "_", new_column_name)
           
         } 
         
         # add
         si_xxx_tbl_df[[new_column_name]] <-  si_xxx_tbl_df[[old_columns_to_upload_i]]
   
         # usually convert datatype
         if(any(names(col_conversions) == old_columns_to_upload_i)) {
         
           col_conversion <- col_conversions[names(col_conversions) == old_columns_to_upload_i]
           si_xxx_tbl_df[[new_column_name]] <- eval(parse(text=gsub("<COL>","si_xxx_tbl_df[[new_column_name]]",col_conversion)[[1]]))
           
         }
         # add to the collection
         new_column_names <- c(new_column_names, new_column_name)
         
         # if I did not add the "file_name_root_prefix"
         # many databases can not have columns of the same name in different cases
   
         si_xxx_tbl_df[[old_columns_to_upload_i]] <- NULL
   
         message(paste0("      End   old column: ", old_columns_to_upload_i))
         
      }
      
      if(length(old_columns_to_upload)) {
   
        si_xxx_tbl_df[["company_id_orig"]] <- si_xxx_tbl_df[["COMPANY_ID"]]
        si_xxx_tbl_df[["dateindex"]]       <- load_days_var_i
        
        # subset
        si_xxx_tbl_df_sub <- si_xxx_tbl_df[ ,c("dateindex", "company_id_orig", new_column_names), drop = FALSE]
         
        # skip_insert_recs = TRUE
        # 
        # 12783 "2004-12-31"00509L10 Acura Pharmaceuticals, Inc. is 
        # not in the target progresd database because the ticker is bad(actually missing).
        # If I do not have enough information to form a "primary key" for insert, then I may receive the message
        # # null value in column "PK" violates not-null constraint # #
        # In this case, I do not really want to insert a new record anyways.
        # (I do not have enough information to form the primary key(dateindex, company_id)
        # (I do not have the TRUE company_id) 
        # So, I will just ignore that record. I will just update the rest of the records.

        columns_direct <- dbWriteTableX(con, "si_finecon2", si_xxx_tbl_df_sub, skip_insert_recs = TRUE, index = c("dateindex", "company_id_orig"))

      }
      
      message(paste0("    End   file_name: ", file_name_i))

    }
    
    message(paste0("  End   dateindex: ", load_days_var_i))

  }

  
  message("End   load_columns_direct")
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  if(exists("columns_direct")) { return(columns_direct) } else { return(NULL) }
} 
# # CALL 0
# BE VERY CAREFUL all_load_days MUST BE INTEGER
# columns_direct <-  load_columns_direct(
#     con
#   , all_load_days     = 12783L # MUST BE INTEGER!
#   , file_names        = "si_ee"
#   , candidate_columns = "QS_DATE"
#                           # Date to integer
#   , col_conversions   = c("QS_DATE" = "as.integer(zoo::as.Date(<COL>))")
#   , add_file_name_root_prefix = FALSE
#   )

# # CALL 1
# columns_direct <-  load_columns_direct(
#     con
#   , file_names        = "si_ee"
#   , candidate_columns = "QS_DATE"
#                           # Date to integer
#   , col_conversions   = c("QS_DATE" = "as.integer(zoo::as.Date(<COL>))")
#   , add_file_name_root_prefix = FALSE
#   ) 

# # CALL 2
# columns_direct <-  load_columns_direct(
#     con
#   # first added to the database # "2004-09-30" # so do not bother looking for earlier data
#   , all_load_days = getvar_all_load_days()[12691 <= getvar_all_load_days()]
#   # I (currently) do not have any "si_rat" columns in the database
#   , file_names    = c("si_ci", "si_isq", "si_cfq", "si_isq", "si_psd", "si_psdc", "si_date", "si_mlt", "si_ee")
#   , candidate_columns = c("LASTMOD","UPDATED") 
#                              # Date to integer
#   , col_conversions   = c(   "LASTMOD" = "as.integer(zoo::as.Date(<COL>))"
#                              # logical to integer
#                            , "UPDATED" = "as.integer(<COL>)"
#                          )
#   ) 





is_connected_postgresql_con <- function() {  

  require(RPostgreSQL)
  
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
      
      # Works well with Npgsql(9.6era)       3.0.8 ( as far as connect/timeout_disconnect )
      # Current version Npgsql(early 10 era) 3.2.x ( will CRASH rstudio on each first attempt in a newly loaded RStudio )
      # Currently last known working well with the 'older' (9.6ish) Npgsql 3.2.6 INSTALLED 12/13/2017
      # [ ] TODO: LEFT_OFF: FUTURE ... ADJUSTMENT 
      
      cid <<- db.connect(user = "postgres", dbname = "finance_econ", default.schemas = "fe_data_store,public")
      # increments up by 1 every time
      con <<- PivotalR:::.localVars$db[[length(PivotalR:::.localVars$db)]]$conn
      
      if(!is_connected_postgresql_con()){
        stop("PostgreSQL database server is not responding.  Is it up/blocked?  Are the client login credentials valid?")
      }
      

      #
      # set search_path to fe_data_store,public;
      # set time zone 'utc';
      # set work_mem to '2047MB';
      # set constraint_exclusion = on;
      # set max_parallel_workers_per_gather to 4;
      
      # EXPERIMENT
      # MEMORY caching data
      # requires PostgreSQL restart
      # ONLY in postgresql.conf
      # postgres=# show shared_buffers;
      #  shared_buffers
      # ----------------
      #  8GB
      
      # EXPERIMENT
      # The setting can be changed within individual sessions, but only before the first use of temporary tables within the session; 
      # subsequent attempts to change the value will have no effect on that session.
      # ANY number I want ( no error )
      # EXPERIMENT 'sorting and temp tables
      # db.q(str_c("set temp_buffers to '14GB';"), nrows =  -1, conn.id = cid)
      
      # ANONYOMOUST ( 2017 / WINDOW 10 ) HAS 16GB of RAM
      
      # EXPERIMENT ( memory for disk caching ) -- 2048(GUESSING) + 4096(shared buffers)
      db.q(str_c("set effective_cache_size to '6144MB';"), nrows =  -1, conn.id = cid) # disk cache by the: os +'shared_buffers'
      # 
      db.q(str_c("set time zone 'utc';"), nrows =  -1, conn.id = cid)
      # windows LIMIT 2047
      # A good rule of thumb is to keep: work_mem*max_connections*2 < 1/4 of memory
      # NOTE: WATCH OUT FOR 'R language: parallel'each process GETS 'work_mem' limit
      db.q(str_c("set work_mem to '2047MB';"), nrows =  -1, conn.id = cid)
      
      # maximum amount of memory to be used by maintenance operations, such as VACUUM, CREATE INDEX, and ALTER TABLE ADD FOREIGN KEY
      # https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server
      db.q(str_c("set maintenance_work_mem to '2047MB';"), nrows =  -1, conn.id = cid)

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



release_connection <- function () {
  
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
  
  release_connection_inner <- function () {

    # How to use dbGetQuery in tryCatch with PostgreSQL?
    # http://stackoverflow.com/questions/34332769/how-to-use-dbgetquery-in-trycatch-with-postgresql
    
    #
    require(PivotalR) #                                     # OSUser

    if(exists("cid", envir = .GlobalEnv) || 
       exists("con", envir = .GlobalEnv) || 
       is_connected_postgresql_con() 
    ) {
      
      # because connection gets hosed
      db.disconnect(conn.id = cid, verbose = TRUE, force = TRUE)
      rm("cid","con", envir = .GlobalEnv)
      
    }
  }
  
  release_connection_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
}
# release_connection()



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
    
    db.q("alter table si_finecon2 add if not exists dateindexyear      int;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindexyearmonth int;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindexmonthsincebirth int;", conn.id = cid)
    db.q("alter table si_finecon2 add if not exists dateindexmonth      int;", conn.id = cid)
    
    db.q("alter table si_finecon2 add if not exists dateindexlbd       int;", conn.id = cid)
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
    
    

    try( { db.q("create unique index if not exists si_finecon2_dateindex_company_id_orig_both_key on si_finecon2(dateindex_company_id_orig);", conn.id = cid) }, silent = TRUE )
    
    try( { db.q("create index if not exists si_finecon2_dateindex_idx  on si_finecon2(dateindex);", conn.id = cid) }, silent = TRUE )

    try( { db.q("create index if not exists si_finecon2_company_id_idx on si_finecon2(company_id);", conn.id = cid) }, silent = TRUE )
    
    # # just SIMPLY
    # # will ERROR OUT ( will not allow to add a second primary key )
    # try( { db.q("alter table if exists si_finecon2 add primary key(dateindex_company_id);", conn.id = cid) }, silent = TRUE ) # only be on
    # # si_finecon2_pkey
    # # singleton
    # 
    # # try( { db.q("alter table if exists si_finecon2 add      unique(dateindex, company_id);", conn.id = cid) }, silent = TRUE )
    # # si_finecon2_dateindex_company_id_key
    # # can be many
    
    # How to add column if not exists on PostgreSQL?
    # https://stackoverflow.com/questions/12597465/how-to-add-column-if-not-exists-on-postgresql
    # Add primary key to PostgreSQL table only if it does not exist
    # https://stackoverflow.com/questions/9906656/add-primary-key-to-postgresql-table-only-if-it-does-not-exist
    # 42.8. Errors and Messages
    # https://www.postgresql.org/docs/10/static/plpgsql-errors-and-messages.html

    # SHOULD always BE IN A database
    # correct, but not used in this function
    # current_database <- db.q("select current_database();", conn.id = cid)[[1]]
    
    current_search_path <-  strsplit(db.q("show search_path;", conn.id = cid)[[1]],",[ ]*")[[1]]
    current_schema      <- db.q("select current_schema();", conn.id = cid)[[1]] 
    # MUST put SOMETHING in there FIRST
    res <- db.q("SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", conn.id = cid) 
    current_temp_schema <- if(NROW(res) == 1) res[[1]] else ""; rm(res)
    SQuote <- function(x) { paste0("'", x, "'") }
    
    db.q("create unique index if not exists si_finecon2_pkey on si_finecon2(dateindex_company_id);", conn.id = cid)
    db.q(str_c("
    do $$ begin
    if not exists(select constraint_name 
                    from information_schema.table_constraints 
                      where constraint_schema   = ", SQuote(current_schema), " 
                      and   table_name          = 'si_finecon2' 
                      and   constraint_type     = 'PRIMARY KEY'
    ) then
      -- psql message
      -- log_min_messages and client_min_messages 
      raise WARNING USING MESSAGE = 'Creating . . .  ' || 'fe_data_store' || '.' || 'si_finecon2' || ' PRIMARY KEY using INDEX';
      alter table if exists si_finecon2 add primary key using index si_finecon2_pkey;
    else
      raise NOTICE USING MESSAGE = 'Already exists ' || 'fe_data_store' || '.' || 'si_finecon2' || ' PRIMARY KEY';
    end if;
    end $$
    "), conn.id = cid)
    
    # (if unamed ndex?) ... WILL JUST KEEP ADDING MORE ... so I name it
    try( { db.q("create unique index if not exists si_finecon2_dateindex_company_id_key          on si_finecon2(dateindex,    company_id);", conn.id = cid) }, silent = TRUE )
    
    try( { db.q("create unique index if not exists si_finecon2_dateindexlbd_company_id_key       on si_finecon2(dateindexlbd, company_id);", conn.id = cid) }, silent = TRUE )
    try( { db.q("create unique index if not exists si_finecon2_dateindexeom_company_id_key       on si_finecon2(dateindexeom, company_id);", conn.id = cid) }, silent = TRUE )

    # can be many
    try( { db.q("create unique index if not exists si_finecon2_dateindex_company_id_orig_key     on si_finecon2(dateindex,    company_id_orig);", conn.id = cid) }, silent = TRUE )
    # can be many

    # PROTECT against a LOSS of INTEGRITY
    try( { db.q("create unique index if not exists si_finecon2_dateindex_ticker_id_key           on si_finecon2(dateindex,    ticker);", conn.id = cid) }, silent = TRUE )
    
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



# uses verify_connection
verify_si_finecon_aggregates_exists <- function () {
  
  # R version 3.4.1 (2017-06-30) # sessionInfo()
  
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
  
  verify_si_finecon_aggregates_exists_inner <- function () {
    

    verify_connection()
    db.q("create table if not exists si_finecon2_aggregates();", conn.id = cid)
    db.q("alter  table               si_finecon2_aggregates add if not exists dateindex    int;", conn.id = cid)

    # (if unnamed index?) ... WILL JUST KEEP ADDING MORE ... so I name it
    try( { db.q("create unique index if not exists si_finecon2_aggregates_dateindex_key on si_finecon2_aggregates(dateindex);", conn.id = cid) }, silent = TRUE )
 
  }
  verify_si_finecon_aggregates_exists_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
}
# verify_si_finecon_aggregates_exists_exists()



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
lwd_of_month <- function(anyday = NULL, within_back = 5) {  
  #
  # uses # to.monthly.lwd
  # uses package zoo
  #
  logical() -> result
  for(anyday_i in anyday) {
    seq(from = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday_i) - within_back), frac = 0)),
          to = as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday_i) - within_back), frac = 1)),
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
  return(anyday == lwd_of_month(anyday, within_back = 0)) 

}
# is_lwd_of_month(c(17164, 17165, 17166))
# [1] FALSE  TRUE FALSE



lbd_of_month <- function(anyday = NULL, within_back = 5) {  

  # uses package zoo

  logical() -> result
  for(anyday_i in anyday) {
    RQuantLib::getEndOfMonth("UnitedStates/NYSE", zoo::as.Date(anyday_i) -  within_back) -> month_day
    as.integer(month_day) -> result_i
    c(result, result_i) -> result
  }
  return(result)
    
}
# lbd_of_month(c(17164, 17165, 17166))
# [1] 17165 17165 17165



yr_of_month <- function(anyday = NULL, within_back = 5) {  

  # uses package zoo

  logical() -> result
  for(anyday_i in anyday) {
    DescTools::Year(zoo::as.Date(anyday_i) -  within_back) -> month_day
    as.integer(month_day) -> result_i
    c(result, result_i) -> result
  }
  return(result)
    
}
# yr_of_month(c(17164, 17165, 17166))
# [1] 2016 2016 2016


yrmnth_of_month <- function(anyday = NULL, within_back = 5) {  

  # uses package zoo

  logical() -> result
  for(anyday_i in anyday) {
    DescTools::YearMonth(zoo::as.Date(anyday_i) -  within_back) -> month_day
    as.integer(month_day) -> result_i
    c(result, result_i) -> result
  }
  return(result)
    
}
# yrmnth_of_month(c(17164, 17165, 17166))
# [1] 201612 201612 201612


# within the xTh month
mnth_since_birth <- function(anyday = NULL, within_back = 5) {

  # "1970-01-01" is 0L

  # uses package zoo

  logical() -> result
  for(anyday_i in anyday) {
                                                              # zoo must be loaded via namespace
    (DescTools::Year(zoo::as.Date(anyday_i)) - 1970) * 12 + cycle(zoo::as.yearmon(zoo::as.Date(anyday_i)  -  within_back)) -> month_day
    as.integer(month_day) - 1L -> result_i
    c(result, result_i) -> result
  }
  return(result)

}
# mnth_since_birth(c(17164, 17165, 17166))
# [1] 563 563 563


mnth_of_month <- function(anyday = NULL, within_back = 5) {  

  # uses package zoo

  logical() -> result
  for(anyday_i in anyday) {
    DescTools::Month(zoo::as.Date(anyday_i) -  within_back) -> month_day
    as.integer(month_day) -> result_i
    c(result, result_i) -> result
  }
  return(result)
    
}
# mnth_of_month(c(17164, 17165, 17166))
# [1] 12 12 12



last_day_of_month <- function(anyday = NULL, within_back =  5) {  
  # uses package zoo
             # S3 dispatch
             # zoo::as.Date.yearmon(. . . , frac = 1)
  as.integer(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(anyday) - within_back), frac = 1))
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

# uses to.monthly.lwd, getsetvar_aaii_sipro_dir, package zoo
getvar_all_load_days <- function () {
  
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
    
    return(all_load_days)
    
  }
  ret <- getvar_all_load_days_lwd_inner()
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
}
# getvar_all_load_days()







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

                      # datish: xts::is.timeBased is converted to integer ( so fractional parts are lost )
                      # everything else must be EXPLICITY filtered here ( else it falls out to become(stay?)'text' )
                      #
financize <- function(df                                                      # FIX? FOR SOME REASON  ???
                      , int_col_rexpr = "sic|employees|^perlen_q.*$|stmtid" # MANUAL ( date_eq0 DID NOT MATCH )
                      , stringsAsFactors = FALSE       # untested # most likely upsiszed to a database to be an integer?
                      , char_col_rexpr = "^pertyp_q.*$"
                      # , num_col_rexpr = "price|mktcap|^.*_q.*$"
                      , num_col_rexpr = "price|mktcap|^.*_q.*$|^prchg_\\d\\dw$|split_fact|bby_1t|stmtstat"
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
              
              # ? xts::is.timeBased Current time-based objects supported are 'Date', 'POSIXct', 'chron', 'yearmon', 'yearqtr', and 'timeDate'.
              # if(xts::xts::xtsible(x)) { # OLD xts - do no use - new xts does not allow non-valid values in the index 
              if(xts::is.timeBased(x)) { # detects Date 
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
                  message(stri_c("numeric(x) conversion failed for column: " %s+% col__names[col__names_iter])) 
                  return(x)
                }
                if(!is.null(round_to_decimal_places)) {
                  out <- try( { round(x, digits=round_to_decimal_places)  }, silent = TRUE)
                  if ( !"try-error" %in% class(out)) { 
                    x <- out 
                  } else { 
                    message(stri_c("round(x, digits=round_to_decimal_places) conversion failed for column: " %s+% col__names[col__names_iter])) 
                    return(x)
                  }
                }
                if(!is.null(char_col_numeric_limit)) {
                  over_the_limit_tf <- {char_col_numeric_limit < x}
                  print(stringi::stri_c("  Note, these many NA_real_s found in x: " %s+% sum(is.na(x)), ignore_null = TRUE))
                  if(any(over_the_limit_tf, na.rm = TRUE)) {                           #  NROW(x[!is.na(x)][x[!is.na(x)] > char_col_numeric_limit]) # SAME
                    message(stri_c("  Note, these many OVER THE LIMIT found in x: " %s+% sum(over_the_limit_tf, na.rm = TRUE), ignore_null = TRUE))
                    print(stri_c("over_the_limit_tf <- x[" %s+% char_col_numeric_limit %s+% " < x] records found for column: " %s+% col__names[col__names_iter]))
                    print(stri_c("Printing those " %s+% sum(over_the_limit_tf, na.rm = TRUE) %s+% " (column_ids)(if any) records Now."))
                    if("company_id" %in% col__names) { print(cbind(df[,"company_id",drop = FALSE],x = x)[!is.na(x) &  { x > 999999.9 },,drop = FALSE])  }
                    out <- try( { x[char_col_numeric_limit < x] <- NA_real_ ; x }, silent = TRUE)
                    if ( !"try-error" %in% class(out)) { 
                      print(stri_c("  SUCCESS for ... over_the_limit_tf <- x[char_col_numeric_limit < x] records found for column: " %s+% col__names[col__names_iter]))
                      x <- out 
                    } else { 
                      message(stri_c("Conversion ACTUALLY failed for ... x[char_col_numeric_limit < x] <- NA_real_ conversion failed for column: " %s+% col__names[col__names_iter])) 
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
                    message(stri_c("round(x, digits=round_to_decimal_places) conversion failed for column: " %s+% col__names[col__names_iter])) 
                    return(x)
                  }
                }
                if(!is.null(char_col_numeric_limit)) {
                  over_the_limit_tf <- {char_col_numeric_limit < x}
                  print(stringi::stri_c("  Note, these many NA_real_s found in x: " %s+% sum(is.na(x)), ignore_null = TRUE))
                  if(any(over_the_limit_tf, na.rm = TRUE)) {                           #  NROW(x[!is.na(x)][x[!is.na(x)] > char_col_numeric_limit]) # SAME
                    message(stri_c("  Note, these many OVER THE LIMIT found in x: " %s+% sum(over_the_limit_tf, na.rm = TRUE), ignore_null = TRUE))
                    print(stri_c("over_the_limit_tf <- x[" %s+% char_col_numeric_limit %s+% " < x] records found for column: " %s+% col__names[col__names_iter]))
                    print(stri_c("Printing those " %s+% sum(over_the_limit_tf, na.rm = TRUE) %s+% " (column_ids)(if any) records Now."))
                    if("company_id" %in% col__names) { print(cbind(df[,"company_id",drop = FALSE],x = x)[!is.na(x) &  { x > 999999.9 },,drop = FALSE])  }
                    out <- try( { x[char_col_numeric_limit < x] <- NA_real_ ; x }, silent = TRUE)
                    if ( !"try-error" %in% class(out)) { 
                      print(stri_c("  SUCCESS for ... over_the_limit_tf <- x[char_col_numeric_limit < x] records found for column: " %s+% col__names[col__names_iter]))
                      x <- out 
                    } else { 
                      message(stri_c("Conversion ACTUALLY failed for ... x[char_col_numeric_limit < x] <- NA_real_ conversion failed for column: " %s+% col__names[col__names_iter])) 
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
        message(stringi::stri_c("In the call to rm_df_dups, parameter element cols[i] '", col_i, "' was entered that does not exist in df"))
      }
    }
  } else {
    message(stringi::stri_c("In the call to rm_df_dups, no parameter element cols[i] was entered that exists in df"))
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


# lcase_a_remove_useless_columns <- function(df) {
# 
#   require(stringr)
# 
#   # all lower ( PostgreSQL friendly )
#   colnames(df) <- tolower(colnames(df))
#   
#   # remove useless columns
#   # nothing starts with 'x'
#   # x_nullflags, x., x, x.1, x.2 ...
#   # 
#   df[, !str_detect(colnames(df),"^x\\.?+")   & 
#                  !str_detect(colnames(df),"^repno$")   & 
#                  !str_detect(colnames(df),"^lastmod$") &
#                  !str_detect(colnames(df),"^updated$") &
#                  !str_detect(colnames(df),"^business$|^analyst_fn$") # UNTESTED - si_ci binary garbage
#   , drop = FALSE] -> df
#   
#   return(df)
# 
# }

lcase_a_remove_useless_columns <- function(df, prefix = NULL) {

  require(stringr)

  # all lower ( PostgreSQL friendly )
  colnames(df) <- tolower(colnames(df))
  
  # remove useless columns
  # nothing starts with 'x'
  # x_nullflags, x., x, x.1, x.2 ...
  # 
  df[,           !str_detect(colnames(df),"^x\\.?+")   & 
                 !str_detect(colnames(df),"^repno$")   & 
                 !str_detect(colnames(df),"^business$|^analyst_fn$")
  , drop = FALSE] -> df
  
  if(!is.null(prefix)) {
    colnames(df)[str_detect(colnames(df),"^lastmod$")] <- str_c(prefix, "_", "lastmod")
    colnames(df)[str_detect(colnames(df),"^updated$")] <- str_c(prefix, "_", "updated")
  }
  
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
              ;", conn.id = cid)

    # SAVE FOR LATER
    # 
    # VIEWS WORK DIFFERNETLY THAT IN 'Oracle' or "Microsoft SQL Server"
    # ERROR:  cannot change name of view column "prchg_04w_ann" to "prchg_f04w_ann"
    # 
    # 
    # # save space ( if nothing else )
    # db.q("create or replace view si_finecon2_jos as select * from  
    #               si_finecon2                                                      where 
    #                 adr = 0 and exchange != 'O' and mktcap > 200.0 
    #                 and (company !~~ '%iShares%') and (company !~~ '%Vanguard%') and (company !~~ 'SPDR') 
    #                 and (company !~~ '%PowerShares%') and (company !~~ '%Fund%') 
    #                 and (company !~~ '%Holding%') and (industry_desc !~~ '%Investment Service%')
    #           ;", conn.id = cid)
  
  }
      
   
}
# verify_finecon_jamesos_partial_idx() 



verify_finecon_sp_500_partial_idx <- function() {

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
      
  if(all(c("dateindex","sp") %in% names(fc_meta))) {
  
    # create index if not exists
    # NOTE: the ORIGINAL did NOT include 'dateindex' ( I am adding that here ... )
    #  Note: BEST performance table is sorted by : sort by dateindex, company_id
    # 
    #

    db.q("create index if not exists si_finecon2_finecon_sp_500_partial_idx on 
                  si_finecon2(dateindex,sp) where sp = '500'
              ;", conn.id = cid)
    
    ## SAVE FOR LATER
    # # save space ( if nothing else )
    # db.q("create or replace view si_finecon2_sp_500 as select * from  
    #               si_finecon2 where sp = '500'
    #           ;", conn.id = cid)
  
  }
      
}
# verify_finecon_sp_500_partial_idx() 

verify_finecon_sp_partial_idx <- function() {

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
      
  if(all(c("dateindex","sp") %in% names(fc_meta))) {
  
    # create index if not exists
    # NOTE: the ORIGINAL did NOT include 'dateindex' ( I am adding that here ... )
    #  Note: BEST performance table is sorted by : sort by dateindex, company_id
    # 
    #

    db.q("create index if not exists si_finecon2_finecon_sp_partial_idx on 
                  si_finecon2(dateindex,sp) where sp in ('500','400','600')
              ;", conn.id = cid)
    
    db.q("create index if not exists si_finecon2_finecon_sp_500_sector_desc_partial_idx on 
                  fe_data_store.si_finecon2(dateindex,sp,sector_desc) where sp = '500'
              ;", conn.id = cid)
    
    ## SAVE FOR LATER
    # # save space ( if nothing else )
    # db.q("create or replace view si_finecon2_sp as select * from  
    #               si_finecon2 where sp in ('500','400','600')
    #           ;", conn.id = cid)
  
  }
      
}
# verify_finecon_sp_partial_idx() 



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

  message(gsub("\"","",capture.output(match.call())))
  print(keys)
  
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
  # if(!db.q(str_c("select count(*) from information_schema.tables where table_name = 'si_finecon2';"), nrows = -1, conn.id = cid)) {
  if(!dbExistsTable(con, "si_finecon2")) {
    verify_si_finecon_exists()
  }

  # SHOULD HAVE column dateindex, ELSE I can not (later in the function) join 
  if(any(colnames(value) %in% "dateindex")) {

    unique(value$dateindex) -> value_unq_index

    Sys.sleep(2)
    message("if NOT *Index Only Scan using ?si_finecon2_dateindex_idx?*, then MOST-LIKELY I need a REINDEX")
    explain_result <- db.q(str_c("explain select distinct dateindex from si_finecon2;"), nrows = -1, conn.id = cid)
    explain_result_full_message <-paste(explain_result[[1]], collapse = " ")
    message(explain_result_full_message)
    if(!grepl(".*Index Only Scan.*", explain_result_full_message)) {
      message("Since not found: Index Only Scan ... vacuum_analyze_reindex ... starting")
      vacuum_analyze_reindex(index = "fe_data_store.si_finecon2_dateindex_idx")
      message("Since not found: Index Only Scan ... vacuum_analyze_reindex ... finished")
    }
    Sys.sleep(5)
    
    # FAULTY LOGIC ( HAVE TO RETURN TO THIS 'TOPIC' )  
    #    ** DO I NEED A 'SETDIFF INSTEAD?' **
    # what 'value df dateindex' are already in the database
    value_unq_index[
      value_unq_index %in% as.vector(unlist(db.q(str_c("select distinct dateindex from si_finecon2;"), nrows = -1, conn.id = cid)))
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
  #
  ## "numeric(8,2)"  -> fc_new_columns[fc_new_columns == "numeric"]  ###
  # new columns to be put in fc
  # for numeric greater than 999999.99 make them bigger
  for (colnames_i in names(fc_new_columns)[fc_new_columns == "numeric"]) {
  
    db_numeric_column_i_storage_limit <- as.numeric("999999.99")
    
    max_value_colnames_i <- max(value[[colnames_i]], na.rm = TRUE)
    min_value_colnames_i <- min(value[[colnames_i]], na.rm = TRUE)
    
    # >= because R may be inprecise
    if((max_value_colnames_i >= db_numeric_column_i_storage_limit) || ( -1 * db_numeric_column_i_storage_limit >= min_value_colnames_i)) {
    
      # get numer of 9s to the left of the decimal
      new_numeric <- numb.digits.left.of.decimal(max(max_value_colnames_i, abs(min_value_colnames_i)))
      str_c("numeric(", new_numeric + 2, ", 2)") -> fc_new_columns[names(fc_new_columns ) == colnames_i]
      
    } else {
    
            "numeric(8,2)"                       -> fc_new_columns[names(fc_new_columns ) == colnames_i]
    }
      
  } 
  #
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
    message("in call to function upsert, no new columns were found to be needed to be added to si_finecon2.  Is this correct?")
  }
  
  # current columns ( or columns added or not added by ""if(length(fc_new_columns) > 0L)"" ) to be updated
  value_meta[names(value_meta)  %in% names(fc_meta)] -> fc_tobeupdated_columns
  
  # current columns in fc
  # no numeric greater than 999999.99
  for (colnames_i in names(fc_tobeupdated_columns)[fc_tobeupdated_columns == "numeric"]) {
  
    schema_current <- db.q(str_c("select current_schema();"), nrows = -1, conn.id = cid)[1,1,drop = TRUE]
    precision_numeric <- db.q(str_c("select numeric_precision from information_schema.columns where table_schema = '", schema_current, "' and table_name = '", "si_finecon2", "' and column_name = '", colnames_i, "';"), nrows = -1, conn.id = cid)[1,1,drop = TRUE]
    db_numeric_column_i_storage_limit <- as.numeric(paste0(paste0(rep("9", precision_numeric - 2), collapse = ""),".99"))
    
    max_value_colnames_i <- max(value[[colnames_i]], na.rm = TRUE)
    min_value_colnames_i <- min(value[[colnames_i]], na.rm = TRUE)
    # >= because R may be inprecise
    if((max_value_colnames_i >= db_numeric_column_i_storage_limit) || ( -1 * db_numeric_column_i_storage_limit >= min_value_colnames_i)) {
    
      # get numer of 9s to the left of the decimal
      new_numeric <- numb.digits.left.of.decimal(max(max_value_colnames_i, abs(min_value_colnames_i)))
      db.q(str_c("alter table ", "si_finecon2", " alter column ", colnames_i, " type numeric(", new_numeric + 2, ", 2);"), conn.id = cid)
      
    }
     
  } 
  # to what is in the database ( just in case I did something above )
  db.data.frame("si_finecon2", conn.id = cid, verbose = FALSE) -> ptr_si_finecon2  # class (db.#)
  col.types(ptr_si_finecon2) -> fc_meta
  names(ptr_si_finecon2) -> names(fc_meta) 

  

  # PREPARE FOR UPSERTS/UPDATES #
  
  # add a 'dataindex + keys'primary key column to input 'value data.fram ( needed for PivotalR and OTHER things ) 
  # currenly 'really' only tested used with/about company_id

  # NOTICE
  # str_c(c("dateindex",character()), collapse = "_")
  # [1] "dateindex"
  # str_c(c("dateindex",""), collapse = "_")
  # [1] "dateindex_"
  str_c(c("dateindex",keys), collapse = "_")  -> value_primary_key
  str_c(c("dateindex",keys), collapse = " %s+% \'_\' %s+% ") -> value_primary_key_concat_expr
  # chokes often
  # with( value, { eval(parse(text=eval(parse(text=('str_c(c("dateindex",keys), collapse = " %s+% \'_\' %s+% ")'))))) } ) -> value[,value_primary_key]
  with(value,{ eval(parse(text=value_primary_key_concat_expr)) } ) -> value[[value_primary_key]]
  DataCombine::MoveFront(value,value_primary_key) -> value
  
  # upload 'value' into the database 
  
  # eventually
  # CAN GET HUNG UP HERE!!
  message("Begin - drop table if exists upsert_temp")
  {function() { db.q("drop table if exists upsert_temp", conn.id = cid) }} -> drop_upsert_temp
  # TRY LESSEN THE CONGESTION 
  # try( { db.q("delete from upsert_temp;", conn.id = cid) }, silent = TRUE )
  # try( { db.q("truncate table upsert_temp;", conn.id = cid) }, silent = TRUE )
  db.q("select pg_sleep(1);", conn.id = cid)
  drop_upsert_temp()
  db.q("select pg_sleep(1);", conn.id = cid)
  message("END - drop table if exists upsert_temp")
  # # upsert into the database
  # SEEMS must CREATE A pk THIS WAY
  db.q("select pg_sleep(1);", conn.id = cid) # MANUALLY HANDLED BELOW: # , key = value_primary_key
  as.db.data.frame(value, "upsert_temp", is.temp = if(!is.null(getOption("upsert_temp_is_temporary"))) { TRUE } else { FALSE }, conn.id = cid, verbose = FALSE) -> ptr_upsert_temp
  db.q("select pg_sleep(1);", conn.id = cid)
  
  # # db.q(str_c("create unique index upsert_temp_unqpkidx on upsert_temp(" %s+% value_primary_key %s+% ");"), conn.id = cid)
  # 
  # # garantee that upsert_Temp values are unique
  # try( { db.q(str_c("alter table if exists upsert_temp add primary key(" %s+% value_primary_key %s+% ");"), conn.id = cid) }, silent = TRUE )
  # # upsert_temp_<value_primary_key>_pkey ( singleton )
  # 
  # # db.q(str_c("alter table upsert_temp add constraint upsert_temp_unqkey unique(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid)
  # # db.q(str_c("create unique index upsert_temp_unqidx on upsert_temp(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid)
  # 
  # # try( { db.q(str_c("alter table if exists upsert_temp add unique(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid) }, silent = TRUE )
  # 
  # # garantee unqueness 
  # try( { db.q(str_c("create unique index if not exists upsert_temp_keyz_key on upsert_temp(" %s+% str_c(c("dateindex",keys), collapse = ", ") %s+% ");"), conn.id = cid) }, silent = TRUE )
  # # upsert_temp_keyz_key ( can be many )
  
  # SHOULD always BE IN A database
  current_database    <- db.q("SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", conn.id = cid)[[1]]
  current_search_path <-  strsplit(db.q("show search_path;", conn.id = cid)[[1]],",[ ]*")[[1]]
  current_schema      <- db.q("select current_schema();", conn.id = cid)[[1]] 
  # MUST put SOMETHING in there FIRST
  res <- db.q("SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", conn.id = cid) 
  current_temp_schema <- if(NROW(res) == 1) res[[1]] else ""; rm(res)
  SQuote <- function(x) { paste0("'", x, "'") }
  
  # garantee that upsert_Temp values are unique
  # 'dateindex' and 'some other column(s)'
  if( value_primary_key != "dateindex" ) {
  
    db.q(str_c("create unique index if not exists upsert_temp_pkey on upsert_temp(" %s+% value_primary_key %s+% ");"), conn.id = cid)
    db.q(str_c("
    do $$ begin
    if not exists(select constraint_name 
                    from information_schema.table_constraints 
                      where constraint_schema   in  (", SQuote(current_schema),", ", SQuote(current_temp_schema),")
                      and   table_name          = 'upsert_temp' 
                      and   constraint_type     = 'PRIMARY KEY'
    ) then
      -- psql message
      -- log_min_messages and client_min_messages 
      raise WARNING USING MESSAGE = 'Creating . . .  ' || '_schema_' || '.' || 'upsert_temp' || ' PRIMARY KEY using INDEX';
      alter table if exists upsert_temp add primary key using index upsert_temp_pkey;
    else
      raise NOTICE USING MESSAGE = 'Already exists ' || '_schema_' || '.' || 'upsert_temp' || ' PRIMARY KEY';
    end if;
    end $$
    "), conn.id = cid)
  
  }
  # since 'dateindex' itself can not be a primary key ( 'dateindex' is not unique )
  if( value_primary_key == "dateindex" ) {
    db.q(str_c("create index if not exists upsert_temp_mainidx on upsert_temp(" %s+% value_primary_key %s+% ");"), conn.id = cid)
  }
  
  
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
  # as.db.data.frame(ptr_upsert_temp[,], table.name ="upsert_temp_greater", is.temp = if(!is.null(getOption("upsert_temp_is_temporary"))) { TRUE } else { FALSE }, field.types = LL) -> ptr_upsert_temp_gr
  
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
  
  # if( all(c("dateindex_company_id_orig","ticker") %in% names(upsert_meta)) ) {
  # 
  #   # detect if si_finecon2.dateindex_company_id_orig exists, then this is th required MIN columns to be updated 
  #   if (!any( !c("dateindex_company_id_orig", "dateindex_company_id", "dateindex", 
  #                          "company_id",      "company_id_orig", "ticker", "company") %in% names(upsert_meta) )) {
  #     # si_ci GO EXACTLY HERE
  #     FALSE           -> upsert_temp_perform_nothing
  #     TRUE            -> upsert_temp_perform_upsert
  #     "dateindex_company_id_orig" -> conflict_column
  #   } else {
  #     TRUE           -> upsert_temp_perform_nothing # NOTHING IS HERE
  #     FALSE          -> upsert_temp_perform_upsert
  #     stop("dateindex_company_id_orig and ticker exist BUT the mininum columns do not")
  #   }
  # } else { 
  #   if("dateindex_company_id_orig" %in% names(upsert_meta)) { 
  #     # si_NON_ci GO EXACTLY HERE
  #     FALSE                           -> upsert_temp_perform_nothing
  #     FALSE                           -> upsert_temp_perform_upsert # then do update-ish
  #     "dateindex_company_id_orig"     -> conflict_column # STILL
  #   } else {
  #     if("dateindex_company_id" %in% names(upsert_meta)) { # 
  #       # load_inbnd_stmtstats EXACTLY HERE
  #       FALSE                      -> upsert_temp_perform_nothing
  #       FALSE                      -> upsert_temp_perform_upsert # then do update-ish
  #       "dateindex_company_id"     -> conflict_column 
  #     } 
  #     if("dateindex" %in% names(upsert_meta)) { # 
  #       # verify_return_dates(dateindex) EXACTLY HERE 
  #       FALSE                      -> upsert_temp_perform_nothing
  #       FALSE                      -> upsert_temp_perform_upsert # then do update-ish
  #       "dateindex"                -> conflict_column 
  #     } 
  #     # else
  #     TRUE            -> upsert_temp_perform_nothing
  #     FALSE           -> upsert_temp_perform_upsert
  #     stop(str_c("update_temp dateindex_company_id_orig NOT exist AND ticker NOT exist " %s+% " and primary key is only "  %s+% str_c(c("dateindex",keys), collapse = ", ")))
  #     
  #   }
  # }
  

  ### strait QUERY of the disk ### ( ci, mgdsc, exchg )
  ## verify_company_basics ## ( NOT directly tried to UPDATED/UPSERT into the database )
  #
  #  dateindex_company_id_orig # dateindex  # ticker  # street
  #                            # company_id # company
  # company_id_orig            
  # NOT PASSED TO UPSERT 
  # data passed IN from verify_company_basics          NOT PASSED OUT
  # IF "if(ref < 15184 )" THEN update (set company_id, dateindex_company_id) using (company_id, ticker , street)
  # OTHERWISE .. make NO CHANGE in data values
  # INHERITS from ABOVE  
  #
  ## update_from_future_new_company_ids ##
  #
  # dateindex_company_id_orig # dateindex  # ticker  # street
  #                           # company_id # company
  # company_id_orig   
  # PASSED TO UPSERT
  
  
  ### strait QUERY of the disk ### (non ci)
  ## verify_company_details ##
  # 
  # dateindex_company_id_orig # dateindex 
  # dateindex_company_id      # company_id
  # company_id_orig
  # PASSED TO UPSERT (non ci)
  
  
  ### no QUERY of anything ... strait math generation ###
  ## verify_return_dates ##
  #
  #                          # dateindex
  # 
  # PASSED TO UPSERT (generated dates only)
  
  
  ### strait QUERY of the database ###
  ## verify_week_often_week_returns ##      # ( REPLACE dateindex_company_id_orig by dateindex_company_id ) TO_DO [X]
  # dateindex_company_id      # dateindex 
  #                           # company_id
  # PASSED TO UPSERT
  
  
  ### strait QUERY of the database ###
  ## verify_month_often_month_past_returns ##
  # dateindex_company_id      # dateindex   # ( REPLACE dateindex_company_id_orig by dateindex_company_id ) TO_DO [X]
  #                           # company_id
  # PASSED TO UPSERT
  
  
  ### strait QUERY of the database ###
  ## load_inbnd_stmtstats ##
  #  dateindex_company_id*   # dateindex #  ( ADD dateindex_company_id ) TO_DO [x]
  #                          # company_id
  # PASSED TO UPSERT
  
  
  # **** IMPORTANT ( what UPDATE/UPSERT and what CONFLICT column is CHOSEN base on "what" column(z) are sent ) ****
  # **** heirarchy: "ticker", "dateindex_company_id_orig", "dateindex_company_id", "dateindex" ****
  
  Chosen_UpsertUpdate_Path_Done <- FALSE
  
  # ci, msdsc, exchg ( verify_company_basics -> update_from_future_new_company_ids )
  if(("ticker" %in% names(upsert_meta)) && 
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                       -> upsert_temp_perform_nothing
    TRUE                        -> upsert_temp_perform_upsert
    "dateindex_company_id_orig" -> conflict_column
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  # non_ci ( verify_company_details )
  if(("dateindex_company_id_orig" %in% names(upsert_meta)) && 
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                           -> upsert_temp_perform_nothing
    FALSE                           -> upsert_temp_perform_upsert 
    "dateindex_company_id_orig"     -> conflict_column
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  # db_query_only ( verify_week_often_week_returns, verify_month_often_month_past_returns, load_inbnd_stmtstats )
  if(( "dateindex_company_id"      %in% names(upsert_meta)) && 
     (!"dateindex_company_id_orig" %in% names(upsert_meta)) &&
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                      -> upsert_temp_perform_nothing
    FALSE                      -> upsert_temp_perform_upsert 
    "dateindex_company_id"     -> conflict_column 
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  # math_only ( verify_return_dates )
  if(( "dateindex"                 %in% names(upsert_meta)) && 
     (!"company_id"                %in% names(upsert_meta)) &&
     (!"dateindex_company_id     " %in% names(upsert_meta)) &&
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                      -> upsert_temp_perform_nothing
    FALSE                      -> upsert_temp_perform_upsert 
    "dateindex"                -> conflict_column 
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  if(!Chosen_UpsertUpdate_Path_Done)       stop("Chosen_UpsertUpdate_Path_Done has not been completed.")
  if(!length(upsert_temp_perform_nothing)) stop("upsert_temp_perform_nothing is not determined") # LATER REMOVE
  if(!length(upsert_temp_perform_upsert))  stop("upsert_temp_perform_upsert is not determined")  # LATER REMOVE
  
  if(!upsert_temp_perform_nothing && upsert_temp_perform_upsert) {
  
    # actually perform the upsert
    str_trim(str_c(rstring('<%= 
    paste0(
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
  
  ### then do update-ish STILL based on company_id_orig  
  ### ( and do not UPDATE 
  ###   dateindex, company_id, dateindex_company_id, company_id_orig         # at least ONE COLUMN to try to update ( so the SQL DML UPDATE statement is valid ) # BELOW ARE 'non-updatable'
  ##if(!upsert_temp_perform_nothing && !upsert_temp_perform_upsert && sum(!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")) ) {
    
  if(!upsert_temp_perform_nothing && !upsert_temp_perform_upsert) {
  
    # avoid sprintf 8192 character limit
    # # actually perform the update 
    # str_trim(str_c(rstring(str_c('<%= 
    # paste0(
    # "update \\"', target_table_name, '\\" s " %s+% " set " %s+% 
    #  str_c(sprintf("\n  %1$s = t.%1$s", names(upsert_meta)[!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")] ), collapse = ", ") %s+% " \n" %s+%
    # "    from upsert_temp t " %s+% " \n" %s+%
    # "      where " %s+% "s." %s+% conflict_column %s+% " = " %s+% "t." %s+% conflict_column  %s+% ";" 
    # )
    # %>'))))  %>% clean_text(.) -> fc_col_val_changes_sql
    
    col_names <- names(upsert_meta)[!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")]
    # actually perform the update
    str_trim(str_c(rstring(str_c('<%= 
    paste0(
    "update \\"', "si_finecon2", '\\" s " %s+% " set " %s+% 
     str_c(str_c(str_c("\n ", col_names), str_c(" = t.", col_names)), collapse = ", ") %s+% " \n" %s+%
    "    from upsert_temp t " %s+% " \n" %s+%
    "      where " %s+% "s." %s+% conflict_column %s+% " = " %s+% "t." %s+% conflict_column  %s+% ";" 
    )
    %>'))))  %>% clean_text(.) -> fc_col_val_changes_sql
    
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
    verify_finecon_sp_500_partial_idx()
    verify_finecon_sp_partial_idx()
  }
  
  drop_upsert_temp()
  
  options(ops)
  return(invisible(NULL))
   
}




## assumes(uses) that input value(data.frame) ALREADY has a column called dateindex
##   (future) SHOULD BE VECTORIZED: INPUT MANY 'values's (data.frames)
# NOTE: keys MUST be entered in lowercase
upsert2 <-  function(value = NULL, keys = NULL, target_table_name = "si_finecon2", upsert_temp_perform_upsert_force = FALSE) { # vector of primary key values

  message(gsub("\"","",capture.output(match.call())))
  print(keys);
  
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
  
  # SHOULD BE PUT 'outside' OF THE FUNCTION
  # verify that target_table_name exists  
  # if(!db.q(str_c("select count(*) from information_schema.tables where table_name = '", target_table_name, "';"), nrows = -1, conn.id = cid)) {
  if(!dbExistsTable(con, target_table_name)) {
    if(target_table_name == "si_finecon2") {
      verify_si_finecon_exists()
    }
    if(target_table_name == "si_finecon2_aggregates") {
      verify_si_finecon_aggregates_exists()
    }
  }

  # SHOULD HAVE column dateindex, ELSE I can not (later in the function) join 
  if(any(colnames(value) %in% "dateindex")) {

    unique(value$dateindex) -> value_unq_index

    Sys.sleep(2)
    message("if NOT *Index Only Scan using ?si_finecon2_dateindex_idx?*, then MOST-LIKELY I need a REINDEX")
    explain_result <- db.q(str_c("explain select distinct dateindex from si_finecon2;"), nrows = -1, conn.id = cid)
    explain_result_full_message <-paste(explain_result[[1]], collapse = " ")
    message(explain_result_full_message)
    if(!grepl(".*Index Only Scan.*", explain_result_full_message)) {
      message("Since not found: Index Only Scan ... vacuum_analyze_reindex ... starting")
      vacuum_analyze_reindex(index = "fe_data_store.si_finecon2_dateindex_idx")
      message("Since not found: Index Only Scan ... vacuum_analyze_reindex ... finished")
    }
    Sys.sleep(5)
    
    # FAULTY LOGIC ( HAVE TO RETURN TO THIS 'TOPIC' )  
    #    ** DO I NEED A 'SETDIFF INSTEAD?' **
    # what 'value df dateindex' are already in the database
    value_unq_index[
      value_unq_index %in% as.vector(unlist(db.q(str_c("select distinct dateindex from ", dbQuoteIdentifier(con, target_table_name), ";"), nrows = -1, conn.id = cid)))
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
  db.data.frame(target_table_name, conn.id = cid, verbose = FALSE) -> ptr_target_table_name  # class (db.#)
  col.types(ptr_target_table_name) -> fc_meta
      names(ptr_target_table_name) -> names(fc_meta) 
  
  # names(with types) in value that are 'value only'(setdiff) that need to (soon) be new columns in 'fc'
  value_meta[names(value_meta) %in% setdiff(names(value_meta),names(fc_meta))] -> fc_new_columns

  # plan to change "numeric"(R) to "numeric(8,2)"(PostgreSQL)
  #
  # + other custom column types
  # 
  "text"          -> fc_new_columns[fc_new_columns == "character"] 
  #
  ## "numeric(8,2)"  -> fc_new_columns[fc_new_columns == "numeric"]  ###
  # new columns to be put in fc
  # for numeric greater than 999999.99 make them bigger
  for (colnames_i in names(fc_new_columns)[fc_new_columns == "numeric"]) {
  
    db_numeric_column_i_storage_limit <- as.numeric("999999.99")
    
    max_value_colnames_i <- max(value[[colnames_i]], na.rm = TRUE)
    min_value_colnames_i <- min(value[[colnames_i]], na.rm = TRUE)
    
    # >= because R may be inprecise
    if((max_value_colnames_i >= db_numeric_column_i_storage_limit) || ( -1 * db_numeric_column_i_storage_limit >= min_value_colnames_i)) {
    
      # get numer of 9s to the left of the decimal
      new_numeric <- numb.digits.left.of.decimal(max(max_value_colnames_i, abs(min_value_colnames_i)))
      str_c("numeric(", new_numeric + 2, ", 2)") -> fc_new_columns[names(fc_new_columns ) == colnames_i]
      
    } else {
    
            "numeric(8,2)"                       -> fc_new_columns[names(fc_new_columns ) == colnames_i]
    }
      
  } 
  #
  "smallint"      -> fc_new_columns[names(fc_new_columns) %in% c("drp_avail","adr")] 
   
  # actually add NEW columns to si_finecon 
  # if any columns exist to add
  # EXPECTED 'IF-THEN' to be extended
  if(length(fc_new_columns) > 0L) {
  
    # to fc, add new columns ( of 'new columns' from 'value' that do not exist in 'fc' ) 
    str_trim(str_c(rstring(str_c('
    alter table if exists ', dbQuoteIdentifier(con, target_table_name), '
      <% for (i in seq_along(fc_new_columns)) { -%>
          <% sprintf("add %1$s %2$s", 
               names(fc_new_columns)[i], fc_new_columns[i]) -> res
          -%><%= str_c("    ",res) %><%=if(i  < length(fc_new_columns)) ", \n" -%><%=if(i == length(fc_new_columns)) "  \n" -%>
      <% } %>
    ;                     
    '))))  %>% clean_text(.) -> add_columns_sql # 
              # remove at the beginning
              # debug at <text>#30: .base_paste0 <- base::paste0\n\n 
              # stop the rstudio debugger OUTPUT from going inside the string

    db.q(add_columns_sql, conn.id = cid)
    
    # to what is in the database
    db.data.frame(target_table_name, conn.id = cid, verbose = FALSE) -> ptr_target_table_name  # class (db.#)
    col.types(ptr_target_table_name) -> fc_meta
        names(ptr_target_table_name) -> names(fc_meta) 
    
  } else {
    message(str_c("in call to function upsert, no new columns were found to be needed to be added to ", target_table_name, ".  Is this correct?"))
  }
  
  # current columns ( or columns added or not added by ""if(length(fc_new_columns) > 0L)"" ) to be updated
  value_meta[names(value_meta)  %in% names(fc_meta)] -> fc_tobeupdated_columns
  
  # current columns in fc
  # no numeric greater than 999999.99
  for (colnames_i in names(fc_tobeupdated_columns)[fc_tobeupdated_columns == "numeric"]) {
  
    schema_current <- db.q(str_c("select current_schema();"), nrows = -1, conn.id = cid)[1,1,drop = TRUE]
    precision_numeric <- db.q(str_c("select numeric_precision from information_schema.columns where table_schema = '", schema_current, "' and table_name = '", target_table_name, "' and column_name = '", colnames_i, "';"), nrows = -1, conn.id = cid)[1,1,drop = TRUE]
    db_numeric_column_i_storage_limit <- as.numeric(paste0(paste0(rep("9", precision_numeric - 2), collapse = ""),".99"))
    
    max_value_colnames_i <- max(value[[colnames_i]], na.rm = TRUE)
    min_value_colnames_i <- min(value[[colnames_i]], na.rm = TRUE)
    # >= because R may be inprecise
    if((max_value_colnames_i >= db_numeric_column_i_storage_limit) || ( -1 * db_numeric_column_i_storage_limit >= min_value_colnames_i)) {
    
      # get numer of 9s to the left of the decimal
      new_numeric <- numb.digits.left.of.decimal(max(max_value_colnames_i, abs(min_value_colnames_i)))
      db.q(str_c("alter table ", dbQuoteIdentifier(con, target_table_name), " alter column ", colnames_i, " type numeric(", new_numeric + 2, ", 2);"), conn.id = cid)
      
    }
     
  } 
  # to what is in the database ( just in case I did something above )
  db.data.frame(target_table_name, conn.id = cid, verbose = FALSE) -> ptr_target_table_name  # class (db.#)
  col.types(ptr_target_table_name) -> fc_meta
  names(ptr_target_table_name) -> names(fc_meta) 

  

  # PREPARE FOR UPSERTS/UPDATES #
  
  # add a 'dataindex + keys'primary key column to input 'value data.fram ( needed for PivotalR and OTHER things ) 
  # currenly 'really' only tested used with/about company_id
  
  # NOTICE
  # str_c(c("dateindex",character()), collapse = "_")
  # [1] "dateindex"
  # str_c(c("dateindex",""), collapse = "_")
  # [1] "dateindex_"
  str_c(c("dateindex",keys), collapse = "_")  -> value_primary_key
  str_c(c("dateindex",keys), collapse = " %s+% \'_\' %s+% ") -> value_primary_key_concat_expr
  # chokes often
  # with( value, { eval(parse(text=eval(parse(text=('str_c(c("dateindex",keys), collapse = " %s+% \'_\' %s+% ")'))))) } ) -> value[,value_primary_key]
  with(value,{ eval(parse(text=value_primary_key_concat_expr)) } ) -> value[[value_primary_key]]
  DataCombine::MoveFront(value,value_primary_key) -> value
  
  # upload 'value' into the database 
  
  # eventually
  # CAN GET HUNG UP HERE!!
  message("Begin - drop table if exists upsert_temp")
  {function() { db.q("drop table if exists upsert_temp", conn.id = cid) }} -> drop_upsert_temp
  # TRY LESSEN THE CONGESTION 
  # try( { db.q("delete from upsert_temp;", conn.id = cid) }, silent = TRUE )
  # try( { db.q("truncate table upsert_temp;", conn.id = cid) }, silent = TRUE )
  db.q("select pg_sleep(1);", conn.id = cid)
  drop_upsert_temp()
  db.q("select pg_sleep(1);", conn.id = cid)
  # # upsert into the database
  # SEEMS must CREATE A pk THIS WAY
  db.q("select pg_sleep(1);", conn.id = cid) # MANUALLY HANDLED BELOW: # , key = value_primary_key
  as.db.data.frame(value, "upsert_temp", is.temp = if(!is.null(getOption("upsert_temp_is_temporary"))) { TRUE } else { FALSE }, conn.id = cid, verbose = FALSE) -> ptr_upsert_temp
  db.q("select pg_sleep(1);", conn.id = cid)

  #   # db.q(str_c("create unique index upsert_temp_unqpkidx on upsert_temp(" %s+% value_primary_key %s+% ");"), conn.id = cid)
  # 
  # # garantee that upsert_Temp values are unique
  # try( { db.q(str_c("alter table if exists upsert_temp add primary key(" %s+% value_primary_key %s+% ");"), conn.id = cid) }, silent = TRUE )
  # # upsert_temp_<value_primary_key>_pkey ( singleton )
  # 
  # # db.q(str_c("alter table upsert_temp add constraint upsert_temp_unqkey unique(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid)
  # # db.q(str_c("create unique index upsert_temp_unqidx on upsert_temp(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid)
  # 
  # # try( { db.q(str_c("alter table if exists upsert_temp add unique(" %s+%  "dateindex, " %s+% str_c(keys,collapse = ", ", sep = "") %s+% ");"), conn.id = cid) }, silent = TRUE )
  # 
  # # garantee unqueness 
  # try( { db.q(str_c("create unique index if not exists upsert_temp_keyz_key on upsert_temp(" %s+% str_c(c("dateindex",keys), collapse = ", ") %s+% ");"), conn.id = cid) }, silent = TRUE )
  # # upsert_temp_keyz_key ( can be many )
  
  # SHOULD always BE IN A database
  current_database    <- db.q("SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", conn.id = cid)[[1]]
  current_search_path <-  strsplit(db.q("show search_path;", conn.id = cid)[[1]],",[ ]*")[[1]]
  current_schema      <- db.q("select current_schema();", conn.id = cid)[[1]] 
  # MUST put SOMETHING in there FIRST
  res <- db.q("SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", conn.id = cid) 
  current_temp_schema <- if(NROW(res) == 1) res[[1]] else ""; rm(res)
  SQuote <- function(x) { paste0("'", x, "'") }
  
  # garantee that upsert_Temp values are unique
  # 'dateindex' and 'some other column(s)'
  if( value_primary_key != "dateindex" ) {
  
    db.q(str_c("create unique index if not exists upsert_temp_pkey on upsert_temp(" %s+% value_primary_key %s+% ");"), conn.id = cid)
    db.q(str_c("
    do $$ begin
    if not exists(select constraint_name 
                    from information_schema.table_constraints 
                      where constraint_schema   in  (", SQuote(current_schema),", ", SQuote(current_temp_schema),")
                      and   table_name          = 'upsert_temp' 
                      and   constraint_type     = 'PRIMARY KEY'
    ) then
      -- psql message
      -- log_min_messages and client_min_messages 
      raise WARNING USING MESSAGE = 'Creating . . .  ' || '_schema_' || '.' || 'upsert_temp' || ' PRIMARY KEY using INDEX';
      alter table if exists upsert_temp add primary key using index upsert_temp_pkey;
    else
      raise NOTICE USING MESSAGE = 'Already exists ' || '_schema_' || '.' || 'upsert_temp' || ' PRIMARY KEY';
    end if;
    end $$
    "), conn.id = cid)
  
  }
  # since 'dateindex' itself can not be a primary key ( 'dateindex' is not unique )
  if( value_primary_key == "dateindex" ) {
    db.q(str_c("create index if not exists upsert_temp_mainidx on upsert_temp(" %s+% value_primary_key %s+% ");"), conn.id = cid)
  }
  
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
  # as.db.data.frame(ptr_upsert_temp[,], table.name ="upsert_temp_greater", is.temp = if(!is.null(getOption("upsert_temp_is_temporary"))) { TRUE } else { FALSE }, field.types = LL) -> ptr_upsert_temp_gr
  
  # upsert ( these columns have a different data type )
  # of upsert_temp these colums are what I want to change  dataypes to match what is in si_finecon2

  # REQUIRED refresh of meta?!
  # ovewrite
  db.data.frame("upsert_temp", conn.id = cid, verbose = FALSE) -> ptr_upsert_temp  # class (db.#)
  col.types(ptr_upsert_temp) -> upsert_meta
      names(ptr_upsert_temp) -> names(upsert_meta)

  # to what is in the database ( RE-calculate fc_meta: REDONE from above )
  # REQUIRED refresh of meta?!
  db.data.frame(target_table_name, conn.id = cid, verbose = FALSE) -> ptr_target_table_name  # class (db.#)
  col.types(ptr_target_table_name) -> fc_meta
      names(ptr_target_table_name) -> names(fc_meta) 

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
  
  # if( all(c("dateindex_company_id_orig","ticker") %in% names(upsert_meta)) ) {
  # 
  #   # detect if si_finecon2.dateindex_company_id_orig exists, then this is th required MIN columns to be updated 
  #   if (!any( !c("dateindex_company_id_orig", "dateindex_company_id", "dateindex", 
  #                          "company_id",      "company_id_orig", "ticker", "company") %in% names(upsert_meta) )) {
  #     # si_ci GO EXACTLY HERE
  #     FALSE           -> upsert_temp_perform_nothing
  #     TRUE            -> upsert_temp_perform_upsert
  #     "dateindex_company_id_orig" -> conflict_column
  #   } else {
  #     TRUE           -> upsert_temp_perform_nothing # NOTHING IS HERE
  #     FALSE          -> upsert_temp_perform_upsert
  #     stop("dateindex_company_id_orig and ticker exist BUT the mininum columns do not")
  #   }
  # } else { 
  #   if("dateindex_company_id_orig" %in% names(upsert_meta)) { 
  #     # si_NON_ci GO EXACTLY HERE
  #     FALSE                           -> upsert_temp_perform_nothing
  #     FALSE                           -> upsert_temp_perform_upsert # then do update-ish
  #     "dateindex_company_id_orig"     -> conflict_column # STILL
  #   } else {
  #     if("dateindex_company_id" %in% names(upsert_meta)) { # 
  #       # load_inbnd_stmtstats EXACTLY HERE
  #       FALSE                      -> upsert_temp_perform_nothing
  #       FALSE                      -> upsert_temp_perform_upsert # then do update-ish
  #       "dateindex_company_id"     -> conflict_column 
  #     } 
  #     if("dateindex" %in% names(upsert_meta)) { # 
  #       # verify_return_dates(dateindex) EXACTLY HERE 
  #       FALSE                      -> upsert_temp_perform_nothing
  #       FALSE                      -> upsert_temp_perform_upsert # then do update-ish
  #       "dateindex"                -> conflict_column 
  #     } 
  #     # else
  #     TRUE            -> upsert_temp_perform_nothing
  #     FALSE           -> upsert_temp_perform_upsert
  #     stop(str_c("update_temp dateindex_company_id_orig NOT exist AND ticker NOT exist " %s+% " and primary key is only "  %s+% str_c(c("dateindex",keys), collapse = ", ")))
  #     
  #   }
  # }
  

  ### strait QUERY of the disk ### ( ci, mgdsc, exchg )
  ## verify_company_basics ## ( NOT directly tried to UPDATED/UPSERT into the database )
  #
  #  dateindex_company_id_orig # dateindex  # ticker  # street
  #                            # company_id # company
  # company_id_orig            
  # NOT PASSED TO UPSERT 
  # data passed IN from verify_company_basics          NOT PASSED OUT
  # IF "if(ref < 15184 )" THEN update (set company_id, dateindex_company_id) using (company_id, ticker , street)
  # OTHERWISE .. make NO CHANGE in data values
  # INHERITS from ABOVE  
  #
  ## update_from_future_new_company_ids ##
  #
  # dateindex_company_id_orig # dateindex  # ticker  # street
  #                           # company_id # company
  # company_id_orig   
  # PASSED TO UPSERT
  
  
  ### strait QUERY of the disk ### (non ci)
  ## verify_company_details ##
  # 
  # dateindex_company_id_orig # dateindex 
  # dateindex_company_id      # company_id
  # company_id_orig
  # PASSED TO UPSERT (non ci)
  
  
  ### no QUERY of anything ... strait math generation ###
  ## verify_return_dates ##
  #
  #                          # dateindex
  # 
  # PASSED TO UPSERT (generated dates only)
  
  
  ### strait QUERY of the database ###
  ## verify_week_often_week_returns ##      # ( REPLACE dateindex_company_id_orig by dateindex_company_id ) TO_DO [X]
  # dateindex_company_id      # dateindex 
  #                           # company_id
  # PASSED TO UPSERT
  
  
  ### strait QUERY of the database ###
  ## verify_month_often_month_past_returns ##
  # dateindex_company_id      # dateindex   # ( REPLACE dateindex_company_id_orig by dateindex_company_id ) TO_DO [X]
  #                           # company_id
  # PASSED TO UPSERT
  
  
  ### strait QUERY of the database ###
  ## load_inbnd_stmtstats ##
  #  dateindex_company_id*   # dateindex #  ( ADD dateindex_company_id ) TO_DO [x]
  #                          # company_id
  # PASSED TO UPSERT
  
  
  # **** IMPORTANT ( what UPDATE/UPSERT and what CONFLICT column is CHOSEN base on "what" column(z) are sent ) ****
  # **** heirarchy: "ticker", "dateindex_company_id_orig", "dateindex_company_id", "dateindex" ****
  
  Chosen_UpsertUpdate_Path_Done <- FALSE
  
  # ci, msdsc, exchg ( verify_company_basics -> update_from_future_new_company_ids )
  if(("ticker" %in% names(upsert_meta)) && 
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                       -> upsert_temp_perform_nothing
    TRUE                        -> upsert_temp_perform_upsert
    "dateindex_company_id_orig" -> conflict_column
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  # non_ci ( verify_company_details )
  if(("dateindex_company_id_orig" %in% names(upsert_meta)) && 
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                           -> upsert_temp_perform_nothing
    FALSE                           -> upsert_temp_perform_upsert 
    "dateindex_company_id_orig"     -> conflict_column
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  # db_query_only ( verify_week_often_week_returns, verify_month_often_month_past_returns, load_inbnd_stmtstats )
  if(( "dateindex_company_id"      %in% names(upsert_meta)) && 
     (!"dateindex_company_id_orig" %in% names(upsert_meta)) &&
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                      -> upsert_temp_perform_nothing
    FALSE                      -> upsert_temp_perform_upsert 
    "dateindex_company_id"     -> conflict_column 
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  # math_only ( verify_return_dates )
  if(( "dateindex"                 %in% names(upsert_meta)) && 
     (!"company_id"                %in% names(upsert_meta)) &&
     (!"dateindex_company_id     " %in% names(upsert_meta)) &&
     (Chosen_UpsertUpdate_Path_Done == FALSE)) {
    FALSE                      -> upsert_temp_perform_nothing
    FALSE                      -> upsert_temp_perform_upsert 
    "dateindex"                -> conflict_column 
    Chosen_UpsertUpdate_Path_Done <- TRUE
  }
  
  if(!Chosen_UpsertUpdate_Path_Done)       stop("Chosen_UpsertUpdate_Path_Done has not been completed.")
  if(!length(upsert_temp_perform_nothing)) stop("upsert_temp_perform_nothing is not determined") # LATER REMOVE
  if(!length(upsert_temp_perform_upsert))  stop("upsert_temp_perform_upsert is not determined")  # LATER REMOVE
  
  if((!upsert_temp_perform_nothing && upsert_temp_perform_upsert) || upsert_temp_perform_upsert_force) {
  
    
    # Error in sprintf("insert into \"si_finecon2_aggregates\"(" %s+% str_c(names(upsert_meta),  : 
    # 'fmt' length exceeds maximal format length 8192
    
    # actually perform the upsert
    str_trim(str_c(rstring(str_c('<%= 
    paste0(
    "insert into \\"', target_table_name, '\\"(" %s+% str_c(names(upsert_meta), collapse = ", ") %s+% ")" %s+% " \n" %s+% 
    "  select " %s+% str_c(names(upsert_meta), collapse = ", ") %s+% " \n" %s+% 
    "    from upsert_temp" %s+% " \n" %s+% 
    "      on conflict (" %s+% conflict_column %s+% ")" %s+% " \n" %s+%
    "        do update set (" %s+% str_c(names(upsert_meta), collapse = ", ") %s+% ") = (" %s+%  str_c("excluded.", names(upsert_meta), collapse = ", ") %s+% ");" 
    )
    %>'))))  %>% clean_text(.) -> fc_col_val_changes_sql 
    
    print("UPSERT")

  }
  
  
  # %s+% if( conflict_column == "dateindex" ) { " and dateindex = " %s+% dateindex %s+%  ";" }  else { ""  %s+% ";" }
  
  ### then do update-ish STILL based on company_id_orig  
  ### ( and do not UPDATE 
  ###   dateindex, company_id, dateindex_company_id, company_id_orig         # at least ONE COLUMN to try to update ( so the SQL DML UPDATE statement is valid ) # BELOW ARE 'non-updatable'
  ##if(!upsert_temp_perform_nothing && !upsert_temp_perform_upsert && sum(!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")) ) {
    
  if(!upsert_temp_perform_nothing && !upsert_temp_perform_upsert && !upsert_temp_perform_upsert_force) {
  
    # avoid sprintf 8192 character limit
    # # actually perform the update 
    # str_trim(str_c(rstring(str_c('<%= 
    # paste0(
    # "update \\"', target_table_name, '\\" s " %s+% " set " %s+% 
    #  str_c(sprintf("\n  %1$s = t.%1$s", names(upsert_meta)[!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")] ), collapse = ", ") %s+% " \n" %s+%
    # "    from upsert_temp t " %s+% " \n" %s+%
    # "      where " %s+% "s." %s+% conflict_column %s+% " = " %s+% "t." %s+% conflict_column  %s+% ";" 
    # )
    # %>'))))  %>% clean_text(.) -> fc_col_val_changes_sql
    
    col_names <- names(upsert_meta)[!names(upsert_meta) %in% c("dateindex", "company_id", "dateindex_company_id", "company_id_orig", "dateindex_company_id_orig")]
    # actually perform the update
    str_trim(str_c(rstring(str_c('<%= 
    paste0(
    "update \\"', target_table_name, '\\" s " %s+% " set " %s+% 
     str_c(str_c(str_c("\n ", col_names), str_c(" = t.", col_names)), collapse = ", ") %s+% " \n" %s+%
    "    from upsert_temp t " %s+% " \n" %s+%
    "      where " %s+% "s." %s+% conflict_column %s+% " = " %s+% "t." %s+% conflict_column  %s+% ";" 
    )
    %>'))))  %>% clean_text(.) -> fc_col_val_changes_sql
    
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
    
    # SHOULD BE PUT 'outside' OF THE FUNCTION
    if(target_table_name == "si_finecon2") {
      # if I have the columns for this index, then create the index ( if it does not already exist )
      verify_finecon_jamesos_partial_idx()
      verify_finecon_sp_500_partial_idx()
    }
  }
  
  drop_upsert_temp()
  
  options(ops)
  return(invisible(NULL))
   
}
# upsert2(value = SFS, target_table_name = "si_finecon2_aggregates", upsert_temp_perform_upsert_force = TRUE)




## (future) BUSINESS LOGIC ERROR HERE: (RETURNS too_early(after first loop)) SHOULD INSTEAD 'ADD TO A 'LIST OF DATA.FRAMES'
# 
verify_company_basics <- function (dateindex = NULL) {
  
  message(gsub("\"","",capture.output(match.call())))
  print(dateindex)
  
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
    # if(!db.q(str_c("select count(*) from information_schema.tables where table_name = 'si_finecon2';"), nrows = -1, conn.id = cid)) {
    if(!dbExistsTable(con, "si_finecon2")) {
      verify_si_finecon_exists()
    }
    
    # run once
    # getvar_all_load_days_lwd_var <- getvar_all_load_days_lwd()
    getvar_all_load_days_var <- getvar_all_load_days()
    
    # # some dateindexs in arg not found on disk
    # if(any(!dateindex %in% getvar_all_load_days_lwd_var)) { 
    #   dateindex[!dateindex %in% getvar_all_load_days_lwd_var] -> dateindexes_not_found_on_disk
    #   message("one/some arg dateindex not found on disk" %s+% str_c(dateindexes_not_found_on_disk, collapse = "") )
    # }
    
    # # some dateindexs in arg not found on disk
    if(any(!dateindex %in% getvar_all_load_days_var)) { 
      dateindex[!dateindex %in% getvar_all_load_days_var] -> dateindexes_not_found_on_disk
      message("one/some arg dateindex not found on disk: " %s+% str_c(dateindexes_not_found_on_disk, collapse = "") )
    }
    
    # getsetvar_aaii_sipro_dir() 
    # if(!any(dateindex %in% getvar_all_load_days_lwd_var)) stop("no arg dateindex was found on disk")
    if(!dateindex %in% getvar_all_load_days_var) stop("no arg dateindex was found on disk")
    
    # just the ones on found on disk    
    # spec date(not all dates) IN 'all disk possible aaii si_pro last weekday of the month'
    # dateindex_redux <- dateindex[dateindex %in% getvar_all_load_days_lwd_var]
    dateindex_redux <- dateindex[dateindex %in% getvar_all_load_days_var]
    
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
        
      #    lcase_a_remove_useless_columns(si_si_tbl_df)       -> si_si_tbl_df
        if(si_tbl_i == "si_ci") {
           lcase_a_remove_useless_columns(si_si_tbl_df, "ci") -> si_si_tbl_df
        } else { # "si_exchg","si_mgdsc"
          # all lower ( PostgreSQL friendly )
          colnames(si_si_tbl_df) <- tolower(colnames(si_si_tbl_df))
        }
        
        # unique ids
        
        if(si_tbl_i == "si_ci") {

          within( si_si_tbl_df, { assign("dateindexeom", rep(last_day_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexeom")    -> si_si_tbl_df
          
          within( si_si_tbl_df, { assign("dateindexlwd", rep(     lwd_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexlwd") -> si_si_tbl_df
          
          within( si_si_tbl_df, { assign("dateindexlbd", rep(     lbd_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexlbd") -> si_si_tbl_df

          within( si_si_tbl_df, { assign("dateindexmonth", rep(     mnth_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexmonth") -> si_si_tbl_df

          within( si_si_tbl_df, { assign("dateindexmonthsincebirth", rep(     mnth_since_birth(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexmonthsincebirth") -> si_si_tbl_df
          
          within( si_si_tbl_df, { assign("dateindexyearmonth", rep(     yrmnth_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexyearmonth") -> si_si_tbl_df

          within( si_si_tbl_df, { assign("dateindexyear", rep(     yr_of_month(dateindex_redux_i),NROW(si_si_tbl_df[,1])) )  } ) -> si_si_tbl_df
          DataCombine::MoveFront(si_si_tbl_df,   "dateindexyear") -> si_si_tbl_df

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

      
      # financize(si_all_df) -> si_all_df
      # inconsistent when/where logic SO I AM deciding NOT to 'repalce with NA' here
      financize(si_all_df, char_col_numeric_limit = 99999999999999.99) -> si_all_df
      
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
  
  message(gsub("\"","",capture.output(match.call())))
  print(ref)
  
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
    
    # str( db.q(str_c("select * from trg"), nrows = -1, conn.id = cid) )
    
    
    # the_past(whatever) 
    # past data looking forward to grab company_ids, tickers, and company s
    # expect 2 SQL updates per loop iteration
    
    # FIRST(+future+) date that HAS the NEW(earliest of latest) company_id system
    for(month_i in 15184) {
    
    # will downsize into integers ( 1 months ahead is sufficient to find a lost link )
    # ?? loop iterations ( CURRENLY JUST ONE iteration)
    # for(month_i in zoo::as.Date(ref) %m+% months(1:1)) {
      
      lwd_of_month(month_i) -> lwd
      
                                                                       # OLD # lcase_a_remove_useless_columns(.) %>%
      
      print(str_c("Looking in direction at ... ", zoo::as.Date(lwd)," ",lwd," Maybe in "))
      if(file.exists(str_c("W:/AAIISIProDBFs/",lwd))) { print(str_c("  Exists: ","W:/AAIISIProDBFs/",lwd)) }
      
      suppressWarnings(suppressMessages(foreign::read.dbf(file = str_c("W:/AAIISIProDBFs/",lwd,"/si_ci.dbf")
                                                          , as.is = TRUE))) %>% lcase_a_remove_useless_columns(.,"ci") %>%
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
      
      ##########################################################################
      ## BEGIN (ORIGINAL) LONG (DEVELOPMENT) TIME RUNING TRG/SRC UPDATED QUERY #
      
      # db.q(str_c("
      #            update trg
      #              set company_id =                     src.company_id,
      #        dateindex_company_id = dateindex || '_' || src.company_id  
      #            from src
      #              where trg.company_id != src.company_id and
      #                    trg.ticker      = src.ticker and
      #                    trg.street      = src.street
      # "), nrows =  -1, conn.id = cid)    # SRC.STREET # a little extra safety, hp & hpq
      
      ## END ( ORIGINAL ) LONG (DEVELOPMENT) TIME RUNNING TRG/SRC UPDATED QUERY #
      ###########################################################################
      
      ## BEGIN BIG SET OF THREE ( NO FUZZY ) ##
      
      # db.q(str_c("
      #            update trg
      #              set company_id =                     src.company_id,
      #        dateindex_company_id = dateindex || '_' || src.company_id  
      #            from src
      #              where src.company_id != trg.company_id and
      #                    src.ticker      = trg.ticker and
      #                    src.street      = trg.street
      # "), nrows =  -1, conn.id = cid)    # SRC.STREET # a little extra safety, hp & hpq
      # 
      # db.q(str_c("
      #            update trg
      #              set company_id =                     src.company_id,
      #        dateindex_company_id = dateindex || '_' || src.company_id  
      #            from src
      #              where src.company_id != trg.company_id and
      #                    src.ticker      = trg.ticker and
      #                    src.company     = trg.company
      # "), nrows =  -1, conn.id = cid)    
      # 
      # db.q(str_c("
      #            update trg
      #              set company_id =                     src.company_id,
      #        dateindex_company_id = dateindex || '_' || src.company_id  
      #            from src
      #              where src.company_id != trg.company_id and
      #                    src.street      = trg.street and
      #                    src.company     = trg.company
      # "), nrows =  -1, conn.id = cid)   
      
      ## END BIG SET OF FUNCTIONS ( NO FUZZY ) ##
      
      ## BEGIN BIG SET OF THREE   ( YES FUZZY ) ##
      
     db.q(str_c("
        create or replace function sif_agrep(pattern text, x text)
        returns boolean as $$
          if(is.null(pattern) || is.na(pattern) || !length(pattern)  ) return(F)
          if(is.null(x)       || is.na(x)       || !length(x)        ) return(F)
          ret <- tryCatch({ agrepl(pattern = pattern, x = x, ignore.case = TRUE) }, error = function(e) { print(e); print(pattern); print(x); stop() })
          return(ret)
        $$ language plr;
      "), nrows =  -1, conn.id = cid) 
      
     str_c("
                 update trg
                   set company_id =                     src.company_id,
             dateindex_company_id = dateindex || '_' || src.company_id  
                 from src
                   where src.company_id != trg.company_id and
                         src.ticker      = trg.ticker and
                         sif_agrep(src.street, trg.street) 
      ") -> changes_sql
      writeLines(changes_sql)
      rs <- dbSendQuery(con, changes_sql)
      if(dbHasCompleted(rs)) { print(str_c("Rows Affected: ", dbGetRowsAffected(rs)))  }
      if(dbHasCompleted(rs)) dbClearResult(rs)

      str_c("
                 update trg
                   set company_id =                     src.company_id,
             dateindex_company_id = dateindex || '_' || src.company_id  
                 from src
                   where src.company_id != trg.company_id and
                         src.ticker      = trg.ticker and
                         sif_agrep(src.company, trg.company)
      ") -> changes_sql
      writeLines(changes_sql)
      rs <- dbSendQuery(con, changes_sql)
      if(dbHasCompleted(rs)) { print(str_c("Rows Affected: ", dbGetRowsAffected(rs)))  }
      if(dbHasCompleted(rs)) dbClearResult(rs)
      
      # -- skyskrapers
      # -- "45 Fremont Street"
      # select street 
      # from si_finecon2
      # where dateindex = 14911
      # group by street
      # having count(*) > 1
      # order by 1
      # -- 179
      # 
      # -- "iShares Dow Jones US Consumer"  ( OLD DATA: 30 character chop-off name problem )
      # -- "iShares Dow Jones US Financial" ( NOTE: EARLY DATA COMPANY_ID and TICKER dups ARE removed MAYBE also COMPANY exactly SHOULD be REMOVED
      # select company 
      # from si_finecon2
      # where dateindex = 14911
      # group by company
      # having count(*) > 1
      # order by 1
      
      # NOT BOLD ENOUGH TO TRY THIS
      
      # str_c("
      #            update trg
      #              set company_id =                     src.company_id,
      #        dateindex_company_id = dateindex || '_' || src.company_id  
      #            from src
      #              where src.company_id != trg.company_id and
      #                    sif_agrep(src.street, trg.street)   and
      #                    sif_agrep(src.company, trg.company) and 
      #                    src.street not in ( select street from src group by street having count(*) > 1 ) and
      #                    trg.street not in ( select street from trg group by street having count(*) > 1 ) and
      #                    src.company not in ( select company from src group by company having count(*) > 1 ) and
      #                    trg.company not in ( select company from trg group by company having count(*) > 1 ) 
      # ")  -> changes_sql
      # writeLines(changes_sql)
      # rs <- dbSendQuery(con, changes_sql)
      # if(dbHasCompleted(rs)) { print(str_c("Rows Affected: ", dbGetRowsAffected(rs)))  }
      # if(dbHasCompleted(rs)) dbClearResult(rs)
      ## END BIG SET OF FUNCTIONS ( YES FUZZY ) ##
      
      
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
  
  message(gsub("\"","",capture.output(match.call())))
  print(dateindex)
  
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
    # getvar_all_load_days_lwd_var <- getvar_all_load_days_lwd()
    getvar_all_load_days_var <- getvar_all_load_days()
    
    # # some dateindexs in arg not found on disk
    # if(any(!dateindex %in% getvar_all_load_days_lwd_var)) { 
    #   dateindex[!dateindex %in% getvar_all_load_days_lwd_var] -> dateindexes_not_found_on_disk
    #   message("one/some arg dateindex not found on disk" %s+% str_c(dateindexes_not_found_on_disk, collapse = "") )
    # }
    
    # # some dateindexs in arg not found on disk
    if(any(!dateindex %in% getvar_all_load_days_var)) { 
      dateindex[!dateindex %in% getvar_all_load_days_var] -> dateindexes_not_found_on_disk
      message("one/some arg dateindex not found on disk: " %s+% str_c(dateindexes_not_found_on_disk, collapse = "") )
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
    
    # lcase_a_remove_useless_columns(si_si_tbl_df) -> si_si_tbl_df
    lcase_a_remove_useless_columns(si_si_tbl_df,  str_replace(si_tbl_i,"si_","")) -> si_si_tbl_df
    
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
    # financize(si_all_df) -> si_all_df
    # inconsistent when/where logic SO I AM deciding NOT to 'repalce with NA' here
    financize(si_all_df, char_col_numeric_limit = 99999999999999.99) -> si_all_df
    
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



# CAN BE generalized
# uses verify_connection
updated_from_future_upd_netinc_q1 <- function (dir_i = NULL) {
  
  # version  R version 3.5.0 Patched (2018-04-28 r74673) # devtools::session_info()
  
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
  
  # verify_si_finecon_exists()
  verify_connection()
  
  # NOTE: last_inbnd_stmtid_dateindex not 'fully' correct , not fully reliable, can (and has been) sometimes incorrect
  #       qs_date is better

  db.q("alter table si_finecon2 add if not exists orig_netinc_q1        numeric(8,2);", conn.id = cid)
  db.q("alter table si_finecon2 add if not exists updated_netinc_q1_src text;",         conn.id = cid)

  dml <- paste0("
  -- run ( ZERO-ISH )
  -- update once ever
  update fe_data_store.si_finecon2 f
    set  
      orig_netinc_q1 = f.netinc_q1
    where 
      f.orig_netinc_q1 is null and f.dateindex = ", dir_i, "; -- 17562
  ")
  message(dml)
  dbExecute(con, dml)
  
  # not-null netinc_q1
  db.q("drop table if exists future_temp;", conn.id = cid)
  ddl <- paste0("
  
    create temporary table future_temp as 
    --
    with company_ids as (
    
      select distinct now.company_id
      from fe_data_store.si_finecon2 now where now.dateindex = ", dir_i, " -- 17562 
      -- and now.sp in ('500','400','600')
      order by now.company_id
    ), 
    dateindexes_fut1 as (
    
      select fut.dateindex from (select distinct dateindex 
      from fe_data_store.si_finecon2 
      where dateindex > ", dir_i, "                       -- 17562 -- monthly production load: max(dateindex) xor 'current index'
      order by dateindex      limit 1 offset 0) fut      
    
    ), -- 17590
    dateindexes_fut2 as (
    
      select fut.dateindex from (select distinct dateindex 
      from fe_data_store.si_finecon2 
      where dateindex > ", dir_i, "                       -- 17562 -- monthly production load: max(dateindex) xor 'current index'
      order by dateindex      limit 1 offset 1) fut      
    
    ) -- 17619
    select now.*, 
      -- fut1.dateindex                fut1_dateindex,
      fut1.qs_date                     fut1_qs_date,
      fut1.perend_q1                   fut1_perend_q1,
      fut1.now_inbnd_stmtid_dateindex  fut1_now_inbnd_stmtid_dateindex,
      fut1.last_inbnd_stmtid_dateindex fut1_last_inbnd_stmtid_dateindex,
      fut1.netinc_q1                   fut1_netinc_q1,
    
      -- fut2.dateindex                fut2_dateindex,
      fut2.qs_date                     fut2_qs_date,
      fut2.perend_q1                   fut2_perend_q1,
      fut2.now_inbnd_stmtid_dateindex  fut2_now_inbnd_stmtid_dateindex,
      fut2.last_inbnd_stmtid_dateindex fut2_last_inbnd_stmtid_dateindex,
      fut2.netinc_q1                   fut2_netinc_q1
    
    from ( 
    
      select curr.dateindex, 
             dateindexes_fut1.dateindex fut1_dateindex,  -- here --
             dateindexes_fut2.dateindex fut2_dateindex,  -- here --
             curr.company_id, curr.sp, curr.ticker, curr.company,
             curr.perend_q1,
             curr.qs_date,
             curr.now_inbnd_stmtid_dateindex, 
             curr.last_inbnd_stmtid_dateindex,
             curr.netinc_q1
      from fe_data_store.si_finecon2 curr, 
           dateindexes_fut1, dateindexes_fut2  
      where curr.dateindex = ", dir_i, " -- 17562        
        and curr.company_id in ( select company_ids.company_id from company_ids )
        -- and curr.netinc_q1 is null  -- -- CRITERIA TO FIND BAD/MISSING DATUMS -- --
                             
    ) now         left join lateral ( -- 17590
    
      -- per item processing: may not be the fastest
      select curr.dateindex, curr.company_id, 
             curr.perend_q1,
             curr.qs_date,
             curr.now_inbnd_stmtid_dateindex, 
             curr.last_inbnd_stmtid_dateindex,
             curr.netinc_q1
      from fe_data_store.si_finecon2 curr
      where curr.dateindex = now.fut1_dateindex and curr.company_id = now.company_id
                             -- here --
    
    ) fut1 on true left join lateral ( -- 17619
    
      -- per item processing: may not be the fastest
      select curr.dateindex, curr.company_id, 
             curr.perend_q1,
             curr.qs_date,
             curr.now_inbnd_stmtid_dateindex, 
             curr.last_inbnd_stmtid_dateindex,
             curr.netinc_q1
      from fe_data_store.si_finecon2 curr
      where curr.dateindex = now.fut2_dateindex and curr.company_id = now.company_id
                             -- here --
    
    ) fut2 on true;
    
  ")
  message(ddl)
  dbExecute(con, ddl)
  
  try( { db.q("create unique index if not exists future_temp_dateindex_company_id_idx on future_temp(dateindex, company_id);", conn.id = cid) }, silent = TRUE )

  dml <- paste0("
  -- run ( FIRST )
  -- replacing a bad value
  -- look into the future (fut1 and fut2) and vote (anonymously) to replace the earlier bad value
  update fe_data_store.si_finecon2 f
    set  
      updated_netinc_q1_src = 'fut1_netinc_q1 and fut1_netinc_q2',    
              netinc_q1     =  t.fut1_netinc_q1
    from
      future_temp t
    where t.dateindex = f.dateindex and t.company_id = f.company_id 
      and t.qs_date = t.fut1_qs_date and t.qs_date = t.fut2_qs_date
      and t.netinc_q1 is not null and t.fut1_netinc_q1 is not null and t.fut2_netinc_q1 is not null and t.netinc_q1 != t.fut1_netinc_q1 and t.fut1_netinc_q1 = t.fut2_netinc_q1;
  ")
  message(dml)
  dbExecute(con, dml)
  
  db.q("drop table if exists future_temp;", conn.id = cid)
  
  ##
  
  # null netinc_q1
  db.q("drop table if exists future_temp;", conn.id = cid)
  ddl <- paste0("
  
    create temporary table future_temp as 
    --
    with company_ids as (
    
      select distinct now.company_id
      from fe_data_store.si_finecon2 now where now.dateindex = ", dir_i, " -- 17562 
      -- and now.sp in ('500','400','600')
      order by now.company_id
    ), 
    dateindexes_fut1 as (
    
      select fut.dateindex from (select distinct dateindex 
      from fe_data_store.si_finecon2 
      where dateindex > ", dir_i, "                       -- 17562 -- monthly production load: max(dateindex) xor 'current index'
      order by dateindex      limit 1 offset 0) fut      
    
    ), -- 17590
    dateindexes_fut2 as (
    
      select fut.dateindex from (select distinct dateindex 
      from fe_data_store.si_finecon2 
      where dateindex > ", dir_i, "                       -- 17562 -- monthly production load: max(dateindex) xor 'current index'
      order by dateindex      limit 1 offset 1) fut      
    
    ) -- 17619
    select now.*, 
      -- fut1.dateindex                fut1_dateindex,
      fut1.qs_date                     fut1_qs_date,
      fut1.perend_q1                   fut1_perend_q1,
      fut1.now_inbnd_stmtid_dateindex  fut1_now_inbnd_stmtid_dateindex,
      fut1.last_inbnd_stmtid_dateindex fut1_last_inbnd_stmtid_dateindex,
      fut1.netinc_q1                   fut1_netinc_q1,
    
      -- fut2.dateindex                fut2_dateindex,
      fut2.qs_date                     fut2_qs_date,
      fut2.perend_q1                   fut2_perend_q1,
      fut2.now_inbnd_stmtid_dateindex  fut2_now_inbnd_stmtid_dateindex,
      fut2.last_inbnd_stmtid_dateindex fut2_last_inbnd_stmtid_dateindex,
      fut2.netinc_q1                   fut2_netinc_q1
    
    from ( 
    
      select curr.dateindex, 
             dateindexes_fut1.dateindex fut1_dateindex,  -- here --
             dateindexes_fut2.dateindex fut2_dateindex,  -- here --
             curr.company_id, curr.sp, curr.ticker, curr.company,
             curr.perend_q1,
             curr.qs_date,
             curr.now_inbnd_stmtid_dateindex, 
             curr.last_inbnd_stmtid_dateindex,
             curr.netinc_q1
      from fe_data_store.si_finecon2 curr, 
           dateindexes_fut1, dateindexes_fut2  
      where curr.dateindex = ", dir_i, " -- 17562        
        and curr.company_id in ( select company_ids.company_id from company_ids )
        and curr.netinc_q1 is null  -- -- CRITERIA TO FIND BAD/MISSING DATUMS -- --
                             
    ) now         left join lateral ( -- 17590
    
      -- per item processing: may not be the fastest
      select curr.dateindex, curr.company_id, 
             curr.perend_q1,
             curr.qs_date,
             curr.now_inbnd_stmtid_dateindex, 
             curr.last_inbnd_stmtid_dateindex,
             curr.netinc_q1
      from fe_data_store.si_finecon2 curr
      where curr.dateindex = now.fut1_dateindex and curr.company_id = now.company_id
                             -- here --
    
    ) fut1 on true left join lateral ( -- 17619
    
      -- per item processing: may not be the fastest
      select curr.dateindex, curr.company_id, 
             curr.perend_q1,
             curr.qs_date,
             curr.now_inbnd_stmtid_dateindex, 
             curr.last_inbnd_stmtid_dateindex,
             curr.netinc_q1
      from fe_data_store.si_finecon2 curr
      where curr.dateindex = now.fut2_dateindex and curr.company_id = now.company_id
                             -- here --
    
    ) fut2 on true;
    
  ")
  message(ddl)
  dbExecute(con, ddl)
  
  try( { db.q("create unique index if not exists future_temp_dateindex_company_id_idx on future_temp(dateindex, company_id);", conn.id = cid) }, silent = TRUE )
  
  dml <- paste0("
  -- then run ( must be SECOND )
  -- replacing a null value -- look forward into fut1_netinc_q1 
  update fe_data_store.si_finecon2 f
    set  
      updated_netinc_q1_src =  'fut1_netinc_q1',
              netinc_q1     = t.fut1_netinc_q1
    from
      future_temp t
    where t.dateindex = f.dateindex and t.company_id = f.company_id;
      and t.qs_date = t.fut1_qs_date
      and t.netinc_q1 is null and t.fut1_netinc_q1 is not null; 
  ")
  message(dml)
  dbExecute(con, dml)
  
  dml <- paste0("
  -- then run ( must be THIRD )
  -- replacing a null value -- look forward into fut1_netinc_q1(must be null) and then look into fut2_netinc_q1
  update fe_data_store.si_finecon2 f
    set  
      updated_netinc_q1_src =  'fut2_netinc_q1',
              netinc_q1     = t.fut2_netinc_q1
      from
        future_temp t
      where t.dateindex = f.dateindex and t.company_id = f.company_id
        and t.qs_date = t.fut1_qs_date and t.qs_date = t.fut2_qs_date
        and t.netinc_q1 is null and t.fut1_netinc_q1 is null and t.fut2_netinc_q1 is not null;
  ")
  message(dml)
  dbExecute(con, dml)
  
  db.q("drop table if exists future_temp;", conn.id = cid)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
}
# ONCE ONLY
# 
# updated_from_future_upd_netinc_q1(dir_i = 17562)
# 
# MASS(MANUAL RUN) (goes backwards in time)
#
# for(dir_i sort(as.integer(dir("W:/AAIISIProDBFs")), decreasing = TRUE)) {
#   
#   Sys.sleep(2)
#   message(paste0("**** Beginning disk dbf dir: ", dir_i," ", dir_i," ****"))
#   
#   updated_from_future_upd_netinc_q1(dir_i = dir_i)
#   
#   message(paste0("**** Ending disk dbf dir: ", dir_i," ", dir_i," ****"))
#   Sys.sleep(2)
#   
# }
# 
# 
# MASS(SEMI-AUTO MONTH-END RUN) ( expected to go backwards in time )
# 
# 
# # part of "upload_lwd_sipro_dbfs_to_db"
# 
# verify_company_details(dateindex = c(dir_i),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
# print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
# NEW #
# update_from_future_upd_netinc_q1(dateindex = c(dir_i))
# 




verify_return_dates <- function(dateindex = NULL, months_limit = NULL, within_back = 5) {
  
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
  
  verify_return_dates_inner <- function (dateindex = NULL, months_limit = NULL, within_back = NULL) {
    
    verify_si_finecon_exists()
    
    require(magrittr)
    require(stringi)
    require(stringr)
    require(lubridate)
    # uses zoo::as.Date.integer, zoo::as.Date.yearmon, zoo::as.yearmon
    # uses Hmisc::truncPOSIXt
    #   minor README correction spelling - trunPOSIXt to truncPOSIXt ( with 'c' ) #86
    #   https://github.com/harrelfe/Hmisc/issues/86
    #   Hmisc::trunc.POSIXt -> Hmisc::truncPOSIXt
    
    
    if(is.null(months_limit)) stop("must supply a number of months: e.g. months_limit = 38")

    # create the data.frame
    
    1:months_limit -> both_months_range 

        both_months_range ->  future_months_range
                    # not used
                    # length(future_months_range) -> len_direction_months_range
                       length(future_months_range) -> len_future_months_range
    
    rev(both_months_range) -> past_months_range 
                       length(past_months_range)   -> len_past_months_range
    
    c(rev(future_months_range), future_months_range) ->    past_and_future_months_range
    length(past_and_future_months_range)             -> len_past_and_future_months_range

    # rep(c('p','f'), each = len_past_and_future_months_range) %>%
    #   # TWO_m per month
    #   str_c(., rep(past_and_future_months_range, each= 2) %>% str_pad(.,2,'left','0') ) %>%
    #     # suffix 'eom','lwd'
    #     str_c( .,rep(c('lwd','eom'), times = len_past_and_future_months_range)) %>%
    #       # prefix 'dateindex'
    #       str_c('dateindex', .) -> column_names
    
    # past
    
    str_c(                        # c("year", "yearmonth", "month", "lbd","lwd","eom")
      rep("dateindex" %s+% "p",   each = len_past_months_range * 6), 
                               # c("year", "yearmonth", "month", "lbd","lwd","eom")
      rep(  past_months_range, each = 6) %>% str_pad(.,2,'left','0'), 
      rep(c("year", "yearmonth", "month", "lbd","lwd","eom"), times = len_past_months_range) 
    ) -> column_names_p
    
    # future
    
    str_c(                        # c("year", "yearmonth", "month", "lbd","lwd","eom")
      rep("dateindex" %s+% "f",   each = len_future_months_range * 6), 
                                 # c("year", "yearmonth", "month", "lbd","lwd","eom")
      rep(  future_months_range, each = 6) %>% str_pad(.,2,'left','0'), 
      rep(c("year", "yearmonth", "month", "lbd","lwd","eom"), times = len_future_months_range) 
    ) -> column_names_f
    
    column_names <- c(column_names_p, column_names_f)
    c("dateindex", "dateindexyear", "dateindexyearmonth", "dateindexmonth", "dateindexlbd", "dateindexlwd", "dateindexeom", column_names) -> all_col_names 
    
    # eval(parse(text=str_c("data.frame(dateindex=integer(), dateindexlwd=integer(), dateindexeom=integer()," %s+% str_c(column_names,"=integer()",collapse = ", ") %s+% ")"))) -> si_all_df
    
    # fill the data.frame
    
    dateindex -> now_date
    zoo::as.Date(now_date) -> now_date

    # this month
    (now_date - within_back) %m+% months(1) %>%
      # 1st day of next month # last day of this month
      Hmisc::truncPOSIXt(., units='months') %m+% days(-1) %>% 
        zoo::as.Date(.)  %>%
          # add in lwd ( Sat or Sun falls back to Fri)
          lapply(.,function(x) {  
                                  yr_of_month(x)                                                -> yr
                                  yrmnth_of_month(x)                                            -> yrmnth
                                  mnth_of_month(x)                                              -> mnth
                                  lbd_of_month(x)                                               -> lbd
                                  lwd_of_month(x)                                               -> lwd
                                  # yr, yrmnth, mnth, lbd, lwd, eom
                                  c(yr, yrmnth, mnth, lbd, lwd , x)
                               } ) %>% 
            # flattened (Date class is stripped)
            unlist(.) %>% zoo::as.Date(.) -> now_calc_dates

    # past lwd eom dates

    # previous Nth(months_limit) month back
    (now_date - within_back) %m+% months(-months_limit) %>%
      # 1st day of previous Nth month
      Hmisc::truncPOSIXt(., units='months')  %>%
        # N(months_limit) or so months
        seq(., by = 'month', length.out = months_limit) %m+% 
          # last day of month
          months(1) %m+% days(-1) %>% 
            zoo::as.Date(.) %>% 
              # add in lwd ( Sat or Sun falls back to Fri)
              lapply(.,function(x) { 
                                    yr_of_month(x)                                                -> yr
                                    yrmnth_of_month(x)                                            -> yrmnth
                                    mnth_of_month(x)                                              -> mnth
                                    lbd_of_month(x)                                               -> lbd
                                    lwd_of_month(x)                                               -> lwd
                                    # yr, yrmnth, mnth, lbd, lwd, eom
                                    c(yr, yrmnth, mnth, lbd, lwd , x)
                                   } ) %>% 
                # flattened (Date class is stripped)
                unlist(.) %>% zoo::as.Date(.) -> past_calc_dates

    # future lwd eom dates

    # next month
    (now_date - within_back) %m+% months(1) %>%
      # 1st day of next month
      Hmisc::truncPOSIXt(., units='months') %>%
        # N(months_limit) or so months
        seq(., by = 'month', length.out = months_limit) %m+% 
          # last day of month
          months(1) %m+% days(-1) %>% 
            zoo::as.Date(.) %>% 
              # add in lwd ( Sat or Sun falls back to Fri)
              lapply(.,function(x) { 
                                    DescTools::Year(x)                                            -> yr
                                    DescTools::YearMonth(x)                                       -> yrmnth
                                    DescTools::Month(x)                                           -> mnth
                                    RQuantLib::getEndOfMonth("UnitedStates/NYSE", x)              -> lbd
                                    (x - match(weekdays(x), c('Saturday','Sunday'), nomatch = 0)) -> lwd
                                    # yr, yrmnth, mnth, lbd, lwd, eom
                                    c(yr, yrmnth, mnth, lbd, lwd , x)
                                   } ) %>% 
                # flattened (Date class is stripped)
                unlist(.) %>% zoo::as.Date(.) -> future_calc_dates


    c(now_date,now_calc_dates,past_calc_dates,future_calc_dates) %>% as.integer(.) -> all_dates

    # actual fill 
    
    eval(parse(text=str_c("data.frame(" %s+% str_c(all_col_names,"=",all_dates, "L" , collapse = ", ") %s+% ")"))) -> si_all_df
    
    return(si_all_df) 

  }
  ret <- verify_return_dates_inner(dateindex = dateindex, months_limit = months_limit, within_back = within_back)
                                      
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

  message(gsub("\"","",capture.output(match.call())))
  print(dateindex)
  
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
    #   dateindexf##eom, price, prchg_##w, perend_q#, dps_q# 
    # 
    c("dateindexf01eom","price","prchg_04w","perend_q1","dps_q1") -> sample_columns
    
    if(any(!sample_columns %in% names(fc_meta))) { 
      message("Minumum columns are missing.") 
      message("Sample Missing columns are the following:"  %s+% ' ' %s+% str_c(sample_columns[!sample_columns %in% names(fc_meta)], collapse = " "))
      browser()
    } 
    
    "
    select 
    --
    fe.dateindex,
    fe.dateindex_company_id,
    fe.company_id,
    --
    fe_04w_o.pricebck_04w,
    fe_04w_o.prchg_f04w_ann,
    fe_04w_o.pct_f_div_ret_ov_pr_04w_q1_ann,
    fe_04w_o.pradchg_f04w_ann,
    fe_04w_o.price_f04w,
    --
    fe_13w_o.pricebck_13w,
    fe_13w_o.prchg_f13w_ann,
    fe_13w_o.pct_f_div_ret_ov_pr_13w_q1_ann,
    fe_13w_o.pradchg_f13w_ann,
    fe_13w_o.price_f13w,
    --
    fe_26w_o.pricebck_26w,
    fe_26w_o.prchg_f26w_ann,
    fe_26w_o.pct_f_div_ret_ov_pr_26w_q12_ann,
    fe_26w_o.pradchg_f26w_ann,
    fe_26w_o.price_f26w,
    --
    fe_52w_o.pricebck_52w,
    fe_52w_o.prchg_f52w_ann,
    fe_52w_o.pct_f_div_ret_ov_pr_52w_q1234_ann,
    fe_52w_o.pradchg_f52w_ann,
    fe_52w_o.price_f52w
    from 
      si_finecon2 fe 
    left join lateral ( 
        select 
          fe_04w.dateindex, fe_04w.dateindexeom, fe_04w.company_id,
            nullif(fe_04w.price,0)/(nullif(fe_04w.prchg_04w,-100)/100 + 1)  pricebck_04w,
            fe_04w.prchg_04w * 12                                           prchg_f04w_ann,
                                    30.5 / nullif( fe_04w.perend_q1 - fe_04w.perend_q2, 0) * ( coalesce(fe_04w.dps_q1,0)    )/(nullif(fe_04w.price,0)/(nullif(fe_04w.prchg_04w,-100)/100 + 1)) * 100 * 12 pct_f_div_ret_ov_pr_04w_q1_ann,
            fe_04w.prchg_04w * 12 + 30.5 / nullif( fe_04w.perend_q1 - fe_04w.perend_q2, 0) * ( coalesce(fe_04w.dps_q1,0)    )/(nullif(fe_04w.price,0)/(nullif(fe_04w.prchg_04w,-100)/100 + 1)) * 100 * 12 pradchg_f04w_ann,
            fe_04w.price price_f04w
          from 
              si_finecon2 fe_04w
    ) fe_04w_o on fe.dateindexf01eom  = fe_04w_o.dateindexeom and fe.company_id = fe_04w_o.company_id                              
    left join lateral ( 
        select 
          fe_13w.dateindex, fe_13w.dateindexeom, fe_13w.company_id,
            nullif(fe_13w.price,0)/(nullif(fe_13w.prchg_13w,-100)/100 + 1)  pricebck_13w,
            fe_13w.prchg_13w * 4                                            prchg_f13w_ann,
                                    91.0 / nullif( fe_13w.perend_q1 - fe_13w.perend_q2, 0) * ( coalesce(fe_13w.dps_q1,0)    )/(nullif(fe_13w.price,0)/(nullif(fe_13w.prchg_13w,-100)/100 + 1)) * 100 *  4 pct_f_div_ret_ov_pr_13w_q1_ann,
            fe_13w.prchg_13w * 4 +  91.0 / nullif( fe_13w.perend_q1 - fe_13w.perend_q2, 0) * ( coalesce(fe_13w.dps_q1,0)    )/(nullif(fe_13w.price,0)/(nullif(fe_13w.prchg_13w,-100)/100 + 1)) * 100 *  4 pradchg_f13w_ann,
            fe_13w.price price_f13w
          from 
              si_finecon2 fe_13w
    ) fe_13w_o on fe.dateindexf03eom  = fe_13w_o.dateindexeom and fe.company_id = fe_13w_o.company_id 
    left join lateral ( 
        select 
          fe_26w.dateindex, fe_26w.dateindexeom, fe_26w.company_id,
            nullif(fe_26w.price,0)/(nullif(fe_26w.prchg_26w,-100)/100 + 1)  pricebck_26w,
            fe_26w.prchg_26w * 2                                            prchg_f26w_ann,
                                    182.0 / nullif( fe_26w.perend_q1 - fe_26w.perend_q3, 0) * ( coalesce(fe_26w.dps_q1,0) + 
                                                                                                coalesce(fe_26w.dps_q2,0)    )/(nullif(fe_26w.price,0)/(nullif(fe_26w.prchg_26w,-100)/100 + 1)) * 100 *  2 pct_f_div_ret_ov_pr_26w_q12_ann,
            fe_26w.prchg_26w * 2 +  182.0 / nullif( fe_26w.perend_q1 - fe_26w.perend_q3, 0) * ( coalesce(fe_26w.dps_q1,0) + 
                                                                                                coalesce(fe_26w.dps_q2,0)    )/(nullif(fe_26w.price,0)/(nullif(fe_26w.prchg_26w,-100)/100 + 1)) * 100 *  2 pradchg_f26w_ann,
            fe_26w.price price_f26w
          from 
              si_finecon2 fe_26w
    ) fe_26w_o on fe.dateindexf06eom  = fe_26w_o.dateindexeom and fe.company_id = fe_26w_o.company_id 
    left join lateral ( 
        select 
          fe_52w.dateindex, fe_52w.dateindexeom, fe_52w.company_id,
            nullif(fe_52w.price,0)/(nullif(fe_52w.prchg_52w,-100)/100 + 1)  pricebck_52w,
            fe_52w.prchg_52w * 1                                            prchg_f52w_ann,
                                    365.0 / nullif( fe_52w.perend_q1 - fe_52w.perend_q5, 0) * ( coalesce(fe_52w.dps_q1,0) + 
                                                                                                coalesce(fe_52w.dps_q2,0) + 
                                                                                                coalesce(fe_52w.dps_q3,0) + 
                                                                                                coalesce(fe_52w.dps_q4,0)    )/(nullif(fe_52w.price,0)/(nullif(fe_52w.prchg_52w,-100)/100 + 1)) * 100 *  1 pct_f_div_ret_ov_pr_52w_q1234_ann,
            fe_52w.prchg_52w * 1 +  365.0 / nullif( fe_52w.perend_q1 - fe_52w.perend_q5, 0) * ( coalesce(fe_52w.dps_q1,0) + 
                                                                                                coalesce(fe_52w.dps_q2,0) + 
                                                                                                coalesce(fe_52w.dps_q3,0) + 
                                                                                                coalesce(fe_52w.dps_q4,0)    )/(nullif(fe_52w.price,0)/(nullif(fe_52w.prchg_52w,-100)/100 + 1)) * 100 *  1 pradchg_f52w_ann,
            fe_52w.price price_f52w
          from 
              si_finecon2 fe_52w
    ) fe_52w_o on fe.dateindexf12eom  = fe_52w_o.dateindexeom and fe.company_id = fe_52w_o.company_id 
    where fe.dateindex = " %s+% dateindex %s+% ";" -> add_columns_sql

    db.q(add_columns_sql, nrows = -1, conn.id = cid) -> si_all_df

    # KEEP
    # financize(si_all_df) -> si_all_df
    # inconsistent when/where logic SO I AM deciding NOT to 'repalce with NA' here
    financize(si_all_df, char_col_numeric_limit = 99999999999999.99) -> si_all_df
    
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
      message("Minumum columns are missing.") 
      message("Sample Missing columns are the following:"  %s+% ' ' %s+% str_c(sample_columns[!sample_columns %in% names(fc_meta)], collapse = " "))
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
    select fe.dateindex, fe.dateindex_company_id, fe.company_id,
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
    # financize(si_all_df) -> si_all_df
    # inconsistent when/where logic SO I AM deciding NOT to 'repalce with NA' here
    financize(si_all_df, char_col_numeric_limit = 99999999999999.99) -> si_all_df
    
    return(si_all_df) 

  }
  ret <- verify_month_often_month_past_returns_inner(dateindex = dateindex)

  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
} 


# required columns
#  
# dateindex, dateindexlbd, dateindexp01lbd
# split_date
# company_id
# perend_q1, perend_q2
# date_eq0
# mktcap
# price
# 
# no longer required, no longer used columns, but still in there 
# ( some commented out, some in lower queries )
# 
# ticker, company
# pertyp_q1
# 
# current calculations
# nowlast_columns = c("sales_q1", "netinc_q1", "ncc_q1", "assets_q1", "assets_q2", "tco_q1", "tcf_q1", "tci_q1", "ca_q1", "cl_q1", "liab_q1")
# 
# can not be the basis:        perend_q1, perend_q2, price, mktcap
# can not be the fundmentals:  netinc_q1,  sales_q1
#
# because 'basis' and 'fundamenals' are *hardcoded in* 
#
load_inbnd_stmtstats <- function (dateindex = NULL, nowlast_columns = NULL, support_dateindex_collection = NULL,  char_col_numeric_limit = NULL) {
  
  message(gsub("\"","",capture.output(match.call())))
  print(dateindex)
  
  # R version 3.4.1 (2017-06-30) # sessionInfo()
  
  require(RPostgreSQL)
  require(PivotalR)
  require(stringi)
  require(stringr)
  
  ops <- options() 
  
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  options(warn = 1)
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  load_inbnd_stmtstats_inner <- function (dateindex = NULL, nowlast_columns = NULL, support_dateindex_collection = NULL,  char_col_numeric_limit = NULL) {
    
    # Then do everything
    if(!is.null(dateindex)) {  
      display_where_condition <- str_c(" = ", dateindex)
      support_where_condition <- str_c(" in (", str_c(support_dateindex_collection, collapse = ", "), ")") 
    } else { 
      display_where_condition <- " > -99999 " 
      support_where_condition <- " > -99999 "
      
    }
    
    verify_connection()
    
    # !NOTE! last_value() over () is broken in PostgreSQL 10.0 over clause is completely ignored
    
    message(paste0("Beginning load_inbnd_stmtstats query SQL of dateindex: ", dateindex))
    
    ## ratios not usefull 'right now' because of explosion
    ## interesting compare (current) mktcap vs last_inbnd_stmtstat_mktcap 

    ## CURRENT WORKING GOOD 
    ##
    # str_c("
    #       select sq4.* 
    #       from ( -- sq4
    #         select 
    #           sq3.dateindex_company_id
    #         , sq3.dateindex
    #         , sq3.dateindexlbd
    #         , sq3.company_id  
    #         , sq3.now_inbnd_stmtid_dateindex
    #         , sq3.now_inbnd_stmtid_dateindexlbd
    #         , sq3.now_inbnd_stmtstat_perend_q1
    #         , sq3.now_inbnd_stmtstat_perend_q2
    #         , sq3.now_inbnd_stmtstat_sales_q1
    #         , sq3.now_inbnd_stmtstat_netinc_q1
    #         , sq3.now_inbnd_stmtstat_ncc_q1
    #         , sq3.now_inbnd_stmtstat_assets_q1
    #         , sq3.now_inbnd_stmtstat_assets_q2
    #         , sq3.now_inbnd_stmtstat_mktcap
    #         , sq3.now_inbnd_stmtstat_price
    #         , sq3.now_inbnd_stmtstat_tco_q1
    #         , sq3.now_inbnd_stmtstat_tcf_q1
    #         , sq3.now_inbnd_stmtstat_tci_q1
    #         , sq3.now_inbnd_stmtstat_ca_q1
    #         , sq3.now_inbnd_stmtstat_cl_q1
    #         , sq3.now_inbnd_stmtstat_liab_q1
    #         -- SMALL RATIO EXPLOSIONS MAKE THESE USELESS
    #         --, sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
    #         --, sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
    #         --, sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    #         ---- , sq3.now_inbnd_stmtid_dateindex_partition    -- sql debugging utility
    #         ---- , sq3.now_inbnd_stmtid_dateindexlbd_partition -- sql debugging utility
    #         , first_value(sq3.now_inbnd_stmtid_dateindex)     over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition    order by sq3.dateindex   ) last_inbnd_stmtid_dateindex
    #         , first_value(sq3.now_inbnd_stmtid_dateindexlbd)  over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtid_dateindexlbd
    #         , first_value(sq3.now_inbnd_stmtstat_perend_q1)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtid_perend_q1
    #         , first_value(sq3.now_inbnd_stmtstat_perend_q2)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtid_perend_q2
    #         , first_value(sq3.now_inbnd_stmtstat_sales_q1)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_sales_q1
    #         , first_value(sq3.now_inbnd_stmtstat_netinc_q1)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_netinc_q1
    #         , first_value(sq3.now_inbnd_stmtstat_ncc_q1)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_ncc_q1
    #         , first_value(sq3.now_inbnd_stmtstat_assets_q1)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_assets_q1
    #         , first_value(sq3.now_inbnd_stmtstat_assets_q2)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_assets_q2
    #         , first_value(sq3.now_inbnd_stmtstat_mktcap)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_mktcap
    #         , first_value(sq3.now_inbnd_stmtstat_price)       over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_price
    #         , first_value(sq3.now_inbnd_stmtstat_tco_q1)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_tco_q1
    #         , first_value(sq3.now_inbnd_stmtstat_tcf_q1)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_tcf_q1
    #         , first_value(sq3.now_inbnd_stmtstat_tci_q1)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_tci_q1
    #         , first_value(sq3.now_inbnd_stmtstat_ca_q1)       over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_ca_q1
    #         , first_value(sq3.now_inbnd_stmtstat_cl_q1)       over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_cl_q1
    #         , first_value(sq3.now_inbnd_stmtstat_liab_q1)     over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_liab_q1
    #         --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
    #         --, first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
    #         --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
    #           , sq3.pct_freeprice_ret_01m_ann
    #       from ( -- sq3
    #         select 
    #             sq2.dateindex_company_id
    #           , sq2.dateindex
    #           , sq2.dateindexlbd
    #           , sq2.company_id  
    #           , sq2.ticker
    #           , sq2.company
    #           , sq2.date_eq0
    #           , sq2.now_inbnd_stmtid_dateindex
    #           , sq2.now_inbnd_stmtid_dateindexlbd
    #           , sq2.now_inbnd_stmtstat_perend_q1
    #           , sq2.now_inbnd_stmtstat_perend_q2
    #           , sq2.now_inbnd_stmtstat_sales_q1
    #           , sq2.now_inbnd_stmtstat_netinc_q1
    #           , sq2.now_inbnd_stmtstat_ncc_q1
    #           , sq2.now_inbnd_stmtstat_assets_q1
    #           , sq2.now_inbnd_stmtstat_assets_q2
    #           , sq2.now_inbnd_stmtstat_mktcap
    #           , sq2.now_inbnd_stmtstat_price
    #           , sq2.now_inbnd_stmtstat_tco_q1
    #           , sq2.now_inbnd_stmtstat_tcf_q1
    #           , sq2.now_inbnd_stmtstat_tci_q1
    #           , sq2.now_inbnd_stmtstat_ca_q1
    #           , sq2.now_inbnd_stmtstat_cl_q1
    #           , sq2.now_inbnd_stmtstat_liab_q1
    #        -- , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    #        -- , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    #        -- , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    #        -- TRICK I LEARNED ON THE INTERNET
    #        -- GENERATE A SERIES 1,2,3(sum cummulative) ON MANY RANGES, EACH RANGE OF MANY RECORDS ( therefore 'first_value' is the inbound )
    #        -- AT FIRST POSITION 'non-NULL' value in a range, sets the value for the current range
    #        -- AT THE NEXT RANGE (same as above) and ITS value increases by 1
    #        -- locf: last observation carried forward
    #        -- How do I efficiently select the previous non-null value?
    #        -- https://stackoverflow.com/questions/18987791/how-do-i-efficiently-select-the-previous-non-null-value
    #           , sum(case when sq2.now_inbnd_stmtid_dateindex    is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex)    as now_inbnd_stmtid_dateindex_partition
    #           , sum(case when sq2.now_inbnd_stmtid_dateindexlbd is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindexlbd) as now_inbnd_stmtid_dateindexlbd_partition
    #           , sq2.pct_freeprice_ret_01m_ann
    #         from ( -- sq2
    #           select
    #               sq1.dateindex_company_id
    #             , sq1.dateindex
    #             , sq1.dateindexlbd
    #             , sq1.company_id
    #             , sq1.ticker
    #             , sq1.company
    #             , sq1.date_eq0
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_dateindexlbd         else null end now_inbnd_stmtid_dateindexlbd
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_perend_q1            else null end now_inbnd_stmtstat_perend_q1 
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_perend_q2            else null end now_inbnd_stmtstat_perend_q2 
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_sales_q1             else null end now_inbnd_stmtstat_sales_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_netinc_q1            else null end now_inbnd_stmtstat_netinc_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_ncc_q1               else null end now_inbnd_stmtstat_ncc_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_assets_q1            else null end now_inbnd_stmtstat_assets_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_assets_q2            else null end now_inbnd_stmtstat_assets_q2 
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap 
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price 
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_tco_q1               else null end now_inbnd_stmtstat_tco_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_tcf_q1               else null end now_inbnd_stmtstat_tcf_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_tci_q1               else null end now_inbnd_stmtstat_tci_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_ca_q1                else null end now_inbnd_stmtstat_ca_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_cl_q1                else null end now_inbnd_stmtstat_cl_q1
    #             , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_liab_q1              else null end now_inbnd_stmtstat_liab_q1
    #         --  , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
    #         --  , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
    #         --  , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1 
    #             , sq1.pct_freeprice_ret_01m_ann
    #           from ( -- sq1
    #             select
    #                 now.dateindex_company_id
    #               , now.ticker
    #               , now.company
    #       	      , now.dateindex
    #       	      , now.dateindexlbd
    #       	      , now.company_id
    #               , now.date_eq0
    #               , now.perend_q1        now_perend_q1
    #               , now.perend_q2        now_perend_q2
    #       	      , now.dateindex        now_dateindex
    #       	      , now.dateindexlbd     now_dateindexlbd
    #               , now.company_id       now_company_id
    #               , now.sales_q1         now_sales_q1
    #               , now.netinc_q1        now_netinc_q1  
    #               , now.ncc_q1           now_ncc_q1  
    #               , now.assets_q1        now_assets_q1 
    #               , now.assets_q2        now_assets_q2  
    #               , now.mktcap           now_mktcap     
    #               , now.price            now_price
    #               , now.tco_q1           now_tco_q1 
    #               , now.tcf_q1           now_tcf_q1 
    #               , now.tci_q1           now_tci_q1 
    #               , now.ca_q1            now_ca_q1  
    #               , now.cl_q1            now_cl_q1  
    #               , now.liab_q1          now_liab_q1 
    #           --  , case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end now_perlen_days_q1
    #           -- EASIER THAN ABOVE
    #               , now.perend_q1 - now.perend_q2 now_perlen_days_q1
    #           --  , case when perend_q1 - perend_q2 > 0 then perend_q1 - perend_q2 else (365 / 12)       end now_perlen_days_q1
    #                                                         -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
    #           --  , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
    #           --  , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
    #           --  , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
    #             --  , now.date_eq0         now_date_eq0
    #             --  , now.perend_q1        now_perend_q1
    #       	, case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
    #       	       when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
    #       	       else                                now.perend_q1                           -- ... otherwise everything is null so just null
    #       	    end now_eff_date_eq0
    #             --   , now.pertyp_q1        now_pertyp_q1
    #             --   , now.perlen_q1        now_perlen_q1
    #             --   , p01lbd.dateindex     p01lbd_dateindex
    #             --   , p01lbd.dateindexlbd  p01lbd_dateindexlbd
    #             --   , p01lbd.company_id    p01lbd_company_id
    #             --   , p01lbd.sales_q1      p01lbd_sales_q1
    #             --   , p01lbd.netinc_q1     p01lbd_netinc_q1
    #             --   , p01lbd.mktcap        p01lbd_mktcap
    #             --   , p01lbd.price         p01lbd_price
    #             --   , p01lbd.netinc_q1/nullif(p01lbd.mktcap,0) p01lbd_netinc_q1_o_mktcap  
    #             --   , p01lbd.date_eq0      p01lbd_date_eq0
    #             --   , p01lbd.perend_q1     p01lbd_perend_q1
    #       	, case when   p01lbd.date_eq0             >   p01lbd.perend_q1         then p01lbd.date_eq0 -- greater than and neither is null
    #       	       when   p01lbd.date_eq0 is not null and p01lbd.perend_q1 is null then p01lbd.date_eq0
    #                      else                             p01lbd.perend_q1                              -- ... otherwise everything is null so just null
    #       	    end p01lbd_eff_date_eq0
    #             --   , p01lbd.pertyp_q1     p01lbd_pertyp_q1
    #             --   , p01lbd.perlen_q1     p01lbd_perlen_q1
    #         , case when not ( now.split_date > now.dateindexp01lbd ) -- NEW(from lwd) -- COME_BACK -- assuming no work on a Saturday or Sunday
    #                then  ( lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindexlbd) - (now.mktcap/nullif(now.price,0)) ) /  nullif((now.mktcap/nullif(now.price,0)),0)
    #                else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return  
    #             	 from
    #                 ( select   ins.dateindex_company_id, ins.dateindex, ins.dateindexlbd, ins.dateindexp01lbd, ins.company_id, ins.perend_q1, ins.perend_q2, ins.date_eq0, ins.split_date, ins.perlen_q1, ins.ticker, ins.company, ins.pertyp_q1, ins.price, ins.mktcap, ins.sales_q1, ins.netinc_q1, ins.ncc_q1, ins.assets_q1, ins.assets_q2, ins.tco_q1, ins.tcf_q1, ins.tci_q1, ins.ca_q1, ins.cl_q1, ins.liab_q1  
    #                            from si_finecon2 ins  where ins.dateindex ", support_where_condition, ") now left outer join si_finecon2 p01lbd on now.dateindexp01lbd  = p01lbd.dateindexlbd and now.company_id = p01lbd.company_id 
    #           ) sq1                               -- where ins.ticker in ('AAPL','MSFT') -- VERY easy to test
    #         ) sq2                                 -- where ins.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982) -- first ONE minute AFTER 13 seconds WITH SORT
    #       ) sq3
    #       order by 2,1
    #     ) sq4 where sq4.dateindex ", display_where_condition, "
    # ") -> add_columns_sql

    str_c("
          select sq4.* 
          from ( -- sq4
            select 
              sq3.dateindex_company_id
            , sq3.dateindex
            , sq3.dateindexlbd
            , sq3.company_id  
            , sq3.now_inbnd_stmtid_dateindex
            , sq3.now_inbnd_stmtid_dateindexlbd
            , sq3.now_inbnd_stmtstat_perend_q1
            , sq3.now_inbnd_stmtstat_perend_q2
            , sq3.now_inbnd_stmtstat_price
            , sq3.now_inbnd_stmtstat_mktcap
            ", if(length(nowlast_columns)) { str_c(sprintf(", sq3.now_inbnd_stmtstat_%s", nowlast_columns), collapse = "\n            ")} else { "" }, "
            -- SMALL RATIO EXPLOSIONS MAKE THESE USELESS
            --, sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
            --, sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
            --, sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
            ---- , sq3.now_inbnd_stmtid_dateindex_partition    -- sql debugging utility
            ---- , sq3.now_inbnd_stmtid_dateindexlbd_partition -- sql debugging utility
            , first_value(sq3.now_inbnd_stmtid_dateindex)     over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition    order by sq3.dateindex   ) last_inbnd_stmtid_dateindex
            , first_value(sq3.now_inbnd_stmtid_dateindexlbd)  over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtid_dateindexlbd
            , first_value(sq3.now_inbnd_stmtstat_perend_q1)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtid_perend_q1
            , first_value(sq3.now_inbnd_stmtstat_perend_q2)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtid_perend_q2
            , first_value(sq3.now_inbnd_stmtstat_price)       over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_price
            , first_value(sq3.now_inbnd_stmtstat_mktcap)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_mktcap
            ", if(length(nowlast_columns)) { str_c(sprintf(", first_value(sq3.now_inbnd_stmtstat_%s )    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindexlbd) last_inbnd_stmtstat_%s", nowlast_columns, nowlast_columns), collapse = paste0("\n            "))  } else { "" }, "
            --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
            --, first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
            --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindexlbd_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
              , sq3.pct_freeprice_ret_01m_ann
          from ( -- sq3
            select 
                sq2.dateindex_company_id
              , sq2.dateindex
              , sq2.dateindexlbd
              , sq2.company_id  
              , sq2.ticker
              , sq2.company
              , sq2.date_eq0
              , sq2.now_inbnd_stmtid_dateindex
              , sq2.now_inbnd_stmtid_dateindexlbd
              , sq2.now_inbnd_stmtstat_perend_q1
              , sq2.now_inbnd_stmtstat_perend_q2
              , sq2.now_inbnd_stmtstat_price
              , sq2.now_inbnd_stmtstat_mktcap
              ", if(length(nowlast_columns)) { str_c(sprintf(", sq2.now_inbnd_stmtstat_%s", nowlast_columns), collapse = "\n              ")} else { "" } ,"
           -- , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
           -- , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
           -- , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
           -- TRICK I LEARNED ON THE INTERNET
           -- GENERATE A SERIES 1,2,3(sum cummulative) ON MANY RANGES, EACH RANGE OF MANY RECORDS ( therefore 'first_value' is the inbound )
           -- AT FIRST POSITION 'non-NULL' value in a range, sets the value for the current range
           -- AT THE NEXT RANGE (same as above) and ITS value increases by 1
           -- locf: last observation carried forward
           -- How do I efficiently select the previous non-null value?
           -- https://stackoverflow.com/questions/18987791/how-do-i-efficiently-select-the-previous-non-null-value
              , sum(case when sq2.now_inbnd_stmtid_dateindex    is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex)    as now_inbnd_stmtid_dateindex_partition
              , sum(case when sq2.now_inbnd_stmtid_dateindexlbd is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindexlbd) as now_inbnd_stmtid_dateindexlbd_partition
              , sq2.pct_freeprice_ret_01m_ann
            from ( -- sq2
              select
                  sq1.dateindex_company_id
                , sq1.dateindex
                , sq1.dateindexlbd
                , sq1.company_id
                , sq1.ticker
                , sq1.company
                , sq1.date_eq0
                , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
                , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_dateindexlbd         else null end now_inbnd_stmtid_dateindexlbd
                , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_perend_q1            else null end now_inbnd_stmtstat_perend_q1 
                , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_perend_q2            else null end now_inbnd_stmtstat_perend_q2 
                , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price 
                , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap 
                ", if(length(nowlast_columns)) { str_c(sprintf(", case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_%s             else null end now_inbnd_stmtstat_%s", nowlast_columns, nowlast_columns), collapse = paste0("\n                "))  } else { "" }, "
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lbd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1 
                , sq1.pct_freeprice_ret_01m_ann
              from ( -- sq1
                select
                    now.dateindex_company_id
                  , now.ticker
                  , now.company
          	      , now.dateindex
          	      , now.dateindexlbd
          	      , now.company_id
                  , now.date_eq0
                  , now.perend_q1        now_perend_q1
                  , now.perend_q2        now_perend_q2
          	      , now.dateindex        now_dateindex
          	      , now.dateindexlbd     now_dateindexlbd
                  , now.company_id       now_company_id
                  , now.price            now_price
                  , now.mktcap           now_mktcap
                  ", if(length(nowlast_columns)) { str_c(sprintf(", now.%s      now_%s", nowlast_columns, nowlast_columns), collapse = paste0("\n          	      "))  } else { "" }, "
              --  , case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end now_perlen_days_q1
              -- EASIER THAN ABOVE
                  , now.perend_q1 - now.perend_q2 now_perlen_days_q1
              --  , case when perend_q1 - perend_q2 > 0 then perend_q1 - perend_q2 else (365 / 12)       end now_perlen_days_q1
                                                            -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
              --  , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
              --  , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
              --  , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
                --  , now.date_eq0         now_date_eq0
                --  , now.perend_q1        now_perend_q1
          	, case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
          	       when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
          	       else                                now.perend_q1                           -- ... otherwise everything is null so just null
          	    end now_eff_date_eq0
                --   , now.pertyp_q1        now_pertyp_q1
                --   , now.perlen_q1        now_perlen_q1
                --   , p01lbd.dateindex     p01lbd_dateindex
                --   , p01lbd.dateindexlbd  p01lbd_dateindexlbd
                --   , p01lbd.company_id    p01lbd_company_id
                --   , p01lbd.sales_q1      p01lbd_sales_q1
                --   , p01lbd.netinc_q1     p01lbd_netinc_q1
                --   , p01lbd.mktcap        p01lbd_mktcap
                --   , p01lbd.price         p01lbd_price
                --   , p01lbd.netinc_q1/nullif(p01lbd.mktcap,0) p01lbd_netinc_q1_o_mktcap  
                --   , p01lbd.date_eq0      p01lbd_date_eq0
                --   , p01lbd.perend_q1     p01lbd_perend_q1
          	, case when   p01lbd.date_eq0             >   p01lbd.perend_q1         then p01lbd.date_eq0 -- greater than and neither is null
          	       when   p01lbd.date_eq0 is not null and p01lbd.perend_q1 is null then p01lbd.date_eq0
                         else                             p01lbd.perend_q1                              -- ... otherwise everything is null so just null
          	    end p01lbd_eff_date_eq0
                --   , p01lbd.pertyp_q1     p01lbd_pertyp_q1
                --   , p01lbd.perlen_q1     p01lbd_perlen_q1
            , case when not ( now.split_date > now.dateindexp01lbd ) -- NEW(from lwd) -- COME_BACK -- assuming no work on a Saturday or Sunday
                   then  ( lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindexlbd) - (now.mktcap/nullif(now.price,0)) ) /  nullif((now.mktcap/nullif(now.price,0)),0)
                   else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return  
                	 from
                    ( select   ins.dateindex_company_id, ins.dateindex, ins.dateindexlbd, ins.dateindexp01lbd, ins.company_id, ins.perend_q1, ins.perend_q2, ins.date_eq0, ins.split_date, ins.perlen_q1, ins.ticker, ins.company, ins.pertyp_q1, ins.price, ins.mktcap ", if(length(nowlast_columns)) { str_c(sprintf(", ins.%s", nowlast_columns), collapse = "")  } else { "" } ,"
                               from si_finecon2 ins  where ins.dateindex ", support_where_condition, ") now left outer join si_finecon2 p01lbd on now.dateindexp01lbd  = p01lbd.dateindexlbd and now.company_id = p01lbd.company_id 
              ) sq1                               -- where ins.ticker in ('AAPL','MSFT') -- VERY easy to test
            ) sq2                                 -- where ins.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982) -- first ONE minute AFTER 13 seconds WITH SORT
          ) sq3
          order by 2,1
        ) sq4 where sq4.dateindex ", display_where_condition, "
    ") -> add_columns_sql
    

    db.q(add_columns_sql, nrows = "all", conn.id = cid) -> si_all_df
    
    message(paste0("Ending load_inbnd_stmtstats query SQL of dateindex: ", dateindex))
    
    financize(si_all_df,  char_col_numeric_limit = char_col_numeric_limit) -> si_all_df
    return(si_all_df) 
    
  }
  ret <- load_inbnd_stmtstats_inner(dateindex = dateindex, nowlast_columns = nowlast_columns, support_dateindex_collection = support_dateindex_collection,  char_col_numeric_limit = char_col_numeric_limit)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  return(ret)
}
# # debug(load_inbnd_stmtstats)
# # required date_eq0, perend_q1, perlen_q1, pertyp_q1, dateindex_company_id, dateindex, dateindexp01lwd, company_id, sales_q1, netinc_q1, mktcap, price
# load_inbnd_stmtstats(
#     dateindex = 17347 # e.g. last loaded pay period
#   , support_dateindex_collection = c(17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982)
#   , char_col_numeric_limit = 999999.99    # for right NOW pure AAII data ( unratio-ed )
# )  -> si_all_g_df

## NEWER # nowlast_columns # TEST 
# load_inbnd_stmtstats(
#     dateindex = 17347 # e.g. last loaded pay period
#   , nowlast_columns = c("sales_q1", "netinc_q1", "ncc_q1", "assets_q1", "assets_q2", "tco_q1", "tcf_q1", "tci_q1", "ca_q1", "cl_q1", "liab_q1")
#   , support_dateindex_collection = c(17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982)
#   , char_col_numeric_limit = 999999.99    # for right NOW pure AAII data ( unratio-ed )
# )  -> si_all_g_df


# 
# REASON: SO I CAN DO 'WEIGHTED' MEASURES (SEEMS NOT WORTH THE EFFORT)
# uses now_inbnd_stmtstat last_inbnd_stmtstat
# since MANY SQLs upsertS are done inside
# 
#  WORKING(but not used): load_division_aggregated_now_last_mktcap_per_company_id
# 
load_division_aggregated_now_last_mktcap_per_company_id <- function(dateindex = NULL) {

  message(gsub("\"","",capture.output(match.call())))
  print(dateindex)
  
  ops <- options() 
  options(warn = 1)
  
  require(PivotalR)
  # R.rsp    rstring
  # stringi  stri_join
  
  # require(R.rsp)
  ##NOT USED
  ##stringi::`%s+%` -> `%s+%`

  DATEINDEX         <- dateindex

  DIVISION          <- c("", "sector_desc", "industry_desc")
  
  SP_OPS_WHAT       <- c("('500','400','600')", "('500')")
  SP_OPS_WHAT_SHORT <- c("sp"                 , "sp500"  ) 

  combo_grid   <- expand.grid(DIVISION=DIVISION, SP_OPS_WHAT=SP_OPS_WHAT, stringsAsFactors = FALSE)
  combo_grid_f <- seq_along(row.names(combo_grid))

  verify_connection()
    

  for(combo_i in split(combo_grid, combo_grid_f)) {


    SP_OPS_WHAT_SHORT_I <- SP_OPS_WHAT_SHORT[match(combo_i[["SP_OPS_WHAT"]],SP_OPS_WHAT)]
    
    long_message <- paste0(names(combo_i),"_" , combo_i, collapse = "__")
    
    message(paste0("Beginning load_division_aggregated_now_last_mktcap_per_company_id query SQL of dateindex: ", dateindex, " and ", long_message  ))
    
    # ANDRE SAFE FORM concatination operator
    `%S+%` <- function(x,y) {
    
      if(is.null(x) || is.na(x) || !length(x) ) '' -> x
      if(is.null(y) || is.na(y) || !length(y) ) '' -> y
    
      return(stringi::stri_join(x, y, sep=""))

    }
    
    # [ ] TODO - MAKE THAT REPEATED STUFF INTO A FUNCTION
    local({R.rsp::rstring("
      select 
          sr.company_id  -- REQUIRED SO I ONLY UPSERT specific OPS/DIVISION member COMPANIES
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex
          <%= {if(DIVISION_I != '') { ', ' %S+% DIVISION_I }} %>
          , count(1)                          count<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>
          , count(now_inbnd_stmtstat_mktcap)  count<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_now_inbnd_stmtstat_mktcap
          , sum(now_inbnd_stmtstat_mktcap)      sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_now_inbnd_stmtstat_mktcap
          , count(last_inbnd_stmtstat_mktcap) count<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_last_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)     sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_last_inbnd_stmtstat_mktcap
          , sum(mktcap)                         sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_mktcap
          , count(now_inbnd_stmtstat_assets_q1)  count<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_now_inbnd_stmtstat_assets_q1
          , sum(now_inbnd_stmtstat_assets_q1)      sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_now_inbnd_stmtstat_assets_q1
          , count(last_inbnd_stmtstat_assets_q1) count<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_last_inbnd_stmtstat_assets_q1
          , sum(last_inbnd_stmtstat_assets_q1)     sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_last_inbnd_stmtstat_assets_q1
          , sum(assets_q1)                         sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_assets_q1
          , count(now_inbnd_stmtstat_assets_q2)  count<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_now_inbnd_stmtstat_assets_q2
          , sum(now_inbnd_stmtstat_assets_q2)      sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_now_inbnd_stmtstat_assets_q2
          , count(last_inbnd_stmtstat_assets_q2) count<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_last_inbnd_stmtstat_assets_q2
          , sum(last_inbnd_stmtstat_assets_q2)     sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_last_inbnd_stmtstat_assets_q2
          , sum(assets_q2)                         sum<%= {if(SP_OPS_WHAT_I != '') { '_' %S+% SP_OPS_WHAT_SHORT_I }}  %S+% {if(DIVISION_I != '') { '_' %S+% DIVISION_I }}  %>_assets_q2
        from si_finecon2 where dateindex = <%=DATEINDEX%> and 
                              <%= {if(SP_OPS_WHAT_I != '') { ' sp in ' %S+% SP_OPS_WHAT_I %S+% '     ' }} %> 
        group by dateindex <%= {if(DIVISION_I != '') { ', ' %S+% DIVISION_I }} %> ) sq1 
      on sr.dateindex = sq1.dateindex and 
         <%= {if(SP_OPS_WHAT_I != '') { 'sr.sp in ' %S+% SP_OPS_WHAT_I %S+% ' and ' }} %> 
         <%= {if(DIVISION_I != '') { 'sr.' %S+% DIVISION_I %S+% ' = sq1.' %S+% DIVISION_I %S+% ' and ' }} %>
         sr.dateindex = <%=DATEINDEX%>
      ")}, envir = list2env(list(DIVISION_I = combo_i[["DIVISION"]], SP_OPS_WHAT_I = combo_i[["SP_OPS_WHAT"]]
                             , SP_OPS_WHAT_SHORT_I = SP_OPS_WHAT_SHORT_I
                             , DATEINDEX = DATEINDEX))
    ) -> add_columns_sql
    
    db.q(add_columns_sql, nrows = "all", conn.id = cid) -> si_all_df
    
    if(!NROW(si_all_df)) stop(paste0("Returned zero records ", dateindex))
    
    financize(si_all_df, char_col_numeric_limit = 99999999999999.99) -> si_all_df
    upsert(si_all_df, keys = c("company_id"))
  
    message(paste0("Ending load_division_aggregated_now_last_mktcap_per_company_id query SQL of dateindex: ", dateindex, " and ", long_message  ))

  }
  

  options(ops)
  
  return(TRUE)
  
}
# call 
# uses now_inbnd_stmtstat last_inbnd_stmtstat
# since MANY SQLs upsertS are done inside ( TESTER: USE THIS ONE)
# load_division_aggregated_now_last_mktcap_per_company_id(dateindex = 17347)
# 
# from_dir = "W:/AAIISIProDBFs"
# as.integer(dir(from_dir))         ->     all_dbf_dirs
# is_lwd_of_month(all_dbf_dirs)     -> lwd_all_dbf_dirs_tf
# all_dbf_dirs[lwd_all_dbf_dirs_tf] ->     lwd_dbf_dirs
# seq_along(lwd_dbf_dirs) -> lwd_months_idx
# sort(lwd_dbf_dirs, decreasing = TRUE)[lwd_months_idx]  -> wd_dbf_dirs_ordered
# print(wd_dbf_dirs_ordered)
# [1] 17378 17347 17317 17284
# 
# mass updates
# sapply(wd_dbf_dirs_ordered, load_division_aggregated_now_last_mktcap_per_company_id)
# 
# CAN BE (per recent month)
# uses now_inbnd_stmtstat last_inbnd_stmtstat
# since MANY SQLs upsertS are done inside
# load_division_aggregated_now_last_mktcap_per_company_id(dateindex = dir_i)
#
# MATH_LOGIC WRONG:    : load_division_aggregated_per_dateindex ( WRONG BECAUSE: ALL columns USED must be NOT NULL everwhere )
# 
# uses now_inbnd_stmtstat last_inbnd_stmtstat
# since MANY SQLs upsertS are done inside
load_division_aggregated_per_dateindex <- function(dateindex = NULL) {

  message(gsub("\"","",capture.output(match.call())))
  print(dateindex)
  
  ops <- options() 
  options(warn = 1)
  
  require(PivotalR)
  # R.rsp    rstring
  # stringi  stri_join
  
  # NOTE: COULD/SHOULD LIBERALIZE ... 
  # 
  # , 'sp_desc'::text collection_name01_fct
  # end sp_desc_fct
  # sp in 
  # , sp_desc_fct
  # 
  # BUT currently hightly correlated with
  #
  # load_inbnd_stmtstats
  # load_division_aggregated_now_last_mktcap_per_company_id
  #
  # SO WHEN I DO LIBERALIZE ALSO THEN LIBERALIZE THESE TOO
  #
  
  #                 **** SAME AS ****
  # load_division_aggregated_now_last_mktcap_per_company_id 

  DATEINDEX         <- dateindex

  DIVISION          <- c("industry_desc", "sector_desc", "")
  SP_OPS_WHAT       <- c("('500','400','600')", "('500')")
  SP_OPS_WHAT_SHORT <- c("sp"                 , "sp500"  ) 

  # Re-organized for debugging ( TEMPORARY ) 
  
  # DIVISION          <- c("", "sector_desc", "industry_desc")
  # SP_OPS_WHAT       <- c("('500')", "('500','400','600')")
  # SP_OPS_WHAT_SHORT <- c("sp500"  , "sp"                 ) 
  
  # NEW
  DIVISION_ITEMS <- list()
  DIVISION_ITEMS[["industry_desc"]] <- c("Gold & Silver",   "Furniture & Fixtures", "Oil & Gas Operations") 
  DIVISION_ITEMS[["sector_desc"]]   <- c("Basic Materials", "Energy")
  
  # OLD AGAIN
  
  combo_grid   <- expand.grid(DIVISION=DIVISION, SP_OPS_WHAT=SP_OPS_WHAT, stringsAsFactors = FALSE)
  combo_grid_f <- seq_along(row.names(combo_grid))

  verify_connection()
    

  for(combo_i in split(combo_grid, combo_grid_f)) {


    SP_OPS_WHAT_SHORT_I <- SP_OPS_WHAT_SHORT[match(combo_i[["SP_OPS_WHAT"]],SP_OPS_WHAT)]
    
    long_message <- paste0(paste0(names(combo_i),"_" , combo_i,collapse = "__"), "__", paste0(DIVISION_ITEMS[[as.list(combo_i)[["DIVISION"]]]], colapse ="__"), collapse = "____") 
    
    message(paste0("Beginning load_division_aggregated_per_dateindex of dateindex: ", dateindex, " and ",  long_message  ))
    
    # ANDRE SAFE FORM concatination operator
    `%S+%` <- function(x,y) {
    
      if(is.null(x) || is.na(x) || !length(x) ) '' -> x
      if(is.null(y) || is.na(y) || !length(y) ) '' -> y
    
      return(stringi::stri_join(x, y, sep=""))

    }
    
    # Safe single quote: direction is down/up ( not left/right )
    SQuote <- function(x) paste0("'",x,"'")
    
    ## IN load_division_aggregated_per_dateindex ##
    
    # [ ] TO DO MAKE THAT REPEATED STUFF INTO A FUNCTION
    local({R.rsp::rstring("
      select
          dateindex
        , dateindexlbd
        , dateindexeom
        , dateindexeom::text dateindexeom_fct
        <%= {if(SP_OPS_WHAT_I != '') { ', ' %S+% SQuote('sp_desc') %S+% '::text collection_name01_fct' }} %>
        <%= {if(SP_OPS_WHAT_I != '') { ',     case when sp in ' %S+% SP_OPS_WHAT_I %S+% ' then ' %S+%  SQuote(SP_OPS_WHAT_SHORT_I)  %S+% '::text else ' %S+% SQuote('not' %S+% SP_OPS_WHAT_SHORT_I)  %S+% '::text end sp_desc_fct'  }} %>
        <%= {if(DIVISION_I != '') { ', ' %S+% SQuote(DIVISION_I) %S+% '::text collection_name02_fct' }} %>
        <%= {if(DIVISION_I != '') { ',     ' %S+% DIVISION_I %S+% ' ' %S+% DIVISION_I %S+% '_fct' }} %>
        -- NOWS
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap else null end), 0)  * 10000.00  rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000  
        ,        sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap   else null end), 0) * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap    else null end), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000

        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and                    mktcap is not null then now_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and                    mktcap is not null then                    mktcap else null end), 0)  * 10000.00  rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000  
        ,        sum(case when now_inbnd_stmtstat_sales_q1 is not null and                    mktcap is not null then now_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_sales_q1 is not null and                    mktcap is not null then                    mktcap   else null end), 0) * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and                    mktcap is not null then now_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and                    mktcap is not null then                    mktcap    else null end), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000

        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_netinc_q1  else null end)   /
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_sales_q1  else null end), 0) * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_ncc_q1    else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1 else null end), 0) * 10000.00  rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000  
        ,        sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_sales_q1  else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_netinc_q1  else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1  else null end), 0) * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_ncc_q1  else null end )    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_sales_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100
        -- LASTS
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap else null end), 0)  * 10000.00  rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000
        ,        sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap   else null end), 0) * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap    else null end), 0)  * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000

        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and                     mktcap is not null then last_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and                     mktcap is not null then                     mktcap else null end), 0)  * 10000.00  rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000
        ,        sum(case when last_inbnd_stmtstat_sales_q1 is not null and                     mktcap is not null then last_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_sales_q1 is not null and                     mktcap is not null then                     mktcap   else null end), 0) * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and                     mktcap is not null then last_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and                     mktcap is not null then                     mktcap    else null end), 0)  * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000

        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_netinc_q1  else null end)   /
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_sales_q1  else null end), 0) * 100.00  rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_ncc_q1    else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1 else null end), 0) * 10000.00  rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000
        ,        sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_sales_q1  else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_netinc_q1  else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1  else null end), 0) * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_ncc_q1  else null end )    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_sales_q1 else null end), 0) * 10000.00  rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000
        -- ERROR(ENDGE CASE)SANITY CHECKING
        , count(now_inbnd_stmtid_dateindex)::numeric                                                                                                                                     count_now_inbnd_stmtstat_dateindex
        ,        count(case when now_inbnd_stmtid_dateindex is not null and last_inbnd_stmtid_dateindex is not null then now_inbnd_stmtid_dateindex  else null end)::numeric    / 
          nullif(count(case when now_inbnd_stmtid_dateindex is not null and last_inbnd_stmtid_dateindex is not null then last_inbnd_stmtid_dateindex else null end)::numeric, 0) * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
        , count(now_inbnd_stmtid_dateindexlbd)::numeric                                                                                                                                      count_now_inbnd_stmtstat_dateindexlbd
        ,        count(case when now_inbnd_stmtid_dateindexlbd is not null and last_inbnd_stmtid_dateindexlbd is not null then now_inbnd_stmtid_dateindexlbd  else null end)::numeric    / 
          nullif(count(case when now_inbnd_stmtid_dateindexlbd is not null and last_inbnd_stmtid_dateindexlbd is not null then last_inbnd_stmtid_dateindexlbd else null end)::numeric, 0) * 100.0 rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100
        , sum(sales_q1)  sum_sales_q1
        , sum(netinc_q1) sum_netinc_q1
        , sum(ncc_q1)    sum_ncc_q1
        , sum(assets_q1) sum_assets_q1
        , sum(assets_q2) sum_assets_q2
        , sum(mktcap)    sum_mktcap
        -- NEW
        , sum(netinc_q1) / nullif(sum(ncc_q1),0) * 100.00   rat_netinc_q1_o_ncc_x100 
        , sum(ncc_q1)    / nullif(sum(mktcap),0) * 10000.00    rat_ncc_q1_o_mktcap_x100 -- HERE 
        , sum(assets_q1) / nullif(sum(mktcap),0)            rat_assets_q1_o_mktcap
        --
        , sum(now_inbnd_stmtstat_sales_q1)  sum_now_inbnd_stmtstat_sales_q1
        , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1
        , sum(now_inbnd_stmtstat_ncc_q1)    sum_now_inbnd_stmtstat_ncc_q1
        , sum(now_inbnd_stmtstat_assets_q1) sum_now_inbnd_stmtstat_assets_q1
        , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
        ,        sum(case when now_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap  else null end)     / 
          nullif(sum(case when now_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap else null end), 0) * 100.0 rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
        , sum(last_inbnd_stmtstat_sales_q1)  sum_last_inbnd_stmtstat_sales_q1
        , sum(last_inbnd_stmtstat_netinc_q1) sum_last_inbnd_stmtstat_netinc_q1
        , sum(last_inbnd_stmtstat_ncc_q1)    sum_last_inbnd_stmtstat_ncc_q1
        , sum(last_inbnd_stmtstat_assets_q1) sum_last_inbnd_stmtstat_assets_q1
        , sum(last_inbnd_stmtstat_mktcap)    sum_last_inbnd_stmtstat_mktcap
        , avg(now_inbnd_stmtstat_price)      avg_now_inbnd_stmtstat_price
        , avg(last_inbnd_stmtstat_price)     avg_last_inbnd_stmtstat_price
--       -- OUT OF TIME WED - FIX AGGREGATES [ ]
--       -- RELATIVE CHANGES
--       -- must appear in the GROUP BY clause or be used in an aggregate function
--       , case when now_inbnd_stmtstat_assets_q1 is not null and now_inbnd_stmtstat_assets_q2 is not null then 1 else null end *
--         ( now_inbnd_stmtstat_assets_q1 - now_inbnd_stmtstat_assets_q2 / abs(now_inbnd_stmtstat_assets_q2) ) * 100                   pctchg_now_inbnd_stmtstat_assets_q1_from_q2
--       , case when last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q2 is not null then 1 else null end *
--         ( last_inbnd_stmtstat_assets_q1 - last_inbnd_stmtstat_assets_q2 / abs(last_inbnd_stmtstat_assets_q2) ) * 100                pctchg_last_inbnd_stmtstat_assets_q1_from_q2
--       -- 
--       -- THESE STILL ARE NOT RIGHT ORIGINAL WEIGHTS MUST INCLUDE BOTH PARTS OF THE DENOMINTOR ( FOR FUTURE INTENTIONS )
--       -- FOR EXAMPLE: AGG_MKTCAP_WRT_MKTCAP_AND_PRICE MUST INCLUDE AGGREGATE_OF_MKTCAP_*AND*_WRT__PRICE_IS_NOT_NULL_AND_MKTCAP_IS_NOT_NULL
--       -- 
--       -- OUT OF TIME WED - FIX AGGREGATES [ ]
--       ,    avg(case when last_inbnd_stmtstat_price is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_price * last_inbnd_stmtstat_mktcap else null end    / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_last_inbnd_stmtstat_mktcap,0) )    avg_mktcap_wdt_last_inbnd_stmtstat_price
-- 
--       ,    avg(case when now_inbnd_stmtstat_price is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_price * now_inbnd_stmtstat_mktcap else null end        / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_now_inbnd_stmtstat_mktcap, 0) )    avg_mktcap_wdt_now_inbnd_stmtstat_price
-- 
--       ,    avg(case when last_inbnd_stmtstat_price is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_price * last_inbnd_stmtstat_assets_q1 else null end / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_last_inbnd_stmtstat_assets_q1,0) ) avg_assets_q1_wdt_last_inbnd_stmtstat_price
-- 
--       ,    avg(case when now_inbnd_stmtstat_price is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_price * now_inbnd_stmtstat_assets_q1 else null end     / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_now_inbnd_stmtstat_assets_q1, 0) ) avg_assets_q1_wdt_now_inbnd_stmtstat_price
-- 
--       ,    avg(case when pct_freeprice_ret_01m_ann is not null and mktcap is not null then pct_freeprice_ret_01m_ann *  mktcap  else null end    / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_mktcap,0) )       avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM
-- 
--       ,    avg(case when pct_freeprice_ret_01m_ann is not null and assets_q1 is not null then pct_freeprice_ret_01m_ann * assets_q1 else null end / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_assets_q1,0) )    avg_assets_q1_wdt_pct_freeprice_ret_01m_ann 

      from si_finecon2 where dateindex = <%= DATEINDEX %> and
        <%= {if(SP_OPS_WHAT_I != '') { 'sp in ' %S+% SP_OPS_WHAT_I }} %>
        <%= {if(!is.null(DIVISION_ITEMS_I)) { ' and ' %S+% DIVISION_I %S+% ' in (' %S+% stringi::stri_c(sapply(DIVISION_ITEMS_I, SQuote), collapse = ', ')  %S+% ')     ' }} %>
      group by dateindex, dateindexlbd, dateindexeom<%= {if(SP_OPS_WHAT_I != '') { ', sp_desc_fct' }} %><%= {if(DIVISION_I != '') { ', ' %S+% DIVISION_I %S+% '_fct' }} %> 
      order by dateindex, dateindexlbd, dateindexeom<%= {if(SP_OPS_WHAT_I != '') { ', sp_desc_fct' }} %><%= {if(DIVISION_I != '') { ', ' %S+% DIVISION_I %S+% '_fct' }} %> 
    ")}, envir = list2env(list(
                               DIVISION_I    = combo_i[["DIVISION"]]
                             , SP_OPS_WHAT_I = combo_i[["SP_OPS_WHAT"]]
                             , SP_OPS_WHAT_SHORT_I = SP_OPS_WHAT_SHORT_I
                             , DIVISION_ITEMS_I    = DIVISION_ITEMS[[as.list(combo_i)[["DIVISION"]]]] #  "Gold & Silver" "Furniture & Fixtures"
                             , DATEINDEX           = DATEINDEX))
    ) -> add_columns_sql
    
    db.q(add_columns_sql, nrows = "all", conn.id = cid) -> si_all_df              # SFS
    
    financize(si_all_df, char_col_numeric_limit = 99999999999999.99) -> si_all_df # SFS
    
    if(!NROW(si_all_df)) stop(paste0("Returned zero records ", dateindex))
    
    # SFS ( I REALLY! regret programming using regular expressions )
    liquifyDF(si_all_df, const_cols_regexpr = "^dateindex.*", fctr_cols_rexpr = ".*_fct$") -> si_all_df
    
    # SFS
    print(dateindex);upsert2(value = si_all_df, target_table_name = "si_finecon2_aggregates", upsert_temp_perform_upsert_force = TRUE)
    

    message(paste0("Ending load_division_aggregated_per_dateindex query SQL of dateindex: ", dateindex, " and ", long_message  ))

  }
  

  options(ops)
  
  return(TRUE)
  
}
## TIGHTLY CORRELATED PREREQUISITE:  load_division_aggregated_now_last_mktcap_per_company_id
#
## uses now_inbnd_stmtstat last_inbnd_stmtstat
## REALISTIC mass updates ( per dateindex )
# sapply(wd_dbf_dirs_ordered, function(x) { 
#   ## uses now_inbnd_stmtstat last_inbnd_stmtstat
#   load_division_aggregated_now_last_mktcap_per_company_id(x)
#   load_division_aggregated_per_dateindex(x)
# })
## QUICK MASS UPDATE (if I am sure ABOUT load_division_aggregated_now_last_mktcap_per_company_id(x)
# sapply(wd_dbf_dirs_ordered, load_division_aggregated_per_dateindex)
# INDIVIDUAL
# load_division_aggregated_per_dateindex(dateindex = 17347)
# load_division_aggregated_per_dateindex(dateindex = 17378) # data already in aggr
# load_division_aggregated_per_dateindex(dateindex = 17409)
# upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = 17409, exactly_only_aggregates = TRUE)




# uses load_inbnd_stmtstats
# since MANY SQLs, upsertS are done inside
# REPLACEMENT of WORKING(but not used): load_division_aggregated_now_last_mktcap_per_company_id
# REPLACEMENT of MATH_LOGIC WRONG:    : load_division_aggregated_per_dateindex ( WRONG BECAUSE: ALL columns USED must be NOT NULL everwhere )
load_inbnd_stmtstats_division_aggregates <- function(dateindex = NULL) {

  message(gsub("\"","",capture.output(match.call())))
  print(dateindex)
  
  ops <- options() 
  options(warn = 1)
  
  require(PivotalR)
  # R.rsp    rstring
  # stringi  stri_join
  # Hmisc    llist
  

  
  # ANDRE SAFE FORM concatination operator
  `%S+%` <- function(x,y) {
  
    if(is.null(x) || is.na(x) || !length(x) ) '' -> x
    if(is.null(y) || is.na(y) || !length(y) ) '' -> y
  
    return(stringi::stri_join(x, y, sep=""))

  }
  
  # Safe single quote: direction is down/up ( not left/right )
  SQuote <- function(x) paste0("'",x,"'")
  
  # typically ONE dateindex XOR ALL dateindexes
  # but for PARTIAL dateindexes NEED TO cusomize the name of "DESC" to make THAT MEANING/INTERPRETATION clear
  # e.g. 'odd_year_sp'
  DATEINDEX <- dateindex
  if(is.null(DATEINDEX) || !length(DATEINDEX)) DATEINDEX <- ""
  
  # just 'some' dynamicism
  combo_grid <- Hmisc::llist(
      sp500_general_financialsim = Hmisc::llist(  
             DESC = Hmisc::llist(
               sp_desc         = "case when fei.sp = '500' then 'sp500' else '_error_' end"
             , sector_fin_desc = "case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end"
             )
           , WHERE           = "and fei.sp in('500')"
           , WHERE_DATEINDEX = local({ 
                                 if((length(DATEINDEX) == 1) && DATEINDEX == "") { return("") }
                                 if((length(DATEINDEX) == 1) && DATEINDEX != "") { return("and fei.dateindex = " %S+% DATEINDEX ) }
                                 if((length(DATEINDEX)  > 1)                   ) { return("and fei.dateindex in (values" %S+% paste0("(" %S+% DATEINDEX %S+% ")", collapse = ", ") %S+% ")") }
                                 return("")
                               })
           )
    , sp500_general = Hmisc::llist(
             DESC = Hmisc::llist(
               sp_desc         = "case when fei.sp = '500' then 'sp500' else '_error_' end"
             )
           , WHERE           = "and fei.sp in('500')"
           , WHERE_DATEINDEX = local({ 
                                 if((length(DATEINDEX) == 1) && DATEINDEX == "") { return("") }
                                 if((length(DATEINDEX) == 1) && DATEINDEX != "") { return("and fei.dateindex = " %S+% DATEINDEX ) }
                                 if((length(DATEINDEX)  > 1)                   ) { return("and fei.dateindex in (values" %S+% paste0("(" %S+% DATEINDEX %S+% ")", collapse = ", ") %S+% ")") }
                                 return("")
                               })
           )
  )
  
  verify_connection()
    
  for(combo_i in combo_grid) {

    long_message <- paste0( paste0(names(combo_i$DESC), " = ",paste0(combo_i$DESC)), " AND ", combo_i$WHERE, " AND ", combo_i$WHERE_DATEINDEX, collapse = "")
    
    message(paste0("Beginning load_inbnd_stmtstats_division_aggregates of the following: ", long_message ))
    
    local({R.rsp::rstring("

      -- global flexible: just add/subtract expression: xyz.dateindex = XYZ 
      --
      -- A SOLUTION ( 'WITH' PART MULTPLE FACTOR-ABLE: 'sp500'/'spnot500' )
      -- BREAKDOWN of Financial/NotFinancial 
      -- create index si_finecon2_finecon_sp_500_sector_desc_partial_idx on
      --   fe_data_store.si_finecon2(dateindex, sp, sector_desc) where sp = '500';
      --

      with index_weights as (
        select
            fe.dateindex 
          <%= paste0(', fe.col', seq(length(DESC)*2), '_fct' , collapse = '') %>
          , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f52w_ann is not null) 
                           sum_wrt_pradchg_f52w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f26w_ann is not null) 
                           sum_wrt_pradchg_f26w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f13w_ann is not null) 
                           sum_wrt_pradchg_f13w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f04w_ann is not null) 
                           sum_wrt_pradchg_f04w_ann_mktcap
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'    end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 <%= local({
                       colindex <- 0
                       linegrandtot <- ''
                       linetot      <- ''
                       for(DESC_I in DESC) {
                         colindex <- colindex + 1
                         clausefirst  <- paste0(', ', SQuote(attr(DESC_I, 'label')),'::text col', colindex, '_fct', collapse = '')
                         colindex <- colindex + 1
                         clausesecond <- paste0(', ', DESC_I, ' col', colindex, '_fct\n', collapse = '')
                         linetot      <- paste0(clausefirst, clausesecond, collapse = '')
                         linegrandtot <- paste0(linegrandtot, linetot, collapse = '')
                       }
                       return(linegrandtot)
                     }) 
                 %>
                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1, fei.last_inbnd_stmtstat_liab_q1, fei.mktcap
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') 
              -- and fei.sp in('500')
                 <%= if(WHERE != '') { paste0(WHERE) } %>
              -- and fei.dateindex = 17438 
                 <%= if(WHERE_DATEINDEX != '') { paste0(WHERE_DATEINDEX) } %>
             ) fe -- 261,000 records ( if 'sp500' and 'spnot500' )
        group by   fe.dateindex
                 <%= paste0(', fe.col', seq(length(DESC)*2), '_fct' , collapse = '') %>
        order by   fe.dateindex
                 <%= paste0(', fe.col', seq(length(DESC)*2), '_fct' , collapse = '') %>
      ) -- 354 ROWS ( if 'sp500' and 'spnot500' )
      select
        -- DISPLAY ORDER IS HERE
          fe.dateindex 
     -- NOT SENT TO LIQUIFY (ONLY MEANT FOR SINGLE QUERY INTERACTIVE DEBUGGING)
     -- , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
        <%= paste0(', fe.col', seq(length(DESC)*2), '_fct'  ) %>
        -- THE RESPONSE VALUE
        , sum(fe.mktcap) filter(where fe.mktcap is not null) / nullif(count(fe.mktcap) filter(where fe.mktcap is not null),0) / 1000.0 sum_mktcap_o_count_d1000
        -- CANDIDATE RESPONSE VARIABLES
        , avg(fe.pradchg_f52w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f52w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f52w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f52w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f26w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f26w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f26w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f26w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f13w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f13w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f13w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f13w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f04w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f04w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f04w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f04w_ann_wtd_by_mktcap_x1000
        -- BEGIN NON-RESPONSE VARIABLES
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
        , sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) * 10 avg_last_mktcap_wtd_by_assets_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)                     * 10 avg_mktcap_wtd_by_assets_q1_x10
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null) * 10 avg_last_mktcap_wtd_by_liab_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null)                     * 10 avg_mktcap_wtd_by_liab_q1_x10
        -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 100 sum_last_liab_q1_o_last_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 100 sum_last_liab_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
          (nullif(
            sum(fe.last_inbnd_stmtstat_assets_q1) 
              filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
            sum(fe.last_inbnd_stmtstat_liab_q1) 
              filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
          ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
        -- STAT / ASSETS     STAT / MKTCAP
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000

        -- ere_q1 --
        , sum(fe.last_inbnd_stmtstat_ere_q1) 
            filter(where fe.last_inbnd_stmtstat_ere_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ere_q1    is not null)    * 1000 sum_last_ere_q1_o_assets_q1_x1000
        -- ere_q1 --
        , sum(fe.last_inbnd_stmtstat_ere_q1) 
            filter(where fe.last_inbnd_stmtstat_ere_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ere_q1  is not null)    * 1000 sum_last_ere_q1_o_mktcap_x1000


        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
        -- SUM OF THE CURRENTS CA + CL
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
          ) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
          ) /
          sum(fe.mktcap) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100

         -- others --
         , sum(fe.last_inbnd_stmtstat_ce_q1) 
            filter(where fe.last_inbnd_stmtstat_ce_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ce_q1    is not null)    * 1000 sum_last_ce_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_ce_q1) 
            filter(where fe.last_inbnd_stmtstat_ce_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ce_q1  is not null)    * 1000 sum_last_ce_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_int_q1) 
            filter(where fe.last_inbnd_stmtstat_int_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_int_q1    is not null)    * 1000 sum_last_int_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_int_q1) 
            filter(where fe.last_inbnd_stmtstat_int_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_int_q1  is not null)    * 1000 sum_last_int_q1_o_mktcap_x1000
         , sum(fe.last_inbnd_stmtstat_intno_q1) 
            filter(where fe.last_inbnd_stmtstat_intno_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_intno_q1    is not null)    * 1000 sum_last_intno_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_intno_q1) 
            filter(where fe.last_inbnd_stmtstat_intno_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_intno_q1  is not null)    * 1000 sum_last_intno_q1_o_mktcap_x1000
         , sum(fe.last_inbnd_stmtstat_cash_q1) 
            filter(where fe.last_inbnd_stmtstat_cash_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cash_q1    is not null)    * 1000 sum_last_cash_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_cash_q1) 
            filter(where fe.last_inbnd_stmtstat_cash_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cash_q1  is not null)    * 1000 sum_last_cash_q1_o_mktcap_x1000
        --
         , sum(fe.last_inbnd_stmtstat_divpaid_q1) 
            filter(where fe.last_inbnd_stmtstat_divpaid_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_divpaid_q1    is not null)    * 1000 sum_last_divpaid_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_divpaid_q1) 
            filter(where fe.last_inbnd_stmtstat_divpaid_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_divpaid_q1  is not null)    * 1000 sum_last_divpaid_q1_o_mktcap_x1000
        --
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct 
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'  end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 <%= local({
                       colindex <- 0
                       linegrandtot <- ''
                       linetot      <- ''
                       for(DESC_I in DESC) {
                         colindex <- colindex + 1
                         clausefirst  <- paste0(', ', SQuote(attr(DESC_I, 'label')),'::text col', colindex, '_fct', collapse = '')
                         colindex <- colindex + 1
                         clausesecond <- paste0(', ', DESC_I, ' col', colindex, '_fct\n', collapse = '')
                         linetot      <- paste0(clausefirst, clausesecond, collapse = '')
                         linegrandtot <- paste0(linegrandtot, linetot, collapse = '')
                       }
                       return(linegrandtot)
                     }) 
                 %>
                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
                 , fei.last_inbnd_stmtstat_liab_q1
                 , fei.mktcap
                 , fei.last_inbnd_stmtstat_netinc_q1
                 , fei.last_inbnd_stmtstat_ncc_q1
                 , fei.last_inbnd_stmtstat_tco_q1
                 , fei.last_inbnd_stmtstat_tcf_q1
                 , fei.last_inbnd_stmtstat_tci_q1
                 -- ere_q1 --
                 , fei.last_inbnd_stmtstat_ere_q1
                 , fei.last_inbnd_stmtstat_ca_q1
                 , fei.last_inbnd_stmtstat_cl_q1
                 -- others --
                 , fei.last_inbnd_stmtstat_ce_q1
                 , fei.last_inbnd_stmtstat_int_q1
                 , fei.last_inbnd_stmtstat_intno_q1
                 , fei.last_inbnd_stmtstat_cash_q1
                 --
                 , fei.last_inbnd_stmtstat_divpaid_q1
                 --
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') -- 261,000 records
              -- and fei.sp in('500')
                 <%= if(WHERE != '') { paste0(WHERE) } %>
              -- and fei.dateindex = 17438
                 <%= if(WHERE_DATEINDEX != '') { paste0(WHERE_DATEINDEX) } %>
             ) fe 
           , index_weights w
      where     fe.dateindex = w.dateindex
            <%= paste0(sprintf('  and fe.%1$s = w.%2$s', paste0('col', seq(length(DESC)*2), '_fct'), paste0('col', seq(length(DESC)*2), '_fct') ), collapse = '') %>
      group by  fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
                <%= paste0(', fe.col', seq(length(DESC)*2), '_fct' , collapse = '') %>
      order by  fe.dateindex
                <%= paste0(', fe.col', seq(length(DESC)*2), '_fct' , collapse = '') %>
      ;
    ")}, envir = list2env(combo_i)
    ) -> add_columns_sql
    
    db.q(add_columns_sql, nrows = "all", conn.id = cid) -> si_all_df              # SFS
    
    financize(si_all_df, char_col_numeric_limit = 99999999999999.99) -> si_all_df # SFS
    
    if(!NROW(si_all_df)) stop(paste0("Returned zero records ", dateindex))
    
    # SFS ( I REALLY! regret programming using regular expressions )
    message(paste0("Beginning liquifyDFM"))
    liquifyDFM(si_all_df, const_cols_regexpr = "^dateindex.*", fctr_cols_rexpr = ".*_fct$") -> si_all_df
    message(paste0("Ending liquifyDFM"))
    
    # SFS
    print(dateindex);upsert2(value = si_all_df, target_table_name = "si_finecon2_aggregates", upsert_temp_perform_upsert_force = TRUE)
    
    message(paste0("Ending load_inbnd_stmtstats_division_aggregates of the following: ", long_message ))

  }
  

  options(ops)
  
  return(TRUE)
  
}
# load_inbnd_stmtstats_division_aggregates(dateindex = 17409)






create_inbnd_stmtstats_aggregates_db <- function(exact_lwd_dbf_dirs = NULL) {

  # R version 3.4.1 (2017-06-30) # sessionInfo()
  
  require(RPostgreSQL)
  require(PivotalR)
  require(stringi)
  require(stringr)
  
  ops <- options() 
  
  options(width = 10000) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width
  options(warn = 1)
  
  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  verify_connection()
  
  db.q("
    CREATE TABLE if not exists fe_data_store.inbnd_stmtstats_aggregates
    (
      dateindex integer,
      dateindexeom double precision,
      dateindex_fct text,
      is_sp_fct text,
      is_sp500_fct text,
      sector_desc_fct text,
      is_materials_fct text,
      industry_desc_fct text,
      is_gld_fct text,
      approx_price_o_mktcap_x100 numeric,
      count_now_inbnd_stmtstat_dateindex numeric,
      pct_sum_now_o_last_inbnd_stmtstat_mktcap numeric,
      rat_now_netinc_q1_o_mktcap_x_100 numeric,
      rat_now_sales_q1_o_mktcap_x_100 numeric,
      rat_now_netinc_q1_o_sales_x_100 numeric,
      unweighted_pct_freeprice_ret_01m_ann numeric,
      sum_mktcap numeric
    );
  ", conn.id = cid)
  
  as.integer(dir(getsetvar_aaii_sipro_dir())) -> all_load_days
  
  if( is.null(exact_lwd_dbf_dirs))                                dateindexes <- max(all_load_days) 
  if(!is.null(exact_lwd_dbf_dirs) && exact_lwd_dbf_dirs == 'all') dateindexes <-     all_load_days
  if(!is.null(exact_lwd_dbf_dirs) && exact_lwd_dbf_dirs != 'all') dateindexes <-     exact_lwd_dbf_dirs
  
  for(dateindex in dateindexes) {
  
    message(paste0("Beginning inbnd_stmtstats_aggregates query SQL of dateindex: ", dateindex))
    
    db.q(str_c("delete from fe_data_store.inbnd_stmtstats_aggregates where dateindex = ", dateindex,";"), conn.id = cid)
    
    db.q(str_c("insert into fe_data_store.inbnd_stmtstats_aggregates ", "
      select 
          case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then                                                                  sq2.dateindex_fct::int                                                                                      else null end dateindex
        , case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then (extract( 'epoch' from ( select date_trunc('month', to_timestamp(sq2.dateindex_fct::int * 3600 *24 )::date) +  interval '1 month' - interval '1 day' )) / ( 3600* 24 ))::int else null end dateindexeom
        , sq2.dateindex_fct
        , sq2.is_sp_fct
        , sq2.is_sp500_fct
        , sq2.sector_desc_fct
        , sq2.is_materials_fct
        , sq2.industry_desc_fct
        , sq2.is_gld_fct
        , sq2.approx_price_o_mktcap_x100
        , sq2.count_now_inbnd_stmtstat_dateindex
        , sq2.pct_sum_now_o_last_inbnd_stmtstat_mktcap
        , sq2.rat_now_netinc_q1_o_mktcap_x_100
        , sq2.rat_now_sales_q1_o_mktcap_x_100
        , sq2.rat_now_netinc_q1_o_sales_x_100
        , sq2.unweighted_pct_freeprice_ret_01m_ann
        , sq2.sum_mktcap
      from ( -- sq2
        select 
            coalesce(sq1.dateindex_fct,        'alldateindex') dateindex_fct
          , coalesce(sq1.is_sp_fct,            'allsp') is_sp_fct
          , coalesce(sq1.is_sp500_fct,         'allsp500') is_sp500_fct
          , coalesce(sq1.sector_desc_fct,      'allsector_desc') sector_desc_fct
          , coalesce(sq1.is_materials_fct,     'allmaterials') is_materials_fct
          , coalesce(sq1.industry_desc_fct,    'allindustry') industry_desc_fct
          , coalesce(sq1.is_gld_fct,           'allgld') is_gld_fct
          , sum(sq1.price)                        / nullif(sum(sq1.mktcap), 0)                    * 100.00 approx_price_o_mktcap_x100  -- also EXTERIOR DATA: e.g. just the  S&P value would have been just fine
          , count(sq1.now_inbnd_stmtid_dateindex)::numeric   count_now_inbnd_stmtstat_dateindex                                                         -- cnt           reported this month
          , sum(sq1.now_inbnd_stmtstat_mktcap)    /   nullif(sum(sq1.last_inbnd_stmtstat_mktcap), 0)  * 100.00 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- pct by mktcap reported this month
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00 rat_now_netinc_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_sales_q1)  /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_sales_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_sales_q1), 0) * 100.00 rat_now_netinc_q1_o_sales_x_100
          , avg(sq1.pct_freeprice_ret_01m_ann)                                                                 unweighted_pct_freeprice_ret_01m_ann
          , sum(sq1.mktcap)                                                                                    sum_mktcap
        from ( -- sq1
          select
              coalesce(dateindex::text, 'emptydateindex')  dateindex_fct
            , case when sp in ('500', '400','600') then 'issp'    else 'notissp'    end is_sp_fct
            , case when sp   = '500'               then 'issp500' else 'notissp500' end is_sp500_fct
            , coalesce(sector_desc,   'emptysector')     sector_desc_fct
            , case when sector_desc   = 'Basic Materials'               then 'isbasicmat' else 'notisbasicmat' end is_materials_fct
            , coalesce(industry_desc, 'emptyindustry')      industry_desc_fct
            , case when industry_desc   = 'Gold & Silver'               then 'isgld'      else 'notisgld'     end is_gld_fct
            , mktcap
            , case when price < 0.10 or price > 1000.00 then null else price end price
            , now_inbnd_stmtid_dateindex
            , last_inbnd_stmtid_dateindex
            , now_inbnd_stmtstat_mktcap
            , last_inbnd_stmtstat_mktcap
            , now_inbnd_stmtstat_netinc_q1
            , last_inbnd_stmtstat_netinc_q1
            , now_inbnd_stmtstat_sales_q1
            , last_inbnd_stmtstat_sales_q1
            , case when pct_freeprice_ret_01m_ann > 100.00 or pct_freeprice_ret_01m_ann < -100.00 then null else pct_freeprice_ret_01m_ann end pct_freeprice_ret_01m_ann
          from fe_data_store.si_finecon2 where 
          -- mktcap > 200.0 and
          -- dateindex in (17347, 17317) and
          dateindex = ", dateindex, "  -- EVERYTHING HERE 2.7 SECONDS
          ) sq1
        group by cube(dateindex_fct, is_sp_fct, is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct)
      ) sq2 where sq2.dateindex_fct not in ('emptydateindex','alldateindex')  -- (NO COST DIFFERENCE): SPEED INCREASE by LESS DATA MANIP/RETURNED
      order by dateindex_fct, is_sp_fct , is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct;
    "), conn.id = cid) -> add_data_sql
    
    message(paste0("Ending    inbnd_stmtstats_aggregates query SQL of dateindex: ", dateindex))
    
  }
    
  Sys.setenv(TZ=oldtz)
  options(ops)
  return(TRUE)
  
 }
# create_inbnd_stmtstats_aggregates_db(exact_lwd_dbf_dirs = c(17347, 17317))

            
# liquifyDF <- function(x, const_cols_regexpr = "^id", fctr_cols_rexpr = "_fct$") {
# 
#   require(magrittr)
#   require(dplyr)
#   require(tidyr)
#   require(tidyselect)
#   require(seplyr)
#   require(R.utils)
#   require(stringr)
#   require(wrapr)
#   
#   if(NROW(x) == 0) { warning("liquifyDF found zero rows"); return(x) }
#   
#   # typically "id" columns
#   const_cols <- vars_select(names(x), matches(const_cols_regexpr))
#   lside_row_1      <- select_se(x, const_cols)[1,, drop = FALSE]
#   not_l_side <-  deselect(x, const_cols)
#   
#                      # garantee no 'id' columns ( and the [rest of] factors )
#   FCT_COLS_VECTOR <- setdiff(vars_select(names(x), matches(fctr_cols_rexpr)), const_cols)
#   FCT_COLS_NAME   <- str_c(FCT_COLS_VECTOR, collapse = "__")
#   FCT_COLS_SEP    <- str_c(FCT_COLS_VECTOR, collapse = ", ")
#   
#   UNITE <- function(x) { 
#     let(list(FCT_COLS_NAME = FCT_COLS_NAME, FCT_COLS_SEP = FCT_COLS_SEP), 
#         unite(x, FCT_COLS_NAME, FCT_COLS_SEP, sep = "__")
#       , subsMethod = "stringsubs", strict = FALSE) 
#     }
#   
#   # make ONE column to represent all factors
#   not_l_side %>% UNITE %>% 
#     # change row.names to FCT_COLS_NAME, drop column 1
#     `row.names<-`({browser();.}[[1]]) %>% select(-1) %>% 
#       # to one dimension : one BIG wide ROW
#       as.matrix %>% wrap(sep = "____") -> not_l_side
#   
#   cbind(lside_row_1,as.data.frame(t(not_l_side))) -> res
#   
#   return(res)
# }

# liquifyDF(GT, const_cols_regexpr = "^dateindex.*", fctr_cols_rexpr = ".*_fct$")
#
# could?! (popularily) be called with mutliple 'interleaving' name_factor and value factors
# e.g. 
# division_name_fct
#  # sector
# division_value_fct
#  # energy
#  # materials
#  # ... etc ...
# subdivision_name_fct
#  # industry
# subdivision_value_fct
#  # oil
#  # ... etc ...
#  # gold & sliver
#  # ... etc ...
# therefore OUTPUT WOULD LOOK like
# sector_energy_industry_oil
#
# cols_fct_NA_replace default is NULL that will print 'na'. 
#   Can be any other choice
#   Popular choices could be "any" "all" "each" "none" 
#   Choice depends on the context.
# 
liquifyDF <- function(x, const_cols_regexpr = "^id", fctr_cols_rexpr = "_fct$", cols_fct_NA_replace = NULL) {

  # R version 3.4.1 (2017-06-30)
  # LATE AUG 2017

  #X# require(magrittr) # `%>%`
  
  # dplyr       select
  # tidyr       unite
  # tidyselect  vars_select matches
  # seplyr      select_se   deselect
  # R.utils     wrap
  # stringr     str_c str_replace
  # wrapr       let
  
  # NOT WORK
  # magrittr::`%>%` -> `%M%` # NOTE: DOES WORK; magrittr::`%>%` -> `%>%`
  magrittr::`%>%` -> `%>%`
  
  if(NROW(x) == 0) { message("liquifyDF found zero rows"); return(data.frame()) }
  
  # typically "id" columns
  const_cols <- tidyselect::vars_select(names(x), tidyselect::matches(const_cols_regexpr))
                     # garantee no 'id' columns ( and the [rest of] factors )
  FCT_COLS_VECTOR <- setdiff(tidyselect::vars_select(names(x), tidyselect::matches(fctr_cols_rexpr)), const_cols)
  FCT_COLS_NAME   <- stringr::str_c(FCT_COLS_VECTOR, collapse = "__")
  FCT_COLS_SEP    <- stringr::str_c(FCT_COLS_VECTOR, collapse = ", ")
  
  if(!is.null(cols_fct_NA_replace)) {
    for(coli in FCT_COLS_VECTOR) {
      x[is.na(x[, coli, drop = TRUE]),coli] <- cols_fct_NA_replace
    }
  }
  
  lside_row_1      <- seplyr::select_se(x, const_cols)[1,, drop = FALSE]
  not_l_side <-  seplyr::deselect(x, const_cols)
  
  UNITE <- function(x) { 
    wrapr::let(list(FCT_COLS_NAME = FCT_COLS_NAME, FCT_COLS_SEP = FCT_COLS_SEP), 
        tidyr::unite(x, FCT_COLS_NAME, FCT_COLS_SEP, sep = "__")
      , subsMethod = "stringsubs", strict = FALSE) 
    }
  
  # make ONE column to represent all factors
  not_l_side %>% UNITE %>% 
    # change row.names to FCT_COLS_NAME, drop column 1
    `row.names<-`(.[[1]]) %>% dplyr::select(-1) %>% 
      # to one dimension : one BIG wide ROW
      as.matrix %>% R.utils::wrap(sep = "____") -> not_l_side
  
  cbind(lside_row_1,as.data.frame(t(not_l_side))) -> res
  
  colnames(res) <- tolower(colnames(res))
  colnames(res) <- stringr::str_replace_all(colnames(res),"[ ]","_")
  colnames(res) <- stringr::str_replace_all(colnames(res),"&"  ,"and")
  
  return(res)
}

# A SIMPLE TEST

# data.frame(
    # id = c(1, 1, 1, 1),
    # id_fct = c("1", "1", "1", "1"),
  # col1_fct = c('A1', 'A1', 'A2', 'A2'),
  # col2_fct = c('B1', 'B2', 'B1', 'B2'),
  # aggr1 = c(8, 16, 32, 64),
  # aggr2 = c(10008, 10016, 10032, 10064)
# , stringsAsFactors = FALSE
# )  -> DFS

# > DFS
  # id id_fct col1_fct col2_fct aggr1 aggr2
# 1  1      1       A1       B1     8 10008
# 2  1      1       A1       B2    16 10016
# 3  1      1       A2       B1    32 10032
# 4  1      1       A2       B2    64 10064

# liquifyDF(DFS)

# > liquifyDF(DFS)
  # id id_fct A1__B1____aggr1 A1__B2____aggr1 A2__B1____aggr1 A2__B2____aggr1
# 1  1      1               8              16              32              64
  # A1__B1____aggr2 A1__B2____aggr2 A2__B1____aggr2 A2__B2____aggr2
# 1           10008           10016           10032           10064
# 
# # <NA> _fct
# data.frame(
#    id = c(1, 1, 1, 1),
#    id_fct = c("10", "10", "10", "10"),
#    col1_fct = c('A1', NA_character_  , 'A2', 'A2'),
#    col2_fct = c('B1', 'B2', 'B1', 'B2'),
#    aggr1 = c(8, 16, 32, 64),
#    aggr2 = c(10008, 10016, 10032, 10064)
#  , stringsAsFactors = FALSE
# )  -> DFSMISSNG
# 
# > DFSMISSNG
#   id id_fct col1_fct col2_fct aggr1 aggr2
# 1  1     10       A1       B1     8 10008
# 2  1     10     <NA>       B2    16 10016
# 3  1     10       A2       B1    32 10032
# 4  1     10       A2       B2    64 10064
# 
# > liquifyDF(x = DFSMISSNG, const_cols_regexpr = "^id", fctr_cols_rexpr = "_fct$", cols_fct_NA_replace = "any")
#   id id_fct a1__b1____aggr1 any__b2____aggr1 a2__b1____aggr1 a2__b2____aggr1 a1__b1____aggr2
# 1  1     10               8               16              32              64           10008
#   any__b2____aggr2 a2__b1____aggr2 a2__b2____aggr2
# 1            10016           10032           10064


# CHANGE TEST USAGE TO 
# liquifyDF(SFS,"^dateindex.*")
# BECAUSE I HAVE USED DATEINDEX IN THE MIDDLE OF THE WORD
# count_now_inbnd_stmtstat_dateindex AND rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100

# liquifyDF(GT, const_cols_regexpr = "^dateindex.*", fctr_cols_rexpr = ".*_fct$")
# > str(GT)
# 'data.frame':   8 obs. of  13 variables:
 # $ dateindex                               : int  17347 17347 17347 17347 17347 17347 17347 17347
 # $ dateindexeom                            : int  17347 17347 17347 17347 17347 17347 17347 17347
 # $ dateindex_fct                           : chr  "17347" "17347" "17347" "17347" ...
 # $ is_sp_fct                               : chr  "allsp" "allsp" "allsp" "issp" ...
 # $ is_sp500_fct                            : chr  "allsp500" "issp500" "notissp500" "allsp500" ...
 # $ approx_price_o_mktcap_x100              : num  0.518 0.214 1.414 0.381 0.214 ...
 # $ count_now_inbnd_stmtstat_dateindex      : num  254 28 226 89 28 61 165 165
 # $ pct_sum_now_o_last_inbnd_stmtstat_mktcap: num  5.16 5.29 4.75 5.3 5.29 ...
 # $ rat_now_netinc_q1_o_mktcap_x_100        : num  0.961 1.305 -0.191 1.184 1.305 ...
 # $ rat_now_sales_q1_o_mktcap_x_100         : num  18.2 15 28.7 17.3 15 ...
 # $ rat_now_netinc_q1_o_sales_x_100         : num  5.287 8.68 -0.665 6.849 8.68 ...
 # $ unweighted_pct_freeprice_ret_01m_ann    : num  0.593 1.911 0.417 1.104 1.911 ...
 # $ sum_mktcap                              : num  28113704 20998101 7115603 23323812 209
#

liquifyDFM <- function(x, const_cols_regexpr = "^id", fctr_cols_rexpr = "_fct$", cols_fct_NA_replace = NULL) {

  # R version 3.4.1 (2017-06-30)
  # LATE AUG 2017

  #X# require(magrittr) # `%>%`
  
  # dplyr       select
  # tidyr       unite
  # tidyselect  vars_select matches
  # seplyr      select_se   deselect
  # R.utils     wrap
  # stringr     str_c str_replace
  # wrapr       let
  # stringi     `%s+%`
  # data.table  rbindlist
  
  # NOT WORK
  # magrittr::`%>%` -> `%M%` # NOTE: DOES WORK; magrittr::`%>%` -> `%>%`
  magrittr::`%>%` -> `%>%`
  stringi::`%s+%` -> `%s+%`
  
  if(NROW(x) == 0) { message("liquifyDFM found zero rows"); return(data.frame()) }
  
  # typically "id" columns
  const_cols <- tidyselect::vars_select(names(x), tidyselect::matches(const_cols_regexpr))

  # multi-case
  stringr::str_c(const_cols, collapse = " %s+% \'_\' %s+% ") -> splitter_key_concat_expr
  with(x,{ eval(parse(text=splitter_key_concat_expr)) } )    -> x_splitter
  
  # non-unique const_cols_regexpr - multiple(M)
  # since I can not 'tidyr', I have to split, then tidyr each piece
  # then at the end, recombine
  si_tbl_df_all <- list()
  for(split_i in split(x, x_splitter)) {
  
    x <- split_i
    # other code left in here (instead of 'before the loop') 
    # because a possible 'future' expansion
    # would allow (at the end of the program) a list of 
    # date.frames with 'not-exact' columns to be 'merged'
    
                         # garantee no 'id' columns ( and the [rest of] factors )
    FCT_COLS_VECTOR <- setdiff(tidyselect::vars_select(names(x), tidyselect::matches(fctr_cols_rexpr)), const_cols)
    FCT_COLS_NAME   <- stringr::str_c(FCT_COLS_VECTOR, collapse = "__")
    FCT_COLS_SEP    <- stringr::str_c(FCT_COLS_VECTOR, collapse = ", ")
    
    if(!is.null(cols_fct_NA_replace)) {
      for(coli in FCT_COLS_VECTOR) {
        x[is.na(x[, coli, drop = TRUE]),coli] <- cols_fct_NA_replace
      }
    }
    
    lside_row_1      <- seplyr::select_se(x, const_cols)[1,, drop = FALSE]
    not_l_side <-  seplyr::deselect(x, const_cols)
    
    UNITE <- function(x) { 
      wrapr::let(list(FCT_COLS_NAME = FCT_COLS_NAME, FCT_COLS_SEP = FCT_COLS_SEP), 
          tidyr::unite(x, FCT_COLS_NAME, FCT_COLS_SEP, sep = "__")
        , subsMethod = "stringsubs", strict = FALSE) 
      }
    
    # make ONE column to represent all factors
    not_l_side %>% UNITE %>% 
      # change row.names to FCT_COLS_NAME, drop column 1
      `row.names<-`(.[[1]]) %>% dplyr::select(-1) %>% 
        # to one dimension : one BIG wide ROW
        as.matrix %>% R.utils::wrap(sep = "____") -> not_l_side
    
    cbind(lside_row_1,as.data.frame(t(not_l_side))) -> res
    
    colnames(res) <- tolower(colnames(res))
    colnames(res) <- stringr::str_replace_all(colnames(res),"[ ]","_")
    colnames(res) <- stringr::str_replace_all(colnames(res),"&"  ,"and")
    
    si_tbl_df <- res
    si_tbl_df_all <- c(si_tbl_df_all,list(si_tbl_df))
    
  }
  
  si_tbl_df_all <- data.frame(data.table::rbindlist(si_tbl_df_all))
  return(si_tbl_df_all)
  
}
# 
# data.frame(
# id = c(1, 1, 1, 1, 2, 2, 2, 2),
# id_fct = c("1", "1", "1", "1", "1", "1", "1", "1"),
# col1_fct = c('A1', 'A1', 'A2', 'A2','A1', 'A1', 'A2', 'A2'),
# col2_fct = c('B1', 'B2', 'B1', 'B2','B1', 'B2', 'B1', 'B2'),
# aggr1 = c(8, 16, 32, 64, 208, 216, 232, 264),
# aggr2 = c(10008, 10016, 10032, 10064, 20008, 20016, 20032, 20064)
# , stringsAsFactors = FALSE
# 
# )  -> DFSM
# > liquifyDFM(DFSM)
#   id id_fct a1__b1____aggr1 a1__b2____aggr1 a2__b1____aggr1 a2__b2____aggr1 a1__b1____aggr2 a1__b2____aggr2
# 1  1      1               8              16              32              64           10008           10016
# 2  2      1             208             216             232             264           20008           20016
#   a2__b1____aggr2 a2__b2____aggr2
# 1           10032           10064
# 2           20032           20064



# COPY FROM TEMPORARY
            
# dbGetQuery(con, "

      # select 
          # case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then                                                                  sq2.dateindex_fct::int                                                                                      else null end dateindex
        # , case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then (extract( 'epoch' from ( select date_trunc('month', to_timestamp(sq2.dateindex_fct::int * 3600 *24 )::date) +  interval '1 month' - interval '1 day' )) / ( 3600* 24 ))::int else null end dateindexeom
        # , sq2.dateindex_fct
        # , sq2.is_sp_fct
        # , sq2.is_sp500_fct
        # , sq2.approx_price_o_mktcap_x100
        # , sq2.count_now_inbnd_stmtstat_dateindex
        # , sq2.pct_sum_now_o_last_inbnd_stmtstat_mktcap
        # , sq2.rat_now_netinc_q1_o_mktcap_x_100
        # , sq2.rat_now_sales_q1_o_mktcap_x_100
        # , sq2.rat_now_netinc_q1_o_sales_x_100
        # , sq2.unweighted_pct_freeprice_ret_01m_ann
        # , sq2.sum_mktcap
      # from ( -- sq2
        # select 
            # coalesce(sq1.dateindex_fct,        'alldateindex') dateindex_fct
          # , coalesce(sq1.is_sp_fct,            'allsp') is_sp_fct
          # , coalesce(sq1.is_sp500_fct,         'allsp500') is_sp500_fct
          # , sum(sq1.price)                        / nullif(sum(sq1.mktcap), 0)                    * 100.00 approx_price_o_mktcap_x100  -- also EXTERIOR DATA: e.g. just the  S&P value would have been just fine
          # , count(sq1.now_inbnd_stmtid_dateindex)::numeric   count_now_inbnd_stmtstat_dateindex                                                         -- cnt           reported this month
          # , sum(sq1.now_inbnd_stmtstat_mktcap)    /   nullif(sum(sq1.last_inbnd_stmtstat_mktcap), 0)  * 100.00 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- pct by mktcap reported this month
          # , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00 rat_now_netinc_q1_o_mktcap_x_100
          # , sum(sq1.now_inbnd_stmtstat_sales_q1)  /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_sales_q1_o_mktcap_x_100
          # , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_sales_q1), 0) * 100.00 rat_now_netinc_q1_o_sales_x_100
          # , avg(sq1.pct_freeprice_ret_01m_ann)                                                                 unweighted_pct_freeprice_ret_01m_ann
          # , sum(sq1.mktcap)                                                                                    sum_mktcap
        # from ( -- sq1
          # select
              # coalesce(dateindex::text, 'emptydateindex')  dateindex_fct
            # , case when sp in ('500', '400','600') then 'issp'    else 'notissp'    end is_sp_fct
            # , case when sp   = '500'               then 'issp500' else 'notissp500' end is_sp500_fct
            # , mktcap
            # , case when price < 0.10 or price > 1000.00 then null else price end price
            # , now_inbnd_stmtid_dateindex
            # , last_inbnd_stmtid_dateindex
            # , now_inbnd_stmtstat_mktcap
            # , last_inbnd_stmtstat_mktcap
            # , now_inbnd_stmtstat_netinc_q1
            # , last_inbnd_stmtstat_netinc_q1
            # , now_inbnd_stmtstat_sales_q1
            # , last_inbnd_stmtstat_sales_q1
            # , case when pct_freeprice_ret_01m_ann > 100.00 or pct_freeprice_ret_01m_ann < -100.00 then null else pct_freeprice_ret_01m_ann end pct_freeprice_ret_01m_ann
          # from fe_data_store.si_finecon2 where adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
          # -- AND mktcap > 200.0
          # -- and dateindex in (17347, 17317) 
          # and dateindex = 17347  -- EVERYTHING HERE 2.7 SECONDS
          # ) sq1
        # group by cube(dateindex_fct, is_sp_fct, is_sp500_fct)
      # ) sq2 where sq2.dateindex_fct not in ('emptydateindex','alldateindex')  -- (NO COST DIFFERENCE): SPEED INCREASE by LESS DATA MANIP/RETURNED
      # order by dateindex_fct, is_sp_fct , is_sp500_fct

# ") -> GT

 # dateindex | dateindexeom | dateindex_fct | is_sp_fct | is_sp500_fct | approx_price_o_mktcap_x100 | count_now_inbnd_stmtstat_dateindex | pct_sum_now_o_last_inbnd_stmtstat_mktcap | rat_now_netinc_q1_o_mktcap_x_100 | rat_now_sales_q1_o_mktcap_x_100 | rat_now_netinc_q1_o_sales_x_100 | unweighted_pct_freeprice_ret_01m_ann | sum_mktcap
# -----------+--------------+---------------+-----------+--------------+----------------------------+------------------------------------+------------------------------------------+----------------------------------+---------------------------------+---------------------------------+--------------------------------------+-------------
     # 17347 |        17347 | 17347         | allsp     | allsp500     |   0.5175184990386289970000 |                                254 |                 5.1559631335096084330000 |         0.9613114328189371820000 |       18.1823833047186013600000 |        5.2870485497325448940000 |               0.59277805611222444890 | 28113704.20
     # 17347 |        17347 | 17347         | allsp     | issp500      |   0.2136159816202809800000 |                                 28 |                 5.2907328952632605760000 |         1.3054309464344947210000 |       15.0395397952483461170000 |        8.6799926341292565890000 |                   1.9113617021276596 | 20998101.20
     # 17347 |        17347 | 17347         | allsp     | notissp500   |   1.4143329806342484260000 |                                226 |                 4.7508099140242344410000 |        -0.1907730216161953450000 |       28.7043705990169856470000 |       -0.6646131499665371080000 |               0.41681714934696195344 |  7115603.00
     # 17347 |        17347 | 17347         | issp      | allsp500     |   0.3813841044745876780000 |                                 89 |                 5.3032500109766440120000 |         1.1842060443399664620000 |       17.2896553871473166920000 |        6.8492171638092604970000 |                   1.1041786743515850 | 23323811.60
     # 17347 |        17347 | 17347         | issp      | issp500      |   0.2136159816202809800000 |                                 28 |                 5.2907328952632605760000 |         1.3054309464344947210000 |       15.0395397952483461170000 |        8.6799926341292565890000 |                   1.9113617021276596 | 20998101.20
     # 17347 |        17347 | 17347         | issp      | notissp500   |   1.8961092490277379330000 |                                 61 |                 5.4163613371108738430000 |         0.1141614525746643060000 |       37.1512842963692024520000 |        0.3072880379153441880000 |               0.69091503267973856209 |  2325710.40
     # 17347 |        17347 | 17347         | notissp   | allsp500     |   1.1804087214815630730000 |                                165 |                 4.4190135357192741760000 |        -0.3771013331081302070000 |       23.5429365869327187760000 |       -1.6017599661608768310000 |               0.32018817204301075269 |  4789892.60
     # 17347 |        17347 | 17347         | notissp   | notissp500   |   1.1804087214815630730000 |                                165 |                 4.4190135357192741760000 |        -0.3771013331081302070000 |       23.5429365869327187760000 |       -1.6017599661608768310000 |               0.32018817204301075269 |  4789892.60
# (8 rows)
# -- SELECT is__ THEN all* ACRODSS THE REST OF THE FACTORS

# # GT 

# str(  liquifyDF(GT, const_cols_regexpr = "^dateindex.*", fctr_cols_rexpr = ".*_fct$") )

# 'data.frame':   1 obs. of  67 variables:
 # $ dateindex                                                      : int 17347
 # $ dateindexeom                                                   : int 17347
 # $ dateindex_fct                                                  : chr "17347"
 # $ allsp__allsp500____approx_price_o_mktcap_x100                  : num 0.518
 # $ allsp__issp500____approx_price_o_mktcap_x100                   : num 0.214
 # $ allsp__notissp500____approx_price_o_mktcap_x100                : num 1.41
 # $ issp__allsp500____approx_price_o_mktcap_x100                   : num 0.381
 # $ issp__issp500____approx_price_o_mktcap_x100                    : num 0.214
 # $ issp__notissp500____approx_price_o_mktcap_x100                 : num 1.9
 # $ notissp__allsp500____approx_price_o_mktcap_x100                : num 1.18
 # $ notissp__notissp500____approx_price_o_mktcap_x100              : num 1.18
 # $ allsp__allsp500____count_now_inbnd_stmtstat_dateindex          : num 254
 # $ allsp__issp500____count_now_inbnd_stmtstat_dateindex           : num 28
 # $ allsp__notissp500____count_now_inbnd_stmtstat_dateindex        : num 226
 # $ issp__allsp500____count_now_inbnd_stmtstat_dateindex           : num 89
 # $ issp__issp500____count_now_inbnd_stmtstat_dateindex            : num 28
 # $ issp__notissp500____count_now_inbnd_stmtstat_dateindex         : num 61
 # $ notissp__allsp500____count_now_inbnd_stmtstat_dateindex        : num 165
 # $ notissp__notissp500____count_now_inbnd_stmtstat_dateindex      : num 165
 # $ allsp__allsp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap    : num 5.16
 # $ allsp__issp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap     : num 5.29
 # $ allsp__notissp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap  : num 4.75
 # $ issp__allsp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap     : num 5.3
 # $ issp__issp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap      : num 5.29
 # $ issp__notissp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap   : num 5.42
 # $ notissp__allsp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap  : num 4.42
 # $ notissp__notissp500____pct_sum_now_o_last_inbnd_stmtstat_mktcap: num 4.42
 # $ allsp__allsp500____rat_now_netinc_q1_o_mktcap_x_100            : num 0.961
 # $ allsp__issp500____rat_now_netinc_q1_o_mktcap_x_100             : num 1.31
 # $ allsp__notissp500____rat_now_netinc_q1_o_mktcap_x_100          : num -0.191
 # $ issp__allsp500____rat_now_netinc_q1_o_mktcap_x_100             : num 1.18
 # $ issp__issp500____rat_now_netinc_q1_o_mktcap_x_100              : num 1.31
 # $ issp__notissp500____rat_now_netinc_q1_o_mktcap_x_100           : num 0.114
 # $ notissp__allsp500____rat_now_netinc_q1_o_mktcap_x_100          : num -0.377
 # $ notissp__notissp500____rat_now_netinc_q1_o_mktcap_x_100        : num -0.377
 # $ allsp__allsp500____rat_now_sales_q1_o_mktcap_x_100             : num 18.2
 # $ allsp__issp500____rat_now_sales_q1_o_mktcap_x_100              : num 15
 # $ allsp__notissp500____rat_now_sales_q1_o_mktcap_x_100           : num 28.7
 # $ issp__allsp500____rat_now_sales_q1_o_mktcap_x_100              : num 17.3
 # $ issp__issp500____rat_now_sales_q1_o_mktcap_x_100               : num 15
 # $ issp__notissp500____rat_now_sales_q1_o_mktcap_x_100            : num 37.2
 # $ notissp__allsp500____rat_now_sales_q1_o_mktcap_x_100           : num 23.5
 # $ notissp__notissp500____rat_now_sales_q1_o_mktcap_x_100         : num 23.5
 # $ allsp__allsp500____rat_now_netinc_q1_o_sales_x_100             : num 5.29
 # $ allsp__issp500____rat_now_netinc_q1_o_sales_x_100              : num 8.68
 # $ allsp__notissp500____rat_now_netinc_q1_o_sales_x_100           : num -0.665
 # $ issp__allsp500____rat_now_netinc_q1_o_sales_x_100              : num 6.85
 # $ issp__issp500____rat_now_netinc_q1_o_sales_x_100               : num 8.68
 # $ issp__notissp500____rat_now_netinc_q1_o_sales_x_100            : num 0.307
 # $ notissp__allsp500____rat_now_netinc_q1_o_sales_x_100           : num -1.6
 # $ notissp__notissp500____rat_now_netinc_q1_o_sales_x_100         : num -1.6
 # $ allsp__allsp500____unweighted_pct_freeprice_ret_01m_ann        : num 0.593
 # $ allsp__issp500____unweighted_pct_freeprice_ret_01m_ann         : num 1.91
 # $ allsp__notissp500____unweighted_pct_freeprice_ret_01m_ann      : num 0.417
 # $ issp__allsp500____unweighted_pct_freeprice_ret_01m_ann         : num 1.1
 # $ issp__issp500____unweighted_pct_freeprice_ret_01m_ann          : num 1.91
 # $ issp__notissp500____unweighted_pct_freeprice_ret_01m_ann       : num 0.691
 # $ notissp__allsp500____unweighted_pct_freeprice_ret_01m_ann      : num 0.32
 # $ notissp__notissp500____unweighted_pct_freeprice_ret_01m_ann    : num 0.32
 # $ allsp__allsp500____sum_mktcap                                  : num 28113704
 # $ allsp__issp500____sum_mktcap                                   : num 20998101
 # $ allsp__notissp500____sum_mktcap                                : num 7115603
 # $ issp__allsp500____sum_mktcap                                   : num 23323812
 # $ issp__issp500____sum_mktcap                                    : num 20998101
 # $ issp__notissp500____sum_mktcap                                 : num 2325710
 # $ notissp__allsp500____sum_mktcap                                : num 4789893
 # $ notissp__notissp500____sum_mktcap                              : num 4789893

# LEFT_OFF 
# [x] NEXT, NEED PARAMETERS TO THE 'create' SQL FUNCTION TO GENERATE LESS COMPLEX FACTOR COMBINATIONS
# [ ] inbound statement loader ?? # does it NEED original* 
# DOES IT NEED TO overwrite as NEW CURRENT data COMES in?
# NEXTER need UPSERT generator
# [x] NEED A PERMANET PER COMPANY BALANCE OF MKTCAP PER 'WHAT ITEMS'? 
#  DECIDE WHAT IEMS PER SP/SP500/SECTOR(BASIC MATERIALS,ENERYGY)/INDUSTRY(GOLD & SILVER)

# 
# LOOP(in R) or LATERAL(in SQL) over all dateindexes
# ----------------------------------------------------
# 
# dbGetQuery(con,"
# select
#     dateindex 
#   , dateindexlwd
#   , dateindexeom
#   , dateindexeom::text dateindexeom_fct
#   , 'sector_desc'::text collection_name_fct
#   , sector_desc sector_desc_fct
#   , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1 
#   , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
# from si_finecon2 where sector_desc in ('Energy','Basic Materials') and dateindexeom = 17378
# group by dateindex, dateindexlwd, dateindexeom, sector_desc
# order by dateindex, dateindexlwd, dateindexeom, sector_desc
# ;") -> SFS
# 
# > SFS

#   dateindex dateindexlwd dateindexeom dateindexeom_fct collection_name_fct sector_desc_fct sum_now_inbnd_stmtstat_netinc_q1 sum_now_inbnd_stmtstat_mktcap
# 1     17378        17378        17378            17378         sector_desc Basic Materials                          17503.1                       1002512
# 2     17378        17378        17378            17378         sector_desc          Energy                          12384.8                       1720491

# 
# > liquifyDF(SFS,"dateindex.*") -> SFS

#   dateindex dateindexlwd dateindexeom dateindexeom_fct sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1
# 1     17378        17378        17378            17378                                                          17503.1
#   sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1 sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap
# 1                                                 12384.8                                                       1002512
#   sector_desc__energy____sum_now_inbnd_stmtstat_mktcap
# 1                                              1720491

# OTHER sensitive INDUSTRY  "Household Furniture Manufacturing"


# con RPostgreSQLConnection
# schema_name PostgreSQL collection name
# table_name  PostgreSQL schema spreadsheet
# column_name PostgreSQL table field [OPTIONAL] ... otherwise will show all spreadsheet fields
# is_temporary_table     TRUE/FALSE - is or is not or not a temporary table
#
# if a temporary table, just provide the table_name ( PostgreSQL will find the schema_name )
# 
pgListTableColumns2 <- function(con, schema_name = NULL, table_name = NULL, column_name = NULL, is_temporary_table = FALSE) {

  # stringr str_c
  # RPostgreSQLConnection dbGetQuery

  # information_schema.columns
  #
  # Select datatype of the field in postgres
  # https://stackoverflow.com/questions/2146705/select-datatype-of-the-field-in-postgres
  #
  # query to identify all data types used in postgresql database tables
  # https://dba.stackexchange.com/questions/29901/query-to-identify-all-data-types-used-in-postgresql-database-tables?rq=1

  # pg_attribute
  # 
  # Query to return output column names and data types of a query, table or view
  # https://dba.stackexchange.com/questions/75015/query-to-return-output-column-names-and-data-types-of-a-query-table-or-view
  #
  # How do I list all columns for a specified table
  # https://dba.stackexchange.com/questions/22362/how-do-i-list-all-columns-for-a-specified-table?noredirect=1&lq=1

  # many pg_* join
  # Query the schema details of a table in PostgreSQL?
  # https://stackoverflow.com/questions/4336259/query-the-schema-details-of-a-table-in-postgresql

  # pg_attribute
  # https://www.postgresql.org/docs/9.6/static/catalog-pg-attribute.html
  # pg_class
  # https://www.postgresql.org/docs/9.6/static/catalog-pg-class.html
  # pg_namespace
  # https://www.postgresql.org/docs/9.6/static/catalog-pg-namespace.html

  # [ ]COULD? IMPROVE THIS TO RETURN A 'ZERO RECORD RESULT SET'
  #   [ ] ALSO, SAME WITH TEMP
  # Also note that the cast to regclass resolves the table name somewhat 
  # intelligently according to the current search_path. ( SO I AM EXPLICIT )
  # It also raises an exception if the name is not valid. Details:

  # Re: How to get schema name in which TEMPORARY table is created?
  # https://www.postgresql.org/message-id/201103081518.34461.jens.wilke%40affinitas.de
  
  # I simply joined by schema_name, table_name, column_name, and ordinal_position

  # uses
  # RPostgreSQL dbGetQuery dbSendQuery dbColumnInfo dbClearResult
  # plyr        join
  
  if(is_temporary_table) {
  
    as.vector(unlist(
      RPostgreSQL::dbGetQuery(con, stringr::str_c(
        "  select n.nspname 
            from pg_class c join pg_namespace n on n.oid=c.relnamespace 
          where c.relname ='", table_name, "' and n.nspname like 'pg_temp%';
        ")
      ))) -> schema_name
  
    if(is.null(schema_name)) { 
      message(stringr::str_c("pgListTableColumns2 can not find TEMPORARY table_name ", table_name, " (w/wo the column_name)"))
      return(data.frame()) 
    }
        
  }
  
  if(is.null(schema_name) || is.null(table_name)) { 
    stop("pgListTableColumns2 needs a schema_name and a table_name w/wo a column_name") 
  }

  if(is.null(column_name)) {
    column_name_search_clause <- "" 
  } else {
    column_name_search_clause <- stringr::str_c("and    pg_catalog.pg_attribute.attname = '", column_name, "'")
  } 

  # I do not want an ERROR to stop the program
  #
  # table check if exists
  schema_and_table_exist <- (function() {tryCatch({NROW(suppressMessages(suppressWarnings(RPostgreSQL::dbGetQuery(con, stringr::str_c("select ('", schema_name, "' || '.' || '", table_name, "' )::regclass;"))))) }, error = function(e) { TRUE })})()
  if(schema_and_table_exist) { # both exist with each other
    # column check if exists
    if(!is.null(column_name)){ # a column sent in by the caller
      column_exists <- RPostgreSQL::dbGetQuery(con, stringr::str_c("select count(1) from information_schema.columns where table_schema = '", schema_name, "' and table_name = '", table_name, "' and column_name = '",column_name,"';"))[1,1]
      if(column_exists) {
        pg_catalog.pg_attribute.attrelid_phrase <- stringr::str_c("pg_catalog.pg_attribute.attrelid = ('", schema_name,"' || '.' || '", table_name,"')::regclass")
      } else {
        # pg_catalog.pg_attribute.attrelid_phrase <- "'f'"
        message(stringr::str_c("In table ", table_name," column ", column_name," is not found."))
        return(data.frame())
      }
    } else { # no column sent in by the caller
      pg_catalog.pg_attribute.attrelid_phrase <- stringr::str_c("pg_catalog.pg_attribute.attrelid = ('", schema_name,"' || '.' || '", table_name,"')::regclass")
    }
  } else { # both 'table_and_schema' do not exist and/or each/any does not exist with each other
    # pg_catalog.pg_attribute.attrelid_phrase <- "'f'"
    message(stringr::str_c("schema ", schema_name," and/or/xor table ", table_name," is not found [together]"))
    return(data.frame())
  }
  
  RPostgreSQL::dbGetQuery(con, stringr::str_c("
    select 
        information_schema.columns.udt_catalog
      , information_schema.columns.udt_schema
      , information_schema.columns.table_schema
      , information_schema.columns.table_name
      , information_schema.columns.column_name
      , information_schema.columns.ordinal_position
      , information_schema.columns.udt_name
      , information_schema.columns.data_type
      , information_schema.columns.character_maximum_length
      , information_schema.columns.numeric_precision
      , information_schema.columns.numeric_scale
      , information_schema.columns.datetime_precision
      , format_type(pg_catalog.pg_attribute.atttypid, pg_catalog.pg_attribute.atttypmod) as format_type_type
      , (case 
          when information_schema.columns.domain_name is not null then information_schema.columns.domain_name
          when information_schema.columns.data_type='character varying' then 'varchar('||information_schema.columns.character_maximum_length||')'
          when information_schema.columns.data_type='numeric' then 'numeric('||information_schema.columns.numeric_precision||','||information_schema.columns.numeric_scale||')'
        else 
          information_schema.columns.data_type
        end)::text as info_schema_type
      , information_schema.columns.is_nullable
      , information_schema.columns.column_default
      , information_schema.columns.is_identity
    from information_schema.columns, pg_catalog.pg_attribute, pg_catalog.pg_class, pg_catalog.pg_namespace
    where information_schema.columns.table_schema = '", schema_name, "' and information_schema.columns.table_name = '", table_name,"'
    and    pg_catalog.pg_attribute.attrelid = pg_catalog.pg_class.oid and pg_catalog.pg_class.relnamespace = pg_catalog.pg_namespace.oid
    and    ", pg_catalog.pg_attribute.attrelid_phrase, "
    ",  column_name_search_clause, "
    and    pg_catalog.pg_attribute.attnum > 0
    and    not pg_catalog.pg_attribute.attisdropped
    and    information_schema.columns.table_schema     = pg_catalog.pg_namespace.nspname and 
           information_schema.columns.table_name       = pg_catalog.pg_attribute.attrelid::regclass::text and
           information_schema.columns.column_name      = pg_catalog.pg_attribute.attname and
           information_schema.columns.ordinal_position = pg_catalog.pg_attribute.attnum
    order  by pg_catalog.pg_attribute.attnum;
  ")) -> result
  
  # of no rows then NOTHING
  final_result <- data.frame()
  
  # if regClass table and 'column exists'
  if(NROW(result)) {
    if(is.null(column_name)){
      rec <- RPostgreSQL::dbSendQuery(con, stringr::str_c("select    *                                      from ", dbQuoteIdentifier(con, schema_name), ".", dbQuoteIdentifier(con, table_name), ";"))
    } else {
      rec <- RPostgreSQL::dbSendQuery(con, stringr::str_c("select ", dbQuoteIdentifier(con, column_name), " from ", dbQuoteIdentifier(con, schema_name), ".", dbQuoteIdentifier(con, table_name), ";"))
    }
    # dispatch on RPostgreSQL
    db.col.info <- RPostgreSQL::dbColumnInfo(rec)
    RPostgreSQL::dbClearResult(rec)
    db.col.info_result <- cbind(data.frame(table_schema=schema_name, table_name=table_name, ordinal_position=seq_along(row.names(db.col.info))),db.col.info)
    
    # column renaming
    colnames(db.col.info_result)[colnames(db.col.info_result) == "name"] <- "column_name"
    non_id_cols <- grep("^table_schema$|^table_name$|^column_name$|^ordinal_position$", colnames(db.col.info_result), invert = TRUE)
    colnames(db.col.info_result)[non_id_cols] <- paste0("db_col_info_", colnames( db.col.info_result))[non_id_cols]
    
    # with above, merge() does not keep the order
    final_result <- plyr::join(result, db.col.info_result, by = c("table_schema", "table_name", "column_name", "ordinal_position"))
    
  } 
  
  return(final_result)

}

# conp <- dbConnect(dbDriver("PostgreSQL"), dbname = "finance_econ",user = "postgres", password = "postgres")
# dbWriteTable(conp, mtcars, "mtcars")
# pgListTableColumns2(conp, schema_name = 'public', table_name = 'mtcars', column_name = 'mpg')
# pgListTableColumns2(conp, schema_name = 'public', table_name = 'mtcars'                     )
  # expect printed 'error'
  #   pgListTableColumns2(conp, schema_name = 'public', table_name = 'mtcarsXX'                     )
  #   pgListTableColumns2(conp, schema_name = 'publicXX', table_name = 'mtcars'                     )
  # data frame with 0 columns and 0 rows
# dbGetQuery(conp, "create temporary table temp_xxx(xx int);")
# pgListTableColumns2(conp,                         table_name = 'temp_xxx'                   , is_temporary_table = TRUE)
  # expect printed error
  #   pgListTableColumns2(conp,                         table_name = 'temp_xxxXXX'                   , is_temporary_table = TRUE)
  # data frame with 0 columns and 0 rows
# dbGetQuery(conp, "drop table temp_xxx;")
# Quote Check
# dbWriteTable(conp, "iris", iris)
# pgListTableColumns2(conp, schema_name = 'public', table_name = 'iris'                              )
# pgListTableColumns2(conp, schema_name = 'public', table_name = 'iris', column_name = 'Petal.Length')
# 
## PostgreSQL specific: do not have Upper/mixed table names ( ::regclass does NOT support: drops table_name to lower case )
## pgListTableColumns2(conp, schema_name = 'public', table_name = 'Iris'                              )
## select ('public' || '.' || 'Iris' )::regclass;
## "public.iris"
# dbDisconnect(conp)



# DECIDED that THESE functions will process ONLY one DATEINDEX at a time
# 
# verify_company_details(dateindex = c(15155),  table_f = "si_psd", cnames_e = "^mktcap$") -> si_all_g_df
# ... X ( does not have a ticker ) update_from_future_new_company_ids(df = si_all_g_df, ref = 15155) -> si_all_g_df  ... x
# ... upsert(si_all_g_df, keys = c("company_id")) 
# 

# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid")))
# debugSource('W:/R-3.4._/finecon01.R')
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

upload_us_gov_bonds_to_db <- function(months_only_back = NULL) {

  # load_us_bond_instruments
  for_bonds_is_null_months_only_back_check_NOT_done <- TRUE

  # WARNING: NOT 'dir_i TIME by database BASED' ( SHOULD REWRITE? IF POSSIBLE? )
  # NOTE: IF missed *MANY* months in LOADING cheaper to REBUILD the entire DATABASE
  if(for_bonds_is_null_months_only_back_check_NOT_done && !is.null(months_only_back)) {
    for_bonds_is_null_months_only_back_check_NOT_done <- FALSE
    # NOTE:US BONDS SOME GAPS DO EXIST IN THE DATA ( TO DO [ ] DETECT NULL AND APPROXIMATION [ ]
    load_us_bond_instruments(us_bonds_year_back = (months_only_back %/% 12 + 2) ) # MIMIMUM OF 2 YEARS OF DATA
  }

  if(for_bonds_is_null_months_only_back_check_NOT_done && is.null(months_only_back)) {
    for_bonds_is_null_months_only_back_check_NOT_done <- FALSE
    # NOTE: US BONDS SOME GAPS DO EXIST IN THE DATA ( TO DO [ ] DETECT NULL AND APPROXIMATION [ ]
    load_us_bond_instruments() # ALL OF the data
  }

}
# upload_us_gov_bonds_to_db() # all of the data
# upload_us_gov_bonds_to_db(months_only_back = 13) # recent data



vacuum_analyze_reindex <- function(index = NULL) {

  release_connection()
  verify_connection()

  require(PivotalR)

  # VACUUM cannot be executed from a function or multi-command string
  # db.q("select 1;" , conn.id = cid)
  
  message(" ")
  
  message("Begin vacuum analyze verbose")
  message("Begin vacuum analyze verbose")
  message("Begin vacuum analyze verbose")
  
  message(" ")
  
  db.q("vacuum analyze verbose fe_data_store.si_finecon2;" , conn.id = cid)
  
  message(" ")
  
  message("End   vacuum analyze verbose")
  message("End   vacuum analyze verbose")
  message("End   vacuum analyze verbose")
  
  message(" ")
  
  message("Begin reindex (verbose)")
  message("Begin reindex (verbose)")
  message("Begin reindex (verbose)")
  
  message(" ")
  
  if(is.null(index)){
    db.q(       "reindex (verbose) table fe_data_store.si_finecon2;", conn.id = cid)
  } else {
    # reindex (verbose) index fe_data_store.si_finecon2_dateindex_company_id_key;
    db.q(paste0("reindex (verbose) index ", index, ";"), conn.id = cid)
  }
    
  message(" ")

  message("End reindex (verbose)")
  message("End reindex (verbose)")
  message("End reindex (verbose)")

  message(" ")

  release_connection()
  verify_connection()
           
}
# vacuum_analyze_reindex()
# vacuum_analyze_reindex(index = "fe_data_store.si_finecon2_dateindex_company_id_key")


vacuum_reindex_check <- function(start_time = NULL, how_often = NULL, index = NULL) {

                              # 'last check/do time
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  require(PivotalR) # db.disconnect
  
  verify_connection()
  
  message("Begin vacuum_reindex_check checking_how_often_to/doing vacuum/reindex.")

  # before 'A' run
  now_at_secs_since_UNIX_birth <- as.numeric(Sys.time())
  
  message(paste0("                  start_time: " , start_time))
  message(paste0("                   how_often: " , how_often))
  message(paste0("now_at_secs_since_UNIX_birth: " , now_at_secs_since_UNIX_birth))
  message(paste0("      start_time + how_often: " , start_time + how_often))
  message(paste0("      start_time + how_often < now_at_secs_since_UNIX_birth: " , start_time + how_often < now_at_secs_since_UNIX_birth))
  message(paste0("      how_often < now_at_secs_since_UNIX_birth - start_time: " , now_at_secs_since_UNIX_birth - start_time))

  if(!is.null(how_often)) { # run EVERY-SO-OFTEN
  
    if(is.null(start_time)) start_time <- now_at_secs_since_UNIX_birth
  
    if(start_time + how_often < now_at_secs_since_UNIX_birth) {
    
      message("Begin ACTUAL vacuum analyze reindex")

      # because connection gets hosed
      release_connection()
      verify_connection()
      
      vacuum_analyze_reindex(index = index)
    
      # because connection gets hosed
      release_connection()
      verify_connection()
      
       message("End   ACTUAL vacuum analyze reindex")
    
      # after the RUN
      start_time <-  as.numeric(Sys.time()) 

      Sys.setenv(TZ=oldtz)
      message("End  vacuum_reindex_check checking_how_often_to/doing vacuum/reindex.")
      return(start_time) # 'last check/do time
    
    } else { 
    
      message("  During checking how_often_to/doing vacuum/reindex: no vacuum/reindex needed.")
      message(paste0("    Time passed only: ", now_at_secs_since_UNIX_birth - start_time, " seconds."))
    
      Sys.setenv(TZ=oldtz)
      message("End  vacuum_reindex_check checking_how_often_to/doing vacuum/reindex.")
      return(start_time) # 'last check/do time
    
    }
    
  } else { # is.null(how_often) # run NOW
  
    message("  During checking how_often_to/doing vacuum/reindex: BEGIN doing vacuum/reindex NOW on demand.")
    
    # because connection gets hosed
    release_connection()
    verify_connection()
    
    vacuum_analyze_reindex(index = index)
  
    # because connection gets hosed
    release_connection()
    verify_connection()
    
    message("  During checking how_often_to/doing vacuum/reindex: END doing vacuum/reindex NOW on demand.")
    
    # after the RUN
    start_time <-  as.numeric(Sys.time()) 

    Sys.setenv(TZ=oldtz)
    message("End  vacuum_reindex_check checking_how_often_to/doing vacuum/reindex.")
    return(start_time) # 'last check/do time
    
  }
  
}
# ret <- vacuum_reindex_check()



# DEBUGGING FUNCTION ONLY
vacuum_reindex_check_time_looper_checker <- function(vacuum_reindex_every_x_seconds=10) {

  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  start_at_secs_since_UNIX_birth <- as.numeric(Sys.time())

  for(dir_i in 1:6) {
  
    Sys.sleep(4)
    vacuum_reindex_check(start_time = start_at_secs_since_UNIX_birth, how_often = vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth
  
  }

}
# vacuum_reindex_check_time_looper_checker()


# PROB OBSOLETE NOW
# 
# local({
#   # vacuum_reindex_every_x_seconds=1200
#   vacuum_reindex_every_x_seconds=2
#   # just NOW
#   print("vacuum_reindex_check() -> start_at_secs_since_UNIX_birth")
#   vacuum_reindex_check() -> start_at_secs_since_UNIX_birth
#   # BAD 'DIRECT' FORM: # ALWAYS WILL BE SKIPPED # start_time <- NOW # Time passed only: 0 seconds.
#   # HAS 'some' 'DEEP NESTED PROGRAMMATIC' USEFULNESS
#   print("vacuum_reindex_check(how_often = vacuum_reindex_every_x_seconds) -> start_at_secs_since_UNIX_birth")
#   vacuum_reindex_check(how_often = vacuum_reindex_every_x_seconds) -> start_at_secs_since_UNIX_birth
#   start_at_secs_since_UNIX_birth <- as.numeric(Sys.time()) 
#   print("vacuum_reindex_check(start_at_secs_since_UNIX_birth) ->  start_at_secs_since_UNIX_birth")
#   vacuum_reindex_check(start_at_secs_since_UNIX_birth) ->  start_at_secs_since_UNIX_birth
#   print("Sys.sleep(1.0);vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth")
#   Sys.sleep(1.0)
#   vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth
#   print("Sys.sleep(3.0);vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth")
#   Sys.sleep(3.0)
#   vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth
# })

#
# start_at_secs_since_UNIX_birth <- as.numeric(Sys.time()) 
# vacuum_reindex_check(start_at_secs_since_UNIX_birth, how_often = 3600) ->  start_at_secs_since_UNIX_birth



# meant for rolling 10 trillion of SP companies reporting
# this gives me a better idea of what is coming inbound withing the last 10 trilliion 
# by mktcap
# NOTE: this is NOT meant to show the FULL set
get_sipro_rolling_finstats <- function(
  dateindex_criteria = NULL,
  other_criteria = NULL,
  rolling_limit = NULL
  ) {

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
  
  message("Begin function get_sipro_rolling_finstats.")

  verify_connection()
  
  if(is.null(dateindex_criteria)) {
    dateindex_criteria <- "now.dateindex in ( select distinct dateindex from fe_data_store.si_finecon2 order by dateindex desc offset 0 limit 6 )"
  } 
  if(is.null(other_criteria)) {
    other_criteria <- "now.sp in ('500','400','600')"
  } 
  # 10 trillion
  if(is.null(rolling_limit)) {
    rolling_limit <- "10000000.00"
  } 
  
  query <- paste0("

  with dat as (
  -- all data
  select
    now.qs_date,
    now.dateindex,
    now.perend_q1,
    now.perlen_q1,
    now.pertyp_q1,
    now.company_id,
    now.ticker,
    now.mktcap,
    now.sp,
    now.company,
    now.netinc_q1,
    now.netinc_q2,
    now.liab_q1,
    now.liab_q2
  from fe_data_store.si_finecon2 now
    -- where now.dateindex in (17590, 17562, 17529, 17500, 17470, 17438)  -- dateindexes now and in the past
    -- where now.dateindex in ( select distinct dateindex from fe_data_store.si_finecon2 order by dateindex desc offset 0 limit 6 )
    where 1 = 1
     and ", dateindex_criteria, "
      -- original
      -- and now.ticker in('MSFT','AAPL')
      -- and now.ticker in('MSFT','AAPL','ORCL','IBM','HPQ')
      and ", other_criteria,"
      -- and now.sp in ('500') and sector_desc = 'Financial'
      and now.qs_date is not null -- 28 of 1500 are NULL ( and OLD )
  order by now.qs_date, now.ticker
  ), 
  -- select * from dat
  --  qs_date | dateindex | ticker |  mktcap
  -- ---------+-----------+--------+-----------
  --    17379 |     17438 | AAPL   | 791726.10
  --    17367 |     17438 | MSFT   | 568964.80
  --    17379 |     17470 | AAPL   | 873130.10
  --    17465 |     17470 | MSFT   | 641699.60
  --    17472 |     17500 | AAPL   | 870163.20
  --    17465 |     17500 | MSFT   | 642933.90
  --    17472 |     17529 | AAPL   | 878378.10
  --    17465 |     17529 | MSFT   | 661294.70
  --    17472 |     17562 | AAPL   | 857276.10
  --    17562 |     17562 | MSFT   | 715451.10
  --    17563 |     17590 | AAPL   | 905153.20
  --    17562 |     17590 | MSFT   | 725320.50
  -- (12 rows)
  fv_qs as (
    -- per every dateindex, first value ( first time) that a new qs had appeared ( in the past )
  select
    distinct
    dat.qs_date fv_qs_date,
    first_value(dat.dateindex) over (partition by dat.company_id, dat.qs_date order by dat.dateindex) fv_dateindex,
    dat.company_id fv_company_id
  from dat
  order by fv_qs_date, fv_dateindex
  ),
  -- select * from fv_qs
  --  fv_qs_date | fv_dateindex | fv_ticker
  -- ------------+--------------+-----------
  --       17367 |        17438 | MSFT
  --       17379 |        17438 | AAPL
  --       17465 |        17470 | MSFT
  --       17472 |        17500 | AAPL
  --       17562 |        17562 | MSFT
  --       17563 |        17590 | AAPL
  -- (6 rows)
  qs_dateindex_company_id as (
  -- PostgreSQL 10.0 last_value is BROKE(ignored), instead use 'first_value ... order by ... desc'
  -- for real time reporting, per ticker, last time(most recent) a *new* qs appeared 
  -- the month of the new qs is reported
  -- modifiable here
  -- advice: https://www.postgresql.org/docs/10/static/functions-window.html
  select 
    distinct
    -- last_value: written as over(... order by ... desc)
    first_value(fv_qs.fv_company_id) over (partition by fv_qs.fv_company_id order by fv_qs.fv_qs_date desc) lv_qs_company_id,
    first_value(fv_qs.fv_dateindex)  over (partition by fv_qs.fv_company_id order by fv_qs.fv_qs_date desc) lv_qs_dateindex
  from fv_qs
  order by lv_qs_dateindex, lv_qs_company_id
  ),
  --select * from qs_dateindex_ticker
  --  lv_qs_ticker | lv_qs_dateindex
  -- --------------+-----------------
  --  MSFT         |           17562
  --  AAPL         |           17590
  -- (2 rows)
  -- 
  -- combine to do some inbound statistics
  recent as(
  select dat.qs_date, dat.dateindex, dat.company_id, dat.ticker, dat.mktcap,  -- 1700000.00 -- small list of tech firms only
    sum(dat.mktcap) over (order by dat.qs_date desc)                         <= ", rolling_limit, " is_within_last_10t
  from qs_dateindex_company_id, dat
    where dat.dateindex   = qs_dateindex_company_id.lv_qs_dateindex 
      and dat.company_id  = qs_dateindex_company_id.lv_qs_company_id
  order by dat.qs_date, dat.company_id
  )
  -- select * from recent
  --  qs_date | dateindex | ticker |  mktcap   | is_within_10b
  -- ---------+-----------+--------+-----------+---------------
  --    17514 |     17529 | ORCL   | 198324.10 | f
  --    17549 |     17562 | IBM    | 151478.00 | f
  --    17562 |     17562 | MSFT   | 715451.10 | t
  --    17563 |     17590 | AAPL   | 905153.20 | t
  --    17584 |     17590 | HPQ    |  38695.80 | t
  -- (5 rows)
  -- select * from qs_dateindex_ticker
  select 
    to_timestamp(dat.qs_date*3600*24)::date   qs_date_dt,
    to_timestamp(dat.dateindex*3600*24)::date dateindex_dt,
    to_timestamp(dat.perend_q1*3600*24)::date perend_q1_dt,
    dat.perlen_q1, 
    dat.pertyp_q1,
    recent.is_within_last_10t,
    sum(dat.mktcap)    over (partition by recent.is_within_last_10t) sums_mktcap_within_last,
    sum(dat.netinc_q1) over (partition by recent.is_within_last_10t) / nullif(sum(dat.netinc_q2) over (partition by recent.is_within_last_10t),0) * 100 rat_sums_recent_netinc_q2q1_x100, 
    sum(dat.liab_q1)   over (partition by recent.is_within_last_10t) / nullif(sum(dat.liab_q2)   over (partition by recent.is_within_last_10t),0) * 100 rat_sums_recent_liab_q2q1_x100,
    dat.company_id,
    dat.ticker,
    dat.sp,
    dat.company,
    dat.mktcap,
    dat.liab_q1,
    dat.liab_q2,
    (dat.liab_q1 - dat.liab_q2)  / nullif(abs(dat.liab_q2),0)   * 100.00 prchg_liab_q2q1,
    dat.netinc_q1,
    dat.netinc_q2,
    (dat.netinc_q1 - dat.netinc_q2)/ nullif(abs(dat.netinc_q2),0) * 100.00 prchg_netinc_q2q1,
    ( (dat.liab_q1   - dat.liab_q2)  / nullif(abs(dat.liab_q2),0) ) / nullif((dat.netinc_q1 - dat.netinc_q2)/ nullif(abs(dat.netinc_q2),0),0) * 100 rat_prchg_liab_netinc_q1q2_x_100
  from qs_dateindex_company_id, dat, recent
  where dat.dateindex  = qs_dateindex_company_id.lv_qs_dateindex 
    and dat.company_id = qs_dateindex_company_id.lv_qs_company_id
    -- as of 'now.dateindex' in 17590, 17562, 17529, 17500, 17470, 17438
    -- explicit remove nulls: very little difference in rat_sums_recent_netinc_q2q1_x100 ( 44 percent better net income )
    -- BUT I do not like the number)
    and dat.netinc_q1 is not null and dat.netinc_q2 is not null and dat.liab_q1 is not null and dat.liab_q2 is not null
    and qs_dateindex_company_id.lv_qs_dateindex  = recent.dateindex
    and qs_dateindex_company_id.lv_qs_company_id = recent.company_id
  order by dat.qs_date, dat.company_id
  ;
  ")
    
  sipro_rolling_finstats <- dbGetQuery(con, query)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_rolling_finstats.")
  
  return(sipro_rolling_finstats)
  
}
# View each
# sipro_rolling_finstats <- get_sipro_rolling_finstats()
#
# # PARTIAL view
# # NOT a GOOD view of the 'ENTIRE'
# # instantaneous look-at: sp rolling last 10 trillion
# sipro_rolling_finstats <- get_sipro_rolling_finstats()

# # PARTIAL view
# # NOT A GOOD view of the 'ENTIRE'
# # large financial instititutions
# sipro_rolling_finstats <- get_sipro_rolling_finstats(
#     dateindex_criteria = "now.dateindex in ( select distinct dateindex from fe_data_store.si_finecon2 order by dateindex desc offset 0 limit 6 )"
#   , other_criteria     = "now.sp in ('500') and now.industry_desc in ('Money Center Banks', 'Regional Banks', 'Consumer Financial Services', 'S&Ls/Savings Banks')"
#   , rolling_limit     = "600000.00"
#   )

# USING THIS
# sipro_rolling_finstats <- get_sipro_rolling_finstats()
# View(sipro_rolling_finstats)

# USING THIS ( econ IS MUCH more SENSTIVE to bank PERFORMANCE )
# sipro_rolling_finstats <- get_sipro_rolling_finstats(
#    dateindex_criteria = "now.dateindex in ( select distinct dateindex from fe_data_store.si_finecon2 order by dateindex desc offset 0 limit 6 )"
#  , other_criteria     = "now.sp in ('500') and now.industry_desc in ('Money Center Banks', 'Regional Banks', 'Consumer Financial Services', 'S&Ls/Savings Banks')"
#  , rolling_limit     = "600000.00"
#  )



get_sipro_sp500_earnings <- function() {

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
  
  message("Begin function get_sipro_sp500_earnings.")

  verify_connection()
  
  sipro_sp500_earnings <- dbGetQuery(con, "

  -- NOV 29 2017 -- 101.00
  -- res <- Quandl('MULTPL/SP500_EARNINGS_MONTH') 
  -- '2017-03-31'
  -- NEED eom + INFLATION_ADJ CPI-U
  select 
      co.* 
    , t4q.earnings_x10_4q
    , t4q.earnings_per_avg_share_x10_4q
    , t4q.earnings_per_avg_share_x10_bal_4q 
    , t3q.earnings_x13_3q
    , t3q.earnings_per_avg_share_x13_3q
    , t2q.earnings_x20_2q
    , t2q.earnings_per_avg_share_x20_2q
    , t1q.earnings_x40_1q
    , t1q.earnings_per_avg_share_x40_1q 
  from (
    select 
        to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_co
      , (date_trunc('month', to_timestamp((fe.dateindex -5)*3600*24)::date) + interval '1 month' - interval '1 day')::date  dateindexeom_dt_co
      , count(1) count_elig_earnings
      , sum( mktcap / price ) / count(1) avg_shares_earnings
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
    and price   is not null
    group by dateindex
    order by dateindex asc
  ) co left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_4q
    , count(1) count_4q
    , sum( netinc_q1 + netinc_q2 + netinc_q3 + netinc_q4 ) / count(1) / 100 * 4/4 earnings_x10_4q
    , sum( mktcap / price ) / count(1) avg_shares                                                                                                        
    , sum( netinc_q1 + netinc_q2 + netinc_q3 + netinc_q4 ) / sum( mktcap / price ) * 10 * 4/4 earnings_per_avg_share_x10_4q                                    
    , sum( netinc_q1 + netinc_q2 + netinc_q3 + netinc_q4 ) / sum( mktcap / price ) / 29.8287359691438460 * 101.00 * 10 * 4/4 earnings_per_avg_share_x10_bal_4q 
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
    and price  is not null
  group by dateindex
  order by dateindex asc
  ) t4q on co.dateindex_dt_co = t4q.dateindex_dt_4q left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_3q
    , sum( netinc_q1 + netinc_q2 + netinc_q3  ) / count(1) / 100 * 4/3 earnings_x13_3q                                                                                               
    , sum( netinc_q1 + netinc_q2 + netinc_q3  ) / sum( mktcap / price ) * 10 * 4/3 earnings_per_avg_share_x13_3q                                    
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
    and price  is not null
  group by dateindex
  order by dateindex asc
  ) t3q on co.dateindex_dt_co = t3q.dateindex_dt_3q left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_2q
    , sum( netinc_q1 + netinc_q2 ) / count(1) / 100 * 4/2 earnings_x20_2q                                                                                               
    , sum( netinc_q1 + netinc_q2 ) / sum( mktcap / price ) * 10 * 4/2 earnings_per_avg_share_x20_2q                                    
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
    and price  is not null
  group by dateindex
  order by dateindex asc
  ) t2q on co.dateindex_dt_co = t2q.dateindex_dt_2q left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_1q
    , sum( netinc_q1 ) / count(1) / 100 * 4/1 earnings_x40_1q                                                                                               
    , sum( netinc_q1 ) / sum( mktcap / price ) * 10 * 4/1 earnings_per_avg_share_x40_1q                                    
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
    and price  is not null
  group by dateindex
  order by dateindex asc
  ) t1q on co.dateindex_dt_co = t1q.dateindex_dt_1q 
  order by co.dateindex_dt_co
  ;
  ")
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_sp500_earnings.")
  
  return(sipro_sp500_earnings)
  
}
# sipro_sp500_earnings <- get_sipro_sp500_earnings()  
# > str(sipro_sp500_earnings)
# 'data.frame':	179 obs. of  13 variables:
#  $ dateindex_dt_co                  : Date, format: "2003-01-03" "2003-01-31" "2003-02-28" "2003-04-04" ...
#  $ dateindexeom_dt_co               : Date, format: "2002-12-31" "2003-01-31" "2003-02-28" "2003-03-31" ...
#  $ count_elig                       : num  500 500 500 500 500 500 500 500 499 500 ...
#  $ avg_shares                       : num  563 562 574 573 574 ...
#  $ earnings_x10_4q                  : num  3.27 2.67 1.84 1.68 4.07 ...
#  $ earnings_per_avg_share_x10_4q    : num  5.81 4.75 3.2 2.93 7.1 ...
#  $ earnings_per_avg_share_x10_bal_4q: num  19.68 16.08 10.84 9.92 24.03 ...
#  $ earnings_x13_3q                  : num  3.04 3.72 3.3 2.81 3.87 ...
#  $ earnings_per_avg_share_x13_3q    : num  5.4 6.62 5.76 4.91 6.74 ...
#  $ earnings_x20_2q                  : num  5.46 4.49 2.84 1.77 3.07 ...
#  $ earnings_per_avg_share_x20_2q    : num  9.7 7.99 4.95 3.08 5.36 ...
#  $ earnings_x40_1q                  : num  6.17 2.13 -1.44 -1.68 4.18 ...
#  $ earnings_per_avg_share_x40_1q    : num  10.97 3.79 -2.52 -2.93 7.29 ...
# > head(ret[,c("dateindexeom_dt_co", "count_elig","avg_shares", "earnings_per_avg_share_x10_4q"),drop= FALSE])
#   dateindexeom_dt_co count_elig avg_shares earnings_per_avg_share_x10_4q
# 1         2002-12-31        500   562.5667                      5.813149
# 2         2003-01-31        500   562.0457                      4.747543
# 3         2003-02-28        500   573.6316                      3.202773
# 4         2003-03-31        500   572.9894                      2.931077
# 5         2003-04-30        500   573.5246                      7.096166
# 6         2003-05-31        500   575.0124                      7.799762


# NOTE: NOT used in the code flow into get_quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts
get_sipro_sp500_earnings_eom_xts <- function() {

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
  # uses get_sipro_sp500_earnings
  
  message("Begin function get_sipro_sp500_earnings_eom_xts.")

  sipro_sp500_earnings <- get_sipro_sp500_earnings()
  
  temp <- as.matrix(sipro_sp500_earnings[ ,!colnames(sipro_sp500_earnings) %in% c("dateindex_dt_co", "dateindexeom_dt_co"), drop = FALSE])
  rownames(temp) <- as.character(sipro_sp500_earnings[["dateindexeom_dt_co"]])
  # S3 dispatch as.xts.matrix ... will return with a non-Date index
  temp2 <- as.xts(temp)
  rm(temp)
  index(temp2) <- zoo::as.Date(index(temp2))
  sipro_sp500_earnings_eom_xts <- temp2
  rm(temp2)
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_sp500_earnings_eom_xts.")

  return(sipro_sp500_earnings_eom_xts)

}
#
# USING THIS #
# earnings up/down # pace over time
# sum( NETINC_OVER_QUARTERS ) / sum( mktcap / price ) # earnings_per_avg_share
# earnings_per_avg_share_x20_2q ( 6 month pace )
# earnings_per_avg_share_x40_1q ( 3 month pace: last quarters performance )
# sipro_sp500_earnings_eom_xts        <- get_sipro_sp500_earnings_eom_xts()
# View(sipro_sp500_earnings_eom_xts)
# 

# NOTE: NOT used in the code flow into get_quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts
# sipro_sp500_earnings_eom_xts <- get_sipro_sp500_earnings_eom_xts()
# > str(sipro_sp500_earnings_eom_xts) 
# An 'xts' object on 2002-12-31/2017-11-30 containing:
#   Data: num [1:180, 1:11] 500 500 500 500 500 500 500 500 499 500 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:11] "count_elig" "avg_shares" "earnings_x10_4q" "earnings_per_avg_share_x10_4q" ...
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:  
#  NULL
# > head(sipro_sp500_earnings_eom_xts)
#            count_elig avg_shares earnings_x10_4q earnings_per_avg_share_x10_4q
# 2002-12-31        500   562.5667        3.270284                      5.813149
# 2003-01-31        500   562.0457        2.668336                      4.747543
# 2003-02-28        500   573.6316        1.837212                      3.202773
# 2003-03-31        500   572.9894        1.679476                      2.931077
# 2003-04-30        500   573.5246        4.069826                      7.096166
# 2003-05-31        500   575.0124        4.484960                      7.799762
#            earnings_per_avg_share_x10_bal_4q earnings_x13_3q earnings_per_avg_share_x13_3q earnings_x20_2q
# 2002-12-31                         19.683304        3.035288                      5.395428        5.455972
# 2003-01-31                         16.075163        3.720589                      6.619727        4.490428
# 2003-02-28                         10.844580        3.304765                      5.761129        2.840272
# 2003-03-31                          9.924617        2.812341                      4.908191        1.765340
# 2003-04-30                         24.027595        3.865995                      6.740765        3.071788
# 2003-05-31                         26.409969        4.207661                      7.317514        3.909836
#            earnings_per_avg_share_x20_2q earnings_x40_1q earnings_per_avg_share_x40_1q
# 2002-12-31                      9.698357        6.170928                     10.969239
# 2003-01-31                      7.989435        2.128176                      3.786482
# 2003-02-28                      4.951387       -1.444128                     -2.517518
# 2003-03-31                      3.080930       -1.680296                     -2.932508
# 2003-04-30                      5.355983        4.182344                      7.292353
# 2003-05-31                      6.799568        6.629080                     11.528586
# > tail(ret)
#            count_elig avg_shares earnings_x10_4q earnings_per_avg_share_x10_4q
# 2017-06-30        496   617.6454        19.41229                      31.42951
# 2017-07-31        492   617.3417        19.53819                      31.64890
# 2017-08-31        497   612.9782        19.79365                      32.29096
# 2017-09-30        498   608.1154        19.72096                      32.42963
# 2017-10-31        493   610.6857        20.09662                      32.90829
# 2017-11-30        499   607.1524        19.76131                      32.54752
#            earnings_per_avg_share_x10_bal_4q earnings_x13_3q earnings_per_avg_share_x13_3q earnings_x20_2q
# 2017-06-30                          106.4202        19.66827                      31.84396        19.29080
# 2017-07-31                          107.1631        19.64492                      31.82180        19.49567
# 2017-08-31                          109.3371        19.26056                      31.42128        19.78510
# 2017-09-30                          109.8066        19.26663                      31.68253        19.75596
# 2017-10-31                          111.4274        19.50442                      31.93856        20.28557
# 2017-11-30                          110.2058        20.33730                      33.49621        20.68452
#            earnings_per_avg_share_x20_2q earnings_x40_1q earnings_per_avg_share_x40_1q
# 2017-06-30                      31.23281        19.90568                      32.22833
# 2017-07-31                      31.58003        20.08363                      32.53244
# 2017-08-31                      32.27700        19.96466                      32.56994
# 2017-09-30                      32.48720        19.87249                      32.67881
# 2017-10-31                      33.21769        20.85650                      34.15260
# 2017-11-30                      34.06808        20.59506                      33.92075



get_sipro_sp500_various_int_expenses_eom_xts  <- function() {

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
  # uses package zoo function as.Date

  message("Begin function get_sipro_sp500_various_int_expenses_eom_xts.")

  verify_connection()

  sipro_sp500_various_int_expenses_eom <- dbGetQuery(con, "

  -- '2016-03-31' 
  -- Stock Investor Pro 4.5 (3/31/2016 release)
  -- Null Handling
  --
  -- in some cases, that we had to start calculating the data ourselves.
  -- Interest expense (INT_Q1-Q8, 12M, Y1-Y7)
  --
  -- missing data may simply be converted to a zero

  select 
      to_timestamp(dateindexeom*3600*24)::date dateindexeom_dt
    , dateindexeom
    , count(mktcap)    
    , sum(mktcap)    / count(mktcap)    a_mktcap
    , sum(assets_q1) / count(assets_q1) a_assets
    , sum(liab_q1)   / count(liab_q1)   a_liab
    , (sum(intno_q1) + sum(int_q1))  / count(1)                               a_inttot -- best that I can do
    , sum(mktcap)    / nullif(sum(intno_q1) + sum(int_q1),0)                  s_mktcap_o_s_inttot
    , (sum(assets_q1) - sum(liab_q1)) / nullif(sum(intno_q1) + sum(int_q1),0) s_equity_o_s_inttot
    , sum(assets_q1) / nullif(sum(intno_q1) + sum(int_q1),0)                  s_assets_o_s_inttot
    , sum(liab_q1)   / nullif(sum(intno_q1) + sum(int_q1),0)                  s_liab_o_s_inttot
    , sum(mktcap)    / nullif(sum(liab_q1),0)                                 s_mktcap_o_s_liab
  from ( 
         select dateindexeom, sp, mktcap, assets_q1, liab_q1
         -- Stock Investor Pro 4.5 (3/31/2016 release) -- Null Handling -- calculating the data ourselves -- Interest expense (INT_Q1-Q8, 12M, Y1-Y7)
         -- I do the entire history because it makes sense
         , case when intno_q1 is null then 0 else intno_q1 end intno_q1  -- typically before 2016-03-31
         , case when int_q1   is null then 0 else                        -- typically before 2016-03-31
         -- errors at -- Stock Investor Pro 4.5 (3/31/2016 release)
           case when dateindex = 16891 and int_q1 >= 5071.00 and adr = 0 then int_q1 / 1000.00 else -- MANY errors
           case when dateindex = 17225 and ticker = 'AVB' then int_q1 / 1000.00 else         -- seems outlier error (fixable?)
           case when dateindex = 17470 and ticker = 'JPM' then null             else int_q1  -- seems outlier error (not fixable) -- ONE case
           end end end end int_q1
         from fe_data_store.si_finecon2
       ) sq1
        -- outlier ONE case
  where int_q1 is not null and mktcap is not null and assets_q1 is not null and liab_q1 is not null
  and sp in ('500') -- 
  group by dateindexeom order by dateindexeom

  -- a_inttot
  -- seems trending UP seems to be good 
  -- just 'a' turning point(change in slope) matters ( what it MEANS depends on the context )
  -- before ave interest expense increase through 2007 to a max 212 @ '2007-12-31' ... then started decreasing from there 
  -- bounced eventually around 90 for a long time (min 81 @ '2014-10-31') 
  -- after '2014-12-28' started re-increasing ( and still increasing since )

  -- s_equity_o_s_inttot
  -- seems trending DOWN seems to be good
  -- max 86 @ '2004-09-31' ... min 45 @ '2007-12-31' ... max 168 @ '2015-02-27' ... 
  -- after '2015-02-27' started re-decreasing ( and still decreasing since )

  ;
  ")

  temp_coredata <- as.matrix(sipro_sp500_various_int_expenses_eom[ ,!colnames(sipro_sp500_various_int_expenses_eom) %in% c("dateindexeom_dt", "dateindexeom"), drop = FALSE])
  rownames(temp_coredata) <- as.character(sipro_sp500_various_int_expenses_eom[["dateindexeom_dt"]])
                                              # S3 dispatch as.xts.matrix
  sipro_sp500_various_int_expenses_eom_xts <- as.xts(temp_coredata)
  index(sipro_sp500_various_int_expenses_eom_xts) <- zoo::as.Date(index(sipro_sp500_various_int_expenses_eom_xts))

  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_sp500_various_int_expenses_eom_xts.")

  return(sipro_sp500_various_int_expenses_eom_xts) 

}
# finecon01.R
# sipro_sp500_various_int_expenses_eom_xts <- get_sipro_sp500_various_int_expenses_eom_xts()
# View(sipro_sp500_various_int_expenses_eom_xts[,c("a_inttot","s_equity_o_s_inttot")]) # almost mirror images of each other
# NOTE: was STRONG(good) through late 2015-early 2016 FALSE recession
# But JUN 2016 - MAY 2017 they where NOT mirror images of each other
# large era look at (only peaks and troughs matter)
# short-term pct change is NOT useful




# NOT! INFLATION ADJUSTED!
get_sipro_earnings_per_avg_share_x10_4q_eom_xts <- function() {
  
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
  
  message("Begin function get_sipro_earnings_per_avg_share_x10_4q_eom_xts.")
  
  # uses get_sp500_earnings
  require(xts)

  sp500_earnings <-  get_sipro_sp500_earnings()
 
  temp <- as.matrix(sp500_earnings[, "earnings_per_avg_share_x10_4q", drop = FALSE]) # 2002-12-31
  rownames(temp) <- as.character(sp500_earnings[["dateindexeom_dt_co"]])
  # S3 dispatch as.xts.matrix ... will return with a non-Date index
  temp2 <- as.xts(temp)
  rm(temp)
  index(temp2) <- zoo::as.Date(index(temp2))
  colnames(temp2)[1] <- "earnings_avg_per_share"
  sipro_earnings_per_avg_share_x10_4q_eom_xts <- temp2
  rm(temp2)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_earnings_per_avg_share_x10_4q_eom_xts.")
  
  return(sipro_earnings_per_avg_share_x10_4q_eom_xts)
  
}
# NOT! INFLATION ADJUSTED!
# sipro_earnings_per_avg_share_x10_4q_eom_xts <- get_sipro_earnings_per_avg_share_x10_4q_eom_xts()
# > str(sipro_earnings_per_avg_share_x10_4q_eom_xts)
# An 'xts' object on 2002-12-31/2017-10-31 containing:
#   Data: num [1:179, 1] 5.81 4.75 3.2 2.93 7.1 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr "earnings_avg_per_share"
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:  
#  NULL
# > head(res)
#            earnings_avg_per_share
# 2002-12-31               5.813149
# 2003-01-31               4.747543
# 2003-02-28               3.202773
# 2003-03-31               2.931077
# 2003-04-30               7.096166
# 2003-05-31               7.799762



# INFLATION ADJUSTED
get_quandl_earnings_per_avg_share_x10_4q_eom_xts <- function() {
  
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
  
  message("Begin function get_quandl_earnings_per_avg_share_x10_4q_eom_xts.")
  
  require(Quandl)
  # Quandl.api_key(api_key= "YOURKEYHERE")
  temp <- Quandl("MULTPL/SP500_EARNINGS_MONTH", type = "xts")   # 1871-01-31
  index(temp) <- zoo::as.Date(index(temp), frac= 1) # checked date 'are end month anyways'
  colnames(temp)[1] <- "sp500_earnings_month"
  quandl_earnings_per_avg_share_x10_4q_eom_xts <- temp
  rm(temp)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_quandl_earnings_per_avg_share_x10_4q_eom_xts.")
  
  return(quandl_earnings_per_avg_share_x10_4q_eom_xts)
  
}
# INFLATION ADJUSTED
# Quandl.api_key(api_key= "YOURKEYHERE")
# quandl_earnings_per_avg_share_x10_4q_eom_xts <- get_quandl_earnings_per_avg_share_x10_4q_eom_xts()
# > str(quandl_earnings_per_avg_share_x10_4q_eom_xts)
# An 'xts' object on 1871-01-31/2017-03-31 containing:
#   Data: num [1:1755, 1] 7.88 7.65 7.54 7.82 8 8.13 8.13 8.26 8.06 7.94 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr "sp500_earnings_month"
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:  
#  NULL
# > head(quandl_earnings_per_avg_share_x10_4q_eom_xts)
#            sp500_earnings_month
# 1871-01-31                 7.88
# 1871-02-28                 7.65
# 1871-03-31                 7.54
# 1871-04-30                 7.82
# 1871-05-31                 8.00
# 1871-06-30                 8.13



get_fred_inflation_cpiu_eom_xts <- function() {
  
  # Consumer Price Index for All Urban Consumers: All Items
  # https://fred.stlouisfed.org/series/CPIAUCSL
  
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
  
  message("Begin function get_fred_inflation_cpiu_eom_xts.")
  
  require(quantmod) # attaches zoo ... uses zoo::as.Date
  require(lubridate)
  require(magrittr)
  # uses Hmisc::truncPOSIXt
  
  CPIAUCNS <- getSymbols("CPIAUCNS", src = "FRED", auto.assign = FALSE)  # 1912-12-31
  temp <- CPIAUCNS
  index(temp) <- (zoo::as.Date(index(temp)) - 5L) %m+% months(1) %>% Hmisc::truncPOSIXt(., units='months') %m+% days(-1) %>%  zoo::as.Date(.)
  colnames(temp)[1] <- tolower(colnames(temp)[1])
  fred_inflation_cpiu_eom_xts <- temp
  rm(temp)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_fred_inflation_cpiu_eom_xts.")
  
  return(fred_inflation_cpiu_eom_xts)
  
}
# NOTE: THIS IS A cummulative VALUE ( NOT A 'percent change')
# (I HAVE TO CALCULATE %change% MYSELF)
# ret <- get_fred_inflation_cpiu_eom_xts()
# > str(ret)
# An 'xts' object on 1912-12-31/2017-09-30 containing:
#   Data: num [1:1258, 1] 9.8 9.8 9.8 9.8 9.7 9.8 9.9 9.9 10 10 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr "cpiaucns"
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:  
# List of 2
#  $ src    : chr "FRED"
#  $ updated: POSIXct[1:1], format: "2017-12-02 23:48:56"
# > head(ret)
#            cpiaucns
# 1912-12-31      9.8
# 1913-01-31      9.8
# 1913-02-28      9.8
# 1913-03-31      9.8
# 1913-04-30      9.7
# 1913-05-31      9.8



get_quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts <- function() {
 
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
  
  message("Begin function get_quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts.")
  
  # stitch them together ( the best I can ) 
  require(xts) # uses rbind.xts
  
  # NOT! INFLATION ADJUSTED!
  # uses get_sipro_earnings_per_avg_share_x10_4q_eom_xts
  
  # INFLATION ADJUSTED
  # uses get_quandl_earnings_per_avg_share_x10_4q_eom_xts
  
  # uses get_fred_inflation_cpiu_xts
  
  # historical dates
  quandl_eom_xts <- get_quandl_earnings_per_avg_share_x10_4q_eom_xts()
  # last 5 or 6 months
  sipro_eom_xts <- get_sipro_earnings_per_avg_share_x10_4q_eom_xts()
  # adjustment needed on behalf of (sipro_earnings_per_avg_share_x10_4q_eom_xts)
  inflation_eom_xts <- get_fred_inflation_cpiu_eom_xts()
  
  # fix sipro
  # 1. re-proportion comparted to quandl so last = 101 at the time point
  # 2. make inflation adjusted
  sipro_rel_quandl_eom_xts <- 
    sipro_eom_xts *
    # value = 101 at date: point relative to quandl_eom_xts at its last available date ( typically adjust so last = 101 at the time point )
    ( coredata(last(quandl_eom_xts))[1] / coredata(sipro_eom_xts[index(last(quandl_eom_xts))])[1] ) * 
    # inflation adj: vector relative to CPIAUCNS at every(MANY) date(S) - because of inflation - older earnings are worth MORE today
    ( coredata(inflation_eom_xts[index(last(quandl_eom_xts))])[1] / inflation_eom_xts  ) 

  # actual stitch
  sipro_with_quandl_eom_xts <- 
    rbind.xts(
      # longer term history from Quandl
        quandl_eom_xts[paste0("/", index(last(quandl_eom_xts)))]
      # recent 5/6-month history from SIPro
      , sipro_rel_quandl_eom_xts[paste0(index(last(quandl_eom_xts)) + 1, "/")]
    )
    
  quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts <- sipro_with_quandl_eom_xts
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts.")
  
  return(quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts)
  
}
# inflation ( cpiaucns ) adjusted, so can be aggressive ( different than others that do nominal calculations)
# 
# quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts <- get_quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts()
# 
# > str(quandl_sipro_earnings_per_avg_share_x10_4q_eom_xts)
# An 'xts' object on 1871-01-31/2017-09-30 containing:
#   Data: num [1:1761, 1] 7.88 7.65 7.54 7.82 8 8.13 8.13 8.26 8.06 7.94 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr "sp500_earnings_month"
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:  
#  NULL
# > head(ret)
#            sp500_earnings_month
# 1871-01-31                 7.88
# 1871-02-28                 7.65
# 1871-03-31                 7.54
# 1871-04-30                 7.82
# 1871-05-31                 8.00
# 1871-06-30                 8.13
# > tail(ret,8)
#            sp500_earnings_month
# 2017-02-28              99.1500
# 2017-03-31             101.0000
# 2017-04-30             100.8334
# 2017-05-31             104.9540
# 2017-06-30             106.3063
# 2017-07-31             106.7288
# 2017-08-31             108.3204
# 2017-09-30             108.8544


# get all company_ids that every were/are_now/will_be a member of the sp 500/400/600
# during now and within the last/future 13 months
get_sipro_spany_company_ids <- function(
    ref_dateindex = NULL
  , restriction = "now.sp in ('500','400','600')"
  , past_span = 13
  , fut_span  = 13) {

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
  
  message("Begin function get_sipro_spany_company_ids")

  verify_connection()
  
  query <- paste0("

    select distinct lat.company_id from (

      select fut.dateindex from (select distinct dateindex 
      from fe_data_store.si_finecon2 
      where dateindex > ", ref_dateindex, "                   
      order by dateindex      limit ", fut_span, ") fut     
      union all
      select now.dateindex from (select distinct dateindex 
      from fe_data_store.si_finecon2 
      where dateindex  = ", ref_dateindex, "                  
      order by dateindex desc limit 1) now       
      union all
      select past.dateindex from (select distinct dateindex 
      from fe_data_store.si_finecon2 
      where dateindex <  ", ref_dateindex, "                  
      order by dateindex desc limit ", past_span, ") past 
      order by dateindex
    ) dateindexes
    left join lateral ( 
      select distinct now.company_id
      from fe_data_store.si_finecon2 now
      where 
            now.dateindex = dateindexes.dateindex and 
            ", restriction, "
    ) lat on true order by lat.company_id

  ")

  sipro_spany_company_ids <- dbGetQuery(con, query)
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_spany_company_ids")
  
  return(sipro_spany_company_ids)
  
}
# sipro_spany_company_ids <- get_sipro_spany_company_ids(17011)  
# str(sipro_spany_company_ids)
# 'data.frame':	1703 obs. of  1 variable:
# $ company_id: chr  "000B3" "000EB" "0013D" "0013N" ...




get_sipro_sp500_mktcap_o_netinc <- function() {

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
  
  message("Begin function get_sipro_sp500_mktcap_o_netinc.")

  verify_connection()
  
  sipro_sp500_mktcap_o_netinc <- dbGetQuery(con, "

  select 
      co.* 
    , t4q.mktcap_o_netinc_d10_x10_4q 
    , t3q.mktcap_o_netinc_d13_x10_3q 
    , t2q.mktcap_o_netinc_d20_x10_2q
    , t1q.mktcap_o_netinc_d40_x10_1q
  from (
    select 
        to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_co
      , (date_trunc('month', to_timestamp((fe.dateindex -5)*3600*24)::date) + interval '1 month' - interval '1 day')::date  dateindexeom_dt_co
      , count(1) count_elig_mktcap_o_netinc
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
    group by dateindex
    order by dateindex asc
  ) co left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_4q
    , sum( mktcap ) / ( ( sum( netinc_q1 ) + sum( netinc_q2 ) + sum( netinc_q3 ) + sum( netinc_q4 ) ) * 10 * 4/4 ) * 10 mktcap_o_netinc_d10_x10_4q                                   
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
  group by dateindex
  order by dateindex asc
  ) t4q on co.dateindex_dt_co = t4q.dateindex_dt_4q left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_3q
    , sum( mktcap ) / ( ( sum( netinc_q1 ) + sum( netinc_q2 ) + sum( netinc_q3 ) ) * 10 * 4/3 ) * 10 mktcap_o_netinc_d13_x10_3q 
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
  group by dateindex
  order by dateindex asc
  ) t3q on co.dateindex_dt_co = t3q.dateindex_dt_3q left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_2q
    , sum( mktcap ) / ( ( sum( netinc_q1 ) + sum( netinc_q2 ) ) * 10 * 4/2 ) * 10 mktcap_o_netinc_d20_x10_2q                                      
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
  group by dateindex
  order by dateindex asc
  ) t2q on co.dateindex_dt_co = t2q.dateindex_dt_2q left join (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_1q                                                                                              
    , sum( mktcap ) / ( (sum( netinc_q1 ) ) * 10 * 4/1 ) * 10 mktcap_o_netinc_d40_x10_1q                                    
  from fe_data_store.si_finecon2 fe
  where sp = '500'
    and fe.netinc_q1 is not null
    and fe.netinc_q2 is not null
    and fe.netinc_q3 is not null
    and fe.netinc_q4 is not null
    and mktcap is not null
  group by dateindex
  order by dateindex asc
  ) t1q on co.dateindex_dt_co = t1q.dateindex_dt_1q 
  order by co.dateindex_dt_co
  ;

  ")
  
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_sp500_mktcap_o_netinc.")
  
  return(sipro_sp500_mktcap_o_netinc)
  
}
# ret <- get_sipro_sp500_mktcap_o_netinc()  
# > str(ret)
# 'data.frame':   180 obs. of  7 variables:
#  $ dateindex_dt_co           : Date, format: "2003-01-03" "2003-01-31" "2003-02-28" "2003-04-04" ...
#  $ dateindexeom_dt_co        : Date, format: "2002-12-31" "2003-01-31" "2003-02-28" "2003-03-31" ...
#  $ count_elig_mktcap_o_netinc: num  500 500 500 500 500 500 500 500 499 500 ...
#  $ mktcap_o_netinc_d10_x10_4q: num  51.9 59 84.2 96.4 42.1 ...
#  $ mktcap_o_netinc_d13_x10_3q: num  55.9 42.3 46.8 57.6 44.3 ...
#  $ mktcap_o_netinc_d20_x10_2q: num  31.1 35 54.5 91.7 55.8 ...
#  $ mktcap_o_netinc_d40_x10_1q: num  27.5 73.9 -107.1 -96.3 40.9 ...
# 
# head(ret)
#   dateindex_dt_co dateindexeom_dt_co count_elig_mktcap_o_netinc mktcap_o_netinc_d10_x10_4q mktcap_o_netinc_d13_x10_3q mktcap_o_netinc_d20_x10_2q mktcap_o_netinc_d40_x10_1q
# 1      2003-01-03         2002-12-31                        500                   51.92891                   55.94932                   31.12595                   27.51973
# 2      2003-01-31         2003-01-31                        500                   58.97908                   42.29868                   35.04699                   73.94877
# 3      2003-02-28         2003-02-28                        500                   84.22045                   46.82052                   54.47747                 -107.14481
# 4      2003-04-04         2003-03-31                        500                   96.39047                   57.56253                   91.70215                  -96.34343
# 5      2003-05-02         2003-04-30                        500                   42.08016                   44.29880                   55.75220                   40.94808
# 6      2003-05-30         2003-05-31                        500                   39.58077                   42.18927                   45.40297                   26.77870
# 
# tail(ret,12)
#     dateindex_dt_co dateindexeom_dt_co count_elig_mktcap_o_netinc mktcap_o_netinc_d10_x10_4q mktcap_o_netinc_d13_x10_3q mktcap_o_netinc_d20_x10_2q mktcap_o_netinc_d40_x10_1q
# 169      2016-12-30         2016-12-31                        498                   23.19898                   22.25984                   21.31846                   20.24031
# 170      2017-01-31         2017-01-31                        480                   23.21119                   21.42901                   20.52533                   19.84582
# 171      2017-02-28         2017-02-28                        494                   23.50901                   22.81407                   22.48544                   23.78601
# 172      2017-03-31         2017-03-31                        494                   23.40708                   22.69386                   22.28070                   23.45204
# 173      2017-04-28         2017-04-30                        484                   23.06293                   22.72173                   22.36103                   22.20616
# 174      2017-05-31         2017-05-31                        497                   22.93497                   22.56506                   23.15824                   22.37460
# 175      2017-06-30         2017-06-30                        496                   22.90273                   22.60466                   23.04697                   22.33506
# 176      2017-07-31         2017-07-31                        492                   23.08794                   22.96250                   23.13829                   22.46090
# 177      2017-08-31         2017-08-31                        497                   22.65730                   23.28441                   22.66709                   22.46323
# 178      2017-09-29         2017-09-30                        498                   23.03229                   23.57541                   22.99147                   22.85666
# 179      2017-10-31         2017-10-31                        493                   23.11135                   23.81306                   22.89608                   22.26932
# 180      2017-11-30         2017-11-30                        499                   24.04328                   23.36232                   22.97016                   23.06993
# # SOMEWAY/HOW # I REMEMBER THIS BEING MORE EXTREME ( RUN OF SUN DEC 03 2017 )



get_sipro_sp500_mktcap_o_netinc_eom_xts <- function() {

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
  # uses get_sipro_sp500_mktcap_o_netinc
  
  message("Begin function get_sipro_sp500_mktcap_o_netinc_eom_xts.")

  sipro_sp500_mktcap_o_netinc <- get_sipro_sp500_mktcap_o_netinc()
  
  temp <- as.matrix(sipro_sp500_mktcap_o_netinc[ ,!colnames(sipro_sp500_mktcap_o_netinc) %in% c("dateindex_dt_co", "dateindexeom_dt_co"), drop = FALSE])
  rownames(temp) <- as.character(sipro_sp500_mktcap_o_netinc[["dateindexeom_dt_co"]])
  # S3 dispatch as.xts.matrix ... will return with a non-Date index
  temp2 <- as.xts(temp)
  rm(temp)
  index(temp2) <- zoo::as.Date(index(temp2))
  sipro_sp500_mktcap_o_netinc_eom_xts <- temp2
  rm(temp2)
 
  Sys.setenv(TZ=oldtz)
  options(ops)
  
  message("End   function get_sipro_sp500_mktcap_o_netinc_eom_xts.")

  return(sipro_sp500_mktcap_o_netinc_eom_xts)

}
#
# USING THIS #
# is the cost/return acceptable
# mktcap/netinc # price earnings ratio # up/down # pace over time
# mktcap_o_netinc_d20_x10_2q ( 6 month pace )
# mktcap_o_netinc_d40_x10_1q ( 3 month pace: last quarters performance )
# sipro_sp500_mktcap_o_netinc_eom_xts <- get_sipro_sp500_mktcap_o_netinc_eom_xts()
# View(sipro_sp500_mktcap_o_netinc_eom_xts)
# 
# NOTE: NOT used in the code flow into get_quandl_sipro_mktcap_o_netinc_per_avg_share_x10_4q_eom_xts
# ret <- get_sipro_sp500_mktcap_o_netinc_eom_xts()
# str(ret)
# An 'xts' object on 2002-12-31/2017-11-30 containing:
#   Data: num [1:180, 1:5] 500 500 500 500 500 500 500 500 499 500 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:5] "count_elig_mktcap_o_netinc" "mktcap_o_netinc_d10_x10_4q" "mktcap_o_netinc_d13_x10_3q" "mktcap_o_netinc_d20_x10_2q" ...
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:
#  NULL
# 
# head(ret)
#            count_elig_mktcap_o_netinc mktcap_o_netinc_d10_x10_4q mktcap_o_netinc_d13_x10_3q mktcap_o_netinc_d20_x10_2q mktcap_o_netinc_d40_x10_1q
# 2002-12-31                        500                   51.92891                   55.94932                   31.12595                   27.51973
# 2003-01-31                        500                   58.97908                   42.29868                   35.04699                   73.94877
# 2003-02-28                        500                   84.22045                   46.82052                   54.47747                 -107.14481
# 2003-03-31                        500                   96.39047                   57.56253                   91.70215                  -96.34343
# 2003-04-30                        500                   42.08016                   44.29880                   55.75220                   40.94808
# 2003-05-31                        500                   39.58077                   42.18927                   45.40297                   26.77870
# 
# tail(ret,12)
#            count_elig_mktcap_o_netinc mktcap_o_netinc_d10_x10_4q mktcap_o_netinc_d13_x10_3q mktcap_o_netinc_d20_x10_2q mktcap_o_netinc_d40_x10_1q
# 2016-12-31                        498                   23.19898                   22.25984                   21.31846                   20.24031
# 2017-01-31                        480                   23.21119                   21.42901                   20.52533                   19.84582
# 2017-02-28                        494                   23.50901                   22.81407                   22.48544                   23.78601
# 2017-03-31                        494                   23.40708                   22.69386                   22.28070                   23.45204
# 2017-04-30                        484                   23.06293                   22.72173                   22.36103                   22.20616
# 2017-05-31                        497                   22.93497                   22.56506                   23.15824                   22.37460
# 2017-06-30                        496                   22.90273                   22.60466                   23.04697                   22.33506
# 2017-07-31                        492                   23.08794                   22.96250                   23.13829                   22.46090
# 2017-08-31                        497                   22.65730                   23.28441                   22.66709                   22.46323
# 2017-09-30                        498                   23.03229                   23.57541                   22.99147                   22.85666
# 2017-10-31                        493                   23.11135                   23.81306                   22.89608                   22.26932
# 2017-11-30                        499                   24.04328                   23.36232                   22.97016                   23.06993



# *** THIS_ONE ***
get_sipro_inbnd_netinc_any_marginals_eom_xts <- function(marginals = FALSE) {

  # NOTE USES : now_inbnd_stmtid_dateindex is not null
 
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
  require(TTR)

  message("Begin function get_sipro_inbnd_netinc_any_marginals_eom_xts")

  verify_connection()
  
  query <- paste0(
  "
  select 
      every.dateindex_dt, 
      every_count,                             every_mktcap_div_mill_x_4,        every_a_netinc_q1,   every_wtd_mktcap_a_netinc_q1,     every_rat_mktcap_o_netinc_q1_x_4,    every_a_netinc_q1q2_d_2,
      every_wtd_mktcap_a_netinc_q1q2_d_2,     every_rat_mktcap_o_netinc_q1q2_x_2,
      every_pradchg_f04w_ann_wtd_mktcap, every_pchg_netinc_q1q2,
      banks_count,                             banks_mktcap_div_mill_x_40,       banks_a_netinc_q1,   banks_wtd_mktcap_a_netinc_q1,     banks_rat_mktcap_o_netinc_q1_x_4,    banks_a_netinc_q1q2_d_2,
      banks_wtd_mktcap_a_netinc_q1q2_d_2,      banks_rat_mktcap_o_netinc_q1q2_x_2,
      banks_pradchg_f04w_ann_wtd_mktcap,       banks_pchg_netinc_q1q2,
      notbanks_count,                          notbanks_mktcap_div_mill_x_40, notbanks_a_netinc_q1, notbanks_wtd_mktcap_a_netinc_q1, notbanks_rat_mktcap_o_netinc_q1_x_4, notbanks_a_netinc_q1q2_d_2,
      notbanks_wtd_mktcap_a_netinc_q1q2_d_2,   notbanks_rat_mktcap_o_netinc_q1q2_x_2,
      notbanks_pradchg_f04w_ann_wtd_mktcap,    notbanks_pchg_netinc_q1q2
  from (
  select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt,
    count(netinc_q1 + netinc_q2 + mktcap) every_count,
    sum(mktcap) / 1000000.0 * 4.0 every_mktcap_div_mill_x_4,
    sum(netinc_q1) /  count(netinc_q1 + netinc_q2 + mktcap) every_a_netinc_q1,
    sum(netinc_q1 * mktcap) / sum(mktcap) every_wtd_mktcap_a_netinc_q1,
    sum(mktcap) / sum(netinc_q1 * 4) every_rat_mktcap_o_netinc_q1_x_4,
    sum(netinc_q1 + netinc_q2) /  count(netinc_q1 + netinc_q2 + mktcap) / 2 every_a_netinc_q1q2_d_2,
    sum((netinc_q1 + netinc_q2)* mktcap ) / sum(mktcap) / 2 every_wtd_mktcap_a_netinc_q1q2_d_2,
    sum(mktcap) / sum((netinc_q1 + netinc_q2) * 2) every_rat_mktcap_o_netinc_q1q2_x_2,
    sum(mktcap * pradchg_f04w_ann) / sum(mktcap) every_pradchg_f04w_ann_wtd_mktcap,
    (sum(netinc_q1) - sum(netinc_q2))/ abs( nullif(sum(netinc_q2),0) )* 100 every_pchg_netinc_q1q2
  from fe_data_store.si_finecon2  
  where 
        sp in ('500','400','600') 
    and netinc_q1 is not null and netinc_q2 is not null and mktcap is not null
    ", if(marginals) {"and now_inbnd_stmtid_dateindex is not null"} else {""}, "
  group by dateindex
  order by dateindex
  ) every left join (
  select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt,
    count(netinc_q1 + netinc_q2 + mktcap)        banks_count,
    sum(mktcap) / 1000000.0 * 40.0 banks_mktcap_div_mill_x_40,
    sum(netinc_q1) /  count(netinc_q1 + netinc_q2 + mktcap) banks_a_netinc_q1,
    sum(netinc_q1 * mktcap) / sum(mktcap) banks_wtd_mktcap_a_netinc_q1,
    sum(mktcap) / sum(netinc_q1 * 4) banks_rat_mktcap_o_netinc_q1_x_4,
    sum(netinc_q1 + netinc_q2) /  count(netinc_q1 + netinc_q2 + mktcap) / 2 banks_a_netinc_q1q2_d_2,
    sum((netinc_q1 + netinc_q2)* mktcap ) / sum(mktcap) / 2 banks_wtd_mktcap_a_netinc_q1q2_d_2,
    sum(mktcap) / sum((netinc_q1 + netinc_q2) * 2) banks_rat_mktcap_o_netinc_q1q2_x_2,
    sum(mktcap * pradchg_f04w_ann) / sum(mktcap) banks_pradchg_f04w_ann_wtd_mktcap,
    (sum(netinc_q1) - sum(netinc_q2))/ abs( nullif(sum(netinc_q2),0) )* 100 banks_pchg_netinc_q1q2
  from fe_data_store.si_finecon2  
  where 
        sp in ('500','400','600') and industry_desc     in ('Money Center Banks', 'Regional Banks', 'Consumer Financial Services', 'S&Ls/Savings Banks')
    and netinc_q1 is not null and netinc_q2 is not null and mktcap is not null
    ", if(marginals) {"and now_inbnd_stmtid_dateindex is not null"} else {""}, "
  group by dateindex
  order by dateindex
  ) banks on every.dateindex = banks.dateindex left join (
  select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt,
    count(netinc_q1 + netinc_q2 + mktcap)        notbanks_count,
    sum(mktcap) / 1000000.0 * 40.0 notbanks_mktcap_div_mill_x_40,
    sum(netinc_q1) /  count(netinc_q1 + netinc_q2 + mktcap) notbanks_a_netinc_q1,
    sum(netinc_q1 * mktcap) / sum(mktcap) notbanks_wtd_mktcap_a_netinc_q1,
    sum(mktcap) / sum(netinc_q1 * 4) notbanks_rat_mktcap_o_netinc_q1_x_4,
    sum(netinc_q1 + netinc_q2) /  count(netinc_q1 + netinc_q2 + mktcap) / 2 notbanks_a_netinc_q1q2_d_2,
    sum((netinc_q1 + netinc_q2)* mktcap ) / sum(mktcap) / 2 notbanks_wtd_mktcap_a_netinc_q1q2_d_2,
    sum(mktcap) / sum((netinc_q1 + netinc_q2) * 2) notbanks_rat_mktcap_o_netinc_q1q2_x_2,
    sum(mktcap * pradchg_f04w_ann) / sum(mktcap) notbanks_pradchg_f04w_ann_wtd_mktcap,
    (sum(netinc_q1) - sum(netinc_q2))/ abs( nullif(sum(netinc_q2),0) )* 100 notbanks_pchg_netinc_q1q2
  from fe_data_store.si_finecon2  
  where 
        sp in ('500','400','600') and industry_desc not in ('Money Center Banks', 'Regional Banks', 'Consumer Financial Services', 'S&Ls/Savings Banks')
    and netinc_q1 is not null and netinc_q2 is not null and mktcap is not null
    ", if(marginals) {"and now_inbnd_stmtid_dateindex is not null"} else {""}, "
  group by dateindex
  order by dateindex
  ) notbanks on every.dateindex = notbanks.dateindex 
  "
  )
  sipro_inbnd_netinc_any_marginals_eom <- dbGetQuery(con, query)
  
  temp <- as.matrix(sipro_inbnd_netinc_any_marginals_eom[ ,!colnames(sipro_inbnd_netinc_any_marginals_eom) %in% c("dateindex_dt"), drop = FALSE])
  rownames(temp) <- as.character(sipro_inbnd_netinc_any_marginals_eom[["dateindex_dt"]])
  # S3 dispatch as.xts.matrix ... will return with a non-Date index
  temp2 <- as.xts(temp)
  rm(temp)
  index(temp2) <- zoo::as.Date(index(temp2))
  sipro_inbnd_netinc_any_marginals_eom_xts <- temp2
  rm(temp2)
 
  Sys.setenv(TZ=oldtz)
  options(ops)

  message("End   function get_sipro_inbnd_netinc_any_marginals_eom_xts")

  return(sipro_inbnd_netinc_any_marginals_eom_xts)
    
}
# 
## marginals = TRUE ( NOT default ) 
## every, banks, notbanks
## marginals = TRUE ( just those 'reporting' in THAT month ) # BUT A rolling FUNCTIONs are BETTER
##
## USEFUL in the CASE when the PREVIOUS month WAS a MANY BIG COMPANIES LARGE REPORTING month
## sipro_inbnd_netinc_any_marginals_eom_xts_marginals <- get_sipro_inbnd_netinc_any_marginals_eom_xts(marginals = TRUE)
# 
# USING THIS *** THIS_ONE *** ( NOT THE EASIEST TO READ )
# sipro_inbnd_netinc_any_marginals_eom_xts <- get_sipro_inbnd_netinc_any_marginals_eom_xts()
# marginals = FALSE 
# every, banks, notbanks
# View(sipro_inbnd_netinc_any_marginals_eom_xts)
# banks
# View(sipro_inbnd_netinc_any_marginals_eom_xts[, grep("^bank",colnames(sipro_inbnd_netinc_any_marginals_eom_xts), value = T)])
# 
# sipro_inbnd_netinc_any_marginals_eom_xts <- get_sipro_inbnd_netinc_any_marginals_eom_xts()
# View(sipro_inbnd_netinc_any_marginals_eom_xts[, grep("^bank",colnames(sipro_inbnd_netinc_any_marginals_eom_xts), value = T)])
# 
# tail(sipro_inbnd_netinc_any_marginals_eom_xts[, grep("^bank",colnames(sipro_inbnd_netinc_any_marginals_eom_xts), value = T)])
#            banks_count banks_mktcap_div_mill_x_40 banks_rat_mktcap_o_netinc_q1_x_4 banks_pradchg_f04w_ann_wtd_mktcap
# 2017-08-31           9         2.2166960000000002               38.656110491071431                19.463693496988309
# 2017-09-29           1         0.1204440000000000               44.542899408284022               -38.159999999999997
# 2017-10-31          96        73.4735359999999957               15.893180127018187                24.847137116689197
# 2017-11-30          10         1.2360320000000000               22.588304093567253               -15.357971314658521
# 2017-12-29           1         0.1150120000000000               68.459523809523816                84.719999999999999
# 2018-01-31          98        70.7588279999999941               35.166446003005795                                NA
#            banks_prchg_netinc_q1q2
# 2017-08-31    -10.8457711442786078
# 2017-09-29     20.7142857142857153
# 2017-10-31      4.7173264617514556
# 2017-11-30     13.8103161397670551
# 2017-12-29    -37.8698224852071021
# 2018-01-31    -67.1173668164061468



get_sipro_inbnd_rat_chg_owe_chg_netinc_eom_xts <- function() {

  # NOTE USES : now_inbnd_stmtid_dateindex is not null
 
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

  message("Begin function get_sipro_inbnd_rat_chg_owe_chg_netinc_eom_xts")

  verify_connection()
  
  sipro_inbnd_netinc_marginals_eom <- dbGetQuery(con,
  
  "
  with 
  data_info as (
  select dateindex, 
    liab_q1,
    liab_q2,
    cl_q1,
    cl_q2,
    netinc_q1,
    netinc_q2,
    mktcap,
    now_inbnd_stmtid_dateindex
  from fe_data_store.si_finecon2 fe
  where 
    sp in ('500','400','600')
    and mktcap is not null
    and liab_q1 is not null and liab_q2 is not null and netinc_q1 is not null and netinc_q2 is not null
    and cl_q1 is not null and cl_q2 is not null
  ),
  data_info_agg as (
    select
      dateindex,
      sum(mktcap) sum_mktcap_all
      from data_info
      group by dateindex 
      order by dateindex
  )
  select -- data_info.dateindex, 
    to_timestamp(data_info.dateindex*3600*24)::date dateindex_dt,
    count(netinc_q1 + netinc_q2 + mktcap + liab_q1 + liab_q1 + cl_q1 + cl_q2) / 10.0 every_count_d_10,
    sum(mktcap/sum_mktcap_all) * 100  pr_mktcap,
    sum(mktcap) / 1000000.0 * 4.0 every_mktcap_div_mill_x_4,
    sum(mktcap) / sum(netinc_q1 * 4) every_rat_mktcap_o_netinc_q1_x_4,
    (sum(liab_q1) - sum(liab_q2)) / ( sum(netinc_q1) - sum(netinc_q2) ) * 100 rat_chg_liab_o_chg_netinc,
    (sum(  cl_q1) - sum(  cl_q2)) / ( sum(netinc_q1) - sum(netinc_q2) ) rat_chg_cl_o_chg_netinc
  from data_info, data_info_agg
  where data_info.dateindex = data_info_agg.dateindex and 
        now_inbnd_stmtid_dateindex is not null
  group by data_info.dateindex 
  order by data_info.dateindex
  "
  )
  
  temp <- as.matrix(sipro_inbnd_netinc_marginals_eom[ ,!colnames(sipro_inbnd_netinc_marginals_eom) %in% c("dateindex_dt"), drop = FALSE])
  rownames(temp) <- as.character(sipro_inbnd_netinc_marginals_eom[["dateindex_dt"]])
  # S3 dispatch as.xts.matrix ... will return with a non-Date index
  temp2 <- as.xts(temp)
  rm(temp)
  index(temp2) <- zoo::as.Date(index(temp2))
  sipro_inbnd_netinc_marginals_eom_xts <- temp2
  rm(temp2)

  # COULD BE GENERALIZED
  with(sipro_inbnd_netinc_marginals_eom_xts, {
    lapply( setdiff(ls(),"pr_mktcap"), function(x){ TTR::WMA(get(x), n = 3, wts = as.vector(coredata(get("pr_mktcap")))) })
  }) -> applied
  names(applied) <- paste0(setdiff(colnames(sipro_inbnd_netinc_marginals_eom_xts), "pr_mktcap"), "_wtd", "_", "pr_mktcap")
  results <- c(list(sipro_inbnd_netinc_marginals_eom_xts), applied)
  sipro_inbnd_netinc_marginals_eom_xts <-  do.call(merge.xts, results)
  
  Sys.setenv(TZ=oldtz)
  options(ops)

  message("End   function get_sipro_inbnd_rat_chg_owe_chg_netinc_eom_xts")

  return(sipro_inbnd_netinc_marginals_eom_xts)
    
}
# MORE an example of rolling noise, but an example hof handling WEIGHTED measures and ROLLING measures
# sipro_inbnd_rat_chg_owe_chg_netinc_eom_xts <- get_sipro_inbnd_rat_chg_owe_chg_netinc_eom_xts()
#  View(sipro_inbnd_rat_chg_owe_chg_netinc_eom_xts)


#############################################
### BEGIN METHOD TO ADD A *COMMON* COLUMN ###

# no aggregation
upload_mini_dbfs_no_future_look_to_db <- function(from_dir = "W:/AAIISIProDBFs", months_only_back = NULL, exact_near_month_end_dbf_dirs = NULL, decreasing_sort_order = TRUE, vacuum_reindex_every_x_seconds=3600) {

  
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  start_at_secs_since_UNIX_birth <- as.numeric(Sys.time())
  
  
  ops <- options()
  options(warn=1) # If 'warn' is one, warnings are printed as they occur. ( Because I can not print colors )
  
  if(is.null(exact_near_month_end_dbf_dirs)){
    as.integer(dir(from_dir))         ->     all_dbf_dirs
    ## They are not all 'near month end'
    # is_lwd_of_month(all_dbf_dirs)     -> lwd_all_dbf_dirs_tf 
    #
    # all_dbf_dirs[lwd_all_dbf_dirs_tf] ->     lwd_dbf_dirs
    all_dbf_dirs                        ->     near_month_end_dbf_dirs
  } else {
                      exact_near_month_end_dbf_dirs -> near_month_end_dbf_dirs
  }
  
  # latest to earliest 
  # NOTE: any *new* month, I have to iterate back (months_only_back = 13) 13 months to calculate any *new* future returns
  # 
  # index of lwd months
  seq_along(near_month_end_dbf_dirs) -> near_month_end_dbf_dirs_idx
  if(is.null(months_only_back)) { 
    if(is.null(decreasing_sort_order)) {
      # everything - do not bother to order them
      near_month_end_dbf_dirs -> near_month_end_dbf_dirs_ordered                             # no order at all
    } else {
      # everything - ordered                     # from args: default TRUE
      sort(near_month_end_dbf_dirs, decreasing = decreasing_sort_order)[near_month_end_dbf_dirs_idx]  -> near_month_end_dbf_dirs_ordered
    }
    
  } else { # months_only_back - ! is.null 
    
    if(is.null(decreasing_sort_order)) {
      # not ordered - just the *new(head)* month and the other exact entered 12 months redone (months_only_back = 13) 
      head(near_month_end_dbf_dirs, months_only_back) -> near_month_end_dbf_dirs_ordered     # no order at all
    } else {
      #    ordered - just the *new(head)* month and the             previous 12 months redone (months_only_back = 13)
      sort(near_month_end_dbf_dirs, decreasing = decreasing_sort_order)[head(near_month_end_dbf_dirs_idx, months_only_back)]  -> near_month_end_dbf_dirs_ordered
    }
  }
  
  # # load_us_bond_instruments
  # for_bonds_is_null_months_only_back_check_NOT_done <- TRUE
  
  ## # load_inbnd_stmtstats
  ## # for_inbnd_stmtstats_is_null_months_only_back_check_NOT_done <- TRUE
  
  # WANT TO SEE THIS HAPPENING
  # vacuum_analyze_reindex()
  
  for(dir_i in near_month_end_dbf_dirs_ordered) {
    
    Sys.sleep(2)
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    Sys.sleep(2)
    
    # ere_q
    #   si_cfq
    # 

    # columns CAN be named implicitely be regular expression
    verify_company_details(dateindex = c(dir_i),  table_f = "si_cfq", cnames_e = "^divpaid_q.$") -> si_all_g_df
    print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
    
    # if WANT to include AS an 'inbound' statistics(generally a good) idea ( ADD to this LINE )
    # DYNAMIC SQL
    # columns MUST be named explicitly
    print(dir_i);load_inbnd_stmtstats(dir_i, nowlast_columns = c("sales_q1", "netinc_q1", "ncc_q1", "assets_q1", "assets_q2", "tco_q1", "tcf_q1", "tci_q1", "ere_q1", "ca_q1", "cl_q1", "liab_q1", "ce_q1", "int_q1", "intno_q1", "cash_q1", "divpaid_q1"), support_dateindex_collection = sort(as.integer(dir(from_dir)), decreasing = TRUE)[dir_i>=  sort(as.integer(dir(from_dir)), decreasing = TRUE)][seq_len(min(sum(dir_i >=  sort(as.integer(dir(from_dir)), decreasing = TRUE)),11))], char_col_numeric_limit = 99999999999999.99) -> si_all_g_df
    print(dir_i);upsert(si_all_g_df, keys = c("company_id"))

    # IF INVOLVED IN THIS QUERY
    # STATIC SQL
    # ( and MODIFY this *STATIC* QUERY [x] NOW (add ere_q1 [x])AND permanently AND RUN )
    load_inbnd_stmtstats_division_aggregates(dir_i)
    
    Sys.sleep(2)
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    Sys.sleep(2)
    
    # vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth
    
  }
  
  # at the 'end of the 'monthly' load, then re-index everything
  # vacuum_analyze_reindex()

  Sys.setenv(TZ=oldtz)
  
  options(ops)
  
  return(invisible())
  
}
# 
# NOTE: CAN BE RAN ( RECOMMENDED )
# vacuum_analyze_reindex(); { upload_mini_dbfs_no_future_look_to_db(. . . }; vacuum_analyze_reindex()
# 
# *** ** IMPORTANT ** IMPORTANT ** better run OUTSIDE OF R-STUDIO ( MUCH MUCH MUCH FASTER ) ***
# DBI con 'bloated' problem
# upload_mini_dbfs_no_future_look_to_db(decreasing_sort_order = FALSE)
# vacuum_analyze_reindex()
# 
# if column does not exist in EARLIER DAYS (e.g. divpaid_q first appeared(ReadMe.txt) in 2012-12-31 15705) t
# then LOAD last ONE first
# upload_mini_dbfs_no_future_look_to_db(decreasing_sort_order = FALSE, exact_near_month_end_dbf_dirs = 99999)
# # FOLLOWED BY 
# upload_mini_dbfs_no_future_look_to_db(decreasing_sort_order = FALSE)
#   xor BETTER ( if I know when column first appeared )
# upload_mini_dbfs_no_future_look_to_db(decreasing_sort_order = FALSE, exact_near_month_end_dbf_dirs = sort(as.integer(dir("W:\\AAIISIProDBFs")))[ 15705 <= sort(as.integer(dir("W:\\AAIISIProDBFs")))])
# vacuum_analyze_reindex() 
# 
# e.g. restart from a HUNG
# upload_mini_dbfs_no_future_look_to_db(decreasing_sort_order = FALSE, exact_near_month_end_dbf_dirs = sort(as.integer(dir("W:\\AAIISIProDBFs")))[ 13756 <= sort(as.integer(dir("W:\\AAIISIProDBFs")))])
#   **
#   ALWAYS!! WHILE THE PROGRAM *IS* RUNNING - check that the IS LOADING actually *IS* loading
#   select STATISIC_q1, now_inbnd_stmtstat_STATISIC_q1 from fe_data_store.si_finecon2 where dateindex = <just finished loading this one>;
#   **
# vacuum_analyze_reindex()

# looking for ere_q1
# select dateindex, ere_q1, ere_qi from fe_data_store.si_finecon2 where dateindex = 12055

# looking for *whatever* ere_q1 derivatives ( assuming fe_data_store.si_finecon2 has data )
# -- get from pgadmin
# select 
#   dateindex,
#   sp_desc__sp500__sector_fin_desc__sectfinfinancial____sum_last_ere_q1_o_assets_q1_x1000,
#   sp_desc__sp500__sector_fin_desc__sectfinnotfinancial____sum_last_ere_q1_o_assets_q1_x1000,
#   sp_desc__sp500__sector_fin_desc__sectfinfinancial____sum_last_ere_q1_o_mktcap_x1000,
#   sp_desc__sp500__sector_fin_desc__sectfinnotfinancial____sum_last_ere_q1_o_mktcap_x1000,
#   sp_desc__sp500____sum_last_ere_q1_o_assets_q1_x1000,
#   sp_desc__sp500____sum_last_ere_q1_o_mktcap_x1000
# from fe_data_store.si_finecon2_aggregates where dateindex in (values (12055),(12083),(12111)) -- 12055 -- earliest empty
# order by dateindex;

# AFTER ( TODO PERMANENT - ADD ITEM TO LINE IN *ONE PLACE ONLY* [x] )
# verify_company_details(dateindex = c(dir_i),  table_f = "si_cfq", cnames_e = "^tco_q.$|^tcf_q.$|^tci_q.$|^ere_q.$") -> si_all_g_df

# AFTER ( TODO PERMANENT - MODIFY LINE [X]: ADD ', "ere_q1"' [X] )
# print(dir_i);load_inbnd_stmtstats(dir_i, nowlast_columns = c("sales_q1", "netinc_q1", "ncc_q1", "assets_q1", "assets_q2", "tco_q1", "tcf_q1", "tci_q1", "ere_q1", "ca_q1", "cl_q1", "liab_q1"), support_dateindex_collection = sort(as.integer(dir(from_dir)), decreasing = TRUE)[dir_i>=  sort(as.integer(dir(from_dir)), decreasing = TRUE)][seq_len(min(sum(dir_i >=  sort(as.integer(dir(from_dir)), decreasing = TRUE)),11))], char_col_numeric_limit = 99999999999999.99) -> si_all_g_df

### END METHOD TO ADD A *COMMON* COLUMN ###
###########################################


                                    
                                           # NO CHECK: I must verify
                                                                                                # that (1) exists AND (2) lwd
upload_lwd_sipro_dbfs_to_db <- function(from_dir = "W:/AAIISIProDBFs", months_only_back = NULL, exact_near_month_end_dbf_dirs = NULL, decreasing_sort_order = TRUE, exactly_only_future_returns = FALSE, exactly_only_aggregates = FALSE, exactly_only_aggregates_group_bys_only = FALSE, vacuum_reindex_every_x_seconds=3600, run_update_from_future_new_company_ids = TRUE) {

  # NOTE: to build from scratch
  # start from the earliest date (not default) and go thorugh the current date
  #   this fills in the lwd,lbd,eom dates needed for joins 
  # next(to build a new month), at the current date, go backwards 13 months 
  #   this fills in the weekly percent changes in future pct returns
  
    # next:NOTE: to update: update_from_future_new_company_ids
    # start at 15184 and go backwards to the beginning
    #   this updates the company_ids
    #   this fills in the weekly percent changes
    #   this updates the now/last 
    # next from the beginning, go forward up and through 15184
    #   this updates the now/last 
  
  # NOTE: last(current): to build a *new* month ( if done "NOTE: to build from scratch" )
  #    (to build a new month), at the current date, go backwards 13 months ( and fill in the weekly percent changes)
  
  # to do ALL OF THAT ABOVE SUPER REBUILD FROM SCRATCH in one long VERY VERY LONG session
  # 
  # need a sequence
  #
  #       (first to last(current)) + ( exactly_only_future_returns: last(current) to back 13 back to first ) 
  #     + (first to 15184 + 13   ) + ( exactly_only_future_returns: (15184 + 13) back to first )
  # XOR
  # 15184 + first to last(current) + ( exactly_only_future_returns: last(current) to back 13 back to first )
  
  # best last(current) tester
  # last and go back 3 + back 3 to last(current) see 4w,13w prices, see now/last
  # best 15184 tester
  # 15184 and back 3 + then go back up to 1584 see 4w,13w,26w,52w prices, see now/last
  # best 12055 tester
  # 12055 and go forward 3 + then go back 3
  
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  start_at_secs_since_UNIX_birth <- as.numeric(Sys.time())
  
  
  ops <- options()
  options(warn=1) # If 'warn' is one, warnings are printed as they occur. ( Because I can not print colors )
  
  if(is.null(exact_near_month_end_dbf_dirs)){
    as.integer(dir(from_dir))         ->     all_dbf_dirs
    ## They are not all 'near month end'
    # is_lwd_of_month(all_dbf_dirs)     -> lwd_all_dbf_dirs_tf 
    #
    # all_dbf_dirs[lwd_all_dbf_dirs_tf] ->     lwd_dbf_dirs
    all_dbf_dirs                        ->     near_month_end_dbf_dirs
  } else {
                      exact_near_month_end_dbf_dirs -> near_month_end_dbf_dirs
  }
  
  # latest to earliest 
  # NOTE: any *new* month, I have to iterate back (months_only_back = 13) 13 months to calculate any *new* future returns
  # 
  # index of lwd months
  seq_along(near_month_end_dbf_dirs) -> near_month_end_dbf_dirs_idx
  if(is.null(months_only_back)) { 
    if(is.null(decreasing_sort_order)) {
      # everything - do not bother to order them
      near_month_end_dbf_dirs -> near_month_end_dbf_dirs_ordered                             # no order at all
    } else {
      # everything - ordered                     # from args: default TRUE
      sort(near_month_end_dbf_dirs, decreasing = decreasing_sort_order)[near_month_end_dbf_dirs_idx]  -> near_month_end_dbf_dirs_ordered
    }
    
  } else { # months_only_back - ! is.null 
    
    if(is.null(decreasing_sort_order)) {
      # not ordered - just the *new(head)* month and the other exact entered 12 months redone (months_only_back = 13) 
      head(near_month_end_dbf_dirs, months_only_back) -> near_month_end_dbf_dirs_ordered     # no order at all
    } else {
      #    ordered - just the *new(head)* month and the             previous 12 months redone (months_only_back = 13)
      sort(near_month_end_dbf_dirs, decreasing = decreasing_sort_order)[head(near_month_end_dbf_dirs_idx, months_only_back)]  -> near_month_end_dbf_dirs_ordered
    }
  }
  
  # # load_us_bond_instruments
  # for_bonds_is_null_months_only_back_check_NOT_done <- TRUE
  
  ## # load_inbnd_stmtstats
  ## # for_inbnd_stmtstats_is_null_months_only_back_check_NOT_done <- TRUE
  
  # WANT TO SEE THIS HAPPENING
  # vacuum_analyze_reindex()
  # vacuum_analyze_reindex(index = "? ? ?")
  
  for(dir_i in near_month_end_dbf_dirs_ordered) {
    
    Sys.sleep(2)
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Beginning disk dbf dir: ",dir_i," ", dir_i," ****"))
    Sys.sleep(2)
    
                                                                                                            # no decision
    if((!exactly_only_future_returns && (!is.null(exactly_only_aggregates) && !exactly_only_aggregates)) || is.null(exactly_only_aggregates) ) {
  
      verify_company_basics(dateindex = c(dir_i)) -> si_all_g_df
      # only *NEW MONTH* data needs to be UPDATED
      if(run_update_from_future_new_company_ids) {
        update_from_future_new_company_ids(df = si_all_g_df, ref = dir_i) -> si_all_g_df
      }
      print(dir_i);upsert(si_all_g_df, keys = c("company_id")) # HERE #
  
      verify_company_details(dateindex = c(dir_i),  table_f = "si_psd", cnames_e = "^price$|^mktcap$|^split_fact$|^split_date$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
  
      verify_company_details(dateindex = c(dir_i),  table_f = "si_psd", cnames_e = "^prchg_\\d\\dw$") -> si_all_g_df
      upsert(si_all_g_df, keys = c("company_id"))
  
      verify_return_dates(dateindex = c(dir_i), months_limit = 38)  -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = NULL) # ONLY dateindex is the pk
  
      verify_company_details(dateindex = c(dir_i),  table_f = "si_isq", cnames_e = "^dps_q.$|^int_q.$|^intno_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
  
      verify_company_details(dateindex = c(dir_i),  table_f = "si_date", cnames_e = "^perend_q.$|^perlen_q.$|^pertyp_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
  
      # MAY? have not been reliable?
      verify_company_details(dateindex = c(dir_i),  table_f = "si_ee"  , cnames_e = "^date_eq0$|^qs_date$") -> si_all_g_df
      upsert(si_all_g_df, keys = c("company_id"))
      
      verify_company_details(dateindex = c(dir_i),  table_f = "si_mlt", cnames_e = "^bby_1t$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
  
      verify_company_details(dateindex = c(dir_i),  table_f = "si_cfq", cnames_e = "^tco_q.$|^tcf_q.$|^tci_q.$|^ere_q.$|^ce_q.$|^divpaid_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
    
      verify_company_details(dateindex = c(dir_i),  table_f = "si_bsq", cnames_e = "^ca_q.$|^cl_q.$|^liab_q.$|^cash_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))

    }
    
    if(exactly_only_future_returns) {
  
      # requires
      #   dateindexf##lwd, price, prchg_##w, perend_q#, dps_q#
      verify_week_often_week_returns(dir_i) -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
      
    }
                                                                                                            # no decision
    if((!exactly_only_future_returns && (!is.null(exactly_only_aggregates) && !exactly_only_aggregates)) || is.null(exactly_only_aggregates)  ) {
  
      verify_company_details(dateindex = c(dir_i),  table_f = "si_psdc", cnames_e = "^price_m00[1-9]$|^price_m01[0-7]$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
      
      # requires (above)
      #    price_m001 through price_m017
      verify_month_often_month_past_returns(dir_i,  months_limit = 17) -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
      
      verify_company_details(dateindex = c(dir_i),  table_f = "si_isq", cnames_e = "^sales_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
  
      verify_company_details(dateindex = c(dir_i),  table_f = "si_isq", cnames_e = "^netinc_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
      update_from_future_upd_netinc_q1(dateindex = c(dir_i))
      
      verify_company_details(dateindex = c(dir_i),  table_f = "si_cfq", cnames_e = "^ncc_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
      
      verify_company_details(dateindex = c(dir_i),  table_f = "si_bsq", cnames_e = "^assets_q.$") -> si_all_g_df
      print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
    
    }
                                                                         # no decision
    if((!is.null(exactly_only_aggregates) && exactly_only_aggregates) || is.null(exactly_only_aggregates)) {
      
                                                                                            # no decision
      if((!is.null(exactly_only_aggregates_group_bys_only) && !exactly_only_aggregates_group_bys_only) || is.null(exactly_only_aggregates_group_bys_only)) {

                #
        # support_dateindex_collection is the 
        # minimum of 11 months: current + ( 6 month Quarter period reporter with 4 month Q-10 report filing delay ) 
        #                           # current or earlier                               # current or up to 10 earlier
        print(dir_i);load_inbnd_stmtstats(dir_i, nowlast_columns = c("sales_q1", "netinc_q1", "ncc_q1", "assets_q1", "assets_q2", "tco_q1", "tcf_q1", "tci_q1", "ere_q1", "ca_q1", "cl_q1", "liab_q1", "ce_q1", "int_q1", "intno_q1", "cash_q1", "divpaid_q1"), support_dateindex_collection = sort(as.integer(dir(from_dir)), decreasing = TRUE)[dir_i>=  sort(as.integer(dir(from_dir)), decreasing = TRUE)][seq_len(min(sum(dir_i >=  sort(as.integer(dir(from_dir)), decreasing = TRUE)),11))], char_col_numeric_limit = 99999999999999.99) -> si_all_g_df
        print(dir_i);upsert(si_all_g_df, keys = c("company_id"))
        # 
        # 
      }
                                                                                                         # no decision
      if((!is.null(exactly_only_aggregates_group_bys_only) && exactly_only_aggregates_group_bys_only) || is.null(exactly_only_aggregates_group_bys_only)) {

                      # uses now_inbnd_stmtstat last_inbnd_stmtstat
        # since MANY SQLs upsertS are done inside                                      # if NOT an UPDATE on COMPANY_ID then I CAN go on the OUTSIDE
        # load_division_aggregated_now_last_mktcap_per_company_id(dateindex = dir_i) # # head(lwd_dbf_dirs_ordered,1) ( BUT WILL NOT do this now )
        # WORKING(but not used): load_division_aggregated_now_last_mktcap_per_company_id
        ## load_division_aggregated_now_last_mktcap_per_company_id(dateindex = dir_i)
        # INTERNALLY does MANY upserts
        # vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth
        # 
        # uses
        # load_inbnd_stmtstats
        # load_division_aggregated_now_last_mktcap_per_company_id
        # since MANY SQLs upsertS are done inside
        # 
        # *** WRITES TO ... si_finecon2_aggregates ... ***
        # 
        # MATH_LOGIC WRONG:    : load_division_aggregated_per_dateindex ( WRONG BECAUSE: ALL columns USED must be NOT NULL everwhere )
        ## load_division_aggregated_per_dateindex(dateindex = dir_i)
        # INTERNALLY does MANY upserts
        #
        # vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth
        
        # REPLACEMENT of WORKING(but not used): load_division_aggregated_now_last_mktcap_per_company_id
        # REPLACEMENT of MATH_LOGIC WRONG:    : load_division_aggregated_per_dateindex ( WRONG BECAUSE: ALL columns USED must be NOT NULL everwhere )
        load_inbnd_stmtstats_division_aggregates(dir_i)

      }
      
    }
    
    Sys.sleep(2)
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    message(paste0("**** Ending disk dbf dir: ",dir_i," ", dir_i," ****"))
    Sys.sleep(2)
    
    # vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds) ->  start_at_secs_since_UNIX_birth
    # vacuum_reindex_check(start_at_secs_since_UNIX_birth, vacuum_reindex_every_x_seconds, index = "? ? ?") ->  start_at_secs_since_UNIX_birth
    
  }
  
  # # WARNING: NOT 'dir_i TIME by database BASED' ( SHOULD REWRITE? IF POSSIBLE? )
  # # NOTE: IF missed *MANY* months in LOADING cheaper to REBUILD the entire DATABASE
  # if(for_bonds_is_null_months_only_back_check_NOT_done && !is.null(months_only_back)) {
  #   for_bonds_is_null_months_only_back_check_NOT_done <- FALSE
  #   # NOTE:US BONDS SOME GAPS DO EXIST IN THE DATA ( TO DO [ ] DETECT NULL AND APPROXIMATION [ ]
  #   load_us_bond_instruments(us_bonds_year_back = (months_only_back %/% 12 + 2) ) # MIMIMUM OF 2 YEARS OF DATA
  # }
  # 
  # if(for_bonds_is_null_months_only_back_check_NOT_done && is.null(months_only_back)) {
  #   for_bonds_is_null_months_only_back_check_NOT_done <- FALSE
  #   # NOTE: US BONDS SOME GAPS DO EXIST IN THE DATA ( TO DO [ ] DETECT NULL AND APPROXIMATION [ ]
  #   load_us_bond_instruments() # ALL OF the data
  # }

  # at the 'end of the 'monthly' load, then re-index everything
  # vacuum_analyze_reindex()
  
  Sys.setenv(TZ=oldtz)
  
  options(ops)
  
  return(invisible())
  
}

# --YES: THAT IS HOW TO DO IT repair_compact/coalesce_reindex--
# --
# --vacuum analyze verbose  fe_data_store.si_finecon2;
# --reindex (verbose) table fe_data_store.si_finecon2;
# 
# --SOLUTION: MACHINE IS HOSED AND INDEXES NOT WORKING---
# --check indexes if not working
# --  explain
# --  select distinct dateindex from si_finecon2 order by dateindex desc;
# --TYPICALLY LESS THAN 20 MINTUES ( BUT 'AS LONG AS IT TAKES' )
# --vacuum analyze verbose fe_data_store.si_finecon2;reindex (verbose) table fe_data_store.si_finecon2;
# -- (another psql prompt)
# --select relid::regclass, * from pg_stat_progress_vacuum;
# --check indexes if not working
# --explain
# --select distinct dateindex from si_finecon2 order by dateindex desc;

# NOTE: ** ANY BELOW CAN BE RUN **
# vacuum_analyze_reindex(); { upload_lwd_sipro_dbfs_to_db(. . . }; vacuum_analyze_reindex()

# 
# upload_lwd_sipro_dbfs_to_db(from_dir = "W:/AAIISIProDBFs", months_only_back = NULL, exact_near_month_end_dbf_dirs = NULL, decreasing_sort_order = TRUE)
#
# upload_lwd_sipro_dbfs_to_db() # HARD NOTE: THIS DOES EVERYTHING - ALL (14) YEARS
# upload_lwd_sipro_dbfs_to_db(months_only_back = 13)
# upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = 16678) 
# vacuum_analyze_reindex()

#
# exactly what I want 
#                     exactly in this order ( processed left to right in for-loop)
# upload_lwd_sipro_dbfs_to_db(                      exact_near_month_end_dbf_dirs = any # of elements, decreasing_sort_order = NULL )
# vacuum_analyze_reindex()

# tester
# upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = c(17409,17378,17347,17317, 17284, 17317,17347,17378,17409), decreasing_sort_order = NULL )
# vacuum_analyze_reindex()

# probably not useful
# head of 4 elements, eactly in this order ( processed left to right in for-loop)
# upload_lwd_sipro_dbfs_to_db(months_only_back = 4, exact_near_month_end_dbf_dirs = any # of elements, decreasing_sort_order = NULL)
# vacuum_analyze_reindex()

# tester
# { upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = c(       17409,17378,17347,17317, 17284, decreasing_sort_order = NULL, exactly_only_future_returns = TRUE)
#   upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = c(17284, 17317,17347,17378,17409),       decreasing_sort_order = NULL) 
# }
# vacuum_analyze_reindex()

# NOTE: CAN BE RAN ( RECOMMENDED )
# { upload_lwd_sipro_dbfs_to_db(. . . }
# vacuum_analyze_reindex()
# 
# tester                                                                  # assuming all of the previous months isq,bsq,cfq have been loaded
# upload_lwd_sipro_dbfs_to_db(                                             months_only_back = 5, exactly_only_future_returns = TRUE) 

# typical NEW month
# after INSTALLING latest 'last business day'(MUST BE) SIPro. . .exe installer # SEE OTHER INSTRUCTOINS ### MONTHLY METHOD BEGINS ####
# after checking 'latest' 'new' day using as.integer(getAAIISIProDate())       # SEE OTHER INSTRUCTIONS ### MONTHLY METHOD BEGINS ####
# after COPIED new files using copyAAIISIProDBFs                               # SEE OTHER INSTRUCTIONS ### MONTHLY METHOD BEGINS ####
# BEST: run the following at the COMMAND PROMPT
# after checking W:\AAIISIProDBFs looking for the latest new directory
#
 # # typical *new month*                                # *new(top) month*  # assuming all of the previous months isq,bsq,cfq have been loaded                                                    
 # { upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = c(NEW_INDEX_HERE), exactly_only_aggregates = NULL, exactly_only_aggregates_group_bys_only = NULL,  decreasing_sort_order = FALSE)  
 #   upload_lwd_sipro_dbfs_to_db(                                                months_only_back = 14, exactly_only_future_returns = TRUE, run_update_from_future_new_company_ids = FALSE) 
 # }
# vacuum_analyze_reindex()

# loading 'inbnd' and next 'group by' aggregates (from the beginning)
# upload_lwd_sipro_dbfs_to_db(exactly_only_aggregates = TRUE               , decreasing_sort_order = FALSE)
# vacuum_analyze_reindex()

# loading                  'group by' aggregates (from the beginning)
# upload_lwd_sipro_dbfs_to_db(exactly_only_aggregates_group_bys_only = TRUE, decreasing_sort_order = FALSE)
# vacuum_analyze_reindex()

# maybe for loading or re-testing the (past to now) aggregate calculations
# upload_lwd_sipro_dbfs_to_db(                                              months_only_back = 5, exactly_only_aggregates = TRUE, decreasing_sort_order = FALSE)
# vacuum_analyze_reindex()

# all returns from now through the past to the beginning
# upload_lwd_sipro_dbfs_to_db(exactly_only_future_returns = TRUE)
# vacuum_analyze_reindex()

# load future returns going backward beyond a certain point
# upload_lwd_sipro_dbfs_to_db( exact_near_month_end_dbf_dirs = c(sort(as.integer(dir("W:\\AAIISIProDBFs")))[  12664 > sort(as.integer(dir("W:\\AAIISIProDBFs")))]), exactly_only_future_returns = TRUE)
# vacuum_analyze_reindex()

### ###

# load everything from the beginning

# close RStudio
# db.disconnect(conn.id = SEE_RSTUDIO)

# pgAdmin (as postgres on postgres)
# pgAdmin (as postgres on postgres)
# ... rest ...

# MANUALLY ENAME finance_econ TO finance_econ_old_yymmdd

# CREATE DATABASE finance_econ;

# COMMENT ON DATABASE finance_econ
#  IS 'finance and econ database';

# pgAdmin ( as finance_econ on postgres )
# pgAdmin ( as finance_econ on postgres )
# ... rest ....
# (FROM within NEW database finance_econ)

# CREATE SCHEMA fe_data_store
#   AUTHORIZATION postgres;

# COMMENT ON SCHEMA fe_data_store
#   IS 'finance and economics data store';

# CREATE EXTENSION plr; -- sif_agrep(pattern text, x text)

# start RStudio

# verify_connection()
# sanity check
# db.q("select count(*) from si_finecon2;", conn.id = cid)
# ERROR:  relation "si_finecon2" does not exist
# dbExistsTable(con, "si_finecon2")
# [1] FALSE

# rm(list=setdiff(ls(all.names=TRUE),c("con","cid"))); debugSource('W:/R-3.4._/finecon01.R'); debugSource('W:/R-3.4._/goodsight01.R');verify_connection();options(upsert_temp_is_temporary=Inf)

# everything
# 
# { 
#   # upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = c(15184)) # 
#   upload_lwd_sipro_dbfs_to_db(decreasing_sort_order = FALSE) # bsq,isq,cfq # first to last(current)
#   upload_lwd_sipro_dbfs_to_db(exactly_only_future_returns = TRUE)          # last(current) to first
# }

# everything after a specific date ( DO NOT FORGET: "decreasing_sort_order = NULL" )
# 'past to future' data loadings
# 
# {
#   upload_lwd_sipro_dbfs_to_db( exact_near_month_end_dbf_dirs = c(15184, sort(as.integer(dir("W:\\AAIISIProDBFs")))[ 12384 < sort(as.integer(dir("W:\\AAIISIProDBFs")))]), decreasing_sort_order = NULL)
#   upload_lwd_sipro_dbfs_to_db(exactly_only_future_returns = TRUE)  # last(current) to first
# }

### ###




# # untried BUT truncate table is BETTER for company_id/ticker SYSTEM change PROBLEMS
# # upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = sort(all_load_days_lwd[all_load_days_lwd <= (15155 + 400)], decreasing = TRUE))



# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid")))
# debugSource('W:/R-3.4._/finecon01.R')





# add 'past change'(return-ish) data 04W, 13w, 26w, 52w in 'xts/quantmod style' (ignore weekend days)
chgs_XXw_ann <- function(xtsobj = NULL) {
  
  # uses xts
  
  # get the name e.g. "us_bonds"
  xtsobj_name <- as.character(substitute(xtsobj))
  
  chgs_XXw_ann_inner <- function(xtsobj = NULL, xtsobj_name = NULL) {
    

    xtsobj_chg_04w_ann <- (xtsobj - xts::lag.xts(xtsobj, 1*22)) / abs( xts::lag.xts(xtsobj, 1*22)) * 100 * 12
    colnames(xtsobj_chg_04w_ann) <- paste0(colnames(xtsobj_chg_04w_ann),"_prchg_04w_ann")
    #
    xtsobj_chg_13w_ann <- (xtsobj - xts::lag.xts(xtsobj, 3*22)) / abs( xts::lag.xts(xtsobj, 3*22)) * 100 *  4
    colnames(xtsobj_chg_13w_ann) <- paste0(colnames(xtsobj_chg_13w_ann),"_prchg_13w_ann")
    #
    xtsobj_chg_26w_ann <- (xtsobj - xts::lag.xts(xtsobj, 6*22)) / abs( xts::lag.xts(xtsobj, 6*22)) * 100 *  2
    colnames(xtsobj_chg_26w_ann) <- paste0(colnames(xtsobj_chg_26w_ann),"_prchg_26w_ann")
    #
    xtsobj_chg_52w_ann <- (xtsobj - xts::lag.xts(xtsobj,12*22)) / abs( xts::lag.xts(xtsobj,12*22)) * 100 *  1
    colnames(xtsobj_chg_52w_ann) <- paste0(colnames(xtsobj_chg_52w_ann),"_prchg_52w_ann")
    
    xtsobj_chg <- xts::merge.xts(xtsobj_chg_04w_ann,xtsobj_chg_13w_ann,xtsobj_chg_26w_ann,xtsobj_chg_52w_ann)
    return(xtsobj_chg)
    
  }
  return(chgs_XXw_ann_inner(xtsobj = xtsobj, xtsobj_name = xtsobj_name))
  
}    
# note XXW will have NAs becuase the previous time(XXw) data is not available ( so I want to load the previous year)
# us_bonds_chgs <- chgs_XXw_ann(us_bonds)

# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid","us_bonds","us_bonds_chgs")))

# adds columns: "dateindex", "dateindexlwd", "dateindexeom"
# from an xts, prepares a df (meant later to be uploaded)
#
# split_search/split_replace - to slight change the instrument name ( meant as a pre-reqisite to split_sep )
# split_sep                  - split the instrument name into two pieces 
# e.g. us_bonds_30y_chg_04w_ann -> us_bonds_30y.chg_04w_ann -> two columns: us_bonds_30y chg_04w_ann(then many columsn of slight diff titles)
xtsobjs_2_db_ready_df <- function(xtsobj = NULL, split_search = NULL, split_replace = NULL, split_sep = NULL) {
  
  ops <- options()
  options(warn = 1)
  
  # I can only look at yesterdays data tomorrow
  # remove THAT(IF EXISTS) to.monthly 'FUTURE measure raised'
  if(tail(xts:::index.xts(xtsobj),1) >= Sys.Date()) xtsobj <- xtsobj[-NROW(xtsobj),]

  # uses tidyr stringr DataCombine to.monthly.lwd
  
  cdata <- xts:::coredata.xts(xtsobj)
  
  # common columns
  cdata <- as.data.frame(cdata,stringsAsFactors = FALSE)
  cdata[,c("dateindex")   ] <-        as.integer(xts:::index.xts(xtsobj)) 

  cdata[,c("dateindexyear")]      <- yr_of_month(    as.integer(xts:::index.xts(xtsobj)), within_back = 0)
  cdata[,c("dateindexyearmonth")] <- yrmnth_of_month(as.integer(xts:::index.xts(xtsobj)), within_back = 0)
  
  cdata[,c("dateindexmonthsincebirth")] <- mnth_since_birth(as.integer(xts:::index.xts(xtsobj)), within_back = 0)
  
  cdata[,c("dateindexmonth")]     <- mnth_of_month(  as.integer(xts:::index.xts(xtsobj)), within_back = 0)
  cdata[,c("dateindexlbd")] <- lbd_of_month(         as.integer(xts:::index.xts(xtsobj)), within_back = 0)

  cdata[,c("dateindexlwd")] <- lwd_of_month(         as.integer(xts:::index.xts(xtsobj)), within_back = 0) # not use the coredata(I do not need)
  cdata[,c("dateindexeom")] <- last_day_of_month(xts:::index.xts(xtsobj), within_back = 0) 
  
  dfobj <- DataCombine::MoveFront(cdata, Var=c("dateindex", "dateindexyear", "dateindexyearmonth", "dateindexmonthsincebirth", "dateindexmonth", "dateindexlbd", "dateindexlwd", "dateindexeom")); rm(cdata)
  
  # reshape
  gathered <- tidyr::gather(dfobj, instrument, instrument_value, -dateindex, -dateindexyear, -dateindexyearmonth, -dateindexmonthsincebirth, -dateindexmonth, -dateindexlbd, -dateindexlwd, -dateindexeom)
  # need later to separate on the dot
  if(!is.null(split_search)) gathered$instrument <- stringr::str_replace(gathered$instrument,split_search,split_replace)
  # into columns called "instrument" and "change"  "change" column has MANY values "chg_XXw_ann"
  if(!is.null(split_sep)) separated <- tidyr::separate(gathered, col = instrument, into = c("instrument", "instrument_measure"), sep = split_sep) # "[.]" XOR "\\."
  # put "instrument_value" column MANY values "chg_XXw_ann" into EACHEs own column
  if(exists("separated"))  result <- spreaded <- tidyr::spread(separated, instrument_measure, instrument_value)
  if(!exists("separated")) result <- gathered
  
  options(ops)
  
  return(result)
  
}

# # us_bonds
# dateindex dateindexlwd dateindexeom   instrument instrument_value
# 165     17165        17165        17166 us_bonds_30y             3.06
# 166     17197        17197        17197 us_bonds_30y             3.05
# ...
# 
# # us_bonds_chgs
# dateindex dateindexlwd dateindexeom   instrument chg_04w_ann chg_13w_ann chg_26w_ann chg_52w_ann
# 155     17284        17284        17286 us_bonds_30y   -12.04013   -18.06452    35.85657    15.62500
# 156     17284        17284        17286  us_bonds_3m    30.76923   240.00000   270.58824   247.82609



# typically(in this case) I only care about measures that fall on the last weekday of the month
# 
# us_bonds      <- to.monthly.lwd(us_bonds)
# us_bonds_chgs <- to.monthly.lwd(us_bonds_chgs) 
# add columns, format df
# 
# us_bonds      <- xtsobjs_2_db_ready_df(us_bonds)
# us_bonds_chgs <- xtsobjs_2_db_ready_df(us_bonds_chgs, split_search = "_chg", split_replace = ".chg",  split_sep = "[.]") # XOR "\\."


load_instruments <- function(dfobj = NULL, no_update_earliest_year = NULL) {
  
  load_instruments_inner <- function(dfobj =NULL, no_update_earliest_year = NULL) {
    
    require(stringi)
    require(stringr)
    require(PivotalR)
    
    if(is.null(no_update_earliest_year)) no_update_earliest_year <- FALSE
    
    # no numeric greater than 999999.99
    for (colnames_i in colnames(dfobj)) {
      col_vector_data <- unlist(as.vector(dfobj[,colnames_i]))
      if(is.numeric(col_vector_data) && !is.integer(col_vector_data)) {
        within( dfobj, { 
          assign( colnames_i, 
                  ifelse(get(colnames_i) > 999999.99,999999.99, get(colnames_i)) 
          )      } 
        ) -> dfobj
      }
    } 
    
    
    verify_connection()
    
    # eventually
    # CAN GET HUNG UP HERE!!
    message("Begin - drop table if exists upsert_temp")
    {function() { db.q("drop table if exists upsert_temp", conn.id = cid) }} -> drop_upsert_temp
    # TRY LESSEN THE CONGESTION 
    # try( { db.q("delete from upsert_temp;", conn.id = cid) }, silent = TRUE )
    # try( { db.q("truncate table upsert_temp;", conn.id = cid) }, silent = TRUE )
    db.q("select pg_sleep(1);", conn.id = cid)
    drop_upsert_temp()
    db.q("select pg_sleep(1);", conn.id = cid)
    dbWriteTable(con, "upsert_temp", temporary = if(!is.null(getOption("upsert_temp_is_temporary"))) { TRUE } else { FALSE }, value = dfobj, append = FALSE, row.names = FALSE)
    db.q("select pg_sleep(1);", conn.id = cid)
    
    # garantee that upsert_Temp values are unique
    db.q("create unique index if not exists upsert_temp_dateindex_instruments_key on upsert_temp(dateindex, instrument);", conn.id = cid)
    
    
    db.q("
         create table if not exists fe_data_store.instruments
         (
         dateindex integer,
         dateindexyear integer,
         dateindexyearmonth integer,
         dateindexmonthsincebirth integer,
         dateindexmonth integer,
         dateindexlbd integer,
         dateindexlwd integer,
         dateindexeom integer,
         instrument text,
         instrument_value numeric(8,2),
         prchg_04w_ann numeric(8,2),
         prchg_13w_ann numeric(8,2),
         prchg_26w_ann numeric(8,2),
         prchg_52w_ann numeric(8,2)
         );
         ", conn.id = cid)
    
    db.q("create unique index if not exists instruments_dateindex_instrument_key on instruments(dateindex, instrument);", conn.id = cid)
    
     # want to NOT UPDATE the earliest year of 2( or 3) ( will be incomplete becuase no earlier data to calculate the XXw performance )
    earl_loaded_year <-  db.q("select min(date_part('year', to_timestamp(dateindex*3600*24)::date)) earl_loaded_year from upsert_temp;", nrows = -1, conn.id = cid)
    earl_loaded_year <- as.character(earl_loaded_year)
    
    no_update_earliest_year_f <- function(no_update_earliest_year = NULL, earl_loaded_year = NULL) {
      if(no_update_earliest_year == TRUE) { 
        " where date_part('year', to_timestamp(dateindex*3600*24)::date) > " %s+% earl_loaded_year 
      } else { 
          "" 
      }
    }
    
    # NOTE: some NON-past DATASOURCE do NOT NEED their earlier data removed ( e.g. no-over-time-change-data )
    db.q("insert into 
             instruments(" %s+% str_c(colnames(dfobj), collapse = ", ")  %s+% ")
               select    " %s+% str_c(colnames(dfobj), collapse = ", ")  %s+% " from upsert_temp " %s+% no_update_earliest_year_f(no_update_earliest_year, earl_loaded_year) %s+% "
         on conflict(dateindex, instrument)
         do update set ( " %s+% str_c(colnames(dfobj), collapse = ", ")  %s+% " ) = 
                       ( " %s+% str_c(str_c("excluded.",colnames(dfobj)), collapse = ", ") %s+% " );", conn.id = cid)
    
    # eventually
    # CAN GET HUNG UP HERE!!
    message("Begin - drop table if exists upsert_temp")
    {function() { db.q("drop table if exists upsert_temp", conn.id = cid) }} -> drop_upsert_temp
    # TRY LESSEN THE CONGESTION 
    # try( { db.q("delete from upsert_temp;", conn.id = cid) }, silent = TRUE )
    # try( { db.q("truncate table upsert_temp;", conn.id = cid) }, silent = TRUE )
    db.q("select pg_sleep(1);", conn.id = cid)
    drop_upsert_temp()
    db.q("select pg_sleep(1);", conn.id = cid)
    
    return(TRUE)
    
  }
  return(load_instruments_inner(dfobj = dfobj, no_update_earliest_year = no_update_earliest_year))

}

# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid","us_bonds","us_bonds_orig","us_bonds_chgs","us_bonds_chgs_orig")))

# load_instruments(us_bonds)
# early year does not have past-change data so TOO MANY nulls/NAs
# load_instruments(us_bonds_chgs, no_update_earliest_year = TRUE)

# IF us_bonds_year_back = NULL then load everything from the BEGINNING'
# 1 - this year                   (  'no_update_earliest_year = TRUE' will load an extra year )
# 2 - this year and previous year (  'no_update_earliest_year = TRUE' will YET load anOTHER extra year )
# 3 ... etc ...
#
# called like
# 
# > # months_back %/% 12 + 2 # min always
# > #          13 %/% 12 + 2
# >            13 %/% 12 + 2
# [1] 3
# 



# go out to th web, get the data, return the data into an xts object
retrieve_us_bonds <- function(when = NULL){
  
  retrieve_us_bonds_inner <- function(when = NULL) {
    
    # uses XML zoo xts
    
    # EVERYTHING ( SINCE 1990 )
    base_url        <- "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData"
    url_when_filter <- "?$filter=year(NEW_DATE)%20eq%20"
    run_url         <- base_url
    
    # when(character vectore of size 1): "all" xor "recent"(this year)(default),  xor "<specific year>"
    when_processed <- FALSE
    if((when_processed == FALSE) && (is.null(when) || (when == "recent"))) { 
      run_url <- paste0(run_url,url_when_filter,format(Sys.Date(),"%Y")) 
      when_processed <- TRUE
    }
    if((when_processed == FALSE) && (!is.null(when) && (when == "all"))) { 
      run_url <- run_url 
      when_processed <- TRUE
    }
    
    if((when_processed == FALSE) && (!is.null(when) && (nchar(when) == 4))) { # e.g. specific  year "2010"
      run_url <- paste0(run_url,url_when_filter,when)
      when_processed <- TRUE
    }
    
    # process: go get the data
    treasury.xml <- XML::xmlParse(run_url)
    
    # function to process
    xml.field <- function(name) {
      XML::xpathSApply(XML::xmlRoot(treasury.xml), paste0('//ns:entry/ns:content//d:', name),
                       function(x) {XML::xmlValue(x)},
                       namespaces = c(ns = 'http://www.w3.org/2005/Atom',
                                      d = 'http://schemas.microsoft.com/ado/2007/08/dataservices'))
    }
    
    us_bonds <- data.frame(
      us_bonds_1m = as.numeric(xml.field('BC_1MONTH')),
      us_bonds_3m = as.numeric(xml.field('BC_3MONTH')),
      us_bonds_6m = as.numeric(xml.field('BC_6MONTH')),
      us_bonds_1y = as.numeric(xml.field('BC_1YEAR')),
      us_bonds_3y = as.numeric(xml.field('BC_3YEAR')),
      us_bonds_5y = as.numeric(xml.field('BC_5YEAR')),
      us_bonds_7y = as.numeric(xml.field('BC_7YEAR')),
      us_bonds_10y = as.numeric(xml.field('BC_10YEAR')),
      us_bonds_20y = as.numeric(xml.field('BC_20YEAR')),
      us_bonds_30y = as.numeric(xml.field('BC_30YEAR'))
    )
    
    row.names(us_bonds) <- zoo::as.Date( strptime(xml.field('NEW_DATE'), format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC') )
    # non-Date index
    us_bonds <- xts::as.xts(as.matrix(us_bonds)) # df SO NOT xts:::as.matrix.xts
    
    # index type will be 'Date'
    us_bonds <- xts::xts(xts:::coredata.xts(us_bonds),zoo::as.Date( xts:::index.xts(us_bonds)))
    
    return(us_bonds)
    
  }
  
  return(retrieve_us_bonds_inner(when = when))
  
}
# # this year
# # us_bonds <- retrieve_us_bonds() 
# # us_bonds <- retrieve_us_bonds("2016") # so from a specific year e.g. 2016 ONLY
# # all since 1990
# # us_bonds <- retrieve_us_bonds("all")
# #
# # note XXW will have NAs becuase the previous time(XXw) data is not available 
# #   ( so I want to load the previous year)
# # typically ( often )
#
#             # now(this year up to today)                                # all of last year
# us_bonds <- xts::rbind.xts(retrieve_us_bonds(format(Sys.Date(),"%Y")),  retrieve_us_bonds( as.character(as.integer(format(Sys.Date(),"%Y")) -1) ) )
#



load_us_bond_instruments <- function(us_bonds_year_back = NULL) {
 
  message(gsub("\"","",capture.output(match.call())))
  print(us_bonds_year_back)
  
  load_us_bond_instruments_inner <- function(us_bonds_year_back = NULL) {
 
    print("BEGIN LOADING US BONDS")
    
    
    # this year ( NULL is THIS YEAR ONLY! DIFFERNT FROM OTHERS )
    # us_bonds <- retrieve_us_bonds() 
    
    #
    # # us_bonds <- retrieve_us_bonds("2016") # so from a specific year e.g. 2016 ONLY
    
    # all since 1990
    if (is.null(us_bonds_year_back)) { 
      print(paste0("BEGIN RETRIEVING US BONDS OF ALL YEARS"))
      us_bonds <- retrieve_us_bonds("all") 
      print(paste0("END RETRIEVING US BONDS OF ALL YEARS"))
    }

    # note XXW will have NAs becuase the previous time(XXw) data is not available 
    #    ( so I want to load the previous year)
    # typically ( often )
    #
    if(!is.null(us_bonds_year_back) && (us_bonds_year_back == 2)) {
      print(paste0("BEGIN RETRIEVING US BONDS OF LAST TWO YEARS"))
      #             # now(this year up to today)                                # all of last year
      us_bonds <- xts::rbind.xts(retrieve_us_bonds(format(Sys.Date(),"%Y")),  retrieve_us_bonds( as.character(as.integer(format(Sys.Date(),"%Y")) -1) ) )
      print(paste0("END RETRIEVING US BONDS OF LAST TWO YEARS"))
    }
    
    if(!is.null(us_bonds_year_back) && (us_bonds_year_back > 2)) {
      
      # > as.character(as.integer(format(Sys.Date(),"%Y")) -(0:(us_bonds_year_back -1)))
      # [1] "2017" "2016" "2015"
      us_bonds_year_back_range <- as.character(as.integer(format(Sys.Date(),"%Y")) -(0:(us_bonds_year_back -1)))
      us_bonds_data_list <- list()

      # Reduce
      for(us_bonds_year_back_range_i in us_bonds_year_back_range) {
        
        print(paste0("BEGIN RETREIVING US BONDS YEAR:",us_bonds_year_back_range_i))
        curr_item <- retrieve_us_bonds(us_bonds_year_back_range_i)
        print(paste0("END RETREIVING US BONDS YEAR:",us_bonds_year_back_range_i))
        Sys.sleep(3.0)
        us_bonds_data_list <- append(us_bonds_data_list,list(curr_item)) # 3stra list prevents df from collapsing
      }
      us_bonds <- do.call(xts::rbind.xts,us_bonds_data_list)
      
    }
     
    # note XXW will have NAs becuase the previous time(XXw) data is not available ( so I want to load the previous year)
    print(paste0("BEGIN CALCULATING US BONDS CHANGE RATES"))
    us_bonds_chgs <- chgs_XXw_ann(us_bonds)
    print(paste0("END CALCULATING US BONDS CHANGE RATES"))
    
    
    # typically(in this case) I only care about measures that fall on the last weekday of the month
    # 
    print(paste0("BEGIN US BONDS TO.MONTHLY"))
    us_bonds      <- to.monthly.lwd(us_bonds)
    us_bonds_chgs <- to.monthly.lwd(us_bonds_chgs) 
    print(paste0("END US BONDS TO.MONTHLY"))
    # add columns, format df
    # 
    print(paste0("BEGIN US BONDS XTS TO DF"))
    us_bonds      <- xtsobjs_2_db_ready_df(us_bonds)
    us_bonds_chgs <- xtsobjs_2_db_ready_df(us_bonds_chgs, split_search = "_prchg", split_replace = ".prchg",  split_sep = "[.]") # XOR "\\."
    print(paste0("END US BONDS XTS TO DF"))
    
    print(paste0("BEGIN US BONDS LOAD INTO POSTGRESQL DATABASE"))
    load_instruments(us_bonds)
    # early year does not have past-change data so TOO MANY nulls/NAs
    load_instruments(us_bonds_chgs, no_update_earliest_year = TRUE)
    print(paste0("END US BONDS LOAD INTO POSTGRESQL DATABASE"))
    
    print("END LOADING US BONDS")
   
  }
  return(load_us_bond_instruments_inner(us_bonds_year_back = us_bonds_year_back))
  
}
# load_us_bond_instruments() # ALL OF the data
# load_us_bond_instruments(us_bonds_year_back = 3)
# months_back <- 13; load_us_bond_instruments(us_bonds_year_back = (months_back %/% 12 + 2) )


numb.digits.left.of.decimal <- function(x) {

  res <- floor( log10( abs(x) ) ) + 1
  res <- as.integer(ifelse(res > 0, res, 0 ))
  
  return(res)
  
}

# numb.digits.left.of.decimal(c(9999.00,0.0000,-9999.00))
# [1] 4 0 4



# independent function currenly not used for anything
# 
# inbound tibble/data.frame/xts object
# note: this tblobj is expected to be of class data.frame
load_obj_direct <- function(tblobj = NULL, key_columns = NULL) {

  # have to grab right away
  tblobj_name <- tolower(as.character(substitute(tblobj)))
  
  load_obj_direct_inner <- function(tblobj =NULL, key_columns = NULL) {
    
    # uses package functions: DataCombine::MoveFront, xts::xtsible, xts:::index.xts
    # uses user functions: verify_connection, lwd_of_month, last_day_of_month, numb.digits.left.of.decimal
    
    ops <- options()
    options(warn = 1)
    
    require(stringi)
    require(stringr)
    require(PivotalR)
    
    # as.vector unlist data.frame drop = TRUE
    avud <- function( tblobj, col_name) { 
      as.vector(unlist(tblobj[, col_name, drop = TRUE]))  
    }
    
    if(is.null(key_columns)) stop("paremeter key_columns must be provided")
    key_columns <- tolower(key_columns)
    key_columns <- stringr::str_replace_all(key_columns,"[.]","_")
    key_columns <- stringr::str_replace_all(key_columns,  " ","_")
    
    # make all column names lower case 
    # replace does with underscore
    colnames(tblobj) <- tolower(colnames(tblobj))
    colnames(tblobj) <- stringr::str_replace_all(colnames(tblobj),"[.]","_")
    colnames(tblobj) <- stringr::str_replace_all(colnames(tblobj),  " ","_")
    

    # rip off tibble peculiarities ( [x,y,drop = FALSE] is ignored )
    if("xts" %in% class(tblobj)) {
      tblobj <- data.frame(dateindex = xts:::index.xts(tblobj),tblobj) # at this point dateindex will be Date xor POSIXct
    } else {
      tblobj <- as.data.frame(tblobj, stringsAsFactors = FALSE)
    }
    

    
    # tidyquant/timekit tibble may have a "date"(Date) column xor "index"("POSIXct" "POSIXt")
    # move "date" column to the front 
    if("date" %in% colnames(tblobj)) {
      tblobj <- DataCombine::MoveFront(tblobj,"date")
      # rename date to dateindex
      names(tblobj)[1] <- "dateindex"
    }
    # move "index" column to the front 
    if("index" %in% colnames(tblobj)) {
      tblobj <- DataCombine::MoveFront(tblobj,"index")
      # rename date to dateindex
      names(tblobj)[1] <- "dateindex"
    }
    
    verify_connection()
    
    db.q(str_c("create table if not exists ", tblobj_name,"(dateindex int4, dateindexyear int4, dateindexyearmonth int4, dateindexmonth int4, dateindexlbd int4, dateindexlwd int4, dateindexeom int4);"), conn.id = cid)
    
    # most have ONE column to be eligible to be pointed_at ( REPORT THIS BUG ) # TODO [ ]
    tblobj_db_ptr <- db.data.frame(tblobj_name, cid)
    
    # cycle through the column names in tblobj_db
    # if the column exists in tblobj_db_ptr and also exists in  tblobj 
    # then convert the tblobj column datatype to be the same(or compatiable ) as the tblobj_db datatype
    
     for (col_index_i in seq_along(tblobj_db_ptr@.col.name)) {
      if(colnames(tblobj)[col_index_i] %in% tblobj_db_ptr@.col.name) {
        match_index <- match(colnames(tblobj)[col_index_i], tblobj_db_ptr@.col.name, nomatch = 0) # FALSE
        column_not_addressed <- TRUE
        if(match_index) {
          # is.POSIXct # still will NOT keep POSIXct datatype on drop = TRUE .... becomes 'seconds' 
          if(xts::xtsible(tblobj[,col_index_i]) && 
          ("POSIXct" %in% class(tblobj[,col_index_i])) &&
             (tblobj_db_ptr@.col.udt_name[match_index] == "int4") && column_not_addressed == TRUE) {
             within(tblobj, { assign( colnames(tblobj)[col_index_i], as.integer(get(colnames(tblobj)[col_index_i])) / (3600*24) ) }) -> tblobj
             column_not_addressed <- FALSE
             message(str_c("column ", colnames(tblobj)[col_index_i], " datatype converion attempt to match what is in ", tblobj_name, " in ", tblobj_db_ptr@.col.name[match_index], " type ", tblobj_db_ptr@.col.udt_name[match_index]))
          } 
          # is.Date # still will keep Date datatype on drop = TRUE
          if(xts::xtsible(tblobj[,col_index_i]) && 
          ("Date" %in% class(tblobj[,col_index_i])) &&
             (tblobj_db_ptr@.col.udt_name[match_index] == "int4") && column_not_addressed == TRUE) {
             within(tblobj, { assign( colnames(tblobj)[col_index_i], as.integer(get(colnames(tblobj)[col_index_i]))) }) -> tblobj
             column_not_addressed <- FALSE
             message(str_c("column ", colnames(tblobj)[col_index_i], " datatype converion attempt to match what is in ", tblobj_name, " in ", tblobj_db_ptr@.col.name[match_index], " type ", tblobj_db_ptr@.col.udt_name[match_index]))
          } 
          if(is.factor(tblobj[,col_index_i]) && 
          (tblobj_db_ptr@.col.udt_name[match_index] == "text") && column_not_addressed == TRUE) {
             within(tblobj, { assign( colnames(tblobj)[col_index_i], as.character(get(colnames(tblobj)[col_index_i]))) }) -> tblobj
             column_not_addressed <- FALSE
             message(str_c("column ", colnames(tblobj)[col_index_i], " datatype converion attempt to match what is in ", tblobj_name, " in ", tblobj_db_ptr@.col.name[match_index], " type ", tblobj_db_ptr@.col.udt_name[match_index]))
          } 
          if(!is.numeric(tblobj[,col_index_i]) && 
             (tblobj_db_ptr@.col.udt_name[match_index] == "numeric") && column_not_addressed == TRUE) {
             within(tblobj, { assign( colnames(tblobj)[col_index_i], as.numeric(get(colnames(tblobj)[col_index_i]))) }) -> tblobj
             column_not_addressed <- FALSE
             message(str_c("column ", colnames(tblobj)[col_index_i], " datatype converion attempt to match what is in ", tblobj_name, " in ", tblobj_db_ptr@.col.name[match_index], " type ", tblobj_db_ptr@.col.udt_name[match_index]))
          }
          # note: POSIXct will(would have if where here) drop down to 'numeric'(!integer) 'days * seconds'
          if(!is.integer(tblobj[,col_index_i]) && 
             (tblobj_db_ptr@.col.udt_name[match_index] == "int4") && column_not_addressed == TRUE) {
             within(tblobj, { assign( colnames(tblobj)[col_index_i], as.integer(get(colnames(tblobj)[col_index_i]))) }) -> tblobj
             column_not_addressed <- FALSE
             message(str_c("column ", colnames(tblobj)[col_index_i], " datatype converion attempt to match what is in ", tblobj_name, " in ", tblobj_db_ptr@.col.name[match_index], " type ", tblobj_db_ptr@.col.udt_name[match_index]))
          }
          if(!is.character(tblobj[,col_index_i]) && 
             (tblobj_db_ptr@.col.udt_name[match_index] == "text") && column_not_addressed == TRUE) {
             within(tblobj, { assign( colnames(tblobj)[col_index_i], as.character(get(colnames(tblobj)[col_index_i]))) }) -> tblobj
             column_not_addressed <- FALSE
             message(str_c("column ", colnames(tblobj)[col_index_i], " datatype converion attempt to match what is in ", tblobj_name, " in ", tblobj_db_ptr@.col.name[match_index], " type ", tblobj_db_ptr@.col.udt_name[match_index]))
          } 
        }
      }
    } 
    # cycle through the column names in tblobj
    # if the column does not exist in tblobj_db_ptr then add it to tblobj_db_ptr
    for (col_index_i in seq_along(colnames(tblobj))) {
      if(!colnames(tblobj)[col_index_i] %in% tblobj_db_ptr@.col.name) {
        # special handling Date -> convert to integer
        # convert Date column to integers
        if( "Date" %in% class(avud(tblobj,colnames(tblobj)[col_index_i]))) {
          tblobj[,colnames(tblobj)[col_index_i]] <- as.integer(avud(tblobj,colnames(tblobj)[col_index_i]))
        }
        # convert POSIXct column to integers
        if( "POSIXct" %in% class(avud(tblobj,colnames(tblobj)[col_index_i]))) {
          tblobj[,colnames(tblobj)[col_index_i]] <- as.integer(avud(tblobj,colnames(tblobj)[col_index_i])) / (3600*24)
        }
        datatype_db_col <- switch(class(avud(tblobj,colnames(tblobj)[col_index_i])), integer = "int4", numeric = "numeric(8,2)", character = "text")
        db.q("alter table ", tblobj_name," add if not exists ", colnames(tblobj)[col_index_i]," ", datatype_db_col, ";", conn.id = cid)
        
      }
    }
    
    if("dateindex" %in% colnames(tblobj)) {
      # at this point dateindex is an integer
      # assume if the column dateindex[lwd/oem] is not there in tblobj then a 'custom' fill is not exptected to be done
      # so I can do an automatic fill
      dateindex <- tblobj[,"dateindex",drop =TRUE]
      
      if(!"dateindexyear" %in% colnames(tblobj))      tblobj$dateindexyear      <- yr_of_month(    tblobj$dateindex)
      if(!"dateindexyearmonth" %in% colnames(tblobj)) tblobj$dateindexyearmonth <- yrmnth_of_month(tblobj$dateindex)
      if(!"dateindexmonth" %in% colnames(tblobj))     tblobj$dateindexmonth     <- mnth_of_month(  tblobj$dateindex)
      if(!"dateindexlbd" %in% colnames(tblobj))       tblobj$dateindexlbd <- lbd_of_month(         tblobj$dateindex)
      
      if(!"dateindexlwd" %in% colnames(tblobj)) tblobj$dateindexlwd <-       lwd_of_month(tblobj$dateindex)
      if(!"dateindexeom" %in% colnames(tblobj)) tblobj$dateindexeom <-  last_day_of_month(tblobj$dateindex)
    }
    
    # re-order columns that I have
    matched <- match(c("dateindex", "dateindexyear", "dateindexyearmonth", "dateindexmonth", "dateindexlbd", "dateindexlwd", "dateindexeom"), colnames(tblobj))
    matched <- matched[complete.cases(matched)]
    tblobj <- DataCombine::MoveFront(tblobj,colnames(tblobj)[matched])
    
    # unconditionally put htere
    # only case accidentally/purposelydropped on the target
    db.q(str_c("alter table ", tblobj_name," add if not exists dateindexyear int4;"), conn.id = cid)
    db.q(str_c("alter table ", tblobj_name," add if not exists dateindexyearmonth int4;"), conn.id = cid)
    db.q(str_c("alter table ", tblobj_name," add if not exists dateindexmonth int4;"), conn.id = cid)
    db.q(str_c("alter table ", tblobj_name," add if not exists dateindexlbd int4;"), conn.id = cid)

    db.q(str_c("alter table ", tblobj_name," add if not exists dateindexlwd int4;"), conn.id = cid)
    db.q(str_c("alter table ", tblobj_name," add if not exists dateindexeom int4;"), conn.id = cid)
    
    # add an index to tblobj_db_ptr if it does not exist
    db.q(str_c("create unique index if not exists ", tblobj_name, "_" , str_c(key_columns, collapse = "_"),"_key  on ", tblobj_name, "(",str_c(key_columns, collapse = ", "),");"), conn.id = cid)

    # no numeric greater than 999999.99
    for (colnames_i in colnames(tblobj)) {
                        # avud: note: POSIXct will(would have if where here) drop down to 'numeric' 'days * seconds'
      if(is.numeric(tblobj[,colnames_i]) && !is.integer(tblobj[,colnames_i])) {
        
        # 'volume of s&p 500' NOT FIT problem
        
        ## originally make inbound data smaller to fit the db datatype
        # within( tblobj, { 
        #   assign( colnames_i, 
        #           ifelse(get(colnames_i) > 999999.99,999999.99, get(colnames_i)) 
        #   )      } 
        # ) -> tblobj
        
        ## now (if need be) make db datatype fit inbound data
        
        schema_current <- db.q(str_c("select current_schema();"), nrows = -1, conn.id = cid)[1,1,drop = TRUE]
        precision_numeric <- db.q(str_c("select numeric_precision from information_schema.columns where table_schema = '", schema_current, "' and table_name = '", tblobj_name, "' and column_name = '", colnames_i, "';"), nrows = -1, conn.id = cid)[1,1,drop = TRUE]
        db_numeric_column_i_storage_limit <- as.numeric(paste0(paste0(rep("9", precision_numeric - 2), collapse = ""),".99"))
        
        max_tblobj_colnames_i <- max(tblobj[,colnames_i])
        min_tblobj_colnames_i <- max(tblobj[,colnames_i])
        # >= because R may be inprecise
        if((max_tblobj_colnames_i >= db_numeric_column_i_storage_limit) || ( -1 * db_numeric_column_i_storage_limit  >= min_tblobj_colnames_i)) {
        
          # get numer of 9s to the left of the decimal
          new_numeric <- numb.digits.left.of.decimal(max(max_tblobj_colnames_i, abs(min_tblobj_colnames_i)))
          db.q(str_c("alter table ", tblobj_name," alter column ", colnames_i, " type numeric(", new_numeric + 2, ", 2);"), conn.id = cid)
          
        }
        
      }
    } 
    
    # eventually
    # CAN GET HUNG UP HERE!!
    message("Begin - drop table if exists upsert_temp")
    {function() { db.q("drop table if exists upsert_temp", conn.id = cid) }} -> drop_upsert_temp
    # TRY LESSEN THE CONGESTION 
    # try( { db.q("delete from upsert_temp;", conn.id = cid) }, silent = TRUE )
    # try( { db.q("truncate table upsert_temp;", conn.id = cid) }, silent = TRUE )
    db.q("select pg_sleep(1);", conn.id = cid)
    drop_upsert_temp()
    db.q("select pg_sleep(1);", conn.id = cid)
    # would have used as.db.data.frame but 'mult-column primary key is 'not allowed' ( I SHOULD REPORT THIS BUG )
    dbWriteTable(con, "upsert_temp", temporary = if(!is.null(getOption("upsert_temp_is_temporary"))) { TRUE } else { FALSE }, value = tblobj, append = FALSE, row.names = FALSE)
    db.q("select pg_sleep(1);", conn.id = cid)
    
    # load "upsert_temp" into tblobj_db_ptr ( upsize tblobj_db_ptr )
    db.q("insert into " %s+%
      tblobj_name %s+% "(" %s+% str_c(colnames(tblobj), collapse = ", ")  %s+% ") " %s+% "
                 select  " %s+% str_c(colnames(tblobj), collapse = ", ")  %s+% " from upsert_temp "  %s+% "
             on conflict(" %s+% str_c(key_columns, collapse = ", ")       %s+% ") "    %s+% "
         do update set ( " %s+% str_c(colnames(tblobj), collapse = ", ")  %s+% " ) = " %s+% "
                       ( " %s+% str_c(str_c("excluded.",colnames(tblobj)), collapse = ", ") %s+% " );")
    
    # eventually
    # CAN GET HUNG UP HERE!!
    message("Begin - drop table if exists upsert_temp")
    {function() { db.q("drop table if exists upsert_temp", conn.id = cid) }} -> drop_upsert_temp
    # TRY LESSEN THE CONGESTION 
    # try( { db.q("delete from upsert_temp;", conn.id = cid) }, silent = TRUE )
    # try( { db.q("truncate table upsert_temp;", conn.id = cid) }, silent = TRUE )
    db.q("select pg_sleep(1);", conn.id = cid)
    drop_upsert_temp()
    db.q("select pg_sleep(1);", conn.id = cid)
    
    options(ops)
    return(TRUE)
    
  }
  return(load_obj_direct_inner(tblobj = tblobj, key_columns = key_columns))
  
}
# load_obj_direct(tblobj = tblobj, key_columns = key_columns)
# 
# data("sample_matrix", package = "xts") # NOTE
# my_POSIXct_xts <- xts::as.xts(sample_matrix)
# load_obj_direct(my_POSIXct_xts, key_columns = "dateindex")
# my_tbl_df <- timekit::tk_tbl(my_POSIXct_xts)
# load_obj_direct(my_tbl_df, key_columns = "dateindex")
# my_tbl_df$Higher <- my_tbl_df$High + 100
# load_obj_direct(my_tbl_df, key_columns = "dateindex")
#
# tbl_df', 'tbl' and 'data.frame'
# above: program changes columns "date" or "index" to "dateindex"
# vix <- tidyquant::tq_get(c("VIX"), get  = "stock.prices", from = "2016-01-01", to  = "2017-01-01")[,c("date","close")]
# colnames(vix)[2] <- "vix"
# load_obj_direct(vix, key_columns = "date")

# debugSource('W:/R-3.4._/finecon01.R')
# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid","us_bonds","us_bonds_orig","us_bonds_chgs","us_bonds_chgs_orig","my_POSIXct_xts","my_tbl_df","vix")))

# quantmod::getSymbols("GSPC") # XOR? # quantmod::getSymbols("^GSPC") # quantmod::getSymbols("^GSPC", from = "1940-01-01") #  "1950-01-03"+...

     # As of quantmod 0.4-9, 'getSymbols.yahoo' has been patched to work
     # with changes to Yahoo Finance, which also included the following
     # changes to the raw data:
     # 
     #    . The adjusted close column appears to no longer include
     #      dividend adjustments
     # 
     #    . The close column appears to be adjusted for splits twice
     # 
     #    . The open, high, and low columns are adjusted for splits, and
     # 
     #    . The raw data may contain missing values.

# getSymbols.yahoo

# load_obj_direct(GSPC, key_columns = "dateindex")
# 
# quantmod::getSymbols("^GSPC", from = "1950-01-01") # 67 years '262 days/year ~ 17000 rows of data': # first time ... 5 ... 10 seconds 
# [1] "GSPC"

# ROUGH WAY to look for missing values ( from = "1940-01-01" #  "1950-01-03"+... ) 
# split.xts
# > sapply( split(GSPC, f = "years"), NROW )
#  [1] 249 249 250 251 252 252 251 252 252 253 252 250 252 251 253 252 252 251 226
# [20] 250 254 253 251 252 253 253 253 252 252 253 253 253 253 253 253 252 253 253
# [39] 253 252 253 253 254 253 252 252 254 253 252 252 252 248 252 252 252 252 251
# [58] 251 253 252 252 252 250 252 252 252 252 131

# head(GSPC)
#            GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
# 1950-01-03     16.66     16.66    16.66      16.66     1260000         16.66
# 1950-01-04     16.85     16.85    16.85      16.85     1890000         16.85
# 1950-01-05     16.93     16.93    16.93      16.93     2550000         16.93
# 1950-01-06     16.98     16.98    16.98      16.98     2010000         16.98
# 1950-01-09     17.09     17.09    17.08      17.08     3850000         17.08
# 1950-01-10     17.03     17.03    17.03      17.03     2160000         17.03
# 
# verify_connection()
# load_obj_direct(GSPC, key_columns = "dateindex")
# 
# finance_econ=# select * from gspc order by dateindex limit  6;
#  dateindex | dateindexlwd | dateindexeom | gspc_open | gspc_high | gspc_low | gspc_close | gspc_volume | gspc_adjusted
# -----------+--------------+--------------+-----------+-----------+----------+------------+-------------+---------------
#      -7303 |        -7275 |        -7275 |     16.66 |     16.66 |    16.66 |      16.66 |   999999.99 |         16.66
#      -7302 |        -7275 |        -7275 |     16.85 |     16.85 |    16.85 |      16.85 |   999999.99 |         16.85
#      -7301 |        -7275 |        -7275 |     16.93 |     16.93 |    16.93 |      16.93 |   999999.99 |         16.93
#      -7300 |        -7275 |        -7275 |     16.98 |     16.98 |    16.98 |      16.98 |   999999.99 |         16.98
#      -7297 |        -7275 |        -7275 |     17.09 |     17.09 |    17.08 |      17.08 |   999999.99 |         17.08
#      -7296 |        -7275 |        -7275 |     17.03 |     17.03 |    17.03 |      17.03 |   999999.99 |         17.03
# (6 rows)
# 
# finance_econ=# delete from gspc where dateindex in (-7303,-7202,-7301,-7300);
# DELETE 3
# 
# finance_econ=# select * from gspc order by dateindex limit  6;
#  dateindex | dateindexlwd | dateindexeom | gspc_open | gspc_high | gspc_low | gspc_close | gspc_volume | gspc_adjusted
# -----------+--------------+--------------+-----------+-----------+----------+------------+-------------+---------------
#      -7297 |        -7275 |        -7275 |     17.09 |     17.09 |    17.08 |      17.08 |   999999.99 |         17.08
#      -7296 |        -7275 |        -7275 |     17.03 |     17.03 |    17.03 |      17.03 |   999999.99 |         17.03
#      -7295 |        -7275 |        -7275 |     17.09 |     17.09 |    17.09 |      17.09 |   999999.99 |         17.09
#      -7294 |        -7275 |        -7275 |     16.76 |     16.76 |    16.76 |      16.76 |   999999.99 |         16.76
#      -7293 |        -7275 |        -7275 |     16.67 |     16.67 |    16.67 |      16.67 |   999999.99 |         16.67
#      -7290 |        -7275 |        -7275 |     16.65 |     16.72 |    16.65 |      16.72 |   999999.99 |         16.72
# (6 rows)
# 
# quantmod::getSymbols("^GSPC", from = "1950-01-01", to = "1950-02-01")
# load_obj_direct(GSPC, key_columns = "dateindex")
# 
# finance_econ=# select * from gspc order by dateindex limit  6;
#  dateindex | dateindexlwd | dateindexeom | gspc_open | gspc_high | gspc_low | gspc_close | gspc_volume | gspc_adjusted
# -----------+--------------+--------------+-----------+-----------+----------+------------+-------------+---------------
#      -7303 |        -7275 |        -7275 |     16.66 |     16.66 |    16.66 |      16.66 |   999999.99 |         16.66
#      -7302 |        -7275 |        -7275 |     16.85 |     16.85 |    16.85 |      16.85 |   999999.99 |         16.85
#      -7301 |        -7275 |        -7275 |     16.93 |     16.93 |    16.93 |      16.93 |   999999.99 |         16.93
#      -7300 |        -7275 |        -7275 |     16.98 |     16.98 |    16.98 |      16.98 |   999999.99 |         16.98
#      -7297 |        -7275 |        -7275 |     17.09 |     17.09 |    17.08 |      17.08 |   999999.99 |         17.08
#      -7296 |        -7275 |        -7275 |     17.03 |     17.03 |    17.03 |      17.03 |   999999.99 |         17.03
# (6 rows)



sipro_adhoc_disk <- function(   fields           = c("company_id")
                              , fields_db_types  = c("text") 
                              , tables           = c("si_ci") 
                              , data.frame.out   = TRUE
                              , out_db_tablename = "query01"
                              , sipro_files_disk_loc = "W:\\AAIISIProDBFs"
                            ) {
  ops <- options()
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  options(width = 255)
  options(warn = 1)
  
  require(PivotalR)

  # uses zoo         as.Date.integer (expected) # S3 dispatch of either base or zoo garanteed ( or other loaded )
  #      ti          ti.Date # S3 dispatch
  #      ti          lastBusinessDayOfMonth.ti lastDayOf.ti # S3 dispatch
  #      DataCombine MoveFront
  #      stringr     str_detect  # more flexible regular expressions
  `%>%` <- magrittr::`%>%`
  
  # already in code, I should not repeat HERE move LATER
  numb.digits.left.of.decimal <- function(x) {

    res <- floor( log10( abs(x) ) ) + 1
    res <- as.integer(ifelse(res > 0, res, 0 ))
    return(res)
  
  }
  
  path_file_dbf       <- paste0(sipro_files_disk_loc, "\\15184\\si_ci.dbf")
  path_file_fst       <- paste0(sipro_files_disk_loc, "\\15184\\si_ci.fst")
  tables_fields_types <- rlist::list.zip(fields, fields_db_types, tables)
  
  
  # what I am I interested in 
  
  # latest to earliest
  disk_dateindexes    <- rev(sort(as.integer(dir(sipro_files_disk_loc))))
  # debuggng
  # disk_dateindexes    <- c(12083, 12055) 
  
  if(any(disk_dateindexes < 15184)) {
    
    si_tbl_df_15184 <- try( { fst::read.fst(path_file_fst, toupper(c("company_id", "ticker", "company", "street"))) }, silent = TRUE)
    if(inherits(si_tbl_df_15184, "try-error")) {
      si_tbl_df_15184 <- suppressWarnings(suppressMessages(foreign::read.dbf(file = path_file_dbf, as.is = TRUE)))
      si_tbl_df_15184 <-fst::write.fst(si_tbl_df_15184, path_file_fst)
      si_tbl_df_15184 <- fst::read.fst(path_file_fst, column_names_fst)
    }
        
    # si_tbl_df_15184 <- suppressWarnings(suppressMessages(foreign::read.dbf(file = paste0(sipro_files_disk_loc, "\\15184\\si_ci.dbf"), as.is = TRUE)))
   
    # colnames to lower
    si_tbl_df_15184 <- setNames(si_tbl_df_15184, tolower(colnames(si_tbl_df_15184)))
    
    # remove duplicas
    si_tbl_df_15184 <- si_tbl_df_15184[!(duplicated(si_tbl_df_15184[["company_id"]]) | duplicated(si_tbl_df_15184[["company_id"]], fromLast = TRUE)),,drop = FALSE]
    si_tbl_df_15184 <- si_tbl_df_15184[!(duplicated(si_tbl_df_15184[["ticker"]])     | duplicated(si_tbl_df_15184[["ticker"]]    , fromLast = TRUE)),,drop = FALSE]

    si_tbl_df_15184_ids <- si_tbl_df_15184[,c("company_id", "ticker", "company", "street"), drop = FALSE]
    
  }
    
  for(disk_dateindexes_i in disk_dateindexes) {
    
    # browser( expr = { disk_dateindexes_i == 12629   })
    # browser( expr = { disk_dateindexes_i == 12601   }) # lwd_of_month WRONG EVERYWHERE - but I do not join on it
    
    message(paste0("sipro_adhoc_disk - Beginning disk_dateindexes_i: ", disk_dateindexes_i))
  
    si_tbl_dfs <- list()
    
    # si_tbls <- tables
    for(si_tbl_i in unique(unlist(tables))) {
      
      column_names_fst <- toupper(unlist(sapply(tables_fields_types, function(x) { if(si_tbl_i %in% x[["tables"]] ) { x[["fields"]] } } )))
    
      # si_tbl_df <- suppressWarnings(suppressMessages(foreign::read.dbf(file = paste0(sipro_files_disk_loc, "\\", disk_dateindexes_i, "\\", si_tbl_i,".dbf"), as.is = TRUE)))
      
      path_file_dbf  <- paste0(sipro_files_disk_loc, "\\", disk_dateindexes_i, "\\", si_tbl_i,".dbf")
      path_file_fst  <- paste0(sipro_files_disk_loc, "\\", disk_dateindexes_i, "\\", si_tbl_i,".fst")

      si_tbl_df <- try( { fst::read.fst(path_file_fst, column_names_fst) }, silent = TRUE)
      if(inherits(si_tbl_df, "try-error")) {
        si_tbl_df <- suppressWarnings(suppressMessages(foreign::read.dbf(file = path_file_dbf, as.is = TRUE)))
        si_tbl_df <- fst::write.fst(si_tbl_df, path_file_fst)
        si_tbl_df <- fst::read.fst(path_file_fst, column_names_fst)
      }
      
      # colnames to lower
       si_tbl_df <- setNames(si_tbl_df, tolower(colnames(si_tbl_df)))
      
      # remove useless columns
      si_tbl_df  <- si_tbl_df[,!stringr::str_detect(colnames(si_tbl_df),"^x\\.?+|^repno$|^updated$|^business$|^analyst_fn$"),drop = FALSE]
      
      # just my fields of interest
      si_tbl_df <- si_tbl_df[, fields[fields %in% colnames(si_tbl_df)], drop = FALSE]
      
      # remove duplicas
      si_tbl_df <- si_tbl_df[!(duplicated(si_tbl_df[["company_id"]]) | duplicated(si_tbl_df[["company_id"]], fromLast = TRUE)),,drop = FALSE]
      
      if(si_tbl_i == "si_ci") {
        si_tbl_df <- si_tbl_df[!(duplicated(si_tbl_df[["ticker"]])   | duplicated(si_tbl_df[["ticker"]],     fromLast = TRUE)),,drop = FALSE]
      }
      
      if(si_tbl_i == "si_ci") {
      
        # add id columns
        si_tbl_df <- cbind(dateindex_company_id_orig = paste0(disk_dateindexes_i, "_", si_tbl_df[["company_id"]])               , si_tbl_df, stringsAsFactors = FALSE)
        si_tbl_df <- cbind(dateindex_company_id      =                                 si_tbl_df[["dateindex_company_id_orig"]] , si_tbl_df, stringsAsFactors = FALSE)
        
        # originals                             # (-5): sipro dates ON or BEFORE 2004-12-03
        { zoo::as.Date(disk_dateindexes_i) } ->
           disk_dateindexes_i_dt
        
        {                 disk_dateindexes_i_dt } %>%
            as.integer -> dateindexorig
        
        # paritions
        
        # { DescTools::Year(disk_dateindexes_i_dt       - 5) } %>%
        #     as.integer -> dateindexyear
        
        yr_of_month(disk_dateindexes_i_dt) -> dateindexyear
        
        # { DescTools::YearMonth(disk_dateindexes_i_dt  - 5) } %>%
        #     as.integer -> dateindexyearmonth
        
        yrmnth_of_month(disk_dateindexes_i_dt) -> dateindexyearmonth
        
        mnth_since_birth(disk_dateindexes_i_dt) -> dateindexmonthsincebirth
        
        # { DescTools::Month(disk_dateindexes_i_dt      - 5) } %>%
        #     as.integer -> dateindexmonth
        
        mnth_of_month(disk_dateindexes_i_dt) -> dateindexmonth

        # references
        
        # { RQuantLib::getEndOfMonth("UnitedStates/NYSE", disk_dateindexes_i_dt - 5) } %>%   # TEST!: zoo::as.Date("2010-05-28")
        #     as.integer -> dateindexlbd

        lbd_of_month(disk_dateindexes_i_dt) -> dateindexlbd
        
        # { RQuantLib::getEndOfMonth("WeekendsOnly"     , disk_dateindexes_i_dt - 5) } %>%
        #     as.integer -> dateindexlwd
        
        lwd_of_month(disk_dateindexes_i_dt) -> dateindexlwd
        
        # { DescTools::LastDayOfMonth(disk_dateindexes_i_dt - 5) } %>%
        #     as.integer -> dateindexeom
        
        last_day_of_month(disk_dateindexes_i_dt) -> dateindexeom
        
        # row number ( future? performance for fst::read.fst( . . . from = 1, to = NULL . . . )  )
        si_tbl_df <- cbind(TABLE_NAME      = seq_along(row.names(si_tbl_df)), si_tbl_df, stringsAsFactors = FALSE)
        colnames(si_tbl_df)[NCOL(si_tbl_df)] <- paste0(out_db_tablename, "_rn")
        
        # originals
        si_tbl_df <- cbind(dateindex      = disk_dateindexes_i, si_tbl_df, stringsAsFactors = FALSE) # CURR NOT CHANGED
        si_tbl_df <- cbind(dateindexorig  = disk_dateindexes_i, si_tbl_df, stringsAsFactors = FALSE) 
        
        # partitions
        si_tbl_df <- cbind(dateindexyear      = dateindexyear,        si_tbl_df, stringsAsFactors = FALSE)
        si_tbl_df <- cbind(dateindexyearmonth = dateindexyearmonth,   si_tbl_df, stringsAsFactors = FALSE)
        si_tbl_df <- cbind(dateindexmonthsincebirth = dateindexmonthsincebirth,       si_tbl_df, stringsAsFactors = FALSE)
        si_tbl_df <- cbind(dateindexmonth           = dateindexmonth,                 si_tbl_df, stringsAsFactors = FALSE)
        
        # references
        si_tbl_df <- cbind(dateindexlbd = dateindexlbd,       si_tbl_df, stringsAsFactors = FALSE)
        si_tbl_df <- cbind(dateindexlwd = dateindexlwd,       si_tbl_df, stringsAsFactors = FALSE)
        si_tbl_df <- cbind(dateindexeom = dateindexeom,       si_tbl_df, stringsAsFactors = FALSE)
        
        si_tbl_df <- cbind(company_id_orig = si_tbl_df[["company_id"]], si_tbl_df, stringsAsFactors = FALSE)
        
        si_tbl_df <- DataCombine::MoveFront(si_tbl_df,
             c(paste0(out_db_tablename, "_rn")
           , "dateindex_company_id_orig"
           , "dateindex_company_id"
           , "dateindex"            # CURR NOT CHANGED
           , "dateindexorig"
           , "dateindexyear"
           , "dateindexyearmonth"
           , "dateindexmonthsincebirth"
           , "dateindexmonth"
           , "dateindexlbd"
           , "dateindexlwd"
           , "dateindexeom"
           , "company_id_orig"
          )
        )
      
      }
      
      if(si_tbl_i != "si_ci") {
        
        si_tbl_df <- cbind(dateindex    = disk_dateindexes_i, si_tbl_df, stringsAsFactors = FALSE)
        si_tbl_df <- DataCombine::MoveFront(si_tbl_df, "dateindex")
        
      }
      
      # convert to fields_db_types (if any)

      for(iter_i in seq_along(fields)) {
        
        if(fields[iter_i] %in% colnames(si_tbl_df)) {
          if(fields_db_types[iter_i] == "integer") si_tbl_df[[fields[iter_i]]]  <- as.integer(  si_tbl_df[[fields[iter_i]]])
          if(fields_db_types[iter_i] == "text")    si_tbl_df[[fields[iter_i]]]  <- as.character(si_tbl_df[[fields[iter_i]]])
          if(fields_db_types[iter_i] == "boolean") si_tbl_df[[fields[iter_i]]]  <-              si_tbl_df[[fields[iter_i]]] & rep(1,NROW(si_tbl_df))
          if(fields_db_types[iter_i] %in% c("float8" , "numeric(EXPLODE,2)"))  si_tbl_df[[fields[iter_i]]]  <- as.numeric(  si_tbl_df[[fields[iter_i]]])  
                                                                                   # Supports base or zoo or other loaded S3 displatch
          if(fields_db_types[iter_i] == "Date")    si_tbl_df[[fields[iter_i]]]  <- zoo::as.Date(si_tbl_df[[fields[iter_i]]])
          
        }
      }
      
      # together with the previous si_*
      si_tbl_dfs <- c(si_tbl_dfs, list(si_tbl_df))
      
    }
    # just ONE date.frame to represent a dateindex
    si_tbl_df  <- plyr::join_all(si_tbl_dfs, by = c("dateindex","company_id")) 
    
    if(exists("si_tbl_df_all")) { 
      si_tbl_df_all <- c(si_tbl_df_all,list(si_tbl_df))
    } else {
      si_tbl_df_all <- list(si_tbl_df)
    }
    
    message(paste0("sipro_adhoc_disk - End disk_dateindexes_i: ", disk_dateindexes_i))
    
  }

  # High performance (instananteous)
  si_tbl_df_all <- data.frame(data.table::rbindlist(si_tbl_df_all))
  
  # if("db" %in% out) {
    
  # this one line; NOT for public release
  db.q(paste0("drop table if exists ", out_db_tablename, ";"), conn.id = cid)
  
  # write to the database
  sipro_adhoc_disk_ptr <- as.db.data.frame(si_tbl_df_all, out_db_tablename, conn.id = cid)
  
  # put out to database ( 4 seconds )
  # match old company_ids to company_ids

  if("company_id" %in% colnames(si_tbl_df_all)) {
  
    db.q("drop table if exists si_tbl_df_15184_ids;", conn.id = cid)
    
    si_tbl_df_15184_ids_db_ptr <- as.db.data.frame(si_tbl_df_15184_ids, "si_tbl_df_15184_ids", conn.id = cid, is.temp = TRUE)

    db.q(paste0("create index ", out_db_tablename, "_partial_company_id_lt_15184_idx on ", out_db_tablename, "(dateindex) where dateindex < 15184;"), conn.id = cid)
    
    # update old company_id to that company_id in 15184 ( match by 'ticker' )
    # RAWER # simplified version ( just ticker , no 'street and/or company')
    db.q(paste0("
      update ", out_db_tablename, "
        set           company_id =                     si_tbl_df_15184_ids.company_id,
            dateindex_company_id = dateindex || '_' || si_tbl_df_15184_ids.company_id
     from si_tbl_df_15184_ids
       where si_tbl_df_15184_ids.company_id != ", out_db_tablename, ".company_id and
             si_tbl_df_15184_ids.ticker      = ", out_db_tablename, ".ticker and 
             ", out_db_tablename, ".dateindex < 15184
    "), conn.id = cid)
  
  }
  
  # new data.frame
  si_tbl_df <- si_tbl_df <- db.q(paste0("select * from ", out_db_tablename, ";"), nrows = "all", conn.id = cid) 
  
  delete(si_tbl_df_15184_ids_db_ptr)

  for(iter_i in seq_along(fields)) {
    if( fields_db_types[iter_i] == "numeric(EXPLODE,2)") {
      # could find local or data base left side of the decimal size 
      # slightly SAFER to get from the database
      field_max <- db.q(paste0("select max(",fields[iter_i] ,") from ", out_db_tablename, ";"), nrows = "all", conn.id = cid)[["max"]]
        
      # get numer of 9s to the left of the decimal
      new_numeric <- numb.digits.left.of.decimal(field_max)
        
      # 5067.4 -> numeric(7,2) # STRANGE ... for now just add an extra digit (+1)
      db.q(paste0("alter table ", out_db_tablename, " alter column ", fields[iter_i], " type numeric(", new_numeric + 2 + (1), ", 2);"), conn.id = cid)

    }
  }
  
  # single column primary_key ( many 3rd party applications require exactly this ONE field)
  db.q(paste0("create unique index ", out_db_tablename, "_dateindex_company_id_composite_unqpkidx on ", out_db_tablename, "(dateindex_company_id);"), conn.id = cid)
  db.q(paste0("alter table         ", out_db_tablename, " add primary key using index ", out_db_tablename, "_dateindex_company_id_composite_unqpkidx;"), conn.id = cid)
  
  # useful index
  db.q(paste0("create unique index ", out_db_tablename, "_dateindex_company_id_idx on ", out_db_tablename, "(dateindex, company_id);"), conn.id = cid)
 
  # }
  
  options(ops)
  Sys.setenv(TZ=oldtz)
  
  if(data.frame.out == TRUE) {
    return(si_tbl_df)
  } else {
    return(TRUE)
  }

}
# sipro_adhoc_disk_out <- sipro_adhoc_disk(   fields          = c("company_id"   , "ticker", "company", "sp"  , "mktcap"              , "price"              , "netinc_q1"          , "netinc_q2"          , "sales_q1"          , "sales_q2"           ,"perend_q1", "perend_q2")
#                                           , fields_db_types = c("text"         , "text"  , "text"   , "text", "numeric(EXPLODE,2)"  , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)", "numeric(EXPLODE,2)" ,"integer"  , "integer"  )
#                                           , tables          = list(c("si_ci"   ,                                                                                                                                               
#                                                                      "si_isq"  ,                                                                                                                                               
#                                                                      "si_psd"  ,                                                                                                                                               
#                                                                      "si_date"), "si_ci" , "si_ci" , "si_ci", "si_psd"              , "si_psd"             , "si_isq"             , "si_isq"             , "si_isq"            , "si_isq"             ,"si_date"  , "si_date"  ) 
#                                           , data.frame.out  = TRUE
#                                         )


# sipro_adhoc_disk_out <- sipro_adhoc_disk(   fields          = c("company_id"   , "ticker", "company", "sp"  , "mktcap"              , "price"              , "netinc_q1"          , "netinc_q2"          , "sales_q1"          , "sales_q2"          , "ncc_q1"            , "ncc_q2"            , "assets_q1"          , "assets_q2"          , "perend_q1", "perend_q2" )
#                                           , fields_db_types = c("text"         , "text"  , "text"   , "text", "numeric(EXPLODE,2)"  , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)", "numeric(EXPLODE,2)", "numeric(EXPLODE,2)", "numeric(EXPLODE,2)", "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "integer"  , "integer"   )
#                                           , tables          = list(c("si_ci"   ,                                                                                                                                                                                                                                       
#                                                                      "si_isq"  , 
#                                                                      "si_psd"  ,                                                                      
#                                                                      "si_cfq"  ,
#                                                                      "si_bsq"  ,
#                                                                      "si_date"), "si_ci" , "si_ci" , "si_ci", "si_psd"              , "si_psd"             , "si_isq"             , "si_isq"             , "si_isq"            , "si_isq"            , "si_cfq"            , "si_cfq"            , "si_bsq"             , "si_bsq"             , "si_date"  , "si_date"   ) 
#                                           , data.frame.out  = TRUE
#                                         )
# 

# sipro_adhoc_disk_out <- sipro_adhoc_disk(   fields          = c("company_id"   , "ticker", "company", "sp"  , "employees"          , "mktcap"             , "price"              , "netinc_q1"          , "cgs_q1"             , "sales_q1"           , "cash_q1"            , "ncc_q1"            , "tcf_q1"            , "tci_q1"             , "tco_q1"             , "ere_q1"              , "perend_q1" )
#                                           , fields_db_types = c("text"         , "text"  , "text"   , "text", "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)", "numeric(EXPLODE,2)", "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)" , "numeric(EXPLODE,2)"  , "integer"   )
#                                           , tables          = list(c("si_ci"   ,                                                                                                                                                                                                                                                 
#                                                                      "si_isq"  ,                             
#                                                                      "si_psd"  ,                                                                                
#                                                                      "si_cfq"  ,                             
#                                                                      "si_bsq"  ,                             
#                                                                      "si_date"), "si_ci" , "si_ci" , "si_ci", "si_ci"              , "si_psd"             , "si_psd"             , "si_isq"             , "si_isq"             , "si_isq"             , "si_bsq"             , "si_cfq"            , "si_cfq"            , "si_cfq"             , "si_cfq"             , "si_cfq"              , "si_date"   ) 
#                                           , data.frame.out  = TRUE
#                                         )


# entertaining
get_all_raw_by_dateindex <- function(dateindex = NULL, file_type = "fst", aaii_sipro_dir = getsetvar_aaii_sipro_dir()) { 

  if(is.null(dateindex)) stop("load_all_raw_by_index  missing argument: dateindex")

  # si_ci and si_exchg and si_mgsdc

  require(foreign) # REQUIRED
  require(xts)     # REQUIRED
  # uses data.table LATER REPLACE PRIVATE functions data.table:::duplicated.data.table, 
  #                                                 data.table:::merge.data.table
  # uses foreign
  # uses stringi
  # uses plyr
  # uses dplyr
  # uses rlist
  if(file_type == "sqlite") require(DBI)
    # uses RSQLite
  # uses fst (CRAN)
  #  
  # FUTURE
  # Currently only select() and head()
  # https://krlmlr.github.io/fstplyr/
  # devtools::install_github("krlmlr/fstplyr")
 
    
  # dateindex <- 17562
  
  if(file_type == "fst") {
    fst::threads_fst(nr_of_threads = 4)
    print(fst::threads_fst())
  }
  
  message("  Begin si_ci")
  
  if(file_type == "fst") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.fst"))) {
      df <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.dbf"), as.is = T) # LASTMOD is not in earlier data
      fst::write.fst(df, stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.fst"), compress = 0)
    } 
    ft <- fst::fst(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.fst"))
    df <- ft[,]
  }
  if(file_type == "sqlite") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.sqlite"))) {
      df <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.dbf"), as.is = T) # LASTMOD is not in earlier data
      consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.sqlite"))
      dbWriteTable(consqlite, "si_ci", df)
      dbDisconnect(consqlite)
    } 
    consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_ci.sqlite"))
    st <- dbGetQuery(consqlite, "select * from si_ci")
    dbDisconnect(consqlite)
    df <- st
  }
  
  # columns
  df <- df[,grep("^X.*", colnames(df), value = T, invert = T), drop = F]

  # df <- plyr::rename(df, c("REPNO" = "SI_CI_REPNO"))
  # df <- plyr::rename(df, c("LASTMOD" = "SI_CI_LASTMOD"))  # LASTMOD is not in earlier data
  # CONSISTENT WITH BELOW
  df <- plyr::rename(df, c("REPNO" = "CI_REPNO"))
  df <- plyr::rename(df, c("LASTMOD" = "CI_LASTMOD"))  # LASTMOD is not in earlier data
  
  # rows
  # keep one 'all columns' duplicate
  df <- df[ !data.table:::duplicated.data.table(df, fromLast = TRUE),,drop = FALSE]
  df <- df[ !stringi::stri_duplicated(df$COMPANY_ID) & !stringi::stri_duplicated(df$COMPANY_ID, fromLast = TRUE), , drop = FALSE]
  df <- df[ !stringi::stri_duplicated(df$TICKER)     & !stringi::stri_duplicated(df$TICKER,     fromLast = TRUE), , drop = FALSE]

  message("  End   si_ci")  
  
  message("  Begin si_exchg")

  # exchange
  if(file_type == "fst") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.fst"))) {
      exchg <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.dbf"), as.is = T)
      fst::write.fst(exchg, stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.fst"), compress = 0)
    } 
    ft <- fst::fst(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.fst"))
    exchg <- ft[,]
  }
  if(file_type == "sqlite") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.sqlite"))) {
      exchg <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.dbf"), as.is = T) # LASTMOD is not in earlier data
      consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.sqlite"))
      dbWriteTable(consqlite, "si_exchg", exchg)
      dbDisconnect(consqlite)
    } 
    consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_exchg.sqlite"))
    st <- dbGetQuery(consqlite, "select * from si_exchg")
    dbDisconnect(consqlite)
    exchg <- st
  }
  
  exchg <- exchg[ !data.table:::duplicated.data.table(exchg, fromLast = TRUE),,drop = FALSE]
  exchg <- exchg[ !stringi::stri_duplicated(exchg$EXCHG_CODE) & !stringi::stri_duplicated(exchg$EXCHG_CODE, fromLast = TRUE), , drop = FALSE]
  exchg <- data.table::as.data.table(exchg, keep.rownames= "RN_SI_EXCHG")
  # different from other below
  df$EXCHG_CODE <- df$EXCHANGE
  # df <- dplyr::left_join(df, exchg, by = c("EXCHG_CODE" = "EXCHG_CODE"))
  df <- data.table:::merge.data.table(df, exchg, by.x = "EXCHG_CODE", by.y = "EXCHG_CODE", all.x = TRUE, sort = FALSE)
  df <- df[ !stringi::stri_duplicated(df$COMPANY_ID) & !stringi::stri_duplicated(df$COMPANY_ID, fromLast = TRUE), , drop = FALSE]
    
  message("  End   si_exchg")
  message("  Begin si_mgdsc for industry")
  
  # industry
  if(file_type == "fst") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.fst"))) {
      mgdsc <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.dbf"), as.is = T)
      fst::write.fst(mgdsc, stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.fst"), compress = 0)
    } 
    ft <- fst::fst(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.fst"))
    mgdsc <- ft[,]
  }
  if(file_type == "sqlite") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.sqlite"))) {
      mgdsc <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.dbf"), as.is = T) # LASTMOD is not in earlier data
      consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.sqlite"))
      dbWriteTable(consqlite, "si_mgdsc", mgdsc)
      dbDisconnect(consqlite)
    } 
    consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.sqlite"))
    st <- dbGetQuery(consqlite, "select * from si_mgdsc")
    dbDisconnect(consqlite)
    mgdsc <- st
  }
  mgdsc <- mgdsc[ !data.table:::duplicated.data.table(mgdsc, fromLast = TRUE),,drop = FALSE]
  mgdsc <- mgdsc[ !stringi::stri_duplicated(mgdsc$MG_CODE) & !stringi::stri_duplicated(mgdsc$MG_CODE, fromLast = TRUE), , drop = FALSE]
  mgdsc <- data.table::as.data.table(mgdsc, keep.rownames= "RN_SI_MGDSC_IND")
  df$INDUSTRY_CODE <- df$IND_3_DIG
  # df <- dplyr::left_join(df, mgdsc, by = c("IND_3_DIG" = "MG_CODE"))
  df <- data.table:::merge.data.table(df, mgdsc, by.x = "IND_3_DIG", by.y = "MG_CODE", all.x = TRUE, sort = FALSE)
  df <- df[ !stringi::stri_duplicated(df$COMPANY_ID) & !stringi::stri_duplicated(df$COMPANY_ID, fromLast = TRUE), , drop = FALSE]
  df <- plyr::rename(df, c("MG_DESC" = "INDUSTRY_DESC"))

  message("  End   si_mgdsc for industry")
  message("  Begin si_mgdsc for sector")

  # sector
  if(file_type == "fst") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.fst"))) {
      mgdsc <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.dbf"), as.is = T)
      fst::write.fst(mgdsc, stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.fst"), compress = 0)
    } 
    ft <- fst::fst(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.fst"))
    mgdsc <- ft[,]
  }
  if(file_type == "sqlite") {
    if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.sqlite"))) {
      mgdsc <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.dbf"), as.is = T) # LASTMOD is not in earlier data
      consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.sqlite"))
      dbWriteTable(consqlite, "si_mgdsc", mgdsc)
      dbDisconnect(consqlite)
    } 
    consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/si_mgdsc.sqlite"))
    st <- dbGetQuery(consqlite, "select * from si_mgdsc")
    dbDisconnect(consqlite)
    mgdsc <- st
  }
  mgdsc <- mgdsc[ !data.table:::duplicated.data.table(mgdsc, fromLast = TRUE),,drop = FALSE]
  mgdsc <- mgdsc[ !stringi::stri_duplicated(mgdsc$MG_CODE) & !stringi::stri_duplicated(mgdsc$MG_CODE, fromLast = TRUE), , drop = FALSE]
  mgdsc <- data.table::as.data.table(mgdsc, keep.rownames= "RN_SI_MGDSC_SECT")
  df$SECTOR_CODE <- df$IND_2_DIG
  # df <- dplyr::left_join(df, mgdsc, by = c("IND_2_DIG" = "MG_CODE")) 
  df <- data.table:::merge.data.table(df, mgdsc, by.x = "IND_2_DIG", by.y = "MG_CODE", all.x = TRUE, sort = FALSE)
  df <- df[ !stringi::stri_duplicated(df$COMPANY_ID) & !stringi::stri_duplicated(df$COMPANY_ID, fromLast = TRUE), , drop = FALSE]
  df <- plyr::rename(df, c("MG_DESC" = "SECTOR_DESC"))

  message("  End   si_mgdsc for sector")
  
  for(si_file in c("si_isq","si_cfq","si_bsq","si_date","si_psd","si_psdc","si_psdd","si_mlt","si_rat","si_ee")) {

    # si_file <- "si_isq"
    message(stringi::stri_c("  Begin ",si_file))
    
    if(file_type == "fst") {
      if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".fst"))) {
        dfnew <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".dbf"), as.is = T)
        fst::write.fst(dfnew, stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".fst"), compress = 0)
      } 
      ft <- fst::fst(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".fst"))
      dfnew <- ft[,]
    }
    if(file_type == "sqlite") {
      if(!file.exists(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".sqlite"))) {
        dfnew <- read.dbf(stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".dbf"), as.is = T)
        consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".sqlite"))
        dbWriteTable(consqlite, si_file, dfnew)
        dbDisconnect(consqlite)
      } 
      consqlite <- dbConnect(RSQLite::SQLite(), stringi::stri_c(aaii_sipro_dir,"/",dateindex,"/",si_file,".sqlite"))
      st <- dbGetQuery(consqlite, stringi::stri_c("select * from ", si_file))
      dbDisconnect(consqlite)
      dfnew <- st
    }

    # columns ( tested - WORKS )
    # dfnew <- dfnew[,grep("^X.*", colnames(dfnew), value = T, invert = T), drop = F]
    # dfnew <- plyr::rename(dfnew, c("LASTMOD" = stringi::stri_c(toupper(si_file),"_LASTMOD")))  # LASTMOD is not in earlier data
    # dfnew <- plyr::rename(dfnew, c("UPDATED" = stringi::stri_c(toupper(si_file),"_UPDATED")))  # boolean 
    # dfnew <- plyr::rename(dfnew, c("REPNO"   = stringi::stri_c(toupper(si_file),"_REPNO")))    # character
    
    # BARELY TESTED IN R CONSOLE ( SHOULD WORK )
    # columns
    dfnew <- dfnew[,grep("^X.*", colnames(dfnew), value = T, invert = T), drop = F]
    dfnew <- plyr::rename(dfnew, c("LASTMOD" = stringi::stri_c(toupper(stri_extract_first_regex(si_file, "(?<=si_).*")),"_LASTMOD")))  # LASTMOD is not in earlier data
    dfnew <- plyr::rename(dfnew, c("UPDATED" = stringi::stri_c(toupper(stri_extract_first_regex(si_file, "(?<=si_).*")),"_UPDATED")))  # boolean 
    dfnew <- plyr::rename(dfnew, c("REPNO"   = stringi::stri_c(toupper(stri_extract_first_regex(si_file, "(?<=si_).*")),"_REPNO")))    # character

    # rows
    dfnew <- dfnew[ !data.table:::duplicated.data.table(dfnew, fromLast = TRUE),,drop = FALSE]
    dfnew <- dfnew[ !stringi::stri_duplicated(dfnew$COMPANY_ID) & !stringi::stri_duplicated(dfnew$COMPANY_ID, fromLast = TRUE), , drop = FALSE]
      
    dfnew <- data.table::as.data.table(dfnew, keep.rownames=stringi::stri_c("RN_",toupper(si_file)))

    # df <- dplyr::left_join(df, dfnew, by = "COMPANY_ID") # SEEM(not SORTED) seem the same speed
    df <- data.table:::merge.data.table(df, dfnew, by = "COMPANY_ID", all.x = TRUE, sort = FALSE) # will sort LATER below
    
    message(stringi::stri_c("  End   ",si_file))
    
  }
  
  # last before save ( note: for this here to  be here this late, 'merge' above must do a 'left join' )
  df <- data.table::as.data.table(df, keep.rownames= "RN_SI_CI")
  # sort
  data.table::setkeyv(df, "COMPANY_ID")
  
  # fst keeps Dates as Dates
  # change Dates to integers
  df <- data.frame(plyr::llply(df, function(x) { if(is.timeBased(x)){ return(as.integer(zoo::as.Date(x))) } else { x } } ), stringsAsFactors = FALSE)  

  # fst keeps Booleans as Booleans
  # change Booleans to integers
  df <- data.frame(plyr::llply(df, function(x) { if(is.logical(x))  { return(as.integer(x)) }               else { x } } ), stringsAsFactors = FALSE)  

  df <- data.frame(plyr::llply(rlist::list.zip(as.list(df), colnames(df)), function(x) {  

    if(file_type == "fst") {
      # change to integer
      if(grepl("SIC|EMPLOYEES|^PERLEN_[Q,Y].*$|^RN_.*$",x[[2]])) { return(as.integer(x[[1]])) } else
      # if had been converted to an integer up to this point then keep as an integer
      if(is.integer(x[[1]])) { return(x[[1]]) } else
      # keep as character
      if(grepl("^PERTYP_[Q,Y].*$|^UPDTYP_[Q,Y].*$|^COMPANY_ID$|^COMPANY$|^TICKER$|^EXCHANGE$|^STREET$|^CITY$|^STATE$|^ZIP$|^COUNTRY$|^PHONE$|^WEB_ADDR$|^BUSINESS$|^ANALYST_FN$|^IND_2_DIG$|^IND_3_DIG$|^SP$|^DOW$|^.*CODE$|^.*DESC$|^.*REPNO$",x[[2]]))  { return(x[[1]]) } else
      # others become numeric
      { return(as.numeric(x[[1]]))}
    }
    # sqlite converts Booleans to integer ( I want, so I do not need to convert )
    # sqlite converts Dates to numeric ( but I want integer, so the 'change to integer' line must have more entries )
    if(file_type == "sqlite") {
      # change to integer
      if(grepl("SIC|EMPLOYEES|^PERLEN_[Q,Y].*$|^RN_.*$|LASTMOD|UPDATED|^PEREND_[Q,Y].*$|^PRICED.*$|^.*DATE$|^DATE.*$|^REPDT.*$",x[[2]])) { return(as.integer(x[[1]])) } else
      # if had been converted to an integer up to this point then keep as an integer
      if(is.integer(x[[1]])) { return(x[[1]]) } else
      # keep as character
      if(grepl("^PERTYP_[Q,Y].*$|^UPDTYP_[Q,Y].*$|^COMPANY_ID$|^COMPANY$|^TICKER$|^EXCHANGE$|^STREET$|^CITY$|^STATE$|^ZIP$|^COUNTRY$|^PHONE$|^WEB_ADDR$|^BUSINESS$|^ANALYST_FN$|^IND_2_DIG$|^IND_3_DIG$|^SP$|^DOW$|^.*CODE$|^.*DESC$|^.*REPNO$",x[[2]]))  { return(x[[1]]) } else
      # others become numeric
      { return(as.numeric(x[[1]]))}
    }

  } ), stringsAsFactors = FALSE)
  
  colnames(df) <- tolower(colnames(df))
  all_raw_by_dateindex <- df
  return(all_raw_by_dateindex)
  
}
# expermental: load MUCH
# fst files
# 3 seconds 
# all_raw_by_dateindex <- get_all_raw_by_dateindex(17562) # 
# sqlite files
# 6 seconds ( but much more flexible)
# all_raw_by_dateindex <- get_all_raw_by_dateindex(17562, file_type = "sqlite") 


# -- [X] ALREADY IN SIFINECON.01
# -- BEGIN OUTLIER DETECTION --
# 
# -- KEEP 
#   drop type r_lof4c_type;
# create type r_lof4c_type as (rn int, val int);
# 
# -- KEEP  -- NON PARALLEL VERSION
#              drop function fe_data_store.r_lof4c(in rn bigint[], in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int, in retcount int);
# create or replace function fe_data_store.r_lof4c(in rn bigint[], in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int, in retcount int)
#   returns setof r_lof4c_type as
# $body$
# 
#   Rlof___f.dist.to.knn <- function(dataset, neighbors, ...){
# 
#       m.dist <- as.matrix( Rlof:::distmc(dataset, ...))
#       num.col <- dim(m.dist)[2]
#       l.knndist <- lapply(c(1:num.col), function(i) {
#           order.x <- order(m.dist[, i])
#           kdist <- m.dist[, i][order.x[neighbors + 1]]
#           numnei <- sum(m.dist[, i] <= kdist)
#           data.frame(v.order = order.x[2:numnei], v.dist = m.dist[,
#               i][order.x[2:numnei]])
#       })
#       rm(m.dist)
#       maxnum <- max(unlist(lapply(l.knndist, function(x) {
#           dim(x)[1]
#       })))
#       i <- numeric()
#       knndist <- NULL
#       for(i in 1:num.col)
#           {
#               len <- dim(l.knndist[[i]])[1]
#               RES <- c(l.knndist[[i]]$v.order, rep(NA, (maxnum - len)),
#                   l.knndist[[i]]$v.dist, rep(NA, (maxnum - len)))
#               knndist <- cbind(knndist,RES)
#           }
#       knndist
#   }
# 
#   Rlof__lof <- function(data, k, ...){
# 
#       if (is.null(k))
#           stop("k is missing")
#       if (!is.numeric(k))
#           stop("k is not numeric")
#       data <- as.matrix(data)
#       if (!is.numeric(data))
#           stop("the data contains non-numeric data type")
#       v.k <- as.integer(k)
#       if (max(v.k) >= dim(data)[1])
#           stop("the maximum k value has to be less than the length of the data")
#       distdata <- Rlof___f.dist.to.knn(data, max(v.k), ...)
#       p <- dim(distdata)[2L]
#       dist.start <- as.integer((dim(distdata)[1])/2)
#       dist.end <- dim(distdata)[1]
#       ik <- numeric()
#       m.lof <- NULL
#       for(ik in v.k) 
#       {
#           lrddata <- Rlof:::f.reachability(distdata, ik)
#           v.lof <- rep(0, p)
#           for (i in 1:p) {
#               nneigh <- sum(!is.na(distdata[c((dist.start + 1):dist.end),
#                   i]) & (distdata[c((dist.start + 1):dist.end),
#                   i] <= distdata[(dist.start + ik), i]))
#               v.lof[i] <- sum(lrddata[distdata[(1:nneigh), i]]/lrddata[i])/nneigh
#           }
#           m.lof <- cbind(m.lof, v.lof)
#       }
#       if (length(v.k) > 1)
#           colnames(m.lof) <- v.k
#       return(m.lof)
#   }
# 
#   set.seed(1L)
#   res <- Rlof__lof(data = data.frame(col1, col2, col3, col4), k = k)
#   if(length(res) > 0L) {
#     res <- order(res, decreasing = TRUE)[seq(1,min(retcount,length(res)),1)]
#   } else {
#     res <- integer()
#   }
#   return(data.frame(rn[seq(1,min(retcount,length(res)),1)], res))
# $body$
#   language plr;
# 
# 
# -- [?] ADD netinc < sales ( BUT caught Duke Realty: netinc > sales : SEE BELOW)
# -- [?] ADD MUST have all entries(mktcap, netinc, sales, ncc, assets) over range search e.g. if go back 6 months THAT company MUST pass all filters
# -- -- --- not refound in this one: 'AB NAMARO' missng data AND one ENTRY wrong MKTCAP ( seems NOT in 500: is s european bank (ABN Amro))
# -- [X] ALREADY IN SIFINECON.01
# -- KEEP
# -- all s&p500 members ranked (most outlier-ish)(#1) to (least outlier-ish(#~500)
# select 
#     sq.dateindex, sq.company_id
#   -- -- required in join ( not necessary to view )
#   -- -- orderer ( not necessary to view ) ( but very useful in portability )
#   , sq_r3.rn  sq_r3_rn
#   , sq_r3.val sq_r3_val 
#   ---- redundant of above
#   --, sq.rn sq_rn
#   -- -- redundant payload ( not necessary at all ) 
#   , sq.sp, sq.ticker, sq.company, sq.price, sq.mktcap, sq.netinc_q1, sq.sales_q1 
# from 
#   ( -- sq, sq.rn(1,2,3,...)
#   select 
#       row_number() over (partition by qu2.dateindex order by qu2.company_id) rn
#     , qu2.dateindex, qu2.company_id
#     -- -- redundant payload
#     , qu2.sp, qu2.ticker, qu2.company, qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1
#   from query01 qu2
#   where qu2.sp = '500' and qu2.dateindex = 17409 order by qu2.company_id
#   ) sq, 
#   ( -- sq_r3 -- sq_r3.rn(1,2,3,...) sq_r3.val(137,316,43,...)
#     select (sq_r2.out).* --sq_r2.rn(NEED TO CARRY), sq_r2.val 
#     from
#     ( -- sq_r2
#       select r_lof4c(array_agg(sq_r.rn), array_agg(sq_r.price), array_agg(sq_r.mktcap), array_agg(sq_r.netinc_q1), array_agg(sq_r.sales_q1), k := 5, retcount := 20000) as out 
#       from 
#       ( -- sq_r
#         select 
#             row_number() over (partition by qu.dateindex order by qu.company_id) rn -- random
#           ,                                           qu.price,              qu.mktcap,              qu.netinc_q1,              qu.sales_q1 
#         from  query01 qu
#         where qu.sp = '500' and qu.dateindex = 17409 order by qu.company_id 
#       ) sq_r  
#     ) sq_r2
#   ) sq_r3
# where sq.rn = sq_r3.val
# order by sq_r3.rn
# -- ... k:= 20000(max rows of rowcount is returned)
# 
# -- TECHNOLOGY:WORKS
# -- LATERAL TO GET OUTLIER SP RECORDS IN THE DATABASE
# -- KEEP
# select                quo2.dateindex,      sro2.company_id ,sro2.query_rnk, sro2.sr_val
# from ( select distinct quo.dateindex from query01 quo where quo.sp = '500' order by quo.dateindex ) quo2
# join lateral 
#   ( -- sro2
#     -- COPY AND PASTE FROM ABOVE ( replace ".dateindex = 17409" using ".dateindex = quo2.dateindex"
#     -- WORKS
#   ) sro2 on true
# --30 seconds -> 88,000 rows (GOOD)
# 
# -- KEEP
# -- all s&p500 members except these (20) ones ( and I do not care about tracking 'rank' )
# select 
#     qu3.dateindex, qu3.company_id
#   , qu3.sp, qu3.ticker, qu3.company, qu3.price, qu3.mktcap, qu3.netinc_q1, qu3.sales_q1
# from query01 qu3
# where qu3.sp = '500' and qu3.dateindex = 17409 and ( qu3.dateindex, qu3.company_id ) not in 
#   ( 
#   -- k:= 20
#   ) -- xyz: must not be an alias 'here'
# order by qu3.dateindex, qu3.company_id
# -- 480 rows
# 
# 
# -- END OUTLIER DETECTION (THIS WORKS) --



# LATER HARD_CODE program in sector_desc, industry_desc 

# [ ]
# need a a 'table rename' function that also renames 'indexes/primariy keys'




#### BEGIN WORKFLOW ####

# SOON [ ] **HIGHER** PRIORITY
# TO DO: FIX EVERYWHERE FOUND
# REPLACE   company !~~ '%iShares%' ...   with sp in ('500','400','600')

# LEFT_OFF

# ZERO
# [x] RE-ORGANIZE some R CODE in TEMPORARY.sql
# [x] save on GITHUB
# 
# BEFORE_FIRST (SOMEWHERE)
# [X]RENAME SOME COLUMNS to CLEAR they ARE FUTURE ( SEE SECOND )
# [x] BONDS
# -- TO DO [X] ( RENAME PRICE FUTURE CALCULATION SOMETHING BETTER _ann ALONE DOES NOT WORK )

# FIRST
# [code implemented plr extension to call R/NO_FIRST_MASS_RUN_YET] agrep ( join old companies with name change )
# Approximate String Matching (Fuzzy Matching)
# https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/agrep
# 
# SECOND ( SOME FUTURE TIME )
# [ ] HINT - DO NOT BOTHER TO UPDATE A ROW THE SECOND TIME
# SO ADD COLUMNS and USE IN detect QUERIES
#        chg_xxw_annz_uptodate
# nowlast_inbnd_stmtz_uptodate
# 
# THIRD (WEAK - FEWER 'BIG' QUERIES) ( SOME FUTURE TIME )
# [ ] ALTERNATE - 'everything at ONE time' update method
# (may be good when the same page keeps getting rewritten 
# and/or lots of page contention and MUCH writing, so there exists MUCH waiting for disk

# SOON
## --       [ ] 3 SQL now UPDATE company_idS to NEW company_ids use fuzzy matching in pl/r: ( SEE above: fuzzy matching by Levenshtein )
## --       [ ] <UPSERT> to INSERT/UPDATE ( in prep for partitions ) - CONVERT FROM INSERT(CONFLICT - TO INSERT(NOT EXSITS...UPDATE(EXISTS

## SOON     ( SOME FUTURE TIME (BUT SIMPLE)
## --       [ ] PERFORMANCE: convert pg_temp TO NONDEBUG mot to TEMP TABLE: TEMPORARY/table_index ( for non-debug ) runs ( HALF the DISK IO )
## --         [ ] if exists TRUNCATE TABLE 
## --         [ ] PERFORMANCE trg/src and/or ALSO 'query the PostgreSQL database' to NOT select already 'currented' records


# [ ] 
# OTHER THAN GSPC ( AND i NEED YEAR MEASURE )
# https://en.wikipedia.org/wiki/Thrift_Savings_Plan
#  https://en.wikipedia.org/wiki/Bloomberg_Barclays_US_Aggregate_Bond_Index AGG
#      iShares Core US Aggregate Bond Index (AGG) Vanguard Total Bond Market Index Fund (VBMFX), Fidelity U.S. Bond Index Fund (FBIDX)
#
# ... caret ... xgboost or random_forest  GSPC momentum vs itself ( NICE OUTPUT it THEN WORK BACKWARDS )
# 
# --TODO: [META/DATA THERE BUT NO SQL RAN YET] ADD sp500_total_shares mktcap / price ( to demonstrate buyback in action )
# --TODO: [META/DATA THERE BUT NO SQL RAN YET] ADD mktvalue weight of those that have reported within the last month
# --          [ ] ADD yoy(q1 v.s. q5) pctchg of those that have reported last and 'not reported in the previous month'
# --TODO: Ratio adjustment for those dateindex that do NOT have exactly 500 firms
#         NOTE: generally needed ONLY in ABSOLUTE measures: RELATIVE measures netinc/mktcap NOT NEEDED
# --TODO: [DONE - AUTOMATIC NUMERIC(x,2) ALTER column width expander] 
#           load GetSymbols into the database  # make gspc volume fit, make large numerics fit
#              FIX: load_obj_direct ... FIX: other places
#         [x] How do I determine the number of digits of an integer in C?

# 
# with-'left join lateral' query gnerator

# PROBABLY WOULD WANT
#  Dates and Periods
#    Data Table Name: PERLEN_Q1, Data Table Name:                     [X]
#    pertyp_q1, q2, q3, q4, q5, q6, q7, q8 Q2, Q3, Q4, Q5, Q6, Q7, Q8 [X]
     

# Earnings and Estimates
#   Date--Latest Quarterly EPS
#     Data Category: Dates and Periods
#     Data Table Name: DATE_EQ0  ( SO LOAD THIS ) [X]
#     Field Type: Date (MM/DD/YYYY)
#       The ending date of the last completed fiscal quarter.
# Dates and Periods

# ALREADY HAVE AND 'HAVE HAD'
# fe_data_store.si_finecon2 ADD COLUMN perend_q1 integer;
#
# Dates and Periods
#  Ending date Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8
#    Data Category: Dates and Periods
#    Data Table Name: PEREND_Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8
#    Field Type: Date (YY/MM)
#    The last day for each of the last eight fiscal quarters.



# # LEFT_OFF
# 
# # -- Index Scan using si_finecon2_dateindex_company_id_key
# # -- 6608 rows
# # -- 0.7 seconds
# # 
# # requires sales_q1, netinc_q1, mktcap, price, date_eq0, perend_q1, pertyp_q1, perlen_q1
# # 
# wayback_machine_last_month_sales_netinc <- function(x) {
# 
#   verify_connection() # require(PivotalR)
# 
#   x <- as.integer(x)
# 
#   dbGetQuery(conn, paste0("
# 
#     select
#       now.dateindex
#     , now.company_id
#     , p01lwd.dateindex     p01lwd_dateindex
#     , p01lwd.company_id    p01lwd_company_id
#     , p01lwd.sales_q1      p01lwd_sales_q1
#     , p01lwd.netinc_q1     p01lwd_netinc_q1
#     , p01lwd.mktcap        p01lwd_mktcap
#     , p01lwd.price         p01lwd_price
#     , p01lwd.date_eq0      p01lwd_date_eq0
#     , p01lwd.perend_q1     p01lwd_perend_q1
#     , p01lwd.pertyp_q1     p01lwd_pertyp_q1
#     , p01lwd.perlen_q1     p01lwd_perlen_q1
#       from
#         ( select * from si_finecon2 now  where now.dateindex = ",x," ) now 
#           left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and 
#                                                 now.company_id       = p01lwd.company_id 
#   ")) -> res
#   return(res)
#   
#   # note: ADD more LEFT OUTER JOINS to include MORE months 
#   # ( BUT!! better OFF loading them SEPARATELY in ANOTHER function & call )
#  
# }
# 
# # USAGE
# # 
# # -- Index Scan using si_finecon2_dateindex_company_id_key
# # -- 6608 rows
# # -- 0.7 seconds
# # wayback_machine_last_month_sales_netinc(dir_i) -> si_all_g_df
# # upsert(si_all_g_df, keys = c("company_id"))


# dplyr cubes
# data.cube ( Jan Gorecki )
# dwtools   ( Jan Gorecki )
# multitable ( Steve Walker )
# Charles Driver transforms


# ANDRE NEW PLAN - INBOUND
# ------------------------

 
# MAKE sure the db_temp HAS and INDEX ?
# [warning SURVIAL bias IN DATA missing future should have been -100 coalese(x,-100.00) elseif(is.na(x), -100.00)
#   STILL put in the COMPLETION of the MODEL data
# 
# [ ] redo SIMPLIFY the data imput 
# ( PivotalR )
# .db.writeTable.rpostgresql
# add.row.names
# is.temp
#   .db.buildTableDefinition
#     field.types
# ( RPostgreSQL )
# PostgreSQLSupport.R                                                                             -- LITERAL ENTRY
# postgresqlWriteTable <- function(con, name, value, field.types, row.names = TRUE,   -- field.types: 'numeric(9,2)' I query the df, I query db: max(max(db.col),max(df.col))
#                                  overwrite = FALSE, append = FALSE, ..., allow.keywords = FALSE) {
# COPY
# # PostgreSQLSupport.R     
# # THIS is what I NEED to APPEND
# # 
# # append records to table ( NOTE: no option to create a temp table )
# # overwrite = FALSE
# # append    = TRUE
# # 
# # COPY
# # 
#
# consider
# caroline::dbWriteTable2 ( considert: makes the source df have the same col structure the target db then COPY )
# 
# ( pg )
# pgWriteTable 
#   dbWriteTable(NEW) row.names, append, match.cols(NEW)
# [ ] redo SIMPLIFY the upsert
# (  pg )
# pgSendUpsert  ON CONFLICT
#   pgUpsertTable
#   pgWriteTable
# ( rpostgis ) 
# pgInsert ON CONFLICT
# ( my own : Prepare FOR postgresql 10 PARTITION BY )
# WITH UPDATE RETURNING * SQL INSERT NOT RETURNING
# xor
# BEGIN UPDATE EXISTS INSERT NOT EXISTS END
# ( statar )
# join ( if (update) : option update ONLY is.na VALUES - could be EXTENDED - ) ( MAYBE handle a DATABASE object? )
# THEN
#   plyr::join_all ( COOL )

# ANDRE measureS ( WHERE make SENSE )
# RECIPRICOL_PCTCHG  4% -> 5%   4/100 -> 5/100 = 1/100
# and
# ABS_PCTCHG
#
# ANDRE: True_Stortino  (tsortino SEE PostgreSQL notes) ( see R function parameter ) # ALso see 
# ... of ... OTHER markets(that crash) before MAIN us MARKET ( ANDRE: earthquake ) ( SEE BELOW ON INTERNATIONAL BONDS )
# 
# ANDRE
# what makes $1 stock companies RISE ( like FORD did? ) dollar companies ...

# [x]
# change 'company_id_orig' -> 'company_id' ADJUST
#  FROM 'whatever now' TO 'at least 2 of 3' to make update
#    2of3SAME(company, ticker, street)  -> company_id ADJUST 
# [ ] run on earlier data ( NOT YET )

# 1ST SW
# performance - HIGH (*) [DONE - options( ...  CREATE TEMPORARY TEABLE ...)
#  tbl_temp: CREATE TEMPORARY table tbl_tmp (prevent 'disk backup') on crash -re-use con create tbl_temp_real as select * from tbl_temp
#  (framework) series: 
#       mm_prchg_ann  
#       prchg_f  
#       inbnd_stmtstat
#     series_inbnd_stmtstat_last_update_dt (HEAVY PERFORMANCE) 
#     series_inbnd_stmtstat_good_update(filled) - queries selection on this to NOT try to update
# trg and src ALSO select upon database ( LIMIT the worthless re-updates AND disk-io )
# 2ND HW
# peformance - RAM disk - put 'set tablespace..' in session
# 3RD perf - databaser pg 10 PARITIONS
#   change UPDATE DO CONFLICT -> WITH X UPDATE RETURNS * SQL INSERT NOT *
#                             -> BEGIN TRANS; UPDATE EXISTS, INSERT NOT EXIST, END TRANS
#  AND/OR buy 'other' SSD disk (PUTTEMP/tablepaces there? )
#   create tempoary table 
# SEE ANY 'to do' notes in TEMPORARY.sql
# [ ] FIX everywhere object to/from xts conversons
# [ ] verify OUTPUT renaming on ALL functions
# [x] (2 places) fix inbound fo a numeric() is check for mAX(value) then target destination table column is INCREASED to fit that data
# NOTE: 200x COST: fundamental mistake ( queries (and the main index) SHOULD have been 
#   based on dateindexeom ( an not on dateindex SO  CAN JOIN with other e.g. BONDS ) .... NO! O.K. like it is
# [ ] change dateindex -> dateindex_dt adr_bl ( to show that the stored TYPE is not the actual type in MEAINING
# NEW data
# % increase in 40 year olds in the population ( SEE my NEW datailed DATA on this )
# aaii buyback yield
#   BUYBACK_YIELD: maybe the ONLY reason that a STOCK price goes up [X] loaded
# [x] Inbound Sales/Market,Net_Income/Market,Net_Income/Sales 
#   [ ] by (since last time, since last year this time ) ... can/will be done using xts
# [x] derived data rations:  price, net_income, sales -  [x]
# perhaps MORE from ( through library(Quandl? ): http://www.multpl.com/  # 
# MY FIX and/or *new* DATA from # ALT Tech Indicator Sites: library(AlphaVantageClient WORKS_WELL NEW SEP 2017 (daily, adj close, sma )
#   qmao::.getEconomicCalendarBriefing ( my fix of ) # and/or/xor # library(censusapi # American Community Survey
# INBOUND DATA  
#   missing data
#   [TO REVIEW CODE]  na.locf
#   [TO REVIEW CODE] flag column of na.locf
#   [TO REVIEW CODE] time in days since last measure ( higher values for na.locf ) ( see my Philadelpha/Cleveland ... "Forecasters" )
# OTHER
# Real_Sortino of monthly stock returns ('risk')
# Gold & Silver sector returns ('fear')
# financial stress index and components data provided by the xFederal Reserve Bank of Clevelandx ('risk/fear') SEE MY OTHER NOTES
# 
# Chicago Fed National Financial Conditions Index Nonfinancial Leveral Subindex (NFCINONFINLEVERAGE) 'awesome'
# Leading Index for the United States (USSLIND)
# Chicago Fed National Activity Index (CFNAI)
##
# ABSOLUTE RETURN OF STOCKS OVER BONDS ( COMETITION PRESSURE )
# NOTE: I MAY WANT A SHORT 'YIELD RANGE: E.G. 3 MONTHS)
#   (Wilshire 5000 Total Market Index (WILL5000IND) - lag.xts(Wilshire 5000 Total Market Index (WILL5000IND),220) - 
#   (BofA Merrill Lynch US Corporate BBB Effective Yield (BAMLC0A4CBBBEY))
# NOTE MAY WANT TO SUBTRACT OFF INSTEAD  ( AGG - lag.xts(AGG,220) )
# SEE https://fredblog.stlouisfed.org/2016/07/the-equity-premium/
# 
# # when unemployement is 'increasing' with inflation TOO HIGH
# # WHAT 'tower' the fed does to 'slam' before(into) a recession((lateprevyear?)2000,(late2007)2008)
# FRBData::GetInterestRates("FF")
# FRBData::GetInterestRates("DWPC")
# 
# library(OECD) # from xml2 feed (IF ANY GOOD)
#
# MULTPL/SP500_EARNINGS_MONTH-S-P-500 'awesome'
#
#   Real_Sortino of Gold & Silver sector returns('risk')
#    NEG inverse SORTINO - from ANYWHERE WHERE APPROPRIATE
# Large ticket sales (MORE APPROPRIATE fo FIND if these MEAN anything: see FURNATURE )
#   Housing Indusry
#   Automobile Indusry
# OTHER countries returns ON foreign bonds ( historical data ) - SEE my economic indicator NOTES
# other countries tsortino ON foreign bonds                    - see
#   rate of returns
#   tsortion
# REAL COMPETITON ('Return(per dollar) on bonds')
#   Real_Sorino of Competition('risk')
# Price/Volumn Momentum
# Price/Volume 'PCTCGH' m01b22d m01bw04 ( month 01 by 22 days (yahoo-ish stuff), month 01 by w04 ( aaii-ish stuff ) 
#  OVER and/or above/below/neg_tsortino AS APROPRIATE
# Inflation ('free growth') [ over Competition('Return on bonds')]
# Ratio of Stock_Risk/Bond_Risk
# Ratio of Stock_Return/Bond_Return
# Prevent output preduction 'program made'SMOOTHING
#   PREDICT on ONE only ONE item loop predict(1); rbind(1) end loop
# 
# [ ] AFTER 2017 WINTER VACATION 
# WHAT REALL MATTERS
# 1.PHILDELPHIA FED SURVEY OF FORCASTERS(forecasters) 
#   UNEMPLOYMENT RATE vs TRUE UNEMPLOYMENT RATE - above(1)/below(2) TRUE & long term trend AND SLOPE
#     unemp_unemp3_median
#   also notice: 12 month loss in net_income and slightly( even just one percent loss in jobs (1% up unempoy rate) before a recession
#   also notice: impossiblity/unlikeliness of going higher/lower
# 2. CHICAGO(fed gov) FED STRESS INDEX ( will curve flat then go just a little down before MAJOR recession ) nfcinonfinleverage
#    # ALSO NOTICE: ret and dwpc plotted
#    # ALSO NOTICE: clev_easing_balances
#    Note: 'seems to cover 'some' bond competition' + stress/relax that the FED places down to keep_down_inflation/secure_markets
#    Note: OTHER STRESSORS consider: EARNINGS/PRICE ratio wrt LONG TERM POSITIONS ( sp500_pe_ratio_month_4q )
# 3. WILLSHIRE50000IND vs AGG( 3,6,9,12 mo returns ) equity_prem_p03/6/9/12m_ann (competition) # SIMILAR: zimmermann_equity_premium
#    Note: seems somewhat reflected in near same AAII: bull_bear_spread (investors)
#    [ ] AFTER VACATION
# 4. MISSING: NEED TO FIND/ADD " ave corporation liabilites "
#   Interest expense  Q1 INT_Q1
#   Total liabilities Q1 LIAB_Q1
#   (VERY INTERESTING ... ESP 2015-EARLY_2016) ( I tweeted Jeff Cox )
#   https://fred.stlouisfed.org/series/B471RC1Q027SBEA
#   HUMOR READ
#   https://www.cnbc.com/2017/12/15/surge-in-investor-cash-to-stocks-triggers-more-melt-up-fears.html
# ...
# OTHER
# finecon01
# [x] fe - add common column ere_q1(-8) exhcange rate effects
# [x] document on how to do it
# [FULLDONE] ere_q1 ( HALF/DONE ) - FROZEN
#   further to fix
#   x new columxn [X] x( or -5 and round up ) x
#   fe_data_store.si_finecon2_aggregates
#     (dateindex integer, need eom (MAY BE 'in the time before' dateindex)
#   new column [ ] dateindexp12alb -- absolute last day (same as dateindex)
#    on ( [ ] queries [ ] index defs [ ] testing ) in ('a','b') to become in (values ('a'), ('b'))
#   [ ] insert on confict update [ ] ... begin update, insert end
#     [ ] convert big_table to PARTITION table
# [x] make dynamic
#   finance_econ=# vacuum analyze verbose fe_data_store.si_finecon2;reindex (verbose) table fe_data_store.si_finecon2;
# NEAR FUTURE
#   [ ] test RPostgreSQL old package -> RPostgres new package
# SOMETIME FUTURE
#   [ ] check garantee ... IN( VALUES (), (), () )
#   [ ] (upsert -> insert/select) partitions MAJOR)
# ...
# [ ] DONE - BUT [ ] SAT FIX
# [ ] INSTALL [ ]  fincon01.R-ize ( monthly install )
# http://www.aaii.com/stock-investor-pro/archives
#   update: C:\Users\AnonymousUser\Downloads\stockinvestorinstall_20170929.exe
# eom dbs -> db in R upload_lwd_dbs_to_db 
# > zoo::as.Date(17438)
# [1] "2017-09-29"
# -- last month ( look for 4 week loaded returns )
# select * from fe_data_store.si_finecon2 where dateindex = 17409 and ticker in ('MSFT','AAPL')
# -- current(this) month ( look for general data )
# select * from fe_data_store.si_finecon2 where dateindex = 17438 and ticker in ('MSFT','AAPL')
# -- curent(this) mont ( look to see that the aggregates have been ) processed
# -- FIX [ ]- MISSING *_last*_ ( prbably also *_now_* ) AND _*sum*_derivatives)
# select * from fe_data_store.si_finecon2_aggregates where dateindex = 17438 
# -- FIX -[ ] - DID NOT RETURN ANY RECORDS
# 
# [NA] TEST ... SET 'bigint' arg IF NECESSARY
#   NEW R package RPostgres ( seems !?! case finecon01.R not use dbplyr )
# ...
# **NEW**
# (corps can not get 'debt' funding)
# Fall before each recession (but NOT RECESSIONS 'BEFORE 2000') AND 'late 2015-earl 2016 slowdown'
#   Nonfinancial Corporate Business; Corporate Equities; Liability, Level
#   https://fred.stlouisfed.org/series/MVEONWMVBSNNCB
#     !!! [ ] TOBE INVESTIGATED - bumpy area - Q1-2010 ... Q4-2012 !!!
#        Updated: Dec 7, 2017
# ...
# **NEW**
# YALE- U.S. Crash Confidence Index - [DONE-USELESS]  SUNDAY - TO BE INVESTIGATED ... LOOK AT THE 'OFTEN HUMPS'
# **ALSO NEW*
# survey of professional forecasters 4 unemp BEFORE RECESSION (GOOD - PESSIMISTIC/OPTIMISTIC)
# ...
# Shrinkage vs. Growth
# --------------------
# BUYBACKS - TO INVESTIGATE ... RUN THE QUERY [x]  -- see TEMPORARY.SQL? ... no rank correlation ... wasted money!
# May 4th, 2016
# http://investorfieldguide.com/shrinkage-vs-growth/
# AS OF DEC 17 2017
# Shrinkage vs. Growth
# http://investorfieldguide.com/blog/page/9/
# ...
# Chicago Fed National Financial Conditions Index Nonfinancial Leveral Subindex (NFCINONFINLEVERAGE)
# https://fred.stlouisfed.org/series/NFCINONFINLEVERAGE
# Leading Index for the United States (USSLIND)
# http://research.stlouisfed.org/fred2/series/USSLIND
# Chicago Fed National Activity Index (CFNAI)
# https://fred.stlouisfed.org/series/CFNAI
# #
# # ABSOLUTE RETURN OF STOCKS OVER BONDS ( COMETITION PRESSURE )
# # NOTE: I MAY WANT A SHORT 'YIELD RANGE: E.G. 3 MONTHS)
# # WILL5000IND - since DEC 1970
# #   (Wilshire 5000 Total Market Index (WILL5000IND) - lag.xts(Wilshire 5000 Total Market Index (WILL5000IND),220) - 
# #   (BofA Merrill Lynch US Corporate BBB Effective Yield (BAMLC0A4CBBBEY))
# # NOTE MAY WANT TO SUBTRACT OFF INSTEAD  ( AGG - lag.xts(AGG,220) )
# # SEE https://fredblog.stlouisfed.org/2016/07/the-equity-premium/
# # 
# # # when unemployement is 'increasing' with inflation TOO HIGH
# # # WHAT 'tower' the fed does to 'slam' before(into) a recession((lateprevyear?)2000,(late2007)2008)
# # FRBData::GetInterestRates("FF")
# # FRBData::GetInterestRates("DWPC")
# # 
# by 3-month (4 quarters rolling) PERCENT_RANK comparison ( abs(change in idex value) )
# by 12-month (4 years rolling) percent rank comparison    ( abs(change in index value) )
# AGG-^GSPC ( abs(change in index value ) )               [X] HAVE ENOUGH
# AGG-^GSPC ( abs(change RoR(net_income DB,MUPL)/dollar ) [X] HAVE EQOUGH
# UNEMP ( abs(change in index value ) ) [X]
# New York 'Conference Board' predictions (FIND IT) - FOUND IT - NOTHING TO SCRAPE
# .getEconomicCalendarBriefing (htmltab) ... maybe...convert into an XTS object
# blscrapeR: An API Wrapper for the Bureau of Labor Statistics (BLS)
# [x] !!! - tradingeconomics.com
# R LEFT_OFF ... factores/values to numbers
# [ ] OTHER2 ( at file top )
#   + Bank for International Settlements
# C:\Users\AnonymousUser\Desktop\WORKING.txt
# ...
# [ ] KEEP THIS - MATCH WITH NEW FRED NET_INCOME (if any can find: see FRED NEWS)
# https://news.research.stlouisfed.org/2017/11/fred-adds-quarterly-financial-report-data/
# ...
# panic button ?!? - MAYBE ABLE TO FORCE A 'FALSE RECESSION' ?
# ------------------------------------------------------------
# ...
# ( NOT THE MOST POPULAR ONE - BUT THE *ONE* THAT WORKS )
# mid 2015 - early 2016
# Exports of Goods and Services, Balance of Payments Basis (BOPTEXP)
# Millions of Dollars, Monthly, Seasonally Adjusted
# https://fred.stlouisfed.org/series/BOPTEXP
# ...
# Exports of goods and services from the United States from 1990 to 2015, as a percentage of GDP
# https://www.statista.com/statistics/258779/us-exports-as-a-percentage-of-gdp/
# ...
# Shares of gross domestic product: Exports of goods and services (B020RE1Q156NBEA)
# https://fred.stlouisfed.org/series/B020RE1Q156NBEA
# ...
# DEC 2017


# 
# ANDRE NEW PLAN - PROCESSING and OUTBOUND
# ----------------------------------------
# DATA, DATA-CLEANING, PREDICTION
# 
# require(tidyquant); require(timekit/timekt); my Zachary Ma
# require(wrapr)
# require(Boruta)
# require(UBL)
# require(vtreat)
# 
# workflow
# --------
#
# Hmisc::varclus
# 
# add [my] transforms(<none>/ROLLING/SMA/PCTCHG/LAG[/CORRELATION]/QUANTILE(FINDINTERRVAL) &+ _WEIGHTED .. &+ _12_MO_SEASONAL-> 
#     time(in_days)_since_report_release_date
#     time(in_days)_since_asof_data_date
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

# ---------------------------------------------------------
# ---- last updated VERY EARLY MORNING SUN DEC 03 2017 ----
# 
# Andre_Mikulec@Hotmail.com
# 
# <short_usual>+<code>+<code>WITH RULES(JUST IN TOP OF FILE)
#   Andre_R_AAII_SIPro_rcom_AppActivate_RDCOMClient_SIPro_40_Install.txt
# 
# C:\PG-10._
# C:\PG-10._\PG-10._.bat
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
# W:\R-3.4._
# W:\R-3.4._\R-3.4._.bat
# > shell("rstudio", wait = FALSE)                           # WHITE
# W:\R-3.4._                                             (FILES in HERE) : finecon01.R  finecon01_more_SQL.sql
# SET upsert_temp TO make a temporary table
# rm(list=setdiff(ls(all.names=TRUE),c("con","cid"))); debugSource('W:/R-3.4._/finecon01.R'); debugSource('W:/R-3.4._/goodsight01.R');verify_connection();options(upsert_temp_is_temporary=Inf)# 
#
# # install ..
# # stockinvestorinstall_<lastbusinessday>.exe

# > getAAIISIProDate() # Reads: C:/Program Files (x86)/Stock Investor/Professional
# [1] "17500"
# 
# > zoo::as.Date(as.integer(getAAIISIProDate())) # reads directly from # C:/Program Files (x86)/Stock Investor/Professional/Setup.dbf
# [1] "2017-11-30"                                # MUST BE THE 'last weekday of the lastmonth' (CAN! (and has_been! Christmas))
# 
# 
# # will create the folder in the Monthlies directory ##### ( DO NOT FORGET TO DO ) #####
# copyAAIISIProDBFs(
#     from = "C:/Program Files (x86)/Stock Investor/Professional"
#   , to   = paste0("W:/AAIISIProDBFs/",getAAIISIProDate()) # # Reads: C:/Program Files (x86)/Stock Investor/Professional
# )
# 

# # # CASE: to update past (missing) data with FUTURE data:  
# # # will create the folder in the NonMonthlies directory 
# # copyAAIISIProDBFs(
# #     from = "C:/Program Files (x86)/Stock Investor/Professional"
# #   , to   = paste0("W:/AAIISIProDBFs_NonMonthlies/",getAAIISIProDate()) # # Reads: C:/Program Files (x86)/Stock Investor/Professional
# # )

# 
# SET upsert_temp TO make a temporary table
# rm(list=setdiff(ls(all.names=TRUE),c("con","cid"))); debugSource('W:/R-3.4._/finecon01.R'); debugSource('W:/R-3.4._/goodsight01.R');verify_connection();options(upsert_temp_is_temporary=Inf)
# 
# after the END OF the program #
#   upload_lwd_sipro_dbfs_to_db
# 
# RUN (whatever 'code' it may be)' 
# RUN ( SEARCH BELOW )
# # typical *new month*
# { upload_lwd_sipro_dbfs_to_db ...
#   ...
# }
# 
# # relook ( updated future date columns )
# # view last months data
# set search_path to sipro_data_store,sipro_stage;
# 
# select max(dateindex) from fe_data_store.si_finecon2;
# 
# # view last months data
#
# # 'just loaded month' ( check aggregates loaded )
# select * from fe_data_store.si_finecon2_aggregates where dateindex = 17500 where ticker = 'AAPL';
# 
# # the previous month ( check future prices loaded )
# look at column prchg_04w_ann ( of previous month now should be filled )
# select prchg_f04w_ann, pradchg_f04w_ann from fe_data_store.si_finecon2 where dateindex = 17470 where ticker = 'AAPL';
# 
## the 3 months ago ( check future prices laoded )
# look at column prchg_13w_ann ( of previous month now should be filled )
# select prchg_f04w_ann, pradchg_f04w_ann, prchg_f13w_ann, pradchg_f13w_ann from fe_data_store.si_finecon2 where dateindex = 17409 where ticker = 'AAPL';
#
# -- last month ( look for 4 week loaded returns )
# select * from fe_data_store.si_finecon2 where dateindex = 17470 and ticker in ('MSFT','AAPL')
# -- current(this) month ( look for general data )
# select * from fe_data_store.si_finecon2 where dateindex = 17500 and ticker in ('MSFT','AAPL')
#
# select count(*) from fe_data_store.si_finecon2 where dateindex = 17500;
#
# # MAY NOT? BE DOING ANYMORE?
# # since I now have bonds ( make sure they are loaded )
# # # select * from fe_data_store.instruments where dateindex >= ( select ( max(dateindex) - 366) from fe_data_store.instruments );
# 
# C:\PG-10._>"C:\PG-10._\bin\pg_ctl" -D "C:\PG-10._\data" stop -m fast"
#   any wierd AVG interference? ... then re-cycle PostgreSQL ... make sure
# 
# ---- end VERY EARLY MORNING VERY EARLY MORNING SUN DEC 03 2017 ----
# -------------------------------------------------------------------

#### MONTHLY METHOD ENDS ####
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
# -- LATELY - TYPICALLY 2 TO 5 MINUTES


#### QUERIES ENDED ####

# TO DO: FIX EVERYWHERE FOUND
# REPLACE   company !~~ '%iShares%' ...   with sp in ('500','400','600')

# a market cap of $5.3 billion
# its headquarters in the U.S.
# the value of its market capitalization trade annually
# at least a quarter-million of its shares trade in each of the previous six months
# most of its shares in the public's hands
# at least half a year since its initial public offering
# Four straight quarters of positive as-reported earnings.
# 
# http://www.investopedia.com/articles/investing/090414/sp-500-index-you-need-know.asp?optly_redirect=integrated&lgl=vtas-baseline

# -- last ONE: others after: ONE: 15856 ("2013-05-31")
# 
# "XLY";"Consumer Discretionary SPDR (E"
# "XLE";"Energy Select Sector SPDR (ETF"
# "XLF";"Financial Select Sector SPDR ("
# "XLV";"Health Care SPDR (ETF)"
# "XLI";"Sector Spdr Trust Sbi"
# "DGT";"SPDR DJ Global Titans (ETF)"
# "RWX";"SPDR DJ International Real Est"
# "DIA";"SPDR Dow Jones Industrial Aver"
# "ELR";"SPDR Dow Jones Large Cap ETF"
# "EMM";"SPDR Dow Jones Mid Cap ETF"
# "RWR";"SPDR Dow Jones REIT ETF"
# "TMW";"SPDR Dow Jones Total Market (E"
# "FEZ";"SPDR EURO STOXX 50 ETF"
# "GLD";"SPDR Gold Trust (ETF)"
# "KBE";"SPDR KBW Bank (ETF)"
# "KCE";"SPDR KBW Capital Markets (ETF)"
# "KRE";"SPDR KBW Regional Banking (ETF"
# "MTK";"SPDR Morgan Stanley Technology"
# "MDYG";"SPDR S&P 400 Mid Cap Growth ET"
# "MDYV";"SPDR S&P 400 Mid Cap Value ETF"
# "SPY";"SPDR S&P 500 ETF Trust"           ****
# "SPYG";"SPDR S&P 500 Growth ETF"
# "SPYV";"SPDR S&P 500 Value ETF"
# "SLY";"SPDR S&P 600 Small Cap ETF"
# "SLYG";"SPDR S&P 600 Small Cap Growth"
# "SLYV";"SPDR S&P 600 Small Cap Value E"
# "XBI";"SPDR S&P Biotech (ETF)"
# "SDY";"SPDR S&P Dividend (ETF)"
# "XHB";"SPDR S&P Homebuilders (ETF)"
# "KIE";"SPDR S&P Insurance ETF"
# "XME";"SPDR S&P Metals and Mining (ET"
# "XES";"SPDR S&P Oil & Gas Equipt & Se"
# "XOP";"SPDR S&P Oil & Gas Explore & P"
# "XPH";"SPDR S&P Pharmaceuticals (ETF)"
# "XRT";"SPDR S&P Retail (ETF)"
# "XSD";"SPDR S&P Semiconductor (ETF)"
# "FEU";"SPDR STOXX Europe 50 ETF"
# "XLK";"Technology SPDR (ETF)"
# "XLU";"Utilities SPDR (ETF)"
# 
# 
# 
# select distinct company from si_finecon2 where industry_desc = 'Misc. Financial Services' order by 1;
# 
# industry_desc = 'Misc. Financial Service' (since fall of 2010) -- TODO [ ] 2003-2010
# ---------------------------------------------------------------------------------------
# 
# -- mistake or (later) rename
# "iGen Networks Corp"
# "IGEN Networks Corp"
# "Jupiter Enterprises Inc"
# "Jupiter Enterprises, Inc."
# "Komodo, Inc"
# "Komodo, Inc."
# "Peregrine Industries Inc"
# "Peregrine Industries Inc(NDA)"
# "Rahaxi Inc"
# "Rahaxi, Inc."
# "Resource America Inc"
# "Resource America, Inc."
# "Spi Energy Co Ltd"
# "Spi Energy Co Ltd (ADR)"
# "Star Energy Corp(NDA)"
# "Star Energy Corporation"
# "Stellar Resources Ltd"
# "Stellar Resources Ltd(NDA)"
# "Thrive World Wide Inc"
# "Thrive World Wide, Inc."
# "Tintic Gold Mining Co"
# "Tintic Gold Mining Co(NDA)"
# "Zaxis International Inc"
# "Zaxis International, Inc."
# 
# --industry_desc = 'Misc. Financial Services'
# --si_finecon2(late 2010) -- TODO [ ] 2003-2010
# --%XXX% unless otherise noted
# 
# Capital
# BlackRock
# BLDRS
# Holding
# Cohen & Steers%
# "Consumer Staples Select Sect."
# Fund
# Investment
# "Direxion"
# Dow%
# Dreyfus
# DWS
# Dividen
#   Dividend
# Fnd
#   Fund
# Eaton Vance
# Ventures
# Empire Global ?
# Everyware Global ?
# Technologies
#   Technology
# FinTech Acquisition
#   Acquisition
#     Acquisit
# First Trust
#   Trust
# Merger
# Global%
# Guggenheim
# Helios
# ING%
# Invesco%
# iShares%
# John Hancock%
# Kayne Anderson%
# Total Return
# Leg Mason% (just one)
# Market Vectors
# ETF
# Merrill Lynch
# Trus
# "Montgomery Street Income Secur"
# Morgan Stanley%
# Holdi
#   Holdin
# "Income & Growth"
# Neuberger Ber%
# Income Fund
# Nuveen%
# Investment
#   Investors
# "Peoples Federal Bancshares, In"
# Pimco%
# PIMCO%
# Pioneer%
# PowerShares%
# ProShares%
# Putnam%
# RevenueShares%
# RMK%
# ROI%
# Royale%
# Royce%
# Rydex% (S&P derivatives)
# Resource Corp
# Investment Corp
# SPDR%
# Sunrise%
# Templeton%
# Tortoise%
# United States%
# Vanguard%
# High Div
# Wells Fargo%
# Western Asset%
# Whiting USA%
# WisdomTree
# Zweig
# 
# select distinct company 
# from si_finecon2 
#   where industry_desc = 'Misc. Financial Services' 
#     and sp in ('500','400','600')
# order by 1;
# -- just 7
# "Apollo Investment Corp."
# "Broadridge Financial Solutions"
# "Cash America International Inc"
# "Cash America International, In"
# "Encore Capital Group, Inc."
# "FactSet Research Systems Inc."
# "Prospect Capital Corporation"

#        
#          
#                                                   

# -- CURRENTLY NOT USED ANYWHWERE(YET) --
# 
#              drop function r_lof(in rn bigint[], in cols float8[][], in k int, in retcount int);
# create or replace function r_lof(in rn bigint[], in cols float8[][], in k int, in retcount int)
#   returns table(rn int, val int) as
# $body$
# 
#   # finds outliers using k neighbors
# 
#   # Slightly re-written by Andre Mikulec: pl/r author removed parallelism
#   # based entirely on 
#   # Rlof
#   # Version:	1.1.1
#   # Published:	2015-09-17
#   # https://cran.r-project.org/web/packages/Rlof/index.html
# 
#   # args
# 
#   # rn       - unique integer id for the row: currently: only tested: row_number(): 1,2,3, . . . , n
#   # cols     - numeric columns of the data.frame
#   #          - can be sent by array[array_agg(col1),array_agg(col2), ... ]
#   # k        - nearest neighbors
#   # retcount - number of ranked rows to return
#   #          - to return 'all' of the data, enter the PostgreSQL 'integer max'  
#   #          - i.e.  retcount := 2147483647
# 
#   # return
#   
#   # table(rn int, val int) -- array of pairs 
#   # rn                     -- final ranking (lower values are more outlier-ish (goal))
#   # val                    -- the original input rn of "r_lof( rn := array_agg(.), ... )"
# 
#   # example
# 
#   # 
#   # select s.rn_mtcars, s.car, st.rn car_outlier_rnk, s.mpg, s.cyl, s.disp
#   # from ( -- s 
#   #   select sq.car, sq.mpg, sq.cyl, sq.disp, row_number() over(order by sq.car) rn_mtcars
#   #   from ( -- sq
#   #     select mt."row.names" car, mt.mpg, mt.cyl, mt.disp from mtcars mt order by car 
#   #   ) sq
#   # ) s
#   # , 
#   # ( --st
#   #   select rn(r_lof_res.out), val(r_lof_res.out)
#   #   from ( -- r_lof_res
#   #     select r_lof( rn := array_agg(rd.rn_mtcars), cols := array[array_agg(rd.mpg), array_agg(rd.cyl), array_agg(rd.disp)], k := 5, retcount := 2147483647) as out
#   #     from ( -- rd
#   #       select sq.car, sq.mpg, sq.cyl, sq.disp, row_number() over(order by sq.car) rn_mtcars
#   #       from ( -- sq
#   #         select mt."row.names" car, mt.mpg, mt.cyl, mt.disp from mtcars mt order by car 
#   #       ) sq
#   #     ) rd
#   #   ) r_lof_res
#   # ) st
#   # where s.rn_mtcars = st.val
#   # order by car_outlier_rnk;
#   # 
#   #  rn_mtcars |         car         | car_outlier_rnk | mpg  | cyl | disp
#   # -----------+---------------------+-----------------+------+-----+-------
#   #         31 | Valiant             |               1 | 18.1 |   6 |   225 -- outlier: only 6 cyl over 168
#   #          2 | Cadillac Fleetwood  |               2 | 10.4 |   8 |   472
#   #         15 | Lincoln Continental |               3 | 10.4 |   8 |   460
#   # ...
#   # see 
#   # data.table::data.table(cbind(rn = row.names(mtcars)[order(row.names(mtcars))], mtcars[order(row.names(mtcars)),])) 
#
#   Rlof___f.dist.to.knn <- function(dataset, neighbors, ...){
# 
#       m.dist <- as.matrix( Rlof:::distmc(dataset, ...))
#       num.col <- dim(m.dist)[2]
#       l.knndist <- lapply(c(1:num.col), function(i) {
#           order.x <- order(m.dist[, i])
#           kdist <- m.dist[, i][order.x[neighbors + 1]]
#           numnei <- sum(m.dist[, i] <= kdist)
#           data.frame(v.order = order.x[2:numnei], v.dist = m.dist[,
#               i][order.x[2:numnei]])
#       })
#       rm(m.dist)
#       maxnum <- max(unlist(lapply(l.knndist, function(x) {
#           dim(x)[1]
#       })))
#       i <- numeric()
#       knndist <- NULL
#       for(i in 1:num.col)
#           {
#               len <- dim(l.knndist[[i]])[1]
#               RES <- c(l.knndist[[i]]$v.order, rep(NA, (maxnum - len)),
#                   l.knndist[[i]]$v.dist, rep(NA, (maxnum - len)))
#               knndist <- cbind(knndist,RES)
#           }
#       knndist
#   }
# 
#   Rlof__lof <- function(data, k, ...){
# 
#       if (is.null(k))
#           stop("k is missing")
#       if (!is.numeric(k))
#           stop("k is not numeric")
#       data <- as.matrix(data)
#       if (!is.numeric(data))
#           stop("the data contains non-numeric data type")
#       v.k <- as.integer(k)
#       if (max(v.k) >= dim(data)[1])
#           stop("the maximum k value has to be less than the length of the data")
#       distdata <- Rlof___f.dist.to.knn(data, max(v.k), ...)
#       p <- dim(distdata)[2L]
#       dist.start <- as.integer((dim(distdata)[1])/2)
#       dist.end <- dim(distdata)[1]
#       ik <- numeric()
#       m.lof <- NULL
#       for(ik in v.k) 
#       {
#           lrddata <- Rlof:::f.reachability(distdata, ik)
#           v.lof <- rep(0, p)
#           for (i in 1:p) {
#               nneigh <- sum(!is.na(distdata[c((dist.start + 1):dist.end),
#                   i]) & (distdata[c((dist.start + 1):dist.end),
#                   i] <= distdata[(dist.start + ik), i]))
#               v.lof[i] <- sum(lrddata[distdata[(1:nneigh), i]]/lrddata[i])/nneigh
#           }
#           m.lof <- cbind(m.lof, v.lof)
#       }
#       if (length(v.k) > 1)
#           colnames(m.lof) <- v.k
#       return(m.lof)
#   }
# 
#   set.seed(1L)
#   res <- Rlof__lof(data = as.data.frame(t(cols)), k = k)
#   if(length(res) > 0L) {
#     res <- order(res, decreasing = TRUE)[seq(1,min(retcount,length(res)),1)]
#   } else {
#     res <- (data.frame(rn = integer(), val = integer()))[FALSE,,drop = FALSE]
#     return(res)
#   }
#   return(data.frame(rn[seq(1,min(retcount,length(res)),1)], res))
# $body$
#   language plr;



# -- AUTOMATIC DETERMINATION OF OUTLIERS ( USES pl/r function: r_lof )
#       
# select
#     sq.dateindex, sq.company_id
#   -- -- required in join ( not necessary to view )
#   -- -- orderer ( not necessary to view ) ( but very useful in portability )
#   , sq_r3.rn  outlier_rank -- lower is more 'outlier-ish'
#   , sq_r3.val rn_id       -- row number position ( from the original (rn := ) of where the outlier occurs     
#   ---- redundant of above
#   --, sq.rn sq_rn
#   -- -- redundant payload ( not necessary at all ) 
#   , sq.sp, sq.ticker, sq.company, sq.price, sq.mktcap, sq.netinc_q1, sq.sales_q1, sq.ncc_q1, sq.assets_q1
# from 
#   ( -- sq, sq.rn(1,2,3,...)
#   select 
#       row_number() over (partition by qu2.dateindex order by qu2.company_id) rn
#     , qu2.dateindex, qu2.company_id
#     -- -- redundant payload
#     , qu2.sp, qu2.ticker, qu2.company, qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1, qu2.ncc_q1, qu2.assets_q1
#   from si_finecon2 qu2                  -- 12055
#   where qu2.sp = '500' and qu2.dateindex = 17409 order by qu2.company_id
#   ) sq, 
#   ( -- sq_r3 -- sq_r3.rn(1,2,3,...) sq_r3.val(137,316,43,...)
#     select (sq_r2.out).* --sq_r2.rn(NEED TO CARRY), sq_r2.val 
#     from
#     ( -- sq_r2                                                                                                                                               -- testing: retcount := 20 xor 'all of the data'
#       select 
#           r_lof( rn := array_agg(sq_r.rn)
#         , cols := 
#             array[
#                 -- array_agg(sq_r.price),    array_agg(sq_r.mktcap), 
#                 array_agg(sq_r.netinc_q1 / nullif(sq_r.sales_q1,0))
#               , array_agg(sq_r.ncc_q1    / nullif(sq_r.sales_q1,0))
#             ]
#         , k := 5, retcount := 2147483647) as out 
#       from 
#       ( -- sq_r
#         select 
#             row_number() over (order by qu.company_id) rn -- random
#           , case when qu.price     is not null then qu.price::float8  else '-Infinity'::float8 end price
#           , case when qu.mktcap    is not null then qu.mktcap::float8 else '-Infinity'::float8 end mktcap
#           , case when qu.netinc_q1 is not null then qu.netinc_q1      else '-Infinity'::float8 end netinc_q1
#           , case when qu.sales_q1  is not null then qu.sales_q1       else '-Infinity'::float8 end sales_q1
#           , case when qu.ncc_q1    is not null then qu.ncc_q1         else '-Infinity'::float8 end ncc_q1
#           -- NOTE: Bear Stearns  assets_q1 == 0.00
#           , case when qu.assets_q1 is not null then qu.assets_q1      else '-Infinity'::float8 end assets_q1
#         from  si_finecon2 qu
#         where qu.sp = '500' and qu.dateindex = 12055 order by qu.company_id 
#       ) sq_r  
#     ) sq_r2
#   ) sq_r3
# where sq.rn = sq_r3.val
# order by sq_r3.rn -- limit 10 -- also works 
# 


# 
# -- MANUAL DETERMINATION OF OUTLIERS ( RESULT COLUMNS MATCH THE 'AUTOMATIC METHOD'(SEE ABOVE) --
# 
# select 
#       sq2_r.dateindex,  sq2_r.company_id
#     , sq2_r.sq_r3_rn   outlier_rank -- lower is more 'outlier-ish' ( ALWAYS ZERO(0) IN THIS SQL )
#     , sq2_r.sq_r3_val  rn_id        -- row number position ( from the original (rn := ) of where the outlier occurs  
#     , sq2_r.sp,    sq2_r.ticker,  sq2_r.company
#     , sq2_r.price, sq2_r.mktcap,  sq2_r.netinc_q1,  sq2_r.sales_q1, sq2_r.ncc_q1, sq2_r.assets_q1
# from ( -- sq2_r
#   select qu2.dateindex, qu2.company_id
#     -- later -- distinct on -- so if lof detects and bads detects then lof is taken as the single record
#     -- 2147483647 -- float to the bottom ( zero(0): float to the top )
#     , 0 sq_r3_rn, row_number() over (partition by qu2.dateindex order by qu2.company_id) sq_r3_val
#     , qu2.sp,    qu2.ticker, qu2.company
#     , qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1, qu2.ncc_q1, qu2.assets_q1
#   from si_finecon2 qu2                  -- 12055 
#   where qu2.sp = '500' and qu2.dateindex = 17409 
# ) sq2_r
# where ( 
#     -- tester
#     -- sq2_r.price > 20.00
#     --
#     -- shuld be never
#        sq2_r.price is null    or sq2_r.mktcap is null or sq2_r.netinc_q1 is null or 
#        sq2_r.sales_q1 is null or sq2_r.ncc_q1 is null
#        -- # (price, mktcap, netinc_q1, sales_q1)
#        -- # qu2.sp = '500' and qu2.dateindex = 12055 # 1748N # sq_r2_rn == 0, sq_r3_rn == 92 # "CenturyTel, Inc." ( netinc_q1 > sales_q1 )
#        or ( sq2_r.netinc_q1 > sq2_r.sales_q1 )
#     )
# order by sq2_r.company_id 
# 


# finecon01.R   

# LATELY
# 
# quantmod::getSymbols("^GSPC", from = "1940-01-01")
# rm(list=setdiff(ls(all.names=TRUE),c("con","cid","GSPC"))); debugSource('W:/R-3.4._/finecon01.R'); debugSource('W:/R-3.4._/goodsight01.R');verify_connection();options(upsert_temp_is_temporary=Inf)
#
# rm(list=setdiff(ls(all.names=TRUE),c("con","cid"))); debugSource('W:/R-3.4._/finecon01.R');debugSource('W:/R-3.4._/goodsight01.R');debugSource('W:/R-3.4._/valuesight01.R');verify_connection();options(upsert_temp_is_temporary=Inf);verify_channel();Quandl::Quandl.api_key(api_key= "API_KEY");setDefaults(getSymbols.av, api.key="API.KEY")

