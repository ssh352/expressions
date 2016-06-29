
# data_loading_with_Excel_4.R

options(width = 10000) # 255    
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width 
options(warn=2)
browseOnce <- function() {   # CLOSURE
  old <- getOption("error")
  function() {
    options(error = old)
    browser()
  }
}
options(error = recover) # NULL, recover, browser # browseOnce()


# BORROWED FROM ( tip from rdocumentation.org )
# https://github.com/cran/qdap/blob/master/R/left_just.R
left_just <-function(dataframe, column = NULL, keep.class = FALSE) {
    df.class <- function(dataframe) {
        sapply(1:ncol(dataframe), function(i) {
            x <- class(dataframe[, i])
            x[length(x)]
        })
    }
    CLASS <- df.class(dataframe)
    left.j <- function(x) {
        n <- max(nchar(x))
        return(sprintf(paste("%-", n, "s", sep = ""), x))
    }
    if (is.null(column)) column <- colnames(dataframe)
    lj <- function(DF2, column) {
        if (is.null(column)) column <- colnames(DF2)
        Q <- max(nchar(c(as.character(DF2[, column]), column)))
        DF2 <- data.frame(rbind(colnames(DF2), do.call(cbind,
            lapply(DF2, as.character))), check.names = FALSE)
        DF2[, column] <- left.j(as.character(DF2[, column]))     
        if (is.character(column)) {
            col <- names(DF2)[which(names(DF2) == column)]
                names(DF2)[which(names(DF2) == column)] <- sprintf(paste("%-", 
                Q, "s", sep = ""), col)
        } else {
            if (is.numeric(column)) {
                col <- names(DF2)[column]
                    names(DF2)[column] <- sprintf(paste("%-", Q, "s", 
                    sep = ""), col)
            }
        }
        DF2 <- data.frame(DF2[-1, , drop = FALSE], check.names = FALSE)
        rownames(DF2) <- NULL
        return(DF2)
    }
    if (length(column) < 2) {
        if (!is.data.frame(dataframe)) {
            y <- as.character(substitute(dataframe))
            dataframe <- data.frame(dataframe, check.names = FALSE)
            y <- if (y[1]%in%c("[", "$")) y[2] else y[1]
            names(dataframe) <- y
        }
        DF3 <- lj(DF2=dataframe, column=column)
    } else { 
        if (!is.numeric(column)) column <- match(column, names(dataframe))
        dat <- dataframe[, -c(column), drop=FALSE]
        ndf <- colnames(dataframe)
        LIST <- lapply(column, function(x) {
            lj(DF2=dataframe[, x, drop=FALSE], column = NULL)
        })
        dat2 <- data.frame(cbind(do.call('cbind', LIST), dat), checknames=FALSE)
        NAMES <- colnames(dat2)
        STrim <- function (x) gsub("^\\s+|\\s+$|\\.+$", "", x)
        newloc <- match(ndf, STrim(NAMES))
        DF3 <- dat2[, newloc]
    }
    if (keep.class) {
        colClasses <- function(d, colClasses) {
            colClasses <- rep(colClasses, len=length(d))
            d[] <- lapply(seq_along(d), function(i) switch(colClasses[i], 
                numeric=as.numeric(d[[i]]), 
                character=as.character(d[[i]]), 
                Date=as.Date(d[[i]], origin='1970-01-01'), 
                POSIXct=as.POSIXct(d[[i]], origin='1970-01-01'), 
                factor=as.factor(d[[i]]),
                methods::as(d[[i]], colClasses[i]) ))
            d
        }
        DF3 <- colClasses(DF3, CLASS)
    }
    colnames(DF3) <- gsub("\\.(?=\\.*$)", " ", colnames(DF3), perl=TRUE)
    return(DF3)
}



ept <- function(text = NULL, envir = parent.frame()) {
  
  require(stringr)
  
  # SHOULD DO: check for (formals,missing) arguments
  eval(expr = parse(text = text), envir = envir)
  
}
# GOOD


# vvswitch <-function(EXPR,...) { 
#   NOTE: an 'INPUT vector' WITH A NULL in it will (undesirably) CAUSE a LIST to be returned
#   sapply(X = EXPR, function(X = X, ...) { switch(EXPR = X, ...) }, ...)   
# }

# vvswitch <-function(EXPR,...) { 
# 
#   # uses avu
# 
#   result <- sapply(X = EXPR, function(X = X, ...) { 
#     switch(EXPR = X, ...) 
#     }
#   , ...) 
#  
#   op <- options()
#   options(warn = 1)
#   
#   if(is.list(result)) {
#     warning("", immediate. = TRUE, call. = TRUE)
#     warning("BEGIN WARNING", immediate. = TRUE, call. = FALSE)
#     warning("VVSWITCH or SAPPLY or SWITCH did not RETURN vector", immediate. = TRUE, call. = FALSE)
#     warning("using AVU() to CORRECT now", immediate. = TRUE, call. = FALSE)
#     warning("END WARNING", immediate. = TRUE, call. = FALSE)
#     better_result <- avu(result)
#   } else { # worked correctly
#     better_result <- result
#   }
#  
#   options(op)
#   
#   return(better_result)
# 
# }


vvswitch <- function(EXPR, ...) {

    result <- EXPR

    for(i in seq(along=result))   # VALUE # or 'NULL', invisibly
        result[i] <- if( is.null({ out <- switch(EXPR[i], ...)}) ) {  NA  } else { out  }

    result
}

# > vvswitch(c(NA,1,2,3), "dog", "cat")
# [1] NA    "dog" "cat" NA

# switchv ( BASED ON )
# https://github.com/cran/broman/blob/master/R/switchv.R

# Feb 29, 2016 predicttrain data:

# Browse[2]> str(alluniverses[4240,],list.len = 999,vec.len = 2)
# 'data.frame':	1 obs. of  256 variables:
#  $ NONE                                       : chr "NONE"
#  $ predictclasses                             : chr "train"
#  $ timends                                    : num 16860
#  $ timends_push_to_eom                        : num 16860
#  $ provider_global_internal_id_timepoint_exact: chr "AAIISIProDBFs__AF67C__16860"
#  $ provider                                   : chr NA # ( WHY IS THIS ERROR HAPPENING IN MY DATA ? ) [ ]
#  $ anonymous_internal_id                      : chr "AF67C"
#  $ provider_global_internal_id                : chr "AAIISIProDBFs__AF67C"
#  $ fin_instru_tk                              : chr "CHLN"
#  $ external_id                                : chr "CHLN"
#  $ fin_company_nm                             : chr "China Housing & Land Developme"
#  $ country_nm                                 : chr "China"
#  $ amer_depo_rcpt                             : num 0
#  $ bus_sector_cd                              : chr "09"
#  $ sector_industry_cd                         : chr "0933"
#  $ industry_cd                                : chr "33"
#  $ fin_exchange_cd                            : chr "M"
# 
#  $ market_cap                                 : num NA   # BUT IS NOT NULL


numORdateTonum <- function(x) {

  # originally in Excel
  # "as.numeric(as.POSIXct(<.>,tz='UTC'))/86400"

  # BUT unlist (in the computer program) : reduces 'collections of Dates" to "atomic components" ( numbers )
  # but I want do be safely consistent

  # input (will handle BOTH a Date and a "numeric") . . . returns a (collection of) Date(s)
  as.Date(x, origin = "1970-01-01", tz = "UTC") -> y
  
  # I want numeric
  as.numeric(as.POSIXct(y, origin = "1970-01-01", tz='UTC'))/86400

}
# "numORdateTonum(<.>)"



# KEEP
# it DOES not overwrite what is ALREADY there
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



# KEEP
# depends upon  copyDirectoryByPattern
copyAAIISIProDBFs <- function(from = "C:/Program Files (x86)/Stock Investor/Professional", to = "./Desination" ) {
  
  subdirs <- c("","/Dbfs","/User","/Static","/Temp","/Datadict")
  
  for(subdir in subdirs) {
    
    # it DOES not overwrite what is ALREADY there
    copyDirectoryByPattern(from = paste0(from, subdir)
                           , pattern = "(*\\.dbf$|\\.*DBF$|\\.*DBF$|*.chm$|ReadMe\\.txt)", to=to,  tolower = TRUE
    )
    
  }
  
}


massAAIISIProDBFsDB <- function(conn, from_target = "W:/New_Economics/forsight4.322/AAIISIProDBFs", 
                                load_only_last_weekday_of_month = TRUE,
                                only_dirs_like = "where 1 = 1 ", only_files_like = "where 1 = 1 ") {
  
  # often uses  is.lastWeekDayDateOfMonth
  # uses package foreign and caroline and sqldf

  require(RPostgreSQL) # needed by caroline:: AND ( sqldf::sqldf -> require(tcltk) ) 
  # load require(DBI)  # needed by db*

  # original search path
  osp <- dbGetQuery(conn,"show search_path")[[1]]

  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage"))
   
  orig_all_dirs <- list.files(from_target)

  env <- list2env(list(tinfo=data.frame(cbind(cinfo=orig_all_dirs), stringsAsFactors = FALSE)))
  new_all_dirs <- sqldf::sqldf(paste0("select cinfo from tinfo ", only_dirs_like, "order by cinfo"), envir = env, drv = "SQLite")[[1]]

  for(that_dir in new_all_dirs) {

    if( (load_only_last_weekday_of_month == TRUE)  && !is.lastWeekDayDateOfMonth(zoo::as.Date(as.numeric(that_dir))) ) { next } else { NULL }
      
    path_name <-paste0(from_target,"/", that_dir)

    print(paste0("SOURCE: ", path_name))

    orig_all_files <- list.files(path_name, pattern = "\\.dbf$")

    env <- list2env(list(tinfo=data.frame(cbind(cinfo=orig_all_files), stringsAsFactors = FALSE)))
    new_all_files <- sqldf::sqldf(paste0("select cinfo from tinfo ", only_files_like, "order by cinfo"), envir = env, drv = "SQLite")[[1]]

    for(that_file in new_all_files) {

      path_file_name <- paste0(path_name, "/", that_file)

      # upload to R
      data_frame_loaded <- suppressWarnings(suppressMessages(foreign::read.dbf(file = path_file_name, as.is = TRUE)))

      # remove bad columns ( keep good columns )
      data_frame_loaded <- data_frame_loaded[,!grepl("(^X\\.??$|^X\\.\\d+?$|^X_NullFlags$)", colnames(data_frame_loaded)), drop = FALSE]
      
      # lowercase column names
      colnames(data_frame_loaded) <- tolower(colnames(data_frame_loaded))

      # real database table name
      dbf_file_stem <- sub("\\.dbf$", "", that_file)

      # if not exists, create 'inherited form'

      if(!dbExistsTable(conn,paste0(dbf_file_stem))) { 
        dbGetQuery(conn, paste0("create table ", dbf_file_stem, "( )"))} else { NULL } 

      dbf_file_stem_plus_dir <- paste0(dbf_file_stem,"_", that_dir)
      
      if(dbExistsTable(conn,paste0(dbf_file_stem_plus_dir))) { 
        dbGetQuery(conn, paste0("drop table ",dbf_file_stem_plus_dir))} else { NULL } 
      
      # fill.null = TRUE: Should new db present fields be added to the data.frame before it is loaded?.
      # upload to the database
      # caroline::dbWriteTable2(conn, dbf_file_stem,  df = data_frame_loaded, , add.id= FALSE)

      dbWriteTable(conn, dbf_file_stem_plus_dir, cbind(dateindex = rep(as.integer(that_dir),NROW(data_frame_loaded)),data_frame_loaded))
      dbGetQuery(conn, paste0("alter table " , dbf_file_stem_plus_dir, " inherit " , dbf_file_stem))
      dbGetQuery(conn, paste0("create index ", dbf_file_stem_plus_dir, "_dateindex_idx on " , dbf_file_stem_plus_dir, "( dateindex )"))
    
      if("company_id" %in% dbListFields(conn, dbf_file_stem_plus_dir)) {
        dbGetQuery(conn, paste0("create index ", dbf_file_stem_plus_dir, "_company_id_idx on " , dbf_file_stem_plus_dir, "( company_id )"))
      }
    }

  }

  # put back the search 
  dbGetQuery(conn, paste0("set search_path to ", osp))

  return(invisible())
  
}

require(RPostgreSQL)
drv  <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, user="postgres", password="postgres", port = 5432, dbname="finance_econ") # RPostgreSQL # 
# dbGetQuery(conn,"select * from sipro_stage.atable")
# dbDisconnect(conn)
# dbUnloadDriver(drv)

# fill the sipro_stage schema
# massAAIISIProDBFsDB(conn)
#
# just a particular month
# massAAIISIProDBFsDB(conn, only_dirs_like = "where cinfo = 16952 ")
#
# from other tools, get the output(way to recreate) the sipro_data_store schema
# 
# C:\Users\AnonymousUser\Documents\UnxUtils\usr\local\wbin>tail -f --lines=40 "C:\Users\AnonymousUser\Documents\pgadmin.log"
#
# set PATH=C:\Users\AnonymousUser\Documents\BINARIES\graphviz-2.38\release\bin;%PATH%
# W:\New_Economics\forsight4.322>java -jar schemaSpy_5.0.0.jar -dp "C:\Program Files (x86)\PostgreSQL\pgJDBC\postgresql-9.4.1208.jar" -t pgsql -host localhost -db finance_econ -s sipro_data_store -u postgres -p postgres -o "C:\Users\AnonymousUser\Desktop\R-Portable.3.2.2\App\R-Portable\bin\x64\RDebug\Home\schemaSpy_meta"
#
# C:\Program Files\PostgreSQL\9.5>"%PGSQL%\bin\pg_dump" --host=localhost --port=5432 --username=postgres --no-password --dbname=finance_econ  --schema=sipro_data_store  --schema-only               --file="C:\Users\AnonymousUser\Desktop\R-Portable.3.2.2\App\R-Portable\bin\x64\RDebug\Home\pg_dump_out_YYMMDDHHMM.txt"
#
# lately
# C:\Program Files\PostgreSQL\9.5>"%PGSQL%\bin\pg_dump" --host=localhost --port=5432 --username=postgres --no-password --dbname=finance_econ  --schema=sipro_data_store                               --file="C:\Users\AnonymousUser\Desktop\R-Portable.3.2.2\App\R-Portable\bin\x64\RDebug\Home\pg_dump_out_YYMMDDHHMM.txt"
# 
# most late
# C:\Program Files\PostgreSQL\9.5>"%PGSQL%\bin\pg_dump" --host=localhost --port=5432 --username=postgres --no-password --dbname=finance_econ  --schema=sipro_data_store                   --inserts --file="C:\Users\AnonymousUser\Desktop\R-Portable.3.2.2\App\R-Portable\bin\x64\RDebug\Home\pg_dump_out_YYMMDDHHMM.txt"
#
# the script contains COPY
# finance_econ=# \i C:/Users/AnonymousUser/Desktop/R-Portable.3.2.2/App/R-Portable/bin/x64/RDebug/Home/pg_dump_out_YYMMDDHHMM.txt
# 
# BUT BETTER ( if the script contains INSERT ) ... use pgAdminIII Query window
#


# because PostgreSQL dbListTables reads all tables in all schemas and does not know about 'search_path'
#  NOTE: dbExistsTable looks in the current schema only "SELECT current_schema()"
dbListTablesOneSearchPath <- function(conn, ...) {

  out <- dbGetQuery(conn, paste0("select tablename from pg_tables where schemaname !='information_schema'", 
    "and schemaname !='pg_catalog'", 
    " and schemaname =","'",gsub(" ","",dbGetQuery(conn,"show search_path")[[1]], fixed = TRUE),"'", ...))
    
  if (is.null(out) || nrow(out) == 0) 
    out <- character(0) 
  else 
    out <- out[,1]
  
  out
  
} 
# based off of 
# https://github.com/cran/RPostgreSQL/blob/master/R/PostgreSQL.R



# # prefers schema.table ??
# # MUST be RAN as a pgScript ( in pgAdminIII )
# 
# # prefers schema.table ??
# # MUST be RAN as a pgScript ( in pgAdminIII )

# ost <- dbGetQuery(conn,"show time zone")[[1]]
# osp <- dbGetQuery(conn,"show search_path")[[1]]
# # update search path
# dbGetQuery(conn, paste0("set search_path to sipro_stage"))
# # update time zone
# dbGetQuery(conn, "set time zone 'utc'")
# sapply(sort(dbListTablesOneSearchPath(conn)), function(x,cn) { 
#   if(x != "atable" ) {
#     # browser()
#     stmt <- paste0("DO $$ DECLARE BEGIN drop table sipro_stage.", x, " cascade; END; $$")
#     cat(noquote(stmt),"\n")
#     try( { dbGetQuery(cn, stmt) }, silent = TRUE )
#   } else { 
#     NULL 
#   } 
#   return(invisible())
# }, cn = conn ) -> X; invisible(); rm(X) 
# # update search path
# dbGetQuery(conn, paste0("set search_path to ", osp))
# # update time zone
# dbGetQuery(conn, paste0("set time zone '",ost,"'"))
  
  
# WAS not worth the TIME: I forgot that I already indexed by company_id
massAAIIinstallOtherCommonColumnsIndexes <- function(conn, 
  tabl_regex_expr    = "_\\d+$", #  specific month  _######$
  interested_columns = c("adr","country","exchange", "ind_2_dig", "ind_3_dig","mg_code", "mktcap") 
  ) { 

  # make sure autovacuum is turned on(default) ***IMPORTANT **
  # select * from pg_settings where name like 'autovacuum%';

  ost <- dbGetQuery(conn,"show time zone")[[1]]
  osp <- dbGetQuery(conn,"show search_path")[[1]]
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage") )
  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")

  tables <- dbListTablesOneSearchPath(conn)
  
   #  specific month  _######$
  interested_tables <- sort(tables[grepl(tabl_regex_expr,tables)])
  interested_tables_length <- length(interested_tables)
  interested_tables_length_index <- 0
  for (interested_table in interested_tables) {
    interested_tables_length_index <- interested_tables_length_index + 1
    cat(paste0("Begin interested_table: ", interested_table, 
             " number ", interested_tables_length_index,
             " of ", interested_tables_length,
             "\n"))
    
    for( interested_column in interested_columns) {
       cat(paste0("  Begin interested_column: ", interested_column, " of ", interested_table,"\n"))

      if(interested_column %in% dbListFields(conn, interested_table)) {
          col_exists_ind <- 1 
      } else {
          col_exists_ind <- 0 
      }
      
      # column exists indicator 
      if(any(interested_column == c("adr","country","exchange", "ind_2_dig", "ind_3_dig","mg_code", "mktcap")) && col_exists_ind == 1) {
      
        stmt_col_index_create <-  gsub("your_table", interested_table, gsub("your_column", interested_column,
                                  "create index if not exists your_table_your_column_idx on your_table(your_column)"))
        cat(noquote(stmt_col_index_create),"\n")
        try( { dbGetQuery(conn, stmt_col_index_create) }, silent = TRUE )
    
      }
      
      # little extra
      if(interested_column == "mg_code" && col_exists_ind == 1) {
        
        stmt_col_substr_index_create <- gsub("your_table", interested_table, gsub("your_column", interested_column, 
                                        "create index if not exists your_table__your_column_substr_1_2_idx on your_table(substring(your_column,1,2))"))
        cat(noquote(stmt_col_substr_index_create),"\n")  
        try( { dbGetQuery(conn, stmt_col_substr_index_create) }, silent = TRUE )

      }
      
    cat(paste0("  End interested_column: ", interested_column,"\n"))
    }
    

    if(grepl("^si_ci_\\d+$",interested_table) && 
       any("si_ci_AND_company_id_unq_AND_ticker_unq" == interested_columns) && 
      !any("company_id_unq" == dbListFields(conn, interested_table)) &&
      !any("ticker_unq" == dbListFields(conn, interested_table)) 
    ) {

      cat(paste0("  Begin interested_column: ", "si_ci_AND_company_id_unq_AND_ticker_unq", " of ", interested_table,"\n"))

      stmt_action <-  gsub("your_table", interested_table, "

        alter table your_table add company_id_unq text; 

        update your_table set company_id_unq = company_id 
        where company_id in ( select company_id from your_table group by company_id having count(company_id) = 1); 

        create index your_table_company_id_unq_idx on your_table(company_id_unq); 

        alter table your_table add ticker_unq text;

        update your_table set ticker_unq = ticker 
        where company_id_unq is not null; 

        create index your_table_ticker_unq_idx on your_table(ticker_unq);  

      ")

      cat(noquote(stmt_action),"\n") 
      try( { dbGetQuery(conn, stmt_action) }, silent = TRUE ) 

      cat(paste0("  End interested_column: ", "si_ci_AND_company_id_unq_AND_ticker_unq","\n")) 
      
    }
    
    if(!grepl("^si_ci_\\d+$",interested_table) && grepl("\\d+$",interested_table) &&
      any("not_si_ci_AND_company_id_unq_AND_ticker_unq" == interested_columns) && 
       any("company_id"     == dbListFields(conn, interested_table)) &&
      !any("company_id_unq" == dbListFields(conn, interested_table)) &&
      !any("ticker_unq" == dbListFields(conn, interested_table)) 
      # && interested_table == "si_cfq_16952"
    ) {

      cat(paste0("  Begin interested_column: ", "not_si_ci_AND_company_id_unq_AND_ticker_unq", " of ", interested_table,"\n"))

      my_epoch <- gsub("^[a-z_]+_","",interested_table)

      stmt_action <-  gsub("your_table", interested_table, " 

        alter table your_table add company_id_unq text; 

        update your_table set company_id_unq = company_id 
        where company_id in ( select company_id from your_table group by company_id having count(company_id) = 1) and company_id in (select company_id_unq from si_ci_my_epoch); 

        create index your_table_company_id_unq_idx on your_table(company_id_unq); 

        alter table your_table add ticker_unq text; 

        update your_table trg set ticker_unq = ci.ticker_unq 
        from si_ci_my_epoch ci where 
        trg.company_id_unq is not null and  -- too extra safe 
        trg.company_id_unq = ci.company_id_unq; 

        create index your_table_ticker_unq_idx on your_table(ticker_unq); 

      ")
      
      stmt_action <-  gsub("my_epoch", my_epoch, stmt_action)
      
      cat(noquote(stmt_action),"\n") 
      try( { dbGetQuery(conn, stmt_action) }, silent = TRUE ) 

      cat(paste0("  End interested_column: ", "not_si_ci_AND_company_id_unq_AND_ticker_unq","\n")) 
      
    }
    
    if(grepl("\\d+$",interested_table) &&
      any("dateindexeom" == interested_columns) && 
       any("dateindex"    == dbListFields(conn, interested_table)) &&
      !any("dateindexeom" == dbListFields(conn, interested_table)) 
      # && interested_table == "si_cfq_16952"
    ) {

      cat(paste0("  Begin interested_column: ", "dateindexeom", " of ", interested_table,"\n")) 

      stmt_action <-  gsub("your_table", interested_table, " 

        alter table your_table add dateindexeom integer;

        update your_table tbl set dateindexeom = ( select extract( 
          'epoch' from ( select date_trunc('month', to_timestamp(tbl.dateindex*3600*24)::date) + 
          interval '1 month' - interval '1 day' )
        ) / ( 3600*24 ) );

        create index your_table_dateindexeom_idx on your_table(dateindexeom);

      ")
      

      cat(noquote(stmt_action),"\n") 
      try( { dbGetQuery(conn, stmt_action) }, silent = TRUE ) 

      cat(paste0("  End interested_column: ", "dateindexeom","\n")) 
      
    }

  cat(paste0("End interested_table: ", interested_table,"\n"))
  }

  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))
  
  return(invisible())

}

# con <- file(paste0("OUTPUT_massAAIIinstallCommonColumnsIndexes", ".txt"));sink(con);sink(con, type="message")
# 
# massAAIIinstallOtherCommonColumnsIndexes(conn)
# massAAIIinstallOtherCommonColumnsIndexes(conn, interested_columns = "si_ci_AND_company_id_unq_AND_ticker_unq")
# massAAIIinstallOtherCommonColumnsIndexes(conn, interested_columns = "not_si_ci_AND_company_id_unq_AND_ticker_unq")
# massAAIIinstallOtherCommonColumnsIndexes(conn,interested_columns = "dateindexeom")
# 

# sink();sink(type="message");close(con)
# 
# just one month
# massAAIIinstallOtherCommonColumnsIndexes(conn, tabl_regex_expr = "16952")
#
# massAAIIinstallOtherCommonColumnsIndexes(conn, tabl_regex_expr = "16952", interested_columns = "si_ci_AND_company_id_unq_AND_ticker_unq")
#
# massAAIIinstallOtherCommonColumnsIndexes(conn, tabl_regex_expr = "16952", interested_columns = "not_si_ci_AND_company_id_unq_AND_ticker_unq")
#
# massAAIIinstallOtherCommonColumnsIndexes(conn, tabl_regex_expr = "16952", interested_columns = "dateindexeom")
#


# NOTE: IF ZERO records exist, then this error, ( IGNORABLE for right now )
# alter table table_99999 add constraint table_99999_column_chk check (column = ) 
massAAIIinstallPartitionCheckConstraints <- function(conn, 
  tabl_regex_expr             = "_\\d+$", #  specific month  _######$
  checked_col                 = "dateindex",
  checked_val_pre             = "", # SQL  pre-surround
  checked_val_post            = ""  # SQL post-surround
  ) { 
  
  # SQL QUERIES
  # set constraint_exclusion = partition; -- default
  # set constraint_exclusion = on;        -- all tables (MAYBE USEFULE IN 'UNION's)
  
  # make sure autovacuum is turned on(default) ***IMPORTANT **
  # select * from pg_settings where name like 'autovacuum%';

  ost <- dbGetQuery(conn,"show time zone")[[1]]
  osp <- dbGetQuery(conn,"show search_path")[[1]]
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage") )
  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")

  tables <- dbListTablesOneSearchPath(conn)
  
   #  specific month  _######$
  interested_tables <- sort(tables[grepl(tabl_regex_expr,tables)])
  interested_tables_length <- length(interested_tables)
  interested_tables_length_index <- 0
  for (interested_table in interested_tables) {
    interested_tables_length_index <- interested_tables_length_index + 1
    cat(paste0("Begin interested_table: ", interested_table, 
             " number ", interested_tables_length_index,
             " of ", interested_tables_length,
             "\n"))
  
    interested_column <- checked_col
  
    cat(paste0("  Begin interested_column: ", interested_column, " of ", interested_table,"\n"))
  
    if(interested_column %in% dbListFields(conn, interested_table)) {
    
      # NOT USED
      # my_epoch <- gsub("^[a-z_]+_","",interested_table)
      # gsub("my_epoch", my_epoch, )
      
      stmt_col_index_create <-  gsub("your_table", interested_table, gsub("your_column", interested_column,
        "select your_column from your_table limit 1"))
      cat(noquote(stmt_col_index_create),"\n")
      my_result <- try( { dbGetQuery(conn, stmt_col_index_create) }, silent = TRUE )
      if(class(my_result) == "try-error") {    
        cat(paste0("  ERROR FAILURE: ", interested_column, " of ", interested_table,"\n"))
        next 
      } else {
        my_result <- as.vector(unlist(my_result))
      }
        
      
      your_value <- paste0(checked_val_pre, my_result, checked_val_post)
    
      # alter table t_1980 add constraint t_1980_year_chk check (year = 1980) not valid;
      
      stmt_col_index_create <-  gsub("your_value", your_value, gsub("your_table", interested_table, gsub("your_column", interested_column,
       "alter table your_table add constraint your_table_your_column_chk check (your_column = your_value)"))) #  not valid ( almost zero resources to validate )
      cat(noquote(stmt_col_index_create),"\n")
       try( { dbGetQuery(conn, stmt_col_index_create) }, silent = TRUE )

    }
    cat(paste0("  End interested_column: ", interested_column,"\n"))
  
    cat(paste0("End interested_table: ", interested_table,"\n"))
  }

  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))
  
  return(invisible())
   
}

# NOTE: IF ZERO records exist, then this error, ( IGNORABLE for right now )
# alter table table_99999 add constraint table_99999_column_chk check (column = ) 

# con <- file(paste0("OUTPUT_massAAIIinstallPartitionCheckConstraints", ".txt"));sink(con);sink(con, type="message")
#
# # dateindex
# massAAIIinstallPartitionCheckConstraints(conn)
#
# # dateindexeom ( AFTER massAAIIinstallOtherCommonColumnsIndexes, THEN run)
# massAAIIinstallPartitionCheckConstraints(conn, checked_col = "dateindexeom")
#
# sink();sink(type="message");close(con)
# 
# just one month 16952
#  
# massAAIIinstallPartitionCheckConstraints(conn, tabl_regex_expr = "16952")
# # dateindexeom ( AFTER massAAIIinstallOtherCommonColumnsIndexes, THEN run)
# massAAIIinstallPartitionCheckConstraints(conn, tabl_regex_expr = "16952", checked_col = "dateindexeom")
#
#



# WORKS
createAAIIDataStoreSIProRetDateTable <- function(conn, new_month_inserted = " 1 = 1 " ) {

  ost  <- dbGetQuery(conn,"show time zone")[[1]]
  osp  <- dbGetQuery(conn,"show search_path")[[1]]
  oswm <- dbGetQuery(conn,"show work_mem")[[1]]
  
  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '1200MB'"))
  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage") ) # BLINDLY CREATE TO SCHEMA sipro_data_store
  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")

  dbGetQuery(conn, paste0("
  
  -- BEGIN VERY KEEP --
  -- BEGIN VERY KEEP --

  ", if(new_month_inserted == " 1 = 1 ") { " create table sipro_data_store.si_retdate as " } else { " insert into sipro_data_store.si_retdate "  },
  " select 
  dateindex,
  dateindexf12meom,
  case 
    when extract(dow from mldom.f12mldom) = 0  then (extract('epoch' from mldom.f12mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f12mldom) = 6  then (extract('epoch' from mldom.f12mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f12mldom) /(3600*24))::int end dateindexf12mlwd,
  dateindexf09meom,
  case 
    when extract(dow from mldom.f09mldom) = 0  then (extract('epoch' from mldom.f09mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f09mldom) = 6  then (extract('epoch' from mldom.f09mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f09mldom) /(3600*24))::int end dateindexf09mlwd,
  dateindexf06meom,
  case 
    when extract(dow from mldom.f06mldom) = 0  then (extract('epoch' from mldom.f06mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f06mldom) = 6  then (extract('epoch' from mldom.f06mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f06mldom) /(3600*24))::int end dateindexf06mlwd,
  dateindexf03meom,
  case 
    when extract(dow from mldom.f03mldom) = 0  then (extract('epoch' from mldom.f03mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f03mldom) = 6  then (extract('epoch' from mldom.f03mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f03mldom) /(3600*24))::int end dateindexf03mlwd
  from (
  select 
    dateindex,
                          f1m_and_b1d.f1m_and_b1d + interval '12 month'                             f12mldom, -- future12m last day of month
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '12 month')  /(3600*24))::int dateindexf12meom,
                          f1m_and_b1d.f1m_and_b1d + interval '09 month'                             f09mldom,
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '09 month')  /(3600*24))::int dateindexf09meom,
                          f1m_and_b1d.f1m_and_b1d + interval '06 month'                             f06mldom,
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '06 month')  /(3600*24))::int dateindexf06meom,
                          f1m_and_b1d.f1m_and_b1d + interval '03 month'                             f03mldom,
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '03 month')  /(3600*24))::int dateindexf03meom
  from (
  -- forward 1 month and back 1 day
  select date_trunc('month', to_timestamp(dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  f1m_and_b1d, 
         dateindex 
  from ( select distinct on (dateindex) dateindex from si_ci where ", new_month_inserted, ") f1m_and_b1d -- PostgreSQL proprietary - returns just one of each
  ) f1m_and_b1d -- where dateindex = 16952 -- WORKS - just load the (current) month 
  ) mldom; 
  -- WORKS - INSTANTANEIOUS

  ---- set search_path to sipro_stage,sipro_data_store;
  ---- set time zone 'utc';

  ---- select * from sipro_data_store.si_retdate;

  -- END VERY KEEP --
  -- END VERY KEEP --
  
  "))
  
  dbSendQuery(conn, paste0("vacuum analyze sipro_data_store.si_retdate"))
  
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))
  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '",oswm,"'"))
  
  return(invisible())

} 

# JUST ONCE - BIG INITIALIZATION
# createAAIIDataStoreSIProRetDateTable(conn)

# ADD A NEW RECORD EVERY TIME
# createAAIIDataStoreSIProRetDateTable(conn, new_month_inserted = " dateindex = 16952 ")
# 



# SHOULD WORK
createAAIIDataStoreSIProSomeTables <- function(conn) {

  ost  <- dbGetQuery(conn,"show time zone")[[1]]
  osp  <- dbGetQuery(conn,"show search_path")[[1]]
  oswm <- dbGetQuery(conn,"show work_mem")[[1]]
  
  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '1200MB'"))
  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_data_store,sipro_stage") )
  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")

  dbGetQuery(conn, paste0("
  
  create table sipro_data_store.si_ci as select * from sipro_stage.si_ci;
  -- 90 seconds


  -- index: sipro_data_store.si_ci_adr_idx

  -- drop index sipro_data_store.si_ci_adr_idx;

  create index si_ci_adr_idx
    on sipro_data_store.si_ci
    using btree
    (adr);

  -- index: sipro_data_store.si_ci_company_id_idx

  -- drop index sipro_data_store.si_ci_company_id_idx;

  create index si_ci_company_id_idx
    on sipro_data_store.si_ci
    using btree
    (company_id collate pg_catalog.default);

  -- index: sipro_data_store.si_ci_company_id_unq_idx

  -- drop index sipro_data_store.si_ci_company_id_unq_idx;

  create index si_ci_company_id_unq_idx
    on sipro_data_store.si_ci
    using btree
    (company_id_unq collate pg_catalog.default);

  -- index: sipro_data_store.si_ci_country_idx

  -- drop index sipro_data_store.si_ci_country_idx;

  create index si_ci_country_idx
    on sipro_data_store.si_ci
    using btree
    (country collate pg_catalog.default);

  -- index: sipro_data_store.si_ci_dateindex_idx

  -- drop index sipro_data_store.si_ci_dateindex_idx;

  create index si_ci_dateindex_idx
    on sipro_data_store.si_ci
    using btree
    (dateindex);

  -- index: sipro_data_store.si_ci_dateindexeom_idx

  -- drop index sipro_data_store.si_ci_dateindexeom_idx;

  create index si_ci_dateindexeom_idx
    on sipro_data_store.si_ci
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_ci_exchange_idx

  -- drop index sipro_data_store.si_ci_exchange_idx;

  create index si_ci_exchange_idx
    on sipro_data_store.si_ci
    using btree
    (exchange collate pg_catalog.default);

  -- index: sipro_data_store.si_ci_ind_2_dig_idx

  -- drop index sipro_data_store.si_ci_ind_2_dig_idx;

  create index si_ci_ind_2_dig_idx
    on sipro_data_store.si_ci
    using btree
    (ind_2_dig collate pg_catalog.default);

  -- index: sipro_data_store.si_ci_ind_3_dig_idx

  -- drop index sipro_data_store.si_ci_ind_3_dig_idx;

  create index si_ci_ind_3_dig_idx
    on sipro_data_store.si_ci
    using btree
    (ind_3_dig collate pg_catalog.default);

  -- index: sipro_data_store.si_ci_ticker_unq_idx

  -- drop index sipro_data_store.si_ci_ticker_unq_idx;

  create index si_ci_ticker_unq_idx
    on sipro_data_store.si_ci
    using btree
    (ticker_unq collate pg_catalog.default);

  -- 52.3 seconds
  -- ____

  ----------------
  ----------------

  create table sipro_data_store.si_psd as select * from sipro_stage.si_psd;
  -- 88 seconds


  -- index: sipro_data_store.si_psd_company_id_idx

  -- drop index sipro_data_store.si_psd_company_id_idx;

  create index si_psd_company_id_idx
    on sipro_data_store.si_psd
    using btree
    (company_id collate pg_catalog.default);

  -- index: sipro_data_store.si_psd_company_id_unq_idx

  -- drop index sipro_data_store.si_psd_company_id_unq_idx;

  create index si_psd_company_id_unq_idx
    on sipro_data_store.si_psd
    using btree
    (company_id_unq collate pg_catalog.default);

  -- index: sipro_data_store.si_psd_dateindex_idx

  -- drop index sipro_data_store.si_psd_dateindex_idx;

  create index si_psd_dateindex_idx
    on sipro_data_store.si_psd
    using btree
    (dateindex);

  -- index: sipro_data_store.si_psd_dateindexeom_idx

  -- drop index sipro_data_store.si_psd_dateindexeom_idx;

  create index si_psd_dateindexeom_idx
    on sipro_data_store.si_psd
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_psd_mktcap_idx

  -- drop index sipro_data_store.si_psd_mktcap_idx;

  create index si_psd_mktcap_idx
    on sipro_data_store.si_psd
    using btree
    (mktcap collate pg_catalog.default);

  -- index: sipro_data_store.si_psd_ticker_unq_idx

  -- drop index sipro_data_store.si_psd_ticker_unq_idx;

  create index si_psd_ticker_unq_idx
    on sipro_data_store.si_psd
    using btree
    (ticker_unq collate pg_catalog.default);

  -- 37 seconds

  ----------------------
  ----------------------

  create table sipro_data_store.si_isq as select * from sipro_stage.si_isq;
  -- 145 seconds


  -- index: sipro_data_store.si_isq_company_id_idx

  -- drop index sipro_data_store.si_isq_company_id_idx;

  create index si_isq_company_id_idx
    on sipro_data_store.si_isq
    using btree
    (company_id collate pg_catalog.default);

  -- index: sipro_data_store.si_isq_company_id_unq_idx

  -- drop index sipro_data_store.si_isq_company_id_unq_idx;

  create index si_isq_company_id_unq_idx
    on sipro_data_store.si_isq
    using btree
    (company_id_unq collate pg_catalog.default);

  -- index: sipro_data_store.si_isq_dateindex_idx

  -- drop index sipro_data_store.si_isq_dateindex_idx;

  create index si_isq_dateindex_idx
    on sipro_data_store.si_isq
    using btree
    (dateindex);

  -- index: sipro_data_store.si_isq_dateindexeom_idx

  -- drop index sipro_data_store.si_isq_dateindexeom_idx;

  create index si_isq_dateindexeom_idx
    on sipro_data_store.si_isq
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_isq_ticker_unq_idx

  -- drop index sipro_data_store.si_isq_ticker_unq_idx;

  create index si_isq_ticker_unq_idx
    on sipro_data_store.si_isq
    using btree
    (ticker_unq collate pg_catalog.default);

  -- _38_ seconds


  ----------
  ----------


  ------------------------------------------
  ---- BEGIN company_id_unq patch  ---------
  ----

  alter table sipro_data_store.si_ci add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_ci set company_id_unq_orig = company_id_unq;
  -- 5 minutes

  update sipro_data_store.si_ci ci 
  set company_id_unq = ci_f.company_id_unq
  from sipro_data_store.si_ci ci_f
  where ci.ticker_unq = ci_f.ticker_unq
  and   ci.dateindex   in (
                           14911,
                           14943,
                           14974,
                           15005,
                           15033,
                           15064,
                           15093,
                           15125,
                           15155
                          )
  and ci_f.dateindex = 15184;
  -- 21 seconds


  ---- GOOD one to COPY psd DOES NOT SHOW UP IN WORDS

  alter table sipro_data_store.si_psd add column company_id_unq_orig text;

  update sipro_data_store.si_psd set company_id_unq_orig = company_id_unq;

  update sipro_data_store.si_psd psd 
  set company_id_unq = psd_f.company_id_unq
  from sipro_data_store.si_psd psd_f
  where psd.ticker_unq = psd_f.ticker_unq
  and   psd.dateindex   in (
                           14911,
                           14943,
                           14974,
                           15005,
                           15033,
                           15064,
                           15093,
                           15125,
                           15155
                          )
  and psd_f.dateindex = 15184;

  ----

  alter table sipro_data_store.si_isq add column company_id_unq_orig text;

  update sipro_data_store.si_isq set company_id_unq_orig = company_id_unq;

  update sipro_data_store.si_isq isq 
  set company_id_unq = isq_f.company_id_unq
  from sipro_data_store.si_isq isq_f
  where isq.ticker_unq = isq_f.ticker_unq
  and   isq.dateindex   in (
                           14911,
                           14943,
                           14974,
                           15005,
                           15033,
                           15064,
                           15093,
                           15125,
                           15155
                          )
  and isq_f.dateindex = 15184;

  ----
  ----  END company_id_unq patch  ---------
  -----------------------------------------
  
  "))
  
  dbSendQuery(conn, "vacuum analyze sipro_data_store.si_ci(company_id_unq)")

  dbSendQuery(conn, "vacuum analyze sipro_data_store.si_psd(company_id_unq)")

  dbSendQuery(conn, "vacuum analyze sipro_data_store.si_isq(company_id_unq)")
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))
  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '",oswm,"'"))
  
  return(invisible())

} 

# INIT RAN ONCE
# createAAIIDataStoreSIProSomeTables(conn)
#
# PER EACH NEW abc TABLE
#   create table sipro_data_store.si_abc as select * from sipro_stage.si_abc;
#     
#   ... COULD BE MANY INDEXES ...
#   create index si_isq_column_idx
#   on sipro_data_store.si_isq
#   using btree
#   (column);
#
#
# alter table sipro_data_store.si_abc add column company_id_unq_orig text;
#
# update sipro_data_store.si_abc set company_id_unq_orig = company_id_unq;
#
# update sipro_data_store.si_abc abc 
# set company_id_unq = abc_f.company_id_unq
# from sipro_data_store.si_abc abc_f
# where abc.ticker_unq = abc_f.ticker_unq
# and   abc.dateindex   in (
#                          14911,
#                          14943,
#                          14974,
#                          15005,
#                          15033,
#                          15064,
#                          15093,
#                          15125,
#                          15155
#                         )
# and abc_f.dateindex = 15184;
#
# 
# vacuum analyze sipro_data_store.si_abc(company_id_unq);
# 

# SHOULD WORK 
createAAIIDataStoreSIProSomeTablesNewMonthInserted <- function(conn, new_month_inserted = " 1 = 0 " ) {

  ost  <- dbGetQuery(conn,"show time zone")[[1]]
  osp  <- dbGetQuery(conn,"show search_path")[[1]]
  oswm <- dbGetQuery(conn,"show work_mem")[[1]]
  
  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '1200MB'"))

  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage") ) # BLINDLY CREATE TO SCHEMA sipro_data_store

  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")

  dbGetQuery(conn, 
  paste0("

  insert into  sipro_data_store.si_ci as select * from sipro_stage.si_ci   where ", new_month_inserted, "; 
 
  insert into  sipro_data_store.si_psd as select * from sipro_stage.si_psd where ", new_month_inserted, "; 

  insert into  sipro_data_store.si_isq as select * from sipro_stage.si_isq where ", new_month_inserted, "; 

   "))

  dbSendQuery(conn, "vacuum analyze sipro_data_store.si_ci")

  dbSendQuery(conn, "vacuum analyze sipro_data_store.si_psd")

  dbSendQuery(conn, "vacuum analyze sipro_data_store.si_isq")

  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))

  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))

  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '",oswm,"'"))
  
  return(invisible())

} 
# SHOULD WORK -- EACH NEW MONTH -
# createAAIIDataStoreSIProSomeTablesNewMonthInserted(conn, new_month_inserted = " dateindex = 16952 ")





# CURRENTLY
# MUST BE REGENERATED *ANEW* EACH MONTH ( so I get the just past date fractional data)
#
# MAYBE FUTURE?
# OR MUST RUN MANY SMALLER 'insert into table sipro_data_store.si_returns' 
# OVER the last years times to capture
# the partial year returns ( e.g. 13w, 26w)
#
# MAYBE FUTURE?
# use within 366 future date ranges
#
createAAIIDataStoreSIProReturnsTable <- function(conn) {

  ost  <- dbGetQuery(conn,"show time zone")[[1]]
  osp  <- dbGetQuery(conn,"show search_path")[[1]]
  oswm <- dbGetQuery(conn,"show work_mem")[[1]]
  
  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '1200MB'"))
  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_data_store,sipro_stage") )
  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")

  dbGetQuery(conn, 
  paste0("
  create table sipro_data_store.si_returns as
  -- explain --analyze
  select retdate.dateindex retdate_dateindex, 
    ci.dateindex         now_dateindex,
    ci.dateindexeom      now_dateindexeom,
    ci.company_id_unq    now_company_id_unq,
    ci.ticker_unq        now_ticker_unq,
    ci.company           now_company,
    psd.price            now_price,
    w52.dateindex        w52_dateindex,
    w52.dateindexeom     w52_dateindexeom,
    w52.company_id_unq   w52_company_id_unq,
    w52.ticker_unq       w52_ticker_unq,
    w52.company          w52_company,
    w52.pricebck         w52_pricebck,
    w52.prchg_52w        w52_prchg_52w,
    w52.prchg_52w_ann    w52_prchg_52w_ann,
    w52.price            w52_price,
    w52.divaccmf4q       w52_divaccmf4q,
    w52.pradchg_52w      w52_pradchg_52w,
    w52.pradchg_52w_ann  w52_pradchg_52w_ann,
    w26.dateindex        w26_dateindex,
    w26.dateindexeom     w26_dateindexeom,
    w26.company_id_unq   w26_company_id_unq,
    w26.ticker_unq       w26_ticker_unq,
    w26.company          w26_company,
    w26.pricebck         w26_pricebck,
    w26.prchg_26w        w26_prchg_26w,
    w26.prchg_26w_ann    w26_prchg_26w_ann,
    w26.price            w26_price,
    w26.divaccmf2q       w26_divaccmf2q,
    w26.pradchg_26w      w26_pradchg_26w,
    w26.pradchg_26w_ann  w26_pradchg_26w_ann,
    w13.dateindex        w13_dateindex,
    w13.dateindexeom     w13_dateindexeom,
    w13.company_id_unq   w13_company_id_unq,
    w13.ticker_unq       w13_ticker_unq,
    w13.company          w13_company,
    w13.pricebck         w13_pricebck,
    w13.prchg_13w        w13_prchg_13w,
    w13.prchg_13w_ann    w13_prchg_13w_ann,
    w13.price            w13_price,
    w13.divaccmf1q       w13_divaccmf1q,
    w13.pradchg_13w      w13_pradchg_13w,
    w13.pradchg_13w_ann  w13_pradchg_13w_ann
  from 
                  si_retdate retdate
             join si_ci ci 
             -- REPLACEMENT partial generation ( not useful )
             --     ( select * from si_retdate where dateindex = 15705  ) retdate
             -- join ( select * from si_ci where dateindex  = 15705  ) ci 
             
                on retdate.dateindex = ci.dateindex 
                
                
                
                
        left join si_psd psd
                on ci.dateindex      = psd.dateindex
               and ci.company_id_unq = psd.company_id_unq

        left join lateral ( select cif.dateindex, cif.dateindexeom, cif.company_id_unq, cif.ticker_unq, cif.company,
        fut.pricebck,
        fut.prchg_52w,
        fut.prchg_52w_ann,
        fut.price,
        fut.divaccmf4q,
        fut.pradchg_52w,
        fut.pradchg_52w_ann
        from
                
                   si_ci cif 

        left join (
           select fut_i.dateindex, fut_i.company_id_unq, 
                  fut_i.pricebck, 
                  fut_i.prchg_52w, 
                  fut_i.prchg_52w_ann,
                  fut_i.price,
                  fut_i.divaccmf4q,

                  fut_i.prchg_52w + 
                    case 
                    when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                    else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                    end * 100 pradchg_52w,

                ( fut_i.prchg_52w + 
                    case 
                    when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                    else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                    end * 100 ) * 1 pradchg_52w_ann

           from (
           select psd.dateindex, psd.company_id_unq,

           psd.price::numeric(15,2)/(nullif(psd.prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
           psd.prchg_52w::numeric(15,2),
           psd.prchg_52w::numeric(15,2) * 1 prchg_52w_ann,

           coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
           coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
           coalesce(isq.dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
           coalesce(isq.dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q,

           psd.price::numeric(15,2)

           from
           si_isq isq full outer join si_psd psd 
           on  isq.company_id_unq = psd.company_id_unq
           and isq.dateindex      = psd.dateindex
           ) fut_i
           ) fut on cif.dateindex      = fut.dateindex 
                and cif.company_id_unq = fut.company_id_unq 

           where ci.company_id_unq       = cif.company_id_unq
           and retdate.dateindexf12mlwd  = cif.dateindex

           ) w52 on (true)

        left join lateral ( select cif.dateindex, cif.dateindexeom, cif.company_id_unq, cif.ticker_unq, cif.company,
        fut.pricebck,
        fut.prchg_26w,
        fut.prchg_26w_ann,
        fut.price,
        fut.divaccmf2q,
        fut.pradchg_26w,
        fut.pradchg_26w_ann
        from
                
                   si_ci cif 

        left join (
           select fut_i.dateindex, fut_i.company_id_unq, 
                  fut_i.pricebck, 
                  fut_i.prchg_26w, 
                  fut_i.prchg_26w_ann,
                  fut_i.price,
                  fut_i.divaccmf2q,

                  fut_i.prchg_26w + 
                    case 
                    when  fut_i.divaccmf2q = 0.00::numeric(15,2) then 0.00 
                    else (fut_i.divaccmf2q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                    end * 100 pradchg_26w,

                ( fut_i.prchg_26w + 
                    case 
                    when  fut_i.divaccmf2q = 0.00::numeric(15,2) then 0.00 
                    else (fut_i.divaccmf2q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                    end * 100 ) * 2  pradchg_26w_ann

           from (
           select psd.dateindex, psd.company_id_unq,

           psd.price::numeric(15,2)/(nullif(psd.prchg_26w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
           psd.prchg_26w::numeric(15,2),
           psd.prchg_26w::numeric(15,2) * 2 prchg_26w_ann,

           coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
           coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   divaccmf2q,

           psd.price::numeric(15,2)

           from
           si_isq isq full outer join si_psd psd 
           on  isq.company_id_unq = psd.company_id_unq
           and isq.dateindex      = psd.dateindex
           ) fut_i
           ) fut on cif.dateindex      = fut.dateindex 
                and cif.company_id_unq = fut.company_id_unq 

           where ci.company_id_unq       = cif.company_id_unq
           and retdate.dateindexf12mlwd  = cif.dateindex

           ) w26 on (true)
           
        left join lateral ( select cif.dateindex, cif.dateindexeom, cif.company_id_unq, cif.ticker_unq, cif.company,
        fut.pricebck,
        fut.prchg_13w,
        fut.prchg_13w_ann,
        fut.price,
        fut.divaccmf1q,
        fut.pradchg_13w,
        fut.pradchg_13w_ann
        from
                
                   si_ci cif 

        left join (
           select fut_i.dateindex, fut_i.company_id_unq, 
                  fut_i.pricebck, 
                  fut_i.prchg_13w, 
                  fut_i.prchg_13w_ann,
                  fut_i.price,
                  fut_i.divaccmf1q,

                  fut_i.prchg_13w + 
                    case 
                    when  fut_i.divaccmf1q = 0.00::numeric(15,2) then 0.00 
                    else (fut_i.divaccmf1q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                    end * 100 pradchg_13w,

                ( fut_i.prchg_13w + 
                    case 
                    when  fut_i.divaccmf1q = 0.00::numeric(15,2) then 0.00 
                    else (fut_i.divaccmf1q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                    end * 100 ) * 4 pradchg_13w_ann

           from (
           select psd.dateindex, psd.company_id_unq,

           psd.price::numeric(15,2)/(nullif(psd.prchg_13w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
           psd.prchg_13w::numeric(15,2),
           psd.prchg_13w::numeric(15,2) * 4 prchg_13w_ann,

           coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   divaccmf1q,

           psd.price::numeric(15,2)

           from
           si_isq isq full outer join si_psd psd 
           on  isq.company_id_unq = psd.company_id_unq
           and isq.dateindex      = psd.dateindex
           ) fut_i
           ) fut on cif.dateindex      = fut.dateindex 
                and cif.company_id_unq = fut.company_id_unq 

           where ci.company_id_unq       = cif.company_id_unq
           and retdate.dateindexf12mlwd  = cif.dateindex

           ) w13 on (true)
  ;
  -- 5:45
  
   "))

  
  dbGetQuery(conn, paste0("

  create index si_returns_now_dateindex_idx
    on sipro_data_store.si_returns
    using btree
    (now_dateindex);

  create index si_returns_now_dateindexeom_idx
    on sipro_data_store.si_returns
    using btree
    (now_dateindexeom);

  create index si_returns_now_ticker_unq_idx
    on sipro_data_store.si_returns
    using btree
    (now_ticker_unq collate pg_catalog.default);

  create index si_returns_now_company_id_unq_idx
    on sipro_data_store.si_returns
    using btree
    (now_company_id_unq collate pg_catalog.default);

  "))

  dbSendQuery(conn, "vacuum analyze sipro_data_store.si_returns")

  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))

  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))

  # update session work memory
  dbGetQuery(conn, paste0("set work_mem to '",oswm,"'"))
  
  return(invisible())

} 
#
# CURRENLTY, REGENERATE EVERY MONTH
# createAAIIDataStoreSIProReturnsTable(conn)






massAAIIinstallOtherAddParentColumns <- function(conn, 
  not_parent_tabl_regex_expr    = "_\\d+$", 
      parent_tabl_regex_expr    = "",
       child_tabl_epoch         = "16952"
  ) { 


  # make sure autovacuum is turned on(default) ***IMPORTANT **
  # select * from pg_settings where name like 'autovacuum%';

  ost <- dbGetQuery(conn,"show time zone")[[1]]
  osp <- dbGetQuery(conn,"show search_path")[[1]]
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage") )
  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")

  tables <- dbListTablesOneSearchPath(conn)
  
  interested_tables <- sort(tables[!grepl(not_parent_tabl_regex_expr,tables) & grepl(parent_tabl_regex_expr,tables)])
  interested_tables_length <- length(interested_tables)
  interested_tables_length_index <- 0
  for (interested_table in interested_tables) {
    interested_tables_length_index <- interested_tables_length_index + 1
    cat(paste0("Begin interested_table: ", interested_table, 
             " number ", interested_tables_length_index,
             " of ", interested_tables_length,
             "\n"))
    
    if( 1 == 1
    ) {

      your_epoch <- child_tabl_epoch
      
      cat(paste0("  Begin child_tabl_epoch: ", your_epoch, " of ", interested_table,"\n"))

      stmt_action <-  gsub("your_table", interested_table, " 

        select  string_agg('alter table your_table add \"' || column_name || '\" ' || data_type || '; ' ,  E'\n' order by ordinal_position) 
        from information_schema.columns 
        where table_name = 'your_table_your_epoch';

      ")
      
      stmt_action <-  gsub("your_epoch",your_epoch, stmt_action)
      
      cat(noquote(stmt_action),"\n") 
      dynSQLresult <- try( { dbGetQuery(conn, stmt_action) }, silent = TRUE ) 
      
      dynSQLresult_usable <- as.vector(unlist(dynSQLresult))
      
      cat(noquote(dynSQLresult_usable),"\n") 
      try( { dbGetQuery(conn, dynSQLresult_usable) }, silent = TRUE )  

      cat(paste0("  End child_tabl_epoch: ", your_epoch, " of ", interested_table,"\n")) 
      
    }
    
  cat(paste0("End interested_table: ", interested_table,"\n"))
  }

  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))
  
  return(invisible())

}

# Once Only Ever run.  Later add new columns to the PARENT manually by hand

# con <- file(paste0("OUTPUT_massAAIIinstallOtherAddParentColumns", ".txt"));sink(con);sink(con, type="message")
# massAAIIinstallOtherAddParentColumns(conn)
# sink();sink(type="message");close(con)

# testing 
# massAAIIinstallOtherAddParentColumns(conn, not_parent_tabl_regex_expr = "_\\d+$",  parent_tabl_regex_expr = "usrpts", child_tabl_epoch = "16952")
# alter table usrpts drop column "row.names"; 
# alter table usrpts drop column "dateindex"; 
# alter table usrpts drop column "rptid"; 
# alter table usrpts drop column "aaii_id"; 
# alter table usrpts drop column "name"; 
# alter table usrpts drop column "descr"; 
# alter table usrpts drop column "dateindexeom"; 

#
# OUTPUT_massAAIIinstallOtherAddParentColumns_Error_fixes_or_NOT.txt
#
# ERROR:  syntax error at or near "NA"
# LAST ERROR WAS errorlog_16769   ( NOTHING TO DO ( CURRENTLY ) )
# error_log no longer exists ( and I do not use ), su not bothering to child_INHERITS
#
# ( NOTHING TO DO ( CURRENTLY ) )
# ERROR:  child table "si_mgav2_14911" has different type for column "avm_03m"
# so I currently do not need si_mgav2 # so not bothering to child_INHERITS
#





# massAAIISIProIterScreenFScore  <- function(conn, 
#                                            asOfDate = Sys.Date(), 
#                                            earliest_asOfDate = Sys.Date() - 365 * 10 + 3,
#                                            fscore     = "fscore_y1",
#                                            fscore_min =         "8",
#                                            print_sqlstring = FALSE
#                                            ) { 
massAAIISIProIterScreenFScore  <- function(conn, 
                                           asOfDate          = Sys.Date(), 
                                           earliest_asOfDate = Sys.Date() - 365 * 10 + 3,
                                           fscore            = "fscore_y1", # fscore          = "fscore_12m" # current ONLY other
                                           fscore_rexprval   = ">= 8",      # fscore_rexprval = "<= 1",  # MAKE sure all 4 are unique
                                           rpbvps_rexprval   = "<= 20",     # rpbvps_rexprval = "> 80"   # MAKE sure all 4 are unique
                                           mktcap_rexprval   = "> 0.01",    # mktcap_rexprval = "> 200.01"
                                           print_sqlstring   = FALSE
                                           ) { 

  print("print(args(massAAIISIProIterScreenFScore))")
  print(args(massAAIISIProIterScreenFScore))
  
  print("print(match.call())")
  print( match.call())

  # # ANDRE some experince: falls below $2.00/share 
  # #  then gets de-listed but does not go 'Over-The-Counter'
  # 
  # This data no longer exists!
  # Why?
  # 1) The entity/company is now private ( GETS ABSORBED: Happens )
  # 2) This entity was delisted
  # 3) This entity has filed for bankruptcy
  # If none of these reasons seem applicable, please let us 
  # 
  # http://www.wikinvest.com/stock/Banks.com_Inc_(BNX)
  # 
  # BNX is defunct.
  # http://seekingalpha.com/symbol/BNX
  
  ost <- dbGetQuery(conn,"show time zone")[[1]]

  osp <- dbGetQuery(conn,"show search_path")[[1]]

  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage, ", osp) )

  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")
  print(dbGetQuery(conn, "show time zone"))
  
  # previous last weekday of last month 
  interested_Date <- if( asOfDate < (lastWeekDayDateOfMonth(asOfDate) + 1) ) { lastWeekDayDateOfMonth( lubridate::`%m+%`(asOfDate, base::months(-1)))  } else {  lastWeekDayDateOfMonth(asOfDate)  }
    

  "
  with allofit as (
  -- BEGIN PIOTROSKI
  select
  --set search_path=sipro_stage, pg_catalog, public; show search_path;
  scrn.dateindex, 
  scrn.company_id, 
  scrn.company, 
  scrn.ticker, 
  scrn.sp, 
  scrn.mktcap,
  scrn.f_score_winner,
  case when rtns2.later_dateindex is not null then 't'::boolean else 'f'::boolean end later_dateindex_found,
  rtns2.later_dateindex,
  rtns2.later_company_id, 
  rtns2.later_price, 
  rtns2.later_prchg_52w, 
  rtns2.later_prchg_52w_eq_neg_100,
  rtns2.later_price_back, 
  rtns2.later_dividends_accm_back,
  rtns2.later_price_back_eq_zero, 
  rtns2.later_divs_ret_chg,
  rtns2.later_price_a_divs_ret_chg,   
  -- if I can not figure out an 'effective price and dividends return change'
  --  then just return -100.00 ( assume the company when 'out of business'): Patrick OS 
  --      coalesce(NULL,x): right side of a left outer join
  --    but note: this may be TOO conservative, the company(ticker) may have been brought out and engulfed ( e.g. HPQ )
  --      but sipro does not seem to have that DIRECT engulfed information
  coalesce(rtns2.later_eff_price_a_divs_ret_chg,-100.00) later_eff_price_a_divs_ret_chg,
  case when scrn.sp = '500' then scrn.mktcap / scrn.sp500_tot_mktcap_wt * coalesce(rtns2.later_eff_price_a_divs_ret_chg,-100.00) 
       else null end later_eff_price_a_divs_ret_chg_if_sp500_mktcap_wt

  from (
    select 
    ci2.dateindex, 
    ci2.company_id, 
    ci2.company, 
    ci2.ticker, 
    ci2.sp, 
    -- if sp500, then total market cap weight
    -- NOTE: inline: ci_s is not related(joinable?) to ci 
    (select sum(mktcap::numeric(15,2)) from si_psd_15705 psd_s, si_ci_15705 ci_s where psd_s.company_id = ci_s.company_id and ci_s.sp = '500' ) sp500_tot_mktcap_wt,
    psd2.mktcap,
    -- case when ci2.adr = 'f' and ci2.exchange != 'O' and perc2.rpbvps::smallint <= 20 and rat2.fscore_y1::smallint >= 8                        then 't'::boolean else 'f'::boolean end f_score_winner  
       case when ci2.adr = 'f' and ci2.exchange != 'O' and perc2.rpbvps::smallint <= 20 and rat2.fscore_y1::smallint >= 8 and psd2.mktcap > 0.01 then 't'::boolean else 'f'::boolean end f_score_winner
    from (
      select ci.* from (      -- = '500' : S&P 500 Index, S&P MidCap 400, S&P SmallCap 600: mutually exclusive
      select dateindex, company_id, company, ticker, exchange, sp, adr from si_ci_15705 
        where company_id 
          in ( select company_id from si_ci_15705 where company_id is not null 
                 group by company_id having count(company_id) = 1 
             ) 
          --and adr = 'f'           -- f_score 1 of 4
          --and  exchange != 'O'    -- f_score 2 of 4
                               ) ci 
       ) ci2, (
      select psd.* from ( 
      select company_id, mktcap::numeric(15,2) from si_psd_15705
        where company_id 
          in ( select company_id from si_psd_15705 where company_id is not null 
                 group by company_id having count(company_id) = 1 
             ) 
                                    ) psd
      ) psd2, (
      select perc.* from (
      select company_id, rpbvps::smallint from si_perc_15705
        where company_id 
          in ( select company_id from si_perc_15705 where company_id is not null 
                 group by company_id having count(company_id) = 1 
             ) 
         --and rpbvps::smallint <= 20      -- f_score 3 of 4
                                    ) perc
      ) perc2, (
      select rat.* from (
      select company_id, fscore_y1::integer from si_rat_15705
        where company_id 
          in ( select company_id from si_rat_15705 where company_id is not null 
                 group by company_id having count(company_id) = 1 
             ) 
          --and fscore_y1::smallint >= 8   -- f_score 4 of 4
                                        ) rat
      ) rat2
    where 
      ci2.company_id = psd2.company_id and
      ci2.company_id = perc2.company_id and 
      ci2.company_id = rat2.company_id
  ) scrn
  -- note: if the future value (means no future company record) does not exist, 
  -- by left outer join rules its RIGHT side empty value is SQL(NULL), R(NA)
  --   that means effective_ret is NULL/NA ( I have 'NOT YET' changed this to some OTHER value
  --   e.g. 0.00, -100.00 or galaxy, universe, sector, or industry average or whatever.
  left outer join (
  select  
    rtns.dateindex later_dateindex,
    rtns.company_id later_company_id, 
    rtns.ticker later_ticker, 
    rtns.price later_price, 
    rtns.prchg_52w later_prchg_52w, 
    rtns.prchg_52w_eq_neg_100 later_prchg_52w_eq_neg_100,
    rtns.price_back later_price_back, 
    rtns.dividends_accm_back later_dividends_accm_back,
    rtns.price_back_eq_zero later_price_back_eq_zero, 
    rtns.divs_ret_chg later_divs_ret_chg,
    rtns.price_a_divs_ret_chg later_price_a_divs_ret_chg,   
    rtns.eff_price_a_divs_ret_chg later_eff_price_a_divs_ret_chg
  from (
    select 
    psd.dateindex,
    psd.company_id, 
    ci.ticker,
    psd.price, 
    psd.prchg_52w, 
    psd.prchg_52w_eq_neg_100, 
    psd.price_back, isq.dividends_accm_back,
    psd.price_back::numeric(15,2) = 0.00::numeric(15,2) price_back_eq_zero,
                    --researved for BETTER_FUTURE_FIX edge case detection and correction
                    --price back(ABOVE: 199 cases of 8000 (16070)) one year is zero, 
                    --  then I CAN NOT calculate the 'relative percent % return on dividends'
                    --  so for RIGHT NOW ( this edge case, just make 'relative dividends return change' to be NULL )
                    (isq.dividends_accm_back / nullif(psd.price_back::numeric(15,2),0.00::numeric(15,2))) * 100 divs_ret_chg,
    psd.prchg_52w + (isq.dividends_accm_back / nullif(psd.price_back::numeric(15,2),0.00::numeric(15,2))) * 100 price_a_divs_ret_chg,
    -- ULTIMATE ANSWER: eff_price_a_divs_ret_chg
    -- patch fix(FROM LINE ABOVE): if the absolute accumulated dividends is JUST zero, 
    --  then 'relative (price and dividends return)' is JUST 'relative price return' 
    --  otherwise: TRY to calculate the (relative price return) PLUS 'relative dividends return change'
    -- NOTE: this (indirectly) does not TRUELY handle: prchg_52w::numeric(15,2) = -100.00::numeric(15,2)
    --   but a 'poor company' may not pay dividends anyways
    psd.prchg_52w::numeric(15,2) + case when isq.dividends_accm_back = 0.00::numeric(15,2) then 0.00 
                                           else (isq.dividends_accm_back / nullif(psd.price_back::numeric(15,2),0.00::numeric(15,2))) * 100 
                                           end eff_price_a_divs_ret_chg
    from (
      select 
      dateindex,
      company_id, 
      price::numeric(15,2), 
      prchg_52w::numeric(15,2), 
                             prchg_52w::numeric(15,2) = -100.00::numeric(15,2) prchg_52w_eq_neg_100,
                             -- prchg_52w_eq_neg_100( ABOVE: 4 cases of 9871 (16070)) is -100
                             --   then I can not calclulate the ABSOLUTE price_back
                             --      this problem cascades UP such that I 
                             --      can not calculate the 'relative percent % return on dividends'
      price::numeric/(nullif(prchg_52w::numeric(15,2),  -100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) price_back 
      from 
      si_psd_16070 
        where company_id 
          in ( select company_id from si_psd_16070 where company_id is not null 
                 group by company_id having count(company_id) = 1 
             )
      ) psd,(
      select 
      company_id, 
      coalesce(dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
      coalesce(dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
      coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
      coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   dividends_accm_back
      from 
        si_isq_16070 
        where company_id 
          in ( select company_id from si_isq_16070 where company_id is not null 
                 group by company_id having count(company_id) = 1 
             ) 
      ) isq ,(  -- need ci.ticker because at 15184 - 2011-07-29 -> old company_id changed to the new company_id
      select    -- so future performance joins on ticker to ticker_later in a 12 month span ( to determine future returns )
      company_id, 
      ticker
      from 
        si_ci_16070 
        where company_id 
          in ( select company_id from si_ci_16070 where company_id is not null 
                 group by company_id having count(company_id) = 1 
             ) 
      ) ci
    where 
      -- psd.company_id = isq.company_id
      ci.company_id  = psd.company_id and
      ci.company_id  = isq.company_id and
      psd.company_id = isq.company_id 
  ) rtns  -- rtns where rtns.price_back_eq_zero = 't' or rtns.prchg_52w_eq_neg_100 = 't';
                        -- 199 cases                     -- 4 cases  ( of 9871 cases in 16070 )
  ) rtns2 -- at 2011-07-29(15184) the company_id changes 
          -- so all past companies back and 'through and including' 2010-07-30(14820) match on ticker = later_ticker
  on ( select case when (14819+1) < substr('_15705', 2, 6)::int and substr('_15705', 2, 6)::int < (15185-1) then  scrn.ticker       else  scrn.company_id       end )  = 
     ( select case when (14819+1) < substr('_15705', 2, 6)::int and substr('_15705', 2, 6)::int < (15185-1) then rtns2.later_ticker else rtns2.later_company_id end ) 
     where 1 = 1
  -- END PIOTROSKI
  )
  select to_timestamp(dateindex*3600*24)::date || ' ' || company_id || ' ' || mktcap || ' ' || ticker || ' ' || company as who, later_price_a_divs_ret_chg effective_ret 
  from allofit where allofit.f_score_winner = 't'
  union all
  select NULL who, ( select avg(later_price_a_divs_ret_chg) from ( select later_price_a_divs_ret_chg from allofit where allofit.f_score_winner = 't') sum_me) effective_ret
  union all
  select 'sp500' who, sum(later_eff_price_a_divs_ret_chg_if_sp500_mktcap_wt) 
  from allofit
  " -> sqlstring 

  sqlstring_all <- sqlstring
  
  sqlstring_all <- gsub("_15705", "_XXXXXX"  ,  sqlstring_all)
  sqlstring_all <- gsub("_16070", "_YYYYYY"  ,  sqlstring_all)
    
  # if(fscore == "fscore_y1") # then, do nothing, this is the default 
  if(fscore == "fscore_12m") { 
    sqlstring_all <- gsub("fscore_y1", "fscore_12m",  sqlstring_all)
  }
  print(paste0("fscore == " ,"'", fscore,"'"))
  print("")
  
#   # if(fscore_min == "8") # then, do nothing, this is the default
#   if(fscore_min == "9") {
#     sqlstring_all <- gsub(paste0(fscore,"::smallint >= 8"), paste0(fscore,"::smallint >= 9"),  sqlstring_all)
#   }
#   print(paste0("fscore_min ==  " ,"'", fscore_min,"'"))
#   print("")
  
  # if(fscore_rexprval == ">= 8") # then, do nothing, this is the default
  if(fscore_rexprval != ">= 8") {
    sqlstring_all <- gsub(paste0(fscore, "::smallint >= 8"), paste0(fscore, "::smallint ", fscore_rexprval),  sqlstring_all)
  }
  print(paste0("fscore_rexprval ==  " ,"'", fscore_rexprval,"'"))
  print("")

  # if(rpbvps_rexprval == "<= 20") # then, do nothing, this is the default
  if(rpbvps_rexprval != "<= 20") {
    sqlstring_all <- gsub(paste0("rpbvps", "::smallint <= 20"), paste0("rpbvps", "::smallint ", rpbvps_rexprval),  sqlstring_all)
  }
  print(paste0("rpbvps_rexprval ==  " ,"'", rpbvps_rexprval,"'"))
  print("")

  # if(mktcap_rexprval == "> 0.01") # then, do nothing, this is the default
  if(mktcap_rexprval != "> 0.01") {
    sqlstring_all <- gsub(paste0("mktcap ", "> 0.01"), paste0("mktcap ", mktcap_rexprval),  sqlstring_all)
  }
  print(paste0("mktcap_rexprval ==  " ,"'", mktcap_rexprval,"'"))
  print("")
  
  MoreDates_iter <- 1
  MoreDates <- TRUE
  while(MoreDates) {
    # going from the *present* going backwards in time
    MoreDates_iter <- MoreDates_iter - 1
  
    # screen date
    new_interested_Date_integer        <- as.integer(lastWeekDayDateOfMonth( lubridate::`%m+%`( zoo::as.Date(interested_Date), base::months(MoreDates_iter))))
  
    # if earlier than earliest_asOfDate
    if(zoo::as.Date(new_interested_Date_integer) < earliest_asOfDate) { 
      print(paste0(zoo::as.Date(new_interested_Date_integer) ," ", new_interested_Date_integer,
                          " new_interested_Date is earlier than earliest_asOfDate: "," ", 
                          earliest_asOfDate, " ", as.integer(earliest_asOfDate), " so stopping."
      ))
      break 
    }
    
    # if no initial screen date then exit
    if(!dbExistsTable(conn, paste0("si_ci_", new_interested_Date_integer))) { 
      print(paste0("no initial source table: ",zoo::as.Date(new_interested_Date_integer) ," ", new_interested_Date_integer))
      break 
    }
    
    
    # later date
    new_later_interested_Date_integer  <- as.integer(lastWeekDayDateOfMonth( lubridate::`%m+%`( zoo::as.Date(interested_Date), base::months(MoreDates_iter+12))))
  

  
    # if not a 'later table' then create it ( at least I will have the company predictions)
    # dbExistsTable # fails with zero record tables
      if(dbGetQuery(conn, paste0("select count(*) from information_schema.tables  
          where table_type in ('LOCAL TEMPORARY','BASE TABLE') and table_name = '","si_ci_", new_later_interested_Date_integer,"'")) == 0) {
        
        # empty stubs so I can run the query
        dbGetQuery(conn, paste0("create temporary table ", "si_ci_",   new_later_interested_Date_integer, " as select * from ", "si_ci_",   new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_psd_",  new_later_interested_Date_integer, " as select * from ", "si_psd_",  new_interested_Date_integer, " where 1 = 0" ))  
        dbGetQuery(conn, paste0("create temporary table ", "si_perc_", new_later_interested_Date_integer, " as select * from ", "si_perc_", new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_rat_",  new_later_interested_Date_integer, " as select * from ", "si_rat_",  new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_isq_",  new_later_interested_Date_integer, " as select * from ", "si_isq_",  new_interested_Date_integer, " where 1 = 0" ))
      
      }
      
      sqlstring_all <- gsub("_XXXXXX", paste0("_", new_interested_Date_integer      ),  sqlstring_all)
      sqlstring_all <- gsub("_YYYYYY", paste0("_", new_later_interested_Date_integer),  sqlstring_all)
  
      #  "2012-12-31" == 15705
      if(print_sqlstring == TRUE) print(writeLines(sqlstring_all))
      
      print(paste0("new_interested_Date: ",       as.character(zoo::as.Date(new_interested_Date_integer)), " ", new_interested_Date_integer))
      print(paste0("new_later_interested_Date: ", as.character(zoo::as.Date(new_later_interested_Date_integer)), " ", new_later_interested_Date_integer))
      
      result <- dbGetQuery(conn, sqlstring_all)
      print(left_just(result))
      
      # back to original
      sqlstring_all <- gsub(paste0("_", new_interested_Date_integer      ), "_XXXXXX",  sqlstring_all)
      sqlstring_all <- gsub(paste0("_", new_later_interested_Date_integer), "_YYYYYY",  sqlstring_all)

  }
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))

  return(invisible())
  
}

# testing 
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date(15705) + 2)
# testing
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date("2011-12-30") + 2)
# testing
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date("2012-12-31") + 2) # HERE #
# testing
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date("2013-12-31") + 2) # HERE #
# testing
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date("2014-12-31") + 2)
# running
# massAAIISIProIterScreenFScore(conn)
# 
# con <- file(paste0("OUTPUT_fscore_12m", ".txt"));sink(con);sink(con, type="message")
# 
# massAAIISIProIterScreenFScore(conn, fscore = "fscore_12m")
# 
# sink();sink(type="message");close(con)
#  
# massAAIISIProIterScreenFScore(conn, fscore = "fscore_12m", fscore_min = "9")
#
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date("2012-12-31") + 2, earliest_asOfDate =  zoo::as.Date("2012-12-31") -1, print_sqlstring = TRUE)
#
# FUTURE NAs
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date("2011-02-28") + 2, earliest_asOfDate =  zoo::as.Date("2011-02-28") -1, print_sqlstring = TRUE)
#   
# 
# massAAIISIProIterScreenFScore(conn, asOfDate = zoo::as.Date("2012-12-31") + 2, earliest_asOfDate =  zoo::as.Date("2012-12-31") -1, fscore = "fscore_12m", fscore_rexprval = "<= 1", rpbvps_rexprval =  "> 80", mktcap_rexprval = "> 200.01", print_sqlstring = TRUE)      





massAAIISIProIterScreenOSME  <- function(conn, 
                                           asOfDate = Sys.Date(), 
                                           earliest_asOfDate = Sys.Date() - 365 * 10 + 3,
                                           me_pctrk_ord_expr = "asc nulls last limit 5",
                                           print_sqlstring = FALSE
                                           ) { 

  print("print(args(massAAIISIProIterScreenOSME))")
  print(args(massAAIISIProIterScreenOSME))

  print("print(match.call())")
  print( match.call())

  # # ANDRE some experince: falls below $2.00/share 
  # #  then gets de-listed but does not go 'Over-The-Counter'
  # 
  # This data no longer exists!
  # Why?
  # 1) The entity/company is now private ( GETS ABSORBED: Happens )
  # 2) This entity was delisted
  # 3) This entity has filed for bankruptcy
  # If none of these reasons seem applicable, please let us 
  # 
  # http://www.wikinvest.com/stock/Banks.com_Inc_(BNX)
  # 
  # BNX is defunct.
  # http://seekingalpha.com/symbol/BNX
  
  ost <- dbGetQuery(conn,"show time zone")[[1]]

  osp <- dbGetQuery(conn,"show search_path")[[1]]

  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage, ", osp) )

  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")
  print(dbGetQuery(conn, "show time zone"))
  
  # previous last weekday of last month 
  interested_Date <- if( asOfDate < (lastWeekDayDateOfMonth(asOfDate) + 1) ) { lastWeekDayDateOfMonth( lubridate::`%m+%`(asOfDate, base::months(-1)))  } else {  lastWeekDayDateOfMonth(asOfDate)  }
    
  "
  with allofit as (
  -- BEGIN PIOTROSKI
  select
  --set search_path=sipro_stage, pg_catalog, public; show search_path;
  scrn.dateindex, 
  scrn.company_id, 
  scrn.company, 
  scrn.ticker, 
  scrn.sp, 
  scrn.mktcap,
  scrn.me_pctrk,
  case when rtns2.later_dateindex is not null then 't'::boolean else 'f'::boolean end later_dateindex_found,
  rtns2.later_dateindex,
  rtns2.later_company_id, 
  rtns2.later_price, 
  rtns2.later_prchg_52w, 
  rtns2.later_prchg_52w_eq_neg_100,
  rtns2.later_price_back, 
  rtns2.later_dividends_accm_back,
  rtns2.later_price_back_eq_zero, 
  rtns2.later_divs_ret_chg,
  rtns2.later_price_a_divs_ret_chg,   
  -- if I can not figure out an 'effective price and dividends return change'
  --  then just return -100.00 ( assume the company when 'out of business'): Patrick OS 
  --      coalesce(NULL,x): right side of a left outer join
  --    but note: this may be TOO conservative, the company(ticker) may have been brought out and engulfed ( e.g. HPQ )
  --      but sipro does not seem to have that DIRECT engulfed information
  coalesce(rtns2.later_eff_price_a_divs_ret_chg,-100.00) later_eff_price_a_divs_ret_chg,
  case when scrn.sp = '500' then scrn.mktcap / scrn.sp500_tot_mktcap_wt * coalesce(rtns2.later_eff_price_a_divs_ret_chg,-100.00) 
  else null end later_eff_price_a_divs_ret_chg_if_sp500_mktcap_wt

  from (

  select
  scrn4.dateindex, 
  scrn4.company_id, 
  scrn4.company, 
  scrn4.ticker, 
  scrn4.sp, 
  scrn4.adr,
  scrn4.exchange,
  scrn4.sp500_tot_mktcap_wt,
  scrn4.mktcap,
  scrn4.me_value,
    scrn4.me_value_pctrk,
  scrn4.me_earn_qual,
    scrn4.me_earn_qual_pctrk,
  scrn4.me_shr_orien,
    scrn4.me_shr_orien_pctrk,
  scrn4.equity_q1,
  scrn4.me_ret_inv_cap,
    scrn4.me_ret_inv_cap_pctrk,
  scrn4.me_26w_prc_momntm,
    scrn4.me_26w_prc_momntm_pctrk,

  scrn4.me_value_pctrk + scrn4.me_earn_qual_pctrk + scrn4.me_shr_orien_pctrk + scrn4.me_ret_inv_cap_pctrk + scrn4.me_26w_prc_momntm_pctrk me_pctrk

  from (
  select 
  scrn3.dateindex, 
  scrn3.company_id, 
  scrn3.company, 
  scrn3.ticker, 
  scrn3.sp, 
  scrn3.adr,
  scrn3.exchange,
  scrn3.sp500_tot_mktcap_wt,
  scrn3.mktcap,
  scrn3.me_value,
          -- ** SOLUTION ** --
          -- ONLY NOT obfuscates 'my criteria'        # partitions twice(boolean): once on 'my criteria' and once not on 'my criteria'

         -- value criteria: higher # is better ( lesser # rank )
         case when                                     scrn3.me_value is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 
         then       percent_rank() over (partition by (scrn3.me_value is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 ) order by scrn3.me_value desc) * 100.00 
         else null end  me_value_pctrk,

  scrn3.me_earn_qual,

         -- earn_qual criteria: higher # is better ( lesser # rank )
         case when                                     scrn3.me_earn_qual is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 
         then       percent_rank() over (partition by (scrn3.me_earn_qual is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 ) order by scrn3.me_earn_qual desc) * 100.00 
         else null end  me_earn_qual_pctrk,

  scrn3.me_shr_orien,

         -- me_shr_orien criteria: LOWER(-1 at source) # is better ( lesser # rank )
         case when                                     scrn3.me_shr_orien is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 
         then       percent_rank() over (partition by (scrn3.me_shr_orien is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 ) order by scrn3.me_shr_orien desc) * 100.00 
         else null end  me_shr_orien_pctrk,

  scrn3.equity_q1,

  scrn3.me_ret_inv_cap,

         -- earn_qual criteria: higher # is better ( lesser # rank )
         case when                                     scrn3.me_ret_inv_cap is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 
         then       percent_rank() over (partition by (scrn3.me_ret_inv_cap is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 ) order by scrn3.me_ret_inv_cap desc) * 100.00 
         else null end  me_ret_inv_cap_pctrk,

  scrn3.me_26w_prc_momntm,

         -- me_me_26w_prc_momntm: higher # is better ( lesser # rank )
         case when                                     scrn3.me_26w_prc_momntm is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 
         then       percent_rank() over (partition by (scrn3.me_26w_prc_momntm is not null and scrn3.adr = 'f' and scrn3.exchange != 'O' and scrn3.mktcap >= 200.00 ) order by scrn3.me_26w_prc_momntm desc) * 100.00 
         else null end  me_26w_prc_momntm_pctrk

  from (
      select 
      ci2.dateindex, 
      ci2.company_id, 
      ci2.company, 
      ci2.ticker, 
      ci2.sp, 
      ci2.adr,
      ci2.exchange,

      -- if sp500, then total market cap weight
      -- NOTE: inline: ci_s is not related(joinable?) to ci 
      (select sum(mktcap::numeric(15,2)) from si_psd_15705 psd_s, si_ci_15705 ci_s where psd_s.company_id = ci_s.company_id and ci_s.sp = '500' ) sp500_tot_mktcap_wt,

      -- value criteria: higher # is better ( lesser # rank )
      case when ( dividends_accm  + fcfps_accm ) = 0.00::numeric(15,2) and psd2.mktcap = 0.00::numeric(15,2) then null else ( dividends_accm  + fcfps_accm )/ nullif(psd2.mktcap, 0.00::numeric(15,2))  end me_value,

      -- earn_qual criteria: higher # is better ( lesser # rank )
      ( cfq2.tco_accm - isq2.netinc_accm ) / nullif(psd2.mktcap, 0.00::numeric(15,2)) me_earn_qual,

      -- shr_orien criteria: LOWER(-1 at source) # is better ( lesser # rank )
                     -1.0 * cfq2.tcf_accm / nullif(psd2.mktcap, 0.00::numeric(15,2)) me_shr_orien,

      bsq2.equity_q1,

      -- me_ret_inv_cap: higher # is better ( lesser # rank )
             isq2.epsdc_accm * psd2.shr_aq1  / nullif(bsq2.book_den, 0.00::numeric(15,2)) me_ret_inv_cap,

      -- me_me_26w_prc_momntm: higher # is better ( lesser # rank )
      psd2.prchg_26w me_26w_prc_momntm,

      psd2.mktcap
      from (
        select ci.* from (      -- = '500' : S&P 500 Index, S&P MidCap 400, S&P SmallCap 600: mutually exclusive
        select dateindex, company_id, company, ticker, exchange, sp, adr
        from si_ci_15705 
          where company_id 
            in ( select company_id from si_ci_15705 where company_id is not null 
                   group by company_id having count(company_id) = 1 
               ) 
                                      ) ci 
         ) ci2, (
        select psd.* from ( 
        select company_id, mktcap::numeric(15,2), shr_aq1::numeric(15,2), prchg_26w::numeric(15,2)
        from si_psd_15705
          where company_id 
            in ( select company_id from si_psd_15705 where company_id is not null 
                   group by company_id having count(company_id) = 1 
               ) 
                                      ) psd
        ) psd2, (
        select isq.* from (
        select company_id,
        coalesce(dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   dividends_accm,
        coalesce(netinc_q1::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(netinc_q2::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(netinc_q3::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(netinc_q4::numeric(15,2),0.00::numeric(15,2))   netinc_accm,
        coalesce(epsdc_q1::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(epsdc_q2::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(epsdc_q3::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(epsdc_q4::numeric(15,2),0.00::numeric(15,2))   epsdc_accm
        from si_isq_15705
          where company_id 
            in ( select company_id from si_isq_15705 where company_id is not null 
                   group by company_id having count(company_id) = 1 
               ) 
                                      ) isq
        ) isq2, (
        select cfq.* from (
        select company_id,
        coalesce(fcfps_q1::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(fcfps_q2::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(fcfps_q3::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(fcfps_q4::numeric(15,2),0.00::numeric(15,2))   fcfps_accm,
        coalesce(tco_q1::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(tco_q2::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(tco_q3::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(tco_q4::numeric(15,2),0.00::numeric(15,2))   tco_accm,
        coalesce(tcf_q1::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(tcf_q2::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(tcf_q3::numeric(15,2),0.00::numeric(15,2))   + 
        coalesce(tcf_q4::numeric(15,2),0.00::numeric(15,2))   tcf_accm
        from si_cfq_15705
          where company_id 
            in ( select company_id from si_cfq_15705 where company_id is not null 
                   group by company_id having count(company_id) = 1 
               ) 
                                       ) cfq
        ) cfq2, (
        select bsq.* from (
        select company_id, coalesce(equity_q1::numeric(15,2),0.00::numeric(15,2)) equity_q1, -- sanity check
                           --I do not like: equity can be negative
                           --abs value: punish a company for having negative equity ( andre patch )
                           abs(coalesce(equity_q1::numeric(15,2),0.00::numeric(15,2))) +    
                           coalesce(liab_q1::numeric(15,2),0.00::numeric(15,2))    -
                           coalesce(cash_q1::numeric(15,2),0.00::numeric(15,2))    book_den
        from si_bsq_15705
          where company_id 
            in ( select company_id from si_bsq_15705 where company_id is not null 
                   group by company_id having count(company_id) = 1 
               ) 
                                      ) bsq
        ) bsq2
      where 
        ci2.company_id = psd2.company_id and
        ci2.company_id = isq2.company_id and 
        ci2.company_id = cfq2.company_id and
        ci2.company_id = bsq2.company_id
        
  ) scrn3  -- order by scrn3.mktcap desc              -- just for testing visualization: production does not have these restrictions
  ) scrn4  -- order by me_pctrk nulls last -- limit 5      -- order by scrn4.me_ret_inv_cap_pctrk nulls last -- where scrn4.me_value_pctrk2 < 1.00 order by scrn4.me_value_pctrk2  -- where scrn4.me_value_pctrk is not null  -- just for testing visualization: production does not have these restrictions
  ) scrn
  -- note: if the future value (means no future company record) does not exist, 
  -- by left outer join rules its RIGHT side empty value is SQL(NULL), R(NA)
  --   that means effective_ret is NULL/NA ( I have 'NOT YET' changed this to some OTHER value
  --   e.g. 0.00, -100.00 or galaxy, universe, sector, or industry average or whatever.
  left outer join (
  select  
  rtns.dateindex later_dateindex,
  rtns.company_id later_company_id, 
  rtns.ticker later_ticker, 
  rtns.price later_price, 
  rtns.prchg_52w later_prchg_52w, 
  rtns.prchg_52w_eq_neg_100 later_prchg_52w_eq_neg_100,
  rtns.price_back later_price_back, 
  rtns.dividends_accm_back later_dividends_accm_back,
  rtns.price_back_eq_zero later_price_back_eq_zero, 
  rtns.divs_ret_chg later_divs_ret_chg,
  rtns.price_a_divs_ret_chg later_price_a_divs_ret_chg,   
  rtns.eff_price_a_divs_ret_chg later_eff_price_a_divs_ret_chg
  from (
  select 
  psd.dateindex,
  psd.company_id, 
  ci.ticker,
  psd.price, 
  psd.prchg_52w, 
  psd.prchg_52w_eq_neg_100, 
  psd.price_back, isq.dividends_accm_back,
  psd.price_back::numeric(15,2) = 0.00::numeric(15,2) price_back_eq_zero,
              --researved for BETTER_FUTURE_FIX edge case detection and correction
              --price back(ABOVE: 199 cases of 8000 (16070)) one year is zero, 
              --  then I CAN NOT calculate the 'relative percent % return on dividends'
              --  so for RIGHT NOW ( this edge case, just make 'relative dividends return change' to be NULL )
              (isq.dividends_accm_back / nullif(psd.price_back::numeric(15,2),0.00::numeric(15,2))) * 100 divs_ret_chg,
  psd.prchg_52w + (isq.dividends_accm_back / nullif(psd.price_back::numeric(15,2),0.00::numeric(15,2))) * 100 price_a_divs_ret_chg,
  -- ULTIMATE ANSWER: eff_price_a_divs_ret_chg
  -- patch fix(FROM LINE ABOVE): if the absolute accumulated dividends is JUST zero, 
  --  then 'relative (price and dividends return)' is JUST 'relative price return' 
  --  otherwise: TRY to calculate the (relative price return) PLUS 'relative dividends return change'
  -- NOTE: this (indirectly) does not TRUELY handle: prchg_52w::numeric(15,2) = -100.00::numeric(15,2)
  --   but a 'poor company' may not pay dividends anyways
  psd.prchg_52w::numeric(15,2) + case when isq.dividends_accm_back = 0.00::numeric(15,2) then 0.00 
                                     else (isq.dividends_accm_back / nullif(psd.price_back::numeric(15,2),0.00::numeric(15,2))) * 100 
                                     end eff_price_a_divs_ret_chg
  from (
  select 
  dateindex,
  company_id, 
  price::numeric(15,2), 
  prchg_52w::numeric(15,2), 
                       prchg_52w::numeric(15,2) = -100.00::numeric(15,2) prchg_52w_eq_neg_100,
                       -- prchg_52w_eq_neg_100( ABOVE: 4 cases of 9871 (16070)) is -100
                       --   then I can not calclulate the ABSOLUTE price_back
                       --      this problem cascades UP such that I 
                       --      can not calculate the 'relative percent % return on dividends'
  price::numeric/(nullif(prchg_52w::numeric(15,2),  -100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) price_back 
  from 
  si_psd_16070 
  where company_id 
    in ( select company_id from si_psd_16070 where company_id is not null 
           group by company_id having count(company_id) = 1 
       )
  ) psd,(
  select 
  company_id, 
  coalesce(dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
  coalesce(dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
  coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
  coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   dividends_accm_back
  from 
  si_isq_16070 
  where company_id 
    in ( select company_id from si_isq_16070 where company_id is not null 
           group by company_id having count(company_id) = 1 
       ) 
  ) isq ,(  -- need ci.ticker because at 15184 - 2011-07-29 -> old company_id changed to the new company_id
  select    -- so future performance joins on ticker to ticker_later in a 12 month span ( to determine future returns )
  company_id, 
  ticker
  from 
  si_ci_16070 
  where company_id 
    in ( select company_id from si_ci_16070 where company_id is not null 
           group by company_id having count(company_id) = 1 
       ) 
  ) ci
  where 
  -- psd.company_id = isq.company_id
  ci.company_id  = psd.company_id and
  ci.company_id  = isq.company_id and
  psd.company_id = isq.company_id 
  ) rtns  -- rtns where rtns.price_back_eq_zero = 't' or rtns.prchg_52w_eq_neg_100 = 't';
                  -- 199 cases                     -- 4 cases  ( of 9871 cases in 16070 )
  ) rtns2 -- at 2011-07-29(15184) the company_id changes 
    -- so all past companies back and 'through and including' 2010-07-30(14820) match on ticker = later_ticker
  on ( select case when (14819+1) < substr('_15705', 2, 6)::int and substr('_15705', 2, 6)::int < (15185-1) then  scrn.ticker       else  scrn.company_id       end )  = 
  ( select case when (14819+1) < substr('_15705', 2, 6)::int and substr('_15705', 2, 6)::int < (15185-1) then rtns2.later_ticker else rtns2.later_company_id end ) 
  where 1 = 1
  -- END PIOTROSKI
  )
  select * from ( select to_timestamp(dateindex*3600*24)::date || ' ' ||  me_pctrk ||' ' || company_id || ' ' || mktcap || ' ' || ticker || ' ' ||  company as who, later_price_a_divs_ret_chg effective_ret 
  -- , avg(later_price_a_divs_ret_chg) effective_ret 
  from allofit order by me_pctrk asc nulls last limit 5) all_me
  -- group by grouping sets ((who), ()) 
  union all
  select NULL who, ( select avg(later_price_a_divs_ret_chg) from ( select later_price_a_divs_ret_chg from allofit order by me_pctrk asc nulls last limit 5) sum_me) effective_ret
  union all
  select 'sp500' who, sum(later_eff_price_a_divs_ret_chg_if_sp500_mktcap_wt) 
  from allofit
  
  
  
  " -> sqlstring

  sqlstring_all <- sqlstring
  
  sqlstring_all <- gsub("_15705", "_XXXXXX"  ,  sqlstring_all)
  sqlstring_all <- gsub("_16070", "_YYYYYY"  ,  sqlstring_all)
    
  # if(me_pctrk_ord_expr == "asc nulls last limit 5") # then, do nothing, this is the default
  if(me_pctrk_ord_expr != "asc nulls last limit 5") {
    sqlstring_all <- gsub(paste0("me_pctrk ", "asc nulls last limit 5"), paste0("me_pctrk ",  me_pctrk_ord_expr),  sqlstring_all)
  }
  print(paste0("me_pctrk_ord_expr ==  " ,"'", me_pctrk_ord_expr,"'"))
  print("")
  
  MoreDates_iter <- 1
  MoreDates <- TRUE
  while(MoreDates) {
    # going from the *present* going backwards in time
    MoreDates_iter <- MoreDates_iter - 1
  
    # screen date
    new_interested_Date_integer        <- as.integer(lastWeekDayDateOfMonth( lubridate::`%m+%`( zoo::as.Date(interested_Date), base::months(MoreDates_iter))))
  
    # if earlier than earliest_asOfDate
    if(zoo::as.Date(new_interested_Date_integer) < earliest_asOfDate) { 
      print(paste0(zoo::as.Date(new_interested_Date_integer) ," ", new_interested_Date_integer,
                          " new_interested_Date is earlier than earliest_asOfDate: "," ", 
                          earliest_asOfDate, " ", as.integer(earliest_asOfDate), " so stopping."
      ))
      break 
    }
    
    # if no initial screen date then exit
    if(!dbExistsTable(conn, paste0("si_ci_", new_interested_Date_integer))) { 
      print(paste0("no initial source table: ",zoo::as.Date(new_interested_Date_integer) ," ", new_interested_Date_integer))
      break 
    }
    
    
    # later date
    new_later_interested_Date_integer  <- as.integer(lastWeekDayDateOfMonth( lubridate::`%m+%`( zoo::as.Date(interested_Date), base::months(MoreDates_iter+12))))
  

  
    # if not a 'later table' then create it ( at least I will have the company predictions)
    # dbExistsTable # fails with zero record tables
      if(dbGetQuery(conn, paste0("select count(*) from information_schema.tables  
          where table_type in ('LOCAL TEMPORARY','BASE TABLE') and table_name = '","si_ci_", new_later_interested_Date_integer,"'")) == 0) {
        
        # empty stubs so I can run the query
        dbGetQuery(conn, paste0("create temporary table ", "si_ci_",   new_later_interested_Date_integer, " as select * from ", "si_ci_",   new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_psd_",  new_later_interested_Date_integer, " as select * from ", "si_psd_",  new_interested_Date_integer, " where 1 = 0" ))  
        dbGetQuery(conn, paste0("create temporary table ", "si_cfq_", new_later_interested_Date_integer, " as select * from ", "si_perc_", new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_bsq_",  new_later_interested_Date_integer, " as select * from ", "si_rat_",  new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_isq_",  new_later_interested_Date_integer, " as select * from ", "si_isq_",  new_interested_Date_integer, " where 1 = 0" ))
      
      }
      
      sqlstring_all <- gsub("_XXXXXX", paste0("_", new_interested_Date_integer      ),  sqlstring_all)
      sqlstring_all <- gsub("_YYYYYY", paste0("_", new_later_interested_Date_integer),  sqlstring_all)
  
      #  "2012-12-31" == 15705
      if(print_sqlstring == TRUE) print(writeLines(sqlstring_all))
      
      print(paste0("new_interested_Date: ",       as.character(zoo::as.Date(new_interested_Date_integer)), " ", new_interested_Date_integer))
      print(paste0("new_later_interested_Date: ", as.character(zoo::as.Date(new_later_interested_Date_integer)), " ", new_later_interested_Date_integer))
      
      result <- dbGetQuery(conn, sqlstring_all)
      print(left_just(result))
      
      # back to original
      sqlstring_all <- gsub(paste0("_", new_interested_Date_integer      ), "_XXXXXX",  sqlstring_all)
      sqlstring_all <- gsub(paste0("_", new_later_interested_Date_integer), "_YYYYYY",  sqlstring_all)

  }
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))

  return(invisible())
  
}

# testing 
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date(15705) + 2)
# testing
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date("2011-12-30") + 2)
# testing
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date("2012-12-31") + 2) # HERE #
# testing
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date("2013-12-31") + 2) # HERE #
# testing
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date("2014-12-31") + 2)
# running
# massAAIISIProIterScreenOSME(conn)
# 
# con <- file(paste0("OUTPUT_OSME", ".txt"));sink(con);sink(con, type="message")
# 
# massAAIISIProIterScreenOSME(conn)
# 
# sink();sink(type="message");close(con)
#  

#
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date("2012-12-31") + 2, earliest_asOfDate =  zoo::as.Date("2012-12-31") -1, print_sqlstring = TRUE)
# 
# FUTURE NAs
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date("2011-02-28") + 2, earliest_asOfDate =  zoo::as.Date("2011-02-28") -1, print_sqlstring = TRUE)
# 
# massAAIISIProIterScreenOSME(conn, asOfDate = zoo::as.Date("2012-12-31") + 2, earliest_asOfDate =  zoo::as.Date("2012-12-31") -1, me_pctrk_ord_expr = "desc nulls last limit 5", print_sqlstring = TRUE)


massAAIISIProIterMktSheets  <- function(conn, 
                                           asOfDate = Sys.Date(), 
                                           earliest_asOfDate = Sys.Date() - 365 * 10 + 3,
                                           print_sqlstring = FALSE
                                           ) { 

  print("print(args(massAAIISIProIterMktSheets))")
  print(args(massAAIISIProIterMktSheets))

  print("print(match.call())")
  print( match.call())

  # # ANDRE some experince: falls below $2.00/share 
  # #  then gets de-listed but does not go 'Over-The-Counter'
  # 
  # This data no longer exists!
  # Why?
  # 1) The entity/company is now private ( GETS ABSORBED: Happens )
  # 2) This entity was delisted
  # 3) This entity has filed for bankruptcy
  # If none of these reasons seem applicable, please let us 
  # 
  # http://www.wikinvest.com/stock/Banks.com_Inc_(BNX)
  # 
  # BNX is defunct.
  # http://seekingalpha.com/symbol/BNX
  
  ost <- dbGetQuery(conn,"show time zone")[[1]]

  osp <- dbGetQuery(conn,"show search_path")[[1]]

  # update search path
  dbGetQuery(conn, paste0("set search_path to sipro_stage, ", osp) )

  # update time zone
  dbGetQuery(conn, "set time zone 'utc'")
  print(dbGetQuery(conn, "show time zone"))
  
  # previous last weekday of last month 
  interested_Date <- if( asOfDate < (lastWeekDayDateOfMonth(asOfDate) + 1) ) { lastWeekDayDateOfMonth( lubridate::`%m+%`(asOfDate, base::months(-1)))  } else {  lastWeekDayDateOfMonth(asOfDate)  }
    
  "

  with earn as 
    ( select ci2.dateindex, ci2.company_id, ci2.sp, ci2.ticker, ci2.company, date2.perend_rct,
        case when date2.perend_rct = date2.perend_q1 then                isq2.netinc_q1 * psd2.mktcap
             when date2.perend_rct = date2.perend_q2 then                isq2.netinc_q2 * psd2.mktcap
             when date2.perend_rct = date2.perend_q3 then                isq2.netinc_q3 * psd2.mktcap
             when date2.perend_rct = date2.perend_q4 then                isq2.netinc_q4 * psd2.mktcap
        end netinc_rct,
        case when date2.perend_rct = date2.perend_q1 then cfq2.fcfps_q1 * psd2.shr_aq1 * psd2.mktcap 
             when date2.perend_rct = date2.perend_q2 then cfq2.fcfps_q2 * psd2.shr_aq2 * psd2.mktcap
             when date2.perend_rct = date2.perend_q3 then cfq2.fcfps_q3 * psd2.shr_aq3 * psd2.mktcap
             when date2.perend_rct = date2.perend_q4 then cfq2.fcfps_q4 * psd2.shr_aq4 * psd2.mktcap
        end fcf_rct
    from
                ( 
      select dateindex, company_id, 
        perend_q1, perend_q2, perend_q3, perend_q4,
        greatest(perend_q1, perend_q2, perend_q3, perend_q4) perend_rct 
       from ( select dateindex, company_id, perend_q1, perend_q2, perend_q3, perend_q4 from ( select * from si_date_16952 where company_id in ( select company_id from si_date_16952 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_date_16952 
              union all
              select dateindex, company_id, perend_q1, perend_q2, perend_q3, perend_q4 from ( select * from si_date_16584 where company_id in ( select company_id from si_date_16584 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_date_16584 
       ) si_date_many
       ) date2, ( 
       select dateindex, company_id, ticker, company, sp  
       from ( select dateindex, company_id, ticker, company, sp from ( select * from si_ci_16952 where company_id in ( select company_id from si_ci_16952 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_ci_16952 
              union all
              select dateindex, company_id, ticker, company, sp from ( select * from si_ci_16584 where company_id in ( select company_id from si_ci_16584 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_ci_16584 
       ) si_ci_many
       ) ci2,  ( 
       select dateindex, company_id, 
       coalesce(netinc_q1::numeric(15,2),0.00::numeric(15,2)) netinc_q1,
       coalesce(netinc_q2::numeric(15,2),0.00::numeric(15,2)) netinc_q2,
       coalesce(netinc_q3::numeric(15,2),0.00::numeric(15,2)) netinc_q3,
       coalesce(netinc_q4::numeric(15,2),0.00::numeric(15,2)) netinc_q4
       from ( select dateindex, company_id, netinc_q1, netinc_q2, netinc_q3, netinc_q4 from ( select * from si_isq_16952 where company_id in ( select company_id from si_isq_16952 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_isq_16952 
              union all
              select dateindex, company_id, netinc_q1, netinc_q2, netinc_q3, netinc_q4 from ( select * from si_isq_16584 where company_id in ( select company_id from si_isq_16584 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_isq_16584 
       ) si_isq_many
       ) isq2, ( 
       select dateindex, company_id, 
       coalesce(fcfps_q1::numeric(15,2),0.00::numeric(15,2)) fcfps_q1,
       coalesce(fcfps_q2::numeric(15,2),0.00::numeric(15,2)) fcfps_q2,
       coalesce(fcfps_q3::numeric(15,2),0.00::numeric(15,2)) fcfps_q3,
       coalesce(fcfps_q4::numeric(15,2),0.00::numeric(15,2)) fcfps_q4
       from ( select dateindex, company_id, fcfps_q1, fcfps_q2, fcfps_q3, fcfps_q4 from ( select * from si_cfq_16952 where company_id in ( select company_id from si_cfq_16952 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_cfq_16952 
              union all
              select dateindex, company_id, fcfps_q1, fcfps_q2, fcfps_q3, fcfps_q4 from ( select * from si_cfq_16584 where company_id in ( select company_id from si_cfq_16584 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_cfq_16584 
       ) si_cfq_many
       ) cfq2, ( 
       select dateindex, company_id, mktcap::numeric(15,2) mktcap,
       coalesce(shr_aq1::numeric(15,2),0.00::numeric(15,2)) shr_aq1,
       coalesce(shr_aq2::numeric(15,2),0.00::numeric(15,2)) shr_aq2,
       coalesce(shr_aq3::numeric(15,2),0.00::numeric(15,2)) shr_aq3,
       coalesce(shr_aq4::numeric(15,2),0.00::numeric(15,2)) shr_aq4
       from ( select dateindex, company_id, mktcap, shr_aq1, shr_aq2, shr_aq3, shr_aq4 from ( select * from si_psd_16952 where company_id in ( select company_id from si_psd_16952 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_psd_16952 
              union all
              select dateindex, company_id, mktcap, shr_aq1, shr_aq2, shr_aq3, shr_aq4 from ( select * from si_psd_16584 where company_id in ( select company_id from si_psd_16584 where company_id is not null  group by company_id having count(company_id) = 1  ) ) si_psd_16584 
       ) si_psd_many
       ) psd2

-- not implemented: out of time
--
-- at 2011-07-29(15184) the company_id changes 
-- so all past companies back and 'through and including' 2010-07-30(14820) match on ticker = later_ticker
--
-- NEED to add 'ticker' BACK UP to the 'xxx2' tables above
--
-- select case when (14819+1) < substr('_16952', 2, 6)::int and substr('_16952', 2, 6)::int < (15185-1) then  scrn.ticker       else  scrn.company_id       end  = 
-- select case when (14819+1) < substr('_16952', 2, 6)::int and substr('_16952', 2, 6)::int < (15185-1) then  rtns2.later_ticker else rtns2.later_company_id end 
--
    where ( 
--      select case when (14819+1) < substr('_16952', 2, 6)::int and substr('_16952', 2, 6)::int < (15185-1) 
--        then
--         ci2.sp = '500' and
--         ci2.ticker = date2.ticker  and 
--           ci2.dateindex =  date2.dateindex and 
--         ci2.ticker =  isq2.ticker  and 
--           ci2.dateindex =   isq2.dateindex and
--         ci2.ticker =  cfq2.ticker  and 
--           ci2.dateindex =   cfq2.dateindex and
--         ci2.ticker =  psd2.ticker  and 
--           ci2.dateindex =   psd2.dateindex 
--        else
        ci2.sp = '500' and
        ci2.company_id = date2.company_id  and 
          ci2.dateindex =  date2.dateindex and 
        ci2.company_id =  isq2.company_id  and 
          ci2.dateindex =   isq2.dateindex and
        ci2.company_id =  cfq2.company_id  and 
          ci2.dateindex =   cfq2.dateindex and
        ci2.company_id =  psd2.company_id  and 
          ci2.dateindex =   psd2.dateindex 
--        end
    )
    order by date2.perend_rct desc, netinc_rct desc )
  --select avg(earn2.netinc_rct) from (
  --GOOD 1174
  --select avg(netinc_rct) from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.sp = '500' and earn.dateindex = 16952 limit 2 ) earn2
  --GOOD COST HD
  --select ticker from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.sp = '500' and earn.dateindex = 16952 limit 2 ) earn2
  -- ANSWER 1088.5000000000000000
  -- select avg(netinc_rct) 
  -- from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.dateindex = 16584 and earn.ticker in (   
  --   select ticker from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.sp = '500' and earn.dateindex = 16952 limit 2 ) earn3
  -- )) earn2 
  -- CORRECT
  -- R > ( 1579 + 598 ) / 2.0    [1] 1088.5
  -- OUTPUT
  -- select dateindex, perend_rct, ticker, netinc_rct from earn where earn.dateindex = 16584 and earn.ticker in ( 'COST', 'HD' );
  --16584;'2015-05-03';'HD';1579.00
  --16584;'2015-02-15';'COST';598.00
  -- select avg(netinc_rct) 
  -- from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.dateindex = 16952 and earn.ticker in (   
  --   select ticker from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.sp = '500' and earn.dateindex = 16952 limit 2 ) earn_now
  -- )) earn_now_now 
  -- union
  -- select avg(netinc_rct) 
  -- from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.dateindex = 16584 and earn.ticker in (   
  --   select ticker from ( select dateindex, perend_rct, ticker, netinc_rct from earn where earn.sp = '500' and earn.dateindex = 16952 limit 2 ) earn_now
  -- )) earn_earlier 
  -- 1088.5000000000000000
  -- 1174.0000000000000000
  
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_fcf_limit_0020_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 20 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_fcf_limit_0020_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 20 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_fcf_limit_0050_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 50 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_fcf_limit_0050_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 50 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_fcf_limit_0100_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 100 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_fcf_limit_0100_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 100 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_fcf_limit_0500_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 500 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_fcf_limit_0500_sp500' what, avg(fcf_rct) results, sum(fcf_rct)/avg(fcf_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, fcf_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 500 ) earn_now
  )) earn_earlier 
  
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_netinc_limit_0020_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 20 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_netinc_limit_0020_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 20 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_netinc_limit_0050_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 50 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_netinc_limit_0050_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 50 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_netinc_limit_0100_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 100 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_netinc_limit_0100_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 100 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'avg_netinc_limit_0500_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16952 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 500 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16584*3600*24)::date as_of_dt, 'avg_netinc_limit_0500_sp500' what, avg(netinc_rct) results, sum(netinc_rct)/avg(netinc_rct) how_many
  from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
    where earn.dateindex = 16584 and earn.company_id in (   
      select company_id from ( select dateindex, perend_rct, company_id, ticker, netinc_rct from earn 
      where earn.sp = '500' and earn.dateindex = 16952 limit 500 ) earn_now
  )) earn_earlier 
  
  union all
  select to_timestamp(16952*3600*24)::date as_of_dt, 'sp500_tot_mktcap_adj' what, 
  (select sum(mktcap::numeric(15,2)) from si_psd_16952 psd_s, si_ci_16952 ci_s where psd_s.company_id = ci_s.company_id and ci_s.sp = '500' ) * 500 /  (select sum(mktcap::numeric(15,2) * price::numeric(15,2) ) / avg(mktcap::numeric(15,2) * price::numeric(15,2) ) from si_psd_16952 psd_s, si_ci_16952 ci_s where psd_s.company_id = ci_s.company_id and ci_s.sp = '500' ) results,
  (select sum(mktcap::numeric(15,2) * price::numeric(15,2) ) / avg(mktcap::numeric(15,2) * price::numeric(15,2) ) from si_psd_16952 psd_s, si_ci_16952 ci_s where psd_s.company_id = ci_s.company_id and ci_s.sp = '500' ) how_many

  " -> sqlstring

  sqlstring_all <- sqlstring
  
  sqlstring_all <- gsub("16952", "XXXXXX"  ,  sqlstring_all)
  sqlstring_all <- gsub("16584", "YYYYYY"  ,  sqlstring_all)
  
  MoreDates_iter <- 1
  MoreDates <- TRUE
  while(MoreDates) {
    # going from the *present* going backwards in time
    MoreDates_iter <- MoreDates_iter - 1
  
    # screen date
    new_interested_Date_integer        <- as.integer(lastWeekDayDateOfMonth( lubridate::`%m+%`( zoo::as.Date(interested_Date), base::months(MoreDates_iter))))
  
    # if earlier than earliest_asOfDate
    if(zoo::as.Date(new_interested_Date_integer) < earliest_asOfDate) { 
      print(paste0(zoo::as.Date(new_interested_Date_integer) ," ", new_interested_Date_integer,
                          " new_interested_Date is earlier than earliest_asOfDate: "," ", 
                          earliest_asOfDate, " ", as.integer(earliest_asOfDate), " so stopping."
      ))
      break 
    }
    
    # if no initial screen date then exit
    if(!dbExistsTable(conn, paste0("si_ci_", new_interested_Date_integer))) { 
      print(paste0("no initial source table: ",zoo::as.Date(new_interested_Date_integer) ," ", new_interested_Date_integer))
      break 
    }
    
    
    # later date ( THIS IS ACTUALLY 'new earlier interested Date integer'
    # new_later_interested_Date_integer  <- as.integer(lastWeekDayDateOfMonth( lubridate::`%m+%`( zoo::as.Date(interested_Date), base::months(MoreDates_iter+12))))
    # later date ( THIS IS ACTUALLY 'new earlier interested Date integer'
    new_later_interested_Date_integer  <- as.integer(lastWeekDayDateOfMonth( lubridate::`%m+%`( zoo::as.Date(interested_Date), base::months(MoreDates_iter-12))))

    # if not a 'later table' then create it ( at least I will have the company predictions)
    # dbExistsTable # fails with zero record tables
      if(dbGetQuery(conn, paste0("select count(*) from information_schema.tables  
          where table_type in ('LOCAL TEMPORARY','BASE TABLE') and table_name = '","si_ci_", new_later_interested_Date_integer,"'")) == 0) {
        
        # empty stubs so I can run the query
        dbGetQuery(conn, paste0("create temporary table ", "si_ci_",   new_later_interested_Date_integer, " as select * from ", "si_ci_",   new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_psd_",  new_later_interested_Date_integer, " as select * from ", "si_psd_",  new_interested_Date_integer, " where 1 = 0" ))  
        dbGetQuery(conn, paste0("create temporary table ", "si_cfq_", new_later_interested_Date_integer, " as select * from ", "si_cfq_", new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_date_",  new_later_interested_Date_integer, " as select * from ", "si_date_",  new_interested_Date_integer, " where 1 = 0" ))
        dbGetQuery(conn, paste0("create temporary table ", "si_isq_",  new_later_interested_Date_integer, " as select * from ", "si_isq_",  new_interested_Date_integer, " where 1 = 0" ))
      
      }
      
      sqlstring_all <- gsub("XXXXXX", paste0(new_interested_Date_integer      ),  sqlstring_all)
      sqlstring_all <- gsub("YYYYYY", paste0(new_later_interested_Date_integer),  sqlstring_all)
  
      #  "2012-12-31" == 15705
      if(print_sqlstring == TRUE) print(writeLines(sqlstring_all))
      
      print(paste0("new_interested_Date: ",       as.character(zoo::as.Date(new_interested_Date_integer)), " ", new_interested_Date_integer))
      print(paste0("new_later_interested_Date: ", as.character(zoo::as.Date(new_later_interested_Date_integer)), " ", new_later_interested_Date_integer))
      
      result <- dbGetQuery(conn, sqlstring_all)
      print(left_just(result))
      
      # back to original
      sqlstring_all <- gsub(paste0(new_interested_Date_integer      ), "XXXXXX",  sqlstring_all)
      sqlstring_all <- gsub(paste0(new_later_interested_Date_integer), "YYYYYY",  sqlstring_all)

  }
  
  # update search path
  dbGetQuery(conn, paste0("set search_path to ", osp))
  
  # update time zone
  dbGetQuery(conn, paste0("set time zone '",ost,"'"))

  return(invisible())
  
}

# con <- file(paste0("OUTPUT_fcf_netinc", ".txt"));sink(con);sink(con, type="message")
# 
# massAAIISIProIterMktSheets(conn)
# 
# sink();sink(type="message");close(con)
#  # 
# massAAIISIProIterMktSheets(conn, asOfDate = zoo::as.Date("2012-12-31") + 2, earliest_asOfDate =  zoo::as.Date("2012-12-31") -1, print_sqlstring = TRUE)



library(quantmod)
# getSymbols.PostgreSQL {{{
"getSymbols.PostgreSQL" <- function(Symbols,env,return.class='xts',
                               db.fields=c('date','o','h','l','c','v','a'),
                               field.names = NULL,
                               user=NULL,password=NULL,dbname=NULL,host='localhost',port=5432,options="",search_path=NULL,
                               ...) {
     importDefaults("getSymbols.PostgreSQL")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(!hasArg(verbose)) verbose <- FALSE
     if(!hasArg(auto.assign)) auto.assign <- TRUE

     if(!requireNamespace("DBI", quietly=TRUE))
       stop("package:",dQuote("DBI"),"cannot be loaded.")
     if(!requireNamespace("RPostgreSQL", quietly=TRUE))
       stop("package:",dQuote("RPostgreSQL"),"cannot be loaded.")

        if(is.null(user) || is.null(password) || is.null(dbname)) {
          stop(paste(
              'At least one connection argument (',sQuote('user'),
              sQuote('password'),sQuote('dbname'),
              ") is not set"))
        }
        con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),user=user,password=password,dbname=dbname,host=host,port=port,options=options)

        if(!is.null(search_path)) { 
          dbGetQuery(con, paste0("set search_path to ", search_path) )
        }

        db.Symbols <- DBI::dbListTables(con)
        if(length(Symbols) != sum(tolower(Symbols) %in% tolower(db.Symbols))) {
          missing.db.symbol <- Symbols[!tolower(Symbols) %in% tolower(db.Symbols)]
                warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
                Symbols <- Symbols[tolower(Symbols) %in% tolower(db.Symbols)]
        }
        for(i in seq_along(Symbols)) {
            if(verbose) {
                cat(paste('Loading ',Symbols[[i]],paste(rep('.',10-nchar(Symbols[[i]])),collapse=''),sep=''))
            }
            query <- paste0("SELECT ",paste(db.fields,collapse=',')," FROM \"",
              if(any(Symbols[[i]] == tolower(db.Symbols))) { tolower(Symbols[[i]]) } else { toupper(Symbols[[i]]) }  
            , "\" ORDER BY date")
            rs <- DBI::dbSendQuery(con, query)
            fr <- DBI::fetch(rs, n=-1)
            #fr <- data.frame(fr[,-1],row.names=fr[,1])
            fr <- xts(as.matrix(fr[,-1]),
                      order.by=as.Date(fr[,1],origin='1970-01-01'),
                      src=dbname,updated=Sys.time())
            colnames(fr) <- paste(Symbols[[i]],
                                  c('Open','High','Low','Close','Volume','Adjusted'),
                                  sep='.')
            fr <- convert.time.series(fr=fr,return.class=return.class)
            if(auto.assign)
              assign(Symbols[[i]],fr,env)
            if(verbose) cat('done\n')
        }
        DBI::dbDisconnect(con)
        if(auto.assign)
          return(Symbols)
        return(fr)
}
"getSymbols.PostgreSQL" <- getSymbols.PostgreSQL 
# }}}


convert.time.series <- quantmod:::convert.time.series
library(RPostgreSQL)

# 
# set search_path to sipro_stage
# create table xxxx( -- note psql will automatically make UPPER to lower UPON CREATE
# date date, 
# o numeric(15,2),
# h numeric(15,2),
# l numeric(15,2),
# c numeric(15,2),
# v numeric(15,2),
# a numeric(15,2)
# );
# 
# insert into XXXX(date,o,h,l,c,v,a) 
#   values('1970-01-01',1.0,2.0,3.0,4.0,5.0,6.0);
# 
# postgres=# select * from  XXXX;
#     date    |  o   |  h   |  l   |  c   |  v   |  a
# ------------+------+------+------+------+------+------
#  1970-01-01 | 1.00 | 2.00 | 3.00 | 4.00 | 5.00 | 6.00
# (1 row)


# if UPPERCASE table name
# getSymbols.PostgreSQL("XXXX", env =  environment(), user="postgres", password="postgres", dbname="finance_econ", search_path="sipro_stage")

# if lowercase table name
# getSymbols.PostgreSQL("xxxx", env =  environment(), user="postgres", password="postgres", dbname="finance_econ", search_path="sipro_stage")

# [1] "xxxx"
# > str(xxxx)
# An 'xts' object on 1970-01-01/1970-01-01 containing:
#   Data: num [1, 1:6] 1 2 3 4 5 6
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:6] "xxxx.Open" "xxxx.High" "xxxx.Low" "xxxx.Close" ...
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:  
# List of 2
#  $ src    : chr "finance_econ"
#  $ updated: POSIXct[1:1], format: "2016-06-05 21:46:25"
#   




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


# KEEP - NEVER RAN

# depends upon  DESCRIPTION Imports R.utils
masscopyAAIISIProDBFs <- function(from = "L:/MyVMWareSharedFolder", to_target = "W:/New_Economics/forsight4.322") {
  
  paths <- paste0(from,"/",R.utils::listDirectory(pattern="Professional.*", path=from))
  
  for(path in paths) {
    
    print(paste0("SOURCE: ",path))
    to_target_detail <- paste0(to_target,"/","AAIISIProDBFs","/",getAAIISIProDate(from = path))
    print(paste0("DESTINATION: ",to_target_detail))
    
    # UNCOMMENT TO RUN
    copyAAIISIProDBFs(from = path, to = to_target_detail)
    
  }
  
}

# After, the last Friday of the month
# On Saturday at 10:30 a.m. update SI Pro 
#   AAII employess update the program/data about at 10:30 a.m. on Saturday ( and not a 'Holiday weekend' )
# NEXT, run EXACTLY, the following
# masscopyAAIISIProDBFs(from = "C:/Program Files (x86)/Stock Investor/Professional", to = "W:/New_Economics/forsight4.322")

avu <- function(...) { 
    as.vector(unlist(...)) 
}

# form of c that will drop NA ( because openxlsx assigns an empty cell as NA instead of NULL )
# NOTE: input MUST be a vector(named or not) and 'not a list' )
# combine vectors
cv <- function(...) { 
  mylist <- list(...)
  mynewvector <- unlist(mylist)
  return(mynewvector[!is.na(mynewvector)])
}

# begin 'joins' functions

NottedNADuplicated <- function(data_frame_table = NULL, data_frame_table_column = NULL) {


  
    # local copy
    olduniverses <-  get("olduniverses",envir = parent.frame())

    data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
    print(paste0("Beginning NottedNADuplicated:","data_frame_name_temp:",data_frame_name_temp))
    
    data_frame_temp <- olduniverses[[data_frame_name_temp]]
    
    data_frame_table_column_name_temp <-  ept(get(data_frame_table_column, envir = parent.frame()))
    print(paste0("  Beginning NottedNADuplicated:","data_frame_table_column_name_temp:",data_frame_table_column_name_temp))
    
    data_frame_table_column_items <- avu(data_frame_temp[, data_frame_table_column_name_temp, drop = FALSE])

    !(duplicated(data_frame_table_column_items) | duplicated(data_frame_table_column_items, fromLast = TRUE)) ->  
    data_frame_table_column_items_not_duplicated_switches

    # illegal assignmet ( if the NROWs are different ) 
    # data_frame_temp[, data_frame_table_column_name_temp] <- data_frame_temp[data_frame_table_column_items_not_duplicated_switches, data_frame_table_column_name_temp, drop = FALSE]
    # BUT I CAN SKIP if next I am doing a list assignment (BELOW)anyways
    
    # local copy
    # olduniverses[[data_frame_name_temp]] <- data_frame_temp
    olduniverses[[data_frame_name_temp]] <- data_frame_temp[data_frame_table_column_items_not_duplicated_switches, , drop = FALSE]

    assign(" olduniverses", olduniverses, envir = parent.frame())
 
    print(paste0("  Ending NottedNADuplicated:","data_frame_table_column_name_temp:",data_frame_table_column_name_temp))
    print(paste0("Ending NottedNADuplicated:","data_frame_name_temp:",data_frame_name_temp))
    
    return(invisible(NULL))

}



# end joins functions


# begin attributes functions
# 
# toLower <- function(data_frame_table = NULL, data_frame_table_columns = NULL) {
# 
#     # local copy
#     olduniverses <-  get("olduniverses",envir = parent.frame())
# 
#     data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
#     data_frame_temp <- olduniverses[[data_frame_name_temp]]
#     
#     data_frame_table_column_name_temp <-  ept(get(data_frame_table_column, envir = parent.frame()))
#     
#     data_frame_table_column_items <- avu(data_frame_temp[, data_frame_table_column_name_temp, drop = FALSE])
# 
#     !(duplicated(data_frame_table_column_items) | duplicated(data_frame_table_column_items, fromLast = TRUE)) ->  
#     data_frame_table_column_items_not_duplicated_switches
# 
#     data_frame_temp[, data_frame_table_column_name_temp] <- data_frame_temp[data_frame_table_column_items_not_duplicated_switches, data_frame_table_column_name_temp, drop = FALSE]
# 
#     # local copy
#     olduniverses[[data_frame_name_temp]] <- data_frame_temp
# 
#     assign("olduniverses", olduniverses, envir = parent.frame())
#  
#     return(invisible(NULL))
# 
# }


# currently 'not used' - does work
toLower <- function(data_frame_table = NULL, data_frame_table_columns = NULL) {

    # local copy
    careuniverses <-  get("careuniverses",envir = parent.frame())

    data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
    data_frame_temp <- careuniverses[[data_frame_name_temp]]
    
    data_frame_table_column_names_temp <-  ept(get(data_frame_table_columns, envir = parent.frame()))
    
    for(column_name in data_frame_table_column_names_temp) {

      data_frame_table_column_items <- avu(data_frame_temp[, column_name, drop = FALSE])

      data_frame_table_column_items <- tolower(data_frame_table_column_items)

      data_frame_temp[, column_name] <- data_frame_table_column_items

    }

    # local copy
    careuniverses[[data_frame_name_temp]] <- data_frame_temp

    assign("careuniverses", careuniverses, envir = parent.frame())
 
    return(invisible(NULL))

}



asNumeric <- function(data_frame_table = NULL, data_frame_table_columns = NULL) {

    # local copy
    careuniverses <-  get("careuniverses",envir = parent.frame())

    data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
    data_frame_temp <- careuniverses[[data_frame_name_temp]]
    
    data_frame_table_column_names_temp <-  ept(get(data_frame_table_columns, envir = parent.frame()))
    
    for(column_name in data_frame_table_column_names_temp) {

      data_frame_table_column_items <- avu(data_frame_temp[, column_name, drop = FALSE])

      data_frame_table_column_items <- as.numeric(data_frame_table_column_items)

      data_frame_temp[, column_name] <- data_frame_table_column_items

    }

    # local copy
    careuniverses[[data_frame_name_temp]] <- data_frame_temp

    assign("careuniverses", careuniverses, envir = parent.frame())
 
    return(invisible(NULL))

}



asNumeric1000000 <- function(data_frame_table = NULL, data_frame_table_columns = NULL) {

    # local copy
    careuniverses <-  get("careuniverses",envir = parent.frame())

    data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
    data_frame_temp <- careuniverses[[data_frame_name_temp]]
    
    data_frame_table_column_names_temp <-  ept(get(data_frame_table_columns, envir = parent.frame()))
    
    for(column_name in data_frame_table_column_names_temp) {

      data_frame_table_column_items <- avu(data_frame_temp[, column_name, drop = FALSE])

      data_frame_table_column_items <- as.numeric(data_frame_table_column_items) * 1000000

      data_frame_temp[, column_name] <- data_frame_table_column_items

    }

    # local copy
    careuniverses[[data_frame_name_temp]] <- data_frame_temp

    assign("careuniverses", careuniverses, envir = parent.frame())
 
    return(invisible(NULL))

}



strSub <- function(data_frame_table = NULL, data_frame_table_columns = NULL, start = NULL, end = NULL) {

    # local copy
    careuniverses <-  get("careuniverses",envir = parent.frame())

    data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
    data_frame_temp <- careuniverses[[data_frame_name_temp]]
    
    data_frame_table_column_names_temp <-  ept(get(data_frame_table_columns, envir = parent.frame()))
    
    for(column_name in data_frame_table_column_names_temp) {

      data_frame_table_column_items <- avu(data_frame_temp[, column_name, drop = FALSE])

      data_frame_table_column_items <-  stringr::str_sub(data_frame_table_column_items, start, end)

      data_frame_temp[, column_name] <- data_frame_table_column_items

    }

    # local copy
    careuniverses[[data_frame_name_temp]] <- data_frame_temp

    assign("careuniverses", careuniverses, envir = parent.frame())
 
    return(invisible(NULL))

}

strCatCols <- function(data_frame_table = NULL, data_frame_table_column_1 = NULL, data_frame_table_column_2 = NULL) {
  # finds columns of the 2nd(_1) and 3rd(_2) argument,  puts back data to the 2nd(_1)argument

  # local copy
   careuniverses <-  get("careuniverses",envir = parent.frame())

  data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
  data_frame_temp <- careuniverses[[data_frame_name_temp]]

  # text column names 2nd and 3rd argument
  data_frame_table_column_1_name_temp <- ept(get(data_frame_table_column_1, envir = parent.frame()))
  data_frame_table_column_2_name_temp <- ept(get(data_frame_table_column_2, envir = parent.frame()))

  data_frame_table_column_items <- stringr::str_c(data_frame_table_column_2_name_temp, "__", avu(data_frame_temp[, data_frame_table_column_1_name_temp, drop = FALSE]))

  # put back to the second argumenet
  data_frame_temp[, data_frame_table_column_1_name_temp] <- data_frame_table_column_items

  # local copy
  careuniverses[[data_frame_name_temp]] <- data_frame_temp

  assign("careuniverses", careuniverses, envir = parent.frame())
 
  return(invisible(NULL))

}




numORdateTonum <- function(data_frame_table = NULL, data_frame_table_columns = NULL) {

    # local copy
    careuniverses <-  get("careuniverses",envir = parent.frame())

    data_frame_name_temp <- ept(get(data_frame_table, envir = parent.frame()))
    data_frame_temp <- careuniverses[[data_frame_name_temp]]
    
    data_frame_table_column_names_temp <-  ept(get(data_frame_table_columns, envir = parent.frame()))
    
    for(column_name in data_frame_table_column_names_temp) {

      data_frame_table_column_items <- avu(data_frame_temp[, column_name, drop = FALSE])

      data_frame_table_column_items <- as.Date(data_frame_table_column_items, origin = "1970-01-01", tz = "UTC")
      
      data_frame_table_column_items <- as.numeric(as.POSIXct(data_frame_table_column_items, origin = "1970-01-01", tz='UTC'))/86400

      data_frame_temp[, column_name] <- data_frame_table_column_items

    }

    # local copy
    careuniverses[[data_frame_name_temp]] <- data_frame_temp

    assign("careuniverses", careuniverses, envir = parent.frame())
 
    return(invisible(NULL))

}

# end attributes functions



GetColLitValue <- function(x, envir = parent.frame()) {

  ept(ept(ept(x, envir = envir),envir = envir),envir = envir)

}

# NOTHING IN THE 'R' LANGUAGE will 'apply an INTERVAL' and RETURN the 'ORIGINAL ORDER'
SportsRanking <- function(numb_of_divisions = NA, perdivisions = NULL, instances = NULL) {

  # price_pct_chge_since_13w_winner ( perdivisions is "alldata"  )
  # browser( expr = { (instances[5] == -13.43) && (instances[6] == -12) }) 
  
  # edge case: split (perhaps others also?) drops NA group, I NEED(REQUIRED) to keep it 
  perdivisions[which(is.na(perdivisions))] <- "NOTAPPLICABLE"
  
  split(data.frame(row_id = seq_along(perdivisions),perdivisions,instances), perdivisions)  -> thedata   

  # too SEE a function ( .fun = myfunction) , it MUST be defined OUTSIDE the CURRENT (SportsRanking) function
  plyr::llply(.data = thedata, .fun =  function(x,numb_ranks = numb_ranks) { 
    
      if(is.character(numb_ranks) && !stringr::str_detect(numb_ranks,'^\\d+$')) { 

        # CURRENLY, 'NO IMPLEMENTATION' SMALL '# OF INDUSTRIES' WOULD 'GET TOO OPTMIMISTIC RESULTS'
        # IN GENRAL, JUST USE HARDCODED *2* AND *10* QUANTILE. ( FOR GENERAL USE JUST ONLY USE HARDCODED *10* )
        
        # several USEFUL hardcoded ONES                 # change datatype to numeric
        
        # ** remember: this goes into *quantile where 'higher' is better **

        # In a 'small group': IT hurts HARD to be 2ND or LAST
        if(numb_ranks ==  'NLT10xorNROW') numb_ranks <- max( 10, NROW(x[complete.cases(x),])) # Math Penalty for being is small group ( less competition ) 
        
        if(numb_ranks ==          'NROW') numb_ranks <-          NROW(x[complete.cases(x),]) # Maybe use composites/finals
        
        # NOT USEFUL ( small industry BIAS )
        
        if(numb_ranks ==   'NGT2xorNROW') numb_ranks <- min(  2, NROW(x[complete.cases(x),])) 
        if(numb_ranks ==   'NGT5xorNROW') numb_ranks <- min(  5, NROW(x[complete.cases(x),])) 
        if(numb_ranks ==  'NGT10xorNROW') numb_ranks <- min( 10, NROW(x[complete.cases(x),])) 
        if(numb_ranks ==  'NGT20xorNROW') numb_ranks <- min( 20, NROW(x[complete.cases(x),])) 
        if(numb_ranks == 'NGT100xorNROW') numb_ranks <- min(100, NROW(x[complete.cases(x),])) 

      } 
      if(is.character(numb_ranks) &&  stringr::str_detect(numb_ranks,'^\\d+$')) numb_ranks <- as.numeric(numb_ranks)
    
      SportsRank(x = x$instances, numb_ranks = numb_ranks) 
      
    }, numb_ranks =  numb_of_divisions )  -> sports_ranks

  rlist::list.zip(thedata,sports_ranks) -> zipped 

  plyr::llply(zipped, .fun = function(x) { do.call("cbind", x )   } )  -> stacked

  rlist::list.rbind(stacked) -> rbinded # SANITY CHECK

  plyr::arrange(rbinded, thedata.row_id)[,"sports_ranks"] -> returned

  return(returned)
  
}



hdquantile <- function (x, probs = seq(0, 1, 0.25), se = FALSE, na.rm = FALSE
  , names = TRUE, weights = FALSE, outlook = "optimistic" ) {

  # put in hdquantile ( seems to solve PROBLEMS )

  # solves this edge case c(Inf,INF)
  x[x == -Inf] <- .Machine$double.xmin
  x[x ==  Inf] <- .Machine$double.xmax

  # NOT A PROBLEM, put here for CODE ROBUSTNESS
  x[is.nan(x)] <- NA

  # outlook = "BLAH"
  if( !(outlook %in% c("optimistic","pessimistic","neutral")) ) stop("outlook must be \"optimistic\", \"pessimistic\", or \"neutral\"")

  if(!na.rm && !is.null(x)) {
    if(any(is.na(x))) stop("missing values and NaN's not allowed if 'na.rm' is FALSE" )
  }

  # edge case not handled properly
  is_nrow_is_zero <- FALSE
  if(is.null(x)) is_nrow_is_zero <- TRUE  

  # edge case not handled properly
  if(na.rm) x[!is.na(x)] -> x

  if( length(unique(x)) == 1 ) {  # length(unique(NULL)) == 0

    # optimistic - behaves like stats::quantile
    if( outlook == "optimistic" )   rep( length(probs) -1,length(probs)) -> y

    # pessimistic
    if( outlook == "pessimistic" )  rep( 1,length(probs)) -> y

    if( outlook == "neutral" ) {

      middle_value <- length(probs) %/% 2 + length(probs) %% 2
      rep(middle_value,length(probs)) -> y 

    }

  # format headers
  setNames( y, stringr::str_c(probs * 100,"%")) -> y; return(y)

  }

  Hmisc::hdquantile(x, probs = probs, se = se, na.rm = na.rm , names = names, weights = weights ) -> y

  # edge case : subtle bug
  if( all(is.na(y)) ) { y[is.na(y)] <- NA_real_ ; setNames( y, stringr::str_c(probs * 100,"%")) -> y; return(y) }

  # format headers
  setNames( y, stringr::str_replace(as.numeric(names(y)) * 100, "$", "%" ) ) -> y

  # same as quantile
  if(is_nrow_is_zero)  y[] <- NA_real_

  return(y)

}


# NOT USED: matrixStats IS 30x? FASTER
Count <- function(...) {
  
  if(length(alist(...)) == 0) stop("Error found in Count: Nothing sent to function call.")

  as.vector(unlist(plyr::alply( cbind( ... ) , 1 ,  .fun = function(x) { 
    
    # sum(as.vector(unlist(plyr::llply( x , .fun = function(x) { !is.na(x) } ))))
    # Reputation for speed    
    colSums(cbind(as.vector(unlist(plyr::llply( x , .fun = function(x) { !is.na(x) } )))))
    
  })))
  
}
# Count(c(1,2), c(NA,8), c(16,32))
# [1] 2 3

# NOT USED: matrixStats IS 30x? FASTER
Sum <- function(...) {

  if(length(alist(...)) == 0) stop("Error found in Sum: Nothing sent to function call.")
  
  as.vector(unlist(plyr::alply( cbind( ... ) , 1 ,  .fun = function(x) { 
    
    # sum(x , na.rm = TRUE )
    # Reputation for speed
    colSums(cbind(x), na.rm = TRUE)

  })))
  
}
# Sum(c(1,2), c(NA,8), c(16,32))
# [1] 17 42

# TO FIX [ ]
# > SportsRank(c(NaN,5))
# [1] NA  1 # wrong result returned should be   'NA 100'
# TO FIX: (length(which(!is.na(x) & !is.nan(x))) == 1) && (length(x) > 1 )

SportsRank <- function(x, numb_ranks = 100, outlook = "pessimistic", calc_method = "base_quantile_7") {
  
  if(all(is.null(x))) return(invisible(NULL))

  if(all(is.na(x)))   return(rep(NA,length(x)))

  # ROBUST vec_interval WOULD BE numeric(0)
  if( numb_ranks == 1 ) {
  
    # high number value
    returned <- rep(1,length(x)) 
    return(returned)
  
  }
  
  if(outlook == "optimistic"  ) outlook_quantile <- "pessimistic"

  if(outlook == "pessimistic" ) outlook_quantile <- "optimistic"
  
  # Hmisc (ALWAYS HANDLES DUPLICATES WELL) # DISCOVERED LATE THAT (HANDLE EXTREMES Inf,-Inf THE POOREST POSSIBLE ) 
  if(calc_method == "my_hdquantile") {
    vec_interval <- tail(head(    hdquantile(x = x, probs = seq(0, 1, 1/numb_ranks) , na.rm = TRUE, outlook = outlook_quantile ),-1),-1)
  }   
  
  # REQUIRED TO SWITCH, BECAUSE HDQUANLEY THE_POOREST HANDLES Inf,-Inf
  # stats ( NOT TESTED IN THIS CONTEXT FOR EDGE CASES ) ( BUT USING findInterval(or R changed) DOES NOW PROPERLY HANDLE DUPLICATES ) )
  if(calc_method == "base_quantile_7") {
    vec_interval <- tail(head(stats::quantile(x = x, probs = seq(0, 1, 1/numb_ranks) , na.rm = TRUE                             ),-1),-1)               
  }                          
  
  if( (length(unique(x)) == 1)  && ( outlook == "pessimistic" )  ) {
  
    # high number value
    returned <- rep((length( seq(0, 1, 1/numb_ranks)) -1),length(x))
    return(returned)
  
  }
  
  if( (length(unique(x)) == 1)  && ( outlook == "optimistic" ) ) {
  
    # low number value
    returned <- rep(1                                    ,length(x))
    return(returned)

  }
  
  # is.unsorted: undocumented function in base ( used internally by 'findInterval' )
  # if is.unsorted == TRUE, then findInterval will fail
  # SEE https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16672
  
  if(is.unsorted(vec_interval)) {
  
    vec_interval_old <- vec_interval # for 'later' debugging ( if any )
  
    # SOMEWHAT fix the BUG
    # vec_interval[order(vec_interval)] -> vec_interval
  
    # BETTER FIX(PATCH) - 'NOT SYSTEM' TESTED ( BECAUSE I 'CURRENTLY'/CAN_NOT/TOO_LAZY FIND/REPEAT THE PROBLEM )
    # tested by: vec_interval <- c(1,0,3,4,-1,-2,1,6)
    # *DID TEST* USING DATA FROM: https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16672
    zoo::rollapply(data = vec_interval, width = length(vec_interval), FUN = function(x) { 
      max(x) 
    }, partial = TRUE, align = "right") -> vec_interval
    
    op <- options()
    options(warn = 1)
    
    warning("", immediate. = TRUE, call. = TRUE)
    warning("BEGIN WARNING", immediate. = TRUE, call. = FALSE)
    warning("In function: SportsRank vec_interval found 'is.unsorted(vec_interval)  == TRUE'", immediate. = TRUE, call. = FALSE)
    warning("avoid findInterval error: 'vec' must be sorted non-decreasingly[ and not contain NAs]", immediate. = TRUE, call. = FALSE)
    warning(paste0("calc_method was ",calc_method), immediate. = TRUE, call. = FALSE)
    warning("old vec_interval", immediate. = TRUE, call. = FALSE)
    warning(vec_interval_old[1:min(length(vec_interval_old),5)], immediate. = TRUE, call. = FALSE)
    warning("new vec_interval", immediate. = TRUE, call. = FALSE)
    warning(vec_interval[1:min(length(vec_interval),5)], immediate. = TRUE, call. = FALSE)
    warning("END WARNING", immediate. = TRUE, call. = FALSE)
  
    options(op)
    
  }

  # THE COMMON 99% of the time - 'non edge-case'
  abs(findInterval(x = x, vec = vec_interval ) + 1 - length(seq(0, 1, 1/numb_ranks)))  -> returned

  return(returned)
  
  # KEE HERE FOR HISTORY
  # abs(findInterval(x = x, tail(head(hdquantile(x = x, probs = seq(0, 1, 1/numb_ranks) , na.rm = TRUE, outlook = outlook_final ),-1),-1)) + 1 - length(seq(0, 1, 1/numb_ranks)))

}

# Get 'R Program' Code_Exe_Result/Variable Value ( from within a spreadsheet cell
gpv <- function(x) ept(text = x, envir=parent.frame(7)) # LESS LIBERAL:  get(x = x, envir=parent.frame(7))




Balance <- function(better_direction = NULL, composites_interest = NULL, regex = NULL, max_count = NULL, pct_of_max_count = NULL) {   

  # browser( expr = {  '__alldata__sportsranking6000$' == regex } )
  
  # collect alluniverses variables
  # ls(envir= parent.frame()) # alluniverses VARIABLES = ls(envir= parent.frame(4))

  # collect program vaiables
  # ls(envir= parent.frame(7))

  # get the historical vector
  composites_local_h <- get('exposes__composites_instances_h', envir= parent.frame(7)) # "\"traditionalvalue\"" AND OTHERS
 
  # get the evaluated cells - detect string - cell value -> c(TRUE,FALSE)
  composites_local_h_list_ele_detected <- plyr::llply(composites_local_h, .fun = function(x,y) { 
    stringr::str_detect(string = ept(ept(x)), pattern = y)
  }, y = paste0("^",composites_interest,"$") )
  
  # 'any TRUE' in that cell value
  composites_local_h_list_ele_detected_any <- avu(plyr::llply(composites_local_h_list_ele_detected, .fun = any ))

  # which 'vector of cells' value's cells contains at least one TRUE
  composites_local_h_list_ele_detected_any_indexes <- which(composites_local_h_list_ele_detected_any == TRUE)
  
  # RESUME
  composites_interest_indexes <- composites_local_h_list_ele_detected_any_indexes
  
  # get its locations ( OLD CODE )
  # composites_interest_indexes <- which(paste0("\"",composites_interest,"\"") == composites_local_h)

  # get the composites instances measures
  measures__instances_interest <- get('measures__instances_h', envir= parent.frame(7))[composites_interest_indexes]

  # Browse[2]> data.frame(measures__instances_interest)
  #          measures__instances_interest
  # 1 "diluted_earnings_over_price_ratio"
  # 2    "diluted_sales_over_price_ratio" # not financial
  # 3        "free_cash_over_price_ratio"
  # 4       "book_value_over_price_ratio" # financial
  # 5          "ebitda_over_entval_ratio"
  # 6     "shareholder_yield_pct_diluted"
  
  if(max_count == 'max_count') {
  
    # dynamic: all measures found is total possible from the spreadsheet
    max_count <- as.numeric(length(measures__instances_interest))
  
  } else {
  
    # user specified
    max_count <- as.numeric(max_count)
  }

  pct_of_max_count <- as.numeric(pct_of_max_count) / 100.0
  
  # get the composites instances measures complementary conditionals
  measures__contitionals__businesses_interest <- get('measures__contitionals__businesses_h', envir= parent.frame(7))[composites_interest_indexes]

  # build the cbind statement
  cbinds <- c()
  iter <- 0
  for(mi in measures__instances_interest) { 
    iter <- iter + 1
  
    # browser( expr = { composites_interest == "millennialvalue" } )
    
    # find the exact alluniverses column names  
                                      # (because ORDER matters)
    composite_item_name <- ls(sorted = FALSE, envir = parent.frame())[str_detect(ls(sorted = FALSE, envir = parent.frame()),paste0('^',ept(mi),'.*',regex))]
    if(length(composite_item_name) > 1)  { print('in Balance, multiple column names found using the same regular expression - NEED exactly ONE'); browser() }
    if(length(composite_item_name) == 0) { print('in Balance, zero column names found using the same regular expression     - NEED exactly ONE'); browser() }

    # if no restriction just add
    if( is.na(ept(measures__contitionals__businesses_interest[iter]))) cbinds <-  c(cbinds,composite_item_name)

    # something different - retrictive  
    if(!is.na(ept(measures__contitionals__businesses_interest[iter])))  {

      # enhanced composite_item_name
      mcitem <- ept(measures__contitionals__businesses_interest[iter])
      mcitemnew <- paste0('ifelse(',mcitem,',',composite_item_name,',NA)')

      # append the append the enhanced composite_item_name
      cbinds <- c(cbinds,mcitemnew)

    }
    
    
  }

  # create the sentence
  cbind_statement <- paste0('cbind(',paste0(cbinds, collapse = ', '),')')

  # run it
  ept(cbind_statement, envir = parent.frame()) -> binded
  binded -> binded_tf

  # get rid of FALSE zeros(0) in prep for rowCounts 
  binded_tf[binded_tf == 0] <- .Machine$double.xmax; binded_tf & 1 -> binded_tf;

  as.numeric(matrixStats::rowCounts(binded_tf, na.rm = TRUE)) -> total_count; rm(binded_tf);

  # accumulate
  rowSums(binded, na.rm = TRUE) -> total_sum; rm(binded);
  
  if(better_direction == 'lower')   -1 -> multiply_by 
  if(better_direction == 'higher')   1 -> multiply_by 
  if( (better_direction != 'higher') && (better_direction != 'lower')) stop("Balance NEEDS better_direction")
  
  # scale it : need at least 50% of the measures to 'not be NA'
  # ifelse( ( max_count / total_count <= 2 )  & ( max_count %/% total_count  <= 2 ), multiply_by * max_count / total_count * total_sum, NA ) -> returned

  ifelse( ( max_count / total_count <= (1/pct_of_max_count) )  & ( max_count %/% total_count  <= (1/pct_of_max_count) ), multiply_by * max_count / total_count * total_sum, NA ) -> returned
  
  return(returned)
  
}


# KEEP - UTILITY
# Length of the Count With alluniverses (lcwa)
lcwa <- function(x)  sum(with(get("alluniverses", envir = parent.frame()),{ept(paste0(x,' & 1'))}), na.rm = TRUE)

# be aware of numerics (both zero and non-zero: in context 'what do I want' )

# > c(TRUE,1,55,0, FALSE,NA) & 1   
# [1]  TRUE  TRUE  TRUE FALSE FALSE    NA

# > sum( 1 & c(TRUE,1,55,0, FALSE,NA) , na.rm = TRUE)
# [1] 3

# lcwa('is_inv_is_price_chng_is_eps_well') 
# [1] 668

# OTHER UTILITES
# str(alluniverses, list.len = 999, vec.len = 3 )
# View(t(alluniverses[1:100,]))



Sel <- function( at_what = NULL, called_at_what = NULL,  bests = NULL, univ_field_expr = NULL) {

  univ_field_many <- ls(envir=parent.frame(), sort = FALSE)[stringr::str_detect(ls(envir=parent.frame(), sort = FALSE),univ_field_expr)]

  for(univ_field_one in univ_field_many) {

    univ_field_local <- get(univ_field_one, envir = parent.frame())

    if( at_what == "lower" )     multiply <-  1     # 1 5 10 == 1 2 3
    if( at_what == "higher"  )   multiply <- -1

    for(best in ept(bests)) {

      univ_field_local_rank <- ifelse(  rank( multiply * univ_field_local, ties.method = "random",  na.last = "keep") <= best, 1.0, 0.0) 

      assign(paste0(univ_field_one, '__', called_at_what, best), univ_field_local_rank, envir = parent.frame())

    }

  }

  return(NULL) # will destroy the return variable

}



as.Date.ti <- function(xTi, origin = "1970-01-01", tz = "UTC", offset = 0) {

  base::as.Date(x = tis:::as.POSIXct.ti(
      x = xTi, tz = tz, offset = offset
      , origin = origin
    )  
  , tz = tz, origin = origin)
}



lastWeekDayDateOfMonth_nv <- function(x, origin = "1970-01-01", tz = "UTC") {

  # uses: as.Date.ti
                                      
  fdtis <- tis::firstDayOf(tis::as.ti(base::as.Date(x = x, origin = origin, tz = tz),"monthly"))

                                                       # character date; silently ignored
  ldtis <- tis::lastDayOf(tis::as.ti(base::as.Date(x = x, origin = origin, tz = tz),"monthly"))

  numeric_days_of_this_month <- seq( as.numeric(as.Date.ti(fdtis)), as.numeric(as.Date.ti(ldtis)) )

  Date_last_three_days_of_this_month <-  tail(base::as.Date(x =  numeric_days_of_this_month, origin = origin, tz = tz),3)

  tail(Date_last_three_days_of_this_month[ !(base::weekdays(Date_last_three_days_of_this_month) %in% c("Saturday","Sunday")) ],1)

}


lastWeekDayDateOfMonth <- function(x, origin = "1970-01-01", tz = "UTC") {

  # uses: lastWeekDayDateOfMonth_nv

  lastWeekDayDateOfMonth_v <- sapply(x,lastWeekDayDateOfMonth_nv, origin = origin, tz = tz, USE.NAMES = FALSE)

  last_weekday_of_month <- lastWeekDayDateOfMonth_v

  # because dates are ?unlist? ed
  base::as.Date(last_weekday_of_month, origin = origin, tz = tz)
}


is.lastWeekDayDateOfMonth <- function(x) if( lastWeekDayDateOfMonth(x) == x ) { TRUE } else { FALSE }
# > is.lastWeekDayDateOfMonth(zoo::as.Date("2016-05-31"))
# [1] TRUE


# replace and -Inf,Inf found in a data.frame with NAs
cleanLargeValues <- function(x) {
 
  if(!any(grepl("package:data.table",search()))) {
    suppressMessages(require(data.table))
  }

  DT <- data.table(x)

  # set. This avoids some internal copying.
  for (j in 1:ncol(DT)) set(DT, which(is.infinite(DT[[j]])), j, NA) 

  data.frame(DT) -> returned
  detach(package:data.table) # prevent namespace pollution

  return(returned)

}



cleanNaNValues <- function(x) {
 
  if(!any(grepl("package:data.table",search()))) {
    suppressMessages(require(data.table))
  }

  DT <- data.table(x)

  # set. This avoids some internal copying.
  for (j in 1:ncol(DT)) set(DT, which(is.nan(DT[[j]])), j, NA) 

  data.frame(DT) -> returned
  detach(package:data.table) # prevent namespace pollution

  return(returned)

}



# CLEANS THE COLUMNS THAT ARE THE 'DIFFERNCE BETWEEN THE 'BEFORE' COLUMNS AND THE 'AFTER' COLUMNS

cleanBADValuesSpecColumns <- function(df, before_columns, after_columns) {

  df_other_b <- df[,which( ! after_columns %in% before_columns ),drop = FALSE]

  iter <- 0
  for(i in colnames(df_other_b)) {

     iter <- iter + 1
     print("BEGIN clean values")
     print(i)

     # Inf,-Inf,NaN
     if(!is.numeric(df_other_b[[iter]])) { print(paste0("SKIPPING Not Numeric: ", i)) ; next }

     no_large        <-  cleanLargeValues(df_other_b[,iter,drop = FALSE])
     no_large_no_nan <-  cleanNaNValues(    no_large[,    ,drop = FALSE])

     df[,i] <- no_large_no_nan[,,drop = FALSE]
     
     print(colnames(df_other_b)[iter])
     print("END clean values")


  }

  return(df)

}




data_loading_with_Excel_4 <- function(
    predictclasses = "train"
  , traintest_index # required INPUT
  , spreadsheet = "W:/New_Economics/forsight4.322/meta48calc71.xlsx"
  , timends = 16717
  , perspectives = "general+other"
  , repository_location = "W:/New_Economics/forsight4.322"
  , other_universes = list()
  , return_at_end_ofattributes = FALSE
  ) {
  
  # begin THIS function
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  # begin sheet perspectives
  
  spreadsheet_tab_perspectives <- openxlsx::readWorkbook(xlsxFile = spreadsheet, sheet = "perspectives")
  
  bookmarkhere <- 1
  

  for(rowindex in seq_along(row.names(spreadsheet_tab_perspectives))) {
  
    record0 <-  avu(spreadsheet_tab_perspectives[rowindex,,drop = FALSE])
    names(record0) <- colnames(spreadsheet_tab_perspectives)
    
    # each column becomes a local variable
    for(nameindex in names(record0)) {
      assign(nameindex, record0[nameindex])
    }

    # if this record is not eligable, cleanup up the envoronment, then go to the next loop
    if(!any(timends       %in% ept(timeends_instances_ranges))) { rm(list = names(record0)); next }
    if(!any(perspectives  %in% ept(perspectives__plans)))       { rm(list = names(record0)); next }

    
    # accumulate ( if I need to ) 
    if(!exists("perspectives__plans_clusterexpressions")) perspectives__plans_clusterexpressions <- c() # NULL
                                                           # combine old with the new
    # PERMANENT                              # cv removes NA # unique dies on NULL  # c ONE + NULL -> ONE   # setdiff  - note: setdiff : incl - excl AND removes duplicates # openxlsx return NA on empty cell
    perspectives__plans_clusterexpressions <- cv(unique(c(perspectives__plans_clusterexpressions,setdiff(ept(perspectives__plans_clusterexpressions_includes),ept(perspectives__plans_clusterexpressions_excludes)))))
    
    bookmarkhere <- 1
    
    # clean up that last record
    suppressWarnings(rm(list=names(record0)))
    
  } #  for(rowindex in seq_along(row.names(spreadsheet_tab_perspectives)))

  
  # end sheet perspectives
  
  # begin sheet joins
  
  spreadsheet_tab_joins <- openxlsx::readWorkbook(xlsxFile = spreadsheet, sheet = "joins")
  

 
  for(rowindex in seq_along(row.names(spreadsheet_tab_joins))) {
  
    record0 <-  avu(spreadsheet_tab_joins[rowindex,,drop = FALSE])
    names(record0) <- colnames(spreadsheet_tab_joins)
    
    # each column becomes a local variable
    for(nameindex in names(record0)) {
      assign(nameindex, record0[nameindex])
    }

    
    # if this record is not able, cleanup up the envoronment, then go to the next loop
    if(!any(timends                                  %in% ept(timeends_instances_ranges)))   { rm(list = names(record0)); next }
    if(!any(perspectives__plans_clusterexpressions   %in% ept(fromsheets__instances_plans))) { rm(list = names(record0)); next }
    if(!any(predictclasses                           %in% ept(timeends__predictiteclasses))) { rm(list = names(record0)); next }
    
    
    # if I do not have on make one 
    # PERMANENT
    if(!exists("olduniverses")) olduniverses <- list() # list()
 
    if(!exists("repository_providers_timends_symbols_already_loaded")) repository_providers_timends_symbols_already_loaded <- list()
    
    list(
        repository_location_el = repository_location 
      , providers__instances_el = ept(providers__instances)
      , timends_el =  timends
      , fromsheets__symbols_el = ept(fromsheets__symbols)
    ) -> list_item
    
    # check to see if already in the 'loaded list'
    if(length(rlist::list.findi( repository_providers_timends_symbols_already_loaded, repository_location_el  == repository_location  && 
                                                                       providers__instances_el == ept(providers__instances) &&
                                                                       timends_el              == timends &&
                                                                       fromsheets__symbols_el == ept(fromsheets__symbols)
    )) == 0) {  # returns a vector of 'index_numbers' # a vector length of zero elemeents means 'no indexes found'
    
      # load load the data from external source
    

      
      # load information table
      if(ept(providers__instances_fmt) == "dbf") {
        
        print(paste0("Investigating external data of date: ", as.character(zoo::as.Date(timends)) ))
        
        path_file_name <- paste0(repository_location,"/", ept(providers__instances),"/", timends,"/",ept(fromsheets__symbols),".dbf")
        print(paste0("Begin attempting to load external data: ",  path_file_name))

        # data_frame_loaded <- suppressWarnings(suppressMessages(foreign::read.dbf(file = paste0(repository_location,"/", ept(providers__instances),"/", timends,"/",ept(fromsheets__symbols),".dbf"), as.is = TRUE)))
        
        data_frame_loaded <- suppressWarnings(suppressMessages(foreign::read.dbf(file = path_file_name, as.is = TRUE)))
        
        print(paste0("End attempting to load external data: ", path_file_name ))
              
      } # if(ept(providers__instances_fmt) == "dbf")
      
      # browser(expr = { !is.na(ept(fromsheets__symbols__another))  && predictclasses == "train" } )
      
      # regular load
      
      olduniverses[[ept(fromsheets__symbols)]] <- data_frame_loaded
      rm("data_frame_loaded")
           
      # AAII SIPro specific # remove bad columns
      # note: EXPECTING uppercase column names
      keep_columns_switches <- !grepl("(^X\\.??$|^X\\.\\d+?$|^X_NullFlags$)",names(olduniverses[[ept(fromsheets__symbols)]]))
      olduniverses[[ept(fromsheets__symbols)]] <- olduniverses[[ept(fromsheets__symbols)]][,keep_columns_switches ,drop = FALSE]
      rm("keep_columns_switches")
      
      # lower case column names
      
      data_frame_temp <- olduniverses[[ept(fromsheets__symbols)]]
      colnames(data_frame_temp) <- tolower(colnames(data_frame_temp))
      olduniverses[[ept(fromsheets__symbols)]] <- data_frame_temp
      rm("data_frame_temp")
      
      # add it to the list of previously loaded
      new_element_index <- length(repository_providers_timends_symbols_already_loaded) + 1
      
      repository_providers_timends_symbols_already_loaded[[new_element_index]] <- list_item
      rm("new_element_index")
    
    } # do not load load the date from an external source # do not add it to the list ( already on the list )


    
    # if copying, make a copy and give it a new name
    
    if( !is.na(fromsheets__symbols__another) ) {
      
      olduniverses[[ept(fromsheets__symbols__another)]] <- olduniverses[[ept(fromsheets__symbols)]]
         
      #       # AAII SIPro specific # remove bad columns
      #       # note: EXPECTING uppercase column names
      #       keep_columns_switches <- !grepl("(^X\\.??$|^X\\.\\d+?$|^X_NullFlags$)",names(olduniverses[[ept(fromsheets__symbols__another)]]))
      #       olduniverses[[ept(fromsheets__symbols__another)]] <- olduniverses[[ept(fromsheets__symbols__another)]][,keep_columns_switches ,drop = FALSE]
      #       rm("keep_columns_switches")
      #       
      #       # lower case column names
      #       
      #       data_frame_temp <- olduniverses[[ept(fromsheets__symbols__another)]]
      #       colnames(data_frame_temp) <- tolower(colnames(data_frame_temp))
      #       olduniverses[[ept(fromsheets__symbols__another)]] <- data_frame_temp
      #       rm("data_frame_temp")
      
    }
   

    bookmarkhere <- 1
    
    if( !is.na(ept(fromsheets__uniqueexpressions)) & # NA - empty cell
      stringr::str_detect(ept(fromsheets__uniqueexpressions), "<.*>") ) {
      
      # run the function
      fromsheets__uniqueexpressions_new <-stringr::str_replace_all(stringr::str_replace_all(ept(fromsheets__uniqueexpressions),"<",""),">","")
      
      ept(fromsheets__uniqueexpressions_new)
      rm("fromsheets__uniqueexpressions_new")
    }
       

    
    # paste0(repository_location,"/", providers__instances,"/", timends)
    
    # clean up that last record
    suppressWarnings(rm(list=names(record0)))
    
    bookmarkhere <- 1
    
  } # for(rowindex in seq_along(row.names(spreadsheet_tab_joins)))

  bookmarkhere <- 1
  
  # end sheet joins
  
  # begin sheet attributes
  
  spreadsheet_tab_attributes <- openxlsx::readWorkbook(xlsxFile = spreadsheet, sheet = "attributes")
  
  for(rowindex in seq_along(row.names(spreadsheet_tab_attributes))) {
  
    record0 <-  avu(spreadsheet_tab_attributes[rowindex,,drop = FALSE])
    names(record0) <- colnames(spreadsheet_tab_attributes)
    
    # each column becomes a local variable
    for(nameindex in names(record0)) {
      assign(nameindex, record0[nameindex])
    }

    # if this record is not able, cleanup up the envoronment, then go to the next loop
    if(!any(timends                                  %in% ept(timeends_instances_ranges)))   { rm(list = names(record0)); next }
    if(!any(perspectives__plans_clusterexpressions   %in% ept(fromsheets__instances_plans))) { rm(list = names(record0)); next }
    if(!any(predictclasses                           %in% ept(timeends__predictiteclasses))) { rm(list = names(record0)); next }

    # assign(reduce) to columns that I car about
    
    if(!exists("careuniverses")) careuniverses <- list() # olduniverses
    
#     data_frame_name_temp <- ept(fromsheets__symbols)
#     data_frame_temp <- olduniverses[[data_frame_name_temp]]
#     data_frame_table_column_names_temp_new <- ept(fromattributes__clusterexpressions)
# 
#     if( !is.null( careuniverses[[data_frame_name_temp]] ) ) { 
#       
#       # data_frame_table_column_names_temp_newer <- ept(midattributes__clusterexpressions)
#       # olduniverses[[data_frame_name_temp]][,data_frame_table_column_names_temp_new,drop = FALSE]
#       
#       careuniverses[[data_frame_name_temp]] <- cbind( careuniverses[[data_frame_name_temp]], olduniverses[[data_frame_name_temp]][,data_frame_table_column_names_temp_new,drop = FALSE] ) 
#       
#       
#     } else { # first time 
#       careuniverses[[data_frame_name_temp]] <- olduniverses[[data_frame_name_temp]][,data_frame_table_column_names_temp_new,drop = FALSE]
#     }
    
    print(ept(fromsheets__symbols))
    print(ept(fromattributes__clusterexpressions))
    print(ept(midattributes__clusterexpressions))
    print("")
    
    # browser( expr = { ept(fromsheets__symbols) == "si_mgdsc2"  && predictclasses == "train" } )
    
    data_frame_name_temp <- ept(fromsheets__symbols)
    
    data_frame_table_column_names_temp_new         <- ept(fromattributes__clusterexpressions)
    data_frame_table_column_names_temp_new_indexes <- match( data_frame_table_column_names_temp_new, colnames(olduniverses[[data_frame_name_temp]]) )
    
    data_frame_temp_new <- olduniverses[[data_frame_name_temp]][,data_frame_table_column_names_temp_new_indexes,drop = FALSE]
    colnames(data_frame_temp_new) <- ept(midattributes__clusterexpressions)
    
    
    if( !is.null( careuniverses[[data_frame_name_temp]] ) ) { 
      
      careuniverses[[data_frame_name_temp]] <- cbind( careuniverses[[data_frame_name_temp]],data_frame_temp_new)
    
    } else { # first time
    
      careuniverses[[data_frame_name_temp]] <- data_frame_temp_new
    }
    
    # change the datatype, subset it, lowercase it. or 'date to numeric(UNIX Epoch)'
    
    if( !is.na(ept(midattributes__expressions)) & # NA - empty cell
       stringr::str_detect(ept(midattributes__expressions), "<.*>") ) {
      
       # run the function
       midattributes__expression_new <-stringr::str_replace_all(stringr::str_replace_all(ept(midattributes__expressions),"<",""),">","")
      
       ept(midattributes__expression_new)
       rm("midattributes__expression_new")
    
    }
    
    bookmarkhere <- 1
    
    
    # clean up that last record
    suppressWarnings(rm(list=names(record0)))
    
    bookmarkhere <- 1
    
  } # for(rowindex in seq_along(row.names(spreadsheet_tab_attributes)))

  
  # end sheet attributes

  bookmarkhere <- 1
  
  # return to join sheets - do actual joins
  
  for(rowindex in seq_along(row.names(spreadsheet_tab_joins))) {
  
    record0 <-  avu(spreadsheet_tab_joins[rowindex,,drop = FALSE])
    names(record0) <- colnames(spreadsheet_tab_joins)
    
    # each column becomes a local variable
    for(nameindex in names(record0)) {
      assign(nameindex, record0[nameindex])
    }

    # if this record is not able, cleanup up the envoronment, then go to the next loop
    if(!any(timends                                  %in% ept(timeends_instances_ranges)))   { rm(list = names(record0)); next }
    if(!any(perspectives__plans_clusterexpressions   %in% ept(fromsheets__instances_plans))) { rm(list = names(record0)); next }
    if(!any(predictclasses                           %in% ept(timeends__predictiteclasses))) { rm(list = names(record0)); next }
    

    
    # not part of the join flow
    if(ept(fromsheets_currentjoinsymbol) == "NONE" && ept(fromsheets_bulkjoinsymbol) == "NONE" ) {
      next
    }
    
    # create a bulk on the left side - symbols
    # start with on left side
    if(is.na(ept(fromsheets__symbols__another)) &&  ept(fromsheets_currentjoinsymbol) == "INITIATE" && ept(fromsheets_bulkjoinsymbol) == "INITIATE" ) {
      alluniverses <- careuniverses[[ept(fromsheets__symbols)]]
      next
    } 
    
    # create a bulk on the left side - symbols__another
    if(!is.na(ept(fromsheets__symbols__another)) && ept(fromsheets_currentjoinsymbol) == "INITIATE" && ept(fromsheets_bulkjoinsymbol) == "INITIATE" ) {
        alluniverses <- careuniverses[[ept(fromsheets__symbols__another)]]
    }
    
    # has to be "INITIATE" somewhere - skip until I find it
    if(!exists("alluniverses")) next
    
    # left outer join - right side
    sides <- ept(fromsheets_currentjoinsymbol)
    
    # left outer join - left side
    names(sides) <- ept(fromsheets_bulkjoinsymbol)
    
    # browser( expr = { ept(fromsheets__symbols) == "si_exchg"  && predictclasses == "train" } )
    
    # bulk join by right side - fromsheets__symbols
    if(is.na(ept(fromsheets__symbols__another))) {
      
      # browser( expr = { ept(fromsheets__symbols) == "si_isq" } )
      
      alluniverses <- dplyr::left_join(alluniverses ,careuniverses[[ept(fromsheets__symbols)]], by =sides)
    }
    
    # bulk join by right side - fromsheets__symbols__another
    if(!is.na(ept(fromsheets__symbols__another))) {
      alluniverses <- dplyr::left_join(alluniverses ,careuniverses[[ept(fromsheets__symbols__another)]], by =sides)
    }


    # clean up that last record
    suppressWarnings(rm(list=names(record0)))

  } # for(rowindex in seq_along(row.names(spreadsheet_tab_joins)))
  
  # end of return to join sheets - do actual joins
  
  # begin - return to attributes - give column names - user friendly names

  # need the provider name for later from the excel sheet
  # program input variable
  # CREATE A NEW COLUMN
  cbind(provider = NA, alluniverses, stringsAsFactors = FALSE) -> alluniverses
  
  for(rowindex in seq_along(row.names(spreadsheet_tab_attributes))) {
    
    record0 <-  avu(spreadsheet_tab_attributes[rowindex,,drop = FALSE])
    names(record0) <- colnames(spreadsheet_tab_attributes)
    
    # each column becomes a local variable
    for(nameindex in names(record0)) {
      assign(nameindex, record0[nameindex])
    }

    # if this record is not able, cleanup up the envoronment, then go to the next loop
    if(!any(timends                                  %in% ept(timeends_instances_ranges)))   { rm(list = names(record0)); next }
    if(!any(perspectives__plans_clusterexpressions   %in% ept(fromsheets__instances_plans))) { rm(list = names(record0)); next }
    if(!any(predictclasses                           %in% ept(timeends__predictiteclasses))) { rm(list = names(record0)); next }

    # browser( expr = { ept(midattributes__symbols) == "epscon_q"  && predictclasses == "train" } )
    
    # old column names
    data_frame_table_column_names_temp_new         <- ept(midattributes__clusterexpressions)

    # location of old column names
    data_frame_table_column_names_temp_new_indexes <- match(data_frame_table_column_names_temp_new, colnames(alluniverses) )

    # found old column name
    if(all(!is.na( data_frame_table_column_names_temp_new_indexes))) {
    
      # rename old column names to new user frienldy names
      colnames(alluniverses)[data_frame_table_column_names_temp_new_indexes] <- ept(toattributes__clusterexpressions)

    }
    # if(!is.na( data_frame_table_column_names_temp_new_indexes)) { } # do nothing
    
    # before 'that last record' memory has been erased
    # need the provider name for later from the excel sheet
                       # program input variable
    # UPDATE
    alluniverses[rowindex, "provider"] <- ept(providers_instances)
    
    
    
    # clean up that last record
    suppressWarnings(rm(list=names(record0)))
    
    bookmarkhere <- 1
    
  } # for(rowindex in seq_along(row.names(spreadsheet_tab_attributes)))

  # end  - return to attributes - give column names - user friendly names

  # need this new column to unquely id among future/past dataa
  cbind(        
    predictclasses = predictclasses,                                    # program input variable
    timends = timends,                                                  # program input variable
    timends_push_to_eom = as.numeric(zoo::as.Date(zoo::as.yearmon(zoo::as.Date(timends), origin = "1970-01-01", tz = "UTC"), frac = 1, origin = "1970-01-01", tz = "UTC")),
    provider_global_internal_id_timepoint_exact = with(alluniverses, {  # from the excel sheet(already in alluniverses)
      paste0(provider_global_internal_id,'__',timends) 
      } 
  ),     
    alluniverses, stringsAsFactors = FALSE) -> alluniverses
  
  # PATCH
  # if program parameter traintest_index does not exist, then add it (need for a composite SQL join later)
  within( alluniverses, { if(!exists("traintest_index")) traintest_index <- NA } ) -> alluniverses

  bookmarkhere <- 1
  # View(data.frame(colnames(alluniverses)))
  
  # combine (prefix current data with program input data)
  if(length(other_universes) > 0) { plyr::rbind.fill(  append(other_universes,list(alluniverses))  ) -> alluniverses }
  
  # PATCH
  # any NAs form the 'current data' ( and should be NOT the 'program input data' 
  # convert to the 'program sent parameter' traintest_index ( need for a composite SQL join later) )
  traintest_index_nbr <- traintest_index
  within( alluniverses, { ifelse(!is.na(traintest_index),traintest_index, traintest_index_nbr) -> traintest_index   } ) -> alluniverses
  rm("traintest_index_nbr")
  
  # could have: mlr::capLargeValues as businessly/techically appropriate
  #
  # DO REMOVE Inf,-Inf HERE ***
  # remove any Inf,-Inf #, these can reak havok on my SportsRanks
  #
  cleanLargeValues(alluniverses) -> alluniverses
  cleanNaNValues(alluniverses)   -> alluniverses
  
  # return only attributes part
  if(return_at_end_ofattributes == TRUE) return (alluniverses) 
  

  # begin sheet measures
  
  
  spreadsheet_tab_measures <- openxlsx::readWorkbook(xlsxFile = spreadsheet, sheet = "measures")
  
  # programatically added column : needed for later partitioning on 'all or none'
  # COULD? be move LATER in the program BEFORE partitioning begins?
  cbind(NONE = "NONE", alluniverses, stringsAsFactors = FALSE) -> alluniverses
  
  for(rowindex in seq_along(row.names(spreadsheet_tab_measures))) {
  
    record0 <-  avu(spreadsheet_tab_measures[rowindex,,drop = FALSE])
    names(record0) <- colnames(spreadsheet_tab_measures)
    
    # each column becomes a local variable
    for(nameindex in names(record0)) {
      assign(nameindex, record0[nameindex])
    }

    print(paste0("Beginning measures__instances:", ept(measures__instances), collapse = "__" ))
    
    
    # if this record is not able, cleanup up the envoronment, then go to the next loop
    if(!any(timends                                  %in% ept(timeends_instances_ranges)))   { print(paste0("    Skipping(timeends) measures__instances:", ept(measures__instances), collapse = "__" )) ; rm(list = names(record0)); next }
    if(!any(perspectives__plans_clusterexpressions   %in% ept(measures__instances_plans)))   { print(paste0("    Skipping(perspectives__plans_clusterexpressions) measures__instances:", ept(measures__instances), collapse = "__" )) ; rm(list = names(record0)); next }
    if(!any(predictclasses                           %in% ept(timeends__predictiteclasses))) { print(paste0("    Skipping(predictclasses) measures__instances:", ept(measures__instances), collapse = "__" )) ; rm(list = names(record0)); next }

    # if this record survived the filteres above 
    # then just add it to history of spreadsheet column names with history values
    for(nameindex in names(record0)) {

      # if the 'history' vector does does not exist, create it, then assign its first value
      if(!exists(paste0(nameindex,'_h')))  { 
        assign(paste0(nameindex,'_h'), get(nameindex))  # will also assign the element name
      }  
      else {  # if the 'history' vector already exists, then append to it.
        ept( paste0( nameindex, '_h[', length(get(paste0(nameindex,'_h')))+1, '] <- ', nameindex ) ) 
         # give it a acolumn element name # not '+1' because I added it above
        ept(paste0('names(',nameindex, '_h)[', length(get(paste0(nameindex,'_h')))+0, '] <- ','"',nameindex,'"' ))
      }
    }
    
    
    ## EXTERNAL(OTHER) DATA 'COULD' BE ADDED HERE ##
    ## EXTERNAL(OTHER) DATA 'COULD' BE ADDED HERE ## ( PLACE 1 OF 2 ) ## LEFT_OFF ##
    
    # browser()
    
                 ## EXTERNAL DATA LOAD ##
    
    # YES 1. - (RETURN FROM PROGRAM HERE WHICH - ALL UNIVERSES DATA)
    # XOR ( GIVE PROGRAMMING OPTIONS )
    # YES 2. - alluniverses - PLYR::RBIND.FILL - A/MANY PREVIOUS 'ALL UNIVERSE HERE' 
    # ATTRIBUTES SHEET WAS JUST COMPLETED ( BEGINNING 1ST measure 'is_united_states' BELOW )
    
    # str(alluniverses, list.len = 999, vec.len = 1)
    # $ bus_sector_nm                     : chr  "Financial" ...
    # $ fin_exchange_nm                   : chr  "Nasdaq" ...
    # $ bus_industry_nm                   : chr  "Regional Banks" ...
    #
    
    #### CURRENLTLY - NO IMPLEMENTATION
    #### if( is.na(ept(measures__toexpresssions__withwhat))  ) {
    #### # regular within_assign_versereassign
    
    # browser( expr = { "pct_prices_return" == ept(measures__instances)  && predictclasses == "test" } )
    
    # browser( expr = { "finalscore_alldata_www_mashup_isbest" == ept(measures__instances)  && predictclasses == "train" } )
    
    # NEW MEASURE - direct run - no angle bars
    if(!stringr::str_detect(measures__toexpresssions, "<[',A-Za-z,_]+?>")) {
      
      u_before_columns <- colnames(alluniverses)
      
      within(alluniverses, {   
        # browser( expr = { "finalscore_alldata_www_mashup_isbest" == ept(measures__instances)  && predictclasses == "train" } )
        # browser( expr = { "company_size_level_name" == ept(measures__instances)  } ) # NOT STOPPING? WHY???
        # browser()
        assign(ept(measures__instances),ept(ept(measures__toexpresssions))) 
      }) -> alluniverses
      
      u_after_columns  <- colnames(alluniverses)
      alluniverses     <- cleanBADValuesSpecColumns(df = alluniverses,  before_columns = u_before_columns, after_columns = u_after_columns)
      rm("u_before_columns","u_after_columns")
      
    }
    
    # browser( expr = { "finalscore_alldata_www_mashup_isbest" == ept(measures__instances)  && predictclasses == "train" } )
    
    # browser( expr = { "pct_prices_return" == ept(measures__instances)  && predictclasses == "test" } )
    
    ## LEFT OFF ## LEFT OFF ## 
    
#     NEED TO GET THE COMPOSITES REBALANCE RESULT
# 
#     NEED 3 GREEN LINES ( REPLACE THE OLD ONE LINE )
#     __alldata__  __bus_sector_nm___  __bus_industry_nm___  
# 
#     # REPLACE THE LIGHT GREEN 
#     within assign ept(ept # USAGE 'SOME FORM' 
# 
#     # WITHING A FUNCTION SO THAT I CAN DEBUG IT # OR # NOTE: ls(envir=parent.frame(3))@'within assign' # INSIDE TEXT
#     #  TEST BY SAFELY RETURNING THE FIRST RECONIZABLE OBJECT ('USER OBJECT')
#     #  ls(envir=parent.frame(3))[1] # result will always BE A CHARACTER [1] " olduniverses"
#     #     use get('alluniverses', envir=parent.frame(3)) 
#  
#     # power ( e.g. if 5 total measures are possible ) 
#     # if a company has 3(majority) then power is  5/3 # 5 WILL BE AN 'EARLY DETERMINED(BUT STILL DYNMICALLY) DETERMINED CONSTANT
#     # if a company has 4           then power is  5/4
#     # if a company has 2(minority) then power is   NA
#     # score = SUM of ( measures_intances __ ^.*__alldata__sportsranking$ ) * power ... then SEND out to programe to THEN reduce(redundant) by technicals THEN quantiles
# 
#     CompositesMajorityRebalScoring <-function(
# 
#       , composites                 =  'exposes__composites_instances_cumvector'      # cumulative HISTORY buffer
#       , composites_value           =  'traditionalvalue'
#       , measures                   =  'measures__instances_cumvector'                # cumulative HISTORY buffer
#       , universe                   =  'alluniverses' # TRY? TO GET IT DYMNIACLLAY
#       , measuresfurther_expression =  '^.*__alldata__sportsranking$'
#       , contitionals               =  'measures__contitionals_businesses_cumvector'  # cumulative HISTORY buffer
# 
#     ){  }
    
    # NEW MEASURE - IN-direct run - PROCESS - angle bars
    if(stringr::str_detect(measures__toexpresssions, "<[',A-Za-z,_]+?>")) {
    }
    
    #### }
    
#     # CURRENLTLY - NO IMPLEMENTATION ( CODE IN HERE ) DOES WORK
#     if(!is.na(ept(measures__toexpresssions__withwhat))  ) {
#     # DIFFERENT FROM regular within_assign_versereassign
# 
#       if(ept(measures__toexpresssions__withwhat) == "SELFASSIGNS") {
#       # DO JUST within_versereassign ( within CONTENTS must/should contain assignment themselves )
# 
#         bookmarkhere <- 1
#         
#         names_alluniverses_before_selfassigns <- names(alluniverses)
#         
#         # measures__toexpresssions2 <- "\"{Big <- price_pct_change_well + 1000;bb <- 3}\""
#         # within(alluniverses, { ept(ept(measures__toexpresssions2))}) -> alluniverses
#         
#         within(alluniverses, {     
#           ept(ept(measures__toexpresssions))
#         }) -> alluniverses
#         
#         # BUGGY ( AT LEAST IN THE DEBUGGER ) # 'SOMETIMES' will SILENTLY FAIL so run it *TWICE*
#         
#         within(alluniverses, {     
#           ept(ept(measures__toexpresssions))
#         }) -> alluniverses
#         
#         # ( CODE IN HERE ) DOES WORK
#         
#         names_alluniverses_afteralluniverses <- names(alluniverses)
#         
#         new_names_alluniverses_fromalluniverses <- setdiff(names_alluniverses_afteralluniverses, names_alluniverses_before_selfassigns)
#         rm("names_alluniverses_before_selfassigns")
#         rm("names_alluniverses_afteralluniverses")
#         
#         # ON FUTURE CELL USE
#         # USE if(ept(measures__toexpresssions__withwhat) == "SELFASSIGNS")  AND THEN new_names_alluniverses_fromalluniverses on FUTURE 
#         # TO DO PROCESSING ( CURRENLY NO IMPLEMENTATION )
#         
#         bookmarkhere <- 1
# 
#       }
#       
#     }
    
    # notNA - filled cell # HAS a condition  
    if(!is.na(ept(measures__contitionals__technicals))  ) {
    
      # CONDITION - direct run - no angle bars
      if(!stringr::str_detect(measures__contitionals__technicals, "<[',A-Za-z,_]+?>")) {
        
        within(alluniverses, {     
           assign(ept(measures__instances),ept(ept(measures__contitionals__technicals)))  
        }) -> alluniverses
        
      }
      # CONDITION - IN-direct run - angle bars
      # *** DANGER ( SHOULD ) MAKE THIS REGULAR EXPRESSON CHANGE EVERYWHERE ( [ ] DONE )
      if( stringr::str_detect(measures__contitionals__technicals, "<[',A-Za-z,_]+?>")) {
      
        # *** DANGER ( SHOULD ) MAKE THIS REGULAR EXPRESSON CHANGE EVERYWHERE ( [ ] DONE )
        # do not replace '<=' and do not replace '>='
        measures__contitionals__technicals_new <-stringr::str_replace_all(stringr::str_replace_all(ept(measures__contitionals__technicals),"<(?!=)",""),">(?!=)","")
      
        within(alluniverses, {     
          assign(ept(measures__instances),ept(measures__contitionals__technicals_new))  
        }) -> alluniverses
        
        rm("measures__contitionals__technicals_new")
        
      }
      
    }
    # NA - empty cell # does not have a condition - ASIS
    if( is.na(ept(measures__contitionals__technicals))  ) {
    }
    
    ## EXTERNAL(OTHER) DATA 'COULD' BE ADDED HERE ##
    ## EXTERNAL(OTHER) DATA 'COULD' BE ADDED HERE ## ( PLACE 2 OF 2 )
    # NO - "alluniverses$is_united_states                  : num  1 1 ..." HAS BEEN JUST ADDED
    # SO THE TIME IS 'TOO LATE'
    
    # browser()

    # process (if any) instances___expressions ( SportsRankings )
    if(!is.na(ept(measurestypes__instances___expressions))  ) {
    
      # FIX EVERY WHERE AN 'NA cell check' is.na(ept(my_cell_name))[1] TO_DO [ ]
      # excel returnes : empty cell: NA,  one element cell: FALSE, two element cell: FALSE, FALSE
      if( is.na(ept(measurestypes__perdivisions))[1]  ) {
      # default: if NOTHING is there'
      #   calculate an instances___expression SportsRanking? on the 'original (unparitioned?/dirty?) data'
      # TYPICALLY found 'VERY EARLY" in the 'measures' spreadsheet
    
      # if a cell in NA send "NONE" ( required by SportsRanking ( actually base::split ) )
        
      # temporary
      #####measurestypes__perdivisions <- "\"NONE\""

      #####within(alluniverses, {     
      #####  assign( str_c(ept(measures__instances),"__origdata__",ept(measurestypes__instances_suffixes)),ept(ept(measurestypes__instances___expressions))) 
      #####}) -> alluniverses
        
      
      
      
      
      # undo temporary
      #####measurestypes__perdivisions <- NA
        
      # head(plyr::arrange(alluniverses[,c("smallplustocks_market_cap__origdata__sportsranking","smallplustocks_market_cap","external_id", "fin_company_nm")], plyr::desc(smallplustocks_market_cap)), 990)
      
      }
    
      
      ######if(!is.na(ept(measurestypes__perdivisions))[1]  ) {
      # if 'SOMETHINGS' are there' 
      #   ( TYPICALLY "alldata" measures_instances WOULD HAVE BEEN calculated at this point and NOW USED )
      # calculate an instances___expression ON 

      # EACH measurestypes__perdivision FOUND IN measurestypes__perdivisions
      #     TYPICALLY "alldata"+
      # LOOP
    
      # temporary
      if( is.na(ept(measurestypes__perdivisions))[1]  )   measurestypes__perdivisions <- "\"NONE\""
      measurestypes__perdivisions_original <- measurestypes__perdivisions
      
      for(division_ite in ept(measurestypes__perdivisions)) {
        
        division_ite <- str_c('"',division_ite,'"') # put back SO compatiable AND consitent with the rest of the code
        measurestypes__perdivisions <- division_ite
      
        # browser( expr = { "finalscore_alldata_millennialvalue" == ept(measures__instances)  && predictclasses == "train" } )
      
        # within(alluniverses, {    
        #   # browser( expr = { "finalscore_alldata_millennialvalue" == ept(measures__instances)  && predictclasses == "train" } )
        #   assign( str_c(ept(measures__instances),"__",ept(measurestypes__perdivisions),"__",ept(measurestypes__instances_suffixes)),ept(ept(measurestypes__instances___expressions))) 
        # }) -> alluniverses
        
        

        first_numb_of_division_ite <- TRUE
        for(numb_of_division_ite in ept(ept(measurestypes__instances__numb_of_divisions))) {

          #  ept(ept("\"alist(10,100, 'NGT100xorNROW',max(55:as.integer('555')))\"")) # THIS DOES WORK
          
          if( is.numeric(numb_of_division_ite) )   { rk <- numb_of_division_ite      ; suffixx <- as.character(rk) }
          if( is.call(numb_of_division_ite) )      { rk <- eval(numb_of_division_ite); suffixx <- as.character(rk) }
          if( is.character(numb_of_division_ite) ) { rk <- paste0("\'",numb_of_division_ite,"\'"); suffixx <- as.character(numb_of_division_ite) }

          # override
          if(first_numb_of_division_ite) suffixx <- "";  # default ON FIRST 
          
          expressionxx <- measurestypes__instances___expressions
          
          stringr::str_replace(expressionxx,'DIV',rk) -> expressionxx

          if( first_numb_of_division_ite) long_suffixx <- ept(measurestypes__instances_suffixes) # default name
          if(!first_numb_of_division_ite) long_suffixx <- paste0(ept(measurestypes__instances_suffixes),suffixx)

          u_before_columns <- colnames(alluniverses)
          
          within(alluniverses, {    
            # browser( expr = { "finalscore_alldata_millennialvalue" == ept(measures__instances)  && predictclasses == "train" } )
            # assign( str_c(ept(measures__instances),"__",ept(measurestypes__perdivisions),"__",ept(measurestypes__instances_suffixes)),ept(ept(measurestypes__instances___expressions))) 
            assign( str_c(ept(measures__instances),"__",ept(measurestypes__perdivisions),"__",long_suffixx),ept(ept(expressionxx))) 
          }) -> alluniverses
          
          u_after_columns  <- colnames(alluniverses)
          alluniverses <- cleanBADValuesSpecColumns(df = alluniverses,  before_columns = u_before_columns, after_columns = u_after_columns)
          rm("u_before_columns","u_after_columns")
          
          if(first_numb_of_division_ite) first_numb_of_division_ite <- FALSE # 2nd loop +
        }
        rm("numb_of_division_ite")


      }
      rm("division_ite")
      
      # browser( expr = { "finalscore_alldata_millennialvalue" == ept(measures__instances)  && predictclasses == "train" } )
      
      # sanity check
      # head(plyr::arrange(alluniverses[,c("smallplustocks_market_cap__bus_sector_nm__sportsranking","smallplustocks_market_cap","external_id", "fin_company_nm")], plyr::desc(smallplustocks_market_cap)), 990)
      
      # undo temporary
      measurestypes__perdivisions <- measurestypes__perdivisions_original
      rm("measurestypes__perdivisions_original")
      if( is.na(ept(measurestypes__perdivisions))[1]  )   measurestypes__perdivisions  <- NA
      
      ######}
      
      # WORK HERE
      # SEARCH NOTES:  use lazyeval and dplyr::mutate_ to dyamamically and a column and its values
      # MULTITABLE or/xor SOME COMBO: dplyr::mutate_each  # R.utils::unwrap  # hdquantile+  # reshape2::melt, # reshape2::acast
      
    }

    # (currenly ONLY to prevent the program from going TOO far )
    # browser( expr = { "finalscore_alldata_mashup" == ept(measures__instances)  && predictclasses == "train" } )
    
    # browser( expr = { "pct_return_on_inv_capital_y1_betters" == ept(measures__instances)  && predictclasses == "train" } )
    # browser( expr = { "finalscore_alldata_mashup" == ept(measures__instances)  && predictclasses == "train" } )
    
    # browser( expr = { "finalscore_alldata_millennialvalue" == ept(measures__instances)  && predictclasses == "train" } )
    
    # browser( expr = { "pct_prices_return" == ept(measures__instances)  && predictclasses == "test" } )

    bookmarkhere <- 1
    
    print(paste0("  Ending measures__instances:", ept(measures__instances), collapse = "__" ))

    # clean up that last record
    suppressWarnings(rm(list=names(record0)))
    
    bookmarkhere <- 1

  } # for(rowindex in seq_along(row.names(spreadsheet_tab_measures)))

  # end THIS function
  Sys.setenv(TZ=oldtz)
  
  bookmarkhere <- 1

  # A VERY COMMON PLACE
  # browser()
  
  return(alluniverses)
  
  # NEED YET, SPECIFIC COMPOSITE BREAK DOWN LEFT_OFF [ ]
  
  # View(table(alluniverses$fundamentalvalue_alldata_composite))
  # View(table(alluniverses$earningsquality_alldata_composite))
  # View(table(alluniverses$financialstrength_alldata_composite))
  # View( with( alluniverses, { cbind(fundamentalvalue_alldata_composite, fundamentalvalue_alldata_composite__alldata__sportsranking,earningsquality_alldata_composite, earningsquality_alldata_composite__alldata__sportsranking,financialstrength_alldata_composite, financialstrength_alldata_composite__alldata__sportsranking,fundamentalvalue_expose_alldata_composite,fundamentalvalue_expose_alldata_composite__alldata__sportsranking2,finalscore_alldata_www_mashup, finalscore_alldata_www_mashup__alldata__sportsranking,finalscore_alldata_www_mashup__alldata__sportsranking__is_best25)[which(finalscore_alldata_www_mashup__alldata__sportsranking__is_best25 == 1),] } ) )
  # View( with( alluniverses, { cbind(fundamentalvalue_alldata_composite, fundamentalvalue_alldata_composite__alldata__sportsranking,earningsquality_alldata_composite, earningsquality_alldata_composite__alldata__sportsranking,financialstrength_alldata_composite, financialstrength_alldata_composite__alldata__sportsranking,fundamentalvalue_expose_alldata_composite,fundamentalvalue_expose_alldata_composite__alldata__sportsranking2,finalscore_alldata_www_mashup_bad, finalscore_alldata_www_mashup_bad__alldata__sportsranking,finalscore_alldata_www_mashup_bad__alldata__sportsranking__is_worst25)[which(finalscore_alldata_www_mashup_bad__alldata__sportsranking__is_worst25 == 1),] } ) )
  
  
  # View( with( alluniverses, { cbind(price_pct_chge_since_26w_betters, price_pct_chge_since_26w_betters__alldata__sportsranking,free_cash_over_price_ratio, free_cash_over_price_ratio__alldata__sportsranking,shareholder_orientation, shareholder_orientation__alldata__sportsranking,pct_return_on_inv_capital_y1_betters, pct_return_on_inv_capital_y1_betters__alldata__sportsranking,earnings_quality, earnings_quality__alldata__sportsranking, finalscore_alldata_millennialvalue__alldata__sportsranking, finalscore_alldata_millennialvalue__alldata__sportsranking__is_best25)[which(finalscore_alldata_millennialvalue__alldata__sportsranking__is_best25 == 1),]  } ) )
  # View( with( alluniverses, { cbind(price_pct_chge_since_26w_betters, price_pct_chge_since_26w_betters__alldata__sportsranking,free_cash_over_price_ratio, free_cash_over_price_ratio__alldata__sportsranking,shareholder_orientation, shareholder_orientation__alldata__sportsranking,pct_return_on_inv_capital_y1_betters, pct_return_on_inv_capital_y1_betters__alldata__sportsranking,earnings_quality, earnings_quality__alldata__sportsranking, finalscore_alldata_millennialvalue__alldata__sportsranking, finalscore_alldata_millennialvalue__alldata__sportsranking__is_worst25)[which(finalscore_alldata_millennialvalue__alldata__sportsranking__is_worst25 == 1),]  } ) )
  
  # end sheet measures
  
  # NOT TESTED (YET)
  # ascending (going down the page ) best to 'not so best'
  # plyr::arrange(alluniverses[,c("external_id","internal_id","fin_company_nm","bus_sector_nm","bus_industry_nm","company_size_level_name")], desc(finalscore_alldata_mashup__alldata__sportsranking))
  
                                                                            # last check: END_OF_traditional_value TRUE (EXCLLENT)
  # isFinite <- function(x) { if(is.na(x)) return(TRUE) ; is.finite (x) }   # KEEP CODE - GENERAL CHECK FOR ANY  Inf,-Inf SLIPPING IN THERE
  # !any(sapply(as.vector(unlist(alluniverses)), isFinite))                 # KEEP CODE - GENERAL CHECK FOR ANY  Inf,-Inf SLIPPING IN THERE

  # !any(sapply(as.vector(unlist(alluniverses)), is.nan))                  # last check FALSE(NOT_GREAT: STILL *SHOULD HANDLE* )
                                                                          # KEEP CODE - GENERAL CHECK FOR ANY  NaN SLIPPING IN THERE


}
# data_loading_with_Excel_4()   



# rm(list=ls(all.names=TRUE))  
# debugSource(paste0(getwd(),'/data_loading_with_Excel_4.R'))     
# universei <- data_loading_with_Excel_4(predictclasses = "train", traintest_index = 1)                                                                                                                                                             
# universei <- data_loading_with_Excel_4(predictclasses = "test" , traintest_index = 1)   
#
#      



data_working_from_Excel4 <- function(
   # IF AAII has not (yet) delivered Month End Update: Error in foreign::read.dbf(file = path_file_name, as.is = TRUE) : unable to open DBF file
    asOfDate = Sys.Date() # zoo::as.Date("2016-01-29") # Sys.Date() # zoo::as.Date("2016-01-29") # dev/testing date
  , predicttrain_1Date  = if( asOfDate < (lastWeekDayDateOfMonth(asOfDate) + 1) ) { lastWeekDayDateOfMonth( lubridate::`%m+%`(asOfDate, base::months(-1)))  } else {  lastWeekDayDateOfMonth(asOfDate)  }
#   predicttrain_1Date  = if( Sys.Date() < (lastWeekDayDateOfMonth(Sys.Date()) + 1) ) { lastWeekDayDateOfMonth( lubridate::`%m+%`(Sys.Date(), base::months(-1)))  } else {  lastWeekDayDateOfMonth(Sys.Date())  }

  , truetest_2Date      = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months( -0  - 0)))
    ,  truetrain_2Date  = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months(-12  - 0)) ) 
  , truetest_1Date      = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months( -0  - 6)))
    , truetrain_1Date   = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months(-12  - 6)) )

  ,     test_2Date      = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months( -0  - 0 - 3)))
    ,      train_2Date  = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months(-12  - 0 - 3)) ) 
  ,     test_1Date      = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months( -0  - 6 - 3)))
    ,      train_1Date  = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months(-12  - 6 - 3)) )

  , calibtest_1Date     = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months( -0  - 6 - 3 - 3)))
    , calibtrain_1Date  = lastWeekDayDateOfMonth( lubridate::`%m+%`( predicttrain_1Date, base::months(-12  - 6 - 3 - 3)) ) 

  ) {

  # (CURRENTLY) # ONLY THE HIGHEST LEVEL FUNCTION
  # begin function
  op <- options()
  oldwd <- getwd()
  setwd("C:/Users/AnonymousUser/Desktop/R-Portable.3.2.2/App/R-Portable/bin/x64/RDebug/Home") 

  # begin THIS function
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  bookmark <- 1
  
  # HARD NOTE
  # NOTE: IN THE FUTURE IF I GET BOTH 
  # CALIB/TRAIN/TEST from the SAME big_chunk I WILL WANT TO ** stratify ***

  # could be LOOPED?

  # (logically late as possible)
  
  print(paste0("BEGIN predicttrain_1Date: ",predicttrain_1Date))
  universe_1_predicttrain  <- data_loading_with_Excel_4('train', traintest_index = 1, timends = as.numeric(predicttrain_1Date))

  print(paste0("BEGIN truetest_1Date: ",truetest_1Date))
  universe_1_truetest    <- data_loading_with_Excel_4( 'truetest', traintest_index = 1, timends = as.numeric(truetest_1Date), return_at_end_ofattributes = TRUE)
  print(paste0("BEGIN truetest_2Date: ",truetest_2Date))
  universe_12_truetest   <- data_loading_with_Excel_4( 'truetest', traintest_index = 2, timends = as.numeric(truetest_2Date),   other_universes = list(universe_1_truetest))

  print(paste0("BEGIN truetrain_1Date: ",truetrain_1Date))
  universe_1_truetrain   <- data_loading_with_Excel_4('truetrain', traintest_index = 1, timends = as.numeric(truetrain_1Date), return_at_end_ofattributes = TRUE)
  print(paste0("BEGIN truetrain_2Date: ",truetrain_2Date))
  universe_12_truetrain  <- data_loading_with_Excel_4('truetrain', traintest_index = 2, timends = as.numeric(truetrain_2Date),   other_universes = list(universe_1_truetrain))

  print(paste0("BEGIN test_1Date: ",test_1Date))
  universe_1_test    <- data_loading_with_Excel_4( 'test', traintest_index = 1, timends = as.numeric(test_1Date), return_at_end_ofattributes = TRUE)
  print(paste0("BEGIN test_2Date: ",test_2Date))
  universe_12_test   <- data_loading_with_Excel_4( 'test', traintest_index = 2, timends = as.numeric(test_2Date),   other_universes = list(universe_1_test))

  print(paste0("BEGIN train_1Date: ",train_1Date))
  universe_1_train   <- data_loading_with_Excel_4('train', traintest_index = 1, timends = as.numeric(train_1Date), return_at_end_ofattributes = TRUE)
  print(paste0("BEGIN train_2Date: ",train_2Date))
  universe_12_train  <- data_loading_with_Excel_4('train', traintest_index = 2, timends = as.numeric(train_2Date),   other_universes = list(universe_1_train))

  print(paste0("BEGIN calibtest_1Date: ",calibtest_1Date))
  universe_1_calibtest    <- data_loading_with_Excel_4( 'calibtest', traintest_index = 1, timends = as.numeric(calibtest_1Date))
  print(paste0("BEGIN calibtrain_1Date: ",calibtrain_1Date))
  universe_1_calibtrain   <- data_loading_with_Excel_4('calibtrain', traintest_index = 1, timends = as.numeric(calibtrain_1Date))


  
  
  # smartly combin test with train
  # could be LOOPED?


  # train and test area

  # alone and unique
  universe_12_test_unique_columns_location <- which(colnames(universe_12_test) %in% setdiff(colnames(universe_12_test),colnames(universe_12_train)))
  universe_12_test_adj <- universe_12_test

  # prepend test__ to the test columns ( NOW: resp__ )
  colnames(universe_12_test_adj)[universe_12_test_unique_columns_location] <- paste0('resp__',colnames(universe_12_test_adj)[universe_12_test_unique_columns_location])

  # RETURN  ( IN FUTURE - JOIN ON SOMETHING BETTER A (NOT CREATED YET) )
  # A globala_global_internal_id- SO I CAN BRING IN EDGAR/YAHOO/GOOGLE DATA )

  # bring together by 'common key(s)
  universe_12_traintest <- plyr::join(universe_12_train, universe_12_test_adj[,c(c('provider_global_internal_id','traintest_index'),setdiff(colnames(universe_12_test_adj),colnames(universe_12_train)))], by = c('provider_global_internal_id','traintest_index'), type = "inner", match = "all")


  # truetrain and truetest area

  # alone and unique
  universe_12_truetest_unique_columns_location <- which(colnames(universe_12_truetest) %in% setdiff(colnames(universe_12_truetest),colnames(universe_12_truetrain)))
  universe_12_truetest_adj <- universe_12_truetest

  # EXCELENT ONE TO 'BROWSE ON' ( KEEP ) # or AAPL or WMT
  # Browse[2]> str(universe_12_traintest[universe_12_traintest$fin_instru_tk == "MSFT",], list.len = 999)
  
  # prepend truetest__ to the truetest columns ( NOW: resp__ )
  colnames(universe_12_truetest_adj)[universe_12_truetest_unique_columns_location] <- paste0('resp__',colnames(universe_12_truetest_adj)[universe_12_truetest_unique_columns_location])

  # RETURN  ( IN FUTURE - JOIN ON SOMETHING BETTER A (NOT CREATED YET) )
  # A globala_global_internal_id- SO I CAN BRING IN EDGAR/YAHOO/GOOGLE DATA )

  # bring together by 'common key(s)
  universe_12_truetraintruetest <- plyr::join(universe_12_truetrain, universe_12_truetest_adj[,c(c('provider_global_internal_id','traintest_index'),setdiff(colnames(universe_12_truetest_adj),colnames(universe_12_truetrain)))], by = c('provider_global_internal_id','traintest_index'), type = "inner", match = "all")

  
  # calibtrain and calibtest area

  # alone and unique
  universe_1_calibtest_unique_columns_location <- which(colnames(universe_1_calibtest) %in% setdiff(colnames(universe_1_calibtest),colnames(universe_1_calibtrain)))
  universe_1_calibtest_adj <- universe_1_calibtest

  # prepend truetest__ to the truetest columns ( NOW: resp__ )
  colnames(universe_1_calibtest_adj)[universe_1_calibtest_unique_columns_location] <- paste0('resp__',colnames(universe_1_calibtest_adj)[universe_1_calibtest_unique_columns_location])

  # RETURN  ( IN FUTURE - JOIN ON SOMETHING BETTER A (NOT CREATED YET) )
  # A globala_global_internal_id- SO I CAN BRING IN EDGAR/YAHOO/GOOGLE DATA )

  # bring together by 'common key(s)
  universe_1_calibtraincalibtest <- plyr::join(universe_1_calibtrain, universe_1_calibtest_adj[,c(c('provider_global_internal_id','traintest_index'),setdiff(colnames(universe_1_calibtest_adj),colnames(universe_1_calibtrain)))], by = c('provider_global_internal_id','traintest_index'), type = "inner", match = "all")

  bookmarkhere <- 1
  
  # NEVER DONE
  # save(list = "universecoll",file = paste0("universecoll_before_leaky_record_elim", "_", as.numeric(as.POSIXlt(Sys.time(), "GMT")),".Rdata"))

  print(paste0("Begin leaky record elimination"))
  
  # detect leaky records
  leaky_records_df <- sqldf::sqldf("select a.provider_global_internal_id, a.date_at_end_of_q1 from universe_12_truetraintruetest a, universe_12_traintest b where a.provider_global_internal_id = b.provider_global_internal_id and a.date_at_end_of_q1 = b.date_at_end_of_q1")
  
  # detect leaky truetrain records
  leaky_truetrain_records_df <- sqldf::sqldf("select a.provider_global_internal_id, a.date_at_end_of_q1 from universe_12_truetraintruetest a, leaky_records_df b where a.provider_global_internal_id = b.provider_global_internal_id and a.date_at_end_of_q1 = b.date_at_end_of_q1")
  
  # detect leaky train records
  leaky_train_records_df <- sqldf::sqldf("select a.provider_global_internal_id, a.date_at_end_of_q1 from universe_12_traintest a, leaky_records_df b where a.provider_global_internal_id = b.provider_global_internal_id and a.date_at_end_of_q1 = b.date_at_end_of_q1")
  
  # reduce records to those that are not leaking
  
  # required
  universe_12_truetraintruetest_unleaking <- sqldf::sqldf(c("create index idx on universe_12_truetraintruetest(provider_global_internal_id, date_at_end_of_q1)","create index idx2 on leaky_truetrain_records_df(provider_global_internal_id, date_at_end_of_q1)","select a.* from universe_12_truetraintruetest a where not exists (select b.provider_global_internal_id, b.date_at_end_of_q1 from leaky_truetrain_records_df b where a.provider_global_internal_id = b.provider_global_internal_id and a.date_at_end_of_q1 = b.date_at_end_of_q1)" ))
  
# optional
  universe_12_traintest_unleaking <- sqldf::sqldf(c("create index idx on universe_12_traintest(provider_global_internal_id, date_at_end_of_q1)","create index idx2 on leaky_train_records_df(provider_global_internal_id, date_at_end_of_q1)","select a.* from universe_12_traintest a where not exists (select b.provider_global_internal_id, b.date_at_end_of_q1 from leaky_train_records_df b where a.provider_global_internal_id = b.provider_global_internal_id and a.date_at_end_of_q1 = b.date_at_end_of_q1)" ))  

  print(paste0("End leaky record elimination"))
  
  # NEVER DONE
  # save(list = "universecoll",file = paste0("universecoll_after_leaky_record_elim", "_", as.numeric(as.POSIXlt(Sys.time(), "GMT")),".Rdata"))
  
  
  # could have: mlr::capLargeValues as businessly/techically appropriate
  # do again ( Xth(2nd) time in the program )
  # ( NOTE: later complete.cases WILL remove NaN and 'of course' NAs )
  cleanLargeValues(universe_1_predicttrain)                 -> universe_1_predicttrain
  cleanNaNValues(universe_1_predicttrain)                   -> universe_1_predicttrain
  
  cleanLargeValues(universe_12_truetraintruetest_unleaking) -> universe_12_truetraintruetest_unleaking
  cleanNaNValues(universe_12_truetraintruetest_unleaking)   -> universe_12_truetraintruetest_unleaking
  
  cleanLargeValues(universe_12_traintest_unleaking)         -> universe_12_traintest_unleaking
  cleanNaNValues(universe_12_traintest_unleaking)           -> universe_12_traintest_unleaking
  
  cleanLargeValues(universe_1_calibtraincalibtest)          -> universe_1_calibtraincalibtest # NOTE: I HAVE NOT CHECKED FOR LEAKS!
  cleanNaNValues(universe_1_calibtraincalibtest)            -> universe_1_calibtraincalibtest # NOTE: I HAVE NOT CHECKED FOR LEAKS!
                                                                # MAY? WANT TO SIMPLY MOVE IT BACK YET ONE MORE QUARTER 
  
  list(
      universe_all_predicttrain         = universe_1_predicttrain
    , universe_all_truetraintruetest    = universe_12_truetraintruetest_unleaking
    , universe_all_traintest            = universe_12_traintest_unleaking
    , universe_all_calibtraincalibtest  = universe_1_calibtraincalibtest # vin-vector video ( HOW? DO I APPLY to 'train' and 'truetrain' ) )
  ) -> returned

  # end THIS function
  Sys.setenv(TZ=oldtz)

  # (CURRENTLY) # ONLY THE HIGHEST LEVEL FUNCTION
  # end function
  setwd(oldwd)
  options(op)     # reset (all) initial options
  
  return(returned)

}

# rm(list=ls(all.names=TRUE))  
# debugSource(paste0(getwd(),'/data_loading_with_Excel_4.R'))        
# data_working_from_Excel4()        
# # returned.Rdata
# universecoll <- data_working_from_Excel4() 

# OLD DATE DEBUGGING ( SEE IF WORKED FINE ON OLD MO DAY BEFORE TRY TO FIX: THIS MONTHS DATA )
# universecoll_OLD <- data_working_from_Excel4(asOfDate = zoo::as.Date("2016-03-15"))

# #  REM - end of month - the data.set changes ( gets shifted forward to current month )
#       
# save(list = "universecoll",file = paste0("universecoll", "_", as.numeric(as.POSIXlt(Sys.time(), "GMT")),".Rdata"))


# ret <- universecoll[["universe_all_traintest"]]
# with( ret ,{ ept(str_c("((", str_c(sprintf("earn_per_share_diluted_q%1$d",1:4), collapse = " + "), " ) / shares_common_out_average_q1 - ( ", str_c(sprintf("earn_per_share_diluted_q%1$d",5:8), collapse = " + "), " ) / shares_common_out_average_q5 ) / 1.0")) } )

bookmarkhere <- 1

data_processing_from_Excel4 <- function(universecoll = NULL, quickdebug = FALSE, sinkOutput = TRUE, 
    quickdebug_TRUE_file = "universecoll_1457230118.09143.Rdata", 
    # weights_expr = "abs(train_model_small_data[[traintest_data_list[['response_column']]]] - 6001)**1.27",
    weights_expr = "rep(1,NROW(train_model_small_data[[traintest_data_list[['response_column']]]]))", 
                                        
#   tree_params = list(n.trees         =  20000, #  300  #  20000 # 300 adaptive_cv # 200 non-A
#                    shrinkage         = 0.0005, # 0.05  # 0.0005
#                    interaction.depth =      5, #    2  #      5
#                    repeatedcv_number  =     5),#    2  #      5
  
    tree_params = list(n.trees         =  300  , #  300  #  20000 # 300 adaptive_cv # 200 non-A
                     shrinkage         = 0.05  , # 0.05  # 0.0005
                     interaction.depth =      2, #    2  #      5
                     repeatedcv_number  =     2),#    2  #      5
  
  traintest_data_list = list(

    response_column      = c('resp__pct_totals_12m_return__alldata__sportsranking6000'),
  
    response_aes_column  = c('resp__pct_totals_12m_return'),
  
    other_aes_columns    = c('fin_instru_tk','fin_company_nm','bus_industry_nm','company_size_level_name'),
  
    predictor_columns   =  c('price_pct_chge_since_26w_betters__alldata__sportsranking6000',
                             'aaii_free_cash_plus_divs_over_price_ratio__alldata__sportsranking6000',
                             'shareholder_orientation__alldata__sportsranking6000',
                             'return_on_inv_capital__alldata__sportsranking6000',
                             'earnings_quality__alldata__sportsranking6000'),
  
    predictor_aes_columns = c('price_pct_chge_since_26w_betters',
                             'aaii_free_cash_plus_divs_over_price_ratio',
                             'shareholder_orientation',
                             'return_on_inv_capital',
                             'earnings_quality'),
    
    composite_expression_name = c('millennial_composite'),
    composite_expression  = c('price_pct_chge_since_26w_betters__alldata__sportsranking6000 + aaii_free_cash_plus_divs_over_price_ratio__alldata__sportsranking6000 + 
                               shareholder_orientation__alldata__sportsranking6000 + return_on_inv_capital__alldata__sportsranking6000 + 
                               earnings_quality__alldata__sportsranking6000'),
    
    test_timeends_column  = c('resp__test_timends'),
  
    global_join_columns   = c('provider_global_internal_id_timepoint_exact','provider_global_internal_id'),
  
    local_join_columns    = c('timends','traintest_index'),
  
    rowindex_expression   = c('rowindex <- as.numeric(seq_along(row.names(get("data",envir = parent.frame(5)))))')

  ),

  traintest_data_list_factors = c()

  ) {
  
  
  
  
  # if the user did not actually supply the data, then go get it ( Style? )
  # ???  quickdebug == FALSE
  # if(missing(universecoll)) universecoll <- data_working_from_Excel4() 
  
  # debugging EXPECT MODIFY OFTEN
  # February 24 2016
  # if(quickdebug == TRUE) load(file = "universecoll_1457230118.09143.Rdata", envir = environment())
  if(quickdebug == TRUE) load(file = quickdebug_TRUE_file, envir = environment())
  # quickdebug == FALSE, then the data MUST be acuired by SEARCH
  
  # (CURRENTLY) # ONLY THE HIGHEST LEVEL FUNCTION
  # begin function
  
  options(width = 10000) # 255    
  options(digits = 22) 
  options(max.print=99999)
  options(scipen=255) # Try these = width 
  options(warn=2)
  
  op <- options()
  oldwd <- getwd()
  setwd("C:/Users/AnonymousUser/Desktop/R-Portable.3.2.2/App/R-Portable/bin/x64/RDebug/Home") 

  # begin THIS function
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  bookmark <- 1
  
  # what I am interested in 

  # "classic_millennial" ( actions ) # COULD BE IN EXCEL

  # MAYBE not the right place ( should be higher )
   
  # NOTE: Inf/-Inf ALREADY HANDLED by the PREVIOUS program 
  # treatment and/or imputation and/or complete.cases ( removes NaN and NA )
  # package vtreat, OTHERS mlr::capLargeValues, stats::complete.cases
       
  # X TO BE PUT ON THE spreadsheet X - NO put LATER IN A transform
  #   millennial_sum_of_factors <-  price_pct_chge_since_26w_betters__alldata__sportsranking6000 +
  #                           aaii_free_cash_plus_divs_over_price_ratio__alldata__sportsranking6000 +
  #                           shareholder_orientation__alldata__sportsranking6000 +
  #                           return_on_inv_capital__alldata__sportsranking6000 +
  #                           earnings_quality__alldata__sportsranking6000'),


  # truetest_response_column_rename_expression = "colnames(WW)[which(colnames(WW) %in% 'truetest__pct_totals_12m_return__alldata__sportsranking6000')] <- 'test__pct_totals_12m_return__alldata__sportsranking6000'"  
  

  
  # all the columns
  # train_model_data <- ept(traintest_data_list[['traintest_data_expression']])
  
  calibtraincalibtest_data_expression <- "universecoll[['universe_all_calibtraincalibtest']]"
  calibtrain_model_data <- ept(calibtraincalibtest_data_expression)
  
  traintest_data_expression <- "universecoll[['universe_all_traintest']]"
  train_model_data <- ept(traintest_data_expression)
  
  truetraintruetest_data_expression <- "universecoll[['universe_all_truetraintruetest']]"
  truetrain_model_data <- ept(truetraintruetest_data_expression)
  
  # SPECIAL
  predicttrain_data_expression <- "universecoll[['universe_all_predicttrain']]"
  predicttrain_model_data <- ept(predicttrain_data_expression)
  # required for CORRECT gbm::predict.gbm  Y=X1+X2
  predicttrain_model_data[,traintest_data_list$response_column] <- NA
  

  # SHOULD VALIDATE EACH VECTOR to make sure those rows exist
  
  validate_traintest_cols_exist <- function(x,l,isTrainOnly = FALSE) {
    
    x_name <- as.character(substitute(x))
    
    print(paste0(x_name,' Verifying: response_column'))
    if(!any((colnames(x) %in% l$response_column))) stop()
    if(isTrainOnly != TRUE) {                                     # train AND test
      print(paste0(x_name,' Verifying: response_aes_column'))
      if(!any((colnames(x) %in% l$response_aes_column))) stop()
    }
    print(paste0(x_name,' Verifying: other_aes_columns'))
    if(!any((colnames(x) %in% l$other_aes_columns))) stop()
    print(paste0(x_name,' Verifying: predictor_columns'))
    if(!any((colnames(x) %in% l$predictor_columns))) stop()
    print(paste0(x_name,' Verifying: predictor_aes_columns'))
    if(!any((colnames(x) %in% l$predictor_aes_columns))) stop()
    if(isTrainOnly != TRUE) {                                     # train AND test
      print(paste0(x_name,' Verifying: test_timeends_column'))
      if(!any((colnames(x) %in% l$test_timeends_column))) stop()
    }
    print(paste0(x_name,' Verifying: global_join_columns'))
    if(!any((colnames(x) %in% l$global_join_columns))) stop()
    print(paste0(x_name,' Verifying: local_join_columns'))
    if(!any((colnames(x) %in% l$local_join_columns))) stop()
  }
  validate_traintest_cols_exist(  calibtrain_model_data,traintest_data_list)
  validate_traintest_cols_exist(       train_model_data,traintest_data_list)
  validate_traintest_cols_exist(   truetrain_model_data,traintest_data_list)
  validate_traintest_cols_exist(predicttrain_model_data,traintest_data_list,isTrainOnly = TRUE)
  
  # now reduce columns collection
  the_traintest_model_data_columns <- with( traintest_data_list, { 
                                c(response_column,
                                response_aes_column,
                                other_aes_columns,
                                predictor_columns, 
                                predictor_aes_columns,
                                test_timeends_column,
                                global_join_columns,
                                local_join_columns) } )
  
  the_train_model_data_columns <- with( traintest_data_list, { 
                                c(response_column,
                                # response_aes_column,
                                other_aes_columns,
                                predictor_columns, 
                                predictor_aes_columns,
                                # test_timeends_column,
                                global_join_columns,
                                local_join_columns) } )
  
  # now add actually reduce those to just those columns
  calibtrain_model_data_temp     <- calibtrain_model_data[  ,the_traintest_model_data_columns,drop = FALSE]
  train_model_data_temp          <- train_model_data[       ,the_traintest_model_data_columns,drop = FALSE]
  truetrain_model_data_temp      <- truetrain_model_data[   ,the_traintest_model_data_columns,drop = FALSE]
  predicttrain_model_data_temp   <- predicttrain_model_data[,the_train_model_data_columns,drop = FALSE]
  

  # now add actually add the composite expression1 column
  # within( train_model_data_temp, { ept(traintest_data_list$composite_expression1)} ) -> train_model_data_temp
  
  # now add actually rowindex column ( note sure how useful (here) )
  within( calibtrain_model_data_temp  , { ept(traintest_data_list$rowindex_expression)} ) -> calibtrain_model_data_temp
  within( train_model_data_temp       , { ept(traintest_data_list$rowindex_expression)} ) -> train_model_data_temp
  within( truetrain_model_data_temp   , { ept(traintest_data_list$rowindex_expression)} ) -> truetrain_model_data_temp
  within( predicttrain_model_data_temp, { ept(traintest_data_list$rowindex_expression)} ) -> predicttrain_model_data_temp
  
  # View(train_model_data_temp[train_model_data_temp$fin_instru_tk == 'AAPL',]) # 2 records
  # View(train_model_data_temp) # FROM 10000 TO 4000(complete.cases(BELOW) DIFFICULT: lots of holes )
  
  # Do imputation/vtreatment HERE
  # Do imputation/vtreatment HER
  
  
  if(sinkOutput == TRUE ) {
    print("SINK is beginning.")
    con <- file("SinkOutput.txt")
    sink(con) # type="output"
    sink(con, type="message")
  }
  
  # just what is (almost) sent to train
  
  # now reduce columns collection                                     # add back the rowindex                                                      
  calibtrain_model_small_data_columns    <- with( traintest_data_list, { c(response_column,predictor_columns,'rowindex') } )
  train_model_small_data_columns         <- with( traintest_data_list, { c(response_column,predictor_columns,'rowindex') } )
  truetrain_model_small_data_columns     <- with( traintest_data_list, { c(response_column,predictor_columns,'rowindex') } )
  predicttrain_model_small_data_columns  <- with( traintest_data_list, { c(response_column,predictor_columns,'rowindex') } )
  
  # row counts (rc)
  calibtrain_model_rc_before_completed_cases   <- NROW(calibtrain_model_data_temp)
    print(paste0("calibtrain_model_rc_before_completed_cases: ",       calibtrain_model_rc_before_completed_cases))
  train_model_rc_before_completed_cases        <- NROW(train_model_data_temp)
    print(paste0("train_model_rc_before_completed_cases: ",                 train_model_rc_before_completed_cases))
  truetrain_model_rc_before_completed_cases    <- NROW(truetrain_model_data_temp)
    print(paste0("truetrain_model_rc_before_completed_cases: ",         truetrain_model_rc_before_completed_cases))
  predicttrain_model_rc_before_completed_cases <- NROW(predicttrain_model_data_temp)
    print(paste0("predicttrain_model_rc_before_completed_cases: ",   predicttrain_model_rc_before_completed_cases))
  
  # all the columns of model interest + 'rowindex' - incomplete_row_cases
  calibtrain_model_small_data     <- calibtrain_model_data_temp[complete.cases(  calibtrain_model_data_temp),                         calibtrain_model_small_data_columns,drop = FALSE]
  train_model_small_data               <- train_model_data_temp[complete.cases(       train_model_data_temp),                              train_model_small_data_columns,drop = FALSE]
  truetrain_model_small_data       <- truetrain_model_data_temp[complete.cases(   truetrain_model_data_temp),                          truetrain_model_small_data_columns,drop = FALSE] 
  # y value is not KNOWN yet ( the future )
  predicttrain_model_small_data <- predicttrain_model_data_temp[complete.cases(predicttrain_model_data_temp[,-1,drop = FALSE]),     predicttrain_model_small_data_columns,drop = FALSE] 

  # NOTE: train_model_small_data; from 10000 down to 4000 records  many holes in the data.

  # row counts (rc)
  calibtrain_model_rc_after_completed_cases   <- NROW(calibtrain_model_small_data)
    print(paste0("calibtrain_model_rc_after_completed_cases: ",       calibtrain_model_rc_after_completed_cases))
  train_model_rc_after_completed_cases        <- NROW(train_model_small_data)
    print(paste0("train_model_rc_after_completed_cases: ",                 train_model_rc_after_completed_cases))
  truetrain_model_rc_after_completed_cases    <- NROW(truetrain_model_small_data)
    print(paste0("truetrain_model_rc_after_completed_cases: ",         truetrain_model_rc_after_completed_cases))
  predicttrain_model_rc_after_completed_cases <- NROW(predicttrain_model_small_data)
    print(paste0("predicttrain_model_rc_after_completed_cases: ",   predicttrain_model_rc_after_completed_cases))
  
  # here because I do not want EXTRA factor values(levels) (elim by complete.cases) accidentally 
  #  sent to the modeller

  for(factori in traintest_data_list_factors) {

    calibtrain_model_small_data[[factori]]     <- as.factor(  calibtrain_model_small_data[[factori]])
    train_model_small_data[[factori]]          <- as.factor(       train_model_small_data[[factori]])
    truetrain_model_small_data[[factori]]      <- as.factor(   truetrain_model_small_data[[factori]])
    predicttrain_model_small_data[[factori]]   <- as.factor(predicttrain_model_small_data[[factori]])
  }
  
  # gather truetrain data necessary for true testing
  truetrainnewdata <- truetrain_model_small_data[,-NCOL(truetrain_model_small_data),drop = FALSE]
  
  # FUTURE gather predicttrain data necessar for predicting
  predicttrainnewdata <- predicttrain_model_small_data[,-NCOL(predicttrain_model_small_data),drop = FALSE]

  quarter_idx_all <- unique(truetrain_model_data_temp[["traintest_index"]])
  quarter_idx_all_ordered <- quarter_idx_all[order(quarter_idx_all)]
  
  bookmark <- 1 # LEFT_OFF # build the caret

  gbmmodels <- c()
  
  gbmmodels <- c("carettrainedGBM", gbmmodels)
  

  
  print(paste0("Begin ",gbmmodels[1]))

  gbmGrid <- expand.grid( n.trees=  seq(10,tree_params[["n.trees"]],10), # 200 trees # 20000 ( prod choice )
                      interaction.depth = tree_params[["interaction.depth"]], 
                      shrinkage = tree_params[["shrinkage"]],    # 0.0005 ( prod choice )
                      n.minobsinnode = 10)
  
  microbenchmark::microbenchmark( {
    
    set.seed(2)
    
    training_input <- train_model_small_data[,-NCOL(train_model_small_data),drop = FALSE] # remove "rowindex"
                                    # resp__pct_totals_12m_return__alldata__sportsranking6000 ~ .,
    carettrainedGBM <- caret::train(formula(training_input), 
                    data = training_input,
                    distribution='gaussian',
                    verbose = FALSE,
                    method = "gbm",  # gbm_2.1-06 ( LAST KNOWN )
                    # weights = abs(train_model_small_data[[traintest_data_list[["response_column"]]]] - 6001)**1.27, # 60000
                    # weights = ept("abs(train_model_small_data[[traintest_data_list[['response_column']]]] - 6001)**1.27"), # 60000 
                    weights = ept(weights_expr),
                    tuneGrid =  gbmGrid, # number = 10,  repeats = 3 # COMMON choice 
                    trControl = caret::trainControl(method = "repeatedcv" # "adaptive_cv" # 
                      , number = tree_params[["repeatedcv_number"]], repeats = 1  # adaptive_cv repeats = 2
                      , preProcOptions = NULL # NEW closer perfomance NEAR gbm::gbm # ???
                      , verboseIter = FALSE
                      , returnResamp = "all"
  #                   , adaptive = list(min = 2, # AND repeats = 2
  #                                     alpha = 0.05,
  #                                     method = "gls",
  #                                     complete = TRUE
  #                                 )
                      
                      )
                    # trControl = caret::trainControl(method = "none",  returnResamp = "all")
                    # COMMON CHOICE
                    # trControl = caret::trainControl(method = "repeatedcv", number = 5,   repeats = 1,  verboseIter = FALSE,  returnResamp = "all")
                    )
    rm("training_input")
  
  }, times = 1L, unit = "s") -> bench_result # 5? minutes # 4000 trees
  
  # print(bench_result)
  
  print("")
  print(paste0("Relative Influence: ",gbmmodels[1]))
  print(summary(carettrainedGBM, plotit = FALSE))
  print("")
  
  print("carettrainedGBM best tuned n.trees")
  print(carettrainedGBM$finalModel$tuneValue$n.trees)
  
  # recession sight# gbm into gbm(OOB) into gbm
  ops <- options()
  options(warn = 1)
  # NOT USED
  perfedOOBcarettrainedGBM <- gbm::gbm.perf(carettrainedGBM$finalModel, method="OOB", plot.it = FALSE) # , plot.it = TRUE
  # optiman number of trees
  options(ops)
  print("perfedOOB carettrainedGBM best tuned n.trees")
  print(perfedOOBcarettrainedGBM)
  
  #  ? caret::predict.train
  ## S3 method for class 'train'
  # predict(object, newdata = NULL, type = "raw", na.action = na.omit, ...)
  
  # caret stays caret
  assign("carettrainedGBM_truetrainnewdata_predicted",caret::predict.train(carettrainedGBM, truetrainnewdata))
  
  # gather predictions
  caret_gbm_predicted_small_data <- cbind(carettrainedGBM_truetrainnewdata_predicted, truetrain_model_small_data)

  # no 1.x 1.y # default left_join and match_all
  caret_gbm_predicted_all_data <- plyr::join(x = caret_gbm_predicted_small_data, truetrain_model_data_temp, by = "rowindex")

  # remove 2nd column name duplicates
  caret_gbm_predicted_all_data <- caret_gbm_predicted_all_data[, !duplicated(colnames(caret_gbm_predicted_all_data)),drop = FALSE]

  # add back in the composite
  within( caret_gbm_predicted_all_data, { ept(paste0(traintest_data_list[["composite_expression_name"]]," <- ",traintest_data_list[["composite_expression"]]) )}) -> caret_gbm_predicted_all_data_w_math
  
  caret_gbm_predicted_all_data_w_math <- DataCombine::MoveFront(caret_gbm_predicted_all_data_w_math, 
    c(traintest_data_list$response_aes_column,"carettrainedGBM_truetrainnewdata_predicted",traintest_data_list[["composite_expression_name"]],traintest_data_list$other_aes_columns)
  )                                                                                                                                 

  for( quarter_idx in quarter_idx_all_ordered ) {

    assign(paste0("caret_gbm_predicted_all_data_w_math_filtered","_",quarter_idx),dplyr::filter(caret_gbm_predicted_all_data_w_math, traintest_index == quarter_idx))

    for( top_n in c(2,5,10,25,50) ) {

      current <- plyr::arrange(get(paste0("caret_gbm_predicted_all_data_w_math_filtered","_",quarter_idx)), ept(traintest_data_list[["composite_expression_name"]]))
      print(paste0("carettrainedGBM * composite top_n: * ",top_n," of quarter ",quarter_idx))
      print(head(current,top_n))
      print(paste0("carettrainedGBM * composite top_n: * ",top_n," of quarter ",quarter_idx))
      print(paste0("carettrainedGBM * composite top_n mean *: ",mean(head(current,top_n)[[ traintest_data_list[["response_aes_column"]] ]]  )))
      print("")
      
      current <- plyr::arrange(get(paste0("caret_gbm_predicted_all_data_w_math_filtered","_",quarter_idx)), ept("carettrainedGBM_truetrainnewdata_predicted"))
      print(paste0("carettrainedGBM * carettrainedGBM_truetrainnewdata_predicted top_n *: ",top_n," of quarter ",quarter_idx))
      print(head(current,top_n))
      print(paste0("carettrainedGBM * carettrainedGBM_truetrainnewdata_predicted top_n *: ",top_n," of quarter ",quarter_idx))
      print(paste0("carettrainedGBM * carettrainedGBM_truetrainnewdata_predicted top_n mean *: ",mean(head(current,top_n)[[ traintest_data_list[["response_aes_column"]] ]]  )))
      print("")
      
      bookmarkhere <- 1
      
    }
    print("")
    
  }
  print("")

  print(paste0("End ",gbmmodels[1]))
  print("")
  print("")
  print("")
  print("")
  
  
  gbmmodels <- c("gbmtrainedGBM", gbmmodels)
  
  print(paste0("Begin ",gbmmodels[1]))
  
  microbenchmark::microbenchmark( {
    
    set.seed(2)

    training_input <- train_model_small_data[,-NCOL(train_model_small_data),drop = FALSE] # remove "rowindex"
    
    
    # avoind rstudio? MAGICALLY SHOWS UP bug # 'package:stats' may not be available when loading"
    # I HATE to PUT this HERE
    op <- options()
    options(warn = 1)
                              # resp__pct_totals_12m_return__alldata__sportsranking6000 ~ .
    gbmtrainedGBM <- gbm::gbm(formula(training_input),  # gbm_2.1-06 ( LAST KNOWN )
                      data = training_input,
                      # distribution='gaussian',
                      verbose = FALSE,
                      # weights = abs(train_model_small_data[[traintest_data_list[["response_column"]]]] - 6001)**1.27, # 60000
                      # weights = ept("abs(train_model_small_data[[traintest_data_list[['response_column']]]] - 6001)**1.27"), # 60000
                      weights = ept(weights_expr),
                      n.trees=tree_params[["n.trees"]],              # 20000 200
                      cv.folds = tree_params[["repeatedcv_number"]], # 5 2
                      # <none>
                      # cv.folds = 5, # COMMON
                      interaction.depth = tree_params[["interaction.depth"]], # 5 2
                      shrinkage = tree_params[["shrinkage"]],         # 0.0005 0.05
                      n.minobsinnode = 10)
    
    options(op)
    
    rm("training_input")
  
  }, times = 1L, unit = "s") -> bench_result # 8 minutes
  
  # print(bench_result)
  
  print("")
  print(paste0("Relative Influence: ",gbmmodels[1]))
  print(summary(gbmtrainedGBM, plotit = FALSE))
  print("")
  
  # get OPTIMAL number of trees
  perfedcvgbmtrainedGBM <-gbm::gbm.perf(gbmtrainedGBM,method="cv", plot.it = FALSE) # , plot.it = TRUE
  
  print("perfedcv gbmtrainedGBM best tuned n.trees")
  print(perfedcvgbmtrainedGBM)
  
  # gbm into gbm
  ## S3 method for class 'gbm' # DANGER; caret MUST load GBM first
  # gbm::predict.gbm # NAMESPACE # not exported # S3method(predict,gbm)
  assign("gbmtrainedGBM_truetrainnewdata_perfedcvgbmtrainedGBM_predicted",predict(gbmtrainedGBM,truetrainnewdata,perfedcvgbmtrainedGBM))
  
  # gather predictions
  gbm_gbm_predicted_small_data <- cbind(gbmtrainedGBM_truetrainnewdata_perfedcvgbmtrainedGBM_predicted, truetrain_model_small_data)

  # no 1.x 1.y # default left_join and match_all
  gbm_gbm_predicted_all_data <- plyr::join(x = gbm_gbm_predicted_small_data, truetrain_model_data_temp, by = "rowindex")

  # remove 2nd column name duplicates
  gbm_gbm_predicted_all_data <- gbm_gbm_predicted_all_data[, !duplicated(colnames(gbm_gbm_predicted_all_data)),drop = FALSE]

  # add back in the composite
  within( gbm_gbm_predicted_all_data, { ept(paste0(traintest_data_list[["composite_expression_name"]]," <- ",traintest_data_list[["composite_expression"]]) )}) -> gbm_gbm_predicted_all_data_w_math
  
  gbm_gbm_predicted_all_data_w_math <- DataCombine::MoveFront(gbm_gbm_predicted_all_data_w_math, 
    c(traintest_data_list$response_aes_column,"gbmtrainedGBM_truetrainnewdata_perfedcvgbmtrainedGBM_predicted",traintest_data_list[["composite_expression_name"]],traintest_data_list$other_aes_columns)
  )  
  
  for( quarter_idx in quarter_idx_all_ordered ) {

    assign(paste0("gbm_gbm_predicted_all_data_w_math_filtered","_",quarter_idx),dplyr::filter(gbm_gbm_predicted_all_data_w_math, traintest_index == quarter_idx))

    for( top_n in c(2,5,10,25,50) ) {

      current <- plyr::arrange(get(paste0("gbm_gbm_predicted_all_data_w_math_filtered","_",quarter_idx)), ept(traintest_data_list[["composite_expression_name"]]))
      print(paste0("gbmtrainedGBM * composite top_n *: ",top_n," of quarter ",quarter_idx))
      print(head(current,top_n))
      print(paste0("gbmtrainedGBM * composite top_n *: ",top_n," of quarter ",quarter_idx))
      print(paste0("gbmtrainedGBM * composite top_n mean *: ",mean(head(current,top_n)[[ traintest_data_list[["response_aes_column"]] ]]  )))
      print("")
      
      current <- plyr::arrange(get(paste0("gbm_gbm_predicted_all_data_w_math_filtered","_",quarter_idx)), ept("gbmtrainedGBM_truetrainnewdata_perfedcvgbmtrainedGBM_predicted"))
      print(paste0("gbmtrainedGBM * gbmtrainedGBM_truetrainnewdata_perfedcvgbmtrainedGBM_predicted top_n *: ",top_n," of quarter ",quarter_idx))
      print(head(current,top_n))
      print(paste0("gbmtrainedGBM * gbmtrainedGBM_truetrainnewdata_perfedcvgbmtrainedGBM_predicted top_n *: ",top_n," of quarter ",quarter_idx))
      print(paste0("gbmtrainedGBM * gbmtrainedGBM_truetrainnewdata_perfedcvgbmtrainedGBM_predicted top_n mean *: ",mean(head(current,top_n)[[ traintest_data_list[["response_aes_column"]] ]]  )))
      print("")

      bookmarkhere <- 1
      
    }
    print("")
    
  }
  print("")
  
  
  print(paste0("End ",gbmmodels[1]))
  print("")
  print("")
  print("")
  print("")

  print(sessionInfo())
  
  if(sinkOutput == TRUE ) {
    sink()
    sink(type="message")
    close(con)
    print("SINK has ended.")
  }

  
  # 'cbinding/sorting/Viewing' # LEFT_OFF
  
  bookmark <- 1
  

  
  # Maybe some data treating here
  
  # end THIS function
  Sys.setenv(TZ=oldtz)

  # (CURRENTLY) # ONLY THE HIGHEST LEVEL FUNCTION
  # end function
  setwd(oldwd)
  options(op)     # reset (all) initial options
  
}

# method1
# rm(list=ls(all.names=TRUE))
# debugSource(paste0(getwd(),'/data_loading_with_Excel_4.R'))
# results <- data_processing_from_Excel4(quickdebug = TRUE, quickdebug_TRUE_file = "universecoll_1457230118.09143.Rdata") # HARD CODE SLOW DATA LOAD


# method2 ( CURRENT )
# February 24
# load(file = "universecoll_1457230118.09143.Rdata")
## X rm(list=ls(all.names=TRUE)) X
# gdata::keep(universecoll, all=TRUE, sure = TRUE); debugSource(paste0(getwd(),'/data_loading_with_Excel_4.R'))
# put down a breakpoint
# results <- data_processing_from_Excel4(universecoll = universecoll, sinkOutput = FALSE)


bookmarkhere <- 1   

#      
#                              
#                                                                                                                                                                                                                                                        
