

# finecon01.R   

# A SIDE-WORK IN PROGRESS
sqldf2 <-
  function (x, stringsAsFactors = FALSE, row.names = FALSE, envir = parent.frame(),
            method = getOption("sqldf.method"), file.format = list(),
            dbname, drv = getOption("sqldf.driver"), user, password = "",
            host = "localhost", port, dll = getOption("sqldf.dll"), connection = getOption("sqldf.connection"),
            verbose = isTRUE(getOption("sqldf.verbose")))
  {
    as.POSIXct.numeric <- function(x, ...) structure(x, class = c("POSIXct",
                                                                  "POSIXt"))
    as.POSIXct.character <- function(x) structure(as.numeric(x),
                                                  class = c("POSIXct", "POSIXt"))
    as.Date.character <- function(x) structure(as.numeric(x),
                                               class = "Date")
    as.Date2 <- function(x) UseMethod("as.Date2")
    as.Date2.character <- function(x) as.Date.character(x)
    as.Date.numeric <- function(x, origin = "1970-01-01", ...) base::as.Date.numeric(x,
                                                                                     origin = origin, ...)
    as.dates.character <- function(x) structure(as.numeric(x),
                                                class = c("dates", "times"))
    as.times.character <- function(x) structure(as.numeric(x),
                                                class = "times")
    backquote.maybe <- function(nam) {
      if (drv == "h2") {
        nam
      }
      else if (drv == "mysql") {
        nam
      }
      else if (drv == "pgsql") {
        nam
      }
      else if (drv == "postgresql") {
        nam
      }
      else {
        if (regexpr(".", nam, fixed = TRUE)) {
          paste("`", nam, "`", sep = "")
        }
        else nam
      }
    }
    name__class <- function(data, ...) {
      if (is.null(data))
        return(data)
      cls <- sub(".*__([^_]+)|.*", "\\1", names(data))
      f <- function(i) {
        if (cls[i] == "") {
          data[[i]]
        }
        else {
          fun_name <- paste("as", cls[i], sep = ".")
          fun <- mget(fun_name, envir = environment(),
                      mode = "function", ifnotfound = NA, inherits = TRUE)[[1]]
          if (identical(fun, NA))
            data[[i]]
          else {
            names(data)[i] <<- sub("__[^_]+$", "", names(data)[i])
            fun(data[[i]])
          }
        }
      }
      data[] <- lapply(1:NCOL(data), f)
      data
    }
    colClass <- function(data, cls) {
      if (is.null(data))
        return(data)
      if (is.list(cls))
        cls <- unlist(cls)
      cls <- rep(cls, length = length(data))
      f <- function(i) {
        if (cls[i] == "") {
          data[[i]]
        }
        else {
          fun_name <- paste("as", cls[i], sep = ".")
          fun <- mget(fun_name, envir = environment(),
                      mode = "function", ifnotfound = NA, inherits = TRUE)[[1]]
          if (identical(fun, NA))
            data[[i]]
          else {
            names(data)[i] <<- sub("__[^_]+$", "", names(data)[i])
            fun(data[[i]])
          }
        }
      }
      data[] <- lapply(1:NCOL(data), f)
      data
    }
    overwrite <- FALSE
    request.open <- missing(x) && is.null(connection)
    request.close <- missing(x) && !is.null(connection)
    request.con <- !missing(x) && !is.null(connection)
    request.nocon <- !missing(x) && is.null(connection)
    dfnames <- fileobjs <- character(0)
    if (!is.list(method))
      method <- list(method, NULL)
    to.df <- method[[1]]
    to.db <- method[[2]]
    if (request.close || request.nocon) {
      on.exit({
        dbPreExists <- attr(connection, "dbPreExists")
        dbname <- attr(connection, "dbname")
        if (!missing(dbname) && !is.null(dbname) && dbname ==
            ":memory:") {
          if (verbose) {
            cat("sqldf: dbDisconnect(connection)\n")
          }
          dbDisconnect(connection)
        } else if (!dbPreExists && drv == "sqlite") {
          if (verbose) {
            cat("sqldf: dbDisconnect(connection)\n")
            cat("sqldf: file.remove(dbname)\n")
          }
          dbDisconnect(connection)
          file.remove(dbname)
        } else {
          for (nam in dfnames) {
            nam2 <- backquote.maybe(nam)
            if (verbose) {
              cat("sqldf: dbRemoveTable(connection, ",
                  nam2, ")\n")
            }
            dbRemoveTable(connection, nam2)
          }
          for (fo in fileobjs) {
            if (verbose) {
              cat("sqldf: dbRemoveTable(connection, ",
                  fo, ")\n")
            }
            dbRemoveTable(connection, fo)
          }
          if (verbose) {
            cat("sqldf: dbDisconnect(connection)\n")
          }
          dbDisconnect(connection)
        }
      }, add = TRUE)
      if (request.close) {
        if (identical(connection, getOption("sqldf.connection")))
          options(sqldf.connection = NULL)
        return()
      }
    }
    if (request.open || request.nocon) {
      if (is.null(drv) || drv == "") {
        drv <- if ("package:RPostgreSQL" %in% search()) {
          "PostgreSQL"
        }
        else if ("package:RpgSQL" %in% search()) {
          "pgSQL"
        }
        else if ("package:RMySQL" %in% search()) {
          "MySQL"
        }
        else if ("package:RH2" %in% search()) {
          "H2"
        }
        else "SQLite"
      }
      drv <- sub("^[Rr]", "", drv)
      pkg <- paste("R", drv, sep = "")
      if (verbose) {
        if (!is.loaded(pkg))
          cat("sqldf: library(", pkg, ")\n", sep = "")
        library(pkg, character.only = TRUE)
      }
      else library(pkg, character.only = TRUE)
      drv <- tolower(drv)
      if (drv == "mysql") {
        if (verbose)
          cat("sqldf: m <- dbDriver(\"MySQL\")\n")
        m <- dbDriver("MySQL")
        if (missing(dbname) || is.null(dbname)) {
          dbname <- getOption("RMySQL.dbname")
          if (is.null(dbname))
            dbname <- "test"
        }
        connection <- if (missing(dbname) || dbname == ":memory:") {
          dbConnect(m)
        }
        else dbConnect(m, dbname = dbname)
        dbPreExists <- TRUE
      }
      else if (drv == "postgresql") {
        if (verbose)
          cat("sqldf: m <- dbDriver(\"PostgreSQL\")\n")
        m <- dbDriver("PostgreSQL")
        if (missing(user) || is.null(user)) {
          user <- getOption("sqldf.RPostgreSQL.user")
          if (is.null(user))
            user <- "postgres"
        }
        if (missing(password) || is.null(password)) {
          password <- getOption("sqldf.RPostgreSQL.password")
          if (is.null(password))
            password <- "postgres"
        }
        if (missing(dbname) || is.null(dbname)) {
          dbname <- getOption("sqldf.RPostgreSQL.dbname")
          if (is.null(dbname))
            dbname <- "test"
        }
        if (missing(host) || is.null(host)) {
          host <- getOption("sqldf.RPostgreSQL.host")
          if (is.null(host))
            host <- "localhost"
        }
        if (missing(port) || is.null(port)) {
          port <- getOption("sqldf.RPostgreSQL.port")
          if (is.null(port))
            port <- 5432
        }
        connection.args <- list(m, user = user, password,
                                dbname = dbname, host = host, port = port)
        connection.args.other <- getOption("sqldf.RPostgreSQL.other")
        if (!is.null(connection.args.other))
          connection.args <- modifyList(connection.args,
                                        connection.args.other)
        connection <- do.call("dbConnect", connection.args)
        if (verbose) {
          cat(sprintf("sqldf: connection <- dbConnect(m, user='%s', password=<...>, dbname = '%s', host = '%s', port = '%s', ...)\n",
                      user, dbname, host, port))
          if (!is.null(connection.args.other)) {
            cat("other connection arguments:\n")
            print(connection.args.other)
          }
        }
        dbPreExists <- TRUE
      }
      else if (drv == "pgsql") {
        if (verbose)
          cat("sqldf: m <- dbDriver(\"pgSQL\")\n")
        m <- dbDriver("pgSQL")
        if (missing(dbname) || is.null(dbname)) {
          dbname <- getOption("RpgSQL.dbname")
          if (is.null(dbname))
            dbname <- "test"
        }
        connection <- dbConnect(m, dbname = dbname)
        dbPreExists <- TRUE
      }
      else if (drv == "h2") {
        if (verbose)
          cat("sqldf: m <- dbDriver(\"H2\")\n")
        m <- dbDriver("H2")
        if (missing(dbname) || is.null(dbname))
          dbname <- ":memory:"
        dbPreExists <- dbname != ":memory:" && file.exists(dbname)
        connection <- if (missing(dbname) || is.null(dbname) ||
                          dbname == ":memory:") {
          dbConnect(m, "jdbc:h2:mem:", "sa", "")
        }
        else {
          jdbc.string <- paste("jdbc:h2", dbname, sep = ":")
          dbConnect(m, jdbc.string)
        }
      }
      else {
        if (verbose)
          cat("sqldf: m <- dbDriver(\"SQLite\")\n")
        m <- dbDriver("SQLite")
        if (missing(dbname) || is.null(dbname))
          dbname <- ":memory:"
        dbPreExists <- dbname != ":memory:" && file.exists(dbname)
        dll <- getOption("sqldf.dll")
        if (length(dll) != 1 || identical(dll, FALSE) ||
            nchar(dll) == 0) {
          dll <- FALSE
        }
        else {
          if (dll == basename(dll))
            dll <- Sys.which(dll)
        }
        options(sqldf.dll = dll)
        if (!identical(dll, FALSE)) {
          if (verbose) {
            cat("sqldf: connection <- dbConnect(m, dbname = \"",
                dbname, "\", loadable.extensions = TRUE\n",
                sep = "")
            cat("sqldf: select load_extension('", dll,
                "')\n", sep = "")
          }
          connection <- dbConnect(m, dbname = dbname, loadable.extensions = TRUE)
          s <- sprintf("select load_extension('%s')", dll)
          dbGetQuery(connection, s)
        }
        else {
          if (verbose) {
            cat("sqldf: connection <- dbConnect(m, dbname = \"",
                dbname, "\")\n", sep = "")
          }
          connection <- dbConnect(m, dbname = dbname)
        }
        if (verbose)
          cat("sqldf: initExtension(connection)\n")
        initExtension(connection)
      }
      attr(connection, "dbPreExists") <- dbPreExists
      if (missing(dbname) && drv == "sqlite")
        dbname <- ":memory:"
      attr(connection, "dbname") <- dbname
      if (request.open) {
        options(sqldf.connection = connection)
        return(connection)
      }
    }
    if (request.con) {
      drv <- if (inherits(connection, "PostgreSQLConnection"))
        "PostgreSQL"
      else if (inherits(connection, "pgSQLConnection"))
        "pgSQL"
      else if (inherits(connection, "MySQLConnection"))
        "MySQL"
      else if (inherits(connection, "H2Connection"))
        "H2"
      else "SQLite"
      drv <- tolower(drv)
      # ANDRE FIX: IF A CONNECTION WAS EXTERNALLY PROVIDED THEN 
      # ASSUME THAT THE DATABASE DOES NOT NEED TO BE CREATED ( IT IS ALREADY THERE )
      # IN CONTRACT TO SQLITE ( THAT CAN HAVE ITS DATABASE CREATED )
      dbPreExists <- attr(connection, "dbPreExists") <- TRUE
      # END ANDRE FIX
      ## OLD BEGIN
      ## dbPreExists <- attr(connection, "dbPreExists")
      ## OLD END
    }
    engine <- getOption("gsubfn.engine")
    if (is.null(engine) || is.na(engine) || engine == "") {
      engine <- if (require("tcltk"))
        "tcl"
      else "R"
    }
    else if (engine == "tcl")
      require("tcltk")
    words. <- words <- if (engine == "tcl") {
      strapplyc(x, "[[:alnum:]._]+")
    }
    else strapply(x, "[[:alnum:]._]+", engine = "R")
    if (length(words) > 0)
      words <- unique(unlist(words))
    is.special <- sapply(mget(words, envir, "any", NA, inherits = TRUE),
                         function(x) is.data.frame(x) + 2 * inherits(x, "file"))
    dfnames <- words[is.special == 1]
    for (i in seq_along(dfnames)) {
      nam <- dfnames[i]
      if (dbPreExists && !overwrite && dbExistsTable(connection,
                                                     nam)) {
        dfnames <- head(dfnames, i - 1)                       # ANDRE# SHOULD ALSO BE # DIFFERENT # ?
        stop(paste("sqldf:", "table", nam, "already in",
                   dbname, "\n"))
      }
      DF <- as.data.frame(get(nam, envir))
      if (!is.null(to.db) && is.function(to.db))
        DF <- to.db(DF)
      nam2 <- backquote.maybe(nam)
      if (verbose)
        cat("sqldf: dbWriteTable(connection, '", nam2, "', ",
            nam, ", row.names = ", row.names, ")\n", sep = "")
      dbWriteTable(connection, nam2, DF, row.names = row.names)
    }
    fileobjs <- if (is.null(file.format)) {
      character(0)
    }
    else {
      eol <- if (.Platform$OS == "windows")
        "\r\n"
      else "\n"
      words[is.special == 2]
    }
    for (i in seq_along(fileobjs)) {
      fo <- fileobjs[i]
      Filename <- summary(get(fo, envir))$description
      if (dbPreExists && !overwrite && dbExistsTable(connection,
                                                     Filename)) {
        fileobjs <- head(fileobjs, i - 1)
        stop(paste("sqldf:", "table", fo, "from file", Filename,
                   "already in", dbname, "\n"))
      }
      args <- c(list(conn = connection, name = fo, value = Filename),
                modifyList(list(eol = eol), file.format))
      args <- modifyList(args, as.list(attr(get(fo, envir),
                                            "file.format")))
      filter <- args$filter
      if (!is.null(filter)) {
        args$filter <- NULL
        Filename.tmp <- tempfile()
        args$value <- Filename.tmp
        filter.subs <- filter[-1]
        if (length(filter.subs) > 0) {
          filter.subs <- filter.subs[sapply(names(filter.subs),
                                            nzchar)]
        }
        filter.nms <- names(filter.subs)
        filter.tempfiles <- sapply(filter.nms, tempfile)
        cmd <- filter[[1]]
        for (nm in filter.nms) {
          cat(filter.subs[[nm]], file = filter.tempfiles[[nm]])
          cmd <- gsub(nm, filter.tempfiles[[nm]], cmd,
                      fixed = TRUE)
        }
        cmd <- if (nchar(Filename) > 0)
          sprintf("%s < \"%s\" > \"%s\"", cmd, Filename,
                  Filename.tmp)
        else sprintf("%s > \"%s\"", cmd, Filename.tmp)
        if (.Platform$OS == "windows") {
          cmd <- paste("cmd /c", cmd)
          if (FALSE) {
            key <- "SOFTWARE\\R-core"
            show.error.messages <- getOption("show.error.message")
            options(show.error.messages = FALSE)
            reg <- try(readRegistry(key, maxdepth = 3)$Rtools$InstallPath)
            reg <- NULL
            options(show.error.messages = show.error.messages)
            if (!is.null(reg) && !inherits(reg, "try-error")) {
              Rtools.path <- file.path(reg, "bin", fsep = "\\")
              path <- Sys.getenv("PATH")
              on.exit(Sys.setenv(PATH = path), add = TRUE)
              path.new <- paste(path, Rtools.path, sep = ";")
              Sys.setenv(PATH = path.new)
            }
          }
        }
        if (verbose)
          cat("sqldf: system(\"", cmd, "\")\n", sep = "")
        system(cmd)
        for (fn in filter.tempfiles) file.remove(fn)
      }
      if (verbose)
        cat("sqldf: dbWriteTable(", toString(args), ")\n")
      do.call("dbWriteTable", args)
    }
    if (drv == "sqlite" || drv == "mysql" || drv == "postgresql") {
      for (xi in x) {
        if (verbose) {
          cat("sqldf: dbGetQuery(connection, '", xi, "')\n",
              sep = "")
        }
        rs <- dbGetQuery(connection, xi)
      }
    }
    else {
      for (i in seq_along(x)) {
        if (length(words.[[i]]) > 0) {
          dbGetQueryWords <- c("select", "show", "call",
                               "explain", "with")
          if (tolower(words.[[i]][1]) %in% dbGetQueryWords ||
              drv != "h2") {
            if (verbose) {
              cat("sqldf: dbGetQuery(connection, '", x[i],
                  "')\n", sep = "")
            }
            rs <- dbGetQuery(connection, x[i])
          }
          else {
            if (verbose) {
              cat("sqldf: dbSendUpdate:", x[i], "\n")
            }
            rs <- get("dbSendUpdate")(connection, x[i])
          }
        }
      }
    }
    if (is.null(to.df))
      to.df <- "auto"
    if (is.function(to.df))
      return(to.df(rs))
    if (identical(to.df, "raw"))
      return(rs)
    if (identical(to.df, "name__class"))
      return(do.call("name__class", list(rs)))
    if (!identical(to.df, "nofactor") && !identical(to.df, "auto")) {
      return(do.call("colClass", list(rs, to.df)))
    }
    row_names_name <- grep("row[_.]names", names(rs), value = TRUE)
    if (length(row_names_name) > 1)
      warning(paste("ambiguity regarding row names:", row_names_name))
    row_names_name <- row_names_name[1]
    rs <- if (!is.na(row_names_name)) {
      if (identical(row.names, FALSE)) {
        rs[names(rs) != row_names_name]
      }
      else {
        rn <- rs[[row_names_name]]
        rs <- rs[names(rs) != row_names_name]
        if (all(regexpr("^[[:digit:]]*$", rn) > 0))
          rn <- as.integer(rn)
        rownames(rs) <- rn
        rs
      }
    }
    else rs
    tab <- do.call("rbind", lapply(dfnames, function(dfname) {
      df <- get(dfname, envir)
      nms <- names(df)
      do.call("rbind", lapply(seq_along(df), function(j) {
        column <- df[[j]]
        cbind(dfname, nms[j], toString(class(column)), toString(levels(column)))
      }))
    }))
    tabu <- unique(tab[, -1, drop = FALSE])
    dup <- unname(tabu[duplicated(tabu[, 1]), 1])
    auto <- function(i) {
      cn <- colnames(rs)[[i]]
      if (!cn %in% dup && (ix <- match(cn, tab[, 2], nomatch = 0)) >
          0) {
        df <- get(tab[ix, 1], envir)
        if (inherits(df[[cn]], "ordered")) {
          if (identical(to.df, "auto")) {
            u <- unique(rs[[i]])
            levs <- levels(df[[cn]])
            if (all(u %in% levs))
              return(factor(rs[[i]], levels = levels(df[[cn]]),
                            ordered = TRUE))
            else return(rs[[i]])
          }
          else return(rs[[i]])
        }
        else if (inherits(df[[cn]], "factor")) {
          if (identical(to.df, "auto")) {
            u <- unique(rs[[i]])
            levs <- levels(df[[cn]])
            if (all(u %in% levs))
              return(factor(rs[[i]], levels = levels(df[[cn]])))
            else return(rs[[i]])
          }
          else return(rs[[i]])
        }
        else if (inherits(df[[cn]], "POSIXct"))
          return(as.POSIXct(rs[[i]]))
        else if (inherits(df[[cn]], "times"))
          return(as.times.character(rs[[i]]))
        else {
          asfn <- paste("as", class(df[[cn]]), sep = ".")
          asfn <- match.fun(asfn)
          return(asfn(rs[[i]]))
        }
      }
      if (stringsAsFactors && is.character(rs[[i]]))
        factor(rs[[i]])
      else rs[[i]]
    }
    rs2 <- lapply(seq_along(rs), auto)
    rs[] <- rs2
    rs
  }


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

insert_df(iris[1:26,],letters,"Let",3)
# DataCombine::MoveFront
# insert_df(iris[1:26,],letters,"Let",0)
##   Let Sepal.Length Sepal.Width Petal.Length Petal.Width Species
##1    a          5.1         3.5          1.4         0.2  setosa
##2    b          4.9         3.0          1.4         0.2  setosa

#   Sepal.Length Sepal.Width Petal.Length Let Petal.Width Species
##1           5.1         3.5          1.4   a         0.2  setosa
##2           4.9         3.0          1.4   b         0.2  setosa

# ANDRE



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
                 !str_detect(colnames(df),"^updated$")
  , drop = FALSE] -> df
  
  return(df)

}



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
    # uses insert_df
    
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
          insert_df(., .$company_id, "company_id_orig", match("company_id", colnames(.)) - 1 ) -> si_si_tbl_df
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

      financize(si_all_df) -> si_all_df
       optimize(si_all_df) -> si_all_df
      
      rm("si_mgdsc")
      
      return(si_all_df)
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



# (WORK IN PROGRESS)
# WORK ON 'grabbing future' company_ids
# --------------------------------------

update_from_future_new_company_ids <- function(df = NULL, ref = NULL) {
  
  require(magrittr)
  require(lubridate)
  require(stringr)
  require(PivotalR)
  
  # uses zoo::as.Date, foreign::read.dbf
  # uses lwd_of_month, rm_df_dups
  
  # zoo::as.Date(ref) %m+% months(1:13) %>% lwd_of_month(x) %>% zoo::as.Date(.)
  #    "2013-02-28"
  
  #  [1] 15793 15825 15856 15884 15917 15947 15978 16009 16038 16070 16101 16129
  # [13] 16160
  
  #  [1] "2013-03-29" "2013-04-30" "2013-05-31" "2013-06-28" "2013-07-31"
  #  [6] "2013-08-30" "2013-09-30" "2013-10-31" "2013-11-29" "2013-12-31"
  # [11] "2014-01-31" "2014-02-28" "2014-03-31"
  
  verify_connection()
  

  # just work with the core
  if("DFI" %in% class(df)) df <- df$x
  
                    # must be a data.frame ALONE
  trg_db_temp    <- as.db.data.frame(as.data.frame(df), conn.id = cid, key = "company_id", verbose = FALSE)
  trg_db_temp_nm <- trg_db_temp@.content
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
  # expect 2 SQL updates
  
  # will downsize into integers ( 3 months ahead is sufficient to find a lost link )
  for(month_i in zoo::as.Date(ref) %m+% months(1:3)) {
    
    lwd_of_month(month_i) -> lwd
    
    print(str_c("Looking in direction at ... ", zoo::as.Date(lwd)," ",lwd," Maybe in "))
    if(file.exists(str_c("W:/AAIISIProDBFs/",lwd))) { print(str_c("  Exists: ","W:/AAIISIProDBFs/",lwd)) }
    
    suppressWarnings(suppressMessages(foreign::read.dbf(file = str_c("W:/AAIISIProDBFs/",lwd,"/si_ci.dbf")
                                                        , as.is = TRUE))) %>% lcase_a_remove_useless_columns(.) %>%
      ## setNames(.,tolower(colnames(.))) %>%
      rm_df_dups(.,c("company_id","ticker"))  -> trg_db
    
    src_db_temp    <- as.db.data.frame(trg_db, conn.id = cid, key = "company_id", verbose = FALSE)
    src_db_temp_nm <- src_db_temp@.content
    db.q(str_c("drop table if exists src"), conn.id = cid)
    db.q(str_c("create table if not exists src as select * from ", src_db_temp_nm, collapse = ""), conn.id = cid)
    db.q(str_c("drop index if exists src_company_id_idx"), conn.id = cid)
    db.q(str_c("create index if not exists src_company_id_idx on src(company_id)"), conn.id = cid)
    db.q(str_c("drop index if exists src_ticker_idx"), conn.id = cid)
    db.q(str_c("create index if not exists src_ticker_idx on src(ticker)"), conn.id = cid)
    db.q(str_c("drop index if exists src_company_idx"), conn.id = cid)
    db.q(str_c("create index if not exists src_company_idx on src(company)"), conn.id = cid)
    
    # UPDATE 1
    db.q(str_c("
               update trg
                 set company_id =    src.company_id
               from src
                 where trg.company_id != src.company_id and
                       trg.ticker      = src.ticker
    "), nrows =  -1, conn.id = cid)
    
    ## MAY? WANT TO SHUT OFF DURING DEVELOPMENT ( THIS TAKES 5 SECONDS TO RUN)
    
    # UPDATE 2 ( UPDATE 1 IS REQUIRED )
    db.q(str_c("
              update trg
              set company_id = src.company_id
                      from src
                    where trg.company_id != src.company_id and
                          trg.ticker     != src.ticker and
                          trg.company     = src.company  -- 117
              and trg.company not in -- IN -> NOT_IN -> ELMINTATES DUPLICATED COMPANY NAMES
              (select trg.company 
              from trg
              where trg.company
              in ( 
                select src.company
                   from trg, src
                    where trg.company_id = src.company_id and
                          trg.ticker     = src.ticker 
                )
              ) -- 99 rows ( 5 second update in PgAdminIII)
               "), nrows =  -1, conn.id = cid)
    
    ci_tk <- db.q(str_c("select company_id, ticker from trg"), nrows =  -1, conn.id = cid)
    df[,"company_id"] <- ci_tk[,"company_id"]
    
    print(str_c("Done looking in direction at ... ", zoo::as.Date(lwd)," ",lwd," Maybe in "))
  }
  
  db.q(str_c("drop table if exists trg"), conn.id = cid)
  db.q(str_c("drop table if exists src"), conn.id = cid)
  
  optimize(df) -> df

  return(df)
  
}
# and   ci.dateindex   in (
#   14911,
#   14943,
#   14974,
#   15005,
#   15033,
#   15064,
#   15093,
#   15125,
#   15155
# )
# and ci_f.dateindex = 15184;

## NOT THIS TEST verify_company_basics(dateindex = c(15764)) -> si_all_g_df
# verify_company_basics(dateindex = c(15155)) -> si_all_g_df
# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid")))
# update_from_future_new_company_ids(si_all_g_df,15155) -> si_all_g_df
#   PROPER WAY TO RUN ... BACKWARDS (THIS MONTH AND GO BACK THREE DAYS)
#   SO INITIAL LOADING IS FROM *NOW* TO *EARLIEST*
# LEFT_oFF
#   FAR FUTURE: LOOP THROUGH and UPDATE COMPANY IDS
# NEXT_TIME
#   CAROLINE_LIKE LOAD (update) si_finecon2 # START? si_isq
#   update_si_fe(
#       dateindex = c(15155,15184)
#     , tbls_regex = c("^si_ci$","^si_isq$")
#     , cols_regx = c("AAA_q.*|","|")
#   ) # or "|" separators
#

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
  
 


  
