Systematic Investor Toolkit
https://github.com/systematicinvestor/SIT/R/data.r
ALSO IN 
( FOR LONG-TERM SAFETY: I HAD FORKED TO 
https://github.com/AndreMikulec/SIT
( SHOULD BE STILL THE SAME ON OCT 10, 2013 )

    sp500.components <- function()
    {
      url = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
      txt = join(readLines(url))
      
      # extract table from this page	
      temp = extract.table.from.webpage(txt, 'Ticker', hasHeader = T)
      tickers = temp[, 'Ticker symbol']
      sector = temp[, 'GICS Sector']

      return(list(tickers=tickers, sector=sector))
    }




Get and save the S & P 500 Historical fundamental data
--------------------------------------------------

N:
mkdir "S_AND_P_500_DATA"
cd    "S_AND_P_500_DATA"


library(SIT)

sp500 <- sp500.components()
totalRobjectsCount <- length(sp500[["tickers"]])

i <- 0
while (i < totalRobjectsCount ) { 
  print(paste(i+1, ":" ,  sp500[["tickers"]][i+1]   ,sep=' ') )
  
  symbolip1 <- sp500[["tickers"]][i+1]
  fundip1   <- fund.data(symbolip1, 80)

  # SAVES WITH IT:  ASSOC VARIABLE fundip1
  save(fundip1, file = paste (symbolip1,".Rdata",sep='') )
  save(fundip1, file = paste (symbolip1,".txt",sep='') ,ascii = TRUE)

  i <- i + 1 
}

TUESDAY SEPT 29   7:50 PM STARTED
TUESDAY SEPT 29 APPROX_3_HOURS LATER EMDED ( HAD SOME DATA: DID NOT HAVE SOME DATA )

# later LOAD
# load(paste(symbolip1,".Rdata",sep=''), .GlobalEnv)
# load(paste(symbolip1,".txt"  ,sep=''), .GlobalEnv, verbose=TRUE)




How to get a list of stocks symbol of a specific exchange? [duplicate]
-----------------------------------------------------------------------

Edit: With TTR_0.20-3 from r-forge, 
the list appears more complete than with the prior version:
Aug 16 '11' 

  TTR_0.2 now on CRAN
  Joshua Ulrich - 2009-02-20 
  https://r-forge.r-project.org/projects/ttr/

require(TTR)

SYMs <- TTR::stockSymbols()  # NOTE: stockSymbols() SEEMS UNDOCUMENTED

#Fetching AMEX symbols...
#Fetching NASDAQ symbols...
#Fetching NYSE symbols...

table(SYMs$Exchange)

> SYMs <- TTR::stockSymbols()
Fetching AMEX symbols...
Fetching NASDAQ symbols...
Fetching NYSE symbols...
>
head(table(SYMs$Exchange))

  AMEX NASDAQ   NYSE
   447   2730   3259

table(SYMs$Exchange)

  AMEX NASDAQ   NYSE
   447   2730   3259

str(SYMs)
'data.frame':   6436 obs. of  8 variables:
 $ Symbol   : chr  "AA-P" "AAMC" "AAU" "ACU" ...
 $ Name     : chr  "Alcoa Inc." "Altisource Asset Management Corp" "Almaden Mine
 $ LastSale : num  73 533 1.4 15.1 20.4 ...
 $ MarketCap: num  0.00 1.25e+09 9.03e+07 4.80e+07 3.15e+07 ...
 $ IPOyear  : int  NA NA NA 1988 NA NA NA NA NA NA ...
 $ Sector   : chr  "Capital Goods" "Finance" "Basic Industries" "Capital Goods"
 $ Industry : chr  "Metal Fabrications" "Real Estate" "Precious Metals" "Industr
 $ Exchange : chr  "AMEX" "AMEX" "AMEX" "AMEX" ...

http://quant.stackexchange.com/questions/1735/how-to-get-a-list-of-stocks-symbol-of-a-specific-exchange




get a ton of historical data
----------------------------

dependent R functions of 
of October 10, 2013
Systematic Investor Toolkit
https://github.com/systematicinvestor/SIT

( FOR LONG-TERM SAFETY: I HAD FORKED TO 
https://github.com/AndreMikulec/SIT


(SOME) MOTIVATION FOR USAGE
  Multiple Factor Model – Fundamental Data
  January 29, 2012 systematicinvestor

# NOTE: Below SIT I Installed as an R package
# Basically to create
# ( Possibly) edit .gitignore the SIT directory  /SIT/
#   Next, run R -file make.sit.pkg.r
# https://github.com/AndreMikulec/SIT/blob/master/make.sit.pkg.r
# BUT I was working out of the local git repository
#    M:\YDrive\All_Economics\eclipse_workspace\SIT
# SO I HAD TO DO SOME "RAZZLE DAZZLE"

--NEW ( WORKS )

N:
REM mkdir "AMEX_NASDAQ_NYSE_DATA"
cd        "AMEX_NASDAQ_NYSE_DATA"

R

library(SIT)

fund.data31 <-  function (Symbol, n = 10, mode = c("quarterly", "annual"), max.attempts = 5)
{
  all.data = c()
  option.value = -1
  start_date = spl("istart_date,start_date")
  names(start_date) = spl("quarterly,annual")
  repeat {
    if (option.value >= 0) {
      url = paste("http://uk.advfn.com/p.php?pid=financials&symbol=",
                  Symbol, "&btn=", mode[1], "_reports&", start_date[mode[1]],
                  "=", option.value, sep = "")
    }
    else {
      url = paste("http://uk.advfn.com/p.php?pid=financials&symbol=",
                  Symbol, "&btn=", mode[1], "_reports", sep = "")
    }
    cat("Downloading", url, "\n")
    for (iattempt in 1:max.attempts) {
      flag = T
      tryCatch({
        txt = join(readLines(url))
      }, interrupt = function(ex) {
        flag <<- F
        Sys.sleep(0.1)
      }, error = function(ex) {
        flag <<- F
        Sys.sleep(0.1)
      }, finally = {
        if (flag)
          break
      })
    }
    if (len(grep("INDICATORS", txt, ignore.case = T)) ==
          0) {
      cat("No Data Found for", Symbol, "\n")
      return(all.data)
    }
    
    HTMLOPENTITLETAGposStart    <- regexpr(pattern="<title>", txt,ignore.case=TRUE)[1]
    HTMLCLOSETITLETAGposStart   <- regexpr(pattern="</title>",txt,ignore.case=TRUE)[1]
    HTMLOPENTITLETAGlength      <- nchar("<title>")
    HTMLTITLEtext <- substr(txt, HTMLOPENTITLETAGposStart  + HTMLOPENTITLETAGlength , HTMLCLOSETITLETAGposStart - 1 )

    data = extract.table.from.webpage(txt, "INDICATORS",
                                      hasHeader = T)
    colnames(data) = data[1, ]
    rownames(data) = data[, 1]
    data = data[, -1, drop = F]
    add.index = which(is.na(match(colnames(data), colnames(all.data))))
    all.data = cbind(data[, add.index, drop = F], all.data)
    if (ncol(all.data) >= n)
      break
    if (option.value == 0)
      break
    temp = gsub(pattern = "<option", replacement = "<tr>",
                txt, perl = TRUE)
    temp = gsub(pattern = "</option>", replacement = "</tr>",
                temp, perl = TRUE)
    temp = extract.table.from.webpage(temp, "All amounts",
                                      hasHeader = T)
    temp = apply(temp, 1, join)
    index.selected = grep("selected", temp)
    option.value = 0
    if (len(index.selected))
      option.value = as.double(gsub(".*value='([0-9]*).*",
                                    "\\1", temp[index.selected]))
    if (option.value > 0) {
      option.value = option.value - 5
      option.value = max(0, option.value)
    }
    else {
      break
    }
  }
  all.data = all.data[, colSums(nchar(trim(all.data))) > 0]
  # if converted to a vector, then make it a matrix again
  if (is.vector(all.data)) {
    all.data.temp <- matrix(all.data,nrow=length(all.data))
    rownames(all.data.temp) <- names(all.data)
    colnames(all.data.temp) <- all.data.temp[1, ]
    all.data <- all.data.temp
    all.data.temp <- matrix(nrow=0, ncol=0)
  }
  if (ncol(all.data) > n) {
    all.data <- all.data[, (ncol(all.data) - n + 1):ncol(all.data)]
    # if converted to a vector, then make it a matrix again
    if (is.vector(all.data)) {
      all.data.temp <- matrix(all.data,nrow=length(all.data))
      rownames(all.data.temp) <- names(all.data)
      colnames(all.data.temp) <- all.data.temp[1, ]
      all.data <- all.data.temp
      all.data.temp <- matrix(nrow=0, ncol=0)
    }
    # add a row of the HTMLTITLEtext values
    all.data <- rbind(all.data, rep( HTMLTITLEtext, ncol(all.data) ) )
    # to the new 'added row' name it "HTMLTITLEtext"
    rownames(all.data)[nrow(all.data)] <- "HTMLTITLEtext"
    return(all.data)
  }
  else {
    # add a row of the HTMLTITLEtext values
    all.data <- rbind(all.data, rep( HTMLTITLEtext, ncol(all.data) ) )
    # to the new 'added row' name it "HTMLTITLEtext"
    rownames(all.data)[nrow(all.data)] <- "HTMLTITLEtext"
    return(all.data)
  }
}


require(TTR)

SYMs <- TTR::stockSymbols()  # NOTE: stockSymbols() SEEMS UNDOCUMENTED

save(SYMs, file = "SYMsBefore.Rdata" )
save(SYMs, file = "SYMsBefore.txt", ascii = TRUE)

totalRobjectsCount <- length(SYMs[["Symbol"]])

Sys.time()
i <- 0
while (i < totalRobjectsCount ) { 
  print(paste(i+1, ":", paste (SYMs[["Exchange"]][i+1], ":", SYMs[["Symbol"]][i+1], sep="" ) , sep=' ') )
  
  symbolip1 <- paste (SYMs[["Exchange"]][i+1], ":", SYMs[["Symbol"]][i+1], sep="" )  
  fundip1   <- fund.data31(symbolip1, 80)

  # SAVES WITH IT:  ASSOC VARIABLE fundip1
  save(fundip1, file = paste (sub(":","_",symbolip1),".Rdata",sep='') )
  save(fundip1, file = paste (sub(":","_",symbolip1),".txt",sep='') ,ascii = TRUE)

  i <- i + 1 
}
Sys.time()

save(SYMs, file = "SYMsAfter.Rdata" )
save(SYMs, file = "SYMsAfter.txt", ascii = TRUE)

# later LOAD
# load(paste(sub(":","_",symbolip1),".Rdata",sep=''), .GlobalEnv)
# load(paste(sub(":","_",symbolip1),".txt"  ,sep=''), .GlobalEnv, verbose=TRUE)

# IN PROGRESS ... STARTED 11:28 P.M. ON THURSDAY, OCT 10, 2013
# DONE 23:30 MINUTES LATER

--END OF NEW





############################
# START EXECUTABLE AREA HERE

# N:
# cd "AMEX_NASDAQ_NYSE_DATA"
# R --vanilla

# FILES O ( ALL AND LOCATION SPECIFIC EXCHANGES )
all_exch_ticker <- vector(mode="character")
# dirdata <- vector(length(dir()), mode="list")
file_number <- 0;
for(x in dir(pattern = ".*_.*\\.Rdata")){
  file_number = file_number+1
  # dirdata[[file_number]] <- read.table(file=x)
  dot_found_pos <- regexpr("\\." ,x, ignore.case = FALSE, perl = TRUE)[1]
  all_exch_ticker[file_number] <- substr(x, 1, dot_found_pos  -1 )
  # print(x)
}
file_number
# [1] 6445
all_exch_ticker

# HYPHENS FILES ONLY ( LOCATION SPECIFIC EXCHANGES )
hyph_exch_ticker <- vector(mode="character")
# dirdata <- vector(length(dir()), mode="list")
file_number <- 0;
for(x in dir(pattern = ".*_.*-.*\\.Rdata")){
  file_number = file_number+1
  # dirdata[[file_number]] <- read.table(file=x)
  dot_found_pos <- regexpr("\\." ,x, ignore.case = FALSE, perl = TRUE)[1]
  hyph_exch_ticker[file_number] <- substr(x, 1, dot_found_pos  -1 )
  # print(x)
}
file_number
# [581] 
hyph_exch_ticker

# WORKS
# setdiff(c("a","c","b"),c("a","b"))

nohyph_exch_ticker <- setdiff(all_exch_ticker,hyph_exch_ticker)
# 5864

length(nohyph_exch_ticker)

# END EXECUTABLE AREA HERE
############################



############################
# BEGIN EXECUTABLE AREA HERE

Sys.time()
firmshistory <- list()
file_number <- 0;
for(x in nohyph_exch_ticker){
  file_number = file_number+1
  fundip1 <- NULL
  load(file=paste(x,".Rdata",sep=""))
  firmshistory[[x]] <- fundip1
  fundip1 <- NULL
  if ( file_number %% 100 == 0 ) {
    print(paste(file_number," completed.",sep=""))

  }
  # testing uses 1000
  if ( file_number %% 10000 == 0 ) {
    break
  }
}
Sys.time()

# > Sys.time()
# [1] "2013-10-26 18:40:31 CDT"
# ...
# [1] "5600 completed."
# [1] "5700 completed."
# [1] "5800 completed."
# > Sys.time()
# [1] "2013-10-26 19:00:39 CDT"
# 20 MINUTES

# 1000 ( 820 entries ) (34 seconds  )
length(firmshistory)
# 1000 ( 820 entries )

# > length(firmshistory)
# [1] 4734


# 1000 ( 820 entries ) ( 21 seconds ) ( 118M )
Sys.time()
save(firmshistory, file="firmshistory.Rdata")
firmshistory <- NULL
Sys.time()

# Sys.time()
# [1] "2013-10-26 19:16:17 CDT"
# 5900 ( 4734 entries ) ( 120 seconds ) ( 120M  STILL??? )
# Sys.time()
# [1] "2013-10-26 19:18:21 CDT"
# 

# 1000 ( 820 entries ) ( 16 seconds )
# 5900 ( 4734 entries ) ( 100 seconds ) 
Sys.time()
load(file="firmshistory.Rdata")
Sys.time()

# > Sys.time()
# [1] "2013-10-26 19:28:24 CDT"
# > Sys.time()
# [1] "2013-10-26 19:29:45 CDT"
# MEMORY HOLDS STEADY AT 566M

length(firmshistory)

# > length(firmshistory) 
# [1] 4734

# END EXECUTABLE AREA HERE
############################



############################
# BEGIN EXECUTABLE AREA HERE

Sys.time()
load(file="firmshistory.Rdata")
Sys.time()

firm_index <- 0 
for ( x in firmshistory ) {
  firm_index <- firm_index + 1

  # add the row data only
  EXCHANGE_TICKERtext <- names(firmshistory)[firm_index]
  firmshistory[[firm_index]] <- rbind(firmshistory[[firm_index]], rep( EXCHANGE_TICKERtext , ncol(firmshistory[[firm_index]]) ) )

  # add the 'row name'
  rownames(firmshistory[[firm_index]])[nrow(firmshistory[[firm_index]])] <- "EXCHANGE_TICKERtext"

  # testing
  if ( firm_index == 100000 ) {
    print(firm_index)
    break
  }

  if ( firm_index %% 100 == 0 ) {
    print(paste(firm_index," completed.",sep=""))

  }
  # testing uses 1000
  if ( firm_index %% 10000 == 0 ) {
    break
  }
}
Sys.time()

# 13 SECONDS
# [1] "2013-10-27 16:27:33 CDT"
# [1] "4700 completed." ( JUST A LITTLE OVER )
# [1] "2013-10-27 16:27:45 CDT"

# TEST
# show me the data ( and 'row name' )
# tail(firmshistory[[1]],2) # JUST THE VERY LAST TWO ROWS

# show me the data ( and 'row name' )
# tail(firmshistory[[2]],2) # JUST THE VERY LAST TWO ROWS

Sys.time()
save(firmshistory, file="firmshistory_w_bottom_EXCHANGE_TICKERtext.Rdata")
firmshistory <- NULL
Sys.time()

#  120 seconds
# [1] "2013-10-27 16:34:20 CDT"
# [1] "2013-10-27 16:36:16 CDT"
# only 200K bigger
# N:\AMEX_NASDAQ_NYSE_DATA\firmshistory_w_bottom_EXCHANGE_TICKERtext.Rdata

Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext.Rdata")
Sys.time()

# 86 seconds
# [1] "2013-10-27 16:40:07 CDT"
# [1] "2013-10-27 16:41:26 CDT"

# TEST ( company 1  and company 2)
# show me the data ( and 'row name' )
# tail(firmshistory[[1]],2) # JUST THE VERY LAST TWO ROWS

# show me the data ( and 'row name' )
# tail(firmshistory[[2]],2) # JUST THE VERY LAST TWO ROWS

# END EXECUTABLE AREA HERE
############################



############################
# BEGIN EXECUTABLE AREA HERE 

Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext.Rdata")
Sys.time()

# 78 seconds
# [1] "2013-10-27 19:00:26 CDT"
# [1] "2013-10-27 19:01:44 CDT"

# TEST ( company 1  and company 2)
# show me the data ( and 'row name' )
# tail(firmshistory[[1]],2) # JUST THE VERY LAST TWO ROWS

# HTMLTITLEtext       "Almaden Minerals"
# EXCHANGE_TICKERtext "AMEX_AAU"

# show me the data ( and 'row name' )
# tail(firmshistory[[2]],2) # JUST THE VERY LAST TWO ROWS

# HTMLTITLEtext       "Acme United Corporation Company"
# EXCHANGE_TICKERtext "AMEX_ACU"

Sys.time()
firm_index <- 0 
for ( x in firmshistory ) {
  firm_index <- firm_index + 1

  for ( y in 1:nrow(firmshistory[[firm_index]])  ) {
    # print( rownames(firmshistory[[firm_index]])[y]   )
    rownames(firmshistory[[firm_index]])[y] <- paste0( names(firmshistory)[firm_index], ".", rownames(firmshistory[[firm_index]])[y] )
    # print(y)
    # print( rownames(firmshistory[[firm_index]])[y]   )
  }
  
  if ( firm_index %% 100 == 0 ) {
    print(paste(firm_index," completed.",sep=""))
  }
  
}
Sys.time()

# about 6 seconds per 100
# [1] "2013-10-27 19:16:55 CDT"
# [1] "2013-10-27 19:23:35 CDT"
# 6 minutes and 30 seconds

# TEST ( company 1  and company 2)
# show me the data ( and 'row name' )
# tail(firmshistory[[1]],2) # JUST THE VERY LAST TWO ROWS

# AMEX_AAU.HTMLTITLEtext       "Almaden Minerals"
# AMEX_AAU.EXCHANGE_TICKERtext "AMEX_AAU"

# show me the data ( and 'row name' )
# tail(firmshistory[[2]],2) # JUST THE VERY LAST TWO ROWS

# AMEX_ACU.HTMLTITLEtext       "Acme United Corporation Company"
# AMEX_ACU.EXCHANGE_TICKERtext "AMEX_ACU"

Sys.time()
save(firmshistory, file="firmshistory_w_bottom_EXCHANGE_TICKERtext_left_EXCHANGE_TICKER.Rdata")
firmshistory <- NULL
Sys.time()

# 120 seconds

Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext_left_EXCHANGE_TICKER.Rdata")
Sys.time()

# 101 seconds

# TEST ( company 1  and company 2)
# show me the data ( and 'row name' )
# tail(firmshistory[[1]],2) # JUST THE VERY LAST TWO ROWS

# AMEX_AAU.HTMLTITLEtext       "Almaden Minerals"
# AMEX_AAU.EXCHANGE_TICKERtext "AMEX_AAU"

# show me the data ( and 'row name' )
# tail(firmshistory[[2]],2) # JUST THE VERY LAST TWO ROWS

# AMEX_ACU.HTMLTITLEtext       "Acme United Corporation Company"
# AMEX_ACU.EXCHANGE_TICKERtext "AMEX_ACU"

# END EXECUTABLE AREA HERE
############################

############## BEGIN EXECUTABLE AREA #######################

# CREATE A STRONG SYMs matrix with rownames of EXCHANGE_TICKER primary key values

# load the symbols
load(file="SYMsBefore.Rdata")

# I need 'exchange and ticker' to be like a strong primary key
# actually convert to a matrix
SYMs <- as.matrix(SYMs)

# remove the SYMS rownames
rownames(SYMs) <- NULL 

# give the rownames  names the same name as the exchange_symbols columns
rownames(SYMs) <- paste(SYMs[,"Exchange"],SYMs[,"Symbol"],sep="_")

# make persistent
save(SYMs, file="SYMsBeforeDETAIL.Rdata")  # new: note: this is now a matrix

# check for a successfull 'bring into memory'
load(file="SYMsBeforeDETAIL.Rdata")

############## END EXECUTABLE AREA #######################




############## BEGIN EXECUTABLE AREA  #######################

# REQUIRED **** ( IF NOT ALEADY DONE )( SEE ABOVE )
# check for a successfull 'bring into memory'
load(file="SYMsBeforeDETAIL.Rdata")

# CREATE COLUMNS
# get the data from 
# SYMs matrix with rownames of EXCHANGE_TICKER primary key values
# column bind to the right
# give the column a new name
# move over to the left

# "MARKETCAP" "SECTOR" "INDUSTRY" "EXCHANGE_TICKER" "rownombres"

# run at the start of a debugging sessoin
# load(file="SYMsBeforeDETAIL.Rdata")
# load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext.Rdata")
# firmshistory1 <- firmshistory[[1]]
# firmshistory2 <- firmshistory[[2]]

# wider printing is needed
options(width = 255)

# REQUIRED ( 90 seconds ) ( 420 MB of RAM )
Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext.Rdata")
Sys.time()
# NOTE:    firmshistory_w_bottom_EXCHANGE_TICKERtext_left_EXCHANGE_TICKER.Rdata
#          IS NO LONGER BEING USED

# run at the top of EACH debugging batch run
# firmshistory[[1]] <- firmshistory1
# firmshistory[[2]] <- firmshistory2
# firm_index <- 1

# ANDRE NOTE: R does not do global variables
# THEREFORE: after the running function exits
# variables in the outside scope ARE NOT MODIFIED 

# runme <- function() {
  firm_index <- 0
  if ( length(firmshistory) > 0 ) {
    for ( x in firmshistory ) {
      firm_index <- firm_index + 1

      # real work begins in here
      
      # if debugging
      # ... browser()

      # Problem 3
      # if just ONE column ( and no column name  "" ) the situation in not salvagable
      # also I can not make any inference from 'just ONE quarter'
      # I need to remove this matrix list item and go on to the next
      # I can not use NULL to remove the list item, this will  mess up my loop count
      # at the end I should just remove those bad entries
      # for now I will 
      #   assign an empty matrix
      #   continue ( next ) to the following iteration
      if ( colnames(firmshistory[[firm_index]])[1] == "" &&
           ncol(firmshistory[[firm_index]]) < 2
      ) {
           firmshistory[[firm_index]] <- matrix(nrow = 0, ncol = 0)
           next
      }

      # Problem 1
      # earliest_by_date column sometimes is missing a column name
      # this is the fault of the row 1 item having a zero lenght string
      # instead of a data in the format of "9999/99" 
      # easiest to drop the column
      # I do not like this!
      # but ONE error in 420 firms AND one 'EARLY column drop' probabally will not matter
      #
      # Problem 2 ( related to Problem 1 )
      # drop = FALSE because rare case after  ( number 1944 is the problem )
      # where only one column  remains and R transforms THAT column into a vector
      if ( colnames(firmshistory[[firm_index]])[1] == "" &&
           ncol(firmshistory[[firm_index]]) > 1
      ) {
        firmshistory[[firm_index]] <- firmshistory[[firm_index]][,2:ncol(firmshistory[[firm_index]]),drop = FALSE]
      }
      
      # save the rownames data entries to a newcolumn on the right ( because I am going to remove them)
      firmshistory[[firm_index]] <- cbind(firmshistory[[firm_index]],rownames(firmshistory[[firm_index]]))
      
      # give the new column a name
      colnames(firmshistory[[firm_index]])[ncol(firmshistory[[firm_index]])] <- c("rownombres")
      
      # move this column all the way over to the left
      firmshistory[[firm_index]] <- firmshistory[[firm_index]][,c( tail(colnames(firmshistory[[firm_index]]),1), head(colnames(firmshistory[[firm_index]]),-1))]
      
      # remove all of the 'official' rownames
      rownames(firmshistory[[firm_index]]) <- NULL
      
     # prepare to fill the right side EXCHANGE TICKER column with data 
     # observe the data from the bottom row and 'second from the left' 
     # ( earlier in column one I inserted 'rownombres' ) 
     # verify the data I want to fill ( for a later step )
     # firmshistory[[firm_index]][nrow(firmshistory[[firm_index]]),2]
     #   2004/12
     #"AMEX_AAU"
     
      # keep the current firm
      current_firm <- firmshistory[[firm_index]][nrow(firmshistory[[firm_index]]),2]
      #   2004/12
      #"AMEX_AAU"
     
      # remove the useless names attribute
      attr(current_firm,"names") <- NULL
      #current_firm
      
      # of the current firm, verify the Symbol
      #SYMs[ c(current_firm),c("Symbol")] 
     
     # now for REAL fill the right side EXCHANGE TICKER column with data
      # fill the right side column with data from the previous step
      firmshistory[[firm_index]] <- cbind( 
            firmshistory[[firm_index]]
          , firmshistory[[firm_index]][nrow(firmshistory[[firm_index]]),2] 
          )
      
      # call the new column column at the end: EXCHANGE_TICKER"
      colnames(firmshistory[[firm_index]])[ncol(firmshistory[[firm_index]])] <- c("EXCHANGE_TICKER")
      
      # move this column all the way over to the left
      firmshistory[[firm_index]] <- firmshistory[[firm_index]][,c( tail(colnames(firmshistory[[firm_index]]),1), head(colnames(firmshistory[[firm_index]]),-1))]

      # of the current firm, verify the Industry
      # SYMs[ c(current_firm),c("Industry")] 
      
      # fill the right side column with data from the previous step
      firmshistory[[firm_index]] <- cbind( 
            firmshistory[[firm_index]]
          , SYMs[ c(current_firm),c("Industry")] 
          )

      # call the new column column at the end: INDUSTRY"
      colnames(firmshistory[[firm_index]])[ncol(firmshistory[[firm_index]])] <- c("INDUSTRY")

      # move this column all the way over to the left
      firmshistory[[firm_index]] <- firmshistory[[firm_index]][,c( tail(colnames(firmshistory[[firm_index]]),1), head(colnames(firmshistory[[firm_index]]),-1))]

      # of the current firm, verify the Sector
      # SYMs[ c(current_firm),c("Sector")] 
      
      # fill the right side column with data from the previous step
      firmshistory[[firm_index]] <- cbind( 
            firmshistory[[firm_index]]
          , SYMs[ c(current_firm),c("Sector")] 
          )

      # call the new column column at the end: SECTOR"
      colnames(firmshistory[[firm_index]])[ncol(firmshistory[[firm_index]])] <- c("SECTOR")
      
      # move this column all the way over to the left
      firmshistory[[firm_index]] <- firmshistory[[firm_index]][,c( tail(colnames(firmshistory[[firm_index]]),1), head(colnames(firmshistory[[firm_index]]),-1))]

      # of the current firm, verify the MarketCap
      # SYMs[ c(current_firm),c("MarketCap")] 
      # [1] "8.190652e+07
      
      # NOTE: I did not use as.integer to convert to an ONLY integer
      # The *_AAPL result is beyond the .Machine[["integer.max"]] of 2 billion
      # I will deal with THIS later
      
      # fill the right side column with data from the previous step
      firmshistory[[firm_index]] <- cbind( 
            firmshistory[[firm_index]]
          , SYMs[ c(current_firm),c("MarketCap")] 
          )

      # call the new column column at the end: MARKETCAP"
      colnames(firmshistory[[firm_index]])[ncol(firmshistory[[firm_index]])] <- c("MARKETCAP")
      
      # move this column all the way over to the left
      firmshistory[[firm_index]] <- firmshistory[[firm_index]][,c( tail(colnames(firmshistory[[firm_index]]),1), head(colnames(firmshistory[[firm_index]]),-1))]

      # verify that my new column names are there
      # dimnames(firmshistory[[firm_index]])[2][1][[1]][1:5]
      #[1] "MARKETCAP"       "SECTOR"          "INDUSTRY"        "EXCHANGE_TICKER"
      #[5] "rownombres"
      
      # verify my ( colnames and ) first row of data are there
      print(firmshistory[[firm_index]][1,1:5])

      # show the number finished every 100 records
      if ( firm_index %% 100 == 0 ) {
        print(paste(firm_index," completed.",sep=""))
      }
      
      # if testing
      # if ( firm_index == 101 ) break
      
    }
  }
# }

# debug(runme) # bombed out after 400
# undebug(runme)

# Sys.time()
# runme()  # 150 seconds ( memory increases from 420M to 840M
# traceback()
# Sys.time()
# about one second every 20 firms


#  length(firmshistory)
# [1] 4734

# remove those '0 row and 0 column' matrixes 
# this is a FIX upon problem 3 ABOVE
# thoroughly tested 'list item removal' 
# remove empty matrixes
new_firm_index <- 0
for ( x in firmshistory ) { 
  new_firm_index <- new_firm_index + 1
  if(ncol(x) == 0 ) {
     firmshistory[[new_firm_index]] <- NULL 
     # because my entire list has shifted down one ( became more compact )
     new_firm_index <- new_firm_index - 1
  } 
}

# length(firmshistory)
# [1] 4731
#          lost ONLY 3 firms


Sys.time()
save(firmshistory, file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()
# 120 seconds

Sys.time()
load(              file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()
# 90 seconds

# browse
# print(firmshistory[[4000]][1,1:5])
#                   MARKETCAP                        SECTOR                      INDUSTRY               EXCHANGE_TICKER                    rownombres
#              "8.996784e+09"            "Public Utilities" "Electric Utilities: Central"                    "NYSE_NRG"            "quarter end date"

# print(firmshistory[[4000]][282,5:6])
# firmshistory[[4000]][282,5:6]
#                               rownombres                                    2004/03
#                          "HTMLTITLEtext" "Nrg Energy Company Financial Information"


############## END EXECUTABLE AREA #######################


  
################ BEGIN EXECUTABLE AREA ############

# GOAL: FIND THE NUMBER OF FIRMS THAT HAVE A MARKET CAP OF AT LEAST 200 MILLION
#   BY O'SHAUN: BELOW 200 MILLION IS A FIRM TOO SMALL TO BUY/SELL
#   FOR AN INSTITUTIONAL BUYER/SELLER

# BUT I TESTED AT 250 MILLION

# IF NOT ALREADY DONE
Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()

# SHOW MY ALL REMAINING FIRMS AFTER SCRUBBING
length(firmshistory)
# [1] 4731

# Sys.time()
summarycounts <- 0
firm_index <- 0
if ( length(firmshistory) > 0 ) {
  for ( x in firmshistory ) {
    firm_index <- firm_index + 1

    # if the number on the right side of 'e' is greater or equal to 100 million ( 8 zeros )
    # 100,000,000 ( 8 zeros ) MARKET CAP OF AT LEAST 250 MILLION
    if( ( as.numeric(substr(firmshistory[[firm_index]][1,1],regexpr(pattern ="e",firmshistory[[firm_index]][1,1])[1] + 2, nchar(firmshistory[[firm_index]][1,1]))) >= 8 &
          # everything between 250 million and 999 million
          as.numeric(substr(firmshistory[[firm_index]][1,1],1, regexpr(pattern ="e",firmshistory[[firm_index]][1,1])[1] -1 )) >= 2.5 ) |
          # everything greater than 1 billion
        ( as.numeric(substr(firmshistory[[firm_index]][1,1],regexpr(pattern ="e",firmshistory[[firm_index]][1,1])[1] + 2, nchar(firmshistory[[firm_index]][1,1]))) >= 9 )
    ) {
      # add them up
      summarycounts <- summarycounts + 1
    }

    # show the number finished every 10 records
    if ( firm_index %% 10 == 0 ) {
      print(paste(firm_index," completed.",sep=""))
    }
    
  }
}
# Sys.time()
# how many of 4731
print(summarycounts)
# [1] 3240

# REALLY NOT 2000 ( I MAY HAVE *MORE* FIRMS THAN *COMPUSTAT* AND/OR *CRSP* )
# MIN 200 MARKETCAP TO BE SELLABLE ( IN FACT ) MY MARKETCAP IS 250 MILL MIN
#   AND I *STILL* HAVE 3000 FIRMS ...

############# END EXECUTABLE AREA ########################





###################### BEGIN EXECUTABLE AREA ##########################

# FOR YOUR INFORMATION #

# GOAL: FIND OUT 'HOW OFTEN' A GAP IN QUARTERLY DATA IS FOUND
#   FIND OUT WHICH FIRMS HAVE GAPS

# IF NOT ALREADY DONE
Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()

library(zoo)
firm_index <- 0
if ( length(firmshistory) > 0 ) {
  for ( x in firmshistory ) {
    firm_index <- firm_index + 1

    # from the left, this is the furthest right column that is NOT a date column
    date_col_index <- 5
    if ( ncol(firmshistory[[firm_index]]) >= 6 ) {
      for ( y in 6:ncol(firmshistory[[firm_index]]) ) {
        date_col_index <- date_col_index + 1
        # print(firmshistory[[firm_index]][1,date_col_index])
        # print(as.yearmon(firmshistory[[firm_index]][1,date_col_index], "%Y/%m"))
        # print(paste0(" ",as.yearmon(seq(as.Date(as.yearmon(firmshistory[[firm_index]][1,date_col_index],"%Y/%m")), by = "3 month", length = 2)[2],"%Y/%m")))
        # IF I AM AT THE LAST DATE, DO NOT LOOK FORWARD TO THE NEXT DATE. IT DOES NOT EXIST
        if ( date_col_index == ncol(firmshistory[[firm_index]]) ) { break } # no test for the 'next' because I am at the 'last' 
        # GIVEN THE CURRENT DATE_COL_INDEX, GENERATE A DATE THAT IS 3 MONTHS LATER
        generatednextdate <- as.yearmon(seq(as.Date(as.yearmon(firmshistory[[firm_index]][1,date_col_index],"%Y/%m")), by = "3 month", length = 2)[2],"%Y/%m")
        
        if (generatednextdate != as.yearmon(firmshistory[[firm_index]][1,date_col_index + 1], "%Y/%m") ) {
          print(paste0("Gap in firmhistory x quarters is found: firm_index ", firm_index," EXCHANGE_TICKER: ", firmshistory[[firm_index]][1,4] ))
          print(paste0("    Missing next date: ",generatednextdate))
        } 
      }
    }

    # show the number finished every 100 records
    if ( firm_index %% 100 == 0 ) {
      print(paste(firm_index," completed.",sep=""))
    }
    
  }
}
detach("package:zoo", unload=TRUE)

[1] "Gap in firmhistory x quarters is found: firm_index 5 EXCHANGE_TICKER: AMEX_ADK"
[1] "    Missing next date: Mar 2013"

# ONE OUT OF 3 FIRMS: AMEX
# ONE OUT OF 4 FIRMS: NASDAQ
# ONE OUT OF 5 FIMS: NYSE

# END OF 30 PLUS MINUTE RUN

# WILL DEAL WITH THIS LATER

###################### END EXECUTABLE AREA ##########################





########################## ( FYI ONLY ) BEGIN EXECUTABLE AREA ######################

# FOR YOUR INFORMATION #

# FIND firm_index OF EACH ITEM IN A COLLECTION OF TICKERS

# IF NOT ALREADY DONE
Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()


Sys.time()
EXCHANGE_TICKERvect <- c("NYSE_WMT","NYSE_TGT","NYSE_ORCL","NYSE_SAP")
found_so_far_count <- 0
firm_index <- 0
if ( length(firmshistory) > 0 ) {
  for ( x in firmshistory ) {
    firm_index <- firm_index + 1

    EXCHANGE_TICKERvect_index <- 0
    if ( length(EXCHANGE_TICKERvect) > 0 ) {
      for ( y in EXCHANGE_TICKERvect ) {
        EXCHANGE_TICKERvect_index <- EXCHANGE_TICKERvect_index + 1
        # READ JUST 'ROW 1' and 'column EXCHANGE_TICKER'
        # CONVERT TO DF DO EASY QUERY ( USE SUBSET )
        if ( 0 < nrow(subset(  as.data.frame( firmshistory[[firm_index]][1,4,drop=FALSE] ) , EXCHANGE_TICKER == EXCHANGE_TICKERvect[EXCHANGE_TICKERvect_index] )) ) {
          print(paste0("At index: ",firm_index," EXCHANGE_TICKER found: ",EXCHANGE_TICKERvect[EXCHANGE_TICKERvect_index]))
          found_so_far_count <- found_so_far_count + 1
          # IF I HAVE REACHED THE END OF THE VECTOR (SEARCH COLLECTION) THEN DONE WITH LOOP
          if ( found_so_far_count == length(EXCHANGE_TICKERvect) ) {
            break
          }
        }
      }
    }

    # show the number finished every 100 records
    if ( firm_index %% 100 == 0 ) {
      print(paste(firm_index," completed.",sep=""))
    }
    
  }
}
Sys.time()

# [1] "4000 completed."
# [1] "At index: 4066 EXCHANGE_TICKER found: NYSE_ORCL"
# [1] "4100 completed."
# [1] "4200 completed."
# [1] "At index: 4279 EXCHANGE_TICKER found: NYSE_SAP"
# [1] "4300 completed."
# [1] "4400 completed."
# [1] "At index: 4471 EXCHANGE_TICKER found: NYSE_TGT"
# [1] "4500 completed."
# [1] "4600 completed."
# [1] "At index: 4669 EXCHANGE_TICKER found: NYSE_WMT"
# [1] "4700 completed."

##################### ( FYI ONLY ) END EXECUTABLE AREA HERE #####################




######### BEGIN EXECUTABLE ( MERGE METHOD: NEVER USED: NOT ENOUGH MEMORY TO FINISH ) firms_total_revenue ############

# NOTE: merge into a perfect DF followed by rbind MAY HAVE BEEN FASTER
# perhaps ANOTHER day

# merge into a new firms_total_revenue and save it.

# options(width = 255)

# IF NOT ALREADY DONE
Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()
# 315M to load ( but not first access yet )

# I want do generate all month columns and in all in order
# seems useful to 'many' time series
#   ts,timeSeries, its,(irts),zoo,xts,quantmod and derivatives
# seems useful to sql: e.g. library(sqldef) "select ... UNION ..."
#   such that in UNION the column order is expected

# create a vector of ordered dates from "1990/01" to "2013/12"
alldatecolumns <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    yyyymm <- paste0(i,"/", if  ( j < 10 ) { paste0(0,j) } else { j } )
    alldatecolumns <- c(alldatecolumns,yyyymm)
  } 
} 
rm("i","j","yyyymm")


# create my alldatacolumns ( notice the 'a' ) columns and in my specific order
alldatacolumns <- c(
   "MARKETCAP","SECTOR","INDUSTRY"
   ,"EXCHANGE_TICKER","rownombres"   
   ,alldatecolumns
   )
# keep alldatacolumns and  alldatecolumns ( use former later to 'custom' sort )
 

# generate some columns
tempdf <- data.frame(row.names=alldatacolumns)
# make these change from row.names into rownames
tempdf <- as.matrix(tempdf)
# flip along the diagnol axis ... rownames becomes colnames
tempdf <- t(tempdf)
# recreate a data frame as the 'target' of a merge ( see below )
tempdf <- as.data.frame(tempdf)


Sys.time()
firm_index <- 0
if ( length(firmshistory) > 0 ) {
  for ( x in firmshistory ) {
    firm_index <- firm_index + 1
    
    # exract only the "total revenue" row
    firms_item_revenue <- as.matrix (subset(  as.data.frame( firmshistory[[firm_index]] ) , rownombres == "total revenue" ) )

    # side affect: the 'number of' the column is kemp as a rowname: remove it
    rownames(firms_item_revenue) <- NULL
    
    # perform merges
    # perform and outer join ( all=TRUE )
    # do not mix-up the row data ( sort=FALSE)
    # note: this converts a matrix of characters to a data frame of factors
    #  by the 'the programmer of merge' design
    # actually merge
    if ( firm_index == 1 ) { 
      # merge into tempdf data from firms_item_revenue
      firms_total_revenue <- merge(tempdf,firms_item_revenue,all=TRUE,sort=FALSE)
    } else {
      # merge into firms_total_revenue data from firms_item_revenue
      firms_total_revenue <- merge(firms_total_revenue,firms_item_revenue,all=TRUE,sort=FALSE)
    }

    # convert back to a matrix ( those factors will be now strings )
    firms_total_revenue <- as.matrix(firms_total_revenue) 
    # unfortunately the columns are in a mixed order
    # sort them to my custom sort
    firms_total_revenue <- firms_total_revenue[,alldatacolumns,drop=FALSE]

    # show the number finished every 10 records NOT FAST
    if ( firm_index %% 10 == 0 ) {
      print(paste(firm_index," completed.",sep=""))
    }
    
    # testing
    # if ( firm_index == 2 ) {
      # break
    # }
    
  }
}
Sys.time()
# seems 5 minutes 500 records ( memory is holding GOOD  400M )

rm("x","tempdf") # firms_item_revenue 

# working with firms_total_revenue SO WE KEEP IT
Sys.time()
save(firms_total_revenue, file="firms_total_revenue_ALL.Rdata")
Sys.time()

rm("firms_item_revenue") 

######### END EXECUTABLE ( MERGE METHOD: NEVER USED: NOT ENOUGH MEMORY TO FINISH ) firms_total_revenue ############




############ BEGIN EXECUTABLE AREA  ################### 

Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()

# below: trying to find the UNIQUE financial statement attributes
# ( So I may use mysql to 'partition by these attributes' )

firm_index <- 0
unique_rownombres <- c()
if ( length(firmshistory) > 0 ) {
  for ( x in firmshistory ) {
    firm_index <- firm_index + 1
    
    # note: 1 row matrix will become a vector
    # just appending to a non-unique collection
    # not UNIQUE yet: will be made unique BELOW
    unique_rownombres <-  append(unique_rownombres, firmshistory[[firm_index]][,5])

    if ( firm_index %% 100 == 0 ) {
      print(paste(firm_index," completed.",sep=""))
    }
    
    if ( firm_index %% 1000 == 0 ) {
      print("Done with 1000 records.")
      # break
    }
    
  }
}
## rm(x)
#Sys.time()
# ONE SECOND PER 100 START ... EVENTUALLY 3 SECONDS / 100 ( EXP SLOWDOWN )

# I SUSPECT that the situation is faster to run this
# ONCE every 1000 records ( rather than here at the end )
# remove duplicates ( I just want non-repeating financial statement attributes )
# any(duplicated(unique_rownombres))
# [1] TRUE     # BUT IS CASE SENSTITIVE
unique_rownombres <- unique(unique_rownombres)
# instantaneous
# any(duplicated(unique_rownombres))
# [1] FALSE   # BUT IS CASE SENSTITIVE

# length(unique_rownombres)
# 409

# visually view
# sort(unique_rownombres)

# see ...
# some plurals
# many unique items

# find the place where ONLY duplicates by case-senstivity is possible
# MySQL partitions have to have partition names THAT are case-insensitive

for ( x in unique_rownombres) {
  for ( y in unique_rownombres) {
     if( ( x != y ) & ( tolower(x) == tolower(y) ) ) { print(paste0(x," _may_equal_ ",y)) }
  }
}

# [1] "Net Income from Total Operations (YTD) _may_equal_ Net income from Total Operations (YTD)"
# [1] "Net income from Total Operations (YTD) _may_equal_ Net Income from Total Operations (YTD)"

# sort(unique_rownombres)
# [238] "net income from total operations"
# [239] "Net income from Total Operations (YTD)"
# [240] "Net Income from Total Operations (YTD)"
# [241] "net increase federal funds sold"

# need to be enclosed in single quotes NOW
# because in the "PARTITION ... VALUES_IN 'a','A'" for below I lost the quoting logic

unique_rownombres_index <- 0
for ( x in unique_rownombres) {
  unique_rownombres_index <- unique_rownombres_index + 1
  unique_rownombres[unique_rownombres_index] <- paste0("'",unique_rownombres[unique_rownombres_index],"'")
}

# sort(unique_rownombres)

# [238] "'net income from total operations'"
# [239] "'Net income from Total Operations (YTD)'"
# [240] "'Net Income from Total Operations (YTD)'"
# [241] "'net increase federal funds sold'"

# need to master vector containing all the VALUES_IN clauses in the CREATE TABLE statement
# I am combining finanacial statement attributes that ONLY differ by case of the caracters
# This will ( eventually ) lead to a multi-valued MySql PARTITION VALUES_IN item
# e.g.  PARTITION ... VALUES_IN 'a','A'

partition_values <- c()
unique_rownombres_already_processed <- c()
unique_rownombres_index <- 0
for ( x in unique_rownombres) {
  unique_rownombres_index <- unique_rownombres_index + 1
  if ( unique_rownombres[unique_rownombres_index] %in% unique_rownombres_already_processed ) {
    # already checked
    next
  } 
  # itself ( later: plus additional possible values to the right ( if any ) )
  partition_ele_original_plus_stored_duplicates_text <- paste0("",unique_rownombres[unique_rownombres_index],"")
  if (unique_rownombres_index < length(unique_rownombres) ) {
    for ( right_side_index in (unique_rownombres_index + 1):length(unique_rownombres)  ) {
      # print(paste0(unique_rownombres_index,right_side_index))
      if( ( unique_rownombres[unique_rownombres_index] != unique_rownombres[right_side_index] ) & ( tolower( unique_rownombres[unique_rownombres_index]) == tolower(unique_rownombres[right_side_index]) ) ) { 
          print(paste0("DUPLICATE FOUND")) 
          print(paste0(" ORIGINAL:",unique_rownombres[unique_rownombres_index])) 
          print(paste0("DUPLICATE:",unique_rownombres[right_side_index])) 
          # adding the near case sensitive copies
          # ( earlier: from itself ) plus additional possible values to the right ( if any )
          partition_ele_original_plus_stored_duplicates_text  <- paste0(partition_ele_original_plus_stored_duplicates_text,",",unique_rownombres[right_side_index],"")
          # remove right_side_index, so in the future it is not checked
          unique_rownombres_already_processed <- append(unique_rownombres_already_processed,unique_rownombres[right_side_index])
      } 
    }
  } 
  # print(partition_ele_original_plus_stored_duplicates_text)
  # add it to the master collection of partition VALUES_IN"
  partition_values <- append(partition_values,partition_ele_original_plus_stored_duplicates_text)
  # remove unique_rownombres_index, so in the future it is not checked
  # NOTE: this is in an ORDERED loop, so it is not rechecked ANYWAY
  # This line is here for programing LOGICAL consistency
  unique_rownombres_already_processed <- append(unique_rownombres_already_processed,unique_rownombres[unique_rownombres_index])
}

# notice below: only differ by 'i' and 'I' in i/Income

# [1] "DUPLICATE FOUND"
# [1] " ORIGINAL:'Net Income from Total Operations (YTD)'"
# [1] "DUPLICATE:'Net income from Total Operations (YTD)'"

# all mixed-case combinations
# length(unique_rownombres)
# [1] 409

# length(partition_values)
# [1] 408

# see the mixed cases all in one column value
# sort(partition_values)

# [238] "'net income from total operations'"
# [239] "'Net Income from Total Operations (YTD)','Net income from Total Operations (YTD)'"
# [240] "'net increase federal funds sold'"

# MANUALLY look at and choose the ones that NEED to be combined anyways
# sort(partition_values)
# ONLY 'THE VERY VERY CLOSE' ONES 'ONE CHARACTER ONLY'

 # [57] "'Basic EPS (Cum. Effect of Acc. Change)'"
 # [58] "'Basic EPS (Cum. Effect of Acct. Change)'"

 # [64] "'Basic EPS from Total Operations'"
 # [65] "'Basic EPS from Total Operations)'"

 # [95] "'cumulative translation adjustment'"
 # [96] "'cumulative translation adjustments'"
 
 # [99] "'current defered income taxes'"
# [100] "'current deferred income taxes'"
 
# [126] "'dividends paid per share'"
# [127] "'Dividends Paid Per Share (DPS)'"
 
# [143] "'extraordinary income losses'"
# [144] "'extraordinary income/losses'"
 
# [146] "'federal funds sold (purchased)'"
# [147] "'federal funds sold (securities purchased)'"
 
# [166] "'INCOME STATEMENT (YEAR-TO-DATE)Revenue (YTD)'"
# [167] "'INCOME STATEMENT (YEAR-TO-DATE)Revenues (YTD)'"

# [222] "'NET CASH FLOWeffect exchange rate changes'"
# [223] "'NET CASH FLOWeffect of exchange rate changes'"

# [277] "'other gains (losses)'"
# [278] "'other gains/losses'"
 
# [282] "'other investing changes net'"
# [283] "'other investing changes, net'"
 
# [292] "'other receivable'"
# [293] "'other receivables'"
 
# [310] "'preliminary full context ind'"
# [311] "'preliminary full context indicator'"
 
# [326] "'provision for loan loss'"
# [327] "'provision for loan losses'"
 
# [363] "'separate account business'"
# [364] "'separate accounts business'"

# [367] "'short-term debt'"
# [369] "'short -term debt'"

# [371] "'special income (charges)'"
# [372] "'special income charges'"

# [404] "'unearned premiums'"
# [405] "'unearnedp remiums'"

# create a matrix of together items
# slight DANGER here, I only have DOUBLE combinations
# a TRIPLE combination would require different programming

to_be_together_collection <- matrix ( 
   c(
    "'Basic EPS (Cum. Effect of Acc. Change)'",
    "'Basic EPS (Cum. Effect of Acct. Change)'",

    "'Basic EPS from Total Operations'",
    "'Basic EPS from Total Operations)'",

    "'cumulative translation adjustment'",
    "'cumulative translation adjustments'",

    "'current defered income taxes'",
    "'current deferred income taxes'",

    "'dividends paid per share'",
    "'Dividends Paid Per Share (DPS)'",

    "'extraordinary income losses'",
    "'extraordinary income/losses'",

    "'federal funds sold (purchased)'",
    "'federal funds sold (securities purchased)'",

    "'INCOME STATEMENT (YEAR-TO-DATE)Revenue (YTD)'",
    "'INCOME STATEMENT (YEAR-TO-DATE)Revenues (YTD)'",

    "'NET CASH FLOWeffect exchange rate changes'",
    "'NET CASH FLOWeffect of exchange rate changes'",

    "'other gains (losses)'",
    "'other gains/losses'",

    "'other investing changes net'",
    "'other investing changes, net'",

    "'other receivable'",
    "'other receivables'",

    "'preliminary full context ind'",
    "'preliminary full context indicator'",

    "'provision for loan loss'",
    "'provision for loan losses'",

    "'separate account business'",
    "'separate accounts business'",

    "'short-term debt'",
    "'short -term debt'",

    "'special income (charges)'",
    "'special income charges'",

    "'unearned premiums'",
    "'unearnedp remiums'"
    ), nrow=18, ncol=2, byrow=TRUE
)

# print to see
to_be_together_collection
      # [,1]
 # [1,] "'Basic EPS (Cum. Effect of Acc. Change)'"
 # [2,] "'Basic EPS from Total Operations'"
 # [3,] "'cumulative translation adjustment'"
 # [4,] "'current defered income taxes'"
 # [5,] "'dividends paid per share'"
 # [6,] "'extraordinary income losses'"
 # [7,] "'federal funds sold (purchased)'"
 # [8,] "'INCOME STATEMENT (YEAR-TO-DATE)Revenue (YTD)'"
 # [9,] "'NET CASH FLOWeffect exchange rate changes'"
# [10,] "'other gains (losses)'"
# [11,] "'other investing changes net'"
# [12,] "'other receivable'"
# [13,] "'preliminary full context ind'"
# [14,] "'provision for loan loss'"
# [15,] "'separate account business'"
# [16,] "'short-term debt'"
# [17,] "'special income (charges)'"
# [18,] "'unearned premiums'"
      # [,2]
 # [1,] "'Basic EPS (Cum. Effect of Acct. Change)'"
 # [2,] "'Basic EPS from Total Operations)'"
 # [3,] "'cumulative translation adjustments'"
 # [4,] "'current deferred income taxes'"
 # [5,] "'Dividends Paid Per Share (DPS)'"
 # [6,] "'extraordinary income/losses'"
 # [7,] "'federal funds sold (securities purchased)'"
 # [8,] "'INCOME STATEMENT (YEAR-TO-DATE)Revenues (YTD)'"
 # [9,] "'NET CASH FLOWeffect of exchange rate changes'"
# [10,] "'other gains/losses'"
# [11,] "'other investing changes, net'"
# [12,] "'other receivables'"
# [13,] "'preliminary full context indicator'"
# [14,] "'provision for loan losses'"
# [15,] "'separate accounts business'"
# [16,] "'short -term debt'"
# [17,] "'special income charges'"
# [18,] "'unearnedp remiums'"


# partition_values to be fixed
partition_values_fixed <- partition_values

# sort(partition_values_fixed)
# [404] "'unearned premiums'" ( HERE AND )
# [405] "'unearnedp remiums'" ( HERE TO BE COMBINED )
# [406] "'working capital'"
# [407] "'working capital as % of price'"
# [408] "'working captial as % of equity'"


# append(paste0) together 'the right element after the left element
# to become the new left element

if (nrow(to_be_together_collection) > 0) {
  to_be_together_collection_both_side_index <- 0
  for ( w in 1:nrow(to_be_together_collection) ) {
    to_be_together_collection_both_side_index <- to_be_together_collection_both_side_index + 1

    partition_values_left_side_index <- 0
    partition_values_index <- 0
    # find the index of left (first column) matrix element
    for ( x in partition_values ) {
      partition_values_index <- partition_values_index + 1
      if ( partition_values[partition_values_index] == to_be_together_collection[to_be_together_collection_both_side_index ,1] )  {
      partition_values_left_side_index <- partition_values_index
      print(paste0("# ", partition_values_index," is ",partition_values[partition_values_index]))
      }
    }

    partition_values_right_side_index <- 0
    partition_values_index <- 0
    # find the index of right (second column) matrix element
    for ( x in partition_values ) {
      partition_values_index <- partition_values_index + 1
      if ( partition_values[partition_values_index] == to_be_together_collection[to_be_together_collection_both_side_index ,2] )  {
        partition_values_right_side_index <- partition_values_index
        print(paste0("# ",partition_values_index," is ",partition_values[partition_values_index]))

        print( paste0( partition_values_left_side_index, " "
                    ,  partition_values_right_side_index, " "
                    ) 
              )
          
        # append together the elements reference by 
        #   partition_values_left_side_index AND
        #   partition_values_right_side_index

        partition_values_fixed[partition_values_left_side_index] <- paste0(
          partition_values_fixed[partition_values_left_side_index],","
          , partition_values_fixed[partition_values_right_side_index]
        )
    
        print(partition_values_fixed[partition_values_left_side_index])
        
      }
    }

  }
}

# [1] "# 366 is 'unearned premiums'"
# [1] "# 345 is 'unearnedp remiums'"
# [1] "366 345 "
# [1] "'unearned premiums','unearnedp remiums'"

# clean up
# remove from the partition_values_fixed collection, the element 
# that IS ONLY equal to to_be_together_collection[to_be_together_collection_index,2]
# that is 'to_be_together_collection_index matrix column 2'
# [R] Asking Favor For "Remove element with Particular Value In Vector"
# https://stat.ethz.ch/pipermail/r-help/2011-August/288183.html

if ( length(partition_values_fixed) > 0 ) {
  partition_values_fixed_index <- 0
  for ( y in partition_values_fixed ) {
    partition_values_fixed_index <- partition_values_fixed_index + 1

    if ( nrow(to_be_together_collection) > 0 ) {
      to_be_together_collection_index <- 0
      for ( x in 1:nrow(to_be_together_collection) ) {
        to_be_together_collection_index <- to_be_together_collection_index + 1
        
        # create a new partition_values_fixed that does not contain that
        # right side 'to_be_together_collection column 2 value'
        partition_values_fixed <- partition_values_fixed[partition_values_fixed != to_be_together_collection[to_be_together_collection_index,2] ]
        
      }
    }

  }
}

# sort(partition_values_fixed)
# 408 [408] - 18 to_be_together_collection matrix rows = 390 [390]
# ...
# [355] "'special income (charges)','special income charges'"
# ...
# [387] "'unearned premiums','unearnedp remiums'"
# [388] "'working capital'"
# [389] "'working capital as % of price'"
# [390] "'working captial as % of equity'"
# >

partition_values_fixed_part_name <- partition_values_fixed

# take the 'first comma delmited thing' of the partition_values_fixed collection
# and use that 'first comma delmited thing' as the partition name

if ( length(partition_values_fixed_part_name) > 0 ) {
  partition_values_fixed_part_name_index <- 0
  for ( y in partition_values_fixed_part_name ) {
    partition_values_fixed_part_name_index <- partition_values_fixed_part_name_index + 1
    
    # actually extract and that 'first comma delmited thing' 
    # shorten partition_values_fixed_part_name to contain ONLY that 'first'
    if ( regexpr(pattern ="','",partition_values_fixed_part_name[partition_values_fixed_part_name_index])[1] != -1 ) {
      partition_values_fixed_part_name[partition_values_fixed_part_name_index] <- substr(partition_values_fixed_part_name[partition_values_fixed_part_name_index],
        regexpr(pattern ="'",partition_values_fixed_part_name[partition_values_fixed_part_name_index])[1] + 1,
        regexpr(pattern ="','",partition_values_fixed_part_name[partition_values_fixed_part_name_index])[1] -1
      ) 
    } else {
        # no "','", therefore no 'first','second','third' ... just 'first'
        # just chop off the first tick mark and the last tick mark
      partition_values_fixed_part_name[partition_values_fixed_part_name_index] <- substr(partition_values_fixed_part_name[partition_values_fixed_part_name_index],
        regexpr(pattern ="'",partition_values_fixed_part_name[partition_values_fixed_part_name_index])[1] + 1,
        gregexpr(pattern ="'",partition_values_fixed_part_name[partition_values_fixed_part_name_index])[[1]][length(gregexpr(pattern ="'",partition_values_fixed_part_name[partition_values_fixed_part_name_index])[[1]])] - 1
      )
    }
    
  }
}

# sort(partition_values_fixed_part_name)
# [386] "trust fees by commissions"
# [387] "unearned premiums"
# [388] "working capital"
# [389] "working capital as % of price"
# [390] "working captial as % of equity"

# prepare to ... create partition names and values

partition_values_fixed_part_name_database <- partition_values_fixed_part_name

# testing
# partition_values_fixed_part_name_database_orig_copy <- partition_values_fixed_part_name_database

# testing put back
# partition_values_fixed_part_name_database <- partition_values_fixed_part_name_database_orig_copy

# tranform into my own partition names

partition_values_fixed_part_name_database_index <- 0
if ( length(partition_values_fixed_part_name_database) > 0 ) {
  for ( x in partition_values_fixed_part_name_database ) {
    partition_values_fixed_part_name_database_index <- partition_values_fixed_part_name_database_index + 1
    
    # remove problem characters that can not  or I do not want
    # to  be MySQL identifiers
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub(" ","_",partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("\\*","_ASK_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("%","_PCT_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("-","_HYP_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("&","_AND_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub(",","_CMMA_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("/","_RSL_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("\\$","_DOL_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("\\.","_PRD_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("\\(","_LPN_",partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- gsub("\\)","_RPN_" ,partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- tolower(partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])

    # MySQL limit of 64 characters for a partition name. Therefore, truncate it
    # I am NOT FULLY happy with this solution, but it is GOOD enough for right now
    partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index] <- substr(partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index],1,64)
    
    # debugging
    # print(partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index])

  }
}

# visually view
# sort(partition_values_fixed_part_name_database)

# [1] "_ask_auditors_name"
# [348] "selling_cmma__general__and__administrative__lpn_sg_and_a_rpn__ex"
## WITHOUT 64 CHARACTER TRUNCATION
## [348] "selling_cmma__general__and__administrative__lpn_sg_and_a_rpn__expense"

# In the absence of other information, the programs use the compiled-in default 
# character set, usually latin1
# http://dev.mysql.com/doc/refman/5.6/en/charset-connection.html

# R language is case sensitive
# I found out: NEED TO HAVE IN PARTITIONS VALUES list when ITEMS only differ by CASE
#   
# http://mysqldatabaseadministration.blogspot.com/2006/09/case-sensitive-mysql.html

# ONLY allowable combination that is case sentitive
########################################
# create datababase MySQL database advfn
########################################
# CREATE DATABASE `advfn` COLLATE 'latin1_general_cs' ;

# NOTE: default OBJECTS will be created: CHARACTER SET latin1 COLLATE latin1_general_cs 
# NOTE: I want it THIS WAY ( from experience: I want my JOINS to work )

# generate the MySQL: create table names and rows

create_table_firmshistory_partition_values_fixed_part_name_database <- "
CREATE TABLE `firmshistory_partition_rownombres` (
  `MARKETCAP` VARCHAR(64),
  `SECTOR` VARCHAR(64),
  `INDUSTRY` VARCHAR(64),
  `EXCHANGE_TICKER` VARCHAR(21),
  `rownombres` VARCHAR(64),  
  `X1990_01` TEXT,
  `X1990_02` TEXT,
  `X1990_03` TEXT,
  `X1990_04` TEXT,
  `X1990_05` TEXT,
  `X1990_06` TEXT,
  `X1990_07` TEXT,
  `X1990_08` TEXT,
  `X1990_09` TEXT,
  `X1990_10` TEXT,
  `X1990_11` TEXT,
  `X1990_12` TEXT,
  `X1991_01` TEXT,
  `X1991_02` TEXT,
  `X1991_03` TEXT,
  `X1991_04` TEXT,
  `X1991_05` TEXT,
  `X1991_06` TEXT,
  `X1991_07` TEXT,
  `X1991_08` TEXT,
  `X1991_09` TEXT,
  `X1991_10` TEXT,
  `X1991_11` TEXT,
  `X1991_12` TEXT,
  `X1992_01` TEXT,
  `X1992_02` TEXT,
  `X1992_03` TEXT,
  `X1992_04` TEXT,
  `X1992_05` TEXT,
  `X1992_06` TEXT,
  `X1992_07` TEXT,
  `X1992_08` TEXT,
  `X1992_09` TEXT,
  `X1992_10` TEXT,
  `X1992_11` TEXT,
  `X1992_12` TEXT,
  `X1993_01` TEXT,
  `X1993_02` TEXT,
  `X1993_03` TEXT,
  `X1993_04` TEXT,
  `X1993_05` TEXT,
  `X1993_06` TEXT,
  `X1993_07` TEXT,
  `X1993_08` TEXT,
  `X1993_09` TEXT,
  `X1993_10` TEXT,
  `X1993_11` TEXT,
  `X1993_12` TEXT,
  `X1994_01` TEXT,
  `X1994_02` TEXT,
  `X1994_03` TEXT,
  `X1994_04` TEXT,
  `X1994_05` TEXT,
  `X1994_06` TEXT,
  `X1994_07` TEXT,
  `X1994_08` TEXT,
  `X1994_09` TEXT,
  `X1994_10` TEXT,
  `X1994_11` TEXT,
  `X1994_12` TEXT,
  `X1995_01` TEXT,
  `X1995_02` TEXT,
  `X1995_03` TEXT,
  `X1995_04` TEXT,
  `X1995_05` TEXT,
  `X1995_06` TEXT,
  `X1995_07` TEXT,
  `X1995_08` TEXT,
  `X1995_09` TEXT,
  `X1995_10` TEXT,
  `X1995_11` TEXT,
  `X1995_12` TEXT,
  `X1996_01` TEXT,
  `X1996_02` TEXT,
  `X1996_03` TEXT,
  `X1996_04` TEXT,
  `X1996_05` TEXT,
  `X1996_06` TEXT,
  `X1996_07` TEXT,
  `X1996_08` TEXT,
  `X1996_09` TEXT,
  `X1996_10` TEXT,
  `X1996_11` TEXT,
  `X1996_12` TEXT,
  `X1997_01` TEXT,
  `X1997_02` TEXT,
  `X1997_03` TEXT,
  `X1997_04` TEXT,
  `X1997_05` TEXT,
  `X1997_06` TEXT,
  `X1997_07` TEXT,
  `X1997_08` TEXT,
  `X1997_09` TEXT,
  `X1997_10` TEXT,
  `X1997_11` TEXT,
  `X1997_12` TEXT,
  `X1998_01` TEXT,
  `X1998_02` TEXT,
  `X1998_03` TEXT,
  `X1998_04` TEXT,
  `X1998_05` TEXT,
  `X1998_06` TEXT,
  `X1998_07` TEXT,
  `X1998_08` TEXT,
  `X1998_09` TEXT,
  `X1998_10` TEXT,
  `X1998_11` TEXT,
  `X1998_12` TEXT,
  `X1999_01` TEXT,
  `X1999_02` TEXT,
  `X1999_03` TEXT,
  `X1999_04` TEXT,
  `X1999_05` TEXT,
  `X1999_06` TEXT,
  `X1999_07` TEXT,
  `X1999_08` TEXT,
  `X1999_09` TEXT,
  `X1999_10` TEXT,
  `X1999_11` TEXT,
  `X1999_12` TEXT,
  `X2000_01` TEXT,
  `X2000_02` TEXT,
  `X2000_03` TEXT,
  `X2000_04` TEXT,
  `X2000_05` TEXT,
  `X2000_06` TEXT,
  `X2000_07` TEXT,
  `X2000_08` TEXT,
  `X2000_09` TEXT,
  `X2000_10` TEXT,
  `X2000_11` TEXT,
  `X2000_12` TEXT,
  `X2001_01` TEXT,
  `X2001_02` TEXT,
  `X2001_03` TEXT,
  `X2001_04` TEXT,
  `X2001_05` TEXT,
  `X2001_06` TEXT,
  `X2001_07` TEXT,
  `X2001_08` TEXT,
  `X2001_09` TEXT,
  `X2001_10` TEXT,
  `X2001_11` TEXT,
  `X2001_12` TEXT,
  `X2002_01` TEXT,
  `X2002_02` TEXT,
  `X2002_03` TEXT,
  `X2002_04` TEXT,
  `X2002_05` TEXT,
  `X2002_06` TEXT,
  `X2002_07` TEXT,
  `X2002_08` TEXT,
  `X2002_09` TEXT,
  `X2002_10` TEXT,
  `X2002_11` TEXT,
  `X2002_12` TEXT,
  `X2003_01` TEXT,
  `X2003_02` TEXT,
  `X2003_03` TEXT,
  `X2003_04` TEXT,
  `X2003_05` TEXT,
  `X2003_06` TEXT,
  `X2003_07` TEXT,
  `X2003_08` TEXT,
  `X2003_09` TEXT,
  `X2003_10` TEXT,
  `X2003_11` TEXT,
  `X2003_12` TEXT,
  `X2004_01` TEXT,
  `X2004_02` TEXT,
  `X2004_03` TEXT,
  `X2004_04` TEXT,
  `X2004_05` TEXT,
  `X2004_06` TEXT,
  `X2004_07` TEXT,
  `X2004_08` TEXT,
  `X2004_09` TEXT,
  `X2004_10` TEXT,
  `X2004_11` TEXT,
  `X2004_12` TEXT,
  `X2005_01` TEXT,
  `X2005_02` TEXT,
  `X2005_03` TEXT,
  `X2005_04` TEXT,
  `X2005_05` TEXT,
  `X2005_06` TEXT,
  `X2005_07` TEXT,
  `X2005_08` TEXT,
  `X2005_09` TEXT,
  `X2005_10` TEXT,
  `X2005_11` TEXT,
  `X2005_12` TEXT,
  `X2006_01` TEXT,
  `X2006_02` TEXT,
  `X2006_03` TEXT,
  `X2006_04` TEXT,
  `X2006_05` TEXT,
  `X2006_06` TEXT,
  `X2006_07` TEXT,
  `X2006_08` TEXT,
  `X2006_09` TEXT,
  `X2006_10` TEXT,
  `X2006_11` TEXT,
  `X2006_12` TEXT,
  `X2007_01` TEXT,
  `X2007_02` TEXT,
  `X2007_03` TEXT,
  `X2007_04` TEXT,
  `X2007_05` TEXT,
  `X2007_06` TEXT,
  `X2007_07` TEXT,
  `X2007_08` TEXT,
  `X2007_09` TEXT,
  `X2007_10` TEXT,
  `X2007_11` TEXT,
  `X2007_12` TEXT,
  `X2008_01` TEXT,
  `X2008_02` TEXT,
  `X2008_03` TEXT,
  `X2008_04` TEXT,
  `X2008_05` TEXT,
  `X2008_06` TEXT,
  `X2008_07` TEXT,
  `X2008_08` TEXT,
  `X2008_09` TEXT,
  `X2008_10` TEXT,
  `X2008_11` TEXT,
  `X2008_12` TEXT,
  `X2009_01` TEXT,
  `X2009_02` TEXT,
  `X2009_03` TEXT,
  `X2009_04` TEXT,
  `X2009_05` TEXT,
  `X2009_06` TEXT,
  `X2009_07` TEXT,
  `X2009_08` TEXT,
  `X2009_09` TEXT,
  `X2009_10` TEXT,
  `X2009_11` TEXT,
  `X2009_12` TEXT,
  `X2010_01` TEXT,
  `X2010_02` TEXT,
  `X2010_03` TEXT,
  `X2010_04` TEXT,
  `X2010_05` TEXT,
  `X2010_06` TEXT,
  `X2010_07` TEXT,
  `X2010_08` TEXT,
  `X2010_09` TEXT,
  `X2010_10` TEXT,
  `X2010_11` TEXT,
  `X2010_12` TEXT,
  `X2011_01` TEXT,
  `X2011_02` TEXT,
  `X2011_03` TEXT,
  `X2011_04` TEXT,
  `X2011_05` TEXT,
  `X2011_06` TEXT,
  `X2011_07` TEXT,
  `X2011_08` TEXT,
  `X2011_09` TEXT,
  `X2011_10` TEXT,
  `X2011_11` TEXT,
  `X2011_12` TEXT,
  `X2012_01` TEXT,
  `X2012_02` TEXT,
  `X2012_03` TEXT,
  `X2012_04` TEXT,
  `X2012_05` TEXT,
  `X2012_06` TEXT,
  `X2012_07` TEXT,
  `X2012_08` TEXT,
  `X2012_09` TEXT,
  `X2012_10` TEXT,
  `X2012_11` TEXT,
  `X2012_12` TEXT,
  `X2013_01` TEXT,
  `X2013_02` TEXT,
  `X2013_03` TEXT,
  `X2013_04` TEXT,
  `X2013_05` TEXT,
  `X2013_06` TEXT,
  `X2013_07` TEXT,
  `X2013_08` TEXT,
  `X2013_09` TEXT,
  `X2013_10` TEXT,
  `X2013_11` TEXT,
  `X2013_12` TEXT
) CHARACTER SET latin1 COLLATE latin1_general_cs 
PARTITION BY LIST COLUMNS (`rownombres`) (

"

# debugging
# writeLines(create_table_firmshistory_partition_values_fixed_part_name_database)
# cat or writeLines
# http://stackoverflow.com/questions/4071586/printing-newlines-with-print-in-r

# testing
# create_table_firmshistory_partition_values_fixed_part_name_database_orig_copy <- create_table_firmshistory_partition_values_fixed_part_name_database 

# testing put back
# create_table_firmshistory_partition_values_fixed_part_name_database <- create_table_firmshistory_partition_values_fixed_part_name_database_orig_copy

# tranform into my own partition names and values
# append partition names and values

partition_snippet <- ""
partition_values_fixed_part_name_database_index <- 0
if ( length(partition_values_fixed_part_name_database) > 0 ) {
  for ( x in partition_values_fixed_part_name_database ) {
    partition_values_fixed_part_name_database_index <- partition_values_fixed_part_name_database_index + 1

partition_snippet <- "
  PARTITION 
    `"
    
    create_table_firmshistory_partition_values_fixed_part_name_database <- paste0(create_table_firmshistory_partition_values_fixed_part_name_database
       , partition_snippet, partition_values_fixed_part_name_database[partition_values_fixed_part_name_database_index],"`"
       )
    
     partition_snippet <- "
      VALUES IN ("
    
    create_table_firmshistory_partition_values_fixed_part_name_database <- paste0(create_table_firmshistory_partition_values_fixed_part_name_database
       , partition_snippet
       , partition_values_fixed[partition_values_fixed_part_name_database_index]
       ,")"
       ,ifelse( partition_values_fixed_part_name_database_index == length(partition_values_fixed_part_name_database),"" ,"," )
       ,"
"
       )
    
  }
  
  # end of the MySQL statement ");"
  partition_snippet <- "
);
"

  create_table_firmshistory_partition_values_fixed_part_name_database <- paste0(
    create_table_firmshistory_partition_values_fixed_part_name_database,
    partition_snippet
  )

}

# print it
# writeLines(create_table_firmshistory_partition_values_fixed_part_name_database)

# easier to see
fileConn <- file("create_table_firmshistory_partition_rownombres.out.txt")
writeLines(create_table_firmshistory_partition_values_fixed_part_name_database,fileConn)
close(fileConn)

# put the text into HeidiSQL
# then execute it

############ END EXECUTABLE AREA ####################


######### BEGIN EXECUTABLE '' ############ 

# UPSIZE TO MYSQL

# options(width = 255)

# IF NOT ALREADY DONE
Sys.time()
load(file="firmshistory_w_bottom_EXCHANGE_TICKERtext__MARKETCAP_SECTOR_INDUSTRY_ET_listitem_ALL.Rdata")
Sys.time()
# 315M to load ( but not first access yet )

# I want do generate all month columns and in all in order
# seems useful to 'many' time series
#   ts,timeSeries, its,(irts),zoo,xts,quantmod and derivatives
# seems useful to sql: e.g. library(sqldef) "select ... UNION ..."
#   such that in UNION the column order is expected

# create a vector of ordered dates from "1990/01" to "2013/12"
alldatecolumns <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    yyyymm <- paste0(i,"/", if  ( j < 10 ) { paste0(0,j) } else { j } )
    alldatecolumns <- c(alldatecolumns,yyyymm)
  } 
} 
rm("i","j","yyyymm")


# create my alldatacolumns ( notice the 'a' ) columns and in my specific order
alldatacolumns <- c(
   "MARKETCAP","SECTOR","INDUSTRY"
   ,"EXCHANGE_TICKER","rownombres"   
   ,alldatecolumns
   )
# keep alldatacolumns and  alldatecolumns ( use former later to 'custom' sort )
 

# generate some columns
tempdf <- data.frame(row.names=alldatacolumns)
# make these change from row.names into rownames
tempdf <- as.matrix(tempdf)
# flip along the diagnol axis ... rownames becomes colnames
tempdf <- t(tempdf)
# recreate a data frame as the 'target' of a merge ( see below )
tempdf <- as.data.frame(tempdf)

# begin R language MySQL
Sys.setenv(MYSQL_HOME = "F:/Program Files/MySQL/MySQL Server 5.6")
library(RMySQL)
# Loading required package: DBI
# MYSQL_HOME defined as F:/Program Files/MySQL/MySQL Server 5.6

# begin DBI MySQL
drv <- dbDriver("MySQL")
# open a session to MySQL database 'advfn'
con <- dbConnect(drv, user="root", pass="root",dbname="advfn",host="localhost")

Sys.time()
firm_index <- 0
if ( length(firmshistory) > 0 ) {
  for ( x in firmshistory ) {
    firm_index <- firm_index + 1
    
    # exract all  the "(attributes)" rows
    firms_item_all <- firmshistory[[firm_index]]
    
    # perform merges
    # perform and outer join ( all=TRUE )
    # do not mix-up the row data ( sort=FALSE)
    # note: this converts a matrix of characters to a data frame of factors
    #   by the 'the programmer of merge' design
    # actually merge

    # merge into tempdf data from firms_item_revenue
    # firms_total_revenue <- merge(tempdf,firms_item_revenue,all=TRUE,sort=FALSE)
    firms_item_all_df <- merge(tempdf,firms_item_all,all=TRUE,sort=FALSE)

    # convert back to a matrix ( those factors will be now strings )
    # firms_total_revenue <- as.matrix(firms_total_revenue) 
    firms_item_all <- as.matrix(firms_item_all_df) 
    
    # unfortunately the columns are in a mixed order
    # sort them to my custom sort
    firms_item_all <- firms_item_all[,alldatacolumns,drop=FALSE]
    
    # convert back into a data frame ( to be ready to be made into a MySQL table )
    firms_item_all_df <- as.data.frame(firms_item_all,stringsAsFactors = FALSE )

    # leave message ( in case of a crash ) I do know the load dataframe will begin next.
    print(paste0("Load dataframe into MySQL: firm: ",firm_index," starting."))
    
    # upsize to MySQL
    dbWriteTable(con, name = "firms_item_all_df", value = firms_item_all_df, row.names = FALSE)
    
    # leave message ( in case of a crash ) I do know what is not yet began.
    print(paste0("Upsize to MySQL advfn.firmshistory_partition_rownombres: firm: ",firm_index," starting."))
    
    # append to the MySQL partitioned table ( by rownombres ) firmshistory_partition_rownombres
    dbSendQuery(con,"INSERT INTO advfn.firmshistory_partition_rownombres SELECT * FROM advfn.firms_item_all_df")
    
    # leave message ( in case of a crash ) I do know what is not yet done.
    print(paste0("Upsize to MySQL advfn.firmshistory_partition_rownombres: firm: ",firm_index," completed."))
    
    # remove the 'now' useless table
    dbSendQuery(con,"DROP TABLE advfn.firms_item_all_df")
    
    # show the number finished every 10 records NOT FAST
    if ( firm_index %% 10 == 0 ) {
      print(paste(firm_index," completed.",sep=""))
    }
    
    # testing
    # if ( firm_index == 1 ) {
      # break
    # }
    
  }
}
Sys.time()
# seems 10 EXCHANGE_TICKERs every 13 seconds ( memory is holding ????  ###M )
# DID NOT MAKE IT

# [1] "Load dataframe into MySQL: firm: 4731 starting."
# [1] "Upsize to MySQL advfn.firmshistory_partition_rownombres: firm: 4731 starting."
# [1] "Upsize to MySQL advfn.firmshistory_partition_rownombres: firm: 4731 completed."
# > Sys.time()
# [1] "2013-12-09 23:01:23 CST" ( I STARTED ABOUT 08:00:00 )

dbDisconnect(con)
dbUnloadDriver(drv)

rm("x","con","drv") # 

# working with firms_total_revenue SO WE KEEP IT
# Sys.time()
### save(firms_total_revenue, file="firms_total_revenue_ALL.Rdata")
# Sys.time()

rm("firms_item_all") 
rm("firms_item_all_df") 

# MySQL memory is steady at 555M
# Rterm.exe only at 15M

# But firmshistory is still there
# > ncol(firmshistory[[4731]])
# [1] 15
# > nrow(firmshistory[[4731]])
# [1] 283
# >

######### END EXECUTABLE ############


################ BEGIN EXECUTABLE AREA #################### 
                                                            
# Note: this HAS BEEN tested with the NEXT EXECUTABLE area

# create a partitioned table ( by ThisMonth ) to hold
#   per ticker
#     end of month Close and AdjustedClose
# remember THIS advfn database is latin1_general_cs 
# if column `ThisMonth` were to be of type `text` 
# then upon attempted creation an error would occur
#   MySQL Database Error: A BLOB field is not allowed in partition function
# CREATE TABLE `firmshistory_quote_partition_thismonth` (
  # `TICKER` varchar(64),
  # `EXCHANGE_TICKER` varchar(64),
  # `ThisMonth` varchar(64),
  # `ThisMonthLastDate` varchar(64),
  # `ThisMonthLastClose` double,
  # `ThisMonthLastAdjustedClose` double
# ) PARTITION BY LIST COLUMNS (`ThisMonth`) (

  # PARTITION 
    # `X1990_01`
      # VALUES IN ('1990/01'),

  # PARTITION 
    # `X2013_01`
      # VALUES IN ('2013/01')

# );


create_table_firmshistory_quote_partition_thismonth <- "
CREATE TABLE `firmshistory_quote_partition_thismonth` (
  `TICKER` varchar(64),
  `EXCHANGE_TICKER` varchar(64),
  `ThisMonth` varchar(64),
  `ThisMonthLastDate` varchar(64),
  `ThisMonthLastClose` double,
  `ThisMonthLastAdjustedClose` double
) PARTITION BY LIST COLUMNS (`ThisMonth`) (

"

# need to generate the complete set of MySQL partition LIST partitions names
allpartitionnames <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    Xyyyy_mm <- paste0("X",i,"_", if ( j < 10 ) { paste0(0,j) } else { j })
    allpartitionnames <- c(allpartitionnames,Xyyyy_mm)
  }
}
rm("i","j","Xyyyy_mm")

# need to generate the complete set of MySQL partition LIST partitions values
allpartitionvalues <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    yyyy_mm <- paste0(i,"/", if ( j < 10 ) { paste0(0,j) } else { j })
    allpartitionvalues <- c(allpartitionvalues,yyyy_mm)
  }
}
rm("i","j","yyyy_mm")


partition_snippet <- ""
allpartitionnames_index <- 0
if ( length(allpartitionnames) > 0 ) {
  for ( x in allpartitionnames ) {
    allpartitionnames_index <- allpartitionnames_index + 1

partition_snippet <- "
  PARTITION 
    `"
    
    create_table_firmshistory_quote_partition_thismonth <- paste0(create_table_firmshistory_quote_partition_thismonth
       , partition_snippet, allpartitionnames[allpartitionnames_index],"`"
       )
    
     partition_snippet <- "
      VALUES IN ("
    
    create_table_firmshistory_quote_partition_thismonth <- paste0(create_table_firmshistory_quote_partition_thismonth
       , partition_snippet
       # FUTURE: if multiple values per list partition ( I could ) iterate through them
       , paste0("'",allpartitionvalues[allpartitionnames_index],"'")
       ,")"
       ,ifelse( allpartitionnames_index == length(allpartitionnames),"" ,"," )
       ,"
"
       )
    
  }
  
  # end of the MySQL statement ");"
  partition_snippet <- "
);
"

  create_table_firmshistory_quote_partition_thismonth <- paste0(
    create_table_firmshistory_quote_partition_thismonth,
    partition_snippet
  )

}

rm("allpartitionnames","allpartitionvalues","partition_snippet")

# easier to see
writeLines(create_table_firmshistory_quote_partition_thismonth)

# easier to see
fileConn <- file("create_table_firmshistory_quote_partition_thismonth.out.txt")
writeLines(create_table_firmshistory_quote_partition_thismonth,fileConn)
close(fileConn)


################ END EXECUTABLE AREA ####################


############### OUTPUT FROM ABOVE ###########

CREATE TABLE `firmshistory_quote_partition_thismonth` (
  `TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `EXCHANGE_TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ThisMonthLastDate` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ThisMonthLastClose` double DEFAULT NULL,
  `ThisMonthLastAdjustedClose` double DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
 PARTITION BY LIST  COLUMNS(ThisMonth)
(PARTITION X1990_01 VALUES IN ('1990/01') ENGINE = InnoDB,
 PARTITION X1990_02 VALUES IN ('1990/02') ENGINE = InnoDB,
 PARTITION X1990_03 VALUES IN ('1990/03') ENGINE = InnoDB,
 PARTITION X1990_04 VALUES IN ('1990/04') ENGINE = InnoDB,
 PARTITION X1990_05 VALUES IN ('1990/05') ENGINE = InnoDB,
 PARTITION X1990_06 VALUES IN ('1990/06') ENGINE = InnoDB,
 PARTITION X1990_07 VALUES IN ('1990/07') ENGINE = InnoDB,
 PARTITION X1990_08 VALUES IN ('1990/08') ENGINE = InnoDB,
 PARTITION X1990_09 VALUES IN ('1990/09') ENGINE = InnoDB,
 PARTITION X1990_10 VALUES IN ('1990/10') ENGINE = InnoDB,
 PARTITION X1990_11 VALUES IN ('1990/11') ENGINE = InnoDB,
 PARTITION X1990_12 VALUES IN ('1990/12') ENGINE = InnoDB,
 PARTITION X1991_01 VALUES IN ('1991/01') ENGINE = InnoDB,
 PARTITION X1991_02 VALUES IN ('1991/02') ENGINE = InnoDB,
 PARTITION X1991_03 VALUES IN ('1991/03') ENGINE = InnoDB,
 PARTITION X1991_04 VALUES IN ('1991/04') ENGINE = InnoDB,
 PARTITION X1991_05 VALUES IN ('1991/05') ENGINE = InnoDB,
 PARTITION X1991_06 VALUES IN ('1991/06') ENGINE = InnoDB,
 PARTITION X1991_07 VALUES IN ('1991/07') ENGINE = InnoDB,
 PARTITION X1991_08 VALUES IN ('1991/08') ENGINE = InnoDB,
 PARTITION X1991_09 VALUES IN ('1991/09') ENGINE = InnoDB,
 PARTITION X1991_10 VALUES IN ('1991/10') ENGINE = InnoDB,
 PARTITION X1991_11 VALUES IN ('1991/11') ENGINE = InnoDB,
 PARTITION X1991_12 VALUES IN ('1991/12') ENGINE = InnoDB,
 PARTITION X1992_01 VALUES IN ('1992/01') ENGINE = InnoDB,
 PARTITION X1992_02 VALUES IN ('1992/02') ENGINE = InnoDB,
 PARTITION X1992_03 VALUES IN ('1992/03') ENGINE = InnoDB,
 PARTITION X1992_04 VALUES IN ('1992/04') ENGINE = InnoDB,
 PARTITION X1992_05 VALUES IN ('1992/05') ENGINE = InnoDB,
 PARTITION X1992_06 VALUES IN ('1992/06') ENGINE = InnoDB,
 PARTITION X1992_07 VALUES IN ('1992/07') ENGINE = InnoDB,
 PARTITION X1992_08 VALUES IN ('1992/08') ENGINE = InnoDB,
 PARTITION X1992_09 VALUES IN ('1992/09') ENGINE = InnoDB,
 PARTITION X1992_10 VALUES IN ('1992/10') ENGINE = InnoDB,
 PARTITION X1992_11 VALUES IN ('1992/11') ENGINE = InnoDB,
 PARTITION X1992_12 VALUES IN ('1992/12') ENGINE = InnoDB,
 PARTITION X1993_01 VALUES IN ('1993/01') ENGINE = InnoDB,
 PARTITION X1993_02 VALUES IN ('1993/02') ENGINE = InnoDB,
 PARTITION X1993_03 VALUES IN ('1993/03') ENGINE = InnoDB,
 PARTITION X1993_04 VALUES IN ('1993/04') ENGINE = InnoDB,
 PARTITION X1993_05 VALUES IN ('1993/05') ENGINE = InnoDB,
 PARTITION X1993_06 VALUES IN ('1993/06') ENGINE = InnoDB,
 PARTITION X1993_07 VALUES IN ('1993/07') ENGINE = InnoDB,
 PARTITION X1993_08 VALUES IN ('1993/08') ENGINE = InnoDB,
 PARTITION X1993_09 VALUES IN ('1993/09') ENGINE = InnoDB,
 PARTITION X1993_10 VALUES IN ('1993/10') ENGINE = InnoDB,
 PARTITION X1993_11 VALUES IN ('1993/11') ENGINE = InnoDB,
 PARTITION X1993_12 VALUES IN ('1993/12') ENGINE = InnoDB,
 PARTITION X1994_01 VALUES IN ('1994/01') ENGINE = InnoDB,
 PARTITION X1994_02 VALUES IN ('1994/02') ENGINE = InnoDB,
 PARTITION X1994_03 VALUES IN ('1994/03') ENGINE = InnoDB,
 PARTITION X1994_04 VALUES IN ('1994/04') ENGINE = InnoDB,
 PARTITION X1994_05 VALUES IN ('1994/05') ENGINE = InnoDB,
 PARTITION X1994_06 VALUES IN ('1994/06') ENGINE = InnoDB,
 PARTITION X1994_07 VALUES IN ('1994/07') ENGINE = InnoDB,
 PARTITION X1994_08 VALUES IN ('1994/08') ENGINE = InnoDB,
 PARTITION X1994_09 VALUES IN ('1994/09') ENGINE = InnoDB,
 PARTITION X1994_10 VALUES IN ('1994/10') ENGINE = InnoDB,
 PARTITION X1994_11 VALUES IN ('1994/11') ENGINE = InnoDB,
 PARTITION X1994_12 VALUES IN ('1994/12') ENGINE = InnoDB,
 PARTITION X1995_01 VALUES IN ('1995/01') ENGINE = InnoDB,
 PARTITION X1995_02 VALUES IN ('1995/02') ENGINE = InnoDB,
 PARTITION X1995_03 VALUES IN ('1995/03') ENGINE = InnoDB,
 PARTITION X1995_04 VALUES IN ('1995/04') ENGINE = InnoDB,
 PARTITION X1995_05 VALUES IN ('1995/05') ENGINE = InnoDB,
 PARTITION X1995_06 VALUES IN ('1995/06') ENGINE = InnoDB,
 PARTITION X1995_07 VALUES IN ('1995/07') ENGINE = InnoDB,
 PARTITION X1995_08 VALUES IN ('1995/08') ENGINE = InnoDB,
 PARTITION X1995_09 VALUES IN ('1995/09') ENGINE = InnoDB,
 PARTITION X1995_10 VALUES IN ('1995/10') ENGINE = InnoDB,
 PARTITION X1995_11 VALUES IN ('1995/11') ENGINE = InnoDB,
 PARTITION X1995_12 VALUES IN ('1995/12') ENGINE = InnoDB,
 PARTITION X1996_01 VALUES IN ('1996/01') ENGINE = InnoDB,
 PARTITION X1996_02 VALUES IN ('1996/02') ENGINE = InnoDB,
 PARTITION X1996_03 VALUES IN ('1996/03') ENGINE = InnoDB,
 PARTITION X1996_04 VALUES IN ('1996/04') ENGINE = InnoDB,
 PARTITION X1996_05 VALUES IN ('1996/05') ENGINE = InnoDB,
 PARTITION X1996_06 VALUES IN ('1996/06') ENGINE = InnoDB,
 PARTITION X1996_07 VALUES IN ('1996/07') ENGINE = InnoDB,
 PARTITION X1996_08 VALUES IN ('1996/08') ENGINE = InnoDB,
 PARTITION X1996_09 VALUES IN ('1996/09') ENGINE = InnoDB,
 PARTITION X1996_10 VALUES IN ('1996/10') ENGINE = InnoDB,
 PARTITION X1996_11 VALUES IN ('1996/11') ENGINE = InnoDB,
 PARTITION X1996_12 VALUES IN ('1996/12') ENGINE = InnoDB,
 PARTITION X1997_01 VALUES IN ('1997/01') ENGINE = InnoDB,
 PARTITION X1997_02 VALUES IN ('1997/02') ENGINE = InnoDB,
 PARTITION X1997_03 VALUES IN ('1997/03') ENGINE = InnoDB,
 PARTITION X1997_04 VALUES IN ('1997/04') ENGINE = InnoDB,
 PARTITION X1997_05 VALUES IN ('1997/05') ENGINE = InnoDB,
 PARTITION X1997_06 VALUES IN ('1997/06') ENGINE = InnoDB,
 PARTITION X1997_07 VALUES IN ('1997/07') ENGINE = InnoDB,
 PARTITION X1997_08 VALUES IN ('1997/08') ENGINE = InnoDB,
 PARTITION X1997_09 VALUES IN ('1997/09') ENGINE = InnoDB,
 PARTITION X1997_10 VALUES IN ('1997/10') ENGINE = InnoDB,
 PARTITION X1997_11 VALUES IN ('1997/11') ENGINE = InnoDB,
 PARTITION X1997_12 VALUES IN ('1997/12') ENGINE = InnoDB,
 PARTITION X1998_01 VALUES IN ('1998/01') ENGINE = InnoDB,
 PARTITION X1998_02 VALUES IN ('1998/02') ENGINE = InnoDB,
 PARTITION X1998_03 VALUES IN ('1998/03') ENGINE = InnoDB,
 PARTITION X1998_04 VALUES IN ('1998/04') ENGINE = InnoDB,
 PARTITION X1998_05 VALUES IN ('1998/05') ENGINE = InnoDB,
 PARTITION X1998_06 VALUES IN ('1998/06') ENGINE = InnoDB,
 PARTITION X1998_07 VALUES IN ('1998/07') ENGINE = InnoDB,
 PARTITION X1998_08 VALUES IN ('1998/08') ENGINE = InnoDB,
 PARTITION X1998_09 VALUES IN ('1998/09') ENGINE = InnoDB,
 PARTITION X1998_10 VALUES IN ('1998/10') ENGINE = InnoDB,
 PARTITION X1998_11 VALUES IN ('1998/11') ENGINE = InnoDB,
 PARTITION X1998_12 VALUES IN ('1998/12') ENGINE = InnoDB,
 PARTITION X1999_01 VALUES IN ('1999/01') ENGINE = InnoDB,
 PARTITION X1999_02 VALUES IN ('1999/02') ENGINE = InnoDB,
 PARTITION X1999_03 VALUES IN ('1999/03') ENGINE = InnoDB,
 PARTITION X1999_04 VALUES IN ('1999/04') ENGINE = InnoDB,
 PARTITION X1999_05 VALUES IN ('1999/05') ENGINE = InnoDB,
 PARTITION X1999_06 VALUES IN ('1999/06') ENGINE = InnoDB,
 PARTITION X1999_07 VALUES IN ('1999/07') ENGINE = InnoDB,
 PARTITION X1999_08 VALUES IN ('1999/08') ENGINE = InnoDB,
 PARTITION X1999_09 VALUES IN ('1999/09') ENGINE = InnoDB,
 PARTITION X1999_10 VALUES IN ('1999/10') ENGINE = InnoDB,
 PARTITION X1999_11 VALUES IN ('1999/11') ENGINE = InnoDB,
 PARTITION X1999_12 VALUES IN ('1999/12') ENGINE = InnoDB,
 PARTITION X2000_01 VALUES IN ('2000/01') ENGINE = InnoDB,
 PARTITION X2000_02 VALUES IN ('2000/02') ENGINE = InnoDB,
 PARTITION X2000_03 VALUES IN ('2000/03') ENGINE = InnoDB,
 PARTITION X2000_04 VALUES IN ('2000/04') ENGINE = InnoDB,
 PARTITION X2000_05 VALUES IN ('2000/05') ENGINE = InnoDB,
 PARTITION X2000_06 VALUES IN ('2000/06') ENGINE = InnoDB,
 PARTITION X2000_07 VALUES IN ('2000/07') ENGINE = InnoDB,
 PARTITION X2000_08 VALUES IN ('2000/08') ENGINE = InnoDB,
 PARTITION X2000_09 VALUES IN ('2000/09') ENGINE = InnoDB,
 PARTITION X2000_10 VALUES IN ('2000/10') ENGINE = InnoDB,
 PARTITION X2000_11 VALUES IN ('2000/11') ENGINE = InnoDB,
 PARTITION X2000_12 VALUES IN ('2000/12') ENGINE = InnoDB,
 PARTITION X2001_01 VALUES IN ('2001/01') ENGINE = InnoDB,
 PARTITION X2001_02 VALUES IN ('2001/02') ENGINE = InnoDB,
 PARTITION X2001_03 VALUES IN ('2001/03') ENGINE = InnoDB,
 PARTITION X2001_04 VALUES IN ('2001/04') ENGINE = InnoDB,
 PARTITION X2001_05 VALUES IN ('2001/05') ENGINE = InnoDB,
 PARTITION X2001_06 VALUES IN ('2001/06') ENGINE = InnoDB,
 PARTITION X2001_07 VALUES IN ('2001/07') ENGINE = InnoDB,
 PARTITION X2001_08 VALUES IN ('2001/08') ENGINE = InnoDB,
 PARTITION X2001_09 VALUES IN ('2001/09') ENGINE = InnoDB,
 PARTITION X2001_10 VALUES IN ('2001/10') ENGINE = InnoDB,
 PARTITION X2001_11 VALUES IN ('2001/11') ENGINE = InnoDB,
 PARTITION X2001_12 VALUES IN ('2001/12') ENGINE = InnoDB,
 PARTITION X2002_01 VALUES IN ('2002/01') ENGINE = InnoDB,
 PARTITION X2002_02 VALUES IN ('2002/02') ENGINE = InnoDB,
 PARTITION X2002_03 VALUES IN ('2002/03') ENGINE = InnoDB,
 PARTITION X2002_04 VALUES IN ('2002/04') ENGINE = InnoDB,
 PARTITION X2002_05 VALUES IN ('2002/05') ENGINE = InnoDB,
 PARTITION X2002_06 VALUES IN ('2002/06') ENGINE = InnoDB,
 PARTITION X2002_07 VALUES IN ('2002/07') ENGINE = InnoDB,
 PARTITION X2002_08 VALUES IN ('2002/08') ENGINE = InnoDB,
 PARTITION X2002_09 VALUES IN ('2002/09') ENGINE = InnoDB,
 PARTITION X2002_10 VALUES IN ('2002/10') ENGINE = InnoDB,
 PARTITION X2002_11 VALUES IN ('2002/11') ENGINE = InnoDB,
 PARTITION X2002_12 VALUES IN ('2002/12') ENGINE = InnoDB,
 PARTITION X2003_01 VALUES IN ('2003/01') ENGINE = InnoDB,
 PARTITION X2003_02 VALUES IN ('2003/02') ENGINE = InnoDB,
 PARTITION X2003_03 VALUES IN ('2003/03') ENGINE = InnoDB,
 PARTITION X2003_04 VALUES IN ('2003/04') ENGINE = InnoDB,
 PARTITION X2003_05 VALUES IN ('2003/05') ENGINE = InnoDB,
 PARTITION X2003_06 VALUES IN ('2003/06') ENGINE = InnoDB,
 PARTITION X2003_07 VALUES IN ('2003/07') ENGINE = InnoDB,
 PARTITION X2003_08 VALUES IN ('2003/08') ENGINE = InnoDB,
 PARTITION X2003_09 VALUES IN ('2003/09') ENGINE = InnoDB,
 PARTITION X2003_10 VALUES IN ('2003/10') ENGINE = InnoDB,
 PARTITION X2003_11 VALUES IN ('2003/11') ENGINE = InnoDB,
 PARTITION X2003_12 VALUES IN ('2003/12') ENGINE = InnoDB,
 PARTITION X2004_01 VALUES IN ('2004/01') ENGINE = InnoDB,
 PARTITION X2004_02 VALUES IN ('2004/02') ENGINE = InnoDB,
 PARTITION X2004_03 VALUES IN ('2004/03') ENGINE = InnoDB,
 PARTITION X2004_04 VALUES IN ('2004/04') ENGINE = InnoDB,
 PARTITION X2004_05 VALUES IN ('2004/05') ENGINE = InnoDB,
 PARTITION X2004_06 VALUES IN ('2004/06') ENGINE = InnoDB,
 PARTITION X2004_07 VALUES IN ('2004/07') ENGINE = InnoDB,
 PARTITION X2004_08 VALUES IN ('2004/08') ENGINE = InnoDB,
 PARTITION X2004_09 VALUES IN ('2004/09') ENGINE = InnoDB,
 PARTITION X2004_10 VALUES IN ('2004/10') ENGINE = InnoDB,
 PARTITION X2004_11 VALUES IN ('2004/11') ENGINE = InnoDB,
 PARTITION X2004_12 VALUES IN ('2004/12') ENGINE = InnoDB,
 PARTITION X2005_01 VALUES IN ('2005/01') ENGINE = InnoDB,
 PARTITION X2005_02 VALUES IN ('2005/02') ENGINE = InnoDB,
 PARTITION X2005_03 VALUES IN ('2005/03') ENGINE = InnoDB,
 PARTITION X2005_04 VALUES IN ('2005/04') ENGINE = InnoDB,
 PARTITION X2005_05 VALUES IN ('2005/05') ENGINE = InnoDB,
 PARTITION X2005_06 VALUES IN ('2005/06') ENGINE = InnoDB,
 PARTITION X2005_07 VALUES IN ('2005/07') ENGINE = InnoDB,
 PARTITION X2005_08 VALUES IN ('2005/08') ENGINE = InnoDB,
 PARTITION X2005_09 VALUES IN ('2005/09') ENGINE = InnoDB,
 PARTITION X2005_10 VALUES IN ('2005/10') ENGINE = InnoDB,
 PARTITION X2005_11 VALUES IN ('2005/11') ENGINE = InnoDB,
 PARTITION X2005_12 VALUES IN ('2005/12') ENGINE = InnoDB,
 PARTITION X2006_01 VALUES IN ('2006/01') ENGINE = InnoDB,
 PARTITION X2006_02 VALUES IN ('2006/02') ENGINE = InnoDB,
 PARTITION X2006_03 VALUES IN ('2006/03') ENGINE = InnoDB,
 PARTITION X2006_04 VALUES IN ('2006/04') ENGINE = InnoDB,
 PARTITION X2006_05 VALUES IN ('2006/05') ENGINE = InnoDB,
 PARTITION X2006_06 VALUES IN ('2006/06') ENGINE = InnoDB,
 PARTITION X2006_07 VALUES IN ('2006/07') ENGINE = InnoDB,
 PARTITION X2006_08 VALUES IN ('2006/08') ENGINE = InnoDB,
 PARTITION X2006_09 VALUES IN ('2006/09') ENGINE = InnoDB,
 PARTITION X2006_10 VALUES IN ('2006/10') ENGINE = InnoDB,
 PARTITION X2006_11 VALUES IN ('2006/11') ENGINE = InnoDB,
 PARTITION X2006_12 VALUES IN ('2006/12') ENGINE = InnoDB,
 PARTITION X2007_01 VALUES IN ('2007/01') ENGINE = InnoDB,
 PARTITION X2007_02 VALUES IN ('2007/02') ENGINE = InnoDB,
 PARTITION X2007_03 VALUES IN ('2007/03') ENGINE = InnoDB,
 PARTITION X2007_04 VALUES IN ('2007/04') ENGINE = InnoDB,
 PARTITION X2007_05 VALUES IN ('2007/05') ENGINE = InnoDB,
 PARTITION X2007_06 VALUES IN ('2007/06') ENGINE = InnoDB,
 PARTITION X2007_07 VALUES IN ('2007/07') ENGINE = InnoDB,
 PARTITION X2007_08 VALUES IN ('2007/08') ENGINE = InnoDB,
 PARTITION X2007_09 VALUES IN ('2007/09') ENGINE = InnoDB,
 PARTITION X2007_10 VALUES IN ('2007/10') ENGINE = InnoDB,
 PARTITION X2007_11 VALUES IN ('2007/11') ENGINE = InnoDB,
 PARTITION X2007_12 VALUES IN ('2007/12') ENGINE = InnoDB,
 PARTITION X2008_01 VALUES IN ('2008/01') ENGINE = InnoDB,
 PARTITION X2008_02 VALUES IN ('2008/02') ENGINE = InnoDB,
 PARTITION X2008_03 VALUES IN ('2008/03') ENGINE = InnoDB,
 PARTITION X2008_04 VALUES IN ('2008/04') ENGINE = InnoDB,
 PARTITION X2008_05 VALUES IN ('2008/05') ENGINE = InnoDB,
 PARTITION X2008_06 VALUES IN ('2008/06') ENGINE = InnoDB,
 PARTITION X2008_07 VALUES IN ('2008/07') ENGINE = InnoDB,
 PARTITION X2008_08 VALUES IN ('2008/08') ENGINE = InnoDB,
 PARTITION X2008_09 VALUES IN ('2008/09') ENGINE = InnoDB,
 PARTITION X2008_10 VALUES IN ('2008/10') ENGINE = InnoDB,
 PARTITION X2008_11 VALUES IN ('2008/11') ENGINE = InnoDB,
 PARTITION X2008_12 VALUES IN ('2008/12') ENGINE = InnoDB,
 PARTITION X2009_01 VALUES IN ('2009/01') ENGINE = InnoDB,
 PARTITION X2009_02 VALUES IN ('2009/02') ENGINE = InnoDB,
 PARTITION X2009_03 VALUES IN ('2009/03') ENGINE = InnoDB,
 PARTITION X2009_04 VALUES IN ('2009/04') ENGINE = InnoDB,
 PARTITION X2009_05 VALUES IN ('2009/05') ENGINE = InnoDB,
 PARTITION X2009_06 VALUES IN ('2009/06') ENGINE = InnoDB,
 PARTITION X2009_07 VALUES IN ('2009/07') ENGINE = InnoDB,
 PARTITION X2009_08 VALUES IN ('2009/08') ENGINE = InnoDB,
 PARTITION X2009_09 VALUES IN ('2009/09') ENGINE = InnoDB,
 PARTITION X2009_10 VALUES IN ('2009/10') ENGINE = InnoDB,
 PARTITION X2009_11 VALUES IN ('2009/11') ENGINE = InnoDB,
 PARTITION X2009_12 VALUES IN ('2009/12') ENGINE = InnoDB,
 PARTITION X2010_01 VALUES IN ('2010/01') ENGINE = InnoDB,
 PARTITION X2010_02 VALUES IN ('2010/02') ENGINE = InnoDB,
 PARTITION X2010_03 VALUES IN ('2010/03') ENGINE = InnoDB,
 PARTITION X2010_04 VALUES IN ('2010/04') ENGINE = InnoDB,
 PARTITION X2010_05 VALUES IN ('2010/05') ENGINE = InnoDB,
 PARTITION X2010_06 VALUES IN ('2010/06') ENGINE = InnoDB,
 PARTITION X2010_07 VALUES IN ('2010/07') ENGINE = InnoDB,
 PARTITION X2010_08 VALUES IN ('2010/08') ENGINE = InnoDB,
 PARTITION X2010_09 VALUES IN ('2010/09') ENGINE = InnoDB,
 PARTITION X2010_10 VALUES IN ('2010/10') ENGINE = InnoDB,
 PARTITION X2010_11 VALUES IN ('2010/11') ENGINE = InnoDB,
 PARTITION X2010_12 VALUES IN ('2010/12') ENGINE = InnoDB,
 PARTITION X2011_01 VALUES IN ('2011/01') ENGINE = InnoDB,
 PARTITION X2011_02 VALUES IN ('2011/02') ENGINE = InnoDB,
 PARTITION X2011_03 VALUES IN ('2011/03') ENGINE = InnoDB,
 PARTITION X2011_04 VALUES IN ('2011/04') ENGINE = InnoDB,
 PARTITION X2011_05 VALUES IN ('2011/05') ENGINE = InnoDB,
 PARTITION X2011_06 VALUES IN ('2011/06') ENGINE = InnoDB,
 PARTITION X2011_07 VALUES IN ('2011/07') ENGINE = InnoDB,
 PARTITION X2011_08 VALUES IN ('2011/08') ENGINE = InnoDB,
 PARTITION X2011_09 VALUES IN ('2011/09') ENGINE = InnoDB,
 PARTITION X2011_10 VALUES IN ('2011/10') ENGINE = InnoDB,
 PARTITION X2011_11 VALUES IN ('2011/11') ENGINE = InnoDB,
 PARTITION X2011_12 VALUES IN ('2011/12') ENGINE = InnoDB,
 PARTITION X2012_01 VALUES IN ('2012/01') ENGINE = InnoDB,
 PARTITION X2012_02 VALUES IN ('2012/02') ENGINE = InnoDB,
 PARTITION X2012_03 VALUES IN ('2012/03') ENGINE = InnoDB,
 PARTITION X2012_04 VALUES IN ('2012/04') ENGINE = InnoDB,
 PARTITION X2012_05 VALUES IN ('2012/05') ENGINE = InnoDB,
 PARTITION X2012_06 VALUES IN ('2012/06') ENGINE = InnoDB,
 PARTITION X2012_07 VALUES IN ('2012/07') ENGINE = InnoDB,
 PARTITION X2012_08 VALUES IN ('2012/08') ENGINE = InnoDB,
 PARTITION X2012_09 VALUES IN ('2012/09') ENGINE = InnoDB,
 PARTITION X2012_10 VALUES IN ('2012/10') ENGINE = InnoDB,
 PARTITION X2012_11 VALUES IN ('2012/11') ENGINE = InnoDB,
 PARTITION X2012_12 VALUES IN ('2012/12') ENGINE = InnoDB,
 PARTITION X2013_01 VALUES IN ('2013/01') ENGINE = InnoDB,
 PARTITION X2013_02 VALUES IN ('2013/02') ENGINE = InnoDB,
 PARTITION X2013_03 VALUES IN ('2013/03') ENGINE = InnoDB,
 PARTITION X2013_04 VALUES IN ('2013/04') ENGINE = InnoDB,
 PARTITION X2013_05 VALUES IN ('2013/05') ENGINE = InnoDB,
 PARTITION X2013_06 VALUES IN ('2013/06') ENGINE = InnoDB,
 PARTITION X2013_07 VALUES IN ('2013/07') ENGINE = InnoDB,
 PARTITION X2013_08 VALUES IN ('2013/08') ENGINE = InnoDB,
 PARTITION X2013_09 VALUES IN ('2013/09') ENGINE = InnoDB,
 PARTITION X2013_10 VALUES IN ('2013/10') ENGINE = InnoDB,
 PARTITION X2013_11 VALUES IN ('2013/11') ENGINE = InnoDB,
 PARTITION X2013_12 VALUES IN ('2013/12') ENGINE = InnoDB);

############## END OF OUTPUT FROM ABOVE ###########


################ BEGIN EXECUTABLE AREA ################

# Get Yahoo End of Month Prices #

# AVOID THIS ERROR
# RS-DBI driver: (Failed to connect to database: Error: Lost connection to MySQL server at 
#   'reading authorization packet', system error: 2

# MUST MUST SET THESE my.ini entries
# http://dev.mysql.com/doc/refman/5.6/en/error-lost-connection.html
# [mysqld]
# net_read_timeout=3000 # 100 times bigger
# connect_timeout=1000 # 100 times bigger
# max_allowed_packet=1073741824 # the max

# HARD NOTE: TO RUN: Open a pink R windows and execute ALL CODE at the same time
#  For some ( perhaps file handle situation ) I can not seem ot requery Yahoo interactively

# gather all EXCHANGE_TICKER end of month Yahoo prices (Close,AdjustedClose)
# put data into a MySQL database 

library(quantmod)
options("getSymbols.warning4.0"=FALSE)
# As of 0.4-0, 'getSymbols' uses env=parent.frame() and auto.assign=TRUE by default.
# This  behavior  will be  phased out in 0.5-0  when the call  will
# default to use auto.assign=FALSE. getOption("getSymbols.env") and
# getOptions("getSymbols.auto.assign") are now checked for alternate defaults

library(timeDate)

# begin R language MySQL
Sys.setenv(MYSQL_HOME = "F:/Program Files/MySQL/MySQL Server 5.6")
library(RMySQL)
# Loading required package: DBI
# MYSQL_HOME defined as F:/Program Files/MySQL/MySQL Server 5.6

# begin DBI MySQL
drv <- dbDriver("MySQL")
# open a session to MySQL database 'advfn'
con <- dbConnect(drv, user="root", pass="root",dbname="advfn",host="localhost")

# load TTR symbols
# if not already done
load("SYMsBefore.Rdata", .GlobalEnv)

# create a vector of 'begining of month'      ordered dates from "1990-01-01" to "2013-12-01"
# create a vector of 'begining of month from' ordered dates from "1990-01-01" to "2013-12-01"
# create a vector of '"end  of month"   to'   ordered dates from "1990-01-EOM" to "2013-12-EOM"

allbeginmonthdates_to <- c()
allbeginmonthdates_to_index <- 0 
allbeginmonthdates_from <- c()
allbeginmonthdates_from_index <- 0 
# create a vector of 'begining of month' ordered dates from "1990-01-01" to "2013-12-01"
allbeginmonthdates <- c()
allbeginmonthdates_index <- 0  # NEW
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {

    # of 0 through 9, pad with a leading zero
    yyyy_mm_dd <- paste0(i,"-", if ( j < 10 ) { paste0(0,j) } else { j },"-","01" )
    # put at the end of the vector
    allbeginmonthdates_index <- allbeginmonthdates_index + 1 # NEW
    allbeginmonthdates <- c(allbeginmonthdates,yyyy_mm_dd)
    
    # a copy from just above
    # for getSymbols(later) create a  'from'       # put at the end of the vector
    allbeginmonthdates_from_index <-   allbeginmonthdates_from_index + 1
    allbeginmonthdates_from       <- c(allbeginmonthdates_from,yyyy_mm_dd)
    
    # Last 'store-bought-calandar' day of the month
    # for getSymbols(later) create a 'to'          # put at the end of the vector
    # UNTESTED: dateTime ... FinCenter = "New York" 
    #  ( but I am only interested in EOD prices within 24 hours 
    #    so I should be O.K. at the default GMT )
    # Note: for Yahoo Finance has to be 'real' 'date of the month'
    allbeginmonthdates_to_index   <-   allbeginmonthdates_to_index   + 1
    # have R package timeData limit date range through the end of THAT month
    # SUPRISINGLY: allbeginmonthdates_to BECOMES AN "R Datatype LIST"
    allbeginmonthdates_to         <- c(allbeginmonthdates_to,timeLastDayInMonth(allbeginmonthdates[allbeginmonthdates_index]))
  }
}
rm("i","j","yyyy_mm_dd")


# just useful firms with out hyphens in the TICKER name
exchange_useful <- c()
exchange_useful_index <- 0
firm_useful   <- c()
firm_useful_index  <- 0 
firm_index <- 0
if ( length(SYMs[["Symbol"]]) > 0 ) {
  for ( x in SYMs[["Symbol"]] ) {
    firm_index <- firm_index + 1
    
    # if no hyphen then this is symbol that ADVFN ( and YAHOO ) can match
    # NOTE: Possible: Yahoo can handle hyphens 
    # ( but this is a different exchange with different data )
    if( !(regexpr("-",SYMs[["Symbol"]][firm_index], ignore.case = FALSE)[1] > -1) ) {
    
      # put at the end of the vector
      firm_useful_index      <- firm_useful_index + 1 # NEW
      firm_useful            <- c(firm_useful,SYMs[["Symbol"]][firm_index])
      exchange_useful_index  <- exchange_useful_index + 1
      exchange_useful        <- c(exchange_useful,SYMs[["Exchange"]][firm_index])
    
    }
    
  }
}

# length(firm_useful)
# [1] 5864
# length(exchange_useful)
# [1] 5864
# tail(firm_useful,200)

# Testing
# firm_useful <- c("WMT","MSFT")
# firm_index <- 1
# exchange_useful <- c("NYSE","NASDAQ")

Sys.time()
firm_index <- 0
if ( length(firm_useful) > 0 ) {
  for ( x in 1:length(firm_useful) ) {
    # dangerous: I used firm_index instead of firm_useful_index
    firm_index <- firm_index + 1
    
    # debugging or mis-run to resume later ( break ( next: continue ) to the next the loop
    # if ( firm_index <= 4632 ) next
    
    print(paste0("Examining ",firm_useful[firm_index]," at firm_index: ",firm_index))
    # [1] "Examining AAMC at firm_index: 2"
  
      # have R package quantmod get all historical data for that month
      quoteCloseAdjClosegetQuote <- NULL
      # only way that works to 'not show' those WARNINGS (if Yahoo can't find any data )
      quoteCloseAdjClosegetQuote <-tryCatch({

        Sys.sleep(1.0)
        getSymbols(firm_useful[firm_index], src='yahoo'
          # the first date of THAT month
          , from = allbeginmonthdates_from[1]
          # have R package timeData limit date range through the end of THAT month
          , to   = allbeginmonthdates_to[[length(allbeginmonthdates_to)]]
          , auto.assign = FALSE # just return to the variable ( do not create an ENV )
          )

        }, warning = function(w) {
             return('WARNING')
        }, error = function(e) {
             return('ERROR')
        }, finally = function(f) {
             NULL
      })
        
      # NOT if a valid response ( 'got data' ) THEN just RETRY once MORE
      if(!is.xts(quoteCloseAdjClosegetQuote)) {
        print(paste0("  RETRY xts data of  ",firm_useful[firm_index]," at firm_index: ",firm_index))
        
        quoteCloseAdjClosegetQuote <-tryCatch({

          Sys.sleep(1.0)
          getSymbols(firm_useful[firm_index], src='yahoo'
            # the first date of THAT month
            , from = allbeginmonthdates_from[1]
            # have R package timeData limit date range through the end of THAT month
            , to   = allbeginmonthdates_to[[length(allbeginmonthdates_to)]]
            , auto.assign = FALSE # just return to the variable ( do not create an ENV )
            )

          }, warning = function(w) {
               return('WARNING')
          }, error = function(e) {
               return('ERROR')
          }, finally = function(f) {
               NULL
        })

      }
        
      # FINALLY: if a valid response ( 'got data' )
      if(is.xts(quoteCloseAdjClosegetQuote)) {
        print(paste0("  Found xts data of  ",firm_useful[firm_index]," at firm_index: ",firm_index, " of ",nrow(quoteCloseAdjClosegetQuote)," rows."))
        # [1] "  Retr xts data of  AIRI at firm_index: 10 for the mo beginning 2013-12-01"

        # Loop over all of the months 

        allbeginmonthdates_from_index <- 0
        
        # empty dataframe: rbind to this dataframe to sent just ONE sql per TICKER
        # <0 rows> (or 0-length row.names)
        quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF <- data.frame( 
            TICKER                     = as.character() 
          , EXCHANGE_TICKER            = as.character() 
          , ThisMonth                  = as.character()
          , ThisMonthLastDate          = as.character()
          , ThisMonthLastClose         = as.double()
          , ThisMonthLastAdjustedClose = as.double()
          , row.names = NULL
          , stringsAsFactors = FALSE
        )
        
        
        if ( length(allbeginmonthdates_from) > 0 ) {
          for ( x in 1:length(allbeginmonthdates_from) ) {
            allbeginmonthdates_from_index <- allbeginmonthdates_from_index + 1
            
            allbeginmonthdates_daterange <- paste0(gsub("-","",as.character(allbeginmonthdates_from[allbeginmonthdates_from_index])),"::",gsub("-","",as.character(allbeginmonthdates_to[[allbeginmonthdates_from_index]])))
            # "19900101::19900131"
            
            
            # in THIS month, of all Monthly Days, on the LAST Yahoo Finance Day, if any xts data items are returned whatsoever
            # note: avoid xts last '1 TIME' to avoid xts periodocity check error on ONE time element
            if (  length(last(quoteCloseAdjClosegetQuote[allbeginmonthdates_daterange])) > 0   ) {
              # data for that month     ... do SQL

              # format the data into a dataframe, so eventually can UPSIZE to MySQL
              
              # get the last date of the month ( using nrow ) that the item was traded
              # used 'NORMAL' xts 'index' and 'data' extraction methods
              quoteCloseAdjClosegetQuoteLastOfMonth_DF <- NULL
              quoteCloseAdjClosegetQuoteLastOfMonth_DF <- data.frame( 
                  TICKER = firm_useful[firm_index]
                , EXCHANGE_TICKER            = paste0(exchange_useful[firm_index],"_",firm_useful[firm_index]) # e.g. "NYSE_WMT" 
                , ThisMonth                  = sub("-","/",substr(allbeginmonthdates[allbeginmonthdates_from_index],1,7))   # e.g. "2013/06" 
                , ThisMonthLastDate          = as.character(index(last(quoteCloseAdjClosegetQuote[allbeginmonthdates_daterange])))
                , ThisMonthLastClose         = coredata(quoteCloseAdjClosegetQuote[,paste0(firm_useful[firm_index],".Close"),drop=FALSE][allbeginmonthdates_daterange,drop=FALSE])[nrow(coredata(quoteCloseAdjClosegetQuote[,paste0(firm_useful[firm_index],".Close"),drop=FALSE][allbeginmonthdates_daterange,drop=FALSE])),drop=FALSE]
                , ThisMonthLastAdjustedClose = coredata(quoteCloseAdjClosegetQuote[,paste0(firm_useful[firm_index],".Adjusted"),drop=FALSE][allbeginmonthdates_daterange,drop=FALSE])[nrow(coredata(quoteCloseAdjClosegetQuote[,paste0(firm_useful[firm_index],".Adjusted"),drop=FALSE][allbeginmonthdates_daterange,drop=FALSE])),drop=FALSE]
                , row.names = NULL
                , stringsAsFactors = FALSE
              )

              # remove that quantmod TICKER. (dot) prefix on the column names
              colnames(quoteCloseAdjClosegetQuoteLastOfMonth_DF) <- c(
                  "TICKER"
                , "EXCHANGE_TICKER"
                , "ThisMonth"
                , "ThisMonthLastDate"
                , "ThisMonthLastClose"
                , "ThisMonthLastAdjustedClose"
              )
                
              # just print NEED WIDER PRINT
              # on testing
              # wider output ( if I choose to print to the screen )
              options(width = 255)
              # does 'NOT PRINTING' help speed?? 24 seconds ... 28 seconds ( 18% slower )
              # print(paste0("    ",quoteCloseAdjClosegetQuoteLastOfMonth_DF))
              # [1] "    ACU"        "    AMEX_ACU"   "    2013/11"    "    2013-11-29" "    14.74"      "    14.74"
              
              # Actually append to the Mass dataframe
              quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF <- rbind(quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF,quoteCloseAdjClosegetQuoteLastOfMonth_DF)
              
            } else {
              # no data for that month   ... do nothing
            }

          }
        }
        
        # Done for that TICKER
        
        
        # OLD failing code: RSI error ( disk problem writing to TEMP directory ) 
        # dbWriteTable(con, name = "quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF", value = quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF, row.names = FALSE)

        # create a temp-like  MySQL table ( partition by ThisMonth values  )
        Sys.sleep(0.01)
        dbSendQuery(con,"
        CREATE TABLE `quotecloseadjclosegetquotelastofmonth_mass_df` (
        `TICKER` text COLLATE latin1_general_cs,
        `EXCHANGE_TICKER` text COLLATE latin1_general_cs,
        `ThisMonth` text COLLATE latin1_general_cs,
        `ThisMonthLastDate` text COLLATE latin1_general_cs,
        `ThisMonthLastClose` double DEFAULT NULL,
        `ThisMonthLastAdjustedClose` double DEFAULT NULL
        ) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
        ")
        Sys.sleep(0.01)
        
        # on MySQL this is created ( Note: database default is latin1_general_cs )
        # CREATE TABLE `quotecloseadjclosegetquotelastofmonth_mass_df` (
        # `TICKER` text COLLATE latin1_general_cs,
        # `EXCHANGE_TICKER` text COLLATE latin1_general_cs,
        # `ThisMonth` text COLLATE latin1_general_cs,
        # `ThisMonthLastDate` text COLLATE latin1_general_cs,
        # `ThisMonthLastClose` double DEFAULT NULL,
        # `ThisMonthLastAdjustedClose` double DEFAULT NULL
        # ) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs;

        # upsize to MySQL: INSERT into a MySQL database table  
        # on production

        # traverse through that dataframe 
        SQLStmt <- ""
        quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index <- 0
        if ( nrow(quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF)  > 0 ) {
          for( z in 1:nrow(quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF) ) {
            quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index <- quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index + 1
            
            # begin writing the ONE insert statement
            SQLStmt <- "INSERT INTO advfn.quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF(TICKER, EXCHANGE_TICKER, ThisMonth, ThisMonthLastDate, ThisMonthLastClose, ThisMonthLastAdjustedClose) VALUES ("
            
            # column data
            # first value - tick delimited
            SQLStmt <- paste0(SQLStmt,"" ,"'",  quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF[quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index,'TICKER']          ,"'",",")
            # ... other tick delimited values
            SQLStmt <- paste0(SQLStmt,"'",  quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF[quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index,'EXCHANGE_TICKER'] ,"'",",")
            SQLStmt <- paste0(SQLStmt,"'",  quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF[quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index,'ThisMonth']       ,"'",",")
            SQLStmt <- paste0(SQLStmt,"'",  quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF[quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index,'ThisMonthLastDate']       ,"'",",")
            # doubles are not tick delimited
            SQLStmt <- paste0(SQLStmt       ,quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF[quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index,'ThisMonthLastClose']             ,    "" ,",")
            # last is double and End of SQL statement
            SQLStmt <- paste0(SQLStmt       ,quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF[quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF_index,'ThisMonthLastAdjustedClose']     ,    "" ,    ")")
            
            # actually Upsize to MySQL
            Sys.sleep(0.01)
            dbSendQuery(con,SQLStmt)
            Sys.sleep(0.01)
            
          }

        }
        rm("SQLStmt")
        
        # Put into a partitioned MySQL table ( partition by ThisMonth values  )
        # append to the MySQL partitioned table ( by ThisMonth ) firmshistory_quote_partition_thismonth
        Sys.sleep(0.01)
        dbSendQuery(con,"INSERT INTO advfn.firmshistory_quote_partition_thismonth SELECT * FROM advfn.quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF")
        Sys.sleep(0.01)
        
        # http://dev.mysql.com/doc/refman/5.6/en/truncate-table.html
        # Truncate operations drop and re-create the table, 
        # which is much faster than deleting rows one by one, particularly for large tables.
        # remove the 'now' useless data
        # dbSendQuery(con,"TRUNCATE TABLE advfn.quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF")

        # actually remove the 'now' useless table ( and data )
        dbSendQuery(con,"DROP TABLE advfn.quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF")

        
      }
      
      # break out of loop 
      # on testing
      # if ( firm_index == 100 ) {
        # break
      # }

      if ( firm_index %% 100 == 0 ) {
        print(paste(firm_index," completed.",sep=""))
      }
      
      if ( firm_index %% 1000 == 0 ) {
        print("Done with 1000 records.")
      }
    
  }
}
Sys.time()

# 3:46:50
# [1] "Examining MA at firm_index: 4633"
# [1] "  Found xts data of  MA at firm_index: 4633 of 1914 rows."

# [1] "Examining ZZJJ at firm_index: 5864"
# [1] "  RETRY xts data of  ZZJJ at firm_index: 5864"
# > Sys.time()
# [1] "2014-01-15 20:37:44 CST" 8:37:44 p.m.  1231 in 5 hours  ( would be a 25 hour total run)

dbDisconnect(con)
dbUnloadDriver(drv)

rm("con","drv") 
rm("quoteCloseAdjClosegetQuoteLastOfMonth_DF"
  ,"quoteCloseAdjClosegetQuoteLastOfMonth_Mass_DF"
  ,"allbeginmonthdates_daterange"
) 

# useful
# SELECT COUNT(*) FROM advfn.firmshistory_quote_partition_thismonth;
# TRUNCATE TABLE advfn.firmshistory_quote_partition_thismonth;
  
################ END OF EXECUTABLE AREA ###############




################ BEGIN EXECUTABLE AREA #################### 
                                                            
# Note: this HAS BEEN tested with the NEXT EXECUTABLE area

# create a partitioned table ( by ThisMonth ) to hold
#   per ticker
#     Dividend
# remember THIS advfn database is latin1_general_cs 
# if column `ThisMonth` were to be of type `text` 
# then upon attempted creation an error would occur
#   MySQL Database Error: A BLOB field is not allowed in partition function

# HARD NOTE
# NOTE: This is different than `firmshistory_quote_partition_thismonth`
#       such that, MULTIPLE dividends are allowed per month
# THEREFORE, rows are NOT uniquely determined
# EXPECT, future database queries to do the SUM of `Dividend` across a time range

# CREATE TABLE `firmshistory_dividend_partition_thismonth` (
  # `TICKER` varchar(64),
  # `EXCHANGE_TICKER` varchar(64),
  # `ThisMonth` varchar(64),
  # `ThisMonthDividend` double
# ) PARTITION BY LIST COLUMNS (`ThisMonth`) (

  # PARTITION 
    # `X1990_01`
      # VALUES IN ('1990/01'),

  # PARTITION 
    # `X2013_01`
      # VALUES IN ('2013/01')

# );


create_table_firmshistory_dividend_partition_thismonth <- "
CREATE TABLE `firmshistory_dividend_partition_thismonth` (
  `TICKER` varchar(64),
  `EXCHANGE_TICKER` varchar(64),
  `ThisMonth` varchar(64),
  `ThisMonthDividend` double
) PARTITION BY LIST COLUMNS (`ThisMonth`) (

"

# need to generate the complete set of MySQL partition LIST partitions names
allpartitionnames <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    Xyyyy_mm <- paste0("X",i,"_", if ( j < 10 ) { paste0(0,j) } else { j })
    allpartitionnames <- c(allpartitionnames,Xyyyy_mm)
  }
}
rm("i","j","Xyyyy_mm")

# need to generate the complete set of MySQL partition LIST partitions values
allpartitionvalues <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    yyyy_mm <- paste0(i,"/", if ( j < 10 ) { paste0(0,j) } else { j })
    allpartitionvalues <- c(allpartitionvalues,yyyy_mm)
  }
}
rm("i","j","yyyy_mm")


partition_snippet <- ""
allpartitionnames_index <- 0
if ( length(allpartitionnames) > 0 ) {
  for ( x in allpartitionnames ) {
    allpartitionnames_index <- allpartitionnames_index + 1

partition_snippet <- "
  PARTITION 
    `"
    
    create_table_firmshistory_dividend_partition_thismonth <- paste0(create_table_firmshistory_dividend_partition_thismonth
       , partition_snippet, allpartitionnames[allpartitionnames_index],"`"
       )
    
     partition_snippet <- "
      VALUES IN ("
    
    create_table_firmshistory_dividend_partition_thismonth <- paste0(create_table_firmshistory_dividend_partition_thismonth
       , partition_snippet
       # FUTURE: if multiple values per list partition ( I could ) iterate through them
       , paste0("'",allpartitionvalues[allpartitionnames_index],"'")
       ,")"
       ,ifelse( allpartitionnames_index == length(allpartitionnames),"" ,"," )
       ,"
"
       )
    
  }
  
  # end of the MySQL statement ");"
  partition_snippet <- "
);
"

  create_table_firmshistory_dividend_partition_thismonth <- paste0(
    create_table_firmshistory_dividend_partition_thismonth,
    partition_snippet
  )

}

rm("allpartitionnames","allpartitionvalues","partition_snippet")

# easier to see
writeLines(create_table_firmshistory_dividend_partition_thismonth)

# easier to see
fileConn <- file("create_table_firmshistory_dividend_partition_thismonth.out.txt")
writeLines(create_table_firmshistory_dividend_partition_thismonth,fileConn)
close(fileConn)


################ END EXECUTABLE AREA ####################



################ OUTPUT FROM ABOVE ######################


CREATE TABLE `firmshistory_dividend_partition_thismonth` (
  `TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `EXCHANGE_TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ThisMonthDividend` double DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
PARTITION BY LIST  COLUMNS(ThisMonth)
(PARTITION X1990_01 VALUES IN ('1990/01') ENGINE = InnoDB,
 PARTITION X1990_02 VALUES IN ('1990/02') ENGINE = InnoDB,
 PARTITION X1990_03 VALUES IN ('1990/03') ENGINE = InnoDB,
 PARTITION X1990_04 VALUES IN ('1990/04') ENGINE = InnoDB,
 PARTITION X1990_05 VALUES IN ('1990/05') ENGINE = InnoDB,
 PARTITION X1990_06 VALUES IN ('1990/06') ENGINE = InnoDB,
 PARTITION X1990_07 VALUES IN ('1990/07') ENGINE = InnoDB,
 PARTITION X1990_08 VALUES IN ('1990/08') ENGINE = InnoDB,
 PARTITION X1990_09 VALUES IN ('1990/09') ENGINE = InnoDB,
 PARTITION X1990_10 VALUES IN ('1990/10') ENGINE = InnoDB,
 PARTITION X1990_11 VALUES IN ('1990/11') ENGINE = InnoDB,
 PARTITION X1990_12 VALUES IN ('1990/12') ENGINE = InnoDB,
 PARTITION X1991_01 VALUES IN ('1991/01') ENGINE = InnoDB,
 PARTITION X1991_02 VALUES IN ('1991/02') ENGINE = InnoDB,
 PARTITION X1991_03 VALUES IN ('1991/03') ENGINE = InnoDB,
 PARTITION X1991_04 VALUES IN ('1991/04') ENGINE = InnoDB,
 PARTITION X1991_05 VALUES IN ('1991/05') ENGINE = InnoDB,
 PARTITION X1991_06 VALUES IN ('1991/06') ENGINE = InnoDB,
 PARTITION X1991_07 VALUES IN ('1991/07') ENGINE = InnoDB,
 PARTITION X1991_08 VALUES IN ('1991/08') ENGINE = InnoDB,
 PARTITION X1991_09 VALUES IN ('1991/09') ENGINE = InnoDB,
 PARTITION X1991_10 VALUES IN ('1991/10') ENGINE = InnoDB,
 PARTITION X1991_11 VALUES IN ('1991/11') ENGINE = InnoDB,
 PARTITION X1991_12 VALUES IN ('1991/12') ENGINE = InnoDB,
 PARTITION X1992_01 VALUES IN ('1992/01') ENGINE = InnoDB,
 PARTITION X1992_02 VALUES IN ('1992/02') ENGINE = InnoDB,
 PARTITION X1992_03 VALUES IN ('1992/03') ENGINE = InnoDB,
 PARTITION X1992_04 VALUES IN ('1992/04') ENGINE = InnoDB,
 PARTITION X1992_05 VALUES IN ('1992/05') ENGINE = InnoDB,
 PARTITION X1992_06 VALUES IN ('1992/06') ENGINE = InnoDB,
 PARTITION X1992_07 VALUES IN ('1992/07') ENGINE = InnoDB,
 PARTITION X1992_08 VALUES IN ('1992/08') ENGINE = InnoDB,
 PARTITION X1992_09 VALUES IN ('1992/09') ENGINE = InnoDB,
 PARTITION X1992_10 VALUES IN ('1992/10') ENGINE = InnoDB,
 PARTITION X1992_11 VALUES IN ('1992/11') ENGINE = InnoDB,
 PARTITION X1992_12 VALUES IN ('1992/12') ENGINE = InnoDB,
 PARTITION X1993_01 VALUES IN ('1993/01') ENGINE = InnoDB,
 PARTITION X1993_02 VALUES IN ('1993/02') ENGINE = InnoDB,
 PARTITION X1993_03 VALUES IN ('1993/03') ENGINE = InnoDB,
 PARTITION X1993_04 VALUES IN ('1993/04') ENGINE = InnoDB,
 PARTITION X1993_05 VALUES IN ('1993/05') ENGINE = InnoDB,
 PARTITION X1993_06 VALUES IN ('1993/06') ENGINE = InnoDB,
 PARTITION X1993_07 VALUES IN ('1993/07') ENGINE = InnoDB,
 PARTITION X1993_08 VALUES IN ('1993/08') ENGINE = InnoDB,
 PARTITION X1993_09 VALUES IN ('1993/09') ENGINE = InnoDB,
 PARTITION X1993_10 VALUES IN ('1993/10') ENGINE = InnoDB,
 PARTITION X1993_11 VALUES IN ('1993/11') ENGINE = InnoDB,
 PARTITION X1993_12 VALUES IN ('1993/12') ENGINE = InnoDB,
 PARTITION X1994_01 VALUES IN ('1994/01') ENGINE = InnoDB,
 PARTITION X1994_02 VALUES IN ('1994/02') ENGINE = InnoDB,
 PARTITION X1994_03 VALUES IN ('1994/03') ENGINE = InnoDB,
 PARTITION X1994_04 VALUES IN ('1994/04') ENGINE = InnoDB,
 PARTITION X1994_05 VALUES IN ('1994/05') ENGINE = InnoDB,
 PARTITION X1994_06 VALUES IN ('1994/06') ENGINE = InnoDB,
 PARTITION X1994_07 VALUES IN ('1994/07') ENGINE = InnoDB,
 PARTITION X1994_08 VALUES IN ('1994/08') ENGINE = InnoDB,
 PARTITION X1994_09 VALUES IN ('1994/09') ENGINE = InnoDB,
 PARTITION X1994_10 VALUES IN ('1994/10') ENGINE = InnoDB,
 PARTITION X1994_11 VALUES IN ('1994/11') ENGINE = InnoDB,
 PARTITION X1994_12 VALUES IN ('1994/12') ENGINE = InnoDB,
 PARTITION X1995_01 VALUES IN ('1995/01') ENGINE = InnoDB,
 PARTITION X1995_02 VALUES IN ('1995/02') ENGINE = InnoDB,
 PARTITION X1995_03 VALUES IN ('1995/03') ENGINE = InnoDB,
 PARTITION X1995_04 VALUES IN ('1995/04') ENGINE = InnoDB,
 PARTITION X1995_05 VALUES IN ('1995/05') ENGINE = InnoDB,
 PARTITION X1995_06 VALUES IN ('1995/06') ENGINE = InnoDB,
 PARTITION X1995_07 VALUES IN ('1995/07') ENGINE = InnoDB,
 PARTITION X1995_08 VALUES IN ('1995/08') ENGINE = InnoDB,
 PARTITION X1995_09 VALUES IN ('1995/09') ENGINE = InnoDB,
 PARTITION X1995_10 VALUES IN ('1995/10') ENGINE = InnoDB,
 PARTITION X1995_11 VALUES IN ('1995/11') ENGINE = InnoDB,
 PARTITION X1995_12 VALUES IN ('1995/12') ENGINE = InnoDB,
 PARTITION X1996_01 VALUES IN ('1996/01') ENGINE = InnoDB,
 PARTITION X1996_02 VALUES IN ('1996/02') ENGINE = InnoDB,
 PARTITION X1996_03 VALUES IN ('1996/03') ENGINE = InnoDB,
 PARTITION X1996_04 VALUES IN ('1996/04') ENGINE = InnoDB,
 PARTITION X1996_05 VALUES IN ('1996/05') ENGINE = InnoDB,
 PARTITION X1996_06 VALUES IN ('1996/06') ENGINE = InnoDB,
 PARTITION X1996_07 VALUES IN ('1996/07') ENGINE = InnoDB,
 PARTITION X1996_08 VALUES IN ('1996/08') ENGINE = InnoDB,
 PARTITION X1996_09 VALUES IN ('1996/09') ENGINE = InnoDB,
 PARTITION X1996_10 VALUES IN ('1996/10') ENGINE = InnoDB,
 PARTITION X1996_11 VALUES IN ('1996/11') ENGINE = InnoDB,
 PARTITION X1996_12 VALUES IN ('1996/12') ENGINE = InnoDB,
 PARTITION X1997_01 VALUES IN ('1997/01') ENGINE = InnoDB,
 PARTITION X1997_02 VALUES IN ('1997/02') ENGINE = InnoDB,
 PARTITION X1997_03 VALUES IN ('1997/03') ENGINE = InnoDB,
 PARTITION X1997_04 VALUES IN ('1997/04') ENGINE = InnoDB,
 PARTITION X1997_05 VALUES IN ('1997/05') ENGINE = InnoDB,
 PARTITION X1997_06 VALUES IN ('1997/06') ENGINE = InnoDB,
 PARTITION X1997_07 VALUES IN ('1997/07') ENGINE = InnoDB,
 PARTITION X1997_08 VALUES IN ('1997/08') ENGINE = InnoDB,
 PARTITION X1997_09 VALUES IN ('1997/09') ENGINE = InnoDB,
 PARTITION X1997_10 VALUES IN ('1997/10') ENGINE = InnoDB,
 PARTITION X1997_11 VALUES IN ('1997/11') ENGINE = InnoDB,
 PARTITION X1997_12 VALUES IN ('1997/12') ENGINE = InnoDB,
 PARTITION X1998_01 VALUES IN ('1998/01') ENGINE = InnoDB,
 PARTITION X1998_02 VALUES IN ('1998/02') ENGINE = InnoDB,
 PARTITION X1998_03 VALUES IN ('1998/03') ENGINE = InnoDB,
 PARTITION X1998_04 VALUES IN ('1998/04') ENGINE = InnoDB,
 PARTITION X1998_05 VALUES IN ('1998/05') ENGINE = InnoDB,
 PARTITION X1998_06 VALUES IN ('1998/06') ENGINE = InnoDB,
 PARTITION X1998_07 VALUES IN ('1998/07') ENGINE = InnoDB,
 PARTITION X1998_08 VALUES IN ('1998/08') ENGINE = InnoDB,
 PARTITION X1998_09 VALUES IN ('1998/09') ENGINE = InnoDB,
 PARTITION X1998_10 VALUES IN ('1998/10') ENGINE = InnoDB,
 PARTITION X1998_11 VALUES IN ('1998/11') ENGINE = InnoDB,
 PARTITION X1998_12 VALUES IN ('1998/12') ENGINE = InnoDB,
 PARTITION X1999_01 VALUES IN ('1999/01') ENGINE = InnoDB,
 PARTITION X1999_02 VALUES IN ('1999/02') ENGINE = InnoDB,
 PARTITION X1999_03 VALUES IN ('1999/03') ENGINE = InnoDB,
 PARTITION X1999_04 VALUES IN ('1999/04') ENGINE = InnoDB,
 PARTITION X1999_05 VALUES IN ('1999/05') ENGINE = InnoDB,
 PARTITION X1999_06 VALUES IN ('1999/06') ENGINE = InnoDB,
 PARTITION X1999_07 VALUES IN ('1999/07') ENGINE = InnoDB,
 PARTITION X1999_08 VALUES IN ('1999/08') ENGINE = InnoDB,
 PARTITION X1999_09 VALUES IN ('1999/09') ENGINE = InnoDB,
 PARTITION X1999_10 VALUES IN ('1999/10') ENGINE = InnoDB,
 PARTITION X1999_11 VALUES IN ('1999/11') ENGINE = InnoDB,
 PARTITION X1999_12 VALUES IN ('1999/12') ENGINE = InnoDB,
 PARTITION X2000_01 VALUES IN ('2000/01') ENGINE = InnoDB,
 PARTITION X2000_02 VALUES IN ('2000/02') ENGINE = InnoDB,
 PARTITION X2000_03 VALUES IN ('2000/03') ENGINE = InnoDB,
 PARTITION X2000_04 VALUES IN ('2000/04') ENGINE = InnoDB,
 PARTITION X2000_05 VALUES IN ('2000/05') ENGINE = InnoDB,
 PARTITION X2000_06 VALUES IN ('2000/06') ENGINE = InnoDB,
 PARTITION X2000_07 VALUES IN ('2000/07') ENGINE = InnoDB,
 PARTITION X2000_08 VALUES IN ('2000/08') ENGINE = InnoDB,
 PARTITION X2000_09 VALUES IN ('2000/09') ENGINE = InnoDB,
 PARTITION X2000_10 VALUES IN ('2000/10') ENGINE = InnoDB,
 PARTITION X2000_11 VALUES IN ('2000/11') ENGINE = InnoDB,
 PARTITION X2000_12 VALUES IN ('2000/12') ENGINE = InnoDB,
 PARTITION X2001_01 VALUES IN ('2001/01') ENGINE = InnoDB,
 PARTITION X2001_02 VALUES IN ('2001/02') ENGINE = InnoDB,
 PARTITION X2001_03 VALUES IN ('2001/03') ENGINE = InnoDB,
 PARTITION X2001_04 VALUES IN ('2001/04') ENGINE = InnoDB,
 PARTITION X2001_05 VALUES IN ('2001/05') ENGINE = InnoDB,
 PARTITION X2001_06 VALUES IN ('2001/06') ENGINE = InnoDB,
 PARTITION X2001_07 VALUES IN ('2001/07') ENGINE = InnoDB,
 PARTITION X2001_08 VALUES IN ('2001/08') ENGINE = InnoDB,
 PARTITION X2001_09 VALUES IN ('2001/09') ENGINE = InnoDB,
 PARTITION X2001_10 VALUES IN ('2001/10') ENGINE = InnoDB,
 PARTITION X2001_11 VALUES IN ('2001/11') ENGINE = InnoDB,
 PARTITION X2001_12 VALUES IN ('2001/12') ENGINE = InnoDB,
 PARTITION X2002_01 VALUES IN ('2002/01') ENGINE = InnoDB,
 PARTITION X2002_02 VALUES IN ('2002/02') ENGINE = InnoDB,
 PARTITION X2002_03 VALUES IN ('2002/03') ENGINE = InnoDB,
 PARTITION X2002_04 VALUES IN ('2002/04') ENGINE = InnoDB,
 PARTITION X2002_05 VALUES IN ('2002/05') ENGINE = InnoDB,
 PARTITION X2002_06 VALUES IN ('2002/06') ENGINE = InnoDB,
 PARTITION X2002_07 VALUES IN ('2002/07') ENGINE = InnoDB,
 PARTITION X2002_08 VALUES IN ('2002/08') ENGINE = InnoDB,
 PARTITION X2002_09 VALUES IN ('2002/09') ENGINE = InnoDB,
 PARTITION X2002_10 VALUES IN ('2002/10') ENGINE = InnoDB,
 PARTITION X2002_11 VALUES IN ('2002/11') ENGINE = InnoDB,
 PARTITION X2002_12 VALUES IN ('2002/12') ENGINE = InnoDB,
 PARTITION X2003_01 VALUES IN ('2003/01') ENGINE = InnoDB,
 PARTITION X2003_02 VALUES IN ('2003/02') ENGINE = InnoDB,
 PARTITION X2003_03 VALUES IN ('2003/03') ENGINE = InnoDB,
 PARTITION X2003_04 VALUES IN ('2003/04') ENGINE = InnoDB,
 PARTITION X2003_05 VALUES IN ('2003/05') ENGINE = InnoDB,
 PARTITION X2003_06 VALUES IN ('2003/06') ENGINE = InnoDB,
 PARTITION X2003_07 VALUES IN ('2003/07') ENGINE = InnoDB,
 PARTITION X2003_08 VALUES IN ('2003/08') ENGINE = InnoDB,
 PARTITION X2003_09 VALUES IN ('2003/09') ENGINE = InnoDB,
 PARTITION X2003_10 VALUES IN ('2003/10') ENGINE = InnoDB,
 PARTITION X2003_11 VALUES IN ('2003/11') ENGINE = InnoDB,
 PARTITION X2003_12 VALUES IN ('2003/12') ENGINE = InnoDB,
 PARTITION X2004_01 VALUES IN ('2004/01') ENGINE = InnoDB,
 PARTITION X2004_02 VALUES IN ('2004/02') ENGINE = InnoDB,
 PARTITION X2004_03 VALUES IN ('2004/03') ENGINE = InnoDB,
 PARTITION X2004_04 VALUES IN ('2004/04') ENGINE = InnoDB,
 PARTITION X2004_05 VALUES IN ('2004/05') ENGINE = InnoDB,
 PARTITION X2004_06 VALUES IN ('2004/06') ENGINE = InnoDB,
 PARTITION X2004_07 VALUES IN ('2004/07') ENGINE = InnoDB,
 PARTITION X2004_08 VALUES IN ('2004/08') ENGINE = InnoDB,
 PARTITION X2004_09 VALUES IN ('2004/09') ENGINE = InnoDB,
 PARTITION X2004_10 VALUES IN ('2004/10') ENGINE = InnoDB,
 PARTITION X2004_11 VALUES IN ('2004/11') ENGINE = InnoDB,
 PARTITION X2004_12 VALUES IN ('2004/12') ENGINE = InnoDB,
 PARTITION X2005_01 VALUES IN ('2005/01') ENGINE = InnoDB,
 PARTITION X2005_02 VALUES IN ('2005/02') ENGINE = InnoDB,
 PARTITION X2005_03 VALUES IN ('2005/03') ENGINE = InnoDB,
 PARTITION X2005_04 VALUES IN ('2005/04') ENGINE = InnoDB,
 PARTITION X2005_05 VALUES IN ('2005/05') ENGINE = InnoDB,
 PARTITION X2005_06 VALUES IN ('2005/06') ENGINE = InnoDB,
 PARTITION X2005_07 VALUES IN ('2005/07') ENGINE = InnoDB,
 PARTITION X2005_08 VALUES IN ('2005/08') ENGINE = InnoDB,
 PARTITION X2005_09 VALUES IN ('2005/09') ENGINE = InnoDB,
 PARTITION X2005_10 VALUES IN ('2005/10') ENGINE = InnoDB,
 PARTITION X2005_11 VALUES IN ('2005/11') ENGINE = InnoDB,
 PARTITION X2005_12 VALUES IN ('2005/12') ENGINE = InnoDB,
 PARTITION X2006_01 VALUES IN ('2006/01') ENGINE = InnoDB,
 PARTITION X2006_02 VALUES IN ('2006/02') ENGINE = InnoDB,
 PARTITION X2006_03 VALUES IN ('2006/03') ENGINE = InnoDB,
 PARTITION X2006_04 VALUES IN ('2006/04') ENGINE = InnoDB,
 PARTITION X2006_05 VALUES IN ('2006/05') ENGINE = InnoDB,
 PARTITION X2006_06 VALUES IN ('2006/06') ENGINE = InnoDB,
 PARTITION X2006_07 VALUES IN ('2006/07') ENGINE = InnoDB,
 PARTITION X2006_08 VALUES IN ('2006/08') ENGINE = InnoDB,
 PARTITION X2006_09 VALUES IN ('2006/09') ENGINE = InnoDB,
 PARTITION X2006_10 VALUES IN ('2006/10') ENGINE = InnoDB,
 PARTITION X2006_11 VALUES IN ('2006/11') ENGINE = InnoDB,
 PARTITION X2006_12 VALUES IN ('2006/12') ENGINE = InnoDB,
 PARTITION X2007_01 VALUES IN ('2007/01') ENGINE = InnoDB,
 PARTITION X2007_02 VALUES IN ('2007/02') ENGINE = InnoDB,
 PARTITION X2007_03 VALUES IN ('2007/03') ENGINE = InnoDB,
 PARTITION X2007_04 VALUES IN ('2007/04') ENGINE = InnoDB,
 PARTITION X2007_05 VALUES IN ('2007/05') ENGINE = InnoDB,
 PARTITION X2007_06 VALUES IN ('2007/06') ENGINE = InnoDB,
 PARTITION X2007_07 VALUES IN ('2007/07') ENGINE = InnoDB,
 PARTITION X2007_08 VALUES IN ('2007/08') ENGINE = InnoDB,
 PARTITION X2007_09 VALUES IN ('2007/09') ENGINE = InnoDB,
 PARTITION X2007_10 VALUES IN ('2007/10') ENGINE = InnoDB,
 PARTITION X2007_11 VALUES IN ('2007/11') ENGINE = InnoDB,
 PARTITION X2007_12 VALUES IN ('2007/12') ENGINE = InnoDB,
 PARTITION X2008_01 VALUES IN ('2008/01') ENGINE = InnoDB,
 PARTITION X2008_02 VALUES IN ('2008/02') ENGINE = InnoDB,
 PARTITION X2008_03 VALUES IN ('2008/03') ENGINE = InnoDB,
 PARTITION X2008_04 VALUES IN ('2008/04') ENGINE = InnoDB,
 PARTITION X2008_05 VALUES IN ('2008/05') ENGINE = InnoDB,
 PARTITION X2008_06 VALUES IN ('2008/06') ENGINE = InnoDB,
 PARTITION X2008_07 VALUES IN ('2008/07') ENGINE = InnoDB,
 PARTITION X2008_08 VALUES IN ('2008/08') ENGINE = InnoDB,
 PARTITION X2008_09 VALUES IN ('2008/09') ENGINE = InnoDB,
 PARTITION X2008_10 VALUES IN ('2008/10') ENGINE = InnoDB,
 PARTITION X2008_11 VALUES IN ('2008/11') ENGINE = InnoDB,
 PARTITION X2008_12 VALUES IN ('2008/12') ENGINE = InnoDB,
 PARTITION X2009_01 VALUES IN ('2009/01') ENGINE = InnoDB,
 PARTITION X2009_02 VALUES IN ('2009/02') ENGINE = InnoDB,
 PARTITION X2009_03 VALUES IN ('2009/03') ENGINE = InnoDB,
 PARTITION X2009_04 VALUES IN ('2009/04') ENGINE = InnoDB,
 PARTITION X2009_05 VALUES IN ('2009/05') ENGINE = InnoDB,
 PARTITION X2009_06 VALUES IN ('2009/06') ENGINE = InnoDB,
 PARTITION X2009_07 VALUES IN ('2009/07') ENGINE = InnoDB,
 PARTITION X2009_08 VALUES IN ('2009/08') ENGINE = InnoDB,
 PARTITION X2009_09 VALUES IN ('2009/09') ENGINE = InnoDB,
 PARTITION X2009_10 VALUES IN ('2009/10') ENGINE = InnoDB,
 PARTITION X2009_11 VALUES IN ('2009/11') ENGINE = InnoDB,
 PARTITION X2009_12 VALUES IN ('2009/12') ENGINE = InnoDB,
 PARTITION X2010_01 VALUES IN ('2010/01') ENGINE = InnoDB,
 PARTITION X2010_02 VALUES IN ('2010/02') ENGINE = InnoDB,
 PARTITION X2010_03 VALUES IN ('2010/03') ENGINE = InnoDB,
 PARTITION X2010_04 VALUES IN ('2010/04') ENGINE = InnoDB,
 PARTITION X2010_05 VALUES IN ('2010/05') ENGINE = InnoDB,
 PARTITION X2010_06 VALUES IN ('2010/06') ENGINE = InnoDB,
 PARTITION X2010_07 VALUES IN ('2010/07') ENGINE = InnoDB,
 PARTITION X2010_08 VALUES IN ('2010/08') ENGINE = InnoDB,
 PARTITION X2010_09 VALUES IN ('2010/09') ENGINE = InnoDB,
 PARTITION X2010_10 VALUES IN ('2010/10') ENGINE = InnoDB,
 PARTITION X2010_11 VALUES IN ('2010/11') ENGINE = InnoDB,
 PARTITION X2010_12 VALUES IN ('2010/12') ENGINE = InnoDB,
 PARTITION X2011_01 VALUES IN ('2011/01') ENGINE = InnoDB,
 PARTITION X2011_02 VALUES IN ('2011/02') ENGINE = InnoDB,
 PARTITION X2011_03 VALUES IN ('2011/03') ENGINE = InnoDB,
 PARTITION X2011_04 VALUES IN ('2011/04') ENGINE = InnoDB,
 PARTITION X2011_05 VALUES IN ('2011/05') ENGINE = InnoDB,
 PARTITION X2011_06 VALUES IN ('2011/06') ENGINE = InnoDB,
 PARTITION X2011_07 VALUES IN ('2011/07') ENGINE = InnoDB,
 PARTITION X2011_08 VALUES IN ('2011/08') ENGINE = InnoDB,
 PARTITION X2011_09 VALUES IN ('2011/09') ENGINE = InnoDB,
 PARTITION X2011_10 VALUES IN ('2011/10') ENGINE = InnoDB,
 PARTITION X2011_11 VALUES IN ('2011/11') ENGINE = InnoDB,
 PARTITION X2011_12 VALUES IN ('2011/12') ENGINE = InnoDB,
 PARTITION X2012_01 VALUES IN ('2012/01') ENGINE = InnoDB,
 PARTITION X2012_02 VALUES IN ('2012/02') ENGINE = InnoDB,
 PARTITION X2012_03 VALUES IN ('2012/03') ENGINE = InnoDB,
 PARTITION X2012_04 VALUES IN ('2012/04') ENGINE = InnoDB,
 PARTITION X2012_05 VALUES IN ('2012/05') ENGINE = InnoDB,
 PARTITION X2012_06 VALUES IN ('2012/06') ENGINE = InnoDB,
 PARTITION X2012_07 VALUES IN ('2012/07') ENGINE = InnoDB,
 PARTITION X2012_08 VALUES IN ('2012/08') ENGINE = InnoDB,
 PARTITION X2012_09 VALUES IN ('2012/09') ENGINE = InnoDB,
 PARTITION X2012_10 VALUES IN ('2012/10') ENGINE = InnoDB,
 PARTITION X2012_11 VALUES IN ('2012/11') ENGINE = InnoDB,
 PARTITION X2012_12 VALUES IN ('2012/12') ENGINE = InnoDB,
 PARTITION X2013_01 VALUES IN ('2013/01') ENGINE = InnoDB,
 PARTITION X2013_02 VALUES IN ('2013/02') ENGINE = InnoDB,
 PARTITION X2013_03 VALUES IN ('2013/03') ENGINE = InnoDB,
 PARTITION X2013_04 VALUES IN ('2013/04') ENGINE = InnoDB,
 PARTITION X2013_05 VALUES IN ('2013/05') ENGINE = InnoDB,
 PARTITION X2013_06 VALUES IN ('2013/06') ENGINE = InnoDB,
 PARTITION X2013_07 VALUES IN ('2013/07') ENGINE = InnoDB,
 PARTITION X2013_08 VALUES IN ('2013/08') ENGINE = InnoDB,
 PARTITION X2013_09 VALUES IN ('2013/09') ENGINE = InnoDB,
 PARTITION X2013_10 VALUES IN ('2013/10') ENGINE = InnoDB,
 PARTITION X2013_11 VALUES IN ('2013/11') ENGINE = InnoDB,
 PARTITION X2013_12 VALUES IN ('2013/12') ENGINE = InnoDB);

################ END OF OUTPUT FROM ABOVE ######################



################ BEGIN EXECUTABLE AREA   ################

# Get Yahoo Dividends #

# http://stat.bell-labs.com/RS-DBI/doc/DBI.pdf
# http://stat.bell-labs.com/RS-DBI/index.html

# AVOID THIS ERROR
# RS-DBI driver: (Failed to connect to database: Error: Lost connection to MySQL server at 
#   'reading authorization packet', system error: 2

# MUST MUST SET THESE my.ini entries
# http://dev.mysql.com/doc/refman/5.6/en/error-lost-connection.html
# [mysqld]
# net_read_timeout=3000 # 100 times bigger
# connect_timeout=1000 # 100 times bigger
# max_allowed_packet=1073741824 # the max

# HARD NOTE: TO RUN: Open a pink R windows and execute ALL CODE at the same time
#  For some ( perhaps file handle situation ) I can not seem ot requery Yahoo interactively

# gather all EXCHANGE_TICKER end of month Yahoo prices (Close,AdjustedClose)
# put data into a MySQL database 

library(quantmod)
options("getSymbols.warning4.0"=FALSE)
# As of 0.4-0, 'getSymbols' uses env=parent.frame() and auto.assign=TRUE by default.
# This  behavior  will be  phased out in 0.5-0  when the call  will
# default to use auto.assign=FALSE. getOption("getSymbols.env") and
# getOptions("getSymbols.auto.assign") are now checked for alternate defaults

library(timeDate)

# begin R language MySQL
Sys.setenv(MYSQL_HOME = "F:/Program Files/MySQL/MySQL Server 5.6")
library(RMySQL)
# Loading required package: DBI
# MYSQL_HOME defined as F:/Program Files/MySQL/MySQL Server 5.6

# begin DBI MySQL
drv <- dbDriver("MySQL")

# open a session to MySQL database 'advfn'
# con <- dbConnect(drv, user="root", pass="root",dbname="advfn",host="localhost")

# open a session to MySQL database 'advfn' ( be able to send many INSERT statements )
con <- dbConnect(drv, user="root", pass="root", dbname="advfn", host="localhost")

# load TTR symbols
# if not already done
load("SYMsBefore.Rdata", .GlobalEnv)

# create a vector of 'begining of month'      ordered dates from "1990-01-01" to "2013-12-01"
# create a vector of 'begining of month from' ordered dates from "1990-01-01" to "2013-12-01"
# create a vector of '"end  of month"   to'   ordered dates from "1990-01-EOM" to "2013-12-EOM"

allbeginmonthdates_to <- c()
allbeginmonthdates_to_index <- 0 
allbeginmonthdates_from <- c()
allbeginmonthdates_from_index <- 0 
# create a vector of 'begining of month' ordered dates from "1990-01-01" to "2013-12-01"
allbeginmonthdates <- c()
allbeginmonthdates_index <- 0  # NEW
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {

    # of 0 through 9, pad with a leading zero
    yyyy_mm_dd <- paste0(i,"-", if ( j < 10 ) { paste0(0,j) } else { j },"-","01" )
    # put at the end of the vector
    allbeginmonthdates_index <- allbeginmonthdates_index + 1 # NEW
    allbeginmonthdates <- c(allbeginmonthdates,yyyy_mm_dd)
    
    # a copy from just above
    # for getSymbols(later) create a  'from'       # put at the end of the vector
    allbeginmonthdates_from_index <-   allbeginmonthdates_from_index + 1
    allbeginmonthdates_from       <- c(allbeginmonthdates_from,yyyy_mm_dd)
    
    # Last 'store-bought-calandar' day of the month
    # for getSymbols(later) create a 'to'          # put at the end of the vector
    # UNTESTED: dateTime ... FinCenter = "New York" 
    #  ( but I am only interested in EOD prices within 24 hours 
    #    so I should be O.K. at the default GMT )
    # Note: for Yahoo Finance has to be 'real' 'date of the month'
    allbeginmonthdates_to_index   <-   allbeginmonthdates_to_index   + 1
    # have R package timeData limit date range through the end of THAT month
    # SUPRISINGLY: allbeginmonthdates_to BECOMES AN "R Datatype LIST"
    allbeginmonthdates_to         <- c(allbeginmonthdates_to,timeLastDayInMonth(allbeginmonthdates[allbeginmonthdates_index]))
  }
}
rm("i","j","yyyy_mm_dd")


# just useful firms with out hyphens in the TICKER name
exchange_useful <- c()
exchange_useful_index <- 0
firm_useful   <- c()
firm_useful_index  <- 0 
firm_index <- 0
if ( length(SYMs[["Symbol"]]) > 0 ) {
  for ( x in SYMs[["Symbol"]] ) {
    firm_index <- firm_index + 1
    
    # if no hyphen then this is symbol that ADVFN ( and YAHOO ) can match
    # NOTE: Possible: Yahoo can handle hyphens 
    # ( but this is a different exchange with different data )
    if( !(regexpr("-",SYMs[["Symbol"]][firm_index], ignore.case = FALSE)[1] > -1) ) {
    
      # put at the end of the vector
      firm_useful_index      <- firm_useful_index + 1 # NEW
      firm_useful            <- c(firm_useful,SYMs[["Symbol"]][firm_index])
      exchange_useful_index  <- exchange_useful_index + 1
      exchange_useful        <- c(exchange_useful,SYMs[["Exchange"]][firm_index])
    
    }
    
  }
}

# length(firm_useful)
# [1] 5864
# length(exchange_useful)
# [1] 5864
# tail(firm_useful,200)

# Testing
# firm_useful <- c("WMT","MSFT")
# firm_index <- 1
# exchange_useful <- c("NYSE","NASDAQ")

Sys.time()
firm_index <- 0
if ( length(firm_useful) > 0 ) {
  for ( x in 1:length(firm_useful) ) {
    # dangerous: I used firm_index instead of firm_useful_index
    firm_index <- firm_index + 1
    
    # debugging or mis-run to resume later ( break ( next: continue ) to the next the loop
    # if ( firm_index <= SOMEFINITENUMBER ) next
    
    print(paste0("Examining ",firm_useful[firm_index]," at firm_index: ",firm_index))
    # [1] "Examining AAMC at firm_index: 2"
  
      # have R package quantmod get all dividend data for that month
      quoteDividendsgetQuote <- NULL
      # only way that works to 'not show' those WARNINGS (if Yahoo can't find any data )
      quoteDividendsgetQuote <-tryCatch({

        Sys.sleep(1.0)
        getDividends(firm_useful[firm_index], src='yahoo'
          # the first date of THAT month
          , from = allbeginmonthdates_from[1]
          # have R package timeData limit date range through the end of THAT month
          , to   = allbeginmonthdates_to[[length(allbeginmonthdates_to)]]
          , auto.assign = FALSE # just return to the variable ( do not create an ENV )
          )

        }, warning = function(w) {
             return('WARNING')
        }, error = function(e) {
             return('ERROR')
        }, finally = function(f) {
             NULL
      })
        
      # NOT if a valid response ( 'got data' ) THEN just RETRY once MORE
      if(!is.xts(quoteDividendsgetQuote)) {
        print(paste0("  RETRY xts data of  ",firm_useful[firm_index]," at firm_index: ",firm_index))
        
        quoteDividendsgetQuote <-tryCatch({

          Sys.sleep(1.0)
          getDividends(firm_useful[firm_index], src='yahoo'
            # the first date of THAT month
            , from = allbeginmonthdates_from[1]
            # have R package timeData limit date range through the end of THAT month
            , to   = allbeginmonthdates_to[[length(allbeginmonthdates_to)]]
            , auto.assign = FALSE # just return to the variable ( do not create an ENV )
            )

          }, warning = function(w) {
               return('WARNING')
          }, error = function(e) {
               return('ERROR')
          }, finally = function(f) {
               NULL
        })

      }
        
# WMT
# quoteDividendsgetQuote
              # [,1]
# 1990-03-19 0.00875
# 1990-06-11 0.00875

# simulate multiple dividends paid per month
# str(rbind(quoteDividendsgetQuote,quoteDividendsgetQuote))
# An 'xts' object on 1990-03-19/2013-12-04 containing:
  # Data: num [1:192, 1] 0.00875 0.00875 0.00875 0.00875 0.00875
  # Indexed by objects of class: [Date] TZ: UTC
  # xts Attributes:
 # NULL

# quoteDividendsgetQuote <- rbind(quoteDividendsgetQuote,quoteDividendsgetQuote)
# 1990-03-19 0.00875
# 1990-03-19 0.00875
        
      # FINALLY: if a valid response ( 'got data' )
      if(is.xts(quoteDividendsgetQuote)) {
        print(paste0("  Found xts data of  ",firm_useful[firm_index]," at firm_index: ",firm_index, " of ",nrow(quoteDividendsgetQuote)," rows."))
        # [1] "  Retr xts data of  AIRI at firm_index: 10 for the mo beginning 2013-12-01"

        # empty dataframe: rbind to this dataframe to sent just ONE sql per TICKER
        # <0 rows> (or 0-length row.names)
        
        # all dividends in a TICKER
        quoteDividendsgetQuoteAllOfMonth_Mass_DF <- data.frame( 
            TICKER                     = as.character() 
          , EXCHANGE_TICKER            = as.character() 
          , ThisMonth                  = as.character()
          , ThisMonthDividend          = as.double()
          , row.names = NULL
          , stringsAsFactors = FALSE
        )
        
        # Testing: WMT: March 1990 is index 3, June 1990 is 6
        # Traverse over all of the months 
        allbeginmonthdates_from_index <- 0
        if ( length(allbeginmonthdates_from) > 0 ) {
          for ( x in 1:length(allbeginmonthdates_from) ) {
            allbeginmonthdates_from_index <- allbeginmonthdates_from_index + 1
            
            allbeginmonthdates_daterange <- paste0(gsub("-","",as.character(allbeginmonthdates_from[allbeginmonthdates_from_index])),"::",gsub("-","",as.character(allbeginmonthdates_to[[allbeginmonthdates_from_index]])))
            # "19900101::19900131"
            
            # in THIS month, of all Monthly Days, on the LAST Yahoo Finance Day, if any xts data items are returned whatsoever
            # note: avoid xts last '1 TIME' to avoid xts periodocity check error on ONE time element
            if (  length(last(quoteDividendsgetQuote[allbeginmonthdates_daterange])) > 0   ) {
              # data for that month     ... do SQL

              # format the data into a dataframe, so eventually can UPSIZE to MySQL
              
              # all dividends in a TICKER's month
              quoteDividendsgetQuoteAllOfMonth_DF <- data.frame( 
                  TICKER                     = as.character() 
                , EXCHANGE_TICKER            = as.character() 
                , ThisMonth                  = as.character()
                , ThisMonthDividend          = as.double()
                , row.names = NULL
                , stringsAsFactors = FALSE
              )
              
              # get all dates and dividends of the month ( using 'all rows' ) 
              ThisMonthDividendZ <- coredata(quoteDividendsgetQuote[,1,drop=FALSE][allbeginmonthdates_daterange,drop=FALSE])
              
              # go through all of the daily dividends and append them to the month dividends
              ThisMonthDividendZ_index <- 0
              if ( nrow(ThisMonthDividendZ) > 0 ) {
                for ( y in 1:nrow(ThisMonthDividendZ) ) {
                  ThisMonthDividendZ_index <- ThisMonthDividendZ_index + 1
                  
                  # used 'NORMAL' xts 'index' and 'data' extraction methods
                  # just one dividend
                  quoteDividendsgetQuoteOneOfMonth_DF <- NULL
                  quoteDividendsgetQuoteOneOfMonth_DF <- data.frame( 
                      TICKER = firm_useful[firm_index]
                    , EXCHANGE_TICKER            = paste0(exchange_useful[firm_index],"_",firm_useful[firm_index]) # e.g. "NYSE_WMT" 
                    , ThisMonth                  = sub("-","/",substr(allbeginmonthdates[allbeginmonthdates_from_index],1,7))   # e.g. "2013/06" 
                    , ThisMonthDividend          = ThisMonthDividendZ[ThisMonthDividendZ_index,1,drop=FALSE]
                    , row.names = NULL
                    , stringsAsFactors = FALSE
                  )

                  # Actually append to the daily dividends to the monthly dividends
                  quoteDividendsgetQuoteAllOfMonth_DF <- rbind(quoteDividendsgetQuoteAllOfMonth_DF,quoteDividendsgetQuoteOneOfMonth_DF)
                  
                }
              }
              
              # just print NEED WIDER PRINT
              # on testing
              # wider output ( if I choose to print to the screen )
              options(width = 255)
              
              # from the month divideds, actually append to the TICKER's Mass dataframe
              quoteDividendsgetQuoteAllOfMonth_Mass_DF <- rbind(quoteDividendsgetQuoteAllOfMonth_Mass_DF,quoteDividendsgetQuoteAllOfMonth_DF)
              
            } else {
              # no data for that month   ... do nothing
            }

          }
        }
        
        # Done for that TICKER
        
        # OLD failing code: RSI error ( disk problem writing to TEMP directory ) 
        # dbWriteTable(con, name = "quoteDividendsgetQuoteAllOfMonth_Mass_DF", value = quoteDividendsgetQuoteAllOfMonth_Mass_DF, row.names = FALSE)

        # create a temp-like  MySQL table ( partition by ThisMonth values  )
        Sys.sleep(0.01)
        dbSendQuery(con,"
          CREATE TABLE `quoteDividendsgetQuoteAllOfMonth_Mass_DF` (
          `TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
          `EXCHANGE_TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
          `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
          `ThisMonthDividend` double DEFAULT NULL
          ) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
        ")
        Sys.sleep(0.01)

        # on MySQL this is created ( Note: database default is latin1_general_cs )
        # CREATE TABLE `firmshistory_dividend_partition_thismonth` (
        # `TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
        # `EXCHANGE_TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
        # `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
        # `ThisMonthDividend` double DEFAULT NULL
        # ) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
        
        # OLD failing code: RSI error ( disk problem writing to TEMP directory ) 
        # dbWriteTable(con, name = "quoteDividendsgetQuoteAllOfMonth_Mass_DF", value = quoteDividendsgetQuoteAllOfMonth_Mass_DF, row.names = FALSE)

        # upsize to MySQL: INSERT into a MySQL database table  
        # on production
        
        # traverse through that dataframe 
        SQLStmt <- ""
        quoteDividendsgetQuoteAllOfMonth_Mass_DF_index <- 0
        if ( nrow(quoteDividendsgetQuoteAllOfMonth_Mass_DF)  > 0 ) {
          for( z in 1:nrow(quoteDividendsgetQuoteAllOfMonth_Mass_DF) ) {
            quoteDividendsgetQuoteAllOfMonth_Mass_DF_index <- quoteDividendsgetQuoteAllOfMonth_Mass_DF_index + 1
            
            # begin writing the ONE insert statement
            SQLStmt <- "INSERT INTO advfn.quoteDividendsgetQuoteAllOfMonth_Mass_DF(TICKER, EXCHANGE_TICKER, ThisMonth, ThisMonthDividend) VALUES ("
            
            # column data
            # first value - tick delimited
            SQLStmt <- paste0(SQLStmt,"" ,"'",  quoteDividendsgetQuoteAllOfMonth_Mass_DF[quoteDividendsgetQuoteAllOfMonth_Mass_DF_index,'TICKER']          ,"'",",")
            # ... other tick delimited values
            SQLStmt <- paste0(SQLStmt,"'",  quoteDividendsgetQuoteAllOfMonth_Mass_DF[quoteDividendsgetQuoteAllOfMonth_Mass_DF_index,'EXCHANGE_TICKER'] ,"'",",")
            SQLStmt <- paste0(SQLStmt,"'",  quoteDividendsgetQuoteAllOfMonth_Mass_DF[quoteDividendsgetQuoteAllOfMonth_Mass_DF_index,'ThisMonth']       ,"'",",")
            # last is double that is not tick delimited,  and End of SQL statement
            SQLStmt <- paste0(SQLStmt       ,quoteDividendsgetQuoteAllOfMonth_Mass_DF[quoteDividendsgetQuoteAllOfMonth_Mass_DF_index,'ThisMonthDividend']     ,    "" ,")")
            
            # actually Upsize to MySQL
            Sys.sleep(0.01)
            dbSendQuery(con,SQLStmt)
            Sys.sleep(0.01)
            
          }

        }
        rm("SQLStmt")
        
        # Put into a partitioned MySQL table ( partition by ThisMonth values  )
        # append to the MySQL partitioned table ( by ThisMonth ) firmshistory_dividend_partition_thismonth
        Sys.sleep(0.01)
        dbSendQuery(con,"INSERT INTO advfn.firmshistory_dividend_partition_thismonth SELECT * FROM advfn.quoteDividendsgetQuoteAllOfMonth_Mass_DF")
        Sys.sleep(0.01)
        
        # http://dev.mysql.com/doc/refman/5.6/en/truncate-table.html
        # Truncate operations drop and re-create the table, 
        # which is much faster than deleting rows one by one, particularly for large tables.
        # remove the 'now' useless data
        # dbSendQuery(con,"TRUNCATE TABLE advfn.quoteDividendsgetQuoteAllOfMonth_Mass_DF")

        # actually remove the 'now' useless table ( and data )
        dbSendQuery(con,"DROP TABLE advfn.quoteDividendsgetQuoteAllOfMonth_Mass_DF")

      }
      
      # break out of loop 
      # on testing
      # if ( firm_index == 10 ) {
        # break
      # }

      if ( firm_index %% 100 == 0 ) {
        print(paste(firm_index," completed.",sep=""))
      }
      
      if ( firm_index %% 1000 == 0 ) {
        print("Done with 1000 records.")
      }
    
  }
}
Sys.time()

# Yahoo seems happier with time delays

# START 8:26:35 PM ON SUNDAY NIGHT
#       8:34:13 PM ( 100ROWS ) ... EVERY 8 MINUTES 100 ROWS 
#       6000 / 100 = 60 ... 60 * 8 MINUTES = 480 MINUTES = 8 HOURS
# [1] "Examining ZZJJ at firm_index: 5864"
# [1] "  RETRY xts data of  ZZJJ at firm_index: 5864"
# > Sys.time()
# [1] "2014-01-13 04:09:08 CST" ( APPROX 7.5 H )

dbDisconnect(con)
dbUnloadDriver(drv)

rm("con","drv") 
rm("quoteDividendsgetQuoteOneOfMonth_DF"
  ,"quoteDividendsgetQuoteAllOfMonth_DF"
  ,"quoteDividendsgetQuoteAllOfMonth_Mass_DF"
  ,"allbeginmonthdates_daterange"
) 

# useful
# SELECT COUNT(*) FROM advfn.firmshistory_dividend_partition_thismonth;
# TRUNCATE TABLE advfn.firmshistory_dividend_partition_thismonth;
  

################ END OF EXECUTABLE AREA ###############


####### NEVER USED - WHEN FILLED ( WOULD HAVE ) FILLED MY HARD DRIVE 37G ( WOULD HAVE USED 186G of SPACE ) ###########
################ BEGIN EXECUTABLE AREA #################### 
                                                            
# note: luckily: easy 'one to one' mapping of partition names to partition values

# need useful intermediary table
# JUST A CHANGE IN LITERAL FROM THE SOURCE CODE ABOVE
                                                                                                                        
create_table_month_quality <- "

CREATE TABLE `month_quality` (
  `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ReportType` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ReportingIndicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `EXCHANGE_TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `Quality` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `QualityValue` text COLLATE latin1_general_cs DEFAULT NULL,
  `Source` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `YearMonSource` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `DateOfExecutionOfSource` varchar(64) COLLATE latin1_general_cs DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
PARTITION BY LIST COLUMNS(ThisMonth) (

"

# need to generate the complete set of MySQL partition LIST partitions names
allpartitionnames <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    Xyyyy_mm <- paste0("X",i,"_", if ( j < 10 ) { paste0(0,j) } else { j })
    allpartitionnames <- c(allpartitionnames,Xyyyy_mm)
  }
}
rm("i","j","Xyyyy_mm")

# need to generate the complete set of MySQL partition LIST partitions values
allpartitionvalues <- c()
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
    # of 0 through 9, pad with a leading zero
    yyyy_mm <- paste0(i,"/", if ( j < 10 ) { paste0(0,j) } else { j })
    allpartitionvalues <- c(allpartitionvalues,yyyy_mm)
  }
}
rm("i","j","yyyy_mm")


partition_snippet <- ""
allpartitionnames_index <- 0
if ( length(allpartitionnames) > 0 ) {
  for ( x in allpartitionnames ) {
    allpartitionnames_index <- allpartitionnames_index + 1

partition_snippet <- "
  PARTITION 
    `"
    
    create_table_month_quality <- paste0(create_table_month_quality
       , partition_snippet, allpartitionnames[allpartitionnames_index],"`"
       )
    
     partition_snippet <- "
      VALUES IN ("
    
    create_table_month_quality <- paste0(create_table_month_quality
       , partition_snippet
       # FUTURE: if multiple values per list partition ( I could ) iterate through them
       , paste0("'",allpartitionvalues[allpartitionnames_index],"'")
       ,")"
       ,ifelse( allpartitionnames_index == length(allpartitionnames),"" ,"," )
       ,"
"
       )
    
  }
  
  # end of the MySQL statement ");"
  partition_snippet <- "
);
"

  create_table_month_quality <- paste0(
    create_table_month_quality,
    partition_snippet
  )

}

rm("allpartitionnames","allpartitionvalues","partition_snippet")

# easier to see
writeLines(create_table_month_quality)

# easier to see
fileConn <- file("create_table_month_quality.out.txt")
writeLines(create_table_month_quality,fileConn)
close(fileConn)


################ END EXECUTABLE AREA ####################



####### NEVER USED - WHEN FILLED ( WOULD HAVE ) FILLED MY HARD DRIVE 37G ( WOULD HAVE USED 186G of SPACE ) ###########
################ BEGIN EXECUTABLE AREA ########################


# OUTPUT FROM ABOVE

CREATE TABLE `month_quality` (
  `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ReportType` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ReportingIndicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `EXCHANGE_TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `Quality` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `QualityValue` text COLLATE latin1_general_cs DEFAULT NULL,
  `Source` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `YearMonSource` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `DateOfExecutionOfSource` varchar(64) COLLATE latin1_general_cs DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
PARTITION BY LIST COLUMNS(ThisMonth) (


  PARTITION 
    `X1990_01`
      VALUES IN ('1990/01'),

  PARTITION 
    `X1990_02`
      VALUES IN ('1990/02'),

  PARTITION 
    `X1990_03`
      VALUES IN ('1990/03'),

  PARTITION 
    `X1990_04`
      VALUES IN ('1990/04'),

  PARTITION 
    `X1990_05`
      VALUES IN ('1990/05'),

  PARTITION 
    `X1990_06`
      VALUES IN ('1990/06'),

  PARTITION 
    `X1990_07`
      VALUES IN ('1990/07'),

  PARTITION 
    `X1990_08`
      VALUES IN ('1990/08'),

  PARTITION 
    `X1990_09`
      VALUES IN ('1990/09'),

  PARTITION 
    `X1990_10`
      VALUES IN ('1990/10'),

  PARTITION 
    `X1990_11`
      VALUES IN ('1990/11'),

  PARTITION 
    `X1990_12`
      VALUES IN ('1990/12'),

  PARTITION 
    `X1991_01`
      VALUES IN ('1991/01'),

  PARTITION 
    `X1991_02`
      VALUES IN ('1991/02'),

  PARTITION 
    `X1991_03`
      VALUES IN ('1991/03'),

  PARTITION 
    `X1991_04`
      VALUES IN ('1991/04'),

  PARTITION 
    `X1991_05`
      VALUES IN ('1991/05'),

  PARTITION 
    `X1991_06`
      VALUES IN ('1991/06'),

  PARTITION 
    `X1991_07`
      VALUES IN ('1991/07'),

  PARTITION 
    `X1991_08`
      VALUES IN ('1991/08'),

  PARTITION 
    `X1991_09`
      VALUES IN ('1991/09'),

  PARTITION 
    `X1991_10`
      VALUES IN ('1991/10'),

  PARTITION 
    `X1991_11`
      VALUES IN ('1991/11'),

  PARTITION 
    `X1991_12`
      VALUES IN ('1991/12'),

  PARTITION 
    `X1992_01`
      VALUES IN ('1992/01'),

  PARTITION 
    `X1992_02`
      VALUES IN ('1992/02'),

  PARTITION 
    `X1992_03`
      VALUES IN ('1992/03'),

  PARTITION 
    `X1992_04`
      VALUES IN ('1992/04'),

  PARTITION 
    `X1992_05`
      VALUES IN ('1992/05'),

  PARTITION 
    `X1992_06`
      VALUES IN ('1992/06'),

  PARTITION 
    `X1992_07`
      VALUES IN ('1992/07'),

  PARTITION 
    `X1992_08`
      VALUES IN ('1992/08'),

  PARTITION 
    `X1992_09`
      VALUES IN ('1992/09'),

  PARTITION 
    `X1992_10`
      VALUES IN ('1992/10'),

  PARTITION 
    `X1992_11`
      VALUES IN ('1992/11'),

  PARTITION 
    `X1992_12`
      VALUES IN ('1992/12'),

  PARTITION 
    `X1993_01`
      VALUES IN ('1993/01'),

  PARTITION 
    `X1993_02`
      VALUES IN ('1993/02'),

  PARTITION 
    `X1993_03`
      VALUES IN ('1993/03'),

  PARTITION 
    `X1993_04`
      VALUES IN ('1993/04'),

  PARTITION 
    `X1993_05`
      VALUES IN ('1993/05'),

  PARTITION 
    `X1993_06`
      VALUES IN ('1993/06'),

  PARTITION 
    `X1993_07`
      VALUES IN ('1993/07'),

  PARTITION 
    `X1993_08`
      VALUES IN ('1993/08'),

  PARTITION 
    `X1993_09`
      VALUES IN ('1993/09'),

  PARTITION 
    `X1993_10`
      VALUES IN ('1993/10'),

  PARTITION 
    `X1993_11`
      VALUES IN ('1993/11'),

  PARTITION 
    `X1993_12`
      VALUES IN ('1993/12'),

  PARTITION 
    `X1994_01`
      VALUES IN ('1994/01'),

  PARTITION 
    `X1994_02`
      VALUES IN ('1994/02'),

  PARTITION 
    `X1994_03`
      VALUES IN ('1994/03'),

  PARTITION 
    `X1994_04`
      VALUES IN ('1994/04'),

  PARTITION 
    `X1994_05`
      VALUES IN ('1994/05'),

  PARTITION 
    `X1994_06`
      VALUES IN ('1994/06'),

  PARTITION 
    `X1994_07`
      VALUES IN ('1994/07'),

  PARTITION 
    `X1994_08`
      VALUES IN ('1994/08'),

  PARTITION 
    `X1994_09`
      VALUES IN ('1994/09'),

  PARTITION 
    `X1994_10`
      VALUES IN ('1994/10'),

  PARTITION 
    `X1994_11`
      VALUES IN ('1994/11'),

  PARTITION 
    `X1994_12`
      VALUES IN ('1994/12'),

  PARTITION 
    `X1995_01`
      VALUES IN ('1995/01'),

  PARTITION 
    `X1995_02`
      VALUES IN ('1995/02'),

  PARTITION 
    `X1995_03`
      VALUES IN ('1995/03'),

  PARTITION 
    `X1995_04`
      VALUES IN ('1995/04'),

  PARTITION 
    `X1995_05`
      VALUES IN ('1995/05'),

  PARTITION 
    `X1995_06`
      VALUES IN ('1995/06'),

  PARTITION 
    `X1995_07`
      VALUES IN ('1995/07'),

  PARTITION 
    `X1995_08`
      VALUES IN ('1995/08'),

  PARTITION 
    `X1995_09`
      VALUES IN ('1995/09'),

  PARTITION 
    `X1995_10`
      VALUES IN ('1995/10'),

  PARTITION 
    `X1995_11`
      VALUES IN ('1995/11'),

  PARTITION 
    `X1995_12`
      VALUES IN ('1995/12'),

  PARTITION 
    `X1996_01`
      VALUES IN ('1996/01'),

  PARTITION 
    `X1996_02`
      VALUES IN ('1996/02'),

  PARTITION 
    `X1996_03`
      VALUES IN ('1996/03'),

  PARTITION 
    `X1996_04`
      VALUES IN ('1996/04'),

  PARTITION 
    `X1996_05`
      VALUES IN ('1996/05'),

  PARTITION 
    `X1996_06`
      VALUES IN ('1996/06'),

  PARTITION 
    `X1996_07`
      VALUES IN ('1996/07'),

  PARTITION 
    `X1996_08`
      VALUES IN ('1996/08'),

  PARTITION 
    `X1996_09`
      VALUES IN ('1996/09'),

  PARTITION 
    `X1996_10`
      VALUES IN ('1996/10'),

  PARTITION 
    `X1996_11`
      VALUES IN ('1996/11'),

  PARTITION 
    `X1996_12`
      VALUES IN ('1996/12'),

  PARTITION 
    `X1997_01`
      VALUES IN ('1997/01'),

  PARTITION 
    `X1997_02`
      VALUES IN ('1997/02'),

  PARTITION 
    `X1997_03`
      VALUES IN ('1997/03'),

  PARTITION 
    `X1997_04`
      VALUES IN ('1997/04'),

  PARTITION 
    `X1997_05`
      VALUES IN ('1997/05'),

  PARTITION 
    `X1997_06`
      VALUES IN ('1997/06'),

  PARTITION 
    `X1997_07`
      VALUES IN ('1997/07'),

  PARTITION 
    `X1997_08`
      VALUES IN ('1997/08'),

  PARTITION 
    `X1997_09`
      VALUES IN ('1997/09'),

  PARTITION 
    `X1997_10`
      VALUES IN ('1997/10'),

  PARTITION 
    `X1997_11`
      VALUES IN ('1997/11'),

  PARTITION 
    `X1997_12`
      VALUES IN ('1997/12'),

  PARTITION 
    `X1998_01`
      VALUES IN ('1998/01'),

  PARTITION 
    `X1998_02`
      VALUES IN ('1998/02'),

  PARTITION 
    `X1998_03`
      VALUES IN ('1998/03'),

  PARTITION 
    `X1998_04`
      VALUES IN ('1998/04'),

  PARTITION 
    `X1998_05`
      VALUES IN ('1998/05'),

  PARTITION 
    `X1998_06`
      VALUES IN ('1998/06'),

  PARTITION 
    `X1998_07`
      VALUES IN ('1998/07'),

  PARTITION 
    `X1998_08`
      VALUES IN ('1998/08'),

  PARTITION 
    `X1998_09`
      VALUES IN ('1998/09'),

  PARTITION 
    `X1998_10`
      VALUES IN ('1998/10'),

  PARTITION 
    `X1998_11`
      VALUES IN ('1998/11'),

  PARTITION 
    `X1998_12`
      VALUES IN ('1998/12'),

  PARTITION 
    `X1999_01`
      VALUES IN ('1999/01'),

  PARTITION 
    `X1999_02`
      VALUES IN ('1999/02'),

  PARTITION 
    `X1999_03`
      VALUES IN ('1999/03'),

  PARTITION 
    `X1999_04`
      VALUES IN ('1999/04'),

  PARTITION 
    `X1999_05`
      VALUES IN ('1999/05'),

  PARTITION 
    `X1999_06`
      VALUES IN ('1999/06'),

  PARTITION 
    `X1999_07`
      VALUES IN ('1999/07'),

  PARTITION 
    `X1999_08`
      VALUES IN ('1999/08'),

  PARTITION 
    `X1999_09`
      VALUES IN ('1999/09'),

  PARTITION 
    `X1999_10`
      VALUES IN ('1999/10'),

  PARTITION 
    `X1999_11`
      VALUES IN ('1999/11'),

  PARTITION 
    `X1999_12`
      VALUES IN ('1999/12'),

  PARTITION 
    `X2000_01`
      VALUES IN ('2000/01'),

  PARTITION 
    `X2000_02`
      VALUES IN ('2000/02'),

  PARTITION 
    `X2000_03`
      VALUES IN ('2000/03'),

  PARTITION 
    `X2000_04`
      VALUES IN ('2000/04'),

  PARTITION 
    `X2000_05`
      VALUES IN ('2000/05'),

  PARTITION 
    `X2000_06`
      VALUES IN ('2000/06'),

  PARTITION 
    `X2000_07`
      VALUES IN ('2000/07'),

  PARTITION 
    `X2000_08`
      VALUES IN ('2000/08'),

  PARTITION 
    `X2000_09`
      VALUES IN ('2000/09'),

  PARTITION 
    `X2000_10`
      VALUES IN ('2000/10'),

  PARTITION 
    `X2000_11`
      VALUES IN ('2000/11'),

  PARTITION 
    `X2000_12`
      VALUES IN ('2000/12'),

  PARTITION 
    `X2001_01`
      VALUES IN ('2001/01'),

  PARTITION 
    `X2001_02`
      VALUES IN ('2001/02'),

  PARTITION 
    `X2001_03`
      VALUES IN ('2001/03'),

  PARTITION 
    `X2001_04`
      VALUES IN ('2001/04'),

  PARTITION 
    `X2001_05`
      VALUES IN ('2001/05'),

  PARTITION 
    `X2001_06`
      VALUES IN ('2001/06'),

  PARTITION 
    `X2001_07`
      VALUES IN ('2001/07'),

  PARTITION 
    `X2001_08`
      VALUES IN ('2001/08'),

  PARTITION 
    `X2001_09`
      VALUES IN ('2001/09'),

  PARTITION 
    `X2001_10`
      VALUES IN ('2001/10'),

  PARTITION 
    `X2001_11`
      VALUES IN ('2001/11'),

  PARTITION 
    `X2001_12`
      VALUES IN ('2001/12'),

  PARTITION 
    `X2002_01`
      VALUES IN ('2002/01'),

  PARTITION 
    `X2002_02`
      VALUES IN ('2002/02'),

  PARTITION 
    `X2002_03`
      VALUES IN ('2002/03'),

  PARTITION 
    `X2002_04`
      VALUES IN ('2002/04'),

  PARTITION 
    `X2002_05`
      VALUES IN ('2002/05'),

  PARTITION 
    `X2002_06`
      VALUES IN ('2002/06'),

  PARTITION 
    `X2002_07`
      VALUES IN ('2002/07'),

  PARTITION 
    `X2002_08`
      VALUES IN ('2002/08'),

  PARTITION 
    `X2002_09`
      VALUES IN ('2002/09'),

  PARTITION 
    `X2002_10`
      VALUES IN ('2002/10'),

  PARTITION 
    `X2002_11`
      VALUES IN ('2002/11'),

  PARTITION 
    `X2002_12`
      VALUES IN ('2002/12'),

  PARTITION 
    `X2003_01`
      VALUES IN ('2003/01'),

  PARTITION 
    `X2003_02`
      VALUES IN ('2003/02'),

  PARTITION 
    `X2003_03`
      VALUES IN ('2003/03'),

  PARTITION 
    `X2003_04`
      VALUES IN ('2003/04'),

  PARTITION 
    `X2003_05`
      VALUES IN ('2003/05'),

  PARTITION 
    `X2003_06`
      VALUES IN ('2003/06'),

  PARTITION 
    `X2003_07`
      VALUES IN ('2003/07'),

  PARTITION 
    `X2003_08`
      VALUES IN ('2003/08'),

  PARTITION 
    `X2003_09`
      VALUES IN ('2003/09'),

  PARTITION 
    `X2003_10`
      VALUES IN ('2003/10'),

  PARTITION 
    `X2003_11`
      VALUES IN ('2003/11'),

  PARTITION 
    `X2003_12`
      VALUES IN ('2003/12'),

  PARTITION 
    `X2004_01`
      VALUES IN ('2004/01'),

  PARTITION 
    `X2004_02`
      VALUES IN ('2004/02'),

  PARTITION 
    `X2004_03`
      VALUES IN ('2004/03'),

  PARTITION 
    `X2004_04`
      VALUES IN ('2004/04'),

  PARTITION 
    `X2004_05`
      VALUES IN ('2004/05'),

  PARTITION 
    `X2004_06`
      VALUES IN ('2004/06'),

  PARTITION 
    `X2004_07`
      VALUES IN ('2004/07'),

  PARTITION 
    `X2004_08`
      VALUES IN ('2004/08'),

  PARTITION 
    `X2004_09`
      VALUES IN ('2004/09'),

  PARTITION 
    `X2004_10`
      VALUES IN ('2004/10'),

  PARTITION 
    `X2004_11`
      VALUES IN ('2004/11'),

  PARTITION 
    `X2004_12`
      VALUES IN ('2004/12'),

  PARTITION 
    `X2005_01`
      VALUES IN ('2005/01'),

  PARTITION 
    `X2005_02`
      VALUES IN ('2005/02'),

  PARTITION 
    `X2005_03`
      VALUES IN ('2005/03'),

  PARTITION 
    `X2005_04`
      VALUES IN ('2005/04'),

  PARTITION 
    `X2005_05`
      VALUES IN ('2005/05'),

  PARTITION 
    `X2005_06`
      VALUES IN ('2005/06'),

  PARTITION 
    `X2005_07`
      VALUES IN ('2005/07'),

  PARTITION 
    `X2005_08`
      VALUES IN ('2005/08'),

  PARTITION 
    `X2005_09`
      VALUES IN ('2005/09'),

  PARTITION 
    `X2005_10`
      VALUES IN ('2005/10'),

  PARTITION 
    `X2005_11`
      VALUES IN ('2005/11'),

  PARTITION 
    `X2005_12`
      VALUES IN ('2005/12'),

  PARTITION 
    `X2006_01`
      VALUES IN ('2006/01'),

  PARTITION 
    `X2006_02`
      VALUES IN ('2006/02'),

  PARTITION 
    `X2006_03`
      VALUES IN ('2006/03'),

  PARTITION 
    `X2006_04`
      VALUES IN ('2006/04'),

  PARTITION 
    `X2006_05`
      VALUES IN ('2006/05'),

  PARTITION 
    `X2006_06`
      VALUES IN ('2006/06'),

  PARTITION 
    `X2006_07`
      VALUES IN ('2006/07'),

  PARTITION 
    `X2006_08`
      VALUES IN ('2006/08'),

  PARTITION 
    `X2006_09`
      VALUES IN ('2006/09'),

  PARTITION 
    `X2006_10`
      VALUES IN ('2006/10'),

  PARTITION 
    `X2006_11`
      VALUES IN ('2006/11'),

  PARTITION 
    `X2006_12`
      VALUES IN ('2006/12'),

  PARTITION 
    `X2007_01`
      VALUES IN ('2007/01'),

  PARTITION 
    `X2007_02`
      VALUES IN ('2007/02'),

  PARTITION 
    `X2007_03`
      VALUES IN ('2007/03'),

  PARTITION 
    `X2007_04`
      VALUES IN ('2007/04'),

  PARTITION 
    `X2007_05`
      VALUES IN ('2007/05'),

  PARTITION 
    `X2007_06`
      VALUES IN ('2007/06'),

  PARTITION 
    `X2007_07`
      VALUES IN ('2007/07'),

  PARTITION 
    `X2007_08`
      VALUES IN ('2007/08'),

  PARTITION 
    `X2007_09`
      VALUES IN ('2007/09'),

  PARTITION 
    `X2007_10`
      VALUES IN ('2007/10'),

  PARTITION 
    `X2007_11`
      VALUES IN ('2007/11'),

  PARTITION 
    `X2007_12`
      VALUES IN ('2007/12'),

  PARTITION 
    `X2008_01`
      VALUES IN ('2008/01'),

  PARTITION 
    `X2008_02`
      VALUES IN ('2008/02'),

  PARTITION 
    `X2008_03`
      VALUES IN ('2008/03'),

  PARTITION 
    `X2008_04`
      VALUES IN ('2008/04'),

  PARTITION 
    `X2008_05`
      VALUES IN ('2008/05'),

  PARTITION 
    `X2008_06`
      VALUES IN ('2008/06'),

  PARTITION 
    `X2008_07`
      VALUES IN ('2008/07'),

  PARTITION 
    `X2008_08`
      VALUES IN ('2008/08'),

  PARTITION 
    `X2008_09`
      VALUES IN ('2008/09'),

  PARTITION 
    `X2008_10`
      VALUES IN ('2008/10'),

  PARTITION 
    `X2008_11`
      VALUES IN ('2008/11'),

  PARTITION 
    `X2008_12`
      VALUES IN ('2008/12'),

  PARTITION 
    `X2009_01`
      VALUES IN ('2009/01'),

  PARTITION 
    `X2009_02`
      VALUES IN ('2009/02'),

  PARTITION 
    `X2009_03`
      VALUES IN ('2009/03'),

  PARTITION 
    `X2009_04`
      VALUES IN ('2009/04'),

  PARTITION 
    `X2009_05`
      VALUES IN ('2009/05'),

  PARTITION 
    `X2009_06`
      VALUES IN ('2009/06'),

  PARTITION 
    `X2009_07`
      VALUES IN ('2009/07'),

  PARTITION 
    `X2009_08`
      VALUES IN ('2009/08'),

  PARTITION 
    `X2009_09`
      VALUES IN ('2009/09'),

  PARTITION 
    `X2009_10`
      VALUES IN ('2009/10'),

  PARTITION 
    `X2009_11`
      VALUES IN ('2009/11'),

  PARTITION 
    `X2009_12`
      VALUES IN ('2009/12'),

  PARTITION 
    `X2010_01`
      VALUES IN ('2010/01'),

  PARTITION 
    `X2010_02`
      VALUES IN ('2010/02'),

  PARTITION 
    `X2010_03`
      VALUES IN ('2010/03'),

  PARTITION 
    `X2010_04`
      VALUES IN ('2010/04'),

  PARTITION 
    `X2010_05`
      VALUES IN ('2010/05'),

  PARTITION 
    `X2010_06`
      VALUES IN ('2010/06'),

  PARTITION 
    `X2010_07`
      VALUES IN ('2010/07'),

  PARTITION 
    `X2010_08`
      VALUES IN ('2010/08'),

  PARTITION 
    `X2010_09`
      VALUES IN ('2010/09'),

  PARTITION 
    `X2010_10`
      VALUES IN ('2010/10'),

  PARTITION 
    `X2010_11`
      VALUES IN ('2010/11'),

  PARTITION 
    `X2010_12`
      VALUES IN ('2010/12'),

  PARTITION 
    `X2011_01`
      VALUES IN ('2011/01'),

  PARTITION 
    `X2011_02`
      VALUES IN ('2011/02'),

  PARTITION 
    `X2011_03`
      VALUES IN ('2011/03'),

  PARTITION 
    `X2011_04`
      VALUES IN ('2011/04'),

  PARTITION 
    `X2011_05`
      VALUES IN ('2011/05'),

  PARTITION 
    `X2011_06`
      VALUES IN ('2011/06'),

  PARTITION 
    `X2011_07`
      VALUES IN ('2011/07'),

  PARTITION 
    `X2011_08`
      VALUES IN ('2011/08'),

  PARTITION 
    `X2011_09`
      VALUES IN ('2011/09'),

  PARTITION 
    `X2011_10`
      VALUES IN ('2011/10'),

  PARTITION 
    `X2011_11`
      VALUES IN ('2011/11'),

  PARTITION 
    `X2011_12`
      VALUES IN ('2011/12'),

  PARTITION 
    `X2012_01`
      VALUES IN ('2012/01'),

  PARTITION 
    `X2012_02`
      VALUES IN ('2012/02'),

  PARTITION 
    `X2012_03`
      VALUES IN ('2012/03'),

  PARTITION 
    `X2012_04`
      VALUES IN ('2012/04'),

  PARTITION 
    `X2012_05`
      VALUES IN ('2012/05'),

  PARTITION 
    `X2012_06`
      VALUES IN ('2012/06'),

  PARTITION 
    `X2012_07`
      VALUES IN ('2012/07'),

  PARTITION 
    `X2012_08`
      VALUES IN ('2012/08'),

  PARTITION 
    `X2012_09`
      VALUES IN ('2012/09'),

  PARTITION 
    `X2012_10`
      VALUES IN ('2012/10'),

  PARTITION 
    `X2012_11`
      VALUES IN ('2012/11'),

  PARTITION 
    `X2012_12`
      VALUES IN ('2012/12'),

  PARTITION 
    `X2013_01`
      VALUES IN ('2013/01'),

  PARTITION 
    `X2013_02`
      VALUES IN ('2013/02'),

  PARTITION 
    `X2013_03`
      VALUES IN ('2013/03'),

  PARTITION 
    `X2013_04`
      VALUES IN ('2013/04'),

  PARTITION 
    `X2013_05`
      VALUES IN ('2013/05'),

  PARTITION 
    `X2013_06`
      VALUES IN ('2013/06'),

  PARTITION 
    `X2013_07`
      VALUES IN ('2013/07'),

  PARTITION 
    `X2013_08`
      VALUES IN ('2013/08'),

  PARTITION 
    `X2013_09`
      VALUES IN ('2013/09'),

  PARTITION 
    `X2013_10`
      VALUES IN ('2013/10'),

  PARTITION 
    `X2013_11`
      VALUES IN ('2013/11'),

  PARTITION 
    `X2013_12`
      VALUES IN ('2013/12')

);

################ END EXECUTABLE AREA ########################



########## NEVER USED - FILLED MY HARD DRIVE 37G ( WOULD HAVE USED 186G of SPACE ) ###########
#############################################

-- BEGIN EXECUTABLE AREA --

-- insert uk.advfn data into the intermediary verticle table partioned by ThisMonth

-- CREATE TABLE `month_quality` (
-- `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
-- `ReportType` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
-- `ReportingIndicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
-- `EXCHANGE_TICKER` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
-- `Quality` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
-- `QualityValue` text COLLATE latin1_general_cs DEFAULT NULL,
-- `Source` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
-- `YearMonSource` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
-- `DateOfExecutionOfSource` varchar(64) COLLATE latin1_general_cs DEFAULT NULL
-- ) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
-- PARTITION BY LIST  COLUMNS(ThisMonth)

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`sp_some`;

DELIMITER $$
CREATE PROCEDURE sp_some()
  MODIFIES SQL DATA
BEGIN
  DECLARE l_last_row_fetched INT;
  DECLARE l_column_name         VARCHAR(64);
  DECLARE l_column_time         VARCHAR(64);

  -- +-------------+-------------+
  -- | COLUMN_NAME | COLUMN_TIME |
  -- +-------------+-------------+
  -- | X1990_01    | 1990/01     |
  -- | X1990_02    | 1990/02     |
  
  -- 288 rows
  DECLARE c_cursor cursor FOR
    SELECT COLUMN_NAME, REPLACE(REPLACE(COLUMN_NAME,'_','/'),'X','') "COLUMN_TIME"
      FROM INFORMATION_SCHEMA.COLUMNS
      WHERE table_schema = 'advfn' AND
            table_name = 'firmshistory_partition_rownombres' AND 
            column_name LIKE 'X%'
    ORDER BY 1;
  -- debugging
  -- AND column_name = 'X2013_04'
    
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET l_last_row_fetched=1;
  SET l_last_row_fetched=0;
  

    OPEN c_cursor;
      cursor_loop:LOOP
        FETCH c_cursor INTO l_column_name, l_column_time;
      
        IF l_last_row_fetched=1 THEN
          LEAVE cursor_loop;
        END IF;
        
        -- INSERT INTO month_quality
        -- (ThisMonth, ReportType, ReportingIndicator, EXCHANGE_TICKER,
        -- Quality, QualityValue,
        -- Source, YearMonSource, DateOfExecutionOfSource
        -- )
        -- SELECT '2013/12','Q',NULL,EXCHANGE_TICKER,
        --         rownombres, X2013_12,
        --          'uk.advfn','2013/09','2013/10/10'
        --  FROM firmshistory_partition_rownombres
        
        SET @l_source = CONCAT("
        INSERT INTO month_quality
        (ThisMonth, ReportType, ReportingIndicator, EXCHANGE_TICKER, 
         Quality, QualityValue, 
         Source, YearMonSource, DateOfExecutionOfSource
         ) 
           SELECT '",l_column_time,"','Q',NULL,EXCHANGE_TICKER,
                   rownombres, ",l_column_name,",
                   'uk.advfn','2013/09','2013/10/10'
            FROM firmshistory_partition_rownombres");
        -- debugging
        -- WHERE EXCHANGE_TICKER = 'NYSE_WMT'
        
        SELECT @l_source;
        
        -- 90 seconds per each of 288 = 7.2 hours
        PREPARE stmt1 FROM @l_source;
        EXECUTE stmt1;
        DEALLOCATE PREPARE stmt1;
        
      END LOOP cursor_loop;
    CLOSE c_cursor;
    SET l_last_row_fetched=0;

END$$

DELIMITER ;
CALL sp_some();

-- END EXECUTABLE AREA --

#############################################



############# BEGIN EXECUTBLE ( NEVER USED )##########

# AN EXAMPLE STARTER CODE OF DOING A DIFFICULT
# JOB OF MAKEING SOMETHING HORIZONTAL INTO SOMETHING VERTICLE

# IT WILL DYNAMICALLY BUILD A 'CASE' STATEMENT

--

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`sp_emptycolumns`;

DELIMITER $$
CREATE PROCEDURE sp_emptycolumns()
  MODIFIES SQL DATA
BEGIN

  DECLARE l_last_row_fetched INT;
  DECLARE l_column_name         VARCHAR(64);
  DECLARE l_column_time         VARCHAR(64);

  -- 288 rows
  -- need to create variables "SET @XYYY_MM = NULL" ( required for the CASE statement )
  DECLARE c_cursor CURSOR FOR 
    SELECT COLUMN_NAME, REPLACE(REPLACE(COLUMN_NAME,'_','/'),'X','') "COLUMN_TIME"
      FROM INFORMATION_SCHEMA.COLUMNS
      WHERE table_schema = 'advfn' AND
            table_name = 'firmshistory_partition_rownombres' AND 
            column_name LIKE 'X%'
    ORDER BY 1 DESC;
  -- debugging
  -- AND column_name = 'X2013_04'
  
  -- 288 rows
  -- need to create CASE statement select_items
  DECLARE b_cursor CURSOR FOR 
  SELECT COLUMN_NAME, REPLACE(REPLACE(COLUMN_NAME,'_','/'),'X','') "COLUMN_TIME"
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE table_schema = 'advfn' AND
          table_name = 'firmshistory_partition_rownombres' AND 
          column_name LIKE 'X%'
  ORDER BY 1 DESC;
    
  -- find where the PARTITION ( quarterly_indicator ) columns are not null
  DECLARE d_cursor CURSOR FOR 
  SELECT COLUMN_NAME, REPLACE(REPLACE(COLUMN_NAME,'_','/'),'X','') "COLUMN_TIME"
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE table_schema = 'advfn' AND
          table_name = 'firmshistory_partition_rownombres' AND 
          column_name LIKE 'X%'
  ORDER BY 1 DESC;
    
    
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET l_last_row_fetched=1;
  
  SET SESSION group_concat_max_len = 10240;
  
  SET l_last_row_fetched=0;
  
    OPEN c_cursor;
      cursor_loop:LOOP
        FETCH c_cursor INTO l_column_name, l_column_time;
      
        IF l_last_row_fetched=1 THEN
          LEAVE cursor_loop;
        END IF;
        
        SET @l_source = CONCAT("SET @",l_column_name," = NULL");
        -- debugging
        -- WHERE EXCHANGE_TICKER = 'NYSE_WMT'
        
        -- just see output
        -- SELECT @l_source;
        -- +----------------------+
        -- | @l_source            |
        -- +----------------------+
        -- | SET @X1990_01 = NULL |  last is SET @X2013_12 = NULL
        -- +----------------------+
        -- 1 row in set (0.06 sec)
        
        PREPARE stmt1 FROM @l_source;
        EXECUTE stmt1;
        DEALLOCATE PREPARE stmt1;
        
      END LOOP cursor_loop;
    CLOSE c_cursor;
    SET l_last_row_fetched=0;
    
  
  SET @l_source = "SELECT";
  SET l_last_row_fetched=0;
  SET @l_selitemcollection = "";
  SET @cursor_loop_index = 0;
  OPEN b_cursor;
  cursor_loop:LOOP
      FETCH b_cursor INTO l_column_name, l_column_time;
      
      IF l_last_row_fetched=1 THEN
        LEAVE cursor_loop;
      END IF;
      SET @cursor_loop_index = @cursor_loop_index + 1;
      
      SET @l_selitemcollection = CONCAT(@l_selitemcollection,"
      CASE WHEN ",l_column_name," IS NOT NULL AND ",l_column_name," != '' THEN '",l_column_name,"' ELSE NULL END, ");
      -- +---------------------------------------------------------------------------------+
      -- | @l_selitem                                                                      |
      -- +---------------------------------------------------------------------------------+
      -- | CASE WHEN X2013_12 IS NOT NULL AND X2013_12 != '' THEN 'X2013_12' ELSE NULL END |
      -- +---------------------------------------------------------------------------------+

  END LOOP cursor_loop;
  CLOSE b_cursor;
  SET l_last_row_fetched=0;
  
  -- if at least one record returned
  -- simple: just remove that last comma_space
  IF @cursor_loop_index > 0 THEN
    SET @l_selitemcollection = SUBSTRING(@l_selitemcollection, 1,CHAR_LENGTH(@l_selitemcollection) - 2 );
  END IF;
  
  -- SELECT plus select list
  SET @l_source = CONCAT(@l_source,@l_selitemcollection);
  
  -- print
  -- SELECT @cursor_loop_index;
  -- SELECT @l_source;
  
  -- SELECT
  -- CASE WHEN X1990_02 IS NOT NULL AND X1990_02 != '' THEN 'X1990_02' ELSE NULL END,
  -- CASE WHEN X1990_01 IS NOT NULL AND X1990_01 != '' THEN 'X1990_01' ELSE NULL END |
  
  SET @into_list = "";
  SELECT GROUP_CONCAT(CONCAT("@",COLUMN_NAME) ORDER BY 1 DESC SEPARATOR ",") INTO @into_list
    FROM INFORMATION_SCHEMA.COLUMNS
     WHERE table_schema = 'advfn' AND
           table_name = 'firmshistory_partition_rownombres' AND 
           column_name LIKE 'X%'
  ORDER BY 1 DESC;

  -- print
  -- SELECT @into_list;
  
  SET @l_source = CONCAT(@l_source,"
  INTO ",  @into_list);
  
  -- print
  -- SELECT @l_source;
  
  SET @l_source = CONCAT(@l_source,"
    FROM firmshistory_partition_rownombres PARTITION (quarterly_indicator ) 
      WHERE EXCHANGE_TICKER IN ('NYSE_WMT')");
  -- above: NEED and OUTER LOOP ( I will have to come back )
  
  -- print
  -- SELECT @l_source;
  
  -- remove thee NULL columns
  -- LEFT_OFF
  
  -- load @ user variables into the environment 
  PREPARE stmt1 FROM @l_source;
  EXECUTE stmt1;
  DEALLOCATE PREPARE stmt1;
  
  -- @ user variables ARE loaded into the external environment ( not local scope confined )
  -- mysql> SELECT @X2012_07;
  -- +-----------+
  -- | @X2012_07 |
  -- +-----------+
  -- | X2012_07  |
  -- +-----------+
  
  -- SELECT @X2012_07;
  
  SET @l_source = " SELECT a.COLUMN_NAME FROM (
      ";

  SET l_last_row_fetched=0;
  SET @cursor_loop_index = 0;

  OPEN d_cursor;
  cursor_loop:LOOP
    FETCH d_cursor INTO l_column_name, l_column_time;
    
    IF l_last_row_fetched=1 THEN
      LEAVE cursor_loop;
    END IF;
    SET @cursor_loop_index = @cursor_loop_index + 1;
      
      SET @l_source = CONCAT(@l_source,"SELECT @",l_column_name," AS COLUMN_NAME UNION ALL
      ");
      
  END LOOP cursor_loop;
  CLOSE d_cursor;
  
  SET l_last_row_fetched=0;
  
  -- if at least one record returned
  -- simple: just remove that last UNION ALL
  IF @cursor_loop_index > 0 THEN
    SET @l_source = SUBSTRING(@l_source, 1,CHAR_LENGTH(@l_source) - 16 );
  END IF;
  
  -- SELECT plus select list
  SET @l_source = CONCAT(@l_source,"
  ) a
  WHERE a.COLUMN_NAME IS NOT NULL LIMIT 1");
  
  -- print
  SELECT @l_source;
  -- SELECT a.COLUMN_NAME FROM (
  --       SELECT @X2013_12 AS COLUMN_NAME UNION ALL
  --       SELECT @X2013_11 AS COLUMN_NAME UNION ALL
  --       SELECT @X1990_02 AS COLUMN_NAME UNION ALL
  --       SELECT @X1990_01 AS COLUMN_NAME
  -- ) a
  --   WHERE a.COLUMN_NAME IS NOT NULL LIMIT 1 
  
  
END$$

DELIMITER ;
CALL sp_emptycolumns();

-- END EXECUTABLE AREA --

################ END EXECUTABLE ( NEVER USED )###########



############################### EXECUTABLE AREA: BEGIN ################################

-- need a friendly SQL useful format of TIME down AND attributes ACROSS
-- need the 'balance sheet' and 'income statement' items across ( partition names ) 
-- need the time ( YYYY_MM ) down


-- base time table

CREATE TABLE firmshistory_thismonth
AS
SELECT DISTINCT MARKETCAP, SECTOR, INDUSTRY, EXCHANGE_TICKER
  FROM firmshistory_partition_rownombres
    ORDER BY SECTOR, INDUSTRY, EXCHANGE_TICKER;

-- include the time (thismonth) column

ALTER TABLE advfn.firmshistory_thismonth
 ADD ThisMonth VARCHAR(64) AFTER EXCHANGE_TICKER;
 
 
 -- add ACROSS: all of the other 'balance sheet' and 'income statement' attributes
 -- ( price and sumdividends will be added later )
 
SELECT CONCAT("ALTER TABLE ","advfn",".", "firmshistory_thismonth"
              ," ADD ",PARTITION_NAME," VARCHAR(64);") AS exec_value
FROM 
   INFORMATION_SCHEMA.PARTITIONS
WHERE 
  table_schema = 'advfn' AND 
  table_name = 'firmshistory_partition_rownombres' AND
  partition_name LIKE '%'
ORDER BY 
  PARTITION_ORDINAL_POSITION;
 
 
 
-- actually add 'balance sheet' and 'income statement' attributies ( columns )
-- the new column names are the older firmshistory_partition_rownombres partition names 

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`annonproc`;

DELIMITER $$
CREATE PROCEDURE advfn.annonproc()
  MODIFIES SQL DATA
BEGIN
  DECLARE l_last_row_fetched INT;
  DECLARE l_exec_value          VARCHAR(1023);

  -- add the columns from older irmshistory_partition_rownombres partition names 
  DECLARE c_cursor cursor FOR
    SELECT CONCAT("ALTER TABLE ","advfn",".", "firmshistory_thismonth"
                  ," ADD ",PARTITION_NAME," VARCHAR(64)") AS exec_value
    FROM 
       INFORMATION_SCHEMA.PARTITIONS
    WHERE 
      table_schema = 'advfn' AND 
      table_name = 'firmshistory_partition_rownombres' AND
      partition_name LIKE '%'
    ORDER BY 
      PARTITION_ORDINAL_POSITION;
  
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET l_last_row_fetched=1;
  SET l_last_row_fetched=0;
  
    OPEN c_cursor;
      cursor_loop:LOOP
        FETCH c_cursor INTO l_exec_value;
      
        IF l_last_row_fetched=1 THEN
          LEAVE cursor_loop;
        END IF;
        
        SET @l_source = l_exec_value;
        
        SELECT @l_source;
        
        PREPARE stmt1 FROM @l_source;
        EXECUTE stmt1;   -- actual execution of "ALTER TABLE advfn.firmshistory_thismonth ADD"
        DEALLOCATE PREPARE stmt1;
        
      END LOOP cursor_loop;
    CLOSE c_cursor;
    SET l_last_row_fetched=0;

END$$

DELIMITER ;
CALL annonproc();

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`annonproc`;

 
 -- create firmshistory_thismonth_partition
 -- just take the columns from     firmshistory_thismonth
 -- just take the partitions from  firmshistory_quote_partition_thismonth
 
 CREATE TABLE `firmshistory_thismonth_partition` (
  `MARKETCAP` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `SECTOR` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `INDUSTRY` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `EXCHANGE_TICKER` varchar(21) COLLATE latin1_general_cs DEFAULT NULL,
  `ThisMonth` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `quarter_end_date` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `date_preliminary_data_loaded` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `earnings_period_indicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `quarterly_indicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_earnings_indicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `template_indicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `preliminary_full_context_ind` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `projected_fiscal_year_date` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `number_of_months_last_report_period` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_statementoperating_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `adjustments_to_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cost_of_sales` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cost_of_sales_with_depreciation` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `gross_margin` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `gross_operating_profit` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `research__and__development__lpn_r_and_d_rpn__expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `selling_cmma__general__and__administrative__lpn_sg_and_a_rpn__ex` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `advertising` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `operating_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ebitda` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `depreciation` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `depreciation__lpn_unrecognized_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `amortization` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `amortization_of_intangibles` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `operating_profit_after_depreciation` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `interest_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `earnings_from_equity_interest` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_income_net` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_cmma__acquired_in_process_r_and_a` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_cmma__restructuring_and_m_and_a` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_special_charges` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ebit` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `interest_expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `pre_hyp_tax_income_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `minority_interest` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `pref_prd__securities_of_subsid_prd__trust` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_before_income_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income__lpn_continuing_operations_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income__lpn_discontinued_operations_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income__lpn_total_operations_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_from_cum_prd__effect_of_acct_prd__change` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_from_tax_loss_carryforward` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_net_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income_available_for_common` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `preferred_dividends` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `excise_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_ask_basic_eps__lpn_continuing_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__lpn_discontinued_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps_from_total_operations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__lpn_extraordinary_items_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__lpn_tax_loss_carry_forward_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__lpn_other_gains_rsl_losses_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__hyp__total` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__hyp__normalized` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_ask_diluted_eps__lpn_continuing_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__lpn_discontinued_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps_from_total_operations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__lpn_extraordinary_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__lpn_cum_prd__effect_of_acct_prd__change_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__lpn_tax_loss_carry_forward_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__lpn_other_gains_rsl_losses_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__hyp__total` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__hyp__normalized` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_statement__lpn_year_hyp_to_hyp_date_rpn_revenue__lpn_ytd_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income_from_total_operations__lpn_ytd_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `eps_from_total_operations__lpn_ytd_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `dividends_paid_per_share__lpn_ytd_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `balance_sheetassetscash__and__equivalents` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `restricted_cash` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `marketable_securities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accounts_receivable` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `loans_receivable` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_receivable` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `receivables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventories_cmma__raw_materials` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventories_cmma__work_in_progress` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventories_cmma__purchased_components` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventories_cmma__finished_goods` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventories_cmma__other` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventories_cmma__adjustments__and__allowances` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventories` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `prepaid_expenses` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `current_defered_income_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_current_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_current_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `land_and_improvements` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `building_and_improvements` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `machinery_cmma__furniture__and__equipment` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `construction_in_progress` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_fixed_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_fixed_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `gross_fixed_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accumulated_depreciation` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_fixed_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `intangibles` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cost_in_excess` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `non_hyp_current_deferred_income_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_non_hyp_current_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_non_hyp_current_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventory_valuation_method` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `equity__and__liabilitiesaccounts_payable` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `notes_payable` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `short_hyp_term_debt` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accrued_expenses` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accrued_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `deferred_revenues` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_current_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_current_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `long_hyp_term_debt` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `capital_lease_obligations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `deferred_income_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_non_hyp_current_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `minority_interest_liability` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `preferred_secur_prd__of_subsid_prd__trust` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `preferred_equity_outside_stock_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_non_hyp_current_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `preferred_stock_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `common_stock_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `common_par` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `additional_paid_hyp_in_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `retained_earnings` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `treasury_stock` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_equity_adjustments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_capitalization` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_liabilities__and__stock_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_ask_cash_flow` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `working_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `free_cash_flow` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `invested_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_ask_shares_out__lpn_common_class_only_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `preferred_shares` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_ordinary_shares` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_common_shares_out` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `treasury_shares` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_weighted_shares` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_weighted_shares` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `number_of_employees` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `number_of_part_hyp_time_employees` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_hyp_flow_statementoperating_activitiesnet_income_rsl_loss` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `operating_gains` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `extraordinary_gains` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_lpn_increase_rpn__decrease_in_receivables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_lpn_increase_rpn__decrease_in_inventories` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_lpn_increase_rpn__decrease_in_prepaid_expenses` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_lpn_increase_rpn__decrease_in_other_current_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `decrease__lpn_increase_rpn__in_payables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `decrease__lpn_increase_rpn__in_other_current_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `decrease__lpn_increase_rpn__in_other_working_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_non_hyp_cash_items` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_cash_from_continuing_operations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_cash_from_discontinued_operations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_cash_from_total_operating_activities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `investing_activitiessale_of_property_cmma__plant__and__equipment` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `sale_of_long_hyp_term_investments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `sale_of_short_hyp_term_investments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `purchase_of_property_cmma__plant__and__equipment` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `acquisitions` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `purchase_of_long_hyp_term_investments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `purchase_of__short_hyp_term_investments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_from_discontinued_investing_activities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_cash_from_investing_activities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `financing_activitiesissuance_of_debt` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `issuance_of_capital_stock` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `repayment_of_long_hyp_term_debt` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `repurchase_of_capital_stock` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `payment_of_cash_dividends` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_financing_charges_cmma__net` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_from_discontinued_financing_activities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_cash_from_financing_activities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_cash_floweffect_exchange_rate_changes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_change_in_cash__and__equivalents` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_at_beginning_of_period` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_end_of_period` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_ask_foreign_sales` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `domestic_sales` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `auditor_name` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `auditor_report` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `ratios_calculationsprofit_marginsclose_pe_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `high_pe_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `low_pe_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `gross_profit_margin` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `pre_hyp_tax_profit_margin` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `post_hyp_tax_profit_margin` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_profit_margin` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `interest_coverage__lpn_cont_prd__operations_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `interest_as__pct__of_invested_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `effective_tax_rate` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_per_employee` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_ratiosnormalized_close_pe_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_high_pe_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_low_pe_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_net_profit_margin` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_roe` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_roa` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_roci` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `normalized_income_per_employee` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `solvency_ratiosquick_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `current_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `payout_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_debt_rsl_equity_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `long_hyp_term_debt_rsl_total_capital_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `efficiency_ratiosleverage_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `asset_turnover` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_as__pct__of_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `receivables_as__pct__of_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `sg_and_a_as__pct__of_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `r_and_d_as__pct__of_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `activity_ratiosrevenue_per__dol__cash` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `revenue_per__dol___plant__lpn_net_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `revenue_per__dol__common_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `revenue_per__dol__invested_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `liquidity_ratiosreceivables_turnover` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventory_turnover` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `receivables_per_day_sales` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `sales_per__dol__receivables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `sales_per__dol__inventory` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `revenue_rsl_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `number_of_days_cost_of_goods_in_inventory` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `current_assets_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_assets_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `intangibles_as__pct__of_book_hyp_value` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `inventory_as__pct__of_revenue` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `capital_structure_ratioslong_hyp_term_debt_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `current_liabilities_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `lt_hyp_debt_to_equity_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `lt_hyp_debt_as__pct__of_invested_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `lt_hyp_debt_as__pct__of_total_debt` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_debt_as__pct__total_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `working_captial_as__pct__of_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `revenue_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `book_value_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `tangible_book_value_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `price_rsl_revenue_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `price_rsl_equity_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `price_rsl_tangible_book_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `working_capital_as__pct__of_price` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `profitabilityworking_capital_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_flow_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `free_cash_flow_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `return_on_stock_equity__lpn_roe_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `return_on_capital_invested__lpn_roci_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `return_on_assets__lpn_roa_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `price_rsl_cash_flow_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `price_rsl_free_cash_flow_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `sales_per_employee` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `against_the_industry_ratios_pct__of_sales_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_earnings_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_eps_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_price_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_pe_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_price_rsl_book_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_price_rsl_sales_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_price_rsl_cashflow_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_pric_rsl_free_cashlow_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_debt_rsl_equity_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_current_ratio_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_gross_profit_margin_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_pre_hyp_tax_profit_margin_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_post_hyp_tax_profit_margin_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_net_profit_margin_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_roe_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_pct__of_leverage_hyp_to_hyp_industry` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `htmltitletext` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `exchange_tickertext` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `format_indicator` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `projected_fiscal_year_end_date` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `number_of_months_since_last_reporting_period` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_statementloans` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `investment_securities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `lease_financing_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_interest_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `federal_funds_sold__lpn_purchased_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `interest_bearing_deposits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `loans_held_for_resale` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `trading_account_securities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `time_deposits_placed` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_money_market_investments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_money_market_investments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_interest_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `deposits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `short_hyp_term_deposits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `long_hyp_term_deposits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `federal_funds_purchased__lpn_securities_sold_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `capitalized_lease_obligations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_interest_expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_interest_expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_interest_income__lpn_expense_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `provision_for_loan_loss` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `trust_fees_by_commissions` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `service_charge_on_deposit_accounts` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_service_charges` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `security_transactions` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `premiums_earned` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_realized_capital_gains` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `investment_banking_profit` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_non_hyp_interest_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_non_hyp_interest_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `salaries_and_employee_benefits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_occupancy_expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `promotions_and_advertising` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `property_liability_insurance_claims` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `policy_acquisition_costs` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `amortization_deferred_policy_acquisition_cost` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `current_and_future_benefits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_non_hyp_interest_expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_non_hyp_interest_expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `premium_tax_credit` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_cmma__aquired_in_process_r_and_d` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_cmma_restructuring_and_m_and_a` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `special_income__lpn_charges_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income_from_continuing_operations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income_from_discontinued_operations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_income_from_total_operations` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `extraordinary_income_losses` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `income_from_cumumulative_effect_of_accounting_change` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_gains__lpn_losses_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__lpn_extraordinary_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_eps__lpn_cum_prd__effect_of_acc_prd__change_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps__lpn_cum_prd__effect_of_acc_prd__change_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `dividends_paid_per_share` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_eps_from_total_operations__lpn_ytd_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `balance_sheetassetscash_and_due_from_banks` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `interest_bearing_deposits_at_other_banks` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `investment_securities_cmma__net` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `loans` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `allowance_for_loans_and_lease_losses` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_loans` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `premises__and__equipment` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `due_from_customers_acceptance` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accrued_interest` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `deferred_acquisition_cost` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accrued_investment_income` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `separate_account_business` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `intangible_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_assets` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `equity__and__liabilitiesnon_hyp_interest_bearing_deposits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_liabilities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `bankers_acceptance_outstanding` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accrued_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `accrued_interest_payables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_payables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `claims_and_claim_expense` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `future_policy_benefits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `unearned_premiums` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `policy_holder_funds` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `participating_policy_holder_equity` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `additional_paid_in_capital` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cumulative_translation_adjustment` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `foreign_currency_adjustments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_unrealized_loss__lpn_gain_rpn__on_investments` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_unrealized_loss__lpn_gain_rpn__on_foreign_currency` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_other_unearned_losses__lpn_gains_rpn_` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_ask_shares_outstanding_common_class_only` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_common_shares_outstanding` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `basic_weighted_shares_outstanding` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `diluted_weighted_shares_outstanding` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_flow_statementoperating_activitiesnet_income_earnings` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `depreciation_and_amortization` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `change_in_assets__hyp__receivables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `change_in_liabilities__hyp__payables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `investment_securities_gain` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_policy_acquisition_costs` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `realized_investment_gains` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_premiums_receivables` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `change_in_income_taxes` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_cash_from_operating_activities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `investing_activitiesproceeds_from_sale__hyp__material_investment` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `purchase_of_investment_securities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `net_increase_federal_funds_sold` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `purchase_of_property__and__equipment` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_investing_changes_net` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `financing_activitiesnet_change_in_deposits` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_dividends_paid` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `change_of_short_hyp_term_debt` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `issuance_of_long_hyp_term_debt` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `issuance_of_preferred_stock` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `issuance_of_common_stock` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `purchase_of_treasury_stock` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `other_financing_activities` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `cash_at_end_of_period` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `total_risk_hyp_based_capital_ratio` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `_ask_auditors_name` varchar(64) COLLATE latin1_general_cs DEFAULT NULL,
  `auditors_report` varchar(64) COLLATE latin1_general_cs DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_general_cs
/*!50500 PARTITION BY LIST  COLUMNS(ThisMonth)
(PARTITION X1990_01 VALUES IN ('1990/01') ENGINE = InnoDB,
 PARTITION X1990_02 VALUES IN ('1990/02') ENGINE = InnoDB,
 PARTITION X1990_03 VALUES IN ('1990/03') ENGINE = InnoDB,
 PARTITION X1990_04 VALUES IN ('1990/04') ENGINE = InnoDB,
 PARTITION X1990_05 VALUES IN ('1990/05') ENGINE = InnoDB,
 PARTITION X1990_06 VALUES IN ('1990/06') ENGINE = InnoDB,
 PARTITION X1990_07 VALUES IN ('1990/07') ENGINE = InnoDB,
 PARTITION X1990_08 VALUES IN ('1990/08') ENGINE = InnoDB,
 PARTITION X1990_09 VALUES IN ('1990/09') ENGINE = InnoDB,
 PARTITION X1990_10 VALUES IN ('1990/10') ENGINE = InnoDB,
 PARTITION X1990_11 VALUES IN ('1990/11') ENGINE = InnoDB,
 PARTITION X1990_12 VALUES IN ('1990/12') ENGINE = InnoDB,
 PARTITION X1991_01 VALUES IN ('1991/01') ENGINE = InnoDB,
 PARTITION X1991_02 VALUES IN ('1991/02') ENGINE = InnoDB,
 PARTITION X1991_03 VALUES IN ('1991/03') ENGINE = InnoDB,
 PARTITION X1991_04 VALUES IN ('1991/04') ENGINE = InnoDB,
 PARTITION X1991_05 VALUES IN ('1991/05') ENGINE = InnoDB,
 PARTITION X1991_06 VALUES IN ('1991/06') ENGINE = InnoDB,
 PARTITION X1991_07 VALUES IN ('1991/07') ENGINE = InnoDB,
 PARTITION X1991_08 VALUES IN ('1991/08') ENGINE = InnoDB,
 PARTITION X1991_09 VALUES IN ('1991/09') ENGINE = InnoDB,
 PARTITION X1991_10 VALUES IN ('1991/10') ENGINE = InnoDB,
 PARTITION X1991_11 VALUES IN ('1991/11') ENGINE = InnoDB,
 PARTITION X1991_12 VALUES IN ('1991/12') ENGINE = InnoDB,
 PARTITION X1992_01 VALUES IN ('1992/01') ENGINE = InnoDB,
 PARTITION X1992_02 VALUES IN ('1992/02') ENGINE = InnoDB,
 PARTITION X1992_03 VALUES IN ('1992/03') ENGINE = InnoDB,
 PARTITION X1992_04 VALUES IN ('1992/04') ENGINE = InnoDB,
 PARTITION X1992_05 VALUES IN ('1992/05') ENGINE = InnoDB,
 PARTITION X1992_06 VALUES IN ('1992/06') ENGINE = InnoDB,
 PARTITION X1992_07 VALUES IN ('1992/07') ENGINE = InnoDB,
 PARTITION X1992_08 VALUES IN ('1992/08') ENGINE = InnoDB,
 PARTITION X1992_09 VALUES IN ('1992/09') ENGINE = InnoDB,
 PARTITION X1992_10 VALUES IN ('1992/10') ENGINE = InnoDB,
 PARTITION X1992_11 VALUES IN ('1992/11') ENGINE = InnoDB,
 PARTITION X1992_12 VALUES IN ('1992/12') ENGINE = InnoDB,
 PARTITION X1993_01 VALUES IN ('1993/01') ENGINE = InnoDB,
 PARTITION X1993_02 VALUES IN ('1993/02') ENGINE = InnoDB,
 PARTITION X1993_03 VALUES IN ('1993/03') ENGINE = InnoDB,
 PARTITION X1993_04 VALUES IN ('1993/04') ENGINE = InnoDB,
 PARTITION X1993_05 VALUES IN ('1993/05') ENGINE = InnoDB,
 PARTITION X1993_06 VALUES IN ('1993/06') ENGINE = InnoDB,
 PARTITION X1993_07 VALUES IN ('1993/07') ENGINE = InnoDB,
 PARTITION X1993_08 VALUES IN ('1993/08') ENGINE = InnoDB,
 PARTITION X1993_09 VALUES IN ('1993/09') ENGINE = InnoDB,
 PARTITION X1993_10 VALUES IN ('1993/10') ENGINE = InnoDB,
 PARTITION X1993_11 VALUES IN ('1993/11') ENGINE = InnoDB,
 PARTITION X1993_12 VALUES IN ('1993/12') ENGINE = InnoDB,
 PARTITION X1994_01 VALUES IN ('1994/01') ENGINE = InnoDB,
 PARTITION X1994_02 VALUES IN ('1994/02') ENGINE = InnoDB,
 PARTITION X1994_03 VALUES IN ('1994/03') ENGINE = InnoDB,
 PARTITION X1994_04 VALUES IN ('1994/04') ENGINE = InnoDB,
 PARTITION X1994_05 VALUES IN ('1994/05') ENGINE = InnoDB,
 PARTITION X1994_06 VALUES IN ('1994/06') ENGINE = InnoDB,
 PARTITION X1994_07 VALUES IN ('1994/07') ENGINE = InnoDB,
 PARTITION X1994_08 VALUES IN ('1994/08') ENGINE = InnoDB,
 PARTITION X1994_09 VALUES IN ('1994/09') ENGINE = InnoDB,
 PARTITION X1994_10 VALUES IN ('1994/10') ENGINE = InnoDB,
 PARTITION X1994_11 VALUES IN ('1994/11') ENGINE = InnoDB,
 PARTITION X1994_12 VALUES IN ('1994/12') ENGINE = InnoDB,
 PARTITION X1995_01 VALUES IN ('1995/01') ENGINE = InnoDB,
 PARTITION X1995_02 VALUES IN ('1995/02') ENGINE = InnoDB,
 PARTITION X1995_03 VALUES IN ('1995/03') ENGINE = InnoDB,
 PARTITION X1995_04 VALUES IN ('1995/04') ENGINE = InnoDB,
 PARTITION X1995_05 VALUES IN ('1995/05') ENGINE = InnoDB,
 PARTITION X1995_06 VALUES IN ('1995/06') ENGINE = InnoDB,
 PARTITION X1995_07 VALUES IN ('1995/07') ENGINE = InnoDB,
 PARTITION X1995_08 VALUES IN ('1995/08') ENGINE = InnoDB,
 PARTITION X1995_09 VALUES IN ('1995/09') ENGINE = InnoDB,
 PARTITION X1995_10 VALUES IN ('1995/10') ENGINE = InnoDB,
 PARTITION X1995_11 VALUES IN ('1995/11') ENGINE = InnoDB,
 PARTITION X1995_12 VALUES IN ('1995/12') ENGINE = InnoDB,
 PARTITION X1996_01 VALUES IN ('1996/01') ENGINE = InnoDB,
 PARTITION X1996_02 VALUES IN ('1996/02') ENGINE = InnoDB,
 PARTITION X1996_03 VALUES IN ('1996/03') ENGINE = InnoDB,
 PARTITION X1996_04 VALUES IN ('1996/04') ENGINE = InnoDB,
 PARTITION X1996_05 VALUES IN ('1996/05') ENGINE = InnoDB,
 PARTITION X1996_06 VALUES IN ('1996/06') ENGINE = InnoDB,
 PARTITION X1996_07 VALUES IN ('1996/07') ENGINE = InnoDB,
 PARTITION X1996_08 VALUES IN ('1996/08') ENGINE = InnoDB,
 PARTITION X1996_09 VALUES IN ('1996/09') ENGINE = InnoDB,
 PARTITION X1996_10 VALUES IN ('1996/10') ENGINE = InnoDB,
 PARTITION X1996_11 VALUES IN ('1996/11') ENGINE = InnoDB,
 PARTITION X1996_12 VALUES IN ('1996/12') ENGINE = InnoDB,
 PARTITION X1997_01 VALUES IN ('1997/01') ENGINE = InnoDB,
 PARTITION X1997_02 VALUES IN ('1997/02') ENGINE = InnoDB,
 PARTITION X1997_03 VALUES IN ('1997/03') ENGINE = InnoDB,
 PARTITION X1997_04 VALUES IN ('1997/04') ENGINE = InnoDB,
 PARTITION X1997_05 VALUES IN ('1997/05') ENGINE = InnoDB,
 PARTITION X1997_06 VALUES IN ('1997/06') ENGINE = InnoDB,
 PARTITION X1997_07 VALUES IN ('1997/07') ENGINE = InnoDB,
 PARTITION X1997_08 VALUES IN ('1997/08') ENGINE = InnoDB,
 PARTITION X1997_09 VALUES IN ('1997/09') ENGINE = InnoDB,
 PARTITION X1997_10 VALUES IN ('1997/10') ENGINE = InnoDB,
 PARTITION X1997_11 VALUES IN ('1997/11') ENGINE = InnoDB,
 PARTITION X1997_12 VALUES IN ('1997/12') ENGINE = InnoDB,
 PARTITION X1998_01 VALUES IN ('1998/01') ENGINE = InnoDB,
 PARTITION X1998_02 VALUES IN ('1998/02') ENGINE = InnoDB,
 PARTITION X1998_03 VALUES IN ('1998/03') ENGINE = InnoDB,
 PARTITION X1998_04 VALUES IN ('1998/04') ENGINE = InnoDB,
 PARTITION X1998_05 VALUES IN ('1998/05') ENGINE = InnoDB,
 PARTITION X1998_06 VALUES IN ('1998/06') ENGINE = InnoDB,
 PARTITION X1998_07 VALUES IN ('1998/07') ENGINE = InnoDB,
 PARTITION X1998_08 VALUES IN ('1998/08') ENGINE = InnoDB,
 PARTITION X1998_09 VALUES IN ('1998/09') ENGINE = InnoDB,
 PARTITION X1998_10 VALUES IN ('1998/10') ENGINE = InnoDB,
 PARTITION X1998_11 VALUES IN ('1998/11') ENGINE = InnoDB,
 PARTITION X1998_12 VALUES IN ('1998/12') ENGINE = InnoDB,
 PARTITION X1999_01 VALUES IN ('1999/01') ENGINE = InnoDB,
 PARTITION X1999_02 VALUES IN ('1999/02') ENGINE = InnoDB,
 PARTITION X1999_03 VALUES IN ('1999/03') ENGINE = InnoDB,
 PARTITION X1999_04 VALUES IN ('1999/04') ENGINE = InnoDB,
 PARTITION X1999_05 VALUES IN ('1999/05') ENGINE = InnoDB,
 PARTITION X1999_06 VALUES IN ('1999/06') ENGINE = InnoDB,
 PARTITION X1999_07 VALUES IN ('1999/07') ENGINE = InnoDB,
 PARTITION X1999_08 VALUES IN ('1999/08') ENGINE = InnoDB,
 PARTITION X1999_09 VALUES IN ('1999/09') ENGINE = InnoDB,
 PARTITION X1999_10 VALUES IN ('1999/10') ENGINE = InnoDB,
 PARTITION X1999_11 VALUES IN ('1999/11') ENGINE = InnoDB,
 PARTITION X1999_12 VALUES IN ('1999/12') ENGINE = InnoDB,
 PARTITION X2000_01 VALUES IN ('2000/01') ENGINE = InnoDB,
 PARTITION X2000_02 VALUES IN ('2000/02') ENGINE = InnoDB,
 PARTITION X2000_03 VALUES IN ('2000/03') ENGINE = InnoDB,
 PARTITION X2000_04 VALUES IN ('2000/04') ENGINE = InnoDB,
 PARTITION X2000_05 VALUES IN ('2000/05') ENGINE = InnoDB,
 PARTITION X2000_06 VALUES IN ('2000/06') ENGINE = InnoDB,
 PARTITION X2000_07 VALUES IN ('2000/07') ENGINE = InnoDB,
 PARTITION X2000_08 VALUES IN ('2000/08') ENGINE = InnoDB,
 PARTITION X2000_09 VALUES IN ('2000/09') ENGINE = InnoDB,
 PARTITION X2000_10 VALUES IN ('2000/10') ENGINE = InnoDB,
 PARTITION X2000_11 VALUES IN ('2000/11') ENGINE = InnoDB,
 PARTITION X2000_12 VALUES IN ('2000/12') ENGINE = InnoDB,
 PARTITION X2001_01 VALUES IN ('2001/01') ENGINE = InnoDB,
 PARTITION X2001_02 VALUES IN ('2001/02') ENGINE = InnoDB,
 PARTITION X2001_03 VALUES IN ('2001/03') ENGINE = InnoDB,
 PARTITION X2001_04 VALUES IN ('2001/04') ENGINE = InnoDB,
 PARTITION X2001_05 VALUES IN ('2001/05') ENGINE = InnoDB,
 PARTITION X2001_06 VALUES IN ('2001/06') ENGINE = InnoDB,
 PARTITION X2001_07 VALUES IN ('2001/07') ENGINE = InnoDB,
 PARTITION X2001_08 VALUES IN ('2001/08') ENGINE = InnoDB,
 PARTITION X2001_09 VALUES IN ('2001/09') ENGINE = InnoDB,
 PARTITION X2001_10 VALUES IN ('2001/10') ENGINE = InnoDB,
 PARTITION X2001_11 VALUES IN ('2001/11') ENGINE = InnoDB,
 PARTITION X2001_12 VALUES IN ('2001/12') ENGINE = InnoDB,
 PARTITION X2002_01 VALUES IN ('2002/01') ENGINE = InnoDB,
 PARTITION X2002_02 VALUES IN ('2002/02') ENGINE = InnoDB,
 PARTITION X2002_03 VALUES IN ('2002/03') ENGINE = InnoDB,
 PARTITION X2002_04 VALUES IN ('2002/04') ENGINE = InnoDB,
 PARTITION X2002_05 VALUES IN ('2002/05') ENGINE = InnoDB,
 PARTITION X2002_06 VALUES IN ('2002/06') ENGINE = InnoDB,
 PARTITION X2002_07 VALUES IN ('2002/07') ENGINE = InnoDB,
 PARTITION X2002_08 VALUES IN ('2002/08') ENGINE = InnoDB,
 PARTITION X2002_09 VALUES IN ('2002/09') ENGINE = InnoDB,
 PARTITION X2002_10 VALUES IN ('2002/10') ENGINE = InnoDB,
 PARTITION X2002_11 VALUES IN ('2002/11') ENGINE = InnoDB,
 PARTITION X2002_12 VALUES IN ('2002/12') ENGINE = InnoDB,
 PARTITION X2003_01 VALUES IN ('2003/01') ENGINE = InnoDB,
 PARTITION X2003_02 VALUES IN ('2003/02') ENGINE = InnoDB,
 PARTITION X2003_03 VALUES IN ('2003/03') ENGINE = InnoDB,
 PARTITION X2003_04 VALUES IN ('2003/04') ENGINE = InnoDB,
 PARTITION X2003_05 VALUES IN ('2003/05') ENGINE = InnoDB,
 PARTITION X2003_06 VALUES IN ('2003/06') ENGINE = InnoDB,
 PARTITION X2003_07 VALUES IN ('2003/07') ENGINE = InnoDB,
 PARTITION X2003_08 VALUES IN ('2003/08') ENGINE = InnoDB,
 PARTITION X2003_09 VALUES IN ('2003/09') ENGINE = InnoDB,
 PARTITION X2003_10 VALUES IN ('2003/10') ENGINE = InnoDB,
 PARTITION X2003_11 VALUES IN ('2003/11') ENGINE = InnoDB,
 PARTITION X2003_12 VALUES IN ('2003/12') ENGINE = InnoDB,
 PARTITION X2004_01 VALUES IN ('2004/01') ENGINE = InnoDB,
 PARTITION X2004_02 VALUES IN ('2004/02') ENGINE = InnoDB,
 PARTITION X2004_03 VALUES IN ('2004/03') ENGINE = InnoDB,
 PARTITION X2004_04 VALUES IN ('2004/04') ENGINE = InnoDB,
 PARTITION X2004_05 VALUES IN ('2004/05') ENGINE = InnoDB,
 PARTITION X2004_06 VALUES IN ('2004/06') ENGINE = InnoDB,
 PARTITION X2004_07 VALUES IN ('2004/07') ENGINE = InnoDB,
 PARTITION X2004_08 VALUES IN ('2004/08') ENGINE = InnoDB,
 PARTITION X2004_09 VALUES IN ('2004/09') ENGINE = InnoDB,
 PARTITION X2004_10 VALUES IN ('2004/10') ENGINE = InnoDB,
 PARTITION X2004_11 VALUES IN ('2004/11') ENGINE = InnoDB,
 PARTITION X2004_12 VALUES IN ('2004/12') ENGINE = InnoDB,
 PARTITION X2005_01 VALUES IN ('2005/01') ENGINE = InnoDB,
 PARTITION X2005_02 VALUES IN ('2005/02') ENGINE = InnoDB,
 PARTITION X2005_03 VALUES IN ('2005/03') ENGINE = InnoDB,
 PARTITION X2005_04 VALUES IN ('2005/04') ENGINE = InnoDB,
 PARTITION X2005_05 VALUES IN ('2005/05') ENGINE = InnoDB,
 PARTITION X2005_06 VALUES IN ('2005/06') ENGINE = InnoDB,
 PARTITION X2005_07 VALUES IN ('2005/07') ENGINE = InnoDB,
 PARTITION X2005_08 VALUES IN ('2005/08') ENGINE = InnoDB,
 PARTITION X2005_09 VALUES IN ('2005/09') ENGINE = InnoDB,
 PARTITION X2005_10 VALUES IN ('2005/10') ENGINE = InnoDB,
 PARTITION X2005_11 VALUES IN ('2005/11') ENGINE = InnoDB,
 PARTITION X2005_12 VALUES IN ('2005/12') ENGINE = InnoDB,
 PARTITION X2006_01 VALUES IN ('2006/01') ENGINE = InnoDB,
 PARTITION X2006_02 VALUES IN ('2006/02') ENGINE = InnoDB,
 PARTITION X2006_03 VALUES IN ('2006/03') ENGINE = InnoDB,
 PARTITION X2006_04 VALUES IN ('2006/04') ENGINE = InnoDB,
 PARTITION X2006_05 VALUES IN ('2006/05') ENGINE = InnoDB,
 PARTITION X2006_06 VALUES IN ('2006/06') ENGINE = InnoDB,
 PARTITION X2006_07 VALUES IN ('2006/07') ENGINE = InnoDB,
 PARTITION X2006_08 VALUES IN ('2006/08') ENGINE = InnoDB,
 PARTITION X2006_09 VALUES IN ('2006/09') ENGINE = InnoDB,
 PARTITION X2006_10 VALUES IN ('2006/10') ENGINE = InnoDB,
 PARTITION X2006_11 VALUES IN ('2006/11') ENGINE = InnoDB,
 PARTITION X2006_12 VALUES IN ('2006/12') ENGINE = InnoDB,
 PARTITION X2007_01 VALUES IN ('2007/01') ENGINE = InnoDB,
 PARTITION X2007_02 VALUES IN ('2007/02') ENGINE = InnoDB,
 PARTITION X2007_03 VALUES IN ('2007/03') ENGINE = InnoDB,
 PARTITION X2007_04 VALUES IN ('2007/04') ENGINE = InnoDB,
 PARTITION X2007_05 VALUES IN ('2007/05') ENGINE = InnoDB,
 PARTITION X2007_06 VALUES IN ('2007/06') ENGINE = InnoDB,
 PARTITION X2007_07 VALUES IN ('2007/07') ENGINE = InnoDB,
 PARTITION X2007_08 VALUES IN ('2007/08') ENGINE = InnoDB,
 PARTITION X2007_09 VALUES IN ('2007/09') ENGINE = InnoDB,
 PARTITION X2007_10 VALUES IN ('2007/10') ENGINE = InnoDB,
 PARTITION X2007_11 VALUES IN ('2007/11') ENGINE = InnoDB,
 PARTITION X2007_12 VALUES IN ('2007/12') ENGINE = InnoDB,
 PARTITION X2008_01 VALUES IN ('2008/01') ENGINE = InnoDB,
 PARTITION X2008_02 VALUES IN ('2008/02') ENGINE = InnoDB,
 PARTITION X2008_03 VALUES IN ('2008/03') ENGINE = InnoDB,
 PARTITION X2008_04 VALUES IN ('2008/04') ENGINE = InnoDB,
 PARTITION X2008_05 VALUES IN ('2008/05') ENGINE = InnoDB,
 PARTITION X2008_06 VALUES IN ('2008/06') ENGINE = InnoDB,
 PARTITION X2008_07 VALUES IN ('2008/07') ENGINE = InnoDB,
 PARTITION X2008_08 VALUES IN ('2008/08') ENGINE = InnoDB,
 PARTITION X2008_09 VALUES IN ('2008/09') ENGINE = InnoDB,
 PARTITION X2008_10 VALUES IN ('2008/10') ENGINE = InnoDB,
 PARTITION X2008_11 VALUES IN ('2008/11') ENGINE = InnoDB,
 PARTITION X2008_12 VALUES IN ('2008/12') ENGINE = InnoDB,
 PARTITION X2009_01 VALUES IN ('2009/01') ENGINE = InnoDB,
 PARTITION X2009_02 VALUES IN ('2009/02') ENGINE = InnoDB,
 PARTITION X2009_03 VALUES IN ('2009/03') ENGINE = InnoDB,
 PARTITION X2009_04 VALUES IN ('2009/04') ENGINE = InnoDB,
 PARTITION X2009_05 VALUES IN ('2009/05') ENGINE = InnoDB,
 PARTITION X2009_06 VALUES IN ('2009/06') ENGINE = InnoDB,
 PARTITION X2009_07 VALUES IN ('2009/07') ENGINE = InnoDB,
 PARTITION X2009_08 VALUES IN ('2009/08') ENGINE = InnoDB,
 PARTITION X2009_09 VALUES IN ('2009/09') ENGINE = InnoDB,
 PARTITION X2009_10 VALUES IN ('2009/10') ENGINE = InnoDB,
 PARTITION X2009_11 VALUES IN ('2009/11') ENGINE = InnoDB,
 PARTITION X2009_12 VALUES IN ('2009/12') ENGINE = InnoDB,
 PARTITION X2010_01 VALUES IN ('2010/01') ENGINE = InnoDB,
 PARTITION X2010_02 VALUES IN ('2010/02') ENGINE = InnoDB,
 PARTITION X2010_03 VALUES IN ('2010/03') ENGINE = InnoDB,
 PARTITION X2010_04 VALUES IN ('2010/04') ENGINE = InnoDB,
 PARTITION X2010_05 VALUES IN ('2010/05') ENGINE = InnoDB,
 PARTITION X2010_06 VALUES IN ('2010/06') ENGINE = InnoDB,
 PARTITION X2010_07 VALUES IN ('2010/07') ENGINE = InnoDB,
 PARTITION X2010_08 VALUES IN ('2010/08') ENGINE = InnoDB,
 PARTITION X2010_09 VALUES IN ('2010/09') ENGINE = InnoDB,
 PARTITION X2010_10 VALUES IN ('2010/10') ENGINE = InnoDB,
 PARTITION X2010_11 VALUES IN ('2010/11') ENGINE = InnoDB,
 PARTITION X2010_12 VALUES IN ('2010/12') ENGINE = InnoDB,
 PARTITION X2011_01 VALUES IN ('2011/01') ENGINE = InnoDB,
 PARTITION X2011_02 VALUES IN ('2011/02') ENGINE = InnoDB,
 PARTITION X2011_03 VALUES IN ('2011/03') ENGINE = InnoDB,
 PARTITION X2011_04 VALUES IN ('2011/04') ENGINE = InnoDB,
 PARTITION X2011_05 VALUES IN ('2011/05') ENGINE = InnoDB,
 PARTITION X2011_06 VALUES IN ('2011/06') ENGINE = InnoDB,
 PARTITION X2011_07 VALUES IN ('2011/07') ENGINE = InnoDB,
 PARTITION X2011_08 VALUES IN ('2011/08') ENGINE = InnoDB,
 PARTITION X2011_09 VALUES IN ('2011/09') ENGINE = InnoDB,
 PARTITION X2011_10 VALUES IN ('2011/10') ENGINE = InnoDB,
 PARTITION X2011_11 VALUES IN ('2011/11') ENGINE = InnoDB,
 PARTITION X2011_12 VALUES IN ('2011/12') ENGINE = InnoDB,
 PARTITION X2012_01 VALUES IN ('2012/01') ENGINE = InnoDB,
 PARTITION X2012_02 VALUES IN ('2012/02') ENGINE = InnoDB,
 PARTITION X2012_03 VALUES IN ('2012/03') ENGINE = InnoDB,
 PARTITION X2012_04 VALUES IN ('2012/04') ENGINE = InnoDB,
 PARTITION X2012_05 VALUES IN ('2012/05') ENGINE = InnoDB,
 PARTITION X2012_06 VALUES IN ('2012/06') ENGINE = InnoDB,
 PARTITION X2012_07 VALUES IN ('2012/07') ENGINE = InnoDB,
 PARTITION X2012_08 VALUES IN ('2012/08') ENGINE = InnoDB,
 PARTITION X2012_09 VALUES IN ('2012/09') ENGINE = InnoDB,
 PARTITION X2012_10 VALUES IN ('2012/10') ENGINE = InnoDB,
 PARTITION X2012_11 VALUES IN ('2012/11') ENGINE = InnoDB,
 PARTITION X2012_12 VALUES IN ('2012/12') ENGINE = InnoDB,
 PARTITION X2013_01 VALUES IN ('2013/01') ENGINE = InnoDB,
 PARTITION X2013_02 VALUES IN ('2013/02') ENGINE = InnoDB,
 PARTITION X2013_03 VALUES IN ('2013/03') ENGINE = InnoDB,
 PARTITION X2013_04 VALUES IN ('2013/04') ENGINE = InnoDB,
 PARTITION X2013_05 VALUES IN ('2013/05') ENGINE = InnoDB,
 PARTITION X2013_06 VALUES IN ('2013/06') ENGINE = InnoDB,
 PARTITION X2013_07 VALUES IN ('2013/07') ENGINE = InnoDB,
 PARTITION X2013_08 VALUES IN ('2013/08') ENGINE = InnoDB,
 PARTITION X2013_09 VALUES IN ('2013/09') ENGINE = InnoDB,
 PARTITION X2013_10 VALUES IN ('2013/10') ENGINE = InnoDB,
 PARTITION X2013_11 VALUES IN ('2013/11') ENGINE = InnoDB,
 PARTITION X2013_12 VALUES IN ('2013/12') ENGINE = InnoDB) */;
 
 -- create insert one record per EXCHANGE_TICKER and ThisMonth
 -- into the partitioned table ( created above: advfn.firmshistory_thismonth_partition )
 -- -- partitioned by ThisMonth
 
INSERT INTO advfn.firmshistory_thismonth_partition ( MARKETCAP, SECTOR, INDUSTRY, EXCHANGE_TICKER, ThisMonth)
SELECT et.MARKETCAP, et.SECTOR, et.INDUSTRY, et.EXCHANGE_TICKER, REPLACE(p.PARTITION_DESCRIPTION,"'","") As ThisMonth
  FROM firmshistory_thismonth et, INFORMATION_SCHEMA.PARTITIONS p  
    WHERE
      p.table_schema = 'advfn' AND 
      p.table_name = 'firmshistory_thismonth_partition' AND
      p.partition_name LIKE '%' 
ORDER BY et.SECTOR, et.INDUSTRY, et.EXCHANGE_TICKER, p.PARTITION_ORDINAL_POSITION;
 
-- 11 minutes passed

-- prepare to 'update with market ( Yahoo ) data'
 
ALTER TABLE advfn.firmshistory_thismonth_partition 
  ADD ThisMonthLastDate varchar(64) COLLATE latin1_general_cs DEFAULT NULL;

-- wait 5 minutes

ALTER TABLE advfn.firmshistory_thismonth_partition 
  ThisMonthLastClose double DEFAULT NULL;

-- wait 15 minutes
  
ALTER TABLE advfn.firmshistory_thismonth_partition 
  ThisMonthLastAdjustedClose double DEFAULT NULL;

-- wait 15 minutes

############### END EXECUTABLE AREA ###################
 
############ BEGIN HERE ###############

## UPDATE firmshistory_thismonth_partition with end_of_month ticker Price Yahoo quote data

--- prepare for the update

ALTER TABLE advfn.firmshistory_quote_partition_thismonth
  ADD INDEX fh_quote_partition_thismonth_thismonth_exchange_ticker_idx (ThisMonth,EXCHANGE_TICKER);

-- about 90 seconds
  
--- prepare for the update
ALTER TABLE advfn.firmshistory_thismonth_partition
  ADD INDEX fh_thismonth_partition_thismonth_exchange_ticker_idx (ThisMonth,EXCHANGE_TICKER);

-- about 120 seconds

-- update firmshistory_thismonth_partition
--   with the information of 
--     ThisMonthLastDate
--     ThisMonthLastClose
--     ThisMonthLastAdjustedClose
--       from irmshistory_quote_partition_thismonth

-- start transaction or NOT ( still will stay ACID )
-- and STILL only one transaction ( UPDATE )

 START TRANSACTION;

 UPDATE firmshistory_thismonth_partition fhm 
  JOIN firmshistory_quote_partition_thismonth fhqm ON 
    fhm.ThisMonth = fhqm.ThisMonth AND
    fhm.EXCHANGE_TICKER = fhqm.EXCHANGE_TICKER
      SET fhm.ThisMonthLastDate = fhqm.ThisMonthLastDate
        , fhm.ThisMonthLastClose = fhqm.ThisMonthLastClose
        , fhm.ThisMonthLastAdjustedClose = fhqm.ThisMonthLastAdjustedClose;
        
-- 2:28:22 MEM at 3.14G CPU bounce between 15% and 30%
-- about 3 minutes before disk i/o started
-- 2:32 MEM at 3.14G CPU bounce between 40% and 60%
-- IF SELECTS PASS, TO NOT FORGET TO COMMIT
-- 12 minutes
-- Query OK, 789433 rows affected (11 min 36.64 sec)
-- Rows matched: 789433  Changed: 789433  Warnings: 0
        
 SELECT COUNT(*) FROM 
 firmshistory_thismonth_partition fhm 
  JOIN firmshistory_quote_partition_thismonth fhqm ON 
    fhm.ThisMonth = fhqm.ThisMonth AND
    fhm.EXCHANGE_TICKER = fhqm.EXCHANGE_TICKER;
    
-- +----------+
-- | COUNT(*) |
-- +----------+
-- |   789433 |
-- +----------+
-- 1 row in set (48.59 sec)
    
--  do not select everything
    
 SELECT fhm.ThisMonth, fhm.EXCHANGE_TICKER 
 , fhm.ThisMonthLastDate
 , fhm.ThisMonthLastClose
 , fhm.ThisMonthLastAdjustedClose
  FROM 
 firmshistory_thismonth_partition fhm 
  JOIN firmshistory_quote_partition_thismonth fhqm ON 
    fhm.ThisMonth = fhqm.ThisMonth AND
    fhm.EXCHANGE_TICKER IN ('NYSE_WMT','NASDAQ_MSFT') AND
    fhm.EXCHANGE_TICKER = fhqm.EXCHANGE_TICKER;
    
-- suprising long ( but seems correct ) ( Rem: has to query every DATE partition )
-- 576 rows in set (1 min 45.14 sec)
  
  COMMIT;
  --DONE;
  -- ROLLBACK;

-- rerun query
-- 576 rows in set (1 min 41.86 sec)
-- again ( Rem: has to query every DATE partition )
  
########## END HERE ##########


########## BEGIN HERE ##########

# dividend updating supporting indexes needed

--- prepare for the update

ALTER TABLE advfn.firmshistory_thismonth_partition
 ADD SumThisMonthDividend double DEFAULT NULL;

-- Query OK, 0 rows affected (6 min 46.98 sec)

ALTER TABLE firmshistory_dividend_partition_thismonth
  ADD INDEX fh_dividend_partition_thismonth_idx (ThisMonth,EXCHANGE_TICKER);

-- Query OK, 0 rows affected (1 min 4.03 sec)

########## END HERE ##########



####################### BEGIN PRODUCTION ######################## 

# update firmshistory_thismonth_partition.SumThisMonthDividend
# with cummulative monthly data 
#   from firmshistory_dividend_partition_thismonth SUM( .ThisMonthDividend) 

START TRANSACTION;

-- testing
SELECT fhm.ThisMonth, fhm.EXCHANGE_TICKER, fhm.SumThisMonthDividend
      FROM firmshistory_thismonth_partition fhm
        WHERE fhm.ThisMonth IN ('2012/04','2012/05','2012/06','2012/07','2012/08','2012/09'
                               ,'2012/10','2012/11','2012/12','2013/01','2013/02','2013/03') AND
              fhm.EXCHANGE_TICKER IN ('NYSE_WMT','NASDAQ_MSFT');

-- 24 rows in set (0.73 sec)
              
-- testing
-- SELECT COUNT(*) 
-- SELECT fhm.ThisMonth, fhm.EXCHANGE_TICKER, fhm.SumThisMonthDividend
--       FROM firmshistory_thismonth_partition fhm
--         WHERE fhm.SumThisMonthDividend IS NOT NULL;

# update firmshistory_thismonth_partition.SumThisMonthDividend
# with cummulative monthly data 
#   from firmshistory_dividend_partition_thismonth SUM( .ThisMonthDividend) 
        
UPDATE firmshistory_thismonth_partition fhm 
  JOIN firmshistory_dividend_partition_thismonth fhdm ON 
    fhm.ThisMonth = fhdm.ThisMonth AND
    fhm.EXCHANGE_TICKER = fhdm.EXCHANGE_TICKER 
      SET fhm.SumThisMonthDividend = 
        (SELECT SUM(fhdm2.ThisMonthDividend) 
          FROM firmshistory_dividend_partition_thismonth fhdm2
          WHERE fhm.ThisMonth = fhdm2.ThisMonth AND
                fhm.EXCHANGE_TICKER = fhdm2.EXCHANGE_TICKER 
        GROUP BY fhdm2.ThisMonth, fhdm2.EXCHANGE_TICKER
        );
        
-- began 12:53:10 p.m. -- 90 seconds about 2 firms and 12 months each
-- high oscillating CPU 17% to 80%
-- MEM ( PageFile ) holiding steady at 2.85
-- est time to completion
-- ( 4700 / 2 ) * ( 288 / 12 ) / ( 60 * 60 ) = 
-- SELECT ( 4700 / 2 ) * ( 288 / 12 ) / ( 60 * 60 );
-- Query OK, 116613 rows affected (5 min 10.36 sec)
-- Rows matched: 116613  Changed: 116613  Warnings: 0

-- testing
SELECT fhm.ThisMonth, fhm.EXCHANGE_TICKER, fhm.SumThisMonthDividend
      FROM firmshistory_thismonth_partition fhm
        WHERE fhm.ThisMonth IN ('2012/04','2012/05','2012/06','2012/07','2012/08','2012/09'
                               ,'2012/10','2012/11','2012/12','2013/01','2013/02','2013/03') AND
              fhm.EXCHANGE_TICKER IN ('NYSE_WMT','NASDAQ_MSFT');

-- 24 rows in set (0.58 sec)
-- 
              
-- testing
-- SELECT COUNT(*) 
-- SELECT fhm.ThisMonth, fhm.EXCHANGE_TICKER, fhm.SumThisMonthDividend
--       FROM firmshistory_thismonth_partition fhm
--         WHERE fhm.SumThisMonthDividend IS NOT NULL;


-- testing
-- SELECT fhm.ThisMonth, fhm.EXCHANGE_TICKER, fhm.SumThisMonthDividend
--       FROM firmshistory_thismonth_partition fhm
--         WHERE 
--               fhm.EXCHANGE_TICKER IN ('NYSE_WMT','NASDAQ_MSFT');

-- 576 rows in set (32.27 sec)

-- SELECT fhdm2.ThisMonth, fhdm2.EXCHANGE_TICKER, SUM(fhdm2.ThisMonthDividend) 
--           FROM firmshistory_dividend_partition_thismonth fhdm2
--           WHERE fhdm2.EXCHANGE_TICKER IN ('NYSE_WMT','NASDAQ_MSFT') AND
--                 fhdm2.ThisMonth = fhdm2.ThisMonth AND
--                 fhdm2.EXCHANGE_TICKER = fhdm2.EXCHANGE_TICKER 
--         GROUP BY fhdm2.ThisMonth, fhdm2.EXCHANGE_TICKER;

-- 136 rows in set (30.27 sec)

-- ROLLBACK;
COMMIT;

####################### END PRODUCTION ########################


 

##################### BEGIN #########################

# to update firmshistory_thismonth_partition 
# with attributes from firmshistory_partition_rownombres 
# need a matching indexes

ALTER TABLE firmshistory_partition_rownombres
  ADD INDEX fh_partition_rownombres_rownombres_exchange_ticker_idx (rownombres,EXCHANGE_TICKER);
-- very time consuming
-- started 4:03 p.m.
-- finished at 4:12 p.m.
-- Query OK, 0 rows affected (9 min 22.98 sec)

ALTER TABLE firmshistory_thismonth_partition
  ADD INDEX fh_thismonth_partition_exchange_ticker_idx (EXCHANGE_TICKER);
-- started at 4:18 p.m.
-- Query OK, 0 rows affected (2 min 45.69 sec)
  
##################### END ###########################




### predicted
### expected run time : 12 * 24 * 2.31 sec / 60 sec / min ... 11 MINUTES
###   11 * 8 attributes = 88 minutes = 1 hour and 28 minutes

### actual
## started at 3:23:40 ( ended at 4:04:15 : 41 minutes )

############# BEGIN EXECUTION #########

# put into firmshistory_thismonth_partition
#   the rownombres attributes found in firmshistory_partition_rownombres

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`annonproc`;

DELIMITER $$
CREATE PROCEDURE advfn.annonproc()
  MODIFIES SQL DATA
BEGIN
  DECLARE l_last_row_fetched         INT;
  DECLARE c_l_partition_name         VARCHAR(64);
  DECLARE c_l_partition_description  LONGTEXT;
  DECLARE d_l_partition_name         VARCHAR(64);
  DECLARE d_l_partition_description  LONGTEXT;

  DECLARE c_cursor cursor FOR
    SELECT p.PARTITION_NAME, REPLACE(PARTITION_DESCRIPTION,"´","'") PARTITION_DESCRIPTION 
      FROM INFORMATION_SCHEMA.PARTITIONS p  
        WHERE
          p.table_schema = 'advfn' AND 
          p.table_name = 'firmshistory_partition_rownombres' AND
          p.partition_name IN (
             'quarter_end_date'
            ,'total_common_shares_out'
            ,'total_net_income'
            ,'total_equity'
            ,'total_revenue'
            ,'earnings_period_indicator'
            ,'quarterly_indicator'
            ,'number_of_months_last_report_period'
            ) ORDER BY p.partition_name;
            -- debugging: 
            -- 'total_common_shares_out'
            -- ,'total_net_income'
            -- ,'total_equity'
  
  
  DECLARE d_cursor cursor FOR
    SELECT p.PARTITION_NAME, REPLACE(PARTITION_DESCRIPTION,"´","'") PARTITION_DESCRIPTION
      FROM INFORMATION_SCHEMA.PARTITIONS p  
        WHERE
          p.table_schema = 'advfn' AND 
          p.table_name = 'firmshistory_thismonth_partition' AND
          p.partition_name LIKE 'X%'
    ORDER BY p.PARTITION_NAME DESC;
      -- debugging: p.partition_name IN ('X1990_01','X2013_12','X2006_05')
  
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET l_last_row_fetched=1;
  SET l_last_row_fetched=0;
  
  OPEN c_cursor;
  
    c_cursor_loop:LOOP
      FETCH c_cursor INTO c_l_partition_name, c_l_partition_description;
    
      IF l_last_row_fetched=1 THEN
        LEAVE c_cursor_loop;
      END IF;

      OPEN d_cursor;
        d_cursor_loop:LOOP
          FETCH d_cursor INTO d_l_partition_name, d_l_partition_description;
          
          IF l_last_row_fetched=1 THEN
            LEAVE d_cursor_loop;
          END IF;
          
          -- WORK      
          SET @l_source = CONCAT(
          
            "UPDATE firmshistory_thismonth_partition fhm 
              JOIN firmshistory_partition_rownombres fhr ON 
                fhr.rownombres IN (",c_l_partition_description,") AND 
                fhm.ThisMonth IN (",d_l_partition_description,") AND 
                fhr.EXCHANGE_TICKER = fhm.EXCHANGE_TICKER 
                  SET fhm.",c_l_partition_name," = fhr.",d_l_partition_name," "
          );
          
          SELECT @l_source;
          
          PREPARE stmt1 FROM @l_source;
          EXECUTE stmt1;
          DEALLOCATE PREPARE stmt1;
          
        END LOOP d_cursor_loop;
      CLOSE d_cursor;  
      SET l_last_row_fetched=0;
      
    END LOOP c_cursor_loop;

  CLOSE c_cursor;
  SET l_last_row_fetched=0;
  
END$$

DELIMITER ;
CALL annonproc();

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`annonproc`;


############# END EXECUTION #########

