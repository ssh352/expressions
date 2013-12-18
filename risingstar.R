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
Aug 16 '11 

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

