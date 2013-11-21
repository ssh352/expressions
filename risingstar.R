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







