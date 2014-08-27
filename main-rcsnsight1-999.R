
# RECENT HELP FROM
# N:\FUND_DATA_FIXES_2\scratchFaberRecessionINDQuantstrat.txt


# install.packages("PerformanceAnalytics")

# install.packages("iterators")
# install.packages("foreach")

# install.packages("zoo")
# install.packages("xts")
# install.packages("quantmod")

# install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org", dep = TRUE)
# install.packages("blotter", repos="http://R-Forge.R-project.org", dep = TRUE)

# install.packages("xtsExtra", repos="http://R-Forge.R-project.org", dep = TRUE)

# install.packages("PortfolioAnalytics", repos="http://R-Forge.R-project.org", dep = TRUE)
# install.packages("rgl", repos="http://R-Forge.R-project.org", dep = TRUE)
# install.packages("quantstrat", repos="http://R-Forge.R-project.org", dep = TRUE)

# install.packages("pander")
# install.packages("shiny")
# install.packages("qmao",repos = "http://R-Forge.R-project.org")


options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width
options(sqldf.driver = "SQLite")

library(Hmisc)

library(testthat)

library(tcltk)
library(Rgraphviz)

library(sqldf)

library(lubridate)

library(plyr)
library(dplyr)
library(data.table)

# library(foreign)
options("getSymbols.warning4.0"=FALSE)
library(quantstrat)
# library(qmao)  # alignSymbols  # Deletes rows that not all Symbols have in common

library(qmao)
library(xtsExtra)

library(randomForest)

setwd("N:\\MyVMWareSharedFolder\\rcsnsight1\\R")

# LATER
# source( paste0(getwd(),"/helpers-rcsnsight1-999.R"))

# IN YELLOW window2 only
# library(testthat)
# auto_test("./R", "./tests/testthat") 


main_rcsnsight1_999 <- function(pauseat=NULL) {

  # FROM MY EMAIL AND 'WORK OUTLOOK'
    
  # --------- VERY IMPORTANT ( EVERY 2 WEEKS ) -----------

   
  # FRB: Press Release - Monetary Policy

  # http://www.federalreserve.gov/feeds/press_monetary.xml

  # FRB: Chair Yellen: Speeches and Testimony

  # http://www.federalreserve.gov/feeds/s_t_yellen.xml

  # --

  # Chauvet/Piger

  # http://research.stlouisfed.org/fred2/data/RECPROUSM156N.txt

  # Smoothed U.S. Recession Probabilities

  # Monthly, Not Seasonally Adjusted, RECPROUSM156N, Updated: 2014-06-02 10:01 AM CDT

  # Source: Marcelle Chauvet and Jeremy Piger

  # http://research.stlouisfed.org/fred2/series/RECPROUSM156N

  # --

  # NBER

  # http://research.stlouisfed.org/fred2/data/USRECM.txt

  # (Midpoint ?)

  # NBER based Recession Indicators for the United States from the Peak through the Trough ( 2ND MOST POP )

  # Monthly, Not Seasonally Adjusted, USRECM

  # http://research.stlouisfed.org/fred2/series/USRECM

   
  # http://research.stlouisfed.org/fred2/data/USREC.txt

  # NBER based Recession Indicators for the United States from the Period following the Peak through the Trough ( MOST POP )

  # Monthly, Not Seasonally Adjusted, USREC

  # http://research.stlouisfed.org/fred2/series/USREC


  # http://research.stlouisfed.org/fred2/data/USRECP.txt

  # NBER based Recession Indicators for the United States from the Peak through the Period preceding the Trough

  # Monthly, Not Seasonally Adjusted, USRECP

  # http://research.stlouisfed.org/fred2/series/USRECP

  # ---

  # Sornette

  # FINANCIAL CRISIS OBSERVATOR - Market Bubble Watch Overview

  # Choose

  # Indicators -> Early Bubble Warning Short Time Scale

  # Settings -> 3 Months

  # http://risikopedia.ethz.ch:2375/

  # ---

  # OECD

  # http://research.stlouisfed.org/fred2/data/USARECM.txt

  # OECD based Recession Indicators for the United States from the Peak through the Trough

  # Monthly, Not Seasonally Adjusted, USARECM

  # http://research.stlouisfed.org/fred2/series/USARECM

   
  # http://research.stlouisfed.org/fred2/data/USAREC.txt

  # OECD based Recession Indicators for the United States from the Period following the Peak through the Trough

  # Monthly, Not Seasonally Adjusted, USAREC

  # http://research.stlouisfed.org/fred2/series/USAREC


  # http://research.stlouisfed.org/fred2/data/USARECP.txt

  # OECD based Recession Indicators for the United States from the Peak through the Period preceding the Trough

  # Monthly, Not Seasonally Adjusted, USARECP

  # http://research.stlouisfed.org/fred2/series/USARECP
    
  # IF NOT ALREAY THERE

  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  # MINE DIFFERENT ( HARD CODED : earliest of USRECM )
  initDate='1950-03-01'

  # SURE 100 thousand
  # initEq=100000

  #  HIGHLY DEEPLY CONSIDER ( FOR MY TESTS 'THE BAD 2008-09' RECESSION )
  #  use test dates
  #  initDate="2007-01-01" 
  #  endDate ="2010-12-31" 

  
  # S & P 500 ( 0 month lag )
  
  if( file.exists(paste0(getwd(),"/GSPC.Rdata"))){
    load(file = paste0(getwd(),"/GSPC.Rdata"))
  } else {
    # by luck always the last calendar day of the month
    suppressWarnings(suppressMessages(getSymbols("^GSPC",from=initDate,src='yahoo', index.class=c("POSIXt","POSIXct"))))
    symbols <- c("GSPC")
    # register to be like a stock
    currency("USD")
    stock(symbols[1], currency="USD",multiplier=1)  # FinancialInstrument ( NOT REALLY A STOCK )
    symbol <- symbols
    x <- get(symbol)
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE) # faber.R
    indexFormat(x) <- '%Y-%m-%d'
    colnames(x)<-gsub("x",symbol,colnames(x))
    assign(symbol,x)

    save("GSPC",file = paste0(getwd(),"/GSPC.Rdata"))
  }

  # NBER ( 2 month lag )
  
  if( file.exists(paste0(getwd(),"/USRECM.Rdata"))){
    load(file = paste0(getwd(),"/USRECM.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("USRECM" , src = "FRED", index.class=c("POSIXt","POSIXct") )))
    USRECM <- USRECM[paste0(substr(initDate,1,7),'::')]
    symbols <- c("USRECM")
    symbol <- symbols
    x <- get(symbol)
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,ncol(x)] # remove quantmod o/h/l
    colnames(x) <- tolower("USRECM") # usrecm
    assign(symbol,x)

    save("USRECM",file = paste0(getwd(),"/USRECM.Rdata"))
  }

  # Chauvet/Piger ( 3 month lag )
 
   if( file.exists(paste0(getwd(),"/RECPROUSM156N.Rdata"))){
    load(file = paste0(getwd(),"/RECPROUSM156N.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("RECPROUSM156N" , src = "FRED", index.class=c("POSIXt","POSIXct") )))
    RECPROUSM156N <- RECPROUSM156N[paste0(substr(initDate,1,7),'::')]
    symbols <- c("RECPROUSM156N")
    symbol <- symbols
    x <- get(symbol)
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,ncol(x)] # remove quantmod o/h/l
    colnames(x) <- tolower("RECPROUSM156N") # usrecm
    assign(symbol,x)

    save("RECPROUSM156N",file = paste0(getwd(),"/RECPROUSM156N.Rdata"))
  }

  # LEFT_OFF ABOVE need a logit() to convert 'recession probabilites in to better numbers
  # LEFT OFF
  # RECENT HELP FROM
  # N:\FUND_DATA_FIXES_2\scratchFaberRecessionINDQuantstrat.txt
  
  # OECD ( 2 month lag )

   if( file.exists(paste0(getwd(),"/USARECM.Rdata"))){
    load(file = paste0(getwd(),"/USARECM.Rdata"))
  } else {
    suppressWarnings(suppressMessages(getSymbols("USARECM" , src = "FRED", index.class=c("POSIXt","POSIXct") )))
    USARECM <- USARECM[paste0(substr(initDate,1,7),'::')]
    symbols <- c("USARECM")
    symbol <- symbols
    x <- get(symbol)
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)  # NOT USED: indexAt='yearmon'  # quantmod
    indexFormat(x)<-'%Y-%m-%d'
    x <- x[,ncol(x)] # remove quantmod o/h/l
    colnames(x) <- tolower("USARECM") # usrecm
    assign(symbol,x)

    save("USARECM",file = paste0(getwd(),"/USARECM.Rdata"))
  }
  
  
  
  the_end <- 1
  
}

# toggleable
# main_rcsnsight1_999()
# View
# 
