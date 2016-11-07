

anon <- function() {

# O'Shaunessy 2010 book:
# Large Cap bid/ask spreads 0.15 ( more liquid )
# Small Cap bid/ask spreads 0.50

# IN STOCK OPTIONS FIND 'FORWARD CONVERSION' ARBITRAGE CASES ( IF ANY ) 
# COVERED CALLS +
#  1. I BUY THE STOCK(NOW IT IS MINE)
#  2. I SELL ( INSTANTLY) A CALL(MINE) AT BID(OPTION BUYERS FAVOR)
#  3. I BUY  ( INSTANTLY) A PUT(SOMEONEELSE)  AT ASK(OPTIONS SELLERS FAVOR)

# IN STOCK OPTIONS FIND 'REVERSE CONVERSION' ARBITRAGE CASES ( IF ANY ) 
# CASH-SECURED PUTS +
#  1. I I BORROW THE STOCK (NOW IT BELONGS TO A 2ND PARTY)
#  2. I SELL ( INSTANTLY) A PUT(MINE) AT BID(OPTION BUYERS FAVOR)
#  3. I BUY  ( INSTANTLY) A CALL(SOMEONEELSE)  AT ASK(OPTIONS SELLERS FAVOR)

# NOTE THIS CURRENT_IMPLEMENTATION ONLY PERFECTLY MATCHES CALL_STRIKE = BID_STRIKE 
 
options(width = 255)
options("getSymbols.warning4.0"=FALSE)

library(quantmod)
 
currentBulkSYMs <- TTR::stockSymbols()

library(tcltk)
library(Rgraphviz)
library(sqldf)

currentBulkOptionsSYMs <- sqldf(
  "SELECT Exchange, Symbol,Name,LastSale,MarketCap
     FROM currentBulkSYMs 
       WHERE MarketCap > 600000000 AND
             Symbol NOT LIKE '%-%' 
   ORDER BY 
     MarketCap DESC LIMIT 6"
, drv = "SQLite")

# # CURRENTLY - NO LIMIT

# LIMIT 4
#   MarketCap DESC LIMIT 4

# if NO LIMIT
# nrow(currentBulkOptionsSYMs)
# [1] 2803

# NOTE: AFTER TOP 500 (MAYBE 100) ... NOT WORTH QUERYING ANYMORE
#  EVENTUALLY NEED 
#   'COVERED CALL'     - WAIT TO BUY PUT(BID)
#   'CASH SECURED PUT' - WAIT TO SELL CALL(BID)

# choose the yahoo strike year and strike month
currentExpireYear  <- "2014"
currentExpireMonth <- "05"  # FROM EARLIER SESSION"04"

library(timeDate)
library(lubridate) 

# find the ( American options in America )
#   yahoo strike day ( the third friday of the month )
# HARD NOTE: This 'day' is expected to change 'sometime' in 2015
# this month   months(0) ) ( This month 3rd Friday )
# next month ( months(1) ) ( Next month 3rd Friday )
currentChainExpireDate   <- 
as.character(
  timeNthNdayInMonth(
  as.character(timeFirstDayInMonth(
    as.character(
      as.Date(paste0(currentExpireYear,"-",currentExpireMonth,"-01"), tz = "EST") 
        + months(0) # 0 - this calander month, 1 - next calendar month
      )
    ,  zone="EST"
    ) )
    ,  5 , 3, zone="EST"
  )
)

# note: the date returned by Yahoo is Saturday ( not Friday )
currentChainExpireDatePlusOne   <- 
as.character(
  timeNthNdayInMonth(
  as.character(timeFirstDayInMonth(
    as.character(
      as.Date(paste0(currentExpireYear,"-",currentExpireMonth,"-01"), tz = "EST") 
        + months(0) # 0 - this calander month, 1 - next calendar month
      )
    ,  zone="EST"
    ) )
    ,  5 , 3, zone="EST"
  ) + days(1)
)

# need 'form' from "2014-04-18 05:00:00" to  2014-04-18
currentYahooCallChainExpireDate <- substr(currentChainExpireDate,1,10)
# need 'form' from "2014-04-19 05:00:00" to   "140419"
currentSQLChainExpireDate       <- substr(gsub("-", "",currentChainExpireDatePlusOne),3,8)


# This is a free Real-Time quote. 
# https://help.yahoo.com/kb/finance/SLN2321.html?impressions=true

# Each Real-Time quote is from BATS Exchange and 
# does not necessarily represent the top bids or asks in the marketplace.
# http://batstrading.com/


# NOTE: BELOW 'MarketCap' researved for a convenience of future use
#   when I need to know my order size e.g MarketCap/Last ... NO WRONG
#                                         'avail cash'/Last = # shares ... / 100 = # roundlots ... CORRECT
# MAYBE MarketCap compare v.s. OI and/or Vol ( TO GET A 'BETTER' IDEA of 'ability to buy/sell fast' )
# FUTURE 'dump MarketCap' ?

# loop initialization begins
# loop initialization begins
# loop initialization begins

# SEE results OF getwd()
# YELLOW
# "F:/Documents and Settings/Administrator"
# send rolling file to the output
# easier to see outupt

# _forw_conv
filename_forw_conv <- "OptionsArbitrage_forw_conv_Chain_symbol_rate_of_ret_pct_best_20.out.txt"
capture.output(print(""), file = filename_forw_conv)  # overwrite the file ( initialize )
# on-the-fly aggregates 
temp_forw_conv_Chain_symbol_rate_of_ret_pct_best_20 <- data.frame()
forw_conv_Chain_symbol_rate_of_ret_pct_best_20  <- data.frame()

# _revs_conv
filename_revs_conv <- "OptionsArbitrage_revs_conv_Chain_symbol_rate_of_ret_pct_best_20.out.txt"
capture.output(print(""), file = filename_revs_conv)  # overwrite the file ( initialize )
# on-the-fly aggregates 
temp_revs_conv_Chain_symbol_rate_of_ret_pct_best_20 <- data.frame()
revs_conv_Chain_symbol_rate_of_ret_pct_best_20  <- data.frame()

# cycle though all (chosen) tickers
currentBulkOptionsSYMs_index <- 0
if ( nrow(currentBulkOptionsSYMs) > 0 ) {
  for( z in 1:nrow(currentBulkOptionsSYMs) ) {
    currentBulkOptionsSYMs_index <- currentBulkOptionsSYMs_index + 1

    currentChain <- NULL
    # only way that works to 'not show' those WARNINGS (if Yahoo can't find any data )
    currentChain <-tryCatch({

      Sys.sleep(1.3) # yahoo prefers only 3000 per hour ( 3600 seconds )
      getOptionChain(
          currentBulkOptionsSYMs[["Symbol"]][currentBulkOptionsSYMs_index]
        , Exp = currentYahooCallChainExpireDate
      )

      }, warning = function(w) {
           return('WARNING')
      }, error = function(e) {
           return('ERROR')
      }, finally = function(f) {
           NULL
    })
      
    # NOT if a valid response ( 'got data' ) THEN just RETRY once MORE
    if(!is.list(currentChain)) {
      print(paste0("  RETRY list data of  ",currentBulkOptionsSYMs[["Symbol"]][currentBulkOptionsSYMs_index]))

      currentChain <-tryCatch({
      
        Sys.sleep(1.3) # yahoo prefers only 3000 per hour ( 3600 seconds )
        getOptionChain(
            currentBulkOptionsSYMs[["Symbol"]][currentBulkOptionsSYMs_index]
          , Exp = currentYahooCallChainExpireDate
        )

        }, warning = function(w) {
             return('WARNING')
        }, error = function(e) {
             return('ERROR')
        }, finally = function(f) {
             NULL
      })

    }
      
      
    # if successful here(below): I can remove the variables with 'rm' later
    currentChain_calls  <- NULL
    currentChain_puts   <- NULL
    currentChain_symbol <- NULL
    currentChain_symbol_RoR <- NULL

    
    # FINALLY: if a valid response ( 'got data' )
    if(is.list(currentChain)) {
      # print(currentChain)

      currentChain_calls  <- as.data.frame(currentChain$calls)
      currentChain_puts   <- as.data.frame(currentChain$puts)
   
      # include the rownames
      currentChain_calls <- cbind(   
          SymbolOption = rownames(currentChain_calls)
        , currentChain_calls
        , stringsAsFactors = FALSE
      )
       
      # include the rownames
      currentChain_puts <- cbind(   
          SymbolOption = rownames(currentChain_puts)
        , currentChain_puts
        , stringsAsFactors = FALSE
      )

      currentChain_symbol <- currentChain$symbol

      # print(currentChain_symbol)
      
      # note: maxis and minis are returned together e.g. APPL and APPL7
      # note: Yahoo is ACTUALLY called like this
      # http://finance.yahoo.com/q/op?s=AAPL&m=2014-04+Options
      
      # note: the date returned by Yahoo is Saturday ( not Friday )

      
      #### Begin forward conversion area ####
        
      # theoptionsguide.com ... Conversion
      # note: call option: has to be called_out(by my covered call buyer) or call_expires_worthless(by time)
      #                                        (my put expires worthless)   (my put that I exercise(AUTO) is valuable)             
        
      # collect "forward conversion" "Rate of Return" results
      
      # NULL or mangled skip doing the SQL query 
      #   No 'put and call at the same strike price' to match against
      # would end up causing DBI errors
      if ( !( ((is.null(currentChain_puts))  || ( nrow(currentChain_puts)  < 1))    || 
              ((is.null(currentChain_calls)) || ( nrow(currentChain_calls) < 1))
            ) 
         ) 
      {


        currentChain_symbol_RoR <- 
        sqldf(
          # writeLines (
          paste0("
            SELECT * FROM (
              SELECT 
                  '",format(Sys.time() + 3600, format = '%Y%m%d%H%M%S', tz = 'EST', usetz = TRUE),"' n_time
                , '",currentChain_symbol,"'       c_p_symbol 
                , '",currentSQLChainExpireDate,"' c_p_expire_date
                , c.SymbolOption c_SymbolOption
                , tt.MarketCap tt_MarketCap
                , c.Strike c_Strike
                , c.OI   c_OI 
                , c.Vol  c_Vol 
                , c.Chg  c_Chg 
                , c.Ask  c_Ask 
                , c.Last c_Last 
                , c.Bid  c_Bid 
                , tt.LastSale tt_LastSale
                , 'forw_conv' pattern_name
                , c.Strike - tt.LastSale + c.Bid - p.Ask  profit
                ,   (c.Strike - tt.LastSale + c.Bid - p.Ask) / ( tt.LastSale )         tm_rate_of_ret_decimal
                ,   (c.Strike - tt.LastSale + c.Bid - p.Ask) / ( tt.LastSale ) * 100   tm_rate_of_ret_pct
                , p.Ask  p_Ask
                , p.Last p_Last 
                , p.Bid  p_Bid 
                , p.Chg  p_Chg 
                , p.Vol  p_Vol 
                , p.OI   p_OI 
                , p.Strike p_Strike
                , p.SymbolOption p_SymbolOption
                FROM
                    currentChain_calls c
                  , currentChain_puts  p
                  , ( SELECT LastSale, MarketCap FROM currentBulkOptionsSYMs WHERE Symbol = '",currentChain_symbol,"' ) tt
                    WHERE
                      ( c.SymbolOption LIKE '%",currentChain_symbol,"%'  ) AND 
                      ( c.SymbolOption LIKE '%",currentSQLChainExpireDate,"%' AND p.SymbolOption LIKE '%",currentSQLChainExpireDate,"%' ) AND
                      ( SUBSTR( REVERSE(c.SymbolOption),1,INSTR(REVERSE(c.SymbolOption),'C') -1) = 
                        SUBSTR( REVERSE(p.SymbolOption),1,INSTR(REVERSE(p.SymbolOption),'P') -1) 
                      ) AND
                      (
                        SUBSTR( c.SymbolOption,1, LENGTH(c.SymbolOption) - INSTR(REVERSE(c.SymbolOption),'C') ) = 
                        SUBSTR( p.SymbolOption,1, LENGTH(p.SymbolOption) - INSTR(REVERSE(p.SymbolOption),'P') )
                      )
            ) WHERE tm_rate_of_ret_pct  > -2.0 ORDER BY tm_rate_of_ret_pct DESC 
          ") # )
          ,  drv = "SQLite"
        )
        
	      # WHERE tm_rate_of_ret_pct  > -2.0 ORDER BY tm_rate_of_ret_pct DESC 
	
        # see everthing
        # print(currentChain_symbol_RoR)

        # show the best 20 ( if any ) cases
        
        # to the master list combine the latest
        temp_forw_conv_Chain_symbol_rate_of_ret_pct_best_20 <- rbind(
            forw_conv_Chain_symbol_rate_of_ret_pct_best_20
          , currentChain_symbol_RoR) # latest appended
      
        # print(temp_forw_conv_Chain_symbol_rate_of_ret_pct_best_20)
        
        # order by tm_rate_of_ret_pct DESC
        # give me the highest(best 20)
        # note: hardcoded 20 will produce JUNK extra rows ( IGNORABLE for now ) 
        # NA          <NA>
        # NA.1        <NA>
        
        # only bother if a non-zero observations data frame 
        # ... will case a DBI 'no such table' and bind.data must have non-zero dimensions
        if (nrow(temp_forw_conv_Chain_symbol_rate_of_ret_pct_best_20) != 0) {
          
            forw_conv_Chain_symbol_rate_of_ret_pct_best_20 <- sqldf("SELECT * FROM temp_forw_conv_Chain_symbol_rate_of_ret_pct_best_20 ORDER BY tm_rate_of_ret_pct DESC LIMIT 20"
              ,  drv = "SQLite"
            )
    
        }
    
      
      }
      
      #### End forward conversion area ####
           
           

      #### Begin reverse conversion area ####
        
      # theoptionsguide.com ... Reversal
      # note: put option: has to be called_out(by my cash secured put buyer) or put_expires_worthless(by time)
      #                                     (my call expires worthless)        (my call that I exercise(AUTO) is valuable)             
        
      # collect "reverse conversion" "Rate of Return" results
      
      # NULL or mangled skip doing the SQL query 
      #   No 'put and call at the same strike price' to match against
      # would end up causing DBI errors
      if ( !( ((is.null(currentChain_puts))  || ( nrow(currentChain_puts)  < 1))    || 
               ((is.null(currentChain_calls)) || ( nrow(currentChain_calls) < 1))
            ) 
         ) 
      {

        # NOTE: p_Strike == c_Strike 
        # ( althought the math would seem more logical if I used p.Strike instead )
        currentChain_symbol_RoR <- 
        sqldf(
          # writeLines (
          paste0("
            SELECT * FROM (
              SELECT 
                  '",format(Sys.time() + 3600, format = '%Y%m%d%H%M%S', tz = 'EST', usetz = TRUE),"' n_time
                , '",currentChain_symbol,"'       c_p_symbol 
                , '",currentSQLChainExpireDate,"' c_p_expire_date
                , c.SymbolOption c_SymbolOption
                , tt.MarketCap tt_MarketCap
                , c.Strike c_Strike
                , c.OI   c_OI 
                , c.Vol  c_Vol 
                , c.Chg  c_Chg 
                , c.Ask  c_Ask 
                , c.Last c_Last 
                , c.Bid  c_Bid 
                , tt.LastSale tt_LastSale
                , 'revs_conv' pattern_name
                , tt.LastSale - c.Strike  + p.Bid - c.Ask  profit
                ,   (tt.LastSale - c.Strike  + p.Bid - c.Ask) / ( tt.LastSale )         tm_rate_of_ret_decimal
                ,   (tt.LastSale - c.Strike  + p.Bid - c.Ask) / ( tt.LastSale ) * 100   tm_rate_of_ret_pct
                , p.Ask  p_Ask
                , p.Last p_Last 
                , p.Bid  p_Bid 
                , p.Chg  p_Chg 
                , p.Vol  p_Vol 
                , p.OI   p_OI 
                , p.Strike p_Strike
                , p.SymbolOption p_SymbolOption
                FROM
                    currentChain_calls c
                  , currentChain_puts  p
                  , ( SELECT LastSale, MarketCap FROM currentBulkOptionsSYMs WHERE Symbol = '",currentChain_symbol,"' ) tt
                    WHERE
                      ( c.SymbolOption LIKE '%",currentChain_symbol,"%'  ) AND 
                      ( c.SymbolOption LIKE '%",currentSQLChainExpireDate,"%' AND p.SymbolOption LIKE '%",currentSQLChainExpireDate,"%' ) AND
                      ( SUBSTR( REVERSE(c.SymbolOption),1,INSTR(REVERSE(c.SymbolOption),'C') -1) = 
                        SUBSTR( REVERSE(p.SymbolOption),1,INSTR(REVERSE(p.SymbolOption),'P') -1) 
                      ) AND
                      (
                        SUBSTR( c.SymbolOption,1, LENGTH(c.SymbolOption) - INSTR(REVERSE(c.SymbolOption),'C') ) = 
                        SUBSTR( p.SymbolOption,1, LENGTH(p.SymbolOption) - INSTR(REVERSE(p.SymbolOption),'P') )
                      )
            ) WHERE tm_rate_of_ret_pct  > -2.0 ORDER BY tm_rate_of_ret_pct DESC 
          ") # )
          ,  drv = "SQLite"
        )
        
	      # WHERE tm_rate_of_ret_pct  > -2.0 ORDER BY tm_rate_of_ret_pct DESC
	
        # see everthing
        # print(currentChain_symbol_RoR)

        # show the best 20 ( if any ) cases
        
        # to the master list combine the latest
        
        # TEMORARILY DO NOT COLLECT REVERSE CONVERSIONS
        # I WANT TO SEE THE MOST PROFITABLE 'FORWARD CONVERSION' 
        # ( DO NOT KEEP THE RESULTS )
        # currentChain_symbol_RoR <- NULL
        
         temp_revs_conv_Chain_symbol_rate_of_ret_pct_best_20 <- rbind(
             revs_conv_Chain_symbol_rate_of_ret_pct_best_20
          , currentChain_symbol_RoR) # latest appended
      
        # print(temp_revs_conv_Chain_symbol_rate_of_ret_pct_best_20)
        
        # order by tm_rate_of_ret_pct DESC
        # give me the highest(best 20)
        # note: hardcoded 20 will produce JUNK extra rows ( IGNORABLE for now ) 
        # NA          <NA>
        # NA.1        <NA>
        
        # only bother if a non-zero observations data frame 
        # ... will case a DBI 'no such table' and bind.data must have non-zero dimensions
        if (nrow(temp_revs_conv_Chain_symbol_rate_of_ret_pct_best_20) != 0) {

          revs_conv_Chain_symbol_rate_of_ret_pct_best_20 <- sqldf("SELECT * FROM temp_revs_conv_Chain_symbol_rate_of_ret_pct_best_20 ORDER BY tm_rate_of_ret_pct DESC LIMIT 20"
            ,  drv = "SQLite"
          )
          

        }

      }
      
      #### End reverse conversion area ####
           

      ##
      # save 'potentials' to files     
      ##        
        
      # forw_conv     
           
      # after each iteration - I destroy the old file - I create a brand new file
      # save to a new file ( overwrite to zero bytes ( initialize ))
      # exact time on my computer
      capture.output(print(as.character(format(Sys.time(), "%a %b %d %X %Y"))), file = filename_forw_conv) 

      # done symbol and loop ( just append data )
      capture.output(
      print(paste0("After ",currentBulkOptionsSYMs[["Symbol"]][currentBulkOptionsSYMs_index]
        ,"(",currentBulkOptionsSYMs_index," of ",nrow(currentBulkOptionsSYMs),")"
      ))
      ,  file = filename_forw_conv, append = TRUE) 
      
      # actual data I care about - tm - rate of return ( best 20 )
      capture.output(
      print(forw_conv_Chain_symbol_rate_of_ret_pct_best_20)
      ,  file = filename_forw_conv, append = TRUE) 
      

       # revs_conv   
           
      # after each iteration - I destroy the old file - I create a brand new file
      # save to a new file ( overwrite to zero bytes ( initialize ))
      # exact time on my computer
      capture.output(print(as.character(format(Sys.time(), "%a %b %d %X %Y"))), file = filename_revs_conv) 

      # done symbol and loop ( just append data )
      capture.output(
      print(paste0("After ",currentBulkOptionsSYMs[["Symbol"]][currentBulkOptionsSYMs_index]
        ,"(",currentBulkOptionsSYMs_index," of ",nrow(currentBulkOptionsSYMs),")"
      ))
      ,  file = filename_revs_conv, append = TRUE) 
      
      # actual data I care about - tm - rate of return ( best 20 )
      capture.output(
      print(revs_conv_Chain_symbol_rate_of_ret_pct_best_20)
      ,  file = filename_revs_conv, append = TRUE) 
      
      
    }
  
  # forw_conv - tell I am finished with iteration
  capture.output(paste0("Done with loop ",currentBulkOptionsSYMs_index," of ",nrow(currentBulkOptionsSYMs)),  file = filename_forw_conv, append = TRUE)
  
  # revs_conv - tell I am finished with iteration
  capture.output(paste0("Done with loop ",currentBulkOptionsSYMs_index," of ",nrow(currentBulkOptionsSYMs)),  file = filename_revs_conv, append = TRUE)

  
  
  # possibly force a flush?
  # avoid the error
  #   cannot open file '': Permission denied
  closeAllConnections()
  
  }
  
}

# revs_conv_Chain_symbol_rate_of_ret_pct_best_20
# 
# forw_conv_Chain_symbol_rate_of_ret_pct_best_20


# requery top 20s add dividends
# potentials

quotesOfInterest <-
        c( "Previous Close", "Volume", "Shares Owned" ,"Change", "% Change", "Open", "Last Trade Time" ,"Last Trade (Price Only)", "Price Paid" , "Ask" ,"Bid"
           , "Ex-Dividend Date","Dividend Pay Date", "Dividend/Share", "Dividend Yield", "Symbol", "Name"
         ) 

quotesOfInterestNicerNames <- quotesOfInterest  

# nicer column names 
quotesOfInterestNicerNames <- gsub("%", "pct"   ,quotesOfInterestNicerNames)
quotesOfInterestNicerNames <- gsub(" ", "_"     ,quotesOfInterestNicerNames)
quotesOfInterestNicerNames <- gsub("/", "_per_" ,quotesOfInterestNicerNames)
quotesOfInterestNicerNames <- gsub("-", "_"     ,quotesOfInterestNicerNames)
quotesOfInterestNicerNames <- gsub("\\.", "_"   ,quotesOfInterestNicerNames)
# beginning prepend a q_
quotesOfInterestNicerNames <- gsub("^", "q_"    ,quotesOfInterestNicerNames)


         
# forw_conv 
# dummy first row of column names
forw_conv_Item_quotes <- t(data.frame(quotesOfInterestNicerNames))
# nicer column names
colnames(forw_conv_Item_quotes) <- quotesOfInterestNicerNames
# remove that dummy first row
forw_conv_Item_quotes <- forw_conv_Item_quotes[-c(1),]

# testing ( once )
# forw_conv_Chain_symbol_rate_of_ret_pct_best_20_SAVED <- forw_conv_Chain_symbol_rate_of_ret_pct_best_20

# put_back ( often )
# forw_conv_Chain_symbol_rate_of_ret_pct_best_20 <- forw_conv_Chain_symbol_rate_of_ret_pct_best_20_SAVED

# if I found a quote, I want to remember it, so that I do not do it again
forw_conv_Chain_symbol_getQuoteFound <- c()

# storage per unique symbol
# forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes_temp <- data.frame()

# all symbols and their instances 
forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes <- data.frame()

forw_conv_Chain_symbol_rate_of_ret_pct_best_20_index <- 0
if ( nrow(forw_conv_Chain_symbol_rate_of_ret_pct_best_20) > 0 ) {
  for( x in 1:nrow(forw_conv_Chain_symbol_rate_of_ret_pct_best_20) ) {
    forw_conv_Chain_symbol_rate_of_ret_pct_best_20_index <- forw_conv_Chain_symbol_rate_of_ret_pct_best_20_index + 1
  
    Sys.sleep(1.3) # yahoo prefers only 3000 per hour ( 3600 seconds )
    
    # if not already have Quote for that symbol, then go get it
    if ( !(  forw_conv_Chain_symbol_rate_of_ret_pct_best_20[["c_p_symbol"]][forw_conv_Chain_symbol_rate_of_ret_pct_best_20_index] %in% forw_conv_Chain_symbol_getQuoteFound ) ) { 
    
      # SHOULD NOT 'RE-CALL APPLE' IF I ALREADY HAVE APPLE
      #   SLIGHT OPTIMIZATION I 'SHOULD' MAKE ( IN THE FUTURE ) ( CURRENTLY, I RE-CALL APPL PER EVERY LINE OF MY BEST 20)
      currentItem_quote <-
        getQuote(forw_conv_Chain_symbol_rate_of_ret_pct_best_20[["c_p_symbol"]][forw_conv_Chain_symbol_rate_of_ret_pct_best_20_index]
          , what=yahooQF( 
            quotesOfInterest
        ))
      
      # in case getQuote does not work ... protect against an error
      # create a false zero-row dataframe with correct Nice column names
      if( is.null(currentItem_quote) ) {

        # dummy first row of column names
        currentItem_quote <- t(data.frame(quotesOfInterestNicerNames))

        # nicer column names
        colnames(currentItem_quote) <- quotesOfInterestNicerNames

        # remove that dummy first row
        currentItem_quote <- currentItem_quote[-c(1),]
            
      } else {

        # not an error
        # nicer column names 
        colnames(currentItem_quote) <- gsub("%", "pct"   ,colnames(currentItem_quote))
        colnames(currentItem_quote) <- gsub(" ", "_"     ,colnames(currentItem_quote))
        colnames(currentItem_quote) <- gsub("/", "_per_" ,colnames(currentItem_quote))
        colnames(currentItem_quote) <- gsub("-", "_"     ,colnames(currentItem_quote))
        colnames(currentItem_quote) <- gsub("\\.", "_"   ,colnames(currentItem_quote))
        # beginning prepend a q_
        colnames(currentItem_quote) <- gsub("^", "q_"    ,colnames(currentItem_quote))
        # CAN I JUST DO THIS ... ??
        # ?? colnames(currentItem_quote) <- quotesOfInterestNicerNames
        
        print(currentItem_quote)
        

        # add to the found collection
        forw_conv_Chain_symbol_getQuoteFound <- 
          rbind ( forw_conv_Chain_symbol_getQuoteFound,
          forw_conv_Chain_symbol_rate_of_ret_pct_best_20[["c_p_symbol"]][forw_conv_Chain_symbol_rate_of_ret_pct_best_20_index]
        )
        

        # merge - append those new columns per unique symbol
        # number of rows = this symbol number of instances
        forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes_temp <- 
          merge( forw_conv_Chain_symbol_rate_of_ret_pct_best_20
               , currentItem_quote
               , by.x = c("c_p_symbol")
               , by.y = c("q_Symbol")
               )
        
        # row bind # row_append cummulative symol_instances collections
        forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes <-
          rbind(  forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes
                , forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes_temp )
        
        
        # re-sort decending
        # I know that I have 20 rows  I do not have to check the count
        
        forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes <- sqldf("SELECT * FROM forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes ORDER BY tm_rate_of_ret_pct DESC LIMIT 20"
          ,  drv = "SQLite"
        )
        
        # 
        # print(forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes)
        
      }
    
    }
    
    
  }
}


print(forw_conv_Chain_symbol_rate_of_ret_pct_best_20_with_quotes)
  

} # end anon
