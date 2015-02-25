## search for "LEFT_OFF"

# main-valuesight1-999-miniplay1.R 

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) # Try these = width 

setwd("N:\\MyVMWareSharedFolder\\valuesight1\\R") 

bookmark_here <- 1

main_valuesight1_999_miniplay1 <- function(pauseat=NULL) {
  
  main_valuesight1_999_miniplay1_inner <- function(...) {
    
    oldtz <- Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    
    the_end_debug_bookmark_here <- 1
    
    setwd("N:\\MyVMWareSharedFolder\\valuesight1\\R") # TO BE CHANGED LATER
    
    require(quantmod)
    require(PerformanceAnalytics)
    
    # the COL1
    
    if( file.exists(paste0(getwd(),"/","GSPC",".Rdata"))){
      load(file = paste0(getwd(),"/","GSPC",".Rdata"), envir = environment())
    } else {
      Sys.sleep(0.80)
      assign("GSPC", value=getSymbols("^GSPC", from = "1950-03-01", to = "2014-12-31", index.class=c("POSIXt","POSIXct"),auto.assign = FALSE ))
      save(list = c("GSPC"), file = paste0(getwd(),"/","GSPC",".Rdata"))
    }    
    assign("GSPC", value=GSPC, envir = .GlobalEnv)
    assign("GSPC.MO", value=to.monthly(GSPC,indexAt='lastof',drop.time=TRUE))
    assign("GSPC.MO", value=GSPC.MO, envir = .GlobalEnv)
    assign("GSPC.MO.ADJ", value=Ad(GSPC.MO))
    assign("GSPC.MO.ADJ", value=GSPC.MO.ADJ, envir = .GlobalEnv)
    
    bookmark_here <- 1
    
    # as of "Feb 22, 2015"
    
    # LARGEVALUE
    # http://money.usnews.com/funds/mutual-funds/rankings/large-value
    
    # MIDCAPVALUE
    # http://money.usnews.com/funds/mutual-funds/rankings/mid-cap-value
    
    # SMALLVALUE
    # http://money.usnews.com/funds/mutual-funds/rankings/small-value
    
    ALLVALUE <- list(
      LARGEVALUE = list(
          BPAIX = list( DESCR = "Boston Partners All Cap Value Fund (BPAIX)"          , EXPS = 0.70 )
        , FLVEX = list( DESCR = "Fidelity Large Cap Value Enhanced Index Fund (FLVEX)", EXPS = 0.47 )
        , VIVAX = list( DESCR = "Vanguard Value Index Fund (VIVAX)"                      , EXPS =  0.24  )
        , DTMMX = list( DESCR = "DFA Tax Managed U.S. Marketwide Value Portfolio (DTMMX)", EXPS =  0.37  )
        , LSVEX = list( DESCR = "LSV Value Equity Fund (LSVEX)"                          , EXPS =  0.68  )
        , VUVLX = list( DESCR = "Vanguard U.S. Value Fund (VUVLX)"                       , EXPS =  0.29  )
        , DFMVX = list( DESCR = "DFA Tax Managed U.S. Marketwide Value II Portfolio (DFMVX)", EXPS = 0.23  )
        , DFLVX = list( DESCR = "DFA U.S. Large Cap Value Portfolio (DFLVX)"                , EXPS = 0.27  )
        , DFCVX = list( DESCR = "DFA U.S. Large Cap Value II Portfolio (DFCVX)"             , EXPS = 0.17  )
        , HOVLX = list( DESCR = "Homestead Funds Value Fund (HOVLX)"                        , EXPS = 0.64  )

        
      ),
      MIDCAPVALUE = list(
          ACLMX = list( DESCR = "American Century Investments NT Mid Cap Value Fund (ACLMX)", EXPS = 0.81 )
        , HAMVX = list( DESCR = "Harbor Mid Cap Value Fund (HAMVX)"                         , EXPS = 0.93 )
        , NSEIX = list( DESCR = "Nicholas Equity Income Fund (NSEIX)"                       , EXPS = 0.72 )
        , FMPAX = list( DESCR = "Fidelity Mid Cap Value Fund (FMPAX)"                       , EXPS = 1.15 )
        , VASVX = list( DESCR = "Vanguard Selected Value Fund (VASVX)"                      , EXPS = 0.44 )
        , PKPPX = list( DESCR = "Principal MidCap Value Fund III (PKPPX)"                   , EXPS = 1.44 )
        , FDVLX = list( DESCR = "Fidelity Value Fund (FDVLX)"                               , EXPS = 0.76  )
        , TIMVX = list( DESCR = "TIAA-CREF Mid-Cap Value Fund (TIMVX)"                      , EXPS = 0.45  )
        , JVMAX = list( DESCR = "John Hancock Funds Disciplined Value Mid Cap Fund (JVMAX)" , EXPS = 1.18  )
        , HWAAX = list( DESCR = "Hotchkis & Wiley Value Opportunities Fund (HWAAX)"         , EXPS = 1.25  )

      ),
      SMALLVALUE = list(
          HWSAX = list( DESCR = "Hotchkis & Wiley Small Cap Value Fund (HWSAX)"         , EXPS = 1.25  )
        , VISVX = list( DESCR = "Vanguard Small Cap Value Index Fund (VISVX)"           , EXPS = 0.24  )
        , DTMVX = list( DESCR = "DFA Tax Managed U.S. Targeted Value Portfolio (DTMVX)" , EXPS = 0.44  )
        , MRSNX = list( DESCR = "BMO Small-Cap Value Fund (MRSNX)"                      , EXPS = 1.00  )
        , DFSVX = list( DESCR = "DFA U.S. Small Cap Value Portfolio (DFSVX)"            , EXPS = 0.52  )
        , DFFVX = list( DESCR = "DFA U.S. Targeted Value Portfolio (DFFVX)"             , EXPS = 0.37  )
        , ANCCX = list( DESCR = "Ancora MicroCap Fund (ANCCX)"                          , EXPS = 2.59  )
        , PPVIX = list( DESCR = "Principal SmallCap Value Fund II (PPVIX)"              , EXPS = 1.10  )
        , WSCVX = list( DESCR = "Walthausen Small Cap Value Fund (WSCVX)"               , EXPS = 1.25  )
        , VSCAX = list( DESCR = "Invesco Small Cap Value (VSCAX)"                       , EXPS = 1.12  )

      )
    )

    # store in a data.frame for sorting LATER
    COLLECTION <- data.frame(VALUECAT=NA, TICKER=NA, DESCR=NA, EXPS=NA,  MO.BASE.NUMB=NA, BEATS.MO.BASE.PCT=NA, YRS.BASE.NUMB=NA, BEATS.YRS.BASE.PCT=NA, stringsAsFactors=FALSE) 
    COLLECTION <- COLLECTION[FALSE,,drop=FALSE]
    
    for(vars.allvalue in seq_along(ALLVALUE)) {
      
      print("")
      print("")
      print(names(ALLVALUE)[vars.allvalue])
      print("")
      
      for(vars.specvalue in seq_along(ALLVALUE[[vars.allvalue]])) {
        
        print(paste0(names(ALLVALUE[[vars.allvalue]])[vars.specvalue]))
        
        print(paste0(" ",ALLVALUE[[vars.allvalue]][[vars.specvalue]][["DESCR"]]))
        print(paste0("  Expenses: ",ALLVALUE[[vars.allvalue]][[vars.specvalue]][["EXPS"]]))
        print("")
        
        TICKER <- names(ALLVALUE[[vars.allvalue]])[vars.specvalue]
        
        if( file.exists(paste0(getwd(),"/",TICKER,".Rdata"))){
          load(file = paste0(getwd(),"/",TICKER,".Rdata"), envir = environment())
        } else {
          Sys.sleep(0.80)
          assign(TICKER, value=getSymbols(TICKER, from = "1950-03-01", to = "2014-12-31", index.class=c("POSIXt","POSIXct"), auto.assign = FALSE ))  
          save(list = c(TICKER), file = paste0(getwd(),"/",TICKER,".Rdata"))
        }    
        assign(TICKER, value=eval(parse(text=TICKER)), envir = .GlobalEnv)
        
        # monthly
        assign(paste0(TICKER,".MO"), value=to.monthly(eval(parse(text=TICKER)),indexAt='lastof',drop.time=TRUE))
          CORRECTCOLNAMES <- colnames( eval(parse(text=TICKER)) )
          # crazy to.monthly
          NEW <- eval(parse(text=paste0(TICKER,".MO")))
          colnames(NEW) <-  CORRECTCOLNAMES
          assign(paste0(TICKER,".MO"), value=NEW)
          rm(NEW)
        # remove the first month ( It may be 'not a full month' )
        assign(paste0(TICKER,".MO"), value=eval(parse(text=paste0(TICKER,".MO[-1]")))  )
        assign(paste0(TICKER,".MO"), value=eval(parse(text=paste0(TICKER,".MO"))), envir = .GlobalEnv)
        
        # only the .Adjusted column
        assign(paste0(TICKER,".MO.ADJ"), value=Ad(eval(parse(text=paste0(TICKER,".MO")))))
        assign(paste0(TICKER,".MO.ADJ"), value=  eval(parse(text=paste0(TICKER,".MO.ADJ"))), envir = .GlobalEnv)
        
        bookmark_here <- 1
        
        # find maximum drawdown ( Andre Definition ) during 2008
        if(NROW(eval(parse(text=paste0(TICKER,".MO.ADJ")))["2007-12-31"]) == 1 && NROW(eval(parse(text=paste0(TICKER,".MO.ADJ")))["2008-12-31"]) == 1) {
          draw.down.2008.pct =  ( min(eval(parse(text=paste0(TICKER,".MO.ADJ")))["2008"]) - eval(parse(text=paste0(TICKER,".MO.ADJ")))["2007-12-31"] )/ abs(eval(parse(text=paste0(TICKER,".MO.ADJ")))["2007-12-31"]) * 100
        } else {
          draw.down.2008.pct = NA
        }
        names(draw.down.2008.pct)[1] <- "DRAW.DOWN.2008.PCT"
        
        bookmark_here <- 1
        
        # xts::cbind.xts WRAPS over xts::merge.xts
        prices <- cbind(GSPC.MO.ADJ, eval(parse(text=paste0(TICKER,".MO.ADJ"))) )
        
        # zoo::na.trim.default
        prices <- na.trim(prices)
        
        # monthly returns
        returns <- Return.calculate(prices)
        
        # zoo::na.trim.default
        returns <- na.trim(returns)
        
        # subtract off expense fees
        returns[,2] <- returns[,2] - ALLVALUE[[vars.allvalue]][[vars.specvalue]][["EXPS"]] / 100 / 12.0
        
        # yearly returns
        # xts::apply.yearly
        returns.yearly <- apply.yearly(returns, Return.cumulative)
        
        # eliminate that early NON-FULL years
        
        if( 
            substr(index(eval(parse(text=paste0(TICKER,".MO.ADJ"))))[1],6,10)  == "01-31" &&
            substr(index(GSPC.MO.ADJ)[1],6,10)  == "01-31" 
        ) 
        {
          returns.yearly <- returns.yearly
        } else {
          returns.yearly <- returns.yearly[paste0(
            max(
              (as.integer(substr(index( eval(parse(text=paste0(TICKER,".MO.ADJ"))) )[1] ,1,4)) + 1),
              (as.integer(substr(index(GSPC.MO.ADJ)[1],1,4)) + 1)
            )
            ,"::")
            ]
        }
        
        # which 'time ranges' that the VALUE fund beat the baseline
        
        # monthly
        returns.COL2BEATS1 <- cbind(returns, local({ COL2BEATS1 = ifelse(returns[,2] > returns[,1], 1.0, 0.0); colnames(COL2BEATS1)[1] <- "COL2BEATS1"; return(COL2BEATS1) }))
        
        # yearly
        returns.yearly.COL2BEATS1 <- cbind(returns.yearly, local({ COL2BEATS1 = ifelse(returns.yearly[,2] > returns.yearly[,1], 1.0, 0.0); colnames(COL2BEATS1)[1] <- "COL2BEATS1"; return(COL2BEATS1) }))
        
        # what  'pct of time ranges'' that the VALUE fund beat the baseline
        
        # monthly
        returns.COL2BEATS1.PCT <- sum(returns.COL2BEATS1[,"COL2BEATS1"])/NROW(returns.COL2BEATS1) * 100.0
        
        # yearly
        returns.yearly.COL2BEATS1.PCT <- sum(returns.yearly.COL2BEATS1[,"COL2BEATS1"])/NROW(returns.yearly.COL2BEATS1) * 100.0
        
        print(paste0("Number of months to compare:          " , NROW(returns.COL2BEATS1)))
        print(paste0("Beats BASELINE month pct of time: " , returns.COL2BEATS1.PCT))
        print("")
        print(paste0("Number of years to  compare:          " , NROW(returns.yearly.COL2BEATS1)))
        print(paste0("Beats BASELINE year  pct of time: " , returns.yearly.COL2BEATS1.PCT))
        print("")
        
        # store in THE data.frame FOR sorting LATER
        COLLECTION <- rbind(COLLECTION,data.frame(
            VALUECAT          = names(ALLVALUE)[vars.allvalue]
          , TICKER            = names(ALLVALUE[[vars.allvalue]])[vars.specvalue]
          , DESCR             = ALLVALUE[[vars.allvalue]][[vars.specvalue]][["DESCR"]]
          , EXPS              = ALLVALUE[[vars.allvalue]][[vars.specvalue]][["EXPS"]]
          , MO.BASE.NUMB          = NROW(returns.COL2BEATS1)
          , BEATS.MO.BASE.PCT     = returns.COL2BEATS1.PCT
          , YRS.BASE.NUMB         = NROW(returns.yearly.COL2BEATS1)
          , BEATS.YRS.BASE.PCT    = returns.yearly.COL2BEATS1.PCT
          , DRAW.DOWN.2008.PCT    = draw.down.2008.pct
        ))
        
        bookmark_here <- 1
        # View(COLLECTION)
        
      }
      
    }
    
    bookmark_here <- 1
    # View(COLLECTION)
    
    # as of "Feb 22, 2015"
    
    # LARGEVALUE
    # http://money.usnews.com/funds/mutual-funds/rankings/large-value
    
    # MIDCAPVALUE
    # http://money.usnews.com/funds/mutual-funds/rankings/mid-cap-value
    
    # SMALLVALUE
    # http://money.usnews.com/funds/mutual-funds/rankings/small-value
    
    the_end_debug_bookmark_here <- 1
    
    Sys.setenv(TZ=oldtz)
    
    print("Done with main_valuesight1_999_miniplay1()")
    
  }  
  
  main_valuesight1_999_miniplay1_inner(pauseat) # should have been do.call
  
}
# main_valuesight1_999_miniplay1()
# debugSource('N:/MyVMWareSharedFolder/valuesight1/R/main-valuesight1-999-miniplay1.R') 
# rm(list=ls(all.names=TRUE))
