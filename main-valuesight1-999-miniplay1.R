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
      ),
      MIDCAPVALUE = list(
          ACLMX = list( DESCR = "American Century Investments NT Mid Cap Value Fund (ACLMX)", EXPS = 0.81 )
        , HAMVX = list( DESCR = "Harbor Mid Cap Value Fund (HAMVX)"                         , EXPS = 0.93 )
      ),
      SMALLVALUE = list(
          HWSAX = list( DESCR = "Hotchkis & Wiley Small Cap Value Fund (HWSAX)", EXPS = 1.25 )
        , VISVX = list( DESCR = "Vanguard Small Cap Value Index Fund (VISVX)"  , EXPS =  0.24 )
      )
    )
    
    
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
        
        bookmark_here <- 1
        
      }
      
    }
    
    bookmark_here <- 1
    
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
