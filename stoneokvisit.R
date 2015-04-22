

# shell("rstudio", wait=FALSE)

okcupid_visit_looper_dev <- function() {
  
  maininner <- function() {
    
    set.seed(runif(1, min = 0, max = 1))
    
    require(RSelenium)
    require(stringr)
    
    # REM: taskmgr - manually KILL off java.exe if it is running
    startServer()    
    Sys.sleep(5.0) # 5 second wait
    
    cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "time861wiz_time861wiz") 
    remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof)
    remDr$open() # oracle.com  
    Sys.sleep(10 + 5* runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    print("opened browser home page")
    
    # Sys.sleep(1 + 5* runif(1, min = 0, max = 1)) # 6 to 11 seconds wait
    
    remDr$navigate("https://www.okcupid.com/login")
    Sys.sleep(10 + 5* runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    print("navigated to okcupid")
    
    webElem1 <- remDr$findElement("css selector", "#login_username")
    webElem1$sendKeysToElement(list("time861wiz"))
    Sys.sleep(3 + 2* runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2 <- remDr$findElement("css selector", "#login_password")
    webElem2$sendKeysToElement(list("739heg08"))
    Sys.sleep(3 + 2* runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2$sendKeysToElement(list(key="enter"))
    Sys.sleep(5 + 5* runif(1, min = 0, max = 1)) # 5 to 10 seconds wait
    
    print("logged into okcupid")
    
    # MAGIC NUMBER
    agerange <- 30:31
    agerange_str <- "30:31"
    
    for(agecurr in agerange) { # testing only 30 and 31 # 70:18
      
      print(paste0("beginning age ",agecurr))
      
      remDr$navigate(paste0("http://www.okcupid.com/match?filter1=0,34&filter2=2,",agecurr,",",agecurr,"&filter3=3,50&filter4=5,604800&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=500"))
      Sys.sleep(10 + 5* runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
      
      # LOOP ( check to see if I am at the END of the PAGE? ) ( no more information to be dynamically loaded )
      
      ## DOES WORK
      
      # begin on the top of the page
      
      print(paste0("beginning scroll down to the bottom of the page of : ",agecurr, " of age ", agerange_str))
      
      
      webElemSB <- remDr$findElement("css selector", "#submit_button") # THE 'SEARCH' button of 'SEARCH/CLEAR' 
      remDr$mouseMoveToLocation(webElement = webElemSB)
      
      bookmarkhere <- 1
      
      # Am I at the end of the page?
      window.innerHeight          <- remDr$executeScript("return window.innerHeight")[[1]]
      window.scrollY              <- remDr$executeScript("return window.scrollY")[[1]]
      document.body.offsetHeight  <- remDr$executeScript("return document.body.offsetHeight")[[1]]
      
      # if not at the end of the page keep scrolling until I get there
      while( !((window.innerHeight + window.scrollY) >= document.body.offsetHeight) ) {
        
        webElemSB$sendKeysToElement(list("\uE010")) # AGGRESSIVE PAGE DOWN
        Sys.sleep(10 + 5* runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
        
        window.innerHeight          <- remDr$executeScript("return window.innerHeight")[[1]]
        window.scrollY              <- remDr$executeScript("return window.scrollY")[[1]]
        document.body.offsetHeight  <- remDr$executeScript("return document.body.offsetHeight")[[1]]
        
      }
      
      print(paste0("now at bottom of the page of : ",agecurr, " of age ", agerange_str))
      
      # now at the bottom of the page, 
      # get the distinct user names found in the HTML 
      
      alinkslength <- remDr$executeScript("return document.getElementsByTagName('a').length;")[[1]]
      Sys.sleep(0.01)

      print(paste0("begin collecting all A elements  of the page of : ",agecurr, " of age ", agerange_str))
      
      apagearefs <- c()
      
      if ( alinkslength > 0 ) { 
        for(alinkcurr in 0:(alinkslength -1)) {
          apagearefs <- c(apagearefs,remDr$executeScript(paste0("return document.getElementsByTagName('a')[",alinkcurr,"].href;"))[[1]])
          Sys.sleep(0.01)
        }
      }

      print(paste0("end collecting all A elements  of the page of : ",agecurr, " of age ", agerange_str))
      
      # unique
      apagearefsu   <- unique(apagearefs)

      # profiles
      apagearefsup  <- apagearefsu[str_detect(apagearefsu,"^.*profile")]

      # regulars
      apagearefsupr <- apagearefsup[str_detect(apagearefsup,"[?]cf=regular$")]

      # I have not seen these(after the filters are done), but just in case crept through
      
      apagearefsupr <- apagearefsupr[!grepl("CALLGIRL",apagearefsupr,ignore.case=TRUE)]
      apagearefsupr <- apagearefsupr[!grepl("ROBOT",apagearefsupr,ignore.case=TRUE)]
      
      # loop and visit each name - from bottom(rev) to top ( testing ) 
      # testing - visit from the BOTTOM going UP
      
      print(paste0("begin visiting each profile of the page of : ",agecurr, " of age ", agerange_str))
      
      for(alink in rev(apagearefsupr)[1:2]) { # testing: reverse and first 2 names ( testing )
        
        print(paste0("begin visiting ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
        
        remDr$navigate(alink)
        Sys.sleep(2 + 2 * runif(1, min = 0, max = 1)) # 2 to 4 seconds wait

        remDr$executeScript("return 0")
         
        print(paste0("end visiting ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
        
        remDr$goBack()
        Sys.sleep(2 + 2 * runif(1, min = 0, max = 1)) # 2 to 4 seconds wait
        
        remDr$executeScript("return 0")
        
      }
      
      print(paste0("end visiting each profile of the page of : ",agecurr, " of age ", agerange_str))
      
      print(paste0("ending age ", agecurr))
      
    }
    
    bookmarkhere <- 1
    
    # manually logout of ok cupid here
    # manually X out ( shutdown ) the browser
    
    print("begin closing remDr")
    remDr$close() 
    print("end closing remDr")

    print("begin closeServer remDr")
    result = tryCatch({ remDr$closeServer() }, warning = function(w) {}, error = function(e) {}, finally = {})
    print("end closeServer remDr")
    
  }
  maininner()
}


# rm(list=ls(),envir = .GlobalEnv)
# debugSource('J:/YDrive/All_NewSeduction/All_ElectronicSpeech/RSeleniumAndBrowsers/AES1/okcupid_visit_looper_dev.R')

# NOTE: Optional, but recommended, for performance, Turn OFF 'view google chrome images'

# REM: taskmgr - manually KILL off java.exe if it is running
# MANUALLY PLACE DOWN THE BREAKPOINT

# okcupid_visit_looper_dev()
