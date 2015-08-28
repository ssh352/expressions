

# okcupid
# DON'T FORGET TO STARTUP PostgreSQL !!


options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) 
options(digits.secs = 6)
# options(error=NULL) 
options(error = recover) 


if(Sys.getenv("RSTUDIO") == "1") {
  debugSource(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
} else {
  source(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
}

okcupid_visit_looper_dev <- function(curr_port = 4444, browser = "firefox", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), on_exit_logoff_site = TRUE, on_exit_close_browser = TRUE, on_exit_stop_selenium_server = FALSE, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE") { 
  # OR action = "message_greet_matchname" "message_random_catchphrase"
  # OR not_to_msg = "all_all"
  
  looper_typed_in_call <- match.call( ) # language
  
  maininner <- function() {
    
    oldtz <- Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    
    set.seed(runif(1, min = 0, max = 1))
    
    require(RSelenium)
    require(stringr)
    
    # Administrative sleep
    # Sys.sleep(3600.0 * 8) # 5:30 + 8 hours = 1:30 p.m.START
    # Administravive sleep ( no profile - with images - will take longer )
    # Sys.sleep(3600.0 * 7) # 5:30 + 8 hours = 1:30 p.m.START
    
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host = "127.0.0.1", dbname = "aes_db", user = "postgres", password = "postgres")
    
    # REM: taskmgr OR 'some other way' KILL off java.exe if it is running
    # SHOULD have been arleady started
    manageRSeleniumServer(curr_port = 4444, doif = "if_server_not_started_please_start" )
    
    # native events: TRUE; default on IE. 
    #    not_default on chrome BUT settable = TRUE works, 
    #    NEVER default settable on firefox(always ends up FALSE)
    # SEE ( Andre Experiments ) # SEE ERROR remDr$value$message # SEE NATIVE EVENTS remDr$sessionInfo$nativeEvents
    # $mouseMoveToLocation without "move" #21
    # https://github.com/ropensci/RSelenium/issues/21

    ### cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "time861wiz_time861wiz") 
    ### remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof, port = curr_port) # default 4444
    
    if(  browser == "chrome"  &&  use_the_custom_profile == FALSE) {
      
      # OVERRIDE # NO PROFILE
      remDr <- remoteDriver(browserName = "chrome", port = curr_port)
      
    } 
    
    print(paste0("PORT ", curr_port))
    
    # chrome - NEVER KILL JAVA ALONE,  always ( x_out of the browser FIRST !! OR remDr$close() FIRST )
    # if fail - retry once - chrome specific 1ST run of the day problem
    result_open <- tryCatch({ remDr$open() }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
    if(class(result_open) == "character" &&  result_open == "ERROR" ) { 
      Sys.sleep(4.0) 
      print("Begin retry of browser open fail")
      result_open <- tryCatch({ remDr$open() }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
      if(class(result_open) == "character" &&  result_open == "ERROR" ) { 
        Sys.sleep(4.0) 
        print("Begin retry of browser open fail 2")
        remDr$open()
        print("End retry of browser open fail 2")
      } 
      print("End retry of browser open fail")
    } 
    Sys.sleep(4.0)

    if(browser == "chrome") {
      # nav to chrome://settings/ and turn off images for performance reasons
      remDr <- google_chrome_set_no_images(remDr = remDr)
    }
    
    print(paste0("PORT ", curr_port))
    print("(tried) opened browser home page")
    
    result = tryCatch({ remDr$navigate("https://www.okcupid.com/logout") }, warning = function(w) {}, error = function(e) {}, finally = {})
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) 
    
    print("(tried) logout from OKCupid")
    
    # Sys.sleep(1 + 5* runif(1, min = 0, max = 1)) # 6 to 11 seconds wait
    
    remDr$navigate("https://www.okcupid.com/login")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    print("navigated to okcupid")
    
    webElem1 <- remDr$findElement("css selector", "#login_username")
    webElem1$sendKeysToElement(list(site_login))
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2 <- remDr$findElement("css selector", "#login_password")
    webElem2$sendKeysToElement(list(site_password))
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2$sendKeysToElement(list(key="enter"))
    Sys.sleep(4 + 2 * runif(1, min = 0, max = 1)) # 5 to 10 seconds wait
    
    print("logged into okcupid")
    
    c(
        "I'm not sure I'm feeling you yet!"
      , "OK, we're breaking up now!"
      , "I'm ex-boyfriend material!"
      , "Question about what you said..."
      , "Ouch! Did you really just say that?"
      , "You have mail!!!"
      , "My elbows hurt :("
      , "You'll never believe what just happened..."
      , "Your profile made me laugh, here's why..."
      , "Here's looking at you kid!"
      , "You talkin' to me?"
      , "Louis, I think that this is the beginning of a beautiful friendship."
      , "We rob banks!"
      , "Well, nobody's perfect!"
      , "You had me at 'Hello!'"
      , "Say 'Hello!' to my little friend."
      , "Of all the gin joints in all the towns in the world, she walks into mine."
      , "What we've got here is a failure to communicate."
      , "Toto, I've got a feeling we are not in Kansas anymore :("
        # ANDRE
      , "Jumping Jack Flash, what a Gasp!"
      , "A lawyer and a priest walked into a bar"
    ) -> message_vector
    
    # NOTE: DOES NOT YET ESCAPE OUT TICK MARKS('), SO DO NOT SEND OUT A TICK MARK(')
     
    # OLD   
    # c("Hi") -> message_greet_matchname_vector
    # NEW ( expect the matchname to come first ) 
    # USED WEEK OF FRI JUNE 12: c(", how is it going?") -> message_greet_matchname_vector
    # USED WEEK OF FRI JUNE 19: c(", hello :)") -> message_greet_matchname_vector  
    # , howdy :)  # SAT - JUNE 20
    # ", Good Tuesday evening. How are you?" - JULY 14 - caught - cajunfaith (NEW PERSON)
    # c(", hi and good evening to you this fine Thursday. How are you?") -> message_greet_matchname_vector # JULY 30
    
    # c(", good day!  This is a joyful Tuesday.  I just had a surprise.  So, how are you today?") -> message_greet_matchname_vector # TUES AUG 6
    # c(", good Tuesday to you! Are you feeling sparkly happy this evening?")  # THE AUG 8
    
    # c(", fabulous Thursday! How are you today?") -> message_greet_matchname_vector #THu AUG 12

    # from function call actual argument
    todays_message -> message_greet_matchname_vector
    
    # NOTE: DOES NOT YET ESCAPE OUT TICK MARKS('), SO DO NOT SEND OUT A TICK MARK(')
    
                                                       # NOTE: THIS MAY BE VOLITILE ( AT FIRST WAS [6]?)
    message_textarea_begin <- "return document.getElementsByTagName(\"textarea\")[5].value = \""
    message_textarea_end   <- "\";"
    
    # ( REMOVE AFTER DEBUGGING )
    # SECOND_TOUCH_ERROR
    # PER 'ALL AGES' ( SEEMS TO BE HAPPENING? AT THE END OF AN AGE_TOUCH_LIST ) 
    already_touched_alink <- c()
    
    
    print("Of THIS progrem, the user hand written call follows.")
    print(looper_typed_in_call) # language

    program_run_started <- Sys.time()
    print(paste0("Program run Starting at: ",program_run_started))
    
    # MAGIC NUMBER 
    
    # agerange <-      18:49      #  30:31  # 50:49   c(25:18,50:31) "25:18,50:31" # LEFT_OFF 29 _diamonds_ "message box full"
    # agerange_str <- "18:49"     # "30:31" # 50:49    
    
    agerange_str <- age_range_str
    agerange     <- eval(parse(text=agerange_str))
    
    for(agecurr in agerange) { # testing only 31 and 30 # 31:30   
      
      print(Sys.time())
      print(paste0("PORT ", curr_port))
     
      print(paste0("beginning age ",agecurr))
      
      # OLD - GET URLS - NO LONGER WORK 
      
      # 18 TO 50: CURRENLY ONLINE_NOW BY MATCH% 
      # SUNDAY: 2:18 P.M. 47 * 3 = 141 entries 
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,3600&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # CURRENTLY ONLINE BY MATCH% AND THIN ( $$ PAID FOR ) 11 entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,3600&filter5=30,4&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # CURRENTLY ONLINE BY MATCH% AND SKINNY ( $$ PAID FOR ) 3 entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,3600&filter5=30,16&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # CURRENTLY ONLINE BY MATCH% AND FIT ( $$ PAID FOR ) 18 entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,3600&filter5=30,64&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # CURRENTLY ONLINE BY MATCH% AND ATHLETIC ( $$ PAID FOR ) 18 entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,3600&filter5=30,128&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      ##
      
      # 18 TO 50: ONLINE_IN_THE_LAST_WEEK BY MATCH%
      # SUNDAY: 2:30 P.M. ~ approx 51 * 50 = 2500
      
      # ONLINE_IN_THE_LAST_WEEK BY MATCH% AND THIN ( $$ PAID FOR ) 124 entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,604800&filter5=30,4&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # ONLINE_IN_THE_LAST_WEEK BY MATCH% AND SKINNY ( $$ PAID FOR ) 40  entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,604800&filter5=30,16&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # ONLINE_IN_THE_LAST_WEEK BY MATCH% AND FIT ( $$ PAID FOR ) 117  entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,604800&filter5=30,64&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # ONLINE_IN_THE_LAST_WEEK BY MATCH% AND ATHLETIC ( $$ PAID FOR ) 92 entries
      # https://www.okcupid.com/match?filter1=0,34&filter2=2,18,50&filter3=3,50&filter4=5,604800&filter5=30,128&filter6=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=18
      
      # IF I SEND (A CUSTOM *MESSAGE* NOT_GOOD_MATCH ) SOMETHING OUT: 1/38 SEEM TO BE MOTIVATED
      
      # NEW COD - just navigat to the page
      navigate_target <- paste0("http://www.okcupid.com/match")
      remDr$navigate(navigate_target)
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
      # OLD
      if(action == "message_greet_matchname" && online_when == "online_now") {
      
        # navigate_target <- paste0("https://www.okcupid.com/match?filter1=0,34&filter2=2,",agecurr,",",agecurr,"&filter3=3,50&filter4=5,3600&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=500")
        
        # L - last online ( NEW CODE )
        
        webElemSBL <- remDr$findElement("css selector", "span.filter-last_login a")
        webElemSBL$highlightElement() 
        webElemSBL$clickElement()
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
        # NOW 0 ( WORKS ) # DAY 1 ( WORKS )  # WEEK 2 ( WORKS )  *** 0 NOW ***
        webElemSBLWHEN <- remDr$findElement("css selector", "span.filter-last_login span[data-index='0'] span") 
        webElemSBLWHEN$highlightElement() 
        webElemSBLWHEN$clickElement() 
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
      } else { # default: visit everyone online within the last week

        # navigate_target <- paste0("http://www.okcupid.com/match?filter1=0,34&filter2=2,",agecurr,",",agecurr,"&filter3=3,50&filter4=5,604800&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=500") 

        # ALMOST PURE - CODE COPY FROM ABOVE
        
        # L - last online ( NEW CODE )
        
        webElemSBL <- remDr$findElement("css selector", "span.filter-last_login a")
        webElemSBL$highlightElement() 
        webElemSBL$clickElement()
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
        # NOW 0 ( WORKS ) # DAY 1 ( WORKS )  # WEEK 2 ( WORKS )  *** 2 WEEK ***
        webElemSBLWHEN <- remDr$findElement("css selector", "span.filter-last_login span[data-index='2'] span") 
        webElemSBLWHEN$highlightElement() 
        webElemSBLWHEN$clickElement() 
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
        
      }    
      
      # A - age ( NEW CODE ) 
      
      webElemSB <- remDr$findElement("css selector", "span.filter-age > a")
      webElemSB$highlightElement() 
      webElemSB$clickElement()
      Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
      
      # BACKSPACE "\uE003" ( could have used .clear()? ) 
      
      webElemSBA <- remDr$findElement("css selector", "input[name=minimum_age]")
      webElemSBA$highlightElement() 
      
      # right arrows and backspaces
      webElemSBA$sendKeysToElement(list("\uE014","\uE014","\uE003","\uE003"))
      webElemSBA$sendKeysToElement(list(as.character(agecurr)))
      Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
      
      webElemSBA2 <- remDr$findElement("css selector", "input[name=maximum_age]")
      webElemSBA2$highlightElement()

      # right arrows and backspaces
      webElemSBA2$sendKeysToElement(list("\uE014","\uE014","\uE003","\uE003"))
      webElemSBA2$sendKeysToElement(list(as.character(agecurr), key = "enter")) # enter - executes the search
      Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
      
      # OLD 
      # remDr$navigate(navigate_target)
      # Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
      # LOOP ( check to see if I am at the END of the PAGE? ) ( no more information to be dynamically loaded )
      
      ## DOES WORK  
      
      # begin on the top of the page
      
      print(paste0("beginning scroll down to the bottom of the page of : ",agecurr, " of age ", agerange_str))
      
      # OLD CODE
      # webElemSB <- remDr$findElement("css selector", "#submit_button") # THE 'SEARCH' button of 'SEARCH/CLEAR' 
      # remDr$mouseMoveToLocation(webElement = webElemSB)
      
      # NEW(ADJUSTED) - begin collectin links as I scroll down 
      # ( THIS MAY BE SLOW WHEN DOING - ONLINE WITHIN THE LAST WEEK)
      
      # get the distinct user names found in the HTML  
      
      alinkslength <- remDr$executeScript("return document.getElementsByTagName('a').length;")[[1]]
      Sys.sleep(0.01)
      
      print(paste0("begin collecting all A elements of THIS SEGMENT of the page of : ",agecurr, " of age ", agerange_str))
      
      apagearefs <- c()
      
      # SLOW VERSION
#       if ( alinkslength > 0 ) { 
#         for(alinkcurr in 0:(alinkslength -1)) {
#           apagearefs <- c(apagearefs,remDr$executeScript(paste0("return document.getElementsByTagName('a')[",alinkcurr,"].href;"))[[1]])
#           Sys.sleep(0.001)
#         }
#       }
#       # FAST VERSION OF ABOVE ( IF WORKS, THEN REMOVE THE SLOW VERSION ABOVE )
      # s = "x".repeat(Math.pow(2, 23)) + "y"; s.length;
      # http://bytes.com/topic/javascript/answers/92088-max-allowed-length-javascript-string
      apagearefs_superstring <- ""
      if ( alinkslength > 0 ) { 
        getelmentsbytagname_js_command <- paste0("return ", paste("document.getElementsByTagName('a')[",0:(alinkslength -1),"].href", sep = "", collapse =" + ' ' + "),";" )
        apagearefs_superstring <-remDr$executeScript(getelmentsbytagname_js_command)[[1]]
      }
      if(apagearefs_superstring != "") {
        apagearefs <- str_split(apagearefs_superstring,"[ ]")[[1]]
      }
      
      
      print(paste0("end collecting all A elements of THIS SEGMENT of the page of : ",agecurr, " of age ", agerange_str))
      
      # removing 'cf event' - these are not 'the name' and not 'the url' so thes do not have 'use' to me
      # un-removal MAY be causing the last person in an age to be 'double contacted'?
      apagearefs <- str_replace(apagearefs,"[?]cf=event","")
      # AUG 25 - MORE CLEANUP CODE
      apagearefs <- str_replace(apagearefs,"[?]cf=regular","")

      # unique
      apagearefsu   <- unique(apagearefs)
      
      # NEW - UNIQUE TOTAL
      apagearefsu_total <- apagearefsu
      
      bookmarkhere <- 1
      
      # Am I at the end of the page?
      window.innerHeight          <- remDr$executeScript("return window.innerHeight")[[1]]
      window.scrollY              <- remDr$executeScript("return window.scrollY")[[1]]
      document.body.offsetHeight  <- remDr$executeScript("return document.body.offsetHeight")[[1]]
      
      # if not at the end of the page keep scrolling until I get there
      while( !((window.innerHeight + window.scrollY) >= document.body.offsetHeight) ) {
        
        webElemSB$sendKeysToElement(list("\uE010")) # AGGRESSIVE PAGE DOWN
        Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
        
        # NEW(ADJUSTED) - continue collectin links as I scroll down 
        
        # get the distinct user names found in the HTML  
        
        alinkslength <- remDr$executeScript("return document.getElementsByTagName('a').length;")[[1]]
        Sys.sleep(0.01)
        
        print(paste0("begin collecting all A elements of THIS SEGMENT of the page of : ",agecurr, " of age ", agerange_str))
        
        apagearefs <- c()

        # SLOW VERSION
#         if ( alinkslength > 0 ) { 
#           for(alinkcurr in 0:(alinkslength -1)) {
#             apagearefs <- c(apagearefs,remDr$executeScript(paste0("return document.getElementsByTagName('a')[",alinkcurr,"].href;"))[[1]])
#             Sys.sleep(0.001)
#           }
#         }
        # FAST VERSION OF ABOVE ( IF WORKS, THEN REMOVE THE SLOW VERSION ABOVE )
        # s = "x".repeat(Math.pow(2, 23)) + "y"; s.length;
        # http://bytes.com/topic/javascript/answers/92088-max-allowed-length-javascript-string
        apagearefs_superstring <- ""
        if ( alinkslength > 0 ) { 
           getelmentsbytagname_js_command <- paste0("return ", paste("document.getElementsByTagName('a')[",0:(alinkslength -1),"].href", sep = "", collapse =" + ' ' + "),";" )
           apagearefs_superstring <-remDr$executeScript(getelmentsbytagname_js_command)[[1]]
        }
        if(apagearefs_superstring != "") {
          apagearefs <- str_split(apagearefs_superstring,"[ ]")[[1]]
        }

        print(paste0("end collecting all A elements of THIS SEGMENT of the page of : ",agecurr, " of age ", agerange_str))
        
        # removing 'cf event' - these are not 'the name' and not 'the url' so thes do not have 'use' to me
        # un-removal MAY be causing the last person in an age to be 'double contacted'?
        apagearefs <- str_replace(apagearefs,"[?]cf=event","")
        # AUG 25 - MORE CLEANUP CODE
        apagearefs <- str_replace(apagearefs,"[?]cf=regular","")

        # unique
        apagearefsu   <- unique(apagearefs)
        
        # NEW - UNIQUE TOTAL
        apagearefsu_total <- unique(c(apagearefsu_total,apagearefsu))
        
        # information needed for the next loop
        
        window.innerHeight          <- remDr$executeScript("return window.innerHeight")[[1]]
        window.scrollY              <- remDr$executeScript("return window.scrollY")[[1]]
        document.body.offsetHeight  <- remDr$executeScript("return document.body.offsetHeight")[[1]]
        
      }
      
      print(paste0("now at bottom of the page of : ",agecurr, " of age ", agerange_str))
      
      # now at the bottom of the page, 
      
      # NEW - SO THE REST OF THE 'find profile name' CODE WORKS 
      # and I DO NOT HAVE to CHANGE THE VARIABLE NAME
      apagearefsu <- apagearefsu_total
      
      # OF ALL LINKS 'ACCUMULATED WHILE SCROLLING DOWN' get the profiles
      
      # profiles
      # OLD
      # apagearefsup  <- apagearefsu[str_detect(apagearefsu,"^.*profile")]
      # NEW - INCLUDE THE SLASH - ONE /profile EXISTS
      apagearefsup  <- apagearefsu[str_detect(apagearefsu,"^.*profile/")]  

      # regulars
      # OLD
      # apagearefsupr <- apagearefsup[str_detect(apagearefsup,"[?]cf=regular$")] # SOME WILL HAVE A SECOND LINK WITH ENDING: ?cf=recently_visited
      # NEW - to filter by 'regular' is not longer applicable
      apagearefsupr <- apagearefsup
      
      # I have not seen these(after the filters are done), but just in case crept through
      
      # apagearefsupr <- apagearefsupr[!grepl("CALLGIRL",apagearefsupr,ignore.case=TRUE)]
      # apagearefsupr <- apagearefsupr[!grepl("ROBOT",apagearefsupr,ignore.case=TRUE)]
      
      # (TEMPORARILY) REMOVE send_message TEST CANDIDATE
      # apagearefsupr <- apagearefsupr[!grepl("redbeanredbean",apagearefsupr)]
      
      # previous/a_person people that I prefer not to 're-contact of any kind'
      # NOTE: I can not elmininate someone with part of the name 'https://www.okcupid.com/profile/USERNAME?cf=regular"'
      
      special    <- c("robot")
      
                           # DIAG         # DATE
      some_curr_dialog <- c("woqueen1225","Kat0o","southernkitsune","Scottie_Lynn","gangsta_grll","msblue5159")
      
      # Pleasant response and good person
             # NEW            # SOME DIALOG
      rec9 <- c("ImSooUnique","iwillteachyouhow","howlokitty")
      
      # Pleasant response
      rec8 <- c("howlokitty")
      
      # actually they just visited me
      rec7 <- c("kittycatstevens","sunburnqueenie","Menina_Bella","sophiahelen1","clarelynew","woqueen1225","CBD34","umbria24","CaliGirlinNOLA")
      
      rec6 <- c("NOLASpringtime","Maddy_M_C","blptqt_","afaternoon726","KissKissUsagi","Scottie_Lynn","kathattack05","geminileebaby","twa-corbies")
      rec5 <- c("breezybaby2710","gangsta_grrl","dontcrycupid","courtneyesl","cna2hair","sweet_gal67","islandplaya")
      rec4 <- c("NOLApink","Kat0o","msspecial14","msblue5159","Kira24K","ndcooper85","dezy703","kittygirrll","MLR15")
      rec3 <- c("VonKathryn","justina_4u2nv","marciauptop","Cindilou4","OoshaBoom","MsLindsay1983","suny1974")
      rec2 <- c("Missaaronharry","belledenola","FaultyVictory","BrittanyGamer87","browneyegirl8383","hphphp96")
      rec1 <- c("im9124", "Stephameows","kthib23","solangelinoq","SMARTi8984","Alice")
      
      rec_all <-c(rec9,rec8,rec7,rec6,rec5,rec4,rec3,rec2,rec1)
      
      lik3 <- c("courtneyesl","autumnrose1991","sourpatchcam","sparkly_cakepop","mslblue5159")
      lik2 <- c("Adizgeguzman","BrittanieRenee","FaultyVictory","jenna3312","ArgentAura")
      lik1 <- c("Flyme2themoon","gangsta_grrl","charmanderx","kimtschelie","NOLApink","Kat0o")
      
      lik_all <- c(lik3,lik2,lik1)
      
      # all_all <- c(lik_all,rec_all,some_curr_dialog) 
      
      # MANUAL OVERRIDE    # OPEN MARRIAGE JUST WIERD
      all_all <- c()
      all_all <- c(all_all,"Kat0o","cajunfaith") # prev dates
      all_all <- c(all_all,"southernkitsune")   # hot but no car
      all_all <- c(all_all,"smartsassysweet1")   # wierd swinger
      all_all <- c(all_all,"Alpha0227") # just weird
      all_all <- c(all_all,"Ms_MandaJ") # THE AUG 6 - sent back weak repsonse: YOUNG BL GIRL: I HAVE TO THINK IF I WANT TO ASK HER OUT
      all_all <- c(all_all,"gangsta_grrl","CriuseChick","catsyoulater") # some recent dialog
    # all_all <- c(all_all,"5000kwatts")  # far way but too hot to miss
      all_all <- c(all_all,"jennh379") # # OVER THE HILL WOMAN - AVE ( BUT ASKED OFFLINE IF I MAY MEET HER IN PERSON  (TH AUG 6) ) New Orleans
      all_all <- c(all_all,"fireunleashed","Carpinteria01") # OVER THE HILL WOMAN - BUT HOT ( I GAVE A CUSTOM REPLY )
      all_all <- c(all_all,"Bozzo327") # from Sunday # You should sms my cell at 5045151708 # far away but hot
    # all_all <- c(all_all,"nataleebabinn") # I sent out of the blue ( she is too hot )
      all_all <- c(all_all,"AniLevee") # Tells me that I am too old for her

      all_all <- c(all_all,"lilbird987","xoxosunshinexoxo","PoopySoupy","pizzaforbrkfst") # she tells me directly that she is not interested

      all_all <- c(all_all,"LadyTSydney") # direct to me - NO

    # all_all <- c(all_all,"kaykay1279") # online today - not responding

    # all_all <- c(all_all,"jtrybulski228") # some diag ONLINE NOW -26 MANDLEVILL AND HOT

    # all_all <- c(all_all,"Carpinteria01") # TUESDAY - asked her FOR A DATE - ... getting together with her x
    #                                       NO LONGER EXISTS

    # all_all <- c(all_all,"anitkiln") # PLEASE FOLLOW UP ( NO LONGER EXISTS )
      all_all <- c(all_all,"cmm303")   # PLEASE FOLLOW UP ( 33 AND BOILING HOT ) ( ASKED OUT: TH 28 - WAITING FOR A RESPONSE )

      # 28 TUESDAY - ALREADY SENT TODAYS RESPONSE - SENT RESPONSE - HOT ( SOME DIALOG) - LATER -ANS BACK TUES EVEN *** COME BACK
      all_all <- c(all_all, "Sarahnaden")  # HOT - 28 - SOME DIALOG
      all_all <- c(all_all, "maria3991")  # SHE - ASKED ME OUT ( FOLLOW UP ON THIS ONE )

      # NOTE okcupid logic: a msg INCLUDES a vst
      #  OKCUPID IDEA: turn anonymous browsing ON WHILE sending messages $$ A-list
      
      do_not_vst <- c()
      if(not_to_vst == "NONE") {     # default
        do_not_vst <- c(special,"Kat0o","cajunfaith") # previous dates
      }
      
     if(not_to_vst == "SOME") {     # default
       do_not_vst <- c(special,"Kat0o","cajunfaith") # previous dates
     }

      # exclusive choices
      if(not_to_msg == "NONE") {     # default
        do_not_vst_msg <- c(do_not_vst)
      }
      if(not_to_msg == "all_all") {
        do_not_vst_msg <- c(do_not_vst,all_all)
      }  
      
      # ACTUALLY do not vst/msg
      for(match_var in c(
        do_not_vst_msg 
      )) { apagearefsupr <- apagearefsupr[!grepl(match_var,apagearefsupr,ignore.case=TRUE)]  } 
      
      # loop and visit each name - from bottom(rev) to top ( testing ) 
      # testing - visit from the BOTTOM going UP 
      
      print(paste0("begin visiting each profile of the page of : ",agecurr, " of age ", agerange_str))
      
      # rev(apagearefsupr)[1:2]               BOTTOM OF PAGE:  testing: reverse and first 2 names ( testing )
      # letters[1:(length(letters) %/% 2)]    TOP    OF PAGE:  50% of the unique links GOING DOWN
      
      apagearefsupr_total       <-  apagearefsupr
      apagearefsupr_total_count <- length(apagearefsupr)
      print(paste0("Total possible profiles: ",apagearefsupr_total_count))    
            
      if(apagearefsupr_total_count == 0) {
        print("since Total possible profiles: 0, then SKIP AHEAD to next LOOP")
        next
      }
      
      # choose e.g. visit everyone
      apagearefsupr_reduced <- apagearefsupr[1:(length(apagearefsupr) %/% 1)]
      
      # choose e.g. visit only the top half
      # apagearefsupr_reduced <- apagearefsupr[1:(length(apagearefsupr) %/% 2)]
      
      apagearefsupr_reduced_count <- length(apagearefsupr_reduced)
      print(paste0("Reduced actually actioning  profiles: ",apagearefsupr_reduced_count))
      
      # get the name out of the url

      # MOVED UP ( to a better place ) - probably? too late here
      # NEW - slight cleanup SOME have THESE FLAGS
      # apagearefsupr_reduced <- str_replace(apagearefsupr_reduced,"[?]cf=event","")

      begin_matchnames_str_locations <- (str_locate(apagearefsupr_reduced,"profile/") + 1)[,2,drop=FALSE]
      
      # OLD
      # end_matchnames_str_locations   <- (str_locate(apagearefsupr_reduced,"[?]"     ) - 1)[,1,drop=FALSE]
      # NEW - since no more ?regular ...  just get the location of the end of the string
      end_matchnames_str_locations   <- (str_locate(apagearefsupr_reduced,"$"     ) - 1)[,1,drop=FALSE]
      
      # NOTE: (str_locate  finds 'first occurance"
      # could possible break if  "?" is found in a strange spot
      
      # eliminate matchnames that I would not visit - no matter how old they are
      # TO_DO_THIS_WEEKEND [ ]
      
      # hadly S-logic
      matchnames <- str_sub(apagearefsupr_reduced, start = cbind(begin_matchnames_str_locations, end_matchnames_str_locations))
      
      print(paste0("Will (try) to visit these links of age: ", agecurr))
      print(as.data.frame(sort(apagearefsupr_reduced)))

      action_ref_counter <- 0                  # NOT TEST:     apagearefsupr_reduced
      for(alink in  apagearefsupr_reduced)   { #     TEST: rev(apagearefsupr_reduced)[1] # test: send a message to the bottom most link

        # so I know where I am
        action_ref_counter <- action_ref_counter + 1
        # TEST ( *** REMOVE AFTER TEST *** )
        # action_ref_counter <- length(apagearefsupr_reduced)
        
        print(paste0("begin visiting ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
        
        print(paste0("  current matchname: ",matchnames[action_ref_counter]))
        
        print(paste0("  action ", action_ref_counter, " of ",apagearefsupr_reduced_count ))
        
        # ( REMOVE AFTER DEBUGGING )
        # SECOND_TOUCH_ERROR
        # BAD PATCH COVERALL  
        # LATER - I SHOULD FIND HOW THAT DUPLICATE GETS IN AN (any_visit DEBUG) AND CORRECT IT
        if(alink %in% already_touched_alink) { 
          print(paste0("      ** skipping - already visited / greet_matchname (below) - THIS SHOULD NOT HAPPEN - please fix"));
          print(paste0("      ** SKIP(NEXT) and END visiting ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
          already_touched_alink <- c(already_touched_alink,alink)
          next; # alink in  apagearefsupr_reduced
        }
        
        navigate_target <- alink
        
        # LESS SAFE (OLD)
        # remDr$navigate(navigate_target)
        
        # THIS 'tcl' after WORKS in TEST but DOES NOT seem to work in PRODUCTION
        # MORE SAFE ( AFTER A 'HANG OF MORE 25 SECONDS' WILL GO TO 'backout_url')
        safe_navigate_to_new_url_success <- safe_navigate_to_new_url(new_url = navigate_target, remote_driver = remDr, backout_url = "refresh")
        print(paste0("safe navigation to new url success: ",safe_navigate_to_new_url_success[["success"]]))
        # in case some internals that I do not know of
        rmDir <- safe_navigate_to_new_url_success[["remote_driver"]]
        # identical( remDr, safe_navigate_to_new_url_success[["remote_driver"]] ) # [1] TRUE
        
        Sys.sleep(2.1 + 1 * runif(1, min = 0, max = 1)) # 2 to 4 seconds wait

        remDr$executeScript("return 0")
        
        if(isTRUE(safe_navigate_to_new_url_success[["success"]])) {
          
          # NOTE: DOES NOT YET ESCAPE OUT TICK MARKS('), SO DO NOT SEND OUT A TICK MARK(')
          dbGetQuery(con, paste0("insert into 
          aes_have_visited_list(
            id, match_source, my_matchname, her_matchname, her_age)
              values(", as.numeric(Sys.time()), ", 'okcupid_NO_metro'", ", 'time861wiz'",", '", matchnames[action_ref_counter], "', ", agecurr, ");")
          )
          # as.Date(as.POSIXct(1433110111.9225857, origin="1970-01-01"))
          # [1] "2015-05-31"
          
        }
         
        # BEGIN SEND MESSAGE AREA
        
        # A WORK INPROGRESS ( NEED TO BE ABBLE TO DETECT INNER TEXT ON A PAGE ( THORUGH JAVASCRIPT)" )
        
        if( action == "message_greet_matchname" || action == "message_random_catchphrase" ) {
          
          # A 19 year old slipped backwards in with the  18 year old ( how is that possible ? )
          # https://www.okcupid.com/profile/Divaqueen03 SHE_IS_OLD but  MSG POPUP ON THE 18 YEAR OLD PAGE
          # ( If the message popup is on the grid page It is included in the PAGED SCRAPED LINKS - different )
          
          # on her PERSONAL page, her PRINTED age 
          webElemHERPAGE_AGE <- remDr$findElement("css selector", "div#basic_info div#aso_loc p.infos span:nth-child(1)")
          # webElemHERPAGE_AGE$highlightElement()
          current_her_page_age <- as.integer(webElemHERPAGE_AGE$getElementText()[[1]])
          
          if( current_her_page_age != agecurr  ) {
            
            print(paste0("      **** WRONG AGE ON PAGE of matchname ", matchnames[action_ref_counter]," of search criteria age ", agecurr," BUT SHE HAS page age ", current_her_page_age))
                   print("      **** SKIPPING send message to HER ... next loop ...")
            next
            
          }
          
          print(paste0("begin send message ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
          
          if( action == "message_greet_matchname" ) {
            # OLD
            # current_message  <- paste0(message_greet_matchname_vector[trunc( 1 + length(message_greet_matchname_vector)*runif(1, min = 0, max = 1) - 0.001 )], " ",matchnames[action_ref_counter]) 
            # NEW expect the matchname to come first  # *** NOTE: STICK ," Ivan" at the end to add a signature  ***
            
            matchname_current <- matchnames[action_ref_counter]
            
            # If she has a REAL name, I will use it in messages
            
            matchnames_aliases <- list(
              c("aaplude92","Amanda"),    # THESE PEOPLE TOLD ME THEIR NAMES OR IT WAS I THEIR PROFILE
              c("jsbutterfly","Jocelyn"),
              c("breezybaby2710","Alexa"),
              c("MOFlynn37","Michelle"),
              c("LetsDoThis46","Tracey"),
              c("JaNaeMarie37","JM"),
              c("nonnon333","Maggie"),
              c("usernametaken985","Tonia"), # some weak dialog # I care not to respond
              c("courtneyesl","Courtney"),
              c("loveecovee","Angela"),
              c("lindsayp13","Lindsay") # BUT *SHE* IS A BITCH
              
            )
            
            matchnames_aliases_db <- as.data.frame(t(data.frame(matchnames_aliases)), stringsAsFactors = FALSE)
            
            if(any(str_detect(matchname_current,matchnames_aliases_db[,1]))) {
              # as an alias  # find the alias
              # NOTE: could fail if TWO entries are found ( really should have tested more)
              matchname_to_message <- matchnames_aliases_db["V2"][matchnames_aliases_db["V1"] == matchname_current]
            } else {
              matchname_to_message <- matchname_current
            }
            print(paste0("Messaging her real name instead: ", matchname_to_message))
            
            current_message  <- paste0(matchname_to_message,message_greet_matchname_vector[trunc( 1 + length(message_greet_matchname_vector)*runif(1, min = 0, max = 1) - 0.001 )])            
            # current_message  <- paste0(matchnames[action_ref_counter],message_greet_matchname_vector[trunc( 1 + length(message_greet_matchname_vector)*runif(1, min = 0, max = 1) - 0.001 )])
            
          } 
        
          if( action == "message_random_catchphrase" ) { # NOTE: UN-'TESTED IN PROD - BUT SHOULD WORK'
            current_message  <- message_vector[trunc( 1 + length(message_vector)*runif(1, min = 0, max = 1) - 0.001 )] 
          } 
          
          # send message button  
          
          webElemSMB <- remDr$findElement("css selector", "#footer_send_btn")
          webElemSMB$highlightElement() # THAT WORKED
          remDr$mouseMoveToLocation(webElement = webElemSMB) 
          webElemSMB$sendKeysToElement(list(key = "enter")) 
          
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))
          
          # type characters in textarea
          
          # *DETECT HERE*
          # IF THE MESSAGE BOX IS NOT IN THE FOREGROUND "BECAUSE OF A MESSAGE"
          # SHOULD *DETECT HERE* AND NOT BOTHER TO EXECUTE THE REMAINDER OF THE CODE
          
          # MOST RELIABLE WAY TO DO IT.  NOTE: querySelectorAll also WORKS
          message_textarea <-                               "document.querySelectorAll('textarea')[5].value"
          message_textarea_detect_exists <- "try{ retvalue = document.querySelectorAll('textarea')[5].value; return 0 } catch(err) { return -1 };"
          message_textarea_detect_exists_result <- remDr$executeScript(message_textarea_detect_exists)[[1]]

          if(message_textarea_detect_exists_result == -1) {
            
            print("after pressing MESSAGE . . . could not be found on the page")
            print(message_textarea)
            
          } else {

            # print("after pressing MESSAGE . . . is on the page") 

            ##  COMMENTED OUT 'WRITING A MESSAGE IN THE TEXT AREA' ( DEBUGGING )
            remDr$executeScript(paste0(message_textarea_begin,current_message,message_textarea_end))[[1]]
            writeLines(paste0(message_textarea_begin,current_message,message_textarea_end))
            
            # code changed somewhere 6 --> 5  
            # return document.getElementsByTagName("textarea")[6].value = "Hi redbeanredbean"; 
            
            Sys.sleep(1 + 2 * runif(1, min = 0, max = 1))
            
            # true message button
            
            # webElemTMB <- remDr$findElement("css selector", "#global_messaging_container > div > form > button")
            webElemTMB <- NULL
            # IF THE MESSAGE BOX IS NOT OPEN ( NEVER CURRENTLY ABLE TO REPEAT THE TEST )
            HER_MESSAGE_BOX_FULL_ERROR <- FALSE
            result <- tryCatch({ webElemTMB <- remDr$findElement("css selector", "#global_messaging_container > div > form > button") }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
            if(class(result) == "webElement") { HER_MESSAGE_BOX_FULL_ERROR <- FALSE } else { HER_MESSAGE_BOX_FULL_ERROR <- TRUE }
            
            # Error: Summary: NoSuchElement
            #        Detail: An element could not be located on the page using the given search parameters.
            #        class: org.openqa.selenium.NoSuchElementException 
            
            if(isTRUE(safe_navigate_to_new_url_success[["success"]]) && !HER_MESSAGE_BOX_FULL_ERROR) {
              
              # continue: true message button
              
              remDr$mouseMoveToLocation(webElement = webElemTMB) 
              webElemTMB$highlightElement()
              webElemTMB$sendKeysToElement(list(key = "enter"))
              
              # I enter a message, then JUST AFTER I  press the "enter" key
              #
              # They can't get messages until they delete some.
              # <div id="windowshade" class="show"><div id="send_to_full_promo" class="modal aligncenter fixed default_type ui-draggable show" style="display: block; margin-left: -270px;">    <div class="title_container"> <h2 class="title">They've reached their message limit</h2> </div>   <div class="desc"> <p> They can't get messages until they delete some. <br> If you're not above a little bribery, we'll let it slide. </p> </div>  <div class="content empty">  </div>  <div class="drag_area"> <div class="top"></div> <div class="left"></div> <div class="right"></div> <div class="bottom"></div> </div> <a class="close" href="javascript:void(0)" onclick="Modal.close('send_to_full_promo')"> <span class="icon i-close"></span> </a>  <div class="buttons"> <ul>  <li> <button onclick="SendToFullPromo.go()" class="flatbutton blue">
              #       		Message them for $1
              #				</button> </li>  </ul> </div>  </div></div>
              
              # AFTER I PRESS THE 'X' ( to close out of the dialog )
              # <div id="windowshade" class="">
              
              # WHEN EXACTLY ( SEEMS TO BE TIME! DEPEND? WITHIN 10 MINUTES? )
              # You can't send the same message twice. Be more original!
              # <span class="okform-feedback message" style="height: 30px;">You can't send the same message twice. Be more original!</span>
              
              # "Hi X again"
              # If successful
              # <span class="okform-feedback message empty" style="height: 0px;"></span>
              
              ## BOX STAYS UP - AND MESSAGE SHOWS SENT 
              Sys.sleep(2 + 2* runif(1, min = 0, max = 1))
              
              # TO_DO [ ] ... DETECT AND HANDLE ( FUTURE - DETECT ) 
              # You can't send the same message twice. Be more original!
              # <span class="okform-feedback message" style="height: 30px;">You can't send the same message twice. Be more original!</span>
              
              print(paste0("end message sent to ", matchnames[action_ref_counter], " the message : ", current_message))
              print(paste0("end send message ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
              
              # dangerously assume - if I reached the page, sending messages are successfull
              
              # NOTE: DOES NOT YET ESCAPE OUT TICK MARKS('), SO DO NOT SEND OUT A TICK MARK(')
              dbGetQuery(con, paste0("insert into 
              aes_have_sent_message_list(
              id, match_source, my_matchname, her_matchname, her_age, sent_message)
                values(", as.numeric(Sys.time()), ", 'okcupid_NO_metro'", ", 'time861wiz'",", '", matchnames[action_ref_counter], "', ", agecurr,", '", current_message, "');")
              )
              # as.Date(as.POSIXct(1433110111.9225857, origin="1970-01-01"))
              # [1] "2015-05-31"
              
            
            } 
          
          } 
          
        }
        
        # END SEND MESSAGE AREA

        #### BEGIN "LIKE SOMEONE" "BOOKMARK AREA"  ###
        #
        # http://www.okcupid.com/profile/cheesexmonger?cf=regular
        
        # TO LIKE SOMEONE
        # <button name="like" id="rate_user_profile" data-tuid="5265647272868517021" class="binary_rating_button flatbutton silver like"> <i class="icon i-star"></i> <span class="rating_like">Like</span> <span class="rating_liked">Liked</span> </button>
        
        # TO UNLIKE SOMEONE
        # <button name="like" id="rate_user_profile" data-tuid="5265647272868517021" class="binary_rating_button flatbutton silver like liked"> <i class="icon i-star"></i> <span class="rating_like">Like</span> <span class="rating_liked">Liked</span> </button>
        
        # NOTE: keyboard RETURN does not work: only workable by javascript click ( IF POSSIBLE )
        
        # ADD TO BOOKMARKS ( 2 PART DEAL : MORE COMPLICATED )
        
        # OPEN the MENU ( ENTER KEY NOT WORK )
        # <div class="trigger_action_options_wrapper"> <a href="#" id="trigger_action_options"> <span class="icon i-ellipsis-h"></span> </a> </div>
        # WHEN CLOSED BELOW (class="open") WILL NOT BE EXISTING
        
        # BOOKMARK ( ENTER KEY NOT WORK )
        # <ul id="more_options_menu" class="open"> <li class=""> <a href="#" id="save_unsave" class="">Bookmark</a> </li> <li class=""> <a href="#" id="hide_user" class=""> Hide </a> </li>  <li class=""> <a href="#" id="flag_btn"> Report </a> </li>  </ul>
        
        # REMOVE BOOKMARK ( ENTER KEY NOT WORK )
        # <ul id="more_options_menu" class="open"> <li class=""> <a href="#" id="save_unsave" class="bookmarked">Remove bookmark</a> </li> <li class=""> <a href="#" id="hide_user" class=""> Hide </a> </li>  <li class=""> <a href="#" id="flag_btn"> Report </a> </li>  </ul>
        #
        #### END "LIKE SOMEONE" "BOOKMARK AREA"  ###
        
        print(paste0("end visiting ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
        
        #######
        # COMMENTED OUT ( TOO MUCH TIME )
        # remDr$goBack()
        # Sys.sleep(1 + 2 * runif(1, min = 0, max = 1)) # 2 to 4 seconds wait
        #######        

        remDr$executeScript("return 0")
        
      }
      
      print(paste0("end visiting each profile of the page of : ",agecurr, " of age ", agerange_str))
      
      print(paste0("ending age ", agecurr))

      print("Of THIS progrem, the user hand written call follows.")
      print(looper_typed_in_call) # language
      
    }
    
    print(paste0("This Program run Started at: ",program_run_started))
    print(paste0("Program run Ending at: ",Sys.time()))

    bookmarkhere <- 1
    
    # manually logout of ok cupid here 
    # manually X out ( shutdown ) the browser
    
    # often I may want this to be FALSE
    if( on_exit_logoff_site == TRUE ) {
      
      remDr$navigate("http://www.okcupid.com/logout")
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
      remDr$navigate("http://www.oracle.com")
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
    }

    if( on_exit_close_browser == TRUE ) {
      
      result_close <- tryCatch({ remDr$close() }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
      if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
        Sys.sleep(4.0) 
        print("Begin retry of browser close fail")
        result_close <- tryCatch({ remDr$close() }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
        if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
          print("BROWSER STILL FAILED TO CLOSE ... SO IGNORING ... NOT CLOSING")
        }
        print("End retry of browser close fail")
      } 
      Sys.sleep(4.0)
      
    }

    bookmarkhere <- 1
    
    if( on_exit_stop_selenium_server == TRUE ) {
      
      print("begin closeServer remDr")
      # IF I really! want to stop
      # result = tryCatch({ remDr$closeServer() }, warning = function(w) {}, error = function(e) {}, finally = {})
      manageRSeleniumServer(curr_port = curr_port, doif = "if_server_not_stopped_please_stop")
      print("end closeServer remDr")
      
    }

    dbDisconnect(con)
    Sys.setenv(TZ=oldtz)

    # NOT IMPLMENTED YET
    true_attempted_send_message_count <- 1
    return(list(remDr = remDr, SentMsgAttemptedCount = true_attempted_send_message_count))

  }
  RETURN <- maininner()
  return(RETURN) 
}

# BEGIN INSTRUCTIONS
# BEGIN INSTRUCTIONS

# Every so often
# Help-> About Google Chrome ( checking for updates )

# MAIN DEVELOPMENT NOTES ARE HERE: J:\YDrive\All_NewSeduction\All_ElectronicSpeech

# START UP PostgreSQL !!!

# rm(list=ls(),envir = .GlobalEnv)

# setwd("J:/YDrive/All_NewSeduction/All_ElectronicSpeech/RSeleniumAndBrowsers/AES1") # getwd()
# MAKE SURE THAT THE .R file in the tab(hover over) has the same dir path as 'setwd'

# ABSOLUTE PATH IS BEST
# debugSource('J:/YDrive/All_NewSeduction/All_ElectronicSpeech/RSeleniumAndBrowsers/AES1/okcupid_visit_looper_dev.R')

# NOTE: Optional, but HIGHLY recommended, for performance, Turn OFF 'view google chrome images'
# NOTE: Optional, but HIGHLY recommended, for performance, Turn OFF 'view google chrome images'

# REM: taskmgr - manually KILL off java.exe if it is running
# XOR
# "command prompt"->"right click"->"run as adminsitrator"  
# VISIT WITHIN LAST WEEK
# netstat -o -a -b  -n | find /i "listening" | find /i ":4444" 
# taskkill /F /T /PID <above_right_col_number>
# XOR
# SEND MESSAGE TO 'ONLINE NOW'
# netstat -o -a -b  -n | find /i "listening" | find /i ":4444"
# taskkill /F /T /PID <above_right_col_number>

# MANUALLY PLACE DOWN THE BREAKPOINT
#   e.g. remDr$open() # oracle.com  

# MAKE SURE - I am (IF PAID FOR) NOT browsing anonymously

# visiting
# okcupid_visit_looper_dev()
# okcupid_visit_looper_dev <- function(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE")
#
# lately ( just prev dates - SOME not visit)
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE")
# 

# messaging - not previous dates
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all")  

# END INSTRUCTIONS  
# END INSTRUCTIONS    



