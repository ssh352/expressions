

# okcupid
# DON'T FORGET TO STARTUP PostgreSQL !!


options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) 
options(digits.secs = 6)
# options(error=NULL) 

dump_and_quit <- function() {
  # Save debugging info to file last.dump.rda
  dump.frames(to.file = TRUE)
  # Quit R with error status
  q(status = 1)
}
# options(error = dump_and_quit) # NOTHING
options(error = recover) # 90 PERCENT #
# options(error = browser) # DOES NOTHING
## options(error = browser()) # ?? # BAD ???

if(Sys.getenv("RSTUDIO") == "1") {
  debugSource(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
} else {
  source(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
}

okcupid_visit_looper_dev <- function(curr_port = 4444, browser = "firefox", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), on_exit_logoff_site = TRUE, on_exit_close_browser = TRUE, on_exit_stop_selenium_server = FALSE, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", face_color = "anything", loop_forever = "no" , age_range_str_group = "all_ages_page") { 
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
      remDr <- google_chrome_set_no_images(remDr = remDr)  ### TEMP OFF/ON GRAPHICS #  
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
    
    
    
    # TOP OF PROGRAM - today_file - ok_already_messaged_today_vector
    
    today_number <- floor((as.numeric(Sys.time()) - ( 6 * 3600 ))/ ( 24 * 3600 )) # UTC down adj
    
    today_file <- paste0("ok_already_messaged_today_", today_number,".Rdata")
    
    print(paste0("ok_already_messaged_today file: ", today_file ))
    
    
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
    # JANUARY 31 2016 textarea location changed from [5] to [4]
    message_textarea_begin <- "return document.getElementsByTagName(\"textarea\")[4].value = \""
    message_textarea_end   <- "\";"
    
    # ( REMOVE AFTER DEBUGGING )
    # SECOND_TOUCH_ERROR
    # PER 'ALL AGES' ( SEEMS TO BE HAPPENING? AT THE END OF AN AGE_TOUCH_LIST ) 
    already_touched_alink <- c()
    
    
    print("Of THIS progrem, the user hand written call follows.")
    print(looper_typed_in_call) # language

    program_run_started <- Sys.time()
    print(paste0("Program run Starting at: ",program_run_started))
    
    if( loop_forever == "yes" ) { 
      
      loop_forever_vector <- 1:1000
      
    } 
    
    if( loop_forever == "no" ) {
      
      loop_forever_vector <- 1
      
    }
    
    for(loop_index in loop_forever_vector) {
      
      # BEGINNING OF OLD MAIN PROGRAM
    
      # MAGIC NUMBER 
      
      # agerange <-      18:49      #  30:31  # 50:49   c(25:18,50:31) "25:18,50:31" # LEFT_OFF 29 _diamonds_ "message box full"
      # agerange_str <- "18:49"     # "30:31" # 50:49    
      
      agerange_str <- age_range_str
      agerange     <- eval(parse(text=agerange_str))
      age_min <- head(agerange,1)
      age_max <- tail(agerange,1)
      
      if(age_range_str_group == "all_ages_page") { 
        
        agerange_orig <- agerange
        agerange <- 999
        
      }
      
      for(agecurr in agerange) { # testing only 31 and 30 # 31:30   
        
        print(Sys.time())
        print(paste0("PORT ", curr_port))
       
        print(paste0("beginning age ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
        
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
        
        try({   
          
          # MORE RIGHT SIDE BLUE BAR PROBLEMS
          Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
          webElemMAYBE_LATER <- remDr$findElement("css selector", "button.closereport" )
          webElemMAYBE_LATER$highlightElement()
          webElemMAYBE_LATER$clickElement()
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))
          
        }, silent = TRUE )
        
        
        # AS LONG AS ON ( https://www.okcupid.com/match PAGE ) I can do ANY_TIME
        #
        # CLEAR search "A-list" criteria
        #
        #  no error: length(webElemCLEAR_S) returns zero(0) if no elements are found
        webElemCLEAR_S <- remDr$findElements("css selector", "button.clear-tag" )
        lapply(webElemCLEAR_S, function(x){ x$highlightElement(); x$clickElement(); Sys.sleep(1.0) } )
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        # WORKS
        
        # OLD
        # REMOVED SUNDAY FEBRUARY 21ST
        # if(action == "message_greet_matchname" && online_when == "online_now") {
        # REPLACED SUNDAY FEBRUARY 21ST ( ALLOW THIS LIBERTY )
        if(                                       online_when == "online_now") {
        
          # navigate_target <- paste0("https://www.okcupid.com/match?filter1=0,34&filter2=2,",agecurr,",",agecurr,"&filter3=3,50&filter4=5,3600&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=500")
          
          # L - last online ( NEW CODE )
          
#           webElemSBL <- remDr$findElement("css selector", "span.filter-last_login a")
#           webElemSBL$highlightElement() 
#           webElemSBL$clickElement()
#           Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
#           
#           # NOW 0 ( WORKS ) # DAY 1 ( WORKS )  # WEEK 2 ( WORKS )  *** 0 NOW ***
#           # webElemSBLWHEN <- remDr$findElement("css selector", "span.filter-last_login span[data-index='0'] span") 
# 
#           # FEBURARY 10 2016
#           # SETTING THE AGE ( ONLINE NOW )
#           # SHOULD BE THIS ( BUT I AM WRONG )
#           # webElemSBLWHEN <- remDr$findElement("css selector", "div.filter-slider-ticks span.filter-slider-tick[data-index='0'] span.filter-slider-tick-label")
#           webElemSBLWHEN <- remDr$findElement("xpath", '//*[@id="match-filters"]/div[1]/span/span[11]/span/span/span[3]/div/div[1]/div/div[2]/span[1]/span')
#           webElemSBLWHEN$highlightElement() 
#           webElemSBLWHEN$clickElement() 
#           Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
#           
          
          # L - last online ( NEW CODE )
          
          # webElemSBL <- remDr$findElement("css selector", "span.filter-last_login a")
          # UPDATED FEBRURARY 10 2016
          webElemSBL <- remDr$findElement("css selector", "span.filter-wrapper.filter-last-login button.open-basic-filter")
          webElemSBL$highlightElement() 
          webElemSBL$clickElement()
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
          
          # NOW 0 ( WORKS ) # DAY 1 ( WORKS )  # WEEK 2 ( WORKS )  *** 2 WEEK ***
          # FEBURARY 10 2016
          # ONLINE WITHIN THE LAST TWO WEEKS  # last element -  1 now - 2 day - 3 week                                                        # 1 - NOW, 2 - DAY, 3 - WEEK
          webElemSBLWHEN <- remDr$findElement("xpath", '//*[@id="match-filters"]/div[1]/span/span[11]/span/span/span[3]/div/div[1]/div/div[2]/span[1]/span')          
          webElemSBLWHEN$highlightElement() 
          webElemSBLWHEN$clickElement() 
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
          
        } else { # default: visit everyone online within the last week
  
          # navigate_target <- paste0("http://www.okcupid.com/match?filter1=0,34&filter2=2,",agecurr,",",agecurr,"&filter3=3,50&filter4=5,604800&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=500") 
  
          # ALMOST PURE - CODE COPY FROM ABOVE
          
          # L - last online ( NEW CODE )
          
          # webElemSBL <- remDr$findElement("css selector", "span.filter-last_login a")
          # UPDATED FEBRURARY 10 2016
          webElemSBL <- remDr$findElement("css selector", "span.filter-wrapper.filter-last-login button.open-basic-filter")
          webElemSBL$highlightElement() 
          webElemSBL$clickElement()
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
          
          # NOW 0 ( WORKS ) # DAY 1 ( WORKS )  # WEEK 2 ( WORKS )  *** 2 WEEK ***
          # FEBURARY 10 2016
          # ONLINE WITHIN THE LAST TWO WEEKS  # last element -  1 now - 2 day - 3 week                                                        # 1 - NOW, 2 - DAY, 3 - WEEK
          webElemSBLWHEN <- remDr$findElement("xpath", '//*[@id="match-filters"]/div[1]/span/span[11]/span/span/span[3]/div/div[1]/div/div[2]/span[3]/span')          
          webElemSBLWHEN$highlightElement() 
          webElemSBLWHEN$clickElement() 
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
          
          
        }    
        
        
        # A - age ( NEW CODE ) # startupr age dialog box
        
        # UPDATED FEBRUARY 10 2016
        # webElemSB <- remDr$findElement("css selector", "span.filter-age > a")
        webElemSB <- remDr$findElement("css selector", "span.filter-wrapper.filter-age  button.open-basic-filter")
        webElemSB$highlightElement() 
        webElemSB$clickElement()
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
        # if exists '[?] Only matches into my age', then make sure, force it to be unchecked
        # PROB also COULD detect if UNCHECKED
#         if(remDr$executeScript("return document.querySelectorAll('#checkbox-age_recip-2').length;")[[1]] != 0) {
#           
#           js_result <- remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked = false;")[[1]]
#           # checkbox # RESULT ? # 0/false/unchecked 1/true/checked  bad_css/ERROR
#           if(js_result == FALSE) print("Only matches onto my age is now Garanteed UNCHECKED")
#           
#         }

        try( {

        # turn off: Only matches into my age Check Box # FEBRUARY 10
        if(remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked == true;")[[1]]) {
          # SHOULD HAVE WORKED
          # webElemRESTRICTAGE <- remDr$findElement("css selector", "span.checkbox-text")
          webElemRESTRICTAGE <- remDr$findElement("xpath",'//*[@id="match-filters"]/div[1]/span/span[7]/span/span/span[3]/div/div[1]/label/div')
          webElemRESTRICTAGE$highlightElement()
          webElemRESTRICTAGE$clickElement()
        }
        
        }, silent = TRUE)

        # BACKSPACE "\uE003" ( could have used .clear()? ) 
        
        webElemSBA <- remDr$findElement("css selector", "input[name=minimum_age]")
        webElemSBA$highlightElement() 
        
        # right arrows and backspaces
        webElemSBA$sendKeysToElement(list("\uE014","\uE014","\uE003","\uE003"))
        
        try( {
          
          # turn off: Only matches into my age Check Box # FEBRUARY 10
          if(remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked == true;")[[1]]) {
            # SHOULD HAVE WORKED
            # webElemRESTRICTAGE <- remDr$findElement("css selector", "span.checkbox-text")
            webElemRESTRICTAGE <- remDr$findElement("xpath",'//*[@id="match-filters"]/div[1]/span/span[7]/span/span/span[3]/div/div[1]/label/div')
            webElemRESTRICTAGE$highlightElement()
            webElemRESTRICTAGE$clickElement()
          }
          
        }, silent = TRUE)
        
        # enter 'min age'
        webElemSBA$sendKeysToElement(list(as.character(if(age_range_str_group == "all_ages_page") age_min else agecurr)))
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
        
        try( {
          
          # turn off: Only matches into my age Check Box # FEBRUARY 10
          if(remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked == true;")[[1]]) {
            # SHOULD HAVE WORKED
            # webElemRESTRICTAGE <- remDr$findElement("css selector", "span.checkbox-text")
            webElemRESTRICTAGE <- remDr$findElement("xpath",'//*[@id="match-filters"]/div[1]/span/span[7]/span/span/span[3]/div/div[1]/label/div')
            webElemRESTRICTAGE$highlightElement()
            webElemRESTRICTAGE$clickElement()
          }
          
        }, silent = TRUE)
        

        webElemSBA2 <- remDr$findElement("css selector", "input[name=maximum_age]")
        webElemSBA2$highlightElement()

        # right arrows and backspaces
        webElemSBA2$sendKeysToElement(list("\uE014","\uE014","\uE003","\uE003"))

        # enter 'max age'
        webElemSBA2$sendKeysToElement(list(as.character(if(age_range_str_group == "all_ages_page") age_max else agecurr))) 
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
      try( {
        
        # turn off: Only matches into my age Check Box # FEBRUARY 10
        if(remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked == true;")[[1]]) {
          # SHOULD HAVE WORKED
          # webElemRESTRICTAGE <- remDr$findElement("css selector", "span.checkbox-text")
          webElemRESTRICTAGE <- remDr$findElement("xpath",'//*[@id="match-filters"]/div[1]/span/span[7]/span/span/span[3]/div/div[1]/label/div')
          webElemRESTRICTAGE$highlightElement()
          webElemRESTRICTAGE$clickElement()
        }
        
      }, silent = TRUE)
        
        # SEE 'JUST BELOW' - ENTER AGE then PRESS enter KEY

        # 'if the small message box, is still open, this may obscure the WHITE_ONLY choice'
        # SO CLOSE IT BY THE xing out the Upper right corner X

        try( {
          
            cat(paste0("BEGIN TRY TO FIND SMALL MESSAGEBOX","\n"))
            webElemSMALLMSGBOXES <- remDr$findElements("css",'a.minimize')
            cat(paste0("BEGIN TRY MINIMIZING SMALL MESSAGEBOX","\n"))
            webElemSMALLMSGBOXES[[1]]$highlightElement()
            webElemSMALLMSGBOXES[[1]]$clickElement()
            cat(paste0("END TRY MINIMIZING SMALL MESSAGEBOX","\n"))
          
        }, silent = TRUE)

        bookmarkhere <- 1
        
        more_physical_criteria <- FALSE
        if( !( face_color == "anything" ) ) more_physical_criteria <- TRUE # ADD MORE (anything) '||' CRITERIA HERE 
        
        if( !more_physical_criteria ) {
          # just press enter now
          
          # if no more physical criteria just press the enter KEY
          webElemSBA2$sendKeysToElement(list(key = "enter")) # enter - executes the search
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
          
        } else { # more_physical_criteria == TRUE
          
          # FEBRUARY 10 
          # NOTE # HAS A TENDENCY TO RE-COME ON BY ITSELF ( WHAT IS THAT ABOUT ) 
          # UNCHECK "Only Matches my Age" CHECK BOX ( if Exists or NOT i DO NOT CARE )
          Sys.sleep(1 + 1 * runif(1, min = 0, max = 1)) 
          # TOO MUCH OF A TENDENCY OF THEM TO TRY TO RECHECK IT, WHEN I AM NOT LOOKING

          try( {
            
            # turn off: Only matches into my age Check Box # FEBRUARY 10
            if(remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked == true;")[[1]]) {
              # SHOULD HAVE WORKED
              # webElemRESTRICTAGE <- remDr$findElement("css selector", "span.checkbox-text")
              webElemRESTRICTAGE <- remDr$findElement("xpath",'//*[@id="match-filters"]/div[1]/span/span[7]/span/span/span[3]/div/div[1]/label/div')
              webElemRESTRICTAGE$highlightElement()
              webElemRESTRICTAGE$clickElement()
            }
            
          }, silent = TRUE)
          
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
          
          # open THAT 'more criteria' icon
          
          # JANUARY 31 2016
          # SIMPLER just use the NEW reliable WAY
          # MORE_CRITERI upper right corner graphic
          # webElemMORECRIT <-  remDr$findElement("css selector", ".toggle-advanced-filters.expand")
          # FEBURARY 10 CHANGE
          webElemMORECRIT <-  remDr$findElement("xpath", '//*[@id="match-filters"]/div[1]/button/span[1]')
          
          # DID WORK 10 DAYS AGON ON JANUARY 31 I HAVE NOT TESTED IT ( IT STILL COULD WORK )
          # remDr$mouseMoveToLocation(webElement = webElemMORECRIT)
          # webElemMORECRIT$sendKeysToElement(list(key = "enter"))
          
          webElemMORECRIT$highlightElement() 
          webElemMORECRIT$clickElement()
          
          
          
#           # JANUARY 31 2016
#           # Always on page weather see-able or NOT - NOT RELIABLE
#           # click on Maybe Later on Right Side Advertising BOOST Bar
#           if(remDr$executeScript("return document.querySelectorAll('button.closereport').length;")[[1]] != 0) {
#             
#             webElemCLOSEREPORT <-  remDr$findElement("css selector", "button.closereport")
#             webElemCLOSEREPORT$highlightElement()
#             webElemCLOSEREPORT$clickElement()
#             
#             # IF THAT 'does not work'
#             # THIS works   document.querySelector('button.closereport').click()
#             #   AND RETURNS javascript_undefinied
#             
#           }
          
#           # JANUARY 31 2016
#           # if exists BLUE RIGHT advertisement BAR
#           # After "Maybe Later" is clicked, if NOT ON page, it still 
#           # can be detected by querySelectorAll and return size '1' *BUT SETTING style.display = 'none';* WILL RETURN AN ERROR
#           if(remDr$executeScript("return document.querySelectorAll('div.panel-boost').length;")[[1]] != 0) {
#             
#             # set its display ( MAY OR MAY NOT MAKE A DIFFERENCE ) ... becomes BLUE but not HIDDEN
#             js_result <- remDr$executeScript("return document.querySelector('div.panel-boost').style.display = 'none';")[[1]]
#             #  returns "none"
#             if(js_result == "none") print("Boost Panel is now HIDDEN")
#             
#             # MANUALLY open the ADVANCED filters THE OTHER WAY
#             
#             # MORE_CRITERI upper right corner graphic
#             webElemMORECRIT <-  remDr$findElement("css selector", ".toggle-advanced-filters.expand")
#             webElemMORECRIT$highlightElement()
#             remDr$mouseMoveToLocation(webElement = webElemMORECRIT)
#             webElemMORECRIT$sendKeysToElement(list(key = "enter"))
#             
#           } else { # DO IT THE OLD WAY
#             
#             # MORE_CRITERI upper right corner graphic
#             webElemMORECRIT <-  remDr$findElement("css selector", ".toggle-advanced-filters.expand")
#             webElemMORECRIT$highlightElement()
#             webElemMORECRIT$clickElement()
#             
#           }
          
        
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
          # WORKS
          
          if ( face_color == "white"  ) {
            
            # Background: choose Ethnicity
            webElemBCKGRDICON <-  remDr$findElement("css selector", "button.advanced-filter-toggle.advanced-filter-toggle-background span.advanced-filter-toggle-icon")
            webElemBCKGRDICON$highlightElement()
            webElemBCKGRDICON$clickElement()
            Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
            # WORKS
            
            # White check box ( actually a DIV ) [X] White
            webElemWHITECHKBOX <-  remDr$findElement("css selector", "label[for=checkbox-ethnicity-white] div.decoration")
            webElemWHITECHKBOX$highlightElement() 
            webElemWHITECHKBOX$clickElement() 
            Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
            # WORKS
            
            # TOO MUCH OF A TENDENCY OF THEM TO TRY TO RECHECK IT, WHEN I AM NOT LOOKING
            try( { remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked = false;")[[1]] }, silent =  TRUE )
            
            # Press BIG green SEARCH button [SEARCH]
            webElemBIGREENSEARCH <-  remDr$findElement("css selector", "button.flatbutton.big.green" )
            webElemBIGREENSEARCH$highlightElement() 
            webElemBIGREENSEARCH$clickElement() 
            Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
            # WORKS
            
            # TOO MUCH OF A TENDENCY OF THEM TO TRY TO RECHECK IT, WHEN I AM NOT LOOKING
            try( { remDr$executeScript("return document.querySelector('#checkbox-age_recip-2').checked = false;")[[1]] }, silent =  TRUE )
            
            
          }
          
        }

        # MAY 3 2016  CRAZY OKCUPID URL AND IMAGES DOWNLOAD ERROR: URLS AND IMAGES ARE NOT LOADING:  
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))
        remDr$refresh()
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))
        remDr$refresh()
        Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))

        ### browser()

        #### webElemSBA2$sendKeysToElement(list(as.character(agecurr), key = "enter")) # enter - executes the search
        #### Sys.sleep(2 + 1 * runif(1, min = 0, max = 1)) 
        
        # OLD 
        # remDr$navigate(navigate_target)
        # Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
        
        # LOOP ( check to see if I am at the END of the PAGE? ) ( no more information to be dynamically loaded )
        
        ## DOES WORK  
        
        # begin on the top of the page
        
        print(paste0("beginning scroll down to the bottom of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
        
        # OLD CODE
        # webElemSB <- remDr$findElement("css selector", "#submit_button") # THE 'SEARCH' button of 'SEARCH/CLEAR' 
        # remDr$mouseMoveToLocation(webElement = webElemSB)
        
        # NEW(ADJUSTED) - begin collectin links as I scroll down 
        # ( THIS MAY BE SLOW WHEN DOING - ONLINE WITHIN THE LAST WEEK)
        
        # get the distinct user names found in the HTML  
        
        alinkslength <- remDr$executeScript("return document.getElementsByTagName('a').length;")[[1]]
        Sys.sleep(0.01)
        


        print(paste0("begin collecting all A elements of THIS SEGMENT of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
        
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
        
        # MAY 3 2016
        print("sorted uniqued 'profile' found in page As: top most area")
        print(data.frame(sort(unique( apagearefs[str_detect(apagearefs,"profile")] ))))
  
        print(paste0("end collecting all A elements of THIS SEGMENT of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
        
        # removing 'cf event' - these are not 'the name' and not 'the url' so thes do not have 'use' to me
        # un-removal MAY be causing the last person in an age to be 'double contacted'?
        apagearefs <- str_replace(apagearefs,"[?]cf=event","")
        # AUG 25 - MORE CLEANUP CODE
        # apagearefs <- str_replace(apagearefs,"[?]cf=regular","")
  
        # JANUARY 31, 2016 - KEEP the [?]cf=regular"
        # apagearefs <- str_replace(apagearefs,"[?]cf=regular","")
  
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
          
          # MAY 3 2016
          # remDr$refresh() # will cause the   webElemSB   to be removed
          ##  webElemSB$sendKeysToElement(list("\uE010")) # AGGRESSIVE PAGE DOWN
          # Detail: An element command failed because the referenced element is no longer attached to the DOM.
          # There is (hopefully) ONE to GRAB
          remDr$findElement("css selector", "a")$sendKeysToElement(list("\uE010")) # AGGRESSIVE PAGE DOWN
          
          Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
          
          # NEW(ADJUSTED) - continue collectin links as I scroll down 
          
          # get the distinct user names found in the HTML  
          
          alinkslength <- remDr$executeScript("return document.getElementsByTagName('a').length;")[[1]]
          Sys.sleep(0.01)
          
          
          print(paste0("begin collecting all A elements of THIS SEGMENT of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
          
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
          # apagearefs <- str_replace(apagearefs,"[?]cf=regular","")
  
          # JANUARY 31, 2016 - KEEP the [?]cf=regular"
          # apagearefs <- str_replace(apagearefs,"[?]cf=regular","")
  
          # unique
          apagearefsu   <- unique(apagearefs)
          
          # NEW - UNIQUE TOTAL
          apagearefsu_total <- unique(c(apagearefsu_total,apagearefsu))
          
          # information needed for the next loop
          
          window.innerHeight          <- remDr$executeScript("return window.innerHeight")[[1]]
          window.scrollY              <- remDr$executeScript("return window.scrollY")[[1]]
          document.body.offsetHeight  <- remDr$executeScript("return document.body.offsetHeight")[[1]]
          
        }
        
        print(paste0("now at bottom of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
        
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
        rec9 <- c("ImSooUnique","iwillteachyouhow")
        
        rec8 <- c()
        
        # actually they just visited me
        rec7 <- c("kittycatstevens","sunburnqueenie","Menina_Bella","sophiahelen1","clarelynew","woqueen1225","CBD34","umbria24","CaliGirlinNOLA")
        
        rec6 <- c("NOLASpringtime","Maddy_M_C","blptqt_","afaternoon726","KissKissUsagi","Scottie_Lynn","kathattack05","geminileebaby","twa-corbies")
        rec5 <- c("breezybaby2710","gangsta_grrl","dontcrycupid","cna2hair","sweet_gal67","islandplaya")
        rec4 <- c("NOLApink","Kat0o","msspecial14","msblue5159","Kira24K","ndcooper85","dezy703","kittygirrll","MLR15")
        rec3 <- c("VonKathryn","justina_4u2nv","marciauptop","Cindilou4","OoshaBoom","MsLindsay1983","suny1974")
        rec2 <- c("Missaaronharry","belledenola","FaultyVictory","BrittanyGamer87","browneyegirl8383","hphphp96")
        rec1 <- c("im9124", "Stephameows","kthib23","solangelinoq","SMARTi8984","Alice")
        
        rec_all <-c(rec9,rec8,rec7,rec6,rec5,rec4,rec3,rec2,rec1)
        
        lik3 <- c("autumnrose1991","sourpatchcam","sparkly_cakepop","mslblue5159")
        lik2 <- c("Adizgeguzman","BrittanieRenee","FaultyVictory","jenna3312","ArgentAura")
        lik1 <- c("Flyme2themoon","gangsta_grrl","charmanderx","kimtschelie","NOLApink","Kat0o")
        
        lik_all <- c(lik3,lik2,lik1)
        
        # all_all <- c(lik_all,rec_all,some_curr_dialog) 
        
        # MANUAL OVERRIDE    # OPEN MARRIAGE JUST WIERD
        all_all <- c()
  
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
  
        # NOT INTERESTED LIST
        all_all <- c(all_all,"lilbird987","xoxosunshinexoxo","PoopySoupy","pizzaforbrkfst","racheltheredhead", "caprifemme27","Vaporwave-Lenin", "theantigoneway", "mhiz_lindsey", "Lolo_nola", "Cyclon3") # she tells me directly that she is not interested
        all_all <- c(all_all,"LifeLovinLady","DerbyDevil74", "Joyceann46", "umbria24","Jen84lsu","Jen84lsu","stolendaughter") 
  
        all_all <- c(all_all,"saxybitch13") # NOT LOOKING TO MEET
  
        all_all <- c(all_all,"LadyTSydney","twinklepie6756","LeAnne717","Elladorado") # direct to me - NO
  
        all_all <- c(all_all,"boxgreen","Graysonpaige", "theantigoneway", "Iwantu2wantme65",  "lbartsy", "AlexisRae2187") # Just do not care to talk to ( anymore )
  
      # all_all <- c(all_all,"kaykay1279") # online today - not responding
  
      # all_all <- c(all_all,"jtrybulski228") # some diag ONLINE NOW -26 MANDLEVILL AND HOT
  
      # all_all <- c(all_all,"Carpinteria01") # TUESDAY - asked her FOR A DATE - ... getting together with her x
      #                                       NO LONGER EXISTS
  
        all_all <- c(all_all,"Kat0o","cajunfaith","aristarla","MOFlynn37") # PREVIOUS DATES # PREVIOUS DATES # 
  
      # all_all <- c(all_all,"anitkiln") # PLEASE FOLLOW UP ( NO LONGER EXISTS )
        all_all <- c(all_all,"cmm303")   # PLEASE FOLLOW UP ( 33 AND BOILING HOT ) ( ASKED OUT: TH 28 - WAITING FOR A RESPONSE )
  
        # 28 TUESDAY - ALREADY SENT TODAYS RESPONSE - SENT RESPONSE - HOT ( SOME DIALOG) - LATER -ANS BACK TUES EVEN *** COME BACK
      # all_all <- c(all_all, "Sarahnaden")  # HOT - 28 - SOME DIALOG - no longer responding - just asked here to re-talk - she is an A_list hidden browser - she is busy
  
       all_all <- c(all_all, "maria3991") # SHE MISSED THE DATE # SHE - ASKED ME OUT - SHE FLAKED!!!
  
       # JUST TODAY - SEP 1 - THIS EVE - I WILL MANUALLY HANDLE ( DO NOT FORGET !!)
       # all_all <- c(all_all,"saxybitch13","NOLAmy", "mallee13","mwg70115", "Sarahnaden", "Annielynn1983")
  
       # JUST TODAY, SAT SEPT 12
       # all_all <- c(all_all,"happyasadoginmud","StarryLyte")
   
       # just for today # Tuesday, September 15th
       # all_all <- c(all_all,"beckward")
  
       # OK - SOME DIALOG - MORN SEPT 23 - TUESDAY
       all_all <- c(all_all,"MeowDaCatSAY", "takemesrsly69", "illlizabeth", "chellie5514")
  
       # # OUT OF TOWN OR ONGOING DIALOG - SEPT 22,23
       all_all <- c(all_all,"chellie5514")
  
       # WEAK PERSON - SOME DIALOG - SEPT 22 23, 24
       all_all <- c(all_all,"takemesrsly69", "MeowDaCatSAY") 
  
       # JUST TODAY - MON SEPT 14
       # all_all <- c(all_all,"chunkypeaches")
  
       all_all <- c(all_all,"Musijo","amadeleinew") # NEW PERSON - SEPTEMBER 16TH / 2ND PERSON LIKED ME
  
       # WEAK PERSON - SOME DIALOG - SEPT 22 23, 24
       all_all <- c(all_all,"takemesrsly69", "MeowDaCatSAY") 
  
       # HEAVY WOMAN: GNERAL NOVEMBER DIALOG
       all_all <- c(all_all,"courtneyesl")
  
       # WEAK PERSON - SOME DIALOG - SEPT 28
       all_all <- c(all_all,"Scarletrayven") # Mandy(I did ask out)
  
       # WEAK PERSON - SOME VERY VERY WEAK DIALOG - SEPT 29, OCT 1, OCT 5, OCT 6
       all_all <- c(all_all,"layla_danielle", "different9080", "Yellolotus", "BlknBlu7", "simplycurious45", "AlyEsc", "MaddieMay_xo") 
  
       # WEAK - OCTOBER 11,12 - NEW
       all_all <- c(all_all,"p4passion101", "Yellolotus")
  
       # MEDIUM DIALOG - OCT 1 - RELATIONSHIP SEEKER
       all_all <- c(all_all, "smilingrosej")
  
      # MORN - OCT 5 - SOME CONTINUING LOUSY DIALOG
      all_all <- c(all_all, "girlnextdoor0126")
  
      # MORN - OCT 5 - TRUCK DRIVER - ALREADY HAS SOMEONE - CONTINUING DIALOG
      all_all <- c(all_all, "chellie5514")
  
      # MORN - OCT 5 - BY_SEXUAL -40 - long continuing WEAK PROFILE
      all_all <- c(all_all, "BlknBlu7")
  
      # MORN - OCT 5 13-- SOME VERY LONG TERM WEAK DIALOG
      all_all <- c(all_all, "amadeleinew","flwr34","loriana1", "Hey_MollyMolly", "selveen")
  
      # OCT 13 - SOME BETTER DIALOG
      all_all <- c(all_all,"Peacock-101","Merrou")
  
      # THE OCT 21 - JUST DATED - SHE IS MOVING TO SF
      all_all <- c(all_all, "Saranescence")
  
      # OCT 8 - 13 14 IMPLIED WE SHOULD GO OUT - SOME GOOD DIALOG
      all_all <- c(all_all, "AlyEsc","Labchick3571","jsbutterfly")  ### COME BACK LATER ###
      
      # OCT 15 - SHE LIKED ME
      ### all_all <- c(all_all, "Lafirefly") ## BEGIN RQUOTING
      
      # OCT 21 - WEAK DIALOG BUT VERY HOT
      ### all_all <- c(all_all, "wheel_watching") ### BEGIN REQUOTING
      
      # OCT 22 - NO PICTURE
      all_all <- c(all_all,"kcheezie")
      
      # OCT 28 - PREFER NOT TO TALK TO ANYMORE
      all_all <- c(all_all,"DTBlonde","NOLAnoob")
  
      # OCT 28 - SOME NEW WEAK DIALOG AND HIGHLY MOTIVATED
      all_all <- c(all_all)
      
      # NOV 2,3,4,8 - SOME NEW WEAK DIALOG
      ### all_all <- c(all_all,"antropofaga","geminileebaby","xkryscrossedx") ### BEGIN REQUOTING
  
      # TODAY NOV 4 ONLY
      ### all_all <- c(all_all,"Mustbefriends1s")
      
      # TODAY NOV 5 ONLY
      # all_all <- c(all_all,"Natalief5625","illlizabeth")

      # NOV 5 - STUPID FAT TEEN-AGER - A ASKED OUT - SHE DID NOT SHOW
      all_all <- c(all_all,"yocarly")

      # TODAY NOV 5 ASKED OUT ( SHE IMPLIED THAT WE SHOULD MEET )
      all_all <- c(all_all,"sweaterwench") # NO - CRAZY LIAR FROM ATLANTA

      # NOV 11 - WEAK ASKED OUT - CONTINUE TALKING TO HER
      all_all <- c(all_all,"mlrobi15")
      
      # NOV 11 - WEAK DIALOG
      ### all_all <- c(all_all,"judythatsme8","februarybb","JadeLotus27")  ## BEGIN RE-QUOTING
      
      # NOV 11, 13 - BETTER DIALOG
      all_all <- c(all_all,"Jesscheaux","hottprincess4728")
      
      # NOV 11, 13 - BETTER DIALOG
      ### all_all <- c(all_all,"hottprincess4728") ## BEGIN REQUOTING
      

      
#       # SUN NOV 15 - ONE CUSTOM MESSAGE WAS SENT ( MONDAY NOV 21 - RECEIVED 2ND CUSTOM MESSAGE )
#        all_all <- c(all_all,"MissBookWorm8","jeanalean","miche2767","gailforcewins","sophiahelen1", "lainey504",
#        "yeezytavghtme","candyplant","LaurenMichelle47","n-y-m-p-h-e-t","tizzaverde","nicoleerinc","jamierad")
# 
#        # SUN NOV 15 - ONE CUSTOM MESSAGE WAS SENT
#        all_all <- c(all_all,"Bette_taco","sam9921","roul3tt3","boxfoxpox","susie81970","MaddiiieSunShine","roseyrides",
#        "Rachel_LeahR","jenellybeans","BookmarkNola","Di_marie22","dame-viking",
#        "bramblebull","AriaLaJune","tiffany656","Livemore83","heartontherail","sharinfun247","stuckuptown",
#        "i_am_Michelle86","ZIANOLA","BethWillowYeah","DRW70130","delasoul504","Steph8708",
#        "Upfromtheblue82","rrose_selavy88","FLSTFb14","JDTho","bbbupbb","Mustbefriends1","smorkin_labbit",
#        "sarah_lynn859","fallengt14","onepmtues","MegsInTheTardis","Verdictfashion","bete_rouge",
#        "Sanna5581","wagg90","ip23","Caballera1984","chirpy214","greeneyedlola","southernbelle326",
#        "spicyjane","ready4change227","cherijen","SaltyPaycheck","SweetandSnarky","ArtFunLuv","paigeeeb",
#        "bigeasyemi","mustlovedogs220","NOLAgirlAmy","femchefeastbank","Quenbea","kjacobs7","ktizzle-o-matic",
#        "jenren12","lapeach85","bnolan00","psychic__hearts","amicamortis","SassySparkles12","alleighrich",
#        "jennb504","New_GirlInTown","younggnfunn504","i_am_smh","alicee504","baBarbie","Mistyb7783",
#        "alittlelagniappe","breathemusic32", "indiefolkgirl","Jerseyfarmer","Quen32000","Hypnotc711",
#        "lookingupstar","Clevah_Gurl","haylidu","kathattack05","Lu6344")
      
       # THU NOV 20 - ONE RESPONSE FROM THE *BULK SENT*
       ## all_all <- c(all_all,"RealClaire") # BEGIN QUOTING
      
       # TUES - NOV 17 - FROM (SUN NOV 15 - ONE CUSTOM MESSAGE WAS SENT) - NEW WEAK DIALOG ( ALSO F BULK )
       ## all_all <- c(all_all,"LifeLovinLady", "awake_and_aware") ## BEGIN SENDING QUOTE
  
       # SUN NOVEMBER22 - NEW JUST GOOD 1ST MESSAGE 

#      all_all <- c(all_all,"heartneworleans","Cryztalbaby","CatEyes201","Artomaton","SeeRedGo88",
#      "jennierose2729","Lust4life03","craftyb123","iammeltee007","kayjay725","Jennigirl1",
#      "Jessie_Lynn_88","LAGirlInNOLa","shelbyk514","shelbyshortcake","DSuzanne","bag_o_Jess34","marisalikesyou")

       # NOV 23-24 SOME WEAK DIALOG
       # all_all <- c(all_all,"GothicCupcake")
  
       # NOV 23 - SOME VERY WEAK DIALOG - JUST TODAY ONLY
       # all_all <- c(all_all,"crybabii","cateyebel77")
        
       # NOV 24 SOME BETTER DIALOG
       ## all_all <- c(all_all,"heartontherail") # MASS-GOOD POEM 1 # RESPONER ## BEGIN RESENDING

       # NOV 8/27 - SUNDAY - WEAK DIALOG BUT HIGHLY MOTIVATED
       # SHE IS 'PERSON OF FEW WORDS' - ASK OUT NOW
       all_all <- c(all_all,"innausa") # SENT TO ME - TUES DEC 1 - n 2 weeks is wil be ok * COME BACK *
       
       # NOV 29 SOME LONG TERM WEAK CONVERSATION
       ## all_all <- c(all_all,"courtneyesl") ## BEGIN REQUOTING
       


       # NEW MONTH OF DECEMBER #
       # DEC 01 - NEW LIKE - FOLLOWING TWO FAMOUS QUOTES
       ### all_all <- c(all_all,"TravelingAwesome") ## RESTART QUOTING

      # PREV DATE -        WED EVE DEC 2       SAT MORN DEC 5  ( NIETHER REPONDED TO MY REQUEST FOR A 2ND DATE )
      all_all <- c(all_all,"SugarcaneMustard", "Elizabeth-1") # NOTE: ELIZ-1:VIOLINST WAS HOT

       # MONTH OF DECEMBER - SHE DOES NOT WANT TO CONTACT ME 
       all_all <- c(all_all,"clevergirl-17","cooldj12","bonlynnkat")

       # MONTH OF DECEMBER - I DO NOT WANT TO CONTACT HER
       all_all <- c(all_all,"Jengracey8675")

       # THU DEC 03 - WEAKEST DIALOG EVER POEM RESPONDERS
       all_all <- c(all_all,"rungakutta","neesie0411","kat6801") # 
       
       # THU DEC 03 - WEAKEST DIALOG EVER POEM RESPONDERS
       all_all <- c(all_all,"kat6801") # 3rd ONE: I do have a boyfriend and currently looking for another one to add.  Must be open to having a threesome with us. 

       # THU DEC 03 - BETTER POEM RESPONDERS
       all_all <- c(all_all,"kat6801","jesameca","Carlatourguide")

      # THu - DEC 3 -BETTER NEW DIALOG 
      all_all <- c(all_all,"sunshinewatcher", "nolaflowergirl","xkryscrossedx")

       # THu - DEC 4 - asked OUT NO RESPNONSE
       all_all <- c(all_all,"NOLApink")

       # DEC 4 - no response - ASKED OUT - REASKED OUT - SHE - FOREVER - CANCELLED
       all_all <- c(all_all,"superherolover")
       
       # DEC 5 - LIKED ME - REPSONDED - MOTIVATED - 26 AND CURVEY
       all_all <- c(all_all,"GirlThuggery")
       
       # DEC 6-11 - SOME NEW WEAK DIALOG
       all_all <- c(all_all, "kajungirl77", "leftofcenter37")



       # DEC 10 - SOME OLD DIALOG RETURNING - VERY WEAK - SHE IS OUT OF A BAD RELATIONSHIP
       all_all <- c(all_all, "StarryLyte")

       # DEC 10 - JUST ASKED OUT ( SHE *ASKED ME OUT* )
       all_all <- c(all_all, "readytoluv93")

       # # #
       ### all_all <- c(all_all, "GambitGirl","Patricia111286","ShelleyK07","Wittynwise1957")

       # LATE February - NO
       ### all_all <- c(all_all,"UptownDez","sandibeach10","lamary","UptownDez")

       # February 28 - Sunday - Simple Contact
       ### all_all <- c(all_all,"LadyNMandeville")

       # Late Feb - ASKED OUT - TURNED DOWN
       ### all_all <- c(all_all,"yoshgirl","BayouBunny","SandraB56","dj0806","yogi3784")

       # Medium Conversations
       ### all_all <- c(all_all,"SuddenlySarah49","Byugirl64","geminileebaby","south2013")
       
       # ASKED OUT - SUN 6 TH - IN PROGRESS
       ### all_all <- c(all_all,"seriouspeopleonl")

       # implicit asked out - SILL CONVERSATION IN PROGRESS
        ### all_all <- c(all_all,"crazyinnola1")

       # Late Feb - Long Conversations
       ### all_all <- c(all_all,"Alycattp","Beverly1965","katzekew","FreeTimeinNOLA","Starlet19","brandiew35")

       # Feb 23 - # asked out  # 60 years old ( MAYBE KEEP PERSUING? )
       all_all <- c(all_all, "SandraB57") # asked out : waiting response TURNED DOWN - tranport problems

       # Late Feb - Should make a contact
       ### all_all <- c(all_all,"Jenbat","Phartzthecat","FunBrunette08","SisInNOLA")

       ## BEGIN tuesday March 8 (really sunday March 6) ##

       # medum term
       ### all_all <- c(all_all,"cjh2016")

       # Late Feb - Asked out - We dated - she accepted for Thursday March 3rd
       all_all <- c(all_all,"PerfectlySpiced") # Date Over, End of two, Dates ( Indian Woman)

       # Date in March, 2016 Bipolar and weird.  She does not want to see me again
       all_all <- c(all_all, "leftofcenter37")

       # She asked me out.  Then, I gave instructions # i reasked out: on April 19th.
       all_all <- c(all_all,"freespirit826") # Gina RE-INPROGRESS

       # a re-ask out
       all_all <- c(all_all,"CanYouHandleHer")

       # she asked me out - april 19th for morning breakfast - could not make it
       # I am still trying to set up
       all_all <- c(all_all,"howlokitty")

       # May 11th just today ONLY
       # all_all <- c(all_all,"smilingnsinging","Krysjoon")
       
       # Preparing for Friday, May 13th # DOES NOT EXIST: Russia/Montana/Louisian
       all_all <- c(all_all,"lindacherry28") # text conversations: 

       # From May 11, for friends, asking out 'in person' - waiting her confermation
       # all_all <- c(all_all,"flawer123") # SHE IS TOUGH: KEEP TRYING

       # NO MORE
       all_all <- c(all_all,"Litgoddess1")

       # for May 13 Fri, follow up ONE on ONE ( kOOKY personality ) SHE FAKED
       all_all <- c(all_all,"CanyouhearGod")

       ### ( three means today only )

       # She could not make the 1st date: handling manully ( SHE CHANGED HER USERNAME)
       all_all <- c(all_all,"carlaj25","cJo525")

       # TODAY ONLY # MONDAY # MAY 16TH
       # all_all <- c(all_all,"Moscato89","alderfae","smilingnsinging")

       # TODAY ONLY # FRIDAY MAY 20 # Laura_Kristin85(30 and not fat)
       # all_all <- c(all_all,"kathyb53","weathergirlmania","Alycattp","too_tall_bbw11",
       #            "Laura_Kristin85","smilingnsinging","Moscato89","rainwater51")


       # christenelaine89 - BORDERLINE ( IF I DO NOT GET A RESPONSE BACK )
   
        # NOTE okcupid logic: a msg INCLUDES a vst
        #  OKCUPID IDEA: turn anonymous browsing ON WHILE sending messages $$ A-list
        
        do_not_vst <- c()
        if(not_to_vst == "NONE") {     # default
          do_not_vst <- c(special,"Kat0o","cajunfaith","aristarla") # previous dates
        }
        
       if(not_to_vst == "SOME") {     # default
         do_not_vst <- c(special,"Kat0o","cajunfaith","aristarla") # previous dates
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
        
        print(paste0("begin visiting each profile of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
        
        # rev(apagearefsupr)[1:2]               BOTTOM OF PAGE:  testing: reverse and first 2 names ( testing )
        # letters[1:(length(letters) %/% 2)]    TOP    OF PAGE:  50% of the unique links GOING DOWN
        
        apagearefsupr_total       <-  apagearefsupr
        apagearefsupr_total_count <- length(apagearefsupr)
        print(paste0("Total possible profiles: ",apagearefsupr_total_count))    
              
        if(apagearefsupr_total_count == 0) {
          ### browser()
          print("since Total possible profiles: 0, then SKIP AHEAD to next LOOP")
          next
        }
        
        # FEBRUARY 23 2016
        # missing the exact suffix "?cf=regular,age_recip_off"
        missing_reg_recip_off_index <- which(!str_detect(apagearefsupr,"[?]cf=regular,age_recip_off"))
        
        # FEBRUARY 23 2016
        # ADD IT BACK to prevent: ( current matchname: NA )
        # NOTE: That *will* WORK but COULD cause a BUG in the future ( FALSE URL - target preferences status )
        apagearefsupr[missing_reg_recip_off_index] <- str_c(apagearefsupr[missing_reg_recip_off_index],"?cf=regular,age_recip_off")

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
        # end_matchnames_str_locations   <- (str_locate(apagearefsupr_reduced,"$"     ) - 1)[,1,drop=FALSE]

        # JANUARY 31 2016 do not collect ",age_recip_off" ( AS PART OF THE matchname )
        # "https://www.okcupid.com/profile/nolatonycbby?cf=regular,age_recip_off"
        end_matchnames_str_locations   <- (str_locate(apagearefsupr_reduced,"[,$]"     ) - 1)[,1,drop=FALSE]
        
        # MAY 3, 2016 - SEE COMPLEMENT BELOW
        # end_matchnames_str_locations   <- (str_locate(apagearefsupr_reduced,"[,?]"     ) - 1)[,1,drop=FALSE]

        # NOTE: (str_locate  finds 'first occurance"
        # could possible break if  "?" is found in a strange spot
        
        # eliminate matchnames that I would not visit - no matter how old they are
        # TO_DO_THIS_WEEKEND [ ]
        
        # hadly S-logic
        # matchnames <- str_sub(apagearefsupr_reduced, start = cbind(begin_matchnames_str_locations, end_matchnames_str_locations))

        # JANUARY 31 2016 Replacement code
        # (c("nolatonycbby","Mmisfits13","nolatonycbby?cf=regular","Mmisfits13?cf=regular"), "[?]cf=regular")
        matchnames_plus_regular <- str_sub(apagearefsupr_reduced, start = cbind(begin_matchnames_str_locations, end_matchnames_str_locations))
        # RUNTIME was different THAN test time
        # matchnames <- matchnames_plus_regular[!str_detect(matchnames_plus_regular, "[?]cf=regular")]
        # JANUARY 31 2016 REPATCH - in runtime, these are ONLY ALL ?cf=regular
  
        matchnames_could_be_more <- str_replace(str_extract(matchnames_plus_regular, "^.*[?]"),"[?]","")
 
        # MAY 3 2016 ( FOR SOME REASON THIS SHOWS UP: "cf=regular?" )
        matchnames <- str_replace(matchnames_could_be_more, "cf=regular[?]","")

        # JANUARY 31 2016 Replacement code
        # "https://www.okcupid.com/profile/nolatonycbby?cf=regular,age_recip_off"
        # "https://www.okcupid.com/profile/nolatonycbby,age_recip_off" ( NOT USEFUL )
        apagearefsupr_reduced_with_NOT_REGULARS_and_REGULARS <- apagearefsupr_reduced 
        apagearefsupr_reduced <-  apagearefsupr_reduced_with_NOT_REGULARS_and_REGULARS[str_detect(apagearefsupr_reduced_with_NOT_REGULARS_and_REGULARS,"[?]cf=regular")]

        print(paste0("Will (try) to visit these links of age: ", agecurr))
        print(as.data.frame(sort(apagearefsupr_reduced)))
  
        action_ref_counter <- 0                  # NOT TEST:     apagearefsupr_reduced
        for(alink in  apagearefsupr_reduced)   { #     TEST: rev(apagearefsupr_reduced)[1] # test: send a message to the bottom most link
  
          # so I know where I am
          action_ref_counter <- action_ref_counter + 1
          # TEST ( *** REMOVE AFTER TEST *** )
          # action_ref_counter <- length(apagearefsupr_reduced)
          
          print(paste0("begin visiting ", alink, " of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
          
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
            
            # JANUARY 31 2016 -See her FULL profile
            # At the bottom of her profile page ( click here +More link )
            # webElemMORELINK <- remDr$findElement("css selector", "span i.okicon" )
            # SHOULD BE ABOVE ( CSS SEENS CORRECT )
            
            # FEBUARY 10 - DOING XPATH
            # webElemMORELINK <- remDr$findElement("xpath",'//*[@id="profile2015"]/div[4]/div[1]/div[1]/div[6]/span' )
            # FEBUARY 10 - DOING XPATH
            # not seen, but document.querySelectorAll('span i.okicon').length; finds 36 on the page
            # would ONLY error here.  So just skip.  I do not care.
            # SEEMS that ITS LOCATIONDYNAMICALLY MOVES - DYNAMIC POSITIONS ON THE PAGE
            ### DOES NOT WORK - INSTEAD OF HITTING A LINK AT THE BOTTOM OF THE PAGE IT HITS
            ###   AN ICON AT THE TOP OF THE PAGE ( ANTI - SCRAPING S* )
            ### ( SKIP FOR RIGHT NOW:  I BELIEIVE THAT  'MOST OF THE ESSAY IS STILL ON THE PAGE - BUT JUST HIDDEN FROM VIEW )
            ### webElemMORELINK_S <-  remDr$findElements("css selector", "span i.okicon" )
            ###sapply( webElemMORELINK_S, function(x) { try( {  x$highlightElement() }, silent = TRUE )  ; try( {  x$clickElement() }, silent = TRUE ) } )
            
            
            # JANUARY 31 2016 Collect her profile essay text
            webElemESSAYS_S <- remDr$findElements("css selector", "div.essays2015-essay-content" )
            paste0(sapply(webElemESSAYS_S, function(x){  
              paste0(x$getElementText()," ") 
            }, simplify = TRUE ),collapse = "    ") -> essay_text
            
            print(essay_text)
            
            # also works x$highlightElement(); Sys.sleep(1.0);
            
            # To return
            # Basic Text Mining in R
            # https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
            
            # Text Mining
            # http://www.rdatamining.com/examples/text-mining
            
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
            # END - Sun Nov 22, 2015
            # webElemHERPAGE_AGE <- remDr$findElement("css selector", "div#basic_info div#aso_loc p.infos span:nth-child(1)")
            # NEW - Sun Nov 29, 2015
            # webElemHERPAGE_AGE <- remDr$findElement("css selector", "span.userinfo2015-basics-asl-age")
            
            # NEW - JANUARY 31  2016 ( "Sorry, this user does not exist.")
            webElemHERPAGE_AGE <- NULL
            result_find <- tryCatch({ webElemHERPAGE_AGE <- remDr$findElement("css selector", "span.userinfo2015-basics-asl-age") }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
            if(class(result_find) == "character" &&  result_find == "ERROR" ) { print(paste0("Problem, User exists?: ", matchnames[action_ref_counter]," skipping ...")) ; next }
            
            
            # webElemHERPAGE_AGE$highlightElement()
            current_her_page_age <- as.integer(webElemHERPAGE_AGE$getElementText()[[1]])
            
            # if( current_her_page_age != agecurr  ) {
            # ADJUSTED January 31, 2016
            if( (age_range_str_group == "one_age_page" ) && (current_her_page_age != agecurr)  ) {
              
              print(paste0("      **** WRONG AGE ON PAGE of matchname ", matchnames[action_ref_counter]," of search criteria age ", agecurr," BUT SHE HAS page age ", current_her_page_age))
                     print("      **** SKIPPING send message to HER ... next loop ...")
              next
              ## MAY 3 2016
              ## print("      **** (BUT NOT) SKIPPING send message to HER ... X next loop X ...")
              
            }
            
            print(paste0("begin send message ", alink, " of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
            
            if( action == "message_greet_matchname" ) {
              # OLD
              # current_message  <- paste0(message_greet_matchname_vector[trunc( 1 + length(message_greet_matchname_vector)*runif(1, min = 0, max = 1) - 0.001 )], " ",matchnames[action_ref_counter]) 
              # NEW expect the matchname to come first  # *** NOTE: STICK ," Ivan" at the end to add a signature  ***
              
              matchname_current <- matchnames[action_ref_counter]
              
              # browser( expr = { is.na(matchname_current) } ) # DEBUG
              
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
                c("lindsayp13","Lindsay"),
                c("christiangal919","Julia"),
                c("Annielynn1983","Annie"), # BUT *SHE* IS A BITCH
                c("mwg70115","Megan"),
                c("NOLApink","Mae"),
                c("mallee13","Mallora"),
                c("sungelique504","Natasha"),
                c("mallee13","Mallora"),
                c("lonelyheartz","Ashley"),
                c("aristarla","Ariana"),
                c("afternoon726","Trisha"),
                c("beckward","Becca"),
                c("Musijo","Joanna"),
                c("Scarletrayven","Mandy"),
                c("takemesrsly69","Allison"),
                c("layla_danielle","Layla"),
                c("smilingrosej","Joann"),
                c("ursweetestpoison","Tonya"),
                c("p4passion101", "Jessica"),
                c("Labchick3571","Michelle"),
                c("Cherribelle318","Cherry"),
                c("xkryscrossedx","Krys"),
                c("VintageLovely94","Trina"),
                c("Jesscheaux","Jessica"),
                c("awake_and_aware","May"),
                c("heartontherail","Savanna"),
                c("courtneyesl","Courtney"),
                c("Carlatourguide","Carla"),
                c("readytoluv93","Miranda"),
                c("thissupergirlis","Caroline"),
                c("yogi3784","Ronda"),
                c("brandiew35","brandie"),
                c("CanYouHandleHer","Ann"),
                c("freespirit826","Gina")
              )
              
              matchnames_aliases_db <- as.data.frame(t(data.frame(matchnames_aliases)), stringsAsFactors = FALSE)
              
              # print(paste0("DEBUG: matchname_current: ", matchname_current))
              # print("DEBUG: matchnames_aliases_db")
              # print(matchnames_aliases_db)
              
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
            
            
            # JUST BEFORE AN ATTEMPTED MESSAGE - today_file - ok_already_messaged_today_vector
            
            if(file.exists(today_file)) {
              
              load(file = today_file, envir = environment(), verbose = TRUE)
              
            }
            
            if( !exists("ok_already_messaged_today_vector", envir = environment()) )  ok_already_messaged_today_vector <- c()
            
            # BEGIN OK CUSTOM *** RETURN HERE ***
            if(matchname_current %in% ok_already_messaged_today_vector) { 
              
              print(paste0("Skipping attempt to send message to ", matchname_current," aka ", matchname_to_message))
              print("  I already HAVE AUTO MESSAGED her today!")
              
              next  # halts the processing # advanced to next loop index
              
            }
            # END OK CUSTOM ( TO DO [x] CUSTOMIZE ***
            
            
            # send message button  
            
            # SUN Nov 22, 2015 - No Longer Working
            # webElemSMB <- remDr$findElement("css selector", "#footer_send_btn")
            # SUN Nov 27, 2015
            webElemSMB <- remDr$findElement("css selector", "button.actions2015-chat.flatbutton.blue")
            webElemSMB$highlightElement() # THAT WORKED
            remDr$mouseMoveToLocation(webElement = webElemSMB) 
            webElemSMB$sendKeysToElement(list(key = "enter")) 
            
            Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))
            
            
            # JUST AFTER AN ATTEMPTED MESSAGE ( AND ONLY AFTER ) - today_file - ok_already_messaged_today_vector
            
            # CUSTOMIZE [X] 
            ok_already_messaged_today_vector <- c(ok_already_messaged_today_vector, matchname_current)
            save(list = c("ok_already_messaged_today_vector"), file = today_file, envir = environment() )
            
            
            # NOTE: BELOW - IF AN ERROR WOULD HAVE OCURRED
            #  CHANCES ARE HER MESSGE BOX IS FULL
            #    DO NOT BOTHER TO TRY TO RE-SEND A MESSAGE
            #      JUST COUNT THE MESSAGE AS 'MESSAGE SENT AND (GOOD ENOUGH)'
            
            # type characters in textarea
            
            # *DETECT HERE*
            # IF THE MESSAGE BOX IS NOT IN THE FOREGROUND "BECAUSE OF A MESSAGE"
            # SHOULD *DETECT HERE* AND NOT BOTHER TO EXECUTE THE REMAINDER OF THE CODE
            
            # Sys.sleep(5.0)
            # INDEX <- -1
            # TEST<- remDr$findElements("css selector", "textarea" ) # TEST # FLASHES ON 5
            # lapply(TEST, function(x){INDEX <<- INDEX + 1 ;x$highlightElement(); x$highlightElement(); print(INDEX) ;Sys.sleep(5.0) } )
            
            # MOST RELIABLE WAY TO DO IT.  NOTE: querySelectorAll also WORKS
            # JANUARY 31 2016 textarea location changed from [5] to [4]
            message_textarea <-                               "document.querySelectorAll('textarea')[4].value"
            message_textarea_detect_exists <- "try{ retvalue = document.querySelectorAll('textarea')[4].value; return 0 } catch(err) { return -1 };"
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
              # return document.getElementsByTagName("textarea")[4].value = "Hi redbeanredbean"; 
              
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
                print(paste0("end send message ", alink, " of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
                
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
          
          print(paste0("end visiting ", alink, " of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
          
          #######
          # COMMENTED OUT ( TOO MUCH TIME )
          # remDr$goBack()
          # Sys.sleep(1 + 2 * runif(1, min = 0, max = 1)) # 2 to 4 seconds wait
          #######        
  
          remDr$executeScript("return 0")
          
        }
        
        print(paste0("end visiting each profile of the page of : ",if(age_range_str_group == "all_ages_page") agerange_str else agecurr, " of age ", agerange_str))
        
        print(paste0("ending age ", agecurr))
  
        print("Of THIS progrem, the user hand written call follows.")
        print(looper_typed_in_call) # language
        
      } # for(agecurr in agerange)
  
  
      # END OF OLD MAIN PROGRAM
  
    } # for(loop_index in loop_forever_vector)
    
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

# setwd("C:/Users/AnonymousUser/All_Romance/AES1") # getwd()
# MAKE SURE THAT THE .R file in the tab(hover over) has the same dir path as 'setwd'

# ABSOLUTE PATH IS BEST
# debugSource('C:/Users/AnonymousUser/All_Romance/AES1/okcupid_visit_looper_dev.R')
## debugSource('C:/Users/AnonymousUser/All_Romance/AES1/utilities_ext_visit_looper_dev.R')

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
# okcupid_visit_looper_dev <- function(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:60", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", face_color = "anything", loop_forever = "no", age_range_str_group = "one_age_page")
#
# lately ( just prev dates - SOME not visit)
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:60", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE", face_color = "anything", loop_forever = "no", age_range_str_group = "one_age_page")
# 

# messaging - not previous dates
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:60", todays_message = paste0(", thank God that it is ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How is it going?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", face_color = "anything", loop_forever = "yes", age_range_str_group = "one_age_page")  

# END INSTRUCTIONS  
# END INSTRUCTIONS    

# NO GITHUB 
# okcupid
# , site_login = "time861wiz", site_password = "739heg08", 
# 

  # lately ( just prev dates - SOME not visit)
  # okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE", face_color = "anything", loop_forever = "no", age_range_str_group = "one_age_page")

# MAIN USE
# lately ( just prev dates - SOME not visit) , face_color = "white"
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE", face_color = "white", loop_forever = "no", age_range_str_group = "one_age_page")

  # messaging - not previous dates
  # okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", face_color = "anything", loop_forever = "yes", age_range_str_group = "one_age_page")  

# MAIN USE -
# messaging - not previous dates , face_color = "white"
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", GLORIUS ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you tonight?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", face_color = "white", loop_forever = "yes", age_range_str_group = "one_age_page")  

####
# NOTE AFTER TESTING     [ ] of new FEATURE  , age_range_str_group = "all_ages_page"
#      CHANGE (ABOVE AND BELOW) DEFAULTS   [ ] from   , age_range_str_group = "one_age_page"   TO    , age_range_str_group = "all_ages_page"
###

# TESTING - MAIN USE - MESSAGING EVERY ONE
# messaging - not previous dates , face_color = "white"                                                                                                                                                                                                                                                                                                                                                                                                          # TESTING NEW
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", GLORIUS ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you tonight?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", face_color = "white", loop_forever = "yes", age_range_str_group = "all_ages_page")  

# TESTING - MAIN USE - VISITING - THIS WEEK
# lately ( just prev dates - SOME not visit) , face_color = "white"
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE", face_color = "white", loop_forever = "no", age_range_str_group = "one_age_page")

# 

# TESTING - MAIN USE - VISITING  - ages 38 - 60 - ONLINE NOW, , loop_forever = "yes"
# lately ( just prev dates - SOME not visit) , face_color = "white", age_range_str_group = "all_ages_page" 
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "online_now", not_to_vst = "SOME", not_to_msg = "all_all", face_color = "white", loop_forever = "yes", age_range_str_group = "all_ages_page")

# TESTING - MAIN USE - MESSAGING  - ages 38 - 60 - ONLINE NOW, , loop_forever = "yes"
# lately ( just prev dates - SOME not visit) , face_color = "white", age_range_str_group = "all_ages_page" 
# okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:60", todays_message = paste0(", what a perfect ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), ". How is it going this evening?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "SOME", not_to_msg = "all_all", face_color = "white", loop_forever = "yes", age_range_str_group = "all_ages_page")


#                 
#                          
 


## LEFT OFF - SAVE THIS [ ] NOTEPAD ++ AND GITHUB [ ]
######### last work inprogress ( HERE ESSAYS ) ##################

# # text diving helpers
# 
# # uses: tm
# 
# essay <- "I'm just a girl in the world\n\nI try really hard not to give a fuck, but I'm really bad at it.\n\nJean jacket weather is better weather.     Working for a jewelry designer and studying the books hard.     Falling down with grace\nBaking\nCooking\nHiking and falling down\nClimbing trees and getting stuck\nReading\nGeography\nLaughing at you and other people\nPhotography\nGrowing plants     Archer\nHBO\nTina n Amy\nIndie film Paul Rudd\n\nChinatown\nThird man\nHarry Potter in all ways\nLots and lots and lots of books\nRichard avedon\nMargaret white\nAnsel Adams\nPiassco\n\nNetflix\n\nWhite girl movies etc etc     Zuko, my puppy\nBlunts\nNetflix\niPhone future device\nCandy\nMy legs     How many blunts are too many blunts, global scale ethnic conflicts, tequila and how much I can drink of it, carrots and hummus, Zuko, total world domination.     Who knows     If you got the goods "
# 
# # clean up (and remvoe carriage returns)the document 
# 
# clean <- function(x) {
#   gsub('[,.;:\'"()]','',x) -> x
#   
#   gsub("[\r\n]", " ", x) -> x
#   
#   return(x)
#   
# }
# 
# clean(essay) -> essay_cleaned
# 
# # do a word cont
# 
# word_count_simple <- function(lines) {
#   chunks <- strsplit(clean(lines),'\\s')
#   words <- do.call(c, chunks)
#   table(words) -> x
#   
#   x[!(names(x) %in% "")] -> x
#   
#   return(x)
# }
# 
# word_count_simple(essay_cleaned) -> essay_cleaned_counted
# 
# stop_words <- tm::stopwords('english')
# 
# logical_index_of_her_stop_words <- sapply( names(essay_cleaned_counted), function(x) {  any( tolower(x)  %in%stop_words )  } )
# 
# essay_cleaned_counted_non_stop <- essay_cleaned_counted[!logical_index_of_her_stop_words]
# 
# essay_cleaned_counted_non_stop_priority_ordered <- essay_cleaned_counted_non_stop [order(essay_cleaned_counted_non_stop , decreasing = TRUE)]
# 
# # almost every WORD appears JUST once
# 
# # From functional programming to MapReduce in R
# # https://cartesianfaith.com/2015/09/17/from-functional-programming-to-mapreduce-in-r/
# # AND
# # remove all line breaks (enter symbols) from the string using R
# # http://stackoverflow.com/questions/21781014/remove-all-line-breaks-enter-symbols-from-the-string-using-r
# 
# 
# From functional programming to MapReduce in R
# Thursday 17th
# Sep 2015
# Posted by Brian Lee Yung Rowe
# https://cartesianfaith.com/2015/09/17/from-functional-programming-to-mapreduce-in-r/
#   
#   ############### END OF TEXT DIVING #########################   
#                                






