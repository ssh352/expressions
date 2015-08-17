

# zk
# DON'T FORGET TO STARTUP PostgreSQL !!

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) 
options(digits.secs = 6)
options(error=NULL) # options(error = recover) 

if(Sys.getenv("RSTUDIO") == "1") {
  debugSource(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
} else {
  source(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
}

zk_visit_looper_dev <- function(curr_port = 4444, browser = "firefox", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), on_exit_logoff_site = TRUE, on_exit_close_browser = TRUE, on_exit_stop_selenium_server = FALSE, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") { 
  # OR action = "message_greet_matchname" "message_random_catchphrase"
  # OR not_to_msg = "all_all"
  # CONSIDER other PARAMETERS: , online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE"
  
  looper_typed_in_call <- match.call( ) # language
  
  maininner <- function() {
    
    oldtz <- Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    
    set.seed(runif(1, min = 0, max = 1))
    
    require(RSelenium)
    require(stringr)
    
    # Administrative sleep POSSIBLE_FUNCTION_PARAMETER
    # e.g. I have to go somewhere and want this to run automatically
    # Sys.sleep(6700) # 7200 - two hours
    
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host = "127.0.0.1", dbname = "aes_db", user = "postgres", password = "postgres")
    
    # REM: taskmgr OR 'some other way' KILL off java.exe if it is running
    
    # netstat -o -a -b  -n | find /i "listening" | find /i ":4471" 
    # taskkill /F /T /PID <above_right_col_number>
    # ABOVE done more elegantly BELOW
    
    # get the parent port 
#     child_port <- as.character(curr_port)
#     tcpip_network_connections <- system2("netstat", args="-o -a -n ", stdout = TRUE)
#     tcpip_network_connections_port_of_interest_index <- str_detect(tcpip_network_connections, child_port)
# 
#     # is at least one value TRUE? 
#     tcpip_network_connections_interested <- c()
#     parent_port <- ""
#     if(any(tcpip_network_connections_port_of_interest_index)) {
#       
#       tcpip_network_connections_interested <- tcpip_network_connections[tcpip_network_connections_port_of_interest_index]
#       parent_port <- unique(str_replace(str_extract(tcpip_network_connections_interested,"[ ]\\d+$"),"[ ]",""))
#       
#       # kills parent and dependent children(java.exe and chromedriver.exe) DOES NOT kill chrome.exe
#       print(paste0("begin killing child port ",child_port," and parent port ( and children ) ", parent_port))
#       system2("taskkill", args=paste0(" /F /T /PID ", parent_port), stdout = TRUE) # need print() to print
#       print(paste0("end killing child port ",child_port," and parent port ( and children ) ", parent_port))
#     }

    # CHROME/FIREFOX - ENABLED NATIVE EVENTS DEFAULT - SEEMS NOT REQUIRED
    # " -Dwebdriver.enable.native.events=1" ( SEEMS *NO AFFECT IN/OUT")
    # SHOULD have been arleady started
    manageRSeleniumServer(curr_port = 4444, doif = "if_server_not_started_please_start" )
    
    # CURRENTLY - zk WILL NOT USE A PROFILE - selenium/chrome - NOT_WORK_profile_MULTI_remDrS 

    if(  browser == "chrome"  &&  use_the_custom_profile == TRUE) {
      
      browser_profile_dir_path   <- "J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data"
      browser_profile            <- "epoch536intel_epoch536intel"
      browser_profile_file       <- "Preferences"
      browser_profile_file_GOOD  <- "Preferences.CORRECT_HOME_PAGE_AND_IMAGES"  # SHOUD BE 'NO_IMAGES' ( LATER: MAKE ONE OF THESE )  
      
      browser_pref_conf_file_name <- paste0(browser_profile_dir_path, "\\", browser_profile, "\\" , browser_profile_file)
      browser_pref_conf_file_name_GOOD <- paste0(browser_profile_dir_path, "\\", browser_profile, "\\" , browser_profile_file_GOOD)
    
      # chrome - CRASH RESTORE LAST SESSION PROBLEM
      # Disable Google Chrome session restore functionality [duplicate]
      # http://superuser.com/questions/461035/disable-google-chrome-session-restore-functionality
      #                   exit_type: None NOT Normal
      # JSON profile exited_cleanly: true
      # http://jsbeautifier.org
      # http://jsonviewer.stack.hu
      # ABOVE - DID NOT WORK
      
      # Solution - JUST REPLACE THE NEWER PREF FILE WITH AN OLDER PREF FILE
      
      # CURRENTLY - zk WILL NOT USE A PROFILE - selenium/chrome - NOT_WORK_profile_MULTI_remDrS 
  
      print("Begin browser pref conf file name GOOD copy ")
      file.copy(from = browser_pref_conf_file_name_GOOD
                , to   = browser_pref_conf_file_name
                , overwrite = TRUE, copy.date = TRUE
      ) -> file_copy_success # return TRUE/FALSE
      if(!isTRUE(file_copy_success)) { stop("Preferences file copy failed!")  }
      # if 'from' file not found, it will * silently fail *
      Sys.sleep(5.0) # flush time
      print("End browser pref conf file name GOOD copy ")
      
      # CURRENTLY - zk WILL NOT USE A PROFILE - selenium/chrome - NOT_WORK_profile_MULTI_remDrS 
  
      # cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "epoch536intel_epoch536intel") 
      cprof <- getChromeProfile(browser_profile_dir_path, browser_profile)
      remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof, port = curr_port) # default 4444

    }

    if(  browser == "chrome"  &&  use_the_custom_profile == FALSE) {
      
      # MAYBE FREEZES IF IT DOES NOT HAVE A PROFILE? ( no images - help? )
      remDr <- remoteDriver(browserName = "chrome", port = curr_port)

    } 

    if(  browser == "firefox" ) {
      
      # not required firefox does native events "MANUALLY type remDr to SEE"
      # remDr <- remoteDriver(port= curr_port, nativeEvents = TRUE) # RSelenium-basics.html async javascript PROBLEM firefox?
      # OVERRIDE
      # OVERRIDE
      # OVERRIDE - CURRENLY ONLY PRACTICING WITH FIREFOX
      # FIREFOX
      # nativeEvents = XXXX # because DEFAULT may be platform specific
      remDr <- remoteDriver(browserName = "firefox", port= curr_port, nativeEvents = TRUE)  # SYNC/ASYNC PAGE LOADING?
      # remDr <- remoteDriver(port= curr_port, nativeEvents = FALSE)  # SYNC/ASYNC PAGE LOADING? - NO AFFECT
      
    } 





    # (hangs too much to use)
    # CHROME ALONE ( NO PROFILE)
    # remDr <- remoteDriver(port= curr_port, browserName = "chrome")

    print(paste0("PORT ", curr_port))
    
    # NOTE: POF # stores 'preferences(cookie-ish) on its servers in Vancouver '
    # pof has DEEP memory
  
    # if fail - retry once - chrome specific 1ST run of the day problem
    result_open <- tryCatch({ remDr$open() }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
    if(class(result_open) == "character" &&  result_open == "ERROR" ) { 
      Sys.sleep(4.0) 
      print("Begin retry of browser open fail")
      remDr$open()
      print("End retry of browser open fail")
    } 
    Sys.sleep(4.0)

    # default is 10 seconds ( 10000 millisecons ) ( playing with this again )
    remDr$setImplicitWaitTimeout(60000)

    # default is 10 seconds ( 10000 millisecons ) ( playing with this again )
    remDr$setAsyncScriptTimeout(60000)
    
    # chrome crashes on this one
    # remDr$setTimeout(60000)

    if(browser == "chrome") {
      # nav to chrome://settings/ and turn off images for performance reasons
      remDr <- google_chrome_set_no_images(remDr = remDr)
    }

    # I did not see on chrome_pof, chrome_okcupid  ( but ff_zoosk may be different )
    #
    # SOME SCIENCE + ( EARLY ) CHEAT/HACK 'search for html element'
    #
    # How to get Selenium to wait for page load after a click ( GOOD ARTICLE )
    # http://www.obeythetestinggoat.com/how-to-get-selenium-to-wait-for-page-load-after-a-click.html
    #
    # REVIEW WITH
    # https://github.com/ropensci/RSelenium/blob/master/R/remoteDriver.R
    # REVIEW WITH
    # https://code.google.com/p/selenium/wiki/JsonWireProtocol#/session/:sessionId/timeouts/implicit_wait
    # https://code.google.com/p/selenium/wiki/JsonWireProtocol#/session/:sessionId/timeouts
    # https://code.google.com/p/selenium/wiki/JsonWireProtocol#/session/:sessionId/async_script
    
    # Invalid call to server. Please check you have opened a browser. ( MUST OPEN A BROWSER FIRST)
    # remDr$setAsyncScriptTimeout(10000) # HELP WITH RSELENIUM MOVING FASTER THAN FIREFOX/ZOOSK/JAVASCRIPT?
    # NO AFFECT

    # STRANGE BEHAVIOR - ZOOSK WILL NOW 'NOT LOG ME IN' BUT 
    #   SEND ME TO INSTEAD "https://www.zoosk.com/login?on_login=https%3A%2F%2Fwww.zoosk.com%3A443%2Fpersonals%2Fsearch%2Fedit&from=noauth"
    # remDr$setImplicitWaitTimeout(10000)
    # remDr$setTimeout(type = "script", milliseconds = 10000)
    # remDr$setTimeout(type = "implicit", milliseconds = 10000)
    # remDr$setTimeout(type = "page load", milliseconds = 10000)

    print(paste0("PORT ", curr_port))
    print("(tried) opened browser home page")
    
    result = tryCatch({ remDr$navigate("https://www.zoosk.com/zoosk-on-the-go?notify=logout&from=user-menu") }, warning = function(w) {}, error = function(e) {}, finally = {})
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) 
    
    print("(tried) logout from zoosk")
    
    # Sys.sleep(1 + 5* runif(1, min = 0, max = 1)) # 6 to 11 seconds wait
    
    remDr$navigate("https://www.zoosk.com")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    print("navigated to zoosk")
    
#     # BEGIN OLD POF ZONE
#     # cookie-ish tends to remember previous input#logincontrol_username and put it there.  
#     # I want to clear this
#     js_clear_login_box <- "try{ retvalue = document.querySelectorAll('input#logincontrol_username')[0].value = ''; return 0 } catch(err) { return -1 };"
#     remDr$executeScript(paste0(js_clear_login_box))[[1]]
#     
#     webElem1 <- remDr$findElement("css selector", "input#logincontrol_username")
#     webElem1$sendKeysToElement(list("era674smart")) # NOTE: BELOW: MAYBE DANGEROUSLY HARDCODED IN a message to here
#     Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds
#     
#     webElem2 <- remDr$findElement("css selector", "input#logincontrol_password")
#     webElem2$sendKeysToElement(list("739heg08"))
#     Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds 
#     
#     webElem2$sendKeysToElement(list(key="enter"))
#     Sys.sleep(4 + 2 * runif(1, min = 0, max = 1)) # 5 to 10 seconds wait
#     # END OLD POF ZONE

    # BLUE PAGE [Log In]
    # 2nd button of 12 (actually 10) buttons on the page is the login button
    #   2 buttons per div_of_interest # I choose the 2nd button (that I login)
    # notice: findElement AND nth-child
    webElemLOGINBTN <- remDr$findElement("css selector", "div.date-smarter-text div.action-container span.button-alt:nth-child(2)")
    webElemLOGINBTN$highlightElement() # VERY WORKED
    webElemLOGINBTN$clickElement()     # a white html 'email address/password' dialog will appear
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) 

    # NOTE: VERIFIED: COOKIE(CUSTOM PROFILE): WOULD REMEMBER epoch536intel@gmail.com AND DISPLAY FROM LAST_TIME ( SEEN )  
    # IF (IN THE FUTURE I USE A CUSTOM PROFILE) 
    # IF JAVASCRIPT CAN NOT CLEAR THEN SEND 'END' THEN MANY NATIVE BACKSPACES

    # 'email address/password' dialog
    # little more reliable

    webElemEMAIL <- remDr$findElement("name", "email")
    webElemEMAIL$highlightElement()
    webElemEMAIL$sendKeysToElement(list(site_login))
    Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))

    webElemEMAILPASS <- remDr$findElement("name", "password")
    webElemEMAILPASS $highlightElement()
    webElemEMAILPASS$sendKeysToElement(list(site_password))
    Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))

    webElemLOGINBTNTRUE <- remDr$findElement("css selector", "button.button-confirm")
    webElemLOGINBTNTRUE$highlightElement() # WORKED 'blinked gray'
    webElemLOGINBTNTRUE$clickElement()     # SEEMS STRANGLY_DANGEROUSLY ASYNCHRONOUS (FF - NATIVE EVENTS PROBLEM)
    Sys.sleep(12 + 1 * runif(1, min = 0, max = 1)) # MAKE SURE AI AM THERE

    # ALT may want to LOOP check (remDr$getCurrentUrl()[[1]] != https://www.zoosk.com/personals%) 
    # if not Sys.sleep(1.0)
    # BUT WONT WORK?! - ALREADY AT https://www.zoosk.com/personals% BUT THE ELMENTS ARE NOT DRAWN YET
    # SOME - ANTI - HACKING remDr$set*Wait ... boots me to URL noauth
    # CHEAPER - JUST TO WAIT 10 SECONDS ( ABOVE)

    print("logged into zoosk")
    

    # 'do not visit/do not message' global collection
    # LATER - re-write using PAIRS? - can I search by ... long name?

    special    <- c("robot")
    some_curr_dialog <- c()
    rec_all <- c() # HAVE SENT A good CUSTOM FIRST MESSAGE - SUN JUL 26
    lik_all <- c()
    all_all <- c(lik_all,rec_all,some_curr_dialog) 
    
    # MANUAL OVERRIDE
    # all_all <- c()
    
    do_not_vst <- c()
    if(not_to_vst == "NONE") {     # default
      do_not_vst <- c(special)
    }
    
    # exclusive choices
    if(not_to_msg == "NONE") {     # default
      do_not_vst_msg <- c(do_not_vst)
    }
    if(not_to_msg == "all_all") {
      do_not_vst_msg <- c(do_not_vst,all_all)
    }  

    # zk: I do not want to ACCIDENTALLY visit a person accidentally twice
    data_guid_already_visited <-c() 

    # unique id will by data_guid ( FOR NOW )
    match_matchname_data_guid_no_reencounter_global <- c()
    match_matchname_no_reencounter_global_list <- list()

    print("Of THIS progrem, the user hand written call follows.")
    print(looper_typed_in_call) # language

    # NOTE: zk: 
    # I MUST (diff from okcupid and pof) LOOP THROUGH ALL PAGES (at least per age ) 
    #        AND COLLECT THE ATTRIBUTES 
    #   before visting anyone. If I manually browse to  "http... page=1", then page data 
    #      will recycle after the 3rd page '4th page = 1st page" WHICH IS FALSE
    
    # MAGIC NUMBER  
    # zk ( ANY numbers: minage < me < maxage ) ??
    
    # agerange     <-  18:49      #  30:31  # 45:44   c(25:18,50:31) "25:18,50:31" # 
    # agerange_str <- "18:49"     # "30:31" # 45:44

    agerange_str <- age_range_str
    agerange     <- eval(parse(text=agerange_str))

    for(agecurr in agerange) { # testing only 31 and 30 # 31:30    
      
      this_age_visit_number_counter <- 0
      
      match_matchname_data_guid_reduced_per_age     <- c() 
      match_matchname_short_reduced_per_age         <- c() 
      match_matchname_recent_online_reduced_per_age <- c() 
      match_matchname_online_now_reduced_per_age    <- c() 
      match_matchname_age_reduced_per_age           <- c() 
      match_matchname_location_reduced_per_age      <- c() 
      
      match_matchname_no_reencounter_list_per_age   <- list()
      
      # IS USED ANYMORE?
      data_guid_range_allthisage <- c()
      
      print(Sys.time())
      print(paste0("PORT ", curr_port))
    
      print(paste0("beginning age ",agecurr," of ",agerange_str))
      
      # going to search page
      
      # remDr$navigate("https://www.zoosk.com/personals/search/edit")
      # Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # TIME CONSUMING
    
      # Note: this will *also* ( properly ) show : Edit Search ( Currenly Unused )
      # https://www.zoosk.com/personals/search?page=1&view=slideshow
      
      # BELOW - no longer has "Edit Search" on the right
#       result_navigate <- tryCatch({ remDr$navigate("https://www.zoosk.com/personals/search/edit") }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
#       if(class(result_navigate) == "character" &&  result_navigate == "ERROR" ) { 
#         Sys.sleep(10.0) 
#         print("Begin retry of fail of nav to personals/search/edit browser open fail")
#         remDr$navigate("https://www.zoosk.com/personals/search/edit")
#         print("End retry of fail of nave to personals/search/edit browser open fail")
#         Sys.sleep(10.0) 
#       } 
      

        # show link "Edit Search" ( Right Side)
        webElemSEARCHMAGGLASS <- remDr$findElement("css selector", "li.main-nav-search span:nth-child(2)")
        webElemSEARCHMAGGLASS$highlightElement() 
        webElemSEARCHMAGGLASS$clickElement() 

        # click on "edit search" ( Right Side )
        webElemEDITSEARCH <- remDr$findElement("css selector", "span.search-edit.edit-search-options.link")
        webElemEDITSEARCH$highlightElement()
        webElemEDITSEARCH$clickElement()
        Sys.sleep(10.0)

      # now at location of tabs 'New Search(HERE)     Saved Searches'
      
      # SOMETIMES (even without cookies)
      # REMEMBERS LAST SEARCH - not what I want safer to
      # 1. reset 2. go to search link again
      
      # reset
      
      webElemSEARCHRESETLINK <- remDr$findElement("css selector", ".js-reset-link")
      webElemSEARCHRESETLINK$highlightElement() 
      webElemSEARCHRESETLINK$clickElement() # works
      Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
      
      # re-navigate to search page
      
      # remDr$navigate("https://www.zoosk.com/personals/search/edit")
      # Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # TIME CONSUMING
      
      # show link "Edit Search" ( Right Side)
      webElemSEARCHMAGGLASS <- remDr$findElement("css selector", "li.main-nav-search span:nth-child(2)")
      webElemSEARCHMAGGLASS$highlightElement() 
      webElemSEARCHMAGGLASS$clickElement() 
      
      # click on "edit search" ( Right Side )
      webElemEDITSEARCH <- remDr$findElement("css selector", "span.search-edit.edit-search-options.link")
      webElemEDITSEARCH$highlightElement()
      webElemEDITSEARCH$clickElement()
      Sys.sleep(10.0)

      # WORKS - but I may/will 'return to' later
      # takes me to the 'saved searches tab'
      
      #     webElemSEARCHTAB <- remDr$findElement("css selector", "li.js-es-tab-saved-searches")
      #     webElemSEARCHTAB$highlightElement() # THAT WORKED
      #     webElemSEARCHTAB$clickElement()  # WORKED # ( hangs in 'selenium chrome' better in firefox? )
      
      # setting the search age ( always (unless saved search?)) # age1 to age1
      
      webElemAGEMIN <- remDr$findElement("css selector", ".js-criteria-age-min.age-input")
      webElemAGEMIN$highlightElement()
      remDr$executeScript(script = "return arguments[0].value = arguments[1];", args = list(webElemAGEMIN,agecurr))[[1]] # WORKS
      Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
      
      webElemAGEMAX <- remDr$findElement("css selector", ".js-criteria-age-max.age-input")
      webElemAGEMAX$highlightElement()
      remDr$executeScript(script = "return arguments[0].value = arguments[1];", args = list(webElemAGEMAX,agecurr))[[1]] 
      Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
      
      # setting the location
      # note zk knows Metairie or 70002 - persistent enough not to code 
      
      # setting the search distance ( always (unless saved search?)) # 50 miles
      
      webElemDISTANCE <- remDr$findElement("name", "distance")
      webElemDISTANCE$highlightElement()                               # <option value="80">50 miles</option>
      remDr$executeScript(script = "return arguments[0].value = arguments[1];", args = list(webElemDISTANCE,80))[[1]] # works
      Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
      
      # default
      if( body_type == "anything" ) {
        # reset(way above) PUT 'advanced critia' at ANY
      }
      
      # Body Type: zk: "SLIM or ATHLETIC"
      if( body_type == "thin_athletic" ) {

        # reset(way above) PUT 'advanced critia' at ANY
        # press the advanced search link
        
        webElemADVSEARCHLINK <- remDr$findElement("css selector", ".js-advanced-search-link")
        webElemADVSEARCHLINK$highlightElement() 
        webElemADVSEARCHLINK$clickElement() # works
        Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
        
        # body type
        webElemSEARCH_CRIT_BODY_TYPE  <- remDr$findElements("css selector", "input.js-criteria-body_type")
        
        # SLIM <input class="js-criteria-body_type" value="1" type="checkbox"> ( will not hightlight )
        webElemSEARCH_CRIT_BODY_TYPE[[1]]$clickElement()
        Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
        
        # ATHLETIC <input class="js-criteria-body_type" value="2" type="checkbox">
        webElemSEARCH_CRIT_BODY_TYPE[[2]]$clickElement()
        Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
        
        # Also - Average [[3]] and Curvy [[4]]
        # NOTE: much work exists to TRY CLICK on the 'ANY box' - detect text "Body Type"
        # OR MAY WANT TO USE A DIFFERENT KIND OF 'text orientated' SELECTOR
      }
      
      print(paste0("body type is ", body_type))
      
      # press the search button 
      
      # ONLY DO EXACTLY BEFORE pressing SEARCH 
      #  ( its changed position basic/advanced will cause webElem to *Error* )
      webElemSEARCHBTN <- remDr$findElement("css selector", "span.js-button-search span")
      webElemSEARCHBTN$highlightElement() 
      webElemSEARCHBTN$clickElement() #  WORKS
      Sys.sleep(4 + 1 * runif(1, min = 0, max = 1))
      
      # now I am at target age current_page THAT first page
      
      # toggle the grid
      
      webElemTOGGLEGRIDBTN <- remDr$findElement("css selector", "span.view-toggle.view-toggle-grid")
      webElemTOGGLEGRIDBTN$highlightElement()
      webElemTOGGLEGRIDBTN$clickElement()
      print("just toggled the GRID")
      Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
      
      # NEXT button will show
      
      pagerange <-      1:99      
      pagerange_str <- "1:99"    
      
      for(pagecurr in pagerange) {
        
        print(paste0("of age ",agecurr, " beginning page collecting information of ",pagecurr," of ",pagerange_str))
               
        # process this page before 'going to the NEXT page'
        
        # RESEARVED - * NOT USED (YET) *
        remDrGridPageSource <- htmlParse(remDr$getPageSource()[[1]])
        
        # get all match guids on the page 
        
        webElemMATCHES <-remDr$findElements("css selector", "li.grid-tile")
        print("just got page all TILES")
        
        # partial url of matches
        match_matchname_data_guid <- unlist(lapply(webElemMATCHES , function(x){x$getElementAttribute("data-guid")}))
        print("just got page all data-guid attributes")
        
        text_of_matches <- unlist(lapply(webElemMATCHES , function(x){x$getElementText()}))
        print("just got page all text of matches")
        
        match_matchname_short <- c()
        match_matchname_recent_online <- c()
        match_matchname_online_now <- c()
        match_matchname_age <- c()
        match_matchname_location <- c()
        
        print(paste0(" of age ", agecurr, " of page ", pagecurr, " length of text of matches found: ",length(text_of_matches) ))
        
        text_of_matches_current_counter <- 0
        if(length(text_of_matches) > 0 ) {        # usually 28 - if not ... another way to detect if I am at the last page
          for(text_of_a_match_current in text_of_matches) {
            text_of_matches_current_counter = text_of_matches_current_counter  + 1
            
            current_match_attributes <- str_split(text_of_matches[text_of_matches_current_counter], "\n")[[1]]
            
            # IF NO ONLINE_STATUS then this will be missing
            # "5 Photos"        "Recently Online" "A Zoosk m..."    "32, New Orleans" 
            
            current_match_attributes_length <- length(current_match_attributes) 
            
            # matchname is always 2nd from the end
            current_match_matchname_short <- current_match_attributes[current_match_attributes_length -1]
            
            # her age - the end  - "32, New Orleans"
            current_match_matchname_age <- str_extract(current_match_attributes[current_match_attributes_length],"^\\d\\d")
            
            # her location - the end - "32, New Orleans"
            current_match_matchname_location <- str_replace(str_extract(current_match_attributes[current_match_attributes_length],"[ ].*"),"[ ]","")
            
            # true/false
            current_match_matchname_recent_online <- ("Recently Online" %in% current_match_attributes)
            current_match_matchname_online_now    <- ("Online Now" %in% current_match_attributes)
            
            # CURRENLY NOT USED
            # store - list of each ("5 Photos"        "Recently Online" "A Zoosk m..."    "32, New Orleans") 
            # match_attributes_list[[text_of_matches_current_counter]]            <-  current_match_attributes
            
            # would have to get match_matchname_long from the datacard page itself
            
            match_matchname_short[text_of_matches_current_counter]         <-  current_match_matchname_short
            match_matchname_recent_online[text_of_matches_current_counter] <-  current_match_matchname_recent_online
            match_matchname_online_now[text_of_matches_current_counter]    <-  current_match_matchname_online_now
            match_matchname_age[text_of_matches_current_counter]           <-  current_match_matchname_age
            match_matchname_location[text_of_matches_current_counter]      <-  current_match_matchname_location
            
          }
        }
        print("done processing the match_matchname_* attributes")
        
        print(paste0(" of age ", agecurr, " of page ", pagecurr, " length of match_matchname_data_guid_no_reencounter_global: ", length(match_matchname_data_guid_no_reencounter_global)))
        
        # keep the ones that I have not encountered before 
        # ( assumption that no two users per page have the same guid )
        keep_current_matchnames_index <- !(match_matchname_data_guid %in% match_matchname_data_guid_no_reencounter_global)
        
        print(paste0(" of age ", agecurr, " of page ", pagecurr, " sum as.integer (TRUEs) of keep_current_matchnames_index: ", sum(as.integer(keep_current_matchnames_index))))
        print(!(match_matchname_data_guid %in% match_matchname_data_guid_no_reencounter_global))
        
        
        # per page: just keep the ones that I have no encountered before
        
        match_matchname_data_guid_reduced      <-  match_matchname_data_guid[keep_current_matchnames_index]
        
        match_matchname_short_reduced          <-  match_matchname_short[keep_current_matchnames_index]           
        match_matchname_recent_online_reduced  <-  match_matchname_recent_online[keep_current_matchnames_index]  
        match_matchname_online_now_reduced     <-  match_matchname_online_now[keep_current_matchnames_index]     
        match_matchname_age_reduced            <-  match_matchname_age[keep_current_matchnames_index]             
        match_matchname_location_reduced       <-  match_matchname_location[keep_current_matchnames_index]       
        

        # per page: put the ones that I have not encountered before into a list
        
        match_matchname_no_reencounter_list <- list(
          
          match_matchname_data_guid              = match_matchname_data_guid_reduced,
          
          match_matchname_short                  = match_matchname_short_reduced,            
          match_matchname_recent_online          = match_matchname_recent_online_reduced,
          match_matchname_online_now             = match_matchname_online_now_reduced,      
          match_matchname_age                    = match_matchname_age_reduced,        
          match_matchname_location               = match_matchname_location_reduced     
        )
        
        # cummulatively per_age: 'save the ones that I have not encountered (reduced)' on this page ( for age processing later)

        match_matchname_data_guid_reduced_per_age     <- c(match_matchname_data_guid_reduced_per_age, match_matchname_data_guid_reduced) 

        match_matchname_short_reduced_per_age         <- c(match_matchname_short_reduced_per_age, match_matchname_short_reduced) 
        match_matchname_recent_online_reduced_per_age <- c(match_matchname_recent_online_reduced_per_age, match_matchname_recent_online_reduced) 
        match_matchname_online_now_reduced_per_age    <- c(match_matchname_online_now_reduced_per_age,match_matchname_online_now_reduced ) 
        match_matchname_age_reduced_per_age           <- c(match_matchname_age_reduced_per_age, match_matchname_age_reduced) 
        match_matchname_location_reduced_per_age      <- c(match_matchname_location_reduced_per_age, match_matchname_location_reduced) 
        
        # Merge Two Lists in R ( and avoid: zero-length inputs cannot be mixed with those of non-zero length )
        # http://stackoverflow.com/questions/9519543/merge-two-lists-in-r
        if(length(match_matchname_no_reencounter_list_per_age) > 0) {
          match_matchname_no_reencounter_list_per_age   <-  mapply(c,match_matchname_no_reencounter_list_per_age, match_matchname_no_reencounter_list, SIMPLIFY=FALSE)          
        } else {
          match_matchname_no_reencounter_list_per_age   <- match_matchname_no_reencounter_list
        }
        
        print(paste0(" of age ", agecurr, " of page ", pagecurr, " length of match_matchname_no_reencounter_list_per_age data_guid: ", length(match_matchname_no_reencounter_list_per_age[["match_matchname_data_guid"]])))
        
        # global: saveonly the new ones that I have not encountered before
        
        match_matchname_data_guid_no_reencounter_global <- c(match_matchname_data_guid_no_reencounter_global, match_matchname_data_guid_reduced)

        # Merge Two Lists in R ( and avoid: zero-length inputs cannot be mixed with those of non-zero length )
        # http://stackoverflow.com/questions/9519543/merge-two-lists-in-r
        if(length(match_matchname_no_reencounter_global_list) > 0) {
          match_matchname_no_reencounter_global_list  <-  mapply(c,match_matchname_no_reencounter_global_list, match_matchname_no_reencounter_list, SIMPLIFY=FALSE)
        } else {
          match_matchname_no_reencounter_global_list  <-  match_matchname_no_reencounter_list
        }

        print(paste0("of age ",agecurr, " ending page collecting information of ",pagecurr," of ",pagerange_str))
        
        # of the (possible) NEXT button
        
        NEXT_BUTTON_STATE <-c()
        # manage NEXT and NEXT_disable
        if(remDr$executeScript("return document.querySelectorAll('li.next-button.paging.disabled').length;")[[1]] != 0) {
          print("NEXT_disable is found")
          # BEGIN PROCESSING THOSE COLLECTED LINKS
          NEXT_BUTTON_STATE <-c("NEXT_DISABLED")
          # END PROCESSING THOSE COLLETED LINKS
        } else {
          print("NEXT not on PAGE ... XOR ... NEXT_enabled")
          if(remDr$executeScript("return document.querySelectorAll('li.next-button.paging').length;")[[1]] != 0) {
            print("  NEXT_enabled")
            NEXT_BUTTON_STATE <-c("NEXT_ENABLED")
          } else {
            print("  NEXT not on PAGE")
            NEXT_BUTTON_STATE <-c("NEXT_NOT_ON_PAGE")
          }
        }
        
        if(NEXT_BUTTON_STATE == "NEXT_ENABLED") {
          
          # press the NEXT button
          
          # two on page ( top and bottom )- selection does not matter - this is the top one
          webElemNEXTBTN <- remDr$findElement("css selector", "li.next-button.paging")
          webElemNEXTBTN$highlightElement()
          webElemNEXTBTN$clickElement()
          print("just pressed the NEXT button")
          Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))
          # NEXT button was pressed
          next; # for(pagecurr in pagerange)
        } else { # NEXT_BUTTON_STATE != "NEXT_ENABLED"
          print(paste0("ending age collecting information of age ", agecurr," of ",agerange_str))
          break; # for(pagecurr in pagerange)
                # below: begin visiting/messaging matches_per_age
        }
        
        
      } # for(pagecurr in pagerange)
      
  
      # per matchname(tile) on all pages in this age range 
      # possible visits/messages
      
      print(paste0("begin visit/message of age ",agecurr," of ",agerange_str))
      
      match_matchname_no_reencounter_list_per_age_data_guid_current_counter <- 0
      for(match_matchname_no_reencounter_list_per_age_data_guid_current in match_matchname_no_reencounter_list_per_age[["match_matchname_data_guid"]]) {
        match_matchname_no_reencounter_list_per_age_data_guid_current_counter <- match_matchname_no_reencounter_list_per_age_data_guid_current_counter + 1
        
        
        # reduced by ('online now' or ('online now' or 'online recent')))
        #
        # NOTE: this does NOTE update: data_guid_already_visited (see below)
        # 
        
        if(online_when == "online_now") {
          # not on GRID COLLECTED PAGE found: 'Online Now'"
          if(!match_matchname_no_reencounter_list_per_age[["match_matchname_online_now"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]){
            next; # so do not visit(at least)
          }
        }
        
        # default "within_the_last_week"  # NOTE:IS ACTUALLY ONLINE_RECENT
        if(online_when == "within_the_last_week"){ 
          # not on GRID COLLECTED PAGE found: ( 'Online Now' OR "Recently Online' )
          if(!(match_matchname_no_reencounter_list_per_age[["match_matchname_recent_online"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]  ||
               match_matchname_no_reencounter_list_per_age[["match_matchname_online_now"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter])
          ){
            next; # so do not visit(at least)
          }
        }

        # NOTE: short_name is 'probably' not unique
        
        # TEMPORARILY - match by guid
        # ACTUALLY do not vst/msg # EXPLICIT by ME
        #
        # NOTE: this does NOTE update: data_guid_already_visited (see below)
        # 
        if(match_matchname_no_reencounter_list_per_age_data_guid_current %in% do_not_vst_msg) { # currenly guids ( TEMP )
          print(paste0("data_guid ", match_matchname_no_reencounter_list_per_age_data_guid_current, " is explicit: do not vst/msg"))
          print(paste0("therfore, skipping to the next user data_guid"))
          next; #  for(match_matchname_no_reencounter_list_per_age_data_guid_current in match_matchname_no_reencounter_list_per_age[["match_matchname_data_guid"]])
                #  so do not visit(at least)
        }
        
        # OTHER 'do not message match methods'? LATER (while on her page) by long_name
        # ...
        
        # actually visit the username
        # actually visit ( in zk a 'message' always includes a 'visit')
        # regular visit
        if( !(match_matchname_no_reencounter_list_per_age_data_guid_current %in% data_guid_already_visited) ) {
          
          # DEFAULT action = "just_visit" ( action = "message_greet_matchname" INCLUDES action = "just_visit" )
          #
          print(paste0("of age ",agecurr, " of ", agerange_str, " begin url nav to  ", match_matchname_no_reencounter_list_per_age_data_guid_current,"  ", match_matchname_no_reencounter_list_per_age[["match_matchname_short"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]))
          remDr$navigate(paste0("https://www.zoosk.com/personals/datecard/",match_matchname_no_reencounter_list_per_age_data_guid_current,"/about"))
          Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) 
          this_age_visit_number_counter <- this_age_visit_number_counter + 1
          print(paste0("  This had been visit number ", this_age_visit_number_counter," of age ", agecurr))
          
          
          if( action == "message_greet_matchname" ) {
            
            # collect her 'long name' (prob unique)? but can login with email ( found per page )
            # but since I login with facebook/google/email - even the long name is not unique?
            #
            # NOTE: this(currently) is only needed for the messaging
            #
            
            # zk: long name is only seen in the profile 
            
            webElemLONGMATCHNAME <-remDr$findElement("css selector", ".profile-header-text")
            webElemLONGMATCHNAME$highlightElement()
            
            # "Profile: Geaux Geaux Gal" ( for her name in a message )
            match_matchname_long_current  <- str_replace(str_extract(webElemLONGMATCHNAME$getElementText()[[1]],":[ ].*"),":[ ]","")
            
            # c(", happy Wednesday! How are you doing today?") -> message_greet_matchname_vector

            # from function call actual argument
            todays_message -> message_greet_matchname_vector
            
            current_message  <- paste0(match_matchname_long_current, message_greet_matchname_vector)
            
            print(paste0("Begin attempt to send message to ", match_matchname_no_reencounter_list_per_age_data_guid_current," aka ", match_matchname_long_current, " with her age ", match_matchname_no_reencounter_list_per_age[["match_matchname_age"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]))            
            writeLines(current_message)
            
            # actually (attempt to) send THE message
            
            # AT THIS POINT - FROM ABOVE
            # I have already filtered out ( on the GRID ) the 
            # survivors (as applicable): *online now* OR *recenly online* 
            
            # NOTE: CODE currenly only implemented for *online now*
            # 
            
            if(online_when == "online_now") {
              
              # CURRENLY MOST RELIABLE
              #
              # (green cirle) Chatting
              # 
              # DETECT *online now* on HER PAGE
              #   if she has  ... (green cirle) Chatting ... then she is 'ONLY *online now*'
              # online - exact css only found only on HER PAGE when she is *online now*
              # <span class="online-indicator-status">Chatting</span>
              if(remDr$executeScript("return document.querySelectorAll('span.online-indicator-status').length;")[[1]] != 0) {
                # FOUND (green cirle) Chatting
                
                # put in message
                
                # index number 1 or 2 ( no logic which - but always the last one found in the DOM )
                # plural
                webElemSMALLCHATAREA <- remDr$findElements("css selector", "input.quick-message-text")
                webElemSMALLCHATAREA <- webElemSMALLCHATAREA[[length(webElemSMALLCHATAREA)]]
                webElemSMALLCHATAREA$highlightElement() # WORKS KEEP
                webElemSMALLCHATAREA$sendKeysToElement(list(current_message)) # WORKS ( AS LONG AS I DO NOT GET THAT STUPID' UPLOAD PHOTO POPUT )
                # note if 'chat area stays gray', it seems to be some sort 
                #   of unknown logic 'anti-spamming' software 
                # My best deal - use her name in front AND keep my sentence long in words
                
                # actually send
                
                # on the right, the blue button 
                # index number 1 or 2 ( no logic which - but always the last one found in the DOM )
                # plural
                # <span tabindex="0" data-zat="send-message-button" style="-moz-user-select: none;" class="minor-button-confirm send-chat"><span  class="button-icon"></span><span style="display: none;" class="processing-bar"></span></span>
                webElemSMALLCHATAREASENDBTN <-remDr$findElements("css selector", "span.minor-button-confirm.send-chat span.button-icon")
                webElemSMALLCHATAREASENDBTN <- webElemSMALLCHATAREASENDBTN[[length(webElemSMALLCHATAREASENDBTN)]]
                webElemSMALLCHATAREASENDBTN$highlightElement() # WORKS
                webElemSMALLCHATAREASENDBTN$clickElement() # WORKS - if does not send ( then its because 'chat area' above is grayed out )
                Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) 
              }
              
            }
            
            # CURRENTLY ONLY OTHER online_when VALUE 
            if(online_when == "within_the_last_week") { # all other cases when SHE IS NOT *online now*
              
              if(remDr$executeScript("return document.querySelectorAll('span.online-indicator-status').length;")[[1]] == 0) {
                # NOT FOUND   (green cirle) Chatting
                
                # NOTE: THIS CODE DOES NOT WORK - THIS CODE IS NOT IMPLEMENTED
                # NOTE: THIS CODE DOES NOT WORK - THIS CODE IS NOT IMPLEMENTED
                
                # RESEARVED FOR 'FUTURE CODING'
                
                # empty white zone
                webElemSMALLINITCHATAREA <- remDr$findElement("css selector", "p.chat-encourage-initial")
                webElemSMALLINITCHATAREA$highlightElement() 
                webElemSMALLINITCHATAREA$clickElement() # pop-up bigger box
                
                # in pop-up bigger box
                webElemSMALLINITCHATAREAPOPUPBOX <- remDr$findElement("css selector", "textarea.quick-message-text")
                webElemSMALLINITCHATAREAPOPUPBOX$highlightElement() 
                
                webElemSMALLINITCHATAREAPOPUPBOX$clickElement() # DOES NOTHING ?
                
                # try to enter text in the txtarea
                
                js_enter_words_textarea_box <- paste0("try{ retvalue = document.querySelector('textarea.quick-message-text').value = '", current_message,"'; return 0 } catch(err) { return -1 };")
                remDr$executeScript(paste0(js_enter_words_textarea_box))[[1]] # WORKS RETURNS 0 ( AND VISUALLY PUTS "Hello..."(in light gray) BOX)
                #
                #  *** NOW HUMAN *** (HAS TO BE DONE) HUMAN MANUALLY PLACES *MOUSE CURSOR* IN THE BOX ( HAVE NOT FIGURED THIS ONE OUT *YET* ) ( IF I EVER CARE TO )
                #
                # NOW (in white) BOX)
                webElemSMALLINITCHATAREAPOPUPBOX$sendKeysToElement(list(current_message)) # WILL WRITE
                
                webElemSMALLINITCHATAREAPOPUPBOXSENDBTN <- remDr$findElement("css selector", "div.chat-messenger-actions span.control-button-confirm")
                webElemSMALLINITCHATAREAPOPUPBOXSENDBTN$highlightElement() 
                webElemSMALLINITCHATAREAPOPUPBOXSENDBTN$clickElement()   
                
              }  
              
            }
            
            # End of 'actually  (attempt to) send THE message'
            
            print(paste0("End attempt to send message to ", match_matchname_no_reencounter_list_per_age_data_guid_current," aka ", match_matchname_long_current, " with her age ", match_matchname_no_reencounter_list_per_age[["match_matchname_age"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]))            
          }
          
          # update - I have already visited/messaged
          data_guid_already_visited <- c(data_guid_already_visited, match_matchname_no_reencounter_list_per_age_data_guid_current)
          print(paste0("of age ",agecurr," of ", agerange_str, "   end url nav to  ", match_matchname_no_reencounter_list_per_age_data_guid_current,"  ", match_matchname_no_reencounter_list_per_age[["match_matchname_short"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]))

          
        } else { # zk - do not accidentally visit a 2nd time
          print(paste0("of age ",agecurr, " of ", agerange_str, " SKIPPING(already visited) url nav to  ", match_matchname_no_reencounter_list_per_age_data_guid_current,"  ", match_matchname_no_reencounter_list_per_age[["match_matchname_short"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]))
        }
        
        print(paste0("end visit/message of age ",agecurr," of ",agerange_str))
        
      } # for(match_matchname_no_reencounter_list_per_age_data_guid_current in match_matchname_no_reencounter_list_per_age[["match_matchname_data_guid"]])
      
 
      # NOT PART
      print(paste0("ending age ", agecurr," of ",agerange_str))
      
      print("Of THIS progrem, the user hand written call follows.")
      print(looper_typed_in_call) # language
      
    } # for(agecurr in agerange)
    
    bookmarkhere <- 1
    
    # manually logout of pof cupid here 
    # manually X out ( shutdown ) the browser
    
    # often I may want this to be FALSE
    if( on_exit_logoff_site == TRUE ) {
      
      remDr$navigate("https://www.zoosk.com/zoosk-on-the-go?notify=logout&from=user-menu")
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
      remDr$navigate("http://www.apple.com")
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
      # result = tryCatch({ remDr$closeServer() }, warning = function(w) {}, error = function(e) {}, finally = {})
      # IF I really! want to stop
      manageRSeleniumServer(curr_port = curr_port, doif = "if_server_not_stopped_please_stop")
      print("end closeServer remDr")
      
    }

    dbDisconnect(con)
    Sys.setenv(TZ=oldtz)

    return(remDr)
    
  }
  remDr <- maininner()
  return(remDr)
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
# debugSource('J:/YDrive/All_NewSeduction/All_ElectronicSpeech/RSeleniumAndBrowsers/AES1/zk_visit_looper_dev.R')

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

# just visit
# zk_visit_looper_dev() # ( MAIN - JUST VISIT - WITHIN THE LAST WEEK )
# zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") # default

# possible testing
# zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), action = "message_greet_matchname", online_when = "_ANY_", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# possible testing
# zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), action = "just_visit", online_when = "_ANY_", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# main send message testing ( this one )
# possible testing
# zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "anything")

# or if 'so few' online, then it is better to send THIS one ( MAIN - SEND MESSAGE - ONLINE_NOW )
# send a message ( if zk does not have a 'restriction on the number of messages sendable')
# zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "anything")

# WONT BE USED IN PRODUCTION if I do not find I way to send messages to those NOT(*online now*)
# JUST TEST THE 'CODE ZONE'
# zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), action = "message_greet_matchname", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "anything")

# send a message ( if zk has a 'restriction on the number of messages sendable')
# zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * (-5)), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# END INSTRUCTIONS      
# END INSTRUCTIONS       




