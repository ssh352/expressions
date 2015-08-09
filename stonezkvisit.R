

# SEARCH FOR # LEFT_OFF ( GET HER REAL NAME )

# DON'T FORGET TO STARTUP PostgreSQL !!

# 
# CREATE TABLE aes_do_not_visit_list
# (
#   id            double precision  NOT NULL,
#   match_source  text,
#   my_matchname  text,
#   her_matchname text,
#   her_age       integer,
# )
# WITH (
#   OIDS=FALSE
# );
# ALTER TABLE aes_do_not_visit_list
# OWNER TO postgres;
# 
# CREATE UNIQUE INDEX aes_do_not_visit_list_pk_idx  
# ON aes_do_not_visit_list
# USING btree (id);  
# 
# ALTER TABLE aes_do_not_visit_list
# ADD CONSTRAINT aes_do_not_visit_list_pk PRIMARY KEY  USING INDEX aes_do_not_visit_list_pk_idx;  
# 
# -- NOTE: now the index will dissapear from the GUI ( but the pk properties showed its there ( e.g. btree ) )
#                                                      
# REINDEX TABLE aes_do_not_visit_list; -- simply rebuild that index
#                                                      
# 
# CREATE TABLE aes_have_visited_list
# (
#   id            double precision  NOT NULL,
#   match_source  text,
#   my_matchname  text,
#   her_matchname text
# )
# WITH (
#   OIDS=FALSE
# );
# ALTER TABLE aes_have_visited_list
# OWNER TO postgres;
# 
# CREATE UNIQUE INDEX aes_have_visited_list_pk_idx  
# ON aes_have_visited_list
# USING btree (id);  
# 
# ALTER TABLE aes_have_visited_list
# ADD CONSTRAINT aes_have_visited_list_pk PRIMARY KEY  USING INDEX aes_have_visited_list_pk_idx;  
# 
# -- NOTE: now the index will disappear from the GUI ( but the pk properties showed its there ( e.g. btree ) )
#                                                      
# REINDEX TABLE aes_have_visited_list; -- simply rebuild that index
#                                                      

# CREATE TABLE aes_have_sent_message_list   
# (
#   id double precision NOT NULL,
#   match_source text,
#   my_matchname text,
#   her_matchname text,
#   her_age integer,
#   sent_message text
# )
# WITH (
#   OIDS=FALSE
# );
# ALTER TABLE aes_have_sent_message_list
# OWNER TO postgres;
# 
# CREATE UNIQUE INDEX aes_have_sent_message_list_pk_idx  
# ON aes_have_sent_message_list
# USING btree (id);  
# 
# ALTER TABLE aes_have_sent_message_list
# ADD CONSTRAINT aes_have_sent_message_list_pk PRIMARY KEY USING INDEX aes_have_sent_message_list_pk_idx; 
# 
# -- NOTE: now the index will disappear from the GUI ( but the pk properties showed its there ( e.g. btree ) )
#                
# REINDEX TABLE aes_have_sent_message_list; -- simply rebuild that index
# 

# install.packages("checkpoint")    

# checkpoint(snapshotDate, project = getwd(), R.version, 
#           scanForPackages = TRUE, checkpointLocation = "~/", verbose = TRUE, 
#           use.knitr = system.file(package = "knitr") != "")  

# INSTALLING 
# checkpoint("2015-05-09", R.version = "3.2.0")  
# COMMON EVERYDAY DEBUGGING 
# I do not want it to scan every time
### checkpoint("2015-05-09", R.version = "3.2.0", scanForPackages = FALSE)


# shell("rstudio", wait=FALSE)  

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) 
options(digits.secs = 6)
options(error=NULL) # options(error = recover) 

safe_navigate_to_new_url <- function(new_url = NULL, remote_driver = NULL, after_how_long = NULL, backout_url = NULL) {
  
  require(tcltk)
  require(RSelenium)
  
  # NOTE: 'note very case is 'code covered' BUT IF A PROBLEM SHOULD BE FIXABLE  
  
  # if browser/site/internet hangs just ...
  # backout_url: "current_url", "goback" "http://www.time.gov"(default) "CUSTOMHTTP" 
  # NOTE: to retry JUST ONCE more: call by 'new_url == backout_url'
  
  #  1000:  good redirect success test
  # 30000:  should never do 'backout_url' ( 25 seconds )
  if(is.null(after_how_long)) { after_how_long <- 25000 } # 30 seconds 
  
  backout_url_set <- FALSE
  if(is.null(backout_url))                                        { backout_url_value <- "http://www.time.gov"  ; backout_url_set <- TRUE } # default
  if(!is.null(backout_url) && backout_url == "gobackgoforward")   { backout_url_value <- "gobackgoforward"      ; backout_url_set <- TRUE }
  if(!is.null(backout_url) && backout_url == "refreshgoforward")  { backout_url_value <- "refreshgoforward"     ; backout_url_set <- TRUE }
  if(!is.null(backout_url) && backout_url == "refresh")           { backout_url_value <- "refresh"              ; backout_url_set <- TRUE }
  if(!is.null(backout_url) && backout_url == "current_url")       { backout_url_value <- "current_url" ; backout_url_set <- TRUE }
  if(!is.null(backout_url) && !isTRUE(backout_url_set))           { backout_url_value <- backout_url   ; backout_url_set <- TRUE }
  
  if(!isTRUE(backout_url_set))  stop(paste0("safe_navigate_to_new_url call is missng a 'good backout_url'")) 
  
  # tcl: functon call does  not 'seem to be allowed to have any parameters ( rely on 'scope and visibility' )
  safe_navigate_backout <- function() { 
    
    # SHOULD be TRUE here
    if(isTRUE(backout_url_set)) {
      
      if(backout_url_value == "gobackgoforward")    { print("GOBACKGOFORWARD")       ; remote_driver$goBack();              remote_driver$goForward();            return() }
      if(backout_url_value == "refreshgoforward")   { print("REFRESHGOFORWARD")      ; remote_driver$refresh();                                                   return() }
      if(backout_url_value == "refresh")            { print("REFRESH")               ; remote_driver$refresh();             remote_driver$goForward();            return() }
      if(backout_url_value == "current_url")        { print("NAVIGATE(CURRENT_URL")  ; remote_driver$navigate(current_url);                                       return() }
      print("NAVIGATE(BACKOUT_URL_VALUE")  ;remote_driver$navigate(backout_url_value) 
      return() 
      
    } else {
      stop("in safe_navigate_backout NOT 'isTrue(backout_url_set)'") 
    }
    
  }
  
  # NOT SURE of the 'tcl and safe_navigate_backout closure' rules, so I put this here
  current_url <- remote_driver$getCurrentUrl()[[1]][1]
  
  # register task
  .id <-tcl("after", after_how_long, safe_navigate_backout) 
  
  # To get info about this scheduled task
  print(paste0("tcl_after_info_id: ",tcl("after", "info", .id)))   
  
  # regular run
  remote_driver$navigate(new_url)
  
  # if hung after "after_how_long" milliseconds, will run (OTHER TRY): safe_navigate_backout
  
  # if I made it THIS far: "remote_driver$navigate(new_url)" ran, so then just cancel
  # cancel the currently scheduled task
  result = tryCatch({ tcl("after", "cancel", .id) }, warning = function(w) {}, error = function(e) {}, finally = {})
  # unfortunately alwayS returns success: <Tcl> 
  
  new_url     <- remote_driver$getCurrentUrl()[[1]][1]
  
  # success if I navigated forward
  return(list( success =(new_url != current_url), remote_driver = remote_driver  ))
  
}



zk_visit_looper_dev <- function(curr_port = 4471, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") { 
  # OR action = "message_greet_matchname" "message_random_catchphrase"
  # OR not_to_msg = "all_all"
  # CONSIDER other PARAMETERS: , online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE"
  
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
      startServer(args = c(paste0("-port ", curr_port),"-timeout 3600","-browserTimeout 3600"))  # default # 4444 # java -jar selenium-server-standalone.jar -h
      Sys.sleep(5.0) # 5 second wait
    
    # CURRENTLY - zk WILL NOT USE A PROFILE - selenium/chrome - NOT_WORK_profile_MULTI_remDrS 

      browser_profile_dir_path   <- "J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data"
      browser_profile            <- "epoch536intel_epoch536intel"
#     browser_profile_file       <- "Preferences"
#     browser_profile_file_GOOD  <- "Preferences.CORRECT_HOME_PAGE_AND_IMAGES"    
#     
#     browser_pref_conf_file_name <- paste0(browser_profile_dir_path, "\\", browser_profile, "\\" , browser_profile_file)
#     browser_pref_conf_file_name_GOOD <- paste0(browser_profile_dir_path, "\\", browser_profile, "\\" , browser_profile_file_GOOD)
    
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

#     print("Begin browser pref conf file name GOOD copy ")
#     file.copy(from = browser_pref_conf_file_name_GOOD
#               , to   = browser_pref_conf_file_name
#               , overwrite = TRUE, copy.date = TRUE
#     ) -> file_copy_success # return TRUE/FALSE
#     if(!isTRUE(file_copy_success)) { stop("Preferences file copy failed!")  }
#     # if 'from' file not found, it will * silently fail *
#     Sys.sleep(5.0) # flush time
#     print("End browser pref conf file name GOOD copy ")
    
    # CURRENTLY - zk WILL NOT USE A PROFILE - selenium/chrome - NOT_WORK_profile_MULTI_remDrS 

    # cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "epoch536intel_epoch536intel") 
    cprof <- getChromeProfile(browser_profile_dir_path, browser_profile)
    remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof, port = curr_port) # default 4444
    
    # MAYBE FREEZES IF IT DOES NOT HAVE A PROFILE?
    # remDr <- remoteDriver(browserName = "chrome", port = curr_port)

    # not required firefox does native events "MANUALLY type remDr to SEE"
    # remDr <- remoteDriver(port= curr_port, nativeEvents = TRUE) # RSelenium-basics.html async javascript PROBLEM firefox?
    # OVERRIDE
    # OVERRIDE
    # OVERRIDE - CURRENLY ONLY PRACTICING WITH FIREFOX
    # FIREFOX
    # nativeEvents = XXXX # because DEFAULT may be platform specific
    remDr <- remoteDriver(port= curr_port, nativeEvents = TRUE)  # SYNC/ASYNC PAGE LOADING?
    # remDr <- remoteDriver(port= curr_port, nativeEvents = FALSE)  # SYNC/ASYNC PAGE LOADING? - NO AFFECT

    print(paste0("PORT ", curr_port))
    
    # NOTE: POF # stores 'preferences(cookie-ish) on its servers in Vancouver '
    # pof has DEEP memory
  
    remDr$open() # <somewhere>.com (PROFILE) # data; (NO PROFILE) 
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    

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
    webElemEMAIL$sendKeysToElement(list("epoch536intel@gmail.com"))
    Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))

    webElemEMAILPASS <- remDr$findElement("name", "password")
    webElemEMAILPASS $highlightElement()
    webElemEMAILPASS$sendKeysToElement(list("739heg08"))
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

    # NOTE: zk: 
    # I MUST (diff from okcupid and pof) LOOP THROUGH ALL PAGES (at least per age ) 
    #        AND COLLECT THE ATTRIBUTES 
    #   before visting anyone. If I manually browse to  "http... page=1", then page data 
    #      will recycle after the 3rd page '4th page = 1st page" WHICH IS FALSE
    
    # MAGIC NUMBER  
    # zk ( ANY numbers: minage < me < maxage ) ??
    
    agerange     <-  18:20      #  30:31  # 45:44   c(25:18,50:31) "25:18,50:31" # 
    agerange_str <- "18:20"     # "30:31" # 45:44

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
      
      remDr$navigate("https://www.zoosk.com/personals/search/edit")
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # TIME CONSUMING
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
      
      remDr$navigate("https://www.zoosk.com/personals/search/edit")
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # TIME CONSUMING
      
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
          if(!match_matchname_no_reencounter_list_per_age[["match_matchname_online_now"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]){
            next; # if not online_when == "online_now"(TRUE) THEN next
                  # so do not visit(at least)
          }
        }
        
        # default "within_the_last_week"  # NOTE:IS ACTUALLY ONLINE_RECENT
        if(online_when == "within_the_last_week"){ 
          if(!(match_matchname_no_reencounter_list_per_age[["match_matchname_recent_online"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]  ||
               match_matchname_no_reencounter_list_per_age[["match_matchname_online_now"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter])
          ){
            next; # if niether online_when == "within_the_last_week"(TRUE) nor online_when == "online_now"(TRUE) THEN next
                  # so do not visit(at least)
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
          
          # collect her 'long name' (prob unique)? but can login with email ( found per page )
          # but since I login with facebook/google/email - even the long name is not unique?
          #
          # NOTE: this(currently) is only needed for the messaging could/should move into message_greet_matchname" BELOW
          #
          webElemLONGMATCHNAME <-remDr$findElement("css selector", ".profile-header-text")
          webElemLONGMATCHNAME$highlightElement()
          
          # "Profile: Geaux Geaux Gal" ( for her name in a message )
          match_matchname_long_current  <- str_replace(str_extract(webElemLONGMATCHNAME$getElementText()[[1]],":[ ].*"),":[ ]","")
          
          if( action == "message_greet_matchname" ) {
            
            c(", happy Xday! How are you doing today?") -> message_greet_matchname_vector
            
            # zk: long name is only seen in the profile 
            
            current_message  <- paste0(match_matchname_long_current, message_greet_matchname_vector)
            
            print(paste0("Begin attempt to send message to ", match_matchname_no_reencounter_list_per_age_data_guid_current," aka ", match_matchname_long_current, " with her age ", match_matchname_no_reencounter_list_per_age[["match_matchname_age"]][match_matchname_no_reencounter_list_per_age_data_guid_current_counter]))            
            writeLines(current_message)
            
            # actually send THE message
            
            # if she 'does NOT have a button to send the message'
            
            if(remDr$executeScript("return document.querySelectorAll('p.chat-encourage-initial').length;")[[1]] != 0) {
              
              webElemSMALLINITCHATAREA <- remDr$findElement("css selector", "p.chat-encourage-initial")
              webElemSMALLINITCHATAREA$highlightElement() # DID A TEST - I DO NOT REMEMBER
              # write
              Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
              
            }
            
            # I THINK - EITHER - above OR below
            
            # if she 'has a button to send the message'
            
            if(remDr$executeScript("return document.querySelectorAll('input.quick-message-text').length;")[[1]] != 0) {
              
              # online use chat box
              
              webElemSMALLCHATAREA <-remDr$findElement("css selector", "input.quick-message-text")
              webElemSMALLCHATAREA$highlightElement()
              # WORKS ( AS FAR AS I CAN TELL )
              # write ...
              Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
              
              # online user send chat button
              
              if(remDr$executeScript("return document.querySelectorAll('span.minor-button-confirm.send-chat').length;")[[1]] != 0) {
                
                webElemSENDCHATBUTTON <-remDr$findElement("css selector", "span.minor-button-confirm.send-chat")
                webElemSENDCHATBUTTON$highlightElement()
                # WORKS ( AS FAR AS I CAN TELL )
                # write ...
                Sys.sleep(1 + 1 * runif(1, min = 0, max = 1))
                
              }
              
            }
            
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
      
      
    } # for(agecurr in agerange)
    
    bookmarkhere <- 1
    
    # manually logout of pof cupid here 
    # manually X out ( shutdown ) the browser
    
    remDr$navigate("https://www.zoosk.com/zoosk-on-the-go?notify=logout&from=user-menu")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
    
    remDr$navigate("http://www.apple.com")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
    
    print("begin closing remDr")
    remDr$close() 
    print("end closing remDr")
    
    bookmarkhere <- 1
    
    print("begin closeServer remDr")
    result = tryCatch({ remDr$closeServer() }, warning = function(w) {}, error = function(e) {}, finally = {})
    print("end closeServer remDr")
    
    dbDisconnect(con)
    Sys.setenv(TZ=oldtz)
    
  }
  maininner()
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
# netstat -o -a -b  -n | find /i "listening" | find /i ":4471" 
# taskkill /F /T /PID <above_right_col_number>
# XOR
# SEND MESSAGE TO 'ONLINE NOW'
# netstat -o -a -b  -n | find /i "listening" | find /i ":4472"
# taskkill /F /T /PID <above_right_col_number>

# MANUALLY PLACE DOWN THE BREAKPOINT
#   e.g. remDr$open() # oracle.com  

# just visit
# zk_visit_looper_dev()
# zk_visit_looper_dev(curr_port = 4471, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") # default

# possible testing
# zk_visit_looper_dev(curr_port = 4472, action = "message_greet_matchname", online_when = "_ANY_", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# possible testing
# zk_visit_looper_dev(curr_port = 4472, action = "just_visit", online_when = "_ANY_", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# send a message
# zk_visit_looper_dev(curr_port = 4472, action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# END INSTRUCTIONS       
# END INSTRUCTIONS        

