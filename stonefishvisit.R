

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
options(error = recover)

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



pof_visit_looper_dev <- function(curr_port = 4461, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") { 
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
    
    # netstat -o -a -b  -n | find /i "listening" | find /i ":4461" 
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
#         
      startServer(args = c(paste0("-port ", curr_port),"-timeout 3600","-browserTimeout 3600"))  # default # 4444 # java -jar selenium-server-standalone.jar -h
      Sys.sleep(5.0) # 5 second wait
    
    
    browser_profile_dir_path   <- "J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data"
    browser_profile            <- "era674smartie_era674smart"
    browser_profile_file       <- "Preferences"
    browser_profile_file_GOOD  <- "Preferences.CORRECT_HOME_PAGE_AND_IMAGES"    
    
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
    
    print("Begin browser pref conf file name GOOD copy ")
    file.copy(from = browser_pref_conf_file_name_GOOD
              , to   = browser_pref_conf_file_name
              , overwrite = TRUE, copy.date = TRUE
    ) -> file_copy_success # return TRUE/FALSE
    if(!isTRUE(file_copy_success)) { stop("Preferences file copy failed!")  }
    # if 'from' file not found, it will * silently fail *
    Sys.sleep(5.0) # flush time
    print("End browser pref conf file name GOOD copy ")
    
    # cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "era674smartie_era674smart") 
    cprof <- getChromeProfile(browser_profile_dir_path, browser_profile)
    remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof, port = curr_port) # default 4444
    
    print(paste0("PORT ", curr_port))
    
    # NOTE: POF # stores 'preferences(cookie-ish) on its servers in Vancouver '
    # pof has DEEP memory
  
    remDr$open() # hp.com  
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    
    print(paste0("PORT ", curr_port))
    print("(tried) opened browser home page")
    
    result = tryCatch({ remDr$navigate("http://www.pof.com/abandon.aspx") }, warning = function(w) {}, error = function(e) {}, finally = {})
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) 
    
    print("(tried) logout from pof")
    
    # Sys.sleep(1 + 5* runif(1, min = 0, max = 1)) # 6 to 11 seconds wait
    
    remDr$navigate("http://www.pof.com/inbox.aspx")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    remDr$navigate("http://www.pof.com/inbox.aspx") # POF SPECFIC?
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    print("navigated to pof")
    
    # cookie-ish tends to remember previous input#logincontrol_username and put it there.  
    # I want to clear this
    js_clear_login_box <- "try{ retvalue = document.querySelectorAll('input#logincontrol_username')[0].value = ''; return 0 } catch(err) { return -1 };"
    remDr$executeScript(paste0(js_clear_login_box))[[1]]
    
    webElem1 <- remDr$findElement("css selector", "input#logincontrol_username")
    webElem1$sendKeysToElement(list("era674smart")) # NOTE: BELOW: MAYBE DANGEROUSLY HARDCODED IN a message to here
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2 <- remDr$findElement("css selector", "input#logincontrol_password")
    webElem2$sendKeysToElement(list("739heg08"))
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds 
    
    webElem2$sendKeysToElement(list(key="enter"))
    Sys.sleep(4 + 2 * runif(1, min = 0, max = 1)) # 5 to 10 seconds wait
    
    print("logged into pof")
    
    # MAGIC NUMBER 
    # pof
    # 45 is the maximum age for a 31 year old
    #  else it defaults to 'a big age range'
    # 31 YEAR OLD - SEARCH ON * age * WILL FAIL - '22 is first success' '45 is the last success'
    #    SAFE MIN: 23 SAFE MAX 44

    agerange     <-  23:44      #  30:31  # 45:44   c(25:18,50:31) "25:18,50:31" # 
    agerange_str <- "23:44"     # "30:31" # 45:44    
    
    # POF limit of 55 messages per 24 hour period
    true_attempted_send_message_ANDRE_this_session_run_limit <- 40
    true_attempted_send_message_count <- 0

    usernamename_already_visited <-c() # pof is extremely page dynamic: I do not want to visit a person accidentally twice
    for(agecurr in agerange) { # testing only 31 and 30 # 31:30    
      
      if(true_attempted_send_message_ANDRE_this_session_run_limit == true_attempted_send_message_count) {
        print(paste0("sent message: Andre this session run limit reached - break from all ages loop"))
        break;
      }
      
      # per age
      no_more_online_when <- FALSE
      
      print(Sys.time())
      print(paste0("PORT ", curr_port))
    
      print(paste0("beginning age ",agecurr," of ",agerange_str))
      
      pagerange <-      1:20      
      pagerange_str <- "1:20"    
      
      for(pagecurr in pagerange) {
        
        if(true_attempted_send_message_ANDRE_this_session_run_limit == true_attempted_send_message_count) {
          print(paste0("sent message: Andre this session run limit reached - break from pagerange of this age loop"))
          break;
        }
        
        if(no_more_online_when == TRUE) {
          print(paste0("ending age ", agecurr," of ",agerange_str))
          break # out of the page loop # since 'online_when' THIS IS 'per age', go to the next age ( above )
        }
        

        # default
        if( body_type == "anything" ) {
          
          # age1 to age1
          # 70002 - 50 miles
          # Body Type: Anything DEFAULT ( but I could sort by 'Newest Users')
          # Sort By: 'Last Visit' ( but I could sort by 'Newest Users')
          navigate_target_age_current_page <- paste0("http://www.pof.com/advancedsearch.aspx?iama=m&minage=",agecurr,"&maxage=",agecurr,"&city=70002&seekinga=f&searchtype=&country=1&heightb=999&maritalstatus=&relationshipage_id=&starsign=&body=&smarts=&fishtype=&pets=&eyes_id=&religionmult=&starsignmult=&thnicitymult=&haircolormult=&income=&profession_id=&Family_id=&intent=&easygoing_id=&confidence_id=&openness_id=&haircolor=&religion=&miles=50&page=",pagecurr,"&count=700")
        
        }
  
        if( body_type == "thin_athletic" ) {
        
          # SINCE I ONLY HAVE A LIMIT OF 55 NEW USERS EVER CONTACTED IN A 24 HOUR PERIOD, I MIGHT AS WELL MAKE THEM COUNT
          # age1 to age1
          # 70002 - 50 miles
          # Body Type: Thin, Athletic
          # Sort By: 'Last Visit' DEFAULT ( but I could sort by 'Newest Users')
          navigate_target_age_current_page <- paste0("http://www.pof.com/advancedsearch.aspx?iama=m&minage=",agecurr,"&maxage=",agecurr,"&state=26&city=70002&seekinga=f&searchtype=&country=1&heightb=999&maritalstatus=&relationshipage_id=&starsign=&body=1_2&smarts=&fishtype=&pets=&eyes_id=&religionmult=&starsignmult=&thnicitymult=&haircolormult=&income=&profession_id=&Family_id=&intent=&easygoing_id=&confidence_id=&openness_id=&haircolor=&religion=&miles=50&page=",pagecurr,"&count=700")
        
        }
        
        print(paste0("body type is ", body_type))
        print(paste0("CURRENT URL"))
        print(navigate_target_age_current_page)
        
        
        print(paste0("of age ",agecurr, " beginning page ",pagecurr," of ",pagerange_str))
        
        remDr$navigate(navigate_target_age_current_page)
        Sys.sleep(5 + 3 * runif(1, min = 0, max = 1)) # 5 to 8 seconds wait ( POF catch up?) 
        remDr$refresh() # may or may not help ( SUDDEN case of empty page ?? )
        # NOTE: manual message management: go to a new tab of the chrome browser ( not to a new opera )
        
        usernamecount <- remDr$executeScript("return document.querySelectorAll('div.about').length;")[[1]] # NOTE: ADD ERROR HANDLING LATER
        if( usernamecount == 0 ) {
          print(paste0("no usernames found of of age ",agecurr, "ending page ",pagecurr," of ",pagerange_str))
          print(paste0("of age ",agecurr, "ending page ",pagecurr," of ",pagerange_str))
          break; # pagecurr in pagerange, GOTO NEXT agecurr in agerange
          
        } else {
          
          
          usernamerange <- 1:usernamecount
          pagerange_str <- paste0(1,":",usernamecount)
          
          
          # collect the page information
          usernameurl         <- c()
          usernamename        <- c()
          usernamequalities   <- list()
          
          for(usernamecurr in usernamerange) {
            
            usernameurl[usernamecurr]        <- remDr$executeScript(paste0("return document.querySelectorAll('div.about > a.link')[",(usernamecurr - 1),"].href;"))[[1]]
            
            usernamename[usernamecurr]       <- remDr$executeScript(paste0("return document.querySelectorAll('div.about > a.link')[",(usernamecurr - 1),"].innerText;"))[[1]]
            
            usernamequalities[[usernamecurr]] <- remDr$executeScript(paste0("return document.querySelectorAll('div.about')[",(usernamecurr - 1),"].innerText;"))[[1]]
            usernamequalities[[usernamecurr]] <- unlist(strsplit(usernamequalities[[usernamecurr]], "[ ]"))
            
          }
          
          # vector: will be updated if the user chose to show her first name e.g. Renee in her profile
          usernamename_lastbetterknown <- usernamename
          
          # per username(elements) on this page
          for(usernamecurr in usernamerange) {
            
            usernameurlcurr <- usernameurl[usernamecurr]          
            usernamenamecurr <- usernamename[usernamecurr]         
            usernamequalitiescurr <- usernamequalities[[usernamecurr]]  
            
            special    <- c("robot")
            
            some_curr_dialog <- c()
            
            rec_all <- c("FitBrittany","southernredhead8","nolacountrygirl985") # HAVE SENT A good CUSTOM FIRST MESSAGE - SUN JUL 26
            
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
            
            # ACTUALLY do not vst/msg 
            if(usernamecurr %in% do_not_vst_msg) {
              print(paste0("usernamecurr ", usernamecurr, " is explicit: do not vst/msg"))
              print(paste0("therfore, skipping to the next user usernamecurr"))
              next; # out of the 'username(elements) on page loop'
            }
            
            # begin determining last online time
            
            usernameonlinexxxcurr <- c("UNKNOWN") 
            
            # javascript is 'zero(0) based'
            
            if(sub("^\\s+", "", usernamequalitiescurr[length(usernamequalitiescurr)-1]) == "Online" && 
                 usernamequalitiescurr[length(usernamequalitiescurr)]  == "Now" ) { 
              usernameonlinexxxcurr <- "ONLINENOW"
              print(paste0(usernamenamecurr, " is Online Now"))   
            } else { }
            
            if(sub("^\\s+", "", usernamequalitiescurr[length(usernamequalitiescurr)-1]) == "Online" && 
                 usernamequalitiescurr[length(usernamequalitiescurr)]  == "Today" ) { 
              usernameonlinexxxcurr <- "ONLINETODAY"
              print(paste0(usernamenamecurr, " is Online Today"))   
            } else {  }
            
            if(sub("^\\s+", "", usernamequalitiescurr[length(usernamequalitiescurr)-2]) == "Online" && 
                 usernamequalitiescurr[length(usernamequalitiescurr)-1]  == "This"  && 
                 usernamequalitiescurr[length(usernamequalitiescurr)  ]  == "Week"  
            ) { 
              usernameonlinexxxcurr <- "ONLINETHISWEEK"
              print(paste0(usernamenamecurr, " is Online This Week"))   
            } else {  }
            
            # what online status?
            print(paste0(usernamenamecurr, " Online status is ", usernameonlinexxxcurr)) 
            
            # possible kick out of the program ( !!! NOT TESTED !!! )
            
            # ALPHAALPHA: NOTE ALPHAALPHA is the same as BETABETA - remove one of the two
            if(online_when == "online_now" &&
                 (
                  usernameonlinexxxcurr ==  "ONLINETODAY" ||
                  usernameonlinexxxcurr ==  "ONLINETHISWEEK" ||
                  usernameonlinexxxcurr ==  "UNKNOWN" 
                 ) 
            ) {
              print("Done with the loop: no more ONLINENOW.")
              no_more_online_when <- TRUE
              print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
              break; # out of the 'username(elements) on page loop'
            }
            
            # NOTE: (CODE ABOVE): I HAVE "NOT DETECTED 'THIS MONTH'" ( SLIGHTLY DANGEROUS HERE)
            #   NOT DETECTED: Online Last 30 Days
            if(online_when == "within_the_last_week" && 
            usernameonlinexxxcurr == "UNKNOWN"  ) {
              print("Done with the loop: no more ONLINENOW, ONLINETODAY, ONLINETHISWEEK")
              no_more_online_when <- TRUE
              print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
              break; # out of the 'username(elements) on page loop'
            }
            
            # BETABETA: NOTE ALPHAALPHA is the same as BETABETA - remove one of the two
            if(online_when == "online_now" && 
                 usernameonlinexxxcurr %in% c("ONLINETODAY", "ONLINETHISWEEK", "UNKNOWN")  ) {
              print("Done with the loop: no more ONLINENOW")
              no_more_online_when <- TRUE
              print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
              break; # out of the 'username(elements) on page loop'
            }
            
            
            # actually visit the username
            navigate_target_age_current_page_current_username <- usernameurlcurr
            
            # actually visit ( in pof a 'message' always includes a 'visit')
            
            # regular visit
            if( !(usernamenamecurr %in% usernamename_already_visited) ) {
              
              print(paste0("of age ",agecurr, " of page ",pagecurr," of ",pagerange_str," begin url nav to  ", usernamenamecurr))
              remDr$navigate(navigate_target_age_current_page_current_username)
              Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) 
              
                if( action == "message_greet_matchname" ) {
                  
                  c(", good Friday morning to you!  How are you?") -> message_greet_matchname_vector
                  # , good evening this fine Monday! Are you well this evening? --Monday July 28th
                  
                  
                  webElemPOSSBTRNAMETEXT <- remDr$findElement("css selector", "span.headline.AboutMe")  
                  webElemPOSSBTRNAMETEXT$highlightElement()   # works
                  
                  headline_about_caption <- webElemPOSSBTRNAMETEXT$getElementText()[[1]]
                  # "About Renee"
                  
                  headline_about_caption_selfid <- str_replace(str_extract(headline_about_caption,"[ ].*"),"[ ]","")
                  # [1] "Renee"
                  
                  if(headline_about_caption_selfid != "Me") {
                    usernamename_lastbetterknown[usernamecurr] <- headline_about_caption_selfid 
                  } else {
                    usernamename_lastbetterknown[usernamecurr] <- usernamenamecurr
                  }
                  usernamenamecurr_lastbetterknowncurr <- usernamename_lastbetterknown[usernamecurr]
                  
                  # AFTER SURE CUSTOM NAME WORKS CORRECTLY THEN REMOVE THIS LINE
                  # current_message  <- paste0(usernamenamecurr, message_greet_matchname_vector)
                  current_message  <- paste0(usernamenamecurr_lastbetterknowncurr, message_greet_matchname_vector)
                  
                  # SEND MESSEGE - PER HERE PAGE STEPS 1-3 ( OF TOTAL 6 )
                  
                  # AFTER SURE CUSTOM NAME WORKS CORRECTLY THEN REMOVE THIS LINE
                  # print(paste0("Begin attempt to send message to ", usernamenamecurr))
                  print(paste0("Begin attempt to send message to ", usernamenamecurr," aka ", usernamenamecurr_lastbetterknowncurr))
                  
                  writeLines(current_message)
                  
                  # TEMP COMMENTED OUT FOR OTHER TESTING
                  
                  webElemMB <- remDr$findElement("css selector", "textarea.profile")  # 1
                  webElemMB$highlightElement()               # THAT WORKED            # 2
                  webElemMB$sendKeysToElement(list(current_message)) # THAT WORKED            # 3
                  
                  webElemSMB <- remDr$findElement("css selector", "input.button.norm-green")   # 4
                  webElemSMB$highlightElement() # THAT WORKED                                  # 5
                  webElemSMB$clickElement() # SEEMS TO HAVE WORKED - THE PAGE CHANGED          # 6
                  Sys.sleep(1 + 1 * runif(1, min = 0, max = 1)) 
                  
                  # AFTER SURE CUSTOM NAME WORKS CORRECTLY THEN REMOVE THIS LINE
                  # print(paste0("End attempt to send message to ", usernamenamecurr))
                  print(paste0("End attempt to send message to ", usernamenamecurr," aka ", usernamenamecurr_lastbetterknowncurr))
                  
                  if(action == "message_greet_matchname") {
                    true_attempted_send_message_count <- true_attempted_send_message_count + 1
                  }

                  if(true_attempted_send_message_ANDRE_this_session_run_limit == true_attempted_send_message_count) {
                    print(paste0("message ",true_attempted_send_message_count," of max messages ", true_attempted_send_message_ANDRE_this_session_run_limit))
                    print(paste0("sent message: Andre this session run limit reached - break from username(elements) on this page loop"))
                    break;
                  }
                  
                }
              
              print(paste0("of age ",agecurr, " of page ",pagecurr," of ",pagerange_str,"   end url nav to  ", usernamenamecurr))
              usernamename_already_visited <-c(usernamename_already_visited,usernamenamecurr)
              
            } else { # pof - do not accidentally visit a 2nd time
              print(paste0("of age ",agecurr, " of page ",pagecurr," of ",pagerange_str," SKIPPING(already visited) url nav to  ", usernamenamecurr))
            }

          }

        }
        
        print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
        
        ## END or ERROR   
        ## next;
        ## break;
      }
      
      print(paste0("ending age ", agecurr," of ",agerange_str))
      
    ## END or ERROR   
    ## next;
    ## break;
      
    }
    
    bookmarkhere <- 1
    
    # manually logout of pof cupid here 
    # manually X out ( shutdown ) the browser
    
    remDr$navigate("http://www.pof.com/abandon.aspx")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
    
    remDr$navigate("http://www.hp.com")
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
# debugSource('J:/YDrive/All_NewSeduction/All_ElectronicSpeech/RSeleniumAndBrowsers/AES1/pof_visit_looper_dev.R')

# NOTE: Optional, but HIGHLY recommended, for performance, Turn OFF 'view google chrome images'
# NOTE: Optional, but HIGHLY recommended, for performance, Turn OFF 'view google chrome images'

# REM: taskmgr - manually KILL off java.exe if it is running
# XOR
# "command prompt"->"right click"->"run as adminsitrator"  
# VISIT WITHIN LAST WEEK
# netstat -o -a -b  -n | find /i "listening" | find /i ":4461" 
# taskkill /F /T /PID <above_right_col_number>
# XOR
# SEND MESSAGE TO 'ONLINE NOW'
# netstat -o -a -b  -n | find /i "listening" | find /i ":4462"
# taskkill /F /T /PID <above_right_col_number>

# MANUALLY PLACE DOWN THE BREAKPOINT
#   e.g. remDr$open() # oracle.com  

# just visit
# pof_visit_looper_dev()
# pof_visit_looper_dev(curr_port = 4461, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") # default

# send a message
# pof_visit_looper_dev(curr_port = 4462, action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# END INSTRUCTIONS  
# END INSTRUCTIONS      

