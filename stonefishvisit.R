

# pof
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

pof_visit_looper_dev <- function(curr_port = 4444, browser = "firefox", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), on_exit_logoff_site = TRUE, on_exit_close_browser = TRUE, on_exit_stop_selenium_server = FALSE, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") { 
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
    # SHOULD have been arleady started
    manageRSeleniumServer(curr_port = 4444, doif = "if_server_not_started_please_start" )
    
    # native events: TRUE; default on IE. 
    #    not_default on chrome BUT settable = TRUE works, 
    #    NEVER default settable on firefox(always ends up FALSE)
    # SEE ( Andre Experiments ) # SEE ERROR remDr$value$message # SEE NATIVE EVENTS remDr$sessionInfo$nativeEvents
    # $mouseMoveToLocation without "move" #21
    # https://github.com/ropensci/RSelenium/issues/21

    if(  browser == "chrome"  &&  use_the_custom_profile == TRUE) {
      
      browser_profile_dir_path   <- "J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data"
      browser_profile            <- "era674smartie_era674smart"
      browser_profile_file       <- "Preferences"
      browser_profile_file_GOOD  <- "Preferences.CORRECT_HOME_PAGE_AND_NO_IMAGES"    
      
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
      
    }

    if(  browser == "chrome"  &&  use_the_custom_profile == FALSE) {
      remDr <- remoteDriver(browserName = "chrome", port = curr_port)
    } 

    if(  browser == "firefox" ) {
      remDr <- remoteDriver(browserName = "firefox", port = curr_port, nativeEvents = TRUE)
    } 
      

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

    if(browser == "chrome") {
      # nav to chrome://settings/ and turn off images for performance reasons
      remDr <- google_chrome_set_no_images(remDr = remDr)
    }

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
    webElem1$sendKeysToElement(list(site_login)) # NOTE: BELOW: MAYBE DANGEROUSLY HARDCODED IN a message to here
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2 <- remDr$findElement("css selector", "input#logincontrol_password")
    webElem2$sendKeysToElement(list(site_password))
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds 
    
    webElem2$sendKeysToElement(list(key="enter"))
    Sys.sleep(4 + 2 * runif(1, min = 0, max = 1)) # 5 to 10 seconds wait
    
    print("logged into pof")
    
    print("Of THIS progrem, the user hand written call follows.")
    print(looper_typed_in_call) # language

    program_run_started <- Sys.time()
    print(paste0("Program run Starting at: ",program_run_started))

    # MAGIC NUMBER 
    # pof
    # 45 is the maximum age for a 31 year old
    #  else it defaults to 'a big age range'
    # 31 YEAR OLD - SEARCH ON * age * WILL FAIL - '22 is first success' '45 is the last success'
    #    SAFE MIN: 23 SAFE MAX 44

    # agerange     <-  23:44      #  30:31  # 45:44   c(25:18,50:31) "25:18,50:31" # 
    # agerange_str <- "23:44"     # "30:31" # 45:44    
    
    agerange_str <- age_range_str
    agerange     <- eval(parse(text=agerange_str))

    # POF limit of 55 messages per 24 hour period to 100% NEW PEOPLE
    true_attempted_send_message_ANDRE_this_session_run_limit <- 55 * 2 # to cover people that I previously 'New messaged'
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
        
        # not 'advanced search'
        
        # Fiddler TextView ( NOT USEFUL: GET ON COMMAND LINE DOES NOT WORK )
        # agelow=42&agehigh=42&miles=50&contacted=3&viewtype=0
        # if CARE, use selenium .submit() instead
        # http://stackoverflow.com/questions/17530104/selenium-webdriver-submit-vs-click
        # JUST BELOW
        
        # choose age: 18 to 18 50 miles of KNOWN_PLACE ( from 'pof' personal email to me )
        # http://www.pof.com/lastonlinemycity.aspx # SELENIUM? add form to page and do a POST request
        #   ALL_USERS             Online Now ... Last Online
        #   USERS_I_HAVE_CONTACTED
        #   USERS_I_HAVE_NOT_CONTACTED ( not much use? - can only restrict by 'age' and 'distance' )
        #                                
        # choose age: 18 to 18 50 miles ofKNOWN_PLACE ( from 'above' bottom of the page link followed )
        # http://www.pof.com/lastsignup.aspx ( already on 'advanced search page' )
        
        # advanced search - does not have - 'new users' - NO CODE implementation yet

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
            
            # NOTE: (CODE ABOVE): I HAVE "NOT DETECTED 'THIS MONTH'" ( SLIGHTLY DANGEROUS HERE)
            #   NOT DETECTED: Online Last 30 Days
            
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
            
#             # ALPHAALPHA: NOTE ALPHAALPHA is the same as BETABETA - remove one of the two
#             if(online_when == "online_now" &&
#                  (
#                   usernameonlinexxxcurr ==  "ONLINETODAY" ||
#                   usernameonlinexxxcurr ==  "ONLINETHISWEEK" ||
#                   usernameonlinexxxcurr ==  "UNKNOWN" 
#                  ) 
#             ) {
#               print("Done with the loop: no more ONLINENOW.")
#               no_more_online_when <- TRUE
#               print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
#               break; # out of the 'username(elements) on page loop'
#             }
            
            # NOTE: (CODE ABOVE): I HAVE "NOT DETECTED 'THIS MONTH'" ( SLIGHTLY DANGEROUS HERE)
            #   NOT DETECTED: Online Last 30 Days

            # BETABETA: NOTE ALPHAALPHA is the same as BETABETA - remove one of the two
            if(online_when == "online_now" && 
                 usernameonlinexxxcurr %in% c("ONLINETODAY", "ONLINETHISWEEK", "UNKNOWN")  ) {
              print("Done with the loop: no more ONLINENOW")
              no_more_online_when <- TRUE
              print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
              break; # out of the 'username(elements) on page loop'
            }

            if(online_when == "online_today" && 
                 usernameonlinexxxcurr %in% c( "ONLINETHISWEEK", "UNKNOWN")  ) {
              print("Done with the loop: no more ONLINETODAY")
              no_more_online_when <- TRUE
              print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
              break; # out of the 'username(elements) on page loop'
            }

            if(online_when == "online_today_ONLY" && 
                 ( usernameonlinexxxcurr %in% c("ONLINETODAY")  ) &&
                !( usernameonlinexxxcurr %in% c("ONLINENOW", "ONLINETHISWEEK", "UNKNOWN")  ) 
            ) {
              print("Done with the loop: no more ONLINENOW")
              no_more_online_when <- TRUE
              print(paste0("of age ",agecurr, " ending page ",pagecurr," of ",pagerange_str))
              break; # out of the 'username(elements) on page loop'
            }

            if(online_when == "within_the_last_week" && 
            usernameonlinexxxcurr == "UNKNOWN"  ) {
              print("Done with the loop: no more ONLINENOW, ONLINETODAY, ONLINETHISWEEK")
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
                  
                  # , good evening this fine Monday! Are you well this evening? --Monday July 28th
                  #", good Friday morning to you!  How are you?" -- Fri Aug 6
                  #c(", happy Saturday afternoon! How are you today?") -> message_greet_matchname_vector 
                  
                  # from function call actual argument
                  todays_message -> message_greet_matchname_vector
                  
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
                  # transition
                  usernamenamecurr_lastbetterknowncurr <- usernamename_lastbetterknown[usernamecurr]
                  
                  
                  
                  # OVERRIDE
                  
                  # THESE PEOPLE TOLD ME THEIR NAMES OR IT WAS I THEIR PROFILE
                  # of the left side DO NOT USE THE 'ABOUT NAME' IT IS TOO COMMON e.g. Michelle
                  matchnames_aliases <- list(
                    c("reyesmama","Rebecca"),
                    c("sungelique504","Natasha")
                  )
                  
                  matchnames_aliases_db <- as.data.frame(t(data.frame(matchnames_aliases)), stringsAsFactors = FALSE)
                  
                  if(any(str_detect(usernamenamecurr,matchnames_aliases_db[,1]))) {
                    # as an alias  # find the alias
                    # NOTE: could fail if TWO entries are found ( really should have tested more)
                    matchname_to_message <- matchnames_aliases_db["V2"][matchnames_aliases_db["V1"] == usernamenamecurr]
                  } else {
                    matchname_to_message <- usernamenamecurr
                  }
                  print(paste0("Messaging her real name instead: ", matchname_to_message))
                  
                  # re-transition
                  usernamenamecurr_lastbetterknowncurr <- matchname_to_message
                  
                  
                  
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

                  print(paste0("Message ",true_attempted_send_message_count," of max messages ", true_attempted_send_message_ANDRE_this_session_run_limit))
                  if(true_attempted_send_message_ANDRE_this_session_run_limit == true_attempted_send_message_count) {
                    print(paste0("SENT MESSAGE: ANDRE_THIS_SESSION_RUNLIMIT reached - break from username(elements) on this page loop"))
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
      
      print("Of THIS progrem, the user hand written call follows.")
      print(looper_typed_in_call) # language
      
    ## END or ERROR   
    ## next;
    ## break;
      
    }

    print(paste0("This Program run Started at: ",program_run_started))
    print(paste0("Program run Ending at: ",Sys.time()))
    
    bookmarkhere <- 1
    
    # manually logout of pof cupid here 
    # manually X out ( shutdown ) the browser
    
    # often I may want this to be FALSE
    if( on_exit_logoff_site == TRUE ) {
      
      remDr$navigate("http://www.pof.com/abandon.aspx")
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
      remDr$navigate("http://www.hp.com")
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

    return(remDr)
    
  }
  remDr <- maininner()
  return(list(remDr = RemDr, SentMsgAttemptedCount = true_attempted_send_message_count))
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
# netstat -o -a -b  -n | find /i "listening" | find /i ":4444" 
# taskkill /F /T /PID <above_right_col_number>
# XOR
# SEND MESSAGE TO 'ONLINE NOW'
# netstat -o -a -b  -n | find /i "listening" | find /i ":4444"
# taskkill /F /T /PID <above_right_col_number>

# MANUALLY PLACE DOWN THE BREAKPOINT
#   e.g. remDr$open() # oracle.com  

# just visit
# pof_visit_looper_dev()
# pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") # default

# send a message
# pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = NULL, site_password = NULL, age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

# END INSTRUCTIONS  
# END INSTRUCTIONS  

# NO GITHUB 
# pof
# , site_login = "era674smartest01", site_password = "739heg08", 
#  

# just visit
# pof_visit_looper_dev() - body_type = "anything" - online_when = "within_the_last_week"
# pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "era674smartest01", site_password = "739heg08", age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") # default


# send a message
# FIRST OF TWO
# send a message - body_type = "thin_athletic" - "online_now"
# pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "era674smartest01", site_password = "739heg08", age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")
# 
# SECOND OF TWO
# send a message - body_type = "thin_athletic" - "online_today_ONLY" ( may end up at early 20 year olds )
# pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "era674smartest01", site_password = "739heg08", age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_today", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")


