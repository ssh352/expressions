


# DON'T FORGET TO STARTUP PostgreSQL !!
# FUTURE: CONSIDER RSelenium FLAG ... native EVENTS


# 
# CREATE TABLE aes_do_not_visit_list
# (
#   id            double precision  NOT NULL,
#   match_source  text,
#   my_matchname  text,
#   her_matchname text
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
# -- NOTE: now the index will dissapear from the GUI ( but the pk properties showed its there ( e.g. btree ) )
#                                                      
# REINDEX TABLE aes_have_visited_list; -- simply rebuild that index
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

# SCANS THIS DIRECTORY FOR .R files

# NOTE 'FULL SYSTEM TEST' WITH THE 'SEND MESSAGE' NOT DONE YET' 
# NOTE 'FULL SYSTEM TEST' WITH THE 'SEND MESSAGE' NOT DONE YET'
# NOTE 'FULL SYSTEM TEST' WITH THE 'SEND MESSAGE' NOT DONE YET'   




# shell("rstudio", wait=FALSE)

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) 
options(digits.secs = 6)

safe_navigate_to_new_url <- function(new_url = NULL, remote_driver = NULL, after_how_long = NULL, backout_url = NULL) {
  
  require(tcltk)
  require(RSelenium)
  
  # NOTE: 'note very case is 'code covered' BUT IF A PROBLEM SHOULD BE FIXABLE
  
  # if browser/site/internet hangs just ...
  # backout_url: "current_url", "goback" "http://www.time.gov"(default) "CUSTOMHTTP"
  # NOTE: to retry JUST ONCE more: call by 'new_url == backout_url'
  
  #  1000:  good redirect success test
  # 30000:  should never do 'backout_url' ( 30 seconds )
  if(is.null(after_how_long)) { after_how_long <- 30000 } # 30 seconds ** CHANGE BACK TO 30 SECONDS
  
  backout_url_set <- FALSE
  if(is.null(backout_url))         { backout_url_value <- "http://www.time.gov"; backout_url_set <- TRUE } # default
  if(!is.null(backout_url) && backout_url == "goback")      { backout_url_value <- "goback"     ; backout_url_set <- TRUE }
  if(!is.null(backout_url) && backout_url == "current_url") { backout_url_value <- "current_url"; backout_url_set <- TRUE }
  if(!is.null(backout_url) && !isTrue( backout_url_set))    { backout_url_value <- backout_url; backout_url_set <- TRUE }
  
  if(!isTRUE(backout_url_set))  stop(paste0("safe_navigate_to_new_url call is missng a 'good backout_url'")) 
  
  # tcl: functon call does  not 'seem to be allowed to have any parameters ( rely on 'scope and visibility' )
  safe_navigate_backout <- function() { 
    
    # SHOULD be TRUE here
    if(isTRUE(backout_url_set)) {
      
      if(backout_url_value == "goback")         { remote_driver$goBack();              return() }
      if(backout_url_value == "current_url")    { remote_driver$navigate(current_url); return() }
      remote_driver$navigate(backout_url_value) 
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


okcupid_visit_looper_dev <- function() {
  
  maininner <- function() {
    
    oldtz <- Sys.getenv('TZ')
    if(oldtz=='') {
      Sys.setenv(TZ="UTC")
    }
    
    set.seed(runif(1, min = 0, max = 1))
    
    require(RSelenium)
    require(stringr)
    
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host = "127.0.0.1", dbname = "aes_db", user = "postgres", password = "postgres")
    
    # REM: taskmgr OR 'some other way' KILL off java.exe if it is running
    startServer(args = c("-port 4451"))  # default # 4456
    Sys.sleep(5.0) # 5 second wait
    
    cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "time861wiz_time861wiz") 
    remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof, port = 4451) # default 4456
    remDr$open() # oracle.com  
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    print("opened browser home page")
    
    # Sys.sleep(1 + 5* runif(1, min = 0, max = 1)) # 6 to 11 seconds wait
    
    remDr$navigate("https://www.okcupid.com/login")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
    print("navigated to okcupid")
    
    webElem1 <- remDr$findElement("css selector", "#login_username")
    webElem1$sendKeysToElement(list("time861wiz"))
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2 <- remDr$findElement("css selector", "#login_password")
    webElem2$sendKeysToElement(list("739heg08"))
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
    
    
    message_textarea_begin <- "return document.getElementsByTagName(\"textarea\")[6].value = \""
    message_textarea_end   <- "\";"
    
    
    # MAGIC NUMBER 
    agerange <-      36:18      #  30:31  # 50:49
    agerange_str <- "36:18"     # "30:31" # 50:49    
    
    for(agecurr in agerange) { # testing only 30 and 31 # 50:18   
      
      print(paste0("beginning age ",agecurr))
      
      navigate_target <- paste0("http://www.okcupid.com/match?filter1=0,34&filter2=2,",agecurr,",",agecurr,"&filter3=3,50&filter4=5,604800&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=500")     
      remDr$navigate(navigate_target)
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
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
        Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
        
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
          Sys.sleep(0.001)
        }
      }

      print(paste0("end collecting all A elements  of the page of : ",agecurr, " of age ", agerange_str))
      
      # unique
      apagearefsu   <- unique(apagearefs)

      # profiles
      apagearefsup  <- apagearefsu[str_detect(apagearefsu,"^.*profile")]

      # regulars
      apagearefsupr <- apagearefsup[str_detect(apagearefsup,"[?]cf=regular$")] # SOME WILL HAVE A SECOND LINK WITH ENDING: ?cf=recently_visited

      # I have not seen these(after the filters are done), but just in case crept through
      
      apagearefsupr <- apagearefsupr[!grepl("CALLGIRL",apagearefsupr,ignore.case=TRUE)]
      apagearefsupr <- apagearefsupr[!grepl("ROBOT",apagearefsupr,ignore.case=TRUE)]
      
      # loop and visit each name - from bottom(rev) to top ( testing ) 
      # testing - visit from the BOTTOM going UP 
      
      print(paste0("begin visiting each profile of the page of : ",agecurr, " of age ", agerange_str))
      
      
      # rev(apagearefsupr)[1:2]               BOTTOM OF PAGE:  testing: reverse and first 2 names ( testing )
      # letters[1:(length(letters) %/% 2)]    TOP    OF PAGE:  50% of the unuque links GOING DOWN
      
      apagearefsupr_total       <-  apagearefsupr
      apagearefsupr_total_count <- length(apagearefsupr)
      print(paste0("Total possible profiles: ",apagearefsupr_total_count))
            
      # choose e.g. visit only the top half
      apagearefsupr_reduced <- apagearefsupr[1:(length(apagearefsupr) %/% 2)]
      apagearefsupr_reduced_count <- length(apagearefsupr_reduced)
      print(paste0("Reduced actually actioning  profiles: ",apagearefsupr_reduced_count))
      
      # get the name out of the url
      begin_matchnames_str_locations <- (str_locate(apagearefsupr_reduced,"profile/") + 1)[,2,drop=FALSE]
      end_matchnames_str_locations   <- (str_locate(apagearefsupr_reduced,"[?]"     ) - 1)[,1,drop=FALSE]
      
      # NOTE: (str_locate  finds 'first occurance"
      # could possible break if  "?" is found in a strange spot
      
      # hadly S-logic
      matchnames <- str_sub(apagearefsupr_reduced, start = cbind(begin_matchnames_str_locations, end_matchnames_str_locations))
      
      action_ref_counter <- 0
      for(alink in apagearefsupr_reduced) { 
        # so I know where I am
        action_ref_counter <- action_ref_counter + 1
        
        print(paste0("begin visiting ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
        
        print(paste0("  current matchname: ",matchnames[action_ref_counter]))
        
        print(paste0("  action ", action_ref_counter, " of ",apagearefsupr_reduced_count ))
        
        navigate_target <- alink
        
        # LESS SAFE (OLD)
        # remDr$navigate(navigate_target)
        
        # MORE SAFE ( AFTER A 'HANG OF MORE 30 SECONDS' WILL GO TO 'TIME.GOV')
        safe_navigate_to_new_url_success <- safe_navigate_to_new_url(new_url = navigate_target, remote_driver = remDr)
        print(paste0("safe navigation to new url success: ",safe_navigate_to_new_url_success[["success"]]))
        # in case some internals that I do not know of
        rmDir <- safe_navigate_to_new_url_success[["remote_driver"]]
        # identical( remDr, safe_navigate_to_new_url_success[["remote_driver"]] ) # [1] TRUE
        
        Sys.sleep(2.1 + 1 * runif(1, min = 0, max = 1)) # 2 to 4 seconds wait

        remDr$executeScript("return 0")
        
        if(isTRUE(safe_navigate_to_new_url_success[["success"]])) {
          
          dbGetQuery(con, paste0("insert into 
          aes_have_visited_list(
            id, match_source, my_matchname, her_matchname)
              values(", as.numeric(Sys.time()), ", 'okcupid_NO_metro'", ", 'time861wiz'",", '",matchnames[action_ref_counter],"');")
          )
          # as.Date(as.POSIXct(1433110111.9225857, origin="1970-01-01"))
          # [1] "2015-05-31"
          
        }
         

        # THIS SHOULD WORK!
        # BEGIN SEND MESSAGE AREA
        
#         print(paste0("begin send message ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
#         
#         current_message  <- message_vector[trunc( 1 + length(message_vector)*runif(1, min = 0, max = 1) - 0.001 )] 
#         writeLines(paste0(message_textarea_begin,current_message,message_textarea_end))
#         
#         
#         # send message button
#         
#         webElemSMB <- remDr$findElement("css selector", "#footer_send_btn")
#         webElemSMB$highlightElement() # THAT WORKED
#         remDr$mouseMoveToLocation(webElement = webElemSMB) 
#         webElemSMB$sendKeysToElement(list(key = "enter")) 
#         Sys.sleep(2 + 1 * runif(1, min = 0, max = 1))
#         
#         
#         # type characters in textarea
#         
#         remDr$executeScript(paste0(message_textarea_begin,current_message,message_textarea_end))[[1]]
#         Sys.sleep(4 + 2* runif(1, min = 0, max = 1))
#         
#         
#         # true message button
#         
#         webElemTMB <- remDr$findElement("css selector", "#global_messaging_container > div > form > button")
#         remDr$mouseMoveToLocation(webElement = webElemTMB) 
#         webElemTMB$highlightElement()
#         webElemTMB$sendKeysToElement(list(key = "enter"))
#         ## BOX STAYS UP - AND MESSAGE SHOWS SENT
#         Sys.sleep(2 + 2* runif(1, min = 0, max = 1))
#         
#         print(paste0("end send message ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
        
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
# netstat -o -a -b  -n | find /i "listening" | find /i ":4451"
# taskkill /F /T /PID <above_right_col_number>

# MANUALLY PLACE DOWN THE BREAKPOINT
#   e.g. remDr$open() # oracle.com  

# okcupid_visit_looper_dev()

# END INSTRUCTIONS
# END INSTRUCTIONS

