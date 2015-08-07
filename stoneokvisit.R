

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


okcupid_visit_looper_dev <- function(curr_port = 4451, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE") { 
  # OR action = "message_greet_matchname" "message_random_catchphrase"
  # OR not_to_msg = "all_all"
  
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
    
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host = "127.0.0.1", dbname = "aes_db", user = "postgres", password = "postgres")
    
    # REM: taskmgr OR 'some other way' KILL off java.exe if it is running
    startServer(args = c(paste0("-port ", curr_port),"-timeout 3600","-browserTimeout 3600"))  # default # 4444 # java -jar selenium-server-standalone.jar -h
    Sys.sleep(5.0) # 5 second wait
    
    cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "time861wiz_time861wiz") 
    remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof, port = curr_port) # default 4444
    
    print(paste0("PORT ", curr_port))
    
    # oracle.com 
    remDr$open() 
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait
    
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
    c(", happy Thursday!  So, how are you doing this fine evening?") -> message_greet_matchname_vector # THE AUG 8
    

    # NOTE: DOES NOT YET ESCAPE OUT TICK MARKS('), SO DO NOT SEND OUT A TICK MARK(')
    
                                                       # NOTE: THIS MAY BE VOLITILE ( AT FIRST WAS [6]?)
    message_textarea_begin <- "return document.getElementsByTagName(\"textarea\")[5].value = \""
    message_textarea_end   <- "\";"
    
    # ( REMOVE AFTER DEBUGGING )
    # SECOND_TOUCH_ERROR
    # PER 'ALL AGES' ( SEEMS TO BE HAPPENING? AT THE END OF AN AGE_TOUCH_LIST ) 
    already_touched_alink <- c()
    
    # MAGIC NUMBER 
    agerange <-      18:49      #  30:31  # 50:49   c(25:18,50:31) "25:18,50:31" # LEFT_OFF 29 _diamonds_ "message box full"
    agerange_str <- "18:49"     # "30:31" # 50:49    
    
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
      all_all <- c(all_all,"5000kwatts")  # far way but too hot to miss
      all_all <- c(all_all,"jennh379") # # OVER THE HILL WOMAN - AVE ( BUT ASKED OFFLINE IF I MAY MEET HER IN PERSON  (TH AUG 6) ) New Orleans
      all_all <- c(all_all,"fireunleashed","Carpinteria01") # OVER THE HILL WOMAN - BUT HOT ( I GAVE A CUSTOM REPLY )
      all_all <- c(all_all,"Bozzo327") # from Sunday # You should sms my cell at 5045151708 # far away but hot
      all_all <- c(all_all,"nataleebabinn") # I sent out of the blue ( she is too hot )
      all_all <- c(all_all,"AniLevee") # Tells me that I am too old for her

      all_all <- c(all_all,"lilbird987","xoxosunshinexoxo","PoopySoupy","pizzaforbrkfst") # she tells me directly that she is not interested

      all_all <- c(all_all,"LadyTSydney") # direct to me - NO

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
          print(paste0("skipping - already visited / greet_matchname (below) - THIS SHOULD NOT HAPPEN - please fix"));
          print(paste0("SKIP(NEXT) and END visiting ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
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
              c("MOFlynn37","Michelle")
            )
            
            matchnames_aliases_db <- as.data.frame(t(data.frame(matchnames_aliases)), stringsAsFactors = FALSE)
            
            if(any(str_detect(matchname_current,matchnames_aliases_db[,1]))) {
              # as an alias  # find the alias
              # NOTE: could fail if TWO entries are found ( really should have tested more)
              matchname_to_message <- matchnames_aliases_db["V2"][matchnames_aliases_db["V1"] == matchname_current]
            } else {
              matchname_to_message <- matchname_current
            }
            print(paste0("Messageing her real name instead: ", matchname_to_message))
            
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
      
    }
    
    print(Sys.time())
    bookmarkhere <- 1
    
    # manually logout of ok cupid here 
    # manually X out ( shutdown ) the browser
    
    remDr$navigate("http://www.okcupid.com/logout")
    Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
    
    remDr$navigate("http://www.oracle.com")
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
# debugSource('J:/YDrive/All_NewSeduction/All_ElectronicSpeech/RSeleniumAndBrowsers/AES1/okcupid_visit_looper_dev.R')

# NOTE: Optional, but HIGHLY recommended, for performance, Turn OFF 'view google chrome images'
# NOTE: Optional, but HIGHLY recommended, for performance, Turn OFF 'view google chrome images'

# REM: taskmgr - manually KILL off java.exe if it is running
# XOR
# "command prompt"->"right click"->"run as adminsitrator"  
# VISIT WITHIN LAST WEEK
# netstat -o -a -b  -n | find /i "listening" | find /i ":4451" 
# taskkill /F /T /PID <above_right_col_number>
# XOR
# SEND MESSAGE TO 'ONLINE NOW'
# netstat -o -a -b  -n | find /i "listening" | find /i ":4452"
# taskkill /F /T /PID <above_right_col_number>

# MANUALLY PLACE DOWN THE BREAKPOINT
#   e.g. remDr$open() # oracle.com  

# MAKE SURE - I am (IF PAID FOR) NOT browsing anonymously

# visiting
# okcupid_visit_looper_dev()
# okcupid_visit_looper_dev <- function(curr_port = 4451, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE")

# visiting - not previous dates
# okcupid_visit_looper_dev(curr_port = 4451, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE")

# messaging
# 


# END INSTRUCTIONS   
# END INSTRUCTIONS     

