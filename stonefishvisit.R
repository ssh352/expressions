

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


pof_visit_looper_dev <- function(curr_port = 4461, action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE") { 
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
    
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host = "127.0.0.1", dbname = "aes_db", user = "postgres", password = "postgres")
    
    # REM: taskmgr OR 'some other way' KILL off java.exe if it is running
    startServer(args = c(paste0("-port ", curr_port),"-timeout 3600","-browserTimeout 3600"))  # default # 4444 # java -jar selenium-server-standalone.jar -h
    Sys.sleep(5.0) # 5 second wait
    
    cprof <- getChromeProfile("J:\\YDrive\\All_NewSeduction\\All_ElectronicSpeech\\RSeleniumAndBrowsers\\AES1_assistance\\RDebug\\Administrator\\AppData\\Local\\Google\\Chrome\\User Data", "era674smartie_era674smart") 
    remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof, port = curr_port) # default 4444
    
    print(paste0("PORT ", curr_port))
    
    # NOTE: POF # stores 'preferences(cookie-ish) on its servers in Vancouver'
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
    webElem1$sendKeysToElement(list("era674smart"))
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds
    
    webElem2 <- remDr$findElement("css selector", "input#logincontrol_password")
    webElem2$sendKeysToElement(list("739heg08"))
    Sys.sleep(3 + 2 * runif(1, min = 0, max = 1)) # 3 to 5 seconds 
    
    webElem2$sendKeysToElement(list(key="enter"))
    Sys.sleep(4 + 2 * runif(1, min = 0, max = 1)) # 5 to 10 seconds wait
    
    print("logged into pof")
    
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
    c(", how is it going?") -> message_greet_matchname_vector
    # NOT USED YET c(", hello :)") -> message_greet_matchname_vector # IN PROGRESS: WEEK OF FRI JUNE 12
    
    # NOTE: DOES NOT YET ESCAPE OUT TICK MARKS('), SO DO NOT SEND OUT A TICK MARK(')
    
    # NOTE: THIS MAY BE VOLITILE ( AT FIRST WAS [6]?)
    message_textarea_begin <- "return document.getElementsByTagName(\"textarea\")[5].value = \""
    message_textarea_end   <- "\";"
    
    # MAGIC NUMBER 
    # pof
    # 45 is the maximum age for a 31 year old
    #  else it defaults to 'a big age range'
    agerange <-      45:18      #  30:31  # 45:44   c(25:18,50:31) "25:18,50:31" # 
    agerange_str <- "45:18"     # "30:31" # 45:44    
    
    for(agecurr in agerange) { # testing only 31 and 30 # 31:30    
      
      print(Sys.time())
      print(paste0("PORT ", curr_port))
    
      print(paste0("beginning age ",agecurr))
      
      # first 'age page' is WHERE I get the refrences to the other pages
      have_url_links_all_reference_pages_this_age <- FALSE
      
      if(action == "message_greet_matchname" && online_when == "online_now") {
        
        # later: change to pof
        # navigate_target <- paste0("https://www.okcupid.com/match?filter1=0,34&filter2=2,",agecurr,",",agecurr,"&filter3=3,50&filter4=5,3600&filter5=1,1&locid=0&timekey=1&matchOrderBy=MATCH&custom_search=0&fromWhoOnline=0&mygender=m&update_prefs=1&sort_type=0&sa=1&using_saved_search=&count=500")
        
      } else { # default: visit everyone in that age: pof does not have a DIRECT "online last weeK" ONLINE now
        
        # http://www.pof.com/advancedsearch.aspx?iama=m&sorting=0&seekinga=f&wantchildren=&MinAge=24&MaxAge=24&smoke=&country=1&drugs=&City=70002&miles=50&interests=&state=&viewtype=1&height=&drink=&heightb=999&haschildren=&secondLanguage=&car=&ambition=&Go+Fishing%21=Go+Fishing%21
        navigate_target_age_first_page <- paste0("http://www.pof.com/advancedsearch.aspx?iama=m&sorting=0&seekinga=f&wantchildren=&MinAge=",agecurr,"&MaxAge=",agecurr,"&smoke=&country=1&drugs=&City=70002&miles=50&interests=&state=&viewtype=1&height=&drink=&heightb=999&haschildren=&secondLanguage=&car=&ambition=&Go+Fishing%21=Go+Fishing%21")
        
      }    
      
      remDr$navigate(navigate_target_age_first_page)
      Sys.sleep(3 + 1 * runif(1, min = 0, max = 1)) # 10 to 15 seconds wait 
      
      # LOOP ( check to see if I am at the END of the PAGE? ) ( no more information to be dynamically loaded )
      
      ## DOES WORK  
       
      # get the distinct user names found in the HTML  
      
      alinkslength <- remDr$executeScript("return document.getElementsByTagName('a').length;")[[1]]
      Sys.sleep(0.01)
      
      print(paste0("begin collecting all A elements  of the HOME page of age: ",agecurr, " of age ", agerange_str))
      
      apagearefs <- c()
      
      if ( alinkslength > 0 ) { 
        for(alinkcurr in 0:(alinkslength -1)) {
          apagearefs <- c(apagearefs,remDr$executeScript(paste0("return document.getElementsByTagName('a')[",alinkcurr,"].href;"))[[1]])
          Sys.sleep(0.001)
        }
      }
      
      print(paste0("end collecting all A elements  of the HOME page of page of age: ",agecurr, " of age ", agerange_str))
      
      # unique - pof NOT A PROBLEM
      # apagearefsu   <- unique(apagearefs)
      apagearefsu <- apagearefs
      
      # A MEMBER ON THE PAGE
      # http://www.pof.com/viewprofile.aspx?profile_id=70002933   # PICTURE(JUST GOES TO HER NAME LINK BELOW)
      # http://www.pof.com/member70002933.htm                     # HER NAME LINK
      
      # OTHER PAGES AND NOT 'PAGE 1'
      # http://www.pof.com/advancedsearch.aspx?
      
      # STOP
      
      # will ONLY do ONCE/age anyways
      ## only need to do ONCE per age
      #if(have_url_links_all_reference_pages_this_age == FALSE) {
        
        ## other references pages of that same age
       apagearefsupr <- apagearefsu[str_detect(apagearefsu,"advancedsearch[.]aspx[?]")] # 
       # have_url_links_all_reference_pages_this_age <- TRUE # so per age: do not do again
        
      #}
      
      # LATER: FIGURE OUT the 'pof' equivalent
      
      #       special    <- c("robot")
      #       
      #       some_curr_dialog <- c()
      #       
      #       rec6 <- c()
      #       rec5 <- c()
      #       rec4 <- c()
      #       rec3 <- c()
      #       rec2 <- c()
      #       rec1 <- c()
      #       
      #       rec_all <-c(rec6,rec5,rec4,rec3,rec2,rec1)
      #       
      #       lik3 <- c()
      #       lik2 <- c()
      #       lik1 <- c()
      #       
      #       lik_all <- c(lik3,lik2,lik1)
      #       
      #       all_all <- c(lik_all,rec_all,some_curr_dialog) 
      #       
      #       # NOTE: XXX logic: a msg INCLUDES a vst
      #       
      #       if(not_to_vst == "NONE") {     # default
      #         do_not_vst <- c(special)
      #       }
      #       
      #       # exclusive choices
      #       if(not_to_msg == "NONE") {     # default
      #         do_not_vst_msg <- c(do_not_vst)
      #       }
      #       if(not_to_msg == "all_all") {
      #         do_not_vst_msg <- c(do_not_vst,all_all)
      #       }  
      #       
      #       # ACTUALLY do not vst/msg ( pof: adjusted to 'p' from 'pr' )
      #       for(match_var in c(
      #         do_not_vst_msg 
      #       )) { apagearefsup <- apagearefsup[!grepl(match_var,apagearefsup,ignore.case=TRUE)]  } 
      
      # loop and visit each name - from bottom(rev) to top ( testing ) 
      # testing - visit from the BOTTOM going UP 
      
      print(paste0("begin visiting each profile of THIS PAGE of the pages of : ",agecurr, " of age ", agerange_str))
      
      # rev(apagearefsupr)[1:2]               BOTTOM OF PAGE:  testing: reverse and first 2 names ( testing )
      # letters[1:(length(letters) %/% 2)]    TOP    OF PAGE:  50% of the unique links GOING DOWN
      
      
      
      

      # BEGIN total number of pages for this age
      # ( pof: adjusted to 'p' from 'pr' )
      
      apagearefsupr_total       <-  apagearefsupr
      apagearefsupr_total_count <- length(apagearefsupr)
      print(paste0("Total possible pages: ",apagearefsupr_total_count))    
      
      if(apagearefsupr_total_count == 0) {
        print("since Total possible pages: 0, then SKIP AHEAD to next LOOP")
        next
      }
      
      # ( pof: adjusted to 'p' from 'pr' )
      
      # choose e.g. visit every page
      apagearefsupr_reduced <- apagearefsupr[1:(length(apagearefsupr) %/% 1)]
      
      # choose e.g. visit only the top half
      # apagearefsupr_reduced <- apagearefsupr[1:(length(apagearefsupr) %/% 2)]
      
      apagearefsupr_reduced_count <- length(apagearefsupr_reduced)
      print(paste0("Reduced actually actioning  profiles: ",apagearefsupr_reduced_count))
      
      # END total number of pages for this age
      
      

      # POF NA and wrong_regular_expression
      # ( pof: adjusted to 'p' from 'pr' )

      # get the name out of the url

      # begin_matchnames_str_locations <- (str_locate(apagearefsup_reduced,"profile/") + 1)[,2,drop=FALSE]
      # end_matchnames_str_locations   <- (str_locate(apagearefsup_reduced,"[?]"     ) - 1)[,1,drop=FALSE]
      
      # NOTE: (str_locate  finds 'first occurance"
      # could possible break if  "?" is found in a strange spot
      
      # eliminate matchnames that I would not visit - no matter how old they are
      # TO_DO_THIS_WEEKEND [ ]
      
      # hadly S-logic
      # matchnames <- str_sub(apagearefsupr_reduced, start = cbind(begin_matchnames_str_locations, end_matchnames_str_locations))
      
      # pof: also visit people on the FIRST page
      apagearefsupr_reduced <- c(navigate_target_age_first_page, apagearefsupr_reduced)
      
      action_ref_counter <- 0                  # NOT TEST:     apagearefsupr_reduced
      for(alink in  apagearefsupr_reduced)   { #     TEST: rev(apagearefsupr_reduced)[1] # test: send a message to the bottom most link
        
        # so I know where I am
        action_ref_counter <- action_ref_counter + 1
        # TEST ( *** REMOVE AFTER TEST *** )
        # action_ref_counter <- length(apagearefsupr_reduced)
        
        print(paste0("begin visiting ", alink, " of the page ",action_ref_counter, "of age page: ",agecurr, " of age ", agerange_str))
        
        # print(paste0("  current matchname: ",matchnames[action_ref_counter]))
        
        print(paste0("  action ", action_ref_counter, " of ",apagearefsupr_reduced_count ))
        
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
          
          
          # get all profiles on this page
          
        
          # get the distincts found in the HTML  
          
          a1linkslength <- remDr$executeScript("return document.getElementsByTagName('a').length;")[[1]]
          Sys.sleep(0.01)
          
          print(paste0("begin collecting all A elements  of the page ",action_ref_counter, "of age page: ",agecurr, " of age ", agerange_str))
          
          a1pagearefs <- c()
          
          if ( a1linkslength > 0 ) { 
            for(alinkcurr in 0:(a1linkslength -1)) {
              a1pagearefs <- c(a1pagearefs,remDr$executeScript(paste0("return document.getElementsByTagName('a')[",alinkcurr,"].href;"))[[1]])
              Sys.sleep(0.001)
            }
          }
          
          
          print(paste0("end collecting all A elements  of the page ",action_ref_counter, "of age page: ",agecurr, " of age ", agerange_str))
          
          # unique - pof NOT A PROBLEM
          # a1pagearefsu   <- unique(a1pagearefs)
          a1pagearefsu <- a1pagearefs
          
          # profiles on this page of this age
          a1pagearefsup  <- a1pagearefsu[str_detect(a1pagearefsu,"^.*viewprofile")]
          
          
          # BEGIN total number of profiles
          # ( pof: adjusted to 'p' from 'pr' )
          
          a1pagearefsup_total       <-  a1pagearefsup
          a1pagearefsup_total_count <- length(a1pagearefsup)
          print(paste0("Total possible profiles: ",a1pagearefsup_total_count))    
          
          if(a1pagearefsup_total_count == 0) {
            print("since Total possible profiles: 0, then SKIP AHEAD to next LOOP")
            next
          }
          
          # ( pof: adjusted to 'p' from 'pr' )
          
          # choose e.g. visit everyone
          a1pagearefsup_reduced <- a1pagearefsup[1:(length(a1pagearefsup) %/% 1)]
          
          # choose e.g. visit only the top half
          # a1pagearefsup_reduced <- a1pagearefsup[1:(length(a1pagearefsup) %/% 2)]
          
          a1pagearefsup_reduced_count <- length(a1pagearefsup_reduced)
          print(paste0("Reduced actually actioning  profiles: ",a1pagearefsup_reduced_count))
          
          # END total number of profiles
          
          a1ction_ref_counter <- 0                  # NOT TEST:     a1pagearefsup_reduced
          for(a1link in  a1pagearefsup_reduced)   { #     TEST: rev(a1pagearefsup_reduced)[1] # test: send a message to the bottom most link
            
            safe_navigate_to_new_url_success <- safe_navigate_to_new_url(new_url = a1link, remote_driver = remDr, backout_url = "refresh")
            print(paste0("safe navigation to new url success: ",safe_navigate_to_new_url_success[["success"]]))
            # in case some internals that I do not know of
            rmDir <- safe_navigate_to_new_url_success[["remote_driver"]]
            
            
            # okcupid: would have had this ALREADY
            # matchnames[action_ref_counter]
            
            hermatchname <- "SOMEONE"
            
            # FOUND_HER_NAME_ERROR <- FALSE
            # resultHNM <- tryCatch({ webElemHNM <- remDr$findElement("css selector", ". . .") }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
            # if(class(resultHNM) == "webElement") { FOUND_HER_NAME_ERROR <- FALSE } else { FOUND_HER_NAME_ERROR <- TRUE }

            # if(FOUND_HER_NAME_ERROR == FALSE) {
            #   hermatchname <- resultHNM$getElementAttributes("value")     # MAY WORK?
            #  hermatchname <- resultHNM$getElementAttributes("innerText") # MAY WORK?
            # } else { hermatchname <- "ERROR" }
            
            
            ## XOR        
            ## style 2
            # js_hermatchname <- "try{ retvalue = document.querySelectorAll('. . .')[5].value XOR .innerText ; return retvalue } catch(err) { return 'ERROR' };"
            # hermatchname <- remDr$executeScript(js_hermatchname)[[1]]
            
            if(hermatchname != "ERROR"){
              next # just skip ahead ???
            }
 
            if(isTRUE(safe_navigate_to_new_url_success[["success"]])) {
              
              # NOTE: DOES NOT YET ESCAPE OUT TICK MARKS('), SO DO NOT SEND OUT A TICK MARK(')
              dbGetQuery(con, paste0("insert into 
                                   aes_have_visited_list(
                                   id, match_source, my_matchname, her_matchname, her_age)
                                   values(", as.numeric(Sys.time()), ", 'pof_NO_metro'", ", 'era674smart'",", '", hermatchname, "', ", agecurr, ");")
              )
              # as.Date(as.POSIXct(1433110111.9225857, origin="1970-01-01"))
              # [1] "2015-05-31"
              
            }
            
          }
          
        }
        
        # BEGIN SEND MESSAGE AREA
        
        # A WORK INPROGRESS ( NEED TO BE ABBLE TO DETECT INNER TEXT ON A PAGE ( THORUGH JAVASCRIPT)" )
        
        if( action == "message_greet_matchname" || action == "message_random_catchphrase" ) {
          
          print(paste0("begin send message ", alink, " of the page of : ",agecurr, " of age ", agerange_str))
          
          if( action == "message_greet_matchname" ) {
            # OLD
            # current_message  <- paste0(message_greet_matchname_vector[trunc( 1 + length(message_greet_matchname_vector)*runif(1, min = 0, max = 1) - 0.001 )], " ",matchnames[action_ref_counter]) 
            # NEW expect the matchname to come first
            current_message  <- paste0(matchnames[action_ref_counter],message_greet_matchname_vector[trunc( 1 + length(message_greet_matchname_vector)*runif(1, min = 0, max = 1) - 0.001 )]," Ivan")
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
              #         	Message them for $1
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
      
      print(paste0("end visiting ", alink, " of the page ",action_ref_counter, "of age page: ",agecurr, " of age ", agerange_str))
      
      print(paste0("ending age ", agecurr))
      
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

# pof_visit_looper_dev()
# pof_visit_looper_dev(curr_port = 4462, action = "message_greet_matchname", online_when = "online_now", not_to_msg = "all_all")  

# END INSTRUCTIONS 
# END INSTRUCTIONS   

