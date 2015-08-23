
# stone utilities

options(width = 255)     
options(digits = 22) 
options(max.print=99999)
options(scipen=255) 
options(digits.secs = 6)
options(error=NULL) # options(error = recover) 



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



# okcupid
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





# will work for a switch to 'daylight savings time'
# e.g. 
# dynamic_UTC_offset()

dynamic_UTC_offset <- function() {  

  return(as.integer(as.POSIXct(as.character(Sys.time()), tz="UTC") - as.POSIXct(as.character(Sys.time()), tz="America/Chicago")))
  
  # NOTE: Sys.timezone() exists - BUT currently I do SO MANY time_zone switches
  # SO - for now I will HARD CODE *PLACE/CITY* in it until I MOVE SOMEWHERE ELSE.
  # Get Current Date, Time and Timezone - 
  # http://astrostatistics.psu.edu/su07/R/html/base/html/Sys.time.html

}

# NOTE: relies on ABOVE: dynamic_UTC_offset()

# Aug 16,2015 # Central Time Zone current time is UTC - 5
# EDT will be observed in New York, Nassau until Nov 1, 2015 at 2:00 AM UTC Offset: UTC -4:00
# http://www.timeanddate.com/time/zones/edt

# e.g. 
# delayUntilAbsoluteTime( starthourmin = 1330) # # 1:30 p.m.
# 
# less better ( if I am 100% sure of the shift )
# delayUntilAbsoluteTime( starthourmin = 1330, timezone_hour_shift = -5 ) # 1:30 p.m. Central Daylight Time

delayUntilAbsoluteTime <- function( 
  starthourmin = 1500, 
  timezone_hour_shift = dynamic_UTC_offset()
) { # 3:00 p.m. CDT - NOT DYNAMICALLY CHANGE
  
  oldtz <- Sys.getenv('TZ') 
  if(oldtz=='') { 
    Sys.setenv(TZ="UTC") # NOTE: NOT DYNAMICALLY CHANGE
  } 
  
  old_timenow <- Sys.time() + 60 * 60 * timezone_hour_shift
  
  hour <- substr(old_timenow,12,13)
  min <- substr(old_timenow,15,16)
  timenow <- as.numeric(paste(hour,min,sep=''))
  
  while( timenow < starthourmin ){
    
    Sys.sleep(10.000) # 10 seconds
    
    old_timenow <- Sys.time() + 60 * 60 * timezone_hour_shift
    
    hour <- substr(old_timenow,12,13)
    min <- substr(old_timenow,15,16)
    timenow <- as.numeric(paste(hour,min,sep=''))
    
    # print(starthourmin)
    # print(timenow)
    # print("")
    
    # R
    # Some inspiration from ( and simplified )
    # Thread: Possible to run a program in specific time in R? 
    # http://www.talkstats.com/showthread.php/17472-Possible-to-run-a-program-in-specific-time-in-R
    
  }
  
  Sys.setenv(TZ=oldtz)
  return(0)
  
}


# Aug 16,2015 # Central Time Zone current time is UTC - 5
# EDT will be observed in New York, Nassau until Nov 1, 2015 at 2:00 AM UTC Offset: UTC -4:00
# http://www.timeanddate.com/time/zones/edt

# e.g.
# delayUntilRelativeTimeFromNow(addhourmin = 1000, timezone_hour_shift = -5) # 10 hours
# delayUntilRelativeTimeFromNow(addhourmin = 5, timezone_hour_shift = -5) # 5 minutes
# delayUntilRelativeTimeFromNow(addhourmin = 1, timezone_hour_shift = -5) # 1 minute


# NOTE: relies on ABOVE: dynamic_UTC_offset()
# NOTE: relies on ABOVE: delayUntilAbsoluteTime()

delayUntilRelativeTimeFromNow <- function( 
  addhourmin = 900,            # 4:30 a.m. to # 3:30 p.m. CDT - NOT DYNAMICALLY CHANGE
  timezone_hour_shift = dynamic_UTC_offset()
) {
  
  oldtz <- Sys.getenv('TZ') 
  if(oldtz=='') { 
    Sys.setenv(TZ="UTC") # NOTE: NOT DYNAMICALLY CHANGE
  } 
  
  old_basehourmin <- Sys.time() + 60 * 60 * timezone_hour_shift # now
  
  basehourmin = as.numeric(paste(substr(old_basehourmin,12,13),substr(old_basehourmin,15,16),sep='')) 
  starthourmin <- basehourmin + addhourmin
  
  delayUntilAbsoluteTime( starthourmin = starthourmin, timezone_hour_shift =  timezone_hour_shift )
  
  Sys.setenv(TZ=oldtz)
  return(0)
  
}



# NOTE: does not 'go back' or 'navigate back'
google_chrome_set_no_images <- function(remDr = remDr) {
  
  require(RSelenium)
  
  remDr$navigate('chrome://settings-frame/') # LUCKY I SAW THIS BY ACCIDENT IN html of ( chrome://settings/ )
  Sys.sleep(1.0)
  
  # Show advanced settings... link
  webElemADVSETEXP <- remDr$findElement('css', '#advanced-settings-expander')
  webElemADVSETEXP$highlightElement()
  webElemADVSETEXP$clickElement()
  Sys.sleep(1.0)
  
  # Content button
  webElemCONSETBTN <- remDr$findElement('css', '#privacyContentSettingsButton')
  webElemCONSETBTN$highlightElement()
  webElemCONSETBTN$clickElement()
  Sys.sleep(1.0)
  
  # 2nd choice - (*) Do not show any images
  webElemsNOTSHOWIMGRADIO <- remDr$findElements('name', 'images')
  webElemsNOTSHOWIMGRADIO[[2]]$highlightElement()
  webElemsNOTSHOWIMGRADIO[[2]]$clickElement()
  Sys.sleep(1.0)
  
  # DONE button
  webElemCONCONFBTN <- remDr$findElement('css', '#content-settings-overlay-confirm')
  webElemCONCONFBTN$highlightElement()
  webElemCONCONFBTN$clickElement()
  Sys.sleep(1.0)
  
  return(remDr)
}




# e.g.
# manageRSeleniumServer(curr_port = 4444, doif = "if_server_not_started_please_start")
# e.g.
# manageRSeleniumServer(curr_port = 4444, doif = "if_server_not_stopped_please_stop")


manageRSeleniumServer <- function(curr_port = 4444, doif = "if_server_not_started_please_start" ) {
  
  require(RSelenium)
  require(stringr)
  
  # get the parent port 
  child_port <- as.character(curr_port)
  tcpip_network_connections <- system2("netstat", args="-o -a -n ", stdout = TRUE)
  tcpip_network_connections_port_of_interest_index <- str_detect(tcpip_network_connections, child_port)
  
  
  if(doif == "if_server_not_started_please_start" ) {
    
    if(!any(tcpip_network_connections_port_of_interest_index)) {
      
      print(paste0("Begin starting seleium server on port ", curr_port)) # 24 hour time out
      # default # 4444 # java -jar selenium-server-standalone.jar -h
      startServer(args = c(paste0("-port ", curr_port),"-timeout 86400","-browserTimeout 86400"))  # default # 4444 # java -jar selenium-server-standalone.jar -h
      print(paste0("End starting seleium server on port ", curr_port))
      Sys.sleep(10.0) # 10 second wait
      
    }
    
  }
  
  if(doif == "if_server_not_stopped_please_stop" ) {
    
    # is at least one value TRUE? 
    tcpip_network_connections_interested <- c()
    parent_port <- ""
    if(any(tcpip_network_connections_port_of_interest_index)) {
      
      tcpip_network_connections_interested <- tcpip_network_connections[tcpip_network_connections_port_of_interest_index]
      parent_port <- unique(str_replace(str_extract(tcpip_network_connections_interested,"[ ]\\d+$"),"[ ]",""))
      
      # kills parent and dependent children(java.exe and chromedriver.exe) DOES NOT kill chrome.exe
      print(paste0("begin killing child port ",child_port," and parent port ( and children ) ", parent_port))
      system2("taskkill", args=paste0(" /F /T /PID ", parent_port), stdout = TRUE) # need print() to print
      print(paste0("end killing child port ",child_port," and parent port ( and children ) ", parent_port))
    }
    
    
  }
  
  
}

############ 
############ 


# e.g timednavigate(remDr,"http://www.microsoft.com", timeout = 100)
# note: use 100 as a 'good' minimum.  Faster speeds may be faster thanthe windows clock

timednavigate <- function(remDr = remDr, url, timeout = 10000) {
  
  require(RSelenium)
  require(tcltk)
  
  do <- function() {
    remDr$navigate(url) 
    result_url <- remDr$getCurrentUrl()[[1]]
    # print(paste0("timednaviage done to : ", result_url))
    assign(x="result_url",value=result_url,envir = parent.frame())
  }
  do_quit <- function() {
    remDr$navigate("about:blank")
    result_url <- remDr$getCurrentUrl()[[1]]
    # print(result_url)
    assign(x="result_url",value=result_url,envir = parent.frame())
  }

  do_quit_task_id <- tcl("after", timeout, do_quit )
  do()
  # if I made it this far - if I navigated and returned within *TIME*
  tcl("after", "cancel", do_quit_task_id) 
  # print(result_url)
  
  if( result_url == "about:blank" ) { 
    # print("navigate timed out")
    return(paste0("TIMED_NAVIGATE_TIMEOUT: ", url))
  } else {
    return(paste0("TIMED_NAVIGATE_SUCCESS: ", url))
  }
}



