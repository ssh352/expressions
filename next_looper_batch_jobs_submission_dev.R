
# ** REQUIRED** TO BE THE TOP LINES OF THE FILE
options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

# FIX: visit_everyone_thin_atheltic_within_last_week -- chromedriver.exe dialog error
# [ ] GOT A STOPPER MESSAGE BOX ( NEED EACH TO RUN IN ITS OWN BATCH FILE: NOT THE SAME: DIALOG BOX AT 24 HANGS ZK PROGRAM



length(args)
print(args)

# > length(args)
# [1] 3
# > print(args)
# [1] "--args" 
# [2] "ok"
# [3] "visit_everyone_all_body_within_last_week"


# NOTE: FOR VARIETY . . . I CAN CHANGE THE "MESSAGE OF THE DAY"
# todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?")


if(Sys.getenv("RSTUDIO") == "1") {
  debugSource(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
} else {
  source(paste0(getwd(),"/","utilities_ext_visit_looper_dev.R"))
}
options(error=NULL)

todays_message = paste0(", wonderful ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you this evening?")

# debugSource('J:/YDrive/All_NewSeduction/All_ElectronicSpeech/RSeleniumAndBrowsers/AES1/next_looper_batch_jobs_submission_dev.R')
# parsecheck <- function () {

# How can I read command line parameters from an R script?
# http://stackoverflow.com/questions/2151212/how-can-i-read-command-line-parameters-from-an-r-script



#                    trailingOnly=TRUE means that only your arguments are returned, check:
# print(commandsArgs(trailingOnly=FALSE))

print(args)
str(args)

if( args[2] == "ok" ) {

  if(Sys.getenv("RSTUDIO") == "1") {
    debugSource(paste0(getwd(),"/","okcupid_visit_looper_dev.R"))
  } else {
    source(paste0(getwd(),"/","okcupid_visit_looper_dev.R"))
  }
  options(error=NULL)

  if( args[3] == "visit_everyone_all_body_within_last_week" ) {
    print(args[3])

    result_close <- tryCatch({ 

      # lately ( just prev dates - SOME not visit)
      ok_RETURN_visit <- okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "YOURPASSWORD", age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE")

    }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
    if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
      print("ERROR ok visit_everyone_all_body_within_last_week")
    }
  }

  if( args[3] == "message_everyone_all_body_online_now" ) {
    print(args[3])
  
    result_close <- tryCatch({ 

      # messaging - not previous dates
      ok_RETURN_message <- okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "YOURPASSWORD", age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all")  

    }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})
    if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
      print("ERROR ok message_everyone_all_body_online_now")
    }
    
  } # "message_everyone_all_body_online_now"

} # "ok"


if( args[2] == "pof" ) {

  if(Sys.getenv("RSTUDIO") == "1") {
    debugSource(paste0(getwd(),"/","pof_visit_looper_dev.R"))
  } else {
    source(paste0(getwd(),"/","pof_visit_looper_dev.R"))
  }
  options(error=NULL)
  
  if( args[3] == "visit_everyone_all_body_within_last_week" ) {
    print(args[3])

    result_close <- tryCatch({ 

      # just visit
      # pof_visit_looper_dev() - body_type = "anything" - online_when = "within_the_last_week"
      pof_RETURN_visit <- pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "era674smartest01", site_password = "YOURPASSWORD", age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") # default

    }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})

    if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
      print("ERROR pof visit_everyone_all_body_within_last_week")
    }

  } # "visit_everyone_all_body_within_last_week"
  
  if( args[3] == "message_everyone_thin_athletic_online_now_T_online_today_ONLY" ) {
    print(args[3])

    result_close <- tryCatch({ 

      # send a message
      # FIRST OF TWO
      # send a message - body_type = "thin_athletic" - "online_now"
      pof_RETURN_message_01 <- pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "era674smartest01", site_password = "YOURPASSWORD", age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_now", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

    }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})

    if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
      print("ERROR pof 1 message_everyone_thin_athletic_online_now_T_online_today_ONLY")
    }

    Sys.sleep(30.0)
    
    result_close <- tryCatch({ 

      # SECOND OF TWO
      # send a message - body_type = "thin_athletic" - "online_today_ONLY" ( may end up at early 20 year olds )
      pof_RETURN_message_02 <- pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "era674smartest01", site_password = "YOURPASSWORD", age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "online_today_ONLY", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

    }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})

    if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
      print("ERROR pof 2 message_everyone_thin_athletic_online_now_T_online_today_ONLY")
    }

  } # "message_everyone_thin_athletic_online_now_T_online_today_ONLY"

} # "pof"


if( args[2] == "zk" ) {

  if(Sys.getenv("RSTUDIO") == "1") {
    debugSource(paste0(getwd(),"/","zk_visit_looper_dev.R"))
  } else {
    source(paste0(getwd(),"/","zk_visit_looper_dev.R"))
  }
  options(error=NULL)

  if( args[3] == "visit_everyone_thin_atheltic_within_last_week" ) {
    print(args[3])

    run_age_range <- 18:49
    for(run_age_now in run_age_range) { 

      run_age_now_vec_range_str <- paste0(run_age_now,":",run_age_now)
    
      result_close <- tryCatch({ 

        # uses run_age_now_vec_range_str
        # just visit - online_when = "within_the_last_week" - FIREFOX
        
        # just visit - online_when = "within_the_last_week"
        zk_RETURN_visit <- zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "epoch536intel@gmail.com", site_password = "YOURPASSWORD", age_range_str = run_age_now_vec_range_str, todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

      }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})

      if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
        print(paste0("ERROR zk visit_everyone_thin_atheltic_within_last_week", run_age_now_vec_range_str))
      }
    
    } # for(run_age_now in run_age_range)
    
  } # "visit_everyone_thin_atheltic_within_last_week"

  if( args[3] == "message_everyone_thin_atheltic_within_last_week" ) {
    print(args[3])

    run_age_range <- 18:49
    for(run_age_now in run_age_range) {
      print(paste0("run_age_range is ", run_age_range))
      print(paste0("run_age_now is ", run_age_now))

      run_age_now_vec_range_str <- paste0(run_age_now,":",run_age_now)
      
      result_close <- tryCatch({ 

        # uses run_age_now_vec_range_str
        # use production - action = "message_greet_matchname", online_when = "within_the_last_week"
        zk_RETURN_message <- zk_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "epoch536intel@gmail.com", site_password = "YOURPASSWORD", age_range_str = run_age_now_vec_range_str, todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "message_greet_matchname", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

      }, warning = function(w) {}, error = function(e) { return("ERROR") }, finally = {})

      if(class(result_close) == "character" &&  result_close == "ERROR" ) { 
        print(paste0("ERROR zk message_everyone_thin_atheltic_within_last_week ", run_age_now_vec_range_str))
      }
        
      print(paste0("run_age_now was ", run_age_now))
      print(paste0("run_age_range was ", run_age_range))
        
    } # for(run_age_now in run_age_range)

  } # "message_everyone_thin_atheltic_within_last_week"
  
} # zk



# } # parsecheck <- function ()


#   
#   


