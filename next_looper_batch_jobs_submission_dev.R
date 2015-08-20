

# How can I read command line parameters from an R script?
# http://stackoverflow.com/questions/2151212/how-can-i-read-command-line-parameters-from-an-r-script

options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

length(args)
print(args)

#                    trailingOnly=TRUE means that only your arguments are returned, check:
# print(commandsArgs(trailingOnly=FALSE))


if( args[1] == "ok" ) {

  if(Sys.getenv("RSTUDIO") == "1") {
    debugSource(paste0(getwd(),"/","okcupid_visit_looper_dev.R"))
  } else {
    source(paste0(getwd(),"/","okcupid_visit_looper_dev.R"))
  }

  if( args[2] == "everyone_all_body_within_last_week" ) {
    print(args[2])

    # lately ( just prev dates - SOME not visit)
    ok_RemDr <- okcupid_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "time861wiz", site_password = "739heg08", age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "SOME", not_to_msg = "NONE")

  }

}


if( args[1] == "pof" ) {

  if(Sys.getenv("RSTUDIO") == "1") {
    debugSource(paste0(getwd(),"/","pof_visit_looper_dev.R"))
  } else {
    source(paste0(getwd(),"/","pof_visit_looper_dev.R"))
  }

  if( args[2] == "everyone_all_body_within_last_week" ) {
    print(args[2])

    # just visit
    # pof_visit_looper_dev() - body_type = "anything" - online_when = "within_the_last_week"
    pof_RemDr <- pof_visit_looper_dev(curr_port = 4444, browser = "chrome", use_the_custom_profile = FALSE, site_login = "era674smartest01", site_password = "739heg08", age_range_str = "23:44", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "NONE", body_type = "anything") # default

  }

}


if( args[1] == "zk" ) {

  if(Sys.getenv("RSTUDIO") == "1") {
    debugSource(paste0(getwd(),"/","zk_visit_looper_dev.R"))
  } else {
    source(paste0(getwd(),"/","zk_visit_looper_dev.R"))
  }

  if( args[2] == "everyone_thin_atheltic_within_last_week" ) {
    print(args[2])

    # just visit - online_when = "within_the_last_week" - FIREFOX
    zk_RemDr <- zk_visit_looper_dev(curr_port = 4444, browser = "firefox", use_the_custom_profile = FALSE, site_login = "epoch536intel@gmail.com", site_password = "739heg08", age_range_str = "18:49", todays_message = paste0(", happy ", weekdays(Sys.time() + 60 * 60 * dynamic_UTC_offset()), "! How are you today?"), action = "just_visit", online_when = "within_the_last_week", not_to_vst = "NONE", not_to_msg = "all_all", body_type = "thin_athletic")

  }

}







