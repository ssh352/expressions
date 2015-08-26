

REM run by 
REM > J:
REM > cd J:\YDrive\All_NewSeduction\All_ElectronicSpeech\RSeleniumAndBrowsers\AES1
REM > next_looper_batch_jobs_submission_dev.bat visit 39600
REM > next_looper_batch_jobs_submission_dev.bat visit 10

REM > next_looper_batch_jobs_submission_dev.bat message  10


REM Is it possible to source a batch file in windows cmd like you can in unix?
REM http://stackoverflow.com/questions/12825790/is-it-possible-to-source-a-batch-file-in-windows-cmd-like-you-can-in-unix

cd "..\AES1_assistance" 
call ..\AES1\next_looper_batch_jobs_submission_dev_environment.bat
cd "..\AES1" 


echo Starting Running %0 %*
date /T
time /T

set WHAT_TO_DO=%1
echo WHAT_TO_DO=%WHAT_TO_DO%

set DELAY_TIME_START=%2
echo DELAY_TIME_START=%DELAY_TIME_START%


REM How to pass command line parameters to a batch file?
REM http://stackoverflow.com/questions/26551/how-to-pass-command-line-parameters-to-a-batch-file

  REM IF
  REM http://ss64.com/nt/if.html

REM How to sleep for 5 seconds in Windows's Command Prompt? (or DOS)
REM http://stackoverflow.com/questions/1672338/how-to-sleep-for-5-seconds-in-windowss-command-prompt-or-dos

REM Displaying Windows command prompt output and redirecting it to a file
REM http://stackoverflow.com/questions/796476/displaying-windows-command-prompt-output-and-redirecting-it-to-a-file

  REM shell redirection of output to a text file ( and after print it to the console )
    REM  BUT see in the console 'AFTER THE RUN' ( WILL NOT BE 'ON THE FLY' )
  REM REM start cmd /k "Rscript --verbose next_batch_jobs_submission.R ok everyone_all_body_within_last_week  > OK_RUN_OUTPUT.txt 2>&1 && type OK_RUN_OUTPUT.txt"


REM start cmd /c "dir"  WILLL EXIT
REM start cmd /k "dir"  WILL STAY THERE

REM GOOD/BAD - we will see
REM ?Rscript
REM Unlike Unix-alikes, this links directly to 'R.dll' rather than
REM running a separate process.

REM wait until 3:30 p.m. ( 4:30 a.m. to 3:30 p.m. ) 11 * 60 * 60 = 39600   ( MAX 99999 ) 

REM echo  4:30 a.m. to 3:30 p.m. 11 * 60 * 60 = 39600
REM %SystemRoot%\system32\timeout /T 4 /NOBREAK
%SystemRoot%\system32\timeout /T %DELAY_TIME_START% /NOBREAK

REM ok

if %WHAT_TO_DO%==visit (  
  echo VISITING ok
  start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args ok visit_everyone_all_body_within_last_week"
)
if %WHAT_TO_DO%==message (  
  echo MESSAGING ok
  start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args ok message_everyone_all_body_online_now"
)

REM wait 40 seconds between starting up browsers
REM %SystemRoot%\system32\timeout /T 5 /NOBREAK
%SystemRoot%\system32\timeout /T 80 /NOBREAK

REM pof

if %WHAT_TO_DO%==visit (  
  echo VISITING pof
  start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args pof visit_everyone_all_body_within_last_week"
)

if %WHAT_TO_DO%==message (  
  echo MESSAGING pof
  start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args pof message_everyone_thin_athletic_online_now_T_online_today_ONLY"

)

REM wait 50 seconds between starting up browsers
REM %SystemRoot%\system32\timeout /T 7 /NOBREAK
%SystemRoot%\system32\timeout /T 60 /NOBREAK

REM FOR /L ( NOTE: no-interactive: ONLY works inside a .BAT file )   L - range of numbers
REM Conditionally perform a command for a range of numbers.
REM http://ss64.com/nt/for_l.html
REM * and *
REM EnableDelayedExpansion
REM   EnableDelayedExpansion is Disabled by default.
REM   EnableDelayedExpansion can also be enabled by starting CMD with the /v switch.
REM   EnableDelayedExpansion can also be set in the registry under HKLM or HKCU:
REM http://ss64.com/nt/delayedexpansion.html
REM * and *
REM SETLOCAL
REM http://ss64.com/nt/setlocal.html
REM * and *
REM FOR
REM http://ss64.com/nt/for.html
REM NOTE: IF I WANT A SPECIFIC LIST:  for %%G in (a,b) do

REM zk

if %WHAT_TO_DO%==visit (  
  echo VISITING zk
  REM start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args zk visit_everyone_thin_atheltic_within_last_week"

  setlocal EnableDelayedExpansion 

  for /l %%G in (18,1,49) do ( 
    set _NOWAGE=%%G
    REM wait 3 minutes
    %SystemRoot%\system32\timeout /T 180 /NOBREAK
    echo NOWAGE is !_NOWAGE!
    start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args zk visit_everyone_thin_atheltic_within_last_week !_NOWAGE!"
  )
  setlocal DisableDelayedExpansion

)

if %WHAT_TO_DO%==message (  
  echo MESSAGING zk
  REM start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args zk message_everyone_thin_atheltic_within_last_week"

  setlocal EnableDelayedExpansion 

  for /l %%G in (18,1,49) do ( 
    set _NOWAGE=%%G
    REM wait 3 minutes
    %SystemRoot%\system32\timeout /T 180 /NOBREAK
    echo NOWAGE is !_NOWAGE!
    start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R --args zk message_everyone_thin_atheltic_within_last_week !_NOWAGE!"
  )
  setlocal DisableDelayedExpansion

)

REM

echo Just Finished Running %0 %*
date /T
time /T
echo WHAT_TO_DO=%WHAT_TO_DO%
echo DELAY_TIME_START=%DELAY_TIME_START%

REM  
REM  



