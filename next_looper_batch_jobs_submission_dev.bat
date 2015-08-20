

REM How to sleep for 5 seconds in Windows's Command Prompt? (or DOS)
REM http://stackoverflow.com/questions/1672338/how-to-sleep-for-5-seconds-in-windowss-command-prompt-or-dos


REM Displaying Windows command prompt output and redirecting it to a file
REM http://stackoverflow.com/questions/796476/displaying-windows-command-prompt-output-and-redirecting-it-to-a-file

REM start cmd /c "dir"  WILLL EXIT
REM start cmd /k "dir"  WILL STAY THERE

REM GOOD/BAD - we will see
REM ?Rscript
REM Unlike Unix-alikes, this links directly to 'R.dll' rather than
REM running a separate process.

REM wait until 3:30 p.m. ( 4:30 a.m. to 3:30 p.m. ) 11 * 60 * 60 = 39600   ( MAX 99999 ) 

%SystemRoot%\system32\timeout /T 10 /NOBREAK
REM echo  11 * 60 * 60
REM %SystemRoot%\system32\timeout /T 39600 /NOBREAK

# ok

start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R ok everyone_all_body_within_last_week"

  REM shell redirection of output to a text file ( and after print it to the console )
    REM  BUT see in the console 'AFTER THE RUN' ( WILL NOT BE 'ON THE FLY' )
  REM REM start cmd /k "Rscript --verbose next_batch_jobs_submission.R ok everyone_all_body_within_last_week  > OK_RUN_OUTPUT.txt 2>&1 && type OK_RUN_OUTPUT.txt"

REM wait 30 seconds between startun up browsers
REM %SystemRoot%\system32\timeout /T 5 /NOBREAK
%SystemRoot%\system32\timeout /T 40 /NOBREAK

# pof

start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R pof everyone_all_body_within_last_week"

REM wait 30 seconds between startun up browsers
REM %SystemRoot%\system32\timeout /T 7 /NOBREAK
%SystemRoot%\system32\timeout /T 50 /NOBREAK

# zk

start cmd /k "Rscript --verbose next_looper_batch_jobs_submission_dev.R zk everyone_thin_atheltic_within_last_week"





