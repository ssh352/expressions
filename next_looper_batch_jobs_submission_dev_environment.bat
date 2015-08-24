



REM *********** TO HALF OF R_AES1.bat ***********************

REM DO NOT remove 'ALL VARIABLES'
REM DO NOT add 'COMPILATION FLAGS' 
REM   else I can not 'check and build' in R Studio

REM devtoolsSRC
REM system.r line 12 with_envvar ... going into WithCAllingHandlers(expr ... MEMORY LEAK AT expr 
REM yellow gets back an 65999 number ( instead of a 'call' that the 'white does' )


REM for /f "tokens=1* delims==" %%a in ('set') do ( set %%a= )

REM windows batch switch help
REM SET /?
REM note: 'current drive with a slash:    %CD:~0,3%
REM Windows Batch Script Get Current Drive name
REM http://stackoverflow.com/questions/5811966/windows-batch-script-get-current-drive-name



REM RStudio QTApplication writes to these ...
REM  ---------------------------------------------
REM Administrator\Local Settings\Application Data
REM Application_Data\RStudio

set USERNAME=Administrator
set ALLUSERSPROFILE=%CD%\RDebug\All_Users
set APPDATA=%CD%\RDebug\Application_Data
set USERPROFILE=%CD%\RDebug\Administrator
set HOME=%CD%\RDebug\Home
set HOMEDRIVE=%CD:~0,2%
set HOMEPATH=%CD%\RDebug\Home

set LOCALAPPDATA=%CD%\RDebug\Administrator\AppData\Local


REM R studio seems to need these
set SystemDrive=C:
set SystemRoot=C:\WINDOWS


set PATH=C:\Program Files\RStudio\bin
set PATH=%PATH%;%CD%\Rtools\bin;%CD%\Rtools\gcc-4.6.3\bin;%CD%\Rtools\gcc-4.6.3\bin64
set PATH=%PATH%;%CD%\R-Portable\App\R-Portable\bin\x64

REM currenly not Windows7_64 adjusted yet

REM call XX_base.bat
REM call RX_base.bat

call OX_base.bat

set DEBUG=T 


REM set COPTFLAG=-O0 -g

REM set CFLAGS=-O0 -g
REM set FFLAGS=-O0 -g
REM set CXXFLAGS=-O0 -g

set TMP=%CD%\RDebug\Temp
set TEMP=%CD%\RDebug\Temp
set TMPDIR=%CD%\RDebug\Temp

REM most cases not necessary
set R_HOME=%CD%\R-Portable\App\R-Portable

REM *WANT THIS ONE*
REM ( but already in Rprofile.site )
set R_KEEP_PKG_SOURCE=yes

REM ? .Library
REM  .libPaths()                 # all library trees R knows about
set R_LIBS=%CD%\rlibraries\R_LIBS
set R_LIBS_USER=%CD%\rlibraries\R_LIBS_USER 
set R_LIBS_SITE=%CD%\rlibraries\R_LIBS_SITE

REM in chdir: There is a /D switch 
REM that needs to be added to chdir so that it changes the drive


REM *********** END OF TOP HALF OF R_AES1.bat ***********************
