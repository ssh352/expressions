REM @echo off

REM VERIFY CORRECT BY search for '$' ( SHOULD NOT FIND )
REM VERIFY CORRECT BY search for 'export' ( SOULD NOT FIND )
REM VERIFY CORRECT BY search for ':' ( SHOULD ONLY FIND AT 'DRIVE:' )

title PostgreSQL 11beta3
cls

REM ANDRE: CLEAN UP MY 'SHELL OF JUNK' 
set PATH=C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\

REM runtime pg_dump.exe NEEDS
REM
REM ftp://ftp.zlatkovic.com/libxml/64bit/zlib-1.2.8-win32-x86_64.7z
set PATH=C:\zlib-1.2.8-win32-x86_64\bin;%PATH%

:: set default code page
chcp 1252 > nul

REM :: psql.exe             uses file  pgpass.conf
REM :: pgAdmin pgAdmin3.exe uses files pgpass.conf and pgadmin_histoqueries.xml
REM :: all found in
REM :: %APPDATA%\postgresql

:: find base directories
set APPBASE=%CD%

:: set up postgres variables
set PGSQL=%APPBASE%
set PGDATA=%APPBASE%\data
set PGLOG=%APPBASE%\data\log.txt
set PGLOCALEDIR=%PGSQL%\share\
set PGDATABASE=postgres
set PGUSER=postgres
set PATH=%PGSQL%\bin;%PATH%

:: initialise a new database on first use
if not exist "%PGDATA%" (
    echo.
    echo Initialising database for first use, please wait...
    "%PGSQL%\bin\initdb" -U %PGUSER% -A trust -E utf8 --locale=C >nul
)

echo set PGSQL
set PGSQL

echo set PGDATA
set PGDATA

echo set PGLOG
set PGLOG

echo set PGLOCALEDIR
set PGLOCALEDIR

echo set PGDATABASE
set PGDATABASE

echo set PGUSER
set PGUSER

REM required for plr
set PATH=%PGSQL%\bin;%PATH%
set PATH=%PGSQL%\lib;%PATH%

REM R R version 3.5.1 (2018-07-02) -- "Feather Spray"
set     R_HOME=W:\R-3.5._\App\R-Portable

REM     seems 10 is required
set     PATH=%R_HOME%\bin\x64;%PATH%

REM **** must have COMPILED_IN tcl ***
REM **** must have COMPILED_IN PERL ***
REM **** must have COMPILED_IN Python ***

REM NOT required on Windows
REM TCLHOME=

REM distributions of Tcl/Tk
REM https://www.tcl.tk/software/tcltk/bindist.html
REM Binary distribution for Windows and Linux are also available from Thomas Perschak.
REM https://bitbucket.org/tombert/tcltk/downloads/
REM https://bitbucket.org/tombert/tcltk/downloads/tcltk86-8.6.8-5.tcl86.Win7.x86_64.tgz
set PATH=W:\tcltk86-8.6.8-5.tcl86.Win7.x86_64\bin;W:\tcltk86-8.6.8-5.tcl86.Win7.x86_64\lib;%PATH%

REM NOT required on Windows
REM PERLHOME=

REM Strawberry Perl
REM 5.26.2.1	2018-04-15
REM strawberry-perl-5.28.0.1-64bit-portable DOES NOT NOT WORK: new_* undefined errors
REM http://strawberryperl.com/releases.html
set PATH=W:\strawberry-perl-5.26.2.1-64bit-portable\perl\bin;%PATH%

REM Download matching EnterpriseDB distribution
REM From the EnterpriseDB installer
REM Look at requirements
REM 
REM Microsoft Visual C++ ??? Redistributable(x64) ???

REM Anaconda Distribution
REM Python 3.6.5 :: Anaconda, Inc.
REM Version 5.2 | Release Date: May 30, 2018
set PYTHONHOME=C:\ProgramData\Anaconda3
set PATH=%PYTHONHOME%;%PATH%
                        
REM IF openssl COMPILED in, then required LIBEAY32.dll, SSLEAY32.dll
REM target case MATTERS
REM set PATH=W:\openssl-1.0.2e-win32-x86_64\bin;%PATH%
copy /B /V /Y "W:\openssl-1.0.2e-win32-x86_64\bin\libeay32.dll" ".\bin\LIBEAY32.dll"
timeout 2
copy /B /V /Y "W:\openssl-1.0.2e-win32-x86_64\bin\ssleay32.dll" ".\bin\SSLEAY32.dll"
timout 2
REM IF uic(62.1) COMPILED in, then required libicuin62.dll, libicuuc62.dll (62)
REM set PATH=W:\icu-62.1\icu4c\dist\bin;W:\icu-62.1\icu4c\dist\lib;%PATH%
timout 2
REM "messagebox: not designed to run on windows"
REM copy /B /V /Y "W:\icu-62.1\icu4c\dist\lib\libicuin.dll.a" ".\bin\libicuin62.dll"
copy /B /V /Y "W:\icu-62.1\icu4c\dist\lib\icuin62.dll" ".\bin\libicuin62.dll"
timout 2
copy /B /V /Y "W:\icu-62.1\icu4c\dist\lib\icuuc62.dll" ".\bin\libicuuc62.dll"
timeout 2
REM IF xml2 COMPILED in, then required libxml2-2.dll, libgcc_s_seh-1.dll
REM set PATH=W:\libxml2-2.9.3-win32-x86_64\bin;W:\libxml2-2.9.3-win32-x86_64\lib;%PATH%
copy /B /V /Y "W:\libxml2-2.9.3-win32-x86_64\bin\libxml2-2.dll" ".\bin"
copy /B /V /Y "C:\x86_64-5.4.0-release-posix-seh-rt_v5-rev0\mingw64\x86_64-w64-mingw32\lib\libgcc_s_seh-1.dll" ".\bin"
copy /B /V /Y "C:\x86_64-5.4.0-release-posix-seh-rt_v5-rev0\mingw64\x86_64-w64-mingw32\lib\libstdc++-6.dll"    ".\bin"
copy /B /V /Y "W:\icu-62.1\icu4c\dist\lib\icuuc62.dll" ".\bin"
copy /B /V /Y "W:\icu-62.1\icu4c\dist\lib\icudt62.dll" ".\bin"
REM ftp://ftp.zlatkovic.com/pub/libxml/64bit/iconv-1.14-win32-x86_64.7z
REM xml2 OPTIONAL ?
copy /B /V /Y "W:\iconv-1.14-win32-x86_64\bin\libiconv-2.dll" ".\bin"
copy /B /V /Y "W:\zlib-1.2.8-win32-x86_64\bin\zlib1.dll"      ".\bin"

REM <stuck>
REM looks line an icu problem?
REM https://bbs.archlinux.org/viewtopic.php?id=239558
REM https://github.com/search?q=ucol_close_62+extension%3Ac&type=Issues
REM
REM procedural entry point ucol_close_62 could not be located in the dynamic link library postgres.exe 
REM 
REM ucol_close_62 IS ONLY found in ...
REM W:\R-3.5._\postgres\build\src\backend\utils\adt\pg_locale.o


timeout 2
echo set PATH
set PATH

::receives environment variables

REM start cmd /k
REM start cmd /k
REM start cmd /k

REM [ ] NOTORIZE
start cmd /k "%PGSQL%\bin\postgres"
start cmd /k "chcp 1252 > nul && timeout 5 && "%PGSQL%\bin\psql.exe""
REM start cmd /k "     "%PGSQL%\bin\pg_ctl" -D "%PGDATA%" stop -m fast"
    start cmd /k "echo "%PGSQL%\bin\pg_ctl" -D "%PGDATA%" stop -m fast""

REM ". . .\bin\pg_ctl.exe" runservice -N "postgresql-x64-9.5" -D "C:\Program Files\PostgreSQL\9.5\data" -w

REM (from a start cmd /k prompt), manual startup for DEEP debugging
REM "%PGSQL%\bin\postgres" -d 5 -p 5434
REM "%PGSQL%\bin\postgres"      -p 5434 --log_min_messages=debug5

REM chcp 1252 > nul && "%PGSQL%\bin\psql.exe" -p 5434

REM :: startup postgres server
REM echo.
REM "%PGSQL%\bin\pg_ctl" -D "%PGDATA%" -l "%PGLOG%" -w start -t 6000 -o "-p 5434" 
REM "%PGSQL%\bin\pg_ctl" -D "%PGDATA%" -l "%PGLOG%" -w start -t 6000 -o "-p 5434 --log_min_messages=debug5"
REM cls
REM echo.
REM echo Type \q to quit psql
REM echo.
REM chcp 1252 > nul && "%PGSQL%\bin\psql.exe" -p 5434
REM echo.
REM 
REM echo "%PGSQL%\bin\pg_ctl" -D "%PGDATA%" stop -m fast -o "-p 5434" 
REM 
REM pause 
REM 
REM "%PGSQL%\bin\pg_ctl" -D "%PGDATA%" stop -m fast -o "-p 5434"
REM 
REM pause
