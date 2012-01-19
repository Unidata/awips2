@echo OFF

REM TODO: Add logic to place the win32 versions of awips2-java and awips2-python
REM       into the path if they are not already present.

set HOME_DIRECTORY=%HOMEDRIVE%%HOMEPATH%
set CAVEDATA_LOG_DIRECTORY=%HOMEDRIVE%%HOMEPATH%\caveData\logs
set CONSOLE_LOG_DIRECTORY=%CAVEDATA_LOG_DIRECTORY%\consoleLogs\%COMPUTERNAME%
IF NOT EXIST "%CONSOLE_LOG_DIRECTORY%" (MKDIR "%CONSOLE_LOG_DIRECTORY%")

set RND=%random%
set RND_DATETIME_FILE=%TMP%\awips2dt_%RND%.tmp
REM Python is used to retrieve the current date and time because the order
REM of the Windows date/time fields is not necessarily guaranteed and the
REM Windows date/time fields can only be extracted using substring operations
REM instead of -formatter- strings like Linux allows.
python -c "from datetime import datetime; print datetime.now().strftime('%%Y%%m%%d_%%H%%M%%S');" > %RND_DATETIME_FILE%
set /p LOG_DATETIME= < %RND_DATETIME_FILE%
del %RND_DATETIME_FILE%

set CONTAINING_DIRECTORY=%~dp0
echo THIS CMD WINDOW CAN BE CLOSED AT ANY TIME!
"%CONTAINING_DIRECTORY%cave.exe" %* > "%CONSOLE_LOG_DIRECTORY%\cave_%LOG_DATETIME%.log" 2>&1
IF ERRORLEVEL 1 (echo CAVE WAS NOT SUCCESSFULLY STARTED - check the logs for additional information. && PAUSE)

EXIT