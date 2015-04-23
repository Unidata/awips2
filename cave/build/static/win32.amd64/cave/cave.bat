@echo OFF

REM Determine where we are located.
SET CONTAINING_DIRECTORY=%~dp0

REM Prepare the environment.

REM Location of AWIPS II Java (the jre).
SET JavaJreDirectory=C:\Program Files\Raytheon\AWIPS II\Java\jre7
REM Location of AWIPS II Python.
SET PythonInstallDirectory=C:\Program Files\Raytheon\AWIPS II\Python

REM Add Java and Python to the path.
SET Path=%PythonInstallDirectory%;%PythonInstallDirectory%\DLLs;%Path%
SET Path=%JavaJreDirectory%\bin;%Path%
REM Add the CAVE lib directory to the path.
SET Path=%CONTAINING_DIRECTORY%lib;%Path%
REM Define 'PythonPath'.
SET PythonPath=%CONTAINING_DIRECTORY%lib;%PythonPath%
SET PythonPath=%PythonInstallDirectory%\Lib\lib-tk;%PythonPath%
SET PythonPath=%PythonInstallDirectory%\DLLs;%PythonPath%
SET PythonPath=%PythonInstallDirectory%\Lib;%PythonPath%
SET PythonPath=%PythonInstallDirectory%;%PythonPath%

REM Eliminate variables that will no longer be used.
SET PythonInstallDirectory=
SET JavaJreDirectory=

REM Determine where we will be logging to.
SET HOME_DIRECTORY=%USERPROFILE%
SET CAVEDATA_LOG_DIRECTORY=%HOME_DIRECTORY%\caveData\logs
SET CONSOLE_LOG_DIRECTORY=%CAVEDATA_LOG_DIRECTORY%\consoleLogs\%COMPUTERNAME%
IF NOT EXIST "%CONSOLE_LOG_DIRECTORY%" (MKDIR "%CONSOLE_LOG_DIRECTORY%")

SET RND=%random%
SET RND_DATETIME_FILE=%TMP%\awips2dt_%RND%.tmp
REM Python is used to retrieve the current date and time because the order
REM of the Windows date/time fields is not necessarily guaranteed and the
REM Windows date/time fields can only be extracted using substring operations
REM instead of -formatter- strings like Linux allows.
python -c "from datetime import datetime; print datetime.now().strftime('%%Y%%m%%d_%%H%%M%%S');" > %RND_DATETIME_FILE%
SET /p LOG_DATETIME= < %RND_DATETIME_FILE%
DEL %RND_DATETIME_FILE%

SET LOGFILE_CAVE=%CAVEDATA_LOG_DIRECTORY%\cave_%LOG_DATETIME%_logs.log
SET LOGFILE_CONSOLE=%CAVEDATA_LOG_DIRECTORY%\cave_%LOG_DATETIME%_console.log
SET LOGFILE_PERFORMANCE=%CAVEDATA_LOG_DIRECTORY%\cave_%LOG_DATETIME%_perf.log
SET LOGFILE_PRODUCT_EDITOR=%CAVEDATA_LOG_DIRECTORY%\cave_%LOG_DATETIME%_productEditor.log

echo THIS CMD WINDOW CAN BE CLOSED AT ANY TIME!
cd %HOMEPATH%
REM Start CAVE.
"%CONTAINING_DIRECTORY%cave.exe" %* > "%CONSOLE_LOG_DIRECTORY%\cave_%LOG_DATETIME%.log" 2>&1
IF ERRORLEVEL 1 (echo CAVE ERROR - check the logs for additional information. && PAUSE)

EXIT
