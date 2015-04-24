@echo OFF

REM Determine where we are located.
SET CONTAINING_DIRECTORY=%~dp0

REM Prepare the environment.

REM Location of AWIPS II Java (the jre).
SET JavaJreDirectory="C:\Program Files\Raytheon\AWIPS II\Java\jre7"
REM Location of AWIPS II Python.
SET PythonInstallDirectory="C:\Program Files\Raytheon\AWIPS II\Python"

REM Add Java and Python to the path.
SET Path=%PythonInstallDirectory%;%PythonInstallDirectory%\DLLs;%Path%
SET Path=%JavaJreDirectory%\bin;%Path%
REM Define 'PythonPath'.
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

echo Starting ALERTVIZ; leave this CMD window open to enable AlertViz 'restart'.
REM Start AlertViz (and implement the alertviz restart capability).
:AlertVizLoopStart
SET RND=%random%
SET RND_DATETIME_FILE=%TMP%\awips2dt_%RND%.tmp
REM Python is used to retrieve the current date and time because the order
REM of the Windows date/time fields is not necessarily guaranteed and the
REM Windows date/time fields can only be extracted using substring operations
REM instead of -formatter- strings like Linux allows.
python -c "from datetime import datetime; print datetime.now().strftime('%%Y%%m%%d_%%H%%M%%S');" > %RND_DATETIME_FILE%
SET /p LOG_DATETIME= < %RND_DATETIME_FILE%
DEL %RND_DATETIME_FILE%

SET LOGFILE_CONSOLE=%CAVEDATA_LOG_DIRECTORY%\alertviz_%LOG_DATETIME%_console.log
SET LOGFILE_ALERTVIZ=%CAVEDATA_LOG_DIRECTORY%\alertviz_%LOG_DATETIME%_admin.log

"%CONTAINING_DIRECTORY%alertviz.exe" %* > "%CONSOLE_LOG_DIRECTORY%\alertviz_%LOG_DATETIME%.log" 2>&1
IF %ERRORLEVEL% == 0 (EXIT)
echo Restarting AlertViz.
GOTO AlertVizLoopStart

