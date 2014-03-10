@echo OFF

REM Determine if we are running on a 32-bit or 64-bit OS.
IF NOT EXIST C:\Windows\SysWOW64\reg.exe (
   SET REG_EXE=C:\Windows\System32\reg.exe
) ELSE (
   SET REG_EXE=C:\Windows\SysWOW64\reg.exe
)

REM Determine where we are located.
SET CONTAINING_DIRECTORY=%~dp0

REM Prepare the environment.

REM Registry Query Variables.
SET A2_JAVA_REG="HKLM\Software\Raytheon\Runtime Environment\AWIPS II Java"
SET A2_PYTHON_REG="HKLM\Software\Raytheon\Runtime Environment\AWIPS II Python"
REM Determine where AWIPS II Java (the jre) is located.
%REG_EXE% QUERY %A2_JAVA_REG% /v JavaJreDirectory > NUL 2>&1
IF ERRORLEVEL 1 (echo ENVIRONMENT ERROR - Unable to find AWIPS II Java. && PAUSE && EXIT)
FOR /F "tokens=2* delims=	 " %%A IN (
'%REG_EXE% QUERY %A2_JAVA_REG% /v JavaJreDirectory') DO (
SET JavaJreDirectory=%%B)
REM Determine where AWIPS II Python is located.
%REG_EXE% QUERY %A2_PYTHON_REG% /v PythonInstallDirectory > NUL 2>&1
IF ERRORLEVEL 1 (echo ENVIRONMENT ERROR - Unable to find AWIPS II Python. && PAUSE && EXIT)
FOR /F "tokens=2* delims=	 " %%A IN (
'%REG_EXE% QUERY %A2_PYTHON_REG% /v PythonInstallDirectory') DO (
SET PythonInstallDirectory=%%B)

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
SET REG_EXE=
SET A2_JAVA_REG=
SET A2_PYTHON_REG=

REM Determine where we will be logging to.
SET HOME_DIRECTORY=%HOMEDRIVE%%HOMEPATH%
SET CAVEDATA_LOG_DIRECTORY=%HOMEDRIVE%%HOMEPATH%\caveData\logs
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
"%CONTAINING_DIRECTORY%alertviz.exe" %* > "%CONSOLE_LOG_DIRECTORY%\alertviz_%LOG_DATETIME%.log" 2>&1
IF %ERRORLEVEL% == 0 (EXIT)
echo Restarting AlertViz.
GOTO AlertVizLoopStart

