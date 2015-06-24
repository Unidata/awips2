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

REM Used by logback configuration files to determine console and admin
SET LOGDIR=%HOME_DIRECTORY%\caveData\logs

echo THIS CMD WINDOW CAN BE CLOSED AT ANY TIME!
cd %HOMEPATH%
REM Start CAVE.
"%CONTAINING_DIRECTORY%cave.exe" %* 
IF ERRORLEVEL 1 (echo CAVE ERROR - check the logs for additional information. && PAUSE)

EXIT
