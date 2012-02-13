@echo OFF
REM This script will compile a Windows version of the gridslice library.
REM In order to compile the gridslice library, you will need to have
REM Microsoft Visual C++ 2008 installed and the AWIPS II Runtime Environment.
REM
REM This script should work on both a 32-bit and a 64-bit Windows 7 
REM installation.


SET CONTAINING_DIR=%~dp0

SET REG_EXE=
REM Determine what our architecture is.
IF "%PROCESSOR_ARCHITECTURE%" == "AMD64" (
   GOTO OS_64_BIT
) ELSE (
   IF "%PROCESSOR_ARCHITECTURE%" == "x86" (
      GOTO OS_32_BIT
   ) ELSE (
      echo "ERROR: Unrecognized Architecture."
      PAUSE && EXIT 1
   )
)

:OS_32_BIT
   SET REG_EXE=C:\Windows\System32\reg.exe
   GOTO ARCH_KNOWN
:OS_64_BIT
   SET REG_EXE=C:\Windows\SysWOW64\reg.exe
   GOTO ARCH_KNOWN
:ARCH_KNOWN

SET MVC_VERSION=9.0
REM TODO: Update so that we could also potentially use the full version of
REM       Visual Studio C++ 2008 if it is available.
SET A2_PYTHON_REG="HKLM\Software\Raytheon\Runtime Environment\AWIPS II Python"
SET MVC_REG="HKLM\Software\Microsoft\VCExpress\%MVC_VERSION%\Setup\VC"

%REG_EXE% QUERY %A2_PYTHON_REG% /v PythonInstallDirectory > NUL 2>&1
IF ERRORLEVEL 1 (
   echo ENVIRONMENT ERROR - Unable to find AWIPS II Python.
   PAUSE && EXIT 1
)
%REG_EXE% QUERY %MVC_REG% /v ProductDir > NUL 2>&1
IF ERRORLEVEL 1 (
   echo ENVIRONMENT ERROR - Unable to find Visual C++ 2008 Express.
   PAUSE && EXIT 1
)

REM Determine where AWIPS II Python has been installed.
FOR /F "tokens=2* delims=	 " %%A IN (
   '%REG_EXE% QUERY %A2_PYTHON_REG% /v PythonInstallDirectory'
) DO (
   SET PythonInstallDirectory=%%B
)

REM Determine where Microsoft Visual C++ 2008 Express has been installed.
FOR /F "tokens=2* delims=	 " %%A IN (
	'%REG_EXE% QUERY %MVC_REG% /v ProductDir'
) DO (
	SET VCPlusPlusProductDir=%%B
)

REM Use the MS Visual Studion vcvarsall.bat utility to prepare
REM the environment for this build.
REM Until further notice, we assume the build is 32-bit.
cd "%VCPlusPlusProductDir%"
CALL vcvarsall.bat x86
IF NOT ERRORLEVEL 0 (
   echo ERROR: Unable to prepare the environment.
   PAUSE && EXIT 1
)
cd "%CONTAINING_DIR%"

REM Compile gridslice
cl.exe /LD "%CONTAINING_DIR%..\sliceConvert.c" ^
   "%CONTAINING_DIR%..\gridslice.c" ^
   -I"%PythonInstallDirectory%\Lib\site-packages\numpy\core\include" ^
   -I"%PythonInstallDirectory%\include" ^
   "%PythonInstallDirectory%\libs\python27.lib" ^
   /link/out:gridslice.pyd /EXPORT:initgridslice
if ERRORLEVEL 1 (
   echo ERROR: The gridslice compile has failed.
   PAUSE && EXIT 1
)

REM Move the build artifacts to the build directory.
IF NOT EXIST "%CONTAINING_DIR%build" (
   MKDIR "%CONTAINING_DIR%build"
)
MOVE /Y "%CONTAINING_DIR%sliceConvert*" ^
   "%CONTAINING_DIR%build"
MOVE /Y "%CONTAINING_DIR%gridslice*" ^
   "%CONTAINING_DIR%build"

echo.
echo.
echo The gridslice compile was successful.
PAUSE