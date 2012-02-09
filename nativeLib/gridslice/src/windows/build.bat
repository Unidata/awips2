@echo OFF
REM This script will compile a Windows version of the gridslice library.
REM In order to compile the gridslice library, you will need to have
REM Microsoft Visual C++ 2008 installed and the AWIPS II Runtime Environment.
REM This script assumes that Microsoft Visual Studio has been installed in the
REM standard location - in the Program Files directory.
REM
REM This script should work on both a 32-bit and a 64-bit Windows 7 
REM installation.


SET CONTAINING_DIR=%~dp0

REM Determine what our architecture is.
SET REG_EXE=
SET PROGRAM_FILES_DIR=
IF "%PROCESSOR_ARCHITECTURE%" == "AMD64" (
   GOTO OS_64_BIT
) ELSE (
   IF "%PROCESSOR_ARCHITECTURE%" == "x86" (
      GOTO OS_32_BIT
   ) ELSE (
      echo "ERROR: Unrecognized Architecture."
      PAUSE
   )
)

REM Set the Program Files location based on the architecture.
:OS_32_BIT
   SET PROGRAM_FILES_DIR=%ProgramFiles%
   SET REG_EXE=C:\Windows\System32\reg.exe
   GOTO ARCH_KNOWN
:OS_64_BIT
   SET PROGRAM_FILES_DIR=%ProgramFiles(x86)%
   SET REG_EXE=C:\Windows\SysWOW64\reg.exe
   GOTO ARCH_KNOWN
:ARCH_KNOWN

REM Determine where AWIPS II Python has been installed.
SET A2_PYTHON_REG="HKLM\Software\Raytheon\Runtime Environment\AWIPS II Python"
%REG_EXE% QUERY %A2_PYTHON_REG% /v PythonInstallDirectory > NUL 2>&1
IF ERRORLEVEL 1 (
   echo ENVIRONMENT ERROR - Unable to find AWIPS II Python.
   PAUSE && EXIT 1
)
FOR /F "tokens=2* delims=	 " %%A IN (
   '%REG_EXE% QUERY %A2_PYTHON_REG% /v PythonInstallDirectory'
) DO (
   SET PythonInstallDirectory=%%B
)

REM Visual Studio 2008 is Version 9.0 of Microsoft Visual Studio.
SET MVS_VERSION=Microsoft Visual Studio 9.0

REM Use the MS Visual Studion vcvarsall.bat utility to prepare
REM the environment for this build.
REM Until further notice, we assume the build is 32-bit.
cd "%PROGRAM_FILES_DIR%\%MVS_VERSION%\VC"
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
   PAUSE
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
