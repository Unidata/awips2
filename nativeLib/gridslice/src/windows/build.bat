@echo OFF
REM This script will compile a Windows version of the gridslice library.
REM In order to compile the jep library, you will need to have
REM MSBuild 4.0 installed and the AWIPS II Runtime Environment.
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
:OS_64_BIT
   SET REG_EXE=C:\Windows\SysWOW64\reg.exe
:ARCH_KNOWN

REM Lookup the location of MSBuild.
SET MSBUILD_VERSION=4.0
SET MSBUILD_REG="HKLM\Software\Microsoft\MSBuild\ToolsVersions\%MSBUILD_VERSION%"

REM Verify that MSBuild is installed.
%REG_EXE% QUERY %MSBUILD_REG% /v MSBuildToolsPath > NUL 2>&1
IF ERRORLEVEL 1 (
   echo ENVIRONMENT ERROR - Unable to find MSBuild %MSBUILD_VERSION%.
   PAUSE && EXIT 1
)

FOR /F "tokens=2* delims=	 " %%A IN (
'%REG_EXE% QUERY %MSBUILD_REG% /v MSBuildToolsPath') DO (
SET MSBuildToolsPath=%%B)

pushd . > NUL 2>&1
cd %CONTAINING_DIR%
%MSBuildToolsPath%MSBuild.exe ^
	/p:Platform=win32 ^
	project\gridslice.sln
if ERRORLEVEL 1 (
   echo ERROR: The gridslice compile has failed.
   PAUSE && EXIT 1
)

popd > NUL 2>&1

echo.
echo.
echo The gridslice compile was successful.
PAUSE
