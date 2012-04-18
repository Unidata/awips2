@echo OFF
REM This script will compile a Windows version of the meteolib library.
REM In order to compile the meteolib library, you will need to have
REM MinGW installed and the AWIPS II Runtime Environment.
REM
REM This script should work on both a 32-bit and a 64-bit Windows 7 
REM installation.

SET CONTAINING_DIR=%~dp0

pushd . > NUL 2>&1
cd %CONTAINING_DIR%
mingw32-make -f Makefile

IF ERRORLEVEL 1 (
   echo ERROR: the meteolib compile has failed.
   PAUSE && EXIT 1
)
popd > NUL 2>&1

IF NOT EXIST "%CONTAINING_DIR%..\build" (
   MKDIR "%CONTAINING_DIR%..\build"
)

REM Move the build artifacts to the build directory.
MOVE /Y "%CONTAINING_DIR%meteoLib.dll" ^
   "%CONTAINING_DIR%..\build"

REM Cleanup the remaining non-essential build artifacts.
IF EXIST "%CONTAINING_DIR%\Meteolibrary_JNI.o" (
   echo Y | DEL "%CONTAINING_DIR%\Meteolibrary_JNI.o"
)
IF EXIST "%CONTAINING_DIR%\Meteolibrary_JNI.c" (
   echo Y | DEL "%CONTAINING_DIR%\Meteolibrary_JNI.c"
)
IF EXIST "%CONTAINING_DIR%\meteoLib.h" (
   echo Y | DEL "%CONTAINING_DIR%\meteoLib.h"
)
IF EXIST "%CONTAINING_DIR%\meteoLib_Java" (
   RMDIR /S /Q "%CONTAINING_DIR%\meteoLib_Java"
)

echo.
echo.
echo The meteolib compile was successful.
PAUSE
