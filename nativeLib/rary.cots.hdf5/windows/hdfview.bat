@ECHO OFF
rem Copyright by The HDF Group.
rem Copyright by the Board of Trustees of the University of Illinois.
rem All rights reserved.
rem
rem This file is part of HDF-JAVA.  The full HDF-JAVA copyright notice, including
rem terms governing use, modification, and redistribution, is contained in
rem the files COPYING and Copyright.html.  COPYING can be found at the root
rem of the source code distribution tree; Copyright.html can be found at the
rem root level of an installed copy of the electronic HDF-JAVA document set and
rem is linked from the top-level documents page.  It can also be found at
rem http://hdfgroup.org/HDF-JAVA/doc/Copyright.html.  If you do not have
rem access to either file, you may request a copy from helphdfgroup.org.

rem File Name: hdfview.bat
rem This batch file is used to execute the hdfview utility
rem

setlocal enabledelayedexpansion
pushd %~dp0

set nerrors=0
if "%1"=="/?" goto help
goto main

rem Print a help message
:help

	echo.Executes HDFVIEW
	echo.
	echo.Usage: %~nx0 [OPTION]
	echo.
	echo.   /?                  Help Information
	
	exit /b 0

rem Parse through the parameters sent to file, and set appropriate variables
:parse_params

    for %%a in (%*) do (
		if "%%a"=="/?" (
            rem Set errorlevel 1 and send to help
            call :help
            exit /b 1
		)
    )
    
    exit /b 0

rem Setup our environment
:setup
	echo.Setting environment
	if !%JAVAHOME%!"\bin\java.exe"=="\bin\java.exe" (
		echo.%JAVAHOME%\bin\java.exe not found,
		echo.please check your java home directory.
		goto error
	)
	set java_run=%JAVAHOME%\bin\java.exe
	
	set INSTALLDIR=%CD%\lib
	
	set CLASSPATH=%INSTALLDIR%\*;%INSTALLDIR%\ext\*

	set PATH=%INSTALLDIR%;%INSTALLDIR%\ext

    exit /b 0

rem Handle errors
:error

    rem For now, our error handling consists of setting nerrors and quitting
    echo.HDF check failed.
    set /a nerrors=%nerrors%+1
    goto end
    
    rem We'll never really get here, but we keep this line for consistency.
    exit /b


rem This is where the magic happens
:main

    call :parse_params %*
    if %errorlevel% neq 0 (
        if %errorlevel% equ 1 (
            rem This isn't an error case-- this means /? was specified.  Simply
            rem quit.
            goto end
            
        ) else (
            rem Error case.
            echo.Error parsing parameters!
            goto error
        )
    )
    
    call :setup
    if %errorlevel% neq 0 (
        echo.Error setting up build environment.
        goto error
    )

    if "%nerrors%"=="0" (
		%JAVAHOME%\bin\java -Xmx1024m -Djava.library.path=%PATH% -Dhdfview.root=%INSTALLDIR% -classpath %CLASSPATH% ncsa.hdf.view.HDFView -root %INSTALLDIR%
    )
    rem Fall through to end

:end
    popd
    endlocal & exit /b %nerrors%

