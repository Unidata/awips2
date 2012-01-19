@echo OFF
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

rem File Name: javainstall.bat
rem This batch file is used to install the java hdf libraries
rem

setlocal enabledelayedexpansion
pushd %~dp0

set nerrors=0
if "%1"=="/?" goto help
goto main

rem Print a help message
:help

	echo.Installs HDF JAVA
	echo.
	echo.Usage: %~nx0 [OPTION]
	echo.
    echo.   INSTALLDIR          Directory to install hdf-java libraries.
	echo.   /?                  Help Information
	
	exit /b 0

rem Parse through the parameters sent to file, and set appropriate variables
:parse_params

    for %%a in (%*) do (
		if "%%a"=="/?" (
            rem Set errorlevel 1 and send to help
            call :help
            exit /b 1
            
        ) else if "%%a"=="" (
            rem Set errorlevel 2 to send to help if we receive a bad parameter
            echo.Must supply a directory name
            call :help
            exit /b 2

		) else (
			set INSTALLDIR=%%a
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

	if "%INSTALLDIR%"=="" (
		set INSTALLDIR=hdfjava
	)
	
	if not exist "%INSTALLDIR%" (
		mkdir %INSTALLDIR%
		if %errorlevel% neq 0 (
			exit /b
		)
	) else (
		echo.Remove old  files
		call :safe_delete  %INSTALLDIR%
	)
	
	if not exist !%INSTALLDIR%!"\lib" (
		mkdir %INSTALLDIR%\lib
	)
	if not exist !%INSTALLDIR%!"\lib\ext" (
		mkdir %INSTALLDIR%\lib\ext
	)
	
	if not exist "win32lib" (
		echo.win32lib not found,
		echo.please verify that the hdf-java directory exists.
		goto error
	)
	
	set classpath=%INSTALLDIR%

    exit /b 0
	
rem Install the HDF Java Libraries.
:installall
    set nerrors=0

    echo.*****************************************************************************
    echo.                        Install HDF Java Libraries
    echo.*****************************************************************************
    echo.

    rem Install Libraries
    echo.Installing Java Libraries...
	call :safe_copy win32lib\*.jar %INSTALLDIR%\lib
	call :safe_copy win32lib\ext\*.jar %INSTALLDIR%\lib\ext
	call :safe_copy win32lib\*.lib %INSTALLDIR%\lib
	call :safe_copy win32lib\*.dll %INSTALLDIR%\lib
	
    exit /b %nerrors%

rem This function actally copies the file over, first making sure it exists.  If not, we increment nerrors
rem Expected parameters:
rem     %1 - name of file to copy
rem     %2 - destination to copy to
:safe_copy
    
    if exist %1 (
        copy /y %1 %2 > nul
    ) else (
        set /a nerrors=%nerrors%+1
    )
    
    exit /b
    

rem Only delete a file if it actually exists.  Return the status of delete if it was called
rem Expected paramters:
rem     %1 - name of file to delete
:safe_delete
    if exist %1 (
        del /S /F /Q %1 > nul
    )

    exit /b 
	
rem Install the hdfview script.
:installdocs
    set nerrors=0

rem	mkdir %INSTALLDIR%\javadocs
rem	mkdir %INSTALLDIR%\javadocs\images
rem	call :safe_copy docs\javadocs\images\*.gif %INSTALLDIR%\javadocs\images

	if not exist "%INSTALLDIR%\UsersGuide" (
		mkdir %INSTALLDIR%\UsersGuide
	)
	if not exist "%INSTALLDIR%\UsersGuide\images" (
		mkdir %INSTALLDIR%\UsersGuide\images
	)
	call :safe_copy docs\hdfview\index.html %INSTALLDIR%\UsersGuide
	call :safe_copy docs\hdfview\UsersGuide\*.html %INSTALLDIR%\UsersGuide
	call :safe_copy docs\hdfview\UsersGuide\images\*.gif %INSTALLDIR%\UsersGuide\images
 
    exit /b %nerrors%
	
rem Install the hdfview script.
:installview
    set nerrors=0

	call :safe_copy windows\hdfview.bat %INSTALLDIR%
	
    exit /b %nerrors%

rem Handle errors
:error

    rem For now, our error handling consists of setting nerrors and quitting
    echo.HDF install failed.
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
        echo.Error setting up install environment.
        goto error
    )
    
    call :installall
    if %errorlevel% neq 0 (
        echo.Error installing code.
        goto error
    )
    
    call :installdocs
    if %errorlevel% neq 0 (
        echo.Error installing docs.
        goto error
    )
    
    call :installview
    if %errorlevel% neq 0 (
        echo.Error installing hdfview.
        goto error
    )

    if "%nerrors%"=="0" (
        echo. All HDF Jars install successfully!
    )
    rem Fall through to end

:end
    popd
    endlocal & exit /b %nerrors%
