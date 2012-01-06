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

rem File Name: jnibuild.bat
rem This batch file is used to build the native hdf libraries
rem

setlocal enabledelayedexpansion
pushd %~dp0

set nerrors=0
if "%1"=="/?" goto help
goto main

rem Print a help message
:help

	echo.Builds HDF Java Native Wrappers
	echo.
	echo.Usage: %~nx0 [OPTION]
	echo.
    echo.   /vs9                Build HDF Java Native using Visual Studio 2008
    echo.   /useenv             Build HDF Java Native using compiler settings defined
    echo.                       in the environment, rather than the IDE.
	echo.   /?                  Help Information
	
	exit /b 0

rem Parse through the parameters sent to file, and set appropriate variables
:parse_params

    for %%a in (%*) do (
		if "%%a"=="/vs9" (
            rem Use Visual Studio 2008
            set hdf_vs2008=true
            
        ) else if "%%a"=="/useenv" (
            rem Pass /useenv flag to devenv
            set hdf_useenv=true
            
        ) else if "%%a"=="/?" (
            rem Set errorlevel 1 and send to help
            call :help
            exit /b 1
            
        ) else (
            rem Set errorlevel 2 to send to help if we receive a bad parameter
            echo.Unknown option: %%a
            call :help
            exit /b 2
        )
    )
    
    exit /b 0

	
rem Setup our environment
:setup

	echo.Setting up environment
	
    rem Figure out which solution file to use based on configuration
    if defined hdf_vs2008 (
        echo.Using Visual Studio 2008
		set hdf_sln="%CD%\windows\proj\all\hdfjava.sln"
	)
    
	
    rem Make sure PROCESSOR_ARCHITECURE is set to either x86 or AMD64
    if "%PROCESSOR_ARCHITECTURE%"=="x86" (
        set hdf_platform=Win32
    ) else if "%PROCESSOR_ARCHITECTURE%"=="AMD64" (
        set hdf_platform=x64
		
    ) else (
        echo.Error: Environment variable PROCESSOR_ARCHITECTURE should contain
        echo.either x86 or AMD64
        exit /b 1
    )

    rem Setup Visual Studio environment. 
    
    set ccflags=
    
	rem Check for vs2008 environment
    if defined hdf_vs2008 (
		if defined vs90comntools (
			rem This sets the Visual Studio 2008 path and environment variables
			if %hdf_platform%==Win32 (
				call "%vs90comntools%\..\..\VC\vcvarsall.bat" x86
			) else (
				call "%vs90comntools%\..\..\VC\vcvarsall.bat" x86_amd64
			)
			
		) else (
			echo.Error: Cannot setup Visual Studio 2008 environment.  Please
			echo.make sure VS90COMNTOOLS is defined in the environment.
			exit /b 1
		)
    )   

    if defined hdf_useenv (
        rem This will tell Visual Studio to use include, library, etc. paths
        rem defined by %INCLUDE% %LIBPATH%, etc.  Assume the user has already
        rem added external library paths to these variables.
        set ccflags=%ccflags% /useenv
    )

    exit /b 0

	
rem Build the HDF Java Native Wrappers.
:all

    echo.*****************************************************************************
    echo.                        Build HDF Java Native Wrappers
    echo.*****************************************************************************
    echo.

    rem Build release versions
    for %%a in (Release) DO (
        echo.Building %hdf_platform% %%a Java Native Wrappers...
        devenv %hdf_sln% %ccflags% /rebuild "%%a|%hdf_platform%"
        if %errorlevel% neq 0 exit /b %errorlevel%
    )

    exit /b 0


rem Handle errors
:error

    rem For now, our error handling consists of setting nerrors and quitting
    echo.HDF Java Native Wrappers build failed.
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
    
    echo.Building HDF Java Native Wrappers
    echo.

    call :all
    if %errorlevel% neq 0 (
        echo.Error building HDF Java Native Wrappers!
        goto error
    )
	
	echo.Copy native libraries to native folder
	if exist windows\proj\all\Release\jhdf5.dll (
        copy /y windows\proj\all\Release\jhdf5.dll native > nul
    ) else (
        set /a nerrors=%nerrors%+1
    )
    if exist windows\proj\all\Release\jhdf5.lib (
        copy /y windows\proj\all\Release\jhdf5.lib native > nul
    ) else (
        set /a nerrors=%nerrors%+1
    )
	if exist windows\proj\all\Release\jhdf.dll (
        copy /y windows\proj\all\Release\jhdf.dll native > nul
    ) else (
        set /a nerrors=%nerrors%+1
    )
    if exist windows\proj\all\Release\jhdf.lib (
        copy /y windows\proj\all\Release\jhdf.lib native > nul
    ) else (
        set /a nerrors=%nerrors%+1
    )


    if "%nerrors%"=="0" (
        echo. All HDF Java Native Wrappers build successfully!
    )
    rem Fall through to end

:end
    popd
    endlocal & exit /b %nerrors%
