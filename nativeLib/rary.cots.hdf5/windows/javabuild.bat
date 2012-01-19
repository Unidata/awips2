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

rem File Name: javabuild.bat
rem This batch file is used to build the java hdf libraries
rem

setlocal enabledelayedexpansion
pushd %~dp0

set nerrors=0
if "%1"=="/?" goto help
goto main

rem Print a help message
:help

	echo.Builds HDF JAR
	echo.
	echo.Usage: %~nx0 [OPTION]
	echo.
    echo.   TESTDIR             Directory of hdf-java source.
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
			set TESTDIR=%%a
		)
    )
    
    exit /b 0

rem Setup our environment
:setup
	echo.Setting environment
	if !%JAVAHOME%!"\bin\javac.exe"=="\bin\javac.exe" (
		echo.%JAVAHOME%\bin\javac.exe not found,
		echo.please check your java home directory.
		goto error
	)
	set java_compile=%JAVAHOME%\bin\javac.exe
	
	if !%JAVAHOME%!"\bin\jar.exe"=="\bin\jar.exe" (
		echo.%JAVAHOME%\bin\jar.exe not found,
		echo.please check your java home directory.
		goto error
	)
	set java_pack=%JAVAHOME%\bin\jar.exe
	
	if not exist "%TESTDIR%\classes" (
		mkdir %TESTDIR%\classes
	)
	if not exist "%TESTDIR%\win32lib" (
		mkdir %TESTDIR%\win32lib
	)
	if "%TESTLIBDIR%"=="" (
		set TESTLIBDIR=%TESTDIR%\win32lib
		if %errorlevel% neq 0 (
			exit /b
		)
	)
	
	echo.Remove old jar files
	call :safe_delete  %TESTLIBDIR%\*.jar

	call :safe_copy %TESTDIR%\lib\netcdf.jar %TESTLIBDIR%
	call :safe_copy %TESTDIR%\lib\fits.jar %TESTLIBDIR%
	
	set classpath=%TESTDIR%;%TESTDIR%\classes;%TESTLIBDIR%\netcdf.jar;%TESTLIBDIR%\fits.jar;%TESTDIR%\lib\jargon.jar;%TESTDIR%\lib\junit.jar

    exit /b 0

rem cleanup our environment
:cleanup
	if  exist "%TESTDIR%\classes" (
		echo.Remove %TESTDIR%\classes.
		call :safe_delete  %TESTDIR%\classes
	)
	
    exit /b 0
	
rem Build the HDF Java Sources.
:buildall

    echo.*****************************************************************************
    echo.                        Build HDF Java Sources
    echo.*****************************************************************************
    echo.

    rem Build sources
    echo.Building Java Sources...
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\hdf5lib\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\hdf5lib\exceptions\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\hdflib\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\object\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\object\fits\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\object\h4\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\object\h5\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\object\nc2\*.java
    if %errorlevel% neq 0 exit /b
rem    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ext\npoess\*.java
rem    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\ncsa\hdf\view\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\test\object\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ %TESTDIR%\test\unittests\*.java
    if %errorlevel% neq 0 exit /b
    echo.Building Java Sources Successful

    exit /b 0
	
rem Build the HDF JARs.
:jarall
    set nerrors=0

    echo.*****************************************************************************
    echo.                        Build HDF JARs
    echo.*****************************************************************************
    echo.

	pushd %TESTDIR%\classes
	if not exist "ncsa\hdf\view\icons" (
		mkdir ncsa\hdf\view\icons
	)
	rem copy icons and html files
	call :safe_copy %TESTDIR%\ncsa\hdf\view\icons\*.gif ncsa\hdf\view\icons
	call :safe_copy %TESTDIR%\ncsa\hdf\view\*.html ncsa\hdf\view
	
    rem Build jars
	echo.Jaring ...
	%java_pack% -cf jhdf.jar ncsa\hdf\hdflib
	if %errorlevel% neq 0 exit /b
	%java_pack% -cf jhdf5.jar ncsa\hdf\hdf5lib
	if %errorlevel% neq 0 exit /b
	%java_pack% -cf jhdfview.jar ncsa\hdf\view
	if %errorlevel% neq 0 exit /b
	%java_pack% -cf jhdfobj.jar ncsa\hdf\object\*.class
	if %errorlevel% neq 0 exit /b
	%java_pack% -cf jhdf4obj.jar ncsa\hdf\object\h4\*.class
	if %errorlevel% neq 0 exit /b
	%java_pack% -cf jhdf5obj.jar ncsa\hdf\object\h5\*.class
	if %errorlevel% neq 0 exit /b
	%java_pack% -cf nc2obj.jar ncsa\hdf\object\nc2\*.class
	if %errorlevel% neq 0 exit /b
	%java_pack% -cf fitsobj.jar ncsa\hdf\object\fits\*.class
	if %errorlevel% neq 0 exit /b
rem	%java_pack% -cf npoess.jar ext\npoess\*.class
rem	if %errorlevel% neq 0 exit /b
	echo.Jaring Successful

	popd
	
    exit /b %nerrors%
	
rem Install the HDF JARs.
:install
    set nerrors=0

    echo.*****************************************************************************
    echo.                        Install HDF JARs
    echo.*****************************************************************************
    echo.
	
	if not exist "%TESTLIBDIR%\ext" (
		mkdir %TESTLIBDIR%\ext
	)

	call :safe_copy %TESTDIR%\classes\nc2obj.jar %TESTLIBDIR%\ext
	call :safe_copy %TESTDIR%\classes\fitsobj.jar %TESTLIBDIR%\ext
rem	call :safe_copy %TESTDIR%\classes\npoess.jar %TESTLIBDIR%\ext
	call :safe_copy %TESTDIR%\classes\jhdf.jar %TESTLIBDIR%
	call :safe_copy %TESTDIR%\classes\jhdf5.jar %TESTLIBDIR%
	call :safe_copy %TESTDIR%\classes\jhdfobj.jar %TESTLIBDIR%
	call :safe_copy %TESTDIR%\classes\jhdf4obj.jar %TESTLIBDIR%
	call :safe_copy %TESTDIR%\classes\jhdf5obj.jar %TESTLIBDIR%
	call :safe_copy %TESTDIR%\classes\jhdfview.jar %TESTLIBDIR%
	
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

rem Handle errors
:error

    rem For now, our error handling consists of setting nerrors and quitting
    echo.HDF build failed.
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
    
    call :cleanup
    if %errorlevel% neq 0 (
        echo.Error removing old files.
        goto error
    )
    
    echo.Building HDF Jars
    echo.
    call :buildall
    if %errorlevel% neq 0 (
        echo.Error building HDF Java Sources!
        goto error
    )

    call :jarall
    if %errorlevel% neq 0 (
        echo.Error jaring HDF Jars!
        goto error
    )

    call :install
    if %errorlevel% neq 0 (
        echo.Error installing HDF Jars!
        goto error
    )

    if "%nerrors%"=="0" (
        echo. All HDF Jars build successfully!
    )
    rem Fall through to end

:end
    popd
    endlocal & exit /b %nerrors%
