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

rem File Name: javacheck.bat
rem This batch file is used to check the java hdf libraries
rem

setlocal enabledelayedexpansion
pushd %~dp0

set nerrors=0
if "%1"=="/?" goto help
goto main

rem Print a help message
:help

	echo.Checks HDF JAR
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

	if !%JAVAHOME%!"\bin\java.exe"=="\bin\java.exe" (
		echo.%JAVAHOME%\bin\java.exe not found,
		echo.please check your java home directory.
		goto error
	)
	set java_run=%JAVAHOME%\bin\java.exe
	
	if not exist "%TESTDIR%\win32lib" (
		mkdir %TESTDIR%\win32lib
	)
	if "%TESTLIBDIR%"=="" (
		set TESTLIBDIR=%TESTDIR%\win32lib
		if %errorlevel% neq 0 (
			exit /b
		)
	)

	call :safe_copy %TESTDIR%\native\*.lib %TESTLIBDIR%
	call :safe_copy %TESTDIR%\native\*.dll %TESTLIBDIR%
	
	set classpath=%TESTDIR%;%TESTLIBDIR%\jhdf5.jar;%TESTLIBDIR%\jhdfobj.jar;%TESTLIBDIR%\jhdf5obj.jar

    exit /b 0
	
rem Build the HDF Java Tests.
:buildtest

    echo.*****************************************************************************
    echo.                        Build HDF Java Tests
    echo.*****************************************************************************
    echo.

    rem Build sources
    echo.Building Java Sources...
    %java_compile% -source 5 -d %TESTDIR%\classes\ -cp %classpath%;%TESTDIR%\lib\junit.jar %TESTDIR%\test\object\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ -cp %classpath%;%TESTDIR%\lib\junit.jar %TESTDIR%\test\unittests\*.java
    if %errorlevel% neq 0 exit /b
    echo.Building Java Sources Succesful

    exit /b 0
	
rem Check the HDF Java Library.
:checktest

    echo.*****************************************************************************
    echo.                        Check HDF Java Library
    echo.*****************************************************************************
    echo.

    rem Check Library
    echo.Checking Java Object Library...
    %java_run% -Xmx1024M -Djava.library.path=%TESTLIBDIR% -cp %TESTDIR%\classes;%classpath% test.object.TestH5Object
    if %errorlevel% neq 0 exit /b
    echo.Checking Java Object Library Unit Tests...
    %java_run% -Xmx1024M -Djava.library.path=%TESTLIBDIR% -cp %TESTDIR%\classes;%classpath%;%TESTDIR%\lib\junit.jar test.unittests.AllH5ObjectTests
    if %errorlevel% neq 0 exit /b
    echo.Checking Java Object Library Successful

    exit /b 0
	
rem Build the HDF Java Examples.
:buildexample

    echo.*****************************************************************************
    echo.                        Build HDF Java Examples
    echo.*****************************************************************************
    echo.

    rem Build Examples
    echo.Building Java Examples...
    %java_compile% -source 5 -d %TESTDIR%\classes\ -cp %classpath% %TESTDIR%\examples\intro\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ -cp %classpath% %TESTDIR%\examples\groups\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ -cp %classpath% %TESTDIR%\examples\datasets\*.java
    if %errorlevel% neq 0 exit /b
    %java_compile% -source 5 -d %TESTDIR%\classes\ -cp %classpath% %TESTDIR%\examples\datatypes\*.java
    if %errorlevel% neq 0 exit /b
    echo.Building Java Examples Successful

    exit /b 0
	
rem Check the HDF Java Examples.
:checkexample

    echo.*****************************************************************************
    echo.                        Check HDF Java Examples
    echo.*****************************************************************************
    echo.

    rem Check Examples
    echo.Checking Java intro Examples...
    call :runexample intro.H5_CreateDataset
    call :runexample intro.H5_CreateAttribute
    call :runexample intro.H5_CreateFile
    call :runexample intro.H5_CreateGroup
    call :runexample intro.H5_CreateGroupAbsoluteRelative
    call :runexample intro.H5_CreateGroupDataset
    call :runexample intro.H5_ReadWrite
	
    echo.
    echo.*****************************************************************************
    echo.Checking Java groups Examples...
    call :runexample groups.H5Ex_G_Create
	call :safe_copy examples\groups\h5ex_g_iterate.h5 examples
    if "%nerrors%"=="0" (
		call :runexample groups.H5Ex_G_Iterate
	) else (
        echo.**FAILED**    groups.H5Ex_G_Iterate not tested
    )

    echo.
    echo.*****************************************************************************
    echo.Checking Java datsets Examples...
    call :runexample datasets.H5Ex_D_Alloc
    call :runexample datasets.H5Ex_D_Checksum
    call :runexample datasets.H5Ex_D_Chunk
    call :runexample datasets.H5Ex_D_Compact
    call :runexample datasets.H5Ex_D_External
    call :runexample datasets.H5Ex_D_FillValue
    call :runexample datasets.H5Ex_D_Gzip
    call :runexample datasets.H5Ex_D_Hyperslab
    call :runexample datasets.H5Ex_D_ReadWrite
    call :runexample datasets.H5Ex_D_Shuffle
    call :runexample datasets.H5Ex_D_Szip
    call :runexample datasets.H5Ex_D_UnlimitedAdd
    call :runexample datasets.H5Ex_D_UnlimitedGzip
    call :runexample datasets.H5Ex_D_UnlimitedMod

    echo.
    echo.*****************************************************************************
    echo.Checking Java datatypes Examples...
    call :runexample datatypes.H5Ex_T_Array
    call :runexample datatypes.H5Ex_T_ArrayAttribute
    call :runexample datatypes.H5Ex_T_Bit
    call :runexample datatypes.H5Ex_T_BitAttribute
    call :runexample datatypes.H5Ex_T_Commit
    call :runexample datatypes.H5Ex_T_Compound
    call :runexample datatypes.H5Ex_T_CompoundAttribute
    call :runexample datatypes.H5Ex_T_Float
    call :runexample datatypes.H5Ex_T_FloatAttribute
    call :runexample datatypes.H5Ex_T_Integer
    call :runexample datatypes.H5Ex_T_IntegerAttribute
    call :runexample datatypes.H5Ex_T_ObjectReference
    call :runexample datatypes.H5Ex_T_ObjectReferenceAttribute
    call :runexample datatypes.H5Ex_T_Opaque
    call :runexample datatypes.H5Ex_T_OpaqueAttribute
    call :runexample datatypes.H5Ex_T_String
    call :runexample datatypes.H5Ex_T_StringAttribute
	
	if !nerrors! neq 0 exit /b !nerrors!
    echo.Checking Java Examples Successful
	
    exit /b 0

:runexample
	
	pushd examples

	echo.
    %java_run% -Xmx1024M -Djava.library.path=%TESTLIBDIR% -cp %TESTDIR%\classes;%classpath% %1 > %1.out
	fc %1.out testfiles\%1.txt
    if %errorlevel% neq 0 (
        set /a nerrors=%nerrors%+1
		echo.**FAILED**    %1
    ) else (
		echo.  PASSED      %1
	)

	popd
	
	exit /b 0

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
	
    call :buildtest
    if %errorlevel% neq 0 (
        echo.Error building HDF Java Tests!
        goto error
    )
    
    call :checktest
    if %errorlevel% neq 0 (
        echo.Error checking HDF Java Library!
        goto error
    )
	
    call :buildexample
    if %errorlevel% neq 0 (
        echo.Error building HDF Java Examples!
        goto error
    )
    
    call :checkexample
    if %errorlevel% neq 0 (
        echo.Error checking HDF Java Examples!
        goto error
    )

    if "%nerrors%"=="0" (
        echo. All HDF Library check successfully!
    )
    rem Fall through to end

:end
    popd
    endlocal & exit /b %nerrors%
