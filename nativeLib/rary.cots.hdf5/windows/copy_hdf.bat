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

    xcopy /y *.bat ..\ > nul

popd
endlocal & exit /b 
