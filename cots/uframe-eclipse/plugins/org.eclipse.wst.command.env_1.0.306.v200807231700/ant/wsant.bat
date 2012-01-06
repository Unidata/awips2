echo off
rem *******************************************************************************
rem  Copyright (c) 2005, 2008 IBM Corporation and others.
rem  All rights reserved. This program and the accompanying materials
rem  are made available under the terms of the Eclipse Public License v1.0
rem  which accompanies this distribution, and is available at
rem  http://www.eclipse.org/legal/epl-v10.html
rem 
rem  Contributors:
rem      IBM Corporation - initial API and implementation
rem *******************************************************************************
setlocal

REM *********** Local envars ***************************

REM The JRE java.exe to be used by Ant.  Note: for WTP 2.0 the JDK needs to be 1.5 or higher.
set JAVAEXE="C:\jdk1.5.0\jre\bin\java.exe"

REM The Eclipse install directory.  Some Eclipse based products may refer to this directory
REM as the non shared directory.  Do not put quotes around this value.
set INSTALL_DIRECTORY=d:\productdirectory\eclipse

REM The shared Eclipse features directory.  Some Eclipse based products may split their 
REM directory structure into shared and non shared folders.  The variable below should be
REM set to the shared directory.  Note: a default WTP installation does not split it's
REM directory structure so the INSTALL_DIRECTORY and the SHARED_DIRECTORY should be the same.
REM Do not put quotes around this value.
set SHARED_DIRECTORY=%INSTALL_DIRECTORY%

REM The Eclipse Equinox Launcher jar.  Usually this plugin jar file is located in the
REM shared plugin directory(ie. plugins\org.eclipse.equinox.launcher*.jar )
set LAUNCHER_JAR="%SHARED_DIRECTORY%\plugins\org.eclipse.equinox.launcher_1.0.0.v20070606.jar"

REM The location of your workspace without the trailing path separator.
REM Do not put quotes around this value.
set WORKSPACE=C:\workspace

REM ****************************************************

if not exist %JAVAEXE% echo ERROR: incorrect java.exe=%JAVAEXE%, edit this file and correct the JAVAEXE envar
if not exist %JAVAEXE% goto done

if not exist %LAUNCHER_JAR% echo ERROR: incorrect launcher jar=%LAUNCHER_JAR%, edit this file and correct the LAUNCHER_JAR envar
if not exist %LAUNCHER_JAR% goto done

:run
@echo on
%JAVAEXE% -jar %LAUNCHER_JAR% -install "%INSTALL_DIRECTORY%" -application org.eclipse.ant.core.antRunner -data "%WORKSPACE%" -file wsgen.xml %* >wsgen.txt 2>&1

:done
pause
