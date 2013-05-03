@echo off
rem **************************************************************************
rem Uses discovery to identify the available config files and build the
rem start-up string for Mule. Depends on Win2K command extnesions.
rem
rem Usage: 
rem    winstart mode [devmode]
rem  where mode is one of client, distribute, and standalone
rem        devmode, when present, starts mule in EDEX dev mode.
rem
rem from start-client.bat:
rem  Using relative paths to ease moving installation around.
rem  Uses the MULE ESB launcher to build the classpath.  Any
rem  application jar in ..\lib directory gets added to the
rem  run time classpath within MULE.
rem 
rem  Uses the Spring configuration builder which means that the MULE 
rem  configuration is defined in Spring Beans.  This allows one configuration
rem  file to be used to configure both Spring and Mule.
rem
rem History:
rem --------------------------------------------------------------------------
rem Apr 2007    MW Fegan          Created. Partially based on start-XXX.bat
rem 06Jun2007   MW Fegan   DR279  Added optional devmode flag. 
rem 28Oct2008   MW Fegan   DR1524 Added export of MULE_HOME       
rem **************************************************************************


rem ---------------------
rem localize the env.
rem ---------------------
setlocal
setlocal ENABLEDELAYEDEXPANSION

rem ---------------------
rem set basic env.
rem ---------------------
SET MULE_HOME=..\..\..\mule
SET MULE_LIB=%MULE_HOME%\lib
SET JYTHON_HOME=..\..\..\jython
SET CUSTOM_LIB=..\lib
SET CONF_DIR=..\conf
SET PLUGIN_DIR=%MULE_HOME%\lib\user\plugins
SET CLASSPATH=%CONF_DIR%;%JYTHON_HOME%
SET HIBERNATE_CONF=../conf/db

rem ---------------------
rem set the config files
rem to be ignored
rem ---------------------
SET GLOBAL_XML=global.xml
SET IGNORE_SET=%GLOBAL_XML%,vtec.xml
SET EDEX_MODE=%1
SHIFT
IF /I %EDEX_MODE%. == client. (
   SET IGNORE_SET=%IGNORE_SET%,fileToJMS.xml,purge.xml
) ELSE (
   IF /I %EDEX_MODE%. == distribute. (
      SET IGNORE_SET=%IGNORE_SET%,ingest.xml
   ) ELSE (
      IF /I NOT %EDEX_MODE%. == standalone. (
         ECHO startup mode not specified, defaulting to standalone
      )
   )
)

rem ---------------------
rem check for dev mode
rem ---------------------
SET DEV_MODE=
IF "%1" == "" (
   SET DEV_MODE=off
) ELSE (
   SET DEV_MODE=on
)
SHIFT

rem ---------------------
rem hides the files that
rem are to be ignored
rem ---------------------
FOR %%d IN (%CONF_DIR% %PLUGIN_DIR%) DO (
   IF EXIST %%d\NUL (
      FOR %%s IN (%IGNORE_SET%) DO (
         IF EXIST %%d\%%s (
            ATTRIB +h %%d\%%s
         )
      )
   )
)

rem ---------------------
rem start with the global
rem config file
rem ---------------------
SET FILE_LIST=file:%CONF_DIR%\%GLOBAL_XML%

java -jar rea.jar %MULE_HOME%/lib/user/plugins ../conf
java -jar rea.jar %MULE_HOME%/lib/user/services ../conf

rem ---------------------
rem find the config files 
rem to load
rem ---------------------
FOR %%d IN (%CONF_DIR% %PLUGIN_DIR%) DO (
   IF EXIST %%d\NUL (
      FOR %%a IN (%%d\*.xml) DO (
         SET FILE_LIST=!FILE_LIST!, file:%%a
      )
   )
)

echo about to call %MULE_HOME%\bin\mule.bat

rem ---------------------
rem start the application
rem ---------------------
CALL %MULE_HOME%\bin\mule.bat console ^
-config "%FILE_LIST%" ^
-builder org.mule.extras.spring.config.SpringConfigurationBuilder

:END_OF_SCRIPT
rem ---------------------
rem un hide files that
rem were ignored
rem ---------------------
FOR %%d IN (%CONF_DIR% %PLUGIN_DIR%) DO (
   IF EXIST %%d\NUL (
      FOR %%s IN (%IGNORE_SET%) DO (
         IF EXIST %%d\%%s (
            ATTRIB -h %%d\%%s
         )
      )
   )
)

endlocal
