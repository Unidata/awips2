setlocal

SET ECLIPSE_HOME=%CD%\eclipse
SET BUILDER=%CD%

cd %ECLIPSE_HOME%\plugins\org.eclipse.pde.build_*
SET PDE_BUILD=%CD%
cd %BUILDER%

java -jar %ECLIPSE_HOME%\plugins\org.eclipse.equinox.launcher_*.jar ^
-application org.eclipse.ant.core.antRunner ^
-buildfile %PDE_BUILD%\scripts\productBuild\productBuild.xml ^
-DbaseLocation=%ECLIPSE_HOME% ^
-Dbuilder=%BUILDER% ^
-DbuildDirectory=%BUILDER%\tmp ^
-Dbase=%BUILDER%

endlocal