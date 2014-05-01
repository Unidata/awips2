## This file should be placed in the root directory of your project.
## Then modify the CMakeLists.txt file in the root directory of your
## project to incorporate the testing dashboard.
## # The following are required to uses Dart and the Cdash dashboard
##   ENABLE_TESTING()
##   INCLUDE(CTest)
set(CTEST_PROJECT_NAME "qpid-cpp")
set(CTEST_NIGHTLY_START_TIME "20:00:00 EST")

set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "www.riverace.com")
set(CTEST_DROP_LOCATION "/CDash-1.4.2/submit.php?project=qpid-cpp")
set(CTEST_DROP_SITE_CDASH TRUE)
