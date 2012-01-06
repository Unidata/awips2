#-------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical support, and with no warranty, express or implied, as
# to its usefulness for any purpose.
#-------------------------------------------------------------------
# $Id: .Makefile__temp7312,v 1.5 2005/07/29 20:57:55 fluke Exp $
#-------------------------------------------------------------------
# Supported Targets:
#   all      - prebuild, buildlib, and buildexe
#   prebuild - Install header files in ${PROJECT_TREE}/inc
#   buildlib - Create libraries and install in ${PROJECT_TREE}/lib
#   buildexe - Create executables and install in ${PROJECT_TREE}/bin
#   test     - Create test executables
#   data     - Install data files in ${PROJECT_TREE}/data
#   doc      - Extract documentation from source and install in ${PROJECT_TREE}/doc
#   clean    - Remove everything that can be rebuilt
#   depend   - Update dependencies
#-------------------------------------------------------------------

include ${PROJECT_TREE}/src/Makefile.config

#-------------------------------------------------------------------
# Executables
#
ifeq ($(PLATFORM),HP-UX)
   BINS = 
else
   BINS = consoleUser getTestMode MonitorTestMode showBanner tmb tmb_exit tmbRemoteCheck tmcp
endif

SCRIPTS   = MonitorTestMode_script  showBanner_script
TEST_BINS =  test_WorkstationTestMode

#--- ----------------------------------------------------------------
# Libraries
#
LIBS = 


#-------------------------------------------------------------------
# Installed header files
#
INSTALL_H_FILES = WorkstationTestMode.H

#-------------------------------------------------------------------
# Noninstalled, private, implementation header files
#
IMPL_H_FILES = \
        getConsoleUser.H                 \
        hourglass.H                      \
        hourglassM.H                     \
        Procstruct.H                     \
        sockhelp.H                       \
	testmode.H                       \
	tmbUtil.H                	 

#-------------------------------------------------------------------
# Installed C files which are likely template implementations
#
INSTALL_C_FILES =

#-------------------------------------------------------------------
# Library C files whose objects go in an installed library
#
LIB_C_FILES = 
LIB_O_FILES = ${LIB_C_FILES:.C=.o}

#-------------------------------------------------------------------
# Nonlibrary C files, whose objects are installed as .o files
#
NONLIB_C_FILES = WorkstationTestMode.C
NONLIB_O_FILES = ${NONLIB_C_FILES:.C=.o}

#-------------------------------------------------------------------
# Noninstalled C files, no .o files installed, not put in a library
# that are included in all executables.
COMMON_C_FILES = 

COMMON_O_FILES = ${COMMON_C_FILES:.C=.o}

#-------------------------------------------------------------------
# Noninstalled C files, no .o files installed, not put in a library
# that are included in some executables but not all.
#
INDEPENDENT_C_FILES = 

# Add the above macros to the list of files that will be used to generate
# dependencies.
IMPLEMENTATION_FILES += ${INDEPENDENT_C_FILES}
IMPLEMENTATION_FILES += ${COMMON_C_FILES}

#-------------------------------------------------------------------
# data files
#
DATA = 

#-------------------------------------------------------------------
# Library dependencies
#
COMMON_OBJECTS = \
	${COMMON_O_FILES} \
	${IMPL_O_FILES} \
        ${LIB_FOUNDATION} \
 	${LIB_LOG_STREAM} \
        ${LIB_FOUNDATION}

UTIL_OBJECTS = \
	${LIB_LOG_STREAM}

SYS_LIBS = ${MATH_LIBRARIES} ${LIB_Z}
GUI_LIBS = ${X11_XMOTIF} ${X11}

CONSOLEUSER_OBJECTS = consoleUser.o getConsoleUser.o Procstruct.o
consoleUser: ${CONSOLEUSER_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${CONSOLEUSER_OBJECTS} ${SYS_LIBS}\
	|| { rm -f $@; false; }

GTM_OBJECTS = getTestMode.o  sockhelp.o ${COMMON_OBJECTS}
getTestMode: ${GTM_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${GTM_OBJECTS} ${INCLUDE_LIBRARIES} ${SYS_LIBS}\
	|| { rm -f $@; false; }

MTM_OBJECTS = MonitorTestMode.o sockhelp.o tmbUtil.o ${COMMON_OBJECTS}
MonitorTestMode: ${MTM_OBJECTS} ${OUR_MAKEFILES}
		time ${CXX} ${CXXFLAGS} -o $@ ${MTM_OBJECTS} ${INCLUDE_FILES} ${INCLUDE_LIBRARIES} ${SYS_LIBS}\
	|| { rm -f $@; false; }

TMCP_OBJECTS = tmcp.o tmbUtil.o ${COMMON_OBJECTS}
tmcp: ${TMCP_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${TMCP_OBJECTS} ${INCLUDE_FILES} ${INCLUDE_LIBRARIES} ${GUI_LIBS} ${SYS_LIBS}\
	|| { rm -f $@; false; }

TMB_EXIT_OBJECTS = tmb_exit.o  sockhelp.o ${COMMON_OBJECTS}
tmb_exit: ${TMB_EXIT_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${TMB_EXIT_OBJECTS} ${SYS_LIBS}\
	|| { rm -f $@; false; }

TMB_OBJECTS = tmb.o sockhelp.o tmbUtil.o ${COMMON_OBJECTS}
tmb: ${TMB_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${TMB_OBJECTS} ${INCLUDE_FILES} ${INCLUDE_LIBRARIES} ${SYS_LIBS}\
	|| { rm -f $@; false; }

TMB_RMC_OBJECTS = tmbRemoteCheck.o tmbUtil.o ${COMMON_OBJECTS}
tmbRemoteCheck: ${TMB_RMC_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${TMB_RMC_OBJECTS} ${SYS_LIBS}\
	|| { rm -f $@; false; }

SHWBNR_OBJECTS = showBanner.o tmbUtil.o ${COMMON_OBJECTS}
showBanner: ${SHWBNR_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${SHWBNR_OBJECTS} ${GUI_LIBS} ${SYS_LIBS}\
	|| { rm -f $@; false; }

TWTM_OBJECTS = test_WorkstationTestMode.o  WorkstationTestMode.o sockhelp.o ${COMMON_OBJECTS}
test_WorkstationTestMode: ${TWTM_OBJECTS} ${OUR_MAKEFILES}
	time ${CXX} ${CXXFLAGS} -o $@ ${TWTM_OBJECTS} ${SYS_LIBS}\
	|| { rm -f $@; false; }

#-------------------------------------------------------------------
# Standard targets and their rules
#
prebuild: ${INSTALL_H_FILES} ${INSTALL_C_FILES}
	${STD_PREBUILD_ACTION}

buildlib: ${LIBS} ${NONLIB_O_FILES} 
	${STD_BUILDLIB_ACTION}

buildlibgcc: 

buildexegcc: 

buildexe: ${BINS} ${SCRIPTS}
	${STD_BUILDEXE_ACTION}

test: ${TEST_BINS}
	${STD_TEST_ACTION}

data: ${DATA}
	${STD_DATA_ACTION}

doc: ${SOURCE_FILES}
	${STD_DOC_ACTION}

clean:
	${STD_CLEAN_ACTION}

depend: 
	${STD_DEPEND_ACTION}

dependgcc: depend

-include Makefile.dependencies
