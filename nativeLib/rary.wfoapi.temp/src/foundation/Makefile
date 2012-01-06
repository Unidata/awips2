#-------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical support, and with no warranty, express or implied, as
# to its usefulness for any purpose.
#-------------------------------------------------------------------
# $Id: .Makefile__temp11029,v 1.11 2003/08/06 00:30:35 fluke Exp $
#-------------------------------------------------------------------
# Supported Targets:
#  all      - performs a prebuild, buildlib, and buildexe
#  prebuild - creates the h templates and publishes all h files
#  buildlib - builds the library and publishes it
#  buildexe - builds the mainsteam executable and publishes it
#  test     - builds the test executables and publishes them
#  data     - publishes all data files
#  doc      - extracts the documentation from source and publishes it
#  clean    - cleans up the directory
#  depend   - update Makefile dependencies
#-------------------------------------------------------------------

ANSI_COMPLIANT = YES

include ${STAGING}/Makefile.commonConfig

#-------------------------------------------------------------------
# Executables
#
BINS      = 
SCRIPTS   =
TEST_BINS = \
        bitArrayTest \
        coordTest \
	dictOfSetTest \
        dictTest \
        fdostreamTest \
	interTest \
        queueTests \
        seqTest \
	setTest \
        stopWatchTest \
        stringTest \
	swapBytesTest \
        timeTest \
	treeTest \
        twoDGridTest \
        uniqueID_Test

#-------------------------------------------------------------------
# Libraries
#
LIBS = libfoundation.a


#-------------------------------------------------------------------
# Installed header files
#
INSTALL_H_FILES = \
	AbsTime.H \
	BFStream.H \
	BitArray.H \
	CartCoord2D.H \
	CartCoord3D.H \
	CartDomain2D.H \
	CartTimeCoord.H \
	CartTimeDomain.H \
	CartTimeExtent.H \
	Dict.H \
	DictKP.H \
	DictOfSet.H \
	DictOfSetKP.H \
	DictOfSetPP.H \
	DictOfSetPV.H \
	DictPP.H \
	DictPV.H \
	ExtFtn.h \
	FDostream.H \
	Grid2D.H \
	Grid2DBit.H \
	Grid2DPackedBit.H \
	Grid3D.H \
	HashUtil.H \
	PolyLine.H \
	Queue.H \
	QueueImpl.H \
	QueuePtr.H \
	RCObject.H \
	RCPtr.H \
	SeqOf.H \
	SeqOfPtr.H \
	Set.H \
	SetP.H \
	StopWatch.H \
	StringTokenizer.H \
	SwapBytes.H \
	TextString.H \
	Thermo.H \
	TimeRange.H \
	Tree.H \
	UniqueID.H \
	commonDefs.h \
	utilityFuncs.H

#-------------------------------------------------------------------
# Installed *.C files, are installed with the *.H files in "inc"
#
INSTALL_C_FILES = \
	CartCoord2D.C \
	CartCoord3D.C \
	CartDomain2D.C \
	Dict.C \
	DictOfSet.C \
        DictOfSetKP.C \
	Grid2D.C \
	Grid3D.C \
	QueueImpl.C \
        SeqOf.C \
	SeqOfPtr.C \
	Set.C \
        Grid2D.C \
        Grid3D.C

#-------------------------------------------------------------------
# Noninstalled, private, implementation header files
#
IMPL_H_FILES = 

#-------------------------------------------------------------------
# Library C files whose objects go in an installed library
#
LIB_ANSI_C_FILES =

LIB_C_FILES =  \
        AbsTime.C \
        BitArray.C \
	CartCoord2D.C \
	CartDomain2D.C \
	CartTimeCoord.C \
	CartTimeDomain.C \
	CartTimeExtent.C \
	Dict.C \
	DictOfSet.C \
        DictOfSetKP.C \
        FDostream.C \
        HashUtil.C \
        Grid2D.C \
	Grid3D.C \
        Grid2DBit.C \
	Grid2DPackedBit.C \
	PolyLine.C \
	QueueImpl.C \
	SeqOf.C \
	SeqOfPtr.C \
	Set.C \
        StopWatch.C \
        StringTokenizer.C \
        TextString.C \
        TimeRange.C \
	Thermo.C \
        Tree.C \
        UniqueID.C \
        decrypt_char.C \
        encrypt_char.C \
	utilityFuncs.C
LIB_O_FILES = ${LIB_C_FILES:.C=.o} ${LIB_ANSI_C_FILES:.c=.o} 


#-------------------------------------------------------------------
# Test C files, different test executables could have different
# lists, but list ALL of them here so that their object dependencies
# will be generated.
#
TEST_C_FILES = \
        bitArrayTest.C \
        coordTest.C \
        dictTest.C \
        dictOfSetTest.C \
        fdostreamTest.C \
	interTest.C \
        queueTests.C \
        seqTest.C \
        stopWatchTest.C \
        stringTest.C \
	swapBytesTest.C \
        timeTest.C \
	treeTest.C \
        twoDGridTest.C \
        uniqueID_Test.C \
        setTest.C 

#-------------------------------------------------------------------
# Data files
#
DATA =


#-------------------------------------------------------------------
# library dependencies
#
libfoundation.a: ${LIB_O_FILES} ${OUR_MAKEFILES}
	-@rm -f $@
	${AR} qvc $@ ${LIB_O_FILES}


#-------------------------------------------------------------------
# Executable dependencies
#
SYS_LIBS = ${MATH_LIBRARIES}

OBJECTS = \
        libfoundation.a \
        ${LIB_LOG_STREAM} \
        ${LIB_FOUNDATION}

# GNU make static pattern rule replaces - ${TEST_BINS}: $$@.o ${TEST_OBJECTS}
${TEST_BINS}: %: %.o ${OBJECTS} ${OUR_MAKEFILES}
	${CXX} ${CXXFLAGS} -o $@ $@.o ${OBJECTS} ${SYS_LIBS} \
	|| { rm -f $@; false; }

${BINS}: %: %.o ${OBJECTS} ${OUR_MAKEFILES}
	${CXX} ${CXXFLAGS} -o $@ $@.o ${OBJECTS} ${SYS_LIBS} \
	|| { rm -f $@; false; }


#-------------------------------------------------------------------
# Standard targets and their rules.  If not using default, add to
# or replace the standard action.
#
prebuild: ${INSTALL_H_FILES} ${INSTALL_C_FILES}
	${STD_PREBUILD_ACTION}

buildlib: ${LIBS} ${NONLIB_O_FILES}
	${STD_BUILDLIB_ACTION}

buildlibgcc: buildlib

test: ${TEST_BINS}
	${STD_TEST_ACTION}

data: ${DATA}
	${STD_DATA_ACTION}

doc:  ${SOURCE_FILES}
	${STD_DOC_ACTION}

clean:
	${STD_CLEAN_ACTION}

depend:
	${STD_DEPEND_ACTION}

dependgcc: depend

#-------------------------------------------------------------------
# Object dependencies
#
-include Makefile.dependencies
