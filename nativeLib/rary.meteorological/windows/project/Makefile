#-------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical support, and with no warranty, express or implied, as
# to its usefulness for any purpose.
#-------------------------------------------------------------------
# $Id: .Makefile__temp7312,v 1.14 2005/07/29 20:57:49 fluke Exp $
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

#include ${PROJECT_TREE}/src/Makefile.config

#-------------------------------------------------------------------
# Executables
#
# Possibly make an environment variable? If not set, use the default.
METEOLIBDIR = ..\..\\
SRCDIR	    = $(METEOLIBDIR)src
GLUEGENDIR  = $(METEOLIBDIR)gluegen
INCDIR      = $(METEOLIBDIR)inc
JavaJdkDirectory = C:\Program Files (x86)\Raytheon\AWIPS II\Java\jdk1.6.0_27

BINS        =
SCRIPTS     =
TEST_BINS   =
CC	    = mingw32-gcc -I$(INCDIR) -I"$(JavaJdkDirectory)\include" -I"$(JavaJdkDirectory)\include\win32" -g 
FC	    = mingw32-gfortran -I$(INCDIR) -fno-underscoring -fno-range-check

#-------------------------------------------------------------------
# Libraries
#

WINDOWSLIB = meteoLib.dll
LIBS = $(WINDOWSLIB)



#-------------------------------------------------------------------
# Installed header files
#
INSTALL_H_FILES = cmeteoLib.h


#-------------------------------------------------------------------
# Noninstalled, private, implementation header files
#
IMPL_H_FILES =


#-------------------------------------------------------------------
# Installed *.C files, are installed with the *.H files in "inc"
#
INSTALL_C_FILES = 


#-------------------------------------------------------------------
# Library ANSI C files whose objects go in an installed library
#
LIB_ANSI_C_FILES = \
              $(SRCDIR)\dist_filter.c \
              $(SRCDIR)\interp.c \
              $(SRCDIR)\temp_mixratio.c \
              $(SRCDIR)\calcWindChill.c \
              $(SRCDIR)\scaleless_analysis.c \
              $(SRCDIR)\adiabatic_te.c \
              $(SRCDIR)\calcHeatIndex.c \
              $(SRCDIR)\calckidx.c \
              $(SRCDIR)\thermoRtns.c \
              $(SRCDIR)\calctotidx.c \
              $(SRCDIR)\temp_of_te.c \
              $(SRCDIR)\heliComp.c \
              $(SRCDIR)\dcapeFunc.c \
	      $(SRCDIR)\sliceConvert.c \
              $(SRCDIR)\capeFunc.c
#-------------------------------------------------------------------
# Library C files whose objects go in an installed library
#
LIB_C_FILES =
LIB_F_FILES = \
              $(SRCDIR)\addaray.f  \
              $(SRCDIR)\addbycnst.f  \
              $(SRCDIR)\alt2press.f  \
              $(SRCDIR)\avwind.f \
              $(SRCDIR)\calccondpr.f  \
              $(SRCDIR)\calccondprdef.f  \
              $(SRCDIR)\calcdpd.f  \
              $(SRCDIR)\calcli.f  \
              $(SRCDIR)\calcpv.f  \
              $(SRCDIR)\calcrh.f  \
              $(SRCDIR)\calcrh2.f  \
              $(SRCDIR)\calctd.f  \
              $(SRCDIR)\calctd2.f  \
              $(SRCDIR)\calcthetae.f  \
              $(SRCDIR)\calcthetae2.f  \
              $(SRCDIR)\calctv.f  \
              $(SRCDIR)\calctv2.f  \
              $(SRCDIR)\calctw.f  \
              $(SRCDIR)\cclpar.f \
              $(SRCDIR)\constant.f  \
              $(SRCDIR)\comp_by.f  \
              $(SRCDIR)\crossvectors.f \
              $(SRCDIR)\ctop.f \
              $(SRCDIR)\cv_date2jul.f \
              $(SRCDIR)\cvgust.f \
              $(SRCDIR)\ddff.f \
              $(SRCDIR)\deftrk.f \
              $(SRCDIR)\density.f \
              $(SRCDIR)\derivative.f \
              $(SRCDIR)\derived_icing.f  \
              $(SRCDIR)\dgeocomps.f  \
              $(SRCDIR)\divaray.f \
              $(SRCDIR)\dotvectors.f \
              $(SRCDIR)\dzdlnp.f  \
              $(SRCDIR)\eqlev.f \
              $(SRCDIR)\eqp.f \
              $(SRCDIR)\esat.f  \
              $(SRCDIR)\exparay.f \
              $(SRCDIR)\fndiverg.f  \
              $(SRCDIR)\forecast.f \
              $(SRCDIR)\fortconbuf.f \
              $(SRCDIR)\frontogen.f  \
              $(SRCDIR)\frzlev.f \
              $(SRCDIR)\fsdiverg.f  \
              $(SRCDIR)\g2gkinematics.f  \
              $(SRCDIR)\gusts.f \
              $(SRCDIR)\hailsiz.f \
              $(SRCDIR)\helicity.f \
              $(SRCDIR)\hgt2pres.f \
              $(SRCDIR)\interp1.f \
              $(SRCDIR)\intpos.f \
              $(SRCDIR)\isenstable.f  \
              $(SRCDIR)\koffset.f  \
              $(SRCDIR)\lapserate.f  \
              $(SRCDIR)\lfcpar.f \
              $(SRCDIR)\lclpar.f \
              $(SRCDIR)\liftedp.f \
              $(SRCDIR)\lintrans.f  \
              $(SRCDIR)\matsln.f \
              $(SRCDIR)\maxmin.f \
              $(SRCDIR)\meanomega.f  \
              $(SRCDIR)\mixrat.f \
              $(SRCDIR)\mslp2thkns.f  \
              $(SRCDIR)\multaray.f \
              $(SRCDIR)\multbycnst.f  \
              $(SRCDIR)\mxtp.f \
              $(SRCDIR)\nadgdt.f \
              $(SRCDIR)\natlog.f \
              $(SRCDIR)\negarea.f \
              $(SRCDIR)\posarea.f \
              $(SRCDIR)\powercalc.f \
              $(SRCDIR)\press2alt.f  \
              $(SRCDIR)\presstable.f  \
              $(SRCDIR)\ptozsa.f \
              $(SRCDIR)\pvadv.f  \
              $(SRCDIR)\pvalue.f \
              $(SRCDIR)\pvpres.f  \
              $(SRCDIR)\qdiverg.f  \
              $(SRCDIR)\qvector.f  \
              $(SRCDIR)\radiation.f \
              $(SRCDIR)\radrtns.f \
              $(SRCDIR)\rang2d.f \
              $(SRCDIR)\replinrange.f \
              $(SRCDIR)\rhbar.f \
              $(SRCDIR)\rotvectors.f \
              $(SRCDIR)\setqsmooth.f  \
              $(SRCDIR)\slfront.f  \
              $(SRCDIR)\slqdiv.f  \
              $(SRCDIR)\slqvect.f  \
              $(SRCDIR)\smooth.f  \
              $(SRCDIR)\solax.f \
              $(SRCDIR)\spechum.f  \
              $(SRCDIR)\spechum2.f  \
              $(SRCDIR)\strmpak.f  \
	      $(SRCDIR)\subaray.f  \
              $(SRCDIR)\sunfuncs.f \
              $(SRCDIR)\sweat.f \
              $(SRCDIR)\sweatidx.f \
              $(SRCDIR)\tdofesat.f  \
              $(SRCDIR)\temp2theta.f  \
              $(SRCDIR)\theta2temp.f  \
              $(SRCDIR)\totals.f \
              $(SRCDIR)\tplcl.f \
              $(SRCDIR)\tsoar.f \
              $(SRCDIR)\tv2temp.f  \
              $(SRCDIR)\uvcomp.f \
              $(SRCDIR)\verpts.f \
              $(SRCDIR)\virttemp.f \
              $(SRCDIR)\virtualt.f \
              $(SRCDIR)\vvel.f \
              $(SRCDIR)\wbzero.f \
              $(SRCDIR)\windspeed.f \
              $(SRCDIR)\winddir.f \
              $(SRCDIR)\wndrho.f \
              $(SRCDIR)\ztopsa.f \
              $(SRCDIR)\BulkRichNo.f \
              $(SRCDIR)\thetawa.f \
              $(SRCDIR)\cgp.f \
              $(SRCDIR)\tpzlcl.f \
              $(SRCDIR)\pottemp.f \
              $(SRCDIR)\dmixr.f \
              $(SRCDIR)\pseudolift.f \
              $(SRCDIR)\vp.f \
	      $(SRCDIR)\IntrinsicFunctions.f
LIB_O_FILES = ${LIB_C_FILES:.C=.o} ${LIB_ANSI_C_FILES:.c=.o} \
              ${LIB_F_FILES:.f=.o} Meteolibrary_JNI.o


# Note: a patch to the HP-UX Fortran77 Compiler performed during Apr 1995
# requires fortcon to be compiled with the +e option, or else Byte
# variable are treated as Logical*1 variables with logical operators
# instead of Integer*1.  Add it to FFLAGS by setting USER_FFLAGS.
# Note that the only Fortran compiler this switch applies to is the HP
# Fortran77 compiler.
ifdef HP_FORTRAN77
     USER_FFLAGS = +e
endif

#-------------------------------------------------------------------
# Nonlibrary ANSI C files, whose objects are installed as .o files
#
NONLIB_ANSI_C_FILES =


#-------------------------------------------------------------------
# Nonlibrary C files, whose objects are installed as .o files
#
NONLIB_C_FILES =
NONLIB_O_FILES = ${NONLIB_C_FILES:.C=.o} ${NONLIB_ANSI_C_FILES:.c=.o}


#-------------------------------------------------------------------
# Nonlibrary ANSI C files, whose objects are installed as .o files
#
IMPL_ANSI_C_FILES =


#-------------------------------------------------------------------
# Noninstalled C files, no .o files installed, not put in a library
#
IMPL_C_FILES =
IMPL_O_FILES = ${IMPL_C_FILES:.C=.o} ${IMPL_ANSI_C_FILES:.c=.o}


#-------------------------------------------------------------------
# Test C files, different test executables could have different
# lists, but list ALL of them here so that their object dependencies
# will be generated.
#
TEST_C_FILES =

TEST_F_FILES = 
#-------------------------------------------------------------------
# data files
#
DATA =


#-------------------------------------------------------------------
# Library dependencies
#
${LIBS}: ${LIB_O_FILES}
	-@rm -f ${WINDOWSLIB}
	-@echo Creating Libraries  ${WINDOWSLIB}
	mingw32-gfortran -v -Wl,--export-all,--kill-at  -shared -lm -o ${WINDOWSLIB} ${LIB_O_FILES}
	-@echo File Created : ${WINDOWSLIB}

#-------------------------------------------------------------------
# Executable dependencies
#
SYS_LIBS =

OBJECTS =

gluegen_jar=$(METEOLIBDIR)../tool.gluegen/lib/gluegen.jar

meteoLib.h: 
	gcc -E -I$(INCDIR) -x c-header $(GLUEGENDIR)/meteolib.ggen -o meteoLib.h

Meteolibrary_JNI.c: meteoLib.cfg meteoLib.h
	java -jar ${gluegen_jar} -CmeteoLib.cfg meteoLib.h
       
Meteolibrary_JNI.o: Meteolibrary_JNI.c


#-------------------------------------------------------------------
# Test executable dependencies
#

#-------------------------------------------------------------------
# Standard targets and their rules.  If not using default, add to
# or replace the standard action.
#
prebuild: ${INSTALL_H_FILES} ${INSTALL_C_FILES}
	${STD_PREBUILD_ACTION}

buildlib: ${LIBS} ${NONLIB_O_FILES}
	${STD_BUILDLIB_ACTION}

buildlibgcc: buildlib

buildexe: ${BINS} ${SCRIPTS}
	${STD_BUILDEXE_ACTION}

buildexegcc: buildexe

test: ${TEST_BINS}
	${STD_TEST_ACTION}

data: ${DATA}
	${STD_DATA_ACTION}

doc:  ${SOURCE_FILES}
	${STD_DOC_ACTION}

clean:
	rm *.o meteoLib.h Meteolibrary_JNI.c *.dll *.so

depend:
	${STD_DEPEND_ACTION}

dependgcc: depend

#-------------------------------------------------------------------
# Object dependencies
#
-include Makefile.dependencies
