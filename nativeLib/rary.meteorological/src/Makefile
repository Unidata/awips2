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
BINS       =
SCRIPTS    =
TEST_BINS  =
CC	   = gcc -g -fPIC
FC	   = gfortran -fPIC -fno-underscoring


#-------------------------------------------------------------------
# Libraries
#

LIBS = $(LINUXLIB) $(WINDOWSLIB)
LINUXLIB = libmeteoLib.so
WINDOWSLIB = libmeteoLib.dll


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
              dist_filter.c \
              interp.c \
              temp_mixratio.c \
              calcWindChill.c \
              scaleless_analysis.c \
              adiabatic_te.c \
              calcHeatIndex.c \
              calckidx.c \
              thermoRtns.c \
              calctotidx.c \
              temp_of_te.c \
              heliComp.c \
              dcapeFunc.c \
	      sliceConvert.c \
              capeFunc.c
#-------------------------------------------------------------------
# Library C files whose objects go in an installed library
#
LIB_C_FILES =
LIB_F_FILES = \
              addaray.f  \
              addbycnst.f  \
              alt2press.f  \
              avwind.f \
              calccondpr.f  \
              calccondprdef.f  \
              calcdpd.f  \
              calcli.f  \
              calcpv.f  \
              calcrh.f  \
              calcrh2.f  \
              calctd.f  \
              calctd2.f  \
              calcthetae.f  \
              calcthetae2.f  \
              calctv.f  \
              calctv2.f  \
              calctw.f  \
              cclpar.f \
              constant.f  \
              comp_by.f  \
              crossvectors.f \
              ctop.f \
              cv_date2jul.f \
              cvgust.f \
              ddff.f \
              deftrk.f \
              density.f \
              derivative.f \
              derived_icing.f  \
              dgeocomps.f  \
              divaray.f \
              dotvectors.f \
              dzdlnp.f  \
              eqlev.f \
              eqp.f \
              esat.f  \
              exparay.f \
              fndiverg.f  \
              forecast.f \
              fortconbuf.f \
              frontogen.f  \
              frzlev.f \
              fsdiverg.f  \
              g2gkinematics.f  \
              gusts.f \
              hailsiz.f \
              helicity.f \
              hgt2pres.f \
              interp1.f \
              intpos.f \
              isenstable.f  \
              koffset.f  \
              lapserate.f  \
              lfcpar.f \
              lclpar.f \
              liftedp.f \
              lintrans.f  \
              matsln.f \
              maxmin.f \
              meanomega.f  \
              mixrat.f \
              mslp2thkns.f  \
              multaray.f \
              multbycnst.f  \
              mxtp.f \
              nadgdt.f \
              natlog.f \
              negarea.f \
              posarea.f \
              powercalc.f \
              press2alt.f  \
              presstable.f  \
              ptozsa.f \
              pvadv.f  \
              pvalue.f \
              pvpres.f  \
              qdiverg.f  \
              qvector.f  \
              radiation.f \
              radrtns.f \
              rang2d.f \
              replinrange.f \
              rhbar.f \
              rotvectors.f \
              setqsmooth.f  \
              slfront.f  \
              slqdiv.f  \
              slqvect.f  \
              smooth.f  \
              solax.f \
              spechum.f  \
              spechum2.f  \
              strmpak.f  \
	      subaray.f  \
              sunfuncs.f \
              sweat.f \
              sweatidx.f \
              tdofesat.f  \
              temp2theta.f  \
              theta2temp.f  \
              totals.f \
              tplcl.f \
              tsoar.f \
              tv2temp.f  \
              uvcomp.f \
              verpts.f \
              virttemp.f \
              virtualt.f \
              vvel.f \
              wbzero.f \
              windspeed.f \
              winddir.f \
              wndrho.f \
              ztopsa.f \
              BulkRichNo.f \
              thetawa.f \
              cgp.f \
              tpzlcl.f \
              pottemp.f \
              dmixr.f \
              pseudolift.f \
              vp.f \
	      IntrinsicFunctions.f
LIB_O_FILES = ${LIB_C_FILES:.C=.o} ${LIB_ANSI_C_FILES:.c=.o} \
              ${LIB_F_FILES:.f=.o}


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
	-@rm -f ${LINUXLIB}
	-@rm -f ${WINDOWSLIB}
	-@echo Creating Libraries : ${LINUXLIB} ${WINDOWSLIB}
	-@echo ${LINUXLIB}
	gcc -shared -lgfortran -lm -lstdc++ -o ${LINUXLIB} ${LIB_O_FILES}
	-@echo ${WINDOWSLIB}
	i386-mingw32-dlltool ${LIB_O_FILES} -e ${WINDOWSLIB}
	-@echo File Created : ${WINDOWSLIB}
	-@echo File Created : ${LINUXLIB}

#-------------------------------------------------------------------
# Executable dependencies
#
SYS_LIBS =

OBJECTS =


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
	${STD_CLEAN_ACTION}

depend:
	${STD_DEPEND_ACTION}

dependgcc: depend

#-------------------------------------------------------------------
# Object dependencies
#
-include Makefile.dependencies
