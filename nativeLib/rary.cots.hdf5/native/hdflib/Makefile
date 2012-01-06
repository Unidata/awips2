
#/****************************************************************************
 #* NCSA HDF                                                                 *
 #* National Comptational Science Alliance                                   *
 #* University of Illinois at Urbana-Champaign                               *
 #* 605 E. Springfield, Champaign IL 61820                                   *
 #*                                                                          *
 #* For conditions of distribution and use, see the accompanying             *
 #* hdf-java/COPYING file.                                                   *
 #*                                                                          *
 #****************************************************************************/


CC = gcc -shared -DUNIX386
RM = /bin/rm

HDFINC = 
HDFLIB = 
JAVAINC1=/common/awips/jdk1.6.0_05/include
JAVAINC2=/common/awips/jdk1.6.0_05/include/linux

LIBDIR=../../lib/linux
ZLIB=
JPEGLIB=
SZLIB=

# Flags passed to the C compiler.  Leave these alone.

LD=gcc
LDOPT=-shared
LLEXT=a
SLEXT=so
JSLEXT=so

COPT=-shared -DUNIX386

CFLAGS = $(DEFS) -I. -I$(JAVAINC1) -I$(JAVAINC2) -I$(HDFINC)

# Be sure to add -lbsd if compiling in a SYSV environment
LIBS =  $(HDFLIB)/libmfhdf.$(LLEXT) $(HDFLIB)/libdf.$(LLEXT) $(SZLIB) $(ZLIB) $(JPEGLIB) -lm

# Macro def: object filename used
OBJECTS =  hdfstructsutil.o \
	hdfImp.o hdfvgroupImp.o hdfvdataImp.o hdfsdsImp.o hdfanImp.o \
	hdfdfanImp.o hdfdfsdImp.o hdfr24Imp.o hdfdfpalImp.o hdfr8Imp.o \
	hdfgrImp.o hdfheImp.o hdfdfuImp.o \
	hdfhxImp.o hdfvfImp.o hdfvhImp.o hdfvqImp.o hdfvsqImp.o \
	hdfexceptionImp.o \
	hdfdfcompImp.o \
	hdfnativeImp.o 

# make depends on the files specified in the objects macro defination
all:  libjhdf

hdflib: libjhdf

libjhdf: $(OBJECTS) 
	-mkdir -p $(LIBDIR);
	$(LD) $(LDOPT) -o $(LIBDIR)/libjhdf.$(JSLEXT) $(LDOPT2) $(OBJECTS) $(LIBS)

clean:
	$(RM) -f *.o so_locations $(LIBDIR)/libjhdf.$(JSLEXT)
