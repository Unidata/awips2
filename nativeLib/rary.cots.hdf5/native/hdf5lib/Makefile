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

HDF5INC = /home/chammack/Desktop/hdf5-1.8.4-pre1/src/
HDF5LIB = /home/chammack/Desktop/hdf5-1.8.4-pre1/src/.libs/
JAVAINC1=/common/awips/jdk1.6.0_05/include
JAVAINC2=/common/awips/jdk1.6.0_05/include/linux
SZLIB=
ZLIB=-lz

LIBDIR=../../lib/linux

# Flags passed to the C compiler.  Leave these alone.

LD=gcc
LDOPT=-shared
LLEXT=a
SLEXT=so
JSLEXT=so

COPT=-shared -DUNIX386

CFLAGS = -DH5_USE_16_API -D_FILE_OFFSET_BITS=64 $(DEFS) -I. -I$(HDF5INC) -I$(JAVAINC1) -I$(JAVAINC2)

LIBS =  $(HDF5LIB)/libhdf5.$(LLEXT) $(ZLIB) $(SZLIB) -lm

# Macro def: object filename used
OBJECTS = lzf/lzf_filter.o lzf/lzf/lzf_c.o lzf/lzf/lzf_d.o  exceptionImp.o h5Constants.o h5Imp.o h5aImp.o h5dImp.o h5fImp.o h5gImp.o h5iImp.o h5pImp.o h5rImp.o h5sImp.o h5tImp.o nativeData.o h5util.o h5zImp.o

# make depends on the files specified in the objects macro defination
all:  libjhdf5

hdf5lib: libjhdf5

libjhdf5: $(OBJECTS)
	-mkdir -p $(LIBDIR);
	$(LD) $(LDOPT) -o $(LIBDIR)/libjhdf5.$(JSLEXT) $(LDOPT2) $(OBJECTS) $(LIBS)

clean:
	$(RM) -f *.o so_locations $(LIBDIR)/libjhdf5.$(JSLEXT)
