# This file contains make(1) macro definitions.

SHELL	= /bin/sh
INSTALL	= cp
YACC	= yacc
CC	= c89
RPCGEN	= rpcgen
AR	= ar
ARFLAGS	= -cru
RANLIB	= ranlib

CFLAGS	= -O -m32 -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64
CONFIGURE_CPPFLAGS	= -DNDEBUG
GDBMLIB	= 
CONFIGURE_LIBS	= 

LD_YACC	= 
LD_GEN	= 

PACKAGE	= ldm

prefix	= /usr/local/ldm/ldm-6.8.0
exec_prefix	= $(prefix)
ldmhome	= /usr/local/ldm
pq_dir	= pq

PROGRAM		= dummy_program
HEADER		= dummy_header
ETC_FILE	= dummy_etc_file
MANUAL		= dummy_manual

INCDIR	= $(prefix)/include
LIBDIR	= $(exec_prefix)/lib
BINDIR	= $(exec_prefix)/bin
ETCDIR	= $(ldmhome)/etc
MANDIR	= $(prefix)/man
# only used to update the release at the source site
FTPDIR	= /home/ftp/pub/ldm

# Manual pages:
WHATIS		= whatis
# The following macro should be empty on systems that don't
# allow users to create their own manual-page indexes.
MAKEWHATIS_CMD	= 

LIBRARY	= ../libldm.a
