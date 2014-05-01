# $Id: Makefile,v 6.2.18.3 2006/03/13 18:33:18 steve Exp $
#
#	Makefile for ulog package
#
include ../macros.make

INCLUDES	= -I../config
LIBRARY		= ../libldm.a
LINT_LIBRARY	= ulog
HEADERS		= ulog.h
MANUALS		= ulog.3
C_PROGRAMS	= hupsyslog
SHELL_PROGRAMS	= newlog
SETUID_PROGRAMS	= hupsyslog
LIB_CSRCS	= ulog.c log.c
GARBAGE		= test0 ulogger
PACKING_LIST	= \
	README \
	newlog \
	ulog.3 \
	Makefile \
	depends \
	log.c \
	log.h \
	ulog.c \
	ulog.h \
	hupsyslog.sh \
	hupsyslog.c \
	ulogger.c

all:	archived_files programs

hupsyslog:	hupsyslog.c
	$(CC) -o $@ $(CFLAGS) $(CPPFLAGS) hupsyslog.c $(LIBS)

ulogger:	ulogger.c
	$(CC) -o $@ $(CFLAGS) $(CPPFLAGS) ulogger.c $(LIBS)

test : test0
	./test0 -l -

test0 : ulog.c ulog.h ../config/ldmconfig.h
	$(CC) -o test0 $(CFLAGS) -DTEST0 ulog.c $(LDLIBS)

lint:   llib-l$(LINT_LIBRARY).ln

include ../rules.make
include depends
