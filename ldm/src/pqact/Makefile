# $Id: Makefile,v 1.1.16.1.2.3 2007/05/04 16:54:09 steve Exp $
#
#	Makefile for "pqact"
#
include ../macros.make

INCLUDES	= -I../config -I../misc -I../ulog -I../protocol -I../pq
TAG_SRCS	= \
	../misc/*.c ../misc/*.h \
	../ulog/*.c ../ulog/*.h \
	../protocol/*.c ../protocol/*.h \
	../pq/*.c ../pq/*.h \
	../gdbm/*.c ../gdbm/*.h
LDLIBS		= -lm $(GDBMLIB)
C_PROGRAMS	= pqact
LIBRARY		= ../libldm.a
LIB_CSRCS	= \
	action.c \
	filel.c \
	palt.c \
	pbuf.c \
	state.c
PROG_CSRCS	= $(LIB_CSRCS) pqact.c
PROG_OBJS	= $(PROG_CSRCS:.c=.o)
MANUALS		= pqact.1
ETC_FILES	= pqact.conf
PACKING_LIST = \
	depends \
	Makefile \
	$(PROG_CSRCS) \
	action.h \
	filel.h \
	palt.h \
	pbuf.h \
	state.h \
	pqact.1 \
	pqact.conf

all: pqact

pqact: $(PROG_OBJS) $(LIBRARY)
	$(CC) -o $@ $(CFLAGS) $(PROG_OBJS) $(LIBS)

include ../rules.make

lint:	$(PROG_CSRCS:.c=.ln)
	$(LINT) $(LINTFLAGS) $(CPPFLAGS) $(PROG_CSRCS:.c=.ln)

check:	
	$(CC) -o date_sub $(CFLAGS) $(CPPFLAGS) -UNDEBUG -DTEST_DATE_SUB \
	    palt.c $(LIBRARY)
	./date_sub
	rm date_sub

include depends
