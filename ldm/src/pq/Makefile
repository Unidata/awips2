# $Id: Makefile,v 3.1 2003/02/26 23:43:48 steve Exp $
#
#	Development Makefile for pq
#
include ../macros.make

INCLUDES	= -I../config -I../misc -I../ulog -I../protocol
TAG_SRCS	= \
    ../misc/*.c ../misc/*.h \
    ../ulog/*.c ../ulog/*.h \
    ../protocol/*.c ../protocol/*.h
LDLIBS		= -lm
LIBRARY		= ../libldm.a
LINT_LIBRARY	= pq
HEADERS		= fbits.h lcm.h pq.h
MANUALS		= pq.3
LIB_CSRCS	= pq.c lcm.c
GARBAGE		= pqt

# inventory
PACKING_LIST = \
	depends \
	fbits.h \
	lcm.h \
	pq.h \
	$(LIB_CSRCS) \
	Makefile \
	pq.3

all:	archived_files

test:	pqt

include ../rules.make
include depends
