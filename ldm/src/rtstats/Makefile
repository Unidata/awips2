# $Id: Makefile,v 1.3 2003/03/27 21:44:52 steve Exp $
#
#	Makefile for "rtstats"
#
include ../macros.make

INCLUDES = -I../config -I../misc -I../ulog -I../protocol -I../pq
LDLIBS = -lm
PROG = rtstats
C_PROGRAMS	= $(PROG)
PROG_CSRCS = \
	rtstats.c \
	binstats.c \
	filel.c \
	ldmsend.c
PROG_OBJS = $(PROG_CSRCS:.c=.o)
TAG_SRCS	= \
	../misc/*.c ../misc/*.h \
	../ulog/*.c ../ulog/*.h \
	../protocol/*.c ../protocol/*.h \
	../pq/*.c ../pq/*.h \
	/opt/rpcsrc_40/rpc/*.c /opt/rpcsrc_40/rpc/*.h
MANUALS	= $(PROG).1
PACKING_LIST = \
	depends \
	Makefile \
	rtstats.1 \
	rtstats.c \
	binstats.c \
	filel.c \
	ldmsend.c 

all : $(PROG)

install : installed_programs installed_manuals

$(PROG) : $(PROG_OBJS)  $(LIBRARY)
	$(CC) -o $@ $(CFLAGS) $(PROG_OBJS) $(LIBS)

include ../rules.make
include depends
