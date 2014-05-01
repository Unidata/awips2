# $Id: Makefile,v 1.2.2.1 2003/06/20 18:42:49 steve Exp $
#
#	Makefile for "pqing"
#
include ../macros.make

INCLUDES	= -I../config -I../misc -I../ulog -I../protocol -I../pq
TAG_SRCS	= \
	../misc/*.c ../misc/*.h \
	../ulog/*.c ../ulog/*.h \
	../protocol/*.c ../protocol/*.h \
	../pq/*.c ../pq/*.h
LDLIBS	= -lm
PROG	= pqing
C_PROGRAMS	= $(PROG)
PROG_CSRCS	= \
	afos_message.c \
	do.c \
	dtime.c \
	faa604_message.c \
	feed.c \
	file.c \
	fxbuf.c \
	pqing.c \
	rawfile.c \
	tokens.c \
	tty.c \
	wmo_header.c \
	wmo_message.c \
	wmo_start.c \
	xbuf.c
PROG_OBJS	= \
	afos_message.o \
	do_check.o \
	do_nocheck.o \
	dtime.o \
	faa604_message.o \
	feed.o \
	file.o \
	fxbuf.o \
	pqing.o \
	rawfile.o  \
	tokens.o \
	tty.o \
	wmo_header.o \
	wmo_message.o \
	wmo_start.o \
	xbuf.o
MANUALS	= pqing.1
PACKING_LIST	= \
	depends \
	Makefile \
	$(PROG_CSRCS) \
	afos_message.h \
	crc_tab.h \
	dtime.h \
	faa604_message.h \
	faa604_message.c \
	feed.h \
	pqing.1 \
	rawfile.h \
	tokens.h \
	wmo_header.h \
	wmo_message.h \
	wmo_start.h \
	xbuf.h \
	test.wmo \
	pqcat.out.expect

all : $(C_PROGRAMS)

install : installed_programs installed_manuals links

$(C_PROGRAMS) : $(PROG_OBJS) $(LIBRARY)
	$(CC) -o $@ $(CFLAGS) $(PROG_OBJS) $(LIBS)

links: $(BINDIR)/$(C_PROGRAMS)
	cd $(BINDIR) ; rm -f dds ; ln -s $(PROG) dds
	cd $(BINDIR) ; rm -f pps ; ln -s $(PROG) pps
	cd $(BINDIR) ; rm -f ids ; ln -s $(PROG) ids
	cd $(BINDIR) ; rm -f hds ; ln -s $(PROG) hds
	cd $(BINDIR) ; rm -f hrs ; ln -s $(PROG) hrs
	cd $(BINDIR) ; rm -f ddplus ; ln -s $(PROG) ddplus
	cd $(BINDIR) ; rm -f feedtest ; ln -s $(PROG) feedtest
	cd $(BINDIR) ; rm -f afos ; ln -s $(PROG) afos

do_check.o : do.c crc_tab.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -DSCAN_CHECK do.c && mv do.o $@

do_nocheck.o : do.c crc_tab.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -USCAN_CHECK do.c && mv do.o $@

include ../rules.make
include depends
