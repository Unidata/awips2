# $Id: Makefile,v 1.2 2003/02/27 22:07:51 steve Exp $
#
#	Makefile for "pqsurf"
#
include ../macros.make

INCLUDES = -I../config -I../misc -I../ulog -I../protocol -I../pq
TAG_SRCS	= \
	../misc/*.c ../misc/*.h \
	../ulog/*.c ../ulog/*.h \
	../protocol/*.c ../protocol/*.h \
	../pq/*.c ../pq/*.h
LDLIBS = -lm
PROG = pqsurf
C_PROGRAMS = $(PROG)
PROG_CSRCS = \
	dtime.c \
	pqsurf.c \
	surf_split.c \
	tokens.c \
	wmo_header.c \
	xbuf.c
PROG_OBJS =  $(PROG_CSRCS:.c=.o)
MANUALS	= $(PROG).1
LINKS = \
	dtime.c dtime.h \
	wmo_header.c wmo_header.h \
	tokens.c tokens.h \
	xbuf.c xbuf.h
PACKING_LIST = \
	depends \
	Makefile \
	pqsurf.c \
	pqsurf.1 \
	surf_split.c \
	surface.h
DISTCLEAN_GARBAGE	= $(LINKS) .linksmade

all : $(PROG)

install : installed_programs installed_manuals

$(PROG) : .linksmade $(PROG_OBJS) $(LIBRARY)
	$(CC) -o $@ $(CFLAGS) $(PROG_OBJS) $(LIBS)

.linksmade:
	@test -r "$@" \
		|| $(MAKE) $(MFLAGS) $(LINKS) \
			&& touch "$@"

dtime.c:
	ln -s ../pqing/$@ $@
dtime.h:
	ln -s ../pqing/$@ $@
wmo_header.c:
	ln -s ../pqing/$@ $@
wmo_header.h:
	ln -s ../pqing/$@ $@
tokens.c:
	ln -s ../pqing/$@ $@
tokens.h:
	ln -s ../pqing/$@ $@
xbuf.c:
	ln -s ../pqing/$@ $@
xbuf.h:
	ln -s ../pqing/$@ $@

include ../rules.make
include depends
