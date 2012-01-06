# $Id: Makefile,v 1.1 2003/02/26 23:43:51 steve Exp $
#
#	Makefile for installing scour files for LDM
#
include ../macros.make

PROG = scour
SHELL_PROGRAMS	= $(PROG)
MANUALS	= $(PROG).1
ETC_FILES	= $(PROG).conf $(PROG).conf.dist
MANUALS	= $(PROG).1
GARBAGE	= $(PROG).conf.dist
DISTCLEAN_GARBAGE	= $(PROG)
PACKING_LIST = \
	Makefile \
	$(PROG).in \
	$(PROG).conf \
	$(PROG).1

all:	$(PROG).conf.dist

install:	installed_programs installed_etc_files installed_manuals

$(PROG).conf.dist:	$(PROG).conf
	cp $(PROG).conf $@

include ../rules.make
