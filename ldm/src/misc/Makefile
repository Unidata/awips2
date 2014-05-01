# $Id: Makefile,v 1.1.18.4 2005/06/24 21:05:18 steve Exp $
#
#	Development Makefile for ldm library misc routines
#
include ../macros.make

INCLUDES	= -I../config -I../ulog
LIBRARY		= ../libldm.a
LINT_LIBRARY	= misc
HEADERS		= \
	ldmalloc.h \
	alrm.h \
	error.h \
	fdnb.h \
	fsStats.h \
	inetutil.h \
	mkdirs_open.h \
	paths.h \
	pattern.h \
	RegularExpressions.h \
	rpcutil.h \
	setenv.h \
	statsMath.h
LIB_CSRCS	= \
	error.c \
	fdnb.c \
	fsStats.c \
	inetutil.c \
	mkdirs_open.c \
	pattern.c \
	RegularExpressions.c \
	rpcutil.c \
	setenv.c \
	statsMath.c \
	StringBuf.c
TAG_SRCS	= ../ulog/*.c ../ulog/*.h
PACKING_LIST = \
	depends \
	Makefile \
	ldmalloc.h \
	alrm.h \
	depends \
	error.c \
	error.h \
	fdnb.c \
	fdnb.h \
	fsStats.c \
	fsStats.h \
	inetutil.c \
	inetutil.h \
	mkdirs_open.c \
	mkdirs_open.h \
	paths.h \
	pattern.h \
	pattern.c \
	RegularExpressions.c \
	RegularExpressions.h \
	rpcutil.c \
	rpcutil.h \
	setenv.c \
	setenv.h \
	statsMath.c \
	statsMath.h \
	StringBuf.c \
	StringBuf.h

all:	archived_files

lint:	llib-l$(LINT_LIBRARY).ln

include ../rules.make
include depends
