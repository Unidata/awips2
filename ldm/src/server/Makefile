# $Id: Makefile,v 1.2.18.10 2007/02/16 20:07:14 steve Exp $
#
#	Makefile for ldm server, rpc.ldmd
#
include ../macros.make

INCLUDES = -I../config -I../misc -I../ulog -I../protocol -I../pq
TAG_SRCS	= \
	../misc/*.c ../misc/*.h \
	../ulog/*.c ../ulog/*.h \
	../protocol/*.c ../protocol/*.h \
	../pq/*.c ../pq/*.h
LDLIBS = $(LD_YACC) -lm
PROG = rpc.ldmd
C_PROGRAMS	= $(PROG)
SETUID_PROGRAMS	= $(PROG)
PROG_CSRCS = \
	abbr.c \
	acl.c \
	child_process_set.c \
	parser.c \
	down6.c \
	DownHelp.c \
	forn5_svc.c \
	ldm6_server.c \
	ldm6_svc.c \
	ldmd.c \
	priv.c \
	remote.c \
	requester6.c \
	svc5.c \
	up6.c \
	UpFilter.c \
	wordexp.c
PROG_OBJS = $(PROG_CSRCS:.c=.o)
CONFFILE = ldmd.conf
MANUALS	= ldmd.1
ETC_FILES	= $(CONFFILE)
GARBAGE	= ldm6_svc.c parser.c y.tab.c conf.tab.c 
PACKING_LIST = \
	Makefile \
	abbr.c \
	abbr.h \
	acl.c \
	acl.h \
	child_process_set.c \
	child_process_set.h \
	depends \
	down6.c \
	down6.h \
	DownHelp.c \
	DownHelp.h \
	forn5_svc.c \
	forn5_svc.h \
	ldmd.1 \
	ldmd.c \
	ldmd.conf \
	parser.y \
	peer_info.h \
	priv.c \
	priv.h \
	remote.c \
	requester6.c \
	requester6.h \
	scanner.l \
	scanner.c \
	svc5.c \
	ldm6_server.c \
	up6.c \
	up6.h \
	UpFilter.c \
	UpFilter.h \
	wordexp.c \
	wordexp.h

all: $(PROG)

test:

install:	installed_programs \
		installed_manuals \
		$(MANDIR)/man1/rpc.ldmd.1 \
		installed_etc_files

install_setuids:	installed_setuid_programs

$(MANDIR)/man1/rpc.ldmd.1 :
	cd $(MANDIR)/man1 ; rm -f rpc.ldmd.1 ; ln -s ldmd.1 rpc.ldmd.1

$(PROG) : $(PROG_OBJS) $(LIBRARY)
	$(CC) -o $@ $(CFLAGS) $(PROG_OBJS) $(LD_GEN) $(LIBS)

# Because both the client-side and server-side code that rpcgen(1) generates
# use identical function names like "hereis_6" and because the LDM links to
# both client-side and server-side functions, the server-side functions are
# renamed using a "_svc" suffix.
#
# Also, the target-file must be modified to be more portable.
#
ldm6_svc.c: ../protocol/ldm.x
	(echo '#include "ldmconfig.h"'; $(RPCGEN) -m ../protocol/ldm.x) | sed \
	    -e 's;<rpc/svc_soc.h>;<rpc/rpc.h>;' \
	    -e 's;feedme_6\([^A-Za-z_]\);feedme_6_svc\1;' \
	    -e 's;notifyme_6\([^A-Za-z_]\);notifyme_6_svc\1;' \
	    -e 's;is_alive_6\([^A-Za-z_]\);is_alive_6_svc\1;' \
	    -e 's;hiya_6\([^A-Za-z_]\);hiya_6_svc\1;' \
	    -e 's;hereis_6\([^A-Za-z_]\);hereis_6_svc\1;' \
	    -e 's;notification_6\([^A-Za-z_]\);notification_6_svc\1;' \
	    -e 's;comingsoon_6\([^A-Za-z_]\);comingsoon_6_svc\1;' \
	    -e 's;blkdata_6\([^A-Za-z_]\);blkdata_6_svc\1;' \
	    -e '/<stropts\.h>/d;' | \
	case `uname` in \
	    Darwin)	sed '/rpcsvcdirty/d';; \
	    *)		cat;; \
	esac >$@

# The following rule is for completeness only because the target-file must be
# manually modified to a great extent.
#
ldm6_server.c:	# ../protocol/ldm.x
	(echo '#include "ldmconfig.h"'; $(RPCGEN) -Ss ../protocol/ldm.x) > $@

#
# yacc/lex
#
parser.c: parser.y
	$(YACC) parser.y
	echo '#include "ldmconfig.h"' >parser.c
	cat y.tab.c >>parser.c || cat conf.tab.c >>parser.c

#
# NOTE: flex(1) is used instead of lex(1) in order to generate a scanner that
# can process "include" statements.
#
scanner.c: scanner.l
	flex scanner.l
	mv lex.yy.c scanner.c

parser.o:	parser.c scanner.c ./acl.h \
		../protocol/atofeedt.h ../protocol/ldm.h \
		../protocol/ldmprint.h ../protocol/timestamp.h ../ulog/ulog.h
	$(COMPILE.c) $(OUTPUT_OPTION) parser.c 

parser.ln:	parser.c scanner.c

lint:	$(PROG_CSRCS:.c=.ln)
	$(LINT) -x $(LINTFLAGS) $(CPPFLAGS) $(PROG_CSRCS:.c=.ln) \
	    -L../protocol -lprotocol -L../misc -lmisc

include ../rules.make
include depends
