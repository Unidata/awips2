#
# @(#)Makefile	2.1 88/07/29 4.0 RPCSRC
#
#
# Copyright (c) 1987 by Sun Microsystems, Inc.
#

CC		= c89
CPPFLAGS	= -DNDEBUG
CFLAGS		= -O -m32 -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64
DEPEND_FLAG	= -M
LINT		= lint
LINTLIB		= oncrpc
LINTFLAGS	= 
AR		= ar
ARFLAGS		= -cru
RANLIB		= ranlib
LIBRARY		= liboncrpc.a
prefix		= /usr/local/ldm/ldm-6.8.0
exec_prefix	= ${prefix}
LIBDIR		= ${exec_prefix}/lib

DESTDIR=

SRCS = auth_none.c auth_unix.c  authunix_prot.c bindresvport.c \
	clnt_generic.c clnt_perror.c clnt_raw.c clnt_simple.c clnt_tcp.c \
	clnt_udp.c rpc_dtablesize.c get_myaddress.c getrpcport.c \
	pmap_clnt.c pmap_getmaps.c pmap_getport.c pmap_prot.c \
	pmap_prot2.c pmap_rmt.c rpc_prot.c rpc_callmsg.c rpc_commondata.c \
	svc.c svc_auth.c svc_auth_unix.c svc_raw.c svc_run.c svc_simple.c \
	svc_tcp.c svc_udp.c xdr.c xdr_array.c xdr_float.c xdr_mem.c \
	xdr_rec.c xdr_reference.c xdr_stdio.c

OBJS = auth_none.o auth_unix.o  authunix_prot.o bindresvport.o \
	clnt_generic.o clnt_perror.o clnt_raw.o clnt_simple.o clnt_tcp.o \
	clnt_udp.o rpc_dtablesize.o get_myaddress.o getrpcport.o \
	pmap_clnt.o pmap_getmaps.o pmap_getport.o pmap_prot.o \
	pmap_prot2.o pmap_rmt.o rpc_prot.o rpc_callmsg.o rpc_commondata.o \
	svc.o svc_auth.o svc_auth_unix.o svc_raw.o svc_run.o svc_simple.o \
	svc_tcp.o svc_udp.o xdr.o xdr_array.o xdr_float.o xdr_mem.o \
	xdr_rec.o xdr_reference.o xdr_stdio.o

HDRS = auth.h auth_unix.h clnt.h pmap_clnt.h \
	pmap_prot.h pmap_rmt.h rpc.h rpc_msg.h svc.h svc_auth.h types.h xdr.h

all:	$(LIBRARY)

$(LIBRARY): $(OBJS) FORCE
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $@

FORCE:

install: $(LIBDIR)/$(LIBRARY)

$(LIBDIR)/$(LIBRARY):	$(LIBDIR) $(LIBRARY)
	cp $(LIBRARY) $(LIBDIR)

export:	
	@if test ! "$(TAG)"; then \
	    echo 2>&1 "Macro TAG not set"; \
	    status=2; \
	else \
	    if cvs -d $$CVSROOT export -d rpc -r $(TAG) rpc/rpc; then \
		status=$$?; \
	    else \
		status=1; \
	    fi; \
	fi; \
	exit $$status

tags: $(SRCS) $(HDRS)
	ctags -tw $(SRCS) $(HDRS)

ref: tags
	sed 's,	/.*,,' tags | \
	awk ' { printf("%-26s%-16s%s\n", $$1, $$2, $$3) }' > ref

.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<

.SUFFIXES:	.i .ln .c

lint: llib-lrpc.ln

llib-lrpc.ln: $(SRCS:.c=.ln)
	$(LINT) -ux -o$(LINTLIB) $(SRCS:.c=.ln)

.c.ln:
	$(LINT) -cuvx $(LINTFLAGS) $(CPPFLAGS) -Dnotdef $<

.c.i:
	$(CC) -E $(CPPFLAGS) $(CFLAGS) $< >$@

clean:
	rm -f *.o *.ln *.i $(LIBRARY)

distclean:
	rm -f *.o *.ln *.i $(LIBRARY) *.log config.status config.cache \
	    Makefile config.h

.SUFFIXES: .x .x~

.x.c:
	rpcgen -c $< | \
	sed 's/^#include \"$*\.h\"/#include <rpc\/$*\.h>/' > $@

.x.h:
	rpcgen -h $< > $@

depend: $(SRCS) $(HDRS)
	$(CC) $(CPPFLAGS) $(DEPEND_FLAG) $(SRCS) > makedep
	@echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	@echo '$$r makedep' >>eddep
	@echo 'w' >>eddep
	@cp Makefile.in Makefile.in.bak
	@ed - Makefile.in < eddep
	@rm eddep makedep Makefile.in.bak

depend.42BSD depend.42bsd:
	cp /dev/null x.c
	for i in $(SRCS) ; do \
              (/bin/grep '^#[         ]*include' x.c $$i | sed \
                      -e '/\.\.\/h/d' \
                      -e '/\.\.\/ufs/d' \
                      -e 's,<\(.*\)>,"/usr/include/\1",' \
                      -e 's/:[^"]*"\([^"]*\)".*/: \1/' \
	                  -e 's/\.c/\.o/' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp makerules makerules.bak
	ed - makerules < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> makerules
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> makerules
	echo '# see make depend above' >> makerules

# DO NOT DELETE THIS LINE
auth_none.o: auth_none.c
auth_none.o: config.h
auth_none.o: types.h
auth_none.o: xdr.h
auth_none.o: types.h
auth_none.o: auth.h
auth_none.o: xdr.h
auth_unix.o: auth_unix.c
auth_unix.o: config.h
auth_unix.o: types.h
auth_unix.o: xdr.h
auth_unix.o: types.h
auth_unix.o: auth.h
auth_unix.o: xdr.h
auth_unix.o: auth_unix.h
authunix_prot.o: authunix_prot.c
authunix_prot.o: config.h
authunix_prot.o: types.h
authunix_prot.o: xdr.h
authunix_prot.o: types.h
authunix_prot.o: auth.h
authunix_prot.o: xdr.h
authunix_prot.o: auth_unix.h
bindresvport.o: bindresvport.c
bindresvport.o: config.h
bindresvport.o: clnt.h
bindresvport.o: auth.h
bindresvport.o: xdr.h
bindresvport.o: types.h
clnt_generic.o: clnt_generic.c
clnt_generic.o: config.h
clnt_generic.o: rpc.h
clnt_generic.o: types.h
clnt_generic.o: xdr.h
clnt_generic.o: types.h
clnt_generic.o: auth.h
clnt_generic.o: xdr.h
clnt_generic.o: clnt.h
clnt_generic.o: auth.h
clnt_generic.o: rpc_msg.h
clnt_generic.o: auth.h
clnt_generic.o: clnt.h
clnt_generic.o: auth_unix.h
clnt_generic.o: svc.h
clnt_generic.o: types.h
clnt_generic.o: rpc_msg.h
clnt_generic.o: svc_auth.h
clnt_generic.o: pmap_clnt.h
clnt_generic.o: pmap_prot.h
clnt_generic.o: xdr.h
clnt_perror.o: clnt_perror.c
clnt_perror.o: config.h
clnt_perror.o: types.h
clnt_perror.o: auth.h
clnt_perror.o: xdr.h
clnt_perror.o: types.h
clnt_perror.o: clnt.h
clnt_perror.o: auth.h
clnt_raw.o: clnt_raw.c
clnt_raw.o: config.h
clnt_raw.o: rpc.h
clnt_raw.o: types.h
clnt_raw.o: xdr.h
clnt_raw.o: types.h
clnt_raw.o: auth.h
clnt_raw.o: xdr.h
clnt_raw.o: clnt.h
clnt_raw.o: auth.h
clnt_raw.o: rpc_msg.h
clnt_raw.o: auth.h
clnt_raw.o: clnt.h
clnt_raw.o: auth_unix.h
clnt_raw.o: svc.h
clnt_raw.o: types.h
clnt_raw.o: rpc_msg.h
clnt_raw.o: svc_auth.h
clnt_raw.o: pmap_clnt.h
clnt_raw.o: pmap_prot.h
clnt_raw.o: xdr.h
clnt_simple.o: clnt_simple.c
clnt_simple.o: config.h
clnt_simple.o: rpc.h
clnt_simple.o: types.h
clnt_simple.o: xdr.h
clnt_simple.o: types.h
clnt_simple.o: auth.h
clnt_simple.o: xdr.h
clnt_simple.o: clnt.h
clnt_simple.o: auth.h
clnt_simple.o: rpc_msg.h
clnt_simple.o: auth.h
clnt_simple.o: clnt.h
clnt_simple.o: auth_unix.h
clnt_simple.o: svc.h
clnt_simple.o: types.h
clnt_simple.o: rpc_msg.h
clnt_simple.o: svc_auth.h
clnt_simple.o: pmap_clnt.h
clnt_simple.o: pmap_prot.h
clnt_simple.o: xdr.h
clnt_tcp.o: clnt_tcp.c
clnt_tcp.o: config.h
clnt_tcp.o: rpc.h
clnt_tcp.o: types.h
clnt_tcp.o: xdr.h
clnt_tcp.o: types.h
clnt_tcp.o: auth.h
clnt_tcp.o: xdr.h
clnt_tcp.o: clnt.h
clnt_tcp.o: auth.h
clnt_tcp.o: rpc_msg.h
clnt_tcp.o: auth.h
clnt_tcp.o: clnt.h
clnt_tcp.o: auth_unix.h
clnt_tcp.o: svc.h
clnt_tcp.o: types.h
clnt_tcp.o: rpc_msg.h
clnt_tcp.o: svc_auth.h
clnt_tcp.o: pmap_clnt.h
clnt_tcp.o: pmap_prot.h
clnt_tcp.o: xdr.h
clnt_tcp.o: pmap_clnt.h
clnt_udp.o: clnt_udp.c
clnt_udp.o: config.h
clnt_udp.o: rpc.h
clnt_udp.o: types.h
clnt_udp.o: xdr.h
clnt_udp.o: types.h
clnt_udp.o: auth.h
clnt_udp.o: xdr.h
clnt_udp.o: clnt.h
clnt_udp.o: auth.h
clnt_udp.o: rpc_msg.h
clnt_udp.o: auth.h
clnt_udp.o: clnt.h
clnt_udp.o: auth_unix.h
clnt_udp.o: svc.h
clnt_udp.o: types.h
clnt_udp.o: rpc_msg.h
clnt_udp.o: svc_auth.h
clnt_udp.o: pmap_clnt.h
clnt_udp.o: pmap_prot.h
clnt_udp.o: xdr.h
clnt_udp.o: pmap_clnt.h
rpc_dtablesize.o: rpc_dtablesize.c
rpc_dtablesize.o: config.h
rpc_dtablesize.o: svc.h
rpc_dtablesize.o: types.h
rpc_dtablesize.o: rpc_msg.h
rpc_dtablesize.o: auth.h
rpc_dtablesize.o: xdr.h
rpc_dtablesize.o: types.h
rpc_dtablesize.o: clnt.h
rpc_dtablesize.o: auth.h
get_myaddress.o: get_myaddress.c
get_myaddress.o: config.h
get_myaddress.o: types.h
get_myaddress.o: clnt.h
get_myaddress.o: auth.h
get_myaddress.o: xdr.h
get_myaddress.o: types.h
get_myaddress.o: pmap_prot.h
get_myaddress.o: xdr.h
getrpcport.o: getrpcport.c
getrpcport.o: config.h
getrpcport.o: rpc.h
getrpcport.o: types.h
getrpcport.o: xdr.h
getrpcport.o: types.h
getrpcport.o: auth.h
getrpcport.o: xdr.h
getrpcport.o: clnt.h
getrpcport.o: auth.h
getrpcport.o: rpc_msg.h
getrpcport.o: auth.h
getrpcport.o: clnt.h
getrpcport.o: auth_unix.h
getrpcport.o: svc.h
getrpcport.o: types.h
getrpcport.o: rpc_msg.h
getrpcport.o: svc_auth.h
getrpcport.o: pmap_clnt.h
getrpcport.o: pmap_prot.h
getrpcport.o: xdr.h
pmap_clnt.o: pmap_clnt.c
pmap_clnt.o: config.h
pmap_clnt.o: rpc.h
pmap_clnt.o: types.h
pmap_clnt.o: xdr.h
pmap_clnt.o: types.h
pmap_clnt.o: auth.h
pmap_clnt.o: xdr.h
pmap_clnt.o: clnt.h
pmap_clnt.o: auth.h
pmap_clnt.o: rpc_msg.h
pmap_clnt.o: auth.h
pmap_clnt.o: clnt.h
pmap_clnt.o: auth_unix.h
pmap_clnt.o: svc.h
pmap_clnt.o: types.h
pmap_clnt.o: rpc_msg.h
pmap_clnt.o: svc_auth.h
pmap_clnt.o: pmap_clnt.h
pmap_clnt.o: pmap_prot.h
pmap_clnt.o: xdr.h
pmap_clnt.o: pmap_prot.h
pmap_clnt.o: pmap_clnt.h
pmap_getmaps.o: pmap_getmaps.c
pmap_getmaps.o: config.h
pmap_getmaps.o: rpc.h
pmap_getmaps.o: types.h
pmap_getmaps.o: xdr.h
pmap_getmaps.o: types.h
pmap_getmaps.o: auth.h
pmap_getmaps.o: xdr.h
pmap_getmaps.o: clnt.h
pmap_getmaps.o: auth.h
pmap_getmaps.o: rpc_msg.h
pmap_getmaps.o: auth.h
pmap_getmaps.o: clnt.h
pmap_getmaps.o: auth_unix.h
pmap_getmaps.o: svc.h
pmap_getmaps.o: types.h
pmap_getmaps.o: rpc_msg.h
pmap_getmaps.o: svc_auth.h
pmap_getmaps.o: pmap_clnt.h
pmap_getmaps.o: pmap_prot.h
pmap_getmaps.o: xdr.h
pmap_getmaps.o: pmap_prot.h
pmap_getmaps.o: pmap_clnt.h
pmap_getport.o: pmap_getport.c
pmap_getport.o: config.h
pmap_getport.o: rpc.h
pmap_getport.o: types.h
pmap_getport.o: xdr.h
pmap_getport.o: types.h
pmap_getport.o: auth.h
pmap_getport.o: xdr.h
pmap_getport.o: clnt.h
pmap_getport.o: auth.h
pmap_getport.o: rpc_msg.h
pmap_getport.o: auth.h
pmap_getport.o: clnt.h
pmap_getport.o: auth_unix.h
pmap_getport.o: svc.h
pmap_getport.o: types.h
pmap_getport.o: rpc_msg.h
pmap_getport.o: svc_auth.h
pmap_getport.o: pmap_clnt.h
pmap_getport.o: pmap_prot.h
pmap_getport.o: xdr.h
pmap_getport.o: pmap_prot.h
pmap_getport.o: pmap_clnt.h
pmap_prot.o: pmap_prot.c
pmap_prot.o: config.h
pmap_prot.o: types.h
pmap_prot.o: xdr.h
pmap_prot.o: types.h
pmap_prot.o: pmap_prot.h
pmap_prot.o: xdr.h
pmap_prot2.o: pmap_prot2.c
pmap_prot2.o: config.h
pmap_prot2.o: types.h
pmap_prot2.o: xdr.h
pmap_prot2.o: types.h
pmap_prot2.o: pmap_prot.h
pmap_prot2.o: xdr.h
pmap_rmt.o: pmap_rmt.c
pmap_rmt.o: config.h
pmap_rmt.o: rpc.h
pmap_rmt.o: types.h
pmap_rmt.o: xdr.h
pmap_rmt.o: types.h
pmap_rmt.o: auth.h
pmap_rmt.o: xdr.h
pmap_rmt.o: clnt.h
pmap_rmt.o: auth.h
pmap_rmt.o: rpc_msg.h
pmap_rmt.o: auth.h
pmap_rmt.o: clnt.h
pmap_rmt.o: auth_unix.h
pmap_rmt.o: svc.h
pmap_rmt.o: types.h
pmap_rmt.o: rpc_msg.h
pmap_rmt.o: svc_auth.h
pmap_rmt.o: pmap_clnt.h
pmap_rmt.o: pmap_prot.h
pmap_rmt.o: xdr.h
pmap_rmt.o: pmap_prot.h
pmap_rmt.o: pmap_clnt.h
pmap_rmt.o: pmap_rmt.h
rpc_prot.o: rpc_prot.c
rpc_prot.o: config.h
rpc_prot.o: rpc.h
rpc_prot.o: types.h
rpc_prot.o: xdr.h
rpc_prot.o: types.h
rpc_prot.o: auth.h
rpc_prot.o: xdr.h
rpc_prot.o: clnt.h
rpc_prot.o: auth.h
rpc_prot.o: rpc_msg.h
rpc_prot.o: auth.h
rpc_prot.o: clnt.h
rpc_prot.o: auth_unix.h
rpc_prot.o: svc.h
rpc_prot.o: types.h
rpc_prot.o: rpc_msg.h
rpc_prot.o: svc_auth.h
rpc_prot.o: pmap_clnt.h
rpc_prot.o: pmap_prot.h
rpc_prot.o: xdr.h
rpc_callmsg.o: rpc_callmsg.c
rpc_callmsg.o: config.h
rpc_callmsg.o: rpc.h
rpc_callmsg.o: types.h
rpc_callmsg.o: xdr.h
rpc_callmsg.o: types.h
rpc_callmsg.o: auth.h
rpc_callmsg.o: xdr.h
rpc_callmsg.o: clnt.h
rpc_callmsg.o: auth.h
rpc_callmsg.o: rpc_msg.h
rpc_callmsg.o: auth.h
rpc_callmsg.o: clnt.h
rpc_callmsg.o: auth_unix.h
rpc_callmsg.o: svc.h
rpc_callmsg.o: types.h
rpc_callmsg.o: rpc_msg.h
rpc_callmsg.o: svc_auth.h
rpc_callmsg.o: pmap_clnt.h
rpc_callmsg.o: pmap_prot.h
rpc_callmsg.o: xdr.h
rpc_commondata.o: rpc_commondata.c
rpc_commondata.o: config.h
rpc_commondata.o: rpc.h
rpc_commondata.o: types.h
rpc_commondata.o: xdr.h
rpc_commondata.o: types.h
rpc_commondata.o: auth.h
rpc_commondata.o: xdr.h
rpc_commondata.o: clnt.h
rpc_commondata.o: auth.h
rpc_commondata.o: rpc_msg.h
rpc_commondata.o: auth.h
rpc_commondata.o: clnt.h
rpc_commondata.o: auth_unix.h
rpc_commondata.o: svc.h
rpc_commondata.o: types.h
rpc_commondata.o: rpc_msg.h
rpc_commondata.o: svc_auth.h
rpc_commondata.o: pmap_clnt.h
rpc_commondata.o: pmap_prot.h
rpc_commondata.o: xdr.h
svc.o: svc.c
svc.o: config.h
svc.o: rpc.h
svc.o: types.h
svc.o: xdr.h
svc.o: types.h
svc.o: auth.h
svc.o: xdr.h
svc.o: clnt.h
svc.o: auth.h
svc.o: rpc_msg.h
svc.o: auth.h
svc.o: clnt.h
svc.o: auth_unix.h
svc.o: svc.h
svc.o: types.h
svc.o: rpc_msg.h
svc.o: svc_auth.h
svc.o: pmap_clnt.h
svc.o: pmap_prot.h
svc.o: xdr.h
svc.o: pmap_clnt.h
svc_auth.o: svc_auth.c
svc_auth.o: config.h
svc_auth.o: rpc.h
svc_auth.o: types.h
svc_auth.o: xdr.h
svc_auth.o: types.h
svc_auth.o: auth.h
svc_auth.o: xdr.h
svc_auth.o: clnt.h
svc_auth.o: auth.h
svc_auth.o: rpc_msg.h
svc_auth.o: auth.h
svc_auth.o: clnt.h
svc_auth.o: auth_unix.h
svc_auth.o: svc.h
svc_auth.o: types.h
svc_auth.o: rpc_msg.h
svc_auth.o: svc_auth.h
svc_auth.o: pmap_clnt.h
svc_auth.o: pmap_prot.h
svc_auth.o: xdr.h
svc_auth_unix.o: svc_auth_unix.c
svc_auth_unix.o: config.h
svc_auth_unix.o: rpc.h
svc_auth_unix.o: types.h
svc_auth_unix.o: xdr.h
svc_auth_unix.o: types.h
svc_auth_unix.o: auth.h
svc_auth_unix.o: xdr.h
svc_auth_unix.o: clnt.h
svc_auth_unix.o: auth.h
svc_auth_unix.o: rpc_msg.h
svc_auth_unix.o: auth.h
svc_auth_unix.o: clnt.h
svc_auth_unix.o: auth_unix.h
svc_auth_unix.o: svc.h
svc_auth_unix.o: types.h
svc_auth_unix.o: rpc_msg.h
svc_auth_unix.o: svc_auth.h
svc_auth_unix.o: pmap_clnt.h
svc_auth_unix.o: pmap_prot.h
svc_auth_unix.o: xdr.h
svc_raw.o: svc_raw.c
svc_raw.o: config.h
svc_raw.o: rpc.h
svc_raw.o: types.h
svc_raw.o: xdr.h
svc_raw.o: types.h
svc_raw.o: auth.h
svc_raw.o: xdr.h
svc_raw.o: clnt.h
svc_raw.o: auth.h
svc_raw.o: rpc_msg.h
svc_raw.o: auth.h
svc_raw.o: clnt.h
svc_raw.o: auth_unix.h
svc_raw.o: svc.h
svc_raw.o: types.h
svc_raw.o: rpc_msg.h
svc_raw.o: svc_auth.h
svc_raw.o: pmap_clnt.h
svc_raw.o: pmap_prot.h
svc_raw.o: xdr.h
svc_run.o: svc_run.c
svc_run.o: config.h
svc_run.o: rpc.h
svc_run.o: types.h
svc_run.o: xdr.h
svc_run.o: types.h
svc_run.o: auth.h
svc_run.o: xdr.h
svc_run.o: clnt.h
svc_run.o: auth.h
svc_run.o: rpc_msg.h
svc_run.o: auth.h
svc_run.o: clnt.h
svc_run.o: auth_unix.h
svc_run.o: svc.h
svc_run.o: types.h
svc_run.o: rpc_msg.h
svc_run.o: svc_auth.h
svc_run.o: pmap_clnt.h
svc_run.o: pmap_prot.h
svc_run.o: xdr.h
svc_simple.o: svc_simple.c
svc_simple.o: config.h
svc_simple.o: rpc.h
svc_simple.o: types.h
svc_simple.o: xdr.h
svc_simple.o: types.h
svc_simple.o: auth.h
svc_simple.o: xdr.h
svc_simple.o: clnt.h
svc_simple.o: auth.h
svc_simple.o: rpc_msg.h
svc_simple.o: auth.h
svc_simple.o: clnt.h
svc_simple.o: auth_unix.h
svc_simple.o: svc.h
svc_simple.o: types.h
svc_simple.o: rpc_msg.h
svc_simple.o: svc_auth.h
svc_simple.o: pmap_clnt.h
svc_simple.o: pmap_prot.h
svc_simple.o: xdr.h
svc_tcp.o: svc_tcp.c
svc_tcp.o: config.h
svc_tcp.o: rpc.h
svc_tcp.o: types.h
svc_tcp.o: xdr.h
svc_tcp.o: types.h
svc_tcp.o: auth.h
svc_tcp.o: xdr.h
svc_tcp.o: clnt.h
svc_tcp.o: auth.h
svc_tcp.o: rpc_msg.h
svc_tcp.o: auth.h
svc_tcp.o: clnt.h
svc_tcp.o: auth_unix.h
svc_tcp.o: svc.h
svc_tcp.o: types.h
svc_tcp.o: rpc_msg.h
svc_tcp.o: svc_auth.h
svc_tcp.o: pmap_clnt.h
svc_tcp.o: pmap_prot.h
svc_tcp.o: xdr.h
svc_udp.o: svc_udp.c
svc_udp.o: config.h
svc_udp.o: rpc.h
svc_udp.o: types.h
svc_udp.o: xdr.h
svc_udp.o: types.h
svc_udp.o: auth.h
svc_udp.o: xdr.h
svc_udp.o: clnt.h
svc_udp.o: auth.h
svc_udp.o: rpc_msg.h
svc_udp.o: auth.h
svc_udp.o: clnt.h
svc_udp.o: auth_unix.h
svc_udp.o: svc.h
svc_udp.o: types.h
svc_udp.o: rpc_msg.h
svc_udp.o: svc_auth.h
svc_udp.o: pmap_clnt.h
svc_udp.o: pmap_prot.h
svc_udp.o: xdr.h
xdr.o: xdr.c
xdr.o: config.h
xdr.o: types.h
xdr.o: xdr.h
xdr.o: types.h
xdr_array.o: xdr_array.c
xdr_array.o: config.h
xdr_array.o: types.h
xdr_array.o: xdr.h
xdr_array.o: types.h
xdr_float.o: xdr_float.c
xdr_float.o: config.h
xdr_float.o: types.h
xdr_float.o: xdr.h
xdr_float.o: types.h
xdr_mem.o: xdr_mem.c
xdr_mem.o: config.h
xdr_mem.o: types.h
xdr_mem.o: xdr.h
xdr_mem.o: types.h
xdr_rec.o: xdr_rec.c
xdr_rec.o: config.h
xdr_rec.o: types.h
xdr_rec.o: xdr.h
xdr_rec.o: types.h
xdr_reference.o: xdr_reference.c
xdr_reference.o: config.h
xdr_reference.o: types.h
xdr_reference.o: xdr.h
xdr_reference.o: types.h
xdr_stdio.o: xdr_stdio.c
xdr_stdio.o: config.h
xdr_stdio.o: types.h
xdr_stdio.o: xdr.h
xdr_stdio.o: types.h
