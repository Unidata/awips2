# $Id: Makefile,v 1.6.2.6.4.4.2.23 2008/04/11 14:54:50 steve Exp $
#
#	top level Makefile for ldm5
#
###

include macros.make

# Order is important here
LIBSUBDIRS = \
	config \
	ulog \
	misc \
	protocol \
	pq \
	fauxPq

PROGSUBDIRS = \
	pqcheck \
	pqcreate \
	pqinsert \
	pqcat \
	pqexpire \
	pqmon \
	pqutil \
	pqsend \
	pqing \
	pqact \
	pqsurf \
	send \
	rtstats \
	feedme \
	notifyme \
	regex \
	server \
	ldmping \
	scripts \
	scour

ALL_TARGETS = \
	config/all \
	ulog/all \
	misc/all \
	protocol/all \
	$(pq_dir)/all \
	pqcheck/all \
	pqcreate/all \
	pqinsert/all \
	pqcat/all \
	pqexpire/all \
	pqmon/all \
	pqutil/all \
	pqsend/all \
	pqing/all \
	pqact/all \
	pqsurf/all \
	send/all \
	rtstats/all \
	feedme/all \
	notifyme/all \
	server/all \
	ldmping/all \
	scripts/all \
	scour/all \
	regex/all

TEST_TARGETS = \
	pqing/test

LIBRARY	= libldm.a

LDM_VERSION = ldm_version

INSTALL_TARGETS = \
	config/install \
	ulog/install \
	misc/install \
	protocol/install \
	$(pq_dir)/install \
	pqcheck/install \
	pqcreate/install \
	pqinsert/install \
	pqcat/install \
	pqexpire/install \
	pqmon/install \
	pqutil/install \
	pqsend/install \
	pqing/install \
	pqact/install \
	pqsurf/install \
	send/install \
	rtstats/install \
	feedme/install \
	notifyme/install \
	server/install \
	ldmping/install \
	scripts/install \
	scour/install \
	regex/install

INSTALL_SETUIDS_TARGETS = \
	ulog/install_setuids \
	server/install_setuids

CLEAN_TARGETS = \
	config/clean \
	ulog/clean \
	misc/clean \
	protocol/clean \
	pq/clean \
	fauxPq/clean \
	pqcheck/clean \
	pqcreate/clean \
	pqinsert/clean \
	pqcat/clean \
	pqexpire/clean \
	pqmon/clean \
	pqutil/clean \
	pqsend/clean \
	pqing/clean \
	pqact/clean \
	pqsurf/clean \
	send/clean \
	rtstats/clean \
	feedme/clean \
	notifyme/clean \
	server/clean \
	ldmping/clean \
	scripts/clean \
	scour/clean \
	regex/clean \
	rpc/clean

DISTCLEAN_TARGETS = \
	config/distclean \
	ulog/distclean \
	misc/distclean \
	protocol/distclean \
	pq/distclean \
	fauxPq/distclean \
	pqcheck/distclean \
	pqcreate/distclean \
	pqinsert/distclean \
	pqcat/distclean \
	pqexpire/distclean \
	pqmon/distclean \
	pqutil/distclean \
	pqsend/distclean \
	pqing/distclean \
	pqact/distclean \
	pqsurf/distclean \
	send/distclean \
	rtstats/distclean \
	feedme/distclean \
	notifyme/distclean \
	server/distclean \
	ldmping/distclean \
	scripts/distclean \
	scour/distclean \
	regex/distclean \
	rpc/distclean

SUBDIRS = rpc config $(LIBSUBDIRS) $(PROGSUBDIRS)

TAG_SRCS	= \
	ulog/*.h ulog/*.c \
	protocol/*.c protocol/*.h \
	server/*.c server/*.h \
	misc/*.c misc/*.h \
	pq/*.c pq/*.h

# inventory
PACKING_LIST = \
	ANNOUNCEMENT \
	configure \
	COPYRIGHT \
	CHANGE_LOG \
	macros.make.in \
	Makefile \
	MANIFEST \
	README \
	RELEASE_NOTES \
	rules.make \
	simple_program.make \
	VERSION

# $(LIBRARY) must be made first because programs built
# in subdirectories link against it.
all:		$(LIBRARY) rpc/all $(ALL_TARGETS)

$(LIBRARY):	$(LDM_VERSION).o
	$(AR) $(ARFLAGS) $(LIBRARY) $(LDM_VERSION).o
	$(RANLIB) $(LIBRARY)

$(LDM_VERSION).o:	$(LDM_VERSION).c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(LDM_VERSION).c

test:		$(TEST_TARGETS)

install:	$(LIBDIR)/$(LIBRARY) $(INSTALL_TARGETS) whatis

whatis:		$(MANDIR)/$(WHATIS)

$(MANDIR)/$(WHATIS):	$(MANDIR)
	@if test "$(MAKEWHATIS_CMD)"; then \
	    touch $@; \
	    eval $(MAKEWHATIS_CMD) || \
		echo 1>&2 "Couldn't build manual-page database"; \
	fi

install_setuids:	$(INSTALL_SETUIDS_TARGETS)

clean:		$(CLEAN_TARGETS)
	rm -f *.a *.o *.ln *.i $(LIBRARY) $(LDM_VERSION).c

distclean:	$(DISTCLEAN_TARGETS)
	rm -f *.a *.o *.ln *.i $(LIBRARY) $(LDM_VERSION).c
	rm -f config.cache config.status *.log MANIFEST *.Z macros.make

$(LDM_VERSION).c: VERSION
	echo 'const char* ldm_version = "'`cat VERSION`'";' >$@

$(ALL_TARGETS) \
$(TEST_TARGETS) \
$(INSTALL_TARGETS) \
$(INSTALL_SETUIDS_TARGETS) \
$(CLEAN_TARGETS) \
$(DISTCLEAN_TARGETS):
	@subdir=`echo $@ | sed 's,/.*,,'`; \
	target=`echo $@ | sed 's,.*/,,'`; \
	$(MAKE) $(MFLAGS) SUBDIR=$$subdir TGET=$$target subdir_target

rpc/all:
	@echo ""
	@cd rpc && \
	    echo "Making \`all' in directory `pwd`" && \
	    echo "" && \
	    $(MAKE) all LIBRARY=../$(LIBRARY) || exit 1
	@echo ""
	@echo "Returning to directory `pwd`"
	@echo ""

subdir_target:
	@echo ""
	@cd $(SUBDIR) && \
	    echo "Making \`$(TGET)' in directory `pwd`" && \
	    echo "" && \
	    $(MAKE) $(TGET) || exit 1
	@echo ""
	@echo "Returning to directory `pwd`"
	@echo ""

$(LIBDIR)/$(LIBRARY) :	$(LIBDIR) FORCE
	-@cmp -s $(LIBRARY) $@ || ($(INSTALL) $(LIBRARY) $@ && \
	echo 'updated $@')

$(LIBDIR):
	mkdir $@

configure: configure.in aclocal.m4
	autoconf

config.status: configure
	-@if [ -f "$@" ]; then \
		$(SHELL) config.status --recheck ; \
	else \
		echo 1>&1 "$@ must be created by the configure script"; \
		exit 1; \
	fi

subdirs:
	@echo $(SUBDIRS)

# Ultimately depends on all the Makefiles, but...
MANIFEST: Makefile FORCE
	@echo 1>&2 Creating MANIFEST
	@$(MAKE) -s $(LOCAL_MACROS) TOP_MANIFEST.echo | sort -o $@

TOP_MANIFEST.echo:
	@echo $(PACKING_LIST) | fmt -1
	@if [ -n "$(SUBDIRS)" ]; then \
	    subdirs="$(SUBDIRS)"; \
	    for subdir in $$subdirs; do \
		(cd $$subdir && \
		echo 1>&2 Creating MANIFEST.echo in `pwd` && \
		$(MAKE) $(MFLAGS) $(SUBDIR_MACROS) MANIFEST.echo | \
		    sed "s|^|$$subdir/|") || exit 1; \
	    done; \
	else \
	    :; \
	fi

tag:
	@tag=ldm-`sed 's/\./_/g' VERSION`; \
	cvs tag -F $$tag . rpc

tar.gz:
	version=`cat VERSION`; \
	$(MAKE) $(MFLAGS) $(PACKAGE)-$$version.tar.gz VERSION=$$version

$(PACKAGE)-$(VERSION).tar.gz:	FORCE
	tag=ldm-`echo "$(VERSION)" | sed 's/\./_/g'`; \
	dir=ldm-$(VERSION); \
	srcdir=$$dir/src; \
	if cvs export -N -d $$dir -r $$tag src && \
		(cd $$srcdir && \
		$(MAKE) -f ../../rpc/Makefile $(MFLAGS) export TAG=$$tag); \
	then \
	    pax -x ustar -w $$srcdir | gzip -c >$@; \
	    status=$$?; \
	else \
	    status=1; \
	fi; \
	rm -rf $$dir; \
	exit $$status

$(PACKAGE)-$(VERSION).binary.tar.gz:	install
	id=$(PACKAGE)-$(VERSION).binary \
	&& rm -rf $$id \
	&& mkdir $$id \
	&& cd $$id \
	&& cp ../[ABD-Z]* . \
	&& cp ../COPYRIGHT . \
	&& mkdir etc \
	&& cd etc \
	&& cp ../../server/*.conf . \
	&& cp ../../pqact/*.conf . \
	&& cp ../../scour/*.conf . \
	&& cp ../../scripts/*.conf . \
	&& cd .. \
	&& mkdir bin \
	&& cd bin \
	&& cp ../../../bin/* . \
	&& rm -f ldmadmin ldmcheck ldmfail ldmprods netcheck syscheck mailpqstats \
	&& rm -f ddplus dds hds feedtest hrs ids pps afos \
	&& cp ../../scripts/*.in . \
	&& cd .. \
	&& ln -s ../../lib . \
	&& ln -s ../../include . \
	&& ln -s ../../man . \
	&& rm -rf Makefile* */Makefile* \
	&& cd .. \
	&& pax -x ustar -w $$id | gzip -c > $@ \
	&& rm -r $$id

binary.tar.gz:
	version=`cat VERSION`; \
	$(MAKE) $(MFLAGS) $(PACKAGE)-$$version.binary.tar.gz VERSION=$$version

$(FTPDIR)/$(PACKAGE)-$(VERSION).tar.gz:	$(PACKAGE)-$(VERSION).tar.gz
	scp $(PACKAGE)-$(VERSION).tar.gz ftp:$@ 
	ssh ftp chmod u+rw,g+rw,o=r $@

ftp:
	if cvs status . rpc 2>/dev/null | grep Status: \
		| grep -v Up-to-date >/dev/null; then \
	    echo 1>&2 'Package or rpc/ needs "cvs commit"'; \
	    exit 1; \
	else true; fi
	$(MAKE) tag
	version=`cat VERSION`; \
	$(MAKE) FTPDIR=$(FTPDIR) PACKAGE=$(PACKAGE) VERSION=$$version \
	    $(FTPDIR)/$(PACKAGE)-$$version.tar.gz; \
	test ! -h $(FTPDIR)/$(PACKAGE).tar.gz && exit 0; \
#	cd $(FTPDIR) || exit 1; \
#	rm $(PACKAGE).tar.gz || exit 1; \
#	ln -s $(PACKAGE)-$$version.tar.gz $(PACKAGE).tar.gz;

beta:
	if cvs status . rpc 2>/dev/null | grep Status: \
		| grep -v Up-to-date >/dev/null; then \
	    echo 1>&2 'Package or rpc/ needs "cvs commit"'; \
	    exit 1; \
	else true; fi
	$(MAKE) tag
	version=`cat VERSION`; \
	$(MAKE) FTPDIR=$(FTPDIR)/beta PACKAGE=$(PACKAGE) VERSION=$$version \
	    $(FTPDIR)/beta/$(PACKAGE)-$$version.tar.gz; \
	test ! -h $(FTPDIR)/beta/$(PACKAGE).tar.gz && exit 0; \

binftp:
	version=`cat VERSION`; \
	ftpbindir=/home/ftp/pub/binary/`system`; \
	$(MAKE) VERSION=$$version FTPBINDIR=$$ftpbindir \
	    $$ftpbindir/$(PACKAGE)-$$version.tar.gz
ftpbin:		binftp

$(FTPBINDIR)/$(PACKAGE)-$(VERSION).tar.gz:
	id=$(PACKAGE)-$(VERSION); \
	&& rm -f $$id \
	&& ln -s $(prefix) $$id \
	&& pax -x ustar -w $$id/bin $$id/etc $$id/include $$id/lib \
	   $$id/man | gzip -c > $@ \
	&& rm $$id
	chmod u+rw,g+rw,o=r $@
	test ! -h $(FTPBINDIR)/$(PACKAGE).tar.gz && exit 0; \
	    cd $(FTPBINDIR) || exit 1; \
	    rm $(PACKAGE).tar.gz || exit 1; \
	    ln -s $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE).tar.gz;

FORCE:

ldm_version.o:	ldm_version.c
