# $Id: Makefile,v 1.1.18.9 2009/07/27 18:53:29 steve Exp $
#
#	Makefile for ldm scripts ldmadmin, netcheck, syscheck
#
include ../macros.make

PROGS = netcheck syscheck ldmcheck ldmadmin ldmfail scriptconfig plotMetrics
SHELL_PROGRAMS	= $(PROGS)
MANUALS	= ldmadmin.1 netcheck.1 syscheck.1 ldmfail.1
ETC_FILES	= netcheck.conf netcheck.conf.dist
GARBAGE	= netcheck.conf.dist
DISTCLEAN_GARBAGE	= ldmadmin ldmcheck ldmfail netcheck syscheck \
	writeConfiguration.pl
PACKING_LIST = \
	Makefile \
	ldmadmin.1 \
	writeConfiguration.pl.in \
	ldmadmin.in \
	ldmcheck.in \
	ldmfail.1 \
	ldmfail.in \
	netcheck.1 \
	netcheck.conf \
	netcheck.conf.dist \
	netcheck.in \
	plotMetrics \
	syscheck.1 \
	syscheck.in \
	scriptconfig

all:

install: installed_programs installed_etc_files $(ETCDIR)/ldmadmin-pl.conf \
	    installed_manuals

netcheck.conf.dist:	netcheck.conf
	cp netcheck.conf $@

$(ETCDIR)/ldmadmin-pl.conf:	FORCE
	@if [ -f "$@" ]; then \
	    if cp $@ $@.old; then \
		: true; \
	    else \
		echo 1>&2 "Couldn't copy old configuration-file"; \
		exit 1; \
	    fi; \
	    perl writeConfiguration.pl $@ >$@.new; \
	else \
	    perl writeConfiguration.pl >$@.new; \
	fi; \
	if test "$$?" -eq 0; then \
	    if mv -f $@.new $@; then \
		: true; \
	    else \
		echo 1>&2 "Couldn't rename new configuration-file"; \
		exit 1; \
	    fi; \
	else \
	    echo 1>&2 "Couldn't create new configuration-file"; \
	    exit 1; \
	fi

include ../rules.make
