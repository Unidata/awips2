# This file contains general-purpose make(1) rules.

LIB_OBJS	= $(LIB_CSRCS:.c=.o)
LINT_OBJS	= $(LIB_CSRCS:.c=.ln)
LINTFLAGS	= -errchk=%all -errfmt=macro -errhdr -errtags -erroff=E_BAD_PTR_CAST_ALIGN,E_FUNC_VAR_UNUSED,E_CAST_UINT_TO_SIGNED_INT,E_CAST_INT_TO_SMALL_INT,E_PTRDIFF_OVERFLOW,E_SIGN_EXTENSION_PSBL,E_IDENTIFIER_REDEFINED,E_IDENTIFIER_REDECLARED,E_PARAM_REDECLARED
PROGRAMS	= $(C_PROGRAMS) $(SHELL_PROGRAMS)
CPPFLAGS	= $(CONFIGURE_CPPFLAGS) $(DEFINES) -I.. $(INCLUDES)
LIBS		= $(LIBRARY) $(LDLIBS) $(CONFIGURE_LIBS)
COMPILE.c	= $(CC) -c $(CFLAGS) $(CPPFLAGS)

.SUFFIXES: .o .i .ln .c

.c.i:
	$(CC) -E $(CFLAGS) $(CPPFLAGS) $< > $@
.c.o:
	$(COMPILE.c) $<

.c.ln:
	$(LINT) -c $(LINTFLAGS) $(CPPFLAGS) -UNDEBUG $<

install:	installed_headers \
		installed_programs \
		installed_etc_files \
		installed_manuals

install_setuids:	installed_setuid_programs

#lint:	lint_library linted_programs

lint_library:	llib-l$(LINT_LIBRARY).ln

clean:
	rm -f *.a *.o *.ln *.i $(C_PROGRAMS) $(GARBAGE)

distclean:
	rm -f *.a *.o *.ln *.i $(C_PROGRAMS) $(GARBAGE) *.log \
	    $(DISTCLEAN_GARBAGE)

linted_programs:
	@for program in $(C_PROGRAMS) ""; do \
	    test ! "$$program" || \
		$(MAKE) PROGRAM=$$program $$program.ln; \
	done;

archived_files:	$(LIB_OBJS)
	$(AR) $(ARFLAGS) $(LIBRARY) $(LIB_OBJS)
	$(RANLIB) $(LIBRARY)

programs:
	@for program in $(C_PROGRAMS) ""; do \
	    test ! "$$program" || \
		$(MAKE) $$program; \
	done;

deps:	FORCE
	$(CC) -xM1 $(CPPFLAGS) *.c >depends
	sort -u -o depends depends

# The following rule is intended to work even if the target file is a symbolic
# link.
tags:		FORCE
	ctags -f $@ *.c *.h \
	    $(TAG_SRCS) \
	    ../rpc/*.c ../rpc/*.h

$(INCDIR) \
$(BINDIR) \
$(ETCDIR) \
$(LIBDIR) \
$(MANDIR) \
$(MANDIR)/man1 \
$(MANDIR)/man3:
	mkdir $@

MANIFEST.echo:
	@$(MAKE) -s $(LOCAL_MACROS) ensure_manifest > /dev/null
	@echo $(PACKING_LIST) | fmt -1

llib-l$(LINT_LIBRARY).ln:	$(LINT_OBJS)
	$(LINT) -o $(LINT_LIBRARY) -x $(LINTFLAGS) $(CPPFLAGS) $(LINT_OBJS)

ensure_manifest:	$(PACKING_LIST)

installed_headers:
	@for header in $(HEADERS) ""; do \
	    test ! "$$header" || \
		$(MAKE) HEADER=$$header $(INCDIR)/$$header; \
	done

installed_programs:
	@for program in $(PROGRAMS) ""; do \
	    test ! "$$program" || \
		$(MAKE) PROGRAM=$$program $(BINDIR)/$$program; \
	done

installed_setuid_programs:
	@for program in $(SETUID_PROGRAMS) ""; do \
	    test ! "$$program" || \
		$(MAKE) SETUID_PROGRAM=$$program setuid_program; \
	done

setuid_program:
	$(MAKE) PROGRAM=$(SETUID_PROGRAM) \
	    $(BINDIR)/$(SETUID_PROGRAM)
	chown root $(BINDIR)/$(SETUID_PROGRAM) && \
	chmod 4755 $(BINDIR)/$(SETUID_PROGRAM)

installed_etc_files:
	@for file in $(ETC_FILES) ""; do \
	    test ! "$$file" || \
		$(MAKE) ETC_FILE=$$file $(ETCDIR)/$$file; \
	done;

installed_manuals:
	@for manual in $(MANUALS) ""; do \
	    if test "$$manual"; then \
		case "$$manual" in \
		    *.1) sub=man1;; \
		    *.3) sub=man3;; \
		esac; \
		$(MAKE) MANUAL=$$manual $(MANDIR)/$$sub/$$manual; \
	    else \
		true; \
	    fi; \
	done;

$(BINDIR)/$(PROGRAM):	 $(BINDIR) $(PROGRAM)
	-@cmp -s $(PROGRAM) $@ || \
	    ($(INSTALL) $(PROGRAM) $@ && chmod +x $@ && echo 'updated $@')

$(INCDIR)/$(HEADER):	$(INCDIR) $(HEADER)
	-@cmp -s $(HEADER) $@ || \
	    ($(INSTALL) $(HEADER) $@ && echo 'updated $@')

$(ETCDIR)/$(ETC_FILE):	$(ETCDIR) $(ETC_FILE)
	@if [ -f "$@" ]; then \
		echo '$@ already installed' ; \
	else \
		($(INSTALL) $(ETC_FILE) $@ && echo '$@ installed') ; \
	fi

$(MANDIR)/man1/$(MANUAL):       $(MANDIR) $(MANDIR)/man1 $(MANUAL) 
	-@cmp -s $(MANUAL) $@ || \
	    ($(INSTALL) $(MANUAL) $@ && echo 'updated $@')

$(MANDIR)/man3/$(MANUAL):       $(MANDIR) $(MANDIR)/man3 $(MANUAL) 
	-@cmp -s $(MANUAL) $@ || \
	    ($(INSTALL) $(MANUAL) $@ && echo 'updated $@')

FORCE:
