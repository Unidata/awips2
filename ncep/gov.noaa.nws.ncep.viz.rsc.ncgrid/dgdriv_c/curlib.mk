.PHONY: all
#################################################################
#								#
#		PLEASE DO NOT MODIFY THIS FILE.			#
#								#
#			curlib.mk				#
#								#
#################################################################
#
#  Define GEMDIR and INCDIR if not already defined.
#
GEMDIR ?= $(PREFIX)/../../gempak/include
INCDIR ?= $(PREFIX)/include

#
#  Add GEMDIR and INCDIR to CFLAGS and FFLAGS so
#	developer's include files are found before
#	the $GEMINC and $OS_INC include files.
#
CFLAGS := -I$(GEMDIR) -I$(INCDIR) $(CFLAGS)
FFLAGS := -I$(GEMDIR) -I$(INCDIR) $(FFLAGS)

#
#  If unique flags are needed add
#  them in a cflags.mk file.
#
-include $(SRCDIR)/cflags.mk

#
#  Create a list of all '.c' and '.f' files.
#
CSORCS := $(notdir $(wildcard $(SRCDIR)/*.c))
FSORCS := $(notdir $(wildcard $(SRCDIR)/*.f))

#
#  Convert list of sources to a list of objects.
#
OBJCTS := $(patsubst %.c,%.o, $(CSORCS)) \
	  $(patsubst %.f,%.o, $(FSORCS))

#
#  Eliminate 'main' and 'PROGRAM' files from
#	list of objects to create library.
#
LIBOBJ := $(filter-out test%, $(OBJCTS))
LIBOBJ := $(filter-out $(LOCDIR).o, $(LIBOBJ))

#
#  If source (object) file(s) exist continue processing.
#
ifeq "$(words $(LIBOBJ))" "0"

all:: ;

else

#
#  C compile rule to ensure all compilers work.
#
  %.o: %.c
	$(CC) $(CFLAGS) -c $<

#
#  FORTRAN compile rule to ensure MCHPRM.PRM exists.
#
  %.o: %.f
	@[[ -d $(OS_INC) ]] || mkdir -p $(OS_INC); \
	[[ -e $(OS_INC)/MCHPRM.PRM ]] || \
	ln -s $(GEMINC)/MCHPRM.$(OS) $(OS_INC)/MCHPRM.PRM
	$(FC) $(FFLAGS) -c $<

#
#  LIBNAM is the actual library name.
#  CURLIB is the actual library directory/name.
#
  LIBNAM := lib$(DIRNAM).a
  CURLIB := $(LIBDIR)/$(LIBNAM)

#
#  Process objects into library.
#
  all:: $(CURLIB)

  $(CURLIB): $(LIBOBJ)
  ifneq "$(LIBDIR)" "$(OS_LIB)"
	@if [ ! -e $(CURLIB) ] ; then \
	    if [ -e $(OS_LIB)/$(LIBNAM) ] ; then \
		 cp $(OS_LIB)/$(LIBNAM) $(LIBDIR) ; \
	    fi ; \
	fi
  endif
	$(AR) $(ARFLAGS) $@ $?

#
#  Do NOT remove target upon error.
#
.PRECIOUS: $(CURLIB)
endif

#
#  End of curlib.mk
#
