.PHONY: link
#################################################################
#								#
#		PLEASE DO NOT MODIFY THIS FILE.			#
#								#
#			link.mk					#
#								#
#################################################################
#
#  Define BINDIR if not already defined.
#
BINDIR ?= $(PREFIX)/bin

#
#  MAIN is the 'main' or 'PROGRAM' file name for this directory.
#
MAIN := $(filter $(LOCDIR).%, $(CSORCS) $(FSORCS)) 
ifeq "$(words $(MAIN))" "0"
  MAIN := $(filter test%, $(CSORCS) $(FSORCS)) 
endif

#
#  If a 'main' or 'PROGRAM' file exists continue processing.
#
ifeq "$(words $(MAIN))" "0"

  link::
	@echo "No 'main' or 'PROGRAM' module found..."

else

#
#  Program names, PROGS, are determined from the '.mk' files.
#
  PROGS := $(notdir $(basename $(wildcard $(SRCDIR)/*.mk)))
  PROGS := $(filter-out cflags, $(PROGS))

#
#  Check for the existence of program makefiles (*.mk). 
#
  ifeq "$(words $(PROGS))" "0"

#
#  If no program makefiles (*.mk) exist
#  get out of Dodge.
#
    link::
	@echo "No '*.mk' files found..."

  else

#
#  If program makefiles (*.mk) exist link
#  the programs.
#
    VPATH += $(LIBDIR) $(OS_LIB)

#
#  Include the program specific library dependencies.
#  The include file MUST contain the line:
#       '$(BINDIR)/<program name>: -l....'
#
    -include $(addprefix $(SRCDIR)/, $(addsuffix .mk, $(PROGS)))

    link:: $(addprefix $(BINDIR)/, $(PROGS))

    $(addprefix $(BINDIR)/, $(PROGS)): $(addsuffix .o, $(basename $(MAIN)))

	@[[ -e $(BINDIR) ]] || mkdir -p $(BINDIR)
	$(FC) $(LDFLAGS) -o $@ $+

#
#  Force static files, '.a', to be loaded
#  before shared files, '.so'.
#
    .LIBPATTERNS = lib%.a lib%.so

#
#  library dependencies
#
    include $(GEMINC)/libdepends.mk

  endif
endif

#
#  End of link.mk
#
