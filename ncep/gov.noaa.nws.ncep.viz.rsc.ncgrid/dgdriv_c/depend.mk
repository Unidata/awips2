#################################################################
#		PLEASE DO NOT MODIFY THIS FILE.			#
#								#
#			depend.mk				#
#								#
#  IMPORTANT:  In order to generate the	source file		#
#		dependencies, use 'make all'.			#
#								#
#		Any other target, including no target		#
#		(make ) will not create the dependencies.	#
#								#
#################################################################
#
#  C sources and target equals all
#
ifneq "$(CSORCS)" ""
  ifeq "$(MAKECMDGOALS)" "all"

-include $(CSORCS:.c=.u)

%.u: %.c
	@set -e; \
	if [ $(OS) = "AIX" ] ; \
	then \
	    $(CC) $(CFLAGS) -M -c $< ; \
	    mv $@ $@.$$$$; \
	elif [ $(CC) = "gcc" ] ; \
	then \
	    $(CC) $(CFLAGS) -M -c $< > $@.$$$$; \
	else \
	    touch $@u.$$$$ ; makedepend -- $(CFLAGS) -- -f$@u.$$$$ $< ; \
	    sed 's,\$(SRCDIR)/,,g' < $@u.$$$$ > $@.$$$$ ; \
	    $(RM) $@u.$$$$ $@u.$$$$.bak ; \
	fi ; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@ ; \
	$(RM) $@.$$$$

  endif
endif

#
#  FORTRAN sources and target equals all
#
ifneq "$(FSORCS)" ""
  ifeq "$(MAKECMDGOALS)" "all"

-include $(FSORCS:.f=.u)

depinc = -I $(GEMDIR) -I $(INCDIR) -I $(SRCDIR) -I $(GEMINC) -I $(OS_INC)
    ifeq "$(shell which perl)" "/usr/bin/perl"
COMMAND := sfmakedepend $(depinc)
    else
      ifeq "$(shell which perl)" "/usr/contrib/bin/perl"
COMMAND := hp_sfmakedepend $(depinc)
      else
       echo "ERROR:  Can NOT locate perl!!!"
      endif
    endif

%.u: %.f
	@set -e; $(RM) $@; \
	[[ -d $(OS_INC) ]] || mkdir -p $(OS_INC); \
	[[ -e $(OS_INC)/MCHPRM.PRM ]] || \
	ln -s $(GEMINC)/MCHPRM.$(OS) $(OS_INC)/MCHPRM.PRM ; \
	$(COMMAND) -f $@ $< ; mv $@ $@.$$$$ ; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	$(RM) $@.old $@.$$$$

  endif
endif

#
#  End of depend.mk
#
