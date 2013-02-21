.PHONY: $(OBJDIR)
#################################################################
#								#
#		PLEASE DO NOT MODIFY THIS FILE.			#
#								#
#			target.mk				#
#								#
#################################################################
#
#  `.SUFFIXES:' turns off all default relationships. 
#
.SUFFIXES:

MAKETARGET = $(MAKE) --no-print-directory -C $@ -f $(CURDIR)/GNUmakefile SRCDIR=$(CURDIR) $(MAKECMDGOALS)

#
#  Note:  The `+' prefix marks those command lines as "recursive" so that
#		they will be executed despite use of the `-t' flag.
#	  The `@' prefix suppresses the echoing of that line.
#
$(OBJDIR):
	+@[ -d $@ ] || mkdir -p $@
	+@$(MAKETARGET)

#
#  Empty target rule so that no attempt is made to 'make' GNUmakefile.
#
GNUmakefile: ;
%.mk :: ;
% :: $(OBJDIR) ;

#
#  End of target.mk
#
