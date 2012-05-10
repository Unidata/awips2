.PHONY: $(TARGETS) $(SUBDIRS)
#################################################################
#								#
#		PLEASE DO NOT MODIFY THIS FILE.			#
#								#
#			subdirs.mk				#
#								#
#	SUBPATH is $(CURDIR) unless it is defined in the	#
#		GNUmakefile.					#
#	TARGETS are (all check clean distclean link) unless	#
#		they are defined in the GNUmakefile.		#
#	SUBDIRS are all sub directories except 'test*' unless	#
#		they are defined in the GNUmakefile.		#
#								#
#################################################################

SUBPATH ?= $(CURDIR)
TARGETS ?= all check clean distclean link
SUBDIRS ?= $(filter-out test%, $(filter %/, $(shell ls -F $(SUBPATH))))

$(TARGETS):: $(SUBDIRS)

$(SUBDIRS)::
	@echo ""
	@echo "================================================================================"
	@echo ""
	@echo "		$(MAKECMDGOALS) -- $@ --"
	@echo ""
	@echo "================================================================================"
	@echo ""
	@$(MAKE) -C $(SUBPATH)/$@ $(MAKECMDGOALS)
	@sleep 1

#
#  End of subdirs.mk
#
