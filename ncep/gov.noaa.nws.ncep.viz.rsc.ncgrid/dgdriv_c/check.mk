.PHONY: check
#################################################################
#                                                               #
#               PLEASE DO NOT MODIFY THIS FILE.                 #
#                                                               #
#                       check.mk                                #
#                                                               #
#################################################################
#
#  check target:  display critical variables...
#
check::
	@echo ""
	@echo "PREFIX = $(PREFIX)"
	@echo "BINDIR = $(BINDIR)"
	@echo "LIBDIR = $(LIBDIR)"
	@echo "INCDIR = $(INCDIR)"
	@echo "GEMDIR = $(GEMDIR)"
	@echo ""
	@echo "CURLIB = $(CURLIB)"
	@echo "CURDIR = $(CURDIR)"
	@echo "SRCDIR = $(SRCDIR)"
	@echo "MAIN   = $(MAIN)"
	@echo "PROGS  = $(PROGS)"
	@echo ""
	@echo "OBJCTS = $(OBJCTS)"
	@echo ""
