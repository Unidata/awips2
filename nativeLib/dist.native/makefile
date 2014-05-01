
makefile_dist=$(wildcard ../build.native/makefile.dist)

ifndef INSTALL_DIR
INSTALL_DIR = $(HOME)/awips
endif

ifneq ($(strip $(makefile_dist)),)
include $(makefile_dist)
endif

%:
	# nothing to do
