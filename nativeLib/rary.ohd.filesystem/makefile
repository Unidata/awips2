
PERMISSIONS_ROOT=awips
export PERMISSIONS_ROOT

_create_permissions_script:
	$(MAKE) -f ../build.native/makefile.perm $@

%:
	$(MAKE) -f ../build.native/makefile.perm $@
	$(MAKE) -f ../build.native/makefile.copy $@ 