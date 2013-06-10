
all: $(INSTALL_FROM)
	# put the jvm library into the root /lib directory so its picked up without setting LD_PRELOAD_PATH.
	cp $(INSTALL_FROM)/jre/lib/i386/client/libjvm.so $(BUILD_DIR)/lib 
	$(MAKE) -f ../build.native/makefile.copy all

$(INSTALL_FROM):
	chmod +x *.bin
	find . -name "*.bin" -exec bash -c 'echo yes | ./{}' \;

clean:
	rm -rf jdk1.6.0_12
	rm -rf uninstall~
	rm -f $(BUILD_DIR)/lib/libjvm.so
	chmod -R +w $(INSTALL_TO)
	$(MAKE) -f ../build.native/makefile.copy clean

%:
	$(MAKE) -f ../build.native/makefile.copy $@