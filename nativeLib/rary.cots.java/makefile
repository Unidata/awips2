
all: $(INSTALL_FROM)
	/bin/tar -xf jdk-7u55-linux-i586.tar.gz jdk1.7.0_55/jre/lib/i386/client/libjvm.so
	cp jdk1.7.0_55/jre/lib/i386/client/libjvm.so $(BUILD_DIR)/lib

clean:
	rm -rf jdk1.7.0_55
	rm -rf uninstall~
	rm -f $(BUILD_DIR)/lib/libjvm.so

%:
	$(MAKE) -f ../build.native/makefile.copy $@