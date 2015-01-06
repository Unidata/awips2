
all: $(INSTALL_FROM)
	/bin/tar -xf jdk-7u65-linux-i586.tar.gz jdk1.7.0_65/jre/lib/i386/client/libjvm.so
	cp jdk1.7.0_65/jre/lib/i386/client/libjvm.so $(BUILD_DIR)/lib
	/bin/tar -xf jdk-7u65-linux-i586.tar.gz jdk1.7.0_65/include/*
	mkdir -p $(BUILD_DIR)/usr/java/include/linux
	cp jdk1.7.0_65/include/*.h $(BUILD_DIR)/usr/java/include
	cp jdk1.7.0_65/include/linux/*.h $(BUILD_DIR)/usr/java/include/linux

clean:
	rm -rf jdk1.7.0_65
	rm -rf uninstall~
	rm -f $(BUILD_DIR)/lib/libjvm.so
	rm -f $(BUILD_DIR)/usr/java/include/*.h
	rm -f $(BUILD_DIR)/usr/java/include/linux/*.h

%:
	$(MAKE) -f ../build.native/makefile.copy $@