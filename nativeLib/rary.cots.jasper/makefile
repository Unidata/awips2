# Only build from command line, configure breaks when running from Eclipse
# Export CFLAGS=-m32 before building
configure_options =--enable-shared --disable-libjpeg --disable-static

-include ../build.native/makefile.cots

all: configure jasper copyJasper

clean: cleanJasper

configure:	
	cd ./jasper-1.900.1; \
	./configure $(configure_options)
	
jasper:
	cd ./jasper-1.900.1;	\
	make all
	
copyJasper:
	/bin/mkdir -p ../build.native/i386-pc-linux-gnu/lib
	/bin/cp -vP ./jasper-1.900.1/src/libjasper/.libs/libjasper.so.1.0.0 \
		./jasper-1.900.1/src/libjasper/.libs/libjasper.so.1 \
		./jasper-1.900.1/src/libjasper/.libs/libjasper.so \
		../build.native/i386-pc-linux-gnu/lib

cleanJasper:
	rm -f installed~
	make -C ./jasper-1.900.1 distclean
	/bin/rm -fv ../build.native/i386-pc-linux-gnu/lib/libjasper.so.1.0.0 \
		../build.native/i386-pc-linux-gnu/lib/libjasper.so.1 \
		../build.native/i386-pc-linux-gnu/lib/libjasper.so
