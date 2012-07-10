
configure_options = --enable-shared --disable-libjpeg --disable-static

-include ../build.native/makefile.arch
all: jasper copyJasper

jasper:	
	cd ./jasper-1.900.1; \
	./configure $(configure_options); \
	make
	
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