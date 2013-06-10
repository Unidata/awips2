
%:
	find . -name "*.jar" -exec bash -c 'unzip -d $$(basename {} .jar) {} *.so' \;
	$(MAKE) -f ../build.native/makefile.copy $@
	
clean:
	rm -rf linux-i386