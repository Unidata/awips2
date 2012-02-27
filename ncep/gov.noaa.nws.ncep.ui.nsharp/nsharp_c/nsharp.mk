$(BINDIR)/nsharp: $(OS_LIB)/ginitp_alt.o $(OS_LIB)/gendp_alt.o -lnsharp \
		  -lsnlist -lsnlib -lsflist -lsflib -lnxmlib -ldiaglib \
		  -lgemlib -lprmcnvlib -lgridlib -lgplt -lcgemlib \
		  -ldevice -lxwp -lxw -lps -lgn -lgemlib -lnetcdf \
		  -ltextlib -lxslt -lxml2 -liconv \
		  $(XLIBS) -lz -lm -lbz2
