RPCGEN = rpcgen -b -C

all: ldm.h ldm_xdr.c

ldm.h: ldm.x
	-@rm -f $@
	$(RPCGEN) -h $? -o $@

ldm_xdr.c: ldm.x
	-@rm -f $@
	$(RPCGEN) -c $? -o $@

ldm_clnt.c: ldm.x
	-@rm -f $@
	$(RPCGEN) -l $? -o $@

ldm_5svc.c: ldm.x
	-@rm -f $@
	$(RPCGEN) -m $? -o $@
