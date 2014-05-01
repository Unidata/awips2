#!/usr/bin/perl

my($modify);

while( <STDIN> ) {

    if (/static[ 	]+struct[ 	]+timeval[ 	]+TIMEOUT/) {
	s/25/60/;  # NOTE: INT_MAX causes EINVAL in clnt_call()
	print $_;
	print "static struct timeval ZERO_TIMEOUT = { 0, 0 };";
	print "\n";
	next;
    }

    if (/notification_6/ || /hereis_6/ || /blkdata_6/) {
	$modify = 1;
    }

    if ($modify) {
	s/xdr_void/NULL/;

	s/TIMEOUT/ZERO_TIMEOUT/
    }

    print $_;

    if (/^}/) {
	$modify = 0;
    }
}

print "\n";
print "void*\n";
print "nullproc_6(void *argp, CLIENT *clnt)\n";
print "{\n";
print "        static char clnt_res;\n";
print "        if (clnt_call(clnt, NULLPROC,\n";
print "                (xdrproc_t) xdr_void, (caddr_t) argp,\n";
print "                (xdrproc_t) xdr_void, (caddr_t) &clnt_res,\n";
print "                TIMEOUT) != RPC_SUCCESS) {\n";
print "            return NULL;\n";
print "        }\n";
print "        return ((void *)&clnt_res);\n";
print "}\n";
print "\n";
print "\n";
print "enum clnt_stat clnt_stat(CLIENT *clnt)\n";
print "{\n";
print "    struct rpc_err rpcErr;\n";
print "\n";
print "    clnt_geterr(clnt, &rpcErr);\n";
print "\n";
print "    return rpcErr.re_status;\n";
print "}\n";
