/* $Id: paths.h,v 1.3.20.1 2007/02/26 17:28:07 steve Exp $ */
/*
 * Set up default paths for ldm system.
 * Can be overridden on the command line.
 */
#define DEFAULT_ETCDIR		LDMHOME "/etc"
#define DEFAULT_ACLPATHNAME	DEFAULT_ETCDIR "/ldmd.conf"
#define DEFAULT_CONFFILENAME	DEFAULT_ETCDIR "/pqact.conf"
#define DEFAULT_DATADIR		LDMHOME
#define DEFAULT_LOGDIR		LDMHOME "/logs"
#define DEFAULT_QUEUE		LDMHOME "/data/ldm.pq"
#define DEFAULT_SURF_OUTQUEUE	LDMHOME "/data/pqsurf.pq"
#define DEFAULT_SURF_CONFFILE	DEFAULT_ETCDIR "/pqsurf.conf"
#define DEFAULT_SURF_DATADIR	LDMHOME
