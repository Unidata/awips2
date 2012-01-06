/* @(#)rpc.h	2.4 89/07/11 4.0 RPCSRC; from 1.9 88/02/08 SMI */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * rpc.h, Just includes the billions of rpc header files necessary to
 * do remote procedure calling.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */
#ifndef __RPC_HEADER__
#define __RPC_HEADER__

/*
 * "types.h" is included before <netinet/in.h> in order to obtain a
 * definition of the type "in_addr_t" before it is used under FreeBSD
 * 4.10-RELEASE-p2.
 */
#include "types.h"		/* some typedefs */
#include <netinet/in.h>

/* external data representation interfaces */
#include "xdr.h"		/* generic (de)serializer */

/* Client side only authentication */
#include "auth.h"		/* generic authenticator (client side) */

/* Client side (mostly) remote procedure call */
#include "clnt.h"		/* generic rpc stuff */

/* semi-private protocol headers */
#include "rpc_msg.h"	/* protocol for rpc messages */
#include "auth_unix.h"	/* protocol for unix style cred */
/*
 *  Uncomment-out the next line if you are building the rpc library with    
 *  DES Authentication (see the README file in the secure_rpc/ directory).
 */
/*#include "auth_des.h"	protocol for des style cred */

/* Server side only remote procedure callee */
#include "svc.h"		/* service manager and multiplexer */
#include "svc_auth.h"	/* service side authenticator */

/*
 * Portmapper
 */
#include "pmap_clnt.h"

#endif /* ndef __RPC_HEADER__ */
