/* @(#)xdr_mem.c	2.1 88/07/29 4.0 RPCSRC */
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
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr_mem.c 1.19 87/08/11 Copyr 1984 Sun Micro";
#endif

/*
 * xdr_mem.h, XDR implementation using memory buffers.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * If you have some data to be interpreted as external data representation
 * or to be converted to external data representation in a memory buffer,
 * then this is the package for you.
 *
 */


#include "config.h"

#include <limits.h>
#include <arpa/inet.h>
#include <inttypes.h>
#include <stddef.h>
#include <strings.h>
#include <sys/types.h>
#include "types.h"
#include "xdr.h"

static bool_t	xdrmem_getlong(XDR *xdrs, uint32_t *lp);
static bool_t	xdrmem_putlong(XDR *xdrs, uint32_t *lp);
static bool_t	xdrmem_getbytes(XDR *xdrs, char* addr, unsigned len);
static bool_t	xdrmem_putbytes(XDR *xdrs, char* addr, unsigned len);
static unsigned	xdrmem_getpos(XDR *xdrs);
static bool_t	xdrmem_setpos(XDR *xdrs, unsigned pos);
static uint32_t*xdrmem_inline(XDR *xdrs, unsigned len);
static void	xdrmem_destroy(XDR *xdrs);

static struct	xdr_ops xdrmem_ops = {
	xdrmem_getlong,
	xdrmem_putlong,
	xdrmem_getbytes,
	xdrmem_putbytes,
	xdrmem_getpos,
	xdrmem_setpos,
	xdrmem_inline,
	xdrmem_destroy
};

/*
 * The procedure xdrmem_create initializes a stream descriptor for a
 * memory buffer.  
 */
void
xdrmem_create(
	register XDR *xdrs,
	char* addr,
	unsigned size,
	enum xdr_op op)
{
	xdrs->x_op = op;
	xdrs->x_ops = &xdrmem_ops;
	xdrs->x_private = xdrs->x_base = addr;
	xdrs->x_handy = size;
}

/*ARGSUSED*/
static void
xdrmem_destroy(
	XDR *xdrs)
{
}

static bool_t
xdrmem_getlong(
	register XDR *xdrs,
	uint32_t *lp)
{
	if (xdrs->x_handy < (unsigned)sizeof(uint32_t))
		return (FALSE);
	xdrs->x_handy -= (unsigned)sizeof(uint32_t);
	*lp = ntohl(*(uint32_t*)(xdrs->x_private));
	xdrs->x_private += sizeof(uint32_t);
	return (TRUE);
}

static bool_t
xdrmem_putlong(
	register XDR *xdrs,
	uint32_t *lp)
{
	if (xdrs->x_handy < (unsigned)sizeof(uint32_t))
		return (FALSE);
	xdrs->x_handy -= (unsigned)sizeof(uint32_t);
	*(uint32_t*)xdrs->x_private = htonl(*lp);
	xdrs->x_private += sizeof(uint32_t);
	return (TRUE);
}

static bool_t
xdrmem_getbytes(
	register XDR *xdrs,
	char* addr,
	register unsigned len)
{
	if (xdrs->x_handy < len)
		return (FALSE);
	xdrs->x_handy -= len;
	bcopy(xdrs->x_private, addr, len);
	xdrs->x_private += len;
	return (TRUE);
}

static bool_t
xdrmem_putbytes(
	register XDR *xdrs,
	char* addr,
	register unsigned len)
{
	if (xdrs->x_handy < len)
		return (FALSE);
	xdrs->x_handy -= len;
	bcopy(addr, xdrs->x_private, len);
	xdrs->x_private += len;
	return (TRUE);
}

static unsigned
xdrmem_getpos(
	register XDR *xdrs)
{
	return (unsigned)(xdrs->x_private - xdrs->x_base);
}

static bool_t
xdrmem_setpos(
	register XDR *xdrs,
	unsigned pos)
{
	register char* newaddr = xdrs->x_base + pos;
	register char* lastaddr = xdrs->x_private + xdrs->x_handy;
	ptrdiff_t	newhandy = lastaddr - newaddr;

	if (newhandy < 0 || newhandy > UINT_MAX)
		return (FALSE);
	xdrs->x_private = newaddr;
	xdrs->x_handy = (unsigned)newhandy;
	return (TRUE);
}

static uint32_t *
xdrmem_inline(
	register XDR *xdrs,
	unsigned len)
{
	uint32_t *buf = 0;

	if (xdrs->x_handy >= len) {
		xdrs->x_handy -= len;
		buf = (uint32_t *) xdrs->x_private;
		xdrs->x_private += len;
	}
	return (buf);
}
