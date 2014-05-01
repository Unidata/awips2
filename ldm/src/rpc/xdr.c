/* @(#)xdr.c	2.1 88/07/29 4.0 RPCSRC */
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
static char sccsid[] = "@(#)xdr.c 1.35 87/08/12";
#endif

/*
 * xdr.c, Generic XDR routines implementation.
 *
 * Copyright (C) 1986, Sun Microsystems, Inc.
 *
 * These are the "generic" xdr routines used to serialize and de-serialize
 * most common data items.  See xdr.h for more info on the interface to
 * xdr.
 */

#include "config.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "xdr.h"

/*
 * constants specific to the xdr "protocol"
 */
#define XDR_FALSE	0
#define XDR_TRUE	1
#define LASTUNSIGNED	((unsigned) 0-1)

#ifndef INT32_MIN
#  define INT32_MIN	-2147483648
#endif
#ifndef INT32_MAX
#  define INT32_MAX	 2147483647
#endif
#ifndef UINT32_MAX
#  define UINT32_MAX	 4294967295
#endif

/*
 * for unit alignment
 */
static char xdr_zero[BYTES_PER_XDR_UNIT] = { 0, 0, 0, 0 };

/*
 * Free a data structure using XDR
 * Not a filter, but a convenient utility nonetheless
 */
void
xdr_free(
	xdrproc_t proc,
	char *objp)
{
	XDR x;
	
	x.x_op = XDR_FREE;
	(*proc)(&x, objp);
}

/*
 * XDR nothing
 */
/*ARGSUSED*/
bool_t
xdr_void(
	XDR *xdrs,
	char* addr)
{
	return (TRUE);
}

/*
 * XDR integers
 */
bool_t
xdr_int(
	XDR *xdrs,
	int *ip)
{
#ifdef lint
	(void) (xdr_short(xdrs, (short *)ip));
	return (xdr_long(xdrs, (long *)ip));
#elif SIZEOF_INT == SIZEOF_LONG
	return (xdr_long(xdrs, (long *)ip));
#elif SIZEOF_INT == SIZEOF_SHORT
	return (xdr_short(xdrs, (short *)ip));
#else
	if (xdrs->x_op == XDR_ENCODE) {
		long	v = *ip;

		return xdr_long(xdrs, &v);
	}

	if (xdrs->x_op == XDR_DECODE) {
		long	v;

		if (!xdr_long(xdrs, &v))
			return FALSE;

		*ip = (int)v;

		return TRUE;
	}

	if (xdrs->x_op == XDR_FREE)
		return (TRUE);

	return FALSE;
#endif
}

/*
 * XDR unsigned integers
 */
bool_t
xdr_u_int(
	XDR *xdrs,
	unsigned *up)
{
#ifdef lint
	(void) (xdr_short(xdrs, (short *)up));
	return (xdr_u_long(xdrs, (unsigned long *)up));
#elif SIZEOF_INT == SIZEOF_LONG
	return (xdr_u_long(xdrs, (unsigned long *)up));
#elif SIZEOF_INT == SIZEOF_SHORT
	return (xdr_u_short(xdrs, (unsigned short *)up));
#else
	if (xdrs->x_op == XDR_ENCODE) {
		unsigned long	v = *up;

		return xdr_u_long(xdrs, &v);
	}

	if (xdrs->x_op == XDR_DECODE) {
		unsigned long	v;

		if (!xdr_u_long(xdrs, &v))
			return FALSE;

		*up = (unsigned)v;

		return TRUE;
	}

	if (xdrs->x_op == XDR_FREE)
		return (TRUE);

	return FALSE;
#endif
}

/*
 * XDR long integers
 * same as xdr_u_long - open coded to save a proc call!
 */
bool_t
xdr_long(
	register XDR *xdrs,
	long *lp)
{
#if SIZEOF_LONG == SIZEOF_INT32
	if (xdrs->x_op == XDR_ENCODE)
		return (XDR_PUTLONG(xdrs, (uint32_t*)lp));

	if (xdrs->x_op == XDR_DECODE)
		return (XDR_GETLONG(xdrs, (uint32_t*)lp));

	if (xdrs->x_op == XDR_FREE)
		return (TRUE);
#else
	if (xdrs->x_op == XDR_ENCODE) {
		if (*lp < INT32_MIN || *lp > INT32_MAX)
			return FALSE;

		{
			int32_t	v = (int32_t)*lp;

			return	XDR_PUTLONG(xdrs, (uint32_t*)&v);
		}
	}

	if (xdrs->x_op == XDR_DECODE) {
		int32_t	v;

		if (!XDR_GETLONG(xdrs, (uint32_t*)&v))
			return FALSE;

		*lp = v;

		return TRUE;
	}

	if (xdrs->x_op == XDR_FREE)
		return (TRUE);
#endif
	return (FALSE);
}

/*
 * XDR unsigned long integers
 * same as xdr_long - open coded to save a proc call!
 */
bool_t
xdr_u_long(
	register XDR *xdrs,
	unsigned long *ulp)
{
#if SIZEOF_LONG == SIZEOF_INT32
	if (xdrs->x_op == XDR_DECODE)
		return (XDR_GETLONG(xdrs, (uint32_t *)ulp));
	if (xdrs->x_op == XDR_ENCODE)
		return (XDR_PUTLONG(xdrs, (uint32_t *)ulp));
	if (xdrs->x_op == XDR_FREE)
		return (TRUE);

#else
	if (xdrs->x_op == XDR_ENCODE) {
		if (*ulp > UINT32_MAX)
			return FALSE;

		{
			uint32_t	v = (uint32_t)*ulp;

			return	XDR_PUTLONG(xdrs, &v);
		}
	}

	if (xdrs->x_op == XDR_DECODE) {
		uint32_t	v;

		if (!XDR_GETLONG(xdrs, &v))
			return FALSE;

		*ulp = v;

		return TRUE;
	}

	if (xdrs->x_op == XDR_FREE)
		return (TRUE);
#endif
	return (FALSE);
}

/*
 * XDR short integers
 */
bool_t
xdr_short(
	register XDR *xdrs,
	short *sp)
{
	int32_t l;

	switch (xdrs->x_op) {

	case XDR_ENCODE:
		l = *sp;
		return (XDR_PUTLONG(xdrs, (uint32_t*)&l));

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, (uint32_t*)&l)) {
			return (FALSE);
		}
		*sp = (short) l;
		return (TRUE);

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}

/*
 * XDR unsigned short integers
 */
bool_t
xdr_u_short(
	register XDR *xdrs,
	unsigned short *usp)
{
	uint32_t l;

	switch (xdrs->x_op) {

	case XDR_ENCODE:
		l = *usp;
		return (XDR_PUTLONG(xdrs, &l));

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, &l)) {
			return (FALSE);
		}
		*usp = (unsigned short) l;
		return (TRUE);

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}


/*
 * XDR a char
 */
bool_t
xdr_char(
	XDR *xdrs,
	char *cp)
{
	int i;

	i = (*cp);
	if (!xdr_int(xdrs, &i)) {
		return (FALSE);
	}
	*cp = (char)i;
	return (TRUE);
}

/*
 * XDR an unsigned char
 */
bool_t
xdr_u_char(
	XDR *xdrs,
	unsigned char *cp)
{
	unsigned u;

	u = (*cp);
	if (!xdr_u_int(xdrs, &u)) {
		return (FALSE);
	}
	*cp = (unsigned char)u;
	return (TRUE);
}

/*
 * XDR booleans
 */
bool_t
xdr_bool(
	register XDR *xdrs,
	bool_t *bp)
{
	int32_t lb;

	switch (xdrs->x_op) {

	case XDR_ENCODE:
		lb = *bp ? XDR_TRUE : XDR_FALSE;
		return (XDR_PUTLONG(xdrs, (uint32_t*)&lb));

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, (uint32_t*)&lb)) {
			return (FALSE);
		}
		*bp = (lb == XDR_FALSE) ? FALSE : TRUE;
		return (TRUE);

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}

/*
 * XDR enumerations
 */
bool_t
xdr_enum(
	XDR *xdrs,
	enum_t *ep)
{
	/*
	 * enums are treated as ints
	 */
#ifdef lint
	(void) (xdr_short(xdrs, (short *)ep));
	return (xdr_long(xdrs, (long *)ep));
#elif SIZEOF_ENUM == SIZEOF_LONG
	return (xdr_long(xdrs, (long *)ep));
#elif SIZEOF_ENUM == SIZEOF_SHORT
	return (xdr_short(xdrs, (short *)ep));
#elif SIZEOF_ENUM == SIZEOF_INT
	return (xdr_int(xdrs, (int *)ep));
#else
#    error An "enum" is not the same size as an integral type.  xdr_enum() cannot be compiled.
#endif
}

/*
 * XDR opaque data
 * Allows the specification of a fixed size sequence of opaque bytes.
 * cp points to the opaque object and cnt gives the byte length.
 */
bool_t
xdr_opaque(
	register XDR *xdrs,
	char* cp,
	register unsigned cnt)
{
	register unsigned	rndup;
	static int	 crud[BYTES_PER_XDR_UNIT];

	/*
	 * if no data we are done
	 */
	if (cnt == 0)
		return (TRUE);

	/*
	 * round byte count to full xdr units
	 */
	rndup = cnt % BYTES_PER_XDR_UNIT;
	if (rndup != 0)
		rndup = BYTES_PER_XDR_UNIT - rndup;

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETBYTES(xdrs, cp, cnt)) {
			return (FALSE);
		}
		if (rndup == 0)
			return (TRUE);
		return (XDR_GETBYTES(xdrs, (char*)crud, rndup));
	}

	if (xdrs->x_op == XDR_ENCODE) {
		if (!XDR_PUTBYTES(xdrs, cp, cnt)) {
			return (FALSE);
		}
		if (rndup == 0)
			return (TRUE);
		return (XDR_PUTBYTES(xdrs, xdr_zero, rndup));
	}

	if (xdrs->x_op == XDR_FREE) {
		return (TRUE);
	}

	return (FALSE);
}

/*
 * XDR counted bytes
 * *cpp is a pointer to the bytes, *sizep is the count.
 * If decoding and *cpp is NULL, then *sizep bytes are allocated
 */
bool_t
xdr_bytes(
	register XDR *xdrs,
	char **cpp,
	register unsigned *sizep,
	unsigned maxsize)
{
	register char *sp = *cpp;  /* sp is the actual string pointer */
	register unsigned nodesize;

	/*
	 * first deal with the length since xdr bytes are counted
	 */
	if (! xdr_u_int(xdrs, sizep)) {
		return (FALSE);
	}
	nodesize = *sizep;
	if ((nodesize > maxsize) && (xdrs->x_op != XDR_FREE)) {
		return (FALSE);
	}

	/*
	 * now deal with the actual bytes
	 */
	switch (xdrs->x_op) {

	case XDR_DECODE:
		if (nodesize == 0) {
			return (TRUE);
		}
		if (sp == NULL) {
			*cpp = sp = (char *)mem_alloc(nodesize);
		}
		if (sp == NULL) {
			(void) fprintf(stderr, "xdr_bytes: out of memory\n");
			return (FALSE);
		}
		/* fall into ... */

	/*FALLTHROUGH*/
	case XDR_ENCODE:
		return (xdr_opaque(xdrs, sp, nodesize));

	case XDR_FREE:
		if (sp != NULL) {
			mem_free(sp, nodesize);
			*cpp = NULL;
		}
		return (TRUE);
	}
	return (FALSE);
}

/*
 * Implemented here due to commonality of the object.
 */
bool_t
xdr_netobj(
	XDR *xdrs,
	struct netobj *np)
{
	return (xdr_bytes(xdrs, &np->n_bytes, &np->n_len, MAX_NETOBJ_SZ));
}

/*
 * XDR a descriminated union
 * Support routine for discriminated unions.
 * You create an array of xdrdiscrim structures, terminated with
 * an entry with a null procedure pointer.  The routine gets
 * the discriminant value and then searches the array of xdrdiscrims
 * looking for that value.  It calls the procedure given in the xdrdiscrim
 * to handle the discriminant.  If there is no specific routine a default
 * routine may be called.
 * If there is no specific or default routine an error is returned.
 */
bool_t
xdr_union(
	register XDR *xdrs,
	enum_t *dscmp,		/* enum to decide which arm to work on */
	char *unp,		/* the union itself */
	struct xdr_discrim *choices,	/* [value, xdr proc] for each arm */
	xdrproc_t dfault)	/* default xdr routine */
{
	register enum_t dscm;

	/*
	 * we deal with the discriminator;  it's an enum
	 */
	if (! xdr_enum(xdrs, dscmp)) {
		return (FALSE);
	}
	dscm = *dscmp;

	/*
	 * search choices for a value that matches the discriminator.
	 * if we find one, execute the xdr routine for that value.
	 */
	for (; choices->proc != NULL_xdrproc_t; choices++) {
		if (choices->value == dscm)
			return ((*(choices->proc))(xdrs, unp, LASTUNSIGNED));
	}

	/*
	 * no match - execute the default xdr routine if there is one
	 */
	return ((dfault == NULL_xdrproc_t) ? FALSE :
	    (*dfault)(xdrs, unp, LASTUNSIGNED));
}


/*
 * Non-portable xdr primitives.
 * Care should be taken when moving these routines to new architectures.
 */


/*
 * XDR null terminated ASCII strings
 * xdr_string deals with "C strings" - arrays of bytes that are
 * terminated by a NULL character.  The parameter cpp references a
 * pointer to storage; If the pointer is null, then the necessary
 * storage is allocated.  The last parameter is the max allowed length
 * of the string as specified by a protocol.
 */
bool_t
xdr_string(
	register XDR *xdrs,
	char **cpp,
	unsigned maxsize)
{
	register char *sp = *cpp;  /* sp is the actual string pointer */
	unsigned size;
	unsigned nodesize;

	/*
	 * first deal with the length since xdr strings are counted-strings
	 */
	switch (xdrs->x_op) {
		case XDR_FREE:
			if (sp == NULL)
				return(TRUE);	/* already free */

		/*FALLTHROUGH*/
		case XDR_ENCODE: {
			size_t sz = strlen(sp);

			if (sz > UINT_MAX)
				return (FALSE);

			size = (unsigned)sz;
		}
	}

	if (! xdr_u_int(xdrs, &size))
		return (FALSE);

	if (size > maxsize)
		return (FALSE);

	nodesize = size + 1;

	/*
	 * now deal with the actual bytes
	 */
	switch (xdrs->x_op) {

	case XDR_DECODE:
		if (nodesize == 0) {
			return (TRUE);
		}
		if (sp == NULL)
			*cpp = sp = (char *)mem_alloc(nodesize);
		if (sp == NULL) {
			(void) fprintf(stderr, "xdr_string: out of memory\n");
			return (FALSE);
		}
		sp[size] = 0;
		/* fall into ... */

	/*FALLTHROUGH*/
	case XDR_ENCODE:
		return (xdr_opaque(xdrs, sp, size));

	case XDR_FREE:
		mem_free(sp, nodesize);
		*cpp = NULL;
		return (TRUE);
	}
	return (FALSE);
}

/* 
 * Wrapper for xdr_string that can be called directly from 
 * routines like clnt_call
 */
bool_t
xdr_wrapstring(
	XDR *xdrs,
	char **cpp)
{
	if (xdr_string(xdrs, cpp, LASTUNSIGNED)) {
		return (TRUE);
	}
	return (FALSE);
}
