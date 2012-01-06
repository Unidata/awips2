/* @(#)xdr.h	2.2 88/07/29 4.0 RPCSRC */
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
/*      @(#)xdr.h 1.19 87/04/22 SMI      */

/*
 * xdr.h, External Data Representation Serialization Routines.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#ifndef __XDR_HEADER__
#define __XDR_HEADER__

#include <inttypes.h>	/* int32_t */
#include <limits.h>	/* CHAR_BIT */
#include <stdio.h>
#include <arpa/inet.h>	/* ntohl & htonl() */

#include "types.h"	/* bool_t */

/*
 * XDR provides a conventional way for converting between C data
 * types and an external bit-string representation.  Library supplied
 * routines provide for the conversion on built-in C data types.  These
 * routines and utility routines defined here are used to help implement
 * a type encode/decode routine for each user-defined type.
 *
 * Each data type provides a single procedure which takes two arguments:
 *
 *	bool_t
 *	xdrproc(xdrs, argresp)
 *		XDR *xdrs;
 *		<type> *argresp;
 *
 * xdrs is an instance of a XDR handle, to which or from which the data
 * type is to be converted.  argresp is a pointer to the structure to be
 * converted.  The XDR handle contains an operation field which indicates
 * which of the operations (ENCODE, DECODE * or FREE) is to be performed.
 *
 * XDR_DECODE may allocate space if the pointer argresp is null.  This
 * data can be freed with the XDR_FREE operation.
 *
 * We write only one procedure per data type to make it easy
 * to keep the encode and decode procedures for a data type consistent.
 * In many cases the same code performs all operations on a user defined type,
 * because all the hard work is done in the component type routines.
 * decode as a series of calls on the nested data types.
 */

/*
 * Xdr operations.  XDR_ENCODE causes the type to be encoded into the
 * stream.  XDR_DECODE causes the type to be extracted from the stream.
 * XDR_FREE can be used to release the space allocated by an XDR_DECODE
 * request.
 */
enum xdr_op {
	XDR_ENCODE=0,
	XDR_DECODE=1,
	XDR_FREE=2
};

/*
 * This is the number of bytes per unit of external data.
 */
#define BYTES_PER_XDR_UNIT	(32/CHAR_BIT)
#define RNDUP(x)  ((((x) + BYTES_PER_XDR_UNIT - 1) / BYTES_PER_XDR_UNIT) \
		    * BYTES_PER_XDR_UNIT)

/*
 * The XDR handle.
 * Contains operation which is being applied to the stream,
 * an operations vector for the paticular implementation (e.g. see xdr_mem.c),
 * and two private fields for the use of the particular impelementation.
 */
typedef struct XDR	XDR;
struct XDR {
	enum xdr_op	x_op;		/* operation; fast additional param */
	struct xdr_ops {
		/* get a long from underlying stream */
		bool_t	(*x_getlong)(XDR *xdrs, uint32_t *lp);

		/* put a long to " */
		bool_t	(*x_putlong)(XDR *xdrs, uint32_t *lp);

		/* get some bytes from " */
		bool_t	(*x_getbytes)(XDR *xdrs, char* addr, unsigned len);

		/* put some bytes to " */
		bool_t	(*x_putbytes)(XDR *xdrs, char* addr, unsigned len);

		/* returns bytes off from beginning */
		unsigned (*x_getpostn)(XDR *xdrs);

		/* lets you reposition the stream */
		bool_t 	 (*x_setpostn)(XDR *xdrs, unsigned pos);

		/* buf quick ptr to buffered data */
		uint32_t*(*x_inline)(XDR *xdrs, unsigned len);

		/* free privates of this xdr_stream */
		void	(*x_destroy)(XDR *xdrs);
	} *x_ops;
	char* 	x_public;	/* users' data */
	char*		x_private;	/* pointer to private data */
	char* 	x_base;		/* private used for position info */
	unsigned 	x_handy;	/* extra private word */
};

/*
 * A xdrproc_t exists for each data type which is to be encoded or decoded.
 *
 * The second argument to the xdrproc_t is a pointer to an opaque pointer.
 * The opaque pointer generally points to a structure of the data type
 * to be decoded.  If this pointer is 0, then the type routines should
 * allocate dynamic storage of the appropriate size and return it.
 * bool_t	(*xdrproc_t)(XDR *, char* *);
 *
 * An old-style function declaration is used because rpcgen(1)-generated
 * code assumes that form.
 */
typedef	bool_t (*xdrproc_t)();

/*
 * Operations defined on a XDR handle
 *
 * XDR		*xdrs;
 * long		*longp;
 * char*	 addr;
 * unsigned 	 len;
 * unsigned 	 pos;
 */
#define XDR_GETLONG(xdrs, longp)			\
	(*(xdrs)->x_ops->x_getlong)(xdrs, longp)
#define xdr_getlong(xdrs, longp)			\
	(*(xdrs)->x_ops->x_getlong)(xdrs, longp)

#define XDR_PUTLONG(xdrs, longp)			\
	(*(xdrs)->x_ops->x_putlong)(xdrs, longp)
#define xdr_putlong(xdrs, longp)			\
	(*(xdrs)->x_ops->x_putlong)(xdrs, longp)

#define XDR_GETBYTES(xdrs, addr, len)			\
	(*(xdrs)->x_ops->x_getbytes)(xdrs, addr, len)
#define xdr_getbytes(xdrs, addr, len)			\
	(*(xdrs)->x_ops->x_getbytes)(xdrs, addr, len)

#define XDR_PUTBYTES(xdrs, addr, len)			\
	(*(xdrs)->x_ops->x_putbytes)(xdrs, addr, len)
#define xdr_putbytes(xdrs, addr, len)			\
	(*(xdrs)->x_ops->x_putbytes)(xdrs, addr, len)

#define XDR_GETPOS(xdrs)				\
	(*(xdrs)->x_ops->x_getpostn)(xdrs)
#define xdr_getpos(xdrs)				\
	(*(xdrs)->x_ops->x_getpostn)(xdrs)

#define XDR_SETPOS(xdrs, pos)				\
	(*(xdrs)->x_ops->x_setpostn)(xdrs, pos)
#define xdr_setpos(xdrs, pos)				\
	(*(xdrs)->x_ops->x_setpostn)(xdrs, pos)

#define	XDR_INLINE(xdrs, len)				\
	(*(xdrs)->x_ops->x_inline)(xdrs, len)
#define	xdr_inline(xdrs, len)				\
	(*(xdrs)->x_ops->x_inline)(xdrs, len)

#define	XDR_DESTROY(xdrs)				\
	if ((xdrs)->x_ops->x_destroy) 			\
		(*(xdrs)->x_ops->x_destroy)(xdrs)
#define	xdr_destroy(xdrs)				\
	if ((xdrs)->x_ops->x_destroy) 			\
		(*(xdrs)->x_ops->x_destroy)(xdrs)

/*
 * Support struct for discriminated unions.
 * You create an array of xdrdiscrim structures, terminated with
 * a entry with a null procedure pointer.  The xdr_union routine gets
 * the discriminant value and then searches the array of structures
 * for a matching value.  If a match is found the associated xdr routine
 * is called to handle that part of the union.  If there is
 * no match, then a default routine may be called.
 * If there is no match and no default routine it is an error.
 */
#define NULL_xdrproc_t ((xdrproc_t)0)
struct xdr_discrim {
	int	value;
	xdrproc_t proc;
};

/*
 * In-line routines for fast encode/decode of primitve data types.
 * Caveat emptor: these use single memory cycles to get the
 * data from the underlying buffer, and will fail to operate
 * properly if the data is not aligned.  The standard way to use these
 * is to say:
 *	if ((buf = XDR_INLINE(xdrs, count)) == NULL)
 *		return (FALSE);
 *	<<< macro calls >>>
 * where ``count'' is the number of bytes of data occupied
 * by the primitive data types.
 *
 * N.B. and frozen for all time: each data type here uses 4 bytes
 * of external representation.
 */
#define IXDR_GET_LONG(buf)		(ntohl(*(buf)++))
#define IXDR_PUT_LONG(buf, v)		(*(buf)++ = htonl(v))

#define IXDR_GET_BOOL(buf)		IXDR_GET_LONG(buf)
#define IXDR_GET_ENUM(buf, t)		((t)IXDR_GET_LONG(buf))
#define IXDR_GET_U_LONG(buf)		((uint32_t)IXDR_GET_LONG(buf))
#define IXDR_GET_SHORT(buf)		((int16_t)IXDR_GET_LONG(buf))
#define IXDR_GET_U_SHORT(buf)		((uint16_t)IXDR_GET_LONG(buf))

#define IXDR_PUT_BOOL(buf, v)		IXDR_PUT_LONG(buf, v)
#define IXDR_PUT_ENUM(buf, v)		IXDR_PUT_LONG(buf, v)
#define IXDR_PUT_U_LONG(buf, v)		IXDR_PUT_LONG(buf, v)
#define IXDR_PUT_SHORT(buf, v)		IXDR_PUT_LONG(buf, v)
#define IXDR_PUT_U_SHORT(buf, v)	IXDR_PUT_LONG(buf, v)

/*
 * These are the "generic" xdr routines.
 */
#define xdr_void	my_xdr_void
extern bool_t	xdr_void(XDR *xdrs, char* addr);
#define xdr_int		my_xdr_int
extern bool_t	xdr_int(XDR *xdrs, int *ip);
#define xdr_u_int	my_xdr_u_int
extern bool_t	xdr_u_int(XDR *xdrs, unsigned *up);
#define xdr_long	my_xdr_long
extern bool_t	xdr_long(XDR *xdrs, long *lp);
#define xdr_u_long	my_xdr_u_long
extern bool_t	xdr_u_long(XDR *xdrs, unsigned long *ulp);
#define xdr_short	my_xdr_short
extern bool_t	xdr_short(XDR *xdrs, short *sp);
#define xdr_u_short	my_xdr_u_short
extern bool_t	xdr_u_short(XDR *xdrs, unsigned short *usp);
#define xdr_bool	my_xdr_bool
extern bool_t	xdr_bool(XDR *xdrs, bool_t *bp);
#define xdr_enum	my_xdr_enum
extern bool_t	xdr_enum(XDR *xdrs, enum_t *ep);
#define xdr_free	my_xdr_free
extern void	xdr_free(xdrproc_t xdr_result, char* result);
#define xdr_array	my_xdr_array
extern bool_t	xdr_array(
	XDR *xdrs,
	char* *addrp,		/* array pointer */
	unsigned *sizep,		/* number of elements */
	unsigned maxsize,		/* max numberof elements */
	unsigned elsize,		/* size in bytes of each element */
	xdrproc_t elproc);	/* xdr routine to handle each element */
#define xdr_bytes	my_xdr_bytes
extern bool_t	xdr_bytes(
	XDR *xdrs,
	char **cpp,
	unsigned *sizep,
	unsigned maxsize);
#define xdr_opaque	my_xdr_opaque
extern bool_t	xdr_opaque(
	XDR *xdrs,
	char* cp,
	unsigned cnt);
#define xdr_string	my_xdr_string
extern bool_t	xdr_string(
	XDR *xdrs,
	char **cpp,
	unsigned maxsize);
#define xdr_union	my_xdr_union
extern bool_t	xdr_union(
	XDR *xdrs,
	enum_t *dscmp,		/* enum to decide which arm to work on */
	char *unp,		/* the union itself */
	struct xdr_discrim *choices,	/* [value, xdr proc] for each arm */
	xdrproc_t dfault);	/* default xdr routine */
#define xdr_char	my_xdr_char
extern bool_t	xdr_char(XDR *xdrs, char *cp);
#define xdr_u_char	my_xdr_u_char
extern bool_t	xdr_u_char(XDR *xdrs, unsigned char *cp);
#define xdr_vector	my_xdr_vector
extern bool_t	xdr_vector(
	XDR *xdrs,
	char *basep,
	unsigned nelem,
	unsigned elemsize,
	xdrproc_t xdr_elem);	
#define xdr_float	my_xdr_float
extern bool_t	xdr_float(XDR *xdrs, float *fp);
#define xdr_double	my_xdr_double
extern bool_t	xdr_double(register XDR *xdrs, double *dp);
#define xdr_reference	my_xdr_reference
extern bool_t	xdr_reference(
	XDR *xdrs,
	char* *pp,		/* the pointer to work on */
	unsigned size,		/* size of the object pointed to */
	xdrproc_t proc);	/* xdr routine to handle the object */
#define xdr_pointer	my_xdr_pointer
extern bool_t	xdr_pointer(
	XDR *xdrs,
	char **objpp,
	unsigned obj_size,
	xdrproc_t xdr_obj);
#define xdr_wrapstring	my_xdr_wrapstring
extern bool_t	xdr_wrapstring(XDR *xdrs, char **cpp);

/*
 * Common opaque bytes objects used by many rpc protocols;
 * declared here due to commonality.
 */
#define MAX_NETOBJ_SZ 1024 
struct netobj {
	unsigned n_len;
	char	*n_bytes;
};
typedef struct netobj netobj;
#define xdr_netobj	my_xdr_netobj
extern bool_t   xdr_netobj(XDR *xdrs, struct netobj *np);

/*
 * These are the public routines for the various implementations of
 * xdr streams.
 */
/* XDR using memory buffers */
#define xdrmem_create	my_xdrmem_create
extern void   xdrmem_create(
	XDR *xdrs,
	char* addr,
	unsigned size,
	enum xdr_op op);

/* XDR using stdio library */
#define xdrstdio_create	my_xdrstdio_create
extern void   xdrstdio_create(
	XDR *xdrs,
	FILE *file,
	enum xdr_op op);

/* XDR pseudo records for tcp */
#define xdrrec_create	my_xdrrec_create
extern void   xdrrec_create(
	XDR *xdrs,
	unsigned sendsize,
	unsigned recvsize,
	char* tcp_handle,
	int (*readit)(	/* like read, but pass it a tcp_handle, not sock */
	    void* handle,
	    char* buf,
	    int len),
	int (*writeit)(	/* like write, but pass it a tcp_handle, not sock */
	    void* handle,
	    char* buf,
	    int len));

/* make end of xdr record */
#define xdrrec_endofrecord	my_xdrrec_endofrecord
extern bool_t xdrrec_endofrecord(XDR *xdrs, bool_t sendnow);

/* move to beginning of next record */
#define xdrrec_skiprecord	my_xdrrec_skiprecord
extern bool_t xdrrec_skiprecord(XDR *xdrs);

/* true if no more input */
#define xdrrec_eof	my_xdrrec_eof
extern bool_t xdrrec_eof(XDR *xdrs);

#endif /* !__XDR_HEADER__ */
