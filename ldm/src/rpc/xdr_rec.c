/* @(#)xdr_rec.c	2.2 88/08/01 4.0 RPCSRC */
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
static char sccsid[] = "@(#)xdr_rec.c 1.21 87/08/11 Copyr 1984 Sun Micro";
#endif

/*
 * xdr_rec.c, Implements TCP/IP based XDR streams with a "record marking"
 * layer above tcp (for rpc's use).
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * These routines interface XDRSTREAMS to a tcp/ip connection.
 * There is a record marking layer between the xdr stream
 * and the tcp transport level.  A record is composed of one or more
 * record fragments.  A record fragment is a 32-bit header followed
 * by n bytes of data, where n is contained in the header.  The header
 * is represented as a htonl(uint32_t).  The high order bit encodes
 * whether or not the fragment is the last fragment of the record
 * (1 => fragment is last, 0 => more fragments to follow. 
 * The other 31 bits encode the byte length of the fragment.
 */

#include "config.h"

#include <arpa/inet.h>	/* htonl(), ntohl() */
#include <inttypes.h>	/* uint32_t, uintptr_t */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/types.h>
#include <unistd.h>

#include "types.h"
#include "xdr.h"

static bool_t	xdrrec_getlong(XDR *xdrs, uint32_t *lp);
static bool_t	xdrrec_putlong(XDR *xdrs, uint32_t *lp);
static bool_t	xdrrec_getbytes(
	XDR *xdrs,
	register char* addr,
	register unsigned len);
static bool_t	xdrrec_putbytes(
	XDR *xdrs,
	register char* addr,
	register unsigned len);
static unsigned	xdrrec_getpos(XDR *xdrs);
static bool_t	xdrrec_setpos(XDR *xdrs, unsigned pos);
static uint32_t*xdrrec_inline(XDR *xdrs, unsigned len);
static void	xdrrec_destroy(XDR *xdrs);

static struct  xdr_ops xdrrec_ops = {
	xdrrec_getlong,
	xdrrec_putlong,
	xdrrec_getbytes,
	xdrrec_putbytes,
	xdrrec_getpos,
	xdrrec_setpos,
	xdrrec_inline,
	xdrrec_destroy
};

/*
 * A record is composed of one or more record fragments.
 * A record fragment is a 32-bit header followed by zero to
 * 2**32-1 bytes.  The header is treated as a uint32_t and is
 * encode/decoded to the network via htonl/ntohl.  The low order 31 bits
 * are a byte count of the fragment.  The highest order bit is a boolean:
 * 1 => this fragment is the last fragment of the record,
 * 0 => this fragment is followed by more fragment(s).
 *
 * The fragment/record machinery is not general;  it is constructed to
 * meet the needs of xdr and rpc based on tcp.
 */

#define LAST_FRAG ((uint32_t)(1ul << 31))

typedef struct rec_strm {
	char* tcp_handle;
	char* the_buffer;
	/*
	 * out-goung bits
	 */
	int (*writeit)(void* handle, char* buf, int len);
	char* out_base;	/* output buffer (points to frag header) */
	char* out_finger;	/* next output position */
	char* out_boundry;	/* data cannot up to this address */
	uint32_t *frag_header;	/* beginning of current fragment */
	bool_t frag_sent;	/* true if buffer sent in middle of record */
	/*
	 * in-coming bits
	 */
	int (*readit)(void* handle, char* buf, int len);
	unsigned long in_size;	/* fixed size of the input buffer */
	char* in_base;
	char* in_finger;	/* location of next byte to be had */
	char* in_boundry;	/* can read up to this location */
	long fbtbc;		/* fragment bytes to be consumed */
	bool_t last_frag;
	unsigned sendsize;
	unsigned recvsize;
} RECSTREAM;

static unsigned	fix_buf_size(unsigned);
static bool_t	fill_input_buf(RECSTREAM *rstrm);


/*
 * Internal useful routines
 */
static bool_t
flush_out(
	register RECSTREAM *rstrm,
	bool_t eor)
{
	register uint32_t eormask = (uint32_t)((eor == TRUE) ? LAST_FRAG : 0);
	register uint32_t len = (uint32_t)((rstrm->out_finger - 
		(char*)rstrm->frag_header) - sizeof(uint32_t));

	*(rstrm->frag_header) = (uint32_t)htonl(len | eormask);
	len = (uint32_t)(rstrm->out_finger - rstrm->out_base);
	if ((*(rstrm->writeit))(rstrm->tcp_handle, rstrm->out_base, (int)len)
		!= (int)len)
		return (FALSE);
	rstrm->frag_header = (uint32_t*)rstrm->out_base;
	rstrm->out_finger = (char*)rstrm->out_base + sizeof(uint32_t);
	return (TRUE);
}

static bool_t  /* knows nothing about records!  Only about input buffers */
get_input_bytes(
	register RECSTREAM *rstrm,
	register char* addr,
	register int len)
{
	register int current;

	while (len > 0) {
		current = (int)(rstrm->in_boundry - rstrm->in_finger);
		if (current == 0) {
			if (! fill_input_buf(rstrm))
				return (FALSE);
			continue;
		}
		current = (len < current) ? len : current;
		bcopy(rstrm->in_finger, addr, current);
		rstrm->in_finger += current;
		addr += current;
		len -= current;
	}
	return (TRUE);
}

static bool_t  /* next 32-bits of the input stream are treated as a header */
set_input_fragment(
	register RECSTREAM *rstrm)
{
	uint32_t header;

	if (! get_input_bytes(rstrm, (char*)&header, sizeof(header)))
		return (FALSE);
	header = ntohl(header);
	rstrm->last_frag = ((header & LAST_FRAG) == 0) ? FALSE : TRUE;
	rstrm->fbtbc = header & (~LAST_FRAG);
	return (TRUE);
}

static bool_t  /* consumes input bytes; knows nothing about records! */
skip_input_bytes(
	register RECSTREAM *rstrm,
	long cnt)
{
	register int current;

	while (cnt > 0) {
		current = (int)(rstrm->in_boundry - rstrm->in_finger);
		if (current == 0) {
			if (! fill_input_buf(rstrm))
				return (FALSE);
			continue;
		}
		current = (cnt < current) ? (int)cnt : current;
		rstrm->in_finger += current;
		cnt -= current;
	}
	return (TRUE);
}


/*
 * Create an xdr handle for xdrrec
 * xdrrec_create fills in xdrs.  Sendsize and recvsize are
 * send and recv buffer sizes (0 => use default).
 * tcp_handle is an opaque handle that is passed as the first parameter to
 * the procedures readit and writeit.  Readit and writeit are read and
 * write respectively.   They are like the system
 * calls expect that they take an opaque handle rather than an fd.
 */
void
xdrrec_create(
	register XDR *xdrs,
	register unsigned sendsize,
	register unsigned recvsize,
	char* tcp_handle,
	int (*readit)(	/* like read, but pass it a tcp_handle, not sock */
	    void* handle, char* buf, int len),
	int (*writeit)(	/* like write, but pass it a tcp_handle, not sock */
	    void* handle, char* buf, int len))
{
	register RECSTREAM *rstrm =
		(RECSTREAM *)mem_alloc(sizeof(RECSTREAM));

	if (rstrm == NULL) {
		(void)fprintf(stderr, "xdrrec_create: out of memory\n");
		/* 
		 *  This is bad.  Should rework xdrrec_create to 
		 *  return a handle, and in this case return NULL
		 */
		return;
	}
	/*
	 * adjust sizes and allocate buffer quad-byte aligned.
	 */
	rstrm->sendsize = sendsize = fix_buf_size(sendsize);
	rstrm->recvsize = recvsize = fix_buf_size(recvsize);
	rstrm->the_buffer = mem_alloc(sendsize + recvsize + BYTES_PER_XDR_UNIT);
	if (rstrm->the_buffer == NULL) {
		(void)fprintf(stderr, "xdrrec_create: out of memory\n");
		return;
	}
	rstrm->out_base = rstrm->the_buffer;
	{
	    int	rem = (int)((uintptr_t)rstrm->the_buffer % BYTES_PER_XDR_UNIT);

	    if (rem != 0)
		rstrm->out_base += BYTES_PER_XDR_UNIT - rem;
	}
	rstrm->in_base = rstrm->out_base + sendsize;
	/*
	 * now the rest ...
	 */
	xdrs->x_ops = &xdrrec_ops;
	xdrs->x_private = (char*)rstrm;
	rstrm->tcp_handle = tcp_handle;
	rstrm->readit = readit;
	rstrm->writeit = writeit;

	rstrm->frag_header = (uint32_t*)rstrm->out_base;
	rstrm->out_boundry = rstrm->out_base + sendsize;
	rstrm->out_finger = rstrm->out_base + sizeof(uint32_t);
	rstrm->frag_sent = FALSE;

	rstrm->in_size = recvsize;
	rstrm->in_boundry = rstrm->in_base + recvsize;
	rstrm->in_finger = rstrm->in_boundry;
	rstrm->fbtbc = 0;
	rstrm->last_frag = TRUE;
}


/*
 * The routines defined below are the xdr ops which will go into the
 * xdr handle filled in by xdrrec_create.
 */

static bool_t
xdrrec_getlong(
	XDR *xdrs,
	uint32_t *lp)
{
	register RECSTREAM *rstrm = (RECSTREAM *)(xdrs->x_private);
	register uint32_t *buflp = (uint32_t *)(rstrm->in_finger);
	uint32_t mylong;

	/* first try the inline, fast case */
	if ((rstrm->fbtbc >= sizeof(uint32_t)) &&
		(((char*)rstrm->in_boundry - (char*)buflp) >= sizeof(uint32_t))) {
		*lp = ntohl(*buflp);
		rstrm->fbtbc -= sizeof(uint32_t);
		rstrm->in_finger += sizeof(uint32_t);
	} else {
		if (! xdrrec_getbytes(xdrs, (char*)&mylong, sizeof(uint32_t)))
			return (FALSE);
		*lp = (uint32_t)ntohl(mylong);
	}
	return (TRUE);
}

static bool_t
xdrrec_putlong(
	XDR *xdrs,
	uint32_t *lp)
{
	register RECSTREAM *rstrm = (RECSTREAM *)(xdrs->x_private);
	register uint32_t *dest_lp = ((uint32_t *)(rstrm->out_finger));

	if ((rstrm->out_finger += sizeof(uint32_t)) > rstrm->out_boundry) {
		/*
		 * this case should almost never happen so the code is
		 * inefficient
		 */
		rstrm->out_finger -= sizeof(uint32_t);
		rstrm->frag_sent = TRUE;
		if (! flush_out(rstrm, FALSE))
			return (FALSE);
		dest_lp = ((uint32_t *)(rstrm->out_finger));
		rstrm->out_finger += sizeof(uint32_t);
	}
	*dest_lp = (uint32_t)htonl(*lp);
	return (TRUE);
}

static bool_t  /* must manage buffers, fragments, and records */
xdrrec_getbytes(
	XDR *xdrs,
	register char* addr,
	register unsigned len)
{
	register RECSTREAM *rstrm = (RECSTREAM *)(xdrs->x_private);
	register int current;

	while (len != 0) {
		current = (int)rstrm->fbtbc;
		if (current == 0) {
			if (rstrm->last_frag)
				return (FALSE);
			if (! set_input_fragment(rstrm))
				return (FALSE);
			continue;
		}
		current = (len < current) ? (int)len : current;
		if (! get_input_bytes(rstrm, addr, current))
			return (FALSE);
		addr += current; 
		rstrm->fbtbc -= current;
		len -= current;
	}
	return (TRUE);
}

static bool_t
xdrrec_putbytes(
	XDR *xdrs,
	register char* addr,
	register unsigned len)
{
	register RECSTREAM *rstrm = (RECSTREAM *)(xdrs->x_private);
	register int current;

	while (len != 0) {
		current = (int)(rstrm->out_boundry - rstrm->out_finger);
		current = (len < current) ? (int)len : current;
		bcopy(addr, rstrm->out_finger, current);
		rstrm->out_finger += current;
		addr += current;
		len -= current;
		if (rstrm->out_finger == rstrm->out_boundry) {
			rstrm->frag_sent = TRUE;
			if (! flush_out(rstrm, FALSE))
				return (FALSE);
		}
	}
	return (TRUE);
}

static unsigned
xdrrec_getpos(
	register XDR *xdrs)
{
	register RECSTREAM *rstrm = (RECSTREAM *)xdrs->x_private;
	register long pos;

	pos = lseek(*(int*)rstrm->tcp_handle, (off_t) 0, SEEK_CUR);
	if (pos != -1)
		switch (xdrs->x_op) {

		case XDR_ENCODE:
			pos += rstrm->out_finger - rstrm->out_base;
			break;

		case XDR_DECODE:
			pos -= rstrm->in_boundry - rstrm->in_finger;
			break;

		default:
			pos = -1;
			break;
		}
	return ((unsigned) pos);
}

static bool_t
xdrrec_setpos(
	register XDR *xdrs,
	unsigned pos)
{
	register RECSTREAM *rstrm = (RECSTREAM *)xdrs->x_private;
	unsigned currpos = xdrrec_getpos(xdrs);
	int delta = currpos - pos;
	char* newpos;

	if ((int)currpos != -1)
		switch (xdrs->x_op) {

		case XDR_ENCODE:
			newpos = rstrm->out_finger - delta;
			if ((newpos > (char*)(rstrm->frag_header)) &&
				(newpos < rstrm->out_boundry)) {
				rstrm->out_finger = newpos;
				return (TRUE);
			}
			break;

		case XDR_DECODE:
			newpos = rstrm->in_finger - delta;
			if ((delta < (rstrm->fbtbc)) &&
				(newpos <= rstrm->in_boundry) &&
				(newpos >= rstrm->in_base)) {
				rstrm->in_finger = newpos;
				rstrm->fbtbc -= delta;
				return (TRUE);
			}
			break;
		}
	return (FALSE);
}

static uint32_t *
xdrrec_inline(
	register XDR *xdrs,
	unsigned len)
{
	register RECSTREAM *rstrm = (RECSTREAM *)xdrs->x_private;
	uint32_t * buf = NULL;

	switch (xdrs->x_op) {

	case XDR_ENCODE:
		if ((rstrm->out_finger + len) <= rstrm->out_boundry) {
			buf = (uint32_t *) rstrm->out_finger;
			rstrm->out_finger += len;
		}
		break;

	case XDR_DECODE:
		if ((len <= rstrm->fbtbc) &&
			((rstrm->in_finger + len) <= rstrm->in_boundry)) {
			buf = (uint32_t *) rstrm->in_finger;
			rstrm->fbtbc -= len;
			rstrm->in_finger += len;
		}
		break;
	}
	return (buf);
}

static void
xdrrec_destroy(
	register XDR *xdrs)
{
	register RECSTREAM *rstrm = (RECSTREAM *)xdrs->x_private;

	mem_free(rstrm->the_buffer,
		rstrm->sendsize + rstrm->recvsize + BYTES_PER_XDR_UNIT);
	mem_free((char*)rstrm, sizeof(RECSTREAM));
}


/*
 * Exported routines to manage xdr records
 */

/*
 * Before reading (deserializing from the stream, one should always call
 * this procedure to guarantee proper record alignment.
 */
bool_t
xdrrec_skiprecord(
	XDR *xdrs)
{
	register RECSTREAM *rstrm = (RECSTREAM *)(xdrs->x_private);

	while (rstrm->fbtbc > 0 || (! rstrm->last_frag)) {
		if (! skip_input_bytes(rstrm, rstrm->fbtbc))
			return (FALSE);
		rstrm->fbtbc = 0;
		if ((! rstrm->last_frag) && (! set_input_fragment(rstrm)))
			return (FALSE);
	}
	rstrm->last_frag = FALSE;
	return (TRUE);
}

/*
 * Look ahead fuction.
 * Returns TRUE iff there is no more input in the buffer 
 * after consuming the rest of the current record.
 */
bool_t
xdrrec_eof(
	XDR *xdrs)
{
	register RECSTREAM *rstrm = (RECSTREAM *)(xdrs->x_private);

	while (rstrm->fbtbc > 0 || (! rstrm->last_frag)) {
		if (! skip_input_bytes(rstrm, rstrm->fbtbc))
			return (TRUE);
		rstrm->fbtbc = 0;
		if ((! rstrm->last_frag) && (! set_input_fragment(rstrm)))
			return (TRUE);
	}
	if (rstrm->in_finger == rstrm->in_boundry)
		return (TRUE);
	return (FALSE);
}

/*
 * The client must tell the package when an end-of-record has occurred.
 * The second paraemters tells whether the record should be flushed to the
 * (output) tcp stream.  (This let's the package support batched or
 * pipelined procedure calls.)  TRUE => immmediate flush to tcp connection.
 */
bool_t
xdrrec_endofrecord(
	XDR *xdrs,
	bool_t sendnow)
{
	register RECSTREAM *rstrm = (RECSTREAM *)(xdrs->x_private);
	register uint32_t len;  /* fragment length */

	if (sendnow || rstrm->frag_sent ||
		    (rstrm->out_finger + sizeof(uint32_t) >=
		    rstrm->out_boundry)) {
		rstrm->frag_sent = FALSE;
		return (flush_out(rstrm, TRUE));
	}
	len = (uint32_t)(rstrm->out_finger - (char*)rstrm->frag_header -
	   sizeof(uint32_t));
	*(rstrm->frag_header) = (uint32_t)htonl((uint32_t)(len | LAST_FRAG));
	rstrm->frag_header = (uint32_t*)rstrm->out_finger;
	rstrm->out_finger += sizeof(uint32_t);
	return (TRUE);
}

static bool_t  /* knows nothing about records!  Only about input buffers */
fill_input_buf(
	register RECSTREAM *rstrm)
{
	register char* where;
	unsigned i;
	register int len;

	where = rstrm->in_base;
	i = (unsigned)((uintptr_t)rstrm->in_boundry % BYTES_PER_XDR_UNIT);
	where += i;
	len = (int)(rstrm->in_size - i);
	if ((len = (*(rstrm->readit))(rstrm->tcp_handle, where, len)) == -1)
		return (FALSE);
	rstrm->in_finger = where;
	where += len;
	rstrm->in_boundry = where;
	return (TRUE);
}

static unsigned
fix_buf_size(
	register unsigned s)
{

	if (s < 100)
		s = 4000;
	return (RNDUP(s));
}
