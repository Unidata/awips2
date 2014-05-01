/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldm_xlen.c,v 3.32.22.1 2005/01/21 21:34:03 steve Exp $ */

/* 
 * Compute how much space an xdr'ed product requires.
 * Needs to stay in sync with the definitions of the data structures
 * and xdr routines. EG, if you add a new field to the product data type,
 * xlen_product changes.
 */

#include <ldmconfig.h>
#include "ldm.h"                /* prod_info */
#include "ldm_xlen.h"
#include <string.h> /* strlen() */

#define xlen_enum   (4)
#define xlen_bool   (4)
#define xlen_int    (4)
#define xlen_u_int  (4)
#define xlen_long (4)
#define xlen_u_long (4)


static size_t
xlen_string(const char *str)
{
        size_t rndup;
#if 1
        size_t len = xlen_u_int;
#else
        /* xdr_stringsafe */
        size_t len = xlen_bool;
        if(str == NULL) return len;
        len += xlen_u_int;
#endif

        len += strlen(str);
        rndup = len % BYTES_PER_XDR_UNIT;
        if(rndup != 0)
        {
                rndup = BYTES_PER_XDR_UNIT - rndup;
                len += rndup;
        }

        return len;
}


#define xlen_timestampt (xlen_long + xlen_long)
#define xlen_feedtypet  (xlen_u_int)
#define xlen_signaturet (16)


size_t
xlen_prod_info(const prod_info *info)
{
        size_t len = xlen_timestampt    /* arrival */
                + xlen_signaturet       /* signature */
                /* origin */
                + xlen_feedtypet        /* feedtype */
                + xlen_u_int            /* seqno */
                /* ident */             
                + xlen_u_int            /* sz */
                ;

        len += xlen_string(info->origin);       /* origin */
        len += xlen_string(info->ident);        /* ident */

        return len;
}


size_t
xlen_dbuf(const dbuf *data)
{
        size_t len = data->dbuf_len;
        size_t rndup;

        len += xlen_u_int;

        rndup = len % BYTES_PER_XDR_UNIT;
        if(rndup != 0)
        {
                rndup = BYTES_PER_XDR_UNIT - rndup;
                len += rndup;
        }

        return len;
}


size_t
xlen_prod_i(const prod_info *info)
{
        size_t len = xlen_prod_info(info);
        size_t rndup;

        len += info->sz;

        rndup = info->sz % BYTES_PER_XDR_UNIT;
        if(rndup != 0)
        {
                rndup = BYTES_PER_XDR_UNIT - rndup;
                len += rndup;
        }
        
        return len;
}


size_t
xlen_product(const product *prod)
{
        return xlen_prod_i(&prod->info);
}
