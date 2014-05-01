/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ldm_xlen.h,v 1.3.18.1 2005/01/21 21:34:16 steve Exp $ */
#ifndef LDM_XLEN_H
#define LDM_XLEN_H

extern size_t xlen_prod_info(const prod_info *info);
extern size_t xlen_dbuf(const dbuf *data);
extern size_t xlen_prod_i(const prod_info *info);
extern size_t xlen_product(const product *prod);

#endif /* !LDM_XLEN_H */
