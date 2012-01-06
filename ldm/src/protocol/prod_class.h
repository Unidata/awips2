/*
 *   Copyright 1994, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: prod_class.h,v 1.17.18.2 2008/04/15 16:34:11 steve Exp $ */

#ifndef _PROD_CLASS_H_
#define _PROD_CLASS_H_

/*
 * Define a set of products
 */
extern const prod_class_t _clss_all;
#define PQ_CLASS_ALL	(&_clss_all)
extern const prod_spec _spec_all;
#define PQ_SPEC_ALL	(&_spec_all)

extern int
timeInClass(const prod_class_t *clss, const timestampt *tsp);

/*
 * returns !0 if "info" matches "clss".
 * 0 otherwise. 
 */
extern int
prodInClass(const prod_class_t *clss, const prod_info *info);

extern void
free_prod_class(prod_class_t *clssp);

extern prod_class_t *
new_prod_class(u_int psa_len);

int spec_eq(prod_spec *left, prod_spec *rght);

int cp_prod_spec(prod_spec *lhs, const prod_spec *rhs);

extern int
clsspsa_eq(const prod_class_t *lhs, const prod_class_t *rhs);

extern int
clss_eq(const prod_class_t *lhs, const prod_class_t *rhs);

extern int
cp_prod_class(prod_class_t *lhs, const prod_class_t *rhs, int shallow);

extern prod_class_t *
dup_prod_class(const prod_class_t *rhs);

extern void
clss_scrunch(prod_class_t *clssp);

extern int
clss_intersect(const prod_class_t *filt, const prod_class_t *want,
	prod_class_t **clsspp);

extern void
clss_regcomp(prod_class_t *clssp);

extern feedtypet
clss_feedtypeU(const prod_class_t *clssp);

#endif /* !_PROD_CLASS_H_ */
