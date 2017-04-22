/*
 * vector_3.h
 *
 *  Created on: Aug 26, 2011
 *      Author: snaples
 */

#ifndef VECTOR_3_H_
#define VECTOR_3_H_
typedef struct {double v[3];} vector_3d;
#ifdef __cplusplus
extern "C" {
#endif
double x_product(double * a,double * b,double * c) ;
#ifdef __cplusplus
}
#endif
#endif /* VECTOR_3_H_ */
