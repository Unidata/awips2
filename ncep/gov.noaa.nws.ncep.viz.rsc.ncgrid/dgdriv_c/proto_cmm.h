/************************************************************************
 * proto_cmm.h                                                          *
 *                                                                      *
 * This include file contains the prototypes of the CMM library.        *
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * R. Tian/SAIC          2/06   Created                                 *
 ***********************************************************************/

#ifndef PROTO_CMM_H_
#define PROTO_CMM_H_

void **cmm_malloc2d ( int nrow, int ncol, int objsiz, int *iret );
void cmm_free2d ( void **ptr2d, int *iret );

#endif
