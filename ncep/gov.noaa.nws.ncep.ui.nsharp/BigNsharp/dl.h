/************************************************************************
 * dl.h                                                  		*
 *                                                                      *
 * This header file contains the prototypes of the DL library.		*
 *                                                                      *
 **                                                                     *
 * S. Gilbert/NCEP        12/05   Created                               *
 ***********************************************************************/

#ifndef DL_H_ 
#define DL_H_

void dl_driv ( char *lfunc, int *iret );
void dl_fvonisfc ( char *uargs, char *glvl, char *stprm, int *iret );
void dl_init  ( int *iret );
void dl_lvls ( char **uargs, int nargs, char *glvl, int *nlev, int *iret );
void dl_mxmn ( char *uargs, char *glvl, char *stprm, int *iret );
void dl_setl ( int lflag, float *rlev, int *nlev, int *iret );
void dl_swtm ( char *uargs, char *glvl, char *stprm, int *iret );

#endif
