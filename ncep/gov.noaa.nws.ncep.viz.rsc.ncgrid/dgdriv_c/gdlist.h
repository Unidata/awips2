/************************************************************************
 * gdlist.h                                                             *
 *                                                                      *
 * This header file is used in the GDLIST program.                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         09/06   Created                                 *
 ************************************************************************/

#ifndef GDLIST_H_
#define GDLIST_H_

#include        "geminc.h"
#include        "gemprm.h"

/*
 * Structure for user inputs.
 */
typedef struct {
    char gdfile[LLMXLN+1];	/* Grid file */
    char gdatim[LLMXLN+1];	/* Grid date/time */
    char glevel[LLMXLN+1];	/* Grid level */
    char gvcord[LLMXLN+1];	/* Grid vertical coordinate */
    char gfunc [LLMXLN+1];	/* Scalar grid */
    char garea [LLMXLN+1];	/* Graphics area */
    char proj  [LLMXLN+1];	/* Map projection/angles/margins|drop flag */
    char scale [LLMXLN+1];	/* Scalar scale / vector scale */
    char output[LLMXLN+1];	/* Output device/filename */
} GDLIST_input;

/*
 * APIs.
 */
void gdlinp ( GDLIST_input *ui, int *iret );
void gdldsp ( const char *gdfile, const char *gdtime1, const char *gdtime2,
              const int *level1, const int *level2, const int *ivcord,
              const char *parm, const char *garea, const int *iscale,
              const float *rmin, const float *rmax, const int *termflg,
              const int *fileflg, int *iret );
void gdldta ( const char *gdfile, const char *gdtime1, const char *gdatime2,
              const int *level1, const int *level2, const int *ivcord,
              const char *parm, const float *grid, const int *kx,
              const int *ky, const char *area, const int *ix1, const int *iy1,
              const int *ix2, const int *iy2, const int *scale,
              const int *termflg, const int *fileflg, const char *outfil,
              int *iret );

#endif

