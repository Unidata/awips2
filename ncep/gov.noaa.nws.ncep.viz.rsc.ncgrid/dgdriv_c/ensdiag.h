/************************************************************************
 * ensdiag.h								*
 * 									*
 * This header file is used in the ensemble grid diagnostics package.	*
 *									*
 ** Log:								*
 * R. Tian/SAIC		12/05	From decmn.cmn				*
 * R. Tian/SAIC		 9/06	Removed grc.h, fortran_wrapppers.h	*
 * M. Li/SAIC		10/06	MXMBRS->MXMBRS+2 for emvalu		*
 * m.gamazaychikov/SAIC	01/08	Added ewtval and iwlist			*
 ************************************************************************/

#ifndef ENSDIAG_H_
#define ENSDIAG_H_

#include "geminc.h"
#include "gemprm.h"
#include "de.h"

/*
 * Maximum number of ensemble file entry.
 */
#define MAXENT	12

/*
 * Maximum number of ensemble members.
 */
#define MXMBRS	128

/*
 * Maximum number of function arguments.
 */
#define MXARGS	16

/*
 * Maximum number of grid files.
 */
#define NGDFLS	MMFILE

struct ensdiag {
    /*
     * List of templates for members.
     */
    char etmplt[MXMBRS][MXFLSZ+1];

    /* 
     * List of paths for members.
     */
    char enspth[MXMBRS][MXFLSZ+1];

    /*
     * List of full file names of members.
     */
    char ensfnm[MXMBRS][MXFLSZ+1];

    /*
     * List of grid time stamps for members.
     */
    char etimes[MXMBRS][41];

    /*
     * List of starting indices of grids holding member results.
     */
    int iglist[MXMBRS];
    int iwlist[MXMBRS];

    /*
     * Sorting array of pointers to member weights.
     */
    int igpntr[MXMBRS];

    /*
     * Sorting array for values at a grid point.
     */
    float emvalu[MXMBRS+2];
    float ewtval[MXMBRS+2];

    /*
     * List of weights assigned to ensemble members.
     */
    float enswts[MXMBRS];

    /*
     * Ensemble specifications for GDFILE.
     * ( blank if entry is not ensemble list )
     */
    char ensspc[NGDFLS][LLMXLN+1];

    /*
     * Save string for DGCMN ntmplt contents.
     */
    char tmplsv[MXFLSZ+1];

    /*
     * Save string for DGCMN gflpth contents.
     */
    char gpthsv[MXFLSZ+1];

    /*
     * Save string for DGCMN crtfnm contents.
     */
    char flnmsv[MXFLSZ+1];

    /*
     * Save string for DGCMN crtgdt time stamp.
     */
    char gdtmsv1[21];
    char gdtmsv2[21];

    /*
     * Save string for DGCMN ingdtm contents.
     */
    char igtmsv[41];

    /*
     * Array of all arguments.
     */
    char allarg[MXARGS][LLMXLN+1];

    /*
     * Index pointing to current ensemble, specified by ensspc(ndxens).
     */
    int ndxens;

    /*
     * Number of ensemble member file names.
     */
    int nummbr;

    /*
     * ID # to name ENS_ output.
     */
    int idgens;
};

/*
 * DE globals.
 */
#ifdef DE_GLOBAL
    struct ensdiag _ensdiag;
#else
    extern struct ensdiag _ensdiag;
#endif

#endif
