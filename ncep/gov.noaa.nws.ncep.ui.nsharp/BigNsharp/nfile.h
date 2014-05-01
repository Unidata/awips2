/************************************************************************
 * nfile.h								*
 *									*
 * This header file is used in the grid diagnostics package.         	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	03/05	From DGCMN.CMN				*
 * R. Tian/SAIC		01/06	Modified				*
 ************************************************************************/

#ifndef _NFILE_H
#define _NFILE_H

#include	"gemprm.h"

/*
 * This structure contains GDFILE entries.
 */
#define		NGDFLS		MMFILE

struct nfile {
    /*
     * template associated with GDFILE entry.
     */
    char	ntmplt[NGDFLS][MXFLSZ+1];

    /*
     * path associated with GDFILE entry if entry is a template.
     */
    char	gflpth[NGDFLS][MXFLSZ+1];

    /*
     * current actual file name associated with GDFILE entry.
     */
    char	crtfnm[NGDFLS][MXFLSZ+1];

    /*
     * the information given after | associated with GDFILE entry.
     */
    char	aftrbr[NGDFLS][21];

    /*
     * current GEMPAK time associated with GDFILE entry.
     */
    char	crtgdt1[NGDFLS][MXFLSZ+1];
    char	crtgdt2[NGDFLS][MXFLSZ+1];

    /*
     * Number of ensemble member currently in use.
     */
    int		mbrnum[NGDFLS];

    /*
     * write flag associated with GDFILE entry.
     */
    int		outflg[NGDFLS];

    /*
     * list of times to process.
     */
    char	dtmlst1[LLMXGT][21];
    char	dtmlst2[LLMXGT][21];

    /*
     * number of times in the list.
     */
    int		ntmlst;

    /*
     * index of last time used in dtmlst.
     */
    int		itmlst;

    /*
     * index of navagation reference entry.
     */
    int		irefnv;

    /*
     * flag to use this new code.
     */
    int		nucode;
};

#endif
