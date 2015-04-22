#include "dg.h"

/************************************************************************
 * dg_ndtmc.c								*
 *									*
 * This module contains subroutines to processe the user input for 	*
 * GDATTIM.								*
 *									*
 * CONTENTS:								*
 *  dg_ndtm()		scans the user input for GDATTIM		*
 *  tmrang()		converts time range to time list		*
 *  tmfill()		fills DTMLST					*
 *  tmflst()		get the first and last time of data set		*
 *  kwrplc()		replace key words with specific time		*
 *  rpflst()		replace FIRST and LAST with specific time	*
 ************************************************************************/

static void tmrang ( const char *tbeg, const char *tend, const char *tinc,
	             int *ityp, int *ntime, char **times, int *iret );
static void tmfill ( int nt1, char **times1, int nt2, char **times2,
                     int *iret );
static void tmflst ( const char *cycle, char *tfst, char *tlst,
		     int *iret );
static void kwrplc ( const char *tmin, char *tmout, int *iret );
static void rpflst ( const char *tmin, char *tmout, int *iret );

/*======================================================================*/

void dgc_ndtm ( const char *gdatm, int *iret )
/************************************************************************
 * dgc_ndtm                                                             *
 *                                                                      *
 * This subroutine scans the user input for GDATTIM and creates an      *
 * internal list of times to process. DG_NFIL must be called first to 	*
 * set either a template for the first GDFILE entry or to open the file	*
 * associated with it. DG_NFIL also determines first and last times	*
 * associated with the first GDFILE entry. The information from DG_NFIL *
 * is in DGCMN.CMN							*
 *									*
 * All indeterminant time substitutions are based on the times		*
 * associated with the first GDFILE entry.				*
 *                                                                      *
 * dgc_ndtm ( gdatm, iret )					        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *gdatm		const char	User input for GDATTIM          *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -22 = invalid time              *
 *                                      -56 = grid file open failed     *
 **                                                                     *
 * Log:                                                                 *
 * K. Brill/HPC          1/04                                           *
 * R. Tian/SAIC		 9/04		Re-coded			*
 * R. Tian/SAIC		 1/06		Recoded from Fortran		*
 ************************************************************************/
{
    char atime[121], tstart[41], tstop[41], tinc[6],
	 first[DTTMSZ], last[DTTMSZ], temptm[DTTMSZ];
    char **gtime, **time, **tlist1, **tlist2;
    int ntime, ntm, itype, ityp1, ityp2, nt1, nt2;
    int nt, ier, ier1, ier2, ier3;
/*----------------------------------------------------------------------*/
    *iret = 0;

    gtime = (char **)cmm_malloc2d ( 32, 121, sizeof(char), &ier1 );
    tlist1 = (char **)cmm_malloc2d ( LLMXGT, DTTMSZ, sizeof(char), &ier2 );
    tlist2 = (char **)cmm_malloc2d ( LLMXGT, DTTMSZ, sizeof(char), &ier3 );
    if ( ier1 != 0 || ier2 != 0 || ier3 != 0 ) {
        cmm_free2d ( (void **)gtime, &ier );
        cmm_free2d ( (void **)tlist1, &ier );
        cmm_free2d ( (void **)tlist2, &ier );
        *iret = -73;
	return;
    }

/*
 * Parse GDATTIM entry for ";"
 */
    cst_clst ( (char *)gdatm, ';', " ", 32, 120, gtime, &ntime, &ier );
    if ( ier != 0 || ntime > 32 ) {
        cmm_free2d ( (void **)gtime, &ier );
	cmm_free2d ( (void **)tlist1, &ier );
	cmm_free2d ( (void **)tlist2, &ier );
	*iret = -22;
	return;
    }

/*
 * Loop over all ";" separated entries
 */
    for ( nt = 0; nt < ntime; nt++ ) {
	strcpy ( atime, gtime[nt] );
	cst_lcuc ( atime, atime, &ier );

/*
 * Replace key words with specific time.
 */
	if ( strstr ( atime, "ALL" ) || strstr ( atime, "FIRST" ) ||
	     strstr ( atime, "LAST" ) ) {
	    kwrplc ( atime, atime, &ier );
	    if ( ier != 0 || strstr ( atime, "FIRST" ) ||
		strstr ( atime, "LAST" ) ) {
        	cmm_free2d ( (void **)gtime, &ier );
		cmm_free2d ( (void **)tlist1, &ier );
		cmm_free2d ( (void **)tlist2, &ier );
		*iret = -22;
		return;
	    }
	}

/*
 * Check GDATTIM entry for ":"
 */
	if ( strchr ( atime, ':' ) ) {
/*
 * Dual time
 */
	    cst_rang ( atime, tstart, tstop, tinc, &itype, &ier );
	    time = (char **)cmm_malloc2d ( 2, DTTMSZ, sizeof(char), &ier );
	    if ( ier != 0 ) {
        	cmm_free2d ( (void **)gtime, &ier );
		cmm_free2d ( (void **)tlist1, &ier );
		cmm_free2d ( (void **)tlist2, &ier );
		*iret = -73;
	        return;
	    }
	    if ( itype == 0 ) {
/*
 * Dual single specified time
 */
		cst_clst ( atime, ':', " ", 2, 20, time, &ntm, &ier );
		if ( _nfile.ntmlst > LLMXGT -1 ) {
        	    cmm_free2d ( (void **)gtime, &ier );
		    cmm_free2d ( (void **)tlist1, &ier );
		    cmm_free2d ( (void **)tlist2, &ier );
        	    cmm_free2d ( (void **)time, &ier );
		    *iret = -22;
		    return;
		}
		ctg_full ( time[0], _dgfile.tfirst[0], _dgfile.tlast[0],
		           _nfile.dtmlst1[_nfile.ntmlst], &ier );
		ctg_full ( time[1], _dgfile.tfirst[0], _dgfile.tlast[0],
		           _nfile.dtmlst2[_nfile.ntmlst], &ier );
		_nfile.ntmlst++;		  	
	    } else if ( itype == 1 ) {
/*
 * Dual time range without increment
 */
        	cmm_free2d ( (void **)gtime, &ier );
		cmm_free2d ( (void **)tlist1, &ier );
		cmm_free2d ( (void **)tlist2, &ier );
        	cmm_free2d ( (void **)time, &ier );
		*iret = -22;
		return;
	    } else if ( itype == 2 ) {
/*
 * Dual time range with increment
 */
		cst_clst ( tstart, ':', " ", 2, 20, time, &ntm, &ier );
		strcpy ( first, time[0] );
		cst_clst ( tstop,  ':', " ", 2, 20, time, &ntm, &ier );
		strcpy ( last, time[0] );
		tmrang ( first, last, tinc, &ityp1, &nt1, tlist1, &ier );
		if ( ier != 0 ) {
        	    cmm_free2d ( (void **)gtime, &ier );
		    cmm_free2d ( (void **)tlist1, &ier );
		    cmm_free2d ( (void **)tlist2, &ier );
        	    cmm_free2d ( (void **)time, &ier );
		    *iret = -22;
		    return;
		}

		cst_clst ( tstart, ':', " ", 2, 20, time, &ntm, &ier );
		strcpy ( first, time[1] );
		cst_clst ( tstop, ':', " ", 2, 20, time, &ntm, &ier );
		strcpy ( last, time[1] );
		tmrang ( first, last, tinc, &ityp2, &nt2, tlist2, &ier );
		if ( ier != 0 || ityp1 != ityp2 || nt1 != nt2 ) {
        	    cmm_free2d ( (void **)gtime, &ier );
		    cmm_free2d ( (void **)tlist1, &ier );
		    cmm_free2d ( (void **)tlist2, &ier );
        	    cmm_free2d ( (void **)time, &ier );
		    *iret = -22;
		    return;
		}
		tmfill ( nt1, tlist1, nt2, tlist2, iret );
	    }

	    cmm_free2d ( (void **)time, &ier );
	} else {
/*
 * No dual time
 */
	    cst_rang ( atime, tstart, tstop, tinc, &itype, &ier );
	    if ( itype == 0 ) {
/*
 * Single specified time
 */
		if ( _nfile.ntmlst > LLMXGT - 1 ) {
        	    cmm_free2d ( (void **)gtime, &ier );
		    cmm_free2d ( (void **)tlist1, &ier );
		    cmm_free2d ( (void **)tlist2, &ier );
		    *iret = -22;
		    return;
		}
		ctg_full ( atime, _dgfile.tfirst[0], _dgfile.tlast[0],
		    temptm, &ier );
		strcpy ( _nfile.dtmlst1[_nfile.ntmlst++], temptm );
	    } else if ( itype == 1 ) {
/*
 * Range without increment
 */
		tinc[0] = '\0';
		tmrang ( tstart, tstop, tinc, &ityp1, &nt1, tlist1, &ier );
		if ( ier != 0 ) {
        	    cmm_free2d ( (void **)gtime, &ier );
		    cmm_free2d ( (void **)tlist1, &ier );
		    cmm_free2d ( (void **)tlist2, &ier );
		    *iret = -22;
		    return;
		}
		tmfill ( nt1, tlist1, 0, tlist2, iret );
	    } else if ( itype == 2 ) {
/*
 * Range with increment
 */
		tmrang ( tstart, tstop, tinc, &ityp1, &nt1, tlist1, &ier );
		if ( ier != 0 ) {
        	    cmm_free2d ( (void **)gtime, &ier );
		    cmm_free2d ( (void **)tlist1, &ier );
		    cmm_free2d ( (void **)tlist2, &ier );
		    *iret = -22;
		    return;
		}
		tmfill ( nt1, tlist1, 0, tlist2, iret );
	    }
	}
    }

    cmm_free2d ( (void **)gtime, &ier );
    cmm_free2d ( (void **)tlist1, &ier );
    cmm_free2d ( (void **)tlist2, &ier );
    
    return;
}

/*=====================================================================*/

static void tmrang ( const char *tbeg, const char *tend, const char *tinc,
	             int *ityp, int *ntime, char **times, int *iret )
/************************************************************************
 * tmrang                                                               *
 *                                                                      *
 * This subroutine converts the time range into a list of times.	*
 *                                                                      *
 * tmrang ( tbeg, tend, tinc, ityp, ntime, times, iret )	       	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *tgeb		const char	Time range begin                *
 *      *tend		const char	Time range end			*
 *      *tinc		const char	Time range increment            *
 *                                                                      *
 * Output parameters:                                                   *
 *	*ityp		int		Vary type			*
 *                                        1 = forecast time varies      *
 *                                        2 = cycle time varies     	*
 *      *ntime		int		Number of times			*
 *      *times		char		Time list			*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -22 = invalid time              *
 *                                      -56 = grid file open failed     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 9/04						*
 * R. Tian/SAIC		 1/05	Changed processing of range with inc	*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 * T. Piper/SAIC        04/07   Modified for cfl_scnt CSC               *
 ***********************************************************************/
{
    char filnam[MXFLSZ], rplc[DTTMSZ], tmplt[65], tstart[DTTMSZ],
	 tstop[DTTMSZ], temptm[DTTMSZ];
    char **tmlist, **tmplst;
    float adum1, adum2;
    int nf, nlist, lns, ifn, mxgd, nfile, ntmp, nt, itmr, itmds, zero, ier;
    int ii, gfalse;
    struct dirent **dnlist=NULL;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *ntime = 0;
    zero = 0;
    gfalse = G_FALSE;

/*
 * Either the cycle time varies or the forecast time varies,
 * but not both. And the time type has to be the same
 */
    ctg_full ( tbeg, _dgfile.tfirst[0], _dgfile.tlast[0], tstart, iret );
    ctg_full ( tend, _dgfile.tfirst[0], _dgfile.tlast[0], tstop,  iret );
    if ( strncmp ( tstart, tstop, 12 ) == 0 ) {
	*ityp = 1;
    } else if ( strcmp ( &tstart[11], &tstop[11] ) == 0 ) {
	*ityp = 2;
    } else {
	*iret = -22;
	return;
    }

    tmlist = (char **)cmm_malloc2d ( LLMXGT, DTTMSZ, sizeof(char), &ier );
    if ( ier != 0 ) {
	*iret = -73;
	return;
    }
    tmplst = (char **)cmm_malloc2d ( LLMXGT, DTTMSZ, sizeof(char), &ier );
    if ( ier != 0 ) {
	cmm_free2d ( (void **)tmlist, &ier );
	*iret = -73;
	return;
    }

/*
 * Get a list of times in dataset.
 */
    if ( strlen ( _nfile.ntmplt[0] ) > 0 ) {
/*
 * First GDFILE entry is a template
 */
	strcpy ( tmplt, _nfile.ntmplt[0] );

/*
 * Try to narrow down the searching of files.
 */
	if ( strncmp ( tstart, tstop, 2 ) == 0 ) {
	    if ( strstr ( tmplt, "YYYY" ) ) {
	        strcpy ( rplc, "YY" );
		strncat ( rplc, tstop, 2 );
		rplc[4] = '\0';
		cst_rpst ( tmplt, "YYYY", rplc, tmplt, &ier );
	    } else {
	        cst_ncpy ( rplc, tstop, 2, &ier );
		cst_rpst ( tmplt, "YY", rplc, tmplt, &ier );
	    }

	    if ( strncmp ( &tstart[2], &tstop[2], 2 ) == 0 ) {
	        cst_ncpy ( rplc, &tstop[2], 2, &ier );
		cst_rpst ( tmplt, "MM", rplc, tmplt, &ier );

		if ( strncmp ( &tstart[4], &tstop[4], 2 ) == 0 ) {
		    cst_ncpy ( rplc, &tstop[4], 2, &ier );
		    cst_rpst ( tmplt, "DD", rplc, tmplt, &ier );

		    if ( strncmp ( &tstart[7], &tstop[7], 2 ) == 0 ) {
		        cst_ncpy ( rplc, &tstop[7], 2, &ier );
			cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
		    }
		}
	    }
	}

	if ( strcmp ( &tstart[12], &tstop[12] ) == 0 ) {
	    if ( strstr ( tmplt, "FFFFF" ) ) {
	        cst_ncpy ( rplc, &tstop[12], 3, &ier );
	        strcat ( rplc, "00" );
		cst_rpst ( tmplt, "FFFFF", rplc, tmplt, &ier );
	    } else if ( strstr ( tmplt, "FFF" ) ) {
	        cst_ncpy ( rplc, &tstop[12], 3, &ier );
		cst_rpst ( tmplt, "FFF", rplc, tmplt, &ier );
	    } else if ( strstr ( tmplt, "FF" ) ) {
	        cst_ncpy ( rplc, &tstop[13], 2, &ier );
		cst_rpst ( tmplt, "FF", rplc, tmplt, &ier );
	    }
	}

/*
 * Do the searching. Find all those files matching the filled
 * template.
 */
	cfl_scnt ( _nfile.gflpth[0], tmplt, 1, &dnlist, &nfile, &ier );

/*
 * Get times from the searched files.
 */
	nlist = 0;
	if ( strstr ( _nfile.ntmplt[0], "fFF" ) ) {
/*
 * Filenames are assumed to contain complete GEMPAK
 * time information.
 */
	    for ( nf = 0; nf < nfile; nf++ ) {
		if ( nlist > LLMXGT - 1 ) {
		    for (ii=nlist;ii<nfile;ii++){
			free(dnlist[ii]);
		    }
		    cmm_free2d ( (void **)tmlist, &ier );
		    cmm_free2d ( (void **)tmplst, &ier );
		    *iret = -22;
		    return;
		}
		cfl_mdat ( dnlist[nf]->d_name, _nfile.ntmplt[0], _dgfile.tlast[0],
		    temptm, &ier );
		free(dnlist[nf]);
		strcpy ( tmlist[nlist++], temptm );
	    }
	} else {
/*
 * File has to be opened to get time(s)
 */
	    strcpy ( filnam, _nfile.gflpth[0] );
	    strcat ( filnam, "/" );
	    lns = strlen ( filnam );
	    for ( nf = 0; nf < nfile; nf++ ) {
		strcpy ( &filnam[lns], dnlist[nf]->d_name );
		free(dnlist[nf]);
		gd_open ( filnam, &gfalse, &zero, &zero, &ifn, &adum1, &adum2,
		          &mxgd, &ier, strlen(filnam) );
		if ( ier != 0 ) {
		    for (ii=nf+1; ii<nfile; ii++){
			free(dnlist[ii]);
		    }
		    cmm_free2d ( (void **)tmlist, &ier );
	    	    cmm_free2d ( (void **)tmplst, &ier );
		    *iret = -56;
		    return;
		}
		cgd_gtim ( ifn, LLMXGT, tmplst, &ntmp, &ier );
		for ( nt = 0; nt < ntmp; nt++ ) {
		    if ( *ityp == 2 ) {
			if ( strstr ( tmplst[nt], &tstop[11] ) ) {
			    if ( nlist > LLMXGT - 1 ) {
		    		cmm_free2d ( (void **)tmlist, &ier );
	    	    		cmm_free2d ( (void **)tmplst, &ier );
				*iret = -22;
				return;
			    }
			    strcpy ( tmlist[nlist++], tmplst[nt] );
			}
		    } else {
			if ( nlist > LLMXGT - 1 ) {
			    cmm_free2d ( (void **)tmlist, &ier );
	    	    	    cmm_free2d ( (void **)tmplst, &ier );
			    *iret = -22;
			    return;
			}
			strcpy ( tmlist[nlist++], tmplst[nt] );
		    }
		}
	    }
	}
	if ( dnlist != NULL ) free(dnlist);
    } else {
/*
 * First GDFILE entry is an actual file
 */
	gd_open ( _nfile.crtfnm[0], &gfalse, &zero, &zero, &ifn, &adum1, &adum2,
	          &mxgd, &ier, strlen(_nfile.crtfnm[0]) );
	if ( ier != 0 ) {
	    cmm_free2d ( (void **)tmlist, &ier );
	    cmm_free2d ( (void **)tmplst, &ier );
	    *iret = -56;
	    return;
	}
	cgd_gtim ( ifn, LLMXGT, tmlist, &nlist, &ier );
    }

/*
 * Keep only those times that satisfy the range specification.
 */
    if ( strlen( tinc ) == 0 ) {
/*
 * Time range without increment
 */
	ctg_rnoi ( tstart, tstop, *ityp, nlist, tmlist, ntime, times, iret );
    } else {
/*
 * Time range with increment
 */
	ctg_rinc ( tstart, tstop, tinc, *ityp, &ntmp, tmplst, iret );
	for ( itmr = 0; itmr < ntmp; itmr++ ) {
	    for ( itmds = 0; itmds < nlist; itmds++ ) {
		if ( strcmp ( tmlist[itmds], tmplst[itmr] ) == 0 ) {
		    strcpy ( times[(*ntime)++], tmlist[itmds] );
		    break;
		}
	    }
	}
    }

    cmm_free2d ( (void **)tmlist, &ier );
    cmm_free2d ( (void **)tmplst, &ier );

    return;
}

/*=====================================================================*/

static void tmfill ( int nt1, char **times1, int nt2, char **times2,
                     int *iret )
/************************************************************************
 * tmfill                                                               *
 *                                                                      *
 * This subroutine fills the DTMLST.					*
 *                                                                      *
 * tmfill ( nt1, times1, nt2, times2, iret )	    			*
 *                                                                      *
 * Input parameters:                                                    *
 *      nt1		int		Number of times in list         *
 *      **times1	cha		rTime list for time 1       	*
 *      nt2		int		Number of times in list         *
 *      **times2	char		Time list for time 2       	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -22 = invalid time              *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 9/04						*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 ************************************************************************/
{
    int nt;
/*----------------------------------------------------------------------*/
    *iret = 0;

    for ( nt = 0; nt < nt1; nt++ ) {
	if ( _nfile.ntmlst > LLMXGT - 1 ) {
	    *iret = -22;
	    return;
	}
	strcpy ( _nfile.dtmlst1[_nfile.ntmlst], times1[nt] );

	if ( nt2 == nt1 ) {
	    strcpy( _nfile.dtmlst2[_nfile.ntmlst], times2[nt] );
	}

	_nfile.ntmlst++;
    }

    return;
}

/*=====================================================================*/

static void tmflst ( const char *cycle, char *tfst, char *tlst,
                     int *iret )
/************************************************************************
 * tmflst                                                               *
 *                                                                      *
 * This subroutine finds the first and last time of the given cycle, or	*
 * the first and last time of the dataset if cycle is blank.		*
 *                                                                      *
 * tmflst ( cycle, tfst, tlst, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *cycle		const char	Cycle time			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *tfst		char		First time			*
 *      *tlst		char		Last time			*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -56 = grid file open failed     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 9/04						*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 * T. Piper/SAIC	04/07	Modified for cfl_scnt CSC		*
 ***********************************************************************/
{
    char tmplt[65], rplc[DTTMSZ], temptm[DTTMSZ], filnam[MXFLSZ];
    char **tmplst;
    float adum1, adum2;
    int ii, ifn, ngrd, mxgd, nf, ntime, nt, zero, ier;
    int gfalse;
    struct dirent **dnlist=NULL;
/*---------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    gfalse = G_FALSE;
    tfst[0] = '\0';
    tlst[0] = '\0';

/*
 * Check the first GDFILE entry
 */
    if ( strlen ( _nfile.ntmplt[0] ) != 0 ) {
/*
 * First GDFILE entry is a template
 */
	strcpy ( tmplt, _nfile.ntmplt[0] );
	if ( strlen ( cycle ) != 0 ) {
	    if ( strstr ( tmplt, "YYYY" ) ) {
	        strcpy ( rplc, "YY" );
		strncat ( rplc, cycle, 6 );
		rplc[8] = '\0';
		cst_rpst ( tmplt, "YYYYMMDD", rplc, tmplt, &ier );
	    } else {
	        cst_ncpy ( rplc, cycle, 6, &ier );
		cst_rpst ( tmplt, "YYMMDD", rplc, tmplt, &ier );
	    }
	    cst_ncpy ( rplc, &cycle[7], 2, &ier );
	    cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
	}
	cfl_scnt ( _nfile.gflpth[0], tmplt, -1, &dnlist, &nf, &ier );
	strcpy ( filnam, _nfile.gflpth[0] );
	strcat ( filnam, "/" );
	if (nf > 0)  strcat ( filnam, dnlist[0]->d_name );
	for ( ii=0; ii<nf;ii++){
	    free(dnlist[ii]);
	}
	if ( dnlist != NULL) free(dnlist);
	gd_open ( filnam, &gfalse, &zero, &zero, &ifn, &adum1, &adum2, &mxgd,
		  &ier, strlen(filnam) );
	if ( ier != 0 ) {
	    *iret = -62;
	    return;
	}
	cgd_ngrd ( ifn, &ngrd, temptm, tlst, &ier );

	cfl_scnt ( _nfile.gflpth[0], tmplt, 1, &dnlist, &nf, &ier );
	strcpy ( filnam, _nfile.gflpth[0] );
	strcat ( filnam, "/" );
	if (nf > 0)  strcat ( filnam, dnlist[0]->d_name );
	for ( ii=0; ii<nf;ii++){
            free(dnlist[ii]);
        }
        if ( dnlist != NULL) free(dnlist);
	gd_open ( filnam, &gfalse, &zero, &zero, &ifn, &adum1, &adum2, &mxgd,
		  &ier, strlen(filnam) );
	if ( ier != 0 ) {
	    *iret = -62;
	    return;
	}
	cgd_ngrd ( ifn, &ngrd, tfst, temptm, &ier );

    } else {
/*
 * First GDFILE entry is an actual file
 */
	gd_open ( _nfile.crtfnm[0], &gfalse, &zero, &zero, &ifn, 
		  &adum1, &adum2, &mxgd, &ier, strlen(_nfile.crtfnm[0]) );
	if ( ier != 0 ) {
	    *iret = -56;
	    return;
	}
	if ( strlen ( cycle ) == 0 ) {
	    cgd_ngrd ( ifn, &ngrd, tfst, tlst, &ier );
	} else {
	    tmplst = (char **)cmm_malloc2d ( LLMXGT, DTTMSZ, sizeof(char), &ier);
	    if ( ier != 0 ) {
	        *iret = -73;
		return;
	    }
	    cgd_gtim ( ifn, LLMXGT, tmplst, &ntime, &ier );
	    for ( nt = 0; nt < ntime; nt++ ) {
		if ( strncmp ( tmplst[nt], cycle, 11 ) == 0 ) {
		    strcpy ( tfst, tmplst[nt] );
		    break;
		}
	    }

	    for ( nt = ntime - 1; nt >= 0; nt-- ) {
		if ( strncmp ( tmplst[nt], cycle, 11 ) == 0 ) {
		    strcpy ( tlst, tmplst[nt] );
		    break;
		}
	    }
	    cmm_free2d ( (void **)tmplst, &ier );
	}
    }

    return;
}

/*=====================================================================*/

static void kwrplc ( const char *tmin, char *tmout, int *iret )
/************************************************************************
 * kwrplc                                                               *
 *                                                                      *
 * This subroutine replaces key words in GDATTIM input with specific	*
 * time. Key words are FIRST, LAST, and ALL (XALL and ALLX ). ALL will	*
 * be replaced with a time range.					*
 *                                                                      *
 * kwrplc ( tmin, tmout, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *tmin		const char	Input time			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *tmout		char		Output time			*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -22 = invalid time              *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/04						*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 ************************************************************************/
{
    char tstart[DTTMSZ], tstop[DTTMSZ], tinc[6], temptm[48];
    int itype, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    strcpy ( temptm, tmin );

/*
 * Key words are invalid in dual time.
 */
    if ( strchr ( temptm, ':' ) ) {
	*iret = -22;
	return;
    }

/*
 * ALL is invalid in time range and by itself. In other cases,
 * ALL will be replaced with FIRST-LAST time range.
 */
    if ( strstr ( temptm, "ALL" ) ) {
	if ( strchr ( temptm, '-' ) || strcmp ( temptm, "ALL" ) == 0 ) {
	    *iret = -22;
	    return;
	} else {
	    cst_rpst ( temptm, "ALL", "FIRST", tstart, &ier );
	    cst_rpst ( temptm, "ALL", "LAST",  tstop,  &ier );
	    strcpy ( temptm, tstart );
	    strcat ( temptm, "-" );
	    strcat ( temptm, tstop );
	}
    }

    cst_rang ( temptm, tstart, tstop, tinc, &itype, &ier );
    if ( itype == 0 ) {
/*
 * No range. 
 */
	rpflst ( temptm, tmout, &ier );
    } else {
/*
 * Range with/without increment.
 */
	rpflst ( tstart, tstart, &ier );
	rpflst ( tstop,  tstop,  &ier );
	if ( itype == 1 ) {
/*
 * Without range.
 */
	    strcpy ( tmout, tstart );
	    strcat ( tmout, "-" );
	    strcat ( tmout, tstop );
	} else if ( itype == 2 ) {
/*
 * With range.
 */
	    strcpy ( tmout, tstart );
	    strcat ( tmout, "-" );
	    strcat ( tmout, tstop );
	    strcat ( tmout, "-" );
	    strcat ( tmout, tinc );
	}
    }

    return;
}

/*=====================================================================*/

static void rpflst ( const char *tmin, char *tmout, int *iret )
/************************************************************************
 * rpflst                                                               *
 *                                                                      *
 * This subroutine replaces the key words FIRST and LAST with specific	*
 * time.								*
 *                                                                      *
 * The meaning of key words FIRST and LAST is data type related and	*
 * position related.							*
 *                                                                      *
 * By data type related, for analysis data, FIRST means the first cycle	*
 * in the dataset, and LAST means the last cycle in the dataset; for	*
 * forecast data, FIRST means the first forecast hour for the given	*
 * cycle or the last cycle in the dataset, and LAST means the last	*
 * forecast hour for the given cycle or the last cycle in the dataset.	*
 *                                                                      *
 * By position related, if they appear at the forecast position		*
 * (general format is cycleFforcast), FIRST means the first forecast	*
 * hour for the given cycle or the last	cycle in the dataset, and LAST	*
 * means the last forecast hour for the	given cycle or the last cycle	*
 * in the dataset. If they appear at the cycle position, FIRST means	*
 * the first cycle in the dataset, and LAST means the last cycle in the	*
 * dataset.								*
 *                                                                      *
 * rpflst ( tmin, tmout, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *tmin		const char	Input time			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *tmout		char		Output time			*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -22 = invalid time              *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/04						*
 * R. Tian/SAIC		 1/06	Translated from Fortran			*
 ************************************************************************/
{
    char cycle[DTTMSZ], first[DTTMSZ], last[DTTMSZ], dumptm[DTTMSZ], rplc[DTTMSZ];
    int intdtf[3], isfcst, ipos1, ipos2, ier, ier1, ier2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    strcpy ( dumptm, tmin );

/*
 * Check data type for analysis data or forecast data.
 */
    tg_ctoi ( _dgfile.tlast[0], intdtf, &ier, strlen(_dgfile.tlast[0]) );
    if ( intdtf[2] == 0 ) {
	isfcst = G_FALSE;
    } else {
	isfcst = G_TRUE;
    }

/*
 * Replace FIRST.
 */
    cst_srch ( 0, strlen(dumptm), "FIRST", dumptm, &ipos1, &ier1 );
    cst_srch ( ipos1+1, strlen(dumptm), "FIRST", dumptm, &ipos2, &ier2 );
    if ( strcmp ( dumptm, "FIRST" ) == 0 ) {
	if ( isfcst == G_TRUE ) {
/*
 * FIRST for forecast data.
 */
	    cst_ncpy ( cycle, _dgfile.tlast[0], 11, &ier );
	} else {
/*
 * FIRST for analysis data.
 */
	    cycle[0] = '\0';
	}
	tmflst ( cycle, first, last, &ier );;
	strcpy ( dumptm, first );
    } else if ( ier1 == 0 && ier2 == 0 ) {
/*
 * FIRSTXFIRST.
 */
	cycle[0] = '\0';
	tmflst ( cycle, first, last, &ier );
	cst_ncpy ( rplc, first, 11, &ier );
	cst_rpst ( dumptm, "FIRST", rplc, dumptm, &ier );
	strcpy ( rplc, &first[12] );
	cst_rpst ( dumptm, "FIRST", rplc, dumptm, &ier );
    } else if ( ier1 == 0 && ipos1 == 0 ) {
/*
 * FIRSTX.
 */
	cycle[0] = '\0';
	tmflst ( cycle, first, last, &ier );
	cst_ncpy ( rplc, first, 11, &ier );
	cst_rpst ( dumptm, "FIRST", rplc, dumptm, &ier );
    } else if ( ier1 == 0 && ipos1 > 0 ) {
/*
 * XFIRST.
 */
	if ( ipos1 > 1 ) {
	    cst_ncpy ( rplc, dumptm, ipos1-1, &ier );
	    ctg_full ( rplc, _dgfile.tfirst[0], _dgfile.tlast[0],
	    	cycle, &ier );
	} else {
	    cst_ncpy ( cycle, _dgfile.tlast[0], 11, &ier );
	}
	tmflst ( cycle, first, last, &ier );
	strcpy ( rplc, &first[12] );
	cst_rpst ( dumptm, "FIRST", rplc, dumptm, &ier );
    }

/*
 * Replace LAST.
 */
    cst_srch ( 0, (int)strlen(dumptm), "LAST", dumptm, &ipos1, &ier1 );
    cst_srch ( ipos1+1, (int)strlen(dumptm), "LAST", dumptm, &ipos2, &ier2 );
    if ( strcmp ( dumptm, "LAST" ) == 0 ) {
	if ( isfcst == G_TRUE ) {
/*
 * LAST for forecast data.
 */
	    cst_ncpy ( cycle, _dgfile.tlast[0], 11, &ier );
	} else {
/*
 * LAST for analysis data.
 */
	    cycle[0] = '\0';
	}
	tmflst ( cycle, first, last, &ier );
	strcpy ( dumptm, last );
    } else if ( ier1 == 0 && ier2 == 0 ) {
/*
 * LASTXLAST.
 */
	cycle[0] = '\0';
	tmflst ( cycle, first, last, &ier );
	cst_ncpy ( rplc, last, 11, &ier );
	cst_rpst ( dumptm, "LAST", rplc, dumptm, &ier );
	strcpy ( rplc, &last[12] );
	cst_rpst ( dumptm, "LAST", rplc, dumptm, &ier );
    } else if ( ier1 == 0 && ipos1 == 0 ) {
/*
 * LASTX.
 */
	cycle[0] = '\0';
	tmflst ( cycle, first, last, &ier );
	cst_ncpy ( rplc, last, 11, &ier );
	cst_rpst ( dumptm, "LAST", rplc, dumptm, &ier );
    } else if ( ier1 == 0 && ipos1 > 0 ) {
/*
 * XLAST.
 */
	if ( ipos1 > 1 ) {
	    cst_ncpy ( rplc, dumptm, ipos1-1, &ier );
	    ctg_full ( rplc, _dgfile.tfirst[0], _dgfile.tlast[0],
	    	cycle, &ier );
	} else {
	    cst_ncpy ( cycle, _dgfile.tlast[0], 11, &ier );
	}
	tmflst ( cycle, first, last, &ier );
	strcpy ( rplc, &last[12] );
	cst_rpst ( dumptm, "LAST", rplc, dumptm, &ier );
    }

/*
 * Catch FIRSTF and LASTF.
 */
    if ( dumptm[strlen(dumptm)-1] == 'F' ) {
	*iret = -22;
	return;
    }

    strcpy ( tmout, dumptm );
    return;
}
