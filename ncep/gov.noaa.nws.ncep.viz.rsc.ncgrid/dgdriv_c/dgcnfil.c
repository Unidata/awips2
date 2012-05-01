#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

#include "dg.h"

void dgc_nfil ( const char *gdfile, const char *gdoutf, int *iret ) 
/************************************************************************
 * dgc_nfil                                                             *
 *                                                                      *
 * This subroutine opens grid files and initializes the grid            *
 * diagnostics package.							*
 *                                                                      *
 * dgc_nfil ( gdfile, dgoutf, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *gdfile		const char	Grid file name or template      *
 *      *gdoutf		const char	Output grid file name           *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -30 = error opening file        *
 *                                      -33 = too many files to open    *
 *                                      -51 = path associated with      *
 *                                            template does not exist	*
 *                                      -62 = grid file open failed     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/03						*
 * K. Brill/HPC		 1/04	Init time list if same GDFILE input	*
 * R. Tian/SAIC		 2/03	Removed gdfprv and gdoprv		*
 * K. Brill/HPC		 2/04	Remove setting dgsubg = .true.; add	*
 *				setting nucode = .true.			*
 * R. Tian/SAIC          3/04   Modified to use new GD file management  *
 * T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
 * T. Lee/SAIC		10/04	Fixed return code			*
 * R. Tian/SAIC		10/04	Changed template tfirst/tlast compu	*
 * A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
 * m.gamazaychikov/SAIC	12/04	Added ion flag to CTB_DTGET call seq.	*
 * T. Lee/SAIC		12/04	Added ensemble DE_ functions		*
 * R. Tian/SAIC		 1/06	Recoded from Fortran			*
 * m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
 * R. Tian/SAIC		07/06	Fixed some error processing		*
 * R. Tian/SAIC		01/07	Fixed bug when template w/o base time	*
 * S. Gilbert/NCEP	02/07	Increased size of filnms                *
 * T. Piper/SAIC        04/07   Modified for cfl_scnt CSC               *
 * M. Li/SAIC		10/07   Add check for files found after cfl_scnt*
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTG_DTGET CSC*
 ***********************************************************************/
{
    char tmpfil[MXFLSZ], filnam[MXFLSZ], cpath[MXFLSZ], tmplt[65],
         dumdtm[DTTMSZ], rplc[DTTMSZ];
    char **filnms, **carr=NULL;
    float adum1, adum2;
    long flen;
    int nfile, ifn, mxgd, ngrd, ncarr, ic, is, iff, ir, iint, ion, ihb,
        mnb, iha, mna, idtmch, mstrct, ii, im1, kk,
	zero, ier, ierm;
    int exist;
    struct dirent **dnlist=NULL;
    char     diagMessage[720],**fhrs,*def = " ",ingdtm[41];
    int nfarr;
/*----------------------------------------------------------------------*/
    *iret   = 0;
    zero = 0;
    _nfile.nucode = G_TRUE;
    sprintf (diagMessage, "%s%s", "gdfile= ", gdfile);
    db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);

/*
 * Close any previously opened file.
 */
    for ( ii = 1; ii <= NGDFLS; ii++ ) {
	dg_clos ( &ii, &ier );
    }

/*
 * Initialize the ensemble common block.
 */
    de_init ( &ier );

/*
 * Initialize /NFILE/ common block.
 */
    for ( ii = 0; ii < NGDFLS; ii++ ) {
	_nfile.ntmplt[ii][0] = '\0';
	_nfile.gflpth[ii][0] = '\0';
	_nfile.crtfnm[ii][0] = '\0';
	_nfile.aftrbr[ii][0] = '\0';
	_nfile.crtgdt1[ii][0] = '\0';
	_nfile.crtgdt2[ii][0] = '\0';
	_nfile.outflg[ii] = G_FALSE;
	_nfile.mbrnum[ii] = 0;
    }
    for ( ii = 0; ii < LLMXGT; ii++ ) {
	_nfile.dtmlst1[ii][0] = '\0';
	_nfile.dtmlst2[ii][0] = '\0';
    }
    _nfile.ntmlst = 0;
    _nfile.itmlst = 0;
    _nfile.irefnv = 0;

/*
 * Get the individual input file names.
 */
    filnms = (char **)cmm_malloc2d ( NGDFLS, /*LLMXLN*/LLNNAV+1, sizeof(char), &ier );
    if ( filnms == NULL ) {
        *iret = -73;
	return;
    }
    cst_clst ( (char *)gdfile, '+', " ", NGDFLS, /*LLMXLN*/ LLNNAV, filnms, &nfile, &ier );
    if ( nfile > NGDFLS || ier != 0 ) {
	cmm_free2d( (void **)filnms, &ier );
	*iret = -33;
	return;
    }
/*
 * Check for the output file to be included among the input files.
 */
    if ( strlen ( gdoutf ) > 0 ) {
	exist = G_FALSE;
	for ( ii = 1; ii <= nfile; ii++ ) {
	    im1 = ii - 1;
	    if ( strcmp ( gdoutf, filnms[im1] ) == 0 ) {
		_nfile.outflg[im1] = G_TRUE;
		_nfile.irefnv = im1;
		exist = G_TRUE;
	    }
	}
	if ( exist == G_FALSE ) {
	    nfile += 1;
	    if ( nfile > NGDFLS ) {
		cmm_free2d( (void **)filnms, &ier );
		*iret = -33;
		return;
	    }
	    strcpy ( filnms[nfile-1], gdoutf );
	    _nfile.outflg[nfile-1] = G_TRUE;
	    _nfile.irefnv = nfile - 1;
	}
    }

/*
 * Loop over all of the entries
 */
    for ( ii = 1; ii <= nfile; ii++ ) {
        im1 = ii  - 1;
      //  printf ("DGC_NFIL ii=%d filnms = %s\n", ii, filnms[im1]);
        sprintf (diagMessage, "%s", "start the loop over entries");
        db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
        sprintf (diagMessage, "%s %d %s %s", "calling de_mbr1 with ii=", ii, "filnms[im1]= ", filnms[im1]);
        db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	de_mbr1 ( &ii, filnms[im1], tmpfil, &ier );
      //  printf ("DGC_NFIL after de_mbr1 tmpfil = %s ier = %d\n", tmpfil, &ier);
        sprintf (diagMessage, "%s %d %s %s", "after de_mbr1 ier=", ier, "tmpfil= ", tmpfil);
        db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	cfl_inqr ( tmpfil, NULL, &flen, filnam, &ier );
      //  printf ("DGC_NFIL after cfl_inqr filnam = %s ier = %d\n", filnam, &ier);
        sprintf (diagMessage, "%s %d %s %s", "after cfl_inqr ier=", ier, "filnam= ", filnam);
        db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);

	if ( ier == 0) {
/*
 * This entry is an actual file.
 */
            sprintf (diagMessage, "%s", "this entry is a file");
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    strcpy ( _nfile.crtfnm[im1], filnam );
          //  printf ("DGC_NFIL calling gd_open\n");
            sprintf (diagMessage, "%s", "calling gd_open");
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    gd_open ( filnam, &_nfile.outflg[im1], &zero, &zero, &ifn, &adum1,
	              &adum2, &mxgd, &ier, strlen(filnam) );
          //  printf ("DGC_NFIL after gd_open ier=%d\n", &ier);
            sprintf (diagMessage, "%s %d", "after gd_open ier=", ier);
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    if ( ier != 0 ) {
		cmm_free2d( (void **)filnms, &ier );
		*iret = -62;
		return;
	    }
          //  printf ("DGC_NFIL calling cgd_ngrd\n");
            sprintf (diagMessage, "%s", "calling cgd_ngrd");
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    cgd_ngrd ( ifn, &ngrd, _dgfile.tfirst[im1], _dgfile.tlast[im1],
	               &ier );
            sprintf (diagMessage, "%s %d", "after cgd_ngrd ier=", ier);
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    if ( ier != 0 ) {
		cmm_free2d( (void **)filnms, &ier );
		*iret = -30;
		return;
	    }

	    _dgfile.tmpflg[im1] = G_FALSE;
	    _dgfile.templt[im1][0] = '\0';
	} else {
/*
 * This entry is a template.
 */
            sprintf (diagMessage, "%s", "this entry is a TEMPLATE");
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    carr = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
	    if ( carr == NULL ) {
		cmm_free2d( (void **)filnms, &ier );
		*iret = -73;
		return;
	    }
	    cst_clst ( tmpfil, '|', " ", 2, MXFLSZ, carr, &ncarr, &ier );
	    strcpy ( tmpfil, carr[0] );
	    if ( ncarr == 2 ) {
		strcpy ( _nfile.aftrbr[im1], carr[1] );
	    }
	    cmm_free2d( (void **)carr, &ier );

	    exist = G_TRUE;
            sprintf (diagMessage, "%s %s", "before ctb_dtget tmpfil=", tmpfil);
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    ctb_dtget ( tmpfil, cpath, tmplt, &ic, &is, &iff, &ir, &iint, 
	        &ion, &ihb, &mnb, &iha, &mna, &mstrct, &idtmch, &ier );
          //  printf ("DGC_NFIL after ctb_dtget cpath = %s\n", cpath);
          //  printf ("DGC_NFIL after ctb_dtget tmplt= %s\n", tmplt);
          //  printf ("DGC_NFIL after ctb_dtget ier = %d\n", &ier);
            sprintf (diagMessage, "%s %s %s %s %s %d", "after ctb_dtget cpath=", cpath, " tmplt=", tmplt, " ier=", ier);
            db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	    if ( ier != 0 ) exist = G_FALSE;
            if (  ( strstr ( cpath, "A2DB" ) ) == NULL) {
               cfl_inqr ( cpath, NULL, &flen, filnam, &ier );
	       if ( ier != 0 ) exist = G_FALSE;
            }
            else {
            /*
             * change for the ensemble stuff
             */
               strcpy ( filnam,cpath);   
            }
	    if ( exist == G_FALSE ) { 
		cmm_free2d( (void **)filnms, &ier );
		*iret = -51;
		return;
	    }

/*
 * First, check if templt contains base time pattern. If not,
 * it is a file name.
 */
	    if ( ! ( strstr ( tmplt, "YY" ) && strstr ( tmplt, "DD" ) ) ) {
              //  printf ("DGC_NFIL it is a FILENAME\n");
                sprintf (diagMessage, "%s", "it is a FILENAME");
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
		strcpy ( _nfile.crtfnm[im1], cpath );
		strcat ( _nfile.crtfnm[im1], "/" );
		strcat ( _nfile.crtfnm[im1], tmplt );
              //  printf ("DGC_NFIL calling gd_open \n");
                sprintf (diagMessage, "%s", "calling gd_open");
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
		gd_open ( _nfile.crtfnm[im1], &_nfile.outflg[im1], &zero,
		    &zero, &ifn, &adum1, &adum2, &mxgd, &ier,
		    strlen(_nfile.crtfnm[im1]) );
              //  printf ("DGC_NFIL after gd_open ier=%d\n", &ier);
                sprintf (diagMessage, "%s %d", "after gd_open ier=", ier);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                if ( ier != 0 ) {
                    cmm_free2d( (void **)filnms, &ier );
                    *iret = -62;
                    return;
                }
              //  printf ("DGC_NFIL calling cgd_ngrd\n");
                sprintf (diagMessage, "%s", "calling cgd_ngrd");
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                cgd_ngrd ( ifn, &ngrd, _dgfile.tfirst[im1], _dgfile.tlast[im1],
                           &ier );
              //  printf ("DGC_NFIL after cgd_ngrd ier=%d\n", &ier);
                sprintf (diagMessage, "%s %d", "after cgd_ngrd ier=", ier);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                if ( ier != 0 ) {
                    cmm_free2d( (void **)filnms, &ier );
                    *iret = -30;
                    return;
                }
                _dgfile.tmpflg[im1] = G_FALSE;
                _dgfile.templt[im1][0] = '\0';
	    } else {
              //  printf ("DGC_NFIL it is a TEMPLATE\n");
                sprintf (diagMessage, "%s", "it is a TEMPLATE");
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	        strcpy ( _nfile.ntmplt[im1], tmplt );
	        strcpy ( _nfile.gflpth[im1], filnam );
	        strcpy ( _dgfile.templt[im1], tmplt );
	        _dgfile.tmpflg[im1] = G_TRUE;

/*
 * Get the first and the last time for the given cycle if
 * user specifies a cycle in the GDFILE input, or for the
 * last cycle in the dataset if user does not specify a 
 * cycle in the GDFILE input.
 */
               if ( _nfile.aftrbr[im1][0] != '\0' ) {
                    cti_stan ( _nfile.aftrbr[im1], "YYMMDD/HHNN", dumdtm,
                        &ier );
                    if ( strstr ( tmplt, "YYYY" ) ) {
                        strcpy ( rplc, "YY" );
                        strncat ( rplc, dumdtm, 6 );
                        rplc[8] = '\0';
                        cst_rpst ( tmplt, "YYYYMMDD", rplc, tmplt, &ier );
                    } else {
                        cst_ncpy ( rplc, dumdtm, 6, &ier );
                        cst_rpst ( tmplt, "YYMMDD", rplc, tmplt, &ier );
                    }
                    cst_ncpy ( rplc, &dumdtm[7], 2, &ier );
                    cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
                    dg_cget ( "INGDTM", ingdtm, &ier );
               }
               else {
                    if ( fhrsClbkPtr != NULL ) {
                  	fhrsClbkPtr();
              	    }

                   sprintf (diagMessage, "%s %s", "Forecast Hour fhrsStrBack=", fhrsStrBack);
                   db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                   fhrs = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
                   cst_clst ( fhrsStrBack, '/', def, 2, MXFLSZ, fhrs, &nfarr, &ier );
                   if ( strstr ( tmplt, "YYYY" ) ) {
                       strcpy ( rplc, "YY" );
                       strncat ( rplc, fhrs[0], 6 );
                       rplc[8] = '\0';
                       cst_rpst ( tmplt, "YYYYMMDD", rplc, tmplt, &ier );
                    } else {
                       cst_ncpy ( rplc, fhrs[0], 6, &ier );
                       cst_rpst ( tmplt, "YYMMDD", rplc, tmplt, &ier );
                    }
                    cst_ncpy ( rplc, fhrs[1], 2, &ier );
                    cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
                    cst_clst ( fhrsStrBack, 'f', def, 2, MXFLSZ, fhrs, &nfarr, &ier );
                    cst_ncpy ( rplc, fhrs[1], 3, &ier );
                    cst_rpst ( tmplt, "FFF", rplc, tmplt, &ier );
                    cmm_free2d((void **)fhrs,&ier);

              }
/*
 * Search the last file that matches the partially filled
 * template, and open it to get the last time.
 */
                sprintf (diagMessage, "%s %s", "calling cfl_scnt with _nfile.gflpth[im1]=", _nfile.gflpth[im1]);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                sprintf (diagMessage, "%s %s", "calling cfl_scnt with tmplt=", tmplt);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	        cfl_scnt ( _nfile.gflpth[im1], tmplt, -1, &dnlist, &ncarr,
		    &ier );
                //sprintf (diagMessage, "%s %s", "after cfl_scnt dnlist[0]->d_name=", dnlist[0]->d_name);
                //db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                sprintf (diagMessage, "%s %d", "after cfl_scnt ncarr=", ncarr);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
		tmpfil[0] = '\0';
	        if (ncarr > 0 ) strcpy ( tmpfil, dnlist[0]->d_name );
		for (kk=0;kk<ncarr;kk++){
		    free(dnlist[kk]);
		}
		if ( dnlist != NULL ) free(dnlist);
	        strcpy ( filnam, _nfile.gflpth[im1] );
	        strcat ( filnam, "/" );
	        strcat ( filnam, tmpfil );
                sprintf (diagMessage, "%s %s", "calling gd_open with filnam=", filnam);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	        gd_open ( filnam, &_nfile.outflg[im1], &zero, &zero, &ifn,
		    &adum1, &adum2, &mxgd, &ier, strlen(filnam) );
                sprintf (diagMessage, "%s %d", "after gd_open ier=", ier );
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	        if ( ier != 0 ) {
		    cmm_free2d( (void **)filnms, &ier );
		    *iret = -62;
		    return;
	        }
	        cgd_ngrd ( ifn, &ngrd, dumdtm, _dgfile.tlast[im1], &ier );

/*
 * Search the first file that has the same cycle with 
 * the last file that matches the partially filled template,
 * and open it to get the first time.
 */
/*	        startp = strstr ( _nfile.ntmplt[im1], "YYYY" );
	        if ( ! startp ) startp = strstr ( _nfile.ntmplt[im1], "YY" );
	        endp = strstr ( _nfile.ntmplt[im1], "HH" );
	        if ( ! endp ) endp = strstr ( _nfile.ntmplt[im1], "DD" );

	        istart = (int)( startp - _nfile.ntmplt[im1] );
	        iend = (int)( endp - _nfile.ntmplt[im1] ) + 2;
	        for ( jj = 0; jj < istart; jj++ ) {
		    tmplt[jj] = _nfile.ntmplt[im1][jj];
	        }
	        for ( jj = istart; jj < iend; jj++ ) {
		    tmplt[jj] = tmpfil[jj];
	        }
	        for ( jj = iend; jj < (int)strlen(_nfile.ntmplt[im1]); jj++ ) {
		    tmplt[jj] = _nfile.ntmplt[im1][jj];
	        }
	        tmplt[strlen(_nfile.ntmplt[im1])] = '\0';
*/
                sprintf (diagMessage, "%s %s", "calling cfl_scnt with _nfile.gflpth[im1]=", _nfile.gflpth[im1]);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                sprintf (diagMessage, "%s %s", "calling cfl_scnt with tmplt=", tmplt);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	        cfl_scnt ( _nfile.gflpth[im1], tmplt, 1, &dnlist, &ncarr,
		    &ier );
                //sprintf (diagMessage, "%s %s", "after cfl_scnt dnlist[0]->d_name=", dnlist[0]->d_name);
                //db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
                sprintf (diagMessage, "%s %d", "after cfl_scnt ncarr=", ncarr);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
		tmpfil[0] = '\0';
	        if (ncarr > 0 ) strcpy ( tmpfil, dnlist[0]->d_name );
                for (kk=0;kk<ncarr;kk++){
                    free(dnlist[kk]);
                }
                if ( dnlist != NULL ) free(dnlist);
	        strcpy ( filnam, _nfile.gflpth[im1] );
	        strcat ( filnam, "/" );
	        strcat ( filnam, tmpfil );
                sprintf (diagMessage, "%s %s", "calling gd_open with filnam=", filnam);
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	        gd_open ( filnam, &_nfile.outflg[im1], &zero, &zero, &ifn,
		    &adum1, &adum2, &mxgd, &ier, strlen(filnam) );
                sprintf (diagMessage, "%s %d", "after gd_open ier=", ier );
                db_msgcave ("dgc_nfil", "debug", diagMessage, &ierm);
	        if ( ier != 0 ) {
		    cmm_free2d( (void **)filnms, &ier );
		    *iret = -62;
		   return;
	        }
	        cgd_ngrd ( ifn, &ngrd, _dgfile.tfirst[im1], dumdtm, &ier );
	    }
	}
	_dgfile.idflnm[im1] = ii;
    }

    cmm_free2d( (void **)filnms, &ier );
    return;
}
