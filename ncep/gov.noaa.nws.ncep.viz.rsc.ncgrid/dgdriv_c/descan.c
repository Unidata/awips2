#include "ensdiag.h"

#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void de_scan ( const int *nina, int *iret )
/************************************************************************
 * de_scan								*
 *									*
 * This subroutine scans the ALLARG array in DECMN.CMN to determine 	*
 * which GDFILE entry has the required ensemble specification.  The 	*
 * allarg array is filled in an ENS_ function subroutine by calling	*
 * ST_CLST to parse out the input arguments before calling DE_SCAN.	*
 * Having found the ensemble, DE_SCAN builds the list of file names	*
 * or paths and templates to process and the time stamp to use for	*
 * each one.								*
 *									*
 * de_scan ( nian, iret )						*
 *									*
 * Input and Output parameters:						*
 *	*nina		const int	Number of input argument	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = more than one ens. file 	*
 *					 -2 = file, template not exist	*
 *					 -3 = invalid forecast time	*
 *					 -5 = no ensemble file referred	*
 *					 -7 = too many ensemble members	*
 *					-11 = Invalid weight entry	*	
 *					-12 = Sum of weights > 100.	*
 *					-13 = Sum of weights < 100.	*
 *					-14 = Ensemble has zero members	* 
 **									*
 * Log:									*
 * T. Lee/SAIC		01/05						*
 * T. Lee/SAIC		06/05	Added weighting factors			*
 * R. Tian/SAIC		12/05	Translated from Fortran			*
 * m.gamazaychikov/SAIC 05/06   Added idtmch flag to CTB_DTGET CS       *
 * T. Piper/SAIC	0806	Added G_DIFF				*
 * T. Piper/SAIC        04/07   Modified for cfl_scnt CSC               *
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
 ***********************************************************************/
{
    char path[MXFLSZ], fpath[MXFLSZ], lmem[MXFLSZ], alias[MXFLSZ],
	 newfil[MXFLSZ], outfl[MXFLSZ], tmplt[MXTMPL], tplate[MXTMPL],
	 cycle[MXTMPL], dum[MXTMPL];
    char rplc[49], strmis[30], ingdtm[41], fff[4], wtmp[11], cent[3];
    char etm1[DTTMSZ], etm2[DTTMSZ], itm[DTTMSZ], ltm1[DTTMSZ], ltm2[DTTMSZ],
	 tfirst[DTTMSZ], tlast[DTTMSZ], vtm1[DTTMSZ], vtm2[DTTMSZ];
    char *def = " ", *starp, *chrptr;
    char **entry, **lmemf=NULL;
    struct dirent **dnlist=NULL;
    float wgtsum, wgt, wtm, wresid, wtprm, ewgts[MXMBRS];
    long lens;
    int nfpn, ifpn[NGDFLS], memap[MXMBRS], nen, num, istar,
        nfile, nf, last;
    int ic, is, iif, ir, ino, ihb, mnb, intrvl, iha, mna, mstrct, idtmch,
	kneg, kmem;
    int ii, jj, kk, ll, ier, icode, len;
    int ierm,nfarr;
    char     diagMessage[720],tmpWt[20],**fhrs;
/*----------------------------------------------------------------------*/
    *iret = 0;
    nfpn = 0;
    _ensdiag.ndxens = 0;

/*
 * Initialize file position numbers.
 */
    for ( ii = 0; ii < NGDFLS; ii++ ) {
        ifpn[ii] = 0;
    }

/*
 * Get user input of file position numbers.
 */
    for ( ii = 0; ii < *nina; ii++ ) {
	de_pfpn ( _ensdiag.allarg[ii], ifpn, &nfpn, iret );
    }

/*
 * Check if each ensemble diagnostic only points to one ensemble file.
 */
    for ( ii = 0; ii < nfpn; ii++ ) {
	if ( ifpn[ii] != _ensdiag.ndxens ) {
	    if ( _ensdiag.ensspc[ifpn[ii]-1][0] != '\0' ) {
		if (  _ensdiag.ndxens == 0 ) {
		     _ensdiag.ndxens = ifpn[ii];
		} else {
		    *iret = -1;
		    return;
		}
	     }
	}
    }

/*
 * If no appropriate file position number is found in the arguments 
 * (ndxens = 0), return.
 */
    if (  _ensdiag.ndxens == 0 ) {
	*iret = -5;
	return;
    }

/*
 * Initialize ensemble member arrays.
 */
    for ( ii = 0; ii < MXMBRS; ii++ ) {
	 _ensdiag.etmplt[ii][0] = '\0';
	 _ensdiag.enspth[ii][0] = '\0';
	 _ensdiag.ensfnm[ii][0] = '\0';
	 _ensdiag.etimes[ii][0] = '\0';
    }

/*
 * Get the governing valid time stamp.
 */
    dgc_qdtm ( &_ensdiag.ndxens, tfirst, tlast, &ier );
 //   printf("++++++  de_scan after dgc_qdtm tfirst=%s tlast=%s ier=%d\n", tfirst, tlast, ier);
    sprintf (diagMessage, "%s %s %s %s %s %d", "after dgc_qdtm tfirst= ", tfirst, "tlast=", tlast, "ier=", ier);
    db_msgcave ("de_scan", "debug", diagMessage, &ierm);
    dg_cget ( "INGDTM", ingdtm, &ier );
 //   printf("++++++  de_scan after dg_cget ingdtm=%s ier=%d\n", ingdtm, ier);
    sprintf (diagMessage, "%s %s %s %d", "after  dg_cget ingdtm=", ingdtm, "ier=", ier);
    db_msgcave ("de_scan", "debug", diagMessage, &ierm);
    grc_gtim ( ingdtm, tfirst, tlast, ltm1, ltm2, &ier );
 //   printf("++++++  de_scan after grc_gtim ltm1=%s ltm2=%s ier=%d\n", ltm1, ltm2, ier);
    sprintf (diagMessage, "%s %s %s %s %s %d", "after grc_gtim ltm1= ", ltm1, "ltm2=", ltm2, "ier=", ier);
    db_msgcave ("de_scan", "debug", diagMessage, &ierm);
    ctg_vald ( ltm1, vtm1, &ier );
 //   printf("++++++  de_scan after ctg_vald vtm1=%s ier=%d\n", vtm1, ier);
    sprintf (diagMessage, "%s %s %s %d", "after ctg_vald vtm1=", vtm1, "ier=", ier);
    db_msgcave ("de_scan", "debug", diagMessage, &ierm);
    chrptr = strchr ( vtm1, 'V' );
    if ( chrptr ) *chrptr = '\0';
    if ( ltm2[0] != '\0' ) {
        ctg_vald ( ltm2, vtm2, &ier );
	chrptr = strchr ( vtm2, 'V' );
	if ( chrptr ) *chrptr = '\0';
    } else {
        vtm2[0] = '\0';
    }

/*
 * Build the list of ensemble member file names or paths and 
 * templates.  The entries inside {} may be file names or aliases 
 * with attached cycle times following |. 
 */
    strcpy ( lmem, _ensdiag.ensspc[_ensdiag.ndxens-1] );
  //   printf("++++++  de_scan lmem=%s\n", lmem);
    sprintf (diagMessage, "%s %s", "lmem=", lmem);
    db_msgcave ("de_scan", "debug", diagMessage, &ierm);
    entry = (char **)cmm_malloc2d ( MAXENT, MXFLSZ, sizeof(char), &ier );
    cst_clst ( lmem, ',', def, MAXENT, MXFLSZ, entry, &nen, &ier );
    _ensdiag.nummbr = 0;
    wgtsum = 0.;
 //   printf("++++++  de_scan after cst_clst nen=%d\n", nen);
    sprintf (diagMessage, "%s %d", "after cst_clst nen=", nen);
    db_msgcave ("de_scan", "debug", diagMessage, &ierm);
    for ( ii = 0; ii < nen; ii++ ) {
     //   printf("++++++  de_scan  _ensdiag.nummbr=%d entry[%d]=%s\n",  _ensdiag.nummbr, ii, entry[ii]);
        sprintf (diagMessage, "%s%d %s%d%s%s", "_ensdiag.nummbr=",_ensdiag.nummbr, "entry[", ii, "]=", entry[ii]);
        db_msgcave ("de_scan", "debug", diagMessage, &ierm);
	if ( strchr ( entry[ii], '%' ) ) {
	    lmemf = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
	    cst_rlch ( RMISSD, 1, strmis, &ier );
	    cst_clst ( entry[ii], '%', strmis, 2, MXFLSZ, lmemf, &num, &ier );
            strcpy ( tmpWt, lmemf[0]); 
            if ( ( strstr ( entry[ii], "A2DB" ) ) != NULL) {
               strcpy (tmpWt, &lmemf[0][5]);
               strcpy ( lmem,"A2DB_");
               strcat ( lmem, lmemf[1] );
            }
            else {
                strcpy ( lmem, lmemf[1] );
            }
            cst_crnm ( tmpWt, &wgt, &ier );
	    cmm_free2d ( (void **)lmemf, &ier );

	    if ( ERMISS ( wgt ) || wgt < 0.0F || wgt > 100.0F ) {
		*iret = -11;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }
	    ewgts[ii] = wgt;
	    wgtsum += wgt;
	    if ( wgtsum > 100.0F ) {
		*iret = -12;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }
	} else {
	    strcpy ( lmem, entry[ii] );
	    ewgts[ii] = -1.0;
	}

	cfl_inqr ( lmem, NULL, &lens, newfil, &ier );
     //   printf("++++++  de_scan after cfl_inqr for lmem=%s ier=%d\n", lmem, ier);
        sprintf (diagMessage, "%s%s%s%d", "after cfl_inqr for lmem=", lmem, " ier=", ier);
        db_msgcave ("de_scan", "debug", diagMessage, &ierm);
	if ( ier == 0 ) {
	    memap[_ensdiag.nummbr] = ii;
	    strcpy ( _ensdiag.ensfnm[_ensdiag.nummbr], newfil );
            _ensdiag.etmplt[_ensdiag.nummbr][0] = '\0';
            _ensdiag.enspth[_ensdiag.nummbr][0] = '\0'; 
            strcpy ( _ensdiag.etimes[_ensdiag.nummbr], ingdtm );
	    _ensdiag.nummbr += 1;
	    if (  _ensdiag.nummbr > MXMBRS - 1 ) {
	        *iret = -7;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }
	} else {
	    lmemf = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
	    cst_clst ( lmem, '|', def, 2, MXFLSZ, lmemf, &num, &ier );
	    strcpy ( alias, lmemf[0] );
	    strcpy ( cycle, lmemf[1] );
	    cmm_free2d ( (void **)lmemf, &ier );
         //   printf("++++++  de_scan before ctb_dtget cycle=%s\n", cycle);
         //   printf("++++++  de_scan before ctb_dtget alias=%s\n", alias);
            sprintf (diagMessage, "%s%s%s%s", "before ctb_dtget cycle=", cycle," alias=", alias);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
	    ctb_dtget ( alias, path, tmplt, &ic, &is, &iif, &ir, &intrvl,
	        &ino, &ihb, &mnb, &iha, &mna, &mstrct, &idtmch, &ier );
            sprintf (diagMessage, "%s %s %s %s %s %d", "after ctb_dtget path= ", path, "tmplt=", tmplt, "ier=", ier);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
         //   printf("++++++  de_scan after ctb_dtget path=%s\n", path);
         //   printf("++++++  de_scan after ctb_dtget tmplt=%s\n", tmplt);
         //   printf("++++++  de_scan after ctb_dtget ier=%d\n", ier);
            if ( ier != 0 ) {
                cmm_free2d ( (void **)entry, &ier );
                icode = -15;
                er_wmsg ( "FL", &icode, lmem, &ier, strlen("FL"), strlen(lmem) );
                *iret = -2;
                return;
            }
            if (  ( strstr ( path, "A2DB" ) ) == NULL) {
	       cfl_inqr ( path, NULL, &lens, fpath, &ier );
            //   printf("++++++  de_scan after cfl_inqr ier=%d\n", ier);
            }
            else {
	       strcpy ( fpath, path );
            } 
	    if ( ier != 0 ) {
		cmm_free2d ( (void **)entry, &ier );
		icode = -1;
		er_wmsg ( "FL", &icode, lmem, &ier, strlen("FL"), strlen(lmem) );
		*iret = -2;
		return;
	    }

	    strcpy ( tplate, tmplt );
             if ( strcmp ( cycle, def ) != 0 ) {
         //   printf("++++++  de_scan changing the tmplt from %s\n", tmplt);
                sprintf (diagMessage, "%s%s", "changing the tmplt from ", tmplt);
                db_msgcave ("de_scan", "debug", diagMessage, &ierm);
                cti_stan ( cycle, "YYMMDD/HHNN", dum, &ier );
                if ( strstr ( tmplt, "YYYYMMDD" ) ) {
                    strcpy ( rplc, "YY" );
                    strncat ( rplc, dum, 6 );
                    rplc[8] = '\0';
                    cst_rpst ( tmplt, "YYYYMMDD", rplc, tmplt, &ier );
                } else {
                    cst_ncpy ( rplc, dum, 6, &ier );
                    cst_rpst ( tmplt, "YYMMDD", rplc, tmplt, &ier );
                }
                cst_ncpy ( rplc, &dum[7], 2, &ier );
                cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
                sprintf (diagMessage, "%s%s", "to ", tmplt);
                db_msgcave ("de_scan", "debug", diagMessage, &ierm);
            }
            else if ( ingdtm[0] != '\0' ) {
                sprintf (diagMessage, "%s %s", "Forecast Hour ingdtm=", ingdtm);
                db_msgcave ("de_scan", "debug", diagMessage, &ierm);
                fhrs = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
                cst_clst ( ingdtm, '/', def, 2, MXFLSZ, fhrs, &nfarr, &ier );
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
                 cst_clst ( ingdtm, 'F', def, 2, MXFLSZ, fhrs, &nfarr, &ier );
                 cst_ncpy ( rplc, fhrs[1], 3, &ier );
                 cst_rpst ( tmplt, "FFF", rplc, tmplt, &ier );
                 cmm_free2d((void **)fhrs,&ier);
            }
         //   printf("++++++  de_scan before cfl_scnt fpath=%s tmplt=%s ier=%d\n", fpath, tmplt, ier);
            sprintf (diagMessage, "%s %s %s %s %s %d", "before cfl_scnt fpath= ", fpath, "tmplt=", tmplt, "ier=", ier);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
	    cfl_scnt ( fpath, tmplt, -1, &dnlist, &nfile, &ier );
            sprintf (diagMessage, "%s%d%s%d", "after cfl_scnt ier=", ier, " nfile=", nfile);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
         //   printf("++++++  de_scan after cfl_scnt ier=%d\n", ier);
         //   printf("++++++  de_scan after cfl_scnt nfile=%d\n", nfile);
	    if ( nfile == 0 ) {
		icode = -1;
		er_wmsg ( "FL", &icode, lmem, &ier, strlen("FL"), strlen(lmem) );
		*iret = -2;
		return;
	    }
	    strcpy ( outfl, dnlist[0]->d_name );
	    for (ll=0; ll<nfile;ll++){
		free ( dnlist[ll] );
	    }
	    if (dnlist != NULL ) free(dnlist);

/*
 * Extract the YYYYMMDDHH information from the name of
 * the file. Subtiitute into the template to create a
 * working template (wtmp in the form of YYYYMMDDHH).
 */
/*
  TODO - remove the line below
            sprintf (tplate, "%s", "YYYYMMDD_HHfFFF");
*/
         //   printf("++++++  de_scan before cfl_mdat outfl=%s\n", outfl);
         //   printf("++++++  de_scan before cfl_mdat tplate=%s\n", tplate);
            sprintf (diagMessage, "%s %s %s %s", "before cfl_mdat outfl= ", outfl, "tplate=", tplate);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
	    cfl_mdat ( outfl, tplate, "YYMMDD/HH00", itm, &ier );
            sprintf (diagMessage, "%s %s %s %d", "after cfl_mdat itm= ", itm, "ier=", ier);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
            //sprintf (itm, "%s", "090701/1300");
            //printf("++++++  de_scan before cfl_mdat outfl=%s\n", outfl);
            //printf("++++++  de_scan before cfl_mdat tplate=%s\n", tplate);
         //   printf("++++++  de_scan after cfl_mdat itm=%s\n", itm);
         //   printf("++++++  de_scan after cfl_mdat ier=%d\n", ier);
	    strcpy ( &itm[9], "00" );
	    if ( strstr ( tplate, "YYYY" ) ) {
		cti_ccnt ( itm, cent, &ier );
		strcpy ( wtmp, cent );
		strncpy ( &wtmp[2], itm, 6 );
		strncpy ( &wtmp[8], &itm[7], 2 );
		wtmp[10] = '\0';
	    } else {
		strncpy ( wtmp, itm, 6 );
		strncpy ( &wtmp[6], &itm[7], 2 );
		wtmp[8] = '\0';
	    }

/*
 * Use the valid time and initial time to construct the
 * forecast time.
 */
         //   printf("++++++  de_scan before ctg_vi2f vtm1=%s itm=%s \n", vtm1, itm);
            sprintf (diagMessage, "%s %s %s %s", "before ctg_vi2f vtm1= ", vtm1, "itm=", itm);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
            ctg_vi2f ( vtm1, itm, etm1, &ier );
         //   printf("++++++  de_scan after ctg_vi2f etm1=%s ier=%d \n", etm1, ier);
            sprintf (diagMessage, "%s %s %s %d", "after ctg_vi2f etm1=", etm1, "ier=", ier);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
            if ( ier != 0 ) {
		*iret = -3;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }

/*
 * Get the FFF part of the date-time stamp.
 */
	    chrptr = strchr ( etm1, 'F' );
	    if ( chrptr ) {
		cst_ncpy ( fff, chrptr+1, 3, &ier );
	    } else {
		strcpy ( fff, "000" );	
	    }

         //   printf("++++++  de_scan fff=%s  \n", fff);
            sprintf (diagMessage, "%s %s", "fff=", fff);
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
            if ( vtm2[0] != '\0' ) {
                ctg_vi2f ( vtm2, itm, etm2, &ier );
                if ( ier != 0 ) {
		    *iret = -3;
		    cmm_free2d ( (void **)entry, &ier );
		    return;
		}
            } else {
                etm2[0] = '\0';
            }

/*
 * Check single v.s. multiple forecast files.
 */
	    starp = strchr ( tplate, '*' );
            if ( ! starp ) {
         //   printf("++++++  de_scan Check single v.s. multiple forecast files. => no star\n");
            sprintf (diagMessage, "%s", "Check single v.s. multiple forecast files. => no star");
            db_msgcave ("de_scan", "debug", diagMessage, &ierm);
                
                memap[_ensdiag.nummbr] = ii;
                _ensdiag.ensfnm[_ensdiag.nummbr][0] = '\0';
                strcpy ( _ensdiag.enspth[_ensdiag.nummbr], fpath );
                strcpy ( _ensdiag.etmplt[_ensdiag.nummbr], tplate );
		if ( etm2[0] == '\0' ) {
		    strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		} else {
		    strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		    strcat ( _ensdiag.etimes[_ensdiag.nummbr], ":" );
		    strcat ( _ensdiag.etimes[_ensdiag.nummbr], etm2 );
		}
                _ensdiag.nummbr += 1;
                if (  _ensdiag.nummbr > MXMBRS - 1 ) {
                    *iret = -7;
		    cmm_free2d ( (void **)entry, &ier );
		    return;
	        }
	    } else {
/*
 * If there is an "*" in the template, this is an
 * ensemble with multiple members. List all the files
 * and set the individual member to the DECMN.CMN.
 */
		if ( strstr ( tplate, "YYYYMMDDHH" ) ) {
		    cst_rpst ( tplate, "YYYYMMDDHH", wtmp, tmplt, &ier );
		} else {
		    cst_rpst ( tplate, "YYMMDDHH", wtmp, tmplt, &ier );
		}

/*
 * If FF or FFF is in the template, replace with the
 * forecast hour string for the scan.
 */
		if ( strstr ( tplate, "FFF" ) ) {
		    cst_rpst ( tmplt, "FFF", fff, tmplt, &ier );
		} else if ( strstr ( tplate, "FF" ) ) {
		    cst_rpst ( tmplt, "FF", &fff[1], tmplt, &ier );
		}

                cfl_scnt ( fpath, tmplt, 1, &dnlist, &nf, &ier );

/*
 * Replace "*" in the template with number names. Note
 * that "*" may be embedded in the template.
 */
		if ( *(starp+1) == '\0' ) {
		    last = G_TRUE;
		} else {
		    last = G_FALSE;
		}

	        istar = (int)( starp - tplate );
                for ( jj = 0; jj < nf; jj++ ) {
		    if ( last == G_TRUE ) {
			strncpy ( tmplt, tplate, istar );
			strcpy ( &tmplt[istar], &dnlist[jj]->d_name[istar] );
		    } else {
			len = strlen(dnlist[jj]->d_name) - strlen(tplate) + 1;
			strncpy ( tmplt, tplate, istar );
			strncpy ( &tmplt[istar], &dnlist[jj]->d_name[istar], len );
			strcpy ( &tmplt[istar+len], &tplate[istar+1] );
		    }
		    free(dnlist[jj]);			
                    memap[_ensdiag.nummbr] = ii;
                    _ensdiag.ensfnm[_ensdiag.nummbr][0] = '\0';
                    strcpy ( _ensdiag.etmplt[_ensdiag.nummbr], tmplt );
                    strcpy ( _ensdiag.enspth[_ensdiag.nummbr], fpath );
		    if ( etm2[0] == '\0' ) {
		        strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		    } else {
		       strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		       strcat ( _ensdiag.etimes[_ensdiag.nummbr], ":" );
		       strcat ( _ensdiag.etimes[_ensdiag.nummbr], etm2 );
		    }
                    _ensdiag.nummbr += 1;
                    if (  _ensdiag.nummbr > MXMBRS - 1 ) {
                        *iret = -7;
		        cmm_free2d ( (void **)entry, &ier );
		        return;
                    }
                }
		if (dnlist != NULL ) free(dnlist);
            }
        }
    }
    cmm_free2d ( (void **)entry, &ier );

/*
 * Write out the number of members as user information:
 */
 //   printf("++++++  de_scan Write out the number of members as user information: %d\n", _ensdiag.nummbr);
    sprintf (diagMessage, "%s %d", "Write out the number of members as user information: ", _ensdiag.nummbr);
    db_msgcave ("de_scan", "debug", diagMessage, &ierm);
    cst_inch ( _ensdiag.nummbr, dum, &ier );
    icode = 4;
    er_wmsg ( "DE", &icode, dum, &ier, strlen("DE"), strlen(dum) );

/*
 * Compute the individual member weights.
 */
    if ( G_DIFF(wgtsum, 0.0F) && _ensdiag.nummbr > 0 ) {

/*
 * If no weights are given, use equal weighting for all members.
 */
	wtm = 1. / _ensdiag.nummbr;
     //   printf( "++++++ de_scan wtm=%f\n", wtm );
        sprintf (diagMessage, "%s %f", "wtm=", wtm);
        db_msgcave ("de_scan", "debug", diagMessage, &ierm);
        for ( ii = 0; ii <  _ensdiag.nummbr; ii++ ) {
            _ensdiag.enswts[ii] = wtm;
        }
    } else {
	wresid = 100.0F - wgtsum;
	if ( wresid < 0 ) wresid = 0.0F;

/*
 * Count the number of -1 weights in the ewgts array.
 */
	kneg = 0;
        for ( ii = 0; ii <  nen; ii++ ) {
            if ( ewgts[ii] < 0.0F ) kneg += 1;
        }
	if ( kneg > 0 ) {
	    wtprm = wresid / kneg;
	    for ( ii = 0; ii <  nen; ii++ ) {
		if ( ewgts[ii] < 0.0F )  ewgts[ii] = wtprm;
	    }
	} else {
/*
 * If total weight less than 100. Return.
 */
	    if ( wresid > 0.0F ) {
		*iret = -13;
		return;
	    }
	}

/*
 * Now we are ready to assign weights to individual members
 * since we have the weight values for each entry in { }.
 */
	for ( ii = 0; ii <  nen; ii++ ) {
/*
 * Count # of members with memap .eq. i, let that number
 * be kmem.
 */
	    kmem = 0;
	    for ( kk = 0; kk < _ensdiag.nummbr; kk++ ) {
		if ( memap[kk] == ii )  kmem += 1;
	    }
	    if ( kmem == 0 ) {
		*iret = -14;
		return;
	    }

/*
 * Compute the individual member weight.
 */
	    wtm = ewgts[ii] / kmem;
	    for ( kk = 0; kk <  _ensdiag.nummbr; kk++ ) {
		if ( memap[kk] == ii ) {
		    _ensdiag.enswts[kk] = wtm / 100.0F;
		}
	    }
	}
    }

/*
 * Save DGCMN information that will be restored by DE_RSET after
 * Ensemble diagnostic.
 */
    de_save ( &_ensdiag.ndxens, iret );

    return;
}
