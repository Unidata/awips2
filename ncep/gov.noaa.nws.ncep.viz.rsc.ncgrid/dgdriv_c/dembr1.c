#include "ensdiag.h"
#include "dbcmn.h"
	
void de_mbr1 ( const int *k, const char *infl, char *outfl, int *iret )
/************************************************************************
 * de_mbr1								*
 *									*
 * This subroutine parses a GDFILE entry to find and handle ensemble 	*
 * specifications.  It is called from DG_NFIL.  The routine turns an 	*
 * ensemble entry into a single entry that DG_NFIL can handle.  	*
 *									*
 * de_mbr1 ( k, infl, outfl, iret )					*
 *									*
 * Input parameters:							*
 *	*k		const int	GDFILE entry position number	*
 *	*infl		const char	Input file entry		*
 *									*
 * Output parameters:							*
 *	*outfl		char*		Output file replaces INFL	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -2 = grid file or template not	*
 *						exist			*
 *									*
 * Log:									*
 * T. Lee/SAIC		12/04						*
 * T. Lee/SAIC		06/05	Parsed weight value			*
 * R. Tian/SAIC		12/05	Translated from Fortran			*
 * m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
 * T. Piper/SAIC	04/07	Modified for cfl_scnt CSC		*
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
 ***********************************************************************/
{
    int index, nfarr, ic, is, iff, ir, ii, ino, ihb, mnb, iha, mna,
	mstrct, idtmch, nfile, istar, last, len, ier,ierm;
    long lens;
    char lsv[MXFLSZ], newfil[MXFLSZ], filnam[MXFLSZ], cycle[DTTMSZ],
        path[MXFLSZ], tmplt[MXFLSZ], dum[DTTMSZ], rplc[DTTMSZ], membr[17];
    char *opnptr, *clsptr, *starp, *ptr, **farr, *def = " " ,ingdtm[41];
    struct dirent **dnlist=NULL;
    int  isA2DB = 0, isWeight=0;
     char     diagMessage[720];
/*----------------------------------------------------------------------*/
    *iret = 0;
    outfl[0] = '\0';

/*
 * Check if INFL contains a curly-bracket enclosed ensemble spec.
 * If not, the first entry is not an ensemble grid.  Do nothing.
 */
    opnptr = strchr ( infl, '{' );
    if ( ! opnptr ) {
        strcpy ( outfl, infl );
	return;
    }
    if (( strstr ( infl, "A2DB" ) ) != NULL) {
        isA2DB = 1;
    }

/*
 * Save the string between curly brackets to ensspc (k).  Get
 * the first element of the file(s). 
 */
    cst_opcl ( infl, opnptr, &clsptr, &ier );
    if ( ier != 0 ) {
        *iret = -2;
	return;
    }
    for ( index = 0, ptr = opnptr + 1; ptr < clsptr; index++, ptr++ ) {
        _ensdiag.ensspc[*k-1][index] = *ptr;
    }
    _ensdiag.ensspc[*k-1][index] = '\0';

    clsptr = strchr ( _ensdiag.ensspc[*k-1], ',' );
    if ( ! clsptr ) {
        strcpy ( lsv, _ensdiag.ensspc[*k-1] );
    } else {
        for ( index = 0, ptr = _ensdiag.ensspc[*k-1];
	      ptr < clsptr; index++, ptr++ ) {
	    lsv[index] = *ptr;
	}
	lsv[index] = '\0';
    }

/*
 * If file name is not an alias, do nothing.
 */
    cfl_inqr ( lsv, NULL, &lens, newfil, &ier );
    if ( ier == 0 ) {
        strcpy ( outfl, lsv );
	return;
    }

/*
 * Get the template.  If the template does not end with "*", 
 * do nothing.
 */
    farr = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
    if ( strchr ( lsv, '%' ) ) {
        cst_clst ( lsv, '%', def, 2, MXFLSZ, farr, &nfarr, &ier );
	strcpy ( lsv, farr[1] );
        isWeight = 1;
    }
    cst_clst ( lsv, '|', def, 2, MXFLSZ, farr, &nfarr, &ier );
    if ( isA2DB == 1 && isWeight == 1) {
       strcpy ( filnam, "A2DB_" );
       strcat (  filnam, farr[0] );
    }
    else  strcpy ( filnam, farr[0] );
    strcpy ( cycle, farr[1] );
    sprintf (diagMessage, "%s%s%s%s", "filnam= ", filnam, " cycle=", cycle);
    db_msgcave ("de_mbr1", "info", diagMessage, &ierm);
    cmm_free2d ( (void **)farr, &ier );
    ctb_dtget ( filnam, path, tmplt, &ic, &is, &iff, &ir, &ii, &ino,
                &ihb, &mnb, &iha, &mna, &mstrct, &idtmch, &ier );
    starp = strchr ( tmplt, '*' );
    istar = (int)( starp - tmplt );
    if ( ! starp ) {
        if ( isWeight == 1 ) {
           strcpy ( outfl,filnam );
        }
        else strcpy ( outfl, lsv );
	return;
    }

/*
 * If the template contains an "*", then it is an ensemble with
 * multiple members.  The last member is the one sought.
 */
    if ( strcmp ( cycle, def ) != 0 ) {
        cti_stan ( cycle, "YYMMDD/HHNN", dum, &ier );
        if ( strstr ( tmplt, "YYYYMMDD" ) ) {
            strcpy ( rplc, "YY" );
            strncat ( rplc, dum, 6 );
            rplc[8] = '\0';
            cst_rpst ( tmplt, "YYYYMMDD", rplc, tmplt, &ier );
        } else {
            strncpy ( rplc, dum, 6 );
            rplc[6] = '\0';
            cst_rpst ( tmplt, "YYMMDD", rplc, tmplt, &ier );
        }
        strncpy ( rplc, &dum[7], 2 );
        rplc[2] = '\0';
        cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
    }
    else {
         dg_cget ( "INGDTM", ingdtm, &ier );
        /*Initialize forecast time*/
/*	if ( fhrsClbkPtr != NULL ) {
        	fhrsClbkPtr();
    	}
    	if ( fhrsStrBack != NULL ){*/
        if (ingdtm[0] != '\0') {
        	sprintf (diagMessage, "%s %s", "Forecast Hour ingdtm =", ingdtm);
        	db_msgcave ("de_mbr1", "debug", diagMessage, &ierm);
        	farr = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
        	cst_clst ( ingdtm, '/', def, 2, MXFLSZ, farr, &nfarr, &ier );
        	if ( strstr ( tmplt, "YYYY" ) ) {
           	    strcpy ( rplc, "YY" );
           	    strncat ( rplc, farr[0], 6 );
           	    rplc[8] = '\0';
           	    cst_rpst ( tmplt, "YYYYMMDD", rplc, tmplt, &ier );
        	} else {
        		cst_ncpy ( rplc, farr[0], 6, &ier );
        		cst_rpst ( tmplt, "YYMMDD", rplc, tmplt, &ier );
        	}
        	cst_ncpy ( rplc, farr[1], 2, &ier );
        	cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
        	cst_clst ( ingdtm, 'F', def, 2, MXFLSZ, farr, &nfarr, &ier );
        	cst_ncpy ( rplc, farr[1], 3, &ier );
        	cst_rpst ( tmplt, "FFF", rplc, tmplt, &ier );
        	cmm_free2d((void **)farr,&ier);
     }
   }

/*
 * Retrieve the last member of the ensemble alias.
 */
    cfl_scnt ( path, tmplt, -1, &dnlist, &nfile, &ier );
    if ( nfile == 0 ) {
	*iret = -2;
	er_wmsg ( "DE", iret, " ", &ier, strlen("DE"), strlen(" ") );
	return;
    }
    strcpy ( newfil, dnlist[0]->d_name );
    for (ii=0;ii<nfile;ii++){
	free(dnlist[ii]);
    }
    if ( dnlist != NULL ) free(dnlist);

/*
 * Replace "*" in the template with member names.  Note
 * that "*" may be embedded in the template.
 */
    if ( *(starp+1) == '\0' ) {
	last = G_TRUE;
    } else {
	last = G_FALSE;
    }
    if ( last == G_TRUE ) {
	strcpy ( membr, &newfil[istar] );
    } else {
	len = strlen ( newfil ) - strlen ( tmplt ) + 1;
	cst_ncpy ( membr, &newfil[istar], len, &ier );
    }

/*
 * Recombine the alias and last member with the cycle.
 */
    sprintf (diagMessage, "%s %s %s %d", "membr=", membr,"length=",strlen(membr));
    db_msgcave ("de_mbr1", "debug", diagMessage, &ierm);

    strcpy ( outfl, filnam );
    if ( strlen(membr) > 0 ) {
    	strcat ( outfl, ":" );
    	strcat ( outfl, membr );
    }
    if ( strcmp ( cycle, def ) != 0 ) {
        strcat ( outfl, "|" );
	strcat ( outfl, cycle );
    }

    return;
}
