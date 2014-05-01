#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

#include "ensdiag.h"

void de_driv ( char *efunc, int *iret )
/************************************************************************
 * de_driv								*
 *									*
 * This subroutine scans the GFUNC/GDPFUN user input for ENS_ functions	*
 * and calls the appropriate ensemble function subroutine for each one	*
 * found.								*
 *									*
 * de_driv ( efunc, iret )						*
 *									*
 * Input and Output parameters:						*
 *	*efunc		char		ENS_ function			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 +2 = No ensemble function	*
 *					 -4 = Operator not recognized	*
 *					-10 = Ens. func. in ens. diag.	*
 **									*
 * Log:									*
 * T. Lee/SAIC		01/05						*
 * T. Lee/SAIC		03/05	Checked ENS_ within ENS_; Used EXX_	*
 * T. Lee/SAIC		04/05	Added DE_VAVG, DE_SSPRD, DE_VSRPD	*
 * R. Tian/SAIC		06/05	Added DE_SMAX, DE_SMIN, DE_SRNG		*
 * T. Lee/SAIC		06/05	Added DE_PRCNTL				*
 * R. Tian/SAIC		07/11	Added DE_MODE				*
 * m.gamazaychikov/SAIC	09/05	Added call to DE_SAVG for ENS_PROB	*
 * R. Tian/SAIC		12/05	Translated from Fortran			*
 * M. Li/SAIC		10/06	Added de_cprb and de_cval		*
 * m.gamazaychikov/SAIC	09/05	Added call to de_swsprd and de_vwsprd	*
 ************************************************************************/
{
    char lefunc[LLMXLN+1], uarg[LLMXLN+1], stprm[LLMXLN+1],
         tpfunc[LLMXLN+1], f[17], errst[61];
    char *ensptr, *opnptr, *clsptr, *ptr;
    int done, index, i, ier;
    int ierm;
    char     diagMessage[720];
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check if EFUNC is an ensemble function.  If not, but ensemble
     * file is specified, return with a warning.
     */
   //  printf ("------in de_driv efunc=%s\n", efunc);
    sprintf (diagMessage, "%s %s", "efunc=", efunc);
    db_msgcave ("de_driv", "debug", diagMessage, &ierm);
    cst_lcuc ( efunc, lefunc, &ier );
    ensptr = strstr ( lefunc, "ENS_" );
    if ( ensptr == NULL ) {
        for ( i = 0; i < NGDFLS; i++ ) {
	    if ( _ensdiag.ensspc[i][0] != '\0' ) *iret = +2;
	}
	return;
    }

    /*
     * Parse the ensemble function and call the appropriate subroutine.
     */
    done = G_FALSE;
    while ( done == G_FALSE ) {
        opnptr = strchr ( ensptr, '(' );
	cst_opcl ( lefunc, opnptr, &clsptr, &ier );
	for ( index = 0, ptr = ensptr+4; ptr < opnptr; ptr++, index++ ) {
	    f[index] = *ptr;
	}
	f[index] = '\0';

	for ( index = 0, ptr = opnptr+1; ptr < clsptr; ptr++, index++ ) {
	    uarg[index] = *ptr;
	}
	uarg[index] = '\0';

        /*
	 * If an ensemble function is embedded in ensemble diagnostics,
	 * return with an error.
	 */
	if ( strstr ( uarg, "ENS_" ) ) {
	    *iret = -10;
	    er_wmsg ( "DE", iret, " ", &ier, strlen("DE"), strlen(" ") );
	    return;
	}

	errst[0] = '\0';
	if ( strcmp ( f, "SAVG" ) == 0 ) {
           //  printf ("de_driv before de_savg _ensdiag.etmplt[0]=%s\n",_ensdiag.etmplt[0]);
            sprintf (diagMessage, "%s %s", "before de_savg _ensdiag.etmplt[0]=", _ensdiag.etmplt[0]);
            db_msgcave ("de_driv", "debug", diagMessage, &ierm);
	    de_savg ( uarg, stprm, iret );
            sprintf (diagMessage, "%s %d %s %s %s %s", "after de_savg iret=", *iret, "_ensdiag.etmplt[0]=", _ensdiag.etmplt[0], "stprm=", stprm);
            db_msgcave ("de_driv", "debug", diagMessage, &ierm);
           //  printf ("de_driv after de_savg iret=%d\n", *iret);
           //  printf ("de_driv after de_savg _ensdiag.etmplt[0]=%s\n", _ensdiag.etmplt[0]);
	} else if ( strcmp ( f, "PROB" ) == 0 ) {
	    /*
	     * NOTE:  ENS_PROB is identical to DE_SAVG
	     */
	    de_savg ( uarg, stprm, iret );
	} else if ( strcmp ( f, "VAVG" ) == 0 ) {
	    de_vavg ( uarg, stprm, iret );
	} else if ( strcmp ( f, "SSPRD" ) == 0 ) {
	    de_ssprd ( uarg, stprm, iret );
	} else if ( strcmp ( f, "VSPRD" ) == 0 ) {
	    de_vsprd ( uarg, stprm, iret );
	} else if ( strcmp ( f, "SMAX" ) == 0 ) {
	    de_smax ( uarg, stprm, iret );
	} else if ( strcmp ( f, "SMIN" ) == 0 ) {
	    de_smin ( uarg, stprm, iret );
	} else if ( strcmp ( f, "SRNG" ) == 0 ) {
	    de_srng ( uarg, stprm, iret );
	} else if ( strcmp ( f, "PRCNTL" ) == 0 ) {
	    de_prcntl ( uarg, stprm, iret );
	} else if ( strcmp ( f, "MODE" ) == 0 ) {
	    de_mode ( uarg, stprm, iret );
	} else if ( strcmp ( f, "CPRB" ) == 0 ) {
            de_cprb ( uarg, stprm, iret );
	} else if ( strcmp ( f, "CVAL" ) == 0 ) {
            de_cval ( uarg, stprm, iret );
	} else if ( strcmp ( f, "SWSPRD" ) == 0 ) {
	    de_swsprd ( uarg, stprm, iret );
	} else if ( strcmp ( f, "VWSPRD" ) == 0 ) {
	    de_vwsprd ( uarg, stprm, iret );
	} else {
	    *iret = -4;
	    strcpy ( errst, f );
	    dg_cset ( "ERRST", errst, &ier );
	    done = G_TRUE;
	    er_wmsg ( "DE", iret, errst, &ier, strlen("DE"), strlen(errst) );
	}

	dg_cget ( "ERRST", errst, &ier );
	if ( *iret == 0 ) {
	    index = 0;

	    ptr = lefunc;
	    while ( ptr < ensptr ) {
	        tpfunc[index++] = *ptr++;
	    }

	    ptr = stprm;
	    while ( *ptr != '\0' ) {
	        tpfunc[index++] = *ptr++;
	    }

	    ptr = clsptr + 1;
	    while ( *ptr != '\0' ) {
	        tpfunc[index++] = *ptr++;
	    }

	    tpfunc[index] = '\0';
	    strcpy ( lefunc, tpfunc );
	} else if ( strcmp ( errst, f ) != 0 ) {
	    strcat ( errst, " in " );
	    strcat ( errst, f );
	    dg_cset ( "ERRST", errst, &ier );
	    done = G_TRUE;
	    er_wmsg ( "DE", iret, errst, &ier, strlen("DE"), strlen(errst) );
	}

        ensptr = strstr ( lefunc, "ENS_" );
	if ( ensptr == NULL ) done = G_TRUE;
    }

    strcpy ( efunc, lefunc );

    return;
}
