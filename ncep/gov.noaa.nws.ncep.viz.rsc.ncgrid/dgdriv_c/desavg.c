#include "ensdiag.h"

#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

void de_savg ( const char *uarg, char *stprm, int *iret )
/************************************************************************
 * de_savg								*
 *									*
 * This subroutine computes the ensemble mean of its scalar argument.	*
 *									*
 * de_savg ( uarg, stprm, iret )					*
 *									*
 * Input and parameters:						*
 *	*uarg		const char	Function argument string	*
 *									*
 * Output parameters:							*
 *	*stprm		char		Substitution string		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -8 = cannot parse argument	*
 *					 -9 = ensemble cannot computed	*
 **									*
 * Log:									*
 * T. Lee/SAIC		01/05						*
 * T. Lee/SAIC		03/05	Replace ENS_ with EXX_			*
 * T. Lee/SAIC		04/05	Free unneeded interval grid		*
 * T. Lee/SAIC		06/05	Compute weighted mean			*
 * R. Tian/SAIC		12/05	Translated from Fortran			*
 * m.gamazaychikov/SAIC	10/07	Add ability to use weights		*
 * m.gamazaychikov/SAIC	10/07	Changed nina to narg in de_scan CS	*
 * m.gamazaychikov/SAIC	11/07	Changed MXFLSZ to LLMXLN for argu size	*
 ************************************************************************/
{
    char tname[13], pdum[13], time1[21], time2[21];
    char **argu;
    int ns, num, nina, kxd, kyd, ksub1, ksub2, level1, level2, ivcord,
        zero, one, i, j, ier, narg, numw, nsw;
    float *gns, *gnum, *gwgt, *gnumw, d1, d2;
    double *dgns;
    int ierm;
    char     diagMessage[720];
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    one = 1;

    dg_ssub ( iret );

    /*
     * Get new grid numbers - for output grid and for sum-weight grid 
     */
    dg_nxts ( &ns, iret );
   //  printf ("de_savg after dg_nxts iret=%d\n", *iret);
    sprintf (diagMessage, "%s %d %s %d", "after dg_nxts ns=", ns, "iret=", *iret);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    if ( *iret != 0 ) return;
    dg_nxts ( &nsw, iret );
   //  printf ("de_savg after dg_nxts iret=%d\n", *iret);
    sprintf (diagMessage, "%s %d %s %d", "after dg_nxts nsw=", nsw, "iret=", *iret);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    if ( *iret != 0 ) return;

    /* 
     * Initialize the output grid.
     * Allocate internal double array.
     */
    dg_getg ( &ns, &gns, &kxd, &kyd, &ksub1, &ksub2, iret );
   //  printf ("de_savg after dg_getg iret=%d\n", *iret);
    sprintf (diagMessage, "%s %d", "after dg_getg for ns iret=", *iret);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    dg_getg ( &nsw, &gwgt, &kxd, &kyd, &ksub1, &ksub2, iret );
   //  printf ("de_savg after dg_getg iret=%d\n", *iret);
    sprintf (diagMessage, "%s %d", "after dg_getg for nsw iret=", *iret);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    G_MALLOC(dgns, double, kxd*kyd, "DE_SAVG");
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
        gns[i] = 0.;
        dgns[i] = 0.;
        gwgt[i] = 0.;
    }

    /*
     * Set the number of input arguments.  There may be two arguments
     * for DE_SAVG.
     */
    for ( i = 0; i < MXARGS; i++ ) {
        _ensdiag.allarg[i][0] = '\0';
    }
    nina = 2;
    argu = (char **)cmm_malloc2d ( 2, LLMXLN, sizeof(char), &ier );
    cst_clst ( (char *)uarg, '&', " ", nina, LLMXLN, argu, &narg, &ier );
   //  printf ("de_savg after cst_clst ier=%d \n", ier);
    sprintf (diagMessage, "%s %d %s %d", "after cst_clst narg=",narg," ier=", ier);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    for ( i = 0; i < narg; i++ ) {
        strcpy ( _ensdiag.allarg[i], argu[i] );
        if ( i > 0 && strcmp(argu[i], " ") == 0 ) {
            cst_rlch ( RMISSD, 1, _ensdiag.allarg[i], &ier );
        }
    }

    if ( narg == 1 ) {
        cst_rlch ( RMISSD, 1, _ensdiag.allarg[1], &ier );
    }

    cmm_free2d ( (void **) argu, &ier ); 

    if ( narg < 1 ) {
        *iret = -15;
        return;
    }

    /*
     * Scan the allarg array.
     */
    de_scan ( &narg, iret );
   //  printf ("de_savg after de_scan iret=%d\n", *iret);
    sprintf (diagMessage, "%s %d", "after de_scan iret=", *iret);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    if ( *iret != 0 ) return;

    /*
     * Loop over number of members set by DE_SCAN.
     */
   //  printf ("de_savg loop over number of members set by DE_SCAN to _ensdiag.nummbr=%d\n", _ensdiag.nummbr);
    sprintf (diagMessage, "%s %d", "loop over number of members set by DE_SCAN to _ensdiag.nummbr=", _ensdiag.nummbr);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    for ( i = 0; i < _ensdiag.nummbr; i++ ) {

      //  printf ("de_savg  computing for member %d\n", i);
        sprintf (diagMessage, "%s %d", "computing for member ", i);
        db_msgcave ("de_savg", "debug", diagMessage, &ierm);
        if ( narg == 2 ) {
           de_mset ( &i, iret );
   //  printf ("de_savg after de_mset iret=%d\n", *iret);
	  /*	
	   * Compute weight grid and retrieve it from the stack. 
	   */	
	   dg_pfun ( _ensdiag.allarg[1], iret );
   //  printf ("de_savg after dg_pfun iret=%d\n", *iret);
	   if ( *iret != 0 ) {
	       er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	      *iret = -8;
	       return;
	   }

           dg_driv ( &one, iret );
   //  printf ("de_savg after dg_driv iret=%d\n", *iret);
	   if ( *iret != 0 ) {
	       er_wmsg ( "DG", iret, _ensdiag.allarg[1], &ier,
	                 strlen("DG"), strlen(_ensdiag.allarg[0]) );
	       *iret = -9;
	       return;
	   }

	   dg_tops ( tname, &numw, time1, time2, &level1, &level2,
                     &ivcord, pdum, iret );
	   dg_getg ( &numw, &gnumw, &kxd, &kyd, &ksub1, &ksub2, iret );

	  /*	
	   * Compute field grid and retrieve it from the stack. 
	   */	
           dg_pfun ( _ensdiag.allarg[0], iret );
	   if ( *iret != 0 ) {
	       er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	      *iret = -8;
	       return;
	   }

           dg_driv ( &one, iret );
	   if ( *iret != 0 ) {
	       er_wmsg ( "DG", iret, _ensdiag.allarg[0], &ier,
	                 strlen("DG"), strlen(_ensdiag.allarg[0]) );
	       *iret = -9;
	       return;
           }

           dg_tops ( tname, &num, time1, time2, &level1, &level2,
                     &ivcord, pdum, iret );
           dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );

	   for ( j = ksub1 - 1; j < ksub2; j++ ) {
                d1 = gnumw[j];
	        d2 = gnum[j];
	        if ( ERMISS ( d1 ) || ERMISS ( gwgt[j] ) ) {
		    gwgt[j]  = RMISSD;
	        } else if ( ERMISS ( d2 ) || ERMISS ( dgns[j] ) ) {
		    dgns[j] = RMISSD;
	        } else {
		    gwgt[j] += gnumw[j];
		    dgns[j]  += d1*d2;
	        }
	   }

	   dg_frig ( &numw, &ier );
	   dg_frig ( &num, &ier );


        } else if (narg == 1)  {
           sprintf (diagMessage, "%s %d", "narg=", narg);
           db_msgcave ("de_savg", "debug", diagMessage, &ierm);
           sprintf (diagMessage, "%s %s", "_ensdiag.etmplt[0]=\n", _ensdiag.etmplt[0]);
           db_msgcave ("de_savg", "debug", diagMessage, &ierm);
          //  printf ("de_savg _ensdiag.etmplt[0]=%s\n", _ensdiag.etmplt[0]);
           sprintf (diagMessage, "%s %d", "calling de_mset for i=", i);
	   de_mset ( &i, iret );
           sprintf (diagMessage, "%s %d", "after de_mset iret=", *iret);
           db_msgcave ("de_savg", "debug", diagMessage, &ierm);
   //  printf ("de_savg after de_mset iret=%d\n", *iret);
          //  printf ("de_savg after de_mset _ensdiag.etmplt[0]=%s\n", _ensdiag.etmplt[0]);
	   dg_pfun ( _ensdiag.allarg[0], iret );
   //  printf ("de_savg after dg_pfun iret=%d\n", *iret);
           sprintf (diagMessage, "%s %d", "after dg_pfun iret=", *iret);
           db_msgcave ("de_savg", "debug", diagMessage, &ierm);
	   if ( *iret != 0 ) {
	       er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	      *iret = -8;
	       return;
	   }
	   dg_driv ( &one, iret );
   //  printf ("de_savg after dg_driv iret=%d\n", *iret);
           sprintf (diagMessage, "%s %d", "after dg_driv iret=", *iret);
           db_msgcave ("de_savg", "debug", diagMessage, &ierm);
	   if ( *iret != 0 ) {
	       er_wmsg ( "DG", iret, _ensdiag.allarg[0], &ier,
	                 strlen("DG"), strlen(_ensdiag.allarg[0]) );
	       *iret = -9;
	       return;
	    }

	/*	
	 * Retrieve the output grid from the stack.  Check that the 
	 * output is a scalar.
	 */	
	     dg_tops ( tname, &num, time1, time2, &level1, &level2,
                       &ivcord, pdum, iret );
             sprintf (diagMessage, "%s %s %s %d %s %d", "after dg_tops tname=", tname, "num=",num,  "iret=", *iret);
             db_msgcave ("de_savg", "debug", diagMessage, &ierm);

	     dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );
             sprintf (diagMessage, "%s %f %s %d", "after dg_getg gnum[0]=", gnum[0], "iret=", *iret);
             db_msgcave ("de_savg", "debug", diagMessage, &ierm);
        
	     for ( j = ksub1 - 1; j < ksub2; j++ ) {
	        d1 = gnum[j];
	        if ( ERMISS ( d1 ) || ERMISS ( dgns[j] ) ) {
		    dgns[j] = RMISSD;
	        } else {
		    dgns[j] += d1 * _ensdiag.enswts[i];
	        }
	     }
	     dg_frig ( &num, &ier );
        }
    }
    /*
     * Normalize the truncated set of ensemble members
     */
    if ( narg == 2 ) {
      for ( j = ksub1 - 1; j < ksub2; j++ ) {
          if ( ERMISS ( gwgt[j] ) || ERMISS ( dgns[j] ) ) {
             dgns[j] = RMISSD;
          } else {
             dgns[j] = dgns[j]/gwgt[j];
          }
      }
    }

    /*
     * Assign the result to the output array and free the internal arrays.
     */
    for ( i = ksub1 - 1; i < ksub2; i++ ) {
	gns[i] = (float)dgns[i];
    }
    G_FREE(dgns, double);

    /*
     * Reset DGCMN.CMN and set internal grid identifier.
     */
    sprintf (diagMessage, "%s %f", "after assign the result to the output array gns[0]=", gns[0]);
    db_msgcave ("de_savg", "debug", diagMessage, &ierm);
    de_rset ( iret );
    dg_udig ( "EXX_", &ns, &zero, &_ensdiag.idgens, stprm, iret );
    dg_esub ( &ns, &zero, &zero, &zero, &ier );
    if ( ier != 0 )  *iret = ier;

    return;
}
