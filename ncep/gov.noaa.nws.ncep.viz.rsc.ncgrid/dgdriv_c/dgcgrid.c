#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

#include "dg.h"

void dgc_grid ( const char *gdattm, const char *glevel, const char *gvcord,
                const char *gfunc, char *pfunc, float *grid, int *igx,
 	        int *igy, char *time1, char *time2, int *level1,
	        int *level2, int *ivcord, char *parm, int *iret )
/************************************************************************
 * dgc_grid								*
 *									*
 * This subroutine computes a grid diagnostic scalar quantity.  The	*
 * inputs for GDATTM, GLEVEL, GVCORD and GFUNC should be the values	*
 * input by the user.							*
 *									*
 * dgc_grid  ( gdattm, glevel, gvcord, gfunc, pfunc, grid, igx, igy,	*
 *             time1, time2, level1, level2, ivcord, parm, iret )	*
 *									*
 * Input parameters:							*
 *	*gdattm		const char	Input date/time			*
 *	*glevel		const char	Input level			*
 *	*gvcord		const char	Input vertical coordinate	*
 *	*gfunc		const char	Diagnostic function		*
 *									*
 * Output parameters:							*
 *	*pfunc		char		Diagnostic error string		*
 *	*grid		float		Output scalar grid		*
 *	*igx		int		Number of points in x dir	*
 *	*igy		int		Number of points in y dir	*
 *	*time1		char		Output date/time		*
 *	*time2		char		Output date/time		*
 *	*level1		int		Output level 			*
 *	*level2		int		Output level 			*
 *	*ivcord		int		Output vertical coordinate	*
 *	*parm		char		Output parameter name		*
 *	*iret		int		Return code			*
 *				  	  3 = user typed EXIT		*
 *					  0 = normal return		*
 *					 -3 = GFUNC is blank		*
 *					 -4 = output grid not a scalar	*
 *					 -6 = wrong number of operands	*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					 -9 = incorrect operands	*
 *					-10 = internal grid list full	*
 *					-11 = operand must be a vector	*
 *					-12 = operand must be a scalar	*
 *					-13 = operand must be from grid	*
 *					-14 = DG_INIT not initialized	*
 *					-15 = polar center invalid	*
 *					-16 = map proj is invalid	*
 *					-17 = LEVEL must be a layer	*
 *					-18 = TIME must be a range	*
 *					-19 = invalid operator		*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 *					-26 = layer of layers invalid	*
 *					-27 = time range layer invalid	*
 *					-47 = internal grid is too big	*
 *					-70 = cannot computer ensemble	*
 *					-71 = cannot computer layer	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 4/86	Cleaned up errors; add GR_FIND		*
 * M. desJardins/GSFC	 5/88	Documentation				*
 * G. Huffman/GSC	 9/88	Error messages				*
 * S. Schotz/GSC	 6/90	Removed respnd flag			*
 * M. desJardins/NMC	 3/92	Change scaling for MIXR,...		*
 * K. Brill/NMC          4/93   Remove initialization of idglst		* 
 * K. Brill/NMC		 4/93	Initialize the grid-in-use flag		*
 * L. Sager/NMC		 5/93   Scale grid only when required   	* 
 * L. Sager/NMC 	 5/93   Allow grid rename			*
 * K. Brill/NMC		 5/93	CALL ST_LCUC for gvcord			*
 * M. desJardins/NMC	 7/93	Eliminate ; as separator for name	*
 * T. Lee/GSC		 4/96	Changed NDGRD to maxdgg; single 	*
 *				dimension for dgg			*
 * K. Tyle/GSC		 5/96	Moved IGDPT outside do-loop		*
 * K. Brill/HPC		11/01	Change for IUSESV replacing USEFLG	*
 * K. Brill/HPC		12/01	Initialize ISUBID			*
 * K. Brill/HPC		11/02	Check for KXYD > LLMXGD			*
 * K. Brill/HPC		 5/03   Falsify SAVFLG for previous grids saved	*
 *				with the same name using //		*
 * T. Lee/SAIC		12/04	Added ensemble function			*
 * T. Lee/SAIC		 3/05	Added layer diagnostics			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * S. Gilbert/NCEP	 7/07	Removed LLMXGD check			*
 ************************************************************************/
{
    char ppp[133], gvc[13], gf[133], carr1[133], carr2[13], glv[LLMXLN+1];
    char *savptr;
    double gscale;
    int len, zero=0, itype, num, ier, i;
    int      ierm;
    char     diagMessage[720];
/*----------------------------------------------------------------------*/
/*
 *  Initialize output parameters.
 */
    pfunc[0]	= '\0';
    *igx	= 0;
    *igy	= 0;
    time1[0]	= '\0';
    time2[0]	= '\0';
    *level1	= 0;
    *level2	= 0;
    *ivcord	= 0;
    parm[0]	= '\0';

/*
 * Check that the diagnostic package has been initialized.
 */
    if ( _dgfile.dgset == G_FALSE) {
	*iret = -14;
	return;
    } else {
	*iret = 0;
    }
    sprintf (diagMessage, "%s %s", "gdfunc =", (char *)gfunc);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);

/*
 * If GFUNC includes both layer and ensemble diagnostics,
 * return with an error for now.
 */
    cst_rmbl ( (char *)gfunc, gf, &len, &ier );
    cst_lcuc ( gf, gf, &ier );
    if ( strstr ( gf, "ENS_" ) && strstr ( gf, "LYR_") ) {
	*iret = -72;
	return;
    }

    /*
     * Break gfunc into two strings separated at //.
     */
    savptr = strstr ( gf, "//" );
    if ( ! savptr ) {
	strcpy ( carr1, gf );
	carr2[0] = '\0';
    } else {
        len = (int)(savptr - gf );
	strncpy ( carr1, gf, len );
	carr1[len] = '\0';
	strcpy ( carr2, savptr + 2 );
    }

    /*
     * Initialize subroutine ID # & the grid-in-use flag.
     */
    _dggrid.isubid = 0;
    for ( i = 0; i < _dggrid.maxdgg; i++ ) {
	_dggrid.iusesv[i] = 0;
    }

    /*
     * Increment subroutine ID.
     */
    dg_ssub ( &ier );
  //  printf (" dgc_grid after dg_ssub ier=%d\n", ier);
    sprintf (diagMessage, "%s %d", "after dg_ssub ier=", ier);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);

    /*
     * Save date/time, level and vertical coordinate in common.
     */
    cst_lcuc ( (char *)gvcord, gvc, &ier );
    cst_lcuc ( (char *)glevel, glv, &ier );
    dg_stlv ( gdattm, glv, gvc, "GFUNC", carr1, iret );
    if ( *iret != 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }
    /*
     * Compute the layer function.
     */
    dl_driv ( carr1, iret );
  //  printf (" dgc_grid after dl_driv iret=%d\n", *iret);
    sprintf (diagMessage, "%s %d", "after dl_driv iret=", *iret);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
    if ( *iret != 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	*iret = -71;
	return;
    }

    /*
     * Compute the ensemble function.
     */
  //  printf ("DGC_GRID before de_driv  _nfile.ntmplt[0]=%s\n", _nfile.ntmplt[0]);
  //  printf ("DGC_GRID calling de_driv\n");
    sprintf (diagMessage, "%s %s", "before de_driv  _nfile.ntmplt[0]=", _nfile.ntmplt[0]);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
    de_driv ( carr1, iret );
  //  printf (" dgc_grid after de_driv iret=%d\n", *iret);
  //  printf (" dgc_grid after de_driv _nfile.ntmplt[0]=%s\n", _nfile.ntmplt[0]);
    sprintf (diagMessage, "%s %s %s %d", "before de_driv  _nfile.ntmplt[0]=", _nfile.ntmplt[0], "iret=", *iret);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
    if ( *iret < 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	*iret = -70;
	return;
    }

    /*
     * Parse the input function.
     */
    dg_pfun ( carr1, iret );
    if ( *iret != 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }

    /*
     * Compute the output grid.
     */
    itype = 1;
  //  printf ("DGC_GRID calling dg_driv\n");
    sprintf (diagMessage, "%s", "calling dg_driv");
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
    dg_driv ( &itype, iret );
    sprintf (diagMessage, "%s %d", "after dg_driv iret=", *iret);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
    if ( *iret != 0 ) {
	strcpy ( pfunc, _dgerr.errst );
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }

    /*
     * Retrieve the output grid from the stack.
     * Check that the output is a scalar.
     */
    num = _dgstck.istack[0];
    sprintf (diagMessage, "%s %d", "retrieving output grid from the stack num=", num);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
    if ( ( _dgstck.itop != 0 ) || ( num <= 0 ) || ( num >= 100 ) ) {
	*iret = -4;
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }

    if ( carr2[0] != '\0' ) {
	/*
	 * Falsify the SAVFLG for grids also having the name
	 * stored in CARR2, and free them.
	 */
        sprintf (diagMessage, "%s", "falsifying the SAVFLG for grids");
        db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
	for ( i = 1; i <= _dggrid.maxdgg; i++ ) {
	    if ( strcmp ( _dggrid.gparmd[i-1], carr2 ) == 0 ) {
	        _dggrid.savflg[i-1] = G_FALSE;
	    }
	}
	strcpy ( _dggrid.gparmd[num-1], carr2 );
	_dggrid.savflg[num-1] = G_TRUE;
    }

    /*
     * Move the grid to the output grid array.  Scale the mixing ratio
     * and Montgomery stream function data.
     */
    cst_rmbl  ( carr1, ppp, &len, &ier );
    if ( ( strncmp ( ppp, "MIXR", 4 ) == 0 ) ||
         ( strncmp ( ppp, "MIXS", 4 ) == 0 ) ||
	 ( strncmp ( ppp, "SMXR", 4 ) == 0 ) ||
	 ( strncmp ( ppp, "SMXS", 4 ) == 0 ) ) {
	gscale = 1000.0;
    } else if ( strncmp ( ppp, "PSYM", 4 ) == 0 ) {
	gscale = 0.01;
    } else {
	gscale = 1.0;
    }

    if ( G_DIFF(gscale, 1.0) ) {
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    grid[i] = _dggrid.dgg[num-1].grid[i];
	}
    } else {
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    if ( ERMISS ( _dggrid.dgg[num-1].grid[i] ) ) {
		grid[i] = RMISSD;
	    } else {
		grid[i] = _dggrid.dgg[num-1].grid[i] * gscale;
	    }
	}
    }

    sprintf (diagMessage, "%s %f", "returning this grid[0]=",grid[0]);
    db_msgcave ("dgc_grid", "debug", diagMessage, &ierm);
    /*
     * Get output variables.
     */
    *igx = _dgfile.kxd;
    *igy = _dgfile.kyd;
    strcpy ( time1, _dggrid.dttimd1[num-1] );
    strcpy ( time2, _dggrid.dttimd2[num-1] );
    *level1 = _dggrid.leveld1[num-1];
    *level2 = _dggrid.leveld2[num-1];
    *ivcord = _dggrid.ivcrdd[num-1];
    strcpy ( parm, _dggrid.gparmd[num-1] );

    dg_esub ( &zero, &zero, &zero, &zero, &ier );

    return;
}
