#include "dg.h"

void dgc_vecr ( const char *gdattm, const char *glevel, const char *gvcord,
                const char *gvect, char *pfunc, float *ugrid, float *vgrid,
	        int *igx, int *igy, char *time1, char *time2, int *level1,
	        int *level2, int *ivcord, char *parmu, char *parmv,
	        int *iret )
/************************************************************************
 * dgc_vecr								*
 *									*
 * This subroutine computes a grid diagnostic vector quantity.  The 	*
 * u and v components returned in UGRID and VGRID are in grid relative	*
 * coordinates.  GDATTM, GLEVEL, GVCORD and GVECT should have the	*
 * values entered by the user.						*
 *									*
 * dgc_vecr ( gdattm, glevel, gvcord, gvect, pfunc, ugrid, vgrid, igx,	*
 *            igy, time1, time2, level1, level2, ivcord, parmu, parmv,	*
 *            iret )							*
 *									*
 * Input parameters:							*
 *	gdattm 		const char	Input date/time			*
 *	glevel		const char	Input level			*
 *	gvcord		const char	Input vertical coordinate	*
 *	gvect		const char	Diagnostic function		*
 *									*
 * Output parameters:							*
 *	pfunc		char		Diagnostic error string		*
 *	ugrid		float		Output u component grid		*
 *	vgrid		float		Output v component grid		*
 *	igx		int		Number of points in x dir	*
 *	igy		int		Number of points in y dir	*
 *	time1		char		Output date/time		*
 *	time2		char		Output date/time		*
 *	level1		int		Output level 			*
 *	level2		int		Output level 			*
 *	ivcord		int		Output vertical coordinate	*
 *	parmu		char		Parameter name for u component 	*
 *	parmv		char		Parameter name for v component 	*
 *	iret		int		Return code			*
 *				  	  3 = user typed EXIT		*
 *					  0 = normal return		*
 *					 -3 = parsing table is empty	*
 *					 -5 = output grid not a vector	*
 *					 -6 = wrong number of operands	*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					 -9 = incorrect operands	*
 *					-10 = internal grid list full	*
 *					-11 = operand must be a vector	*
 *					-12 = operand must be a scalar	*
 *					-13 = operand must be from file	*
 *					-14 = DG_INIT not initialized	*
 *					-15 = polar grid cent. not valid*
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
 * M. desJardins/GSFC	 4/89	Added grid rel and north rel subs	*
 * S. Schotz/GSFC	 6/90	Removed respnd flag			*
 * M. desJardins/NMC	 3/92	Removed scale from common		*
 * K. Brill/NMC		 5/93	Initialize the grid-in-use flag		*
 * K. Brill/NMC		 5/93	Capitalize GDATTM,GLEVEL,GVCORD,GFUNC	*
 * L. Sager/NMC		 7/93	Permit vector grid rename		*
 * M. desJardins/NMC	 7/93	Eliminate ; as separator for name	*
 * L. Sager/NMC		 8/93   Correct misstore of GDATTIM      	*
 * T. Lee/GSC		 4/96	Changed NDGRD to maxdgg; single		*
 *				dimension for dgg			*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		11/01	Change for IUSESV replacing USEFLG	*
 * K. Brill/HPC		12/01	Initialize ISUBID			*
 * K. Brill/HPC		11/02	Check for KXYD > LLMXGD			*
 * K. Brill/HPC		 5/03   Falsify SAVFLG for previous grids saved	*
 *				with the same name using //		*
 * T. Lee/SAIC		12/04	Added ensemble function			*
 * T. Lee/SAIC		 3/05	Added layer diagnostic			*
 * R. Tian/SAIC		 3/06	Recoded from Fortran			*
 * S. Gilbert/NCEP	 7/07	Removed LLMXGD limit check 		*
 * H. Zeng/SAIC		08/07	Initialized output variables		*
 ************************************************************************/
{
    char carr1[133], carr2[13], ctst[13], gf[133], gvc[5], glv[LLMXLN+1];
    char *savptr;
    int ng, itype, num, numu, numv, zero, i, ier;
/*----------------------------------------------------------------------*/
  
/*
 *  Initialize output parameters.
 */
    pfunc[0]	= '\0';
    *ugrid      = 0.0F;
    *vgrid      = 0.0F;
    *igx	= 0;
    *igy	= 0;
    time1[0]	= '\0';
    time2[0]	= '\0';
    *level1	= 0;
    *level2	= 0;
    *ivcord	= 0;
    parmu[0]	= '\0';
    parmv[0]	= '\0';

/*
 *  Initialize constant variables.
 */
    zero = 0;

/*
 *  Check that the diagnostic package has been initialized.
 */
    if ( _dgfile.dgset == G_FALSE ) {
	*iret = -14;
	return;
    } else {
	*iret = 0;
    }

/*
 *  If GFUNC includes both layer and ensemble diagnostics,
 *  return with an error for now.
 */
    cst_rmbl ( (char *)gvect, gf, &ng, &ier );
    cst_lcuc ( gf, gf, &ier );
    if ( strstr ( gf, "ENS_" ) && strstr ( gf, "LYR_" ) ) {
	*iret = -72;
	return;
    }

/*
 *  Break gvect into two strings separated at //.
 */
    savptr = strstr ( gf, "//" );
    if ( ! savptr ) {
	strcpy ( carr1, gf );
	carr2[0] = '\0';
    } else {
	ng = (int)(savptr - gf );
	strncpy ( carr1, gf, ng );
	carr1[ng] = '\0';
	strcpy ( carr2, savptr + 2 );
    }
    
/*
 *  Initialize subroutine ID # & the grid-in-use flag.
 */
    _dggrid.isubid = 0;
    for ( i = 0; i < _dggrid.maxdgg; i++ ) {
	_dggrid.iusesv[i] = 0;
    }

/*
 *  Increment subroutine ID.
 */
    dg_ssub ( &ier );

/*
 *  Save date/time, level and vertical coordinate in common.
 */
    cst_lcuc ( (char *)gvcord, gvc, &ier );
    cst_lcuc ( (char *)glevel, glv, &ier );;
    dg_stlv ( gdattm, glv, gvc, "GVECT", carr1, iret );
    if ( *iret != 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }

/*
 *  Compute the layer function.
 */
    dl_driv ( carr1, iret );
    if ( *iret != 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	*iret = -71;
	return;
    }

/*
 *  Compute the ensemble function.
 */
    de_driv ( carr1, iret );
    if ( *iret != 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	*iret = -70;
	return;
    }

/*
 *  Parse the input function.
 */
    dg_pfun ( carr1, iret );
    if ( *iret != 0 ) {
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }

/*
 *  Compute the output grid.
 */
    itype = 2;
    dg_driv ( &itype, iret );
    if ( *iret != 0 ) {
	strcpy ( pfunc, _dgerr.errst );
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }

/*
 *  Retrieve the output grids from the stack.
 */
    num = _dgstck.istack[0];
    numu = num / 100;
    numv = num - numu * 100;
    if ( ( _dgstck.itop != 0 ) || ( numu <= 0 ) || ( numv <= 0 ) ) {
	*iret = -5;
	dg_esub ( &zero, &zero, &zero, &zero, &ier );
	return;
    }

    if ( carr2[0] != '\0' ) {
/*
 *  Falsify the SAVFLG for grids also having the name
 *  stored in CARR2.
 */
	for ( i = 0; i < _dggrid.maxdgg; i++ ) {
	    strcpy ( ctst, "U" );
	    strcat ( ctst, carr2 );
	    if ( strcmp ( _dggrid.gparmd[i], ctst ) == 0 ) _dggrid.savflg[i] = G_FALSE;
	    strcpy ( ctst, "V" );
	    strcat ( ctst, carr2 );
	    if ( strcmp ( _dggrid.gparmd[i], ctst ) == 0 ) _dggrid.savflg[i] = G_FALSE;
	}

	_dggrid.savflg[numu-1] = G_TRUE;
	_dggrid.savflg[numv-1] = G_TRUE;
	strcpy ( _dggrid.gparmd[numu-1], "U" );
	strcat ( _dggrid.gparmd[numu-1], carr2 );
	strcpy ( _dggrid.gparmd[numv-1], "V" );
	strcat ( _dggrid.gparmd[numv-1], carr2 );
    }

/*
 *  Move u- and v- components into output arrays.
 */
    for ( i = 0; i < _dgfile.kxyd; i++ ) {
	ugrid[i] = _dggrid.dgg[numu-1].grid[i];
	vgrid[i] = _dggrid.dgg[numv-1].grid[i];
    }

/*
 *  Get output variables.
 */
    *igx = _dgfile.kxd;
    *igy = _dgfile.kyd;
    strcpy ( time1, _dggrid.dttimd1[numu-1] );
    strcpy ( time2, _dggrid.dttimd2[numu-1] );
    *level1 = _dggrid.leveld1[numu-1];
    *level2 = _dggrid.leveld2[numu-1];
    *ivcord = _dggrid.ivcrdd[numu-1];
    strcpy ( parmu, _dggrid.gparmd[numu-1] );
    strcpy ( parmv, _dggrid.gparmd[numv-1] );
    if ( strcmp ( parmu, "UOBS" ) == 0 ) strcpy ( parmu, "UREL" );
    if ( strcmp ( parmv, "VOBS" ) == 0 ) strcpy ( parmv, "VREL" );

/*
 *  Free the internal grid.
 */
    dg_esub ( &zero, &zero, &zero, &zero, &ier );

    return;
}
