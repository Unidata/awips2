#include "geminc.h"
#include "gemprm.h"
#include "dbcmn.h"

#include "dg.h"
#include <sys/timeb.h>

#define COMPAR(xx,yy)	( G_ABS ( (xx) - (yy) ) < RDIFFD )
#define LLMXTH 1000000

void dgc_subg ( const char *ijskip, int *maxgrid, int *imll, int *jmll, 
                int *imur, int *jmur, int *iret )
/************************************************************************
 * dgc_subg								*
 *									*
 * This subroutine sets the internal subset grid given the reference	*
 * grid navigation set in GPLT and the map projection set in GPLT.	*
 * If the reference grid is globe wrapping with the addition of an	*
 * extra grid column, then the navigation set in GPLT must be that for	*
 * the grid with the extra column.					*
 * 									*
 * The subset grid is larger by five grid points than that strictly	*
 * needed to cover the map projection area.  This extension permits	*
 * more accurate computation of derivatives.  The subset grid relative	*
 * coordinates of the region strictly needed for the map are returned.	*
 * 									*
 *									*
 * IJSKIP is parsed by IN_GSKP.  IJSKIP information is entered using	*
 * the following format, where items in square brackets are optional:	*
 *									*
 *	IJSKIP = Iskip[;Istart][;Iend][/Jskip[;Jstart][;Jend]],		*
 *									*
 *	IJSKIP=Y[ES], or IJSKIP=N[O]					*
 *									*
 * The following rules apply in using IJSKIP input:			*
 *									*
 * 1.  If only Iskip is entered, then I and J skips are Iskip.  The	*
 *     beginning points and ending points are determined by querying	*
 *     the display projection to find the area on the reference grid	*
 *     needed to cover it.						*
 *									*
 * 2.  If any bounding value is omitted, it is determined automatically *
 *     by querying the display projection as in 1 above.		*
 *									*
 * 3.  If IJSKIP is blank or NO, skipping is not used to determine the	*
 *     internal grid navigation.					*
 * 									*
 * 4.  If IJSKIP is YES, all skip parameters are determined		*
 *     automatically.							*
 * 									*
 * dgc_subg ( ijskip, maxgrid, imll, jmll, imur, jmru, iret )		*
 *									*
 * Input parameters:							*
 *	*ijskip		const char	User input for skip subsetting	*
 *	*maxgrid	int		Maximum grid size               *
 *									*
 * Output parameters:							*
 *	*IMLL		int		Lower left map I bound		*
 *	*JMLL		int		Lower left map J bound		*
 *	*IMUR		int		Upper right map I bound		*
 *	*JMUR		int		Upper right map J bound		*
 * 	*IRET		int		Return code			*
 *					  0 = normal return		*
 *					-37 = no ref grid navigation set*
 *					-38 = glb wrap grd inconsistency*
 *					-39 = map projection is not set *
 *					-40 = subset grd bound error	*
 *					-41 = subset grid is too big	*
 *					-43 = cannot rearrange grid	*
 *					-44 = error set subset grid nav	*
 *					-48 = both I bounds required	*
 **									*
 * Log:									*
 * K. Brill/HPC		08/02						*
 * K. Brill/HPC		 9/02	Also initialize gparmd () to blank	*
 * S. Jacobs/NCEP	11/02	Added check for current nav vs saved nav*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * K. Brill/HPC		12/02	Use IJSKIP input for subset by skipping *
 * R. Tian/SAIC		 3/04	Add check for outflg			*
 * R. Tian/SAIC		 5/04	Added call to DG_CONE			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * S. Gilbert/NCEP	 5/07	Added maxgrid argument                  *
 ************************************************************************/
{
    char gprj[5], cnum[5];
    float aglt1, agln1, aglt2, agln2, ag1, ag2, ag3, rimn, rjmn, rimx,
        rjmx, rglt[2], rgln[2], tnav[LLNNAV];
    double a, b, c;
    int lmx, mx, my, imn, jmn, imx, jmx, nx, ny, ix1, ix2, nsx, iy1, iy2,
        nsy, n, idx, idy, ichk, iadlx, iadly, iadrx, iadry, kxsg, kysg,
	kxysg, nu, mxnu, imn2, jmn2, imx2, jmx2, iadd, navsz;
    int nc, tobig, autos, angflg, navflg, done, ishf, ier, ierr, iir, i, k;

   /*
    * timing vars
    */
    struct   timeb t_gsgprj1, t_gsgprj2, t_gsgprj3, t_gqgprj1, t_gqgprj2, 
             t_gsgprj4, t_setr, t_gqbnd, t_gskp, t_gtrans1, t_mnav, t_cnav, 
             t_cone, t_current;
/*----------------------------------------------------------------------*/
    *iret = 0;
    _dgsubg.dgsubg = G_TRUE;

    for ( i = 0; i < NGDFLS; i++ ) {
	if ( _nfile.outflg[i] == G_TRUE ) {
	    *iret = -63;
	    return;
	}
    }

    /*
     * Set LMX to maximum allowed threshold for ijskip=yes
     */
    lmx = LLMXTH;

    /*
     * Set the reference grid navigation in GPLT.
     */
    cst_itos ( (int *)(&_dgsubg.refnav[1]), 1, &nc, gprj, &ier );
    cst_rmbl ( gprj, gprj, &nc, &ier );
    mx = G_NINT ( _dgsubg.refnav[4] );
    my = G_NINT ( _dgsubg.refnav[5] );
    agln1 = _dgsubg.refnav[7];
    if ( _dgfile.addcol == G_TRUE ) {
	mx += 1;
	agln2 = _dgsubg.refnav[7];
    } else {
	agln2 = _dgsubg.refnav[9];
    }
    ftime(&t_gsgprj1);
    gsgprj ( gprj, &_dgsubg.refnav[10], &_dgsubg.refnav[11], 
             &_dgsubg.refnav[12], &mx, &my, &_dgsubg.refnav[6],
	     &_dgsubg.refnav[7], &_dgsubg.refnav[8], &agln2, &ier,
	     strlen(gprj) );
    ftime(&t_current);
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ierr, strlen("GEMPLT"), strlen(" ") );
	*iret = -37;
	return;
    } else if ( _dgsubg.gwrapg == G_TRUE &&
        ( ! COMPAR ( agln1, agln2 ) && ! COMPAR ( (agln1+360.), agln2 ) ) ) {
	*iret = -38;
	return;
    }

    /*
     * Get the shift for re-arranging any globe wrapping grid.
     * ISHIFT is stored in DGCMN.CMN.
     */
    ftime(&t_setr);
    grc_setr ( &mx, &my, &_dgsubg.ishift, &ier );
    ftime(&t_current);
    if ( ier == -22 ) {
	*iret = -39;
	return;
    } else if ( ier != 0 ) {
	*iret = -43;
	return;
    }
    ftime(&t_gqgprj1);
    gqgprj ( gprj, &ag1, &ag2, &ag3, &mx, &my, &aglt1, &agln1, &aglt2,
             &agln2, &ier, sizeof(gprj) );
    ftime(&t_current);
    gprj[4] = '\0';
    cst_lstr ( gprj, &nc, &ier );
    gprj[nc] = '\0';

    /*
     * Get the grid index bounds for the subset grid.
     */
    ftime(&t_gqbnd);
    gqbnd ( sys_G, &rimn, &rjmn, &rimx, &rjmx, &ier, strlen(sys_D) );
    ftime(&t_current);
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ierr, strlen("GEMPLT"), strlen(" ") );
	*iret = -40;
	return;
    }
    imn = (int)rimn;
    jmn = (int)rjmn;
    imx = G_NINT ( rimx + .5 );
    if ( G_DIFFT((float)((int)rimx), rimx, GDIFFD) ) imx = (int)rimx;
    jmx = G_NINT ( rjmx + .5 );
    if ( G_DIFFT((float)((int)rjmx), rjmx, GDIFFD) ) jmx = (int)rjmx;
    if ( imn < 1 ) imn = 1;
    if ( jmn < 1 ) jmn = 1;
    if ( imx > mx ) imx = mx;
    if ( jmx > my ) jmx = my;
    nx = imx - imn + 1;
    ny = jmx - jmn + 1;
    if ( nx * ny > lmx ) {
        tobig = G_TRUE;
    } else {
        tobig = G_FALSE;
    }

    /*
     * Check for subsetting by skipping.
     *
     * The bounds are returned from IN_GSKP as IMISSD if
     * not provided.  The skip value returned is converted
     * to a stride value by adding one, i.e. IDX=1 means
     * no skipping, IDX=2 means skip one point.
     *
     * The mathematical relationship stating that the
     * original number of grid points from IMN to IMX must
     * equal the number of points skipped plus the number
     * kept is this:
     *
     *	    (IMX - IMN + 1) = N + (N - 1) * nskip
     *
     * where N is the number of points remaining after
     * skipping and nskip is the number of points skipped
     * between the points that are kept.
     *
     * This equation appears a number of times in various
     * forms below.
     */
    ftime(&t_gskp);
    in_gskp ( ijskip, &ix1, &ix2, &nsx, &iy1, &iy2, &nsy, &autos, &ier );
    ftime(&t_current);
    if ( ier != 0 ) {
	er_wmsg ( "IN", &ier, " ", &iir, strlen("IN"), strlen(" ") );
	*iret = -40;
	return;
    }
    if ( ix2 > mx ) {
	ier = -49;
	er_wmsg ( "DG", &ier, "I", &iir, strlen("DG"), strlen("I") );
	*iret = -40;
	return;
    } else if ( iy2 > my ) {
	ier = -49;
	er_wmsg ( "DG", &ier, "J", &iir, strlen("DG"), strlen("J") );
	*iret = -40;
	return;
    }
    if ( autos == G_TRUE && tobig == G_TRUE ) {
	a = (double)( lmx - 1 );
	b = (double)( nx + ny - 2 * lmx );
	c = (double)( lmx - nx * ny );
	n = (int)( ( b + sqrt ( b * b - 4. * a * c ) ) / ( 2. * a ) );
	nsx = n + 1;
	nsy = nsx;
	cst_inch ( nsx, cnum, &ier );
	ier = 7;
	er_wmsg ( "DG", &ier, cnum, &iir, strlen("DG"), strlen(cnum) );
    }
    idx = nsx + 1;
    idy = nsy + 1;
    if ( nsx > 0 ) {
	ichk = nx / nsx;
	if ( ichk <= 4 ) {
	    ier = 6;
	    er_wmsg ( "DG", &ier, "I", &iir, strlen("DG"), strlen("I") );
	}
    }
    if ( nsy > 0 ) {
	ichk = ny / nsy;
	if ( ichk <= 4 ) {
	    ier = 6;
	    er_wmsg ( "DG", &ier, "J", &iir, strlen("DG"), strlen("J") );
	}
    }

    /*
     * Extend the grid bounds if possible.
     */
    iadlx = 0;
    iadly = 0;
    iadrx = 0;
    iadry = 0;
    imn2 = imn;
    jmn2 = jmn;
    imx2 = imx;
    jmx2 = jmx;
    iadd = 0;
    done = G_FALSE;
    while ( done == G_FALSE && iadd < 5 ) {
	iadd += 1;
	if ( imn2 > idx ) {
	    imn2 -= idx;
	    iadlx += idx;
	}
	if ( jmn2 > idy ) {
	    jmn2 -= idy;
	    iadly += idy;
	}
	if ( imx2 < ( mx - idx ) ) {
	    imx2 += idx;
	    iadrx += idx;
	}
	if ( jmx2 < ( my - idy ) ) {
	    jmx2 += idy;
	    iadry += idy;
	}
	kxsg = G_NINT ( (float)( imx2 - imn2 + 1 + nsx ) / (float)( 1 + nsx ) );
	kysg = G_NINT ( (float)( jmx2 - jmn2 + 1 + nsy ) / (float)( 1 + nsy ) );
	kxysg = kxsg * kysg;
	if ( (kxysg > *maxgrid) && (*maxgrid != IMISSD) ) {
	    done = G_TRUE;
	    if ( imn != imn2 ) {
		imn = imn2 + idx;
		iadlx -= idx;
	    }
	    if ( jmn != jmn2 ) {
		jmn = jmn2 + idy;
		iadly -= idy;
	    }
	    if ( imx != imx2 ) {
		imx = imx2 - idx;
		iadrx -= idx;
	    }
	    if ( jmx != jmx2 ) {
		jmx = jmx2 - idy;
		iadry -= idy;
	    }
	} else {
	    imn = imn2;
	    jmn = jmn2;
	    imx = imx2;
	    jmx = jmx2;
	}
    }

    /*
     * Adjust extend margins using the stride values.
     */
    iadlx = iadlx / idx;
    iadrx = iadrx / idx;
    iadly = iadly / idy;
    iadry = iadry / idy;

    /*
     * Set the I dimension extraction bounds.  No shifting
     * is done if the user provides these bounds.  No
     * extend region is allowed if user provides bounds.
     */
    ishf = _dgsubg.ishift;
    if ( ix1 > 0 ) {
	_dgsubg.ishift = 0;
	iadlx = 0;
	imn = ix1;
    }
    if ( ix2 > 0 ) {
	_dgsubg.ishift = 0;
	iadrx = 0;
	imx = ix2;
    }
    if ( ishf != _dgsubg.ishift ) {
	if ( ix1 < 0 || ix2 < 0 ) {
	    *iret = -48;
	    return;
	}

	/*
	 * Reset the grid projection in GPLT.
	 */
	mx = G_NINT ( _dgsubg.refnav[4] );
	my = G_NINT ( _dgsubg.refnav[5] );
	agln1 = _dgsubg.refnav[7];
	if ( _dgfile.addcol == G_TRUE ) {
	    mx += 1;
	    agln2 = _dgsubg.refnav[7];
	} else {
	    agln2 = _dgsubg.refnav[9];
	}
    ftime(&t_gsgprj2);
	gsgprj ( gprj, &_dgsubg.refnav[10], &_dgsubg.refnav[11], 
	    &_dgsubg.refnav[12], &mx, &my, &_dgsubg.refnav[6],
	    &_dgsubg.refnav[7], &_dgsubg.refnav[8], &agln2, &ier,
	    strlen(gprj) );
    ftime(&t_current);
    ftime(&t_gqgprj2);
	gqgprj ( gprj, &ag1, &ag2, &ag3, &mx, &my, &aglt1, &agln1, 
	    &aglt2, &agln2, &ier, sizeof(gprj) );
    ftime(&t_current);
    if ( diagClbkPtr != NULL )
	gprj[4] = '\0';
	cst_lstr ( gprj, &nc, &ier );
	gprj[nc] = '\0';
	ierr = 5;
	er_wmsg ( "DG", &ierr, " ", &ier, strlen("DG"), strlen(" ") );
    }

    /*
     * Adjust IMX and IMN for skipping.
     */
    if ( idx > 1 ) {
	nu = G_NINT ( (float)( imx - imn + 1 + nsx ) / (float)( 1 + nsx ) );
	mxnu = nu * ( 1 + nsx ) + imn - 1 - nsx;
	if ( mxnu > ( mx - idx ) && mxnu != ix2 ) {
	    mxnu = mx;
	    imn = mxnu - nu * ( 1 + nsx ) + 1 + nsx;
	    if ( imn < 1 ) {
		/*
		 * Start at 1 when full range is needed.
		 */
		imn = 1;
		nu = ( mxnu - imn + 1 + nsx ) / ( 1 + nsx );
		mxnu = nu * ( 1 + nsx ) + imn - 1 - nsx;
	    }
	}
	imx = mxnu;
	if ( ( ix2 > 0 && imx != ix2 ) || 
	     ( ix1 > 0 && imn != ix1 ) ) {
	    ierr = 4;
	    er_wmsg ( "DG", &ierr, "I", &ier, strlen("DG"), strlen("I") );
	}
    }

    /*
     * Set the J dimension extraction bounds. No extend
     * region is allowed if user provides bounds.
     */
    if ( iy1 > 0 ) {
	iadly = 0;
	jmn = iy1;
    }
    if ( iy2 > 0 ) {
	iadry = 0;
	jmx = iy2;
    }

    /*
     * Adjust JMX and JMN for skipping.
     */
    if ( idy > 1 ) { 
	nu = G_NINT ( (float)( jmx - jmn + 1 + nsy ) / (float)( 1 + nsy ) );
	mxnu = nu * ( 1 + nsy ) + jmn - 1 - nsy;
	if ( mxnu > ( my - idy ) && mxnu != iy2 ) {
	    mxnu = my;
	    jmn = mxnu - nu * ( 1 + nsy ) + 1 + nsy;
	    if ( jmn < 1 ) {
		/*
		 * Start at 1 when full range is needed.
		 */
		jmn = 1;
		nu = ( mxnu - jmn + 1 + nsy ) / ( 1 + nsy );
		mxnu = nu * ( 1 + nsy ) + jmn - 1 - nsy;
	    }
	}
	jmx = mxnu;
	if ( ( iy2 > 0 && jmx != iy2 ) || ( iy1 > 0 && jmn != iy1 ) ) {
	    ierr = 4;
	    er_wmsg ( "DG", &ierr, "J", &ier, strlen("DG"), strlen("J") );
	}
    }

    /*
     * Compute subset grid final dimensions.
     */
    kxsg = ( imx - imn + 1 + nsx ) / ( 1 + nsx );
    kysg = ( jmx - jmn + 1 + nsy ) / ( 1 + nsy );
    if ( kxsg <= 0 || kysg <= 0 ) {
	*iret = -40;
	return;
    }
    kxysg = kxsg * kysg;
	

    /*
     * Set common block subset coordinates on reference grid.
     */
    _dgsubg.jsgxmn = imn;
    _dgsubg.jsgymn = jmn;
    _dgsubg.jsgxmx = imx;
    _dgsubg.jsgymx = jmx;
    _dgsubg.jsgxsk = idx;
    _dgsubg.jsgysk = idy;

    /*
     * Set DG_HILO area bounds on subset grid.
     */
    _dgarea.kgxmin = iadlx + 1;
    _dgarea.kgymin = iadly + 1;
    _dgarea.kgxmax = kxsg - iadrx;
    _dgarea.kgymax = kysg - iadry;

    /*
     * Strict map bounds are same as above.
     */
    *imll = _dgarea.kgxmin;
    *jmll = _dgarea.kgymin;
    *imur = _dgarea.kgxmax;
    *jmur = _dgarea.kgymax;

    /*
     * Set the DGAREA common grid bounds calculation flag.
     */
    _dgarea.jgxmin = 1;
    _dgarea.jgxmax = kxsg;
    _dgarea.jgymin = 1;
    _dgarea.jgymax = kysg;
    _dgarea.ksub1 = 1;
    _dgarea.ksub2 = kxysg;

    /*
     * Compute grid size and maximum number of internal grids
     * for the common block.
     */
    if ( (kxysg > *maxgrid) && (*maxgrid != IMISSD) ) {
	/*
	 * Here is the future location to set up some other
	 * remapping.
	 */
	*iret = -41;
	return;
    }

    _dgfile.kxd = kxsg;
    _dgfile.kyd = kysg;
    _dgfile.kxyd = kxysg;
    _dggrid.maxdgg = NDGRD;

    /*
     * Compute the navigation of the internal (subset) grid.
     */
    strcpy ( _dgfile.cprj, gprj );
    rglt[0] = _dgsubg.jsgxmn;
    rgln[0] = _dgsubg.jsgymn;
    rglt[1] = _dgsubg.jsgxmx;
    rgln[1] = _dgsubg.jsgymx;
    nc = 2;
    ftime(&t_gtrans1);
    gtrans ( sys_G, sys_M, &nc, rglt, rgln, rglt, rgln, &ier,
        strlen(sys_G), strlen(sys_M) );
    ftime(&t_current);
    if ( G_ABS ( rgln[0] - 180. ) < .01 || G_ABS ( rgln[0] + 180. ) < .01 )
        rgln[0] = -180.;
    if ( G_ABS ( rgln[1] - 180. ) < .01 || G_ABS ( rgln[1] + 180. ) < .01 )
        rgln[0] = 180.;
    if ( G_ABS ( rgln[0] - rgln[1]) < 0.01 )
        rgln[1] = rgln[0];
    ftime(&t_gsgprj3);
    gsgprj ( _dgfile.cprj, &ag1, &ag2, &ag3, &_dgfile.kxd, &_dgfile.kyd, 
        &rglt[0], &rgln[0], &rglt[1], &rgln[1], &ier, strlen(_dgfile.cprj) );
    ftime(&t_current);
    if ( ier != 0 ) {
	if ( _dgsubg.gwrapg == G_TRUE) {
	    ag2 += 180.;
	    if ( ag2 >= 360. ) ag2 -= 360.;
    ftime(&t_gsgprj4);
	    gsgprj ( _dgfile.cprj, &ag1, &ag2, &ag3, &_dgfile.kxd, &_dgfile.kyd,
		&rglt[0], &rgln[0], &rglt[1], &rgln[1], &ier,
		strlen(_dgfile.cprj) ) ;
    ftime(&t_current);
	if ( ier != 0 ) {
		*iret = -44;
		return;
	    }
	} else {
	    *iret = -44;
	    return;
	}
    }
    angflg = G_TRUE;
    ftime(&t_mnav);
    grc_mnav ( _dgfile.cprj, &_dgfile.kxd, &_dgfile.kyd, &rglt[0], &rgln[0],
	&rglt[1], &rgln[1], &ag1, &ag2, &ag3, &angflg, tnav, &ier );
    ftime(&t_current);

    /*
     * Check the current navigation against the saved navigation.
     * If they are different, then set the navigation flag to False.
     */
    navsz = LLNNAV;
    ftime(&t_cnav);
    grc_cnav ( tnav, _dgfile.snav, &navsz, &navflg, &ier );
    ftime(&t_current);

    /*
     * Save the current navigation.
     */
    for ( k = 0; k < LLNNAV; k++ ) {
	_dgfile.snav[k] = tnav[k];
    }

    db_retsubgcrs (_dgfile.cprj, _dgfile.kxd, _dgfile.kyd, rglt[0], rgln[0],
                       rglt[1], rgln[1],ag1, ag2, ag3,&ier); 
    /*
     * Set the constant of the cone for various projections (code
     * duplicated from UPDCON.FOR in GEMPLT).
     */
    _dgfile.anglr1 = ag1 * DTR;
    _dgfile.anglr2 = ag2 * DTR;
    _dgfile.anglr3 = ag3 * DTR;
    ftime(&t_cone);
    dg_cone ( _dgfile.cprj, &_dgfile.anglr1, &_dgfile.anglr3,
    	      &_dgfile.concon, iret );
    ftime(&t_current);

    /*
     * Set lat/lon,  map scale factor, and rotation matrix
     * internal grid pointers to zero.
     */
    _dgfile.idglat = 0;
    _dgfile.idglon = 0;
    _mapscl.ixmscl = 0;
    _mapscl.iymscl = 0;
    _mapscl.ixmsdy = 0;
    _mapscl.iymsdx = 0;
    _dgrtwd.irtcos = 0;
    _dgrtwd.irtsin = 0;
    _dglndc.lndsea = 0;

    /*
     * Initialize orientation angle.
     */
    _dgovec.ornang = RMISSD;

    /*
     * Free all existing grids since navigation is changed.
     */
    if ( navflg == G_FALSE ) {
        dg_fall ( &ier );
    }

    /*
     * Initialize the origin for M calculation.
     */
    _dgorig.orglat = RMISSD;
    _dgorig.orglon = RMISSD;
    _dgorig.orgxpt = RMISSD;
    _dgorig.orgypt = RMISSD;

    /*
     * Since there were no errors, set flag saying dg package has
     * been initialized.
     */
    _dgfile.dgset = G_TRUE;

    /*
     * Initialize the pointer in the internal grid arrays.
     */
    _dggrid.idglst = 0;

    return;
}
