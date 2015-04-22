#define DG_GLOBAL
#include "dg.h"
#undef DG_GLOBAL

void dg_intl ( int *iret )
/************************************************************************
 * dg_intl								*
 *									*
 * This subroutine initializes the grid diagnostics package common	*
 * blocks.  This is a one-time initialization.  DG_INTL must be		*
 * called at the beginning of any program that uses grid diagnostic	*
 * functions.								*
 *									*
 * dg_intl ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		 5/03	Written to do one-time initialization	*
 *				only from application level		*
 * R. Tian/SAIC		10/03	Added init for /NFILE/             	*
 * R. Tian/SAIC		 2/04	Removed gdfprv and gdoprv		*
 * K. Brill/HPC		 2/04	Remove nuflg argument			*
 * R. Tian/SAIC          3/04   Modified to use new GD file management  *
 * R. Tian/SAIC		 5/04	Added init for /HINTRP/             	*
 * T. Lee/SAIC		12/04	Removed crtmem; added mbrnum, iensmb	*
 * T. Lee/SAIC		 3/05	Initialized layer diagnostic		*
 * R. Tian/SAIC		 1/06	Recoded from Fortran			*
 ************************************************************************/
{
    int i;
/*----------------------------------------------------------------------*/
    *iret  = 0;

    /*
     * Initialize layer diagnostic package.
     */
    dl_init ( iret );

    /*
     * Initialize the internal grid storage common block.
     */
    _dggrid.idglst = 0;
    for ( i = 0; i< NDGRD; i++ ) {
	_dggrid.dgg[i].grid = NULL;
	_dggrid.dgg[i].size = 0;
	_dggrid.dttimd1[i][0] = '\0';
	_dggrid.dttimd2[i][0] = '\0';
	_dggrid.leveld1[i] = 0;
	_dggrid.leveld2[i] = 0;
	_dggrid.ivcrdd[i] = 0;
	_dggrid.gparmd[i][0] = '\0';
	_dggrid.ifiled[i] = 0;
	_dggrid.iusesv[i] = 0;
	_dggrid.savflg[i] = G_FALSE;
	_dggrid.iensmb[i] = 0;
    }
    _dggrid.maxdgg = 0;
    _dggrid.isubid = 0;

    /*
     * Initialize the common block used to store user input.
     */
    _dginpt.ddttim1[0] = '\0';
    _dginpt.ddttim2[0] = '\0';
    _dginpt.ldlevl1 = 0;
    _dginpt.ldlevl2 = 0;
    _dginpt.lvcord = 0;
    _dginpt.ingdtm[0] = '\0';
    _dginpt.inglev[0] = '\0';
    _dginpt.invcrd[0] = '\0';

    /*
     * Initialize the common block used to store the table made by
     * parsing GFUNC or GVECT.
     */
    _dgtabl.ltabl = -1;
    for ( i = 0; i < LENTC; i++ ) {
	_dgtabl.ctabl[i][0] = '\0';
	_dgtabl.clevel[i][0] = '\0';
	_dgtabl.cvcord[i][0] = '\0';
	_dgtabl.cgdttm[i][0] = '\0';
	_dgtabl.icflnm[i] = 0;
	_dgtabl.nfargs[i] = 0;
    }

    /*
     * Initialize the common block for the computational stack.
     */
    _dgstck.itop = -1;
    for ( i = 0; i < LNSTK; i++ ) {
	_dgstck.stack[i][0] = '\0';
	_dgstck.istack[i] = 0;
	_dgstck.icpntr[i] = 0;
    }

    /*
     * Initialize the common block for the GDFILE entries.
     */
    _dgfile.dgset = G_FALSE;
    _dgfile.idlun = 0;
    for ( i = 0; i < MMFILE; i++ ) {
	_dgfile.idflnm[i] = 0;
	_dgfile.gdcur[i][0] = '\0';
	_dgfile.tmpflg[i] = G_FALSE;
	_dgfile.templt[i][0] = '\0';
	_dgfile.tdattm[i][0] = '\0';
	_dgfile.tfirst[i][0] = '\0';
	_dgfile.tlast[i][0]  = '\0';
    }
    for ( i = 0; i < LLNNAV; i++ ) {
	_dgfile.snav[i] = 0.0;
    }
    _dgfile.kxd = 0;
    _dgfile.kyd = 0;
    _dgfile.kxyd = 0;
    _dgfile.idglat = 0;
    _dgfile.idglon = 0;
    _dgfile.cprj[0] = '\0';
    for ( i = 0; i < LLGDHD; i++ ) {
	_dgfile.ighdr[i] = 0;
    }
    _dgfile.anglr1 = RMISSD;
    _dgfile.anglr2 = RMISSD;
    _dgfile.anglr3 = RMISSD;
    _dgfile.concon = RMISSD;
    _dgfile.addcol = G_FALSE;

    /*
     * Initialize pointers to map scale factors and the grid spacing.
     */
    _mapscl.ixmscl = 0;
    _mapscl.iymscl = 0;
    _mapscl.ixmsdy = 0;
    _mapscl.iymsdx = 0;
    _mapscl.gddx = RMISSD;
    _mapscl.gddy = RMISSD;

    /*
     * Initialize HORIZON projection.
     */
    _dgstrm.rot = RMISSD;
    _dgstrm.sinclt = RMISSD;
    _dgstrm.cosclt = RMISSD;
    _dgstrm.clon = RMISSD;

    /*
     * Initialize diagnostic error string.
     */
    _dgerr.errst[0] = '\0';

    /*
     * Initialize orientation angle.
     */
    _dgovec.ornang = RMISSD;

    /*
     * Initialize the origin for M calculation.
     */
    _dgorig.orglat = RMISSD;
    _dgorig.orglon = RMISSD;
    _dgorig.orgxpt = RMISSD;
    _dgorig.orgypt = RMISSD;

    /*
     * Initialize the area subset common block.
     */
    _dgarea.kgxmin = 0;
    _dgarea.kgxmax = 0;
    _dgarea.kgymin = 0;
    _dgarea.kgymax = 0;
    _dgarea.kextnd = 0;
    _dgarea.jgxmin = 0;
    _dgarea.jgxmax = 0;
    _dgarea.jgymin = 0;
    _dgarea.jgymax = 0;
    _dgarea.ksub1 = 0;
    _dgarea.ksub2 = 0;

    /*
     * Initialize subset internal grid common block.
     */
    for ( i = 0; i < LLNNAV; i++ ) {
	_dgsubg.refnav[i] = 0.0;
    }
    _dgsubg.gwrapg = G_FALSE;
    _dgsubg.dgsubg = G_FALSE;
    _dgsubg.ishift = 0;
    _dgsubg.jsgxmn = 0;
    _dgsubg.jsgxmx = 0;
    _dgsubg.jsgymn = 0;
    _dgsubg.jsgymx = 0;
    _dgsubg.jsgxsk = 0;
    _dgsubg.jsgysk = 0;

    /*
     * Initialize pointers to rotation sines and cosines.
     */
    _dgrtwd.irtcos = 0;
    _dgrtwd.irtsin = 0;

    /*
     * Initialize pointer to land-sea mask.
     */
    _dglndc.lndsea = 0;

    /*
     * Initialize /NFILE/ common block.
     */
    for ( i = 0; i < NGDFLS; i++ ) {
	_nfile.ntmplt[i][0] = '\0';
	_nfile.gflpth[i][0] = '\0';
	_nfile.crtfnm[i][0] = '\0';
	_nfile.aftrbr[i][0] = '\0';
	_nfile.crtgdt1[i][0] = '\0';
	_nfile.crtgdt2[i][0] = '\0';
	_nfile.outflg[i] = G_FALSE;
	_nfile.mbrnum[i] = 0;
    }
    for ( i = 0; i < LLMXGT; i++ ) {
	_nfile.dtmlst1[i][0] = '\0';
	_nfile.dtmlst2[i][0] = '\0';
    }
    _nfile.ntmlst = 0;
    _nfile.itmlst = 0;
    _nfile.irefnv = 0;
    _nfile.nucode = G_FALSE;

    /*
     * Initialize /HINTRP/ common block.
     */
    for ( i = 0; i < LLNNAV; i++ ) {
	_hintrp.tfrnav[i] = 0.0;
    }
    _hintrp.igrxig = 0;
    _hintrp.igryig = 0;
    _hintrp.isnrot = 0;
    _hintrp.icsrot = 0;
    _hintrp.wndrot = G_FALSE;
    _hintrp.adcltg = G_FALSE;
    _hintrp.gwrptg = G_FALSE;
    _hintrp.didint = G_FALSE;

    return;
}
