package gov.noaa.nws.ncep.viz.gempak.grid.jna;

import com.sun.jna.Callback;
import com.sun.jna.LastErrorException;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.FloatByReference;

/**
 * The Jempak class contains JNA (Java Native Access) interface functions 
 * for GEMPAK grid diagnosis.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer    	Description
 * ------------ ----------	----------- 	--------------------------
 * 3/2009 		168			T. Lee			Initial creation
 * 10/2010		168			mgamazaychikov	Added prototypes db_wsetnavtime_, 
 * 											db_wsetserver_, dg_fall_
 * 11/2010		168			mgamazaychikov	Added db_init_
 * 11/2010		168			mgamazaychikov	Added db_wsetenstime_
 * 11/2010					mgamazaychikov	Added gdc_open_, grc_rnav_
 * 03/2011		168			mgamazaychikov	add MyCallback interface, prototypes for db_callback,
 * 											db_returndata
 * 03/2011		168			mgamazaychikov	Removed setProtected(true) - was causing JVM to crash.
 * 05/2011		168			mgamazaychikov	Renamed db_callback to db_dataCallback, added 
 * 											db_navCallback, db_returnnav for future use.
 * 06/2011		168			mgamazaychikov	Renamed MyCallback to DbCallbackWithMessage,
 * 											added DbCallbackWithoutMessage, added 
 * 											db_duriCallback, db_diagCallback, db_returnduri
 * 09/2011					mgamazaychikov	Added ctb_dtpath_, ctb_dttmpl, cfl_mnam_,
 * 											db_ensmCallback and db_returnensm prototypes
 * 10/2011					mgamazaychikov	Added db_seta2dtinfo_ prototype, removed db_wsetenstime_
 * 											prototype and renamed db_ensmCallback and db_returnensm prototypes
 * 											to db_flnmCallback and db_returnflnm
 * 12/12/2011               X. Guo          Changed navigation call-back function
 * </pre>
 *
 * @author tlee
 * @version 1.0
 */

public class GridDiag {

	/** 
	 * Setting up Singleton 
	 * */
	private static GridDiag instance = null;
	public static GridDiag getInstance() {
		if (instance == null) {
			instance = new GridDiag();	
		}
		return instance;
	}

	/*
	 * Constructor
	 */
    public gempak gem;

    private GridDiag(){
//    	gem = gempak.SYNC_INSTANCE;
    	gem = gempak.INSTANCE;
    }

 
	public interface gempak extends Library {
		
		/*
		 * callbacks
		 */
		public interface DbCallbackWithMessage extends Callback  {
			public boolean callback(String msg);// throws LastErrorException;
		}
		
		public interface DbCallbackWithoutMessage extends Callback {
			public boolean callback();// throws LastErrorException;
		}

		public int db_dataCallback(DbCallbackWithMessage callback);// throws LastErrorException ;
		public int db_navCallback(DbCallbackWithMessage callback);// throws LastErrorException ;
		public int db_duriCallback(DbCallbackWithMessage callback);// throws LastErrorException ;
		public int db_diagCallback(DbCallbackWithMessage callback);// throws LastErrorException ;
		public void db_fhrsCallback(DbCallbackWithoutMessage fhrsCallback);// throws LastErrorException ;
		public int db_flnmCallback(DbCallbackWithMessage callback);// throws LastErrorException ;
		
		public void db_returndata( float [] data, IntByReference nData);// throws LastErrorException ;		
		public void db_returnnav( String navigationStr );// throws LastErrorException ;
		public void db_returnduri( String datauriStr );// throws LastErrorException ;		
		public void db_returnfhrs(String cycleFcstHrsString);// throws LastErrorException ;
		public void db_returnflnm( String ensmStr );// throws LastErrorException ;
		
		public int db_subgCrsCallback(DbCallbackWithMessage callback);// throws LastErrorException ;

		/** Native library declaration and usage. */
		
		gempak INSTANCE = (gempak) Native.loadLibrary("gempak", gempak.class);
		gempak SYNC_INSTANCE = (gempak) Native.synchronizedLibrary(INSTANCE);

		/** 
		 * Fortran functions
		 */
		
		public void in_bdta_(IntByReference iret);
		public void gg_init_(IntByReference mode, IntByReference iret);
		public void gd_init_(IntByReference iret);
		public void gg_sdev_(String device, IntByReference iret);
		public void db_init_(IntByReference iret);

		/*
		 * C functions
		 */
		public void dg_fall_(IntByReference iret);
		public void dg_intl_(IntByReference iret);
		public void dg_kxky_ (IntByReference kx, IntByReference ky, IntByReference iret);
		
		public void dgc_fixa_(String area, String proj, byte[] areout, byte[] prjout, IntByReference iret);
		public void dgc_nfil_(String gdfile, String gdoutf, IntByReference iret);
		public void dgc_ndtm_(String gdatim, IntByReference iret);
		public void dgc_ntim_(IntByReference chngnv, IntByReference coladd, byte[] time1, byte[] time2, 
				IntByReference gottm, IntByReference iret);
		public void dgc_grid_ (byte[] time, String glevel, String gvcord, String gfunc, byte[] pfunc, 
				float[] grid, IntByReference kx, IntByReference ky, byte[] time1, byte[] time2, 
				IntByReference level1, IntByReference level2, IntByReference ivcord, byte[] parm, 
				IntByReference iret);// throws LastErrorException;
		public void dgc_vecr_ (byte[] gdattm, String glevel, String gvcord, String gvect, byte[] pfunc,
				float[] ugrid, float[] vgrid, IntByReference igx, IntByReference igy, byte[] time1,
				byte[] time2, IntByReference level1, IntByReference level2, IntByReference ivcord,
				byte[] parmu, byte[] parmv, IntByReference iret);// throws LastErrorException;
		public void dgc_subg_(String skip, IntByReference maxgrid, IntByReference ix1, IntByReference iy1, 
				IntByReference ix2, IntByReference iy2, IntByReference iret);
		
		public void gdc_open_ (String gdfile, IntByReference wrtflg, IntByReference mxanl, 
				IntByReference mxnav, IntByReference iacss, 
				float[] anl, float[] rnav, IntByReference msxgrd, IntByReference iret);
		
		public void grc_sscl (IntByReference scale, IntByReference kx, IntByReference ky, IntByReference imin,
				IntByReference jmin, IntByReference imax, IntByReference jmax, float[] grid, 
				FloatByReference rmin, FloatByReference rmax, IntByReference iret );
		public void grc_rnav_(float[] rnav, byte[] cproj, IntByReference kx, 
				IntByReference ky, IntByReference iret);
		
		public void gdc_gcyc_ (String anAlias, byte[] cycles,
				IntByReference iret);
		
		public void gdc_gtmf_ (String anAlias, String cycle, byte[] availableTimes,
				IntByReference iret);
		
		public void ctb_dtpath_(String anAlias, byte[] thePath,
				IntByReference iret);
		
		public void ctb_dttmpl_(String anAlias, byte[] theTemplate,
				IntByReference iret);
		
		public void cfl_mnam_(String dbtimeToDattim, String fileNameTemplate,
				byte[] theFileName, IntByReference iret);
		
		public void er_gerrmsg_ (IntByReference index, byte[]msg, IntByReference iret);
		public void er_gnumerr_ (IntByReference numerr, IntByReference iret);
		public void inc_outt_ (String output, String def, IntByReference termflg, IntByReference fileflg, String filnam,
				IntByReference iret);
		
		public void db_seta2dtinfo_ (String alias, String path, String template,
				IntByReference iret);
		
		public void db_setsubgnav_ (float lllat, float lllon, float urlat, float urlon,IntByReference iret );
		/*
		 * Wrapper functions
		 */
		public void db_wsetevtname_ (String anEvent, IntByReference iret);
		public void db_wsetnavtime_ (String aTime, IntByReference iret);
		public void inc_scal (String scale, IntByReference iscale, IntByReference iscalv, IntByReference iret);
		public void ggc_maps (byte[] proj, byte[] garea, String imgfil, IntByReference idrpfl, IntByReference iret);
		public void tgc_dual (byte[] time1, byte[] time2, byte[] time, IntByReference iret);
		public void erc_wmsg (String errgrp, IntByReference numerr, String errstr, IntByReference iret);
		
		public void init_driver( IntByReference iret );
		
	}
}