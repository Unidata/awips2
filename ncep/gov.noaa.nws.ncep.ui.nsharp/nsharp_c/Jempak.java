package gov.noaa.nws.ncep.edex.gempak.jna;

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
 * Date			Ticket#		Engineer    Description
 * ------------ ----------	----------- --------------------------
 * 3/2009 		168			T. Lee		Initial creation
 * </pre>
 *
 * @author tlee
 * @version 1.0
 */

public class Jempak {
	
	/** 
	 * Setting up Singleton 
	 * */
	private static Jempak instance;
	public static Jempak getInstance() {
		if (instance == null) {
			instance = new Jempak();	
		}
		return instance;
	}

	/*
	 * Constructor
	 */
    public Igempak gempak;

    public Jempak(){
    	gempak = Igempak.INSTANCE;
    }

 
	public interface Igempak extends Library {

		/** Native library declaration and usage. */
		
		Igempak INSTANCE = (Igempak) Native.loadLibrary("gempak", Igempak.class);

		/** 
		 * Fortran functions
		 */
		public void in_bdta_(IntByReference iret);
		public void gg_init_(IntByReference mode, IntByReference iret);
		public void gd_init_(IntByReference iret);
		public void gg_sdev_(String device, IntByReference iret);

		/*
		 * C functions
		 */
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
				IntByReference iret);
		public void dgc_vecr_ (byte[] gdattm, String glevel, String gvcord, String gvect, byte[] pfunc,
				float[] ugrid, float[] vgrid, IntByReference igx, IntByReference igy, byte[] time1,
				byte[] time2, IntByReference level1, IntByReference level2, IntByReference ivcord,
				byte[] parmu, byte[] parmv, IntByReference iret);
		public void dgc_subg_(String skip, IntByReference maxgrid, IntByReference ix1, IntByReference iy1, 
				IntByReference ix2, IntByReference iy2, IntByReference iret);
		public void grc_sscl (IntByReference scale, IntByReference kx, IntByReference ky, IntByReference imin,
				IntByReference jmin, IntByReference imax, IntByReference jmax, float[] grid, 
				FloatByReference rmin, FloatByReference rmax, IntByReference iret );
		public void gdldta (String gdfile, byte[] time1, byte[] time2, IntByReference level1, IntByReference level2,
				IntByReference ivcord, byte[] parm, float grid[], IntByReference kx, IntByReference ky,
				String garea, IntByReference ix1, IntByReference iy1, IntByReference ix2, IntByReference iy2,
				IntByReference iscale, IntByReference termflg, IntByReference fileflg, String outfil, IntByReference iret);
		
		public void cst_lcuc (String area, String areout, IntByReference iret);
		public void er_gerrmsg_ (IntByReference index, byte[]msg, IntByReference iret);
		public void er_gnumerr_ (IntByReference numerr, IntByReference iret);
		public void inc_outt_ (String output, String def, IntByReference termflg, IntByReference fileflg, String filnam,
				IntByReference iret);
		
		/*
		 * Wrapper functions
		 */	
		public void inc_scal (String scale, IntByReference iscale, IntByReference iscalv, IntByReference iret);
		public void ggc_maps (byte[] proj, byte[] garea, String imgfil, IntByReference idrpfl, IntByReference iret);
		public void tgc_dual (byte[] time1, byte[] time2, byte[] time, IntByReference iret);
		public void erc_wmsg (String errgrp, IntByReference numerr, String errstr, IntByReference iret);
	}
}