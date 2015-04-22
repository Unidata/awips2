/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.G2GNativeLibrary
 * 
 * March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphtogrid;

//import org.apache.log4j.Logger;

import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * The G2GNativeLibrary class emulates the Native C and Fortran environment in
 * which graph-to-grid algorithm is run. JNA is used to make direct calls to C 
 * library methods, which wraps up the Fortran calls - so we do not need to call 
 * Fortran subroutines directly from Java.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/10  		#215     	J. Wu    	Initial creation
 * 06/10  		#215     	J. Wu    	Added support for Min/Max
 * 07/10  		#215     	J. Wu    	Added support for writing grid to
 * 											GEMPAK grid file
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 */

public class G2GNativeLibrary {
	
//	private final static Logger logger = Logger.getLogger( G2GNativeLibrary.class);
	
	/** Setting up Singleton */
    private static G2GNativeLibrary instance;

    /** Setting up Singleton */
    public static G2GNativeLibrary getInstance() {
        if ( instance == null) {
        	instance = new G2GNativeLibrary();
        }
        return instance;
    }
	
    /**
     * Setting up the G2G library to access c driver in libg2g.so 
     */
	public interface G2GNative extends Library {
		
	    G2GNative INSTANCE = (G2GNative) Native.loadLibrary("g2g", G2GNative.class);
	    
	    int g2g_driver( float[] grid, float[] hist, int kx, int ky, int nlines, 
	    		        int[] nlatlons, float[] latPts, float[] lonPts,
	    		        int[] nsmthpts, float[] smthLat, float[] smthLon,  
	    		        int[] nextpts, float[] extLat, float[] extlon,
	    		        float[] values, int[] ismth, int[] iclosed,
                        int mmnum, float[] mmlat, float[] mmlon,
                        float[] mmfi, float[] mmfj, float[] mmvalue,
	    		        String catmap, String hstgrd,  
	    		        String discrete, String dlines, 
	    		        String gglimt, String edgeopts );	    	    	 
	    
	    int g2g_writer( float[] grid, float[] hist, String hstgrd,  
	    		        String gdfile, String proj, String cpyfil, 
	    		        String gdarea, String anlyss, String kxky,
	    		        String maxgrd, String gparm, String gdatim, 
	    		        String gvcord, String glevel );
	}	
	

    /**
     *  Test 
     */
	public static void main(String[] args) {
//		logger.debug("Test G2G native library");
	    G2GNative INSTANCE = (G2GNative) Native.loadLibrary("g2g", G2GNative.class);
//	    logger.debug("Finish test G2G native library");
	}

	
	/** 
	 *  The G2GNative instance we will use to call the native library 
	 */
    public G2GNative g2gNative = G2GNative.INSTANCE;
           
	
    /** 
	 *  The G2GNative method to start G2G calculation 
	 */
    public int g2g_compute( float[] grid, float[] hist, int kx, int ky, int nlines,                 
    		            int[] nlatlons, float[] latPts, float[] lonPts,
                        int[] nsmthpts, float[] smthLat, float[] smthLon,
                        int[] nextpts, float[] extLat, float[] extLon,
                        float[] values, int[] ismth, int[] iclosed,
                        int mmnum, float[] mmlat, float[] mmlon,
                        float[] mmfi, float[] mmfj, float[] mmvalue,
                        String catmap, String hstgrd, String discrete, 
    		            String dlines, String gglimt, String edgeopts ) {
     	
//    	logger.debug( "Start G2G native calculation ......");
      	    	    	
    	int ier = g2gNative.g2g_driver( grid, hist, kx, ky, nlines, 
        		                    nlatlons, latPts, lonPts,
        		                    nsmthpts, smthLat, smthLon,
        		                    nextpts,  extLat, extLon,
        		                    values, ismth, iclosed,
        	                        mmnum, mmlat, mmlon,
        	                        mmfi, mmfj, mmvalue,
       		                        catmap, hstgrd, discrete, dlines, 
        		                    gglimt, edgeopts  );
   	
//    	logger.debug( "Finish G2G native calculation - ier = " + ier );
    	
    	return ier;
    }
    
    /** 
	 *  The G2GNative method to write G2G output grid to a GEMPAK grid file 
	 */
   public int g2g_write( float[] grid, float[] hist, String hstgrd,  
	        		   String gdfile, String proj, String cpyfil, 
	        		   String gdarea, String anlyss, String kxky,
	        		   String maxgrd, String gparm, String gdatim, 
	        		   String gvcord, String glevel ) {

//	    logger.debug( "Writing G2G grid to file ......");
  	    	
    	int ier = g2gNative.g2g_writer( grid, hist, hstgrd,  
    	        	gdfile, proj, cpyfil, gdarea, anlyss, kxky,
    	            maxgrd, gparm, gdatim, gvcord, glevel );

//    	logger.debug( "Finish writing G2G grid - ier = " + ier );

    	return 0;
    }
                
}
