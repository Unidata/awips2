
/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.G2GCommon
 * 
 * June 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphtogrid;


/**
 * Class to define those parameters as in grphgd.cmn for native Graph-to-Grid 
 * processing and some parameters defined in gemprm.h
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/10		#215		J. Wu   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public class G2GCommon {
	
	/*
	 * Grid point initialized.
	 */
	public static final int INIT      = 0;  
	
	/*
	 * Grid point inside bounded area.
	 */
	public static final int BOUNDED   = 1;
	
	/*
	 * Grid pt lies exactly on contour.
	 */
	public static final int EXACT     = 2;

	/*
	 * Grid pt lies between contours of different values and 
	 * assigned via gradient search
	 */
	public static final int RADIAL    = 3; 
	
	/*
	 * Grid pt inside closed contour w/o minmax.
	 */
	public static final int CLSDCN    = 4;
	
	/*
	 * Grid pt inside closed contour w/  minmax. (not used at this time)
	 */
	public static final int CLSDCNMM  = 5;

	/*
	 * Grid pt surrounded by like values w/  minmax.
	 */
	public static final int LIKVALMM  = 6;

	/*
	 * Grid pt surrounded by like values w/o minmax and assigned 
	 * via gradient search.
	 */
	public static final int LIKVAL    = 7;
	
	/*
	 * Grid pt assigned from surrounding grid pts.
	 */
	public static final int WEIGHTED  = 8;

	/*
	 * Grid pt lies to the right of a contour and assigned the contour line value
	 */
	public static final int DLINE     = 9;

	/*
	 * Grid pt assigned from surrounding grid pts.
	 */
	public static final int DLWEIGHTED  = 10;

	/*
	 * Grid pt assigned default value.
	 */
	public static final int DLWDEFALT  = 11;

	/*
	 * No value for innermost contour
	 */
	public static final int NOMMVALU  = 0;

	/*
	 * Line is closed.
	 */
	public static final int CLSD  = 1;

	/*
	 * Line is open.
	 */
	public static final int OPEN = 0;
	
	/*
	 * Max number of lines
	 */
	public static final int MAXLIN = 200;

	/*
	 * Max number of pts per line
	 */
	public static final int MAXPPL = 500;

     /*
      * Max dimension for holding intersections
      * (this allows for 1000x1000 grid overall)
      */
     public static final int MAXDIM = 2000;

     /*
      * Max number of intersections overall
      */
     public static final int MAXINT = 50000;

     /*
      * Max number of minima/maxima
      */
     public static final int MAXMM = 50;
     
     /*
      * Outside and Inside for BOUNDS check
      */
     public static final int GG_OUT = 0;
     public static final int GG_IN = 1;
    
     
     /*
      * The following constants are defined in gemprm.h
      */
     public static final int LLMXTG	= 1000000;  			//Ultimate max # grid points
     
     public static final float	RMISSD = (float)-9999;		// Missing data value
     public static final int	IMISSD =  -9999;			// Missing integer value

}


