/**
 * 
 */
package gov.noaa.nws.ncep.viz.overlays;

import org.eclipse.swt.graphics.RGB;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 30 Jan 2009  53          bhebbard    Initial Creation.
 * 15 Apr 2009  53B         bhebbard    Add mixed-case text size displayName; comments.
 *                                      
 * 
 * </pre>
 *
 * @author bhebbard
 * @version 1
 */
public interface IPointOverlayResourceData {

    /** Defines marker symbol choice */
	
    public /* static */ enum MarkerType {        //  GEMPAK/NMAP Table Code
    	PLUS_SIGN          ("Plus Sign"),        //   1
    	OCTAGON            ("Octagon"),          //   2
    	TRIANGLE           ("Triangle"),         //   3
    	BOX                ("Box"),              //   4
    	SMALL_X            ("Small X"),          //   5
    	DIAMOND            ("Diamond"),          //   6
    	UP_ARROW           ("Up Arrow"),         //   7
    	X_WITH_TOP_BAR     ("Bar X"),            //   8
    	Z_WITH_BAR         ("Z"),                //   9
    	Y                  ("Y"),                //  10
    	BOX_WITH_DIAGONALS ("Box X"),            //  11
    	ASTERISK           ("Asterisk"),         //  12
    	HOURGLASS_X        ("Hourglass"),        //  13
    	STAR               ("Star"),             //  14
    	DOT                ("Dot"),              //  15
    	LARGE_X            ("Large X"),          //  16
    	FILLED_OCTAGON     ("Filled Octagon"),   //  17
    	FILLED_TRIANGLE    ("Filled Triangle"),  //  18
    	FILLED_BOX         ("Filled Box"),       //  19
    	FILLED_DIAMOND     ("Filled Diamond"),   //  20
    	FILLED_STAR        ("Filled Star"),      //  21
    	MINUS_SIGN         ("Minus");            //  22
    	
    	private String designator;
    	
    	// Constructor    	
    	MarkerType (String designator)
    	{
    		this.designator = designator;
    	}
    	
    	public String getDesignator()
    	{
    		return this.designator;
    	}
    	
    };
    
    /** Defines whether we draw just the marker symbol,
     *  descriptive text, or both */
    
    public enum MarkerState {                    //  GEMPAK/NMAP Table Code
        MARKER_ONLY,                             //   0
        MARKER_PLUS_TEXT,                        //   1
        TEXT_ONLY                                //   2
    };
    
    /** Defines text size choice */
    
    public static enum MarkerTextSize {          //  GEMPAK/NMAP Table Code
    	TINY       ( 0.714f , 10, "Tiny" ),      //   0
    	SMALL      ( 0.857f , 12, "Small" ),     //   1
    	MEDIUM     ( 1.000f , 14, "Medium" ),    //   2
    	LARGE      ( 1.286f , 18, "Large" ),     //   3
    	HUGE       ( 1.714f , 24, "Huge" ),      //   4
    	GIANT      ( 2.429f , 34, "Giant" );     //   5

        private float  softwareSize ;
        private int    hardwareSize ;
        private String displayName ;
        
        // Constructor        
        MarkerTextSize (float softwareSize, int hardwareSize, String displayName)
        {
        	this.softwareSize = softwareSize ;
        	this.hardwareSize = hardwareSize ;
        	this.displayName  = displayName  ;
        }
        
        public float getSoftwareSize()
        {
        	return softwareSize ;
        }
        
        public int getHardwareSize()
        {
        	return hardwareSize ;
        }
        
        public String getDisplayName()
        {
        	return displayName ;
        }
    };

	/**
	 * @return the markerState
	 */
	public abstract MarkerState getMarkerState();

	/**
	 * @param markerState the markerState to set
	 */
	public abstract void setMarkerState(MarkerState markerState);

	/**
	 * @return the markerType
	 */
	public abstract MarkerType getMarkerType();

	/**
	 * @param markerType the markerType to set
	 */
	public abstract void setMarkerType(MarkerType markerType);

	/**
	 * @return the markerSize
	 */
	public abstract float getMarkerSize();

	/**
	 * @param markerSize the markerSize to set
	 */
	public abstract void setMarkerSize(float markerSize);

	/**
	 * @return the markerWidth
	 */
	public abstract int getMarkerWidth();

	/**
	 * @param markerWidth the markerWidth to set
	 */
	public abstract void setMarkerWidth(int markerWidth);

	/**
	 * @return the markerTextSize
	 */
	public abstract MarkerTextSize getMarkerTextSize();

	/**
	 * @param markerTextSize the markerTextSize to set
	 */
	public abstract void setMarkerTextSize(MarkerTextSize markerTextSize);

	/**
	 * The color to set
	 * 
	 * @param color
	 *            the color
	 */
	public abstract void setColor(RGB color);

	/**
	 * Get the currently set color
	 * 
	 * @return the color
	 */
	public abstract RGB getColor();
}
