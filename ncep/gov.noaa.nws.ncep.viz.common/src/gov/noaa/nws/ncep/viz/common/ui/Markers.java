package gov.noaa.nws.ncep.viz.common.ui;

// NOTE : These were moved from IPointOverlayResourceData where they don't belong since 
// they are being referenced by nsharp and ntrans.
//    Markers are Symbols drawn by PGEN and defined in the symbolPatterns.xml file. 
// The names defined here have to match names in this file.
//
// TODO : need to change this. 
public class Markers {

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
     *  descriptive text, or both 
     */    
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
}
