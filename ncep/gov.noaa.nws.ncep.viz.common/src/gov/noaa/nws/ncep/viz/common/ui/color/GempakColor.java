/**
 * 
 */
package gov.noaa.nws.ncep.viz.common.ui.color;

import org.eclipse.swt.graphics.RGB;

/**
 * The Colors of GEMPAK
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 17 Apr 2009  74          B. Hebbard  Initial implementation.
 * 17 May 2010  164         M. Li       Add convertToRGB
 * 01 Nov 2011  397         B. Hebbard  Add getRGBString(); various cleanups
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */
public enum GempakColor {
	
	//  (from GEMPAK COLTBL.XWP:  "Default XWP and XW color table")
	//
	//  Color name       Abbrev    Red   Green    Blue     X color name       GEMPAK #

	    VANILLA     (    "VAN"  ,  255  ,  228  ,  220  ,  "bisque1"        ),  //   1
	    RED         (    "RED"  ,  255  ,    0  ,    0  ,  "red"            ),  //   2
	    GREEN       (    "GRE"  ,    0  ,  255  ,    0  ,  "green"          ),  //   3
	    BLUE        (    "BLU"  ,    0  ,    0  ,  255  ,  "blue"           ),  //   4
	    YELLOW      (    "YEL"  ,  255  ,  255  ,    0  ,  "yellow"         ),  //   5
	    CYAN        (    "CYA"  ,    0  ,  255  ,  255  ,  "cyan"           ),  //   6
	    MAGENTA     (    "MAG"  ,  255  ,    0  ,  255  ,  "magenta"        ),  //   7
	    BROWN       (    "BRO"  ,  139  ,   71  ,   38  ,  "sienna3"        ),  //   8
	    CORAL       (    "COR"  ,  255  ,  130  ,   71  ,  "sienna1"        ),  //   9
	    APRICOT     (    "APR"  ,  255  ,  165  ,   79  ,  "tan1"           ),  //  10
	    PINK        (    "PIN"  ,  255  ,  174  ,  185  ,  "LightPink1"     ),  //  11
	    DKPINK      (    "DKP"  ,  255  ,  106  ,  106  ,  "IndianRed1"     ),  //  12
	    MDVIOLET    (    "MDV"  ,  238  ,   44  ,   44  ,  "firebrick2"     ),  //  13
	    MAROON      (    "MAR"  ,  139  ,    0  ,    0  ,  "red4"           ),  //  14
	    FIREBRIC    (    "FIR"  ,  205  ,    0  ,    0  ,  "red3"           ),  //  15
	    ORRED       (    "ORR"  ,  238  ,   64  ,    0  ,  "OrangeRed2"     ),  //  16
	    ORANGE      (    "ORA"  ,  255  ,  127  ,    0  ,  "DarkOrange1"    ),  //  17
	    DKORANGE    (    "DKO"  ,  205  ,  133  ,    0  ,  "orange3"        ),  //  18
	    GOLD        (    "GOL"  ,  255  ,  215  ,    0  ,  "gold1"          ),  //  19
	    DKYELLOW    (    "DKY"  ,  238  ,  238  ,    0  ,  "yellow2"        ),  //  20
	    LWNGREEN    (    "LWN"  ,  127  ,  255  ,    0  ,  "chartreuse1"    ),  //  21
	    MDGREEN     (    "MDG"  ,    0  ,  205  ,    0  ,  "green3"         ),  //  22
	    DKGREEN     (    "DKG"  ,    0  ,  139  ,    0  ,  "green4"         ),  //  23
	    GRPBLUE     (    "GRP"  ,   16  ,   78  ,  139  ,  "DodgerBlue4"    ),  //  24
	    LTBLUE      (    "LTB"  ,   30  ,  144  ,  255  ,  "DodgerBlue1"    ),  //  25
	    SKY         (    "SKY"  ,    0  ,  178  ,  238  ,  "DeepSkyBlue2"   ),  //  26
	    MDCYAN      (    "MDC"  ,    0  ,  238  ,  238  ,  "cyan2"          ),  //  27
	    VIOLET      (    "VIO"  ,  137  ,  104  ,  205  ,  "MediumPurple3"  ),  //  28
	    PURPLE      (    "PUR"  ,  145  ,   44  ,  238  ,  "purple2"        ),  //  29
	    PLUM        (    "PLU"  ,  139  ,    0  ,  139  ,  "magenta4"       ),  //  30
	    WHITE       (    "WHI"  ,  255  ,  255  ,  255  ,  "white"          ),  //  31
	    BLACK       (    "BLA"  ,    0  ,    0  ,    0  ,  "black"          );  //  32

	    private String abbrev;
	    private int red;
	    private int green;
	    private int blue;
	    private String xColorName;

	    //  Constructor
	    private GempakColor (String abbrev, int red, int green, int blue, String xColorName) {
	        this.abbrev     = abbrev;
	        this.red        = red;
	        this.green      = green;
	        this.blue       = blue;
	        this.xColorName = xColorName;
	    }

	    public String getAbbrev() {
	        return abbrev;
	    }

	    public int getRed() {
	        return red;
	    }

	    public int getGreen() {
	        return green;
	    }

	    public int getBlue() {
	        return blue;
	    }
	    
	    public String getXColorName() {
	        return xColorName;
	    }

	    public RGB getRGB() {
	    	return new RGB (red, green, blue);
	    }

	    public String getRGBString() {
	    	//  Result identical to getRGB().toString(), but doesn't invoke SWT
            return "RGB {" + getRed()   + ", "
                           + getGreen() + ", "
                           + getBlue()  + "}";
	    }
	    
	    public static RGB convertToRGB(int index) {
	    	if (index == 0) {
	    		return new RGB(0, 0, 0);
	    	}
	    	if (index > values().length) {
	    	    return null;
	    	}
    	    return values()[index-1].getRGB();
	    }
}
