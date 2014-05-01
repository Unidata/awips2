package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

/**
 * The grid contour attribute editing dialog help.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov,22 2010  			 X. Guo     Initial creation for both Ensemble/Grid
 * 12/06/2012   #538         Q. Zhou    Added skip and filter areas and implements. 
 * 09/11/2012    #743    Archana        Added tooltip text for CLRBAR
 * 09/11/2013   #1036        S. Gurung  Added tooltip text for TEXT
 * 
 * @author xguo
 * @version 1
 */
public class NcgridAttributesHelp {
	
    public static String TitleToolTipText() {
    	String toolTipText="Title string to be displayed as data resource legend.";
		
    	return toolTipText;	
	}
    public static String ClrbarToolTipText() {
    	String toolTipText="CLRBAR specifies the characteristics of a color bar associated with "+
     "contour fill.  The attributes are separated by slashes:\n"+
     "        color / orientation / anchor / x;y / length;width\n"+
     
     "Color is the color of the labels and the bounding box around the"+
     "color bar.  If color is negative, the bounding box will not be drawn,"+
     "and labels will be drawn in colors corresponding to the color bar."+
     "If the color is 0 or missing, no color bar is drawn.\n"+
     
     "Orientation specifies a vertical or horizontal orientation of the"+
     "color bar where 'V' is a vertical bar and 'H' is a horizontal bar."+
     "The default is 'V'.\n"+
     
    " Anchor describes the location on the color bar corresponding to"+
     "the location given in the next parameter. \n Valid inputs are:\n"+
     "LL, LC, LR, CL, CC, CR, UL, UC, and UR for lower-left, lower-center,"+
     "lower-right, center-left, centered, center-right, upper-left,"+
     "upper-center, and upper-right, respectively. \n For example, an 'LL'"+
     "anchor point, with a x;y of .1,.1, will place the lower-left corner"+
     "of the color bar at view coordinates .1, .1.  The default anchor"+
     "point is 'LL'.\n"+
     " x;y is the position for the anchor point of the color bar in view"+
     "coordinates.  The default is .005, .05.\n"+

     "Length;width are the length and width of the color bar, normalized to"+
     "the view coordinates.  The defaults are .5 for the length, and .01 for"+
     "the width.\n"+
     "To disable the color bar, set CLRBAR = 0 or leave it blank.  If only"+
     "contour lines are drawn (CTYPE = C), the CLRBAR variable is not used.\n\n"+

     "Examples:\n"+
     "CLRBAR = 1     \n"+
     "--- text and bounding box in color 1;\n"+
     "defaults for the rest of the input;\n"+
     "CLRBAR = 5/V/ /.25;.1/1 \n "+
     "--- text and bounding box in color 5;\n"+
     "color bar plotted vertically; length and width .25 and .1\n"+
     "of the view window; all intervals labeled along left side of\n"+
     "the color bar;color bar anchor point at lower-left;\n"+
     "CLRBAR = 1//CL/.1;.5/.75;.1 \n"+
     "-- text and bounding box in color 1; ength and width .75 and .1 \n"+
     "lof the view window; center-left of the color bar \n"+
     "positioned at .1;.5 in viewcoordinates;";
		
    	return toolTipText;	
	}
    
    
    public static String HlsymToolTipText() {
		String toolTipText="HLSYM defines the characteristics for the HILO symbols specified\n"+
		"in HILO.  The text sizes, value position, fonts, text widths and\n"+
		"hardware/software flags are specified for the symbols (s) and\n"+
		"plotted values (v) as:\n\n"+

		"   sizes;sizev/position/fonts;fontv/widths;widthv/hwflgs;hwflgv\n\n"+

		"The size, font, width, and hw flag are the same as the TEXT\n"+
		"variable.  If only one value is given, it is used for both the\n"+
		"symbol and value.\n"+
		"The value plotting position may be 1, 2, or 3 where 2 is the\n"+
		"default.  The number selects the position of the value string\n"+
		"beneath the symbol string.  The three positions are shown below:\n\n"+
		"                        H\n"+
		"                     1  2  3\n\n"+
		"It is common for HILO symbols near the edge of the display to be\n"+
		"hidden when hardware text font is used. Therefore, when using\n"+
		"hardware text font, the number of HILO symbols displayed may be\n"+
		"slightly less than what the user specifies.\n\n"+
		"Examples:\n\n"+
		"           HLSYM = 2;1/3/2//HW  -- symbol text size = 2\n"+
		"                                   value text size = 1\n"+
		"                                   plot value in position 3\n"+
		"                                   hardware text font 2 applies to both\n";

		return toolTipText;	
	}

    public static String HiloToolTipText() {
		String toolTipText="HILO contains the information for plotting relative highs and lows\n"+
		"in the following format:\n\n"+

		"   colorh;colorl/symbolh;symboll/rangeh;rangel/radius/counth;countl/interp\n\n"+

		"Colorh and colorl are the colors for the high and low symbols to be\n"+
		"plotted.  If only a single number is entered, it will be used for\n"+
		" both highs and lows.  The default for this entry is 0.\n\n"+
		"Symbolh and symboll specify the symbols to be plotted.  The format\n"+
		"for the symbol input is:\n\n"+
		"                character # precision\n\n"+
		"where the character is the character to be plotted.  If the character\n"+
		"is an integer, markers corresponding to that number will be plotted.\n"+
		"If the character is an integer preceeded by the character 'S', special\n"+
		"symbols corresponding to that number will be plotted.\n"+
		"Information about markers can be found in the help for MARKER.\n"+
		"The # is a flag to plot values beneath the marker.  The integer followin\n"+
		"the # is the number of decimal places to display in the value.  If a\n"+
		"# is present without the following number, integer values are printed.\n"+
		"The default for the symbols is H;L.\n\n"+
		"Rangeh and rangel are ranges for highs and lows specified as:\n"+
		"                minval - maxval \n\n"+
		"where minval and maxval are integers which specify the range of values\n"+
		"to be considered for designation as a high or low.  The default is to\n"+
		"consider all data.\n\n"+
		"The search radius is a sqaure region of grid points.The default is 3.\n"+
		"A large radius, such as 10 or higher, is not very effective.\n\n"+
		"Counth and countl are integer values for the maximum number of\n"+
		"high and low values to be displayed.  The default is 20;20.\n\n"+
		"Interp is an interpolation flag which specifies whether the values and\n"+
		"locations of the highs and lows will be at grid points, or will be\n"+
		"interpolated between grid points.  The default is NO.\n";

		return toolTipText;	
	}
    
    public static String WindToolTipText() {
		String toolTipText="WIND specifies wind color, size and width \n" +
		"separated by slashes\n\n"+ 

		"		color / size / width\n";

		return toolTipText;	
	}

	public static String FlineToolTipText() {
		String toolTipText="FLINE is the color and fill type to be used for contour fill:\n\n"+

		"   colr1;..;colrn/type1;..;typen\n\n"+

		"The number of fill colors and types needed is one greater than\n"+
		"the number of fill levels in FINT.  The number of fill colors \n"+
		"may be entered as a list of color numbers separated by \n"+
		"semicolons or a range of colors.  The number of fill types may \n"+
		"be entered as a list of numbers separated by semicolons.  \n"+
		" More information on color selection can be found in the help \n"+
		"for COLORS.\n\n"+

		"The fill type may be set any of the following values:\n\n"+

		"           1               Solid\n"+
		"           2               Slanted Dash\n"+
		"           3               Wide-spaced Slanted Line\n"+
		"           4               Medium-spaced Slanted Line\n"+
		"           5               Zig-Zag Line\n"+
		"           6               Dots\n"+
		"           7               Thin-spaced Slanted Line\n\n"+

		"If fill type is set to 0, soild fill is used. If the fill type is\n"+
		"set to a single negative number, negative values will use the\n"+
		"absolute value of the fill type, and positive values will be solid.\n";

		return toolTipText;	
	}

	public static String FintToolTipText() {
		String toolTipText="FINT is the contour fill interval, minimum and maximum values\n"+
		"separated by slashes:\n\n"+

		"           fill interval / minimum / maximum\n\n"+

		"The contour fill interval may be any real number.\n\n"+

		"The minimum and maximum values specify the range of data\n"+
		"to use in selecting the fill levels.  If either value \n"+
		"is not specified, thevalue will be obtained from the \n"+
		"range of values in the dataset.  If the minimum and \n"+
		"maximum are equal, that value will be used and only one \n"+
		"contour fill level will be selected; however, since the \n"+
		"number of colors is one greater than the number of fill \n"+
		"levels, two colors will be needed--the first for filling \n"+
		"regions with values less than the input value and the \n"+
		"second for filling regions of greater value.\n\n"+

		"A list of two or more fill levels may be entered using semicolons\n"+
		"to separate the individual values.  In this case, the minimum \n"+
		"and maximum are ignored.";

		return toolTipText;		
	}

	public static String CintToolTipText() {
    	String toolTipText="TWO FORMATS OF CINT\n\n" +
    			"FORMAT1: INTERVAL1/MIN1/MAX1>INTERVAL2/MIN2/MAX2>...\n\n"+
    			"FORMAT2: VALUE11;VALUE12;...;VALUE1n>VALUE21;VALUE22;...;VALUE2n>...";
    			
    	return toolTipText;		
    }
    
    public static String GlevelToolTipText() {
    	String text = "GLEVEL is the vertical level for the grid.\n" +

     "Grids may contain two levels separated by a colon.  If the grid\n" +
     "to be selected contains only one level, the colon and second \n" +
     "level may be omitted.  In this case, the second level is stored \n" +
     "in the grid file as -1.\n" +
      "\n" +
     "Note that the vertical coordinate system for GLEVEL is \n" +
     "specified by GVCORD.\n\n" +
     "The value in GLEVEL may be overridden by specifying @GLEVEL \n" +
     "with the grids to be found.  For example, the following two \n" +
     "computations are identical:\n\n" +

     "           GFUNC = SUB (TMPF@850,TMPF@500)\n\n" +

     "           GFUNC = LDF (TMPF)  and  GLEVEL = 850:500.\n\n" +

     "See the help information on gparm for information on how \n" +
     "to specify a layer in GLEVEL for a LYR_ function."; 
     		
     
     	return text;
    }
    
    public static String GvcordToolTipText() {
    	String text = "GVCORD is the vertical coordinate of the grid to\n be selected.\n" +

     "The standard values are:\n\n" +

     "   NONE    for surface data\n" +
     "   PRES    data in pressure coordinates (millibars)\n" +
     "   THTA    data in isentropic coordinates (Kelvin)\n" +
     "   HGHT    data in height coordinates (meters)\n" +
     "   SGMA    data in sigma coordinates\n\n" +

     "The value in GVCORD may be overridden by specifying %GVCORD \n" +
     "with the grids to be found.  For example:\n\n" +

     "  GFUNC = SUB ( TMPC @850 %PRES, TMPC @1500 %HGHT )\n\n" +

     "will compute the difference between temperatures on the\n" +
     "850-mb level and the 1500-meter level.\n";
     		
     
     	return text;
    }
    
    public static String SkipToolTipText() {
    	String text = "SKIP is a variable which determines the contour points or \n" +
     "plot points to skip.  Input is entered as: \n" +
        "skip_contour / skip_plot_x ; skip_plot_y \n" +

     "The defaults for skip_contour and skip_plot are 0. \n" +

     "Skip_contour thins the input grid before computing the \n" +
     "contours to be drawn. \n" +

     "Skip_plot_x and _y specify the points at which data is to be \n" +
     "displayed.  If skip_plot_x is positive and skip_plot_y is not \n" +
     "specified, skip_plot_y is set to skip_plot_x. \n" +

     "If skip_plot_x is negative, the x plot locations on alternate \n" +
     "rows are indented by half the skip_plot_x value.  In this case, \n" +
     "the absolute value of skip_plot_x must be odd.  If not, the \n" +
     "absolute value minus 1 is used.  If no value for skip_plot_y \n" +
     "is specified, half the skip_plot_x value is used. \n\n" +

     "Examples: \n" +

        "SKIP    SKIP_CNTR       SKIP_PLOT_X     SKIP_PLOT_Y     STAGGER \n" +

        "' '             0                0              0       	no \n" +
        "2               2                0              0       	no \n" +
        "-1              0                0              0       	no \n" +
        "2;3             2                0              0       	no \n" +

        "/3              0                3              3       	no \n" +
        "/2;3            0                2              3       	no \n" +
        "/;1             0                0              1       	no \n" +

        "/-1             0                1              0       	yes \n" +
        "/-3             0                3              1       	yes \n" +
        "/-1;1           0                1              1       	yes \n\n" +

     "Winds may be thinned by latitude by setting skip_plot_y to\n" +
     "a negative value.  This feature is most useful when plotting\n" +
     "winds on a cylindrical grid on a projection with converging \n" +
     "meridians. In this case, ABS(skip_plot_y) determines how often\n" +
     "rows are plotted.  The variable skip_plot_x is not used.\n";
        return text;
    }
    
    public static String FilterToolTipText() {
    	String text = "FILTER is a logical variable or real number which controls the\n" +
        "filtering of data in order to eliminate plotting of overlapping data.\n\n" +
        "If FILTER is YES, the data will be filtered.\n" +
        "If FILTER is NO, 0, or blank, all data will be plotted.\n" +
        "If FILTER is set to a real number, the default filter will be \n" +
        "scaled by that number.  FILTER = 1 corresponds to FILTER = YES.\n" +
        "0 < FILTER < 1 allows some data overlap.  FILTER > 1 causes \n" +
        "data to be more widely spaced. \n";
     		
        return text;
    }
    
    public static String ScaleToolTipText() {
    	String text = "SCALE is the scaling factor for the data. \n" + 
    	"All data will be multiplied by 10 ** SCALE.";
     		
        return text;
    }
    
    public static String GdpfunToolTipText() {
    	String text = "GDPFUN specifies a grid diagnostic function\n" +
    	"which yields either a scalar or vector quantity.    \n" +
    	"For more information, see the GPARM documentation.";
     	
        return text;
    }
    
    public static String TypeToolTipText() {
    	String text = "TYPE specifies the processing type for the \n" +
    	"GDPFUN parameter. The TYPE list does not need separators, \n" +
    	"however slashes could be     used for clarity:\n\n" +

        "        type 1 / type 2 / ... / type n  \n\n" +

     "Valid inputs for type are:\n\n" +

     "   SCALAR TYPEs:\n" +
     "   C       the original GEMPAK contouring algorithm\n" +
     "   F       contour fill algorithm\n" +
     "   P       plot grid point values\n" +
     "   D       plot scaler as a directional arrow\n\n" +

     "   VECTOR TYPEs:\n" +
     "   A       wind arrows\n" +
     "   B       wind barbs\n" +
     "   S       streamlines\n\n" +

     "   OTHER TYPEs:\n" +
     "   M       plot grid point markers\n" +
     "   G       plot grid indices (row/column numbers)";
     	
        return text;
    }
    
    public static String LineToolTipText() {
    	String text = "LINE is the color, line type, line width\n" +
     "separated by slashes. The individual values in each \n" +
     "group are separated by semicolons:\n\n" +

     "colr1;..;colrn/type1;..;typen/width1;..;widthn\n\n" +

    "There are ten distinct line types:\n\n" +

    "  1       -       solid\n" +
    "  2       -       short dashed\n" +
    "  3       -       medium dashed\n" +
    "  4       -       long dash short dash\n" +
    "  5       -       long dash\n" +
    "  6       -       long dash three short dashes\n" +
    "  7       -       long dash dot\n" +
    "  8       -       long dash three dots\n" +
    "  9       -       medium dash dot\n" +
    "  10      -       dotted";
     	
        return text;
    }
    
    public static String MarkerToolTipText() {
    	String text = "MARKER specifies the marker color, type, size, \n" +
    	"line width separated by slashes:\n\n" +

         "    marker color / marker type / size / width\n\n" +

     "If the marker color is 0, no markers will be drawn.\n" +
     "If the marker color is not specified, a default of 1\n " +
     "will be used.\n\n" +

    "The marker type specifies the shape of the marker.\n" +
     "The software marker types are:\n\n" +

      "   1      plus sign			12      asterisk\n" +
      "   2      octagon			13      hourglass X\n" +
      "   3      triangle			14      star\n" +
      "   4      box				15      dot\n" +
      "   5      small X			16      large X\n" +
      "   6      diamond			17      filled octagon\n" +
      "   7      up arrow			18      filled triangle\n" +
      "   8      X with top bar		19      filled box\n" +
      "   9      Z with bar			20      filled diamond\n" +
      "  10      Y					21      filled star\n" +
      "  11      box with diagonals	22      minus sign\n";
     	
        return text;
    }
    
    public static String ColorsToolTipText() {
    	String toolTipText="COLORS is the color number to be used\n" +
    	"in plotting the grid point values.  If COLORS = 0 \n" +
    	"or blank, grid point values are not plotted.";
    			
    	return toolTipText;		
    }
    
    public static String GrdlblToolTipText() {
    	String text = "GRDLBL is the color number to be used\n" +
    	"in plotting the grid index numbers.  If GRDLBL = 0 \n" +
    	"or blank, grid index numbers are not plotted.";
    	
        return text;
    }
    
    public static String TextToolTipText() {
		String toolTipText="TEXT is the size, font, text width, border, rotation, justification\n"+
	 " and hardware/software flag for graphics text separated with slashes: \n"+
	 "     text size/font/width/border/rotation/justification/hw flag\n"+
	 "	The size may be a real number multiplier for the default\n"+
	 "text size. If the size is zero or unspecified, the current\n"+
	 "size will be used.\n"+
	 "	The size may also be a name, or the first character of a\n"+
	 "name. The name is converted to a real number multiplier. For\n"+
	 "hardware text, the named sizes correspond to discrete point\n"+
	 "sizes. Sizes other than the named sizes will be rounded to the\n"+
	 "nearest point size. Below are the standard names and size values:\n"+
	 "         Name            Size    HW Point\n"+
	 "         ----            ----    --------\n"+
	 "         Tiny            0.714      10\n"+
	 "         Small           0.857      12\n"+
	 "         Medium         1.000      14\n"+
	 "         Large           1.286      18\n"+
	 "         Huge            1.714      24\n"+
	 "         Giant           2.429      34\n"+
	 "	The text width is the integer line width to be used in \n"+
	 "generating software text. If the text size, font or width \n"+
	 "is not specified, the current value is used.\n"+
	 "	The text border is a border/blank fill flag. Border is a three\n"+
	 "digit number, ABC, where:\n"+
	 "   A - Border      B - Blank Fill          C - Border Type \n"+
	 "   ----------       --------------          ---------------\n"+
	 "   1 = No          1 = No                   1 = Regular Box\n"+
	 "   2 = Yes         2 = Yes                 \n"+
	 "                      3 = Reverse video \n"+
	 "	If reverse video is selected, the text color and background\n"+
     "color are switched. \n"+
	 "	The text rotation is a character input that specifies whether\n"+
	 "the text is aligned with the screen (S) or with north (N) on a\n"+
	 "given image. If the choice is invalid or not specified, the\n"+
	 "default value 'S' is used.\n"+
	 "	The text justification is a character input that specifies\n"+
	 "whether the text is justified to the center (C), right (R), \n"+
	 "or left (L). If the choice is invalid or not specified, the \n"+
	 "default value 'C' is used.\n"+
	 "	The hardware/software selector must be HW or SW to \n"+
	 "change to hardware- or software-generated text. \n"+
	 "	The font number must be specified by using the HW \n"+
	 "selector and choosing a font number from the list below.\n"+
	 "                   REGULAR    ITALIC    BOLD     ITALIC-BOLD\n"+
	 "      Courier        1         11       21         31\n"+
	 "      Helvetica      2         12       22         32\n"+
	 "      Times          3         13       23         33\n"+
	 "	If a font does not support a particular style, the next \n"+
	 "lower style is used instead. \n"+
	 "Examples:\n"+
	 "   TEXT = 1/21            --  text size = 1; bold software font 1\n"+
	 "   TEXT = 2.5/2/HW        --  text size = 2.5; hardware text font 2\n"+
	 "   TEXT = 1.24///221/s/c  --  text size = 1.24; current text font;\n"+
	 "                                 current text width; Border = yes,\n"+
	 "                                 Blank fill = yes, Border type = box;\n"+
	 "                                 screen relative; center justified.";

		return toolTipText;	
	}
}
