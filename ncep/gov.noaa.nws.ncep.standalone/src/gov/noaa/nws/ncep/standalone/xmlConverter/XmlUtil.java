package gov.noaa.nws.ncep.standalone.xmlConverter;

import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;

import java.awt.Color;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;


/**
 * XmlConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/25/2010   137         Q. Zhou     Initial created
 * 5/18/2010   137         Q. Zhou     Match continuous colors to the color table
 * 9/09/2010   137         Q. Zhou     Modified fill pattern
 * 12/2/2010   137         Q. Zhou     Modified getFrontType to associate with width
 * 2/9/2011    137		   Q. Zhou     put precision 2 to point lat and lon.
 * 2/23/2011   137         Q. Zhou     Added typeSearch()
 * 3/24/2011               Q. Zhou     Added parameter category to getSymType(). for HPC
 * 4/06/2011   424         Q. Zhou     Modified getSymType() and getType() to add all symbols
 * 11/1/2011   137         Q. Zhou     Added displayType and removed outline for Text.
 * 03/14/2012  #599        Q. Zhou     Added Flood outlook. Fixed Watch on pgenCategory name changing.
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */

public class XmlUtil {
	
	static int getType(String category, String pgenType) { //pgenYype
		int type = 0;
		if ( category.equalsIgnoreCase( "Lines") && 
				(pgenType.equalsIgnoreCase( "LINE_SOLID") || pgenType.startsWith("LINE_DASHED") )) {
			type = 1;
		}
		else if ( category.equalsIgnoreCase( "Lines") && 
			!(pgenType.equalsIgnoreCase( "LINE_SOLID") || pgenType.startsWith("LINE_DASHED") )) {
			type = 20;
		}
		else if ( category.equalsIgnoreCase( "Front")) {
			type = 2;
		}
		else if ( category.equalsIgnoreCase( "Symbol") && pgenType.startsWith("PRESENT_")) {
			type = 5;
		}
		else if ( category.equalsIgnoreCase( "Symbol") && pgenType.startsWith("CLOUD_TYPE_")) {
			type = 10;
		}
		else if ( category.equalsIgnoreCase( "Symbol") && pgenType.startsWith("ICING_")) {
			type = 11;
		}
		else if ( category.equalsIgnoreCase( "Symbol") && pgenType.startsWith("PRESSURE_TENDENCY_")) {
			type = 12;
		}
		else if ( category.equalsIgnoreCase( "Symbol") && pgenType.startsWith("PAST_WX_")) {
			type = 13;
		}		
		else if ( category.equalsIgnoreCase( "Symbol") && pgenType.startsWith("SKY_COVER_")) {
			type = 14;
		}
		else if ( category.equalsIgnoreCase( "Symbol") && pgenType.startsWith("TURBULENCE_")) {
			type = 16;
		}
		else if ( category.equalsIgnoreCase( "Symbol")) {
			type = 15;
		}
		else if ( category.equalsIgnoreCase( "Marker")) {
			type = 19;
		}
		else if ( category.equalsIgnoreCase( "Combo")) {
			type = 25;
		}
		else if ( category.equalsIgnoreCase( "Track")) {
			type = 26;
		}
		else if ( category.equalsIgnoreCase( "Text")) {
			type = 21;
		}
		else if ( category.equalsIgnoreCase( "Vector") && pgenType.equalsIgnoreCase("Arrow")) {
			type = 9;
		}
		else if ( category.equalsIgnoreCase( "Vector") && pgenType.equalsIgnoreCase("Barb")) {
			type = 8;
		}
		else if ( category.equalsIgnoreCase( "Vector") && pgenType.equalsIgnoreCase("Directional")) {
			type = 23;
		}
		else if ( category.equalsIgnoreCase( "Vector") && pgenType.equalsIgnoreCase("Hash")) {
			type = 24;
		}
		else if ( category.equalsIgnoreCase( "Arc")) {
			type = 4;
		}
		else if ( pgenType.equalsIgnoreCase("INTL_SIGMET")) {
			type = 27;
		}
		else if ( pgenType.equalsIgnoreCase("CONV_SIGMET")) {
			type = 29;
		}
		else if ( pgenType.equalsIgnoreCase("NCON_SIGMET")) {
			type = 28;
		}
		else if ( pgenType.equalsIgnoreCase("OUTL_SIGMET")) {
			type = 30;
		}
		else if ( pgenType.equalsIgnoreCase("AIRM_SIGMET")) {
			type = 31;
		}
		else if ( pgenType.equalsIgnoreCase("CCFP_SIGMET")) {
			type = 32;
		}
		else if ( pgenType.equalsIgnoreCase("VOLC_SIGMET")) {
			type = 35;
		}
		else if ( pgenType.equalsIgnoreCase("VACL_SIGMET")) {
			type = 36;
		}
		else if ( pgenType.equalsIgnoreCase( "JET")) {
			type = 37;
		}
		else if ( pgenType.equalsIgnoreCase( "GFA")) {
			type = 38;
		}
		else if ( pgenType.equalsIgnoreCase( "TCA")) {
			type = 39;
		}
		else if ( pgenType.equalsIgnoreCase( "WatchBox")) {
			type = 6;
		}
		
		return type;
	}

	static int	getRecSize (String category, String pgenType)
	{
		int subRecsz = 0;

		if ( category.equalsIgnoreCase( "Lines") && 
				(pgenType.equalsIgnoreCase( "LINE_SOLID") || pgenType.startsWith("LINE_DASHED") )) 
			subRecsz = 60;
		else if ( category.equalsIgnoreCase( "Lines") && 
				!(pgenType.equalsIgnoreCase( "LINE_SOLID") || pgenType.startsWith("LINE_DASHED") )) 
				subRecsz = 64;
		else if ( category.equalsIgnoreCase( "Front") || category.equalsIgnoreCase( "Symbol")
				|| category.equalsIgnoreCase( "Marker") || category.equalsIgnoreCase( "Combo")
				|| category.equalsIgnoreCase( "Vector") || category.equalsIgnoreCase( "Arc")) 
			subRecsz = 68;		
		else if (category.equalsIgnoreCase( "Watch"))
			subRecsz = 5672;	
		else if (category.equalsIgnoreCase( "Track"))
			subRecsz = 1360;	
		else if (category.equalsIgnoreCase( "Text"))
			subRecsz = 93;
		else if ( pgenType.equalsIgnoreCase("INTL_SIGMET") || pgenType.equalsIgnoreCase("CONV_SIGMET")
				|| pgenType.equalsIgnoreCase("NCON_SIGMET") || pgenType.equalsIgnoreCase("OUTL_SIGMET")
				|| pgenType.equalsIgnoreCase("AIRM_SIGMET") )
			subRecsz = 0;
		else if ( pgenType.equalsIgnoreCase( "JET")) {
			subRecsz = 0;
		}
		else if ( pgenType.equalsIgnoreCase( "TCA")) {
			subRecsz = 0;
		}
		else 
			subRecsz = 0;
		
		return subRecsz;
	}
	
	public static int	getColorTag (String major )
	{
		// convert input string to number array
		String[] colorComponent = major.split(","); //r=0,g=255,b=255
		String[] color = new String[3];
		for (int i=0; i<colorComponent.length; i++)	
			color[i] = colorComponent[i].substring(2);
		
		//build int[]
		int[] output1 =  {1, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-228) + Math.abs(Integer.parseInt(color[2])-220)};		
		int[] output2 =  {2, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-0) + Math.abs(Integer.parseInt(color[2])-0)};	
		int[] output3 =  {3, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-255) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output4 =  {4, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-0) + Math.abs(Integer.parseInt(color[2])-255)};
		int[] output5 =  {5, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-255) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output6 =  {6, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-255) + Math.abs(Integer.parseInt(color[2])-255)};
		int[] output7 =  {7, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-0) + Math.abs(Integer.parseInt(color[2])-255)};
		int[] output8 =  {8, Math.abs(Integer.parseInt(color[0])-139) + Math.abs(Integer.parseInt(color[1])-71) + Math.abs(Integer.parseInt(color[2])-38)};
		int[] output9 =  {9, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-130) + Math.abs(Integer.parseInt(color[2])-71)};
		int[] output10 = {10, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-165) + Math.abs(Integer.parseInt(color[2])-79)};
		int[] output11 = {11, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-174) + Math.abs(Integer.parseInt(color[2])-185)};
		int[] output12 = {12, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-106) + Math.abs(Integer.parseInt(color[2])-106)};
		int[] output13 = {13, Math.abs(Integer.parseInt(color[0])-238) + Math.abs(Integer.parseInt(color[1])-44) + Math.abs(Integer.parseInt(color[2])-44)};
		int[] output14 = {14, Math.abs(Integer.parseInt(color[0])-139) + Math.abs(Integer.parseInt(color[1])-0) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output15 = {15, Math.abs(Integer.parseInt(color[0])-205) + Math.abs(Integer.parseInt(color[1])-0) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output16 = {16, Math.abs(Integer.parseInt(color[0])-238) + Math.abs(Integer.parseInt(color[1])-64) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output17 = {17, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-127) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output18 = {18, Math.abs(Integer.parseInt(color[0])-205) + Math.abs(Integer.parseInt(color[1])-133) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output19 = {19, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-215) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output20 = {20, Math.abs(Integer.parseInt(color[0])-238) + Math.abs(Integer.parseInt(color[1])-238) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output21 = {21, Math.abs(Integer.parseInt(color[0])-127) + Math.abs(Integer.parseInt(color[1])-255) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output22 = {22, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-205) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output23 = {23, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-139) + Math.abs(Integer.parseInt(color[2])-0)};
		int[] output24 = {24, Math.abs(Integer.parseInt(color[0])-16) + Math.abs(Integer.parseInt(color[1])-78) + Math.abs(Integer.parseInt(color[2])-139)};
		int[] output25 = {25, Math.abs(Integer.parseInt(color[0])-30) + Math.abs(Integer.parseInt(color[1])-144) + Math.abs(Integer.parseInt(color[2])-255)};
		int[] output26 = {26, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-178) + Math.abs(Integer.parseInt(color[2])-238)};
		int[] output27 = {27, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-238) + Math.abs(Integer.parseInt(color[2])-238)};
		int[] output28 = {28, Math.abs(Integer.parseInt(color[0])-137) + Math.abs(Integer.parseInt(color[1])-104) + Math.abs(Integer.parseInt(color[2])-205)};
		int[] output29 = {29, Math.abs(Integer.parseInt(color[0])-145) + Math.abs(Integer.parseInt(color[1])-44) + Math.abs(Integer.parseInt(color[2])-238)};
		int[] output30 = {30, Math.abs(Integer.parseInt(color[0])-139) + Math.abs(Integer.parseInt(color[1])-0) + Math.abs(Integer.parseInt(color[2])-139)};
		int[] output31 = {31, Math.abs(Integer.parseInt(color[0])-255) + Math.abs(Integer.parseInt(color[1])-255) + Math.abs(Integer.parseInt(color[2])-255)};
		int[] output32 = {32, Math.abs(Integer.parseInt(color[0])-0) + Math.abs(Integer.parseInt(color[1])-0) + Math.abs(Integer.parseInt(color[2])-0)};
		
		//build List<int[]>
		List<int[]> output = new ArrayList<int[]>();
		output.add(output1);
		output.add(output2);
		output.add(output3);
		output.add(output4);
		output.add(output5);
		output.add(output6);
		output.add(output7);
		output.add(output8);
		output.add(output9);
		output.add(output10);
		output.add(output11);
		output.add(output12);
		output.add(output13);
		output.add(output14);
		output.add(output15);
		output.add(output16);
		output.add(output17);
		output.add(output18);
		output.add(output19);
		output.add(output20);
		output.add(output21);
		output.add(output22);
		output.add(output23);
		output.add(output24);
		output.add(output25);
		output.add(output26);
		output.add(output27);
		output.add(output28);
		output.add(output29);
		output.add(output30);
		output.add(output31);
		output.add(output32);
		
		//selection sort List<int[]>
		int i1=0, i2=0;
		for (int i=0; i<output.size()-1; i++) {
	        for (int j=i+1; j<output.size(); j++) {
	        	i1 = output.get(i)[1];
				i2 = output.get(j)[1];
				if (i1 > i2) {
	                //... Exchange elements
					output.set(i, output.get(j));
					output.set(j, output.get(i));
	            }
	        }
	    }
		
		return output.get(0)[0]; //[2, 0.00001]
	}
	
	static int	fill_pattern (String pattern, boolean filled )
	{
		int patt = 0;
		if (filled) {
			if ( pattern.equalsIgnoreCase("SOLID"))
			    patt = 2;
			else if ( pattern.equalsIgnoreCase("TRANSPARENCY"))
			    patt = 9;
			else if ( pattern.equalsIgnoreCase("FILL_PATTERN_0"))
			    patt = 10;
			else if (pattern.equalsIgnoreCase("FILL_PATTERN_1"))
				patt = 3;
			else if ( pattern.equalsIgnoreCase("FILL_PATTERN_2"))
				patt = 4;
			else if ( pattern.equalsIgnoreCase("FILL_PATTERN_3"))
				patt = 5;
			else if ( pattern.equalsIgnoreCase("FILL_PATTERN_4"))
				patt = 6;
			else if ( pattern.equalsIgnoreCase("FILL_PATTERN_5"))
				patt = 7;
			else if ( pattern.equalsIgnoreCase("FILL_PATTERN_6"))
				patt = 8;
		}
		else 
			patt = 0;
		
		return patt;
	}
	
	static int	getIalign (String s_align )
	{
		int align = 0;
	    if (s_align.equalsIgnoreCase( "CENTER"))
	    	align = 0;
	    else if (s_align.equalsIgnoreCase( "LEFT_JUSTIFY"))
	        align = -1;
	    else if (s_align.equalsIgnoreCase( "RIGHT_JUSTIFY"))
	    	align = 1;
	    else 
	    	align = 0;

	    return align;
	}
	
	static String	getSztext (String sztext )
	{
		String s_sztext = null;
	    if (sztext.equalsIgnoreCase("10.0")) //0.71
		    s_sztext = "0.714";
	    else if (sztext.equalsIgnoreCase("12.0")) //0.86
		    s_sztext = "0.857";
	    else if (sztext.equalsIgnoreCase("14.0")) //1.0
		    s_sztext = "1.0";
	    else if (sztext.equalsIgnoreCase("18.0")) //1.29
		    s_sztext = "1.286";
	    else if (sztext.equalsIgnoreCase("24.0")) //1.71
		    s_sztext = "1.714";
	    else if (sztext.equalsIgnoreCase("34.0")) //2.43
		    s_sztext = "2.429";
	    else 
	    	s_sztext = "1.0";

	    return s_sztext;
	}
	
	static int   getFontStyle(String font, String style)
	{
	    int txfn = 0;
	    if (font.equalsIgnoreCase("Courier") && style.equalsIgnoreCase("REGULAR") )
	    	txfn = 1;
	    else if (font.equalsIgnoreCase("Courier") && style.equalsIgnoreCase("ITALIC") )
	    	txfn = 11;
		else if (font.equalsIgnoreCase("Courier") && style.equalsIgnoreCase("BOLD") )
			txfn = 21;
		else if (font.equalsIgnoreCase("Courier") && style.equalsIgnoreCase("BOLD_ITALIC") )
			txfn = 31;
		else if (font.equalsIgnoreCase("Nimbus Sans L") && style.equalsIgnoreCase("REGULAR") )
	    	txfn = 2;
	    else if (font.equalsIgnoreCase("Nimbus Sans L") && style.equalsIgnoreCase("ITALIC") )
	    	txfn = 12;
		else if (font.equalsIgnoreCase("Nimbus Sans L") && style.equalsIgnoreCase("BOLD") )
			txfn = 22;
		else if (font.equalsIgnoreCase("Nimbus Sans L") && style.equalsIgnoreCase("BOLD_ITALIC") )
			txfn = 32;	
		else if (font.equalsIgnoreCase("Liberation Serif") && style.equalsIgnoreCase("REGULAR") )
	    	txfn = 3;
	    else if (font.equalsIgnoreCase("Liberation Serif") && style.equalsIgnoreCase("ITALIC") )
	    	txfn = 13;
		else if (font.equalsIgnoreCase("Liberation Serif") && style.equalsIgnoreCase("BOLD") )
			txfn = 23;
		else if (font.equalsIgnoreCase("Liberation Serif") && style.equalsIgnoreCase("BOLD_ITALIC") )
			txfn = 33;
		else 
			txfn = 1;
	    
	    return txfn;
	}
	
	static String	time_rp ( String sin, String sout ) //"2000-01-01T00:00:00:0000"
	{
	    if (sin == null || sin.isEmpty() || sin == " ") 
		return "";

	    String[] s = sin.split("-T:");
	    sout = s[0] + s[1] + s[2] + ":" + s[3] +s[4] + s[5];
	    sout = sout.substring(2);

	    return sout;
	}

	static int getLineType( String pgenType) {
		int lineType = 0;

		if ( pgenType.equalsIgnoreCase( "LINE_SOLID") )
			lineType = 1;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_2") )
			lineType = 10;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_3") )
			lineType = 2;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_4") )
			lineType = 3;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_5") )
			lineType = 4;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_6") )
			lineType = 5;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_7") )
			lineType = 6;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_8") )
			lineType = 7;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_9") )
			lineType = 8;
		else if ( pgenType.equalsIgnoreCase( "LINE_DASHED_10") )
			lineType = 9;
		else 
			lineType = 1;
		
		return lineType;
	}
	
	static int getSPLineType( String pgenType) {
		int lineType = 0;

		if ( pgenType.equalsIgnoreCase( "POINTED_ARROW") )
			lineType = 4;
		else if ( pgenType.equalsIgnoreCase( "FILLED_ARROW") )
			lineType = 6;
		else if ( pgenType.equalsIgnoreCase( "DASHED_ARROW") )
			lineType = 20;
		else if ( pgenType.equalsIgnoreCase( "DASHED_ARROW_FILLED") )
			lineType = 21;
		else if ( pgenType.equalsIgnoreCase( "BALL_CHAIN") )
			lineType = 1;
		else if ( pgenType.equalsIgnoreCase( "ZIGZAG") )
			lineType = 2;
		else if ( pgenType.equalsIgnoreCase( "SCALLOPED") )
			lineType = 3;
		else if ( pgenType.equalsIgnoreCase( "ANGLED_TICKS_ALT") )
			lineType = 5;
		else if ( pgenType.equalsIgnoreCase( "FILLED_CIRCLES") )
			lineType = 9;
		else if ( pgenType.equalsIgnoreCase( "LINE_WITH_CARETS") )
			lineType = 17;
		else if ( pgenType.equalsIgnoreCase( "LINE_CARET_LINE") )
			lineType = 18;
		else if ( pgenType.equalsIgnoreCase( "SINE_CURVE") )
			lineType = 19;
		else if ( pgenType.equalsIgnoreCase( "BOX_CIRCLE") )
			lineType = 7;
		else if ( pgenType.equalsIgnoreCase( "FILL_OPEN_BOX") )
			lineType = 13;
		else if ( pgenType.equalsIgnoreCase( "LINE_X_LINE") )
			lineType = 12;
		else if ( pgenType.equalsIgnoreCase( "LINE_XX_LINE") )
			lineType = 8;
		else if ( pgenType.equalsIgnoreCase( "FILL_CIRCLE_X") )
			lineType = 14;
		else if ( pgenType.equalsIgnoreCase( "BOX_X") )
			lineType = 15;
		else if ( pgenType.equalsIgnoreCase( "LINE_CIRCLE_ARROW") )
			lineType = 16;
		else if ( pgenType.equalsIgnoreCase( "DOUBLE_LINE") )
			lineType = 23;
		else if ( pgenType.equalsIgnoreCase( "ZZZ_LINE") )
			lineType = 26;
		else if ( pgenType.equalsIgnoreCase( "TICK_MARKS") )
			lineType = 11;
		else if ( pgenType.equalsIgnoreCase( "STREAM_LINE") )
			lineType = 22;
		else 
			lineType = 1;	
		
		return lineType;
	}
	
	static int getFrontType( String pgenType, int width) {
		int lineType = 0;

		if ( pgenType.equalsIgnoreCase( "COLD_FRONT") )
			lineType = 400 + 10*width;  //was 430, changed 8/18
		else if ( pgenType.equalsIgnoreCase( "COLD_FRONT_FORM") )
			lineType = 405 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "COLD_FRONT_DISS") )
			lineType = 408 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "WARM_FRONT") )
			lineType = 200 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "WARM_FRONT_FORM") )
			lineType = 205 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "WARM_FRONT_DISS") )
			lineType = 208 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "STATIONARY_FRONT") )
			lineType = 0 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "STATIONARY_FRONT_FORM") )
			lineType = 5 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "STATIONARY_FRONT_DISS") )
			lineType = 8 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "OCCLUDED_FRONT") )
			lineType = 600 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "OCCLUDED_FRONT_FORM") )
			lineType = 605 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "OCCLUDED_FRONT_DISS") )
			lineType = 608 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "DRY_LINE") )
			lineType = 700 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "TROF") )
			lineType = 800 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "TROPICAL_TROF") )
			lineType = 809 + 10*width;
		else if ( pgenType.equalsIgnoreCase( "INSTABILITY") )
			lineType = 900 + 10*width;
		else 
			lineType = 400 + 10*width;	
		
		return lineType;
	}
	
	static double getSymType( String pgenType) {
		double lineType = 0;
//symbol
		if ( pgenType.equalsIgnoreCase( "SQUARE") )
			lineType = 0.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_SQUARE") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "CIRCLE") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_CIRCLE"))
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "TRIANGLE_SPCL") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_TRIANGLE_SPCL") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "DIAMOND_SPCL") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_DIAMOND_SPCL") )
			lineType = 7.0;		
		else if ( pgenType.equalsIgnoreCase( "STAR_SPCL"))
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_STAR_SPCL") )
			lineType = 9.0;
		else if ( pgenType.equalsIgnoreCase( "HIGH_PRESSURE_H") )
			lineType = 10.0;
		else if ( pgenType.equalsIgnoreCase( "LOW_PRESSURE_L") )
			lineType = 11.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_HIGH_PRESSURE_H") )
			lineType = 12.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_LOW_PRESSURE_L") )
			lineType = 13.0;
		
		else if ( pgenType.equalsIgnoreCase( "SINGLE_BRACKET") )
			lineType = 14.0;
		else if ( pgenType.equalsIgnoreCase( "BOTTOM_HALF_OF_BRACKET") )
			lineType = 15.0;
		else if ( pgenType.equalsIgnoreCase( "TOP_HALF_OF_BRACKET") )
			lineType = 16.0;
		else if ( pgenType.equalsIgnoreCase( "LEFT_ADJUSTED_VERTICAL_BAR") )
			lineType = 17.0;		
		else if ( pgenType.equalsIgnoreCase( "RIGHT_ADJUSTED_VERTICAL_BAR"))
			lineType = 18.0;
		else if ( pgenType.equalsIgnoreCase( "BRACKET_WITH_ONE_CIRCLE") )
			lineType = 19.0;
		else if ( pgenType.equalsIgnoreCase( "BRACKET_WITH_TWO_CIRCLES") )
			lineType = 20.0;
		else if ( pgenType.equalsIgnoreCase( "BRACKET_WITH_ONE_ASTERISK") )
			lineType = 21.0;
		else if ( pgenType.equalsIgnoreCase( "BRACKET_WITH_TWO_ASTERISKS") )
			lineType = 22.0;
		else if ( pgenType.equalsIgnoreCase( "BRACKET_WITH_ONE_TRIANGLE") )
			lineType = 23.0;
		else if ( pgenType.equalsIgnoreCase( "BRACKET_WITH_TWO_TRIANGLES") )
			lineType = 24.0;
		else if ( pgenType.equalsIgnoreCase( "TROPICAL_STORM_NH") )
			lineType = 25.0;
		else if ( pgenType.equalsIgnoreCase( "HURRICANE_NH") )
			lineType = 26.0;
		else if ( pgenType.equalsIgnoreCase( "TROPICAL_STORM_SH") )
			lineType = 27.0;		
		else if ( pgenType.equalsIgnoreCase( "HURRICANE_SH"))
			lineType = 28.0;
		else if ( pgenType.equalsIgnoreCase( "TRIANGLE_WITH_ANTENNA") )
			lineType = 29.0;
		else if ( pgenType.equalsIgnoreCase( "SIDEWAYS_S") )
			lineType = 30.0;
		
		else if ( pgenType.equalsIgnoreCase( "SLASH") )
			lineType = 31.0;
		else if ( pgenType.equalsIgnoreCase( "STORM_CENTER") )
			lineType = 32.0;
		else if ( pgenType.equalsIgnoreCase( "TROPICAL_DEPRESSION"))
			lineType = 33.0;
		else if ( pgenType.equalsIgnoreCase( "TROPICAL_CYCLONE") )
			lineType = 34.0;
		else if ( pgenType.equalsIgnoreCase( "FLAME") )
			lineType = 35.0;
		else if ( pgenType.equalsIgnoreCase( "X_CROSS") )
			lineType = 36.0;
		else if ( pgenType.equalsIgnoreCase( "LOW_X_OUTLINE") )
			lineType = 37.0;		
		else if ( pgenType.equalsIgnoreCase( "LOW_X_FILLED"))
			lineType = 38.0;
		else if ( pgenType.equalsIgnoreCase( "TROPICAL_STORM_NH_WPAC") )
			lineType = 39.0;
		else if ( pgenType.equalsIgnoreCase( "TROPICAL_STORM_SH_WPAC") )
			lineType = 40.0;
		
		else if ( pgenType.equalsIgnoreCase( "NUCLEAR_FALLOUT") )
			lineType = 41.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_A") )
			lineType = 42.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_A_FILLED"))
			lineType = 43.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_C") )
			lineType = 44.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_C_FILLED") )
			lineType = 45.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_X") )
			lineType = 46.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_X_FILLED") )
			lineType = 47.0;		
		else if ( pgenType.equalsIgnoreCase( "LETTER_N"))
			lineType = 48.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_N_FILLED") )
			lineType = 49.0;
		else if ( pgenType.equalsIgnoreCase( "30_KT_BARB") )
			lineType = 50.0;
		
		else if ( pgenType.equalsIgnoreCase( "MT_WAVE") )
			lineType = 51.0;
		else if ( pgenType.equalsIgnoreCase( "MT_OBSC") )
			lineType = 52.0;
		else if ( pgenType.equalsIgnoreCase( "SFC_WND_20K"))
			lineType = 53.0;
		else if ( pgenType.equalsIgnoreCase( "SFC_WND_30K") )
			lineType = 54.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_B") )
			lineType = 55.0;
		else if ( pgenType.equalsIgnoreCase( "LETTER_B_FILLED") )
			lineType = 56.0;
		else if ( pgenType.equalsIgnoreCase( "UP_ARROW_SPCL") )
			lineType = 57.0;		
		else if ( pgenType.equalsIgnoreCase( "DOWN_ARROW_SPCL"))
			lineType = 58.0;
		
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_000") )
			lineType = 0.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_001") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_002") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_003") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_004") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_005") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_006") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_007") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_008") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_009") )
			lineType = 9.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_010") )
			lineType = 10.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_011") )
			lineType = 11.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_012") )
			lineType = 12.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_013") )
			lineType = 13.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_014") )
			lineType = 14.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_015") )
			lineType = 15.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_016") )
			lineType = 16.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_017") )
			lineType = 17.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_018") )
			lineType = 18.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_019") )
			lineType = 19.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_020") )
			lineType = 20.0;

		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_021") )
			lineType = 21.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_022") )
			lineType = 22.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_023") )
			lineType = 23.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_024") )
			lineType = 24.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_025") )
			lineType = 25.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_026") )
			lineType = 26.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_027") )
			lineType = 27.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_028") )
			lineType = 28.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_029") )
			lineType = 29.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_030") )
			lineType = 30.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_031") )
			lineType = 31.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_032") )
			lineType = 32.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_033") )
			lineType = 33.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_034") )
			lineType = 34.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_035") )
			lineType = 35.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_036") )
			lineType = 36.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_037") )
			lineType = 37.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_038") )
			lineType = 38.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_039") )
			lineType = 39.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_040") )
			lineType = 40.0;

		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_041") )
			lineType = 41.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_042") )
			lineType = 42.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_043") )
			lineType = 43.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_044") )
			lineType = 44.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_045") )
			lineType = 45.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_046") )
			lineType = 46.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_047") )
			lineType = 47.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_048") )
			lineType = 48.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_049") )
			lineType = 49.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_050") )
			lineType = 50.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_051") )
			lineType = 51.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_052") )
			lineType = 52.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_053") )
			lineType = 53.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_054") )
			lineType = 54.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_055") )
			lineType = 55.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_056") )
			lineType = 56.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_057") )
			lineType = 57.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_058") )
			lineType = 58.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_059") )
			lineType = 59.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_060") )
			lineType = 60.0;

		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061") )
			lineType = 61.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_062") )
			lineType = 62.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_063") )
			lineType = 63.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_064") )
			lineType = 64.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_065") )
			lineType = 65.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_066") )
			lineType = 66.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_067") )
			lineType = 67.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_068") )
			lineType = 68.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_069") )
			lineType = 69.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_070") )
			lineType = 70.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_071") )
			lineType = 71.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_072") )
			lineType = 72.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_073") )
			lineType = 73.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_074") )
			lineType = 74.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_075") )
			lineType = 75.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_076") )
			lineType = 76.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_077") )
			lineType = 77.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_078") )
			lineType = 78.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_079") )
			lineType = 79.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_080") )
			lineType = 80.0;

		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_081") )
			lineType = 81.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_082") )
			lineType = 82.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_083") )
			lineType = 83.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_084") )
			lineType = 84.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_085") )
			lineType = 85.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_086") )
			lineType = 86.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_087") )
			lineType = 87.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_088") )
			lineType = 88.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_089") )
			lineType = 89.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_090") )
			lineType = 90.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_091") )
			lineType = 91.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_092") )
			lineType = 92.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_093") )
			lineType = 93.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_094") )
			lineType = 94.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_095") )
			lineType = 95.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_096") )
			lineType = 96.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_097") )
			lineType = 97.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_098") )
			lineType = 98.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_099") )
			lineType = 99.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_103") )
			lineType = 103.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_104") )
			lineType = 104.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_105") )
			lineType = 105.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_107") )
			lineType = 107.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_201") )
			lineType = 201.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_202") )
			lineType = 202.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_203") )
			lineType = 203.0;
		
		else if ( pgenType.equalsIgnoreCase( "ICING_00") )
			lineType = 0.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_01") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_02") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_03") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_04") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_05") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_06") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_07") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_08") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_09") )
			lineType = 9.0;
		else if ( pgenType.equalsIgnoreCase( "ICING_10") )
			lineType = 10.0;
		
		else if ( pgenType.equalsIgnoreCase( "PAST_WX_03") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "PAST_WX_04") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "PAST_WX_05") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "PAST_WX_06") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "PAST_WX_07") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "PAST_WX_08") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "PAST_WX_09") )
			lineType = 9.0;

		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_01") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_02") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_03") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_04") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_05") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_06") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_07") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_08") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_09") )
			lineType = 9.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_11") )
			lineType = 11.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_12") )
			lineType = 12.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_13") )
			lineType = 13.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_14") )
			lineType = 14.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_15") )
			lineType = 15.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_16") )
			lineType = 16.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_17") )
			lineType = 17.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_18") )
			lineType = 18.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_19") )
			lineType = 19.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_21") )
			lineType = 21.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_22") )
			lineType = 22.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_23") )
			lineType = 23.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_24") )
			lineType = 24.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_25") )
			lineType = 25.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_26") )
			lineType = 26.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_27") )
			lineType = 27.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_28") )
			lineType = 28.0;
		else if ( pgenType.equalsIgnoreCase( "CLOUD_TYPE_29") )
			lineType = 29.0;
		
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_0") )
			lineType = 0.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_1") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_2") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_3") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_4") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_5") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_6") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_7") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_8") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_46") )
			lineType = 46.0;
		else if ( pgenType.equalsIgnoreCase( "TURBULENCE_67") )
			lineType = 67.0;

		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_00") )
			lineType = 0.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_01") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_02") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_03") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_04") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_05") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_06") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_07") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "PRESSURE_TENDENCY_08") )
			lineType = 8.0;
		
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_00") )
			lineType = 0.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_01") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_02") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_03") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_04") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_05") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_06") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_07") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_08") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_09") )
			lineType = 9.0;
		else if ( pgenType.equalsIgnoreCase( "SKY_COVER_10") )
			lineType = 10.0;
//combo		
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_051") )
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_063") )
			lineType = 13.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_080") )
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_095") )
			lineType = 21.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_063|PRESENT_WX_065") )
			lineType = 12.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_063|PRESENT_WX_080") )
			lineType = 11.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_063|PRESENT_WX_095") )
			lineType = 15.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_065|PRESENT_WX_080") )
			lineType = 14.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_065|PRESENT_WX_095") )
			lineType = 16.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_051|PRESENT_WX_056") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_056") )
			lineType = 10.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_066") )
			lineType = 26.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_071") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_071|PRESENT_WX_073") )
			lineType = 18.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_073|PRESENT_WX_075") )
			lineType = 19.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_071|PRESENT_WX_085") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_073|PRESENT_WX_085") )
			lineType = 17.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_075|PRESENT_WX_085") )
			lineType = 25.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_071|PRESENT_WX_105") )
			lineType = 23.00;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_073|PRESENT_WX_105") )
			lineType = 20.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_075|PRESENT_WX_105") )
			lineType = 24.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_080|PRESENT_WX_095") )
			lineType = 22.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_080|PRESENT_WX_085") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_061|PRESENT_WX_079") )
			lineType = 27.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_079|PRESENT_WX_066") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "PRESENT_WX_071|PRESENT_WX_079") )
			lineType = 28.0;
//marker		
		else if ( pgenType.equalsIgnoreCase( "PLUS_SIGN"))
			lineType = 1.0;
		else if ( pgenType.equalsIgnoreCase( "OCTAGON"))
			lineType = 2.0;
		else if ( pgenType.equalsIgnoreCase( "TRIANGLE") )
			lineType = 3.0;
		else if ( pgenType.equalsIgnoreCase( "BOX") )
			lineType = 4.0;
		else if ( pgenType.equalsIgnoreCase( "SMALL_X") )
			lineType = 5.0;
		else if ( pgenType.equalsIgnoreCase( "DIAMOND") )
			lineType = 6.0;
		else if ( pgenType.equalsIgnoreCase( "UP_ARROW") )
			lineType = 7.0;
		else if ( pgenType.equalsIgnoreCase( "X_WITH_TOP_BAR") )
			lineType = 8.0;
		else if ( pgenType.equalsIgnoreCase( "Z_WITH_BAR") )
			lineType = 9.0;		
		else if ( pgenType.equalsIgnoreCase( "Y") )
			lineType = 10.0;
		else if ( pgenType.equalsIgnoreCase( "BOX_WITH_DIAGONALS") )
			lineType = 11.0;
		else if ( pgenType.equalsIgnoreCase( "ASTERISK") )
			lineType = 12.0;
		else if ( pgenType.equalsIgnoreCase( "HOURGLASS_X") )
			lineType = 13.0;
		else if ( pgenType.equalsIgnoreCase( "STAR") )
			lineType = 14.0;
		else if ( pgenType.equalsIgnoreCase( "DOT") )
			lineType = 15.0;
		else if ( pgenType.equalsIgnoreCase( "LARGE_X") )
			lineType = 16.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_OCTAGON") )
			lineType = 17.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_TRIANGLE") )
			lineType = 18.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_BOX") )
			lineType = 19.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_DIAMOND") )
			lineType = 20.0;
		else if ( pgenType.equalsIgnoreCase( "FILLED_STAR") )
			lineType = 21.0;
		else if ( pgenType.equalsIgnoreCase( "MINUS_SIGN") )
			lineType = 22.0;
		else
			lineType = 22.0;
			
		return lineType;
	}
	
	
	static int getWndType( String pgenType, Boolean clear) {  //new in J
		int lineType = 0;

		if ( pgenType.equalsIgnoreCase( "Hash") )
			lineType = 1;
		else {
			if ( clear )
				lineType = 114;
			else
				lineType = 112;
		}
		
		return lineType;
	}
	//text
	static int getSPTextType( String pgenType, String avnText, Boolean mask, String displayType) {  //new in J
		int lineType = 0;
		
		if ( pgenType.equalsIgnoreCase( "MID_LEVEL_CLOUD") ) {
			lineType = 15;
		}
		else if ( pgenType.equalsIgnoreCase( "AVIATION_TEXT") ) {
			if ( avnText.equalsIgnoreCase("LOW_PRESSURE_BOX") )
				lineType = 1;
			else if ( avnText.equalsIgnoreCase("HIGH_PRESSURE_BOX") )
				lineType = 2;
			else if ( avnText.equalsIgnoreCase("FLIGHT_LEVEL") )
				lineType = 16;
			else if ( avnText.equalsIgnoreCase("FREEZING_LEVEL") )
				lineType = 6;
			else if ( avnText.equalsIgnoreCase("LOW_LEVEL_TURBULENCE") )
				lineType = 7;
			else if ( avnText.equalsIgnoreCase("CLOUD_LEVEL") )
				lineType = 8;
			else if ( avnText.equalsIgnoreCase("HIGH_LEVEL_TURBULENCE") )
				lineType = 9;
			else if ( avnText.equalsIgnoreCase("MID_LEVEL_ICING") )
				lineType = 12;
		}
		else if ( pgenType.equalsIgnoreCase( "General Text") ) {
			if (mask == true && displayType.equalsIgnoreCase("NORMAL") )
				lineType = 5;
			else if (mask == false && displayType.equalsIgnoreCase("NORMAL") )
				lineType = 0;
			else if (mask == true && displayType.equalsIgnoreCase("BOX") )
				lineType = 4;
			else if (mask == false && displayType.equalsIgnoreCase("BOX") )
				lineType = 3;
			else if (mask == true && displayType.equalsIgnoreCase("UNDERLINE") )
				lineType = 11;
			else if (mask == false && displayType.equalsIgnoreCase("UNDERLINE") )
				lineType = 10;
			else if (mask == true && displayType.equalsIgnoreCase("OVERLINE") )
				lineType = 14;
			else if (mask == false && displayType.equalsIgnoreCase("OVERLINE") )
				lineType = 13;			
			else 
				lineType = 0;
		}
		
		return lineType;
	}
	
	static int getTurbSymType( String symPattern) {  //combined avnText and mid_level_cloud text
		int lineType = 0;
	    if (symPattern == null)
	    	return lineType;
	    
		if ( symPattern.equalsIgnoreCase("ICING_00") || symPattern.equalsIgnoreCase("TURBULENCE_0"))
				lineType = 0;
		else if ( symPattern.equalsIgnoreCase("ICING_01") || symPattern.equalsIgnoreCase("TURBULENCE_1"))
				lineType = 1;
		else if ( symPattern.equalsIgnoreCase("ICING_02") || symPattern.equalsIgnoreCase("TURBULENCE_2"))
				lineType = 2;
		else if ( symPattern.equalsIgnoreCase("ICING_03") || symPattern.equalsIgnoreCase("TURBULENCE_3"))
				lineType = 3;
		else if ( symPattern.equalsIgnoreCase("ICING_04") || symPattern.equalsIgnoreCase("TURBULENCE_4"))
				lineType = 4;
		else if ( symPattern.equalsIgnoreCase("ICING_05") || symPattern.equalsIgnoreCase("TURBULENCE_5"))
				lineType = 5;
		else if ( symPattern.equalsIgnoreCase("ICING_06") || symPattern.equalsIgnoreCase("TURBULENCE_6"))
				lineType = 6;
		else if ( symPattern.equalsIgnoreCase("ICING_07") || symPattern.equalsIgnoreCase("TURBULENCE_7"))
				lineType = 7;
		else if ( symPattern.equalsIgnoreCase("ICING_08") || symPattern.equalsIgnoreCase("TURBULENCE_8"))
				lineType = 8;
		else if ( symPattern.equalsIgnoreCase("ICING_09") )
				lineType = 9;		
		else if ( symPattern.equalsIgnoreCase("TURBULENCE_4|TURBULENCE_6") )
				lineType = 56;
		else if ( symPattern.equalsIgnoreCase("TURBULENCE_6|TURBULENCE_7") )
				lineType = 77;
		else 
			lineType = 0;
		
		return lineType;
	}
	
	//mid_lvl
	static String getMidLevSplit(String string, String oldDelim, String newDelim) {
		if (string == null)
			return "";
		
		String[] s = string.split(oldDelim);
		string = "";
		for (int i=0; i< s.length; i++) {
			string += s[i] + newDelim;			
		}
		string = string.substring(0, string.length()-1);
		
		return string;
	}
	
	//track
	static int getIntervalTime(String time) {
		int interval;
		String[] s = time.split(":");
		interval = (Integer.parseInt(s[0]))*60 + Integer.parseInt(s[1]);
		
		return interval;
	}
	
	static int getSymCode(String category, String pgenType) {
		int symCode = 0;

		if ( category.equalsIgnoreCase( "Lines") && pgenType.equalsIgnoreCase( "LINE_SOLID") )
			symCode = 1;
		else if ( category.equalsIgnoreCase( "Lines") && pgenType.equalsIgnoreCase( "LINE_DASHED_2") )
			symCode = 10;
		
		return symCode;
	}
	
	static int getOutlookType( String outlookType) {
		int outType = 0;

		if (outlookType.equalsIgnoreCase("UNDEF")) // cesgtlmstr.c
			outType = 0;
		else if (outlookType.equalsIgnoreCase("CLOUD"))
			outType = 1;
		else if (outlookType.equalsIgnoreCase("TURB"))
			outType = 2;
		else if (outlookType.equalsIgnoreCase("FRONT"))
			outType = 3;
		else if (outlookType.equalsIgnoreCase("JETS"))
			outType = 4;
		else if (outlookType.equalsIgnoreCase("HIGH"))
			outType = 5;
		else if (outlookType.equalsIgnoreCase("LOW"))
			outType = 6;
		
		else if (outlookType.equalsIgnoreCase("OUTLOOK"))
			outType = 7;
		else if (outlookType.equalsIgnoreCase("LABEL"))
			outType = 8;
		else if (outlookType.equalsIgnoreCase("TROPICL"))
			outType = 9;
		else if (outlookType.equalsIgnoreCase("STNMDL"))
			outType = 10;
		else if (outlookType.equalsIgnoreCase("MRFSTN"))
			outType = 11;
		
		else if (outlookType.equalsIgnoreCase("HAILOTLK"))
			outType = 12;
		else if (outlookType.equalsIgnoreCase("TORNOTLK"))
			outType = 13;
		else if (outlookType.equalsIgnoreCase("WINDOTLK"))
			outType = 14;
		else if (outlookType.equalsIgnoreCase("TOTL_SVR"))
			outType = 15;
		else if (outlookType.equalsIgnoreCase("FIREOUTL"))
			outType = 16;
		else if (outlookType.equalsIgnoreCase("CATG_SVR"))
			outType = 17;
		else if (outlookType.equalsIgnoreCase("MESO_DSC"))
			outType = 18;
		
		else if (outlookType.equalsIgnoreCase("DEV1"))
			outType = 19;
		else if (outlookType.equalsIgnoreCase("DEV2"))
			outType = 20;
		else if (outlookType.equalsIgnoreCase("DEV3"))
			outType = 21;
		else if (outlookType.equalsIgnoreCase("DEV4"))
			outType = 22;
		else if (outlookType.equalsIgnoreCase("DEV5"))
			outType = 22;		
		else if (outlookType.equalsIgnoreCase("TSTMOLK"))
			outType = 24;
		else if (outlookType.equalsIgnoreCase("EXT_SVR"))
			outType = 25;
		else if (outlookType.equalsIgnoreCase("EXT_FIRE"))
			outType = 26;
		else if (outlookType.equalsIgnoreCase("ISOBARS"))
			outType = 27;
		else if (outlookType.equalsIgnoreCase("HI_FCST"))
			outType = 28;
		else if (outlookType.equalsIgnoreCase("LO_FCST"))
			outType = 29;
		else if (outlookType.equalsIgnoreCase("WHFT"))
			outType = 30;
		else if (outlookType.equalsIgnoreCase("WHM"))
			outType = 31;
		else if (outlookType.equalsIgnoreCase("WPER"))
			outType = 32;
		else if (outlookType.equalsIgnoreCase("PROB"))
			outType = 33;
		else if (outlookType.equalsIgnoreCase("ENH20"))
			outType = 34;
		else if (outlookType.equalsIgnoreCase("ENH00"))
			outType = 35;
		else if (outlookType.equalsIgnoreCase("ENH04"))
			outType = 36;
		else if (outlookType.equalsIgnoreCase("ENH12"))
			outType = 37;
		else if (outlookType.equalsIgnoreCase("ENH16"))
			outType = 38;
		else //if (outlookType.equalsIgnoreCase("FLOOD"))
			outType = 8;
				
		return outType;
	}
	
	protected static String getPointLats(DrawableElement de) {
		float pointLat = 0;
		String pointsLat = "";
		List<Coordinate> points = de.getPoints();
		
		for (int a=0; a<points.size(); a++) {
			String[] result = points.get(a).toString().split(",");
			if (result.length ==3) {
				try {
					pointLat = new BigDecimal(result[1].trim()).setScale( 2, BigDecimal.ROUND_HALF_UP ).floatValue();
				}
				catch (NumberFormatException e) {
					System.out.println(result[1] + " is not a number.");
				}
				pointsLat += pointLat + ", ";
			}    						
		}
		
		if (pointsLat.length() >2)
			pointsLat = pointsLat.substring(0, pointsLat.length()-2); //remove last ", "
		
		return pointsLat;
	}
	
	protected static String getPointLons(DrawableElement de) {
		float pointLon = 0;
		String pointsLon = "";
		List<Coordinate> points = de.getPoints();
		
		for (int a=0; a<points.size(); a++) {
			String[] result = points.get(a).toString().split(",");
			if (result.length ==3) {
				try {
					pointLon = new BigDecimal(result[0].substring(1).trim()).setScale( 2, BigDecimal.ROUND_HALF_UP ).floatValue();
				}
				catch (NumberFormatException e) {
					System.out.println(result[0].substring(1) + " is not a number.");
				} 
				pointsLon += pointLon + ", ";
			}   						
		}
		
		if (pointsLon.length() >2)
			pointsLon = pointsLon.substring(0, pointsLon.length()-2); //remove last ", "
		
		return pointsLon;
	}
	
	protected static String getPoints(DrawableElement de) {
		
		return getPointLats(de) + ", " + getPointLons(de);
	}
	
	protected static float[] getSortedLat(DrawableElement de) {		
		List<Coordinate> points = de.getPoints();
		
		float[] pointsLat = new float[points.size()];
		
		for (int a=0; a<points.size(); a++) {
			String[] result = points.get(a).toString().split(",");
			if (result.length ==3) {
				try {
					pointsLat[a] = new BigDecimal(result[1].trim()).setScale( 2, BigDecimal.ROUND_HALF_UP ).floatValue();
				}
				catch (NumberFormatException e) {
					System.out.println(result[1] + " is not a number.");
				}
			}    						
		}		
		Arrays.sort(pointsLat);
		
		return pointsLat;
	}
	
	protected static float[] getSortedLon(DrawableElement de) {		
		List<Coordinate> points = de.getPoints();
		
		float[] pointsLon = new float[points.size()];
		
		for (int a=0; a<points.size(); a++) {
			String[] result = points.get(a).toString().split(",");
			if (result.length ==3) {
				try {
					pointsLon[a] = new BigDecimal(result[0].substring(1).trim()).setScale( 2, BigDecimal.ROUND_HALF_UP ).floatValue();
				}
				catch (NumberFormatException e) {
					System.out.println(result[0].substring(1) + " is not a number.");
				}
			}    						
		}		
		Arrays.sort(pointsLon);
		
		return pointsLon;
	}
	
	public static String getColorMaj(DrawableElement de) {
		Color[] color;
		String colMaj = "";
		
		color = de.getColors(); //[Ljava.awt.Color;@edf730    
		if (color != null) {
			String tem = color[0].toString(); //java.awt.Color[r=0,g=255,b=255]	    		
			colMaj =tem.substring(tem.indexOf("[")+1, tem.indexOf("]"));
		}
		return colMaj;
	}
	
	public static String getColorMin(DrawableElement de) {
		Color[] color;
		String colMin = "";
		
		color = de.getColors();     
		if (color != null && color.length == 1)
			return getColorMaj( de);
		else if (color != null && color.length >1) {
			String tem = color[1].toString(); 	    		
			colMin =tem.substring(tem.indexOf("[")+1, tem.indexOf("]"));
		}
		return colMin;
	}
	
	public static boolean typeSearch(String[] types, String name) {
	
		for (int n = 0; n < types.length; n++)
		{
			if (types[n].equals(name))
				return true;
		}
		return false;
	}
}