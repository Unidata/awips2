package gov.noaa.nws.ncep.gempak.parameters.hilo;


import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;


/**
 * HILO contains the information for plotting relative highs and lows
 *    in the following format:
 *
 *       colorh;colorl/symbolh;symboll/rangeh;rangel/radius/counth;countl/interp
 *
 *    Colorh and colorl are the colors for the high and low symbols to be
 *    plotted.  If only a single number is entered, it will be used for
 *    both highs and lows.  The default for this entry is 0.
 *
 *    Symbolh and symboll specify the symbols to be plotted.  The format
 *    for the symbol input is:
 *               character # precision
 *
 *    where the character is the character to be plotted.  If the character
 *    is an integer, markers corresponding to that number will be plotted.
 *    If the character is an integer preceeded by the character 'S', special
 *    symbols corresponding to that number will be plotted.
 *    Information about markers can be found in the help for MARKER.
 *    The # is a flag to plot values beneath the marker.  The integer following
 *    the # is the number of decimal places to display in the value.  If a
 *    # is present without the following number, integer values are printed.
 *    The default for the symbols is H;L.
 *
 *    Rangeh and rangel are ranges for highs and lows specified as:
 *               minval - maxval
 *
 *    where minval and maxval are integers which specify the range of values 
 *    to be considered for designation as a high or low.  The default is to 
 *    consider all data.
 *
 *    The search radius is a sqaure region of grid points. The region is a
 *    moving search area in which all points are compared to derive relative
 *    extrema. The default is 3, which is generally the most effective. To
 *    find more concentrated areas of highs and lows, use a smaller radius.
 *    A large radius, such as 10 or higher, is not very effective. 
 *
 *   Counth and countl are integer values for the maximum number of
 *   high and low values to be displayed.  The default is 20;20.
 *   
 *   Interp is an interpolation flag which specifies whether the values and
 *   locations of the highs and lows will be at grid points, or will be 
 *   interpolated between grid points.  The default is NO.
 *   
 *   In general, the above defaults are used if there is no entry for
 *   a part.  For the parts which have values for both relative highs
 *   and lows, a single entry will be used for both highs and lows.
 * <p>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19,2010   323         X. Guo       Initial Creation
 * Nov 09,2010               X. Guo       Change double to float
 * Dec 06,2010				 X. Guo       Update Symbol Hi/Lo parser
 *                                         
 * </pre>
 * @author xguo
 * @version 1
 * @see $GEMPAK/help/hlx/hilo.hl2 
 */
public class HILOStringParser {
	/**
	 * The object that actually validates each parsed input and generates the lists
	 * of attributes for the HiLo parameters. 
	 **/
	private HILOBuilder hiloBuilder;
	
	//Input string
	private String hiloInputString;
	
	//Parse Flag
	private boolean isHiLoStringParsed;
	
	//Error message
	private String hiloParserErrorMsg = "NO ERROR";
	
	//expression
	private String hiloExpre = "((-|\\+)?[0-9]+(\\.[0-9]+)?)+";

	private final String [] hiloParserErrorDesc = {
						"Empty HILO Input String",
						"Invalid Color in HILO Input String",
						"Color Value Out of Range[1-32] in HILO Input String",
						"Invalid High Color in HILO Input String",
						"High Color Value Out of Range[1-32] in HILO Input String",
						"Invalid Low Color in HILO Input String",
						"Low Color Value Out of Range[1-32] in HILO Input String",
						"Invalid High Symbol Precision in HILO Input String",
						"Invalid Low Symbol Precision in HILO Input String",
						"Invalid High Range Minimun Value in HILO Input String",
						"Invalid High Range Maximun Value in HILO Input String",
						"High Range Minimum value greater than Maximun Value in HILO Input String",
						"Invalid Low Range Minimun Value in HILO Input String",
						"Invalid Low Range Maximun Value in HILO Input String",
						"Low Range Minimum value greater than Maximun Value in HILO Input String",
						"Invalid Radius Value in HILO Input String",
						"Invalid High Count Value in HILO Input String",
						"Invalid Low Count Value in HILO Input String"
	};
	
	
	protected enum HILOParseErrCodes{
						EMPTY(0),
						INVALID_COLOR(1),
						COLOR_OOR(2),
						INVALID_COLOR_HI(3),
						COLOR_HI_OOR(4),
						INVALID_COLOR_LO(5),
						COLOR_LO_OOR(6),
						INVALID_SYMBOL_PREC_HI(7),
						INVALID_SYMBOL_PREC_LO(8),
						INVALID_RANGE_HI_MIN(9),
						INVALID_RANGE_HI_MAX(10),
						RANGE_HI_MIN_GT_MAX(11),
						INVALID_RANGE_LO_MIN(12),
						INVALID_RANGE_LO_MAX(13),
						RANGE_LO_MIN_GT_MAX(14),
						INVALID_RADIUS(15),
						INVALID_COUNT_HI(16),
						INVALID_COUNT_LO(17);
		private final int value;
		
		HILOParseErrCodes (int value){
			this.value = value;
		}
	}
	/**
	 * The default constructor <code>HILOStringParser</code> generates a single
	 **/
	public HILOStringParser(){
		/*A single line with all default attributes set is created*/
		hiloBuilder = new HILOBuilder();
		isHiLoStringParsed = true;
	}
	
	//HILO construct
	public HILOStringParser ( String hiloStr ) {
		
		isHiLoStringParsed = false;
		String tmpStr = null;
		
		setInputHiLoString ( hiloStr );
		hiloBuilder = new HILOBuilder();
		if(!(hiloStr == null) && !(hiloStr.isEmpty())){
			if(hiloStr.contains(" ")){
				String strWithoutBlank = removeBlanksWithinString (hiloStr);
				if ( strWithoutBlank.length() > 0 ) {
					tmpStr = strWithoutBlank;
				}
			}
			else {
				tmpStr = hiloStr;
			}
		}
		
		if ( tmpStr != null ) {
			isHiLoStringParsed = parseHILOString ( tmpStr );
		}
		else {
			this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.EMPTY.value) );
		}
	}
	
	/*
	 * Parse HILO input string
	 * This subroutine scans the user input for HILO.  This consists of 
     * six input strings separated by slashes.  When a string can be two    
     * substrings separated by a semicolon, the first substring applies     
     * to highs, the second to lows.  If no substring is given a default    
     * is provided.  The table below summarizes the input format and        
     * gives the defaults.                                                  
     *                                                                      
     *  STRING      Description        Format      Example     Default      
     *    1       Color number          i;i           1           0         
     *    2       Symbol#precision    c#i;c#i      H#2;L#1       H;L        
     *    3       Range               i-i;i-i       30-40      0-0;0-0      
     *    4       Search radius          i            4           3         
     *    5       Count                 i;i         10;5        20;20       
     *    6       Interpolation flag     l            T           F         
	 */
	private boolean parseHILOString ( String hilo ) {

		boolean pasrseCorrected = true;
		int pos, start = 0, cnt = 0;
		String hiloStr = hilo + "/";
		
		/*
		 * check and see if the input string with 6 strings separated by slashes
		 * otherwise use all default vaules 
		 */
		while (( pos = hiloStr.indexOf("/", start)) >= 0 ) {
			if ( pos != start ) {
				String tmp = hiloStr.substring(start, pos);
				if ( cnt == 0 ) { //parse color high/low
					pasrseCorrected = parseColorHiLo (tmp);
					if ( ! pasrseCorrected ) return pasrseCorrected;
				}
				else if ( cnt == 1 ) { //parse symbol high/low
					pasrseCorrected = parseSymbolHiLo ( tmp);
					if ( ! pasrseCorrected ) return pasrseCorrected;
				}
				else if ( cnt == 2 ) { //parse range high/low
					pasrseCorrected = parseRangeHiLo ( tmp );
					if ( ! pasrseCorrected ) return pasrseCorrected;
				}
				else if ( cnt == 3 ) { // parse radius
					pasrseCorrected = parseRadius (tmp);
					if ( ! pasrseCorrected ) return pasrseCorrected;
				}
				else if ( cnt == 4 ) { //parse count high/low
					pasrseCorrected = parseConutHiLo ( tmp );
					if ( ! pasrseCorrected ) return pasrseCorrected;
				}
				else if ( cnt == 5 ) { //parse interpolation flag
					parseInterpolation ( tmp );
				}
				else {
					return pasrseCorrected;
				}
			}
			start = pos + 1;
			cnt ++;
		}

		return pasrseCorrected;
	}
	
	/*
	 * parse color high/low
	 */
	private boolean parseColorHiLo ( String colorHiLoStr ) {
		boolean hiloParsed = true;
		int pos, value, cnt=0,start=0;
		String tmp, colorStr;
		
		if ( colorHiLoStr.contains(";") ) {
			if ( colorHiLoStr.length() > 1 ) {
				colorStr = colorHiLoStr + ";";
				while ((pos = colorStr.indexOf(";" , start)) >=0 ){
					if ( pos != start ) {
						tmp = colorHiLoStr.substring(start, pos);
						if ( tmp.matches(hiloExpre)) {
							value = Integer.valueOf(tmp);
							if ( (value > 0 ) && ( value <= 32 )) {
								if ( cnt == 0 ) {
									this.hiloBuilder.setColorHi( GempakColor.convertToRGB(value) );
									this.hiloBuilder.setInputColorHi( value );
								}
								else {
									this.hiloBuilder.setColorLo( GempakColor.convertToRGB(value) );
									this.hiloBuilder.setInputColorLo( value );
								}
							}
							else {
								hiloParsed = false;
								if ( cnt == 0 ) {
									this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.COLOR_HI_OOR.value) );
								}
								else {
									this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.COLOR_LO_OOR.value) );
								}
							}
						}
						else {
							hiloParsed = false;
							if ( cnt == 0 ) {
								this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_COLOR_HI.value) );
							}
							else {
								this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_COLOR_LO.value) );
							}
						}
					}
					if ( (! hiloParsed) || ( cnt >= 1 )) break;
					start = pos + 1 ;
					cnt ++;
				}
			}
		}
		else {
			if (colorHiLoStr.matches(hiloExpre)) {
				value = Integer.valueOf(colorHiLoStr);
				if ( (value > 0 ) && (value <=32 )) {
					this.hiloBuilder.setColorHi ( GempakColor.convertToRGB(value) );
					this.hiloBuilder.setColorLo ( GempakColor.convertToRGB(value) );
					this.hiloBuilder.setInputColorHi(value);
					this.hiloBuilder.setInputColorLo(value);
				}
				else {
					hiloParsed = false;
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.COLOR_OOR.value) );
				}
			}
			else {
				if ( colorHiLoStr.length() != 0 ) {
					hiloParsed = false;
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_COLOR.value) );
				}
			}
		}
		return hiloParsed;
	}
	
	/*
	 * parse symbol high/low 
	 * If the first string is a single number, that number is the color     
     * number for both highs and lows.  In the second string, the c in the  
     * table stands for either a character string, or a marker or special   
     * symbol number. Special symbols are specified by preceeding the       
     * number with an 'S'. The # sign is a flag to plot the values beneath  
     * the marker.  The number following # is the number of decimal places  
     * to display in the value.  If the number is absent, integer values are
     * displayed.                                                           
     *                                                                      
     * The defaults above apply if no string is entered.  If one of the     
     * entries of a string is entered, it is assumed to apply to highs      
     * unless it is preceded by a semicolon.  The omitted entry defaults    
     * according to the table below:                                        
     *                                                                      
     *    STRING          DEFAULT FOR OMITTED PART                          
     *       1                 Same value                                   
     *       2                 No symbol                                    
     *       3                 Missing values                               
     *       5                 Same value                                   
     *                                                                     
	 */
	private boolean parseSymbolHiLo ( String symbolHiLoStr ) {
		
		boolean symbolParsed = true;
		String tmp;
		String symbolStr;
		int pos, cnt=0,start=0;
		
		if ( symbolHiLoStr.contains(";")) {
			if ( symbolHiLoStr.length() <= 1 ) {
				return symbolParsed;
			}
			symbolStr = symbolHiLoStr + ";";
		}
		else {
			symbolStr = symbolHiLoStr + ";" + symbolHiLoStr + ";";
		}

		while ((pos = symbolStr.indexOf(";", start)) >= 0 ) {
			if ( pos != start ) {
				tmp = symbolStr.substring(start, pos);
				symbolParsed = parseSymbol ( tmp, cnt );
			}
			if ( (!symbolParsed ) || ( cnt >= 1 ) ) break;
			start = pos + 1;
			cnt ++;
		}
		return symbolParsed;
	}
	
	/**
	 * Parse symbol string. Format: character # precision
	 * @param symbol string
	 * @return true/false
	 */
	private boolean parseSymbol ( String symbol, int cnt ){
		int pos;
		boolean parse = true;
		String tmp1 = null, tmp2 = null;
		
		if ( ( pos = symbol.indexOf("#", 0)) >= 0 ) {
			if ( pos != ( symbol.length() -1 ) ){
				if ( pos == 0 ) { 
					tmp2 = symbol.substring(pos+1, symbol.length());
				}
				else if ( pos == symbol.length()-1 ) {
					tmp1 = symbol.substring(0, pos);
				}
				else {
					tmp1 = symbol.substring(0, pos);
					tmp2 = symbol.substring(pos+1, symbol.length());
				}
			}
			if ( cnt == 0 ) this.hiloBuilder.setSymbolHiPlotValue(true);
			else this.hiloBuilder.setSymbolLoPlotValue(true);
		}
		else {
			tmp1 = symbol;
		}
        
		/*
		 * Check character and precision
		 */
		if ( tmp1 != null ) {//parse character
			String tmp = tmp1.substring(1, tmp1.length());
			if ( tmp1.matches(hiloExpre)) {//after the first character, set marker number
				if ( cnt == 0 ) {
					this.hiloBuilder.setSymbolHi(" ");
					this.hiloBuilder.setSymbolHiMarkerNumber(Integer.valueOf(tmp1));
					this.hiloBuilder.setSymbolHiType(2);
				}
				else {
					this.hiloBuilder.setSymbolLo(" ");
					this.hiloBuilder.setSymbolLoMarkerNumber(Integer.valueOf(tmp1) );
					this.hiloBuilder.setSymbolLoType(2);
				}
			}
			else if ( tmp.matches(hiloExpre) ){//check marker number
				if ( cnt == 0 ) {
					this.hiloBuilder.setSymbolHi(" ");
					this.hiloBuilder.setSymbolHiMarkerNumber(Integer.valueOf(tmp));
				}
				else {
					this.hiloBuilder.setSymbolLo(" ");
					this.hiloBuilder.setSymbolLoMarkerNumber(Integer.valueOf(tmp) );
				}
				
				String firstChar = tmp1.substring(0, 1);
				if (firstChar.toUpperCase().compareTo("S")== 0) {//check the first character for 'S'
					if ( cnt == 0 ) this.hiloBuilder.setSymbolHiType(3);
					else this.hiloBuilder.setSymbolLoType(3);
				}
				else {
					if ( cnt == 0 ) this.hiloBuilder.setSymbolHiType(2);
					else this.hiloBuilder.setSymbolLoType(2);
				}
			}
			else {//the string are characters before '#' sign
				if ( cnt == 0 ) {
					this.hiloBuilder.setSymbolHi(tmp1);
					this.hiloBuilder.setSymbolHiType(1);
				}
				else {
					this.hiloBuilder.setSymbolLo(tmp1);
					this.hiloBuilder.setSymbolLoType(1);
				}
			}
		}
		else if ( pos == 0 ) {
			tmp1 = "";
			if ( cnt == 0 ) {
				this.hiloBuilder.setSymbolHi(tmp1);
				this.hiloBuilder.setSymbolHiType(1);
			}
			else {
				this.hiloBuilder.setSymbolLo(tmp1);
				this.hiloBuilder.setSymbolLoType(1);
			}
		}
		
		if ( tmp2 != null ) {//parse precision
			if ( tmp2.matches(hiloExpre) && (Integer.valueOf (tmp2) >= 0)) { //check and see precision is positive integer 
				if ( cnt == 0 ) this.hiloBuilder.setSymbolHiNumOfDecPls( Integer.valueOf(tmp2) );
				else this.hiloBuilder.setSymbolLoNumOfDecPls( Integer.valueOf(tmp2) );
			}
			else {
				if ( cnt == 0 ) this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_SYMBOL_PREC_HI.value) );
				else this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_SYMBOL_PREC_LO.value) );
				parse = false;
			}
		}
		return parse;
	}
	/*
	 *     Rangeh and rangel are ranges for highs and lows specified as:
	 *     minval - maxval
	 *     where minval and maxval are integers which specify the range of values
	 *     to be considered for designation as a high or low. The default is to consider all data.
	 */
	private boolean parseRangeHiLo ( String rangStr ) {
		
		boolean rangeParsed = true;
		int pos, start = 0, cnt = 0;
		String range;
		
		if ( rangStr.toUpperCase().compareTo("ALL") != 0 ) {
			
			if ( rangStr.contains(";")) {
				if ( rangStr.length() <= 1 ) {
					return rangeParsed;
				}
				range = rangStr + ";";
			}
			else {
				range = rangStr + ";" + rangStr + ";";
			}


			while ((pos = range.indexOf(";", start)) >= 0 ) {
				if ( pos != start ) {
					String tmp = range.substring(start, pos);
					rangeParsed = parseRange ( tmp, cnt );
				}
				if ( (!rangeParsed ) || ( cnt >= 1 ) ) break;
				start = pos + 1;
				cnt ++;
			}
		}
		return rangeParsed;
	}
	
	/**
	 * Parse Range string
	 * @param hilo range string
	 * @return boolean
	 */
	private boolean parseRange ( String rangeStr, int cnt ){
		int pos;
		boolean parse = true;
		float maxVal=0.0f, minVal=0.0f;
		String tmp1 = null, tmp2 = null;
		

		if ( ( pos = rangeStr.indexOf("-", 1)) >= 0 ) {
			if ( pos == 1 ) { 
				tmp2 = rangeStr.substring(pos+1, rangeStr.length());
			}
			else if ( pos == rangeStr.length()-1 ) {
				tmp1 = rangeStr.substring(0, pos);
			}
			else {
				tmp1 = rangeStr.substring(0, pos);
				tmp2 = rangeStr.substring(pos+1, rangeStr.length());
			}
		}
		else {
			tmp1 = rangeStr;
			tmp2 = rangeStr;
		}

		/*
		 * check minval
		 */
		if ( tmp1 != null ) {
			if ( tmp1.matches(hiloExpre)) {
				minVal = Float.valueOf(tmp1);
				if ( cnt == 0 ) this.hiloBuilder.setRangeHiMinval(minVal);
				else this.hiloBuilder.setRangeLoMinval ( minVal );
			}
			else {
				if ( cnt == 0 ) {
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_RANGE_HI_MIN.value) );
				}
				else {
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_RANGE_LO_MIN.value) );
				}
				parse = false;
			}
		}
		/*
		 * check maxval
		 */
		if ( parse && (tmp2 != null)) {
			if ( tmp2.matches(hiloExpre)) {
				maxVal = Float.valueOf(tmp2);
				if ( cnt == 0 ) this.hiloBuilder.setRangeHiMaxval(maxVal);
				else this.hiloBuilder.setRangeLoMaxval ( maxVal );
			}
			else {
				if ( cnt == 0 ) {
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_RANGE_HI_MAX.value) );
				}
				else {
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_RANGE_LO_MAX.value) );
				}
				parse = false;
			}
		}
		
		/*
		 * compare minval and maxval
		 */
		if ( parse && (tmp1 != null) && (tmp2 != null)) {
			if ( maxVal < minVal ) {
				if ( cnt == 0 ) {
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.RANGE_HI_MIN_GT_MAX.value) );
				}
				else {
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.RANGE_LO_MIN_GT_MAX.value) );
				}
				parse = false;
			}
		}
		return parse;
	}
	
	/**
	 * parse and verify Radius
	 * @param Radius string
	 * @return boolean
	 */
	private boolean parseRadius ( String radiusStr ) {
		boolean parse = true;
		
		if ( radiusStr !=null && radiusStr.length() > 0) {
			if ( radiusStr.trim().matches(hiloExpre) ) {
				int val = Integer.valueOf(radiusStr.trim());
				if ( val >= 0 ) {
					this.hiloBuilder.setRadius( val );
				}
				else {
					this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_RADIUS.value) );
					parse = false;
				}
			}
			else {
				this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_RADIUS.value) );
				parse = false;
			}
		}
		return parse;
	}
	
	/**
	 * parse and verify Count Hi/Lo
	 * @param count high/low string
	 * @return boolean
	 */
	private boolean parseConutHiLo ( String countHiLoStr ) {
		int start = 0, cnt = 0 ;
		boolean parse = true;
		String countStr = countHiLoStr;
		int pos = countHiLoStr.indexOf(';');
		
		if ( pos < 0 ) {
			countStr = countHiLoStr + ";" + countHiLoStr ;
		}
		countStr = countStr + ";";
		while ( ( pos = countStr.indexOf(";", start)) >= 0 ) {
			if ( pos != start ) {
				String tmp = countStr.substring(start, pos);
				if ( tmp.matches(hiloExpre) && (Integer.valueOf(tmp) > 0 )) {
					if ( cnt == 0 ) this.hiloBuilder.setCountHi( Integer.valueOf(tmp) );
					else this.hiloBuilder.setCountLo( Integer.valueOf(tmp) );
				}
				else {
					if ( cnt == 0 ) {
						this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_COUNT_HI.value) );
					}
					else {
						this.setErrorMessage ( getParaserErrorDec (HILOParseErrCodes.INVALID_COUNT_LO.value) );
					}
					parse = false;
				}
			}
			if ( ( ! parse ) || ( cnt >= 1 ) ) break;
			start =pos + 1;
			cnt ++;
		}

		return parse;
	}
	
	
	/**
	 * parse Interpolation
	 * @param interpolation string
	 * @return null
	 */
	private void parseInterpolation ( String interpStr ) {
		if ( interpStr !=null && interpStr.length() > 0) {
			String tmp = interpStr.trim().substring(0, 1);
			if ( tmp.toUpperCase().compareTo("Y") == 0 ){
				this.hiloBuilder.setInterp( true );
			}
		}
	}
	/**
	 * @return boolean
	 */
	public boolean isHiLoStringParsed () {
		return this.isHiLoStringParsed;
	}
	/**
	 * @param String  hiloInputString
	 */
	private void setInputHiLoString(String hiloInputString) {
		this.hiloInputString = hiloInputString;
	}
	
	/**
	 * @return String input string
	 */
	public String getInputHiLoString( ) {
		return this.hiloInputString;
	}
	
	/**
	 * return error message
	 */
	public String getErrorMessage () {
		return this.hiloParserErrorMsg ;
	}
	
	/**
	 * @param String error message
	 */
	private void setErrorMessage ( String errorMsg )
	{
		this.hiloParserErrorMsg = errorMsg ;
	}
	
	/**
	 * Get parser error description
	 */
	private String getParaserErrorDec ( int value )
	{
		return hiloParserErrorDesc[value] ;
	}
	
	/**
	 * The method <tt>removeBlanksWithinString</tt> splits the input string into
	 * tokens using a blank-space as a delimiter.
	 * Then it concatenates the tokens, as long as the tokens are non-empty.
	 * @param String inputString
	 * @return String outputString
	 */
	private String removeBlanksWithinString (String inputString){
		StringBuffer strBuff = new StringBuffer();
		String outputString = " ";
		String tokens[] = inputString.split(" ");
		for(String s : tokens){
			if(!(s.isEmpty())){
				strBuff.append(s);
			}
		}
		outputString = strBuff.toString();
		outputString.trim();
        return outputString;
	}
	
	/**
	 * @return HILOBuilder hiloBuilder
	 **/
	public HILOBuilder getInstanceOfHiLoBuilder(){
		return this.hiloBuilder;
	}
	
}

