/*
 * GGLIMS
 * 
 * Date created (23 November 2009)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.gempak.parameters.gridlimits;

import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;

import java.util.regex.Pattern;

/**<pre>
* The GGLIMS (Graph Grid Limits)class parses a user 
* entered string of the format:
* <tt><i>lowerLimit;minValue | upperLimit;maxValue | defaultValue</i></tt>
* where all the individual constituents of this string
* are floating point numbers.
* The sole objective of this class is to extract the 
* floating point numbers from the string and store them.
* There are public methods to access these floating point 
* numbers, so that other classes may use them to implement
* their business logic.
* The purpose of the numbers minValue and lowerLimit is to 
* ensure that:
* Any value in the grid that is less than the minimum threshold
* value(minValue) is set to the number stored as the
* lower limit(lowerLimit).
* Likewise, the purpose of the numbers maxValue and 
* upperLimit is to ensure that:
* Any value in the grid that is higher than the 
* maximum threshold value(maxValue) is set to the number 
* stored as the upper limit(upperLimit).
* If lowerLimit > upperLimit, then all the extracted
* numbers are set to NaN.
* 
* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 23-Nov-2009    199        Archana.S   Initial Creation
* 25-Nov-2009    199        Archana.S   Updated per review comments:
*                                       1.Changed the initialization values for
* 										for lowerLimit and minValue to -Float.MAX_VALUE
*                                       2.Changed the initialization values for
*       								for upperLimit and maxValue to Float.MAX_VALUE                                        	
*                                       3.Added the method parseGridValuesAndLimits.
*                                       
* 30-Nov-2009    199        Archana.S   Updated the logic of the <tt>parse</tt> method to set all
*                                       extracted values to NaN, if the lowerLimit is
*                                       greater than the upperLimit.  
* 07-Jun-2010    199        Archana.S    Updated the method call to 
*                                        the private method removeBlanksWithinString()
*                                        to use the static method from the StringUtilities
*                                        class and deleted the private method
*                                        removeBlanksWithinString() from this class.                                                                    
*                                         Renamed the package as
*                                         gov.noaa.nws.ncep.gempak.parameters.gridlimits       
*                                       
* </pre>
* @author Archana.S
* @version 1
*/
public class GGLIMS {

	/**The minimum allowable limit*/
	private Float lowerLimit;
	
	/**The maximum allowable limit*/
	private Float upperLimit;
	
	/**The minimum value to set*/
	private Float minValue;
	
	/**The maximum value to set*/
	private Float maxValue;
	
	/**The default grid value to be used if the minimum/maximum grid value is missing*/
	private Float defaultGridValue;
	
	/**The string entered by the user*/
	private String ggLimitsStr;

	/**Regular expression to check for a real number*/
	private String REAL_NUMBER            = "-?\\d*\\.?\\d*";
	
	/***
	 * Default constructor - initializes the instance variables
	 * The lower limit and minimum value are set to <tt>-Float.MAX_VALUE</tt> (-3.4028235E38)
	 * Upper limit and maximum value are set to <tt>Float.MAX_VALUE</tt> (3.4028235E38)
	 * 
	 */
	public GGLIMS(){
		/*Initialize the instance variables*/
		setLowerLimit(-Float.MAX_VALUE);
		setMinValue(-Float.MAX_VALUE);
		setUpperLimit(Float.MAX_VALUE);
		setMaxValue(Float.MAX_VALUE);
		setDefaultGridValue(Float.NaN);
	}
	
	/**
	 * This overloaded constructor invokes the <tt>parse</tt> method on input string to 
	 * extract the grid value limits.
	 * Lower limit and minimum value are initialized to <tt>-Float.MAX_VALUE</tt> (-3.4028235E38).
	 * Upper limit and maximum value are initialized to <tt>Float.MAX_VALUE</tt> (3.4028235E38).<p>
	 * If the string entered by the user contains blanks, the method 
	 * <tt>removeBlanksWithinString</tt> is invoked
	 * to return the original input string minus the blanks and this string is then parsed 
	 * using the <tt>parse</tt> method.
	 * Alternatively, if the input string does not contain any blanks, the parse method is
	 * directly invoked on it.
	 * @param gglimsString - the user input string containing the grid limits data
	 */
	public GGLIMS(String gglimsString){
		
		/*Initialize the instance variables*/
		setLowerLimit(-Float.MAX_VALUE);
		setMinValue(-Float.MAX_VALUE);
		setUpperLimit(Float.MAX_VALUE);
		setMaxValue(Float.MAX_VALUE);		
		setDefaultGridValue(Float.NaN);
		
		/*Store the input string*/
		setGgLimitsStr(gglimsString);
		
		/*If the input string is not null and not empty*/
		if((gglimsString != null) && !(gglimsString.isEmpty())){
			/*If the input string contains a blank character*/
			if(gglimsString.contains(" ")){
				/*remove the blanks*/
				String strWithoutBlanks = StringUtil.removeBlanksWithinString(gglimsString);
				/*invoke the parse method on the string without blanks*/
                  parse(strWithoutBlanks);				
			}else{
				/*Else directly invoke the parse method on the input string*/
                  parse(gglimsString);				
			}
		}
	}
	
	
	/**
	 * @return the lower grid limit
	 */
	public Float getLowerLimit() {
		return lowerLimit;
	}

	/**
	 * @return the upper grid limit
	 */
	public Float getUpperLimit() {
		return upperLimit;
	}

	/**
	 * @return the minimum grid value
	 */
	public Float getMinValue() {
		return minValue;
	}

	/**
	 * @return the maximum grid value
	 */
	public Float getMaxValue() {
		return maxValue;
	}

	/**
	 * @return the default grid value
	 */
	public Float getDefaultGridValue() {
		return defaultGridValue;
	}
	

	/**
	 * @return the user input string containing the GGLIMS data
	 */
	public String getGgLimitsStr() {
		return ggLimitsStr;
	}


	/**
	 * Stores the lower limit for the grid values 
	 * @param lowerLimit the lower grid limit to set
	 */
	private void setLowerLimit(Float lowerLimit) {
		this.lowerLimit = lowerLimit;
	}
	
	/**
	 * Stores the upper limit for the grid values
	 * @param upperLimit the upper grid limit to set
	 */
	private void setUpperLimit(Float upperLimit) {
		this.upperLimit = upperLimit;
	}

	/**
	 * Stores the minimum threshold value for the grid values.
	 * @param minValue the minimum grid value to set
	 */
	private void setMinValue(Float minValue) {
		this.minValue = minValue;
	}

	/**Stores the maximum threshold value for the grid values
	 * @param maxValue the maximum grid value to set
	 */
	private void setMaxValue(Float maxValue) {
		this.maxValue = maxValue;
	}

	/**Stores the default grid value
	 * @param defaultGridValue the default grid value to set
	 */
	private void setDefaultGridValue(Float defaultGridValue) {
		this.defaultGridValue = defaultGridValue;
	}

	/**Stores the text input entered by the user. It must comprise of data in the 
	 * format listed below:
	 * <i>lowerLimit;minValue | upperLimit;maxValue | defaultValue</i>
	 * @param ggLimitsStr the user input string containing the GGLIMS data
	 */
	private void setGgLimitsStr(String ggLimitsStr) {
		this.ggLimitsStr = ggLimitsStr;
	}
	
    /**
     * Parses the input string to extract the lower/upper grid limits, 
     * minimum/maximum grid values and the default value.<p>
     * After the parse operation is completed, if <i>valid</i> lower and upper 
     * grid limit values have been extracted and if the lower limit is found 
     * to be greater than the upper limit, then all the extracted values 
     * are set to NaN. 
     * @param strToParse - the string containing the GGLIMS data
     */
	private void parse(String strToParse){
		String[] tokens;
        Float[] floatTokens = {Float.NaN,Float.NaN};
		
		/*If the input string contains a '|' character*/
		if(strToParse.contains("|")){
			
			/*Split it into tokens using the '|' character as a delimiter
			 *Since '|' is a meta-character in regular expressions, it is 
			 *escaped using the '/' character, which in turn must also be escaped */
			tokens = strToParse.split("\\|");
			
			if(tokens.length > 1){
				
				/*Working backwards,first extract the default value*/
				if(tokens.length >= 3){
					setDefaultGridValue(this.extractFloatValueFromString(tokens[2]));
				}
				/*If the token preceding the default value is not null or empty*/
				if((tokens[1] != null) && !(tokens[1].isEmpty())){
					floatTokens = parseGridValuesAndLimits(tokens[1]);
					if(!floatTokens[0].equals(Float.NaN) &&(!floatTokens[1].equals(Float.NaN))){
						setUpperLimit(floatTokens[0]);
						setMaxValue(floatTokens[1]);
					}else{
						setUpperLimit(Float.NaN);
						setMaxValue(Float.NaN);
					}
				}
				
			}
			
			/*If the first token is not null or empty*/
			if((tokens.length > 0) && (tokens[0] != null) && !(tokens[0].isEmpty())){
				floatTokens = parseGridValuesAndLimits(tokens[0]);
				if(!(floatTokens[0].equals(Float.NaN)) && !(floatTokens[1].equals(Float.NaN))){
					setLowerLimit(floatTokens[0]);
					setMinValue(floatTokens[1]);
				}else{
					
					setLowerLimit(Float.NaN);
					setMinValue(Float.NaN);					
				}
			}
			
		}else{
			   /*If the input string does not contain a '|' character
			    *and is of the format:  lowerLimit;minValue
			    **/
				floatTokens = parseGridValuesAndLimits(strToParse);
				if(!(floatTokens[0].equals(Float.NaN)) && !(floatTokens[1].equals(Float.NaN))){
					setLowerLimit(floatTokens[0]);
					setMinValue(floatTokens[1]);
				}else{
					/*Else the input string is not a valid string
					 *Hence, the grid values/limits as well as the
					 *default value are all set to NaN*/
					setLowerLimit(Float.NaN);
					setMinValue(Float.NaN);
					setUpperLimit(Float.NaN);
					setMaxValue(Float.NaN);
					setDefaultGridValue(Float.NaN);		
				}
		}
		/*If the lower limit is greater than the upper limit then the
		 *grid values/limits as well as the
		 *default value are all set to NaN*/
		if(!(this.getLowerLimit().isNaN())
				&& !(this.getUpperLimit().isNaN())
				&& (this.getLowerLimit() > this.getUpperLimit())){
			setLowerLimit(Float.NaN);
			setMinValue(Float.NaN);
			setUpperLimit(Float.NaN);
			setMaxValue(Float.NaN);
			setDefaultGridValue(Float.NaN);
		}
	}
	/**
	 * Splits the input string into tokens using a ';' character as a delimiter
	 * and subsequently invokes the method <tt>extractFloatValueFromString</tt>
	 * on the first and the second token.
	 * @param floatStr
	 * @return
	 * @throws NullPointerException
	 */
	private Float[] parseGridValuesAndLimits (String floatStr) throws NullPointerException{
		Float[] tempFloatArray = {Float.NaN, Float.NaN};

		if(floatStr.contains(";")){
			String[] tokens = floatStr.split(";");
			if(tokens.length >= 2){
			   for(int i=0;i <=1;i++){
				   try{
				         tempFloatArray[i] = extractFloatValueFromString(tokens[i]);
				   }catch(NullPointerException e){
					   tempFloatArray[i] = Float.NaN;
				   }
			   }
			}
		}
		
		return tempFloatArray;
	}
	
	/**
	 * Parses the input <tt>String</tt> object to extract a floating point value.
	 * If the input string matches the regular expression for a floating point number, the method
	 * <tt>parseFloat</tt> is invoked on it.
	 * If the parse operation is successful, the extracted floating point number is returned, else
	 * <tt>NaN</tt> is returned.
	 * @param floatStr - the string from which the floating point value is to be extracted
	 * @return the floating point value (if the parse is successful), or NaN (if the parse fails)
	 * @throws NumberFormatException if the input string does not
	 * represent a valid floating point number.
	 */
	private Float extractFloatValueFromString(String floatStr) throws NumberFormatException{
		Float floatValue=Float.NaN;
		if(Pattern.matches(REAL_NUMBER, floatStr)){
			try{
				floatValue = Float.parseFloat(floatStr);
			}catch(NumberFormatException e){
				floatValue=Float.NaN;
			}
		}
		return floatValue;
	}
	
}



















