/*
 * CATMAP
 * 
 * Date created (20 November 2009)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.gempak.parameters.core.categorymap;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;

/**<pre>
 * Parses an input <tt>String</tt> of the form label1=value1;...;labelN=valueN
 * where label is a non-empty string without any blanks
 * and value is a valid floating point number
 * <p>
 * When queried with a given label, CATMAP returns the matching value and vice-versa. 
 * <p>
* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 16-Nov-2009    194        Archana.S   Initial Creation
* 20-Nov-2009    194        Archana.S   Updated per review comments:
*                                       Redesigned class to use a list of parameterized 
*                                       objects of the form CatMapInfo<String, Float>
*                                       Created class CatMapInfo<String, Float>
*                                       Added method getMatchingLabelForValue.
* 07-Jun-2010    194        Archana.S    Updated the method call to 
*                                        the private method removeBlanksWithinString()
*                                        to use the static method from the StringUtilities
*                                        class and deleted the private method
*                                        removeBlanksWithinString() from this class.
*                                        Renamed the package as
*                                       gov.noaa.nws.ncep.gempak.parameters.categorymap                                        
* </pre>
* @author Archana.S
* @version 1
*/

public class CatMap {

	/**List of parameterized objects of type <tt> CatMapInfo </tt>
	 *wherein each CatMapInfo object comprises of a single <tt>String</tt> label
	 *and its matching <tt>Float</tt> value*/
	private List<CatMapInfo<String,Float>> catmapInfoList;
	
	/**A boolean flag to indicate the success or failure of the parse operation*/
	private boolean isCATMAPStringParsed;
    
    /**CATMAP string of the form label1=value1;...;labelN=valueN;*/
    private String strCategoricalMapping;
    
    /**Regular expression to check if the input string matches a valid CATMAP string
     *of the form label1=value1;...;labelN=valueN;*/
    private String CATMAP_STRING = "([\\w\\p{Punct}]*=-?\\d*\\.?\\d*;*)+";
 
    /**
     * Overloaded constructor that accepts a string of the form label1=value1;...;labelN=valueN
     * as input and parses it.
     * <p>
     * It initializes the instance variables.  
     * After checking that the input string is not empty and null, it invokes the
     * method <tt>removeBlanksWithinString</tt>, if the input string contains any blanks.
     * It then invokes the method <tt>parse</tt> to extract and store the labels and values
     * in separate lists. 
     *  
     * @param strCATMAP - the input CATMAP string of the form label1=value1;...;labelN=valueN
     */
    public CatMap(String strCATMAP){
    	
    	/*Initialize state variables*/
    	setCategoricalMappingString(strCATMAP);
    	setCatmapInfoList(new ArrayList<CatMapInfo<String,Float>>());
	
    	/*If the input string is not null or empty*/
    	if(!(strCATMAP == null)&& (!strCATMAP.isEmpty())){
    		
    		/*Check if the input string contains a blank character*/
    		if(strCATMAP.contains(" ")){
    			
    			/*If so invoke the method removeBlanksWithinString*/
    			String strCATMAPWithoutBlanks = StringUtil.removeBlanksWithinString(strCATMAP);
    			
    			/*Then invoke the method parse
    			 *The boolean returned by parse, is passed as an argument to 
    			 *the method setCATMAPStringParsed(). 
    			 **/
    			setCATMAPStringParsed(parse(strCATMAPWithoutBlanks));
    		}
    		else{
    			/*Else invoke the method parse*/
    			setCATMAPStringParsed(parse(strCATMAP));
    		}
    	}
    	else{/*Else if the string is null or empty set the boolean */
    		    setCATMAPStringParsed(false);
    	}
    	
    }
    
    /**
     * Returns the floating point value associated with the first instance of a 
     * label that matches the input string  
     * <p>
     * The input string containing the label is compared with the 
	 * corresponding string component of each CatMapInfo object in the list of CatMapInfo objects
	 * (populated by parsing the input String)
	 * If the case-insensitive comparison fails,a wild-card comparison is carried out, if the label 
	 * has only 2 characters of the form 'W*' or 'w*' 
	 *In either case,if the comparison succeeds, the corresponding floating point value is returned.<p>
	 *Note:<p>
     * Any label that was entered after the wild-card label and starts with the same letter as the wild-card
     * will be returned the same value as that of the wild-card label.<p>
     * 
     * For instance:<p>
     * If the input string is <tt> abc=4;def=8;a*=20.7;oranges=30;apples=40 </tt>
     * <p>
     * Then, invoking the method <tt>getMatchingValueForLabel</tt> with the label <tt> apples</tt> will return
     * a value of 20.7 (the value of associated with the wild-card label <tt>a*</tt>, whose first character matches
     * the first character in <tt>apples</tt>) and not 40 as might otherwise be expected.<p>
     * On the other-hand, the value returned for the label <tt>abc</tt> will be 4, since the it precedes
     * the label <tt> a* </tt>
     * 
     * @param inputLabelString - the string label for which a matching floating point value is sought
     * @return the matching floating point value 
     */
	public Float getMatchingValueForLabel(String inputLabelString){
		
		Float matchingValue = Float.NaN;
		
		/*If the input string was parsed successfully*/
		if (this.isCATMAPStringParsed()) {
			
			/*For each CatMapInfo object in the list: 
			 * Make a case insensitive comparison with the input string
			 * If this comparison fails, check whether the following conditions are satisfied:
			 * The label is 2 characters long
			 * The first character of the label and the input string are the same
			 * The second and final character of the label is a '*'(wild-card) character
			 * */
			
			for(int i=0; i<this.getCatmapInfoList().size();i++){
				CatMapInfo<String, Float> catmapObj = this.getCatmapInfoList().get(i);
				if(catmapObj.getLabel().equalsIgnoreCase(inputLabelString)
				   || (   (catmapObj.getLabel().length() == 2)
					   && (catmapObj.getLabel().toUpperCase().charAt(0) 
						    == inputLabelString.toUpperCase().charAt(0)) 
					   && (catmapObj.getLabel().charAt(1) == '*'))){
					         matchingValue = catmapObj.getValue();
					         break;
				}
			}
		}
		return matchingValue;
	}
/**
 * Returns the matching <tt>String</tt> label (if found) for the input floating point number or null otherwise
 * <p>
 * The input <tt>Float</tt> value is compared successively with the corresponding <tt>Float</tt> data of each CatMapInfo object 
 * until a match is found. Once a match is found, the corresponding label of the CatMapInfo object is returned
 * otherwise the method returns <tt>null</tt>
 * @param inputFloatValue -  the input <tt>Float</tt> value for which the matching label is sought
 * @return the matching<tt>String</tt> label if it is found or <tt>null</tt> otherwise
 */
	public String getMatchingLabelForValue(Float inputFloatValue){
		
		StringBuffer strBuff = new StringBuffer();

		/*If the input string was parsed successfully*/
		if (this.isCATMAPStringParsed()) {
			
			/*For each CatMapInfo object in the list, check if the input string matches 
			 *its floating point value - if yes, get the corresponding label and return it, else
			 *return null
			 **/
			
			for(int i=0; i<this.getCatmapInfoList().size();i++){
				CatMapInfo<String, Float> catmapObj = this.getCatmapInfoList().get(i);
				if(catmapObj.getValue().equals(inputFloatValue)){
					         strBuff.append(catmapObj.getLabel());
					         break;
				}
			}
		}
		
		return ( strBuff.length() > 0 ? strBuff.toString(): null);
	}
	
    /**
     * Returns the input CATMAP string entered by the user
     * @return The input string for the constructor
     */
	public String getCategoricalMappingString() {
		return strCategoricalMapping;
	}	
	
	/**
	 * Returns the list of CatMapInfo objects
	 * @return the list of CatMapInfo objects
	 */
	private List<CatMapInfo<String, Float>> getCatmapInfoList() {
		return catmapInfoList;
	}

	
	/**
	 * Sets the input list to the list of CatMapInfo objects
	 * @param catmapInfoList the list of CatMapInfo objects to set
	 */
	private void setCatmapInfoList(List<CatMapInfo<String, Float>> catmapInfoList) {
		this.catmapInfoList = catmapInfoList;
	}

	/**
	 * Indicates the status of the parse operation
	 * @return the boolean flag to indicate whether the parse operation succeeded or failed
	 */
	private boolean isCATMAPStringParsed() {
		return isCATMAPStringParsed;
	}
	
	/**
	 * Sets the status of the parse operation to the input boolean parameter
	 * @param parseStatus - boolean flag to indicate whether the parse operation
	 * succeeded or failed
	 */

	private void setCATMAPStringParsed(boolean parseStatus) {
		this.isCATMAPStringParsed = parseStatus;
		
	}
	
	/**
	 * Stores the input string of the constructor
	 * @param strCategoricalMapping - The input string of the constructor
	 */
	private void setCategoricalMappingString(String strCategoricalMapping) {
		this.strCategoricalMapping = strCategoricalMapping;
	}
	

	/**
	 * Parses the input string of the form label1=value1;....;labelN=valueN
	 * into label/value pairs and subsequently extracts
	 * and stores each label/value pair in individual CatMapInfo objects. Each 
	 * CatMapInfo object is added then successively to a list of CatMapInfo objects.
	 * @param categoricalMappingString - the input string of label/value pairs
	 * @return isCATMAPStringParsedCorrectly - boolean flag to indicate the
	 * success or failure of the parse
	 */
	private boolean parse(String categoricalMappingString){
		boolean isCATMAPStringParsedCorrectly = false;
		String[] catmapLabelValuePairs;
		String[] catmapTokens;
		
		/*If the input string matches the regular expression for the CATMAP
		 *string 
		 * */
		if(Pattern.matches(CATMAP_STRING, categoricalMappingString)){
            /*Split the input string using the ';' character as a delimiter*/
			catmapLabelValuePairs = categoricalMappingString.split(";");
			
			/*For each label-value token*/
			for(String catMap:catmapLabelValuePairs){
				
				/*split the token using the '=' character as a delimiter*/
				catmapTokens = catMap.split("=");
				
			    /*if there are exactly 2 tokens*/
				if(catmapTokens.length == 2){
					
					try{
						/*try to extract the floating point value from the string*/
						
						Float tempValue   = Float.parseFloat(catmapTokens[1]);
						String tempString = catmapTokens[0];
						
						/*Create a new CatMapInfo object with the extracted string and floating point
						 *values and add it to the list of CatMapInfo objects.
						 * */
						this.catmapInfoList.add(new CatMapInfo<String,Float>(tempString,tempValue));
						
						/*Set the boolean to true*/
						isCATMAPStringParsedCorrectly = true;
					}
					/*In case there is an exception*/ 
					catch(Exception e){
						
						/*Add a CatMapInfo object initialized with invalid parameters to the list*/
						
						this.catmapInfoList.add(new CatMapInfo<String, Float>("",Float.NaN));
						
						/*Set the boolean to false*/
						isCATMAPStringParsedCorrectly = false;
					}
				}
			}
		}
		return isCATMAPStringParsedCorrectly;
	}

}
	

