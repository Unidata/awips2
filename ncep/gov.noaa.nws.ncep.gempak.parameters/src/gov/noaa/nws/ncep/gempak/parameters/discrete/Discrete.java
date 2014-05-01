/*
 * Discrete
 * 
 * Date created (8-December-2009)
 * 
 * This code has been developed by the SIB for the AWIPS2 system
 */

package gov.noaa.nws.ncep.gempak.parameters.discrete;


import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**<pre>
* Parses an input <tt>String</tt> of the form value1~value2=value3;...;valuen1~valuen2=valuen3
* to create DiscreteData objects, each of which comprises of 3 <tt>Float</tt> values
* The first value is the start of the range, the next is the end of the range
* and the last is the discrete value for that range
* If value1 is missing for the first element, it will be set to -Float.MAX_VALUE
* If value2 is missing for the last element, it will be set to Float.MAX_VALUE
* 
* 
** SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 08-Dec-2009    205       Archana.S   Initial Creation
* 18-Dec-2009    205       Archana.S   Updated per review comments:
*                                      -----------------------------
*                                      1. Removed redundant checks for delimiters
*                                      from parse() method.  
*                                      2. Added a float constant TOLERANCE and used
*                                       it to replace hard-coded values of 0.000001
*                                      3.Added the public method getTolerance().
*                                      4. Removed commented out diagnostic statements from the method
*                                      getDiscreteValueClosestToTwoContourLines().  
*                                      5.Updated the regular expression for DISCRETE_STRING to
*                                      check for negative numbers and changed the range delimiter to '~'.
*                                      6.Updated Javadoc accordingly.
* 07-Jun-2010    205        Archana.S  Updated the method call to 
*                                      the private method removeBlanksWithinString()
*                                      to use the static method from the StringUtilities
*                                      class and deleted the private method
*                                      removeBlanksWithinString() from this class.  
*                                      Renamed the package as
*                                      gov.noaa.nws.ncep.gempak.parameters.discrete                                                                           
* </pre>
* @author Archana.S
* @version 1
*/
public class Discrete {
	
	private final float TOLERANCE = 0.000001f;
	
	/**List of parameterized objects of type <tt> DiscreteData</tt>
	 *wherein each DiscreteData object comprises of 3 <tt>Float</tt> values
	 *The first is the start of the range, the next is the end of the range
	 *and the last is the discrete value for that range*/
	
	private List<DiscreteData<Float>> discreteDataList;
	
	/**A boolean flag to indicate the success or failure of the parse operation*/
	private boolean isDiscreteStringParsed;
    
    /**Discrete string of the form value1~value2=value3;...;valuen1~valuen2=valuen3*/
    private String strDiscrete;
    
    /**Regular expression to check if the input string matches a valid Discrete string
     *of the form value1~value2=value3;...;valuen1~valuen2=valuen3*/
    private String DISCRETE_STRING = "((-?\\d*\\.?\\d*)?~?(-?\\d*\\.?\\d*)?=(-?\\d*\\.?\\d*)?;*)+";

    /**
     * Overloaded constructor that accepts the string containing the discrete values,
     * parses it and then populates a list of discrete objects.<p>
     * It initializes the instance variables.  
     * After checking that the input string is not empty and null, it invokes the
     * method <tt>removeBlanksWithinString</tt>, if the input string contains any blanks.
     * It then invokes the method <tt>parse</tt> to extract and store the three values
     * as a part of a single object of type DiscreteData.<p>
     * The number of DiscreteData objects depends on the number of 
     * value1~value2=value3 pairs available in the input string.
     * All DiscreteData objects are stored in a list
     * @param discreteString - the input string containing the discrete data in the form:<p>
     * <tt>value1~value2=value3;...;valuen1~valuen2=valuen3</tt>
     * 
     */
	public Discrete(String discreteString){
    	
    	/*Initialize state variables*/
    	setStrDiscrete(discreteString);
    	setDiscreteDataList(new ArrayList<DiscreteData<Float>>());
	
    	/*If the input string is not null or empty*/
    	if(!(discreteString == null)&& (!discreteString.isEmpty())){
    		
    		/*Check if the input string contains a blank character*/
    		if(discreteString.contains(" ")){
    			
    			/*If so invoke the method removeBlanksWithinString*/
    			String discreteStringWithoutBlanks = StringUtil.removeBlanksWithinString(discreteString);
    			
    			/*Then invoke the method parse
    			 *The boolean returned by parse, is passed as an argument to 
    			 *the method setDiscreteStringParsed. 
    			 **/
    			setDiscreteStringParsed(parse(discreteStringWithoutBlanks));
    		}
    		else{
    			/*Else invoke the method parse*/
    			setDiscreteStringParsed(parse(discreteString));
    			
    		}
    	}
    	else{/*Else if the string is null or empty set the boolean */
    		    setDiscreteStringParsed(false);
    	}
	}

	/**
	 * Returns the list of DiscreteData objects created by parsing the input string.
	 * @return the discreteDataList
	 */
	public List<DiscreteData<Float>> getDiscreteDataList() {
		return discreteDataList;
	}

	/**
	 * Returns the boolean flag set by the <tt>parse</tt> method
	 * @return the boolean isDiscreteStringParsed
	 */
	public boolean isDiscreteStringParsed() {
		return isDiscreteStringParsed;
	}

	/**
	 * Returns the user-entered string containing the discrete data  
	 * @return the string of the form <tt>value1~value2=value3;...;valuen1~valuen2=valuen3</tt>
	 */
	public String getStrDiscrete() {
		return strDiscrete;
	}
	/**
	 * @return the tOLERANCE
	 */
	public float getTOLERANCE() {
		return TOLERANCE;
	}

	/**
	 * Returns the discrete value preceding the input discrete value only if the input discrete value matches 
	 * a discrete value in the list of DiscreteData objects; else it returns NaN.<p>
	 * Special case - if the input discrete value matches the first discrete value in the list, it
	 * is returned, since it does not have a preceding discrete value.
	 * 
	 * @param currentDiscreteValue - the input floating point value for which a preceding discrete value is sought.
	 * @return the floating point value  preceding the input parameter (if a match is found) and NaN otherwise. 
	 */
	public Float getPrecedingDiscreteValue(Float currentDiscreteValue){
		Float leftDiscreteValue = Float.NaN;

		int currentIndex;
		int previousIndex = 0;
		if (this.getDiscreteDataList() != null && this.getDiscreteDataList().size() > 0) {
			for (currentIndex = 0; currentIndex < this.getDiscreteDataList()
					.size(); currentIndex++) {
				if(currentIndex > 0){
					previousIndex = currentIndex - 1;
				}
				
				if(this.getDiscreteDataList().get(currentIndex)
						.getDiscreteValue().compareTo(currentDiscreteValue)== 0){
					leftDiscreteValue = this.getDiscreteDataList().get(previousIndex)
					.getDiscreteValue();
					break;
				}
			}
		}
		return leftDiscreteValue; 
	}

	/**
	 * Returns the discrete value whose startValue corresponds to contourLine1 
	 * and whose endValue corresponds to contourLine2.<p>
	 * Note:
	 * <p>
	 * The absolute value of the difference between contourLine1 and the startValue must be
	 * less than TOLERANCE. <p>
	 * The absolute value of the difference between contourLine2 and the endValue must be
	 * less than TOLERANCE.
	 * @param contourLine1 - a floating point number that may differ from 
	 *                       startValue by less than TOLERANCE 
	 * @param contourLine2 -a floating point number that may differ from 
	 *                       endValue by less than TOLERANCE 
	 * @return the floating point number, whose startValue matches contourLine1 and whose
	 *         endValue matches contourLine2. 
	 */
	public Float getDiscreteValueClosestToTwoContourLines(Float contourLine1, Float contourLine2){
		Float matchingcontourValue = Float.NaN;
		Float currentStartValue;
		Float currentEndValue;
		Float currentDiscreteValue;
		int currentIndex;

		if (this.getDiscreteDataList() != null && this.getDiscreteDataList().size() > 0) {
			for (currentIndex = 0; currentIndex < this.getDiscreteDataList()
					.size(); currentIndex++) {
				currentStartValue    = this.getDiscreteDataList().get(currentIndex).getStartValue();
				currentEndValue      = this.getDiscreteDataList().get(currentIndex).getEndValue();
				currentDiscreteValue = this.getDiscreteDataList().get(currentIndex).getDiscreteValue();
				
				if((Math.abs(contourLine1.floatValue() - (currentStartValue)) < TOLERANCE)
				&& (Math.abs(contourLine2.floatValue() - (currentEndValue)) < TOLERANCE)){
					
					matchingcontourValue = currentDiscreteValue;

					break;
				}
				else if((Math.abs((-currentStartValue) - Float.MAX_VALUE) < TOLERANCE)
						&& contourLine1.floatValue() >= currentStartValue
						&& (Math.abs((contourLine2.floatValue()-currentEndValue)) < TOLERANCE)){
					      		    	  
					    	matchingcontourValue = currentDiscreteValue;

					    	break;
				}
				else if((Math.abs((currentEndValue) - Float.MAX_VALUE) < TOLERANCE)
						&& (Math.abs(contourLine1.floatValue() - (currentStartValue)) < TOLERANCE)
						&& (contourLine2.floatValue() <= currentEndValue)){
					
					matchingcontourValue = currentDiscreteValue;

					break;
				}
				
			}
			
		}

		return matchingcontourValue;
	}

	/**
	 * @param discreteDataList the discreteDataList to set
	 */
	private void setDiscreteDataList(List<DiscreteData<Float>> discreteDataList) {
		this.discreteDataList = discreteDataList;
	}

	/**
	 * @param isDiscreteStringParsed the isDiscreteStringParsed to set
	 */
	private void setDiscreteStringParsed(boolean isDiscreteStringParsed) {
		this.isDiscreteStringParsed = isDiscreteStringParsed;
	}

	/**
	 * @param strDiscrete the strDiscrete to set
	 */
	private void setStrDiscrete(String strDiscrete) {
		this.strDiscrete = strDiscrete;
	}
	
	/***
	 * The private <tt>parse</tt> method extracts the data from the input string
	 * to create a list of DiscreteData objects.  
	 * It checks if the input string matches the pattern value1~value2=value3;...;valuen1~valuen2=valuen3,
	 * and if it does, it parses the input string using ';' character as a delimiter.
	 * For each value1~value2=value3 pair, if the '=' character is found, then the 
	 * string is split into tokens using the '=' character as a delimiter.
	 * From the first of each token thus created, an attempt is made to parse it further into sub-tokens,
	 * using the '-' character as a delimiter. 
	 * The method extractFloatValueFromString is used to parse the 
	 * floating point number from each sub-token obtained as a result of the parse operation
	 * If a single value1~value2=value3 token is entered, the same task of extracting 
	 * each floating point number is repeated, so that the single DiscreteData 
	 * object is successfully created. 
	 * @param strToParse -   the user entered string of the form 
	 * <tt>value1~value2=value3;...;valuen1~valuen2=valuen3</tt>
	 * @return true or false to indicate the success/failure respectively, of the parse operation
	 * @throws ArrayIndexOutOfBoundsException
	 */
	private boolean parse(String strToParse) throws ArrayIndexOutOfBoundsException{
		boolean isStringParsed=false;
		String[] tokens;
		String discreteRangeAndValueToken[];
		String discreteStartAndEndRangeToken[];
		Float value1 = Float.NaN;
		Float value2 = Float.NaN;
		Float value3 = Float.NaN;
		if(Pattern.matches(DISCRETE_STRING,  strToParse)){
			tokens = strToParse.split(";");
			
			for(String tokenString:tokens){
				discreteRangeAndValueToken = tokenString.split("=");
				if(discreteRangeAndValueToken != null){
							discreteStartAndEndRangeToken=discreteRangeAndValueToken[0].split("~");
							try{
								value1=extractFloatValueFromString(discreteStartAndEndRangeToken[0]);
							}catch (ArrayIndexOutOfBoundsException e) {
                                value1=Float.NaN;
							}
				            try{
							    value2=extractFloatValueFromString(discreteStartAndEndRangeToken[1]);
				            }catch (ArrayIndexOutOfBoundsException e) {
                                 value2 = Float.NaN; 
							}
						try{
							  value3 =extractFloatValueFromString(discreteRangeAndValueToken[1]);
						}catch (ArrayIndexOutOfBoundsException e) {
                              value3=Float.NaN;
						}
						isStringParsed = true;
			 }

				this.getDiscreteDataList().add(new DiscreteData<Float>(value1,value2,value3));
				value1 = Float.NaN;
				value2 = Float.NaN;
				value3 = Float.NaN;
	    		  	
		    }			
		}
		
		if((this.getDiscreteDataList() != null) && (this.getDiscreteDataList().size() > 0)){
			this.updateStartValueForFirstElementInList(this.getDiscreteDataList());
			this.updateEndValueForLastElementInList(this.getDiscreteDataList());
	    }	
		return isStringParsed;
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

			try{
				floatValue = Float.parseFloat(floatStr);
			}catch(NumberFormatException e){
				floatValue=Float.NaN;
			}

		return floatValue;
	}
	/**
	 * Checks if the first element of the DiscreteData list has a startValue set to NaN
	 * and if so, sets it to -Float.MAX_VALUE.
	 *    
	 * @param discreteList - the list of DiscreteData objects
	 */
	private void updateStartValueForFirstElementInList(List<DiscreteData<Float>> discreteList){
		if(discreteList.get(0).getStartValue().compareTo(Float.NaN)== 0){
		 discreteList.get(0).setStartValue(-Float.MAX_VALUE);
		}
	}

	/**
	 * Checks if the first element of the DiscreteData list has an endValue set to NaN
	 * and if yes, sets it to Float.MAX_VALUE.
	 *    
	 * @param discreteList - the list of DiscreteData objects
	 */
	private void updateEndValueForLastElementInList(List<DiscreteData<Float>> discreteList){
		int listSize = discreteList.size();
		if(discreteList.get(listSize-1).getEndValue().compareTo(Float.NaN)== 0){
		 discreteList.get(listSize-1).setEndValue(Float.MAX_VALUE);
		}
	}	
	
}






















