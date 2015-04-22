/*
 * DLINES
 * 
 * Date created (28 December 2009)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.gempak.parameters.dlines;
import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;

/**<pre>
* Parses an input <tt>String</tt> of the form <tt> yes or no;yes or no [ | epsilon ]</tt>
* where epsilon is a <tt>Double</tt> object. Yes or no values are stored as <tt>Boolean</tt> objects, 
* that take the values TRUE or FALSE respectively.The first 'yes' value indicates that 
* the contour values to the right of the current contour line are greater and the second 
* 'yes' value indicates that the values to the left of the current contour line are greater 
* than itself.
* When a DISCRETE string is specified, DLINES defaults to yes;no 
* 'yes' for right-of-line, and NO for left-of-line.
* When only like-valued contours exist (or a single contour), then
* the value of epsilon is added to grid points to the right of the contour
* line.     

* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 28-Dec-2009    211        Archana.S    Initial Creation
* 07-Jun-2010    211        Archana.S    Updated the method call to 
*                                        the private method removeBlanksWithinString()
*                                        to use the static method from the StringUtilities
*                                        class and deleted the private method
*                                        removeBlanksWithinString() from this class.
*                                        Renamed the package as
*                                       gov.noaa.nws.ncep.gempak.parameters.dlines                                          
* </pre>
* @author Archana.S
* @version 1
*/

public class DLines {

 /**A parameterized object that holds the parsed tokens from the DLINES string*/	
 private DLineData<Boolean, Double> dlineObj;	
 
 /**The input string of the constructor, as entered by the user*/
 private String dString;
 
 /**Overloaded constructor that invokes the <tt>parse(String)</tt> method on the input string.<p>
  * It first ensures that the input string is neither null nor empty. Subsequently, it removes
  * all blank spaces (if any exist) from the string.
  * Finally, it sends the user entered string to the <tt>parse(String)</tt> method */
 public DLines(String dlinesString){
 	
	 /*If the input string is not null or empty*/
 	if(!(dlinesString == null)&& (!dlinesString.isEmpty())){
 		
 		/*store the input string*/
 		setDLINEString(dlinesString);
 		
 		/*Initialize the DLINEData object*/
 		setDlineData(new DLineData<Boolean, Double>(Boolean.FALSE,
 				                                    Boolean.FALSE, 
 				                                    Double.NaN));
 		/*Check if the input string contains a blank character*/
 		if(dlinesString.contains(" ")){
 			
 			/*If so invoke the method removeBlanksWithinString(String), before invoking parse(String)*/
 			  String strDLINEWithoutBlanks = StringUtil.removeBlanksWithinString(dlinesString);
              parse(strDLINEWithoutBlanks);
 		}
 		else{
 			/*Else invoke the method parse*/
              parse(dlinesString);
 		}
 	}

 }
 
 /**
  * Returns the user-entered input string
  * @return the dString
  */
 public String getDString() {
	 return dString;
 }

 /**
  * Returns the parameterized object <tt>DLINESData< Boolean, Double > </tt>
  * @return the dlineObj
  */
 public DLineData<Boolean, Double> getDlineData() {
	 return dlineObj;
 }
 
/***
 * Parses the input string and populates the parameterized DLINESData object with the extracted 
 * Boolean and Double values.<p>
 * A local Double object dEpsilon is initialized to Double.MIN_VALUE. 
 * The input string is split using the delimiter '|'. If 2 tokens exist, the second
 * token is parsed to extract a Double object. If the Double object does not exist,
 * a NumberFormatException is thrown and dEpsilon is set to NaN, else it is set to 
 * the extracted Double value. <p>
 * If the first token is non-empty, it is parsed using the delimiter ';'
 * Depending on whether the first sub-token is 'yes' or 'no', the first element of a local
 * boolean array 'tokenState' with 2 elements is set to true or false respectively.
 * Likewise, the same process is repeated for the second sub-token, should one exist.<p>
 * The parameterized DLINESData object is populated with the values of tokenState[] and dEpsilon.  
 * @param strToParse - the user entered string containing the DLINE data.
 */
 private void parse(String strToParse){
	 String[] tokens;
	 String[] dlineTokens;
	 Double dEpsilon = Double.MIN_VALUE;
	 boolean tokenState[] = {false, false};

	 tokens = strToParse.split("\\|");


	 if(tokens != null){
		 if(tokens.length == 2 && (!tokens[1].isEmpty())){
			 try{
				 dEpsilon = Double.parseDouble(tokens[1]);
			 }catch(NumberFormatException e){
				 dEpsilon = Double.NaN;
			 }

		 }

		 if(tokens.length >= 1 && !(tokens[0].isEmpty())){
			 dlineTokens = tokens[0].split(";");
			 if(dlineTokens != null && dlineTokens.length > 0 && dlineTokens.length <= 2){
				 for(int i = 0; i < dlineTokens.length;i++){
					 if(dlineTokens[i] != null && (!dlineTokens[i].isEmpty()) ){
						 if(dlineTokens[i].compareToIgnoreCase("yes") == 0){
							 tokenState[i] = true;
						 }

						 if (dlineTokens[i].compareToIgnoreCase("no") == 0){
							 tokenState[i] = false;
						 }
					 }
				 }

				 setDlineData(new DLineData<Boolean, Double>(
						 new Boolean(tokenState[0]),
						 new Boolean(tokenState[1]),
						 dEpsilon));				 
			 }

		 }
	 }


 }

 /**
  * Sets the input DLINEData< Boolean, Double > object to the private
  * data dlineObj
  * @param dlineObj the DLINEData<Boolean, Double> object to set
  */
 private void setDlineData(DLineData<Boolean, Double> dlineObj) {
	 this.dlineObj = dlineObj;
 }

 /**
  * Stores the string passed as an input to the overloaded constructor.
  * @param dString the String object to set
  */
 private void setDLINEString(String dString) {
	 this.dString = dString;
 }
 
}


























