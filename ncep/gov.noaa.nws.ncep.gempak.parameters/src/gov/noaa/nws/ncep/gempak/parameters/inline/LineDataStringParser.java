package gov.noaa.nws.ncep.gempak.parameters.inline;

import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
/**
 * *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 23-Oct-2009    186       Archana.S   Initial Creation
 * 07-Jun-2010    186       Archana.S    Updated the method call to 
 *                                        the private method removeBlanksWithinString()
 *                                        to use the static method from the StringUtilities
 *                                        class and deleted the private method
 *                                        removeBlanksWithinString() from this class.
 *                                       Renamed the package as
 *                                       gov.noaa.nws.ncep.gempak.parameters.inline                                            
 * </pre>
 * @author Archana.S
 * @version 1 
 */

/**
 *<code>LineDataStringParser</code> accepts a string of the following format as an input:<p>
 * colr1;..;colrn/type1;..;typen/width1;..;widthn/labl1;..;labln/smoothLevel/pointsFilter/smallContourFlag<p>
 * It parses it to generate lists of line colors,line types (line patterns), line widths and line label indicators.
 * The smooth level, the points filter and the small contour flag are common for all the lines.<p>
 * The number of different attributes specified in each category 
 * (for instance in line color or line type) decides the number of lines to be generated.
 * <p>
 *There are default values for all these attributes, 
 *hence even if an attribute is missing for a line, the default value is used<p> 
 * For Line Color and Line Width the default value is 1<p>
 * For Line Type it is solid<p>
 * For Label Present it is 0<p>
 * For Smooth Level it is 0<p>
 * For Points Filter it is 0.0<p>
 * For Small Contour Flag it is false<p>
 * 
 **/
public class LineDataStringParser {

	/**
	 * The object that actually validates each parsed input and generates the lists
	 * of attributes for the lines. 
	 **/
	private LineBuilder lineBuilder;

	/**
	 *The input string containing all the attributes for the lines.
	 **/
	private String lineDataString;
	
	/**
	 *A String indicating the error message
	 **/
	private String errorMessage;
	
	/**
	 * A boolean flag that indicates whether the string was successfully parsed. 
	 **/
	private boolean     isLineDataParsed;
	
	/**
	 *A string that indicates the nature of the error
	 **/
   	private   final String INVALID_STRING = "Invalid Line Attribute";
   	
   	/**
   	 *Regular expression for a real number
   	 **/
	protected final String REAL_NUMBER = "\\d*\\.?\\d*";
	
	/**
	 *Regular expression for an integer
	 **/
	protected final String INTEGER = "-?\\d*";
	
	/**
	 * The default constructor <code>LineDataStringParser</code> generates a single
	 * line with the following default attributes:<p>
	 * Line Color is set to 1<p>
	 * Line Pattern is set to Solid<p>
	 * Line Width is set to 1 <p> 
	 * Line Label indicator is set to 0.<p>  
	 * Smooth factor is set to 0<p>
	 * Points filter is set to 0.0<p>
	 * Small contour flag is set to false<p>
	 **/
	public LineDataStringParser(){
		/*A single line with all default attributes set is created*/
		lineBuilder = new LineBuilder();
		this.lineBuilder.setDefaultAttributesForLinesWithMissingAttributes
		                 (this.lineBuilder.getLineColorsList(), 
		                  this.lineBuilder.getLinePatternsList(),
		                  this.lineBuilder.getLineWidthList(),
		                  this.lineBuilder.getLineLabelPresentList());
	}
	
	/**
	 * @param String strLineData<p>
	 *The overloaded constructor <code>LineDataStringParser</code> accepts the input 
	 *string <code>strLineData</code> containing the attributes for the different lines or for a single line.<p>
	 *If the string is not empty/null, it invokes the method <code> parseLineDataString</code>
	 *on the string which returns<code>true</code> if the input string was successfully parsed.<p>
	 *Irrespective of whether the input string is parsed successfully, default attributes for
	 *lines with missing or invalid attributes are set by invoking the <code>LineBuilder</code> method 
	 *<code>setDefaultAttributesForLinesWithMissingAttributes</code>
	 **/
	public LineDataStringParser(String strLineData){
		   /*The input string with the line attribute information is stored*/
		   setLineDataString(strLineData);
		   /*A new LineBuilder object is created*/
		   lineBuilder = new LineBuilder();
		   
		   /*If the input string is not empty or null
		    *the method parseLineDataString is invoked with the input string. 
		    **/
		   if(!(strLineData == null) && !(strLineData.isEmpty())){
			  if(strLineData.contains(" ")){
				String strWithoutBlanks = StringUtil.removeBlanksWithinString(strLineData);
				isLineDataParsed = parseLineDataString(strWithoutBlanks);
			  }
			  else{
			        isLineDataParsed = parseLineDataString(strLineData);
			  }
		   }
		   
		   /*The default attributes are set for the lines with missing/invalid 
		    *attributes.
		    **/
		   this.lineBuilder.setDefaultAttributesForLinesWithMissingAttributes(     
		         this.lineBuilder.getLineColorsList(), 
                 this.lineBuilder.getLinePatternsList(),
                 this.lineBuilder.getLineWidthList(),
                 this.lineBuilder.getLineLabelPresentList());

	}

	/**
	 * @param String  lineDataString
	 */
	private void setLineDataString(String lineDataString) {
		this.lineDataString = lineDataString;
	}

	/**
	 * @return String lineDataString
	 */
	public String getLineDataString() {
		return lineDataString;
	}

	/**
	 * @return boolean isLineDataParsed
	 */
	public boolean isLineDataParsed() {
		return isLineDataParsed;
	}

	/***
	 * The method parseLineDataString parses the input string <code>lineDataString</code>
	 * to extract the different line attributes. <p>
	 * It first checks for the non-integer attributes: the boolean small contour flag
	 * and the real value the points filter.<p>
	 * After checking for the presence of these two attributes, it invokes the method 
	 * <code>parseLineAttributesWithIntegerValues </code>
	 * to parse and extract the remaining integer attributes.
	 * @param String lineDataString
	 * @return boolean isLineDataStringParsedCorrectly
	 */
	private boolean parseLineDataString(String lineDataString){
		boolean isLineDataStringParsedCorrectly = false;
		String arrLineDataTokens[];
		int arrLineDataTokensLength; 
		int indexOfLastToken;
		int currentIndex;
		/*If the input string contains the character "/" */
		if(lineDataString.trim().contains("/")){
			/*then it is split into an array of string tokens using this character as a delimiter*/
			arrLineDataTokens = lineDataString.split("/");
			arrLineDataTokensLength = arrLineDataTokens.length;
            if(arrLineDataTokensLength > 1){
            	try {
            		/*Working backwards for the last 2 tokens that are non-integer in nature
            		 **/
            		indexOfLastToken = arrLineDataTokensLength-1;

            		/*It is checked if the last token is the small contour flag.*/
					if(isTokenSmallContourFlag(arrLineDataTokens[indexOfLastToken])){
						this.lineBuilder.setSmallContourFlagSuppressed(true);
						isLineDataStringParsedCorrectly = true;
						indexOfLastToken--;

						/*If the last token is the small contour flag and if there is more than 1 token,
						 *it is checked if its preceding token is the filter.*/
						if(checkStringTokenForPointsFilterAndSetPointsFilter(arrLineDataTokens[indexOfLastToken])){
							
								indexOfLastToken--;
								isLineDataStringParsedCorrectly = true;

								/*After the points filter has been validated and extracted, 
								 *the remaining integer attributes are parsed and extracted*/
								if(indexOfLastToken >= 0){
								    isLineDataStringParsedCorrectly = this.parseLineAttributesWithIntegerValues(arrLineDataTokens,indexOfLastToken);
								}
						}
						else{
							/*If the second-to-last token is not the filter data, check for the presence of the rest 
						     *of the line attributes such as the color, type, width and smooth-level
						     **/
							isLineDataStringParsedCorrectly = this.parseLineAttributesWithIntegerValues(arrLineDataTokens,indexOfLastToken);
						}
					}
					
					else{/*If the last token is not the small contour flag, check if it is the points filter.*/

						if(checkStringTokenForPointsFilterAndSetPointsFilter(arrLineDataTokens[indexOfLastToken])){
								       
								indexOfLastToken--;

								/*If there are more tokens to be processed*/
								if(indexOfLastToken >= 0){
									/*Check for the presence of the remaining line attributes such as 
									 *the color, type, width,label and smooth-level*/
									isLineDataStringParsedCorrectly = this.parseLineAttributesWithIntegerValues(arrLineDataTokens,indexOfLastToken);
								}
						}
						else{/*If the last token is not the filter data, check for the presence of the rest 
						      *of the line attributes such as the color, type, width,label and smooth-level
						      **/
							
							isLineDataStringParsedCorrectly = this
							  .parseLineAttributesWithIntegerValues(
									  arrLineDataTokens,indexOfLastToken);
						}
					}	

            	 } catch (Exception e) {
					this.setErrorMessage(INVALID_STRING);
				 }
            }else{
            	/*Input has only one token*/
            	currentIndex=0;
            	/*It is checked if the single token is the small contour flag.*/
            	if(isTokenSmallContourFlag(arrLineDataTokens[currentIndex])){
            		/*If yes, then it is parsed and extracted*/
            		this.lineBuilder.setSmallContourFlagSuppressed(true);
            		isLineDataStringParsedCorrectly = true;
            	}
            	/*Else it is checked if the single token is the points filter,
            	 *using the method checkStringTokenForPointsFilterAndSetPointsFilter
            	 *If true, the points filter data is extracted and stored.
            	 **/
            	else if(checkStringTokenForPointsFilterAndSetPointsFilter(arrLineDataTokens[currentIndex])){
            		isLineDataStringParsedCorrectly = true;
            	}
            	else{
            		/*Else it is checked if the single token is an Integer (the color attribute)*/
            		isLineDataStringParsedCorrectly = this
					  .parseLineAttributesWithIntegerValues(
							  arrLineDataTokens,currentIndex);
            	}
            }

		}
		else{
		    	if (lineDataString.contains(";")) {
					String[] tempArrLineDataTokens = {lineDataString};
					/*Only a single element - a single String with 
					 * multiple line colors separated by a ";" character */
					indexOfLastToken = 0;
					isLineDataStringParsedCorrectly = this
							.parseLineAttributesWithIntegerValues(
									tempArrLineDataTokens, indexOfLastToken);
				}else {
					      /*It is checked if the single token is the small contour flag.*/
					      if(isTokenSmallContourFlag(lineDataString)){
					    	  /*If yes, then it is parsed and extracted*/
					            this.lineBuilder.setSmallContourFlagSuppressed(true);
					            isLineDataStringParsedCorrectly = true;
		    	          }else{
		    	            	/*Else it is checked if the single token is the points filter,
		    	            	 *using the method checkStringTokenForPointsFilterAndSetPointsFilter
		    	            	 *If true, the points filter data is extracted and stored.
		    	            	 **/
		    	        	  
		    	        	     if(checkStringTokenForPointsFilterAndSetPointsFilter(lineDataString)){
										isLineDataStringParsedCorrectly = true;
		    	        	     }
		    	        	     else{
		    	        	    	 /*Else it is checked if the single token is an Integer (the color attribute)*/
		    	        		        currentIndex = 0;
		    	        		        String[] tempArrLineDataTokens = {lineDataString};
		    	        		        isLineDataStringParsedCorrectly = this.parseLineAttributesWithIntegerValues(tempArrLineDataTokens, currentIndex);
		    	        	     }
		    	          }
		    	 }
		}
		
		return isLineDataStringParsedCorrectly;
	}

	/**
	 * @param String errorMessage
	 */
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/**
	 * @return String errorMessage
	 */
	public String getErrorMessage() {
		return errorMessage;
	}
	
	/**
	 *The method <code>isTokenSmallContourFlag</code> returns true only
	 *if the input string is is set to "TRUE" or "T" or "true" or "t".
	 *else it returns false.
	 *
	 *@param String tokenString
	 *@return boolean
	 *
	 **/
	private boolean isTokenSmallContourFlag(String tokenString){

		return((tokenString.compareToIgnoreCase("TRUE") == 0)
			 || (tokenString.compareToIgnoreCase("T")   == 0) ? true:false);
	}
	
	/**
	 *@param String filterString
	 *@return boolean
	 *@throws NumberFormatException<p>
	 *The method <code>checkStringTokenForPointsFilterAndSetPointsFilter</code> attempts
	 *to extract a real number from the input string. If it fails, it throws a NumberFormatException
	 *and returns false.<p>
	 *Otherwise, it checks if the filter data lies in the range from 0.0 to 1.0.<p>
	 *If true, it invokes the <code>LineBuilder</code> method <code>setPointsFilter</code> to store the points filter data
	 *and returns true, else it returns false.<p>
	 **/
	private boolean checkStringTokenForPointsFilterAndSetPointsFilter(String filterString){

		try{
			Double filterData = Double.parseDouble(filterString);
			/*If the extracted number lies between 0.0 and 1.0, then the method 
			 *setPointsFilter is invoked to set the points filter.
			 **/

			if(filterData >= 0.0 && filterData <= 1.0){
				this.lineBuilder.setPointsFilter(filterData);
				return true;
			}else{
				return false;
			}
		}catch(NumberFormatException e){
			return false;
		}

	}
	
	/**
	 *@param String[] strLineAttribute
	 *@param int lastIndex
	 *@return boolean isLineAttributeStringParsed
	 *@throws NumberFormatException
	 *<p>
	 *The method <code> parseLineAttributesWithIntegerValues</code> accepts 
	 *a String array <code>strLineAttribute</code> and an integer parameter <code>lastIndex</code>
	 *that defines the number of different Integer attributes present.<p>
	 *Each token is checked for the presence of multiple Integers separated by ";" or a single Integer
	 *and if it does contain an Integer, the Integer is extracted, else a NumberFormatException is thrown.<p>
	 *If the Integer is successfully extracted, it is added to a list of Integers called <code>lineAttributelist</code>
	 *<p>
	 *Once all the Integers are successfully extracted from each token, the method 
	 *<code>validateAndSetIntegerLineAttributes</code> is invoked with the list of Integers and a number that
	 *denotes the type of line attribute extracted.
	 **/
	private boolean parseLineAttributesWithIntegerValues(String[] strLineAttribute, int lastIndex){
		boolean isLineAttributeStringParsed = false;
		int currentIndex;
		int i;
		
		Integer singleLineAttribute;
		String strLineAttributeToken[];
		/*Even if there are more than 5 attributes, parse only the first 5, i.e attributes
		 *indexed from 0 to 4*/
		if(lastIndex > 4){
		   lastIndex = 4;
		}

		for(currentIndex = 0;currentIndex <= lastIndex;currentIndex++){

			List<Integer> lineAttributesList = new ArrayList<Integer>();
			
			/*If there are multiple values entered for each Integer attribute*/
			if(strLineAttribute[currentIndex].contains(";")){
	
				/*Split the token using ";" as a delimiter*/
				strLineAttributeToken = strLineAttribute[currentIndex].split(";");
				
                for(i=0;i<strLineAttributeToken.length;i++){
                	  	
                	/*It is checked if the token is an Integer*/
               	if(Pattern.matches(INTEGER, strLineAttributeToken[i])){
                		   try {
                			      /*If the token is an Integer it is extracted and added to the list*/
                			       singleLineAttribute = Integer.parseInt(
                			    		                 strLineAttributeToken[i]);
                			       lineAttributesList.add(i,singleLineAttribute);
                			       isLineAttributeStringParsed = true;

						   } catch (NumberFormatException e) {
					
							   this.setErrorMessage(INVALID_STRING);
						   }
                	}
                	else{
                		   this.setErrorMessage(INVALID_STRING);
                	}
                }
			}
			else{/*There is only a single value for the Integer attribute*/
     		   try {
     			   /*The number is extracted and stored in the list*/
     			   		singleLineAttribute = Integer.parseInt(
     			   				   strLineAttribute[currentIndex]);
     			   		
     			   		lineAttributesList.add(0,singleLineAttribute);
     			   		isLineAttributeStringParsed = true;
			       
     		   } catch (NumberFormatException e) {
     			     this.setErrorMessage(INVALID_STRING);
     		   }				
			}
			
			/* For each string token, if the Integer value is successfully extracted, 
			 * the method validateAndSetIntegerLineAttributes() is invoked with 
			 * the list of Integers and the currentIndex
			 * The value of the currentIndex indicates the type of the line attribute extracted from the string*/

			if(isLineAttributeStringParsed){
				this.lineBuilder.validateAndSetIntegerLineAttributes(lineAttributesList, currentIndex);
			
			}
		}
		
		return isLineAttributeStringParsed;
	}
	
	/**
	 * @return LineBuilder lineBuilder
	 **/
	public LineBuilder getInstanceOfLineBuilder(){
		return this.lineBuilder;
	}
	
}








