package gov.noaa.nws.ncep.viz.tools.contourinterval;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
//import java.util.LinkedHashMap;

/**
 *A single string containing the contour data is fed to the overloaded constructor of ContourDataStringParser.
 * The method getContourValuesList returns a list (of type Double) of the contour values.
 *A valid contour data string can take only one of the formats listed below:<p>
 *1. contourInterval/minimumContourLevel/maximumContourLevel/numPaddingdigits <p>
 *(Before generating the list of contour values, if minimumContourLevel and maximumContourLevel 
 *are not exact multiples of contourInterval they will be internally recalculated
 *and set to the next neighboring values that are the exact multiples of contourInterval).<p>
 *2. contourValue1;contourValue2;contourValue3;contourValue4;contourValue5.......<p>
 *3. contourValue1=label1/contourValue1/contourValue1<p>
 *4. contourValue1=label1;contourValue2=label2;contourValue3=label3;............<p>
 * Prior to invoking the method getContourValuesList, it should be checked that the input string was parsed correctly, using the 
 * method isContourStringParsed(). <P>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 16-Oct-2009    174        Archana.S   Initial Creation 
 * Mar 17, 2010   164        M. Li		 If only one number, set to interval
 * </pre>
 * @author Archana.S
 * @version 1
 */

public class ContourDataStringParser {
	
	/** Boolean flag to validate that the contour data string was parsed correctly*/
	protected boolean isContourStringParsed; 
	
	/**The contour data string that is set fed to the overloaded constructor*/
	protected String  contourString;
	
	/** A string to indicate the nature of the error, when the contour data string is not parsed correctly*/
	protected String  errorMessage;
	
	/** A real number that represents the contour interval*/
	protected Double  contourInterval;
	
	/** A real number that represents the minimum contour level*/
	protected Double  minContourLevel;
	
	/** A real number that represents the maximum contour level*/
	protected Double  maxContourLevel;
	
	/**An integer that decides the number of leading padding zeros to be prefixed to a integer contour label*/
	protected Integer numPaddingDigits;
	
	/**The generated list of contour levels*/
	protected List<Double> contourValuesList;

	//protected LinkedHashMap<Double, String> labelledcontourValuesList;

	protected final String REAL_NUMBER    = "-?\\d*\\.?\\d*";
	protected final String INTEGER_FORMAT = "\\d";
	protected final String CONTOUR_INTERVAL_STRING_DELIMITER = "/";
	protected final String CONTOUR_VALUE_STRING_DELIMITER    = ";";
	protected final String CONTOUR_LABEL_DELIMITER           = "=";
	protected final String INVALID_STRING                    = "Invalid String Format";
	protected final String EMPTY_STRING                      = "Contour data string is empty";
	
	/**
	 * The overloaded constructor ContourDataStringParser accepts a String object containing the contour data as an input.
	 * If the contour data string is not empty, the constructor invokes the method parseContourDataString on it.
	 * If the contour data string is empty, it sets the boolean isContourStringParsed to false.
	 * @param contourString
	 */
	public ContourDataStringParser(String contourString) {

		contourInterval       = Double.NaN;
		minContourLevel       = Double.NaN;
		maxContourLevel       = Double.NaN;
		numPaddingDigits      = 0;
	
		if(!contourString.trim().isEmpty()){

		      this.contourString    = contourString; 
			  contourValuesList     = new ArrayList<Double>();
		      isContourStringParsed = parseContourDataString(contourString);
		}
		else{
			isContourStringParsed = false;
			contourValuesList     = null;
			errorMessage          = EMPTY_STRING;
		}
			
	}

	/**
	 *The default constructor initializes the data variables to their default values
	 **/
	public ContourDataStringParser(){
		isContourStringParsed = false;
		contourValuesList     = null;		
		contourInterval       = Double.NaN;
		minContourLevel       = Double.NaN;
		maxContourLevel       = Double.NaN;
		numPaddingDigits      = 0;	
		errorMessage          = EMPTY_STRING;
	}
/**
 * The method isContourStringParsed indicates whether the parsing was successful. It should be invoked after calling the
 * overloaded constructor ContourDataStringParser but before calling the method getContourValuesList.
 * @return boolean isContourStringParsed
 */
	public boolean isContourStringParsed() {
		return isContourStringParsed;
	}	
	
	/**The method getContourstring returns the input contour data string
	 * @return String contourString 
	 */
	public String getContourString() {
		return contourString;
	}
	
    /**
     * The method getErrorMessage returns the string containing the error message.
     * @return String errorMessage.
     */
	public String getErrorMessage() {
		return errorMessage;
	}
	
	/** The method getContourInterval returns the contour interval value
	 *@return Double contourInterval
	 **/
	public Double getContourInterval() {
		return contourInterval;
	}

	/**The method getMinContourLevel returns the value of the minimum contour level
	 * @return Double minContourLevel
	 **/
	public Double getMinContourLevel() {
		return minContourLevel;
	}
	
	/**The method getMaxContourLevel returns the value of the maximum contour level
	 * @return Double maxContourLevel
	 **/	
	public Double getMaxContourLevel() {
		return maxContourLevel;
	}
	
	/** The method getNumPaddingDigits returns the value of the Integer numPaddingDigits
	 *@return Integer numPaddingDigits
	 **/
	public Integer getNumPaddingDigits() {
		return numPaddingDigits;
	}

	/**
	 *The method getContourValuesList returns the List (of type Double) of contour values
	 *if it is non-empty
	 *@return contourValuesList
	 **/
	public List<Double> getContourValuesList() {
             return this.contourValuesList;
	}

    /**
     *@param String contourStringParam
     *@return boolean isInputContourStringParsed<P>
     *The method parseContourDataString accepts the contour data string fed as an input from the constructor.
     *A valid contour data string can take only one of the formats listed below:<P>
     *1. contourInterval/minimumContourLevel/maximumContourLevel/numPaddingdigits <P>
     *2. contourValue1;contourValue2;contourValue3;contourValue4;contourValue5.......<p>
     *3. contourValue1=label1/contourValue1/contourValue1<p>
     *4. contourValue1=label1;contourValue2=label2;contourValue3=label3;............<p>
     *<p>
     *If the input string is entered in the first format, contourStringParam is parsed using the "/" character as a delimiter.
     *All the numeric data is extracted and stored.
     *The method generateContourValuesList is invoked to compute the range of contour values.
     *The method setContourValuesList is invoked to store the computed range of contour values.
     *<p>
     *If the second format is chosen, the string contourStringParam is parsed using the ";" character as a delimiter.
     *All the extracted numeric data is sent as an array of Double objects to the overloaded method generateContourValuesList
     *which creates a single list of Double objects. The method  setContourValuesList is invoked to store this list.
     *<P>
     *If the parsing and subsequent numeric data retrieval is successful, the boolean isInputContourStringParsed is set to true.
     *<P>
     *The method parseContourDataString invokes the method parseContourStringContainingLabel to parse contour values with labels 
     *however the method parseContourStringContainingLabel is currently only a stub.
     **/
	
	private boolean parseContourDataString(String contourStringParam) {
       String contourLevelTokens[]; 
       Double tempContourParam[] = {Double.NaN,Double.NaN,Double.NaN};
	   List<Double> tempContourValList = new ArrayList<Double>();
	   boolean isInputContourStringParsed = false;
	   
	   /*Check if the string with fits the format
	    * contourInterval/minimumContourLevel/maximumContourLevel/numPaddingdigits
	    * */
	   // CONTOUR_INTERVAL_STRING_DELIMITER = "/"  
	   if(contourStringParam.contains(CONTOUR_INTERVAL_STRING_DELIMITER)){
		    
		     /*If yes, then split the string using the "/" character as a delimiter */
			 contourLevelTokens = contourStringParam.split(CONTOUR_INTERVAL_STRING_DELIMITER);
			 if(contourLevelTokens.length > 1){
				 
				 for(int i = 0; i < 3;i++){
                	   try{
          		    	    /*If the string token contains a numeric character sequence, attempt to extract it.*/
                 		     if(Pattern.matches(REAL_NUMBER, contourLevelTokens[i])){ //REAL_NUMBER is a regular expression of the form: -?\\d*\\.?\\d* 
                		        tempContourParam[i] = Double.parseDouble(contourLevelTokens[i]);
                		     }
                  	   }
                	   catch(Exception e){
                		   tempContourParam[i] = Double.NaN;
                	   }
                  }
                  /*Store extracted numeric data as minContourLevel and maxContouLevel */
				  setMinContourLevel(tempContourParam[1]);
				  setMaxContourLevel(tempContourParam[2]);
                
				  /*If the maxContourLevel value is different from the minContourLevel value,
				   *store the contourInterval value as retrieved from the corresponding string token. 
				   **/
				  if(this.getMaxContourLevel() != this.getMinContourLevel()){
                	    setContourInterval(tempContourParam[0]);
              	     
                  }
                  else{
                	    /*If the maxContourLevel value the same as the minContourLevel value,
				         *set the contourInterval to 0
				         **/                	  
                	    setContourInterval(0.0);
                	    /*Special case: check for a label in a single contour value and set the contourInterval to 0 */
                	    if(contourLevelTokens[0].contains(CONTOUR_LABEL_DELIMITER)){
                	     parseContourStringContainingLabel(contourLevelTokens[0]);
                	    }
                  }
	
				  if(   this.getContourInterval().isNaN() 
				     && this.getMinContourLevel().isNaN() 
				     && this.getMaxContourLevel().isNaN()){
					  isInputContourStringParsed = false; /* All three parameters cannot be undefined in a successful parse operation*/
				  }
				  else{
					     isInputContourStringParsed = true;
				  }
                  
				/*If the parsing has been successful, generate the list of contour values*/  
                if(isInputContourStringParsed){
                         
                	     tempContourValList = generateContourValuesList(this.getContourInterval(),
                                                                        this.getMinContourLevel(),
                                                                        this.getMaxContourLevel());

                	     setContourValuesList(tempContourValList);
                }
                /*Extraction of the 4th parameter - numPaddingDigits decides the number digits
                 *in the contour label, if any.
                 **/
                if (contourLevelTokens.length == 4) {
					try {
						 /*Check that the 4th string token is an integer.
						 **/
						if (Pattern.matches(INTEGER_FORMAT,
								contourLevelTokens[3])) {/*INTEGER_FORMAT is a regular expression of the form: \\d    */
							setNumPaddingDigits(Integer.parseInt(contourLevelTokens[3]));
							
						/* TODO Add code to format the labels for the contour values based on numPaddingDigits */
						}
					 } 
					 catch (Exception e) {
						setErrorMessage(INVALID_STRING);
						isInputContourStringParsed = false;
					 }
				}                
  
			 }
			 else{
				     /*If the parsed contour interval string contains only 1 token, 
				      *extract the numeric data (the contour value) and populate the list with this single 
				      *contour value.
				      *
				      *  No, should be set to interval. by mli
				      **/
                        if(contourLevelTokens.length == 1){
				            //isInputContourStringParsed = extractContourValueFromContourStringToGenerateContourValueList(contourLevelTokens[0]);
                        	setContourInterval(tempContourParam[0]);
                        	setMinContourLevel(Double.NaN);
                        	setMaxContourLevel(Double.NaN);
                        	isInputContourStringParsed = true;
                        }
			 }                  
	    }
	    
	   /*Check if the input contour data string fits the following format
	    *contourValue1;contourValue2;contourValue3;contourValue4;contourValue5....... 
	    * */
	    else if(contourStringParam.contains(CONTOUR_VALUE_STRING_DELIMITER)){  /* CONTOUR_VALUE_STRING_DELIMITER = ";"*/
	    	
	    	/*If yes, then split the string using the ";" character as a delimiter */
	    	contourLevelTokens = contourStringParam.split(CONTOUR_VALUE_STRING_DELIMITER);
	    	if(contourLevelTokens.length > 1){
	    		tempContourParam = new Double[contourLevelTokens.length];
	    		int count = 0;
	    		/*While a valid string token exists, extract the numeric value of the string*/
	    		while(count <contourLevelTokens.length){
	    			isInputContourStringParsed = extractContourValueFromContourStringToGenerateContourValueList(contourLevelTokens[count]);
	    			if(!isInputContourStringParsed){
	    				break;
	    			}
	    			count++;
	    		}

	    	}
	    	else{
	    		   if(contourLevelTokens.length == 1){
	    			     /*If the parsed contour interval string contains only 1 token, 
	    			      *extract the numeric data (the contour value) and populate the list with this single 
	    			      *contour value.
	    			      **/
	    			      isInputContourStringParsed = extractContourValueFromContourStringToGenerateContourValueList(contourLevelTokens[0]);
	    			      if(!isInputContourStringParsed){
	    			    	  
	    			    	   /*The input contour data string does not fit any of the acceptable formats*/
	    		    	       setErrorMessage(INVALID_STRING);
	    			      }	    			      
	    		   }
	    	}
	    }
	    else{
	    	
		     /*If the parsed contour interval string contains only 1 token, 
		      *extract the numeric data (the contour value) and populate the list with this single 
		      *contour value.
		      *
		      * No, should be set to interval instead of contour value.  by M. Li
		      **/

		      //isInputContourStringParsed = extractContourValueFromContourStringToGenerateContourValueList(contourStringParam);
	    	if (contourStringParam.trim() != null) {
	    		setContourInterval(Double.valueOf(contourStringParam.trim()));
	    		setMinContourLevel(Double.NaN);
	    		setMaxContourLevel(Double.NaN);
	    		isInputContourStringParsed = true;
	    	}
	    	else {
	    	       setErrorMessage(INVALID_STRING);
	    	}
	    }
		return isInputContourStringParsed;
	}	
	
	/**
	 *The method generateContourValuesList accepts as input the contourInterval, and the minimum and maximum contour values.
	 *It generates a list of Double objects representing the contour levels that lie in the range specified by minContourValue
	 *and maxContourValue. Each contour level is separated from its neighboring contour values by the value of the contourInterval.
	 *If minContourValue and maxContourValue are not exact multiples of contourInterval, then they are set to the next neighboring values
	 *that are exact multiples of the contourInterval.
	 *
	 *@param Double contourInterval
	 *@param Double minContourValue
	 *@param Double maxContourValue
	 *@return List (of type Double) contourValList
	 *
	 ***/

    /*TODO Update method or add new method to generate list when either maxContourValue/minContourValue is NAN. */
	private List<Double> generateContourValuesList(Double contourInterval,
                                                   Double minContourValue,
                                                   Double maxContourValue){
	
		List<Double> cl = new ArrayList<Double>();
              
		     /*Even if the values of minContourValue and maxContourValue parameters are exchanged
              *Swap them and generate the list
              **/ 
			  if (minContourValue > maxContourValue
						&& !Double.isNaN(maxContourValue)
						&& !Double.isNaN(minContourValue)) {
				 
				  Double tempContourVal = minContourValue;
				  minContourValue = maxContourValue;
				  maxContourValue = tempContourVal;
				  setMinContourLevel(minContourValue);
				  setMaxContourLevel(maxContourValue);
			  }
			  
			/*If contourInterval is negative, multiply it by -1*/
			  if(contourInterval < 0) {
					contourInterval*= -1;
				} 
                
			   /*If the contourInterval is defined */
               if(contourInterval != Double.NaN && contourInterval != 0) {
            	   
            	   /*If minContourValue and maxContourValue are not exact multiples of contourInterval
            	    * they are set to neighboring values that are exact multiples of the contourInterval.
            	    *
            	    **/
            	   int intMinContourValueAdjFactor = (int) (minContourValue / contourInterval);
            	   int intMaxContourValueAdjFactor = (int) (maxContourValue / contourInterval);
            	   minContourValue = intMinContourValueAdjFactor * contourInterval;
            	   maxContourValue = intMaxContourValueAdjFactor * contourInterval;
   				  
            	   /*Starting from minContourValue, until maxContourValue is reached, each contourValue is added to the list
   				    *and the next contourValue in the range is computed by adding the contourInterval to the current contour value.
   				    *Finally, the last element in the range - maxContourValue is also added to the list.
   				    **/
            	   while (minContourValue < maxContourValue
						&& !Double.isNaN(maxContourValue)
						&& !Double.isNaN(minContourValue)) {
                    
   					   cl.add(minContourValue);
   					   minContourValue += contourInterval;
				   }
				   cl.add(maxContourValue);
			   }
               else{
				/*TODO Add code to generate the contour values list when contourInterval is 0 or NAN*/
               }

		return cl;
	}
	
	/**
	 *The method checkContourStringForSingleContourValue accepts a String object as an input.
	 *It checks if the String contains a real number. If it does, then the real number is 
	 *added to the contour values list.<P>
	 *Otherwise it calls the method parseContourStringContainingLabel to check if a single labeled contour value exists.<P> 
	 *The boolean isSingleContourValueExists is set to true if the real number is 
	 *successfully extracted from the input String object
	 *
	 *@param String contourString
	 *@return boolean isSingleContourValueExists
	 **/
	
	private boolean extractContourValueFromContourStringToGenerateContourValueList(String contourString){
		Double singleContourValue;
		boolean isSingleContourValueExists = false;
		try{
			 /*Check if the input string contains a real number*/
			  if(Pattern.matches(REAL_NUMBER, contourString)){
				  /*If yes, then extract the real number*/
				  singleContourValue = Double.parseDouble(contourString);
				  /*And add it to the contour values list*/
				  this.getContourValuesList().add(singleContourValue);
				  isSingleContourValueExists = true;
			   }
			
		}catch(Exception e){
			singleContourValue = Double.NaN;
			isSingleContourValueExists = parseContourStringContainingLabel(contourString);
		}
		
		return isSingleContourValueExists;
	}
	
	/**
	 *The method parseContourStringContainingLabel accepts a string of the form contourValue=contourLabel
	 *It parses the string using the "=" character as a delimiter to separate the contourValue portion from its label. 
	 *@param String contourValueWithLabel
	 ***/
	private boolean parseContourStringContainingLabel(String contourValueWithLabel){
	    String singleContourLevelToken[];
	    String singleContourLevelLabel;
	    Double singleContourLevelValue;
	    boolean isLabelExists = false;
        /*check for the "=" delimiter, to see if the string contains a label for the contour value*/
	    if (contourValueWithLabel.contains(CONTOUR_LABEL_DELIMITER)) {
	    	
	    	/*If yes, split the string using the delimiter "=" */
			singleContourLevelToken = contourValueWithLabel.split(CONTOUR_LABEL_DELIMITER);
			
			/*Extract the contour value from the numeric data in the first string token*/
			if (Pattern.matches(REAL_NUMBER, singleContourLevelToken[0])) {
			
				singleContourLevelValue = Double.parseDouble(singleContourLevelToken[0]);
				setContourInterval(singleContourLevelValue);
				singleContourLevelLabel = singleContourLevelToken[1];
				isLabelExists           = true;
				
				/*TODO Implement the functionality to store each pair of the contour labels and values separately.*/
				//setLabelledContourValuesList(singleContourLevelValue,singleContourLevelLabel);
			}
		}
	    else{
	    	  /*No numeric data was found in the string token*/
	    	  this.setErrorMessage(INVALID_STRING);
	    }
	    
	    return isLabelExists;
	}
	
	
	/**
	 * @param List(of type Double) contourValList
	 * */
	private void setContourValuesList(List<Double> contourValList){
		this.contourValuesList = contourValList;
	}
	
	/**
	 * @param String errorMessage 
	 **/
	private void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/**
	 *@param  Double contourInterval
	 **/
	private void setContourInterval(Double contourInterval) {
		this.contourInterval = contourInterval;
	}

	/**
	 *@param  Double minContourLevel
	 **/
	private void setMinContourLevel(Double minContourLevel) {
		this.minContourLevel = minContourLevel;
	}

	/**
	 *@param  Double maxContourLevel
	 **/
	private void setMaxContourLevel(Double maxContourLevel) {
		this.maxContourLevel = maxContourLevel;
	}

	/**
	 *@param  Integer numPaddingDigits
	 **/
	private void setNumPaddingDigits(Integer numPaddingDigits) {
		this.numPaddingDigits = numPaddingDigits;
	}		


}
























