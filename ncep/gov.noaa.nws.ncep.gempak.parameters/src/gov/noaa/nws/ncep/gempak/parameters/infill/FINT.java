package gov.noaa.nws.ncep.gempak.parameters.infill;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import gov.noaa.nws.ncep.gempak.parameters.infill.FINT;
import gov.noaa.nws.ncep.gempak.parameters.core.contourinterval.ContourStringParser;
//import gov.noaa.nws.ncep.gempak.parameters.fillinterval.FINT.ZoomLevel;

/**
 * FINT accepts strings (in its overloaded constructor) matching one of the two formats below:
 * <p>
 * <tt> fillInterval/minimumFillValue/maximumValue<p>
 *  fillVal1;fillVal2;...;fillValn
 *  </tt>
 * <p>
 * <tt>FINT</tt> returns a list of fill fill levels (and/or fill interval,
 * minimum and maximum fill levels) only if the method <tt>isFINTStringParsed()</tt> returns true.
 * Hence before attempting to use a numeric value returned by any method in <tt>FINT</tt>, it is strongly
 * advised that the user checks the result of the method <tt>isFINTStringParsed()</tt>.
 * <p>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 10-Nov-2009    184        Archana.S   Initial Creation
 * 07-Jun-2010    184        Archana.S   Renamed the package as
 *                                                            gov.noaa.nws.ncep.gempak.parameters.infill 
 *  03-Aug-2010   184       Archana.S   Updated the designt to retrieve attributes specific to each zoom level
 *                                                         Implemented each zoom level as a separate FINT object in a list of 
 *                                                         FINT objects.       
 * 17-May-2011				M. Li		add parseFINT
 *                                                                                                                               
 * </pre>
 * @author Archana.S
 * @version 1
 * @see $GEMPAK/help/hlx/fint.hl2 
 */
public class FINT {
	
	/**The object responsible for parsing the FINT string*/
	private ContourStringParser fintParser;
	
	/** The input FINT string*/	
	private String fillIntervalString;
	
	/** A real number that represents the  fill interval*/	
	private Double fillInterval;
	
	/** A real number that represents the minimum  fill level*/
	private Double minFillValue;
	
	/** A real number that represents the maximum  fill level*/
	private Double maxFillValue;
	
	/** Boolean flag to validate that the FINT string was parsed correctly*/
	private boolean isFINTStringParsed;
		
	/**The un-parsed FINT string entered by the user */
	String userInputString;
	
	
	public String getUserInputString() {
		return userInputString;
	}

	private void setUserInputString(String userInputString) {
		this.userInputString = userInputString;
	}

	/**A list of FINT objects where each FINT object represents one zoom level*/
	private List<FINT> listOfFINTObjects = new ArrayList<FINT>(0);	

	/**The list of fill values*/
	private List<Double> fillValuesList;
	
	/**The list of fill values represented as String objects*/
	private List<String> fillValuesListAsString;
	
	/**The list of extracted fill labels. For FINT strings without labels, it is the fill value stored as its String equivalent*/
	private List<String> fillLabelList;
	
	private List<String> getFillLabelList() {
		return fillLabelList;
	}

	private void setFillLabelList(List<String> fillLabelList) {
		this.fillLabelList = fillLabelList;
	}

	/**A HashMap of the fill values and their labels*/
	private Map<Double, String> fintHashMap;
	
	/**@return a list of FINT objects, each of which represents one zoom level*/
	public List<FINT> getListOfFINTObjects() {
		return (new ArrayList<FINT>(listOfFINTObjects));
	}

	/**
	 * Gets the {@code HashMap<Double, String>} of the fill values and labels for a specific zoom level
	 * @param The zoom level
	 * @return The {@code HashMap<Double, String>} of the fill values and labels for the input zoom level
	 * if the FINT object for that zoom level exists or an empty map otherwise.
	 * */
	public Map<Double, String> getFintHashMap(ZoomLevel zLevel) {
		int listSize =   this.listOfFINTObjects.size();
		if(listSize >= zLevel.zoomLevel){
			Map <Double,String> thisMap = new HashMap<Double,String>(this.listOfFINTObjects.get( zLevel.zoomLevel - 1).fintHashMap);
			if(thisMap.size() > 0){
				return (thisMap);
			}
		}
		return Collections.EMPTY_MAP;
	}

/***
 * 
 * @param fintHashMap
 */
	private void setFintHashMap(Map<Double, String> fintHashMap) {
		this.fintHashMap = fintHashMap;
	}

	
	/**
	 * Gets the {@code List<Double>} of the fill values for a specific zoom level
	 * @param the zoom level
	 * @return The {@code List<Double>} of the fill values for the input zoom level
	 * if the FINT object for that zoom level exists or an empty list otherwise.
	 * */
	public List<Double> getFillValuesListAsDouble(ZoomLevel zLevel) {
		int listSize =   this.listOfFINTObjects.size();
		
		if(listSize >= zLevel.zoomLevel){
		List<Double> cList = new ArrayList<Double>(this.listOfFINTObjects.get(zLevel.zoomLevel - 1).fillValuesList);
			if( cList.size() > 0){
				return (new ArrayList<Double>(cList));
			}
		}
		return Collections.EMPTY_LIST;
	}

	/***
	 * 
	 * @param fillValuesList
	 */
	private void setFillValuesList(List<Double> fillValuesList) {
		this.fillValuesList = new ArrayList<Double>(fillValuesList);
	}

	/**An enumeration to define the 5 zoom levels allowed in FINT*/
	public static enum ZoomLevel {
		      FIRST(1), SECOND(2), THIRD(3), FOURTH(4), FIFTH(5);
		      private int zoomLevel;
		      private ZoomLevel(int index){
			          this.zoomLevel = index;
		     }
		     public int getZoomLevelAsInt(){
		    	 return zoomLevel;
		     }
	}
	
	/**Zoom  constant representing the first zoom level*/
	public static final ZoomLevel FIRST_ZOOM_LEVEL      = ZoomLevel.FIRST;
	
	/**Zoom  constant representing the second zoom level*/
	public static final ZoomLevel SECOND_ZOOM_LEVEL = ZoomLevel.SECOND;
	
	/**Zoom  constant representing the third zoom level*/
	public static final ZoomLevel THIRD_ZOOM_LEVEL     = ZoomLevel.THIRD;
	
	/**Zoom  constant representing the fourth zoom level*/
	public static final ZoomLevel FOURTH_ZOOM_LEVEL = ZoomLevel.FOURTH;
	
	/**Zoom  constant representing the fifth zoom level*/
	public static final ZoomLevel FIFTH_ZOOM_LEVEL      = ZoomLevel.FIFTH;
	
	/**Zoom  constant representing the first zoom level as the minimum level of zoom*/
	public static final ZoomLevel MIN_ZOOM_LEVEL         = ZoomLevel.FIRST;
	
	/**Zoom  constant representing the fifth zoom levelas the maximum level of zooom*/
	public static final ZoomLevel MAX_ZOOM_LEVEL        = ZoomLevel.FIFTH;
	
   /**
	 * Gets the {@code List<String>} of the fill values for a specific zoom level
	 * @param the zoom level
	 * @return The {@code List<String>} of the fill values for the input zoom level
	 * if the FINT object for that zoom level exists or an empty list otherwise.
	 * */
	public List<String> getFillValuesListAsString(ZoomLevel zLevel) {
	    List<String> cvList = Collections.EMPTY_LIST;
	    int listSize = this.listOfFINTObjects.size();
	    if( listSize > 0 &&  listSize >= zLevel.zoomLevel){
	    	cvList =  new ArrayList<String>(this.listOfFINTObjects.get(zLevel.zoomLevel - 1).fillValuesListAsString);
	    }
		return cvList;
	}

	   /**
	 * Gets the {@code List<String>} of the fill labels for a specific zoom level
	 * @param the zoom level
	 * @return The {@code List<String>} of the fill labels for the input zoom level
	 * if the FINT object for that zoom level exists or an empty list otherwise.
	 * */
	public List<String> getFillLabelsForZoomLevel(ZoomLevel zLevel){
		List<String> fintLabelList = new ArrayList<String>(0);
		int listSize = this.listOfFINTObjects.size();
		if(listSize >= zLevel.zoomLevel){
			fintLabelList = new ArrayList<String>(this.listOfFINTObjects.get( zLevel.zoomLevel-1).getFillLabelList());
		}
		return fintLabelList;
}
	
	/**
	 *The default constructor initializes the instance variables to their defaults
	 **/
	public FINT(){
		setFillInterval(Double.NaN);
		setMinFillValue(Double.NaN);
		setMaxFillValue(Double.NaN);
		isFINTStringParsed          = false;
		fintParser                          = new ContourStringParser();
		fintHashMap                     = new HashMap<Double, String>(0);
		fillValuesList              = new ArrayList<Double>(0);
		fillValuesListAsString = new ArrayList<String>(0);
		fillLabelList                 = new ArrayList<String>(0);
	}
	
//
//	/**
//	 *The overloaded constructor accepts the FINT string as an input and calls the parse method of the
//	 *ContourStringParser on it.
//	 *If the parsing is successful, the  fill interval, minimum  fill level, maximum 
//	 *fill level and the list of fill values will be populated by the corresponding parsed
//	 *data from the ContourStringParser object.
//	 *
//	 **/
	public FINT(String fillIntervalString){
		
		/*Initialize instance variables*/
		setFillInterval(Double.NaN);
		setMinFillValue(Double.NaN);
		setMaxFillValue(Double.NaN);
		fillValuesList              = new ArrayList<Double>(0);
		fillValuesListAsString = new ArrayList<String>(0);
		fillLabelList                 = new ArrayList<String>(0);

    	setUserInputString(fillIntervalString);
		fintParser = new ContourStringParser();
        parseAndSetAttributes(fillIntervalString);		

	}

	public static List<Double> parseFINT(String fint, int zoomLevelIndex, float minValue, float maxValue) {

		if (fint.equals("0"))
        	fint = "";
		
		List<Double> fvalues = null;
		Double fmin = new Double(minValue);
		Double fmax = new Double(maxValue);
		Double finterval = null;

		if (fint == null || fint.trim().length() < 1) {
			finterval = (fmax - fmin) / 10.0;
			FINT fintInfo = new FINT(finterval.toString()+"/"+fmin.toString()+"/"+fmax.toString());
			fvalues = fintInfo.getUniqueSortedFillValuesFromAllZoomLevels();
		}
		else {
			// Should be done inside FINT.java
			gov.noaa.nws.ncep.gempak.parameters.infill.FINT.ZoomLevel fzoomLevel = FINT.FIRST_ZOOM_LEVEL;
			switch (zoomLevelIndex) {
			case 1:
				fzoomLevel = FINT.FIRST_ZOOM_LEVEL;
				break;
			case 2:
				fzoomLevel = FINT.SECOND_ZOOM_LEVEL;
				break;
			case 3:
				fzoomLevel = FINT.THIRD_ZOOM_LEVEL;
				break;
			case 4:
				fzoomLevel = FINT.FOURTH_ZOOM_LEVEL;
				break;
			case 5:
				fzoomLevel = FINT.FIFTH_ZOOM_LEVEL;
				break;	
			}

			FINT fintInfo = null;
			
			/*
			 * FINT sometimes does not work for fint sep by ';'
			 */
			if (fint.contains(";")) {
				String[] strarray = fint.trim().split(";");
				fvalues = new ArrayList<Double>(strarray.length);
				for (String s : strarray) {
					Double d = new Double(Double.valueOf(s.trim()));
					fvalues.add(d);
				}	
			}
			else {
				fintInfo = new FINT(fint);
				fvalues = fintInfo.getFillValuesListAsDouble(fzoomLevel);
			}

//			System.out.println("getFillValuesListAsDouble=="+fvalues);

			if (fvalues == null || fvalues.size() < 1 /*|| fintInfo.getFillInterval(fzoomLevel) == 0.0*/) {
				fmin = fintInfo.getMinFillValue(fzoomLevel);
				if (fmin == null || fmin.isNaN()) fmin = new Double(minValue);

				fmax = fintInfo.getMaxFillValue(fzoomLevel);
				if (fmax == null || fmax.isNaN()) fmax = new Double(maxValue);
				
				finterval = fintInfo.getFillInterval(fzoomLevel);
				if (finterval == null || finterval.isNaN()) {
					finterval = (fmax - fmin) / 10.0;
				}

				fintInfo = new FINT(finterval.toString()+"/"+fmin.toString()+"/"+fmax.toString());
				fvalues = fintInfo.getUniqueSortedFillValuesFromAllZoomLevels();
			}

		}
		
		return fvalues;
	
	}
	/**@return boolean isFINTStringParsed*/
	public boolean isFINTStringParsed() {
		return isFINTStringParsed;
	}

	/**
	 * @return The portion of the parsed FINT string specific to a zoom level
	 * if the FINT object for that zoom level exists or an empty String otherwise.
	 * */
	public String getFINTString(ZoomLevel zLevel){
	    String currentFINTString="";
		int listSize =   this.listOfFINTObjects.size();
		if(listSize >= zLevel.zoomLevel){
			currentFINTString = new String ( this.listOfFINTObjects.get(zLevel.zoomLevel-1).fillIntervalString);
		}	    
		return currentFINTString;
	}

	/**
	 * @return The fill interval specific to a zoom level
	 * if the FINT object for that zoom level exists or NaN otherwise.
	 * */
	public Double getFillInterval(ZoomLevel zLevel) {
		Double currentFillInterval = Double.NaN;
		int listSize =   this.listOfFINTObjects.size();
		if(listSize >= zLevel.zoomLevel){
			currentFillInterval = new Double ( this.listOfFINTObjects.get(zLevel.zoomLevel-1).fillInterval);
		}
		return currentFillInterval;
	}
	

	/**
	 * @return The minimum fill value specific to a zoom level
	 * if the FINT object for that zoom level exists or NaN otherwise.
	 * */
	public Double getMinFillValue(ZoomLevel zLevel) {
		Double currentMinFillValue = Double.NaN;
		int listSize =   this.listOfFINTObjects.size();
		if(listSize >= zLevel.zoomLevel){
			currentMinFillValue = new Double ( this.listOfFINTObjects.get(zLevel.zoomLevel-1).minFillValue);
		}
		return currentMinFillValue;
	}
	
	/**
	 * @return The maximum fill value specific to a zoom level
	 * if the FINT object for that zoom level exists or NaN otherwise.
	 * */
	public Double getMaxFillValue(ZoomLevel zLevel) {
		Double currentMaxFillValue = Double.NaN;
		int listSize =   this.listOfFINTObjects.size();
		if(listSize >= zLevel.zoomLevel){
			currentMaxFillValue = new Double ( this.listOfFINTObjects.get(zLevel.zoomLevel-1).maxFillValue);
		}
		return currentMaxFillValue;
	}

	/***
	 * 
	 * @return a list String objects representing  the unique fill values from all zoom levels
	 */
	public List <String> getUniqueSortedFillValuesFromAllZoomLevelsAsString(){
		List <Double> sortedKeySet = getUniqueSortedFillValuesFromAllZoomLevels();
		List <String> fillValList = new ArrayList <String>(0); 
		if(sortedKeySet.size() > 0){
			for(Double fillValue : sortedKeySet){
				fillValList.add(fillValue.toString());
			}
		}
		return fillValList;
	}
	
/***
 * 	
 * @return a list of Double objects representing the unique fill values from all zoom levels 
 */
	public List<Double> getUniqueSortedFillValuesFromAllZoomLevels(){
		Set<Double> setOfUnqiueFillValues = new HashSet<Double>(0);
		List<Double> sortedList = new ArrayList<Double>(0);
		
		int length = this.listOfFINTObjects.size();
			for (int index = 0; index < length; index++) {
				FINT currentFINTObj = this.listOfFINTObjects.get(index);
				setOfUnqiueFillValues.addAll(currentFINTObj.fillValuesList);

            sortedList = new ArrayList<Double>(setOfUnqiueFillValues);
			Collections.sort(sortedList);
		}
		return sortedList;
	}

	private void setFillValuesListAsString(List<String> fillValuesListAsString) {
		this.fillValuesListAsString = fillValuesListAsString;
	}
	
	/**@param boolean isFINTStringParsed*/
	private void setFINTStringParsed(boolean isFINTStringParsed) {
		this.isFINTStringParsed = isFINTStringParsed;
	}
	
	/**@param String fintString*/
	private void setFINTString(String fintString){
		fillIntervalString = new String(fintString);
	}

	/**@param Double fillInterval*/
	private void setFillInterval(Double fillInterval) {
		this.fillInterval = new Double(fillInterval);
	}

	/**@param Double minFillValue*/
	private void setMinFillValue(Double minFillValue) {
		this.minFillValue = new Double(minFillValue);
	}
	
	/**@param Double maxFillValue */
	private void setMaxFillValue(Double maxFillValue) {
		this.maxFillValue = new Double (maxFillValue);
	}

	/***
	 * 
	 * @param inputStr
	 */
	private void parseAndSetAttributes(String inputStr){
		boolean isParsed = false;
		if (inputStr != null) {
			String fillLevelStringsArray[] = inputStr.split(">");
			int lengthOfFillLevelStringsArray = fillLevelStringsArray.length;
			if(lengthOfFillLevelStringsArray > FINT.MAX_ZOOM_LEVEL.zoomLevel){
				lengthOfFillLevelStringsArray = FINT.MAX_ZOOM_LEVEL.zoomLevel;
			}
			
			listOfFINTObjects = new ArrayList<FINT>(lengthOfFillLevelStringsArray);
			
			for (int index = 0; index < lengthOfFillLevelStringsArray; index++) {

//				/*Invoke the parse method of ContourStringParser*/
			   fintParser.parse(fillLevelStringsArray[index]);
              
			   //create the FINT object for the current zoom level
			   FINT currentFINTObj = new FINT();
              currentFINTObj.setFINTStringParsed(fintParser.isContourStringParsed());

				/*If the parse operation was successful, extract the numeric
				 *data and set the corresponding instance variables of currentFINTObj*/

				if (currentFINTObj.isFINTStringParsed()) {
					currentFINTObj.setFINTString(fillLevelStringsArray[index]);
					currentFINTObj.setFillInterval(fintParser.getContourInterval());
					currentFINTObj.setMinFillValue(fintParser.getMinContourLevel());
					currentFINTObj.setMaxFillValue(fintParser.getMaxContourLevel());
					currentFINTObj.setFillValuesList(fintParser.getContourValuesList());
					currentFINTObj.setFintHashMap(fintParser.getLabeledContourValuesHashMap());
					Set<Double> tempKeySet  = new LinkedHashSet<Double>(currentFINTObj.fintHashMap.keySet());
					currentFINTObj.setFillValuesList( new ArrayList<Double>(tempKeySet));
					for(Double fillValue : tempKeySet){
						currentFINTObj.fillValuesListAsString.add(fillValue.toString());
				    }
					currentFINTObj.fillLabelList = new ArrayList<String>( new LinkedHashSet<String>(currentFINTObj.fintHashMap.values()));
				}
				
				if(index == 0){
					isParsed = currentFINTObj.isFINTStringParsed();
				}else{
					isParsed = isParsed & currentFINTObj.isFINTStringParsed();
				}
				
				//Sets the status of the parse operations across all zoom levels (i.e. for all FINT objects in the list)
				this.setFINTStringParsed(isParsed);

				//finally add currentFINTObj to the list of FINT objects
				listOfFINTObjects.add(currentFINTObj);
			}
		}		
	}
	
	
	
}
