package gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea;

import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.station.*;
/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25-Sep-2009     171      Archana    Initial Version
 * 
 * </pre>   
 * @author Archana
 * @version 1
 */

public class GraphicsAreaCoordinates {


	protected double lowerLeftLat, lowerLeftLon, upperRightLat, upperRightLon;
	protected double centerLat, centerLon, deltaLat, deltaLon;

	private double defaultAndle1, defaultAngle2; 

	protected String geogAreaCode;
	protected String station_code;
	protected String mapProjectionStr;
	protected String errorMessage;
	protected boolean isGraphicsAreaStrValid;
	public boolean isGraphicsAreaStrValid() {
		return isGraphicsAreaStrValid;
	}

	public void setGraphicsAreaStrValid(boolean isGraphicsAreaStrValid) {
		this.isGraphicsAreaStrValid = isGraphicsAreaStrValid;
	}

	protected boolean errorMsgSet;
	
	private String graphicsAreaString; 
	private String geogFileName = "res/geog.xml"; 
	private String stationFileName = "res/sfstns.xml"; 
	
	/*
	 * All getters go below
	 */
	public String getGraphicsAreaString() {
		return graphicsAreaString;
	}

	public double getLowerLeftLat() {
		return lowerLeftLat;
	}

	public double getLowerLeftLon() {
		return lowerLeftLon;
	}

	public double getUpperRightLat() {
		return upperRightLat;
	}

	public double getUpperRightLon() {
		return upperRightLon;
	}

	public double getCenterLat() {
		return centerLat;
	}

	public double getCenterLon() {
		return centerLon;
	}

	public double getDefaultAngle1UsingGempakProjectionName(String gempakProjectionName) {
		if("CED".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = 0.0; 
		else if("MCD".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = 0.0; 
		else if("MER".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = 0.0; 
		else if("NPS".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = 90.0; 
		else if("LCC".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = 30.0; 
		else if("SCC".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = -30.0; 
		else if("SPS".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = -90.0; 
		else if("UTM".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = getDefaultAngle2UsingGempakProjectionName(gempakProjectionName); 
		else if("NOR".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = 90.0; 
		else if("SOR".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = -90.0; 
		else if("ORT".equalsIgnoreCase(gempakProjectionName))
			defaultAndle1 = 0.0; 
		
		
		return defaultAndle1;
	}

	public double getDefaultAngle2UsingGempakProjectionName(String gempakProjectionName) {
		if("UTM".equalsIgnoreCase(gempakProjectionName))  {
			defaultAngle2 = 0.0; 
		} else {
			defaultAngle2 = calculateAverageLongitude(lowerLeftLon, upperRightLon); 
		}
		return defaultAngle2;
	}
	
	public double getDefaultAngle1UsingGeotoolsProjectionName(String geotoolsProjectionName) {
		if("Equidistant_Cylindrical".equalsIgnoreCase(geotoolsProjectionName))
			defaultAndle1 = 0.0; 
		else if("Polar_Stereographic".equalsIgnoreCase(geotoolsProjectionName))
			defaultAndle1 = 0.0; 
		else if("Mercator_1SP".equalsIgnoreCase(geotoolsProjectionName))
			defaultAndle1 = 0.0; 
		else if("Orthographic".equalsIgnoreCase(geotoolsProjectionName))
			defaultAndle1 = 0.0; 
		else if("Transverse_Mercator".equalsIgnoreCase(geotoolsProjectionName))
			defaultAndle1 = 0.0; 
		else if("Lambert_Azimuthal_Equal_Area".equalsIgnoreCase(geotoolsProjectionName))
			defaultAndle1 = 0.0; 
		else if("Lambert_Conformal_Conic_2SP".equalsIgnoreCase(geotoolsProjectionName))
			defaultAndle1 = 30.0; 
		
		return defaultAndle1;
	}

	public double getDefaultAngle2UsingGeotoolsProjectionName() {
		defaultAngle2 = calculateAverageLongitude(lowerLeftLon, upperRightLon); 
		return defaultAngle2;
	}
	
	public void setGeogFileName(String geogFileName) {
		this.geogFileName = geogFileName;
	}

	public void setStationFileName(String stationFileName) {
		this.stationFileName = stationFileName;
	}

	//*********************************************************************************************************		
	/*  	These variables retain the name of their Fortran counterparts in the file lcabnd.f
        They are used in the calculations of the zoom factor to update the Latitude/Longitude values,
        when a zoom-in/zoom-out option is specified along with either the area code from the geog table or the 
        station code from the station table.*/

	protected double zoomul=0;  
	protected int izoom=0, iartyp=-1;
	protected float zmofst=0;

	//*********************************************************************************************************	
	protected final String VALID_NUMBER                    = "-?\\d{0,3}\\.?\\d*";    
	protected final String EXTRA_ARGS                      = "Too many arguments entered"; 
	protected final String LESS_NUM_ARGS                   = "Too few arguments entered";
	protected final String INVALID_LAT_LON_VALUES          = "Invalid lat/lon values entered";
	protected final String INVALID_CENTER_DELTA_LON_VALUES = "center_lon - delta_lon should be >= -180.0 and center_lon + delta_lon should be <=360.0";
	protected final String NEGATIVE_DELTA_LON              = "Delta Longitude values cannot be negative";
	protected final String INVALID_CENTER_DELTA_LAT_VALUES = "center_lat - delta_lat should be >= -90.0 and center_lat + delta_lat should be <=90.0";
	protected final String NEGATIVE_DELTA_LAT              = "Delta Latitude values cannot be negative";
	protected final String INVALID_CENTER_LON              = "Center Longtude can only take values between -180.00 to 180.00 or 0.00 to 360.00";
	protected final String INVALID_CENTER_LAT              ="Center Latitude can only take values between -90.00 and 90.00";    
	protected final String LL_LAT_GREATER_THAN_UR_LAT      ="Lower left latitude must be less than or equal to upper right latitude";	
	protected final String LL_LON_GREATER_THAN_UR_LON      = "Lower left longitude must be less than or equal to upper right longitude";
	protected final String INVALID_UR_LON                  = "Upper Right Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00";
	protected final String INVALID_UR_LAT                  = "Upper Right Latitude can only take values between -90.00 and 90.00";
	protected final String INVALID_LL_LON                  = "Lower Left Longitude can only take values between -180.00 to 180.00 or 0.00 to 360.00";
	protected final String INVALID_LL_LAT                  = "Lower Left Latitude can only take values between -90.00 and 90.00";
	protected final String INVALID_STR                     = "Invalid String Format";

	/**
	 * Overloaded constructor takes the GAREA string as input. 	    
	 * @param s
	 */
	public GraphicsAreaCoordinates(String str) {
		graphicsAreaString = str; 
		
		lowerLeftLat = -1000.0f; 
		lowerLeftLon = -1000.0f; 
		upperRightLat = -1000.0f; 
		upperRightLon = -1000.0f;
		centerLat = -1000.0f; 
		centerLon = -1000.0f; 
		deltaLat = -1000.0f; 
		deltaLon = -1000.0f;
		errorMsgSet = false;
//		isGraphicsAreaStrValid = parseGraphicsAreaString(str);
	}
	
	/**
	 * a helper method to indicate if the graphic area string starts 
	 * with a valid Geog name in Geog Table
	 */
	public boolean isValidGeogName() {
		boolean isValid = false; 
		if(iartyp == 3 && isGraphicsAreaStrValid)
			isValid = true; 
		return isValid; 
	}
	
	/**
	 * a helper method to indicate if the graphic area string starts 
	 * with a valid station name in the Station table
	 */
	public boolean isValidStationName() {
		boolean isValid = false; 
		if(iartyp == 4 && isGraphicsAreaStrValid)
			isValid = true; 
		return isValid; 
	}
	
	/**
	 * a helper method to indicate if the graphic area string contains 
	 * valid center and delta lat/lon values
	 */
	public boolean isValidCenterDeltaLatLonValues() {
		boolean isValid = false; 
		if(isGraphicsAreaStrValid && 
				!isStringEmpty(graphicsAreaString) && 
				graphicsAreaString.startsWith("#"))
			isValid = true; 
		return isValid; 
	}
	
	/**
	 * a helper method to indicate if the graphic area string contains 
	 * valid lower left and upper right lat/lon values
	 */
	public boolean isValidLowerLeftAndUpperRightLatLonValues() {
		boolean isValid = false; 
		if(isGraphicsAreaStrValid && 
				!isStringEmpty(graphicsAreaString) && 
				!isValidCenterDeltaLatLonValues() &&
				!isValidStationName() &&
				!isValidGeogName())
			isValid = true; 
		return isValid; 
	}
	
	/**
	 * @param s
	 * The method parseGraphicsAreaString() accepts as input a single string that denotes the graphics area coordinates 
	 * in one of the formats listed below:
	 * #clta;clon;dlat;dlon - center latitude/longitude and the delta latitude/longitude values.
	 * lat1;lon1;lat2;lon2  - lower left latitude/longitude and upper right latitude/longitude values.
	 * GEOG - a geographical area code in the file geog.xml
	 * STN  - a station code in the file station.xml  
	 * For the ';' separated strings, after checking the validity of the input string, the function parses it 
	 * into tokens to extract the latitude/longitude values.
	 * For the GEOG/STN codes, if it finds the corresponding record in the geog table or the station table,
	 * it retrieves the corresponding latitude/longitude values. Additionally, for the GEOG code,
	 * it also retrieves the map projection string. 
	 * @return graphics_area_str_valid
	 */

	public boolean parseGraphicsAreaString(String gAreaString) {

		int i = 0; 
		String geogCodeStr;
		
//		boolean isGAreaStringValid = true; 
		
		if(isStringEmpty(gAreaString))
			return false; 
		
//		if(gAreaString.length()!=0){
			if(gAreaString.charAt(0)=='#'){
				iartyp = 1;
				geogCodeStr = gAreaString.substring(1);
				isGraphicsAreaStrValid = validateLatLonInputString(geogCodeStr);

				if(isGraphicsAreaStrValid){

					setCenterDeltaLatLonValues(geogCodeStr);
					isGraphicsAreaStrValid = validateCenterDeltaLatLonValues(centerLat,centerLon,deltaLat,deltaLon);

					if(isGraphicsAreaStrValid){

						calculateLLURLatLonFromCDLatLon(centerLat, centerLon, deltaLat, deltaLon); 

						// The latitude/longitude values of the lower left and upper right coordinates are checked to ensure that
						//they lie within the valid range and that the lower left latitude/longitude values 
						//are less than the upper right latitude/longitude values.

						isGraphicsAreaStrValid = validateLowerUpperLatLonValues(lowerLeftLat,lowerLeftLon,upperRightLat,upperRightLon);
						if(isGraphicsAreaStrValid){
							errorMessage = "Valid String";
							errorMsgSet = true;
						}

					}
				}
//				else{
//					isGraphicsAreaStrValid = false;
//					if(!errorMsgSet){
//						errorMessage = INVALID_STR;
//						errorMsgSet = true;
//					}
//				}

			}
			else{

				isGraphicsAreaStrValid = validateLatLonInputString(gAreaString);
				if(isGraphicsAreaStrValid){
					iartyp = 2;
					setLowerUpperLatLonValues(gAreaString);
					isGraphicsAreaStrValid = validateLowerUpperLatLonValues(lowerLeftLat, lowerLeftLon, upperRightLat, upperRightLon);

					//The calculations for center_lat and center_lon are derived from lcabnd.f				          
					if(isGraphicsAreaStrValid){
						centerLat = (lowerLeftLat+upperRightLat)/2;
						centerLon = (lowerLeftLon+upperRightLon)/2;
						isGraphicsAreaStrValid = validateCenterDeltaLatLonValues(centerLat, centerLon, 0.0f, 0.0f);
						if(isGraphicsAreaStrValid){
							errorMessage = "Valid String";
							errorMsgSet = true;
						}
					}                      	        	 
				}

				else{


					/*		    		                   A suffix such as '+' or '*' character, when appended to the GEOG code or the STN code
 		    	                       increases the extent of the area. On the other hand, appending a '-' character to the GEOG/STN
		    	                       code decreases the extent of the area. This suffix character is ignored and only the substring  
		    	                       containing just the GEOG or STN code alone is considered for comparison with the data in the XML files.*/

					for(i=0;i<gAreaString.length();i++){
						if(gAreaString.charAt(i)== '+'|| gAreaString.charAt(i) == '*'){
							izoom++;
							gAreaString = gAreaString.replace(gAreaString.charAt(i),' ');
						}

						if(gAreaString.charAt(i)== '-'){
							izoom--;
							gAreaString = gAreaString.replace(gAreaString.charAt(i),' ');		    	    		                
						}	    	    	   
					}

					if(izoom != 0){
						zoomul = computeZoomMultiplier(izoom);    

					}

					geogCodeStr = gAreaString.trim().toUpperCase(Locale.ENGLISH);

					try{
						iartyp = searchGeogTable(geogCodeStr);
//						GeographicalData geographicalData = searchGeogTable(geogCodeStr);
						if(iartyp != 3){
//						if(geographicalData == null) {
							iartyp = searchStationTable(geogCodeStr);
							if(iartyp==0){
								isGraphicsAreaStrValid = false;
							}
							else{
								if(izoom != 0){
									deltaLat = (float)(4.0);
									deltaLon = (float)(7.0);
									lowerLeftLat = (float)(centerLat - zoomul*deltaLat);
									upperRightLat = (float)(centerLat + zoomul*deltaLon);
									lowerLeftLon = (float)(centerLon - zoomul*deltaLon);
									upperRightLon = (float)(centerLon + zoomul*deltaLon); 
								}
								isGraphicsAreaStrValid = true;
							}
						}
						else{
							if(izoom != 0){
								zmofst = (float)((1.0 - zoomul)/2);
								lowerLeftLat = lowerLeftLat + (upperRightLat-lowerLeftLat)*zmofst;
								upperRightLat = upperRightLat - (upperRightLat-lowerLeftLat)*zmofst;
								lowerLeftLon = lowerLeftLon + (upperRightLon-lowerLeftLon)*zmofst;
								upperRightLon = upperRightLon - (upperRightLon-lowerLeftLon)*zmofst;
							}
							isGraphicsAreaStrValid = true;
						}                 	        	         
					}
					catch(Exception e){
						e.printStackTrace();
					}

					if(!isGraphicsAreaStrValid && !errorMsgSet){
						errorMessage = INVALID_STR;
					}

				}

			}

//		}
//		else{
//			errorMessage = "Empty String";
//			errorMsgSet = true;
//			isGraphicsAreaStrValid = false;
//		}


		return isGraphicsAreaStrValid;
	}

	/**
	 * The logic of this method is taken from Fortran function PRNLON.f
	 * This method converts a longitude in degrees into degrees which  
	 * fall within the range 180 to -180. 
	 */
	private double convertLongitudeValue(double longitudeValue) {
		double convertedLonValue = longitudeValue; 
		if(longitudeValue < -180.0) 
			convertedLonValue = longitudeValue + 360.0; 
		else if(longitudeValue > 180.0)
			convertedLonValue = longitudeValue - 360.0;
		return convertedLonValue; 
	}
	
	/**
	 * This method calculate the average longitude for setting up projection. 
	 * The logic is taken from Fortran function gammap.f 
	 * @param lowerLeftLon, lower left longitude
	 * @param upperRightLon, upper right longitude
	 * @return average longitude for setting up projection 
	 */
	private double calculateAverageLongitude(double lowerLeftLon, double upperRightLon) { 
		double averageLongitude = 0.0; 
		if(lowerLeftLon == upperRightLon)
			averageLongitude = lowerLeftLon + 180.0; 
		else if(lowerLeftLon > upperRightLon)
			averageLongitude = (360.0 + lowerLeftLon + upperRightLon) / 2; 
		else 
			averageLongitude = (lowerLeftLon + upperRightLon) / 2;
		
		/*
		 * truncate the double number with maximumly 3 digits after the DOT
		 */
		averageLongitude = truncateDoubleValue(averageLongitude); 
		return convertLongitudeValue(averageLongitude); 
	}
	
	private double truncateDoubleValue(double d) {
		double truncatedNumber = d > 0 ? Math.floor(d*1000)/1000.0 :
			Math.ceil(d*1000) / 1000.0; 
		return truncatedNumber; 
	}
	
	/**
	 * The function searchGeogTable accepts as input a string 'code_str' that represents a code in the geog table
	 * and if the input string matches a code for the geographical area in the file geog.xml,
	 * the corresponding Latitude/Longitude values and the projection string are retrieved from the file. 
	 * @param code_str
	 * @return found
	 * @throws Exception
	 */


	public int searchGeogTable(String geogCode) throws Exception{
//	public GeographicalData searchGeogTable(String geogCode) throws Exception{
//		GeographicalData geogData = null; 
		GeographicalDataReader geogDataReader = new GeographicalDataReader(geogFileName);
		List<GeographicalData> geographicalDataList = geogDataReader.getGeographicalData();
		int found = 0;
		for(GeographicalData currentGeogData : geographicalDataList){

			if(currentGeogData.getGeogCode().equals(geogCode)){
				geogAreaCode = currentGeogData.getGeogCode();
				centerLat = currentGeogData.getCenterLat();
				centerLon = currentGeogData.getCenterLon();                               
				lowerLeftLat = currentGeogData.getLowerLeftLat();
				lowerLeftLon = currentGeogData.getLowerLeftLon();
				upperRightLat = currentGeogData.getUpperRightLat();
				upperRightLon = currentGeogData.getUpperRightLon();	
				mapProjectionStr = currentGeogData.getMapProjectionString();
				found = 3; 

//				geogData = currentGeogData; 
				break; 
			}
		}	
		return found;
	}
	
	/**
	 * The function searchStationTable accepts as input a string 'code_str'that represents a code in the station table
	 * and if the input string matches a code for the geographical area in the file sfstns.xml,
	 * the corresponding Latitude/Longitude values of the station are retrieved from the file.	
	 * @param code_str
	 * @return found
	 * @throws Exception
	 */


	public int searchStationTable(String geogCodeStr) throws Exception{
//	public Station searchStationTable(String geogCodeStr) throws Exception{
		int found = 0;
//		Station station = null; 
		StationDataReader stnDataReader = new StationDataReader(stationFileName);
		List<Station> stationList = stnDataReader.getStationData();
		for(Station currentStation : stationList){
			
			if(currentStation.getStid().equals(geogCodeStr)){
				station_code = currentStation.getStid(); 

				// The center_lat and center_lon denote the Latitude/Longitude values of a point location - the station.
				centerLat = currentStation.getLatitude();
				centerLon = currentStation.getLongitude();
				
				/*
				 * At this time, we use a hard coded value deltaLat = 4.0 and 
				 * deltaLon = 7.0 temporarily to calculate lower left and upper
				 * right lat/lon values. The two hard coded values taken from 
				 * lcabnd.f. I am not clear if the values are used correctly. 
				 * M. Gao comments
				 */
				calculateLLURLatLonFromCDLatLon(centerLat, centerLon, 
						(float)4.0, (float)7.0); 
				found = 4;
//				station = currentStation; 
			}
		}

		return found;
//		return station; 
	}

	/**
	 * The function computeZoomMultiplier() is a private function to compute the zoom multiplier.
	 * @param zoom_flag
	 * @return zm
	 */

	private double computeZoomMultiplier(int zoomFlag){
		double zm = 0.0;
		if(zoomFlag>0){
			zm = (1/Math.pow(2, zoomFlag));
		} else {
			zm = Math.pow(2, zoomFlag * (-1));
		} 
		return zm;   
	}

	/**
	 * This function validateInputString() parses the input string 's' using the ';' as a delimiter and extracts 4 latitude/longitude values
	 * It then proceeds to check that each extracted token contains only a valid numeric value for the
	 * latitude(s) and the longitude(s).
	 * @param s
	 * @return flag
	 */
	private boolean validateLatLonInputString(String latLonStr){
		boolean isValid =  false;
		int i=0;

		if(isStringEmpty(latLonStr))
			return isValid; 

		String latLonTokens[] = latLonStr.split(";");

		/*
		 * In the case users type in more values, e.g. latLonTokens.length > 4
		 * We just simply ignore the rest values instead of getting an error
		 * This way our code is more use friendly
		 */
		if(latLonTokens.length < 4) {
			errorMessage = LESS_NUM_ARGS;
			errorMsgSet = true;
			return isValid;
		}
		
		for(i=0;i<4;i++) {
			isValid = Pattern.matches(VALID_NUMBER, latLonTokens[i]);
			if(!isValid){
				errorMessage = INVALID_LAT_LON_VALUES;
				errorMsgSet = true;
				break; 
			} 
		}
		
		return isValid;
	}

	/**
	 * a helper method to check if a string is empty
	 */
	private boolean isStringEmpty(String str) {
		boolean isValid = false; 
		if(str == null || str.trim().length() == 0)
			isValid = true; 
		return isValid; 
	}


	/**
	 * The function validateLatLonValues() checks the accepted range of values for the input latitude/longitude coordinates.
	 * In case there is any discrepancy, the function sets an error message string to indicate the nature of the problem.
	 * Latitudes can range only from -90.00 to 90.00
	 * Longitude values can range only from -180.00 to 360.00
	 * Delta latitude/longitude values cannot be negative.
	 * @param g_area_typ
	 * @param lat1
	 * @param lon1
	 * @param lat2
	 * @param lon2
	 * @return flag
	 */

//	private boolean validateLatLonValues(int g_area_typ, Float lat1, Float lon1, Float lat2, Float lon2){
//		boolean flag = false;
//
//		if (g_area_typ == 1) {
//			if(lat1 >= -90.0f && lat1 <= 90.0){
//				if(lon1 >= -180.0f && lon1 <= 360.0f){
//
//					if(lat2 >= 0){
//						if(((lat1 - lat2 >= -90) && (lat1 + lat2) <=90)){
//							if(lon2 >= 0)  {
//								if ((lon1 - lon2 >= -180) && (lon1 + lon2) <=360){
//									flag = true;
//									errorMessage = "Valid String";	 
//									errorMsgSet = true;
//								}
//								else{
//									flag = false;
//									errorMessage = INVALID_CENTER_DELTA_LON_VALUES;
//									errorMsgSet = true;
//									centerLat = -1000.0f;
//									centerLon = -1000.0f;
//									deltaLat  = -1000.0f;
//									deltaLon  = -1000.0f;
//								}
//							}
//							else{
//								errorMessage = NEGATIVE_DELTA_LON; 
//								errorMsgSet = true;
//								deltaLon = -1000.0f;
//								flag = false;
//							}
//						}
//						else{
//							flag = false;
//							errorMessage = INVALID_CENTER_DELTA_LAT_VALUES;	            	 
//							errorMsgSet = true;
//							centerLat = -1000.0f;
//							centerLon = -1000.0f;
//							deltaLat  = -1000.0f;
//							deltaLon  = -1000.0f;	              
//						}
//					}
//					else{
//						errorMessage = NEGATIVE_DELTA_LAT; 
//						errorMsgSet = true;
//						deltaLat = -1000.0f;
//						flag = false;
//					}
//
//				}
//				else{
//					errorMessage = INVALID_CENTER_LON;
//					errorMsgSet = true;
//					centerLon = -1000.0f;
//					flag = false;
//				}
//			}
//			else{
//				errorMessage = INVALID_CENTER_LAT;
//				errorMsgSet = true;
//				centerLat = -1000.0f;
//				flag = false;
//			}
//		}
//
//		if (g_area_typ == 2) {
//			if(lat1 >= -90.0f && lat1 <= 90.0){
//				if(lon1 >= -180.0f && lon1 <= 360.0f){
//					if(lat2 >= -90.0f && lat2 <= 90.0){
//						if(lon2 >= -180.0f && lon2 <= 360.0f){
//
//							if(lat1 > lat2){
//								this.errorMessage = LL_LAT_GREATER_THAN_UR_LAT;
//								errorMsgSet = true;
//								this.lowerLeftLat = -1000.0f;
//								this.upperRightLat = -1000.0f;
//								flag = false;
//							}
//
//							else if(lon1 > lon2){
//
//								this.errorMessage = LL_LON_GREATER_THAN_UR_LON;
//								errorMsgSet = true;
//								this.lowerLeftLon = -1000.0f;
//								this.upperRightLon = -1000.0f;
//								flag = false;							 
//							}
//
//							else{
//								flag =  true;
//								errorMessage = "Valid String";
//								errorMsgSet = true;						
//							}
//
//						}
//						else{
//							this.errorMessage = INVALID_UR_LON;
//							errorMsgSet = true;
//							this.upperRightLon = -1000.0f;
//							flag = false;								
//						}
//					}
//					else{
//						this.errorMessage = INVALID_UR_LAT;
//						errorMsgSet = true;
//						this.upperRightLat = -1000.0f;
//						flag = false;
//					}
//				}
//				else{
//					this.errorMessage = INVALID_LL_LON;
//					errorMsgSet = true;
//					this.lowerLeftLon = -1000.0f;
//					flag = false;
//				}
//			}
//			else{
//				this.errorMessage = INVALID_LL_LAT;
//				errorMsgSet = true;
//				this.lowerLeftLat = -1000.0f;
//				flag = false;
//
//			}
//
//		}
//
//		return flag;
//	}
	private boolean validateCenterDeltaLatLonValues(double _centerLat, double _centerLon, double _deltalat, double _deltaLon){
		boolean flag = false;

		if(_centerLat >= -90.0f && _centerLat <= 90.0){
			if(_centerLon >= -180.0f && _centerLon <= 360.0f){

				if(_deltalat >= 0){
					if(((_centerLat - _deltalat >= -90) && (_centerLat + _deltalat) <=90)){
						if(_deltaLon >= 0)  {
							if ((_centerLon - _deltaLon >= -180) && (_centerLon + _deltaLon) <=360){
								flag = true;
								errorMessage = "Valid String";	 
								errorMsgSet = true;
							}
							else{
								flag = false;
								errorMessage = INVALID_CENTER_DELTA_LON_VALUES;
								errorMsgSet = true;
								centerLat = -1000.0f;
								centerLon = -1000.0f;
								deltaLat  = -1000.0f;
								deltaLon  = -1000.0f;
							}
						}
						else{
							errorMessage = NEGATIVE_DELTA_LON; 
							errorMsgSet = true;
							deltaLon = -1000.0f;
							flag = false;
						}
					}
					else{
						flag = false;
						errorMessage = INVALID_CENTER_DELTA_LAT_VALUES;	            	 
						errorMsgSet = true;
						centerLat = -1000.0f;
						centerLon = -1000.0f;
						deltaLat  = -1000.0f;
						deltaLon  = -1000.0f;	              
					}
				}
				else{
					errorMessage = NEGATIVE_DELTA_LAT; 
					errorMsgSet = true;
					deltaLat = -1000.0f;
					flag = false;
				}

			}
			else{
				errorMessage = INVALID_CENTER_LON;
				errorMsgSet = true;
				centerLon = -1000.0f;
				flag = false;
			}
		}
		else{
			errorMessage = INVALID_CENTER_LAT;
			errorMsgSet = true;
			centerLat = -1000.0f;
			flag = false;
		}

		return flag;
	}
	
	private boolean validateLowerUpperLatLonValues(double _lowerLat, double _lowerLon, double _upperLat, double _upperLon){
		boolean flag = false;

		if(_lowerLat >= -90.0f && _lowerLat <= 90.0){
			if(_lowerLon >= -180.0f && _lowerLon <= 360.0f){
				if(_upperLat >= -90.0f && _upperLat <= 90.0){
					if(_upperLon >= -180.0f && _upperLon <= 360.0f){

						if(_lowerLat > _upperLat){
							this.errorMessage = LL_LAT_GREATER_THAN_UR_LAT;
							errorMsgSet = true;
							this.lowerLeftLat = -1000.0f;
							this.upperRightLat = -1000.0f;
							flag = false;
						}

						else if(((_lowerLon<=0 && _upperLon<=0) || (_lowerLon>=0 && _upperLon>=0)) && _lowerLon > _upperLon){

							this.errorMessage = LL_LON_GREATER_THAN_UR_LON;
							errorMsgSet = true;
							this.lowerLeftLon = -1000.0f;
							this.upperRightLon = -1000.0f;
							flag = false;							 
						} else if((_lowerLon>0 && _upperLon<0) && _lowerLon < _upperLon) {
							this.errorMessage = LL_LON_GREATER_THAN_UR_LON;
							errorMsgSet = true;
							this.lowerLeftLon = -1000.0f;
							this.upperRightLon = -1000.0f;
							flag = false;							 
						} else if(isLowerLeftPointSameAsUpperRightPoint(_lowerLat, _lowerLon, _upperLat, _upperLon)) {
							this.errorMessage = "The lower left coordinate point is the same as the upper right coordinate point";
							errorMsgSet = true;
							this.lowerLeftLon = -1000.0f;
							this.upperRightLon = -1000.0f;
							flag = false;							 
						} else{
							flag =  true;
							errorMessage = "Valid String";
							errorMsgSet = true;						
						}

					}
					else{
						this.errorMessage = INVALID_UR_LON;
						errorMsgSet = true;
						this.upperRightLon = -1000.0f;
						flag = false;								
					}
				}
				else{
					this.errorMessage = INVALID_UR_LAT;
					errorMsgSet = true;
					this.upperRightLat = -1000.0f;
					flag = false;
				}
			}
			else{
				this.errorMessage = INVALID_LL_LON;
				errorMsgSet = true;
				this.lowerLeftLon = -1000.0f;
				flag = false;
			}
		}
		else{
			this.errorMessage = INVALID_LL_LAT;
			errorMsgSet = true;
			this.lowerLeftLat = -1000.0f;
			flag = false;

		}

		return flag;
	}
	
	private boolean isLowerLeftPointSameAsUpperRightPoint(double _lowerLat, double _lowerLon, double _upperLat, double _upperLon) {
		boolean result = false; 
		if(_lowerLat == _upperLat && _lowerLon ==_upperLon)
			result = true; 
		else if(isPole(_lowerLat, _upperLat)) {
			if((_lowerLon == 180 && _upperLon == -180) || (_lowerLon == 1180 && _upperLon == 180))
				result = true; 
		}
		
		return result; 
	}
	
	private boolean isPole(double _lowerLat, double _upperLat) {
		boolean result = false; 
		if(_lowerLat == 90 && _upperLat == 90)
			result = true; 
		return result; 
	}

	/**
	 * The function getGeogAreaCode() returns the value of the geographical area code, which is retrieved 
	 * only if the boolean flag graphics_area_str_valid is true and if the flag 'iartyp' is set to 3
	 * (indicating that a geog code was entered)
	 * @return geog_area_code
	 */

	public String getGeogAreaCode(){
		if(this.iartyp == 3 && this.isGraphicsAreaStrValid == true){
			return this.geogAreaCode;	
		}
		return null; 
//		else{
//			return this.errorMessage;
//		}

	}


	/**
	 * The function getMapProjectionString() retrieves the map projection string,
	 * only if the boolean flag graphics_area_str_valid is true and if the flag 'iartyp' is set to 3
	 * (indicating that a geog code was entered)
	 * @return map_proj_str
	 */

	public String getMapProjectionString(){
		if(this.iartyp == 3 && this.isGraphicsAreaStrValid == true){
			return this.mapProjectionStr.trim();
		}
		return null; 
//		else{
//			return this.errorMessage;
//		}	
	}

	/**
	 * The function getStationCode() returns the value of the station code, which is  retrieved 
	 * only if the boolean flag graphics_area_str_valid is true and if the flag 'iartyp' is set to 4
	 * (indicating that a station code was entered)
	 * @return station_code
	 */

	public String getStationCode(){
		if(this.iartyp == 4 && this.isGraphicsAreaStrValid == true){
			return this.station_code;	
		}

		else{
			return this.errorMessage;
		}

	}
	/**
	 * The function isGraphicsAreaStringValid() returns the boolean data 'graphics_area_str_valid'.
	 * @return graphics_area_str_valid
	 */

	public boolean isGraphicsAreaStringValid(){
		return this.isGraphicsAreaStrValid;
	}

	/**
	 *  The function getErrorCode() returns the string 'error_message' that contains the error message.
	 * @return error_message
	 */

	public String getErrorCode(){
		return this.errorMessage;
	}

	/**
	 * The function getGAREACoordinates() retrieves the  Lower left/ Upper Right
	 * latitude/longitude values and the Center/Delta latitude/longitude values in a Float[]
	 * data called lat_lon_data. 
	 * @return lat_lon_data
	 */


	public double[] getGAREACoordinates(){

		double[] lat_lon_data = new double[6];
		lat_lon_data[0] = lowerLeftLat;
		lat_lon_data[1] = lowerLeftLon;
		lat_lon_data[2] = upperRightLat;
		lat_lon_data[3] = upperRightLon;
		lat_lon_data[4] = centerLat;
		lat_lon_data[5] = centerLon;

		return lat_lon_data;
	}

	/**
	 *  The private function setLatLonValues() accepts as input a string containing the latitude/longitude values
	 *  and a number that indicates whether the latitude/longitude values represent the Lower left/ Upper Right
	 *  latitude/longitude values or the Center/Delta latitude/longitude values. Depending on the value of
	 *  this number, it the splits the input string into 4 component string, extracts the floating point value
	 *  and stores each value correspondingly. 
	 * @param s
	 * @param i
	 */

//	private void setLatLonValues(String s, int i){
//		String lat_lon_tokens[] = s.split(";");
//		if(i == 1){
//			centerLat = Float.valueOf(lat_lon_tokens[0]).floatValue();
//			centerLon = Float.valueOf(lat_lon_tokens[1]).floatValue();
//			deltaLat  = Float.valueOf(lat_lon_tokens[2]).floatValue();
//			deltaLon  = Float.valueOf(lat_lon_tokens[3]).floatValue();
//		}
//		if(i==2){
//			lowerLeftLat = Float.valueOf(lat_lon_tokens[0]).floatValue();
//			lowerLeftLon = Float.valueOf(lat_lon_tokens[1]).floatValue();
//			upperRightLat = Float.valueOf(lat_lon_tokens[2]).floatValue();
//			upperRightLon = Float.valueOf(lat_lon_tokens[3]).floatValue();    
//		}
//	}
	private void setCenterDeltaLatLonValues(String latLonString){
		String latLonTokens[] = latLonString.split(";");
			centerLat = Double.valueOf(latLonTokens[0]).doubleValue(); 
			centerLon = convertLongitudeValue(Double.valueOf(latLonTokens[1]).doubleValue());
			deltaLat  = Double.valueOf(latLonTokens[2]).doubleValue();
			deltaLon  = convertLongitudeValue(Double.valueOf(latLonTokens[3]).doubleValue());
	}
	
	private void setLowerUpperLatLonValues(String latLonString){
		String latLonTokens[] = latLonString.split(";");
		lowerLeftLat = Double.valueOf(latLonTokens[0]).doubleValue();
		lowerLeftLon = convertLongitudeValue(Double.valueOf(latLonTokens[1]).doubleValue());
		upperRightLat = Double.valueOf(latLonTokens[2]).doubleValue();
		upperRightLon = convertLongitudeValue(Double.valueOf(latLonTokens[3]).doubleValue());    
	}

	/**
	 * The private function computeLLURLatLonFromCDLatLon() calculates the Lower left/ Upper Right
	 * latitude/longitude values from the given Center/Delta latitude/longitude values
	 */

	private void calculateLLURLatLonFromCDLatLon(double centerLat, double centerLon, 
			double deltaLat, double deltaLon){
		lowerLeftLat     = centerLat - deltaLat;
		lowerLeftLon     = centerLon - deltaLon;
		upperRightLat     = centerLat + deltaLat;
		upperRightLon     = centerLon + deltaLon;	
	}

}


