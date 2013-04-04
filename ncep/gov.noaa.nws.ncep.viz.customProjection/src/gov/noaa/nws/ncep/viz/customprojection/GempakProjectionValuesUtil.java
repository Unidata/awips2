package gov.noaa.nws.ncep.viz.customprojection;

import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;

public class GempakProjectionValuesUtil {
	private Logger logger = Logger.getLogger(this.getClass()); 
	
	public final static String MISSING_VALID_PROJECTION_NAME = "A valid projection name is needed";
	
	private final String DEFAULT_PROJECTION_NAME = "NPS"; 
	
	private final int ANGLE_ARRAY_SIZE = 3; 
	/*
	 * This value is mainly for the purpose of displaying error messages
	 */
	private String projectionStringValue; 

	private String gempakProjectionName; 
//	private String [] anglesStringArray; 
	private double [] anglesDoubleArray; 
//	private String angle2; 
//	private String angle3; 
	private boolean isProjectionStringWithoutAngleParameters; 
	private boolean isDefaultProjection; 
	
	private static Map<String, String> gempakAndGeotoolProjectionNameMap; 
    
    static {
    	gempakAndGeotoolProjectionNameMap = initializeProjectionNameMap(); 
    }

    public GempakProjectionValuesUtil(String projectionString) {
    	anglesDoubleArray = initializeAnglesDoubleArray(ANGLE_ARRAY_SIZE); 
    	
    	projectionStringValue = projectionString; 
		if(!isStringEmpty(projectionString)) {
			String []  projectionAngleArray = projectionString.split("/"); 
			if(projectionAngleArray.length == 1)  { 
				isProjectionStringWithoutAngleParameters = true; 
				isDefaultProjection = isDefaultProjectionBeingSpecified(projectionString); 
			}
			setGempakProjectionName(projectionAngleArray[0]); 
			if(projectionAngleArray.length == 2) {
				parseAndSetAngles(projectionAngleArray[1]); 
			}
		} else {
			isProjectionStringWithoutAngleParameters = true; 
			isDefaultProjection = isDefaultProjectionBeingSpecified(projectionString); 
//			setGempakProjectionName(DEFAULT_PROJECTION_NAME); 
		}
	}
	
	public String getProjectionStringValue() {
		return projectionStringValue;
	}
	
	public String getGempakProjectionName() {
		return gempakProjectionName;
	}
	public void setGempakProjectionName(String _gempakProjectionName) {
		this.gempakProjectionName = _gempakProjectionName;
	}

    public boolean isDefaultProjection() {
		return isDefaultProjection;
	}

    public void setDefaultProjection(boolean _isDefaultProjection) {
		this.isDefaultProjection = _isDefaultProjection;
	}

    /**
     * 
     * @return, an indicator to tell is any angle parameters are entered
     */
    public boolean isProjectionStringWithoutAngleValues() {
    	return isProjectionStringWithoutAngleParameters; 
    }
    
    public void setProjectionStringWithoutAngleValues(boolean _isProjectionStringWithoutAngleParameters) {
    	isProjectionStringWithoutAngleParameters = _isProjectionStringWithoutAngleParameters; 
    }
    
	public double getAngle1() {
		return getAngle(0); 
	}
	public double getAngle2() {
		return getAngle(1); 
	}
	public double getAngle3() {
		return getAngle(2); 
	}

	public double getAngle(int angleIndex) {
		double angleValue = Double.NaN; 
		if(angleIndex < anglesDoubleArray.length) 
			angleValue = anglesDoubleArray[angleIndex]; 
		return  angleValue; 
//		if(isStringArrayEmpty(anglesStringArray) || !(anglesStringArray.length > angleIndex))
//			return Double.NaN; 
//		return parseStringToDoubleValue(anglesStringArray[angleIndex]);
	}
	
	public void setAngle(int angleIndex, double angleValue) {
		if(angleIndex < anglesDoubleArray.length) 
			anglesDoubleArray[angleIndex] = angleValue; 
	}
	
//	public void setAngles(String[] _anglesArray) {
//		this.anglesStringArray = _anglesArray;
//	}
	
//	public double getAngle2() {
//		return parseStringToDoubleValue(angle2);
//	}
//	public void setAngle2(String angle2) {
//		this.angle2 = angle2;
//	}
//	
//	public double getAngle3() {
//		return parseStringToDoubleValue(angle3);
//	}
//	public void setAngle3(String angle3) {
//		this.angle3 = angle3;
//	}
	
    /**
     * a helper method to retrieve GEOTool projection name using GEMPAK projection name
     */
    public static String getGeotoolProjectionName(String gempakProjectionName) {
    	return GempakProjectionValuesUtil.gempakAndGeotoolProjectionNameMap.get(gempakProjectionName.toUpperCase()); 
    }
    
    /**
     * a helper method to check if the projection string is valid
     */
    public boolean isGempakProjectionStringValid() {
    	boolean isValid = false; 
    	if(isGempakCylindricalProjection() && isAllAngleValid(2))
    		isValid = true; 
    	else if(isGempakAzmProjection() && isAllAngleValid(2))
    		isValid = true; 
    	else if(isGempakConProjection() && isAllAngleValid(3))
    		isValid = true; 
    	else if(isDefaultProjectionBeingSpecified())
    		isValid = true; 
    	
    	return isValid; 
    }
    
    /**
     * @return true if the projection name is specified as "DEF"
     */
    private boolean isDefaultProjectionBeingSpecified() {
    	return isDefaultProjectionBeingSpecified(gempakProjectionName); 
    }
    
    /**
     * @return true if the projection name is specified as "DEF"
     */
    private boolean isDefaultProjectionBeingSpecified(String projectionNameString) {
    	boolean isValid = false; 
    	if(isStringEmpty(projectionNameString) || ("DEF".equalsIgnoreCase(projectionNameString))) {
    			isValid = true; 
    			setGempakProjectionName(DEFAULT_PROJECTION_NAME); 
    	}
    	return isValid; 
    }
    
    /**
     * a helper method to check if the current projection belongs to Cylindrical
     */
    public boolean isGempakCylindricalProjection() {
    	return isGempakCylindricalProjection(gempakProjectionName); 
    }
    
    /**
     * a helper method to check if the current projection belongs to Cylindrical
     */
    public boolean isGempakCylindricalProjection(String gempakProjectionName) {
    	boolean validFlag = false; 
    	if(!isStringEmpty(gempakProjectionName) && ("CED".equalsIgnoreCase(gempakProjectionName) || 
    			"MER".equalsIgnoreCase(gempakProjectionName) || 
    			"CED".equalsIgnoreCase(gempakProjectionName) || 
    			"MCD".equalsIgnoreCase(gempakProjectionName) || 
    			"UTM".equalsIgnoreCase(gempakProjectionName)))// || 
//    			"TVM".equalsIgnoreCase(gempakProjectionName)))
    		validFlag = true;
    	else if(isAngleParametersRequiredWithProjection(gempakProjectionName) &&
    			!isProjectionStringWithoutAngleParameters) {
    		if(isAllAngleValid(3))
    			validFlag = true; 
    	}
    	return validFlag; 
    }
    
    /**
     * a helper method to check if the current projection belongs to AZM
     */
    public boolean isGempakAzmProjection() {
    	return isGempakAzmProjection(gempakProjectionName); 
    }
    
    /**
     * a helper method to check if the current projection belongs to AZM
     */
    public boolean isGempakAzmProjection(String gempakProjectionName) {
    	boolean validFlag = false; 
    	if(!isStringEmpty(gempakProjectionName) && ( 
    			"NPS".equalsIgnoreCase(gempakProjectionName) ||
    			"SPS".equalsIgnoreCase(gempakProjectionName) ||
    			"ORT".equalsIgnoreCase(gempakProjectionName) ||
    			"NOR".equalsIgnoreCase(gempakProjectionName) ||
    			"SOR".equalsIgnoreCase(gempakProjectionName)))
    		validFlag = true;
    	/*
    	 * force STR projection comes with angle values, otherwise
    	 * consider the projection string is invalid
    	 */
    	else if(isAngleParametersRequiredWithProjection(gempakProjectionName) && 
    			!isProjectionStringWithoutAngleParameters) {
    		if(isAllAngleValid(3))
    			validFlag = true; 
    	}
    	return validFlag; 
    }
    
    private boolean isAngleParametersRequiredWithProjection(String gempakAzmProjectionName) {
    	boolean isRequired = false; 
    	if("STR".equalsIgnoreCase(gempakProjectionName) ||
    			"AED".equalsIgnoreCase(gempakProjectionName) ||
    			"LEA".equalsIgnoreCase(gempakProjectionName) ||
    			"TVM".equalsIgnoreCase(gempakProjectionName))
    		isRequired = true; 
    	return isRequired; 
    }
    
    /**
     * a helper method to check if the current projection belongs to CON
     */
    public boolean isGempakConProjection() {
    	return isGempakConProjection(gempakProjectionName); 
    }
    
    /**
     * a helper method to check if the current projection belongs to CON
     */
    public boolean isGempakConProjection(String gempakProjectionName) {
    	boolean validFlag = false; 
    	if(!isStringEmpty(gempakProjectionName) && ("LCC".equalsIgnoreCase(gempakProjectionName) ||
    			"SCC".equalsIgnoreCase(gempakProjectionName)))
//    	if(!isStringEmpty(gempakProjectionName) && ("LCC".equalsIgnoreCase(gempakProjectionName)))
    		validFlag = true;
    	return validFlag; 
    }
    
	public static String getModifiedGempakProjectionName(String originalGempakProjectName, 
			double angle1) {
		String modifiedGempakProjectName = buildGempakProjectionName(originalGempakProjectName, angle1); 
		return modifiedGempakProjectName;
	}
	
	public static boolean isCylindricalProjectionForGeotools(String geotoolsProjectionName) {
    	boolean isCylindrical = false; 
    	if(!StringUtil.isStringEmpty(geotoolsProjectionName)) {
    		if("Equidistant_Cylindrical".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Mercator_1SP".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Mercator_2SP".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Hotine_Oblique_Mercator".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Hotine_Oblique_Mercator_Two_Point_Center".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Hotine_Oblique_Mercator_Two_Point_Natural_Origin".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Transverse_Mercator".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Transverse Mercator (South Orientated)".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Oblique_Mercator".equalsIgnoreCase(geotoolsProjectionName) 
    				)
    			isCylindrical = true; 
    	}
    	return isCylindrical; 
    }
    
	public static boolean isAzmProjectionForGeotools(String geotoolsProjectionName) {
    	boolean isAzm = false; 
    	if(!StringUtil.isStringEmpty(geotoolsProjectionName)) {
    		if("Polar_Stereographic".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Orthographic".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Oblique_Stereographic".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Polar Stereographic (variant B)".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Stereographic".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Stereographic_North_Pole".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Stereographic_South_Pole".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Lambert_Azimuthal_Equal_Area".equalsIgnoreCase(geotoolsProjectionName))
    			isAzm = true; 
    	}
    	return isAzm; 
    }
    
	public static boolean isConProjectionForGeotools(String geotoolsProjectionName) {
    	boolean isCon = false; 
    	if(!StringUtil.isStringEmpty(geotoolsProjectionName)) {
    		if("Lambert_Conformal_Conic_2SP".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Lambert_Conformal_Conic".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Lambert_Conformal_Conic_1SP".equalsIgnoreCase(geotoolsProjectionName) ||
    				"Lambert_Conformal_Conic_2SP_Belgium".equalsIgnoreCase(geotoolsProjectionName) //||
//    				"Mercator_2SP".equalsIgnoreCase(geotoolsProjectionName) 
    				)
    			isCon = true; 
    	}
    	return isCon; 
    }
    
	private static String buildGempakProjectionName(String gampakProjectionName, double angle1) {
		String modifiedProjectionName = gampakProjectionName; 
		if("STR".equalsIgnoreCase(gampakProjectionName) && 
				!Double.isNaN(angle1) && angle1 != 90.0 && angle1 != -90) {
			modifiedProjectionName = modifiedProjectionName + "_OBLIQUE"; 
		}
		return modifiedProjectionName; 
	}

   
    
    /**
     * a helper method to check if all input angles are valid double values
     */
    private boolean isAllAngleValid(int angleSize) {
    	boolean validFlag = true; 
    	
    	if(!isProjectionStringWithoutAngleParameters) {
    		for(int i=0; i<angleSize; i++) {
    			if(Double.isNaN(getAngle(i))) {
    				validFlag = false; 	
    				break; 
    			}
        	}
    	}
    		
    	return validFlag; 
    }
    
    /**
     * 
     * @return
     */
    private double[] initializeAnglesDoubleArray(int arraySize) {
    	double [] doubleArray = new double[arraySize];  
    	for(int i=0; i < arraySize; i++) {
    		doubleArray[i] = Double.NaN; 
    	}
    	return doubleArray; 
    }
    
    /*
     * a helper method to check if a string array is empty
     */
//    private boolean isStringArrayEmpty(String[] strArray) {
//    	boolean isEmpty = false; 
//    	if(strArray == null || strArray.length == 0)
//    		isEmpty = true; 
//    	return isEmpty; 
//    }
    
    /**
     * a static helper method to initialize the projectionNamemap
     */
    public static Map<String, String> initializeProjectionNameMap() {
    	Map<String, String> projectNameMap = new HashMap<String, String>(10); 
    	//======Default Projection=======//
    	projectNameMap.put("DEF", "Mercator_2SP"); 

    	//======Cylindrical Projection=======//
    	projectNameMap.put("CED", "Equidistant_Cylindrical"); 
    	projectNameMap.put("MCD", "Equidistant_Cylindrical"); 
    	
    	projectNameMap.put("MER", "Mercator_2SP"); 

    	projectNameMap.put("UTM", "Transverse_Mercator"); 
    	projectNameMap.put("TVM", "Transverse_Mercator"); 
    	
    	//======AZM Projection=======//
    	projectNameMap.put("STR_OBLIQUE", "Oblique_Stereographic"); 
    	projectNameMap.put("STR", "Polar_Stereographic"); 
    	projectNameMap.put("SPS", "Stereographic_South_Pole"); 
    	projectNameMap.put("NPS", "Stereographic_North_Pole"); 
   	
    	projectNameMap.put("ORT", "Orthographic"); 
    	projectNameMap.put("NOR", "Orthographic"); 
    	projectNameMap.put("SOR", "Orthographic"); 
    	
    	projectNameMap.put("LEA", "Lambert_Azimuthal_Equal_Area"); 
    	projectNameMap.put("AED", "Lambert_Azimuthal_Equal_Area"); 
    	
    	//======CON Projection=======//
    	projectNameMap.put("LCC", "Lambert_Conformal_Conic_2SP"); 
    	projectNameMap.put("SCC", "Lambert_Conformal_Conic_2SP"); 
    	
    	return projectNameMap; 
    }

    private double parseStringToDoubleValue(String str) {
        double value = Double.NaN;
        if(!isStringEmpty(str))  {
            try {
                value = Double.parseDouble(str);
            } catch (NumberFormatException e1) {
                // do nothing
            }
        }
		return value;
	}

	private void parseAndSetAngles(String angleString) {
		if(!isStringEmpty(angleString)) {
			String [] angleStrArray = angleString.split(";"); 
//			setAngles(angleStrArray); 
			
			int index = 0; 
			for(String currentString : angleStrArray) {
				if(index < anglesDoubleArray.length) {
					anglesDoubleArray[index++] = parseStringToDoubleValue(currentString); 
				}
			}
		}
	}
	
	private boolean isStringEmpty(String str) {
		boolean valid = true; 
		if(str != null && str.trim().length() != 0) 
			valid = false; 
		return valid; 
	}
}
