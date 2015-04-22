package gov.noaa.nws.ncep.edex.plugin.aww.util;

public class LatLonInfo {
	String latStringInDegreeFormat; 
	String lonStringInDegreeFormat; 

	double latInDecimal; 
	double lonInDecimal; 
	
	public LatLonInfo(String latStringInDegreeFormat, String lonStringInDegreeFormat) {
		this.latStringInDegreeFormat = latStringInDegreeFormat; 
		this.lonStringInDegreeFormat = lonStringInDegreeFormat; 
		
		this.latInDecimal = convertLatLonStringToDecimal(latStringInDegreeFormat); 
		this.lonInDecimal = convertLatLonStringToDecimal(lonStringInDegreeFormat); 
	}
	
	public LatLonInfo() {} 

	public LatLonInfo(double latInDecimal, double lonInDecimal) {
		this.latInDecimal = latInDecimal; 
		this.lonInDecimal = lonInDecimal; 
	}
	

	public String getLatStringInDegreeFormat() {
		return latStringInDegreeFormat;
	}
	public void setLatStringInDegreeFormat(String latStringInDegreeFormat) {
		this.latStringInDegreeFormat = latStringInDegreeFormat;
		this.latInDecimal = convertLatLonStringToDecimal(latStringInDegreeFormat); 
	}

	public String getLonStringInDegreeFormat() {
		return lonStringInDegreeFormat;
	}
	public void setLonStringInDegreeFormat(String lonStringInDegreeFormat) {
		this.lonStringInDegreeFormat = lonStringInDegreeFormat;
		this.lonInDecimal = convertLatLonStringToDecimal(lonStringInDegreeFormat); 
	}

	
	public double getLatInDecimal() {
		return latInDecimal;
	}

	public double getLonInDecimal() {
		return lonInDecimal;
	}

	/**
	 * the sample String is: "76-40-18.886W"
	 */
	private double convertLatLonStringToDecimal(String latOrLonString) {
		double decimalValue = 0; 
		String [] parsedStringArray = StringUtil.parseString(latOrLonString, "-"); 
		if(parsedStringArray != null) {
			try {
				double decimalPart1 = Double.parseDouble(parsedStringArray[0].trim()); 
				decimalValue += decimalPart1; 
				
				double decimalPart2 = Double.parseDouble(parsedStringArray[1].trim()); 
				decimalValue += decimalPart2/60; 

				int part3Length = parsedStringArray[2].trim().length(); 
				String decimalPart3String = parsedStringArray[2].trim().substring(0, part3Length-1); 
				String latLonDirectionLetter = parsedStringArray[2].trim().substring(part3Length-1); 
				double decimalPart3 = Double.parseDouble(decimalPart3String); 
				decimalValue += decimalPart3/3600; 
				
				/*
				 * Now decide if the latOrLon value should be positive or negative
				 */
				if("W".equalsIgnoreCase(latLonDirectionLetter) ||
						"S".equalsIgnoreCase(latLonDirectionLetter)) {
					decimalValue = 0 - decimalValue; 
				}
			} catch(Exception e) {
				//do nothing 
			}
		}
		return decimalValue; 
	}

}
