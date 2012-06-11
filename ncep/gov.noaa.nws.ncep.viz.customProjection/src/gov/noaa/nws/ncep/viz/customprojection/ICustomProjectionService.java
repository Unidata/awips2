package gov.noaa.nws.ncep.viz.customprojection;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

public interface ICustomProjectionService {
	boolean isGempakProjectionStringValid();
	boolean isGempakGraphicsAreaStringValid(); 
	boolean isProjectionWithoutAngleValues(); 
	boolean isGraphicAreaTextWithValidGeogName(); 
	boolean isGraphicAreaTextWithValidStationName(); 
	
	boolean isDefaultProjectionBeingUsed(); 
	
	boolean isLowerLeftAndUpperRightLatLonValuesValid();
	boolean isCenterDeltaLatLonValuesValid(); 
	
	double getLowerLeftLat();

//	void setLowerLeftLat(double lowerLeftLat);

	double getLowerLeftLon(); 

//	void setLowerLeftLon(double lowerLeftLon); 
	
	double getUpperRightLat(); 
	
//	void setUpperRightLat(double upperRightLat); 

	double getUpperRightLon(); 

//	void setUpperRightLon(double upperRightLon); 

	double getCenterLat(); 
	
	double getCenterLon(); 
	
	void handleValidGeogName(); 
	
	double[] getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon(); 
	
	double[] getGempakAngleValueArrayUsingGempakProjectionValuesObject(); 
	
	String getGempakProjectionName(); 
	
//	String getModifiedGempakProjectionName(String originalGempakProjectName, double angle1); 
//	
//	boolean isCylindricalProjectionForGeotools(String geotoolsProjectionName); 
//	
//	boolean isAzmProjectionForGeotools(String geotoolsProjectionName); 
//	
//	boolean isConProjectionForGeotools(String geotoolsProjectionName); 

	CoordinateReferenceSystem getCoordinateReferenceSystem(); 
}
