package gov.noaa.nws.ncep.viz.customprojection;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;

public class DisplayViewLowerLeftAndUpperRightLongLatValues {
	private static DisplayViewLowerLeftAndUpperRightLongLatValues instance; 
	
	private double lowerLeftLong = Double.NaN; 
	private double lowerLeftLat = Double.NaN; 
	private double upperRightLong = Double.NaN; 
	private double upperRightLat = Double.NaN; 
	
	static {
		instance = new DisplayViewLowerLeftAndUpperRightLongLatValues(); 
	}
	
	private DisplayViewLowerLeftAndUpperRightLongLatValues() { }
	
	public static DisplayViewLowerLeftAndUpperRightLongLatValues getInstance() {
		return instance; 
	}
	
	public void initialization(IExtent extent, 
			IDescriptor descriptor) {
		if(extent != null && descriptor != null) {
        	double viewMinX = extent.getMinX(); 
        	double viewMaxX = extent.getMaxX(); 
        	double viewMinY = extent.getMinY(); 
        	double viewMaxY = extent.getMaxY(); 

        	double[] lowerLeftPixel = { viewMinX, viewMaxY};
        	double[] lowerLeftLongLat = descriptor.pixelToWorld(lowerLeftPixel); 
        	double[] upperRightPixel = { viewMaxX, viewMinY};
        	double[] upperRightLongLat = descriptor.pixelToWorld(upperRightPixel); 
        	
        	lowerLeftLong = lowerLeftLongLat[0]; 
        	lowerLeftLat = lowerLeftLongLat[1]; 
        	upperRightLong = upperRightLongLat[0]; 
        	upperRightLat = upperRightLongLat[1]; 
		}
	}

	public double getLowerLeftLongitude() {
		return lowerLeftLong;
	}

	public double getLowerLeftLatitude() {
		return lowerLeftLat;
	}

	public double getUpperRightLongitude() {
		return upperRightLong;
	}

	public double getUpperRightLatitude() {
		return upperRightLat;
	}

	public void updateLatLonValues(String lowerLeftLongString, String lowerLeftLatString, 
			String upperRightLongString, String upperRightLatString) {
		double llLatitude, llLongitude, urLatitude, urLongitude; 
		
		try {
			llLatitude = Double.parseDouble(lowerLeftLatString); 
			llLongitude = Double.parseDouble(lowerLeftLongString); 
			urLatitude = Double.parseDouble(upperRightLatString); 
			urLongitude = Double.parseDouble(upperRightLongString); 
			
			lowerLeftLong = llLongitude; 
			lowerLeftLat = llLatitude; 
			upperRightLong = urLongitude; 
			upperRightLat = urLatitude; 
		}catch(NumberFormatException nfe) {
			//do nothing
		}
	}
	
//	public void setLowerLeftLong(double lowerLeftLong) {
//		this.lowerLeftLong = lowerLeftLong;
//	}
//
//	public void setLowerLeftLat(double lowerLeftLat) {
//		this.lowerLeftLat = lowerLeftLat;
//	}
//
//	public void setUpperRightLong(double upperRightLong) {
//		this.upperRightLong = upperRightLong;
//	}
//
//	public void setUpperRightLat(double upperRightLat) {
//		this.upperRightLat = upperRightLat;
//	}

}
