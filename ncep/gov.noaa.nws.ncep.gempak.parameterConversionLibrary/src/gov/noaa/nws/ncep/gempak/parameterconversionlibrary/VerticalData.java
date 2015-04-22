/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameterconversionlibrary;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

/**
 * @author archana
 *
 */
 public class VerticalData {
     /**
	 * @return the temperature
	 */
	public float getTemperature() {
		return temperature;
	}
	/**
	 * @return the thta
	 */
	public float getThta() {
		return thta;
	}
	/**
	 * @return the geoHeight
	 */
	public float getGeoHeight() {
		return geoHeight;
	}
	/**
	 * @return the pressure
	 */
	public float getPressure() {
		return pressure;
	}
	/**
	 * @return the dewpoint
	 */
	public float getDewpoint() {
		return dewpoint;
	}
	private float temperature     = GempakConstants.RMISSD;
     private float thta                = GempakConstants.RMISSD;
     private float geoHeight      = GempakConstants.RMISSD;
     private float pressure         = GempakConstants.RMISSD;
     private float dewpoint       = GempakConstants.RMISSD;
	 public VerticalData( NcSoundingLayer soundingData){
          	      pressure      = soundingData.getPressure();
          	      temperature = soundingData.getTemperature();
          	      thta             = PRLibrary.prThta(temperature, pressure);
          	      geoHeight   = soundingData.getGeoHeight();
          	      dewpoint    = soundingData.getDewpoint();
     }
	 
	 
 }
