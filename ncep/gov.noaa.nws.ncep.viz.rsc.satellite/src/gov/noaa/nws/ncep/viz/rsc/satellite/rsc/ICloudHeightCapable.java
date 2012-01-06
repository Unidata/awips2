package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;
 
import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

import com.vividsolutions.jts.geom.Coordinate;

public interface ICloudHeightCapable {

	//	abstract public ArrayList<CloudHeightData> getTemperatures( );
	
	// Is this satellite image an IR image?
	abstract public boolean isCloudHeightCompatible();
	
	abstract public Double  getRawIRImageValue( Coordinate latlon );
	
	// this will return the Temperature in the Units returned from getDisplayUnits.
	//
	abstract public Double getSatIRTemperature( Coordinate latlon );
	
	abstract public Unit<Temperature> getTemperatureUnits();
}