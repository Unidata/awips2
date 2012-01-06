package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;
 
public class MountainObscThreshMetIndicatorWorstCase extends
		AbstractMetParameter implements Dimensionless {

	public MountainObscThreshMetIndicatorWorstCase() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( CeilingFromSeaLevelWorstCase wcms, MountainObscThresh motv) throws InvalidValueException, NullPointerException{
		if ( wcms.hasValidValue() && motv.hasValidValue() ){
		        Amount val = PRLibrary.prMobs(wcms, motv );
		        setValue ( val );
		}else
			setValueToMissing();
		
		return this;
	}
}










