package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;
 
public class MountainObscThreshMetIndicator extends
		AbstractMetParameter implements Dimensionless {

	public MountainObscThreshMetIndicator() {
		 super( UNIT );
	}
	
	@DeriveMethod
	AbstractMetParameter derive ( CeilingFromSeaLevel tcms, MountainObscThresh motv) throws InvalidValueException, NullPointerException{
		Amount val = PRLibrary.prMobs(tcms, motv );
		setValue ( val );
		return this;
	}
}
