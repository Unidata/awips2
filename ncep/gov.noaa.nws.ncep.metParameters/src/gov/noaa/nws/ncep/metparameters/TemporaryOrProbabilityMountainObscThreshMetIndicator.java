package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;

 
public class TemporaryOrProbabilityMountainObscThreshMetIndicator extends
		AbstractMetParameter implements Dimensionless {

	public TemporaryOrProbabilityMountainObscThreshMetIndicator() {
		 super( UNIT );
	}
	
 	@DeriveMethod
	AbstractMetParameter derive ( TemporaryCeilingAsMeanSeaLevel tcms, MountainObscThresh motv) throws InvalidValueException, NullPointerException{
     if( tcms.hasValidValue() && motv.hasValidValue() ){
 		   Amount val = PRLibrary.prMobs(tcms, motv );
		   setValue ( val );
     }else
    	 setValueToMissing();
		return this;
	}
}
