package gov.noaa.nws.ncep.metparameters;


import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.management.DescriptorKey;
import javax.measure.quantity.Dimensionless;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

//
public class RelativeHumidity extends AbstractMetParameter implements Dimensionless {

	public RelativeHumidity( ) {
		super( UNIT );
	}

	protected AbstractMetParameter create( Amount val ) {
		RelativeHumidity p = new RelativeHumidity();
		p.setValue( val );
		return p;
	}

	@Override
	public boolean isUnitCompatible( Unit<?> u) {
		return UNIT.isCompatible( u ); 
	}

	@DeriveMethod
	public RelativeHumidity derive( AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException, InvalidRangeException {
     if ( t.hasValidValue() && d.hasValidValue() ){
		   Amount theRelhAmount = PRLibrary.prRelh(t, d);
		    this.setValue(theRelhAmount);
     } else
    	 this.setValueToMissing();
     
     return this;
	}
}

