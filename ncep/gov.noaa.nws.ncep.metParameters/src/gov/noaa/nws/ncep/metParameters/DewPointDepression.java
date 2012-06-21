package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.SI;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

public class DewPointDepression extends AbstractMetParameter implements javax.measure.quantity.Temperature  {
	public DewPointDepression() {
		super( UNIT );
	}
	
	@DeriveMethod
	public DewPointDepression derive( AirTemperature t, DewPointTemp d) throws InvalidValueException, NullPointerException {
		if ( t.hasValidValue() &&  d.hasValidValue() ){
			Amount dwdpAmount = PRLibrary.prDdep( t, d );
			this.setValue(  dwdpAmount);
		}
		else
              setValueToMissing();
		return this;
    }
	
	// TODO : could change this to pass along the threshhold instead of hardcoding 30.
	@Override
	public String getFormattedString( String formatStr ) {
		
		if( formatStr == null || formatStr.isEmpty() ||
			formatStr.startsWith("%" ) ) {
			return super.getFormattedString( formatStr );
		}
		else if ( ( formatStr.compareToIgnoreCase("DPDX") == 0 ) ){			
			if( getValueAs( SI.CELSIUS ).doubleValue() >= 30.0 ) {
				return "X";
			}
			else {
				return super.getFormattedString( "%3.0f" );
			}
	    }
		else {
			return getValue().toString();
		}
	}
}

