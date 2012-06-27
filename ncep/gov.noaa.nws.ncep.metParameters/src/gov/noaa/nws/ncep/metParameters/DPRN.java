package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;


public class DPRN  extends AbstractMetParameter implements javax.measure.quantity.Length {

	public DPRN() {
		super( UNIT );
	}

	@DeriveMethod
	AbstractMetParameter derive( Precip24Hr pt, MaxPrecipPR6X mp){
		if ( pt.hasValidValue() && mp.hasValidValue() ){
		  setValue( pt.doubleValue() > mp.doubleValue() ? 
				  pt  :  mp );
		}else
			setValueToMissing();
		
		return this;
	}
}
