package gov.noaa.nws.ncep.metparameters;


import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

 public class WindDirectionVComp extends AbstractMetParameter implements javax.measure.quantity.Velocity{

	public WindDirectionVComp(){
		 super( UNIT );
	}

	@DeriveMethod
	AbstractMetParameter derive( WindSpeed w, WindDirection d ) throws InvalidValueException, NullPointerException{
		 if ( w.hasValidValue() && d.hasValidValue()){
		       Amount vWnd  = PRLibrary.prVwnd( w, d );
		       setValue( vWnd );
		 }else
			 setValueToMissing();
		return this;
	}
}
