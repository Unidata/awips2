package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Length;
import javax.measure.unit.Unit;
 
public class CeilingFromSeaLevelWorstCase extends AbstractMetParameter implements Length {

	public CeilingFromSeaLevelWorstCase(){
		 super( UNIT );
	}

	@DeriveMethod
	 AbstractMetParameter derive  ( CeilingFromSeaLevel  cmsl, TemporaryCeilingAsMeanSeaLevel tcms ){
		if ( cmsl.hasValidValue() ){
		   Amount val = ( !tcms.hasValidValue()   ? cmsl : tcms  ); //prWcms
		   setValue( val );
		}
		else
			setValueToMissing();
		return this;
	}
}
