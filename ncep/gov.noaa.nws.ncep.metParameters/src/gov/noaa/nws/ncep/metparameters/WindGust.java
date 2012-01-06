package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class WindGust extends AbstractMetParameter implements
							javax.measure.quantity.Velocity {

	public WindGust() {
		super( UNIT );		
	}
	
//	@DeriveMethod
//	public AbstractMetParameter getWindSpeedFromGustBarb( GustBarb gb ) {
//		return gb.getWindGust();
//	}
}
