package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class WindCompDirection extends AbstractMetParameter implements javax.measure.quantity.Angle {

	public WindCompDirection() {
		 super( UNIT );
	}
	
}
