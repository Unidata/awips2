package gov.noaa.nws.ncep.metparameters;


// This parameter is intended to be used as a Vertical Coordinate. 
// 

import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class PressureLevel extends AbstractMetParameter implements 
javax.measure.quantity.Pressure {
	public PressureLevel() {
		super( UNIT );
	}
}

