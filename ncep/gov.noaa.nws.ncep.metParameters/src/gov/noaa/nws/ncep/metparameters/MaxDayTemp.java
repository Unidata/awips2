package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class MaxDayTemp extends AbstractMetParameter implements
		Temperature {

	public MaxDayTemp(){
		super( UNIT );
	}
}
