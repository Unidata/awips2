package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Duration;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class WindWavePeriod extends AbstractMetParameter implements Duration {

	public WindWavePeriod() {
		 super( UNIT );
	}
	
}
