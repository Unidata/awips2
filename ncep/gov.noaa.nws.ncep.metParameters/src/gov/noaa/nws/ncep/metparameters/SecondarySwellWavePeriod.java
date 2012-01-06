package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Duration;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class SecondarySwellWavePeriod extends AbstractMetParameter implements Duration {

	public SecondarySwellWavePeriod() {
		 super( UNIT );
	}
	
}
