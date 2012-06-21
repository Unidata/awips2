package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Duration;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class WavePeriod extends AbstractMetParameter implements Duration {

	public WavePeriod() {
		 super( UNIT );
	}
	
 }
