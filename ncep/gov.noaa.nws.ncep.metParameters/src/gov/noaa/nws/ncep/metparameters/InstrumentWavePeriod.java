package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Duration;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class InstrumentWavePeriod extends AbstractMetParameter implements Duration {

	public InstrumentWavePeriod() {
		 super( UNIT );
	}
	
 }
