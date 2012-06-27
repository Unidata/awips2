package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Duration;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class TimeOf5SecPeakWindInHrs extends AbstractMetParameter implements Duration {

	public TimeOf5SecPeakWindInHrs(){
		 super( UNIT );
	}
	
}
