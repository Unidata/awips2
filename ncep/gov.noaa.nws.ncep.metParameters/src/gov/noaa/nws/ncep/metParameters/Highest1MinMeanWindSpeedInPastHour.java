package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Velocity;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

  public class Highest1MinMeanWindSpeedInPastHour extends AbstractMetParameter
		implements Velocity {

	public Highest1MinMeanWindSpeedInPastHour() {
		super( UNIT );
	}
	
  }
