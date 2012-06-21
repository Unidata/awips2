package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

import gov.noaa.nws.ncep.metParameters.quantity.RateOfChangeInTemperatureWithPressure;

public class StabilityWithRespectToPressure extends AbstractMetParameter
		implements RateOfChangeInTemperatureWithPressure {

	public StabilityWithRespectToPressure(){
		 super( UNIT );
	}
  
}
