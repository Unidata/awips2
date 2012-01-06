package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Length;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class TemporaryOrProbabilityCeiling extends AbstractMetParameter implements Length {

	public TemporaryOrProbabilityCeiling() {
		 super( UNIT );
	}
  
}
