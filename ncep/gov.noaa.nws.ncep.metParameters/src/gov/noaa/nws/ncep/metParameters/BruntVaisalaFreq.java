package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Frequency;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class BruntVaisalaFreq extends AbstractMetParameter implements Frequency {

	public BruntVaisalaFreq(){
		super( UNIT );
	}
}
