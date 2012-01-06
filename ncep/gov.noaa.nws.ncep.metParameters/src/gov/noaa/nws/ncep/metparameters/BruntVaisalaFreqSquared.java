package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;

import javax.measure.quantity.Frequency;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class BruntVaisalaFreqSquared extends AbstractMetParameter implements Frequency {
      	
	public BruntVaisalaFreqSquared(){
		super( UNIT );
	}

//	@Override
//	protected AbstractMetParameter create() {
//		BruntVaisalaFrequencySquared b = new BruntVaisalaFrequencySquared();
//		return b;
//	}

}