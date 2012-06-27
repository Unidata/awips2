package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class Min24HrTemp extends AbstractMetParameter implements
							javax.measure.quantity.Temperature {
	public Min24HrTemp() {
		super( UNIT );
	}
}
