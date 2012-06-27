package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Angle;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class ShipCourse extends
		AbstractMetParameter implements Angle {

	public ShipCourse() {
		 super( UNIT );
	}
}
