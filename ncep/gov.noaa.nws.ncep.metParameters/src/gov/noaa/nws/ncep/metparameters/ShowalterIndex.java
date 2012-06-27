package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class ShowalterIndex extends AbstractMetParameter implements
		Dimensionless {

	public ShowalterIndex(){
		 super( UNIT );
	}
	
}
