package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class LiftedIndex extends AbstractMetParameter implements
		Dimensionless {

	public LiftedIndex(){
		super( UNIT );
	}	
}