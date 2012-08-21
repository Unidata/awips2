package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Velocity;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class Avg3HrShipSpeed extends AbstractMetParameter implements Velocity{

	 public Avg3HrShipSpeed(){
		 super( UNIT );
	 }
	
	 @Override
	 public String getParameterDescription( ) {
		 return "Average Ship Speed for the last 3 Hours";
	 }
}
