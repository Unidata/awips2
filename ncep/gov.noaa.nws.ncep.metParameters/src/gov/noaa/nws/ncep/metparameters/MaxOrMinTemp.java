package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class MaxOrMinTemp extends AbstractMetParameter implements 
 Temperature {

	public MaxOrMinTemp(){
		 super( UNIT );
	}
	
 }
