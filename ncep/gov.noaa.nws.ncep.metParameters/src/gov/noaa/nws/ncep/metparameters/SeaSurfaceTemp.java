package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class SeaSurfaceTemp extends AbstractMetParameter implements
		Temperature {

	public SeaSurfaceTemp() {
		 super( UNIT );
	}  
	
 }
