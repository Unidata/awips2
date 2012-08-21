package gov.noaa.nws.ncep.metparameters;


import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class PotentialTempLapseRate extends AbstractMetParameter implements
		gov.noaa.nws.ncep.metParameters.quantity.RateOfChangeInTemperatureWithHeight {

     public PotentialTempLapseRate(){
		 super( UNIT );
     }
	
 }