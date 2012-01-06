package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

 
 public class TempLapseRate extends AbstractMetParameter implements
		gov.noaa.nws.ncep.metParameters.quantity.RateOfChangeInTemperatureWithHeight {

     public TempLapseRate(){
		 super( UNIT );
     }
	
 }
