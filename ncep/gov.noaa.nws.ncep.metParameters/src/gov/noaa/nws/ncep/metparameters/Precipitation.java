package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

 public class Precipitation extends AbstractMetParameter implements javax.measure.quantity.Length {

	 public Precipitation() {
		 super( UNIT );
	}
	 	
 }
