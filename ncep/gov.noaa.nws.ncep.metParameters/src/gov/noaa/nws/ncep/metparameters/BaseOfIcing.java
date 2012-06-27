package gov.noaa.nws.ncep.metparameters;


import javax.measure.quantity.Length;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

  public class BaseOfIcing extends AbstractMetParameter
		implements Length {

	public BaseOfIcing() {
		super( UNIT );
	}	
  }