package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Length;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class SeaIceDriftDist extends AbstractMetParameter
		implements Length {

    public SeaIceDriftDist(){
		 super( UNIT );
    }
	
}

