package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Length;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

// TODO : HET
public class HeightAboveSeaLevel extends AbstractMetParameter
		implements Length {

	@Override
	public String getFormattedString( String formatStr ) {
		if( formatStr == null || formatStr.isEmpty() ||
			formatStr.startsWith("%" ) ) {
			return super.getFormattedString( formatStr );
		}
		else if( !formatStr.equals("STDZ") ) {
			return super.getFormattedString( formatStr );
		}
		
		Number valInMeters = getValueAs( SI.METER );
		
		// TODO : add code to create the abbreviated string.
		
		
		return valInMeters.toString();		
	}
	 
	public HeightAboveSeaLevel() {
    	super( UNIT );
	}
}
