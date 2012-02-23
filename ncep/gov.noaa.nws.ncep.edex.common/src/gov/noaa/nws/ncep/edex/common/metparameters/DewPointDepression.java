package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.unit.SI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to any of  the GEMPAK parameters DPDC or DPDK or DPDF 
 * depending on the unit used to measure the dewpoint depression.
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class DewPointDepression extends AbstractMetParameter 
implements javax.measure.quantity.Temperature , ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8246047973579792393L;

	public DewPointDepression() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}
	
	@DeriveMethod
	public DewPointDepression derive( AirTemperature t, DewPointTemp d) throws InvalidValueException, NullPointerException {
		if ( t.hasValidValue() &&  d.hasValidValue() ){
			Amount dwdpAmount = PRLibrary.prDdep( t, d );
			if (t.getUnit().equals(SI.KELVIN) && d.getUnit().equals(SI.KELVIN) ) {
				// Dew point depression is the difference in degrees Celsius between the temperature 
				// and the dew point, therefore, need to convert the amount to celcius.
				double dwdp = (Double)dwdpAmount.getValue(); 
			    dwdp = dwdp + 273.15;
			    dwdpAmount = new Amount ( dwdp , t.getUnit()); 					
			}
			this.setValue(dwdpAmount);
		}
		else
              setValueToMissing();
		return this;
    }
	
	// TODO : could change this to pass along the threshhold instead of hardcoding 30.
	@Override
	public String getFormattedString( String formatStr ) {
		
		if( formatStr == null || formatStr.isEmpty() ||
			formatStr.startsWith("%" ) ) {
			return super.getFormattedString( formatStr );
		}
		else if ( ( formatStr.compareToIgnoreCase("DPDX") == 0 ) ){			
			if( getValueAs( SI.CELSIUS ).doubleValue() >= 30.0 ) {
				return "X";
			}
			else {
				return super.getFormattedString( "%3.0f" );
			}
	    }
		else {
			return getValue().toString();
		}
	}
}

