package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;
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
		 super( UNIT );
	}
	
	@DeriveMethod
	public DewPointDepression derive( AirTemperature t, DewPointTemp d) throws Exception {
		if ( t.hasValidValue() &&  d.hasValidValue() ){
			String unitStrNeeded = getUnitStr();
			UnitAdapter ua = new UnitAdapter();
			Unit<?> unit = ua.unmarshal(unitStrNeeded);

			Amount tempAmount = new Amount(t.getValueAs( unit ), unit );
			Amount dewPointAmount = new Amount(d.getValueAs( unit ), unit );
			
			
            Amount dwdpFinal = new Amount(tempAmount.doubleValue() - dewPointAmount.doubleValue(),unit);    
            setValue(dwdpFinal );
            setUnit(unit);
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

