package gov.noaa.nws.ncep.edex.common.metparameters;


import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;


import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Maps to the GEMPAK parameter RELH
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class RelativeHumidity extends AbstractMetParameter implements Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	@DynamicSerializeElement
	private static final long serialVersionUID = 1580959009469861384L;

	public RelativeHumidity( ) throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}

//	protected AbstractMetParameter create( Amount val ) {
////		RelativeHumidity p ;
//		try {
//			RelativeHumidity	p = new RelativeHumidity();
//			p.setValue( val );
//			return p;
//		} catch (Exception e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		return null;
//	}
//
//	@Override
//	public boolean isUnitCompatible( Unit<?> u) {
//		return UNIT.isCompatible( u ); 
//	}

	@DeriveMethod
	public RelativeHumidity derive( AirTemperature t, DewPointTemp d ) throws InvalidValueException, NullPointerException  {
     if ( t.hasValidValue() && d.hasValidValue() ){
		   Amount theRelhAmount = PRLibrary.prRelh(t, d);
		    this.setValue(theRelhAmount);
     } else
    	 this.setValueToMissing();
     
     return this;
	}
}

