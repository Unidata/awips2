package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter STID
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



public class StationID extends AbstractMetParameter implements Dimensionless, ISerializableObject {


     /**
	 * 
	 */
	private static final long serialVersionUID = -2649695765720117791L;

	public StationID()  throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
		 setValueIsString();
	}	
	
// 	// NOT Implemented. Just enough to execute and return something
//	@DeriveMethod
//    public AbstractMetParameter getStationIdFromName( StationName stnName ) throws InvalidValueException, NullPointerException {
//    	 this.setStringValue(stnName.getStringValue() );
//    	 return this;
//     }
	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 