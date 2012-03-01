package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;



/**
 * Maps to the GEMPAK parameter PALT
 * This met parameter also be used to represent a StationPressure.
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class SurfacePressure extends AbstractMetParameter implements javax.measure.quantity.Pressure, ISerializableObject {
     /**
	 * 
	 */
	private static final long serialVersionUID = 2544704276202774995L;

	public SurfacePressure() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
     }
	     
     // TODO : check if this is correct.
// 	@DeriveMethod
//	AbstractMetParameter derive( AirTemperature tmpc, PotentialTemp thta) 
// 					throws InvalidValueException, NullPointerException {
//		  if ( tmpc.hasValidValue() && thta.hasValidValue() ){
// 		           Amount val = PRLibrary.prPres(tmpc, thta );
// 		           setValue ( val );
//		  }else
//			  setValueToMissing();
// 		return this;
// 	}

 	@DeriveMethod
 	AbstractMetParameter derive( SeaLevelPressure altm, StationElevation selv )  
 					throws InvalidValueException, NullPointerException  {
 		
 		if( altm.hasValidValue() && selv.hasValidValue() ) {
 			
 			Amount val = PRLibrary.prPalt ( altm, selv );
 			this.setValue( val );
 		}
 		else {
 			this.setValueToMissing();
 		}
 		return this;
 	}
}
