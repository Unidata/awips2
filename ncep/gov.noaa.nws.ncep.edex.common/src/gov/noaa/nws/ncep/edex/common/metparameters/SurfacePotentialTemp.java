/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;
import 
gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;


import javax.measure.unit.SI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
/**
 * Maps to one of the GEMPAK parameters STHA/STHK/STHC,
 * depending on the unit in which the surface potential temperature is to be calibrated
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public final class SurfacePotentialTemp extends AbstractMetParameter implements
							javax.measure.quantity.Temperature, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8627976249494965761L;

	public SurfacePotentialTemp() {
		 super( UNIT );
	}

 	@DeriveMethod
	public SurfacePotentialTemp derive( AirTemperature t, SurfacePressure p ) throws   InvalidValueException, NullPointerException {

 		
 		if ( t.hasValidValue() && p.hasValidValue() ){
 		     Amount thePotentialTempAmount = PRLibrary.prThta(t, p);
		     this.setValue(thePotentialTempAmount);
		}else
			setValueToMissing();
		return this;
	}

}