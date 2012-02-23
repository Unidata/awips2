package gov.noaa.nws.ncep.edex.common.metparameters;


import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter WXVF
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


public class FlightRulesIdWorstCase extends AbstractMetParameter
		implements Dimensionless, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -224394111957151881L;

	public FlightRulesIdWorstCase() {
		 super( UNIT );
	}

	 @DeriveMethod
	AbstractMetParameter derive ( FlightRulesID xvfr, ProbableFlightRuleIdentifier txvf) throws InvalidValueException, NullPointerException{
   	 if ( xvfr.hasValidValue() && txvf.hasValidValue() ){
   	     Amount val = PRLibrary.prWxvf( xvfr, txvf );
   	     setValue ( val );
   	 }else
   		 setValueToMissing();
   	 return this;
    }
	
}







