package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
import javax.measure.quantity.Length;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter TCMS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class ProbableCeilingAsMeanSeaLevel extends AbstractMetParameter
implements Length, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8944843816644953699L;

	public ProbableCeilingAsMeanSeaLevel() {
		 super( UNIT );
	}
	
      @DeriveMethod
	  AbstractMetParameter derive ( ProbableCeiling c, StationElevation se) throws InvalidValueException, NullPointerException{
    	 if ( c.hasValidValue() && se.hasValidValue() ){
    	     Amount val = PRLibrary.prCmsl( c, se );
    	     setValue ( val );
    	 }else
    		 setValueToMissing();
    	 return this;
     }
}















