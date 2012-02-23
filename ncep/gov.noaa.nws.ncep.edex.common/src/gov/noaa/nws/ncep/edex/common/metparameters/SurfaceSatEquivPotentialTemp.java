package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
/**
 * Maps to the GEMPAK parameter STHS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class SurfaceSatEquivPotentialTemp extends AbstractMetParameter implements
 javax.measure.quantity.Temperature, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1998586274867642327L;

	public SurfaceSatEquivPotentialTemp() {
		 super( UNIT );
	}

 	@DeriveMethod
	public SurfaceSatEquivPotentialTemp derive( SurfacePressure p, AirTemperature t, AirTemperature t2 ) throws InvalidValueException, NullPointerException  {
        if ( p.hasValidValue() && t.hasValidValue() && t2.hasValidValue() ){
 		      Amount theEquivPotTempAmount = PRLibrary.prThte( p, t, t2 );
              setValue(theEquivPotTempAmount);
        }else
        	setValueToMissing();
        
        return this;
	}

}

