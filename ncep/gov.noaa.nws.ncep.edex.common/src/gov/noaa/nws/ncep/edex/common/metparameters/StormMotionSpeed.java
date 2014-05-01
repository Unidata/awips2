package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter STMS
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



 public class StormMotionSpeed extends AbstractMetParameter implements
							javax.measure.quantity.Velocity, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1910307321507810389L;

	public StormMotionSpeed(){
		 super( UNIT );
	}
	
 	@DeriveMethod
	AbstractMetParameter derive ( EstStormDirectionUComp u, EstStormDirectionVComp v ) throws InvalidValueException, NullPointerException{
		if ( u.hasValidValue() && v.hasValidValue() ){
 		    Amount val = PRLibrary.prSped( u, v );
		    setValue ( val );
		}else
			setValueToMissing();
		return this;
	}
}



