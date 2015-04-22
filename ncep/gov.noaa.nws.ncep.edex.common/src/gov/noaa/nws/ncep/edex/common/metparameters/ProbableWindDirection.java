package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirectionUComp;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirectionVComp;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to the GEMPAK parameter TDRC
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 
  public class ProbableWindDirection extends
  AbstractMetParameter implements javax.measure.quantity.Angle, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6401802482888694453L;

	public ProbableWindDirection() {
		 super( UNIT );
	} 
	
 	@DeriveMethod
	AbstractMetParameter derive ( WindDirectionUComp u, WindDirectionVComp v) throws InvalidValueException, NullPointerException{
		if ( u.hasValidValue() && v.hasValidValue() ){
 		     Amount windDrct = PRLibrary.prDrct( u , v );
		     setValue(windDrct);
		}else
			setValueToMissing();
		return this;
	}	
 }
