package gov.noaa.nws.ncep.edex.common.metparameters;

import  gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
/**
 * Maps to the GEMPAK parameter LHVP
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class LatentHeatOfVapor extends AbstractMetParameter implements
javax.measure.quantity.Energy, ISerializableObject{
	/**
	 * 
	 */
	private static final long serialVersionUID = -249479667379624821L;

	public LatentHeatOfVapor() {
		super( UNIT );
	}

	@DeriveMethod		
	public LatentHeatOfVapor derive(  AirTemperature t ) throws InvalidValueException, NullPointerException  {
		if ( t.hasValidValue() ){
		   Amount val = PRLibrary.prLhvp( t );
		   setValue(val);
		}else
			setValueToMissing();
		return this;
	}	

}
