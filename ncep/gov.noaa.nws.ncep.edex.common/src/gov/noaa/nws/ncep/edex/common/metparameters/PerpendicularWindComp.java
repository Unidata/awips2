package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import com.raytheon.uf.common.serialization.ISerializableObject;

import gov.noaa.nws.ncep.edex.common.metparameters.WindCompDirection;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirection;
import gov.noaa.nws.ncep.edex.common.metparameters.WindSpeed;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
 
public class PerpendicularWindComp extends AbstractMetParameter implements
javax.measure.quantity.Angle, ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -166841346863071797L;

	public PerpendicularWindComp() {
		 super( UNIT );
	} 
	
	@DeriveMethod
	AbstractMetParameter derive ( WindDirection wd, WindSpeed ws,WindCompDirection d) throws InvalidValueException, NullPointerException {
		Amount windDrct = PRLibrary.prWnml( wd , ws , d );
		this.setValue(windDrct);
		return this;
	}
}
