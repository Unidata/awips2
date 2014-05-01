package gov.noaa.nws.ncep.edex.common.metparameters;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.HashMap;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

 
/**
 * Maps to the GEMPAK parameter DRCT
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class WindDirection extends AbstractMetParameter implements javax.measure.quantity.Angle, ISerializableObject {

	static HashMap<String,Number> newsMap = null;	

	/**
	 * 
	 */
	@DynamicSerializeElement
	private static final long serialVersionUID = -3145116266557037286L;

	public WindDirection() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	} 
	
	// Handle east/west/NE......
	//
	@Override
	public void setValueFromString( String valStr, Unit<?> u ) {
		if( hasStringValue() ) {
			setStringValue( valStr );
		}
		else {			
			if( newsMap == null ) {
				newsMap = new HashMap<String,Number>();
				newsMap.put( "NNE", 22.5 );
				newsMap.put( "NE", 45.0 );
				newsMap.put( "ENE", 67.5 );
				newsMap.put( "E", 90.0 );
				newsMap.put( "ESE", 112.5 );
				newsMap.put( "SE", 135.0 );
				newsMap.put( "SSE", 157.5 );
				newsMap.put( "S", 180.0 );
				newsMap.put( "SSW", 202.5 );
				newsMap.put( "SW", 225.0 );
				newsMap.put( "WSW", 247.5 );
				newsMap.put( "W", 270.0 );
				newsMap.put( "WNW", 292.5 );
				newsMap.put( "NW", 315.0 );
				newsMap.put( "NNW", 337.5 );
				newsMap.put( "N", 360.0 );

				newsMap.put( "NORTHEAST", 45.0 );
				newsMap.put( "EAST", 90.0 );
				newsMap.put( "SOUTHEAST", 135.0 );
				newsMap.put( "SOUTH", 180.0 );
				newsMap.put( "SOUTHWEST", 225.0 );
				newsMap.put( "WEST", 270.0 );
				newsMap.put( "NORTHWEST", 315.0 );
				newsMap.put( "NORTH", 360.0 );				
			}
			
			if( newsMap.containsKey( valStr.toUpperCase() ) ) {
				setValue( newsMap.get( valStr.toUpperCase() ),NonSI.DEGREE_ANGLE );
			}
			else {
				super.setValueFromString(valStr, NonSI.DEGREE_ANGLE );
			}
		}
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
