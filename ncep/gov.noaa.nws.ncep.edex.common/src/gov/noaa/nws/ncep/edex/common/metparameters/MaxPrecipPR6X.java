package gov.noaa.nws.ncep.edex.common.metparameters;
import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import java.util.List;

import gov.noaa.nws.ncep.edex.common.metparameters.Precipitation;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Length;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter PR6X
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class MaxPrecipPR6X extends AbstractMetParameter implements Length,
 ISerializableObject{
         
	/**
	 * 
	 */
	private static final long serialVersionUID = -3742141583445322880L;

	public MaxPrecipPR6X() {
		 super( UNIT );
	}
	 
    @DeriveMethod
    AbstractMetParameter derive( Precipitation p00z,Precipitation p06z,Precipitation p12z,Precipitation p18z ) throws InvalidValueException, NullPointerException{
    	Amount val = PRLibrary.prPr6x( p00z, p06z, p12z, p18z );
    	setValue(val);
    	return this;

    }


 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 