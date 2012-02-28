/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;


import javax.measure.quantity.Dimensionless;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
 
/**
 * Maps to the GEMPAK parameter PP2C
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
 public class Clim24HrPOP extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = -3752589406220123063L;

	public Clim24HrPOP() {
	      super( UNIT );
	}
	 
	 @Override
	 public String getParameterDescription( ) {
		 return "Climatological 24 Hour Probability of Precipitation.";
	 }


 }