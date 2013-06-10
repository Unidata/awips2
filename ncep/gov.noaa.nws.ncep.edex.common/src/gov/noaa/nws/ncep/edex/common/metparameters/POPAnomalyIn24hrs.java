/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters;



import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;
import javax.measure.unit.NonSI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter PP2A
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class POPAnomalyIn24hrs extends AbstractMetParameter implements
		Dimensionless, ISerializableObject {

	 /**
	 * 
	 */
	private static final long serialVersionUID = 8690261205610416205L;

	public POPAnomalyIn24hrs() {
			super( UNIT );
	}	 

	@DeriveMethod
	 public POPAnomalyIn24hrs derive( Clim24HrPOP clim24hrpop,POP24Hrs pop24hrs){
		     if (pop24hrs == null || clim24hrpop == null 
		    		 || (!pop24hrs.hasValidValue())
		    	     || (!clim24hrpop.hasValidValue())){
		    	 setUnit(NonSI.PERCENT); 
		    	 return this;
		     }
		    
		    double pp2c = clim24hrpop.getValueAs("%").doubleValue();
		    double pp24 = pop24hrs.getValueAs("%").doubleValue();
		    double pp2a = pp24 - pp2c;
		    setValueAs(pp2a, "%");
		     
		    return this;
	 }
	
 }

