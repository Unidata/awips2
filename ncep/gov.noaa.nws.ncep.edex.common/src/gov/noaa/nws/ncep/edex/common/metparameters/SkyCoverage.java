package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SKYC
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

 public class SkyCoverage extends AbstractMetParameter implements
 Dimensionless, ISerializableObject {

     /**
	 * 
	 */
	private static final long serialVersionUID = 1882201043516341917L;

	public SkyCoverage() {
		 super( UNIT );
		 setValueIsString();
     }	
	
	
//	@DeriveMethod
//	public AbstractMetParameter determineFromCloudCover( CloudCover[] cldCoverList ) {
//		// TODO : what is the default "", "CLR"
//		if( cldCoverList == null || cldCoverList.length == 0 ) {
//			setStringValue("BLNK");
//			return this;
//		}
//		
//		// TODO : Raytheon reads the cloud_select.txt file to determine the 'rankedField'
//		// but here we'll just encode the rules for determining the skyCoverage from the
//		// various cloud coverages.
//		//
//		for( CloudCover cldCov : cldCoverList ) {
//			
//		}
//		
//		setStringValue("CLR");
//		
//		return this;
//	}
	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 