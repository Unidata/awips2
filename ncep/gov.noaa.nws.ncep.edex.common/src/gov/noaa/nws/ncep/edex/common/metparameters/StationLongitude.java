package gov.noaa.nws.ncep.edex.common.metparameters;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SLON
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class StationLongitude extends AbstractMetParameter 
 						implements javax.measure.quantity.Angle, Serializable {

     /**
	 * 
	 */
	private static final long serialVersionUID = -6858325572623824100L;

	public StationLongitude() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}	
	
// 	@DeriveMethod
//     AbstractMetParameter derive( StationID sid ) throws InvalidValueException, NullPointerException {
//		// TODO : look up the long from the station name.
//		//    	 this.setValue(val);
//        StringBuilder query = new StringBuilder("select longitude from ");
//    	query.append("sfcobs ");
//    	query.append("where stationid = '");
//    	query.append(sid.getStringValue());
//    	query.append("'; ");
//    	try {
//			List<Object[]> results = DirectDbQuery.executeQuery(query.toString(), "metadata", QueryLanguage.SQL);
//			if(results != null && results.size() > 0){
//				for( Object[] theObjectArray : results ){
//					      Double longitude = ( Double ) theObjectArray[0];
//						 this.setValue(new Amount ( longitude, NonSI.DEGREE_ANGLE ) ) ;
//				}
//			}
//		} catch (VizException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//    	 return this;
//     }	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 