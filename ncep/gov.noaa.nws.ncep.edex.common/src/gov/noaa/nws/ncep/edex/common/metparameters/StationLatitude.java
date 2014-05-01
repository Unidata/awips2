package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter SLAT
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class StationLatitude extends AbstractMetParameter 
 						implements javax.measure.quantity.Angle, ISerializableObject {

     /**
	 * 
	 */
	private static final long serialVersionUID = -7368446769312114604L;

	public StationLatitude() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}	
	
// 	@DeriveMethod
//     AbstractMetParameter derive( StationID sid ) throws InvalidValueException, NullPointerException {
//		// TODO : look up the lat from the station name.
//		//    	 this.setValue(val);
//
//        StringBuilder query = new StringBuilder("select latitude from ");
//    	query.append("sfcobs ");
//    	query.append("where stationid = '");
//    	query.append(sid.getStringValue());
//    	query.append("'; ");
//    	try {
//			List<Object[]> results = DirectDbQuery.executeQuery(query.toString(), "metadata", QueryLanguage.SQL);
//			if(results != null && results.size() > 0){
//				for( Object[] theObjectArray : results ){
// 					     Double latitude = ( Double ) theObjectArray[0];
//						 this.setValue(new Amount ( latitude, NonSI.DEGREE_ANGLE ) ) ;
//				}
//			}
//		} catch (VizException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//    	return this;
//     }	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 