package gov.noaa.nws.ncep.metparameters;

import java.util.List;

import javax.measure.unit.NonSI;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import gov.noaa.nws.ncep.metparameters.Amount;

 public class StationLatitude extends AbstractMetParameter 
 						implements javax.measure.quantity.Angle {

     public StationLatitude() {
		 super( UNIT );
	}	
	
// 	@DeriveMethod
//     AbstractMetParameter derive( StationID sid ) throws InvalidValueException, NullPointerException, InvalidRangeException{
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

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 