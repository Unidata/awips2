package gov.noaa.nws.ncep.metparameters;

import java.util.List;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.metparameters.Amount;


 public class StationLongitude extends AbstractMetParameter 
 						implements javax.measure.quantity.Angle {

     public StationLongitude() {
		 super( UNIT );
	}	
	
// 	@DeriveMethod
//     AbstractMetParameter derive( StationID sid ) throws InvalidValueException, NullPointerException, InvalidRangeException{
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

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 