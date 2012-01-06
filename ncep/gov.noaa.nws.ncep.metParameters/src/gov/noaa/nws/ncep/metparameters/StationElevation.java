package gov.noaa.nws.ncep.metparameters;

import java.util.List;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;
 
 public class StationElevation extends AbstractMetParameter implements javax.measure.quantity.Length {

     public StationElevation() {
		 super( UNIT );
	}	
	
//  	// TODO : not implemented, hard code to test derived parameters
//  	@DeriveMethod
//     AbstractMetParameter derive( StationID sid ) throws InvalidValueException, NullPointerException, InvalidRangeException{
//  		if( sid.hasValidValue() ) {
//  			
//  			
//  		}
//  		
//  		this.setValue( 10, SI.METER );
//  		
//  		return this;
//     }

     // TODO : check that this is stationPressure is correct here  
 	@DeriveMethod
     AbstractMetParameter derive( SeaLevelPressure alti, SurfacePressure pres ) throws InvalidValueException, NullPointerException, InvalidRangeException{
    	  if ( alti.hasValidValue() && pres.hasValidValue() ){
 		       Amount val = PRLibrary.prZalt(alti, pres );
    	       setValue(val);
    	  }else
    		  setValueToMissing();
    	 return this;
     }

 	@DeriveMethod
  AbstractMetParameter derive( StationID sid ) throws InvalidValueException, NullPointerException, InvalidRangeException{
		// TODO : look up the lat from the station name.
		//    	 this.setValue(val);

     StringBuilder query = new StringBuilder("select elevation from ");
 	query.append("obs ");
 	query.append("where stationid = '");
 	query.append(sid.getStringValue());
 	query.append("' ");
 	query.append(" and reporttype = 'METAR';");
 	try {
			List<Object[]> results = DirectDbQuery.executeQuery(query.toString(), "metadata", QueryLanguage.SQL);
			if(results != null && results.size() > 0){
				
				Object[] theObjectArray = results.get(0);
 		        Integer selv = ( Integer ) theObjectArray[0];
				setValue(new Amount ( selv, SI.METER ) ) ;

			}
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
 	return this;
  }
 	
 	
 }

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 