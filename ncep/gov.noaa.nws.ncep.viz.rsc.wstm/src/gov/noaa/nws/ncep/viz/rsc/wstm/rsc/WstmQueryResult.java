/**
 * gov.noaa.nws.ncep.viz.rsc.ffa.rsc.WstmQueryResult
 * 
 * Date created October 03, 2011
 *
 * This code is developed by the SIB for use in the AWIPS2 system. 
 */

package gov.noaa.nws.ncep.viz.rsc.wstm.rsc;

import java.util.*;
import java.util.logging.*;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.*;
import com.raytheon.uf.viz.core.catalog.*;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;

/**
 * this class handling database query part for the WstmResource. 
 *  
 *
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011-10-03   456        G. Zhang    Initial creation.
 * 2011-12-21   581        B. Hebbard  Fix attempted cast of BigDecimal to Double (for LatLon) (TTR#319)
 * 2013-01-31   976        Archana       Updated the queryPrefix string to include the 'name' field.
 *                                                              Updated populateFipsMap() to use the 'name' field if the 'shortname' field
 *                                                              is null  
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public class WstmQueryResult {
	
	private static final double ENV_MIN_X = -180.0;
	private static final double ENV_MAX_X = 180.0;
	private static final double ENV_MIN_Y = -90;
	private static final double ENV_MAX_Y = 90.0;	
	
	private static Logger logger = Logger.getLogger("gov.noaa.nws.ncep.viz.rsc.wstm.rsc.WstmQueryResult");	
	
	public String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
							ENV_MIN_X, ENV_MIN_Y, ENV_MAX_X, ENV_MAX_Y);
	
	private StringBuilder query = new StringBuilder();	
	
	private	String queryPrefix = "select AsBinary(the_geom), AsBinary(the_geom_0_001), lat,lon,state_zone, shortname, name from mapdata.zone where ";	
	
	private Map<String,String>		 fipsNameMap   = new HashMap<String,String>();
	private Map<String, LatLonPoint> fipsLatLonMap = new HashMap<String, LatLonPoint>();
	private Map<String, ArrayList<ArrayList<Object[]>>> fipsMultiResultMap = new HashMap<String,ArrayList<ArrayList<Object[]>>>();

	
	
	public WstmQueryResult(){
		
	}
	
	/**
	 * build query part with fips
	 * 
	 * @param aSetOfAwwFips
	 */
	public void buildQueryPart(Set<AwwFips> aSetOfAwwFips){
		
		if( aSetOfAwwFips==null || aSetOfAwwFips.size()==0 )
			return;
		
		String fips;
		for(AwwFips afips : aSetOfAwwFips){
			fips = afips.getFips();
			
			if(fips==null || fips.length()==0)
				continue;
			
			query.append(" ( state_zone = '");
			query.append(fips.substring(0, 2)).append(fips.substring(3));//taking off Z as in PAZ008
			query.append("' ) OR ");			
			
		}
		
	}
	
	/**
	 * query the database then fill the map.
	 */
	public void populateFipsMap(){
		
		if( query==null || query.length()==0 )
			return;
		
		List<Object[]> results = null;
		
		/*
		 * do query
		 */
		try{
			String wholeQuery = queryPrefix + geoConstraint + " AND (" + query.substring(0, query.lastIndexOf("OR")) + " );";
			
			results = DirectDbQuery.executeQuery(wholeQuery, "maps", QueryLanguage.SQL);
		}catch(Exception e){
			logger.log(Level.SEVERE, "_____ Exception with query string or result: "+ e.getMessage());			
			//if query string is wrong or query result is wrong, we stop here and nothing should displayed.
			return;
		}
		
		/*
		 * loop through query result
		 */
		for(Object[] o : results){
			
			if( o==null || o.length!=7 || o[2]==null || o[3]==null || o[4]==null )
				continue;
			if ( o[5] == null  ){
			           if (o[6] == null){
			        	   continue; 			           //continue only if both the shortname as well as the name column is null    	   
			           }
         		}
				
			//geometry
			ArrayList<Object[]> obs = new ArrayList<Object[]>();
			obs.add(new Object[]{ o[0],o[1] });
			
			//state_zone
			String fips = (String)o[4];
			
			if( fips==null || fips.length()!=5 )
				continue;
			
			//zone name
			String name;
			if ( o[5] != null ){
			  name = (String)o[5];
			}else{
				name = (String)o[6];
			}
			
			//put Z back as in PAZ008
			String key = fips.substring(0,2)+"Z"+fips.substring(2);			
			
			//put into fipsMultiResultMap
			if(fipsMultiResultMap.containsKey(key))
				
				fipsMultiResultMap.get(key).add(obs);
			else{	
				ArrayList<ArrayList<Object[]>> list = new ArrayList<ArrayList<Object[]>>();	
				list.add(obs);	
				fipsMultiResultMap.put(key,list);
			}
			
			//put into fipsLatLonMap
			//LatLonPoint value = new LatLonPoint((Double)o[2],(Double)o[3], LatLonPoint.INDEGREES);  // TTR#319 - ClassCastException:  BigDecimal->Double
			LatLonPoint value = new LatLonPoint(((Number)o[2]).doubleValue(),((Number)o[3]).doubleValue(), LatLonPoint.INDEGREES);
			fipsLatLonMap.put(key, value);
			
			//put into fipsNameMap
			fipsNameMap.put(key, name);
			
		}
		
	}
	
	
	public LatLonPoint getLatLonPoint(String fips){
		
		return this.fipsLatLonMap.get(fips);
		
	}
	
	public ArrayList<ArrayList<Object[]>> getZoneResult(String fips){
		
		ArrayList<ArrayList<Object[]>> list = fipsMultiResultMap.get(fips);
		
		if( list == null ){
			logger.log(Level.WARNING, "_______ No result for fips: "+fips);
			
			return new ArrayList<ArrayList<Object[]>>();
		}
		
		return list;
		
	}
	
	public String getZoneName(String fips){
		String n = fipsNameMap.get(fips);
		
		return (n==null) ? "" : n;
	}

}
