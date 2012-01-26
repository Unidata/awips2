/**
 * gov.noaa.nws.ncep.viz.rsc.warn.rsc.WcnCountyQueryResult
 * 
 * Date created September 28, 2011
 *
 *  This code is developed by the SIB for use in the AWIPS2 system. 
 */

package gov.noaa.nws.ncep.viz.rsc.wcn.rsc;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.*;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;


/**
* Class for loading and storing county query result
* for WCN. 
*
* <pre>
* SOFTWARE HISTORY
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* 09/28/11       #456      G. Zhang   Initial Creation                                     
* 
* </pre>
* 
* @author gzhang 
* @version 1.0
*/

public class WcnCountyQueryResult {
	
	private static final double ENV_MIN_X = -180.0;
	private static final double ENV_MAX_X = 180.0;
	private static final double ENV_MIN_Y = -90;
	private static final double ENV_MAX_Y = 90.0;	
	
	private static Logger logger = Logger.getLogger("gov.noaa.nws.ncep.viz.rsc.wcn.rsc.WcnCountyQueryResult");
	
	private String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
			ENV_MIN_X, ENV_MIN_Y, ENV_MAX_X, ENV_MAX_Y);

	private StringBuilder query = new StringBuilder();
	
	private StringBuilder queryMZ = new StringBuilder();
	
	private	String queryPrefix = "select AsBinary(the_geom), AsBinary(the_geom_0_001), state,countyname,fips from mapdata.county where ";
	
	private String queryPrefixMZ = "select AsBinary(the_geom), AsBinary(the_geom_0_001), wfo,name,id from mapdata.marinezones where ";
		
	private Map<String, ArrayList<ArrayList<Object[]>>> fipsMultiResultMap = new HashMap<String,ArrayList<ArrayList<Object[]>>>();
	
	
	public WcnCountyQueryResult(){
		
	}
	
	/**
	 * Build query using fips
	 * 	
	 */	
	public  void buildQueryPart2(IRscDataObject dataObject){
    	
    	String fips;    	
		WcnResource.WcnRscDataObj wData= (WcnResource.WcnRscDataObj)dataObject;	
		
		if(wData == null || wData.countyFips == null) return;								
	
		for(int i = 0; i < wData.countyFips.size();i++) {

			fips = wData.countyFips.get(i);
			if( wData.isCounty ){
				query.append(" ( fips ='");
				query.append(fips);
				query.append("' ) OR  ");
			}else{
				queryMZ.append(" ( id ='").append(fips).append("' ) OR  ");
			}
			
		}		
    }
	
	/**
	 * store query result into a map.
	 */
	public void populateMap(){
		
		List<Object[]> results = null;
		
		List<Object[]> resultsMZ = null;
		
		try{
			
			if(query.length() > 0){
				String wholeQuery = queryPrefix + geoConstraint+" AND ("+ query.substring(0, query.lastIndexOf("OR")) + " );";	
				results = DirectDbQuery.executeQuery(wholeQuery, "maps", QueryLanguage.SQL);
			}
			
			if(queryMZ.length() > 0){
				String wholeQueryMZ = queryPrefixMZ + geoConstraint+" AND ("+ queryMZ.substring(0, queryMZ.lastIndexOf("OR")) + " );";	
				resultsMZ = DirectDbQuery.executeQuery(wholeQueryMZ, "maps", QueryLanguage.SQL);
			}
		}catch(Exception e){
			logger.log(Level.SEVERE, "_____ Exception in query string or result: "+ e.getMessage());
			return;
		}
		
		if( results != null){
			for(Object[] o : results){
				if( o==null || o.length!=5 || o[2]==null || o[3]==null || o[4]==null )
					continue;
				
				ArrayList<Object[]> obs = new ArrayList<Object[]>();
				obs.add(new Object[]{ o[0],o[1] });			
	
				String key = (String)o[4];			
	
				if(fipsMultiResultMap.containsKey(key))
		
					fipsMultiResultMap.get(key).add(obs);
				else{	
					ArrayList<ArrayList<Object[]>> list = new ArrayList<ArrayList<Object[]>>();	
					list.add(obs);	
					fipsMultiResultMap.put(key,list);
				}	
				
			}
		}
		
		if( resultsMZ != null){
			for(Object[] o : resultsMZ){
				if( o==null || o.length!=5 || o[2]==null || o[3]==null || o[4]==null )
					continue;
				
				ArrayList<Object[]> obs = new ArrayList<Object[]>();
				obs.add(new Object[]{ o[0],o[1] });			
	
				String key = (String)o[4];			
	
				if(fipsMultiResultMap.containsKey(key))
		
					fipsMultiResultMap.get(key).add(obs);
				else{	
					ArrayList<ArrayList<Object[]>> list = new ArrayList<ArrayList<Object[]>>();	
					list.add(obs);	
					fipsMultiResultMap.put(key,list);
				}	
				
			}
		}
	}
	
	
	/**
	 * 2011-09-28: Loiza county in Puerto Rico with fips 72087 
	 * has NO record in Raytheon's database: maps mapdata.county table
	 * and we need to handle cases like that.
	 * 
	 * TODO: move this handling to the query place?
	 */	
	public ArrayList<ArrayList<Object[]>> getStateCountyResult2(String fips){
		
		ArrayList<ArrayList<Object[]>> list = fipsMultiResultMap.get(fips);
		
		if( list == null ){
			logger.log(Level.WARNING, "_______ No result for fips: "+fips);
			
			return new ArrayList<ArrayList<Object[]>>();
		}
		
		return list;
	}

}
