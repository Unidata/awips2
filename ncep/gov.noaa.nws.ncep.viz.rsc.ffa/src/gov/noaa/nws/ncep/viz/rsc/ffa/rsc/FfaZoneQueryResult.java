/**
 * gov.noaa.nws.ncep.viz.rsc.ffa.rsc.FfaZoneQueryResult
 * 
 * Date created September 20, 2011
 *
 * This code is developed by the SIB for use in the AWIPS2 system. 
 */

package gov.noaa.nws.ncep.viz.rsc.ffa.rsc;

import java.util.*;
import java.util.logging.*;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.*;
import com.raytheon.uf.viz.core.catalog.*;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;

/**
 * FfaZoneQueryResult: this class handling database query 
 * part for the FFAResource. 
 *  
 *
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011-09-20   456        G. Zhang    Initial creation.
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public class FfaZoneQueryResult {
	
	private static final double ENV_MIN_X = -180.0;
	private static final double ENV_MAX_X = 180.0;
	private static final double ENV_MIN_Y = -90;
	private static final double ENV_MAX_Y = 90.0;	
	
	private static Logger logger = Logger.getLogger("gov.noaa.nws.ncep.viz.rsc.ffa.rsc.FfaZoneQueryResult");
	
	private Object[] pdoList;
	
	private FFAResource ffaRsc;	
	
	private String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
							ENV_MIN_X, ENV_MIN_Y, ENV_MAX_X, ENV_MAX_Y);
	
	private StringBuilder query = new StringBuilder();
	
	private	String queryPrefix = "select AsBinary(the_geom), AsBinary(the_geom_0_001), state,name,state_zone from mapdata.zone where ";
	
	//replaced by fipsMultiResultMap; keep for reference
	private Map<String, List<Object[]>> fipsResultMap = new HashMap<String, List<Object[]>>();
	
	private Map<String, ArrayList<ArrayList<Object[]>>> fipsMultiResultMap = new HashMap<String,ArrayList<ArrayList<Object[]>>>();
	
	public FfaZoneQueryResult(){
		
	}
	
	/**
	 * Build query using fips.
	 * 
	 * Note: 
	 * in Raytheon's mapdata.zone table of maps database, state_zone is like: IA092,
	 * but in Raytheon's aww_fips table of metadata database, fips like: PAZ009;
	 * so we need to take off the Z for querying mapdata.zone table.
	 */	
	public  void buildQueryPart2(IRscDataObject dataObject){
		
    	String fips;    	
		FFAResource.FfaRscDataObj fData= (FFAResource.FfaRscDataObj)dataObject;	
		
		if(fData == null || fData.fips == null) return;								
		
		for(int i = 0; i < fData.fips.size();i++) {
			//StringBuilder sbfips = new StringBuilder();
			fips = fData.fips.get(i);
//System.out.println("___ fips: "+fips+" : vtecline: "+fData.vtecline);			
			//sbfips.append(fips.substring(0, 2)).append(fips.substring(3));//take off Z as in PAZ009
//List<String> getFips(AwwUgc eachAwwUgc) handled the taking off Z business			
			query.append(" ( state_zone ='");
			query.append(fips);//sbfips.toString());
			query.append("' ) OR  ");
			
		}		
    }
	
	public void populateMap(){
		
		List<Object[]> results = null;
		
		try{
			String wholeQuery = queryPrefix + geoConstraint+" AND ("+ query.substring(0, query.lastIndexOf("OR")) + " );";
			
			results = DirectDbQuery.executeQuery(wholeQuery, "maps", QueryLanguage.SQL);//"ncep", QueryLanguage.SQL);//20110830
			
		}catch(Exception e){
			logger.log(Level.SEVERE, "_____ Exception with query string or result: "+ e.getMessage());
			
			//if query string is wrong or query result is wrong, we stop here and nothing should displayed.
			return;
		}
		
		for(Object[] o : results){
			if( o==null || o.length!=5 || o[2]==null || o[3]==null || o[4]==null )
				continue;
			
			ArrayList<Object[]> obs = new ArrayList<Object[]>();
			obs.add(new Object[]{ o[0],o[1] });			

			String key = (String)o[4];	//state_zone		

			if(fipsMultiResultMap.containsKey(key))
	
				fipsMultiResultMap.get(key).add(obs);
			else{	
				ArrayList<ArrayList<Object[]>> list = new ArrayList<ArrayList<Object[]>>();	
				list.add(obs);	
				fipsMultiResultMap.put(key,list);
			}
	
			fipsResultMap.put((String)o[4], obs);
		}
	}
	
	public ArrayList<ArrayList<Object[]>> getZoneResult(String fips){
		
		ArrayList<ArrayList<Object[]>> list = fipsMultiResultMap.get(fips);
		
		if( list == null ){
			logger.log(Level.WARNING, "_______ No result for fips: "+fips);
			return new ArrayList<ArrayList<Object[]>>();
		}
		
		return list;
	}

}
