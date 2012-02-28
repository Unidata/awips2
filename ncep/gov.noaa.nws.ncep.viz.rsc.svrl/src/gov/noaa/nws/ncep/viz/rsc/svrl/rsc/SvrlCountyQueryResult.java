/**
 * gov.noaa.nws.ncep.viz.rsc.ffa.rsc.FfaZoneQueryResult
 * 
 * Date created October 04, 2011
 *
 * This code is developed by the SIB for use in the AWIPS2 system. 
 */

package gov.noaa.nws.ncep.viz.rsc.svrl.rsc;

import java.util.*;
import java.util.logging.*;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.*;
import com.raytheon.uf.viz.core.catalog.*;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;


/**
 * This class handling database query part for the SvrlResource. 
 *  
 *
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011-10-04   456        G. Zhang    Initial creation.
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public class SvrlCountyQueryResult {	
	
	private static final double ENV_MIN_X = -180.0;
	private static final double ENV_MAX_X = 180.0;
	private static final double ENV_MIN_Y = -90;
	private static final double ENV_MAX_Y = 90.0;	
	
	private static Logger logger = Logger.getLogger("gov.noaa.nws.ncep.viz.rsc.svrl.rsc.SvrlCountyResult");	
	
	private String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
			ENV_MIN_X, ENV_MIN_Y, ENV_MAX_X, ENV_MAX_Y);
	
	private StringBuilder query = new StringBuilder();
	
	private String queryPrefix = "select AsBinary(the_geom), AsBinary(the_geom_0_001), state,countyname,fips from mapdata.county where "; 
	
	private Map<String, ArrayList<ArrayList<Object[]>>> fipsMultiResultMap = new HashMap<String,ArrayList<ArrayList<Object[]>>>();
	
	
	public SvrlCountyQueryResult(){
		
	}
	
	public void buildeQueryPart(IRscDataObject rdo){
		
		if( ! (rdo instanceof SvrlResource.SvrlData ) )
			return;
		
    	String fips;    	
    	SvrlResource.SvrlData sData= (SvrlResource.SvrlData)rdo;	
		
		if(sData == null || sData.countyFips == null) return;								
	
		for(int i = 0; i < sData.countyFips.size();i++) {

			fips = sData.countyFips.get(i);
			
			query.append(" ( fips ='");
			query.append(fips);
			query.append("' ) OR  ");
			
			
		}	
		
	}
	
	public void populateMap(){
		
		List<Object[]> results = null;
		
		try{
			if(query.length() > 0){
			
				String wholeQuery = queryPrefix + geoConstraint+" AND ("+ query.substring(0, query.lastIndexOf("OR")) + " );";			
				results = DirectDbQuery.executeQuery(wholeQuery, "maps", QueryLanguage.SQL);
			}
			
		}catch(Exception e){
			logger.log(Level.SEVERE, "_____ Exception with query string or result: "+ e.getMessage());
			
			//if query string is wrong or query result is wrong, we stop here and nothing should displayed.
			return;
		}
		
		if( results == null ) return;
		
		for(Object[] o : results){
			if( o==null || o.length!=5 || o[2]==null || o[3]==null || o[4]==null )
				continue;
			
			ArrayList<Object[]> obs = new ArrayList<Object[]>();
			obs.add(new Object[]{ o[0],o[1] });			

			String key = (String)o[4];	//county fips		

			if(fipsMultiResultMap.containsKey(key))
	
				fipsMultiResultMap.get(key).add(obs);
			else{	
				ArrayList<ArrayList<Object[]>> list = new ArrayList<ArrayList<Object[]>>();	
				list.add(obs);	
				fipsMultiResultMap.put(key,list);
			}
	
		}
	}
	
	public ArrayList<ArrayList<Object[]>> getCountyResult(String fips){
		
		ArrayList<ArrayList<Object[]>> list = fipsMultiResultMap.get(fips);
		
		if(list == null){
			logger.info("No County result, empty list returned!");
			return new ArrayList<ArrayList<Object[]>>();
		}
		
		return list;		
	}

}
