/**
 * gov.noaa.nws.ncep.viz.rsc.warn.rsc.WarnCountyResult
 * 
 * Date created August 25, 2011
 *
 *  This code is developed by the SIB for use in the AWIPS2 system. 
 */

package gov.noaa.nws.ncep.viz.rsc.warn.rsc;

import java.util.*;
import java.util.logging.*;
//import java.util.concurrent.*;

//import com.vividsolutions.jts.geom.*;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.*;
import com.raytheon.uf.viz.core.catalog.*;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;

/**
 * WarnCountyResult: this class handling database query 
 * part for the WarnResource. 
 *  
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011-08-25   456        G. Zhang    Initial creation.
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public class WarnCountyResult {
	
	private static final double ENV_MIN_X = -180.0;
	private static final double ENV_MAX_X = 180.0;
	private static final double ENV_MIN_Y = -90;
	private static final double ENV_MAX_Y = 90.0;	
	
	private static Logger logger = Logger.getLogger("gov.noaa.nws.ncep.viz.rsc.warn.rsc.WarnCountyResult");
	
	private Object[] pdoList;
	
	private WarnResource wcnRsc;	
	
	private String geoConstraint;
	
	private StringBuilder query = new StringBuilder();
	
	private	String queryPrefix = "select AsBinary(the_geom), AsBinary(the_geom_0_001), state,countyname,fips from mapdata.county where ";
	
	//replaced by fipsMultiResultMap; keep for reference
	private Map<String, List<Object[]>> fipsResultMap = new HashMap<String, List<Object[]>>();
	
	private Map<String, ArrayList<ArrayList<Object[]>>> fipsMultiResultMap = new HashMap<String,ArrayList<ArrayList<Object[]>>>();
	
/*	//before using fips, StateCounty was used; keep for reference
	public class StateCounty{	
		
		String state = "";	
		String county = "";	
		
		public StateCounty(String s, String c){
			state = s;
			county = c;
		}
		
		@Override 
		public boolean equals(Object o){
			if( ! (o instanceof StateCounty)) return false;
			
			return ((StateCounty)o).state.equals(state) 
					&& ((StateCounty)o).county.equals(county);
		}
		
		@Override 
		public int hashCode(){
			return state.hashCode()*11+county.hashCode()*19;
		}
	}
*/	
//	private /*static final*/ Map<StateCounty, List<Object[]>> STATECOUNTY_RESULT_MAP = new HashMap<StateCounty, List<Object[]>>();	
	
	
	public WarnCountyResult(){
		
	}
	
	public WarnCountyResult(WarnResource wcnRsc, Object[] pdoList){
		this.pdoList = pdoList;
		this.wcnRsc = wcnRsc;
		geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
							ENV_MIN_X, ENV_MIN_Y, ENV_MAX_X, ENV_MAX_Y);
		
	}
/*	
	public List<Object[]> getStateCountyResult(String state, String county){
		if(county.contains("_")){			county=county.replace("_"," ");		}//20110830
		
		StateCounty sc = new WarnCountyResult().new StateCounty(state,county);
		//sc.state = state;	sc.county = county;
		
		return STATECOUNTY_RESULT_MAP.get(sc);//this.scResultMap.get(sc);
	}   
*/
	
	/**
	 * Build query using fips
	 * 	
	 */	
	public  void buildQueryPart2(IRscDataObject dataObject){
    	
    	String fips;    	
		WarnResource.WarnRscDataObj wData= (WarnResource.WarnRscDataObj)dataObject;	
		
		if(wData == null || wData.countyFips == null) return;								
		
		for(int i = 0; i < wData.countyFips.size();i++) {

			fips = wData.countyFips.get(i);

			query.append(" ( fips ='");
			query.append(fips);
			query.append("' ) OR  ");
			
		}		
    }
	
	public void populateMap(){
		
		List<Object[]> results = null;
		
		try{
			String wholeQuery = queryPrefix + geoConstraint+" AND ("+ query.substring(0, query.lastIndexOf("OR")) + " );";
		
			results = DirectDbQuery.executeQuery(wholeQuery, "maps", QueryLanguage.SQL);//"ncep", QueryLanguage.SQL);//20110830
			
		}catch(Exception e){
			logger.log(Level.SEVERE, "_____ Exception in query string or result: "+ e.getMessage());
			return;
		}
		
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
	
			fipsResultMap.put((String)o[4], obs);
		}
	}
	

	public List<Object[]> getStateCountyResult(String fips){
				
		return fipsResultMap.get(fips);
	}
	
	/**
	 * 2011-09-01: Loiza county in Puerto Rico with fips 72087 
	 * has NO record in Raytheon's database: maps mapdata.county table
	 * and we need to handle cases like that.
	 * 
	 * TODO: move this handling to the query place?
	 */
	
	public ArrayList<ArrayList<Object[]>> getStateCountyResult2(String fips){
		
		ArrayList<ArrayList<Object[]>> list = fipsMultiResultMap.get(fips);
		
		if( list == null ){
			//logger.log(Level.WARNING, "_______ No result for fips: "+fips);
			
			return new ArrayList<ArrayList<Object[]>>();
		}
		
		return list;
	}
}
