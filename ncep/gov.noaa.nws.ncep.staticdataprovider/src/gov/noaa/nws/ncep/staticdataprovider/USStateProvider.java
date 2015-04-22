/*
 * gov.noaa.nws.ncep.staticDataProvider.USStateProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.common.staticdata.USState;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Class to load US states maps.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?			B. Yin   	Moved from PGEN 
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class USStateProvider {
	
	private static List<USState> allStates;
	private static volatile boolean stateLoaded = false;
	
	/**
	 * Get all counties and marine zones from the database
	 * @return
	 */
	public static List<USState> getAllStates(){
		if ( !stateLoaded ){
			loadStateTable();
		}
		
		return allStates;
	}
	
	public static synchronized List<USState> loadStateTable(){
		
		if ( !stateLoaded ) {
			
			allStates = new ArrayList<USState>();

			List<Object[]> states;

			String queryStates = "Select AsBinary(the_geom),state,name,fips,lat, lon FROM mapdata.states;";

			try {
				states = NcDirectDbQuery.executeQuery(
						queryStates, "maps", QueryLanguage.SQL);

				WKBReader wkbReader = new WKBReader();

				int tt = 0;
				for ( Object[] st : states ){

						//read shape
						byte[] wkb = (byte[]) st[0];
						Geometry stateGeo = null;
						try {
							stateGeo =  wkbReader.read(wkb);
						} catch (ParseException e) {
							e.printStackTrace();
						}

						String stateAbb = (String)st[1];
						String fullName = (String)st[2];
						String fips = (String)st[3];

						Coordinate loc = new Coordinate(0,0); 
						try {
							loc.x = ((Number)st[5]).doubleValue(); 
							loc.y =	((Number)st[4]).doubleValue();
						}
						catch ( Exception e ){
							// center location missing in database 
						}

								USState state = new USState(stateAbb, fullName, fips, loc, stateGeo);
								allStates.add(state);
								tt++;
							
						}	
				

				
			}
			catch (Exception e ){
				System.out.println("db exception reading state tables!");	
				e.printStackTrace();
			}
			stateLoaded = true;
			
		}
		return allStates;
	}
	
	public static ArrayList<USState> statesInGeometry(Geometry geo){
		loadStateTable();
		ArrayList<USState> ret= new ArrayList<USState>();
		for ( USState st : allStates){
			if ( st.intersectGeometry(geo)){
				ret.add(st);
			}
		}
		return ret;
	}
	
	public static boolean isLoaded(){
		return stateLoaded;
	}
}
