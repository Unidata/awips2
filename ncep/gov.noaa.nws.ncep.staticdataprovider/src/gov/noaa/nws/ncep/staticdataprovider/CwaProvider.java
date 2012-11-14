/*
 * gov.noaa.nws.ncep.staticDataProvider.CwaProvider
 * 
 * 24 June 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.common.staticdata.Cwa;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

public class CwaProvider {
	private static List<Cwa> allCwas;
	private static volatile boolean cwaLoaded = false; 
	
	/**
	 * Get all counties and marine zones from the database
	 * @return
	 */
	public static List<Cwa> getAllCwas(){
		if ( !cwaLoaded ){
			loadCwaTable();
		}
		
		return allCwas;
	}
	/**
	 * load cwa table
	 * @return
	 */
	public static synchronized List<Cwa> loadCwaTable(){
		
		if ( !cwaLoaded ) {
			
			allCwas = new ArrayList<Cwa>();

			List<Object[]> cwas;

			String queryStates = "Select AsBinary(the_geom),cwa,wfo,lat, lon FROM mapdata.cwa;";

			try {
				cwas = NcDirectDbQuery.executeQuery(
						queryStates, "maps", QueryLanguage.SQL);

				WKBReader wkbReader = new WKBReader();

				int tt = 0;
				for ( Object[] st : cwas ){

						//read shape
						byte[] wkb = (byte[]) st[0];
						Geometry cwaGeo = null;
						try {
							cwaGeo =  wkbReader.read(wkb);
						} catch (ParseException e) {
							e.printStackTrace();
						}

						String cwa = (String)st[1];
						String wfo = (String)st[2];

						Coordinate loc = new Coordinate(0,0); 
						try {
							loc.x = ((Number)st[4]).doubleValue(); 
							loc.y =	((Number)st[3]).doubleValue();
						}
						catch ( Exception e ){
							// center location missing in database 
						}

						
						Cwa aCwa = new Cwa(cwa, wfo, loc, cwaGeo);
						allCwas.add(aCwa);
						tt++;
							
				}	
			}
			catch (Exception e ){
				System.out.println("db exception reading state tables!");	
				e.printStackTrace();
			}
			cwaLoaded = true;
			
		}
		return allCwas;
	}
	
	/**
	 * check the cwa in the polygon
	 * @param geo
	 * @return
	 */
	public static ArrayList<Cwa> cwasInGeometry(Geometry geo){
		getAllCwas();
		ArrayList<Cwa> ret= new ArrayList<Cwa>();
		for ( Cwa oneCwa : allCwas){
			if ( oneCwa.intersectGeometry(geo)){
				ret.add(oneCwa);
			}
		}
		return ret;
	}
	/**
	 * check if cwa table loaded or not
	 * @return
	 */
	public static boolean isLoaded(){
		return cwaLoaded;
	}

}
