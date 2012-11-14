/*
 * gov.noaa.nws.ncep.staticDataProvider.RfcProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.common.staticdata.Rfc;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Class to hold RFC information
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?	    	B. Yin   	Initial Creation.
 * 06/12        734         J. Zneg     Move from PGEN
 * </pre>
 * 
 * @author	B. Yin
 */

public class RfcProvider {
	
	private static List<Rfc> allRfcs;
	private static volatile boolean rfcLoaded = false; 
	
	/*
	 * Get all counties and marine zones from the database
	 * @return
	 */
	public static List<Rfc> getAllRfcs(){
		if ( !rfcLoaded ){
			loadRfcTable();
		}
		
		return allRfcs;
	}
	/**
	 * Read from database and load Rfc table
	 * @return
	 */
	public static synchronized List<Rfc> loadRfcTable(){
		
		if ( !rfcLoaded ) {
			
			allRfcs = new ArrayList<Rfc>();

			List<Object[]> rfcs;

			String queryStates = "Select AsBinary(the_geom),site_id,state,rfc_name,rfc_city,basin_id FROM mapdata.rfc";

			try {
				rfcs = NcDirectDbQuery.executeQuery(
						queryStates, "maps", QueryLanguage.SQL);

				WKBReader wkbReader = new WKBReader();

				int tt = 0;
				for ( Object[] rfc : rfcs ){

						//read shape
						byte[] wkb = (byte[]) rfc[0];
						Geometry rfcGeo = null;
						try {
							rfcGeo =  wkbReader.read(wkb);
						} catch (ParseException e) {
							e.printStackTrace();
						}

						String siteId = (String)rfc[1];
						String state = (String)rfc[2];
						String rfcName = (String)rfc[3];
						String rfcCity = (String)rfc[4];
						String basinId = (String)rfc[5];

					
						Rfc aRfc = new Rfc(rfcName, rfcCity, state, siteId, basinId, rfcGeo);
						allRfcs.add(aRfc);
						tt++;
							
				}	
				

				
			}
			catch (Exception e ){
				System.out.println("db exception reading state tables!");	
				e.printStackTrace();
			}
			rfcLoaded = true;
			
		}
		return allRfcs;
	}

	/**
	 * Return a list of RFC in the input geometry
	 * @param geo
	 * @return
	 */
	public static ArrayList<Rfc> rfcsInGeometry(Geometry geo){
		getAllRfcs() ;
		ArrayList<Rfc> ret= new ArrayList<Rfc>();
		for ( Rfc rfc : allRfcs){
			if ( rfc.intersectGeometry(geo)){
				ret.add(rfc);
			}
		}
		return ret;
	}
	
	/**
	 * Check if RFC table is loaded.
	 * @return true if RFC has been loaded
	 */
	public static boolean isRfcLoaded(){
		
		return rfcLoaded;
	}
	
}
