/*
 * gov.noaa.nws.ncep.common.staticDataProvider.GreatLakeProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.common.staticdata.GreatLake;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Class to provide bounds of Great Lakes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?			B. Yin   	Initial Creation.
 * 05/12		?			B. Yin		Changed column 'area' to 'name in SQL string
 * 07/13		?			J. Wu		Used mapdata.lake since NCEP DB has invalid geometries
 *
 * </pre>
 * 
 * @author	B. Yin
 */


public class GreatLakeProvider {

	private static List<GreatLake> greatLakes;
	private static volatile boolean greatLakesLoaded = false;

	private static String[] lakeNames = { "Lake Erie","Lake Michigan", "Lake Huron", 
		                                  "Lake Ontario", "Lake Superior" };


	/**
	 * Get Great Lake information 
	 * @return - list of Great Lakes
	 */
	public static List<GreatLake> getGreatLakes(){
		if ( !greatLakesLoaded ){
			loadGreatLakeMaps();
		}

		return greatLakes;
	}
	
	/**
	 * Load Great Lake information from NCEP database.
	 * 
	 * This one fails to load bounds since the bounds in NCEP DB have invalid 
	 * geometries (e.g., stick-outs). Instead we should use mapdata.lake unless 
	 * we correct the bounds in NCEP DB.
	 * 
	 * @return - list of Great Lakes
	 */
	public static synchronized List<GreatLake> loadGreatLakeTable(){

		if ( !greatLakesLoaded ) {
			greatLakes = new ArrayList<GreatLake>();
			String sql = "select bid, name, ctrloc, numblocks,id,AsBinary(the_geom) from " + "bounds.greatlakesbnds";
			List<Object[]> results;

			try {
				results = NcDirectDbQuery.executeQuery(sql, "ncep", QueryLanguage.SQL);

				WKBReader wkbReader = new WKBReader();

				for ( Object[] fa : results ){
					if ( fa[1] != null && fa[5] != null ) {

						int numblocks = 1; 
						try{ 
							numblocks = ((Number)fa[3]).intValue();
						}
						catch (Exception e){

						}

						Coordinate loc = BoundsUtil.parseCtrloc( (String)fa[2] ); 
						Geometry g = wkbReader.read((byte[]) fa[5]);

						if ( g.isValid() ){
							greatLakes.add( new GreatLake( (String)fa[0],
									(String)fa[1],
									loc,
									numblocks,
									(String)fa[4],
									g ));
						}
					}
				}
			}
			catch (Exception e ){
				System.out.println("db exception reading the Great Lake bounds table!");	
				e.printStackTrace();
			}
		}

		return greatLakes;
	}

	
	/**
	 * Load Great Lake information from the maps database.
	 * 
	 * This one reads those 5 lakes, one at a time from DB.  This one is
	 * faster than loadGreatLakeMaps1().
	 * 
	 * @return - list of Great Lakes
	 */
	public static synchronized List<GreatLake> loadGreatLakeMaps(){

		greatLakes = new ArrayList<GreatLake>();

		for ( String gls : lakeNames ) {
			String sql = "select name, lat, lon, AsBinary(the_geom) from " + "mapdata.lake" + 
			" where name = '" + gls + "'";
			List<Object[]> results;

			try {
				results = NcDirectDbQuery.executeQuery( sql, "maps", QueryLanguage.SQL );

				WKBReader wkbReader = new WKBReader();

				for ( Object[] fa : results ){
					if ( fa[0] != null && fa[3] != null ) {

						Geometry g = wkbReader.read((byte[]) fa[3]);

						Coordinate loc = new Coordinate( 0, 0 );; 
						try {
							loc.x = ((Number)fa[2]).doubleValue(); 
							loc.y =	((Number)fa[1]).doubleValue();
						}
						catch ( Exception e ){
							// center location missing in database 
						}

						if ( g.isValid() ){

							if ( loc.x == 0 && loc.y == 0 ) {
								loc = new Coordinate( g.getCentroid().getX(), g.getCentroid().getY() );
							}

							String[] gn = ((String)fa[0]).split(" ");
							String  shortName = new String( gn[0].substring(0,1) + gn[1].substring(0,1));

							greatLakes.add( new GreatLake( "", (String)fa[0], loc, 1, shortName, g ) );
						}
					}
				}
			}
			catch (Exception e ){
				System.out.println("db exception reading the Great Lake bounds table!");	
				e.printStackTrace();
			}

		}
			
		return greatLakes;
	}
	
	/**
	 * Load Great Lake information from the maps database (mapdata.lake).
     *
	 * This one first reads all lakes in from DB. Then pick those 5 lakes. It is
	 * faster than loadGreatLakeMaps() since tehre are about one thousand lakes in DB.
	 * 
	 * @return - list of Great Lakes
	 */
	public static synchronized List<GreatLake> loadGreatLakeMaps1(){

		if ( !greatLakesLoaded ) {

			greatLakes = new ArrayList<GreatLake>();
			String sql = "select name, lat, lon, AsBinary(the_geom) from " + "mapdata.lake" ;
			List<Object[]> results;

			try {
				results = NcDirectDbQuery.executeQuery(sql, "maps", QueryLanguage.SQL);

				WKBReader wkbReader = new WKBReader();

				for ( Object[] fa : results ){
					if ( fa[0] != null && fa[3] != null ) {
                        
						if ( !isGreatLake( (String)fa[0] ) ) {
                        	continue;
                        }
                        
						Geometry g = wkbReader.read((byte[]) fa[3]);
                        
						Coordinate loc = new Coordinate( 0, 0 );; 
						try {
							loc.x = ((Number)fa[2]).doubleValue(); 
							loc.y =	((Number)fa[1]).doubleValue();
						}
						catch ( Exception e ){
							// center location missing in database 
						}
						
						if ( g.isValid() ){
							
							if ( loc.x == 0 && loc.y == 0 ) {
								loc = new Coordinate( g.getCentroid().getX(), g.getCentroid().getY() );
							}
							
							String[] gn = ((String)fa[0]).split(" ");
							String  shortName = new String( gn[0].substring(0,1) + gn[1].substring(0,1));

							greatLakes.add( new GreatLake( "", (String)fa[0], loc, 1, shortName, g ) );
						}
					}
				}
	        }
			catch (Exception e ){
				System.out.println("db exception reading the Great Lake bounds table!");	
				e.printStackTrace();
			}			
		}
		
		return greatLakes;
		
	}
	
	/*
	 * Check if a lake name is one of the Great Lake.
	 */
	private static boolean isGreatLake( String name ) {
		
	    for ( String ss : lakeNames ) {
			if ( name.equalsIgnoreCase( ss ) ) {
				return true;
			}
		}
	    
	    return false;
	}

}
