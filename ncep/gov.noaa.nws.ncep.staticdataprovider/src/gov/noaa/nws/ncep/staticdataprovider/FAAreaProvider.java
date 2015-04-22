/*
 * gov.noaa.nws.ncep.common.staticDataProvider.FAAreaProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.common.staticdata.FAArea;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Class to provide FA area and extended FA area information.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?			B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class FAAreaProvider {
	
	private static List<FAArea> allFAAreas;
	private static volatile boolean faAreaLoaded = false;
	
	//for extended FA areas
	private static List<FAArea> allFAAreaX;
	private static volatile boolean faAreaXLoaded = false;	
	
	/**
	 * Get FA areas 
	 * @return - list of FA areas
	 */
	public static List<FAArea> getFAAreas(){
		if ( !faAreaLoaded ){
			allFAAreas = loadFAAreaTable("fa_area");
		}
		
		return allFAAreas;
	}
	
	/**
	 * Get extended FA areas 
	 * @return - list of extended FA areas
	 */
	public static List<FAArea> getFAAreaX(){
		if ( !faAreaXLoaded ){
			allFAAreaX = loadFAAreaTable("fa_areax");
		}
		
		return allFAAreaX;
	}
	
	/**
	 * Load FA area or extended areas from the database
	 * @param table - FA area(or extended area) table name 
	 * @return - list of FA areas
	 */
	public static List<FAArea> loadFAAreaTable(String table){

		ArrayList<FAArea> faa = new ArrayList<FAArea>();
		String sql = "select bid, area, name, states, numblocks, ctrloc, AsBinary(the_geom) from " + "bounds." + table;
		List<Object[]> results;

		try {
			results = NcDirectDbQuery.executeQuery(sql, "ncep", QueryLanguage.SQL);

			WKBReader wkbReader = new WKBReader();

			for ( Object[] fa : results ){
				if ( fa[1] != null && fa[6] != null ) {

					int numblocks = 1; 
					try{ 
						numblocks = ((Number)fa[4]).intValue();
					}
					catch (Exception e){

					}

					Coordinate loc = BoundsUtil.parseCtrloc( (String)fa[5] ); 
					Geometry g = wkbReader.read((byte[]) fa[6]);

					if ( g.isValid() ){
						faa.add( new FAArea( (String)fa[0],
								(String)fa[1],
								(String)fa[2],
								(String)fa[3],
								numblocks,
								loc,
								g ));
					}
				}
			}
		}
		catch (Exception e ){
			System.out.println("db exception reading FA Area tables!");	
			e.printStackTrace();
		}


		return faa;
	}
}
