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
 *
 * </pre>
 * 
 * @author	B. Yin
 */


public class GreatLakeProvider {

	private static List<GreatLake> greatLakes;
	private static volatile boolean greatLakesLoaded = false;

	/**
	 * Get Great Lake information 
	 * @return - list of Great Lakes
	 */
	public static List<GreatLake> getGreatLakes(){
		if ( !greatLakesLoaded ){
			loadGreatLakeTable();
		}

		return greatLakes;
	}
	
	/**
	 * Load Great Lake information  from the database
	 * @return - list of Great Lakes
	 */
	public static synchronized List<GreatLake> loadGreatLakeTable(){

		if ( !greatLakesLoaded ) {
			greatLakes = new ArrayList<GreatLake>();
			String sql = "select bid, area, ctrloc, numblocks,id,AsBinary(the_geom) from " + "bounds.greatlakesbnds";
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

}
