/*
 * gov.noaa.nws.ncep.common.staticDataProvider.FARegionProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.common.staticdata.FARegion;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Class to provide FA region information.
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

public class FARegionProvider {
	
	private static List<FARegion> allFARegions;
	private static volatile boolean faRegionLoaded = false;
	
	/**
	 * Get FA regions 
	 * @return - list of FA regions
	 */
	public static List<FARegion> getFARegions(){
		if ( !faRegionLoaded ){
			loadFARegionTable();
		}
		
		return allFARegions;
	}
	
	/**
	 * Load FA region from the database
	 * @return - list of FA regions
	 */
	public static synchronized List<FARegion> loadFARegionTable(){

		if ( !faRegionLoaded ) {
			allFARegions = new ArrayList<FARegion>();
			String sql = "select bid, region, numblocks, ctrloc, AsBinary(the_geom) from " + "bounds.fa_region";
			List<Object[]> results;

			try {
				results = NcDirectDbQuery.executeQuery(sql, "ncep", QueryLanguage.SQL);

				WKBReader wkbReader = new WKBReader();

				for ( Object[] fa : results ){
					if ( fa[1] != null && fa[4] != null ) {
						
						int numblocks = 1; 
						try{ 
							numblocks = ((Number)fa[2]).intValue();
						}
						catch (Exception e){
							
						}
						
						Coordinate loc = BoundsUtil.parseCtrloc( (String)fa[3] ); 
						Geometry g = wkbReader.read((byte[]) fa[4]);
						
						if ( g.isValid() ){
							allFARegions.add( new FARegion( (String)fa[0],
														(String)fa[1],
														numblocks,
														loc,
														g ));
						}
					}
				}
			}
			catch (Exception e ){
				System.out.println("db exception reading FA Region tables!");	
				e.printStackTrace();
			}
		}
		
		return allFARegions;
	}
}
