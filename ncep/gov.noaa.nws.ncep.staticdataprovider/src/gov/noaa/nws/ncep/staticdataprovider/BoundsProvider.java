/*
 * gov.noaa.nws.ncep.staticdataprovider
 * 
 * 25 January 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Implementation of the bounds provider.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/13		#966		B. Yin		Initial creation
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class BoundsProvider {
	
	// List of bounds
	private static List<String> boundsList = null;

	/**
	 * Gets the a list of bounds available.
	 * @return
	 */
	public static List<String> getBoundsList() {
		if ( boundsList == null ){
			boundsList = loadBoundsList();
		}
		if ( boundsList != null ){
			System.out.println(boundsList);
		}

		return boundsList;
	}

	/**
	 * Load the list of bounds available from the clo table in ncep database.
	 * @return
	 */
	private static List<String> loadBoundsList() {
		String sql = "select table_name, file_type from config.clo";
		List<Object[]> results;
		List<String> ret = new ArrayList<String>();

		try {
			results = NcDirectDbQuery.executeQuery(sql, "ncep", QueryLanguage.SQL);

			for ( Object[] obj : results ){
				if ( obj[0] != null ) {
					String str = (String)obj[0];
					if ( !str.isEmpty() && ((Integer)obj[1]) == 1){
						ret.add(str);
					}
				}
			}
		}
		catch (Exception e ){
			System.out.println("db exception loading bounds list!");	
			e.printStackTrace();
		}

		return ret;
	}

	/**
	 * Loads a list of bounds name from a specified bounds table.
	 * @param table
	 * @return
	 */
	public static List<String> loadBoundsNames( String table ){
		String sql = "select name from bounds." + table;
		List<Object[]> results;
		List<String> ret = new ArrayList<String>();

		try {
			results = NcDirectDbQuery.executeQuery(sql, "ncep", QueryLanguage.SQL);

			for ( Object[] obj : results ){
				if ( obj[0] != null ) {
					String str = (String)obj[0];
					if ( !str.isEmpty() ){
						ret.add(str);
					}
				}
			}
		}
		catch (Exception e ){
			System.out.println("db exception loading bounds names!");	
			e.printStackTrace();
		}

		return ret;
	}

	/**
	 * Loads the bounds polygon for the specified bounds name and bounds table.
	 * @param boundsTable
	 * @param boundsName
	 * @return
	 */
	public static Polygon loadBounds( String boundsTable, String boundsName ){
		String sql = "SELECT AsBinary(the_geom) FROM " + "bounds." + boundsTable  + " WHERE "
		+ "name = '" +  boundsName + "'";

		List<Object[]> results;
		Polygon ret = null;

		try {
			results = NcDirectDbQuery.executeQuery(sql, "ncep", QueryLanguage.SQL);

			WKBReader wkbReader = new WKBReader();
			if (results != null && !results.isEmpty()) {
				Object[] bound = results.get(0); // take the very first one
				GeometryFactory	geometryFactory	= new GeometryFactory();
				
				Geometry g = wkbReader.read((byte[]) bound[0]);
				if(g instanceof MultiPolygon){
					MultiPolygon mg = (MultiPolygon) g;
					int max = 0;
					int index = 0;
					for (int i = 0; i < mg.getNumGeometries(); i++) {
						if (max < mg.getGeometryN(i).getNumPoints()){
							max = mg.getGeometryN(i).getNumPoints();
							index = i;
						}
					}
					ret = (Polygon) mg.getGeometryN(index);
					return ret;
				}

				CoordinateSequence sequence = new CoordinateArraySequence(g.getCoordinates());
				LinearRing ring = new LinearRing(sequence, geometryFactory);
				ret = new Polygon(ring, null, geometryFactory);
			} 
		}catch (Exception e) {
				System.out.println("db exception loading bounds: " + boundsName +"@" + boundsTable );	
				e.printStackTrace();
		}

		return ret;

		}

	}
