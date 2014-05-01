/*
 * gov.noaa.nws.ncep.common.staticDataProvider.G2GBoundsProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;


/**
 * Class to provide a method to load bounds required by G2G
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?			B. Yin   	Moved from G2G package.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class G2GBoundsProvider {
	
	/**
	 * Get a list of multi-polygons that have the certain value(columnValue) for a column(columnName)
	 * from a table in the datbase.
	 * @param tableAlias
	 * @param columnName
	 * @param columnValue
	 * @return - list of multi-polygons
	 */
	public static ArrayList<MultiPolygon> getG2GBounds(String tableAlias, String columnName, String columnValue){

		// Query the bound table name in config.clo
		String bndsTableSql = "select t.table_name from config.clo t where t.alias_name = '" +
		tableAlias.toUpperCase() + "'";

		List<Object[]> tableFile = null;		
		try {	    
			tableFile  = NcDirectDbQuery.executeQuery(
					bndsTableSql, "ncep", QueryLanguage.SQL );
		} catch (Exception e ) {
			e.printStackTrace();
		}


		String tableName = null; 
		if ( tableFile.isEmpty() ){
			// no record, try to use it directly as a table name
			tableName = tableAlias;
		} else {
			tableName = (String) tableFile.get(0)[0];
		}	        	

		// Query the bounds from the table
		String queryBnds;
		queryBnds = "SELECT AsBinary(t.the_geom) FROM " + "bounds" + "." + tableName;	    
		if ( columnName != null  && columnValue != null ) {
			//	    	queryBnds += " t" + " WHERE t." + columnName + " like '" + columnValue + "%'";
			queryBnds += " t" + " WHERE t." + columnName.toUpperCase() + " = '" + 
			columnValue.toUpperCase() + "'";
		}

		List<Object[]> bounds = null;
		try {	    
			bounds = NcDirectDbQuery.executeQuery(
					queryBnds, "ncep", QueryLanguage.SQL );

		} catch (Exception e ) {
			e.printStackTrace();
		}


		// Read and store it
		WKBReader wkbReader = new WKBReader();
		GeometryFactory	geometryFactory	= new GeometryFactory();
		ArrayList<MultiPolygon> multiPolygons = new ArrayList<MultiPolygon>();

		if ( bounds != null && !bounds.isEmpty() ) {
			for ( Object[] bnd : bounds ){

				if ( bnd[0] != null ){

					MultiPolygon mpoly = null;
					try {
						Geometry g = wkbReader.read( (byte[]) bnd[0] );
						if ( g instanceof MultiPolygon ) {
							mpoly = (MultiPolygon) g;
						}
						else if ( g instanceof Polygon ) {
							mpoly = new MultiPolygon( new Polygon[]{ (Polygon)g },
									geometryFactory );
						}

					} catch ( ParseException e) {
						e.printStackTrace();
					}

					if ( mpoly != null ) {
						multiPolygons.add( mpoly );
					}

				}
			}
		}	    

		return multiPolygons;
	}
}
