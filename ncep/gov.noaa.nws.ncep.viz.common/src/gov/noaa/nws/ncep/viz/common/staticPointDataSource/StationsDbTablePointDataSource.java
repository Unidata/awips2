/**
 * example for cities station table mapping
 * 
 * This java class defines the getters and setters for the 
 *      
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 01/2010		Uma Josyula	Initial creation	
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.common.staticPointDataSource;


import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.IStaticPointDataSource.StaticPointDataSourceType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

// point data from common_obs_spatial

// TODO : add the capability to specify, query and store more than one field from the db
// (ie. the station id, name, country, elevation, 
public class StationsDbTablePointDataSource extends AbstractPointDataSource {
		
	private final String dbName = "metadata";

	private final String dbTableName = "common_obs_spatial";

	
	// an integer or one of recognize some of the common strings and convert
	// ie "METAR" UAIR....
	private String catType = "*";
	
	private String dbFieldName = "stationid";
	
	private List<LabeledPoint> stationPoints = new ArrayList<LabeledPoint>();
	
	public StationsDbTablePointDataSource( String srcName, String dbfld ) { 
	
		// The name indicates which catalogType to query. These can either
		// be from the ObStation constants or another meaningful string for convenience
		if( srcName.startsWith("CAT_TYPE") ) {
			srcName = srcName.substring( 8 );
		}

		//		CAT_TYPE_ICAO = 1;
		if( srcName.equalsIgnoreCase("ICAO" ) ||
			srcName.equalsIgnoreCase("METAR") ) {
			catType = "1";
		}
		//CAT_TYPE_SAO = 2; Legacy SAO identifiers - Still some around
		else if( srcName.equalsIgnoreCase("SAO" )  ) {
			catType = "2";
		}
		// CAT_TYPE_WFOID = 10; National Weather Service Weather Forecast Office (WFO) locations.
		else if( srcName.equalsIgnoreCase("WFOID" )  ) {
			catType = "10";
		}
		// CAT_TYPE_NEXRAD = 11; National Weather Service NEXRAD radar sites.
		else if( srcName.equalsIgnoreCase("NEXRAD" )  ) {
			catType = "11";
		}
		// CAT_TYPE_PROF = 12; Profiler site locations
		else if( srcName.equalsIgnoreCase("PROF" ) ||
				 srcName.equalsIgnoreCase("PROFILER" ) ) {
			catType = "12";
		}
		// CAT_TYPE_SFC_FXD = 20; WMO fixed surface synoptic locations
		else if( srcName.equalsIgnoreCase("SFC_FXD" ) ||
				 srcName.equalsIgnoreCase("SFC_FIXED" ) ||
				 srcName.equalsIgnoreCase("SYNOP" ) ||
				 srcName.equalsIgnoreCase("SYNOP_FIXED" ) ) {
			catType = "20";
		}
		// CAT_TYPE_SFC_MOB = 21; WMO mobil surface synoptic locations
		else if( srcName.equalsIgnoreCase("SYNOP_MOBILE" ) ||
				 srcName.equalsIgnoreCase("SFC_MOB" ) ) {
			catType = "21";
		}
		// CAT_TYPE_SFC_RAOB = 22; WMO fixed upperair locations
		else if( srcName.equalsIgnoreCase("RAOB") ||
				 srcName.equalsIgnoreCase("UAIR") ||
				 srcName.equalsIgnoreCase("UPPER_AIR") ) {
			catType = "22";
		}
		//  CAT_TYPE_SHIP_MOB = 30; Known ship identifications - Mobile no lat/lon
		else if( srcName.equalsIgnoreCase("SHIP" )  ) {
			catType = "30";
		}
		// CAT_TYPE_BUOY_MOB = 31; Drifting buoy locations
		else if( srcName.equalsIgnoreCase("BUOY_MOB" )  ) {
			catType = "31";
		}
		//  CAT_TYPE_BUOY_FXD = 32; Moored (Fixed) buoy locations
		else if( srcName.equalsIgnoreCase("BUOY_FXD") ||
				 srcName.equalsIgnoreCase("BUOY_FIXED") ||
				 srcName.equalsIgnoreCase("BUOY") ) {
			catType = "32";
		}
		//  CAT_TYPE_CMAN = 33; Coastal Marine (CMAN) locations.
		else if( srcName.equalsIgnoreCase("CMAN" )  ) {
			catType = "33";
		}
		//  CAT_TYPE_ACFT_WAYPT = 100; Aircraft waypoint locations
		else if( srcName.equalsIgnoreCase("ACFT_WAYPT" ) ||
				 srcName.equalsIgnoreCase("ACFT_WAYPOINT" ) ) {
			catType = "100";
		}
		// CAT_TYPE_ACFT_PIREP = 101; Aircraft pirep locations
		else if( srcName.equalsIgnoreCase("ACFT_PIREP" )  ) {
			catType = "101";
		}
		// CAT_TYPE_MESONET = 1000;
		else if( srcName.equalsIgnoreCase("MESONET" )  ) {
			catType = "1000";
		}
		// MESONET_NWSFAA = 1001;
		else if( srcName.equalsIgnoreCase("MESONET_NWSFAA" )  ) {
			catType = "1001";
		}
		else {// if its an int just use it
			try {
				Integer.parseInt( srcName );
				catType = srcName;
			}
			catch( NumberFormatException e) {
				System.out.println("Warning expecting int for catalogType in PointDataSource");
				catType = "*";
			}
		}
		
		dbFieldName = dbfld;
	}
	
	@Override
	public StaticPointDataSourceType getSourceType() {
		return StaticPointDataSourceType.STATIONS_DB_TABLE;
	} 

	// TODO : we could add a flag to not load all the data in memory and instead
	// query the database each time.
	@Override
	public void loadData( ) throws VizException {
		String geomField = (catType.equals("22") ? "upperairgeom" : "the_geom" );
		
		String sql = "SELECT AsBinary("+ geomField + "),"+dbFieldName+" FROM "+dbTableName+
				     " WHERE catalogtype = "+ catType;

		try {
            List<Object[]> stnList = DirectDbQuery.executeQuery( sql, dbName, 
            		DirectDbQuery.QueryLanguage.SQL );

            // We could validate the stations by calling createGeomProvider but there isn't anything 
            // we can do to fix the table and it will take time so just wait to validate til its needed.
            for( Object[] stnData : stnList ) {
                if( stnData == null || stnData.length != 2 ) {
            		System.out.println("Error querying common obs spatial. wrong # of expected return values??");
                    continue;
                }
                else if( !(stnData[0] instanceof byte[] ) || 
                		 !(stnData[1] instanceof String) ) {
            		System.out.println("Error querying common obs spatial. wrong # of expected return values??");
                	continue;
                }
                
                byte[] bytes = (byte[]) stnData[0];
                WKBReader reader = new WKBReader();
                Geometry geo = null;
                
                try {
                    geo = reader.read(bytes);
                    Coordinate loc = geo.getCoordinate();
                    
                    LabeledPoint lp = new LabeledPoint( (String)stnData[1], loc.y, loc.x );
                    insertPoint( lp );
                    stationPoints.add( lp );
                } 
                catch (ParseException e) {
                	System.out.println("Error querying common obs spatial. error parsing Geometry coord?");
                    continue;
                }                    	
            }		
            
        } catch (Exception e) {
        	throw new VizException( e );
        }
	}

	@Override
	public List<LabeledPoint> getPointData() {
		return stationPoints;
	}

	// not implemented
	@Override
	public Map<String, LabeledPoint> getPointDataByLabel() {
		return null;
	}

}
