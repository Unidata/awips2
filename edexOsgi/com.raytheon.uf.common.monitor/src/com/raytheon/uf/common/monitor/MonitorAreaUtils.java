package com.raytheon.uf.common.monitor;

import java.io.IOException;
import java.util.ArrayList;

import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.xml.StationIdXML;
import com.raytheon.uf.common.site.SiteMap;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKTWriter;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * (previous history missing)
 * Apr 29, 2011 DR#8986   zhao       Read in "counties", not "forecast zones", 
 * Feb 22, 2012 14413     zhao       modified getAdjacentZones to add "C" or "Z"
 * 
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
public class MonitorAreaUtils {
	
	public static final String COUNTY_TABLE = "mapdata.county";

	public static final String FORECAST_ZONE_TABLE = "mapdata.zone";

	public static final String MARINE_ZONE_TABLE = "mapdata.marinezones";

	public static final String MAPS_DB = "maps";

	public static final String OBS = "obs";

	public static final String META_DB = "metadata";

    /**
     * Get a list of counties from database for given cwa
     * [DR#8986]
     *  
     * @param cwa
     * @return
     */
	public static ArrayList<String> getUniqueCounties(String cwa) {
		ArrayList<String> counties = new ArrayList<String>();
		String sql = "select distinct state, fips from "
				+ MonitorConfigurationManager.COUNTY_TABLE + " where cwa = '"
				+ cwa + "' order by fips";

		ISpatialQuery sq = null;

		try {
			sq = SpatialQueryFactory.create();
			Object[] results = sq.dbRequest(sql, "maps");

			if (results.length > 0) {
				for (int i = 0; i < results.length; i++) {
					Object[] oa = (Object[]) results[i];
					String state = (String) oa[0];
					String fips = (String) oa[1];

					counties.add(state + "C" + fips.substring(2));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return counties;
	}
    
    /**
     * Get a list of forecast zones from database for given cwa
     * [DR#9905]
     *  
     * @param cwa
     * @return
     */
	public static ArrayList<String> getForecastZones(String cwa) {
		ArrayList<String> zones = new ArrayList<String>();
		String sql = "select distinct state, zone from "
				+ MonitorConfigurationManager.FORECAST_ZONE_TABLE + " where cwa = '"
				+ cwa + "' order by state, zone";

		ISpatialQuery sq = null;

		try {
			sq = SpatialQueryFactory.create();
			Object[] results = sq.dbRequest(sql, "maps");

			if (results.length > 0) {
				for (int i = 0; i < results.length; i++) {
					Object[] oa = (Object[]) results[i];
					String state = (String) oa[0];
					String zone = (String) oa[1];

					zones.add(state + "Z" + zone);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return zones;
	}

	/**
     * Gets the marine zones in the CWA
     * 
     * @param cwa
     *            The county warning area
     * @return
     */
    public static ArrayList<String> getMarineZones(String cwa) {
        ArrayList<String> zones = new ArrayList<String>();
        String sql = "select distinct id from " + MonitorConfigurationManager.MARINE_ZONE_TABLE + " where wfo = '"
                + cwa + "' order by id";

        ISpatialQuery sq = null;
        
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, MAPS_DB);

            if (SpatialQueryFactory.getType().equals("CAVE")) {
                if (results.length > 0) {
                    for (int i = 0; i < results.length; i++) {
                        Object[] oa = (Object[]) results[i];
                        zones.add((String) oa[0]);
                    }
                }
            }
            else {
                if (results.length > 0) {
                    for (int i = 0; i < results.length; i++) {
						zones.add((String) results[i]);
                    }
                }
            }
            
        } catch (Exception e) {
            e.printStackTrace();
        }        

        return zones;
    }

    /**
     * 
     * @param zone : zone ID (either a county zone or a maritime zone) 
     * @return a list of stations (with station ID and station type) associated with the zone 
     * @throws Exception
     */
	public static ArrayList<StationIdXML> getZoneReportingStationXMLs(
			String zone) throws Exception {
		
		ArrayList<StationIdXML> stations = new ArrayList<StationIdXML>();

		try {
			String zoneEnvelope = getZoneEnvelope(zone); 

			String sql = "select catalogtype, stationid " + "from common_obs_spatial " + "where "
				+ "ST_Contains(ST_GeomFromText('" + zoneEnvelope + "', -1), the_geom) " 
					+ "and (catalogtype = 1 or catalogtype = 33 or catalogtype = 32 or catalogtype = 1000) order by stationid asc";

			ISpatialQuery sq = SpatialQueryFactory.create();
			Object[] results = sq.dbRequest(sql, META_DB);
			if (results.length != 0) {
				StationIdXML stn = null;
				if (results[0] instanceof Object[]) {
					for (int i = 0; i < results.length; i++) {
						Object[] objs = (Object[]) results[i];
						stn = setStation(Integer.parseInt(objs[0].toString()),
								objs[1].toString());
						if ( !stations.contains(stn) ) {
							stations.add(stn);
						}
					}
				} else {
					stn = setStation(Integer.parseInt(results[0].toString()),
							results[1].toString());
					if ( !stations.contains(stn) ) {
						stations.add(stn);
					}
				}
			}
		} catch (Exception e) {
			System.err.println("============== zone = " + zone ); 
			e.printStackTrace();
			throw new Exception("Unable to query for Zone envelope and stations list", e);
		}
		return stations;
	}

	private static StationIdXML setStation(Integer catalogType, String stationId) {

		StationIdXML station = new StationIdXML();
		station.setName(stationId);
		if (catalogType.intValue() == 1) {
			station.setType("METAR");
		} else if (catalogType.intValue() == 33 || catalogType == 32) {
			station.setType("MARITIME");
		} else if (catalogType.intValue() == 1000) {
            station.setType("MESONET");
		} else {
			// other types ???
		}

		return station;
	}

	/**
	 * gets the zone obs list
	 * 
	 * @param zone
	 * @return
	 * @throws VizException
	 */
	public static ArrayList<String> getZoneReportingStations(String zone)
			throws Exception {
		ArrayList<String> stations = new ArrayList<String>();

		try {
			String zoneEnvelope = getZoneEnvelope(zone); 

			String sql = "select icao " + "from common_obs_spatial " + "where "
					+ "ST_Contains(ST_GeomFromText('" + zoneEnvelope 
					+ "', -1), the_geom) order by icao asc";

			ISpatialQuery sq = SpatialQueryFactory.create();
			Object[] results = sq.dbRequest(sql, META_DB);

			if (SpatialQueryFactory.getType().equals("CAVE")) {
				if (results.length != 0) {
			        if (results.length == 1) {
			            if (results[0] != null) {
			                if (!results[0].equals("")) {
			                    stations.add((String) results[0]);
			                }
			            }
			        } else {
			            for (int i = 0; i < results.length; i++) {
			                Object[] objs = (Object[]) results[i];
			                if (objs[0] != null) {
			                    if (!objs[0].equals("")) {
			                        stations.add((String) objs[0]);
			                    }
			                }
			            }
			        }
			    }
			} else {
			    if (results.length > 0) {
			        for (int i = 0; i < results.length; i++) {
			            String obj = (String) results[i];
			            if ((obj != null) && !obj.equals("")) {
			                stations.add(obj);
			            }
			        }
			    }
			}
			
		} catch (Exception e) {
			System.err.println("============== zone = " + zone ); 
			e.printStackTrace();
			throw new Exception("Unable to query for Zone envelope and stations list", e);
		}
		return stations;
	}

	/**
	 * Gets the envelope of the zone in the CWA
	 * 
	 * @param cwa
	 * @return
	 */
	private static String getZoneEnvelope(String zone) throws Exception {

		WKBReader wkbReader = new WKBReader();
		Geometry geo = null;

		String sql = null;
		/**
		 * [DR#9905: Read in data from the "county" table for a CONUS site, 
		 * from "forecast zone" table for an OCONUS site] 
		 */
		if (isMarineZone(zone)) {
			sql = "select distinct AsBinary(" 
			        + ScanUtils.getStandardResolutionLevel("marinezones") 
			        + ") from " + MonitorConfigurationManager.MARINE_ZONE_TABLE 
			        + " where id = '" + zone + "'";
		} else if ( zone.charAt(2) == 'Z' ) { // "forecast zone" 
			/**
			 * The first two characters in a zone indicate "state", and the last 
			 * three characters zone ID; e.g., AKZ101, where the first two characters 
			 * indicate state "AK" and the last three characters "101" indicate zone ID 
			 */
			String state_zone = zone.substring(0, 2) + zone.substring(3); 
			sql = "select AsBinary(" 
			        + ScanUtils.getStandardResolutionLevel("zone") 
			        + ") from " + MonitorConfigurationManager.FORECAST_ZONE_TABLE 
					+ " where state_zone = '" + state_zone + "'";
		} else {
			/**
			 * The first two characters in a zone indicate "state", and the last 
			 * three characters are the same as the last three characters of "fips" in 
			 * the county database table; e.g., IAC001, where the first two characters 
			 * indicate state "IA" and the last three characters "001" are the same 
			 * as the last three characters "001" of the "fips = '19001'"
			 */
			String state = zone.substring(0, 2);	
			String fipsLike = "%" + zone.substring(3); 
			sql = "select distinct AsBinary(" 
			        + ScanUtils.getStandardResolutionLevel("county") 
			        + ") from " + MonitorConfigurationManager.COUNTY_TABLE 
					+ " where state = '" + state + "' and fips like '" + fipsLike + "'";

		}				

        ISpatialQuery sq = SpatialQueryFactory.create();
        Object[] results = sq.dbRequest(sql, MAPS_DB);
        if (results.length > 0) {
            if (results[0] instanceof Object[]) {
                Object obj[] = (Object[]) results[0];
                geo = readGeometry(obj[0], wkbReader);
            } else {
                geo = readGeometry(results[0], wkbReader);
            }
        }
		
        return getPolygonText(geo);
	}

	/**
	 * extract geometry
	 * 
	 * @param obj
	 * @return
	 */
	public static Geometry readGeometry(Object obj, WKBReader wkbReader) {
		Geometry geometry = null;
		try {
			geometry = wkbReader.read((byte[]) obj);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return geometry.buffer(0);
	}

	/**
	 * Gets the text string of the polygon describing the areal coverage
	 * 
	 * @param Geometry
	 * @return
	 * @throws IOException
	 */
	public static String getPolygonText(Geometry geometry) {

		WKTWriter wktWriter = new WKTWriter();
		return wktWriter.writeFormatted(geometry);
	}

	public static ArrayList<String> getAdjacentZones(String[] cwaList) {
		ArrayList<String> zones = new ArrayList<String>();

		String sqlCounty = "select state, fips from "
				+ MonitorConfigurationManager.COUNTY_TABLE + " where cwa in (''";
		String sqlForecastZone = "select state, zone from "
			+ MonitorConfigurationManager.FORECAST_ZONE_TABLE + " where cwa in (''";
		String sqlMaritimeZone = "select id from "
				+ MonitorConfigurationManager.MARINE_ZONE_TABLE
				+ " where wfo in (''";
		for (int i = 0; i < cwaList.length; i++) {
			if ( SiteMap.getInstance().getSite4LetterId(cwaList[i]).charAt(0) == 'K' ) { // "CONUS site 
				sqlCounty += ", '" + cwaList[i] + "'";
			} else { // OCONUS site
				sqlForecastZone += ", '" + cwaList[i] + "'";
			}
			sqlMaritimeZone += ", '" + cwaList[i] + "'";
		}

		sqlCounty += ") order by fips";
		sqlForecastZone += ") order by state, zone"; 
		sqlMaritimeZone += ") order by id";

		ISpatialQuery sq = null;

		try {
			sq = SpatialQueryFactory.create();
			Object[] resultsCounty = sq.dbRequest(sqlCounty, "maps");
			Object[] resultsForecastZone = sq.dbRequest(sqlForecastZone, "maps");
			Object[] resultsMaritimeZone = sq.dbRequest(sqlMaritimeZone, "maps");

			if (SpatialQueryFactory.getType().equals("CAVE")) {
				if (resultsCounty.length > 0) {
					for (int i = 0; i < resultsCounty.length; i++) {
						Object[] oa = (Object[]) resultsCounty[i];
						String state = (String) oa[0];
						String fips = (String) oa[1];
						zones.add(state + "C" + fips.substring(2));
					}
				}
				if (resultsForecastZone.length > 0) {
					for (int i = 0; i < resultsForecastZone.length; i++) {
						Object[] oa = (Object[]) resultsForecastZone[i];
						String state = (String) oa[0];
						String zone = (String) oa[1];
						zones.add(state + "Z" + zone);
					}
				}
				if (resultsMaritimeZone.length > 0) {
					for (int i = 0; i < resultsMaritimeZone.length; i++) {
						Object[] oa = (Object[]) resultsMaritimeZone[i];
						zones.add((String) oa[0]);
					}
				}
			} else {
				if (resultsCounty.length > 0) {
					for (int i = 0; i < resultsCounty.length; i++) {
						Object[] oa = (Object[]) resultsCounty[i];
						String czn = oa[0].toString() + "C" + oa[1].toString().substring(2);
						zones.add(czn);
					}
				}
				if ( resultsForecastZone.length > 0 ) {
					for (int i = 0; i < resultsForecastZone.length; i++) {
						Object[] oa = (Object[]) resultsForecastZone[i];
						String fcz = oa[0].toString() + "Z" + oa[1].toString();
						zones.add(fcz);
					}
				}
				if ( resultsMaritimeZone.length > 0 ) {
					for (int i = 0; i < resultsMaritimeZone.length; i++) {
						String mzn = resultsMaritimeZone[i].toString();
						zones.add(mzn);
					}
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}

		return zones;
	}
	
	/**
	 * Gets the geometry of the zone in the CWA
	 * 
	 * @param cwa
	 * @return
	 */
	public static Geometry getZoneGeometry(String zone) throws Exception {

		WKBReader wkbReader = new WKBReader();
		Geometry geo = null;
		/**
		 * DR#9905: "county" for a CONUS site; 'forecast zone" for an OCONUS site
		 */
		String sql = "";
		ISpatialQuery sq = null;
		if (isMarineZone(zone)) {
			sql = "select AsBinary(" + ScanUtils.getStandardResolutionLevel("marineZones") + ") from " + MARINE_ZONE_TABLE
					+ " where id = '" + zone + "'";
		} else if ( zone.charAt(2) == 'Z' ) { // "forecast zone" 
			String state_zone = zone.substring(0, 2) + zone.substring(3); 
			sql = "select AsBinary(" + ScanUtils.getStandardResolutionLevel("zone") + ") from " + FORECAST_ZONE_TABLE
					+ " where state_zone = '" + state_zone + "'";
		} else { // "county"
			String state = zone.substring(0, 2);
			String fipsLike = "%" + zone.substring(3);
			sql = "select AsBinary(" + ScanUtils.getStandardResolutionLevel("county") + ") from " + COUNTY_TABLE
					+ " where state = '" + state + "' and fips like '"
					+ fipsLike + "'";
		}

		// List<Object[]> results = DirectDbQuery.executeQuery(sql, MAPS_DB,
		// QueryLanguage.SQL);
		sq = SpatialQueryFactory.create();
		Object[] results = sq.dbRequest(sql, MAPS_DB);
		if (results.length > 0) {
            if (results[0] instanceof Object[]) {
                Object obj[] = (Object[]) results[0];
                geo = readGeometry(obj[0], wkbReader);
            } else {
                geo = readGeometry(results[0], wkbReader);
            }
		}

		return geo;
	}

	/**
	 * Is a zone a Marine zone or not?
	 * 
	 * @param zone
	 * @return true if the zone is a marine zone
	 * @throws SpatialException
	 * @throws VizException
	 */
	public static boolean isMarineZone(String zone) throws SpatialException {
		String sql = "select distinct id from " + MARINE_ZONE_TABLE + " where id = '"
				+ zone + "'";
		ISpatialQuery sq = null;
		// List<Object[]> results = DirectDbQuery.executeQuery(sql, MAPS_DB,
		// QueryLanguage.SQL);
		sq = SpatialQueryFactory.create();
		Object[] results = sq.dbRequest(sql, MAPS_DB);
		if ((results.length > 0) && results[0].equals(zone)) {
			return true;
		}

		return false;
	}

	public static double[] getZoneCenter(String zone) throws Exception {
		double[] zoneCenter = null;
		/**
		 * DR#9905: "county" for a CONUS site; "forecast zone" for an OCONUS site
		 */
		String sql = "";
		ISpatialQuery sq = null;
		if (isMarineZone(zone)) { // "marine zone"
			sql = "select distinct lat, lon from " + MARINE_ZONE_TABLE + " where id = '"
					+ zone + "'";
		} else if ( zone.charAt(2) == 'Z' ) { // "forecast zone"
			String state_zone = zone.substring(0, 2) + zone.substring(3);
			sql = "select lat, lon from " + FORECAST_ZONE_TABLE + " where state_zone = '" + state_zone + "'"; 
		} else { // "county" 
			String state = zone.substring(0, 2);
			String fipsLike = "%" + zone.substring(3);
			sql = "select lat, lon from " + COUNTY_TABLE + " where state = '"
					+ state + "' and fips like '" + fipsLike + "'";
		}

		sq = SpatialQueryFactory.create();
		Object[] results = sq.dbRequest(sql, MAPS_DB);

        Double lat = null;
        Double lon = null;

        if (results[0] instanceof Object[]) {
            Object[] res = (Object[]) results[0];
            lat = ((Number) res[0]).doubleValue();
            lon = ((Number) res[1]).doubleValue();
        } else {
            lat = ((Number) results[0]).doubleValue();
            lon = ((Number) results[1]).doubleValue();
        }

        zoneCenter = new double[] { lon, lat };
		return zoneCenter;
	}

}
