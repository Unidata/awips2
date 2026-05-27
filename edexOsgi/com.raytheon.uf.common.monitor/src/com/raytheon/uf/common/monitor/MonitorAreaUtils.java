package com.raytheon.uf.common.monitor;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.xml.StationIdXML;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKBReader;
import org.locationtech.jts.io.WKTWriter;

/**
 * Monitor Area Utilities.
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
 * Apr 30, 2014  3086     skorolev   Replaced MonitorConfigurationManager with FSSObsMonitorConfigurationManager
 * Oct 17, 2014 2757      skorolev   Corrected SQL in the getAdjacentZones to avoid duplicates.
 * Nov 03, 2014 3741      skorolev   Updated getZoneCenter and added getStationCenter methods.
 * Jul 11, 2018 7175      tgurney    Fixed centering on marine stations
 *
 * </pre>
 *
 * @author mpduff
 */
public class MonitorAreaUtils {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MonitorAreaUtils.class);

    public static final String COUNTY_TABLE = "mapdata.county";

    public static final String FORECAST_ZONE_TABLE = "mapdata.zone";

    public static final String MARINE_ZONE_TABLE = "mapdata.marinezones";

    public static final String MAPS_DB = "maps";

    public static final String OBS = "obs";

    public static final String META_DB = "metadata";

    /*
     * Order is important here, it determines which geom to use when multiple
     * geoms are associated with a single station id in common_obs_spatial
     */
    private static String[] CATALOG_TYPES = new String[] {
            ObStation.CAT_TYPE_ICAO.toString(),
            ObStation.CAT_TYPE_BUOY_FXD.toString(),
            ObStation.CAT_TYPE_CMAN.toString(),
            ObStation.CAT_TYPE_MESONET.toString() };

    private static final String CATALOG_TYPE_CONDITION_SQL = " catalogtype in ("
            + String.join(",", CATALOG_TYPES) + ") ";

    /**
     * Get a list of counties from database for given cwa [DR#8986]
     *
     * @param cwa
     * @return
     */
    public static List<String> getUniqueCounties(String cwa) {
        List<String> counties = new ArrayList<>();
        String sql = "select distinct state, fips from "
                + FSSObsMonitorConfigurationManager.COUNTY_TABLE
                + " where cwa = '" + cwa + "' order by fips";

        ISpatialQuery sq = null;

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, "maps");

            if (results.length > 0) {
                for (Object result : results) {
                    Object[] oa = (Object[]) result;
                    String state = (String) oa[0];
                    String fips = (String) oa[1];

                    counties.add(state + "C" + fips.substring(2));
                }
            }
        } catch (SpatialException e) {
            statusHandler.error("Failed to get counties from database", e);
        }
        return counties;
    }

    /**
     * Get a list of forecast zones from database for given cwa [DR#9905]
     *
     * @param cwa
     * @return
     */
    public static List<String> getForecastZones(String cwa) {
        List<String> zones = new ArrayList<>();
        String sql = "select distinct state, zone from "
                + FSSObsMonitorConfigurationManager.FORECAST_ZONE_TABLE
                + " where cwa = '" + cwa + "' order by state, zone";

        ISpatialQuery sq = null;

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, "maps");

            if (results.length > 0) {
                for (Object result : results) {
                    Object[] oa = (Object[]) result;
                    String state = (String) oa[0];
                    String zone = (String) oa[1];

                    zones.add(state + "Z" + zone);
                }
            }
        } catch (SpatialException e) {
            statusHandler.error("Failed to get forecast zones from database",
                    e);
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
    public static List<String> getMarineZones(String cwa) {
        List<String> zones = new ArrayList<>();
        String sql = "select distinct id from "
                + FSSObsMonitorConfigurationManager.MARINE_ZONE_TABLE
                + " where wfo = '" + cwa + "' order by id";

        ISpatialQuery sq = null;

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, MAPS_DB);

            if ("CAVE".equals(SpatialQueryFactory.getType())) {
                if (results.length > 0) {
                    for (Object result : results) {
                        Object[] oa = (Object[]) result;
                        zones.add((String) oa[0]);
                    }
                }
            } else {
                if (results.length > 0) {
                    for (Object result : results) {
                        zones.add((String) result);
                    }
                }
            }

        } catch (SpatialException e) {
            statusHandler.error("Failed to get marine zones from database", e);
        }

        return zones;
    }

    /**
     *
     * @param zone
     *            : zone ID (either a county zone or a maritime zone)
     * @return a list of stations (with station ID and station type) associated
     *         with the zone
     * @throws ParseException
     */
    public static List<StationIdXML> getZoneReportingStationXMLs(String zone)
            throws ParseException {

        List<StationIdXML> stations = new ArrayList<>();

        try {
            String zoneEnvelope = getZoneEnvelope(zone);

            StringBuilder sql = new StringBuilder();
            sql.append("select catalogtype, stationid ")
                    .append("from common_obs_spatial where ")
                    .append("ST_Contains(ST_GeomFromText('")
                    .append(zoneEnvelope).append("', -1), the_geom) and ")
                    .append(CATALOG_TYPE_CONDITION_SQL)
                    .append(" order by stationid asc");

            ISpatialQuery sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql.toString(), META_DB);
            if (results.length != 0) {
                StationIdXML stn = null;
                if (results[0] instanceof Object[]) {
                    for (Object result : results) {
                        Object[] objs = (Object[]) result;
                        stn = setStation(Integer.parseInt(objs[0].toString()),
                                objs[1].toString());
                        if (!stations.contains(stn)) {
                            stations.add(stn);
                        }
                    }
                } else {
                    stn = setStation(Integer.parseInt(results[0].toString()),
                            results[1].toString());
                    if (!stations.contains(stn)) {
                        stations.add(stn);
                    }
                }
            }
        } catch (SpatialException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error retrieving station info for zone " + zone, e);
        }
        return stations;
    }

    /**
     * Sets station type.
     *
     * @param catalogType
     * @param stationId
     * @return
     */
    private static StationIdXML setStation(Integer catalogType,
            String stationId) {

        StationIdXML station = new StationIdXML();
        station.setName(stationId);
        if (catalogType == ObStation.CAT_TYPE_ICAO) {
            station.setType("METAR");
        } else if (catalogType == ObStation.CAT_TYPE_CMAN
                || catalogType == ObStation.CAT_TYPE_BUOY_FXD) {
            station.setType("MARITIME");
        } else if (catalogType.intValue() == ObStation.CAT_TYPE_MESONET) {
            station.setType("MESONET");
        }
        return station;
    }

    /**
     * Gets the zone obs list
     *
     * @param zone
     * @return
     * @throws ParseException
     */
    public static List<String> getZoneReportingStations(String zone)
            throws ParseException {
        List<String> stations = new ArrayList<>();

        try {
            String zoneEnvelope = getZoneEnvelope(zone);

            StringBuilder sql = new StringBuilder();
            sql.append("select icao from common_obs_spatial where ")
                    .append("ST_Contains(ST_GeomFromText('")
                    .append(zoneEnvelope)
                    .append("', -1), the_geom) order by icao asc");

            ISpatialQuery sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql.toString(), META_DB);

            if ("CAVE".equals(SpatialQueryFactory.getType())) {
                if (results.length != 0) {
                    if (results.length == 1) {
                        if (results[0] != null) {
                            if (!"".equals(results[0])) {
                                stations.add((String) results[0]);
                            }
                        }
                    } else {
                        for (Object result : results) {
                            Object[] objs = (Object[]) result;
                            if (objs[0] != null) {
                                if (!"".equals(objs[0])) {
                                    stations.add((String) objs[0]);
                                }
                            }
                        }
                    }
                }
            } else {
                if (results.length > 0) {
                    for (Object result : results) {
                        String obj = (String) result;
                        if ((obj != null) && !"".equals(obj)) {
                            stations.add(obj);
                        }
                    }
                }
            }

        } catch (SpatialException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error retrieving stations for zone " + zone, e);
        }
        return stations;
    }

    /**
     * Gets the envelope of the zone in the CWA
     *
     * @param zone
     * @return the text string of the polygon describing the area coverage
     * @throws SpatialException
     * @throws ParseException
     */
    private static String getZoneEnvelope(String zone)
            throws SpatialException, ParseException {

        WKBReader wkbReader = new WKBReader();
        Geometry geo = null;

        String sql = null;
        /**
         * [DR#9905: Read in data from the "county" table for a CONUS site, from
         * "forecast zone" table for an OCONUS site]
         */
        if (isMarineZone(zone)) {
            sql = "select distinct ST_AsBinary("
                    + ScanUtils.getStandardResolutionLevel("marinezones")
                    + ") from "
                    + FSSObsMonitorConfigurationManager.MARINE_ZONE_TABLE
                    + " where id = '" + zone + "'";
        } else if (zone.charAt(2) == 'Z') {
            // "forecast zone"
            /**
             * The first two characters in a zone indicate "state", and the last
             * three characters zone ID; e.g., AKZ101, where the first two
             * characters indicate state "AK" and the last three characters
             * "101" indicate zone ID
             */
            String state_zone = zone.substring(0, 2) + zone.substring(3);
            sql = "select ST_AsBinary("
                    + ScanUtils.getStandardResolutionLevel("zone") + ") from "
                    + FSSObsMonitorConfigurationManager.FORECAST_ZONE_TABLE
                    + " where state_zone = '" + state_zone + "'";
        } else {
            /**
             * The first two characters in a zone indicate "state", and the last
             * three characters are the same as the last three characters of
             * "fips" in the county database table; e.g., IAC001, where the
             * first two characters indicate state "IA" and the last three
             * characters "001" are the same as the last three characters "001"
             * of the "fips = '19001'"
             */
            String state = zone.substring(0, 2);
            String fipsLike = "%" + zone.substring(3);
            sql = "select distinct ST_AsBinary("
                    + ScanUtils.getStandardResolutionLevel("county") + ") from "
                    + FSSObsMonitorConfigurationManager.COUNTY_TABLE
                    + " where state = '" + state + "' and fips like '"
                    + fipsLike + "'";

        }

        ISpatialQuery sq = SpatialQueryFactory.create();
        Object[] results = sq.dbRequest(sql, MAPS_DB);
        if (results.length > 0) {
            try {
                if (results[0] instanceof Object[]) {
                    Object obj[] = (Object[]) results[0];
                    geo = readGeometry(obj[0], wkbReader);
                } else {
                    geo = readGeometry(results[0], wkbReader);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error to read geometry for zone " + zone, e);
            }
        }
        return getPolygonText(geo);
    }

    /**
     * Extracts geometry.
     *
     * @param obj
     * @param wkbReader
     * @return
     * @throws ParseException
     */
    public static Geometry readGeometry(Object obj, WKBReader wkbReader)
            throws ParseException {
        Geometry geometry = null;
        geometry = wkbReader.read((byte[]) obj);
        return geometry.buffer(0);
    }

    /**
     * Gets the text string of the polygon describing the areal coverage
     *
     * @param geometry
     * @return
     */
    public static String getPolygonText(Geometry geometry) {
        WKTWriter wktWriter = new WKTWriter();
        return wktWriter.writeFormatted(geometry);
    }

    /**
     * Gets Adjacent Zones.
     *
     * @param cwaList
     * @return
     * @throws SpatialException
     */
    public static List<String> getAdjacentZones(String[] cwaList)
            throws SpatialException {
        List<String> zones = new ArrayList<>();

        StringBuilder sqlCounty = new StringBuilder();
        sqlCounty.append("select distinct state, fips from ")
                .append(FSSObsMonitorConfigurationManager.COUNTY_TABLE)
                .append(" where cwa in (''");
        StringBuilder sqlForecastZone = new StringBuilder();
        sqlForecastZone.append("select distinct state, zone from ")
                .append(FSSObsMonitorConfigurationManager.FORECAST_ZONE_TABLE)
                .append(" where cwa in (''");
        StringBuilder sqlMaritimeZone = new StringBuilder();
        sqlMaritimeZone.append("select distinct id from ")
                .append(FSSObsMonitorConfigurationManager.MARINE_ZONE_TABLE)
                .append(" where wfo in (''");
        for (String element : cwaList) {
            if (SiteMap.getInstance().getSite4LetterId(element)
                    .charAt(0) == 'K') {
                // CONUS site
                sqlCounty.append(", '").append(element).append("'");
            } else {
                // OCONUS site
                sqlForecastZone.append(", '").append(element).append("'");
            }
            sqlMaritimeZone.append(", '").append(element).append("'");
        }

        sqlCounty.append(") order by fips");
        sqlForecastZone.append(") order by state, zone");
        sqlMaritimeZone.append(") order by id");

        ISpatialQuery sq = null;

        sq = SpatialQueryFactory.create();
        Object[] resultsCounty = sq.dbRequest(sqlCounty.toString(), "maps");
        Object[] resultsForecastZone = sq.dbRequest(sqlForecastZone.toString(),
                "maps");
        Object[] resultsMaritimeZone = sq.dbRequest(sqlMaritimeZone.toString(),
                "maps");

        if ("CAVE".equals(SpatialQueryFactory.getType())) {
            if (resultsCounty.length > 0) {
                for (Object element : resultsCounty) {
                    Object[] oa = (Object[]) element;
                    String state = (String) oa[0];
                    String fips = (String) oa[1];
                    zones.add(state + "C" + fips.substring(2));
                }
            }
            if (resultsForecastZone.length > 0) {
                for (Object element : resultsForecastZone) {
                    Object[] oa = (Object[]) element;
                    String state = (String) oa[0];
                    String zone = (String) oa[1];
                    zones.add(state + "Z" + zone);
                }
            }
            if (resultsMaritimeZone.length > 0) {
                for (Object element : resultsMaritimeZone) {
                    Object[] oa = (Object[]) element;
                    zones.add((String) oa[0]);
                }
            }
        } else {
            if (resultsCounty.length > 0) {
                for (Object element : resultsCounty) {
                    Object[] oa = (Object[]) element;
                    String czn = oa[0].toString() + "C"
                            + oa[1].toString().substring(2);
                    zones.add(czn);
                }
            }
            if (resultsForecastZone.length > 0) {
                for (Object element : resultsForecastZone) {
                    Object[] oa = (Object[]) element;
                    String fcz = oa[0].toString() + "Z" + oa[1].toString();
                    zones.add(fcz);
                }
            }
            if (resultsMaritimeZone.length > 0) {
                for (Object element : resultsMaritimeZone) {
                    String mzn = element.toString();
                    zones.add(mzn);
                }
            }
        }
        return zones;
    }

    /**
     * Gets the geometry of the zone in the CWA
     *
     * @param zone
     * @return
     * @throws SpatialException
     */
    public static Geometry getZoneGeometry(String zone)
            throws SpatialException {

        WKBReader wkbReader = new WKBReader();
        Geometry geo = null;
        /**
         * DR#9905: "county" for a CONUS site; 'forecast zone" for an OCONUS
         * site
         */
        StringBuilder sql = new StringBuilder();
        ISpatialQuery sq = null;
        if (isMarineZone(zone)) {
            sql.append("select ST_AsBinary(")
                    .append(ScanUtils.getStandardResolutionLevel("marinezones"))
                    .append(") from ").append(MARINE_ZONE_TABLE)
                    .append(" where id = '").append(zone).append("'");
        } else if (zone.charAt(2) == 'Z') {
            // "forecast zone"
            String state_zone = zone.substring(0, 2) + zone.substring(3);
            sql.append("select ST_AsBinary(")
                    .append(ScanUtils.getStandardResolutionLevel("zone"))
                    .append(") from ").append(FORECAST_ZONE_TABLE)
                    .append(" where state_zone = '").append(state_zone)
                    .append("'");
        } else {
            // "county"
            String state = zone.substring(0, 2);
            String fipsLike = "%" + zone.substring(3);
            sql.append("select ST_AsBinary(")
                    .append(ScanUtils.getStandardResolutionLevel("county"))
                    .append(") from ").append(COUNTY_TABLE)
                    .append(" where state = '" + state)
                    .append("' and fips like '").append(fipsLike).append("'");
        }

        sq = SpatialQueryFactory.create();
        Object[] results = sq.dbRequest(sql.toString(), MAPS_DB);
        if (results.length > 0) {
            try {
                if (results[0] instanceof Object[]) {
                    Object obj[] = (Object[]) results[0];
                    geo = readGeometry(obj[0], wkbReader);
                } else {
                    geo = readGeometry(results[0], wkbReader);
                }
            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error to read geometry for zone " + zone, e);
            }
        }
        return geo;
    }

    /**
     * Is a zone a Marine zone or not?
     *
     * @param zone
     * @return
     * @throws SpatialException
     */
    public static boolean isMarineZone(String zone) throws SpatialException {
        String sql = "select distinct id from " + MARINE_ZONE_TABLE
                + " where id = '" + zone + "'";
        ISpatialQuery sq = null;
        sq = SpatialQueryFactory.create();
        Object[] results = sq.dbRequest(sql, MAPS_DB);
        if ((results.length > 0) && results[0].equals(zone)) {
            return true;
        }
        return false;
    }

    /**
     * Gets Zone Center.
     *
     * @param zone
     * @return
     * @throws SpatialException
     */
    public static Coordinate getZoneCenter(String zone)
            throws SpatialException {
        Coordinate zoneCenter = null;
        Geometry geom = getZoneGeometry(zone);
        if (geom != null) {
            Point ctrd = geom.getCentroid();
            if (ctrd != null) {
                zoneCenter = ctrd.getCoordinate();
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Problem to get the coordinates for zone " + zone);
            }
        }
        return zoneCenter;
    }

    /**
     * Get station coordinates.
     *
     * @param stationid
     * @return
     * @throws SpatialException
     * @throws ParseException
     */
    public static Coordinate getStationCenter(String stationid)
            throws SpatialException, ParseException {

        ISpatialQuery sq = null;
        String sql = "select catalogtype, ST_AsBinary(the_geom) "
                + " from common_obs_spatial where stationid = '" + stationid
                + "' and " + CATALOG_TYPE_CONDITION_SQL + ";";
        sq = SpatialQueryFactory.create();
        Object[] results = sq.dbRequest(sql, "metadata");
        if (results.length > 0) {
            if (!(results[0] instanceof Object[])) {
                // only one result row
                WKBReader wkbReader = new WKBReader();
                Geometry stationGeo = wkbReader.read((byte[]) results[1]);
                return stationGeo.getCoordinate();
            }
            for (String catalogType : CATALOG_TYPES) {
                for (Object result : results) {
                    Object[] row = (Object[]) result;
                    Integer thisCatalogType = (Integer) row[0];
                    if (catalogType.equals(thisCatalogType.toString())) {
                        WKBReader wkbReader = new WKBReader();
                        Geometry stationGeo = wkbReader.read((byte[]) row[1]);
                        return stationGeo.getCoordinate();
                    }
                }
            }
        }
        return null;
    }
}
