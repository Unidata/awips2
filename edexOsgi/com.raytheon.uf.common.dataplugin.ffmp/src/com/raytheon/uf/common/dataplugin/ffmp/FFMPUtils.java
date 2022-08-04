/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.dataplugin.ffmp;

import java.awt.geom.Point2D;
import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.request.GetRadarSpatialRequest;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderMode;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.geospatial.request.SpatialDbQueryRequest;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.GuidanceType;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKBReader;
import org.locationtech.jts.io.WKTWriter;

/**
 * FFMPUtils
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jun 22, 2009  2152     D. Hladky  Initial release
 * Jun 18, 2012  15108    G. Zhang   Fix County FIPS 4-digit issue
 * Jan 02, 2013  1569     D. Hladky  constants, arraylist to list and moved
 *                                   common menthods here
 * Mar 01, 2013  13228    G. Zhang   Add state for VGB query and related code
 * Mar 18, 2013  1817     D. Hladky  Fixed issue with BOX where only 1 HUC was
 *                                   showing up.
 * Aug 20, 2013  2250     mnash      Fixed incorrect return types for database
 *                                   queries.
 * Sep 05, 2014  17346    G. Zhang   Fixed issue with DB return types.
 * Apr 21, 2014  2060     njensen    Remove dependency on grid dataURI column
 * Apr 22, 2014  2984     njensen    Remove dependency on edex/CoreDao
 * Nov 18, 2014  3831     dhladky    StatusHandler logging. Proper list sizing.
 * Jul 13, 2015  4500     rjpeter    Fix SQL Injection concerns.
 * Aug 08, 2015  4722     dhladky    Added Grid coverage and parsing methods.
 * Sep 17, 2015  4756     dhladky    Multiple guidance source bugs.
 * Feb 12, 2016  5370     dhladky    Camel case for insertTime.
 * Apr 07, 2016  5491     tjensen    Fix NullPointerException from
 *                                   getRawGeometries
 * May 11, 2017  6266     nabowle    Use GetRadarSpatialRequest in
 *                                   getRadarCenter().
 * Jun 20, 2018  6641     njensen    Delete dead code
 * Jul 20, 2018  6642     randerso   Removed buffer(0) from readGeometry. Code
 *                                   cleanup.
 * Aug 06, 2018  6642     randerso   Changed to use executeRequest() instead of
 *                                   deprecated dbRequest()
 * Oct 18, 2018  11861    mfontaine  FFMP use of QPF in Basin Table
 * Jul 15, 2019  7627     mroos      Changed KmToDegrees to public for use in 
 *                                   external classes
 *
 * </pre>
 *
 * @author dhladky
 */

public class FFMPUtils {

    /**
     * schema name for metadata database
     */
    public static final String META_DB = "metadata";

    /**
     * Missing data value
     */
    public static final float MISSING = -99999.0f;

    private static final GeometryFactory factory = new GeometryFactory();

    private static final String FFMP_TABLE = "mapdata.ffmp_basins";

    private static final String MAPS_DB = "maps";

    public static final double KmToDegrees = 111.12;

    private static final NumberFormat formatter = new DecimalFormat("#.##");

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPUtils.class);

    /**
     * Gets the base level basins in the County Warning Area
     *
     * @param cwa
     * @return the virtual gage basins
     */
    public static LinkedHashMap<String, FFMPVirtualGageBasinMetaData> getVirtualGageBasins(
            String cwa) {

        LinkedHashMap<String, FFMPVirtualGageBasinMetaData> virtualBasins = null;
        /*
         * DR 13228 state added to the below query
         */
        String sql = "SELECT lid, county, name, lat, lon, state FROM location "
                + "where lid in (select distinct(lid) from IngestFilter "
                + "where pe in ('PC', 'PP') and ingest = 'T' and dur < 2000)";
        try {
            Object[] results = executeSqlQuery(sql, ShefConstants.IHFS);
            virtualBasins = new LinkedHashMap<>(results.length, 1.0f);
            Geometry poly = getCwaGeometry(cwa);
            PreparedGeometry pg = PreparedGeometryFactory.prepare(poly);
            Coordinate coor = poly.getCentroid().getCoordinate();

            if (results.length > 0) {
                for (int i = 0; i < results.length; i++) {
                    Object[] results2 = (Object[]) results[i];
                    if (results2.length > 0) {

                        FFMPVirtualGageBasinMetaData vb = getVirtualMetaDataBasin(
                                results2, coor);
                        vb.setLookupId((long) i + 1);
                        if (pg.contains(
                                factory.createPoint(vb.getCoordinate()))) {
                            virtualBasins.put(vb.getLid(), vb);
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.error("Error querying Virtual Gage's: +sql: " + sql,
                    e);
        }

        return virtualBasins;
    }

    /**
     * determine the depth of the upstream listings in the FFMP table
     *
     * @return
     */
    private static List<String> determineUpstreamDepth() {
        List<String> upstreams = null;

        try {
            SpatialDbQueryRequest request = new SpatialDbQueryRequest();
            request.setSchema("information_schema");
            request.setTable("columns");
            request.addRequestField("column_name");
            request.addConstraint("table_name",
                    new RequestConstraint("ffmp_basins"));
            request.addConstraint("column_name",
                    new RequestConstraint("upstream%", ConstraintType.ILIKE));
            request.setGeometryField(null);
            request.setReturnGeometry(false);

            ISpatialQuery sq = SpatialQueryFactory.create();
            SpatialQueryResult[] results = sq.executeRequest(request);
            upstreams = new ArrayList<>(results.length);
            int j = 1;

            if (results.length > 0) {
                for (SpatialQueryResult result : results) {
                    String column_name = (String) result.attributes
                            .get("column_name");
                    if (column_name.startsWith("upstream")) {
                        upstreams.add("upstream" + j);
                        j++;
                    }
                }
            }
        } catch (SpatialException e) {
            statusHandler.error("Error determining upstream depth: ", e);
        }

        return upstreams;
    }

    /**
     * gets the depth of the HUC to be generated
     *
     * @param prelimstartDepth
     * @param primaryCWA
     * @return the HUC parameters
     */
    public static List<Integer> getHucParameters(int prelimstartDepth,
            String primaryCWA) {

        // Analyze primary CWA for depth of HUC to produce
        SpatialQueryResult[] results;
        try {
            SpatialDbQueryRequest request = new SpatialDbQueryRequest();
            request.setTable("ffmp_basins");
            request.addRequestField("pfaf_id");
            request.addConstraint("cwa", new RequestConstraint(primaryCWA));
            request.setGeometryField(null);
            request.setReturnGeometry(false);

            ISpatialQuery sq = SpatialQueryFactory.create();
            results = sq.executeRequest(request);
        } catch (SpatialException e) {
            statusHandler.error(
                    "Failed to lookup Huc Parameters for cwa: " + primaryCWA,
                    e);
            // FIXME: really?
            throw new RuntimeException(e);
        }

        String[] pfafs = new String[results.length];

        if (results.length > 0) {
            for (int i = 0; i < results.length; i++) {
                pfafs[i] = (String) results[i].attributes.get("pfaf_id");
            }
        }

        // analyze pfafs for max depth
        int maxDepth = prelimstartDepth;
        int startDepth = prelimstartDepth;

        for (String pfaf : pfafs) {
            int depth = pfaf.substring(prelimstartDepth).indexOf('0');
            depth = prelimstartDepth + depth;
            if (depth > maxDepth) {
                maxDepth = depth;
            }
        }

        // do an 80% analysis to find min (startDepth)
        if (pfafs.length > 0) {
            for (int myMinDepth = maxDepth; myMinDepth > 0; myMinDepth--) {
                int ilevelcount = 0;
                for (String pfaf : pfafs) {
                    int idepth = pfaf.substring(prelimstartDepth).indexOf('0');
                    idepth = prelimstartDepth + idepth;
                    if (idepth >= myMinDepth) {
                        ilevelcount++;
                    }
                }
                if (((ilevelcount / pfafs.length) * 100) < 80) {
                    startDepth = myMinDepth;
                } else {
                    break;
                }
            }
        }

        List<Integer> retList = new ArrayList<>(2);
        retList.add(startDepth + 1);
        retList.add(maxDepth - (startDepth + 1));

        return retList;
    }

    /**
     * Gets the base level basins in the extents
     *
     * @param cwa
     * @param buffer
     * @param extents
     * @return the basins
     */
    public static Object[] getBasins(String cwa, double buffer,
            String extents) {
        String lowestSimplificationLevel = ScanUtils
                .getHighResolutionLevel("ffmp_basins");
        String highestSimplificationLevel = ScanUtils
                .getStandardResolutionLevel("cwa");
        List<String> upstreamDepth = determineUpstreamDepth();
        double extent = (buffer / 1000) / KmToDegrees;

        if (upstreamDepth != null) {
            StringBuilder sql = new StringBuilder();
            sql.append(
                    "select pfaf_id, streamname, huc_name, basin_id, area_sq_mi, ");
            sql.append("state, cwa, countyname, rfc, ");
            for (String upstream : upstreamDepth) {
                sql.append(upstream + ", ");
            }
            sql.append("ST_AsBinary(" + lowestSimplificationLevel + ") from "
                    + FFMP_TABLE);
            sql.append(" where ST_DWithin((select " + highestSimplificationLevel
                    + " from mapdata.cwa where cwa = '" + cwa + "'),");
            sql.append(" ST_SetSRID(ST_Point(x_centroid, y_centroid), 4326), "
                    + extent + ")");
            sql.append(" and ST_Contains(ST_GeomFromText('");
            sql.append(extents);
            sql.append("', 4326), " + lowestSimplificationLevel + ")"
                    + " order by pfaf_id asc");

            ISpatialQuery sq = null;
            Object[] results = null;

            try {
                sq = SpatialQueryFactory.create();
                results = sq.dbRequest(sql.toString(), MAPS_DB);
            } catch (SpatialException e) {
                statusHandler.error("Error getting basins: sql:" + sql + "\n",
                        e);
            }

            return results;
        }

        return null;

    }

    /**
     * Returns a Map of the raw geometries for a collection of pfafs.
     *
     * @param pfafs
     * @return the raw geometries
     */
    public static Map<Long, Geometry> getRawGeometries(Collection<Long> pfafs) {
        // Initialize rval to an empty Map to use as the default return value.
        Map<Long, Geometry> rval = new HashMap<>();

        if (!pfafs.isEmpty()) {
            StringBuilder builder = new StringBuilder();
            builder.append("SELECT pfaf_id, ST_AsBinary("
                    + ScanUtils.getHighResolutionLevel("ffmp_basins")
                    + "), cwa FROM ");
            builder.append(FFMP_TABLE);
            builder.append(" WHERE pfaf_id IN ('");

            for (Long pfaf : pfafs) {
                builder.append(pfaf);
                builder.append("', '");
            }

            // drop the last two chars
            builder.setLength(builder.length() - 3);
            builder.append(") order by pfaf_id asc");

            List<String> pfafList = new ArrayList<>(pfafs.size());
            for (Long pfaf : pfafs) {
                pfafList.add(pfaf.toString());
            }

            SpatialDbQueryRequest request = new SpatialDbQueryRequest();
            request.setTable("ffmp_basins");
            request.addRequestField("pfaf_id");
            request.setGeometryField(
                    ScanUtils.getHighResolutionLevel("ffmp_basins"));
            RequestConstraint constraint = new RequestConstraint(pfafList);
            request.addConstraint("pfaf_id", constraint);

            SpatialQueryResult[] results;
            try {
                ISpatialQuery sq = SpatialQueryFactory.create();
                results = sq.executeRequest(request);
                rval = new HashMap<>(results.length, 1.0f);

                for (SpatialQueryResult row : results) {
                    Long pfaf = Long
                            .parseLong((String) row.attributes.get("pfaf_id"));
                    rval.put(pfaf, row.geometry);
                }
            } catch (NumberFormatException | SpatialException e) {
                statusHandler.error("Error querying Raw Geometries: ", e);
            }
        }

        return rval;
    }

    /**
     * Gets the County info for the aggregated county
     *
     * @param fips
     * @return the county info
     */
    public static FFMPCounty getCounty(Long fips) {
        // pad the fips code to 5 digits
        String ftxt = String.format("%05d", fips);

        FFMPCounty county = null;
        try {
            SpatialDbQueryRequest request = new SpatialDbQueryRequest();
            request.setTable("county");
            request.addRequestField("countyname");
            request.addRequestField("state");
            request.addConstraint("fips", new RequestConstraint(ftxt));
            request.setGeometryField(null);
            request.setReturnGeometry(false);
            request.setLimit(1);

            ISpatialQuery sq = SpatialQueryFactory.create();
            SpatialQueryResult[] results = sq.executeRequest(request);

            if (results != null && results.length > 0) {
                String countyName = (String) results[0].attributes
                        .get("countyname");
                String state = (String) results[0].attributes.get("state");
                county = new FFMPCounty(fips, countyName, fips.toString(),
                        state, null);
            }

        } catch (SpatialException e) {
            statusHandler.error("Error retrieving COUNTY, fips: " + fips, e);
        }

        return county;
    }

    /**
     * Gets all of the unique counties for umbrella
     *
     * @param cwa
     * @param buffer
     * @param radarExtents
     * @param resolution
     * @return the FIPS codes for the unique counties
     */
    public static List<Long> getUniqueCountyFips(String cwa, double buffer,
            String radarExtents, String resolution) {
        double extent = (buffer / 1000) / KmToDegrees;

        String sql = "SELECT distinct county.fips " + " FROM mapdata.county "
                + " WHERE ST_DWithin(" + "(Select " + resolution
                + " from mapdata.cwa where cwa = '" + cwa + "'), county."
                + resolution + ", " + extent + ")" + " AND "
                + " ST_Intersects(ST_GeomFromText('" + radarExtents
                + "', 4326), county." + resolution + ")"
                + " order by county.fips desc";

        List<Long> keys = null;
        ISpatialQuery sq = null;

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);
            keys = new ArrayList<>(results.length);

            if (results.length > 0) {
                for (Object result : results) {
                    if (result != null) {
                        keys.add(new Integer((String) result).longValue());
                    }
                }
            }
        } catch (SpatialException e) {
            statusHandler
                    .error("Error retrieving COUNTY FIPS list! sql: " + sql, e);
        }

        return removeDuplicates(keys);
    }

    /**
     * Some counties get divided into individual sections based on gid but all
     * share the same fips id. This makes for just one entry.
     *
     * @param fips
     * @return
     */
    private static List<Long> removeDuplicates(List<Long> fips) {

        Set<Long> hashSet = new HashSet<>(fips);
        // Assign the HashSet to a new ArrayList
        List<Long> arrayList2 = new ArrayList<>(hashSet);
        // Ensure correct order, since HashSet doesn't
        Collections.sort(arrayList2);

        return arrayList2;
    }

    /**
     * Get the county geometry
     *
     * @param fips
     * @return list containing the geometry, county name, and state
     */
    public static FFMPCounty getCountyInfo(Long fips) {
        // pad the FIPS code to 5 digits
        String ftxt = String.format("%05d", fips);

        SpatialDbQueryRequest request = new SpatialDbQueryRequest();
        request.setTable("county");
        request.addRequestField("countyname");
        request.addRequestField("state");
        request.addConstraint("fips", new RequestConstraint(ftxt));
        request.setGeometryField(ScanUtils.getHighResolutionLevel("county"));
        request.setLimit(1);

        // Not a debug statement but for Template generation.
        statusHandler
                .info("___FFMPUtils.getCountyInfo(): county FIPS: " + ftxt);
        FFMPCounty info = null;
        try {
            ISpatialQuery sq = SpatialQueryFactory.create();
            SpatialQueryResult[] results = sq.executeRequest(request);

            if (results != null && results.length > 0) {
                String countyName = (String) results[0].attributes
                        .get("countyname");
                String state = (String) results[0].attributes.get("state");
                Geometry geom = results[0].geometry;

                info = new FFMPCounty(fips, countyName, fips.toString(), state,
                        geom);
            }
        } catch (SpatialException e) {
            statusHandler
                    .error("Error retrieving COUNTY INFO for fips: " + fips, e);
        }

        return info;
    }

    /**
     * Gets the default RADAR location center
     *
     * @param icao
     *            The radar id.
     * @return the radar location center
     */
    public static Coordinate getRadarCenter(String icao) {
        GetRadarSpatialRequest request = new GetRadarSpatialRequest();
        request.setIcao(icao);
        try {
            RadarStation radarStation = (RadarStation) RequestRouter
                    .route(request);
            if (radarStation != null) {
                return new Coordinate(radarStation.getLon(),
                        radarStation.getLat());
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to retrieve the radar center for " + icao, e);
        }
        return null;
    }

    /**
     * Gets the default FFMP cwa
     *
     * @param cwa
     * @return the cwa geometry
     */
    public static Geometry getCwaGeometry(String cwa) {
        SpatialDbQueryRequest request = new SpatialDbQueryRequest();
        request.setTable("cwa");
        request.setGeometryField(ScanUtils.getHighResolutionLevel("cwa"));
        request.addConstraint("cwa", new RequestConstraint(cwa));
        request.setLimit(1);

        Geometry geo = null;
        try {
            ISpatialQuery sq = SpatialQueryFactory.create();
            SpatialQueryResult[] results = sq.executeRequest(request);
            if (results != null && results.length > 0) {
                geo = results[0].geometry;
            }
        } catch (SpatialException e) {
            statusHandler.error("Error querying CWA geometry for cwa: " + cwa,
                    e);
        }

        return geo;
    }

    /**
     * Find the rfc in which a site is located
     *
     * @param radarSite
     * @return the rfc
     */
    public static String getRFC(String radarSite) {

        String rfc = null;

        Coordinate radarCenter = FFMPUtils.getRadarCenter(radarSite);
        Point point = factory.createPoint(radarCenter);
        WKTWriter wktWriter = new WKTWriter();
        String radarCoord = wktWriter.writeFormatted(point);

        String sql = "select site_id from mapdata.rfc where " + "ST_Contains("
                + ScanUtils.getStandardResolutionLevel("rfc")
                + ", ST_GeomFromText('" + radarCoord + "', 4326)) limit 1";

        try {
            ISpatialQuery sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);

            if (results.length > 0) {
                rfc = (String) results[0];
                rfc = SiteMap.getInstance().getSite4LetterId(rfc.toUpperCase());
            }
        } catch (Exception e) {
            statusHandler.error("Error querying RFC designation: " + sql, e);
        }

        return rfc;
    }

    /**
     * Gets the available list of FFG grids for each RFC
     *
     * @param rfc
     * @return the FFG grids
     */
    public static Set<String> getFFGParameters(String rfc) {
        Set<String> ffgHash = null;

        /**
         * Had to add this bit of code for ncgrib models
         */
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(GridRecord.class.getName());
        request.setDistinct(true);
        request.addRequestField(GridConstants.PARAMETER_ABBREVIATION);
        request.addConstraint(GridConstants.DATASET_ID,
                new RequestConstraint("FFG-" + rfc.substring(1)));
        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(request);
            ffgHash = new HashSet<>(response.getResults().size(), 1.0f);

            for (Map<String, Object> map : response.getResults()) {
                String key = (String) map
                        .get(GridConstants.PARAMETER_ABBREVIATION);
                ffgHash.add(key);
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Error querying FFG parameters: " + request.toString(), e);
        }

        return ffgHash;
    }

    /**
     * Gets the datauri for this particular FFG
     *
     * @param type
     * @param datasetid
     * @param parameter
     * @return the datauri
     */
    public static String getFFGDataURI(GuidanceType type, String datasetid,
            String parameter) {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(GridRecord.class.getName());
        request.addConstraint(GridConstants.PARAMETER_ABBREVIATION,
                new RequestConstraint(parameter));

        if (type == GuidanceType.RFC) {
            request.addConstraint(GridConstants.DATASET_ID,
                    new RequestConstraint("FFG-" + datasetid.substring(1)));
        } else {
            request.addConstraint(GridConstants.DATASET_ID,
                    new RequestConstraint(datasetid));
        }

        request.setOrderByField(PluginDataObject.REFTIME_ID, OrderMode.DESC);
        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(request);
            GridRecord[] grids = response.getEntityObjects(GridRecord.class);
            if (grids != null && grids.length > 0) {
                return grids[0].getDataURI();
            } else {
                statusHandler.warn("No data available for this FFG Request: "
                        + request.toString());
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Error querying FFG Data URIS: " + request.toString(), e);
        }

        return null;
    }

    /**
     * Gets the text string of the polygon describing the radar areal coverage
     *
     * @param pg
     * @return the radar polygon as text
     */
    public static String getRadarPolygonText(Polygon pg) {
        WKTWriter wktWriter = new WKTWriter();
        return wktWriter.writeFormatted(pg);
    }

    /**
     * Gets the text string of the polygon describing the radar areal coverage
     *
     * @param geo
     * @return the geometry as text
     */
    public static String getGeometryText(Geometry geo) {
        WKTWriter wktWriter = new WKTWriter();
        return wktWriter.writeFormatted(geo);
    }

    /**
     * Create a radar coverage polygon given the center and max extent
     *
     * @param center
     * @param maxExtent
     * @return the radar polygon
     */
    public static Polygon getRadarPolygon(Coordinate center, double maxExtent) {
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(center.x, center.y);
        Coordinate[] coors = new Coordinate[361];

        for (int i = 0; i < 361; i++) {
            double az = 0.00;
            if (i <= 180) {
                az = i;
            } else {
                az = -180 - (180 - i);
            }

            gc.setDirection(az, maxExtent);
            Point2D dstPoint = gc.getDestinationGeographicPoint();
            coors[i] = new Coordinate(dstPoint.getX(), dstPoint.getY());
        }

        gc.setDirection(0.0, maxExtent);
        Point2D dstPoint = gc.getDestinationGeographicPoint();
        coors[6] = new Coordinate(dstPoint.getX(), dstPoint.getY());

        LinearRing lr = factory.createLinearRing(coors);
        Polygon pg = factory.createPolygon(lr, null);

        return pg;
    }

    /**
     * Gets the FFMP basin metadata
     *
     * @param dbResult
     * @return the FFMP basin metadata
     */
    public static FFMPBasinMetaData getMetaDataBasin(Object[] dbResult) {

        FFMPBasinMetaData basin = new FFMPBasinMetaData();

        // pfaf_id
        if (dbResult[0] != null) {
            basin.setPfaf(Long.parseLong((String) dbResult[0]));
        }
        // streamname
        if (dbResult[1] != null) {
            basin.setStreamName((String) dbResult[1]);
        }
        // huc_name
        if (dbResult[2] != null) {
            basin.setHucName((String) dbResult[2]);
        }
        // basin id
        if (dbResult[3] != null) {
            basin.setBasinId(((Number) dbResult[3]).intValue());
        }
        // area_sq_mi
        if (dbResult[4] != null) {
            basin.setArea(((Number) dbResult[4]).doubleValue());
        }
        // state
        if (dbResult[5] != null) {
            basin.setState((String) dbResult[5]);
        }
        // cwa
        if (dbResult[6] != null) {
            basin.setCwa((String) dbResult[6]);
        }
        // countyname
        if (dbResult[7] != null) {
            basin.setCounty((String) dbResult[7]);
        }
        // rfc
        if (dbResult[8] != null) {
            basin.setRfc((String) dbResult[8]);
        }

        for (int i = 9; i < 20; i++) {
            if (dbResult[i] != null) {
                if (dbResult[i] instanceof Number) {
                    Integer streamPfaf = ((Number) dbResult[i]).intValue();
                    basin.addStreamPfaf(streamPfaf);
                } else {
                    return basin;
                }
            }
        }

        return basin;
    }

    /**
     * Gets the VGB metadata
     *
     * @param dbResult
     * @param center
     * @return the VGB metadata
     *
     */
    public static FFMPVirtualGageBasinMetaData getVirtualMetaDataBasin(
            Object[] dbResult, Coordinate center) {

        FFMPVirtualGageBasinMetaData basin = new FFMPVirtualGageBasinMetaData();
        Double lat = Double.NaN;
        Double lon = Double.NaN;

        if (dbResult[0] != null) {
            basin.setLid((String) dbResult[0]);
        }
        if (dbResult[1] != null) {
            basin.setCounty((String) dbResult[1]);
        }
        if (dbResult[2] != null) {
            basin.setName((String) dbResult[2]);
        }
        if (dbResult[3] != null) {
            lat = ((Number) dbResult[3]).doubleValue();
        }
        // area_sq_mi
        if (dbResult[4] != null) {
            lon = ((Number) dbResult[4]).doubleValue();
            /*
             * FIXME: TOTAL HACK IHFS is positive in Western Hemisphere. Had to
             * put in a horrible hack to make it so that VBG's can be used in
             * GIS queries for FFMP
             */
            if (center.x < 0) {
                lon *= -1.0;
            }
        }
        if ((lat != Double.NaN) && (lon != Double.NaN)) {
            basin.setCoordinate(new Coordinate(lon, lat));
        }

        if (dbResult[5] != null) {
            // DR 13228
            basin.setState((String) dbResult[5]);
        }

        return basin;
    }

    /**
     * Extract geometry from binary representation
     *
     * @param bytes
     * @param wkbReader
     * @return the geometry
     * @throws ParseException
     */
    public static Geometry readGeometry(byte[] bytes, WKBReader wkbReader)
            throws ParseException {
        Geometry geometry = wkbReader.read(bytes);
        return geometry;
    }

    /**
     * Gets the ratio value
     *
     * @param qpe
     * @param guid
     * @return the ratio value
     */
    public static float getRatioValue(float qpe, float guid) {
        float value = Float.NaN;
        if ((qpe >= 0.0f) && (guid >= 0.0f)) {
            value = (float) ((qpe / guid) * 100.0);
        }

        return value;
    }

    /**
     * find max ratio in list
     *
     * @param qpes
     * @param guids
     * @return the max ratio
     */
    public static float getMaxRatioValue(List<Float> qpes, List<Float> guids) {
        float ratio = Float.NaN;

        if ((qpes.size() == guids.size()) && (!qpes.isEmpty())
                && (!guids.isEmpty())) {
            for (int i = 0; i < qpes.size(); i++) {
                if (guids.get(i) > 0.0f) {
                    float nratio = getRatioValue(qpes.get(i), guids.get(i));
                    if (((nratio > ratio) && !Float.isNaN(nratio))
                            || Float.isNaN(ratio)) {
                        if (!Float.isInfinite(nratio)) {
                            ratio = nratio;
                        }
                    }
                }
            }
        }

        return ratio;
    }

    /**
     * Gets the diff value
     *
     * @param qpe
     * @param guid
     * @return the diff value
     */
    public static float getDiffValue(float qpe, float guid) {
        float value = Float.NaN;

        if ((qpe >= 0.0f) && (guid >= 0.0f)) {
            float qpeRnd = Float.parseFloat(formatter.format(qpe));
            float guidRnd = Float.parseFloat(formatter.format(guid));
            value = new Float(qpeRnd - guidRnd);
        }

        return value;
    }

    /**
     * find max diff in the list
     *
     * @param qpes
     * @param guids
     * @return the max diff
     */
    public static float getMaxDiffValue(List<Float> qpes, List<Float> guids) {
        float diff = Float.NaN;

        if ((qpes.size() == guids.size()) && (!qpes.isEmpty())
                && (!guids.isEmpty())) {

            for (int i = 0; i < qpes.size(); i++) {
                if (guids.get(i) > 0.0f) {
                    float ndiff = getDiffValue(qpes.get(i), guids.get(i));
                    if ((((ndiff) > diff) && !Float.isNaN(ndiff))
                            || Float.isNaN(diff)) {
                        diff = ndiff;
                    }
                }
            }
        }

        return diff;
    }

    /**
     * Get the file used to store aggregate records
     *
     * @param cwa
     * @param sourceSiteDataKey
     * @return the hdf5 file path
     */
    public static File getHdf5File(String cwa, String sourceSiteDataKey) {
        return new File("ffmp" + File.separatorChar + cwa + File.separatorChar
                + sourceSiteDataKey + ".h5");
    }

    /**
     * Queries the specified database
     *
     * @param query
     *            the SQL query to run
     * @param database
     *            the database to query
     * @return a two dimensional Object[] representing rows and columns
     * @throws Exception
     */
    private static Object[] executeSqlQuery(String query, String database)
            throws Exception {
        QlServerRequest request = new QlServerRequest(query);
        request.setDatabase(database);
        ResponseMessageGeneric resp = (ResponseMessageGeneric) RequestRouter
                .route(request);

        QueryResult result = (QueryResult) resp.getContents();
        List<Object[]> unmappedResults = new ArrayList<>();
        for (QueryResultRow row : result.getRows()) {
            unmappedResults.add(row.getColumnValues());
        }

        return unmappedResults.toArray(new Object[0]);
    }

    /**
     * For Grid FFMP types used as primary sources, request the coverage record
     * for use in domain creation.
     *
     * @param dataPath
     * @return the grid coverage
     * @throws Exception
     */
    public static GridCoverage getGridCoverageRecord(String dataPath)
            throws Exception {

        String[] splitURI = parseGridDataPath(dataPath);
        statusHandler.info("Parsing FFMP Grid <dataPath> " + dataPath);

        // In the case of Grid Records, we only care about the dataSetID
        String datasetID = splitURI[3];

        statusHandler
                .info("Results of <dataPath> parse: DataSetID = " + datasetID);

        GridCoverage coverage = null;

        DbQueryRequest query = new DbQueryRequest();
        query.setDatabase(META_DB);
        query.setEntityClass(GridRecord.class.getName());
        // only need one response
        query.setLimit(1);
        query.addConstraint(GridConstants.DATASET_ID,
                new RequestConstraint(datasetID));
        query.setOrderByField("insertTime", OrderMode.DESC);

        DbQueryResponse resp = (DbQueryResponse) RequestRouter.route(query);

        if (resp != null && !resp.getResults().isEmpty()) {
            for (Map<String, Object> map : resp.getResults()) {
                GridRecord record = (GridRecord) map.get(null);
                coverage = record.getLocation();
            }
        } else {
            statusHandler
                    .error("Query for Grid Coverage returned no results: DataSetID = "
                            + datasetID);
        }

        return coverage;
    }

    /**
     * Parse the <dataPath> FFMPSourceConfig tag for it's URI components.
     *
     * @param dataPath
     * @return the uri components
     */
    public static String[] parseGridDataPath(String dataPath) {
        // parse the path given as the URI match in the source config
        return dataPath.split(DataURI.SEPARATOR);
    }

}
