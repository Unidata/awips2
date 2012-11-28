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

import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderMode;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKBWriter;
import com.vividsolutions.jts.io.WKTWriter;

/**
 * FFMPUtils
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/22/09      2152       D. Hladky   Initial release
 * 06/18/12		 DR 15108   G. Zhang	Fix County FIPS 4-digit issue
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPUtils {
    public static GeometryFactory factory = new GeometryFactory();

    public static String FFMP_TABLE = "mapdata.ffmp_basins";

    public static String COUNTY_TABLE = "mapdata.county";

    public static String MAPS_DB = "maps";

    public static String META_DB = "metadata";

    public static String FFMP = "ffmp";

    public static double KmToDegrees = 111.12;

    public static float MISSING = -99999.0f;

    private static NumberFormat formatter = new DecimalFormat("#.##");

    public static byte[] STREAM_FILL = new byte[] { //
    0x40, 0x08, 0x40, 0x08, //
            0x00, 0x01, 0x00, 0x01, //
            0x12, 0x40, 0x12, 0x40, //
            0x41, 0x04, 0x41, 0x04, //
            0x00, (byte) 0x90, 0x00, (byte) 0x90, //
            0x08, 0x01, 0x08, 0x01, //
            0x40, 0x00, 0x40, 0x00, //
            0x02, 0x44, 0x02, 0x44, //
            0x10, 0x01, 0x10, 0x01, //
            0x01, 0x10, 0x01, 0x10, //
            0x04, 0x00, 0x04, 0x00, //
            0x40, (byte) 0x82, 0x40, (byte) 0x82, //
            0x11, 0x10, 0x11, 0x10, //
            0x00, 0x00, 0x00, 0x00, //
            0x04, 0x44, 0x04, 0x44, //
            0x40, 0x00, 0x40, 0x00, //

            0x40, 0x08, 0x40, 0x08, //
            0x00, 0x01, 0x00, 0x01, //
            0x12, 0x40, 0x12, 0x40, //
            0x41, 0x04, 0x41, 0x04, //
            0x00, (byte) 0x90, 0x00, (byte) 0x90, //
            0x08, 0x01, 0x08, 0x01, //
            0x40, 0x00, 0x40, 0x00, //
            0x02, 0x44, 0x02, 0x44, //
            0x10, 0x01, 0x10, 0x01, //
            0x01, 0x10, 0x01, 0x10, //
            0x04, 0x00, 0x04, 0x00, //
            0x40, (byte) 0x82, 0x40, (byte) 0x82, //
            0x11, 0x10, 0x11, 0x10, //
            0x00, 0x00, 0x00, 0x00, //
            0x04, 0x44, 0x04, 0x44, //
            0x40, 0x00, 0x40, 0x00, //
    };

    /**
     * Gets the base level basins in the County Warning Area
     * 
     * @param cwa
     * @return
     */
    public static ArrayList<Long> getAllPfafs(String extents, String mode) {
        ArrayList<Long> pfafs = new ArrayList<Long>();
        String sql = "select pfaf_id " + " from " + FFMP_TABLE + " where"
                + " ST_Contains(ST_GeomFromText('" + extents + "', 4326), "
                + ScanUtils.getHighResolutionLevel("ffmp_basins") + ")";

        ISpatialQuery sq = null;
        try {
            sq = SpatialQueryFactory.create();

            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);

            if (results.length > 0) {
                if (mode.equals("CAVE")) {
                    for (int i = 0; i < results.length; i++) {
                        Object[] results2 = (Object[]) results[i];
                        for (int j = 0; j < results2.length; j++) {
                            if (((String) results2[j]) != null) {
                                pfafs.add(Long.parseLong((String) results2[j]));
                            }
                        }
                    }
                }

                else {
                    for (int j = 0; j < results.length; j++) {
                        if (((String) results[j]) != null) {
                            pfafs.add(Long.parseLong((String) results[j]));
                        }
                    }
                }
            }
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return pfafs;
    }

    /**
     * Gets the base level basins in the County Warning Area
     * 
     * @param cwa
     * @return
     */
    public static LinkedHashMap<String, FFMPVirtualGageBasinMetaData> getVirtualGageBasins(
            double extent, String cwa, String mode) {

        LinkedHashMap<String, FFMPVirtualGageBasinMetaData> virtualBasins = new LinkedHashMap<String, FFMPVirtualGageBasinMetaData>();
        String sql = "SELECT lid, county, name, lat, lon FROM location "
                + "where lid in " + "(select distinct(lid) from IngestFilter "
                + "where pe in ('PC', 'PP') " + "and ingest = 'T' "
                + "and dur < 2000)";
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
        try {

            Object[] results = dao.executeSQLQuery(sql.toString());
            Geometry poly = getCwaGeometry(cwa, mode);
            PreparedGeometry pg = PreparedGeometryFactory.prepare(poly);
            Coordinate coor = poly.getCentroid().getCoordinate();

            if (results.length > 0) {
                for (int i = 0; i < results.length; i++) {
                    Object[] results2 = (Object[]) results[i];
                    if (results2.length > 0) {

                        FFMPVirtualGageBasinMetaData vb = getVirtualMetaDataBasin(
                                results2, coor);
                        vb.setLookupId((long) i + 1);
                        if (pg.contains(factory.createPoint(vb.getCoordinate()))) {
                            virtualBasins.put(vb.getLid(), vb);
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return virtualBasins;
    }

    /**
     * determine the depth of the upstream listings in the FFMP table
     * 
     * @return
     */
    private static ArrayList<String> determineUpstreamDepth() {
        ISpatialQuery sq = null;
        Object[] results = null;
        ArrayList<String> upstreams = new ArrayList<String>();

        String sql = "select column_name from information_schema.columns where table_name='ffmp_basins'";
        try {
            sq = SpatialQueryFactory.create();
            results = sq.dbRequest(sql, MAPS_DB);
            int j = 1;

            if (results.length > 0) {
                for (int i = 0; i < results.length; i++) {
                    String column_name = (String) results[i];
                    if (column_name.startsWith("upstream")) {
                        upstreams.add("upstream" + j);
                        j++;
                    }
                }
            }
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return upstreams;
    }

    /**
     * gets the depth of the HUC to be generated
     * 
     * @param prelimstartDepth
     * @param primaryCWA
     * @return
     */
    public static ArrayList<Integer> getHucParameters(int prelimstartDepth,
            String primaryCWA) {

        // Analyze primary CWA for depth of HUC to produce
        StringBuffer sql = new StringBuffer();
        sql.append("select pfaf_id ");
        sql.append(" from " + FFMP_TABLE);
        sql.append(" where cwa = '" + primaryCWA + "'");

        ISpatialQuery sq = null;
        Object[] results = null;

        try {
            sq = SpatialQueryFactory.create();
            results = sq.dbRequest(sql.toString(), MAPS_DB);
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        String[] pfafs = new String[results.length];

        if (results != null) {
            if (results.length > 0) {
                for (int i = 0; i < results.length; i++) {
                    pfafs[i] = (String) results[i];
                }
            }
        }

        // analyze pfafs for max depth
        int maxDepth = prelimstartDepth;
        int startDepth = prelimstartDepth;

        for (int i = 0; i < pfafs.length; i++) {
            int depth = pfafs[i].indexOf("0");
            if (depth > maxDepth) {
                maxDepth = depth;
            }
        }

        // do an 80% analysis to find min (startDepth)
        if (pfafs.length > 0) {
            for (int myMinDepth = maxDepth; myMinDepth > 0; myMinDepth--) {
                int ilevelcount = 0;
                for (int i = 0; i < pfafs.length; i++) {
                    int idepth = pfafs[i].indexOf("0");
                    if (idepth >= myMinDepth) {
                        ilevelcount++;
                    }
                }
                if ((ilevelcount / pfafs.length) * 100 < 80) {
                    startDepth = myMinDepth;
                } else {
                    break;
                }
            }
        }

        ArrayList<Integer> retList = new ArrayList<Integer>(2);
        retList.add(startDepth + 1);
        retList.add(maxDepth - (startDepth + 1));

        return retList;
    }

    /**
     * Gets the base level basins in the extents
     * 
     * @param extents
     * @return
     */
    public static Object[] getBasins(String cwa, double buffer,
            String radarExtents, String mode) {
        String lowestSimplificationLevel = ScanUtils
                .getHighResolutionLevel("ffmp_basins");
        String highestSimplificationLevel = ScanUtils
                .getStandardResolutionLevel("cwa");
        ArrayList<String> upstreamDepth = determineUpstreamDepth();
        double extent = (buffer / 1000) / KmToDegrees;

        if (upstreamDepth != null) {
            StringBuffer sql = new StringBuffer();
            sql.append("select pfaf_id, streamname, huc_name, basin_id, area_sq_mi, ");
            sql.append("state, cwa, countyname, rfc, ");
            for (String upstream : upstreamDepth) {
                sql.append(upstream + ", ");
            }
            sql.append("AsBinary(" + lowestSimplificationLevel + ") from "
                    + FFMP_TABLE);
            sql.append(" where ST_DWithin((select "
                    + highestSimplificationLevel
                    + " from mapdata.cwa where cwa = '" + cwa + "'),");
            sql.append(" ST_SetSRID(ST_Point(x_centroid, y_centroid), 4326), "
                    + extent + ")");
            sql.append(" and ST_Contains(ST_GeomFromText('");
            sql.append(radarExtents);
            sql.append("', 4326), " + lowestSimplificationLevel + ")"
                    + " order by pfaf_id asc");

            ISpatialQuery sq = null;
            Object[] results = null;

            try {
                sq = SpatialQueryFactory.create();
                results = sq.dbRequest(sql.toString(), MAPS_DB);
            } catch (SpatialException e) {
                e.printStackTrace();
            }

            return results;
        }

        return null;

    }

    /**
     * Returns a Map of the raw geometries for a collection of pfafs.
     * 
     * @param pfafs
     * @return
     */
    public static Map<Long, Geometry> getRawGeometries(Collection<Long> pfafs) {
        HashMap<Long, Geometry> rval = new HashMap<Long, Geometry>(
                (int) (pfafs.size() * 1.3) + 1);
        if (pfafs.size() > 0) {
            StringBuilder builder = new StringBuilder();
            builder.append("SELECT pfaf_id, AsBinary("
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

            ISpatialQuery sq = null;
            Object[] results = null;

            try {
                sq = SpatialQueryFactory.create();
                results = sq.dbRequest(builder.toString(), MAPS_DB);
            } catch (SpatialException e) {
                e.printStackTrace();
            }

            WKBReader wkbReader = new WKBReader();

            for (Object row : results) {
                try {
                    Object[] columns = (Object[]) row;
                    Long pfaf = Long.parseLong((String) columns[0]);
                    Geometry g = readGeometry(columns[1], wkbReader);
                    rval.put(pfaf, g);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
        }

        return rval;
    }

    public static Long getBaseAggrHuc(FFMPTemplates templates, String huc,
            Long pfaf) {
        Long rval = pfaf;
        if (huc.startsWith("HUC")) {
            long divisor = (long) Math.pow(10,
                    Integer.parseInt(huc.substring(3)));
            rval = new Long(pfaf / divisor);
        } else if ("ALL".equals(huc) || "VIRTUAL".equals(huc)) {
            pfaf.toString().substring(0, templates.getHucDepthStart());
        }

        // COUNTY has no higher aggr

        return rval;
    }

    public static Long getBaseAggrPfaf(FFMPTemplates templates, String huc,
            Long pfaf) {
        Long rval = pfaf;
        if (huc.startsWith("HUC")) {
            long divisor = (long) Math.pow(10,
                    Integer.parseInt(huc.substring(3)));
            rval = new Long(pfaf / divisor);
        } else if ("ALL".equals(huc) || "VIRTUAL".equals(huc)) {
            rval = Long.parseLong(pfaf.toString().substring(0,
                    templates.getHucDepthStart()));
        }
        // COUNTY has no higher aggr

        return rval;
    }

    /**
     * Gets the GID for the aggregated county
     * 
     * @param pfaf
     * @return
     */
    public static FFMPCounty getCounty(Long pfaf, String mode) {

        String ftxt = (pfaf >= 10000 ? "" + pfaf : "0" + pfaf);// DR 15164

        String sql = "SELECT county.countyname, county.state FROM "
                + " mapdata.county WHERE county.fips = '" + ftxt/* pfaf */+ "'";// DR
                                                                                // 15164

        ISpatialQuery sq = null;
        FFMPCounty county = new FFMPCounty();
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);

            // List<Object[]> results = DirectDbQuery.executeQuery(
            // sql, FFMPUtils.MAPS_DB, QueryLanguage.SQL);
            if (results.length > 0) {
                if (mode.equals("EDEX")) {
                    for (int i = 0; i < results.length; i++) {
                        Object[] results2 = (Object[]) results[i];

                        String countyName = null;
                        String state = null;

                        if (results2[0] instanceof String) {
                            countyName = (String) results2[0];
                        }
                        if (results[1] instanceof String) {
                            state = (String) results2[1];
                        }

                        if ((state != null) && (countyName != null)) {

                            county = new FFMPCounty(pfaf, countyName,
                                    pfaf.toString(), state);
                        }
                    }
                } else {

                    for (int i = 0; i < results.length; i++) {

                        String countyName = null;
                        String state = null;

                        Object[] results2 = null;
                        try {
                            results2 = (Object[]) results[i];

                            if (results2[0] instanceof String) {
                                countyName = (String) results2[0];
                            }
                            if (results2[1] instanceof String) {
                                state = (String) results2[1];
                            }

                            if ((state != null) && (countyName != null)) {

                                county = new FFMPCounty(pfaf, countyName,
                                        pfaf.toString(), state);
                            }
                        } catch (Exception e) {
                            if (results.length == 2) {
                                if (results[0] instanceof String) {
                                    countyName = (String) results[0];
                                }
                                if (results[1] instanceof String) {
                                    state = (String) results[1];
                                }

                                county = new FFMPCounty(pfaf, countyName,
                                        pfaf.toString(), state);
                                break;
                            }
                        }
                    }
                }
            }
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return county;
    }

    /**
     * Gets all of the unique counties for umbrella
     * 
     * @param cwa
     * @param center
     * @param maxExtent
     * @return
     */
    public static ArrayList<Long> getUniqueCountyFips(String cwa,
            double buffer, String radarExtents, String mode, String resolution) {
        double extent = (buffer / 1000) / KmToDegrees;

        String sql = "SELECT distinct county.fips " + " FROM mapdata.county "
                + " WHERE ST_DWithin(" + "(Select " + resolution
                + " from mapdata.cwa where cwa = '" + cwa + "'), county."
                + resolution + ", " + extent + ")" + " AND "
                + " ST_Intersects(ST_GeomFromText('" + radarExtents
                + "', 4326), county." + resolution + ")"
                + " order by county.fips desc";

        ArrayList<Long> keys = new ArrayList<Long>();
        ISpatialQuery sq = null;

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);

            if (results != null) {
                if (results.length > 0) {
                    for (int i = 0; i < results.length; i++) {
                        if (results[i] != null) {
                            keys.add(new Integer((String) results[i])
                                    .longValue());
                        }
                    }
                }
            }
        } catch (SpatialException e) {
            e.printStackTrace();
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
    private static ArrayList<Long> removeDuplicates(ArrayList<Long> fips) {

        HashSet<Long> hashSet = new HashSet<Long>(fips);
        // Assign the HashSet to a new ArrayList
        ArrayList<Long> arrayList2 = new ArrayList<Long>(hashSet);
        // Ensure correct order, since HashSet doesn't
        Collections.sort(arrayList2);

        return arrayList2;
    }

    /**
     * Get the county geometry
     * 
     * @param gid
     * @return
     */
    @SuppressWarnings("unchecked")
    public static ArrayList<?> getCountyInfo(Long fips, String mode) {
        String ftxt = (fips >= 10000 ? "" + fips : "0" + fips);// DR 15108: add
                                                               // a leading 0 to
                                                               // 4-digit FIPS.
        String sql1 = "SELECT county.gid from " + FFMPUtils.COUNTY_TABLE
                + " WHERE county.fips = '" + ftxt/* fips */+ "'";// DR 15108:
                                                                 // use the
                                                                 // 5-digit FIPS
                                                                 // string.
        System.out
                .println("___FFMPUtils.getCountyInfo(): county FIPS: " + ftxt);// Not
                                                                               // a
                                                                               // debug
                                                                               // statement
                                                                               // but
                                                                               // for
                                                                               // Template
                                                                               // generation.
        ISpatialQuery sq1 = null;
        ArrayList<Long> gids = new ArrayList<Long>();

        try {
            sq1 = SpatialQueryFactory.create();
            Object[] results = sq1.dbRequest(sql1, FFMPUtils.MAPS_DB);

            if (results != null) {
                if (results.length > 0) {
                    for (int i = 0; i < results.length; i++) {
                        gids.add(((Number) results[i]).longValue());
                    }
                }
            }

        } catch (SpatialException e) {
            e.printStackTrace();
        }

        @SuppressWarnings("rawtypes")
        ArrayList info = new ArrayList();
        Geometry geom = null;
        String countyName = null;
        String state = null;
        /**
         * See if we have more than one GID
         */
        for (Long gid : gids) {

            String sql = "SELECT AsBinary("
                    + ScanUtils.getHighResolutionLevel("county")
                    + "), countyname, state  FROM " + FFMPUtils.COUNTY_TABLE
                    + " WHERE gid = '" + gid + "'";

            ISpatialQuery sq = null;

            try {
                sq = SpatialQueryFactory.create();
                Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);

                if (results.length > 0) {
                    for (int i = 0; i < results.length; i++) {
                        Object[] results2 = (Object[]) results[i];
                        WKBReader wkbReader = new WKBReader();

                        if (results2[0] != null) {
                            if (geom == null) {
                                geom = readGeometry(results2[0], wkbReader);
                            } else {
                                Geometry cGeom = null;
                                Geometry newGeom = readGeometry(results2[0],
                                        wkbReader);
                                cGeom = geom.union(newGeom);
                                geom = cGeom;
                            }
                        }
                        if (results2[1] != null) {
                            countyName = (String) results2[1];
                        }
                        if (results2[2] != null) {
                            state = (String) results2[2];
                        }
                    }
                }

            } catch (SpatialException e) {
                e.printStackTrace();
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }

        info.add(geom);
        info.add(countyName);
        info.add(state);

        return info;
    }

    /**
     * Gets the basin meta data
     * 
     * @param cwa
     * @return
     */
    public static Long getBasin(Integer basinId, String mode) {

        String sql = "select pfaf_id " + " from " + FFMPUtils.FFMP_TABLE
                + " where " + "basin_id = '" + basinId + "'";
        ISpatialQuery sq = null;

        Long pfaf = null;
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);

            if (mode.equals("CAVE")) {
                Object[] results2 = (Object[]) results[0];
                pfaf = Long.parseLong((String) results2[0]);
            }

            else {

            }

        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return pfaf;
    }

    /**
     * Gets the default RADAR location center
     * 
     * @param cwa
     * @return
     */
    public static Coordinate getRadarCenter(String icao, String mode) {
        String sql = "select lon, lat from awips.radar_spatial "
                + " where rda_id = '" + icao.toUpperCase() + "'";
        ISpatialQuery sq = null;
        Coordinate coor = null;
        Double lon = null;
        Double lat = null;
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.META_DB);

            if (results[0] instanceof Object[]) {
                Object[] results2 = (Object[]) results[0];
                lon = (((Number) results2[0]).doubleValue());
                lat = (((Number) results2[1]).doubleValue());
            } else {
                lon = (((Number) results[0]).doubleValue());
                lat = (((Number) results[1]).doubleValue());
            }
            coor = new Coordinate(lon, lat);

        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return coor;
    }

    /**
     * Gets the default FFMP cwa + buffer
     * 
     * @param cwa
     * @return
     */
    public static Geometry getCwaGeometry(String cwa, String mode) {
        // convert buffer to km, then degrees
        String sql = "Select asBinary("
                + ScanUtils.getStandardResolutionLevel("cwa")
                + ") from mapdata.cwa where cwa = '" + cwa + "'";

        ISpatialQuery sq = null;
        Geometry geo = null;
        WKBReader wkbReader = new WKBReader();
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);
            try {
                geo = readGeometry(results[0], wkbReader);
            } catch (ParseException e) {
                e.printStackTrace();
            }
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return geo;
    }

    /**
     * Find me a default list of CWA's under the cwa umbrella
     * 
     * @param cwaUmbrella
     * @return
     */
    public static ArrayList<String> getCWAs(String cwaUmbrella) {

        ArrayList<String> cwas = null;
        ISpatialQuery sq = null;
        String sql = "select cwa from mapdata.cwa where "
                + "ST_INTERSECTS(ST_GeomFromText('" + cwaUmbrella
                + "', 4326), " + ScanUtils.getStandardResolutionLevel("cwa")
                + ")";

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);
            cwas = new ArrayList<String>();

            if (results.length > 0) {
                for (int i = 0; i < results.length; i++) {
                    cwas.add((String) results[i]);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return cwas;
    }

    /**
     * Find the rfc a site is located in
     * 
     * @param radarSite
     * @return
     */
    public static String getRFC(String radarSite) {

        String rfc = null;
        ISpatialQuery sq = null;

        Coordinate radarCenter = FFMPUtils.getRadarCenter(radarSite, "EDEX");
        Point point = factory.createPoint(radarCenter);
        WKTWriter wktWriter = new WKTWriter();
        String radarCoord = wktWriter.writeFormatted(point);

        String sql = "select site_id from mapdata.rfc where " + "ST_Contains("
                + ScanUtils.getStandardResolutionLevel("rfc")
                + ", ST_GeomFromText('" + radarCoord + "', 4326))";

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, FFMPUtils.MAPS_DB);

            if (results.length > 0) {
                rfc = (String) results[0];
                rfc = SiteMap.getInstance().getSite4LetterId(rfc.toUpperCase());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return rfc;
    }

    /**
     * Gets the available list of FFG grids for each RFC
     * 
     * @param rfc
     * @return
     */
    public static Set<String> getFFGParameters(String rfc) {
        Set<String> ffgHash = new HashSet<String>();

        /**
         * Had to add this bit of code for ncgrib models
         */
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(GridRecord.class.getName());
        request.setDistinct(true);
        request.addRequestField(GridConstants.PARAMETER_ABBREVIATION);
        request.addConstraint(GridConstants.DATASET_ID, new RequestConstraint(
                "FFG-" + rfc.substring(1)));
        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(request);

            for (Map<String, Object> map : response.getResults()) {
                String key = (String) map
                        .get(GridConstants.PARAMETER_ABBREVIATION);
                ffgHash.add(key);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return ffgHash;
    }

    /**
     * Gets the datauri for this partiular FFG
     * 
     * @param id
     * @return
     */
    public static String getFFGDataURI(String rfc, String parameter,
            String plugin) {

        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(GridRecord.class.getName());
        request.addRequestField("dataURI");
        request.addConstraint(GridConstants.PARAMETER_ABBREVIATION,
                new RequestConstraint(parameter));
        request.addConstraint(GridConstants.DATASET_ID, new RequestConstraint(
                "FFG-" + rfc.substring(1)));
        request.setOrderByField("dataTime.refTime", OrderMode.DESC);
        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(request);

            for (Map<String, Object> map : response.getResults()) {
                return (String) map.get("dataURI");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    /**
     * Gets a radar record to use derive the center and extents from
     * 
     * @param id
     * @return
     */
    public static String getRadarDataURI(String icao) {
        String sql = "select datauri from awips.radar where icao = '" + icao
                + "' and productcode = 32 limit 1";
        ISpatialQuery sq = null;
        String uri = null;

        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, META_DB);
            if (results.length > 0) {
                uri = (String) results[0];
            }
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return uri;
    }

    /**
     * Gets the Xmrg grid geometry
     * 
     * @param xmrg
     * @return
     */
    public static GridGeometry2D getXmrgGeometry(XmrgFile xmrg, int gridFactor) {

        Rectangle extent = xmrg.getHrapExtent();
        HRAPSubGrid subGrid = null;

        try {
            if ((extent.x == 0) && (extent.y == 0)) {
                Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                if ((extent.width == coord.width)
                        && (extent.height == coord.height)) {
                    extent = coord;
                }
            }
            subGrid = new HRAPSubGrid(extent, gridFactor);

        } catch (Exception e) {
            e.printStackTrace();
        }

        return MapUtil.getGridGeometry(subGrid);
    }

    /**
     * Gets the Xmrg subgrid
     * 
     * @param xmrg
     * @return
     */
    public static HRAPSubGrid getXmrgSubGrid(XmrgFile xmrg, int gridFactor) {

        Rectangle extent = xmrg.getHrapExtent();
        HRAPSubGrid subGrid = null;

        try {
            if ((extent.x == 0) && (extent.y == 0)) {
                Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                if ((extent.width == coord.width)
                        && (extent.height == coord.height)) {
                    extent = coord;
                }
            }
            subGrid = new HRAPSubGrid(extent, gridFactor);

        } catch (Exception e) {
            e.printStackTrace();
        }

        return subGrid;
    }

    /**
     * Gets the text string of the polygon describing the radar areal coverage
     * 
     * @param Polygon
     */
    public static String getRadarPolygonText(Polygon pg) {

        WKTWriter wktWriter = new WKTWriter();

        return wktWriter.writeFormatted(pg);
    }

    /**
     * Gets the text string of the polygon describing the radar areal coverage
     * 
     * @param Polygon
     */
    public static String getGeometryText(Geometry geo) {

        WKTWriter wktWriter = new WKTWriter();

        return wktWriter.writeFormatted(geo);
    }

    /**
     * Write as bytes
     * 
     * @param geo
     * @return
     */
    public static String getGeometryBytes(Geometry geo) {
        WKBWriter writer = new WKBWriter();
        byte[] bytes = writer.write(geo);
        return WKBWriter.bytesToHex(bytes);
    }

    /**
     * Get that polygon
     * 
     * @param coor
     * @param maxExtent
     * @return
     */
    public static Polygon getRadarPolygon(Coordinate coor, double maxExtent) {
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(coor.x, coor.y);
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
     * Holds the metadata
     * 
     * @param Object
     *            [] dbResult
     */
    public static FFMPBasinMetaData getMetaDataBasin(Object[] dbResult,
            String mode) {

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
        // for some reason long and bigInt are returned from CAVE and EDEX for
        // the same query
        // basin_id
        if (mode.equals("CAVE")) {
            if (dbResult[3] != null) {
                basin.setBasinId(((Long) dbResult[3]).intValue());
            }
        } else {
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
     * Holds the VGB metadata
     * 
     * @param Object
     *            [] dbResult
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
            // FIXME: TOTAL HACK IHFS is positive in Western Hemisphere
            // Had to put in a horrible hack to make it so that VBG's can be
            // used in GIS
            // queries for FFMP
            if (center.x < 0) {
                lon = lon * (-1.0);
            }
        }
        if ((lat != Double.NaN) && (lon != Double.NaN)) {
            basin.setCoordinate(new Coordinate(lon, lat));
        }

        return basin;
    }

    /**
     * Get a VGB
     * 
     * @param dbResult
     * @param endTime
     * @return
     */
    public static FFMPVirtualGageBasin getVirtualGageBasin(Object[] dbResult,
            Date endTime) {

        FFMPVirtualGageBasin vgb = new FFMPVirtualGageBasin();

        if (dbResult[1] != null) {
            vgb.setValue(endTime, ((Number) dbResult[1]).floatValue());
        }

        return vgb;
    }

    /**
     * extract geometry
     * 
     * @param obj
     * @return
     * @throws ParseException
     */
    public static Geometry readGeometry(Object obj, WKBReader wkbReader)
            throws ParseException {
        Geometry geometry = wkbReader.read((byte[]) obj);
        return geometry.buffer(0);
    }

    /**
     * Gets the ratio value
     * 
     * @param qpe
     * @param guid
     * @return
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
     * @return
     */
    public static float getMaxRatioValue(ArrayList<Float> qpes,
            ArrayList<Float> guids) {
        float ratio = Float.NaN;

        if ((qpes.size() == guids.size()) && (qpes.size() > 0)
                && (guids.size() > 0)) {
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
     * gets the diff value
     * 
     * @param qpe
     * @param guid
     * @return
     */
    public static float getDiffValue(float qpe, float guid) {
        Float value = Float.NaN;

        if ((qpe >= 0.0f) && (guid >= 0.0f)) {
            float qpeRnd = Float.parseFloat(formatter.format(qpe));
            float guidRnd = Float.parseFloat(formatter.format(guid));
            value = new Float(qpeRnd - guidRnd);
        }
        return value;
    }

    /**
     * find max diff in list
     * 
     * @param qpes
     * @param guids
     * @return
     */
    public static float getMaxDiffValue(ArrayList<Float> qpes,
            ArrayList<Float> guids) {
        float diff = Float.NaN;
        if ((qpes.size() == guids.size()) && (qpes.size() > 0)
                && (guids.size() > 0)) {
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
}
