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
package com.raytheon.uf.common.dataplugin.warning.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest.QueryLanguage;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest.QueryType;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Computes geometries from UGCs, using the maps DB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 19, 2016  5657     tgurney   Initial creation
 * May 26, 2016  5657     tgurney   Always use high-res geometries
 * 
 * </pre>
 * 
 * @author tgurney
 */

public class UGCToGeometryUtil {

    private static Set<String> marineZones = null;

    private UGCToGeometryUtil() {
    }

    /**
     * Take set of UGCs and return a single geometry (Polygon or MultiPolygon)
     * covering all of them. Uses high-resolution geometry.
     * 
     * @param ugcs
     *            Set of UGCs from a WWA product
     * @param phen
     *            Phen from WWA product (e.g. "FL")
     * @return the geometry, or null if no geometries were found for any of the
     *         provided UGCs
     * @throws Exception
     *             if there was a problem when querying the maps database
     */
    public static Geometry ugcsToGeometry(Set<String> ugcs, String phen)
            throws Exception {
        List<String> counties = new ArrayList<>();
        List<String> fireWxZones = new ArrayList<>();
        List<String> marineZones = new ArrayList<>();
        List<String> zones = new ArrayList<>();

        /*
         * Determine type of UGC.
         * 
         * County contains two-letter state code followed by 'C'. (e.g. NEC055)
         * 
         * Marine zone contains two-letter marine zone code followed by 'Z'.
         * (e.g. AMZ234)
         * 
         * Public forecast zone contains two-letter state code followed by 'Z'
         * (e.g. NEZ052)
         * 
         * Fire wx zone is used only for FW.* products; is otherwise
         * indistinguishable from public forecast zone UGC. (e.g. NEZ052)
         */
        for (String ugc : ugcs) {
            if (phen.equals("FW")) {
                fireWxZones.add(ugc);
            } else if (ugc.charAt(2) == 'C') {
                counties.add("'" + ugc + "'");
            } else if (ugc.charAt(2) == 'Z') {
                if (isMarineZone(ugc)) {
                    marineZones.add("'" + ugc + "'");
                } else {
                    zones.add("'" + ugc + "'");
                }
            }
        }

        List<Geometry> geometries = new ArrayList<>();
        geometries.addAll(getGeometriesFromDb("county",
                "state||'C'||substring(fips,3,3)", counties));
        geometries
                .addAll(getGeometriesFromDb("zone", "state||'Z'||zone", zones));
        geometries.addAll(getGeometriesFromDb("firewxzones",
                "state||'Z'||zone", fireWxZones));

        List<Geometry> marineGeoms = getGeometriesFromDb("marinezones", "id",
                marineZones);
        if (marineGeoms.isEmpty()) {
            marineGeoms = getGeometriesFromDb("offshore", "id", marineZones);
        }
        geometries.addAll(marineGeoms);

        Geometry geomResult = null;
        if (!geometries.isEmpty()) {
            GeometryCollection geomCollection = new GeometryFactory()
                    .createGeometryCollection(geometries
                            .toArray(new Geometry[0]));
            geomResult = geomCollection.buffer(0.0);
        }
        return geomResult;
    }

    private static boolean isMarineZone(String ugc) throws Exception {
        return getMarineZonePrefixes().contains(ugc.substring(0, 2));
    }

    /**
     * Get list of two-letter marine zone prefixes by querying maps DB.
     * Subsequent calls will use cached query results rather than querying the
     * DB multiple times.
     * 
     * @return the marine zone prefixes
     * @throws Exception
     *             if there was a problem when querying the maps database
     */
    private static Set<String> getMarineZonePrefixes() throws Exception {
        if (marineZones == null) {
            QlServerRequest req = new QlServerRequest();
            req.setLang(QueryLanguage.SQL);
            req.setType(QueryType.QUERY);
            req.setDatabase("maps");
            req.setQuery("select distinct substr(id, 1, 2) id "
                    + " from mapdata.marinezones;");
            ResponseMessageGeneric response = (ResponseMessageGeneric) RequestRouter
                    .route(req);
            QueryResult result = (QueryResult) response.getContents();
            QueryResultRow[] rows = result.getRows();
            marineZones = new HashSet<>(rows.length, 1.0f);
            for (QueryResultRow row : rows) {
                marineZones.add(row.getColumnValues()[0].toString());
            }
        }
        return marineZones;
    }

    private static List<Geometry> getGeometriesFromDb(String source,
            String field, List<String> constraintList) throws Exception {
        SpatialQueryResult[] features = null;
        Map<String, RequestConstraint> constraintMap = new HashMap<>(2, 1.0f);
        RequestConstraint constraint = null;
        List<Geometry> geometries = null;
        String geomFieldName = "the_geom_0";
        if (!constraintList.isEmpty()) {
            constraint = new RequestConstraint(field, ConstraintType.IN);
            constraint.setConstraintValueList(constraintList
                    .toArray(new String[0]));
            constraintMap.put(field, constraint);
            features = SpatialQueryFactory.create().query(source,
                    geomFieldName, new String[] { field }, null, constraintMap,
                    SearchMode.WITHIN);

            if (features != null) {
                geometries = new ArrayList<>(features.length);
                for (SpatialQueryResult feature : features) {
                    geometries.add(feature.geometry);
                }
            }
        }
        if (geometries == null) {
            geometries = Collections.emptyList();
        }
        return geometries;
    }

}
