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
package com.raytheon.uf.common.dataplugin.maps.dataaccess.util;

import java.util.Iterator;
import java.util.List;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * A utility to construct a query that will be used to retrieve information from
 * the maps table based on the supplied information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2013            bkowal      Initial creation
 * Apr 09, 2014  #2997     randerso    Added support to query against a Geometry
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class MapsQueryUtil {

    /**
     * PostGIS Spatial Reference ID (EPSG code) for WGS84.
     */
    private static final int WGS84_SRID = 4326;

    /**
     * 
     */
    private MapsQueryUtil() {
    }

    /**
     * Builds a query that can be used to query the maps table based on the
     * provided information.
     * 
     * @param boundingGeom
     *            used to limit the selected information to a certain
     *            geographical area
     * @param columns
     *            the list of columns that will be included in the SELECT
     *            statement
     * @param additionalConstraints
     *            the list of constraints that will become part of the AND
     *            statement
     * @param table
     *            the table to select data from
     * @param geomField
     *            the name of the geometry field of interest
     * @return the query
     */
    public static String assembleMapsTableQuery(Geometry boundingGeom,
            List<String> columns, List<String> additionalConstraints,
            String table, String geomField) {

        String geospatialConstraint = null;
        if (boundingGeom != null) {
            geospatialConstraint = "ST_Intersects(the_geom, ST_GeometryFromText('"
                    + boundingGeom.toText() + "', " + WGS84_SRID + "))";
        }

        return assembleMapsTableQuery(geospatialConstraint, columns,
                additionalConstraints, table, geomField);
    }

    /**
     * Builds a query that can be used to query the maps table based on the
     * provided information.
     * 
     * @param env
     *            used to limit the selected information to a certain
     *            geographical area
     * @param columns
     *            the list of columns that will be included in the SELECT
     *            statement
     * @param additionalConstraints
     *            the list of constraints that will become part of the AND
     *            statement
     * @param table
     *            the table to select data from
     * @param geomField
     *            the name of the geometry field of interest
     * @return the query
     */
    public static String assembleMapsTableQuery(Envelope env,
            List<String> columns, List<String> additionalConstraints,
            String table, String geomField) {

        String geospatialConstraint = null;
        if (env != null) {
            geospatialConstraint = String.format(
                    "%s && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,"
                            + WGS84_SRID + ")", geomField, env.getMinX(),
                    env.getMinY(), env.getMaxX(), env.getMaxY());
        }
        return assembleMapsTableQuery(geospatialConstraint, columns,
                additionalConstraints, table, geomField);
    }

    private static String assembleMapsTableQuery(String geospatialConstraint,
            List<String> columns, List<String> additionalConstraints,
            String table, String geomField) {
        StringBuilder query = new StringBuilder("SELECT ");
        if ((columns != null) && !columns.isEmpty()) {
            Iterator<String> iter = columns.iterator();
            query.append(iter.next());
            while (iter.hasNext()) {
                query.append(", ");
                query.append(iter.next());
            }
        }

        query.append(" FROM ");
        query.append(table);

        // add the geospatial constraint
        if (geospatialConstraint != null) {
            query.append(" WHERE ");
            query.append(geospatialConstraint);
        }

        // add any additional constraints
        if ((additionalConstraints != null) && !additionalConstraints.isEmpty()) {
            if (geospatialConstraint == null) {
                query.append(" WHERE ");
            } else {
                query.append(" AND ");
            }
            Iterator<String> iter = additionalConstraints.iterator();
            query.append(iter.next());
            while (iter.hasNext()) {
                query.append(" AND ");
                query.append(iter.next());
            }
        }

        query.append(';');
        return query.toString();
    }
}