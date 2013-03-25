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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.vividsolutions.jts.geom.Envelope;

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
 * Jan 28, 2013            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class MapsQueryUtil {

    /**
     * 
     */
    private MapsQueryUtil() {
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
        // add the geospatial constraint
        if (env != null) {
            // copy before modifying
            if (additionalConstraints == null) {
                additionalConstraints = new ArrayList<String>();
            } else {
                additionalConstraints = new ArrayList<String>(
                        additionalConstraints);
            }
            // geospatial constraint will be first
            additionalConstraints.add(0, String.format(
                    "%s && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
                    geomField, env.getMinX(), env.getMinY(), env.getMaxX(),
                    env.getMaxY()));
        }

        StringBuilder query = new StringBuilder("SELECT ");
        if (columns != null && !columns.isEmpty()) {
            Iterator<String> iter = columns.iterator();
            query.append(iter.next());
            while (iter.hasNext()) {
                query.append(", ");
                query.append(iter.next());
            }
        }

        query.append(" FROM ");
        query.append(table);

        // add any additional constraints
        if (additionalConstraints != null && !additionalConstraints.isEmpty()) {
            query.append(" WHERE ");
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