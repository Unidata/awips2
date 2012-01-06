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
package com.raytheon.uf.common.geospatial;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Abstract spatial query class, converts String-String constraint map to
 * String-RequestConstraints map
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractSpatialQuery implements ISpatialQuery {

    @Override
    public final SpatialQueryResult[] query(String dataSet,
            String[] attributes, Geometry geometry, Map<String, String> filter,
            boolean exclusive, SearchMode mode) throws SpatialException {
        return query(dataSet, "the_geom", attributes, geometry, filter,
                exclusive, mode, null);
    }

    @Override
    public final SpatialQueryResult[] query(String dataSet,
            String[] attributes, Geometry geometry, Map<String, String> filter,
            boolean exclusive, SearchMode mode, Integer limit)
            throws SpatialException {
        Map<String, RequestConstraint> constraintMap = new HashMap<String, RequestConstraint>();
        if (filter != null) {
            ConstraintType type = exclusive ? ConstraintType.NOT_EQUALS
                    : ConstraintType.EQUALS;
            for (Map.Entry<String, String> entry : filter.entrySet()) {
                constraintMap.put(entry.getKey(),
                        new RequestConstraint(entry.getValue(), type));
            }
        }
        return query(dataSet, "the_geom", attributes, geometry, constraintMap,
                mode, limit);
    }

    @Override
    public final SpatialQueryResult[] query(String dataSet,
            String theGeomField, String[] attributes, Geometry geometry,
            Map<String, String> filter, boolean exclusive, SearchMode mode)
            throws SpatialException {
        return query(dataSet, theGeomField, attributes, geometry, filter,
                exclusive, mode, null);
    }

    @Override
    public final SpatialQueryResult[] query(String dataSet,
            String theGeomField, String[] attributes, Geometry geometry,
            Map<String, String> filter, boolean exclusive, SearchMode mode,
            Integer limit) throws SpatialException {
        Map<String, RequestConstraint> constraintMap = new HashMap<String, RequestConstraint>();
        if (filter != null) {
            ConstraintType type = exclusive ? ConstraintType.NOT_EQUALS
                    : ConstraintType.EQUALS;
            for (Map.Entry<String, String> entry : filter.entrySet()) {
                constraintMap.put(entry.getKey(),
                        new RequestConstraint(entry.getValue(), type));
            }
        }
        return query(dataSet, theGeomField, attributes, geometry,
                constraintMap, mode, limit);
    }

    @Override
    public SpatialQueryResult[] query(String dataSet, String[] attributes,
            Geometry geometry, Map<String, RequestConstraint> filter,
            SearchMode mode) throws SpatialException {
        return query(dataSet, "the_geom", attributes, geometry, filter, mode);
    }

    @Override
    public SpatialQueryResult[] query(String dataSet, String[] attributes,
            Geometry geometry, Map<String, RequestConstraint> filter,
            SearchMode mode, Integer limit) throws SpatialException {
        return query(dataSet, "the_geom", attributes, geometry, filter, mode,
                limit);
    }
}
