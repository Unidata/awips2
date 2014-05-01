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
package com.raytheon.viz.core.spatial;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.AbstractSpatialDbQuery;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.geospatial.request.SpatialDbQueryRequest;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.vividsolutions.jts.geom.Geometry;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SpatialDbQuery extends AbstractSpatialDbQuery {

    private static final String GID = "gid";

    @Override
    public Object[] dbRequest(String sql, String dbname)
            throws SpatialException {
        // TODO: This only goes two dimensions deep. Find a more general way in
        // the future.
        List<Object[]> results = null;
        Object[] list = null;
        try {
            results = DirectDbQuery.executeQuery(sql, dbname,
                    DirectDbQuery.QueryLanguage.SQL);
            list = new Object[results.size()];
            if (list.length > 1) {
                for (int i = 0; i < results.size(); i++) {

                    Object[] list1 = new Object[results.get(i).length];
                    if (results.get(i).length > 0) {
                        list[i] = results.get(i);
                    } else {
                        for (int j = 0; j < results.get(i).length; j++) {
                            list1[i] = results.get(i)[j];
                        }
                        list[i] = list1;
                    }
                }
            } else {
                if (list.length > 0) {
                    list = results.get(0);
                }
            }

        } catch (Exception ed2) {
            ed2.printStackTrace();
        }
        return list;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.geospatial.AbstractSpatialDbQuery#query(java.lang
     * .String, java.lang.String, java.lang.String[],
     * com.vividsolutions.jts.geom.Geometry, java.util.Map,
     * com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode,
     * java.lang.Integer)
     */
    @Override
    public SpatialQueryResult[] query(String dataSet, String theGeomField,
            String[] attributes, Geometry areaGeometry,
            Map<String, RequestConstraint> filter, SearchMode mode,
            Integer limit) throws SpatialException {
        if (true) {
            // If map resources start using SpatialQueryFactory, disable this
            // and start caching. Not worth it for point types
            return super.query(dataSet, theGeomField, attributes, areaGeometry,
                    filter, mode, limit);
        }
        SpatialDbQueryRequest request = new SpatialDbQueryRequest();
        Set<String> attrs = new HashSet<String>();
        if (attributes != null) {
            attrs.addAll(Arrays.asList(attributes));
        }
        attrs.add(GID);

        request.setConstraints(filter);
        request.addFields(attrs.toArray(new String[attrs.size()]));
        request.setGeometry(areaGeometry);
        request.setSearchMode(mode);
        request.setLimit(limit);
        if (theGeomField != null) {
            request.setGeometryField(theGeomField);
        } else {
            theGeomField = request.getGeometryField();
        }
        request.setReturnGeometry(false);
        request.setTable(dataSet);

        SpatialQueryResult[] rslts = executeRequest(request);
        if (rslts != null) {
            List<String> gidsToSearch = new ArrayList<String>();
            Map<String, SpatialQueryResult> toAddMap = new HashMap<String, SpatialQueryResult>();
            // Look up geometries in cache
            for (SpatialQueryResult rslt : rslts) {
                String gid = String.valueOf(rslt.attributes.get(GID));
                Geometry geom = GeometryCache.getGeometry(dataSet, gid,
                        theGeomField);
                if (geom != null) {
                    rslt.geometry = geom;
                } else {
                    gidsToSearch.add(gid);
                    toAddMap.put(gid, rslt);
                }
            }

            if (gidsToSearch.size() > 0) {
                Map<String, RequestConstraint> gidConstraints = new HashMap<String, RequestConstraint>();
                RequestConstraint rc = new RequestConstraint();
                rc.setConstraintType(ConstraintType.IN);
                rc.setConstraintValueList(gidsToSearch
                        .toArray(new String[gidsToSearch.size()]));
                gidConstraints.put(GID, rc);
                SpatialQueryResult[] geomRslts = super.query(dataSet,
                        theGeomField, new String[] { GID }, null,
                        gidConstraints, null, limit);
                for (SpatialQueryResult rslt : geomRslts) {
                    String gid = String.valueOf(rslt.attributes.get(GID));
                    SpatialQueryResult toAdd = toAddMap.get(gid);
                    toAdd.geometry = rslt.geometry;
                    GeometryCache.putGeometry(dataSet, gid, theGeomField,
                            toAdd.geometry);
                }

            }
        }
        return rslts;
    }
}
