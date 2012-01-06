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
package com.raytheon.uf.edex.database.handlers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderBy;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.RequestField;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.request.SpatialDbQueryRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Handler for spatial db queries
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SpatialDbQueryHandler implements
        IRequestHandler<SpatialDbQueryRequest> {

    private static final Map<ConstraintType, String> formatMap = new HashMap<ConstraintType, String>();

    static {
        formatMap.put(ConstraintType.BETWEEN, "BETWEEN %s");
        formatMap.put(ConstraintType.EQUALS, "= '%s'");
        formatMap.put(ConstraintType.GREATER_THAN, "> '%s'");
        formatMap.put(ConstraintType.GREATER_THAN_EQUALS, ">= '%s'");
        formatMap.put(ConstraintType.IN, "IN (%s)");
        formatMap.put(ConstraintType.LESS_THAN, "< '%s'");
        formatMap.put(ConstraintType.LESS_THAN_EQUALS, "<= '%s'");
        formatMap.put(ConstraintType.LIKE, "LIKE '%s'");
        formatMap.put(ConstraintType.NOT_EQUALS, "!= '%s'");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(SpatialDbQueryRequest request) throws Exception {
        String database = request.getDatabase();
        String schema = request.getSchema();
        String table = request.getTable();
        SearchMode mode = request.getSearchMode();
        Geometry geom = request.getGeometry();
        Map<String, RequestConstraint> constraints = request.getConstraints();
        List<RequestField> fields = request.getFields();
        Integer limit = request.getLimit();
        OrderBy orderBy = request.getOrderBy();
        String geometryField = request.getGeometryField();
        boolean returnGeom = request.isReturnGeometry();

        // Check arguments
        if (database == null) {
            throw new IllegalArgumentException("Database must be specified");
        }
        if (table == null) {
            throw new IllegalArgumentException("Table must be specified");
        }
        if (fields.size() == 0 && returnGeom == false) {
            throw new IllegalArgumentException(
                    "Must provide request fields or return geometry");
        }
        if (geometryField == null && (geom != null || returnGeom)) {
            throw new IllegalArgumentException(
                    "Must specify geometry field if geometry object set or returning geometry");
        }
        if (geom != null && mode == null) {
            throw new IllegalArgumentException(
                    "Must specify SearchMode for geometry query");
        }
        if (orderBy != null && (orderBy.mode == null || orderBy.field == null)) {
            throw new IllegalArgumentException(
                    "Order By field not properly set, mode and field cannot be null");
        }
        if (orderBy != null) {
            boolean found = false;
            for (RequestField field : fields) {
                if (orderBy.field.equalsIgnoreCase(field.field)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                throw new IllegalArgumentException(
                        "Order By field must be added as a request field");
            }
        }

        StringBuilder query = new StringBuilder(1000);
        query.append("SELECT ");
        boolean first = true;

        String[] fieldNames = new String[fields.size() + (returnGeom ? 1 : 0)];
        int i = 0;
        if (returnGeom) {
            fieldNames[i++] = geometryField;
            query.append("AsBinary(").append(geometryField).append(") as ")
                    .append(geometryField);
            first = false;
        }

        for (RequestField field : fields) {
            fieldNames[i++] = field.field;
            if (!first) {
                query.append(", ");
            }
            if (field.max) {
                query.append("MAX(");
            }
            query.append(field.field);
            if (field.max) {
                query.append(") as ").append(field.field);
            }
            first = false;
        }

        query.append(" FROM ");
        if (schema != null) {
            query.append(schema).append('.');
        }
        query.append(table);

        if (constraints.size() > 0 || geom != null) {
            query.append(" WHERE ");
        }

        first = true;
        for (Entry<String, RequestConstraint> entry : constraints.entrySet()) {
            String key = entry.getKey();
            RequestConstraint constraint = entry.getValue();
            if (!first) {
                query.append(" AND ");
            }
            first = false;

            query.append(key + " ");
            String value = constraint.getConstraintValue();
            String formatStr = formatMap.get(constraint.getConstraintType());
            if (formatStr != null) {
                first = false;
                switch (constraint.getConstraintType()) {
                case LIKE: {
                    value = "%" + value + "%";
                    break;
                }
                case IN: {
                    // Some people pass in the value with quotes
                    // like this "'thing1','thing2'"
                    //
                    // Some people pass in the value without quotes
                    // like this "thing1,thing2"
                    //
                    // This attempts to determine if it is without quotes
                    // and it adds quotes.
                    //
                    // There will not necessarily catch every case but it
                    // grabs the obvious one
                    if (!value.startsWith("'") || !value.endsWith("'")) {
                        value = "'" + value.replace(",", "','") + "'";
                    }
                    break;
                }
                }
                query.append(String.format(formatStr, value));
            }
        }

        if (geom != null) {
            if (!first) {
                query.append(" AND ");
            }
            first = false;

            String geomText = geom.toText();

            switch (mode) {
            case INTERSECTS:
                query.append("ST_Intersects(");
                break;
            case CONTAINS:
                query.append("ST_Contains(");
                break;
            case WITHIN:
                query.append("ST_Within(");
                break;
            case CLOSEST:
                query.append("ST_Distance(ST_GeomFromText('");
                query.append(geomText);
                query.append("', 4326), ");
                query.append(geometryField);
                query.append(") < 4.5 order by ST_Distance(");
                break;
            }

            query.append("ST_GeomFromText('");
            query.append(geomText);
            query.append("', 4326), ");
            query.append(geometryField);
            query.append(")");
        }

        if (limit != null) {
            query.append(" LIMIT ").append(limit);
        }

        if (orderBy != null) {
            query.append("ORDER BY ").append(orderBy.field).append(" ")
                    .append(orderBy.mode);
        }

        CoreDao dao = new CoreDao(DaoConfig.forDatabase(database));
        Object[] results = dao.executeSQLQuery(query.toString());
        List<Map<String, Object>> resultMaps = new ArrayList<Map<String, Object>>(
                results.length);
        for (Object obj : results) {
            if (obj instanceof Object[] == false) {
                obj = new Object[] { obj };
            }
            Object[] objs = (Object[]) obj;
            if (objs.length != fieldNames.length) {
                throw new Exception(
                        "Column count returned does not match expected column count");
            }
            Map<String, Object> resultMap = new HashMap<String, Object>(
                    objs.length * 2);
            for (i = 0; i < fieldNames.length; ++i) {
                resultMap.put(fieldNames[i], objs[i]);
            }
            resultMaps.add(resultMap);
        }

        DbQueryResponse response = new DbQueryResponse();
        response.setResults(resultMaps);
        return response;
    }
}
