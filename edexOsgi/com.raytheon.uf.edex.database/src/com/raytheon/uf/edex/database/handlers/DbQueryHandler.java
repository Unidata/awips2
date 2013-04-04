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

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.dataquery.db.ReturnedField;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.RequestField;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Handler class for DbQueryRequests, queries the database given map of
 * constraints and parameters to retrieve
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class DbQueryHandler implements IRequestHandler<DbQueryRequest> {

    @Override
    public DbQueryResponse handleRequest(DbQueryRequest request)
            throws Exception {
        String dbName = request.getDatabase();
        List<RequestField> fields = request.getFields();
        Map<String, RequestConstraint> constraints = request.getConstraints();
        String pluginName = null;
        String entity = request.getEntityClass();
        if (constraints.containsKey("pluginName")) {
            pluginName = constraints.remove("pluginName").getConstraintValue();
            if (entity == null) {
                // Lookup entity based on plugin name
                entity = PluginFactory.getInstance()
                        .getPluginRecordClass(pluginName).getName();
            }
        } else if (entity == null) {
            throw new IllegalArgumentException(
                    "Constraints must contain pluginName or request must specify entity class");
        }

        DatabaseQuery dbQuery = new DatabaseQuery(entity);
        dbQuery.setMaxResults(request.getLimit());
        dbQuery.setDistinct(request.isDistinct());
        if (fields != null) {
            for (int i = 0; i < fields.size(); ++i) {
                RequestField field = fields.get(i);
                ReturnedField rf = new ReturnedField(field.field);
                if (field.max) {
                    rf.setFunction("MAX");
                }
                dbQuery.addReturnedField(rf);
            }
        }

        for (String key : constraints.keySet()) {
            QueryOperand op = null;
            RequestConstraint constraint = constraints.get(key);
            switch (constraint.getConstraintType()) {
            case BETWEEN: {
                op = QueryOperand.BETWEEN;
                break;
            }
            case EQUALS: {
                op = QueryOperand.EQUALS;
                break;
            }
            case GREATER_THAN: {
                op = QueryOperand.GREATERTHAN;
                break;
            }
            case GREATER_THAN_EQUALS: {
                op = QueryOperand.GREATERTHANEQUALS;
                break;
            }
            case ILIKE: {
                op = QueryOperand.ILIKE;
                break;
            }
            case IN: {
                op = QueryOperand.IN;
                break;
            }
            case ISNULL: {
                op = QueryOperand.ISNULL;
                break;
            }
            case ISNOTNULL: {
                op = QueryOperand.ISNOTNULL;
                break;
            }
            case LESS_THAN: {
                op = QueryOperand.LESSTHAN;
                break;
            }
            case LESS_THAN_EQUALS: {
                op = QueryOperand.LESSTHANEQUALS;
                break;
            }
            case LIKE: {
                op = QueryOperand.LIKE;
                break;
            }
            case NOT_EQUALS: {
                op = QueryOperand.NOTEQUALS;
                break;
            }
            default: {
                op = QueryOperand.EQUALS;
                break;
            }
            }
            QueryParam param = new QueryParam(key,
                    constraint.getConstraintValue(), op);
            dbQuery.addQueryParam(param);
        }

        List<?> vals = new CoreDao(DaoConfig.forDatabase(dbName))
                .queryByCriteria(dbQuery);

        DbQueryResponse response = new DbQueryResponse();
        List<Map<String, Object>> results = new ArrayList<Map<String, Object>>();
        int mapSize = (fields != null ? (int) (fields.size() * 1.25) + 1 : 2);
        for (int i = 0; i < vals.size(); ++i) {
            Map<String, Object> objectMap = new HashMap<String, Object>(mapSize);
            Object row = vals.get(i);
            if (row == null) {
                continue;
            }
            if (fields == null || fields.size() == 0) {
                if (row instanceof PluginDataObject) {
                    ((PluginDataObject) row).setPluginName(pluginName);
                }
                objectMap.put(null, row);
            } else if (fields.size() == 1) {
                objectMap.put(fields.get(0).field, row);
            } else {
                Object[] cols = (Object[]) row;
                for (int j = 0; j < cols.length; ++j) {
                    Object obj = cols[j];
                    objectMap.put(fields.get(j).field, obj);
                }
            }
            results.add(objectMap);
        }

        response.setResults(results);
        return response;
    }
}
