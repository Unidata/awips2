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

package com.raytheon.uf.viz.core.catalog;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderMode;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.RequestField;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Performs a database query.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/05/08     #875       bphillip    Initial Creation.
 * 06/12/08                M. Duff     Added ordering and convenience methods.
 * 10/13/08                chammack    Improve error handling on server-side exceptions
 * Mar 20, 2013 #1638      mschenke    Rewrote to use DbQueryRequest under the hood to remove use of ScriptCreator
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DbQuery {

    private DbQueryRequest request = new DbQueryRequest();

    private ResultOrder ascending = ResultOrder.ASC;

    public DbQuery(Class<?> clazz, String databaseName) {
        request.setEntityClass(clazz);
        request.setDatabase(databaseName);
    }

    public DbQuery(String pluginName) {
        setPlugin(pluginName);
    }

    /**
     * Performs a query
     * 
     * @return The response
     * @throws VizException
     */
    public List<Object[]> performQuery() throws VizException {
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(request);
        List<RequestField> fields = request.getFields();
        if (fields == null || fields.size() == 0) {
            fields = new ArrayList<RequestField>();
            RequestField entity = new RequestField();
            entity.setField(DbQueryResponse.ENTITY_RESULT_KEY);
            fields.add(entity);
        }

        List<Map<String, Object>> results = response.getResults();
        List<Object[]> rval = new ArrayList<Object[]>(results.size());

        for (Map<String, Object> result : results) {
            Object[] objs = new Object[fields.size()];
            int i = 0;
            for (RequestField field : fields) {
                objs[i++] = result.get(field.field);
            }
            rval.add(objs);
        }

        return rval;
    }

    /**
     * @param plugin
     *            the plugin to set
     */
    public void setPlugin(String plugin) {
        request.addConstraint("pluginName", new RequestConstraint(plugin));
    }

    /**
     * @param distinctField
     *            the distinctField to set
     */
    public void setDistinctField(String distinctField) {
        request.setDistinct(true);
        addColumn(distinctField);
    }

    public void addConstraint(String key, ConstraintType operator, Object value) {
        RequestConstraint constraint = new RequestConstraint();

        switch (operator) {
        case BETWEEN:
            constraint.setBetweenValueList(((String) value).split("--"));
            break;
        case IN:
            if (value instanceof String) {
                constraint.setConstraintValue((String) value);
            } else {
                constraint.setConstraintValueList((String[]) value);
            }
            break;
        default:
            constraint.setConstraintValue(String.valueOf(value));
            break;
        }
        constraint.setConstraintType(operator);
        request.addConstraint(key, constraint);
    }

    public void addConstraint(String key, RequestConstraint constraint) {
        addConstraint(key, constraint.getConstraintType(),
                constraint.getConstraintValue());
    }

    public void addConstraint(String key, Object value) {
        addConstraint(key, ConstraintType.EQUALS, value);
    }

    /**
     * Adds a column to the list
     * 
     * @param columnName
     *            A column name
     */
    public void addColumn(String columnName) {
        request.addRequestField(columnName);
    }

    public void addOrderBy(String columnName, ResultOrder order) {
        request.setOrderByField(columnName,
                order == ResultOrder.ASC ? OrderMode.ASC : OrderMode.DESC);
    }

    @Deprecated
    public void addOrderBy(String columnName, ResultOrder order,
            String className) {
        addOrderBy(columnName, order);
    }

    /**
     * Adds a column to the list
     * 
     * @param columnName
     *            A column name
     */
    public void addOrderBy(String columnName) {
        addOrderBy(columnName, ascending, null);
    }

    /**
     * Set the sort order to ascending?
     * 
     * @param ascending
     *            - true if ascending, false if descending
     */
    public void setOrderAscending(ResultOrder order) {
        ascending = order;
    }

    /**
     * Limit the number of rows returned
     * 
     * @param maxResults
     *            - the maximum number of rows to return use -999 for all rows
     */
    public void setMaxResults(Integer maxResults) {
        request.setLimit(maxResults);
    }

}