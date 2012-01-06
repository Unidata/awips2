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
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.JoinField;
import com.raytheon.uf.common.dataquery.db.OrderField;
import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.dataquery.db.ReturnedField;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.exception.VizServerSideException;

public class DbQuery {

    /** The database to query */
    protected String dbName;

    /** The class to query against */
    protected String className;

    protected String plugin = "satellite";

    protected ReturnedField distinctField;

    protected String distinctClass;

    protected List<ReturnedField> columns = new ArrayList<ReturnedField>();

    protected List<OrderField> orderBy = new ArrayList<OrderField>();

    protected List<JoinField> joinFields = new ArrayList<JoinField>();

    protected ResultOrder ascending = ResultOrder.ASC;

    protected Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();

    protected Integer maxResults;

    public DbQuery(Class<?> clazz, String databaseName) {
        this.className = clazz.getName();
        this.dbName = databaseName;
    }

    public DbQuery(String pluginName) {
        try {
            this.className = RecordFactory.getInstance()
                    .getPluginClass(pluginName).getName();
            this.dbName = "metadata";
            this.plugin = pluginName;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Performs a query
     * 
     * @return The response
     * @throws VizException
     */
    @SuppressWarnings("unchecked")
    public List<Object[]> performQuery() throws VizException {
        String query = assembleQuery();
        // System.out.println("[" + query + "]");

        Message message = Connector.getInstance().connectMessage(query, null,
                60000);
        AbstractResponseMessage[] absresponses = message.getBody()
                .getResponses();

        List<Object[]> retVal = new ArrayList<Object[]>();
        for (AbstractResponseMessage response : absresponses) {

            if (response instanceof ResponseMessageGeneric) {
                if (((ResponseMessageGeneric) response).getContents() instanceof List) {
                    ArrayList<?> list = (ArrayList) ((ResponseMessageGeneric) response)
                            .getContents();
                    Object[] vals = new Object[list.size()];
                    for (int i = 0; i < list.size(); i++) {
                        vals[i] = list.get(i);
                    }
                    retVal.add(vals);
                } else {
                    Object obj = ((ResponseMessageGeneric) response)
                            .getContents();
                    Object[] vals = new Object[1];
                    vals[0] = obj;
                    retVal.add(vals);
                }
            } else if (response instanceof ResponseMessageError) {
                ResponseMessageError rme = (ResponseMessageError) response;
                VizServerSideException innerException = new VizServerSideException(
                        rme.toString());
                throw new VizServerSideException(rme.getErrorMsg(),
                        innerException);
            }
        }
        return retVal;
    }

    public List<Object[]> performQuery(boolean bool) throws VizException {
        String query = assembleQuery();
        System.out.println("[" + query + "]");

        Message message = Connector.getInstance().connectMessage(query, null,
                60000);
        AbstractResponseMessage[] absresponses = message.getBody()
                .getResponses();

        List<Object[]> retVal = new ArrayList<Object[]>();
        for (AbstractResponseMessage response : absresponses) {

            if (response instanceof ResponseMessageGeneric) {
                if (((ResponseMessageGeneric) response).getContents() instanceof List) {
                    ArrayList<?> list = (ArrayList<?>) ((ResponseMessageGeneric) response)
                            .getContents();
                    Object[] vals = new Object[list.size()];
                    for (int i = 0; i < list.size(); i++) {
                        vals[i] = list.get(i);
                    }
                    retVal.add(vals);
                } else {
                    Object obj = ((ResponseMessageGeneric) response)
                            .getContents();
                    Object[] vals = new Object[1];
                    vals[0] = obj;
                    retVal.add(vals);
                }
            } else if (response instanceof ResponseMessageError) {
                ResponseMessageError rme = (ResponseMessageError) response;
                VizServerSideException innerException = new VizServerSideException(
                        rme.toString());
                throw new VizServerSideException(rme.getErrorMsg(),
                        innerException);
            }
        }
        return retVal;
    }

    /**
     * Assembles the database query
     * 
     * @param dbName
     *            The database name
     * @param className
     *            The class name to query for
     * @param distinctField
     *            The distinct field (optional)
     * @param columns
     *            The fields to be returned (required)
     * @param constraints
     *            The constraints to apply to the query
     * @return
     * @throws VizException
     */
    private String assembleQuery() throws VizException {

        addConstraint("pluginName", plugin);
        addConstraint("dbName", dbName);
        addConstraint("className", className);

        if (distinctField != null) {
            addConstraint("distinctField",
                    new RequestConstraint(distinctField.getField()));
            addConstraint("distinctClass", new RequestConstraint(distinctClass));
        }

        if (!columns.isEmpty()) {
            StringBuffer fieldList = new StringBuffer();
            for (int i = 0; i < columns.size(); i++) {
                fieldList.append(columns.get(i).getField() + "---"
                        + columns.get(i).getClassName());
                if (i != columns.size() - 1) {
                    fieldList.append(",");
                }

            }
            addConstraint("columns", fieldList);
        }

        if (!joinFields.isEmpty()) {
            StringBuffer joinFieldList = new StringBuffer();
            for (int i = 0; i < joinFields.size(); i++) {
                joinFieldList.append(joinFields.get(i).getJoinClassOne())
                        .append("---");
                joinFieldList.append(joinFields.get(i).getJoinClassTwo())
                        .append("---");
                joinFieldList.append(joinFields.get(i).getJoinFieldOne());
                if (joinFields.get(i).getJoinFieldTwo() != null) {
                    joinFieldList.append("---");
                    joinFieldList.append(joinFields.get(i).getJoinFieldTwo());
                }
                if (i != joinFields.size() - 1) {
                    joinFieldList.append(",");
                }
            }
            addConstraint("joinFields", joinFieldList);
        }

        if (!orderBy.isEmpty()) {
            StringBuffer orderList = new StringBuffer();
            for (int i = 0; i < orderBy.size(); i++) {
                orderList.append(orderBy.get(i).getField() + "---"
                        + orderBy.get(i).getClassName() + "---"
                        + orderBy.get(i).getOrder());
                if (i != orderBy.size() - 1) {
                    orderList.append(",");
                }
            }
            addConstraint("orderBy", orderList);
            addConstraint("sortOrder", String.valueOf(ascending));
        }

        if (maxResults != null) {
            addConstraint("maxResults", maxResults);
        }
        return ScriptCreator.createScript(constraints, Integer.MAX_VALUE,
                "dbquery");
    }

    /**
     * @param plugin
     *            the plugin to set
     */
    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }

    public void setDistinctField(String distinctField, String className) {
        distinctClass = className;
        this.distinctField = new ReturnedField(distinctField, className);
    }

    /**
     * @param distinctField
     *            the distinctField to set
     */
    public void setDistinctField(String distinctField) {
        setDistinctField(distinctField, className);
    }

    public void addConstraint(String key, ConstraintType operator,
            Object value, String className) {
        if (constraints == null) {
            constraints = new HashMap<String, RequestConstraint>();
        }
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
        constraints.put(key, constraint);
    }

    public void addConstraint(String key, RequestConstraint constraint) {
        addConstraint(key, constraint.getConstraintType(),
                constraint.getConstraintValue(), className);
    }

    public void addConstraint(String key, ConstraintType operator, Object value) {
        addConstraint(key, operator, value, className);
    }

    public void addConstraint(String key, Object value) {
        addConstraint(key, ConstraintType.EQUALS, value, className);
    }

    public void addColumn(String columnName, String className) {
        columns.add(new ReturnedField(columnName, className));
    }

    /**
     * Adds a column to the list
     * 
     * @param columnName
     *            A column name
     */
    public void addColumn(String columnName) {
        addColumn(columnName, className);
    }

    public void addOrderBy(String columnName, ResultOrder order,
            String className) {
        orderBy.add(new OrderField(columnName, className, order));
    }

    /**
     * Adds a column to the list
     * 
     * @param columnName
     *            A column name
     */
    public void addOrderBy(String columnName) {
        addOrderBy(columnName, ascending, className);
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
        this.maxResults = maxResults;
    }

    public void addJoinField(Class<?> class1, Class<?> class2, String field1,
            String field2) {
        joinFields.add(new JoinField(field1, field2, class1.getName(), class2
                .getName()));
    }

    public void addJoinField(Class<?> class1, Class<?> class2, String field) {
        addJoinField(class1, class2, field, field);
    }
}