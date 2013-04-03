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
package com.raytheon.uf.common.dataquery.requests;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to query a database
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2010            mschenke     Initial creation
 * Mar 19, 2013 1807       rferrel     Added orderBy to the toString.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DbQueryRequest implements IServerRequest {

    @DynamicSerialize
    public static enum OrderMode {
        ASC, DESC;
    }

    @DynamicSerialize
    public static class OrderBy {
        @DynamicSerializeElement
        public String field;

        @DynamicSerializeElement
        public OrderMode mode = OrderMode.ASC;

        /**
         * @return the field
         */
        public String getField() {
            return field;
        }

        /**
         * @param field
         *            the field to set
         */
        public void setField(String field) {
            this.field = field;
        }

        /**
         * @return the mode
         */
        public OrderMode getMode() {
            return mode;
        }

        /**
         * @param mode
         *            the mode to set
         */
        public void setMode(OrderMode mode) {
            this.mode = mode;
        }

    }

    @DynamicSerialize
    public static class RequestField {
        @DynamicSerializeElement
        public String field;

        @DynamicSerializeElement
        public boolean max = false;

        public String getField() {
            return field;
        }

        public void setField(String field) {
            this.field = field;
        }

        public boolean isMax() {
            return max;
        }

        public void setMax(boolean max) {
            this.max = max;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "RequestField [field=" + field + ", max=" + max + "]";
        }
    }

    @DynamicSerializeElement
    private String database = "metadata";

    @DynamicSerializeElement
    private List<RequestField> fields = new ArrayList<RequestField>();

    @DynamicSerializeElement
    private boolean distinct = false;

    @DynamicSerializeElement
    private Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();

    @DynamicSerializeElement
    private String entityClass;

    @DynamicSerializeElement
    private OrderBy orderBy;

    @DynamicSerializeElement
    private Integer limit;

    public String getDatabase() {
        return database;
    }

    /**
     * The database to query, defaults to metadata
     * 
     * @param database
     */
    public void setDatabase(String database) {
        this.database = database;
    }

    /**
     * @return the entityClass
     */
    public String getEntityClass() {
        return entityClass;
    }

    /**
     * @param entityClass
     *            the entityClass to set
     */
    public void setEntityClass(Class<?> entityClass) {
        setEntityClass(entityClass.getName());
    }

    /**
     * @param entityClass
     *            the entityClass to set
     */
    public void setEntityClass(String entityClass) {
        this.entityClass = entityClass;
    }

    public List<RequestField> getFields() {
        return fields;
    }

    /**
     * The fully qualified hibernate name of the fields to query
     * (dataTime.refTime, modelInfo.parameterName)
     * 
     * @param fields
     */
    public void setFields(List<RequestField> fields) {
        this.fields = fields;
    }

    public Map<String, RequestConstraint> getConstraints() {
        return constraints;
    }

    /**
     * The request constraints of the query, must contain the key "pluginName"
     * with constraint value equal to the plugin (ie radar)
     * 
     * @param constraints
     */
    public void setConstraints(Map<String, RequestConstraint> constraints) {
        this.constraints = constraints;
    }

    public void addConstraint(String key, RequestConstraint constraint) {
        constraints.put(key, constraint);
    }

    public void addFields(String[] fields) {
        for (String f : fields) {
            addRequestField(f);
        }
    }

    public void addRequestField(String name) {
        addRequestField(name, false);
    }

    public void addRequestField(String name, boolean max) {
        RequestField field = new RequestField();
        field.field = name;
        field.max = max;
        fields.add(field);
    }

    public boolean isDistinct() {
        return distinct;
    }

    public void setDistinct(boolean distinct) {
        this.distinct = distinct;
    }

    /**
     * @return the orderBy
     */
    public OrderBy getOrderBy() {
        return orderBy;
    }

    /**
     * @param orderBy
     *            the orderBy to set
     */
    public void setOrderBy(OrderBy orderBy) {
        this.orderBy = orderBy;
    }

    public void setOrderByField(String field) {
        setOrderByField(field, OrderMode.ASC);
    }

    public void setOrderByField(String field, OrderMode mode) {
        OrderBy orderBy = new OrderBy();
        orderBy.field = field;
        orderBy.mode = mode;
        setOrderBy(orderBy);
    }

    /**
     * @return the limit
     */
    public Integer getLimit() {
        return limit;
    }

    /**
     * @param limit
     *            the limit to set
     */
    public void setLimit(Integer limit) {
        this.limit = limit;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "DbQueryRequest [database="
                + database
                + ", entityClass="
                + entityClass
                + ", fields="
                + fields
                + ", distinct="
                + distinct
                + ", constraints="
                + constraints
                + ", orderBy="
                + (orderBy == null ? "null" : String.format(
                        "[field=%s, mode=%s]", orderBy.field,
                        orderBy.mode.toString()))
                + (limit == null ? "" : ", limit=" + limit) + "]";
    }

}
