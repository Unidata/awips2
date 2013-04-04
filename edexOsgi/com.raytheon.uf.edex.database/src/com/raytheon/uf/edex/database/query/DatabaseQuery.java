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

package com.raytheon.uf.edex.database.query;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.SessionFactory;
import org.hibernate.metadata.ClassMetadata;

import com.raytheon.uf.common.dataquery.db.JoinField;
import com.raytheon.uf.common.dataquery.db.OrderField;
import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.dataquery.db.ReturnedField;
import com.raytheon.uf.common.util.ConvertUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Encapsulates a database query. This object can be used for criteria queries
 * or prepared queries.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/29/08     #875       bphillip    Initial Creation
 * 06/03/08     #875       bphillip    Added returned fields
 * 09/19/08     #1531      bphillip    Refactored to include join capability
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DatabaseQuery {

    /** The maximum number or results returned by this query */
    private Integer maxResults;

    /** The list of parameters for this query */
    private List<QueryParam> parameters;

    /** The distinct parameter for this query */
    private ReturnedField distinctParameter;

    /** The list of columns to be returned by the query */
    private List<ReturnedField> returnedFields = new ArrayList<ReturnedField>();

    /** The map of order fields */
    private List<OrderField> orderFields = new ArrayList<OrderField>();

    /** A map of the classes that will be joined together to execute this query */
    private Map<String, String> joinedClasses = new HashMap<String, String>();

    /** A list of JoinFields which define the columns to join */
    private List<JoinField> joinFields = new ArrayList<JoinField>();

    /** The primary entity in which this query is querying for */
    private String entityName;

    private static final Pattern DOT_PATTERN = Pattern.compile("\\.");

    private static final Pattern COMMA_PATTERN = Pattern
            .compile(QueryUtil.COMMA);

    /**
     * Constructs a new DatabaseQuery
     */
    public DatabaseQuery(String entityName) {
        this.entityName = entityName;
        this.addJoinedClass(entityName);
        this.maxResults = null;
        this.parameters = new ArrayList<QueryParam>();
    }

    /**
     * Constructs a new DatabaseQuery
     */
    public DatabaseQuery(Class<?> entityName) {
        this.entityName = entityName.getName();
        this.addJoinedClass(entityName.getName());
        this.maxResults = null;
        this.parameters = new ArrayList<QueryParam>();
    }

    /**
     * Constructs a new DatabaseQuery with a pre-constructed set of parameters.
     * Maximum results defaults to unlimited
     * 
     * @param parameters
     *            The list of query parameters for this query
     */
    public DatabaseQuery(Class<?> entityName, List<QueryParam> parameters) {
        this.entityName = entityName.getName();
        this.addJoinedClass(entityName.getName());
        this.maxResults = null;
        this.parameters = parameters;
    }

    /**
     * Constructs a new DatabaseQuery with a pre-constructed set of parameters
     * and a specified max results
     * 
     * @param parameters
     *            The list of query parameters for this query
     * @param maxResults
     *            The maximum number of results to return
     */
    public DatabaseQuery(Class<?> entityName, List<QueryParam> parameters,
            Integer maxResults) {
        this.entityName = entityName.getName();
        this.addJoinedClass(entityName.getName());
        this.maxResults = maxResults;
        this.parameters = parameters;
    }

    /**
     * Constructs a new DatabaseQuery with maxResults set
     * 
     * @param maxResults
     *            sets the max result number
     */
    public DatabaseQuery(Class<?> entityName, Integer maxResults) {
        this.entityName = entityName.getName();
        this.addJoinedClass(entityName.getName());
        this.maxResults = maxResults;
        this.parameters = new ArrayList<QueryParam>();
    }

    /**
     * Adds a constraint to this query
     * 
     * @param field
     *            The field to query against
     * @param value
     *            The value to query for
     * @param operand
     *            The operator
     * @param className
     *            The class to which the field belongs
     */
    public void addQueryParam(String field, Object value, String operand,
            String className) {
        parameters.add(new QueryParam(field, value, operand, className));
        this.addJoinedClass(className);
    }

    /**
     * Adds a constraint to this query
     * 
     * @param param
     *            The query parameter to add
     */
    public void addQueryParam(QueryParam param) {
        addQueryParam(param.getField(), param.getValue(), param.getOperand(),
                entityName);
    }

    /**
     * Adds a constraint to this query
     * 
     * @param field
     *            The field to query against
     * @param value
     *            The value to query for
     */
    public void addQueryParam(String field, Object value) {
        addQueryParam(field, value, "=", entityName);
    }

    /**
     * Adds a constraint to this query
     * 
     * @param field
     *            The field to query against
     * @param value
     *            The value to query for
     * @param operand
     *            The logic operand to use
     */
    public void addQueryParam(String field, Object value, String operand) {
        addQueryParam(field, value, operand, entityName);
    }

    /**
     * Adds a constraint to this query
     * 
     * @param field
     *            The field to query against
     * @param value
     *            The value to query for
     * @param operand
     *            The logic operand to use
     */
    public void addQueryParam(String field, Object value, QueryOperand operand) {
        addQueryParam(field, value,
                QueryParam.reverseTranslateOperand(operand), entityName);
    }

    /**
     * Adds a distinct parameter to this query
     * 
     * @param field
     *            The distinct field name
     * @param className
     *            The class to which the field belongs
     */
    public void addDistinctParameter(String field, String className) {
        if (distinctParameter == null) {
            distinctParameter = new ReturnedField(field, className);
            returnedFields.add(0, new ReturnedField(field, className));
        } else {
            returnedFields.add(new ReturnedField(field, className));
        }
        addJoinedClass(className);
    }

    /**
     * Set if the query should return distinct results. This method is meant to
     * replace addDistinctParamter*(...) as the group of returned fields is
     * treated as distinct, not one parameter
     */
    public void setDistinct(boolean distinct) {
        if (!distinct) {
            distinctParameter = null;
        } else {
            distinctParameter = new ReturnedField();
        }
    }

    /**
     * Check if the query returns distinct results
     * 
     * @return
     */
    public boolean isDistinct() {
        return distinctParameter != null;
    }

    /**
     * Adds a distinct parameter to this query
     * 
     * @param field
     *            The distinct field name
     */
    public void addDistinctParameter(String field) {
        addDistinctParameter(field, entityName);
    }

    /**
     * Adds a collection of distinct fields to this query
     * 
     * @param fields
     *            The distinct fields
     * @param className
     *            The class to which these fields belong
     */
    public void addDistinctParameter(Collection<String> fields, String className) {
        if (fields != null) {
            for (String field : fields) {
                addDistinctParameter(field, className);
            }
        }
    }

    /**
     * Adds a collection of distinct fields to this query
     * 
     * @param fields
     *            The distinct fields
     */
    public void addDistinctParameter(Collection<String> fields) {
        addDistinctParameter(fields, entityName);
    }

    /**
     * Adds a returned field to this query
     * 
     * @param field
     *            The field to return
     * @param className
     *            The class to which this field belongs
     */
    public void addReturnedField(String field, String className) {
        if (field != null) {
            returnedFields.add(new ReturnedField(field, className));
            this.addJoinedClass(className);
        }
    }

    /**
     * Adds a returned field to this query
     * 
     * @param field
     *            The field to return
     */
    public void addReturnedField(String field) {
        addReturnedField(field, entityName);
    }

    public void addReturnedField(ReturnedField field) {
        if (field.getClassName() == null) {
            field.setClassName(entityName);
        }
        this.returnedFields.add(field);
        this.addJoinedClass(field.getClassName());
    }

    /**
     * Adds a list of returned fields to this query
     * 
     * @param fields
     *            The fields to return
     * @param className
     *            The class to which the fields belong
     */
    public void addReturnedField(Collection<?> fields, String className) {
        if (fields != null) {
            Object obj = null;
            for (Iterator<?> it = fields.iterator(); it.hasNext();) {
                obj = it.next();
                if (obj instanceof ReturnedField) {
                    addReturnedField((ReturnedField) obj);
                } else {
                    addReturnedField((String) obj, className);
                }
            }
        }
    }

    /**
     * Adds a list of return fields to the query
     * 
     * @param fields
     *            The fields to return
     */
    public void addReturnedField(Collection<?> fields) {
        addReturnedField(fields, entityName);
    }

    public void addOrder(String field, boolean ascending, String className) {
        if (field != null) {
            if (ascending) {
                this.orderFields.add(new OrderField(field, className,
                        ResultOrder.ASC));
            } else {
                this.orderFields.add(new OrderField(field, className,
                        ResultOrder.DESC));
            }
            this.addJoinedClass(className);
        }
    }

    /**
     * Adds an order field to the query
     * 
     * @param field
     *            The field to order by
     * @param order
     *            The order
     */
    public void addOrder(String field, ResultOrder order) {
        addOrder(field, order.equals(ResultOrder.ASC), entityName);
    }

    /**
     * Adds an order field to the query
     * 
     * @param field
     *            The field to order by
     * @param ascending
     *            True if ascending order, else descending
     */
    public void addOrder(String field, boolean ascending) {
        addOrder(field, ascending, entityName);
    }

    /**
     * Adds an order field to the query
     * 
     * @param field
     *            The field to order by
     * @param order
     *            The order
     * @param className
     *            The class to which this field belongs
     */
    public void addOrder(String field, ResultOrder order, String className) {
        addOrder(field, order.equals(ResultOrder.ASC), className);
    }

    /**
     * Adds order fields to the query
     * 
     * @param orders
     *            Map of the fields and their orders
     */
    public void addOrder(Map<String, ResultOrder> orders) {
        if (orders != null) {
            String key = null;
            for (Iterator<String> it = orders.keySet().iterator(); it.hasNext();) {
                key = it.next();
                addOrder(key, orders.get(key).equals(ResultOrder.ASC),
                        entityName);
            }
        }
    }

    /**
     * Adds a join definition to this query
     * 
     * @param class1
     *            The first class to be joined
     * @param class2
     *            The second class to be joined
     * @param field1
     *            The field in the first class participating in the join
     * @param field2
     *            The field in the second class participating in the join
     */
    public void addJoinField(String class1, String class2, String field1,
            String field2) {
        this.addJoinedClass(class1);
        this.addJoinedClass(class2);
        joinFields.add(new JoinField(field1, field2, class1, class2));
    }

    /**
     * Adds a join definition to this query assuming the field name is the same
     * in both joined classes
     * 
     * @param class1
     *            The first class to be joined
     * @param class2
     *            The second class to be joined
     * @param field
     *            The common field name in each class participating in the join
     */
    public void addJoinField(String class1, String class2, String field) {
        addJoinField(field, field, class1, class2);
    }

    /**
     * Adds a class to the map of classes being queried by this query
     * 
     * @param className
     *            The class to add
     */
    private void addJoinedClass(String className) {
        if (className != null && !joinedClasses.containsKey(className)) {
            joinedClasses.put(
                    className,
                    QueryUtil.QUERY_OBJ_NAME
                            + String.valueOf(joinedClasses.size()));
        }
    }

    /**
     * Executes a prepared statement. This method is deprecated
     * 
     * @deprecated
     * @param query
     * @return
     * @throws HibernateException
     * @throws Exception
     */
    @Deprecated
    public SQLQuery populatePreparedStatement(SQLQuery query)
            throws HibernateException, Exception {
        for (QueryParam param : parameters) {
            if (param.getValue() instanceof Collection<?>) {
                query.setParameterList(param.getField(),
                        (Collection<?>) param.getValue());
            } else {
                query.setParameter(param.getField(), param.getValue());
            }
        }
        return query;
    }

    /**
     * Generates the delete statement based on the provided criteria
     * 
     * @return The HQL delete statement
     */
    @SuppressWarnings("rawtypes")
    public String createHQLDelete() {
        StringBuffer deleteString = new StringBuffer();
        deleteString.append(QueryUtil.DELETE_CLAUSE);

        /*
         * Add the classes(tables) to be queried
         */
        String key = null;
        for (Iterator<String> it = joinedClasses.keySet().iterator(); it
                .hasNext();) {
            key = it.next();
            deleteString.append(key).append(joinedClasses.get(key));
            if (it.hasNext()) {
                deleteString.append(QueryUtil.COMMA);
            }
        }

        deleteString.append(" ");

        /*
         * Assemble the WHERE clause of the query.
         */
        if (!parameters.isEmpty()) {
            deleteString.append(QueryUtil.WHERE_CLAUSE);

            int constraintIndex = 0;
            for (int i = 0; i < parameters.size(); i++) {

                if (parameters.get(i).getOperand().equalsIgnoreCase("between")) {
                    addWhereConstraint(deleteString, parameters.get(i), ">=",
                            constraintIndex++);
                    deleteString.append(QueryUtil.AND_CLAUSE);
                    addWhereConstraint(deleteString, parameters.get(i), "<=",
                            constraintIndex++);
                } else if (parameters.get(i).getOperand()
                        .equalsIgnoreCase("in")) {
                    deleteString.append(parameters.get(i).getField());
                    deleteString.append(" in (");
                    if (parameters.get(i).getValue() instanceof String) {
                        String[] split = COMMA_PATTERN
                                .split((String) parameters.get(i).getValue());
                        for (int j = 0; j < split.length; j++) {
                            deleteString.append(QueryUtil.COLON);
                            deleteString.append(QueryUtil.QUERY_CONSTRAINT
                                    + constraintIndex++);
                            if (j != (split.length - 1)) {
                                deleteString.append(QueryUtil.COMMA);
                            }
                        }
                    } else if (parameters.get(i).getValue() instanceof List) {
                        for (int j = 0; j < ((List) parameters.get(i)
                                .getValue()).size(); j++) {
                            deleteString.append(QueryUtil.COLON);
                            deleteString.append(QueryUtil.QUERY_CONSTRAINT
                                    + constraintIndex++);
                            if (j != ((List) parameters.get(i).getValue())
                                    .size() - 1) {
                                deleteString.append(QueryUtil.COMMA);
                            }
                        }
                    }
                    deleteString.append(") ");
                } else if (parameters.get(i).getOperand()
                        .equalsIgnoreCase("greater_than")) {
                    addWhereConstraint(deleteString, parameters.get(i), ">",
                            constraintIndex++);
                } else {
                    addWhereConstraint(deleteString, parameters.get(i),
                            constraintIndex++);
                }
                if (i != parameters.size() - 1) {
                    deleteString.append(QueryUtil.AND_CLAUSE);
                }
            }
        }

        return deleteString.toString();
    }

    /**
     * Generates the HQL query from the criteria specified by this class
     * 
     * @return
     */
    public String createHQLQuery() {
        StringBuffer queryString = new StringBuffer();

        /*
         * Add the fields to be returned by the query
         */
        if (!returnedFields.isEmpty()) {
            queryString.append(QueryUtil.SELECT_CLAUSE);
            if (isDistinct()) {
                queryString.append(QueryUtil.DISTINCT_CLAUSE);
            }
            for (int i = 0; i < returnedFields.size(); i++) {
                if (returnedFields.get(i).getFunction() != null) {
                    queryString.append(QueryUtil.SPACE)
                            .append(returnedFields.get(i).getFunction())
                            .append(QueryUtil.OPENPEREN);
                }
                queryString
                        .append(joinedClasses.get(returnedFields.get(i)
                                .getClassName())).append(QueryUtil.DOT);
                queryString.append(returnedFields.get(i));

                if (returnedFields.get(i).getFunction() != null) {
                    queryString.append(QueryUtil.CLOSEDPEREN);
                }

                if (i != returnedFields.size() - 1) {
                    queryString.append(QueryUtil.COMMA);
                }
            }
        }

        queryString.append(QueryUtil.FROM_CLAUSE);

        /*
         * Add the classes(tables) to be queried
         */
        String key = null;
        for (Iterator<String> it = joinedClasses.keySet().iterator(); it
                .hasNext();) {
            key = it.next();
            queryString.append(key).append(joinedClasses.get(key));
            if (it.hasNext()) {
                queryString.append(QueryUtil.COMMA);
            }
        }

        queryString.append(" ");

        /*
         * Assemble the WHERE clause of the query.
         */
        if (!parameters.isEmpty()) {
            queryString.append(QueryUtil.WHERE_CLAUSE);

            int constraintIndex = 0;
            for (int i = 0; i < parameters.size(); i++) {

                if (parameters.get(i).getOperand().equalsIgnoreCase("between")) {
                    addWhereConstraint(queryString, parameters.get(i), ">=",
                            constraintIndex++);
                    queryString.append(QueryUtil.AND_CLAUSE);
                    addWhereConstraint(queryString, parameters.get(i), "<=",
                            constraintIndex++);
                } else if (parameters.get(i).getOperand()
                        .equalsIgnoreCase("in")) {
                    queryString.append(parameters.get(i).getField());
                    queryString.append(" in (");
                    if (parameters.get(i).getValue() instanceof String) {
                        String[] split = COMMA_PATTERN
                                .split((String) parameters.get(i).getValue());
                        for (int j = 0; j < split.length; j++) {
                            queryString.append(QueryUtil.COLON);
                            queryString.append(QueryUtil.QUERY_CONSTRAINT
                                    + constraintIndex++);
                            if (j != (split.length - 1)) {
                                queryString.append(QueryUtil.COMMA);
                            }
                        }
                    } else if (parameters.get(i).getValue() instanceof List) {
                        for (int j = 0; j < ((List) parameters.get(i)
                                .getValue()).size(); j++) {
                            queryString.append(QueryUtil.COLON);
                            queryString.append(QueryUtil.QUERY_CONSTRAINT
                                    + constraintIndex++);
                            if (j != ((List) parameters.get(i).getValue())
                                    .size() - 1) {
                                queryString.append(QueryUtil.COMMA);
                            }
                        }
                    }
                    queryString.append(") ");
                } else if (parameters.get(i).getOperand()
                        .equalsIgnoreCase("greater_than")) {
                    addWhereConstraint(queryString, parameters.get(i), ">",
                            constraintIndex++);
                } else {
                    addWhereConstraint(queryString, parameters.get(i),
                            constraintIndex++);
                }
                if (i != parameters.size() - 1) {
                    queryString.append(QueryUtil.AND_CLAUSE);
                }
            }
        }

        /*
         * Adds any join behavior specified
         */
        if (!joinFields.isEmpty()) {
            if (!parameters.isEmpty()) {
                queryString.append(QueryUtil.AND_CLAUSE);
            } else {
                queryString.append(QueryUtil.WHERE_CLAUSE);
            }
            for (int i = 0; i < joinFields.size(); i++) {
                queryString.append(
                        joinedClasses.get(joinFields.get(i).getJoinClassOne()))
                        .append(QueryUtil.DOT);
                queryString.append(joinFields.get(i).getJoinFieldOne()).append(
                        QueryUtil.EQUALS);
                queryString.append(
                        joinedClasses.get(joinFields.get(i).getJoinClassTwo()))
                        .append(QueryUtil.DOT);
                queryString.append(joinFields.get(i).getJoinFieldTwo());
                if (i != joinFields.size() - 1) {
                    queryString.append(QueryUtil.AND_CLAUSE);
                }
            }
        }

        /*
         * Adds the order by fields
         */
        if (!orderFields.isEmpty()) {
            queryString.append(QueryUtil.ORDER_BY);

            for (int i = 0; i < orderFields.size(); i++) {
                queryString.append(
                        joinedClasses.get(orderFields.get(i).getClassName()))
                        .append(QueryUtil.DOT);
                queryString.append(orderFields.get(i).getField()).append(" ")
                        .append(orderFields.get(i).getOrder());
                if (i != orderFields.size() - 1) {
                    queryString.append(QueryUtil.COMMA);
                }
            }
        }
        return queryString.toString();
    }

    /**
     * Convenience method to add a constraint to add the WHERE clause
     * 
     * @param queryString
     *            The current query string
     * @param field
     *            The field
     * @param operand
     *            The operand
     * @param constraintIndex
     *            The current constraint count
     */
    private void addWhereConstraint(StringBuffer queryString, QueryParam field,
            String operand, int constraintIndex) {

        if (operand.equalsIgnoreCase("ilike")) {
            queryString.append("upper(");
            queryString.append(joinedClasses.get(field.getClassName())).append(
                    QueryUtil.DOT);
            queryString.append(field.getField()).append(")");
            queryString.append(" ").append("=").append(" ");
        } else {
            queryString.append(joinedClasses.get(field.getClassName())).append(
                    QueryUtil.DOT);
            queryString.append(field.getField());
            if (operand.equalsIgnoreCase("isnull")) {
                queryString.append(QueryUtil.ISNULL);
            } else if (operand.equalsIgnoreCase("isnotnull")) {
                queryString.append(QueryUtil.ISNOTNULL);
            } else {
                queryString.append(" ").append(operand).append(" ");
            }
        }

        if (operand.equalsIgnoreCase("ilike")) {
            queryString.append("upper(").append(QueryUtil.COLON)
                    .append(QueryUtil.QUERY_CONSTRAINT + constraintIndex)
                    .append(")");
        } else {
            if (!operand.equalsIgnoreCase("isnull")
                    && !operand.equalsIgnoreCase("isnotnull")) {
                queryString.append(QueryUtil.COLON).append(
                        QueryUtil.QUERY_CONSTRAINT + constraintIndex);
            }
        }

    }

    /**
     * Convenience method to add a constraint to add the WHERE clause
     * 
     * @param queryString
     *            The current query string
     * @param field
     *            The field
     * @param constraintIndex
     *            The current constraint count
     */
    private void addWhereConstraint(StringBuffer queryString, QueryParam field,
            int constraintIndex) {
        addWhereConstraint(queryString, field, field.getOperand(),
                constraintIndex);
    }

    /**
     * Populates the constraint values into the prepared query.
     * 
     * @param query
     *            The prepared query
     * @param sessionFactory
     *            The Hibernate session factory used for getting class metadata
     *            in order to correctly convert object types
     * @return The populated query
     */
    @SuppressWarnings("unchecked")
    public Query populateHQLQuery(Query query, SessionFactory sessionFactory)
            throws DataAccessLayerException {

        Object value = null;

        int constraintIndex = 0;
        for (int i = 0; i < parameters.size(); i++) {
            if (parameters.get(i).getOperand().equalsIgnoreCase("isnull")
                    || parameters.get(i).getOperand()
                            .equalsIgnoreCase("isnotnull")) {
                constraintIndex++;
                continue;
            }
            try {
                value = convertParameter(parameters.get(i), sessionFactory);
                if (parameters.get(i).getOperand().equalsIgnoreCase("between")) {
                    query.setParameter(QueryUtil.QUERY_CONSTRAINT
                            + constraintIndex++, ((Object[]) value)[0]);
                    query.setParameter(QueryUtil.QUERY_CONSTRAINT
                            + constraintIndex++, ((Object[]) value)[1]);
                } else if (parameters.get(i).getOperand()
                        .equalsIgnoreCase("in")) {

                    for (int j = 0; j < ((List<Object>) value).size(); j++) {
                        query.setParameter(QueryUtil.QUERY_CONSTRAINT
                                + constraintIndex++,
                                ((List<Object>) value).get(j));
                    }
                } else {
                    query.setParameter(QueryUtil.QUERY_CONSTRAINT
                            + constraintIndex++, value);
                }
            } catch (Exception e) {
                throw new DataAccessLayerException(
                        "Error populating prepared query", e);
            }
        }

        return query;
    }

    /**
     * Converts a parameter value from a string value to the necessary type
     * 
     * @param param
     *            The parameter to be converted
     * @param sessionFactory
     *            The session factory for determining the desired type
     * @return The converted parameter
     * @throws DataAccessLayerException
     * @throws Exception
     *             If errors occur during reflection
     */
    @SuppressWarnings("unchecked")
    private Object convertParameter(QueryParam param,
            SessionFactory sessionFactory) throws DataAccessLayerException {

        ClassMetadata metadata = sessionFactory.getClassMetadata(param
                .getClassName());
        String field = param.getField();
        Object value = param.getValue();

        Class<?> returnedClass = null;
        if (field.contains(".")) {
            String[] tokens = DOT_PATTERN.split(field);

            try {
                returnedClass = Class.forName(metadata.getEntityName());
            } catch (ClassNotFoundException e2) {
                throw new DataAccessLayerException("Cannot find class: "
                        + metadata.getEntityName());
            }

            for (int i = 0; i < tokens.length; i++) {
                try {
                    returnedClass = returnedClass.getDeclaredField(tokens[i])
                            .getType();
                } catch (NoSuchFieldException e) {
                    boolean found = false;
                    Class<?> clazz = returnedClass;
                    while (!found
                            && !clazz.getSuperclass().equals(Object.class)) {
                        clazz = clazz.getSuperclass();
                        try {
                            returnedClass = clazz.getDeclaredField(tokens[i])
                                    .getType();
                            found = true;
                        } catch (NoSuchFieldException e1) {
                            continue;
                        }
                    }
                    if (!found) {
                        throw new DataAccessLayerException(
                                "Unable to find field", e);
                    }
                }
            }
        } else {
            returnedClass = metadata.getPropertyType(field).getReturnedClass();
        }

        if (value instanceof String) {
            switch (QueryParam.translateOperand(param.getOperand())) {
            case BETWEEN:
                String[] tokens = ((String) value).split("--");
                value = new Object[2];

                ((Object[]) value)[0] = ConvertUtil.convertObject(tokens[0],
                        returnedClass);
                ((Object[]) value)[1] = ConvertUtil.convertObject(tokens[1],
                        returnedClass);
                break;
            case IN:
                String[] valueList = ((String) value).split(",");
                value = new ArrayList<Object>();
                for (String val : valueList) {
                    ((ArrayList<Object>) value).add(ConvertUtil.convertObject(
                            (val).trim(), returnedClass));
                }
                break;
            case ISNOTNULL:
            case ISNULL:
                break;
            default:
                value = ConvertUtil
                        .convertObject((String) value, returnedClass);
                break;
            }

        } else if (value instanceof List) {
            for (int j = 0; j < ((List) value).size(); j++) {
                if (((List) value).get(0) instanceof String) {
                    ((List) value).add(ConvertUtil.convertObject(
                            (String) ((List) value).remove(0), returnedClass));
                }
            }
        }

        return value;
    }

    /**
     * Returns the distinct parameter
     * 
     * @return The distinct parameter
     */
    public ReturnedField getDistinctParameter() {
        return distinctParameter;
    }

    /**
     * Gets the constraints to the query
     * 
     * @return The constraints
     */
    public List<QueryParam> getParameters() {
        return parameters;
    }

    /**
     * Gets the returned field names
     * 
     * @return A list of the returned field names
     */
    public List<String> getReturnedFieldNames() {
        List<String> fieldNames = new ArrayList<String>();
        for (ReturnedField field : returnedFields) {
            fieldNames.add(field.getField());
        }
        return fieldNames;
    }

    /**
     * Gets the returned fields
     * 
     * @return The returned fields
     */
    public List<ReturnedField> getReturnedFields() {
        return returnedFields;
    }

    /**
     * Gets the result limit
     * 
     * @return The result limit
     */
    public Integer getMaxResults() {
        return maxResults;
    }

    /**
     * Sets the result limit
     * 
     * @param maxResults
     *            The result limit
     */
    public void setMaxResults(Integer maxResults) {
        this.maxResults = maxResults;
    }

    /**
     * Gets the primary entity being queried for
     * 
     * @return The primary entity being queried for
     */
    public String getEntityName() {
        return entityName;
    }

    /**
     * Sets the primary entity being queried for
     * 
     * @param entityName
     *            The primary entity being queried for
     */
    public void setEntityName(String entityName) {
        this.entityName = entityName;
    }
}
