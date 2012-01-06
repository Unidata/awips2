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

package com.raytheon.edex.uengine.tasks.query;

import java.util.List;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.dataquery.db.ReturnedField;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Abstract class that contains methods related to database queries.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *                          bphillip    Initial creation.
 * 9/21/2007    368         grichard    Moved plugin operations to subclass.
 * 6/04/2008    875         bphillip    Refactored to use DatabaseQuery.
 * 6/12/2008                M. Duff     Added another setSortBy method.
 * 
 * </pre>
 * 
 * @author bphillip
 */

public abstract class DbQuery extends ScriptTask {

    /** The name of the database in which this query fill examine */
    protected String database;

    /** The class in which the query will look at */
    protected String className;

    /** The data access object this query will use */
    protected CoreDao dao;

    /** The contents of the query */
    protected DatabaseQuery query;

    /**
     * Constructs a new DbQuery on the specified database
     * 
     * @param database
     *            The database name
     */
    public DbQuery(String database, String className) {
        this.database = database;
        this.className = className;
        query = new DatabaseQuery(className);
    }

    @Override
    public Object execute() throws Exception {
        dao = new CoreDao(DaoConfig.forClass(database, className));
        List<?> results = dao.queryByCriteria(query);
        return results;
    }

    /**
     * Adds a constraint to the query.
     * 
     * @param aName
     *            the name of the parameter
     * @param aValue
     *            the value of the parameter
     */
    public void addParameter(String aName, String aValue) {
        addParameter(aName, aValue, "=", this.className);
    }

    /**
     * Adds a constraint to the query.
     * 
     * @param aName
     *            the name of the parameter
     * @param aValue
     *            the value of the parameter
     * @param anOperand
     *            the operand of the parameter
     */
    public void addParameter(String aName, String aValue, String anOperand) {
        addParameter(aName, aValue, anOperand, this.className);
    }

    public void addParameter(String aName, String aValue, String operand,
            String className) {
        if (className == null) {
            query.addQueryParam(aName, aValue, operand, this.className);
        } else {
            query.addQueryParam(aName, aValue, operand, className);
        }
    }

    /**
     * Adds a list constraint to the query
     * 
     * @param field
     *            The name of the parameter
     * @param values
     *            The comma separated list of values
     */
    public void addList(String field, String values) {
        addList(field, values, className);
    }

    public void addList(String field, String values, String className) {
        if (className == null) {
            query.addQueryParam(field, values, "in", this.className);
        } else {
            query.addQueryParam(field, values, "in", className);
        }
    }

    /**
     * Sets the maximum number of results returned by this query
     * 
     * @param aCount
     *            The result count
     */
    public void setCount(int aCount) {
        query.setMaxResults(aCount);
    }

    /**
     * Sets a sort field
     * 
     * @param aSortBy
     *            The field to sort by
     * @param order
     *            The order. True for ascending. False for descending.
     */
    public void setSortBy(String aSortBy, boolean order) {
        setSortBy(aSortBy, order, className);
    }

    public void setSortBy(String aSortBy, boolean order, String className) {
        query.addOrder(aSortBy, order, className);
    }

    /**
     * Sets the sort fields
     * 
     * @param sortList
     *            The list of fields to sort by
     * @param order
     *            The order for all fields. True for ascending, false for
     *            descending.
     */
    public void setSortBy(String sortList, String order) {
        setSortBy(sortList, order, className);
    }

    public void setSortBy(String sortList, String order, String className) {
        if (sortList != null) {
            String[] names = sortList.split(",");
            for (String name : names) {

                String[] tokens = name.split("---");

                if (tokens.length == 1) {
                    query.addOrder(tokens[0], true, this.className);
                } else if (tokens.length == 2) {
                    query.addOrder(tokens[0],
                            tokens[2].equalsIgnoreCase("asc"), this.className);
                } else if (tokens.length == 3) {
                    query.addOrder(tokens[0],
                            tokens[2].equalsIgnoreCase("asc"), tokens[1]);
                }
            }
        }
    }

    // public void addReturnedField(String field) {
    // addReturnedField(field, className);
    // }

    public void addReturnedField(String field, String className) {
        if (className == null) {
            query.addReturnedField(field, this.className);
        } else {
            query.addReturnedField(field, className);
        }
    }

    public void addMaxReturnedField(String field, String className) {

        if (className == null) {
            className = this.className;
        }

        ReturnedField rf = new ReturnedField(field, className);
        rf.setFunction("MAX");
        query.addReturnedField(rf);
    }

    /**
     * Sets the returned field list.
     * 
     * @param nameList
     *            Comma separated list of columns to return
     */
    public void setReturnedFieldList(String nameList) {
        setReturnedFieldList(nameList, className);
    }

    public void setReturnedFieldList(String nameList, String className) {
        String[] names = nameList.split(",");
        for (String name : names) {
            String[] tokens = name.split("---");
            if (tokens.length > 1) {
                query.addReturnedField(tokens[0].trim(), tokens[1].trim());
            } else {
                query.addReturnedField(tokens[0].trim(), this.className);
            }
        }
    }

    /**
     * Sets the distinct field on the query
     * 
     * @param distinctField
     *            The field to query as distinct
     */
    public void setDistinctField(String distinctField) {
        setDistinctField(distinctField, this.className);
    }

    public void setDistinctField(String distinctField, String className) {
        if (className == null) {
            query.addDistinctParameter(distinctField, this.className);
        } else {
            query.addDistinctParameter(distinctField, className);
        }
    }

    public void addJoinField(String class1, String class2, String field1,
            String field2) {
        if (field2 == null) {
            query.addJoinField(class1, class2, field1);
        } else {
            query.addJoinField(class1, class2, field1, field2);
        }
    }

    public void setJoinList(String joinList) {
        String[] names = joinList.split(",");

        for (String name : names) {
            String[] tokens = name.split("---");
            if (tokens.length == 3) {
                addJoinField(tokens[0], tokens[1], tokens[2], tokens[2]);
            } else if (tokens.length == 4) {
                addJoinField(tokens[0], tokens[1], tokens[2], tokens[3]);
            }
        }
    }

}
