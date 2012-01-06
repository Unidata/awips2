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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;

/**
 * SqlBuilder is used to build SQL statements for execution. It provides support
 * for Inserts, Updates, Deletes, AND Selects.
 * 
 * Example:
 * 
 * <code>
 * SqlBuilder builder = new SqlBuilder("TABLE_NAME");
 * String firstName = "John";
 * String lastName = "Doe";
 * builder.setSqlType(SqlBuilder.UPDATE);
 * builder.addString("FIRST_NAME",firstName);
 * builder.addString("LAST_NAME",lastName);
 * builder.setWhereClause(" LAST_NAME = 'Doe'");
 * </code>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2008 1617       grichard    Initial creation.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class SqlBuilder {

    /**
     * <code>UPDATE</code> Tells the builder to create an UPDATE statement.
     */
    public static final int UPDATE = 0;

    /**
     * <code>INSERT</code> Tells the builder to create an INSERT statement.
     */
    public static final int INSERT = 1;

    /**
     * <code>DELETE</code> Tells the builder to create a DELETE statement.
     */
    public static final int DELETE = 2;

    /**
     * <code>SELECT</code> Tells the builder to create a SELECT statement.
     */
    public static final int SELECT = 3;

    private int sqlType = 0;

    private String tableName = "";

    private ArrayList fields = null;

    private ArrayList values = null;

    private String whereClause = "";

    private boolean useWhereClause = false;

    /**
     * SqlBuilder constructor that takes the table name as a String.
     * 
     * @param tableName
     *            String The name of the table in the database.
     * 
     */
    public SqlBuilder(String tableName) {
        this.tableName = tableName;
        fields = new ArrayList();
        values = new ArrayList();
    }

    /**
     * Clears the builder's internal Arrays.
     * 
     */
    public void clear() {
        fields.clear();
        values.clear();
    }

    /**
     * Sets the type of SQL statement.
     * 
     * @param sqlType
     *            int The type of sql statement being built. The choices are:<br>
     *            <br>
     *            <code>SqlBuilder.INSERT</code><br>
     *            <code>SqlBuilder.UPDATE</code><br>
     *            <code>SqlBuilder.DELETE</code><br>
     */
    public void setSqlType(int sqlType) {
        this.sqlType = sqlType;
    }

    /**
     * Adds a primitive int to the SQL statement for the given fieldName.
     * 
     * @param value
     *            int The value to include in the SQL statement.
     * @param fieldName
     *            String The column name to include in the SQL statement.
     */
    public void addInt(String fieldName, int value) {
        fields.add(fieldName);
        values.add(new Integer(value));
    }

    /**
     * Adds a primitive long to the SQL statement for the given fieldName.
     * 
     * @param value
     *            long The value to include in the SQL statement.
     * @param fieldName
     *            String The column name to include in the SQL statement.
     */
    public void addLong(String fieldName, long value) {
        fields.add(fieldName);
        values.add(new Long(value));
    }
    
    public void addDouble(String fieldName, double value) {
        fields.add(fieldName);
        values.add(new Double(value));
    }

    /**
     * Adds a string to the SQL statement for the given fieldName but add it as
     * if it were a number. In other words it leaves off the apostrophes needed
     * for a string.
     * 
     * @param value
     *            String The String value to include in the SQL statement as a
     *            number.
     * @param fieldName
     *            String The column name to include in the SQL statement.
     */
    public void addNumString(String fieldName, String value) {
        fields.add(fieldName);
        values.add(value);
    }

    /**
     * Adds a string to the SQL statement for the given fieldName.
     * 
     * @param value
     *            String The String value to include in the SQL statement.
     * @param fieldName
     *            String The column name to include in the SQL statement.
     */
    public void addString(String fieldName, String value) {
        fields.add(fieldName);
        values.add("'" + value + "'");
    }

    /**
     * Adds a boolean to the SQL statement for the given fieldName as a string.
     * 
     * @param value
     *            String The String value to include in the SQL statement.
     * @param fieldName
     *            String The column name to include in the SQL statement.
     */
    public void addBoolean(String fieldName, String value) {
        fields.add(fieldName);
        values.add("'" + value + "'");
    }

    /**
     * Adds a boolean to the SQL statement for the given fieldName as a string.
     * 
     * @param value
     *            boolean The boolean value to include in the SQL statement as a
     *            single character.
     * @param fieldName
     *            String The column name to include in the SQL statement.
     */
    public void addBooleanAsChar(String fieldName, boolean value) {
        fields.add(fieldName);
        if (value) {
            values.add("'1'");
        } else {
            values.add("'0'");
        }
    }

    /**
     * Adds a boolean to the SQL statement for the given fieldName as a string.
     * 
     * @param value
     *            boolean The boolean value to include in the SQL statement as a
     *            string.
     * @param fieldName
     *            String The column name to include in the SQL statement.
     */
    public void addBoolean(String fieldName, boolean value) {
        fields.add(fieldName);
        values.add(new Boolean(value));
    }

    /**
     * Adds a date using the Oracle Date Format
     * 
     * @param fieldName
     *            String The column name to include in the SQL statement.
     * @param value
     *            String The date value
     * @param dateFormat
     *            String The Oracle date format
     */
    public void addDate(String fieldName, String value, String dateFormat) {
        fields.add(fieldName);
        values.add("STR_TO_DATE('" + value + "', '" + dateFormat + "')");
        // this.values.add("'" + value + "'");
    }

    /**
     * Use the where clause (true/false)
     * 
     * @param b
     *            true to use where clause
     */
    public void useWhereClause(boolean b) {
        useWhereClause = b;
    }

    /**
     * Sets the where clause for the SQL statement. Do not forget this!
     * 
     * @param whereClause
     *            String The column name to include in the SQL statement.
     */
    public void setWhereClause(String whereClause) {
        if (whereClause.toUpperCase().indexOf("WHERE") == -1) {
            this.whereClause = " WHERE " + whereClause;
        } else {
            this.whereClause = whereClause;
        }
    }

    /**
     * Sets the column names for the select statement
     * 
     * @param column
     *            String The column name to include in the SQL statement
     */
    public void setColumn(String column) {
        fields.add(column);
    }

    /**
     * Overloaded toString method that prints the SQL statement that was built.
     * 
     * @return String The SQL statement that was built.
     */
    @Override
    public String toString() {
        StringBuffer sql = new StringBuffer();
        if (sqlType == 0) {
            sql.append("UPDATE ");
            sql.append(tableName);
            sql.append(" SET ");
            for (int i = 0; i < fields.size(); i++) {
                if (i == 0) {
                    sql.append(fields.get(i) + " = ");
                    sql.append(values.get(i));
                } else {
                    sql.append("," + fields.get(i) + " = ");
                    sql.append(values.get(i));
                }
            }
            sql.append(whereClause);
        } else if (sqlType == 1) {
            sql.append("INSERT INTO ");
            sql.append(tableName);
            sql.append(" (");
            for (int i = 0; i < fields.size(); i++) {
                if (i == 0) {
                    sql.append(fields.get(i));
                } else {
                    sql.append("," + fields.get(i));
                }
            }
            sql.append(") VALUES (");
            for (int i = 0; i < values.size(); i++) {
                if (i == 0) {
                    sql.append(values.get(i));
                } else {
                    sql.append("," + values.get(i));
                }
            }
            sql.append(")");
        } else if (sqlType == 2) {
            sql.append("DELETE FROM ");
            sql.append(tableName);
            if (useWhereClause) {
                sql.append(" " + whereClause);
            } else {
                sql.append(" WHERE ");
                for (int i = 0; i < fields.size(); i++) {
                    if (i == 0) {
                        sql.append(fields.get(i) + " = ");
                        sql.append(values.get(i));
                    } else {
                        sql.append(" AND " + fields.get(i) + " = ");
                        sql.append(values.get(i));
                    }
                }
            }
        } else if (sqlType == 3) {
            sql.append("SELECT ");
            for (int i = 0; i < fields.size(); i++) {
                if (fields.size() - 1 == i) {
                    sql.append(fields.get(i) + " ");
                } else {
                    sql.append(fields.get(i) + ", ");
                }
            }
            sql.append(" from " + tableName + " ");
            if (useWhereClause) {
                sql.append(whereClause);
            }
        }
        return sql.toString();
    }

    /**
     * Returns the sql statement in a human readable form
     * 
     * @return sql statement
     */
    public String toStringNoSql() {
        StringBuffer sql = new StringBuffer();
        if (sqlType == 0) {
            for (int i = 0; i < fields.size(); i++) {
                if (i == 0) {
                    sql.append(fields.get(i) + " = ");
                    sql.append(values.get(i));
                } else {
                    sql.append("," + fields.get(i) + " = ");
                    sql.append(values.get(i));
                }
            }
            sql.append(whereClause);
        } else if (sqlType == 1) {
            sql.append(tableName);
            sql.append(" (");
            for (int i = 0; i < fields.size(); i++) {
                if (i == 0) {
                    sql.append(fields.get(i));
                } else {
                    sql.append("," + fields.get(i));
                }
            }
            sql.append(") VALUES (");
            for (int i = 0; i < values.size(); i++) {
                if (i == 0) {
                    sql.append(values.get(i));
                } else {
                    sql.append("," + values.get(i));
                }
            }
            sql.append(")");
        } else if (sqlType == 2) {
            if (useWhereClause) {
                sql.append(whereClause);
            } else {
                sql.append("WHERE ");
                for (int i = 0; i < fields.size(); i++) {
                    if (i == 0) {
                        sql.append(fields.get(i) + " = ");
                        sql.append(values.get(i));
                    } else {
                        sql.append(" AND " + fields.get(i) + " = ");
                        sql.append(values.get(i));
                    }
                }
            }
        }
        return sql.toString();
    }

    /**
     * Sets the table name used for building the sql statement.
     * 
     * @param tableName
     *            String The table name used for the SQL operation.
     */
    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

}
