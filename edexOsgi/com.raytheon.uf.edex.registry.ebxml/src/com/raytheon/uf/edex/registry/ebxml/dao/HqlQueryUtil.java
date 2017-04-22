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
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

/**
 * 
 * Utility class used for assembling HQL queries
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012 #363       bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class HqlQueryUtil {

    public static final String SELECT = " select ";

    public static final String FROM = " from ";

    public static final String WHERE = " where ";

    public static final String TICK = "'";

    public static final String IN = " in ";

    public static final String OPEN_PAREN = " (";

    public static final String CLOSE_PAREN = ") ";

    public static final String COMMA = ", ";

    public static final String AND = " and ";

    public static final String OR = " or ";

    public static final String EQUALS = " = ";

    public static final String LIKE = " like ";

    public static final String OBJ = " obj ";

    public static final String DOT = ".";

    public static final String OBJ_DOT = "obj" + DOT;

    public static final String INNER_JOIN = " inner join ";

    /**
     * Assembles a complete HQL query based on one field, operator, and value
     * 
     * @param clazz
     *            The object type the query is looking for
     * @param field
     *            The field to query on
     * @param operator
     *            The comparison operator to use
     * @param value
     *            The value to use in the query clause
     * @return The assembled HQL query
     */
    public static String assembleSingleParamQuery(Class<?> clazz, String field,
            String operator, Object value) {
        StringBuilder str = new StringBuilder();
        assembleSingleParamQuery(str, clazz, field, operator, value);
        return str.toString();
    }

    /**
     * Assembles a complete HQL query based on one field, operator, and value.
     * This method returns the StringBuilder reference so that additional
     * clauses may be added using the same StringBuilder object
     * 
     * @param str
     *            The StringBuilder to use to assemble the HQL query
     * @param clazz
     *            The object type the query is looking for
     * @param field
     *            The field to query on
     * @param operator
     *            The comparison operator to use
     * @param value
     *            The value to use in the query clause
     */
    public static void assembleSingleParamQuery(StringBuilder str,
            Class<?> clazz, String field, String operator, Object value) {
        str.append(FROM).append(clazz.getName()).append(OBJ).append(WHERE);
        if ((value instanceof List<?>) && ((List<?>) value).size() == 1) {
            operator = "=";
            value = ((List<?>) value).get(0);
        }
        assembleSingleParamClause(str, field, operator, value);
    }

    /**
     * Assembles a clause for an HQL query using the 'in' operator
     * 
     * @param <T>
     *            Generic type
     * @param str
     *            The StringBuilder to use to assemble the HQL query clause
     * @param field
     *            The field to query on
     * @param items
     *            The items which will be queried for
     */
    public static <T extends Object> void assembleInClause(StringBuilder str,
            String field, List<T> items) {
        // If the list provided is empty, we add a placeholder so Hibernate does
        // not throw errors because it tried to do an 'in' query on an empty
        // list
        if (items.isEmpty()) {
            str.append(OPEN_PAREN);
            str.append("'____EMPTY_LIST_____'");
            str.append(CLOSE_PAREN);
            return;
        }
        str.append(OPEN_PAREN);
        for (int i = 0; i < items.size() - 1; i++) {
            str.append(TICK).append(items.get(i)).append(TICK).append(COMMA);
        }
        str.append(TICK).append(items.get(items.size() - 1)).append(TICK)
                .append(CLOSE_PAREN);
    }

    /**
     * Assembles a clause for an HQL query
     * 
     * @param str
     *            The StringBuilder to use to assemble the HQL query
     * @param field
     *            The field to query on
     * @param operator
     *            The comparison operator to use
     * @param value
     *            The value to use in the query clause
     */
    @SuppressWarnings("unchecked")
    public static void assembleSingleParamClause(StringBuilder str,
            String field, String operator, Object value) {
        str.append(OBJ_DOT).append(field).append(" ").append(operator);
        if (operator.trim().equalsIgnoreCase("in")) {
            assembleInClause(str, field, (List<String>) value);
        } else {
            if (value instanceof String) {
                str.append(TICK).append(value).append(TICK);
            } else {
                str.append(value);
            }
        }
    }
}
