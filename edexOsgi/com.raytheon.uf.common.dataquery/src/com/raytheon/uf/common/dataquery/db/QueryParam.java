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

package com.raytheon.uf.common.dataquery.db;

import java.util.HashMap;
import java.util.Iterator;

/**
 * Encapsulates the query parameters for a database query
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/29/08     #875       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class QueryParam {
    
    /** Enumeration containing the logic operands */
    public enum QueryOperand {
        EQUALS, NOTEQUALS, LESSTHAN, LESSTHANEQUALS, GREATERTHAN, GREATERTHANEQUALS, IN, LIKE, ILIKE, BETWEEN, ISNULL, ISNOTNULL
    };

    /**
     * A mapping between the enumeration and the string representation of an
     * operand
     */
    private static HashMap<String, QueryOperand> operandMap = new HashMap<String, QueryOperand>();
    static {
        operandMap.put("=", QueryOperand.EQUALS);
        operandMap.put("!=", QueryOperand.NOTEQUALS);
        operandMap.put("<", QueryOperand.LESSTHAN);
        operandMap.put("<=", QueryOperand.LESSTHANEQUALS);
        operandMap.put(">", QueryOperand.GREATERTHAN);
        operandMap.put(">=", QueryOperand.GREATERTHANEQUALS);
        operandMap.put("in", QueryOperand.IN);
        operandMap.put("like", QueryOperand.LIKE);
        operandMap.put("ilike", QueryOperand.ILIKE);
        operandMap.put("between", QueryOperand.BETWEEN);
        operandMap.put("isNotNull", QueryOperand.ISNOTNULL);
        operandMap.put("isNull", QueryOperand.ISNULL);
    }

    /** The query field */
    private String field;

    /** The query value */
    private Object value;

    /** The query operand */
    private String operand = "=";
    
    private String className;

    /**
     * Creates a new QueryParam. Operand defaults to equals
     * 
     * @param field
     *            The field
     * @param value
     *            The value
     */
    public QueryParam(String field, Object value) {
        this.field = field;
        this.value = value;
    }

    /**
     * Creates a new QueryParam.
     * 
     * @param field
     *            The field
     * @param value
     *            The value
     * @param operand
     *            The operand
     */
    public QueryParam(String field, Object value, String operand) {
        this.field = field;
        this.value = value;
        this.operand = operand;
    }
    
    public QueryParam(String field, Object value, String operand,String className) {
        this.field = field;
        this.value = value;
        this.operand = operand;
        this.className = className;
    }
    
    public QueryParam(String field, Object value, QueryOperand operand) {
        this.field = field;
        this.value = value;
        this.operand = QueryParam.reverseTranslateOperand(operand);
    }
    
    public QueryParam(String field, Object value, QueryOperand operand,String className) {
        this.field = field;
        this.value = value;
        this.operand = QueryParam.reverseTranslateOperand(operand);
        this.className = className;
    }
    

    /**
     * Translates the string representation of an operand to the enumeration
     * value
     * 
     * @param operand
     *            The string representation of an operand
     * @return The enumeration value of the operand
     */
    public static QueryOperand translateOperand(String operand) {
        return operandMap.get(operand);
    }

    public static String reverseTranslateOperand(QueryOperand operand) {
        String key = null;
        for (Iterator<String> it = operandMap.keySet().iterator(); it.hasNext();) {
            key = it.next();
            if (operandMap.get(key).equals(operand)) {
                return key;
            }
        }
        return "=";
    }

    public String toString() {
        return new StringBuffer().append(field).append(" ")
                .append(this.operand).append(" ").append(this.value).toString();
    }

    public String getField() {
        return field;
    }

    public Object getValue() {
        return value;
    }

    public String getOperand() {
        return operand;
    }

    public void setField(String field) {
        this.field = field;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public void setOperand(String operand) {
        this.operand = operand;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
