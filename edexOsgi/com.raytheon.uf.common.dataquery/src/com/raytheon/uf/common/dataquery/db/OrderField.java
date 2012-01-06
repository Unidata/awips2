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

/**
 * Defines an order clause
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/19/08     #1531      bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class OrderField {

    /** The possible result orders */
    public enum ResultOrder {
        ASC, DESC
    };

    /** The field to order by */
    private String field;

    /** The class to which the order by field belongs */
    private String className;

    /** The order direction */
    private ResultOrder order;

    /**
     * Empty constructor
     */
    public OrderField() {

    }

    /**
     * Constructs a new OrderField
     * 
     * @param field
     *            The field to order by
     * @param className
     *            The class to which the order by field belongs
     * @param order
     *            The order direction
     */
    public OrderField(String field, String className, ResultOrder order) {
        this.field = field;
        this.className = className;
        this.order = order;
    }

    /**
     * Gets the field to order by
     * 
     * @return The field to order by
     */
    public String getField() {
        return field;
    }

    /**
     * Sets the field to order by
     * 
     * @param field
     *            The field to order by
     */
    public void setField(String field) {
        this.field = field;
    }

    /**
     * Gets the class to which the order by field belongs
     * 
     * @return The class to which the order by field belongs
     */
    public String getClassName() {
        return className;
    }

    /**
     * Sets the class to which the order by field belongs
     * 
     * @param className
     *            The class to which the order by field belongs
     */
    public void setClassName(String className) {
        this.className = className;
    }

    /**
     * Gets the order direction
     * 
     * @return The order direction
     */
    public ResultOrder getOrder() {
        return order;
    }

    /**
     * Sets the order direction
     * 
     * @param order
     *            The order direction
     */
    public void setOrder(ResultOrder order) {
        this.order = order;
    }

}
