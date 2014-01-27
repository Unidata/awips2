/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.filter;

import org.hibernate.criterion.LikeExpression;
import org.hibernate.criterion.MatchMode;

/**
 * Hibernate like expression that allows for a custom escape character
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class EscapingLikeExpression extends LikeExpression {

    private static final long serialVersionUID = 1L;

    protected String propertyName;

    protected String value;

    protected Character escapeChar;

    /**
     * @param propertyName
     * @param value
     */
    public EscapingLikeExpression(String propertyName, String value) {
        super(propertyName, value);
        this.propertyName = propertyName;
        this.value = value;
    }

    /**
     * @param propertyName
     * @param value
     * @param matchMode
     */
    public EscapingLikeExpression(String propertyName, String value,
            MatchMode matchMode) {
        super(propertyName, value, matchMode);
    }

    /**
     * @param propertyName
     * @param value
     * @param escapeChar
     * @param ignoreCase
     */
    public EscapingLikeExpression(String propertyName, String value,
            Character escapeChar, boolean ignoreCase) {
        super(propertyName, value, escapeChar, ignoreCase);
        this.propertyName = propertyName;
        this.value = value;
        this.escapeChar = escapeChar;
    }

    /**
     * @param propertyName
     * @param value
     * @param matchMode
     * @param escapeChar
     * @param ignoreCase
     */
    public EscapingLikeExpression(String propertyName, String value,
            MatchMode matchMode, Character escapeChar, boolean ignoreCase) {
        super(propertyName, value, matchMode, escapeChar, ignoreCase);
        this.propertyName = propertyName;
        this.escapeChar = escapeChar;
        this.value = value;
    }

    public String toString(){
        return String.format("%s like '%s' escape '%s'", propertyName, value,
                escapeChar);
    }

}
