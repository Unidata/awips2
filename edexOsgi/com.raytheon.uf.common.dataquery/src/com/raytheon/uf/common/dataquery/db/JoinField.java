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
 * Defines a join between two classes.
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
public class JoinField {

    /** The first class participating in the join */
    private String joinClassOne;

    /** The second class participating in the join */
    private String joinClassTwo;

    /** The field in the first class participating in the join */
    private String joinFieldOne;

    /** The field in the second class participating in the join */
    private String joinFieldTwo;

    /**
     * Empty constructor
     */
    public JoinField() {

    }

    /**
     * Constructs a new join field
     * 
     * @param joinFieldOne
     *            The field from the first class participating in the join
     * @param joinFieldTwo
     *            The field from the second class particpating in the join
     * @param joinClassOne
     *            The first class participating in the join
     * @param joinClassTwo
     *            The second class participating in the join
     */
    public JoinField(String joinFieldOne, String joinFieldTwo,
            String joinClassOne, String joinClassTwo) {
        this.joinFieldOne = joinFieldOne;
        this.joinFieldTwo = joinFieldTwo;
        this.joinClassOne = joinClassOne;
        this.joinClassTwo = joinClassTwo;
    }

    /**
     * Gets the field from the first class
     * 
     * @return The field from the first class
     */
    public String getJoinFieldOne() {
        return joinFieldOne;
    }

    /**
     * Sets the field from the first class
     * 
     * @param joinFieldOne
     *            The field from the first class
     */
    public void setJoinFieldOne(String joinFieldOne) {
        this.joinFieldOne = joinFieldOne;
    }

    /**
     * Gets the field from the second class
     * 
     * @return The field from the second class
     */
    public String getJoinFieldTwo() {
        return joinFieldTwo;
    }

    /**
     * Sets the field from the second class
     * 
     * @param joinFieldTwo
     */
    public void setJoinFieldTwo(String joinFieldTwo) {
        this.joinFieldTwo = joinFieldTwo;
    }

    /**
     * Gets the first class in the join
     * 
     * @return The first class in the join
     */
    public String getJoinClassOne() {
        return joinClassOne;
    }

    /**
     * Sets the first class in the join
     * 
     * @param joinClassOne
     *            The first class in the join
     */
    public void setJoinClassOne(String joinClassOne) {
        this.joinClassOne = joinClassOne;
    }

    /**
     * Gets the second class in the join
     * 
     * @return The second class in the join
     */
    public String getJoinClassTwo() {
        return joinClassTwo;
    }

    /**
     * Sets the second class in the join
     * 
     * @param joinClassTwo
     *            The second class in the join
     */
    public void setJoinClassTwo(String joinClassTwo) {
        this.joinClassTwo = joinClassTwo;
    }

}
