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
package com.raytheon.uf.viz.datadelivery.system;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2013            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface Operator<T> {
    /**
     * Evaluate whether the operator would return true when comparing operandOne
     * to operandTwo.
     * 
     * <pre>
     * <code>
     *   OperatorTypes operator = OperatorTypes.GREATER_THAN;
     *   long operandOne = 1;
     *   long operandTwo = 2;
     *   
     *   boolean result = operator.evaluate(operandOne, operandTwo); // Returns false
     * </code>
     * </pre>
     * 
     * @param operandOne
     * @param operandTwo
     * @return true or false
     */
    boolean evaluate(T operandOne, T operandTwo);

    /**
     * Get the name.
     * 
     * @return the name
     */
    String name();
}