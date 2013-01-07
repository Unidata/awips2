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
 * Operator type enumeration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012   730       jpiatt     Initial creation.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */

/** Enumeration to use for Dataset Size operations */
public enum OperatorTypes {
    /** Greater than operation */
    GREATER_THAN(">") {
        @Override
        public boolean evaluate(long operandOne, long operandTwo) {
            return operandOne > operandTwo;
        }
    },
    /** Less than operation */
    LESS_THAN("<") {
        @Override
        public boolean evaluate(long operandOne, long operandTwo) {
            return operandOne < operandTwo;
        }
    },
    /** Greater than or equal operation */
    GREATER_THAN_EQUAL(">=") {
        @Override
        public boolean evaluate(long operandOne, long operandTwo) {
            return operandOne >= operandTwo;
        }
    },
    /** Less than or equal operation */
    LESS_THAN_EQUAL("<=") {
        @Override
        public boolean evaluate(long operandOne, long operandTwo) {
            return operandOne <= operandTwo;
        }
    },
    /** Greater than operation */
    EQUAL("Equal") {
        @Override
        public boolean evaluate(long operandOne, long operandTwo) {
            return operandOne == operandTwo;
        }
    },
    /** Greater than operation */
    NOT_EQUAL("Not Equal") {
        @Override
        public boolean evaluate(long operandOne, long operandTwo) {
            return operandOne != operandTwo;
        }
    };

    /** Datatype operation */
    private final String operator;

    private OperatorTypes(String sizeOperation) {
        this.operator = sizeOperation;
    }

    /**
     * Get operator.
     * 
     * @return operator
     */
    public String getOperation() {
        return operator;
    }

    @Override
    public String toString() {
        return operator;
    }
    
    /**
     * Evaluate whether the operator would return true when comparing operandOne to operandTwo.
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
    public abstract boolean evaluate(long operandOne, long operandTwo);

    /**
     * Return the type from its toString() value.
     * 
     * @param string
     *  the string
     * @return
     *  the type
     */
    public static OperatorTypes fromString(String string) {
        for (OperatorTypes operator : OperatorTypes.values()) {
            if (operator.toString().equals(string)) {
                return operator;
            }
        }
        return null;
    }       

}
