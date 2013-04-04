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

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

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
@XmlType(name = "operatorType")
@XmlEnum
public enum OperatorTypes implements Operator<Long> {
    /** Greater than operation */
    @XmlEnumValue(">")
    GREATER_THAN(">") {
        @Override
        public boolean evaluate(Long operandOne, Long operandTwo) {
            return operandOne > operandTwo;
        }
    },
    /** Less than operation */
    @XmlEnumValue("<")
    LESS_THAN("<") {
        @Override
        public boolean evaluate(Long operandOne, Long operandTwo) {
            return operandOne < operandTwo;
        }
    },
    /** Greater than or equal operation */
    @XmlEnumValue(">=")
    GREATER_THAN_EQUAL(">=") {
        @Override
        public boolean evaluate(Long operandOne, Long operandTwo) {
            return operandOne >= operandTwo;
        }
    },
    /** Less than or equal operation */
    @XmlEnumValue("<=")
    LESS_THAN_EQUAL("<=") {
        @Override
        public boolean evaluate(Long operandOne, Long operandTwo) {
            return operandOne <= operandTwo;
        }
    },
    /** Greater than operation */
    @XmlEnumValue("Equal")
    EQUAL("Equal") {
        @Override
        public boolean evaluate(Long operandOne, Long operandTwo) {
            return operandOne.longValue() == operandTwo.longValue();
        }
    },
    /** Greater than operation */
    @XmlEnumValue("Not Equal")
    NOT_EQUAL("Not Equal") {
        @Override
        public boolean evaluate(Long operandOne, Long operandTwo) {
            return operandOne.longValue() != operandTwo.longValue();
        }
    };

    /** Datatype operation */
    private String operator;

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

    // public abstract boolean evaluate(Long operandOne, Long operandTwo);

    /**
     * Return the type from its toString() value.
     * 
     * @param string
     *            the string
     * @return the type
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
