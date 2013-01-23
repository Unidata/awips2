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
package com.raytheon.uf.viz.datadelivery.utils;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.datadelivery.system.Operator;

/**
 * Data Type Operation Enum.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013    1420     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlType(name = "typeOperationItems")
@XmlEnum
public enum TypeOperationItems implements Operator<String> {
    /** Operation IN */
    @XmlEnumValue("IN")
    IN("IN"),
    /** Operation NOT IN */
    @XmlEnumValue("NOT IN")
    NOT_IN("NOT IN");

    /** Datatype operation */
    private final String typeOperation;

    private TypeOperationItems(String typeOperation) {
        this.typeOperation = typeOperation;
    }

    /**
     * Get datatype operation.
     * 
     * @return typeOperation
     */
    public String getOperation() {
        return typeOperation;
    }

    @Override
    public String toString() {
        return typeOperation;
    }

    @Override
    public boolean evaluate(String operandOne, String operandTwo) {
        if (TypeOperationItems.IN == this) {
            if (operandOne.toLowerCase().contains(operandTwo.toLowerCase())) {
                return true;
            }
        } else if (TypeOperationItems.NOT_IN == this) {
            if (!operandOne.contains(operandTwo)) {
                return true;
            }
        }

        return false;
    }
}