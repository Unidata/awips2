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
 * Name Operation Enum.
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

/** Enumeration to use for Dataset Name operations */
@XmlType(name = "nameOperationItems")
@XmlEnum
public enum NameOperationItems implements Operator<String> {
    /** Operation Like */
    @XmlEnumValue("Like")
    LIKE("Like");

    /** Dataset Name operation */
    private final String operation;

    private NameOperationItems(String operation) {
        this.operation = operation;
    }

    /**
     * Get dataset name operation.
     * 
     * @return operation
     */
    public String getOperation() {
        return operation;
    }

    @Override
    public String toString() {
        return operation;
    }

    @Override
    public boolean evaluate(String operandOne, String operandTwo) {
        if (operandOne.toLowerCase().contains(operandTwo.toLowerCase())) {
            return true;
        }
        return false;
    }
}