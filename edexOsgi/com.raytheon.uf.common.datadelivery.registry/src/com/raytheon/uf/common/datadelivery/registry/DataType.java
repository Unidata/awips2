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
package com.raytheon.uf.common.datadelivery.registry;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * Enumeration of data types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743        djohnson     Initial creation
 * Nov 19, 2012 1166        djohnson     Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlEnum
public enum DataType {
    @XmlEnumValue(DataType.GRID_STRING_VALUE)
    GRID(DataType.GRID_STRING_VALUE);

    private static final String GRID_STRING_VALUE = "Grid";
    
    private final String displayString;

    private DataType(String displayString) {
        this.displayString = displayString;
    }

    @Override
    public String toString() {
        return displayString;
    }
    
    /**
     * Retrieve a {@link DataType} by its text-representation ignoring
     * case-differences.
     * 
     * @param value
     * @return
     */
    public static DataType valueOfIgnoreCase(String value) {
        for (DataType dataSetType : DataType.values()) {
            if (dataSetType.toString().equalsIgnoreCase(value)) {
                return dataSetType;
            }
        }

        throw new IllegalArgumentException(value + " is not a valid DataType!");
    }
}
