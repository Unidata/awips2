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
package com.raytheon.viz.ui.parameter.converters;

import org.eclipse.core.commands.AbstractParameterValueConverter;
import org.eclipse.core.commands.ParameterValueConversionException;

/**
 * Converts parameter string to Integer
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class IntegerParameterValueConverter extends
        AbstractParameterValueConverter {

    /**
     * 
     */
    public IntegerParameterValueConverter() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractParameterValueConverter#convertToObject
     * (java.lang.String)
     */
    @Override
    public Object convertToObject(String parameterValue)
            throws ParameterValueConversionException {
        try {
            if (parameterValue == null) {
                return null;
            }
            return Integer.parseInt(parameterValue);
        } catch (NumberFormatException e) {
            throw new ParameterValueConversionException(
                    "Error converting String \"" + parameterValue
                            + "\" to Integer", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractParameterValueConverter#convertToString
     * (java.lang.Object)
     */
    @Override
    public String convertToString(Object parameterValue)
            throws ParameterValueConversionException {
        if (parameterValue instanceof Integer) {
            return ((Integer) (parameterValue)).toString();
        } else {
            throw new ParameterValueConversionException(Integer.class.getName()
                    + " expected, received "
                    + parameterValue.getClass().getName());
        }
    }

}
