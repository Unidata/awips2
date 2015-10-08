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
package com.raytheon.uf.edex.netcdf.decoder.util;

import ucar.nc2.Attribute;
import ucar.nc2.Variable;

/**
 * Utility class for decoding netcdf files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2015 4698       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class NetcdfDecoderUtils {
    public static final String MISSING_VALUE = "missing_value";

    public static final String FILL_VALUE = "fill_value";

    public static final String[] NO_DATA_ATTRS = new String[] {
            "missing_value", "fill_value", "_FillValue" };

    public static final String ADD_OFFSET = "add_offset";

    public static final String SCALE_FACTOR = "scale_factor";

    public static final Integer DEFAULT_SCALE_FACTOR = Integer.valueOf(1);

    public static final Integer DEFAULT_ADD_OFFSET = Integer.valueOf(0);

    private NetcdfDecoderUtils() {
        super();
    }

    /**
     * Get's the no-data value for the variable. If a no-data attribute cannot
     * be found, the supplied default value is returned.
     * 
     * @param variable
     *            The variable to get the no-data value for.
     * @param defaultValue
     *            The value to return if a no-data attribute cannot be found.
     * @return The no-data value for the variable, or the supplied default value
     *         if it could not be determined.
     */
    public static Number getNoDataValue(Variable variable, Number defaultValue) {
        return getNumericAttributeValue(variable, defaultValue, NO_DATA_ATTRS);
    }

    /**
     * Gets the add offset for the variable.
     *
     * @param variable
     *            The variable.
     * @return The add offset for the variable, or {@link #DEFAULT_ADD_OFFSET}
     *         if not found.
     */
    public static Number getAddOffset(Variable variable) {
        return getNumericAttributeValue(variable, DEFAULT_ADD_OFFSET,
                ADD_OFFSET);
    }

    /**
     * Gets the scale factor for the variable.
     *
     * @param variable
     *            The variable.
     * @return The scale factor for the variable, or
     *         {@link #DEFAULT_SCALE_FACTOR} if not found.
     */
    public static Number getScaleFactor(Variable variable) {
        return getNumericAttributeValue(variable, DEFAULT_SCALE_FACTOR,
                SCALE_FACTOR);
    }

    /**
     * Gets a numeric attribute value from the variable. The supplied list of
     * names will be checked in order, returning the value for only the first
     * found attribute. If no attribute is found for any of the attribute names,
     * the default value is returned.
     *
     * @param variable
     *            The variable.
     * @param defaultVal
     *            The default value to return if the attribute cannot be found.
     * @param attributeNames
     *            The list of possible names for the attribute, case
     *            insensitive.
     * @return The numeric value for the attribute, or the supplied default
     *         value if the attribute could not be found.
     */
    public static Number getNumericAttributeValue(Variable variable,
            Number defaultVal, String... attributeNames) {
        Attribute attr;
        for (String attrName : attributeNames) {
            attr = variable.findAttributeIgnoreCase(attrName);
            if (attr != null) {
                return attr.getNumericValue();
            }
        }
        return defaultVal;
    }

}
