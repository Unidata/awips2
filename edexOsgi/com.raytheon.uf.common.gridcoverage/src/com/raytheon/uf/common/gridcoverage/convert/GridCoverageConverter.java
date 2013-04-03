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
package com.raytheon.uf.common.gridcoverage.convert;

import org.apache.commons.beanutils.ConversionException;
import org.apache.commons.beanutils.Converter;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;

/**
 * Convert an integer or a string representing a grid coverage id to a grid
 * coverage using the GridCoverageLookup.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridCoverageConverter implements Converter {

    @Override
    @SuppressWarnings("rawtypes")
    public GridCoverage convert(Class clazz, Object value) {
        Integer intValue = null;
        if (value instanceof Integer) {
            intValue = (Integer) value;
        } else if (value instanceof String) {
            try {
                intValue = Integer.parseInt((String) value);
            } catch (NumberFormatException e) {
                ;// ignore and throw conversion exception later.
            }
        }
        if (intValue != null) {
            GridCoverage result = GridCoverageLookup.getInstance().getCoverage(
                    intValue);
            if (result != null) {
                return result;
            }
            throw new ConversionException(
                    "Cannot find GridCoverage with id of "
                            + String.valueOf(intValue));
        }
        throw new ConversionException("Cannot convert " + String.valueOf(value)
                + " of type " + value.getClass().getSimpleName()
                + " to a GridCoverage.");
    }

}
