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

import com.raytheon.uf.common.dataplugin.annotations.DataURIFieldConverter;
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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 12, 2012           bsteffen    Initial creation
 * Nov 25, 2013  2574     bsteffen    Switch to DataURIFieldConverter
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridCoverageConverter implements DataURIFieldConverter, Converter {

    @Override
    public String toString(Object field) {
        if (field instanceof GridCoverage) {
            Integer id = ((GridCoverage) field).getId();
            if (id == null) {
                throw new UnsupportedOperationException(
                        "Coverage Information Not Specified yet");
            } else {
                return id.toString();
            }
        }
        return null;
    }

    @Override
    public GridCoverage fromString(String string) {
        try {
            if (string == null || "null".equals(string)) {
                return null;
            }
            return fromInteger(Integer.parseInt(string));
        } catch (NumberFormatException e) {
            throw new UnsupportedOperationException(string
                    + " is not a valid GridCoverage id.");
        }
    }

    @Override
    public GridCoverage convert(Class clazz, Object value) {
        if (value instanceof Integer) {
            return fromInteger((Integer) value);
        } else if (value instanceof String) {
            return fromString((String) value);
        } else {
            throw new ConversionException("Cannot convert "
                    + String.valueOf(value) + " of type "
                    + value.getClass().getSimpleName() + " to a GridCoverage.");
        }
    }

    public GridCoverage fromInteger(Integer integer) {
        if (integer != null) {
            GridCoverage result = GridCoverageLookup.getInstance().getCoverage(
                    integer);
            if (result != null) {
                return result;
            }
        }
        throw new UnsupportedOperationException(
                "Cannot find GridCoverage with id of "
                        + String.valueOf(integer));
    }

}
