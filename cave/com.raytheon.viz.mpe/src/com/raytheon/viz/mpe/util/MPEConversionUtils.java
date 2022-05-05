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
package com.raytheon.viz.mpe.util;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import com.raytheon.uf.common.units.UnitConv;

import tec.uom.se.AbstractConverter;

/**
 * Common conversion utilities for MPE.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2017 6407       bkowal      Initial creation
 * Apr 15, 2019 7596       lsingh      Upgraded Units Framework to JSR-363. Handled
 *                                     unit conversion.
 *
 * </pre>
 *
 * @author bkowal
 */

public final class MPEConversionUtils {
    

    private MPEConversionUtils() {
    }

    /**
     * Creates a {@link UnitConverter} converting the from unit to the to unit.
     * 
     * @param from
     *            the specified {@link Unit} to convert from
     * @param to
     *            the specified {@link Unit} to convert to
     * @return The unit converter or null if units are not compatible
     */
    public static UnitConverter constructConverter(Unit<?> from, Unit<?> to) {
        if (from == null || to == null) {
            return AbstractConverter.IDENTITY;
        }
        return UnitConv.getConverterToUnchecked(from, to);
    }
}