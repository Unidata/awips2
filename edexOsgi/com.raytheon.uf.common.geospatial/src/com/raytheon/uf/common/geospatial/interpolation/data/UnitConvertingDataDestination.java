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
package com.raytheon.uf.common.geospatial.interpolation.data;

import javax.measure.converter.UnitConverter;

/**
 * A data destination that converts to a unit with the specified unit converter;
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class UnitConvertingDataDestination implements DataDestination {

    protected UnitConverter unitConverter;

    protected DataDestination wrappedDestination;

    /**
     * Constructor
     * 
     * @param converter
     *            the unit converter to apply when setting the values in the
     *            destination
     * @param destination
     *            the destination to set values on
     */
    public UnitConvertingDataDestination(UnitConverter converter,
            DataDestination destination) {
        this.unitConverter = converter;
        this.wrappedDestination = destination;
    }

    @Override
    public void setDataValue(double dataValue, int x, int y) {
        wrappedDestination.setDataValue(unitConverter.convert(dataValue), x, y);
    }

    public DataDestination getWrappedDestination() {
        return wrappedDestination;
    }

}
