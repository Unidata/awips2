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

package com.raytheon.uf.common.dataplugin.satellite.units.water;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Velocity;
import javax.measure.unit.DerivedUnit;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * Represents a pixel value of a rainfall rate (mm/hr) on a POES or DMSP
 * satellite image
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2007            njensen     Initial creation
 * Mar 23, 2009      2086  jsanchez    Changed unit from mm^3/hr to mm/hr
 * 
 * </pre>
 * 
 * @author njensen
 */
public class RainfallRatePixel extends DerivedUnit<Velocity> {

    private static final long serialVersionUID = 1L;

    @Override
    public boolean equals(Object anObject) {
        return (anObject instanceof RainfallRatePixel);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Unit<Velocity> getStandardUnit() {
        return (Unit<Velocity>) (SI.MILLI(SI.METRE)).divide(NonSI.HOUR);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public UnitConverter toStandardUnit() {
        return new RainfallPixelToRateConverter();
    }
}
