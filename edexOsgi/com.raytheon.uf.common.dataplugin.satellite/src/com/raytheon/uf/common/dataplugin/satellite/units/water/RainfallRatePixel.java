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

import java.util.Map;

import javax.measure.Dimension;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Speed;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import si.uom.SI;
import tec.uom.se.AbstractUnit;
import tec.uom.se.quantity.QuantityDimension;
import tec.uom.se.unit.MetricPrefix;
import tec.uom.se.unit.Units;

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
 * Apr 15, 2019      7596  lsingh      Updated units framework to JSR-363.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class RainfallRatePixel extends AbstractUnit<Speed> {

    private static final long serialVersionUID = 1L;

    @Override
    public boolean equals(Object anObject) {
        return (anObject instanceof RainfallRatePixel);
    }

    @Override
    public Unit<Speed> toSystemUnit() {
        return MetricPrefix.MILLI(SI.METRE).divide(Units.HOUR).asType(Speed.class);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public UnitConverter getSystemConverter() {
        return new RainfallPixelToRateConverter();
    }

    @Override
    public Map<? extends Unit<?>, Integer> getBaseUnits() {
        return null;
    }

    @Override
    public Dimension getDimension() {
        return QuantityDimension.of(Speed.class);
    }
}
