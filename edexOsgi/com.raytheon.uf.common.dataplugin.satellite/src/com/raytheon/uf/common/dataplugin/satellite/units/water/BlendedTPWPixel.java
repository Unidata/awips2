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
import javax.measure.quantity.Length;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import si.uom.SI;
import tec.uom.se.AbstractUnit;
import tec.uom.se.function.AddConverter;
import tec.uom.se.quantity.QuantityDimension;
import tec.uom.se.unit.MetricPrefix;

/**
 * Represents a pixel value on a satellite blended total precipitable water(TPW)
 * image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 18, 2010           jsanchez    Initial creation
 * Nov 20, 2013  2492     bsteffen    Make conversion unbounded.
 * Apr 15, 2019  7596     lsingh      Updated units framework to JSR-363.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class BlendedTPWPixel extends AbstractUnit<Length> {

    private static final long serialVersionUID = 1L;

    @Override
    public boolean equals(Object anObject) {
        return (anObject instanceof PrecipPixel);
    }

    @Override
    public Unit<Length> toSystemUnit() {
        return SI.METRE;
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public UnitConverter getSystemConverter() {
        return MetricPrefix.MILLI(SI.METRE).getConverterTo(SI.METRE)
                .concatenate(new AddConverter(-176));
    }

    @Override
    public Map<? extends Unit<?>, Integer> getBaseUnits() {
        return null;
    }

    @Override
    public Dimension getDimension() {
        return QuantityDimension.LENGTH;
    }
}
