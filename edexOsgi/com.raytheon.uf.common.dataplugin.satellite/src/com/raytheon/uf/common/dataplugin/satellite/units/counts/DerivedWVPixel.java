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
package com.raytheon.uf.common.dataplugin.satellite.units.counts;

import java.util.Map;

import javax.measure.Dimension;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Temperature;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import si.uom.SI;
import tec.uom.se.AbstractUnit;
import tec.uom.se.quantity.QuantityDimension;

public class DerivedWVPixel extends AbstractUnit<Temperature> {

    private static final long serialVersionUID = 1L;

    @Override
    public boolean equals(Object anObject) {
        return (anObject instanceof DerivedWVPixel);
    }

    @Override
    public Unit<Temperature> toSystemUnit() {
        return SI.KELVIN;
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public UnitConverter getSystemConverter() {
        return new DerivedWVPixelToTempConverter();
    }

    @Override
    public Map<? extends Unit<?>, Integer> getBaseUnits() {
        return this.getSystemUnit().getBaseUnits();
    }

    @Override
    public Dimension getDimension() {
        return QuantityDimension.TEMPERATURE;
    }

}
