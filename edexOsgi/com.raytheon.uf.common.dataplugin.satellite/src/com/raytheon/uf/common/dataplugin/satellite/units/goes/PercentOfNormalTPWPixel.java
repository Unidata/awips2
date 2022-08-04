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
package com.raytheon.uf.common.dataplugin.satellite.units.goes;

import java.util.Map;

import javax.measure.Dimension;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Dimensionless;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.satellite.units.goes.convert.PixelToPercentConverter;

import tec.uom.se.AbstractUnit;
import tec.uom.se.quantity.QuantityDimension;

/**
 * A unit for the percent of normal total precipitable water pixels.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 12, 2010           jsanchez    Initial creation
 * Apr 15, 2014  2947     bsteffen    Fix equals.
 * Apr 15, 2019  7596     lsingh      Updated units framework to JSR-363.
 *                                    Overrided additional methods.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class PercentOfNormalTPWPixel extends AbstractUnit<Dimensionless> {
    private static final long serialVersionUID = 1L;

    @Override
    public boolean equals(Object anObject) {
        return (anObject instanceof PercentOfNormalTPWPixel);
    }

    @Override
    public Unit<Dimensionless> toSystemUnit() {
        return AbstractUnit.ONE;
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public UnitConverter getSystemConverter() {
        return new PixelToPercentConverter();
    }

    @Override
    public Map<? extends Unit<?>, Integer> getBaseUnits() {
        return null;
    }

    @Override
    public Dimension getDimension() {
        return QuantityDimension.NONE;
    }
}
