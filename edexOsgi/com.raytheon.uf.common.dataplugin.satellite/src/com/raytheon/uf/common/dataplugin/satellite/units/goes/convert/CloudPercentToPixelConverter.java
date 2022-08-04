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

package com.raytheon.uf.common.dataplugin.satellite.units.goes.convert;

import java.math.BigDecimal;
import java.math.MathContext;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import tec.uom.se.AbstractConverter;

/**
 * Converts a cloud amount percent to a pixel value
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2007            njensen     Initial creation
 * Mar 23, 2010     2086   jsanchez    Multiplied percent by 100.
 * Apr 15, 2019     7596   lsingh      Updated units framework to JSR-363.
 *                                     Overrided additional methods
 * 
 * </pre>
 * 
 * @author njensen
 */
public class CloudPercentToPixelConverter extends AbstractConverter {

    private static final long serialVersionUID = 1L;

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#convert(double)
     */
    @Override
    public double convert(double aPercent) {
        double result = 0.0;

        // Need to fix. aPercent gets divided by 100 prior to being
        // passed due to the RationalConverter
        aPercent = aPercent * 100;

        if (aPercent < 0) {
            result = 0;
        } else if (aPercent <= 1) {
            result = aPercent + 75.0;
        } else if (aPercent <= 50) {
            result = (90.0 / 49.0 * (aPercent - 1.0)) + 76.0;
        } else if (aPercent <= 51) {
            result = aPercent + 116.0;
        } else if (aPercent <= 99) {
            result = (43.0 / 24.0 * (aPercent - 51.0)) + 167.0;
        } else if (aPercent <= 100) {
            result = aPercent + 154.0;
        } else {
            result = 255.0;
        }

        if (result < 0) {
            result = 0;
        } else if (result > 255) {
            result = 255;
        }

        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object aConverter) {
        return (aConverter instanceof CloudPercentToPixelConverter);
    }

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#hashCode()
     */
    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#inverse()
     */
    @Override
    public AbstractConverter inverse() {
        return new CloudPixelToPercentConverter();
    }

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#isLinear()
     */
    @Override
    public boolean isLinear() {
        return false;
    }

    @Override
    public BigDecimal convert(BigDecimal value, MathContext ctx)
            throws ArithmeticException {
        return BigDecimal.valueOf(convert(value.doubleValue()));
    }

}
