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
 * Converts a pixel value of cloud amount to a cloud amount in percent
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2007            njensen     Initial creation
 * Mar 24, 2009     2086   jsanchdz    Updated convert to return a percent.
 * Apr 15, 2019    7596    lsingh      Updated units framework to JSR-363.
 *                                     Overrided additional methods
 * 
 * </pre>
 * 
 * @author njensen
 */
public class CloudPixelToPercentConverter extends AbstractConverter {

    private static final long serialVersionUID = 1L;

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#convert(double)
     */
    @Override
    public double convert(double aPixel) {
        double result = 0.0;

        if (aPixel <= 75) {
            result = 0.0;
        } else if (aPixel <= 76) {
            result = aPixel - 75.0;
        } else if (aPixel <= 166) {
            result = (49.0 / 90.0 * (aPixel - 76.0)) + 1.0;
        } else if (aPixel <= 167) {
            result = aPixel - 116.0;
        } else if (aPixel <= 253) {
            result = (24.0 / 43.0 * (aPixel - 167.0)) + 51.0;
        } else if (aPixel <= 254) {
            result = aPixel - 154.0;
        } else {
            result = 100.0;
        }

        return result / 100;
    }

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object aConverter) {
        return (aConverter instanceof CloudPixelToPercentConverter);
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
        return new CloudPercentToPixelConverter();
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
