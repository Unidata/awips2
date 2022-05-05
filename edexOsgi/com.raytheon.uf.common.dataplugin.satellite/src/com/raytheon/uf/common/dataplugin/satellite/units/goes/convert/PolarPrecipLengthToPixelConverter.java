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

import javax.measure.UnitConverter;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import si.uom.SI;
import tec.uom.se.AbstractConverter;
import tec.uom.se.unit.MetricPrefix;

/**
 * Converts meters of total precipitation of water to a pixel value
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2009            njsanchez    Initial creation
 * Apr 15, 2019   7596     lsingh       Updated units framework to JSR-363.
 *                                      Overrided additional methods
 * 
 * </pre>
 * 
 * @author jsanchez
 */
public class PolarPrecipLengthToPixelConverter extends AbstractConverter {

    private static final long serialVersionUID = 1L;

    private static UnitConverter meterToMillimeter = SI.METRE.getConverterTo(MetricPrefix
            .MILLI(SI.METRE));

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#convert(double)
     */
    @Override
    public double convert(double aLength) {

        // value is in meters, but below calculates pixel based on value being
        // millimeters
        aLength = meterToMillimeter.convert(aLength);

        double result = 0.0;

        if (aLength < 1) {
            result = 0.0;
        } else if (aLength <= 70) {
            result = aLength + 176;
        } else if (aLength <= 75) {
            result = (18900 - aLength) / 75;
        } else {
            result = 255;
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
        return (aConverter instanceof PolarPrecipLengthToPixelConverter);
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
        return new PolarPrecipPixelToLengthConverter();
    }

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#isLinear()
     */
    @Override
    public boolean isLinear() {
        return true;
    }

    @Override
    public BigDecimal convert(BigDecimal value, MathContext ctx)
            throws ArithmeticException {
        return BigDecimal.valueOf(convert(value.doubleValue()));
    }
}
