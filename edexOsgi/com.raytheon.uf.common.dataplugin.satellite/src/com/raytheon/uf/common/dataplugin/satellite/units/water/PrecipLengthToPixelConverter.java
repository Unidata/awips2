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

import java.math.BigDecimal;
import java.math.MathContext;

import javax.measure.UnitConverter;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import si.uom.SI;
import tec.uom.se.AbstractConverter;
import tec.uom.se.unit.MetricPrefix;

/**
 * TODO class description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2007                 njensen     Initial creation
 * Apr 15, 2019   7596          lsingh      Updated units framework to JSR-363.
 *                                          Overrided additional methods
 * 
 * </pre>
 * 
 * @author njensen
 */
public class PrecipLengthToPixelConverter extends AbstractConverter {

    private static final long serialVersionUID = 1L;

    private static UnitConverter meterToMillimeter = SI.METRE.getConverterTo(MetricPrefix
            .MILLI(SI.METRE));

    /*
     * (non-Javadoc)
     * 
     * @see tec.uom.se.AbstractConverter#convert(double)
     */
    @Override
    public double convert(double aPrecip) {
        double result = 0.0;

        // value is in meters, but below calculates pixel based on value being
        // millimeters
        aPrecip = meterToMillimeter.convert(aPrecip);

        if (aPrecip > 0 && aPrecip <= 75) {
            result = aPrecip + 176.0;
        }

        if (result < 0) {
            result = 0.0;
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
        return (aConverter instanceof PrecipLengthToPixelConverter);
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
        return new PrecipPixelToLengthConverter();
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
