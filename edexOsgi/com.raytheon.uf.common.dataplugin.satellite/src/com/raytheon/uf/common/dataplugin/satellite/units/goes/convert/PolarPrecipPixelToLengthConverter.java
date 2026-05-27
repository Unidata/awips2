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

import javax.measure.MetricPrefix;
import javax.measure.UnitConverter;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import si.uom.SI;
import tech.units.indriya.function.AbstractConverter;

/**
 * Converts a pixel representation of total precipitation of water to meters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2009     2086       jsanchez     Initial creation
 * Apr 15, 2019     7596       lsingh       Updated units framework to JSR-363.
 *                                          Overrided additional methods
 * Aug 05, 2022     8905        lsingh      Updated units framework to 2.0.2.
 *                                          Renamed methods, and overrided additional methods.
 * 
 * </pre>
 * 
 * @author jsanchez
 */
public class PolarPrecipPixelToLengthConverter extends AbstractConverter {

    private static final long serialVersionUID = 1L;

    private static UnitConverter millimeterToMeter = (MetricPrefix.MILLI(SI.METRE))
            .getConverterTo(SI.METRE);

    @Override
    public Number convertWhenNotIdentity(Number pixel) {

        double result = 0.0;
        double aPixel = pixel.doubleValue();

        if (aPixel < 1) {
            result = 0.0;
        } else if (aPixel <= 251) {
            result = aPixel - 176;
        } else if (aPixel <= 252) {
            result = -75 * aPixel + 18900;
        } else {
            result = 0.0;
        }
        // above converts pixel to millimeters, but we need to change it to
        // meters
        result = millimeterToMeter.convert(result);

        return result;
    }

    @Override
    public boolean equals(Object aConverter) {
        return (aConverter instanceof PolarPrecipPixelToLengthConverter);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public AbstractConverter inverseWhenNotIdentity() {
        return new PolarPrecipLengthToPixelConverter();
    }

    @Override
    public boolean isLinear() {
        return true;
    }

    @Override
    public boolean isIdentity() {
        return false;
    }

    @Override
    public int compareTo(UnitConverter o) {
        // This method hasn't been implemented yet since it's unused
        return 0;
    }

    @Override
    protected String transformationLiteral() {
        // This method hasn't been implemented yet since it's unused
        return null;
    }

    @Override
    protected boolean canReduceWith(AbstractConverter that) {
        // This method hasn't been implemented yet since it's unused
        return false;
    }

}
