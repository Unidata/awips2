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

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import org.apache.commons.lang3.builder.HashCodeBuilder;

public class DerivedWVPixelToTempConverter extends UnitConverter {

    private static final long serialVersionUID = 1L;

    private static UnitConverter celsiusToKelvin = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    /*
     * (non-Javadoc)
     * 
     * @see javax.measure.converter.UnitConverter#convert(double)
     */
    @Override
    public double convert(double aPixel) throws ConversionException {
        // aPixel should be an unsigned byte(0-255)
        // If it is negative, assume we accidently got a signed byte and unsign
        // it
        if (aPixel < 0) {
            aPixel = 256 + aPixel;
        }
        double result = 145 - aPixel;
        result = celsiusToKelvin.convert(result);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.measure.converter.UnitConverter#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object aConverter) {
        return (aConverter instanceof DerivedWVPixelToTempConverter);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.measure.converter.UnitConverter#hashCode()
     */
    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.measure.converter.UnitConverter#inverse()
     */
    @Override
    public UnitConverter inverse() {
        return new DerivedTempToWVPixelConverter();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.measure.converter.UnitConverter#isLinear()
     */
    @Override
    public boolean isLinear() {
        return false;
    }

}
