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

package com.raytheon.uf.common.dataplugin.satellite.units.ir;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Converts a temperature value in Kelvin to a pixel value.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 29, 2007           njensen     Initial creation
 * Nov 20, 2013  2492     bsteffen    Make conversion unbounded.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class IRTempToPixelConverter extends UnitConverter {

    private static final long serialVersionUID = 1L;

    @Override
    public double convert(double aTemperature) throws ConversionException {
        double result = 0.0;

        if (aTemperature < 238.15) {
            result = 418.15 - aTemperature;
        } else {
            result = 656.3 - (2.0 * aTemperature);
        }

        return result;
    }

    @Override
    public boolean equals(Object aConverter) {
        return (aConverter instanceof IRTempToPixelConverter);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public UnitConverter inverse() {
        return new IRPixelToTempConverter();
    }

    @Override
    public boolean isLinear() {
        return false;
    }

}
