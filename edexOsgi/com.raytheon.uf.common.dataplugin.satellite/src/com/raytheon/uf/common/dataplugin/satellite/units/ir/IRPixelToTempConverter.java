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

import javax.measure.UnitConverter;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import tech.units.indriya.function.AbstractConverter;

/**
 * Converts a pixel value from 0-255 into a temperature in Kelvin
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2007            njensen     Initial creation
 * Apr 15, 2019   7596     lsingh      Updated units framework to JSR-363.
 *                                     Overrided additional methods
 * Aug 05, 2022   8905     lsingh      Updated units framework to 2.0.2.
 *                                     Renamed methods, and overrided additional methods.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class IRPixelToTempConverter extends AbstractConverter {

    private static final long serialVersionUID = 1L;

    @Override
    public Number convertWhenNotIdentity(Number pixel) {
        double result = 0.0;
        double aPixel = pixel.doubleValue();

        if (aPixel >= 180) {
            result = 418.15 - aPixel;
        } else {
            result = 328.15 - (aPixel / 2.0);
        }

        return result;
    }

    @Override
    public boolean equals(Object aConverter) {
        return (aConverter instanceof IRPixelToTempConverter);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }

    @Override
    public AbstractConverter inverseWhenNotIdentity() {
        return new IRTempToPixelConverter();
    }

    @Override
    public boolean isLinear() {
        return false;
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
