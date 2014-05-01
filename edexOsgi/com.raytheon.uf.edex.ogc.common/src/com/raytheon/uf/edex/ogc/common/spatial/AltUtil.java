/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import javax.measure.converter.ConversionException;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;

/**
 * Altitude utility methods and constants
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 4, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AltUtil {

    private static final double T0 = 288.0;

    private static final double gamma = 0.0065;

    private static final double p0 = 1013.2;

    private static final double c1 = 5.256;

    private static final double c2 = 14600.0;

    private static final double z11 = 11000.0;

    private static final double p11 = 226.0971;

    private static final double Flag = 1e37;

    private static final double Flg = 1.0E10;

    /**
     * Convert Millibars (hPa) to Meters
     * 
     * @param p
     * @return
     */
    public static double mbToMeters(double p) {
        double rval;
        if (p > Flg || p < 1.0) {
            rval=Flag;
        }else if (p > p11){
            rval = ((T0 - T0 * Math.pow(p / p0, 1 / c1)) / gamma);
        }else{
            rval = (c2 * Math.log10(p11 / p) + z11);
        }
        return rval;
    }

    /**
     * Convert Meters to Millibars (hPa)
     * 
     * @param m
     * @return
     */
    public static double metersToMb(double m) {
        double rval;
        if (m > Flg) {
            rval = Flag;
        } else if (m < z11) {
            rval = p0 * Math.pow(((T0 - gamma * m) / T0), c1);
        } else {
            rval = p11 * Math.pow(10.0, ((z11 - m) / c2));
        }
        return rval;
    }

    /**
     * Convert value from srcUnits to targetUnits
     * 
     * @param targetUnits
     * @param srcUnits
     * @param value
     * @return
     * @throws ConversionException
     */
    public static double convert(Unit<?> targetUnits, Unit<?> srcUnits,
            double value) throws ConversionException {
        if (targetUnits == null || srcUnits == null) {
            return value;
        }
        if (targetUnits.equals(srcUnits)) {
            return value;
        }
        if (targetUnits.isCompatible(srcUnits)) {
            return srcUnits.getConverterTo(targetUnits).convert(value);
        }
        if (srcUnits.isCompatible(SI.PASCAL)
                && targetUnits.isCompatible(SI.METER)) {
            double inPa = srcUnits.getConverterTo(SI.PASCAL).convert(value);
            double inM = mbToMeters(inPa / 100);
            return SI.METER.getConverterTo(targetUnits).convert(inM);
        } else if (srcUnits.isCompatible(SI.METER)
                && targetUnits.isCompatible(SI.PASCAL)) {
            double inM = srcUnits.getConverterTo(SI.METER).convert(value);
            double inPa = metersToMb(inM) * 100;
            return SI.PASCAL.getConverterTo(targetUnits).convert(inPa);
        } else {
            throw new ConversionException("Unable to find converter between "
                    + srcUnits
                    + " and " + targetUnits);
        }
    }

    /**
     * Converts a vertical coordinate to the target units and reference point
     * 
     * @param targetUnits
     * @param targetRef
     * @param vert
     * @return
     * @throws ConversionException
     */
    public static VerticalCoordinate convert(Unit<?> targetUnits,
            Reference targetRef, VerticalCoordinate vert)
            throws ConversionException {
        if (targetUnits == null) {
            return vert;
        }
        // FIXME handle reference
        Unit<?> srcUnits = vert.getUnits();
        if (targetUnits.equals(srcUnits)) {
            return vert;
        }
        if (!vert.isRange()) {
            return new VerticalCoordinate(convert(targetUnits, srcUnits,
                    vert.getValue()), targetUnits, targetRef);
        } else {
            return new VerticalCoordinate(convert(targetUnits, srcUnits,
                    vert.getMin()), convert(targetUnits, srcUnits,
                    vert.getMax()), targetUnits, targetRef);
        }
    }

}
