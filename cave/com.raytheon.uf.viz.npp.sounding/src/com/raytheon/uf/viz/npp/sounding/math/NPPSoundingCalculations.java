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
package com.raytheon.uf.viz.npp.sounding.math;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

/**
 * Common NPP Sounding calculations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NPPSoundingCalculations {

    public static final Unit<?> PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

    public static final Unit<?> HEIGHT_UNIT = SI.METER;

    public static final Unit<?> TEMPERATURE_UNIT = SI.CELSIUS;

    public static final Unit<?> TEMPERATURE_CALC_UNIT = SI.KELVIN;

    public static final Unit<?> H2O_UNIT = SI.GRAM.divide(SI.KILOGRAM);

    public static final Unit<?> DEWPOINT_UNIT = SI.CELSIUS;

    /**
     * convert h2o in g/kg and pressure in hPa to dewpoint in kelvin.
     * 
     * @param h2o
     * @param pressure
     * @return
     */
    public static float convertH2OtoDewpoint(float h2o, float pressure) {
        double eee = pressure * h2o / (622.0 + 0.378 * h2o);
        double b = 26.66082 - Math.log(eee);
        return (float) ((b - Math.sqrt(b * b - 223.1986)) / 0.0182758048);
    }

    /**
     * convert h2o in g/kg and pressure in hPa to relative humidity.
     * 
     * @param h20
     * @param temperature
     * @param pressure
     * @return
     */
    public static float convertH20ToRelativeHumidity(float h20,
            float temperature, float pressure) {
        double a = 22.05565;
        double b = 0.0091379024;
        double c = 6106.396;
        double epsilonx1k = 622.0;

        double shxDenom = h20 * 0.378;
        shxDenom += epsilonx1k;

        double tDenom = -b * temperature;
        tDenom += a;
        tDenom -= c / temperature;

        double RH = pressure * h20;
        RH /= shxDenom;
        RH /= Math.exp(tDenom);

        return (float) RH;
    }
}
