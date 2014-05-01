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
package com.raytheon.uf.common.wxmath;

import static java.lang.Math.exp;
import static java.lang.Math.pow;

/**
 * Calculates Equivalent Potential Temperature.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2013 2262       bsteffen    Converted from meteolib thermoRtns.c
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class EquivalentPotentialTemperature {

    /*
     * EPS = RATIO OF THE MEAN MOLECULAR WEIGHT OF WATER (18.016 G/MOLE) TO THAT
     * OF DRY AIR (28.966 G/MOLE)
     */
    private static final double eps = 0.62197;

    /* ES0 = SATURATION VAPOR RESSURE OVER LIQUID WATER AT 0C */
    private static final double es0 = 6.1078;

    /**
     * THIS FUNCTION RETURNS THE EQUIVALENT POTENTIAL TEMP EPT (KELVIN) FOR A
     * PARCEL OF AIR INITIALLY AT TEMP T (KELVIN), DEW POINT TD (KELVIN) AND
     * PRESSURE P (MILLIBARS).
     */
    public static double ept(double t, double td, double p) {
        /*
         * BAKER,SCHLATTER 17-MAY-1982 Original version
         * 
         * THE FORMULA USED IS EQ.(43) IN BOLTON, DAVID, 1980: "THE COMPUTATION
         * OF EQUIVALENT POTENTIAL TEMPERATURE," MONTHLY WEATHER REVIEW, VOL.
         * 108, NO. 7 (JULY), PP. 1046-1053. THE MAXIMUM ERROR IN EPT IN 0.3C.
         * IN MOST CASES THE ERROR IS LESS THAN 0.1C.
         */

        /*
         * COMPUTE THE MIXING RATIO (GRAMS OF WATER VAPOR PER KILOGRAM OF DRY
         * AIR).
         */
        double tdc = td - 273.16;
        double w = wmr(p, tdc);

        /* COMPUTE THE TEMP (CELSIUS) AT THE LIFTING CONDENSATION LEVEL. */
        double tc = t - 273.16;
        double tlcl = tcon(tc, tdc);
        double tk = t; /* + 273.16; */
        double tl = tlcl + 273.16;
        double d1 = (1e3 / p);
        double d2 = ((1.0 - w * 2.8e-4) * 0.2854);
        double pt = tk * pow(d1, d2);
        double eptk = pt * exp((3.376 / tl - 0.00254) * w * (w * 8.1e-4 + 1.0));
        return eptk; /* - 273.16; */
    }

    /**
     * THIS FUNCTION APPROXIMATES THE MIXING RATIO WMR (GRAMS OF WATER VAPOR PER
     * KILOGRAM OF DRY AIR) GIVEN THE PRESSURE P (MB) AND THE TEMPERATURE T
     * (CELSIUS).
     */
    private static double wmr(double p, double t) {
        /*
         * BAKER,SCHLATTER 17-MAY-1982 Original version
         * 
         * THE FORMULA USED IS GIVEN ON P. 302 OF THE SMITHSONIAN METEOROLOGICAL
         * TABLES BY ROLAND LIST (6TH EDITION).
         */

        /*
         * THE NEXT TWO LINES CONTAIN A FORMULA BY HERMAN WOBUS FOR THE
         * CORRECTION FACTOR WFW FOR THE DEPARTURE OF THE MIXTURE OF AIR AND
         * WATER VAPOR FROM THE IDEAL GAS LAW. THE FORMULA FITS VALUES IN TABLE
         * 89, P. 340 OF THE SMITHSONIAN METEOROLOGICAL TABLES, BUT ONLY FOR
         * TEMPERATURES AND PRESSURES NORMALLY ENCOUNTERED IN IN THE ATMOSPHERE.
         */
        double x = (t - 12.5 + 7500.0 / p) * 0.02;
        double wfw = p * 4.5e-6 + 1.0 + x * 0.0014 * x;
        double fwesw = wfw * esw(t);
        double r = eps * fwesw / (p - fwesw);

        /* CONVERT R FROM A DIMENSIONLESS RATIO TO GRAMS/KILOGRAM. */
        return r * 1e3;
    }

    /**
     * THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE ESW (MILLIBARS) OVER
     * LIQUID WATER GIVEN THE TEMPERATURE T (CELSIUS).
     */
    private static double esw(double t) {
        /*
         * BAKER,SCHLATTER 17-MAY-1982 Original version
         * 
         * THE POLYNOMIAL APPROXIMATION BELOW IS DUE TO HERMAN WOBUS, A
         * MATHEMATICIAN WHO WORKED AT THE NAVY WEATHER RESEARCH FACILITY,
         * NORFOLK, VIRGINIA, BUT WHO IS NOW RETIRED. THE COEFFICIENTS OF THE
         * POLYNOMIAL WERE CHOSEN TO FIT THE VALUES IN TABLE 94 ON PP. 351-353
         * OF THE SMITHSONIAN METEOROLOGICAL TABLES BY ROLAND LIST (6TH ED). THE
         * APPROXIMATION IS VALID FOR -50 < T < 100C.
         */
        double pol = t * -3.0994571e-20 + 1.1112018e-17;
        pol = t * pol - 1.7892321e-15;
        pol = t * pol + 2.1874425e-13;
        pol = t * pol - 2.9883885e-11;
        pol = t * pol + 4.3884187e-9;
        pol = t * pol - 6.1117958e-7;
        pol = t * pol + 7.8736169e-5;
        pol = t * pol - 0.0090826951;
        pol = t * pol + 0.99999683;

        /* Computing 8th power */
        pol *= pol;
        pol *= pol;
        pol *= pol;
        return es0 / pol;
    }

    /**
     * THIS FUNCTION RETURNS THE TEMPERATURE TCON (CELSIUS) AT THE LIFTING
     * CONDENSATION LEVEL, GIVEN THE TEMPERATURE T (CELSIUS) AND THE DEW POINT D
     * (CELSIUS).
     */
    private static double tcon(double t, double d) {
        /* BAKER,SCHLATTER 17-MAY-1982 Original version */

        /* COMPUTE THE DEW POINT DEPRESSION S. */
        double s = t - d;

        /*
         * THE APPROXIMATION BELOW, A THIRD ORDER POLYNOMIAL IN S AND T, IS DUE
         * TO HERMAN WOBUS. THE SOURCE OF DATA FOR FITTING THE POLYNOMIAL IS
         * UNKNOWN.
         */
        double dlt = s
                * (t * 0.001278 + 1.2185 + s
                        * (s * 1.173e-5 - 0.00219 - t * 5.2e-6));
        return t - dlt;
    }
}
