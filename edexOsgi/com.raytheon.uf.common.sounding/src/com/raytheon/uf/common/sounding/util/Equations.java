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
package com.raytheon.uf.common.sounding.util;

import static java.lang.Math.abs;
import static java.lang.Math.exp;
import static java.lang.Math.log;
import static java.lang.Math.log10;
import static java.lang.Math.pow;
import static java.lang.Math.signum;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

/**
 * 
 * Common utility functions for UA Soundings
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2013 1638       mschenke    Moved from edex.common util package
 *
 * </pre>
 *
 * @author unknown
 * @version 1.0
 */
public class Equations {
    public interface VaporPressure {
        double es(double temperature);

        double inv_es(double vaporPressure);
    }

    // Latent heat of vaporization
    private static final double Lv = 636.4544664d;

    // specific heat of dry air at constant pressure
    private static final double cp = 0.24d;

    private static final double B = Lv / cp;

    private static final double Rd = 0.2870586d;

    private static final double EsAT_ZERO = 6.1121d;

    private static final double KEL_CEL = 273.15;

    // molar weight of moist air
    private static final double Mmoist = 18.016d;

    // molar weight of dry air
    private static final double Mdry = 28.966d;

    private static final double EPS = Mmoist / Mdry;

    private static final VaporPressure BOLTON = new VaporPressure() {
        public double es(double temperature) {
            temperature -= KEL_CEL;

            double n = (17.67 * temperature) / (temperature + 243.5);
            double esat = EsAT_ZERO * exp(n);
            return esat;
        }

        public double inv_es(double vaporPressure) {
            double n = log(vaporPressure / EsAT_ZERO);

            n = (243.5 * n) / (17.67 - n);

            return n + KEL_CEL;
        }
    };

    private static final VaporPressure TETENS = new VaporPressure() {
        public double es(double temperature) {
            temperature -= KEL_CEL;

            double n = (7.5 * temperature) / (temperature + 237.3);
            double esat = EsAT_ZERO * pow(10, n);
            return esat;
        }

        public double inv_es(double vaporPressure) {
            double n = log10(vaporPressure / EsAT_ZERO);

            n = (237.3 * n) / (7.5 - n);

            return n + KEL_CEL;
        }
    };

    public static VaporPressure vpCalc = null;

    private static final HashMap<String, VaporPressure> vpAlgorithms = new HashMap<String, VaporPressure>();
    static {
        vpAlgorithms.put("BOLTON", BOLTON);
        vpAlgorithms.put("TETENS", TETENS);
        // Set BUCK as the default
        vpCalc = TETENS;
    }

    public static double poisson(double startPressure, double stopPressure,
            double temperature) {
        return temperature * pow((startPressure / stopPressure), Rd);
    }

    /**
     * 
     * @param pressure
     * @param temperature
     * @return
     */
    public static double mixingRatio(double pressure, double temperature) {
        double es = vpCalc.es(temperature);
        return EPS * (es / (pressure - es));
    }

    public static double invMixingRatio(double pressure, double mixingRatio) {
        double es = (mixingRatio / EPS);

        es = (es * pressure) / (1 + es);

        return vpCalc.inv_es(es);
    }

    /**
     * 
     * @param pressure
     *            Pressure in hPa
     * @param temperature
     *            Temperature in degrees Kelvin.
     * @return Equivalent Potential Temperature in degrees Kelvin.
     */
    public static double ept(double pressure, double temperature) {
        double ept = poisson(1000.0d, pressure, temperature);

        return ept * exp(B * mixingRatio(pressure, temperature) / temperature);
    }

    private static double findTemperature(double pressure, double adiabat) {
        double tempLo = adiabat;
        double tempHi = adiabat;

        if (pressure < 1000.0) {
            tempLo = poisson(pressure, 1000.0, adiabat);
        }

        double eptAtLevel = ept(1000.0, adiabat);

        int count = 0;
        double eptAtGuess = 0;
        double guess = 0;
        while (abs(eptAtLevel - eptAtGuess) > 0.05) {
            guess = (tempHi + tempLo) / 2;
            eptAtGuess = ept(pressure, guess);

            if (eptAtGuess > eptAtLevel) {
                tempHi = guess;
            }
            if (eptAtGuess < eptAtLevel) {
                tempLo = guess;
            }
            if (count++ > 20)
                break;
        }
        return guess;
    }

    /**
     * 
     * @param startPressure
     * @param stopPressure
     * @param increment
     * @param adiabat
     * @return
     */
    public static List<UAPoint> saturatedAdiabats(double startPressure,
            double stopPressure, int increment, double adiabat) {
        ArrayList<UAPoint> adiabats = null;
        if (startPressure != stopPressure) {
            if (increment > 0) {
                adiabats = new ArrayList<UAPoint>();

                double delta = signum(stopPressure - startPressure) * increment;

                // int i = 0;
                for (; startPressure >= stopPressure; startPressure += delta) {
                    UAPoint point = new UAPoint();
                    point.pressure = startPressure;
                    point.temperature = findTemperature(startPressure, adiabat);
                    adiabats.add(point);
                }
            }
        }
        return adiabats;
    }

    public static List<List<UAPoint>> getSaturatedAdiabats(
            double startPressure, double stopPressure, double increment,
            double startTemp, double endTemp, double tempOffset) {
        List<List<UAPoint>> saturatedAdiabats = new ArrayList<List<UAPoint>>();

        for (double t = startTemp; t < 60; t += tempOffset) {
            saturatedAdiabats.add(Equations.saturatedAdiabats(1000, 100, 20,
                    t + 273.15));
        }
        return saturatedAdiabats;
    }

    public static List<List<UAPoint>> getDryAdiabats(double startPressure,
            double stopPressure, double increment, double startTemp,
            double endTemp, double tempOffset) {
        List<List<UAPoint>> dryAdiabats = new ArrayList<List<UAPoint>>();

        for (double t = startTemp; t < 60; t += tempOffset) {
            dryAdiabats.add(Equations.dryAdiabats(1000, 100, 20, t + 273.15));
        }
        return dryAdiabats;
    }

    /**
     * 
     * @param startPressure
     * @param stopPressure
     * @param increment
     * @param adiabat
     * @return
     */
    public static List<UAPoint> dryAdiabats(double startPressure,
            double stopPressure, int increment, double adiabat) {
        ArrayList<UAPoint> adiabats = null;
        if (startPressure != stopPressure) {
            if (increment > 0) {
                adiabats = new ArrayList<UAPoint>();

                double delta = signum(stopPressure - startPressure) * increment;

                double basePressure = startPressure;
                // int i = 0;
                for (; startPressure >= stopPressure; startPressure += delta) {
                    UAPoint point = new UAPoint();
                    point.pressure = startPressure;
                    point.temperature = poisson(startPressure, basePressure,
                            adiabat);
                    adiabats.add(point);
                }
            }
        }
        return adiabats;
    }

    /**
     * 
     * @return
     */
    public static Iterator<String> getVaporPressureAlgorithms() {
        return vpAlgorithms.keySet().iterator();
    }

    public static boolean setVaporPressureAlgorithms(String algName) {
        boolean algSet = false;
        if (vpAlgorithms.containsKey(algName)) {
            vpCalc = vpAlgorithms.get(algName);
            algSet = true;
        }
        return algSet;
    }

}
