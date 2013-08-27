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
package com.raytheon.uf.common.sounding;

import com.raytheon.uf.common.sounding.util.SoundingPrefs;
import com.raytheon.uf.common.wxmath.EquivalentPotentialTemperature;
import com.raytheon.uf.common.wxmath.TempOfTe;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Weather related equations for calculating various parameters. Most of these
 * equations were ported from the NSHARP code.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Nov 2006             jkorman     Initial Coding
 * 29 Sept 2008            dhladky     Added more stuff to finish SkewT.
 * 25 Jul 2013  2190       mschenke    Moved common sounding calculation  from
 *                                     PopupSkewTDialog to here
 * Aug 26, 2013 2262       bsteffen    Port ept to java.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class WxMath {
    private static double ROCP = 0.28571428;

    static final double gamma = 0.0065;

    static final double dryGasCompositeConstant1 = 1.73;

    static final double dryGasCompositeConstant2 = -0.000157;

    static final double stdAtmPressureTrop = 226.5;

    static final double TO = 288.0;

    static final double stdAtmPLimit = 232.0;

    static final double stdAtmHLimit = 10500.0;

    static final double p0 = 1013.25;

    static final double c1 = 5.256;

    static final double c2 = 14600;

    static final double z11 = 11000;

    static final double p11 = 226.0971;

    // dry gas constant
    static final double Rd = 287.0;

    // molar weight of moist air
    private static final double Mmoist = 18.016d;

    // molar weight of dry air
    private static final double Mdry = 28.966d;

    private static final double EPS = Mmoist / Mdry;

    private static float[] muParcelTrajectoryPressures = new float[20];

    private static double[][][] DRY_ADIABATS;

    static {
        DRY_ADIABATS = new double[9][][];
        int j = 0;
        for (int i = -30; i <= 50; i += 10) {
            DRY_ADIABATS[j++] = createDryAdiabatIsoLine(i, 1000, 300, 10);
        }

        int i = 0;
        for (float p = 1000; p >= 50; p -= 50, ++i) {
            muParcelTrajectoryPressures[i] = p;
        }
    }

    /**
     * Create the isolines for various dry adiabats.
     * 
     * @param temperature
     *            Dry adiabatic temperature (Kelvin) requested.
     * @param startPressure
     *            Starting pressure in hectoPascals (millibars).
     * @param stopPressure
     *            Ending pressure in hectoPascals (millibars).
     * @param increment
     *            Pressure step in hectoPascals (millibars).
     * @return Calculated temperature and pressure values for the isoline.
     */
    public static double[][] createDryAdiabatIsoLine(double temperature,
            double startPressure, double stopPressure, double increment) {
        double[][] tpSegments = null;

        if (startPressure < stopPressure) {
            double t = startPressure;
            startPressure = stopPressure;
            stopPressure = t;
        }
        increment = Math.abs(increment);
        // determine the number of points to be created.
        int steps = (int) Math
                .round((startPressure - stopPressure) / increment) + 1;
        tpSegments = new double[steps][2];

        tpSegments[0][0] = startPressure;
        for (int i = 1; i < tpSegments.length; i++) {
            tpSegments[i][0] = tpSegments[i - 1][0] - increment;
        }

        tpSegments[0][1] = temperature;
        for (int i = 1; i < tpSegments.length; i++) {
            tpSegments[i][1] = WxMath.poisson(tpSegments[i - 1][0],
                    tpSegments[i][0], tpSegments[i - 1][1]);
        }

        return tpSegments;
    }

    /**
     * Get the dry adiabatic isolines.
     * 
     * @return The dry adiabatic isolines.
     */
    public static double[][][] getDryAdiabats() {
        return DRY_ADIABATS;
    }

    /**
     * @param sounding
     */
    public static float[] derivemuParcelTrajectory(VerticalSounding soundingData) {
        float[] muParcelTrajectory = new float[muParcelTrajectoryPressures.length];
        float thetae, maxthetae = -999.0f;
        float etpar, tp;
        for (int i = 0; i < soundingData.size(); i++) {
            float tt = soundingData.get(i).getTemperature();
            float td = soundingData.get(i).getDewpoint();
            float p = soundingData.get(i).getPressure();
            thetae = (float) EquivalentPotentialTemperature.ept(tt, td, p);
            if (thetae > maxthetae && soundingData.get(i).getPressure() > 500)
                maxthetae = thetae;
        }
        for (int i = 0; i < muParcelTrajectoryPressures.length; ++i) {
            float p = muParcelTrajectoryPressures[i];
            etpar = (float) (maxthetae * (Math.pow(p / 1000.0f, 0.286f)));
            tp = (float) TempOfTe.temp_of_te(etpar, p);
            muParcelTrajectory[20 - (int) (p / 50)] = tp;
        }
        return muParcelTrajectory;
    }

    /**
     * Returns the pressures used in
     * {@link #derivemuParcelTrajectory(VerticalSounding)}
     * 
     * @return
     */
    public static float[] getMuParcelTrajectoryPressures() {
        return muParcelTrajectoryPressures;
    }

    /**
     * Convert wind speed and direction to U/V components.
     * 
     * @param windSpeed
     * @param windDirection
     * @return coordinate containing u,v
     */
    public static Coordinate uvComp(float windSpeed, float windDirection) {
        double angle = Math.toRadians(windDirection);
        double u = -windSpeed * Math.sin(angle);
        double v = -windSpeed * Math.cos(angle);

        return new Coordinate(u, v);
    }

    public static Coordinate speedDir(float u, float v) {
        double spd = Math.hypot(u, v);
        double dir = Math.toDegrees(Math.atan2(-u, -v));
        if (dir < 0) {
            dir += 360;
        }
        return new Coordinate(spd, dir);
    }

    /**
     * Convert a pressure and temperature to a skew-t x,y coordinate in
     * centimeters where 0,0 occurs at 1000 hPa and 0 degrees Celsius.
     * 
     * @param pressure
     *            The pressure in hectoPascals (millibars).
     * @param temperature
     *            The temperature in degrees Celsius.
     * @return The calculated coordinate in centimeters.
     */
    public static final Coordinate getSkewTXY(double pressure,
            double temperature) {
        temperature -= SoundingPrefs.getSoundingPrefs().getTemperatureOffset();
        Coordinate point = new Coordinate();

        point.y = 132.182 - 44.061 * Math.log10(pressure);
        point.x = (0.54 * temperature) + (0.90692 * point.y);

        return point;
    }

    /**
     * Reverse a skewT coordinate (in centimeters) to the corresponding
     * temperature and pressure.
     * 
     * @param point
     * @return The temperature and pressure. coordinate.x = temperature in
     *         Celsius, coordinate.y = the pressure in hectoPascals (millibars).
     */
    public static final Coordinate reverseSkewTXY(Coordinate point) {
        Coordinate tempPressure = new Coordinate();
        tempPressure.y = Math.pow(10, ((point.y - 132.182) / -44.061));
        tempPressure.x = (point.x - (0.90692 * point.y)) / 0.54;

        tempPressure.x += SoundingPrefs.getSoundingPrefs()
                .getTemperatureOffset();

        return tempPressure;
    }

    /**
     * Compute the final temperature of a parcel moved dry adiabatically from
     * its initial pressure and temperature to a final pressure.
     * 
     * @param p0
     * @param p1
     * @param temperature
     *            in kelvin
     * @return
     */
    public static final double poisson(double p0, double p1, double temperature) {
        double t = (temperature) * Math.pow((p1 / p0), ROCP);
        return t;
    }

    /**
     * Compute the potential temperature of a parcel at a specific pressure and
     * temperature.
     * 
     * @param initPressure
     *            Initial pressure in hectoPascals (millibars).
     * @param temperature
     *            in kelvin
     * @return
     */
    public static final double potentialTemperature(double initPressure,
            double temperature) {
        return poisson(initPressure, 1000, temperature);
    }

    /**
     * Compute the potential temperature of a parcel at a specific pressure and
     * temperature.
     * 
     * @param initPressure
     *            Initial pressure in hectoPascals (millibars).
     * @param temperature
     *            Initial temperature in degrees Celsius.
     * @return
     */
    public static final double theta(double pressure1, double temperature,
            double pressure2) {
        temperature = temperature + 273.15;
        return (temperature * Math.pow((pressure2 / pressure1), ROCP)) - 273.15;
    }

    /**
     * Calculate the pressure in hectoPascals (millibars) for a given potential
     * temperature (theta) and actual temperature.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : THALVL
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param thta
     *            Theta temperature of the parcel in Celsius.
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @return The mixing ratio in gram of moist air per kilogram of dry air.
     */
    public static final double thalvl(double thta, double temperature) {
        double term1 = (thta + 273.15) / (temperature + 273.15);
        return 1000.0 / Math.pow(term1, (1 / ROCP));
    }

    /**
     * Calculate the mixing ratio for a parcel of air.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : VAPPRES
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Pressure of the parcel in hectoPascals(millibars)
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @return The mixing ratio in gram of moist air per kilogram of dry air.
     */
    public static final double vaporPressure(double temperature) {
        double pol = temperature
                * (1.1112018e-17 + temperature * (-3.0994571e-20));
        pol = temperature
                * (2.1874425e-13 + temperature * (-1.789232e-15 + pol));
        pol = temperature
                * (4.3884180e-09 + temperature * (-2.988388e-11 + pol));
        pol = temperature
                * (7.8736169e-05 + temperature * (-6.111796e-07 + pol));
        pol = .99999683e-00 + temperature * (-9.082695e-03 + pol);
        pol = (pol * pol);
        pol = (pol * pol);
        return 6.1078F / (pol * pol);
    }

    /**
     * Calculate the mixing ratio for a parcel of air.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : MIXRATIO
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Pressure of the parcel in hectoPascals(millibars)
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @return The mixing ratio in grams of moist air per kilogram of dry air.
     */
    public static final double mixingRatio(double pressure, double temperature) {
        double x = .02 * (temperature - 12.5 + 7500.0 / pressure);
        double wfw = 1 + .0000045 * pressure + .0014 * x * x;
        double fwesw = wfw * vaporPressure(temperature);
        return 621.97 * (fwesw / (pressure - fwesw));
    }

    /**
     * Compute the temperature of a parcel given its pressure and mixing ratio.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : TEMP_AT_MIXRAT
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Pressure of the parcel in hectoPascals(millibars)
     * @param mixingRatio
     *            Mixing ratio of the parcel in grams per kilogram.
     * @param temperature
     *            Temperature of the parcel in Kelvin.
     * @return .
     */
    public static final double tempAtMixingRatio(double pressure,
            double mixingRatio) {
        double c1 = .0498646455;
        double c2 = 2.4082965;
        double c3 = 7.07475;
        double c4 = 38.9114;
        double c5 = .0915;
        double c6 = 1.2035;
        // This should probably be using 621.97 per mixingRatio
        double x = Math.log10(mixingRatio * pressure / (622 + mixingRatio));
        double tmrk = Math.pow(10., c1 * x + c2) - c3 + c4
                * Math.pow(Math.pow(10., c5 * x) - c6, 2.);
        return (float) tmrk;

    }

    /**
     * Compute the lifting condensation level temperature for a parcel of air.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : LCLTEMP
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @param dewpoint
     *            Dewpoint of the parcel in Celsius.
     * @return .
     */
    public static final double lclTemp(double temperature, double dewpoint) {
        double s = temperature - dewpoint;
        double dlt = s
                * (1.2185 + 0.001278 * temperature + s
                        * (-0.00219 + 1.173E-05 * s - 0.0000052 * temperature));
        return temperature - dlt;
    }

    /**
     * Calculates the final temperature of a parcel lifted moist adiabatically
     * from an initial to final pressure.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : WETLIFT
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure1
     *            Initial pressure of the parcel in hectoPascals(millibars).
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @param presssure2
     *            Final pressure of the parcel in hectoPascals(millibars).
     * @return The final temperature of the parcel.
     */
    public static final double wetlift(double pressure1, double temperature,
            double presssure2) {
        double tha = theta(pressure1, temperature, 1000);

        double woth = wobf(tha);
        double wott = wobf(temperature);
        double thm = tha - woth + wott;
        return satlft(presssure2, thm);
    }

    /**
     * Calculates the equivalent potential temperature of a parcel.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : THETAE
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Pressure of the parcel in hectoPascals(millibars)
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @param dewpoint
     *            Dewpoint of the parcel in Celsius.
     * @return The equivalent potential temperature.
     */
    public static final double thetae(double pressure, double temperature,
            double dewpoint) {
        double t2 = lclTemp(temperature, dewpoint);
        double p2 = dryliftPressure(pressure, temperature, dewpoint);

        return theta(100, wetlift(p2, t2, 100), 1000);
    }

    /** ********************************************************** */
    /**
     * Calculates the wet-bulb potential temperature of a parcel.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : THETAW
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Pressure of the parcel in hectoPascals(millibars)
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @param dewpoint
     *            Dewpoint of the parcel in Celsius.
     * @return The equivalent potential temperature.
     */
    public static final double thetaw(double pressure, double temperature,
            double dewpoint) {
        double t2 = lclTemp(temperature, dewpoint);
        double p2 = dryliftPressure(pressure, temperature, dewpoint);

        return wetlift(p2, t2, 1000);
    }

    /**
     * Intermediate function used by other calculations.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : WOBF
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param temperature
     *            Temperature of the parcel in Celsius.
     * @return
     */
    public static final double wobf(double temperature) {
        double pol = 0;

        double x = temperature - 20;
        if (x <= 0) {
            pol = 1
                    + x
                    * (-8.841660499999999e-03 + x
                            * (1.4714143e-04 + x
                                    * (-9.671989000000001e-07 + x
                                            * (-3.2607217e-08 + x
                                                    * (-3.8598073e-10)))));
            pol = pol * pol;
            return 15.13F / (pol * pol);
        } else {
            pol = x
                    * (4.9618922e-07 + x
                            * (-6.1059365e-09 + x
                                    * (3.9401551e-11 + x
                                            * (-1.2588129e-13 + x * (1.6688280e-16)))));
            pol = 1 + x * (3.6182989e-03 + x * (-1.3603273e-05 + pol));
            pol = pol * pol;
            return 29.93 / (pol * pol) + .96 * x - 14.8;
        }
    }

    /**
     * Calculates the wetbulb temperature of a parcel.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : SATLFT
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Final pressure in hectoPascals (millibars).
     * @param satTheta
     *            Final staturated theta temperature in degrees Celsius.
     * @return The final temperature in Celsius.
     */
    public static final double satlft(double pressure, double satTheta) {
        double pwrp = Math.pow(pressure / 1000.0, ROCP);
        double t1 = (satTheta + 273.15) * pwrp - 273.15;
        double t2 = 0;
        double woto = wobf(t1);
        double wotm = wobf(satTheta);
        double wot2;
        double woe2;
        double e1 = woto - wotm;
        double e2 = 0;
        double rate = 0;
        double eor = 999;

        if ((Math.abs(pressure - 1000) - .001) <= 0) {
            return satTheta;
        }

        while (Math.abs(eor) - .1 > 0) {
            if (eor == 999) {
                /* First Pass */
                rate = 1;
            } else /* Successive Passes */
            {
                rate = (t2 - t1) / (e2 - e1);
                t1 = t2;
                e1 = e2;
            }
            t2 = t1 - e1 * rate;
            e2 = (t2 + 273.15) / pwrp - 273.15;
            wot2 = wobf(t2);
            woe2 = wobf(e2);
            e2 = e2 + wot2 - woe2 - satTheta;
            eor = e2 * rate;
        }
        return t2 - eor;
    }

    /**
     * Calculates the wetbulb temperature of a parcel.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : WETBULB
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Pressure in hectoPascals (millibars).
     * @param temperature
     *            Temperature in degrees Celsius.
     * @param dewpoint
     *            Dewpoint of the parcel in Celsius.
     * @return The wetbulb temperature in Celsius.
     */
    public static final double wetbulb(double pressure, double temperature,
            double dewpoint) {
        double t2 = lclTemp(temperature, dewpoint);
        double p2 = dryliftPressure(pressure, temperature, dewpoint);

        return wetlift(p2, t2, pressure);
    }

    /**
     * Lifts a parcel to its Lifting Condensation Level (LCL) and calculates it
     * final pressure.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : DRYLIFT
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Initial pressure in hectoPascals (millibars).
     * @param temperature
     *            Temperature in degrees Celsius.
     * @param dewpoint
     *            Dewpoint of the parcel in Celsius.
     * @param lvl2
     *            Final pressure in hectoPascals (millibars).
     * @return LCL pressure in hectoPascals (millibars).
     */
    public static final double dryliftPressure(double pressure,
            double temperature, double dewpoint) {
        return thalvl(theta(pressure, temperature, 1000d),
                lclTemp(temperature, dewpoint));
    }

    /**
     * Calculates a lifted index for a parcel.
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : LIFTED
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Initial pressure in hectoPascals (millibars).
     * @param temperature
     *            Temperature in degrees Celsius.
     * @param dewpoint
     *            Dewpoint of the parcel in Celsius.
     * @param lvl2
     *            Final pressure in hectoPascals (millibars).
     * @return Lifted index.
     */
    public static final double lifted(double pressure, double temperature,
            double dewpoint, double lvl2) {
        double p2 = dryliftPressure(pressure, temperature, dewpoint);
        double t2 = lclTemp(temperature, dewpoint);

        return wetlift(p2, t2, lvl2);
    }

    /**
     * Calculate effective surface for elevated convection. Assumes that lowest
     * layer with CAPE >= val is "sfc" Returns level (mb) of effective surface.
     * 
     * STATUS - In work
     * 
     * <pre>
     * Ported ******
     *    file     : thero.c
     *    function : LIFTED
     *    Author   : John Hart  NSSFC KCMO
     * *************
     * </pre>
     * 
     * @param pressure
     *            Initial pressure in hectoPascals (millibars).
     * @return
     */
    double esfc(double pressure) {
        // short i;
        // struct _parcel pcl;
        // struct _parcel
        // {
        // float lplpres;
        // float lpltemp;
        // float lpldwpt;
        // float blayer;
        // float tlayer;
        // float entrain;
        // float lclpres;
        // float lfcpres;
        // float elpres;
        // float mplpres;
        // float bplus;
        // float bminus;
        // float bfzl;
        // float li5;
        // float li3;
        // float brn;
        // float limax;
        // float limaxpres;
        // float cap;
        // float cappres;
        // };
        //
        // // Begin at surface and search upward for instability
        // for(int i = sfc();i < sndgp->numlev,i++)
        // {
        //
        //
        // }
        // for(i=sfc();i<sndgp->numlev;i++)
        // {
        // parcel( -1, -1, sndgp->sndg[i].pres, sndgp->sndg[i].temp,
        // sndgp->sndg[i].dwpt, &pcl);
        // if(pcl.bplus >= val)
        // {
        // return 0; // sndgp->sndg[i].pres;
        // }
        // }

        return -999.0;
    }

    /**
     * Convert pressure in millibars to Height in meters.
     * 
     * @param p
     * @return
     */
    public static double pressureToHeight(double pressureInMillibars) {
        double retValue = -999;
        // if (pressureInMillibars >= 0) {
        // retValue = (Math.log(pressureInMillibars / stdAtmPressureTrop) -
        // dryGasCompositeConstant1)
        // / dryGasCompositeConstant2;
        // }
        // return retValue;

        if (pressureInMillibars > 1.0e10 || pressureInMillibars < 1.0) {
            retValue = 1e37;
        } else if (pressureInMillibars > p11) {
            retValue = (TO - (TO * Math.pow((pressureInMillibars / p0),
                    (1 / c1)))) / gamma;
        } else {
            retValue = (c2 * Math.log10(p11 / pressureInMillibars) + z11);
        }

        return retValue;

    }

    /**
     * Convert height in meters to pressure in millibars.
     * 
     * @param z
     * @return
     */
    public static double heightToPressure(double heightInMeters) {
        double retValue = -999;
        // if (heightInMeters >= 0) {
        // retValue = Math.exp(dryGasCompositeConstant1
        // - (dryGasCompositeConstant2 * heightInMeters))
        // * stdAtmPressureTrop;
        // }
        // return retValue;

        if (heightInMeters > 1.0e10) {
            retValue = 1e37;
        } else if (heightInMeters < z11) {
            retValue = p0
                    * Math.pow(((TO - (gamma * heightInMeters)) / TO), c1);
        } else {
            retValue = p11 * (Math.pow(10, ((z11 - heightInMeters) / c2)));
        }

        return retValue;

    }

    /**
     * Compute the virtual temperature of a parcel.
     * 
     * @param pressure
     *            Pressure of the parcel in hectoPascals(millibars)
     * @param temperature
     *            Temperature of the parcel in degrees Celsius.
     * @param dewpoint
     *            Dewpoint of the parcel in degrees Celsius.
     * @return Computed virtual temperature in degrees Celsius. If
     */
    public static double virtualT(double pressure, double temperature,
            double dewpoint) {

        double r = mixingRatio(pressure, dewpoint) / 1000;
        double v = 1 + (r / EPS);

        v = temperature / (1 + r) * v;

        return v;
    }

    public static double virttemp(double T, double Td, double P) {
        // Calculates virtual temperature as a function of temperature,
        // dewpoint, and pressure (mb).

        // Author: D Perry

        // Internal variables
        double e, w, K, Kd;

        // Data error flags
        double Flag = 1e37;

        // Account for both Celsius and Kelvin.
        K = T;
        Kd = Td;
        if (K < 100.) {
            K = K + 273.15;
        }
        if (Kd < 100.) {
            Kd = Kd + 273.15;
        }

        // Flag ridiculous values.
        if (K < 0.0 || K > 373.15) {
            return Flag;
        }

        // Avoid floating underflow.
        if (Kd < 173.15) {
            return K;
        }

        // Calculation for normal range of values.
        e = esat(Kd);
        w = (0.622 * e) / (P - e);
        return K * (1 + (0.6 * w));
    }

    public static double esat(double T) {
        // Calculates saturation vapor pressure in millibars as a function of
        // either Kelvin of Celcius temperature.

        // Author: J Ramer written in the late 1980's

        // Is based upon a variation of the integrated form of the
        // Clausius-Clapeyron
        // equation. Has an additional linear term in it and is fit to data in
        // the Smithsonian Meterological Tables. Is accurate to one part in a
        // thousand over the range from -25C to +35C. Its main advantage is that
        // it is invertable.

        double K;

        double Flag = 1e37;

        // Account for both Celsius and Kelvin.
        K = T;
        if (K < 100.) {
            K = K + 273.15;
        }

        // Flag ridiculous values.
        if (K < 0.0 || K > 373.15) {
            return Flag;
        }

        // Avoid floating underflow.
        if (K < 173.15) {
            return 3.777647E-05;
        }

        // Calculation for normal range of values.
        return Math.exp(26.660820 - 0.0091379024 * K - 6106.3960 / K);
    }

    /**
     * Specific Humidity SH= (0.622*E)/(Mb-(0.378*E)) where E is the vapor
     * pressure
     * 
     * @return double
     */
    public static double specificHumidity(double temperature, double pressure) {

        return (0.622 * vaporPressure(temperature))
                / (pressure - (0.378 * vaporPressure(temperature)));
    }

    public static final void main(String[] args) {
        System.out.println(pressureToHeight(850));

        System.out.println(pressureToHeight(500));

        System.out.println(pressureToHeight(300));

        System.out.println(pressureToHeight(100));

        System.out.println(tempAtMixingRatio(845.0, 6.42));

        System.out.println("virtualT "
                + (virtualT(845.0, 281.55, 183.15) + 273.15));
        System.out.println("virttemp " + virttemp(281.55f, 183.15f, 845.0f));

    }

}
