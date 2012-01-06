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

package com.raytheon.edex.meteoLib;

import java.nio.FloatBuffer;

import com.raytheon.edex.meteolibrary.Meteolibrary;

/*
 * the Meteolib capabilities. To access capabilities, please call
 * Controller.methodName(parameters);
 */

/**
 * 
 * Interface to native meteolib methods
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2007            mnash       Initial creation
 * Jan  9, 2008            njensen     Refactored python interfaces
 * Oct  6, 2009            randerso    Removed python interfaces
 * </pre>
 * 
 * @version 1.0
 * 
 *          Check each function for units to match those originally in legacy
 *          code. If units are not given, units were not given in legacy code.
 */
public class Controller {

    /**
     * <b>Main Description</b> : Adds two arrays to each other and finds the sum
     * of each field
     * 
     * @param first_array
     * @param second_array
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param tmode
     * @return result <br>
     * <br>
     * 
     *         result is the computed array
     */
    public static float[] add_aray(float[] first_array, float[] second_array,
            int totalDimension, int IGridDimension, int JGridDimension,
            int tmode) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        int mode[] = { tmode };
        Meteolibrary.add_aray(first_array, 0, second_array, 0, result, 0, mni,
                0, ni, 0, nj, 0, mode, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Adds a constant to each value in an array
     * 
     * @param array
     * @param const_to_add
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return result <br>
     * <br>
     * 
     *         result is the computed array
     */
    public static float[] add_by_cnst(float[] array, float const_to_add,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        float cnst[] = { const_to_add };
        Meteolibrary.add_by_cnst(array, 0, cnst, 0, result, 0, mni, 0, ni, 0,
                nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : This routine calculates the equivalent
     * tempurature of a temperature and pressure using the adiabatic definition,
     * assuming saturation put a fudge factor into L/cp to get agreement of
     * moist adiabats with a published thermodynamic diagram
     * 
     * @param temp
     *            (K)
     * @param press
     *            (mb)
     * @return Moist_Adiabatic_Temperature <br>
     * <br>
     * 
     */
    public static float adiabatic_te(float temp, float press) {
        float ttemp[] = { temp };
        float tpress[] = { press };
        return Meteolibrary.adiabatic_te(ttemp, 0, tpress, 0);
    }

    /**
     * <b>Main Description</b> : Converts elevation and altimeter setting to
     * pressure
     * 
     * @param altSetting
     *            (X)
     * @param elevation
     *            (m)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return pressure (X) <br>
     * <br>
     * 
     */
    public static float[] alt2press(float[] altSetting, float[] elevation,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] pressure = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.alt2press(altSetting, 0, elevation, 0, mni, 0, ni, 0, nj,
                0, pressure, 0);
        return pressure;
    }

    /**
     * <b>Main Description</b> : Calculates layer mean wind given top and bottom
     * of the layer defined in terms of kilometers above ground level
     * 
     * @param elevation
     *            (m agl)
     * @param topWindLayer
     *            (km agl)
     * @param bottomWindLayer
     *            (km agl)
     * @param windLevelHeights
     *            (m asl)
     * @param windLevelPressure
     * @param windLevelTemp
     *            (K)
     * @param windLevelUComp
     *            (m/s)
     * @param windLevelVComp
     *            (m/s)
     * @param numWindLevels
     * @param layerMeanUComp
     * @param layerMeanVComp
     * @param layerMeanWindDir
     *            (deg)
     * @param layerMeanWindSpd
     *            (m/s) <br>
     * <br>
     * 
     *            layerMeanUComp, layerMeanVComp, layerMeanWindDir, and
     *            layerMeanWindSpd are output arrays
     */
    public static WindComp avwind(float elevation, float topWindLayer,
            float bottomWindLayer, float[] windLevelHeights,
            float[] windLevelPressure, float[] windLevelTemp,
            float[] windLevelUComp, float[] windLevelVComp) {
        WindComp wind = new WindComp();
        float elev[] = { elevation };
        float top[] = { topWindLayer };
        float bottom[] = { bottomWindLayer };
        int nw[] = { windLevelUComp.length };
        float uavg[] = new float[1];
        float vavg[] = new float[1];
        float avdir[] = new float[1];
        float avspd[] = new float[1];
        Meteolibrary.avwind(elev, 0, top, 0, bottom, 0, windLevelHeights, 0,
                windLevelPressure, 0, windLevelTemp, 0, windLevelUComp, 0,
                windLevelVComp, 0, nw, 0, uavg, 0, vavg, 0, avdir, 0, avspd, 0);
        wind.setWindDirection(avdir[0]);
        wind.setWindSpeed(avspd[0]);
        wind.setUComp(uavg[0]);
        wind.setVComp(vavg[0]);
        return wind;
    }

    /**
     * <b>Main Description</b> : Calculates the heat index using temperature and
     * dewpoint in Celsius
     * 
     * @param temperature
     *            (C)
     * @param dewPoint
     *            (C)
     * @return Heat_Index (C) <br>
     * <br>
     *         Bail out if temperature < 80F or dewpoint is missing. Lans'
     *         formula really doesn't work well below 80F, and there's not much
     *         point in calculating it.
     */
    public static float calcHeatIndex(float temperature, float dewPoint) {
        return Meteolibrary.calcHeatIndex(temperature, dewPoint);
    }

    /**
     * <b>Main Description</b> : Calculates the wind chill from temperature and
     * wind speed
     * 
     * @param temperature
     *            (C)
     * @param windSpd
     *            (km/h)
     * @return Wind_Chill (C) <br>
     * <br>
     *         Arbitrarily do the calculation only for temps at or below 60F <br>
     *         no chilling if wind speed < 6.44km/h <br>
     *         peg speed at = 128.75 km/h
     */
    public static float calcWindChill(float temperature, float windSpd) {
        return Meteolibrary.calcWindChill(temperature, windSpd);
    }

    /**
     * <b>Main Description</b> : Calculate condensation pressure from pressure,
     * temperature, and relative humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return condensationPressure (mb) <br>
     * <br>
     *         condensationPressure is the computed array
     */
    public static float[] calccondpr(float[] pressure, float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] condensationPressure = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calccondpr(pressure, 0, temperature, 0, relativeHumidity,
                0, mni, 0, ni, 0, nj, 0, condensationPressure, 0);
        return condensationPressure;
    }

    /**
     * <b>Main Description</b> : Calculates condensation pressure deficit from
     * pressure, temperature, and relative humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return condensationPressureDeficit (mb) <br>
     * <br>
     *         condensationPressureDeficit is the computed array
     */
    public static float[] calccondprdef(float[] pressure, float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] condensationPressureDeficit = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calccondprdef(pressure, 0, temperature, 0,
                relativeHumidity, 0, mni, 0, ni, 0, nj, 0,
                condensationPressureDeficit, 0);
        return condensationPressureDeficit;
    }

    /**
     * <b>Main Description</b> : Calculates dewpoint depression from temperature
     * and relative humidity
     * 
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return dewpointDepression <br>
     * <br>
     *         dewpointDepression is the computed array
     */
    public static float[] calcdpd(float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] dewpointDepression = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calcdpd(temperature, 0, relativeHumidity, 0, mni, 0, ni,
                0, nj, 0, dewpointDepression, 0);
        return dewpointDepression;
    }

    /**
     * <b>Main Description</b> : Calculates storm relative helicity from the
     * surface to 3000 meters
     * 
     * @param windLevelHeights
     *            (m asl)
     * @param windLevelPressures
     *            (mb)
     * @param windLevelUComp
     *            (m/s)
     * @param windLevelVComp
     *            (m/s)
     * @param elevation
     *            (m agl)
     * @param topHelicityLayer
     *            (m agl)
     * @param tghx
     * @param tghy
     * @param avgWindDir
     *            (deg)
     * @param avgWindSpd
     *            (m/s)
     * @return wind (object with
     *         stormMotionDir,stormMotionSpd,helicity,stormRelativeHelicity
     *         (m2/s2)) <br>
     * <br>
     * 
     *         stormMotionDir (30 deg right of avgWindDir), stormMotionSpd (75%
     *         avgWindSpd), and stormRelativeHelicity are computed values <br>
     *         Values are calculated 0-6km
     */
    public static WindComp calchelicity(float[] windLevelHeights,
            float[] windLevelPressures, float[] windLevelUComp,
            float[] windLevelVComp, float elevation, float topHelicityLayer,
            float[] ghx, float[] ghy, float avgWindDir, float avgWindSpd) {
        WindComp wind = new WindComp();
        int nw[] = { windLevelUComp.length };
        float elev[] = { elevation };
        float ztop[] = { topHelicityLayer };
        // float ghx[] = new float[1];
        // float ghy[] = new float[1];
        float diravg[] = { avgWindDir };
        float spdavg[] = { avgWindSpd };
        float stmdir[] = new float[1];
        float stmspd[] = new float[1];
        float thelicity[] = new float[1];
        float SRHel[] = new float[1];
        Meteolibrary.calchelicity(windLevelHeights, 0, windLevelPressures, 0,
                windLevelUComp, 0, windLevelVComp, 0, nw, 0, elev, 0, ztop, 0,
                ghx, 0, ghy, 0, diravg, 0, spdavg, 0, stmdir, 0, stmspd, 0,
                thelicity, 0, SRHel, 0);
        wind.setStormMotionDir(stmdir[0]);
        wind.setStormMotionSpd(stmspd[0]);
        wind.setHelicity(thelicity[0]);
        wind.setStormRelativeHelicity(SRHel[0]);
        return wind;
    }

    /**
     * <b>Main Description</b> : Calculates the K index
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     * @param dewpoint
     * @param numOfLevel
     * @return K_Index <br>
     * <br>
     *         k is computed value
     */
    public static float calckidx(float pressure, float temperature,
            float dewpoint, int numOfLevel) {
        float k = 0;
        float press[] = { pressure };
        float temp[] = { temperature };
        float ttd[] = { dewpoint };
        float tk[] = { k };
        Meteolibrary.calckidx(press, 0, temp, 0, ttd, 0, numOfLevel, tk, 0);
        return tk[0];
    }

    /**
     * <b>Main Description</b> : Calculates the lifted index from the pressure,
     * temperature, and relative humidity at 500mb
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param tempAt500
     *            (K)
     * @param upperPressure
     *            (normally 500mb)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return liftedIndex (C) <br>
     * <br>
     *         lifted index is the computed array
     */
    public static float[] calcli(float[] pressure, float[] temperature,
            float[] relativeHumidity, float[] tempAt500, float upperPressure,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] liftedIndex = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        float up[] = { upperPressure };
        Meteolibrary.calcli(pressure, 0, temperature, 0, relativeHumidity, 0,
                tempAt500, 0, up, 0, mni, 0, ni, 0, nj, 0, liftedIndex, 0);
        return liftedIndex;
    }

    /**
     * <b>Main Description</b> : Calculates the isentropic potential vorticity
     * through the layer
     * 
     * @param upperIsentropePressure
     *            (mb)
     * @param currentIsentropePressure
     *            (mb)
     * @param upperIsentrope
     *            (K)
     * @param currentIsentrope
     *            (K)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param uWindsUpperIsentrope
     *            (m/s)
     * @param vWindsUpperIsentrope
     *            (m/s)
     * @param uWindsLowerIsentrope
     *            (m/s)
     * @param vWindsLowerIsentrope
     *            (m/s)
     * @param array1AbsVorticity
     *            (/s)
     * @param array2AbsVorticity
     *            (/s)
     * @param xGridInterval
     *            (m)
     * @param yGridInterval
     *            (m)
     * @param coriolisParam
     *            (/s)
     * @return potentialVoritcity (K/mb/s) <br>
     * <br>
     *         1. Stability is defined as -dP/d(theta). We calculate this
     *         through the layer from the isentropic surface 'n' to the surface
     *         above it, 'n+1'.<br>
     *         2. Since we are dealing with a layer, we calculate a mean
     *         absolute vorticity using the winds at the upper and lower layers.<br>
     *         3. The PV is then [mean abs. vort.]/[stability]
     */
    public static float[] calcpv(float[] upperIsentropePressure,
            float[] currentIsentropePressure, float upperIsentrope,
            float currentIsentrope, int totalDimension, int IGridDimension,
            int JGridDimension, float[] uWindsUpperIsentrope,
            float[] vWindsUpperIsentrope, float[] uWindsLowerIsentrope,
            float[] vWindsLowerIsentrope, float[] array1AbsVorticity,
            float[] array2AbsVorticity, float[] xGridInterval,
            float[] yGridInterval, float[] coriolisParam) {
        float[] potentialVorticity = new float[upperIsentropePressure.length];
        float o_low[] = { currentIsentrope };
        float o_up[] = { upperIsentrope };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calcpv(upperIsentropePressure, 0,
                currentIsentropePressure, 0, o_up, 0, o_low, 0,
                potentialVorticity, 0, mni, 0, ni, 0, nj, 0,
                uWindsUpperIsentrope, 0, vWindsUpperIsentrope, 0,
                uWindsLowerIsentrope, 0, vWindsLowerIsentrope, 0,
                array1AbsVorticity, 0, array2AbsVorticity, 0, xGridInterval, 0,
                yGridInterval, 0, coriolisParam, 0);
        return potentialVorticity;
    }

    /**
     * <b>Main Description</b> : Calculates the relative humidity from
     * temperature and dewpoint
     * 
     * @param temperature
     *            (C or K)
     * @param dewpoint
     *            (C or K)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return relativeHumidity (0...100) <br>
     * <br>
     *         relativeHumidity is the computed array
     */
    public static float[] calcrh(float[] temperature, float[] dewpoint,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] relativeHumidity = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calcrh(temperature, 0, dewpoint, 0, mni, 0, ni, 0, nj, 0,
                relativeHumidity, 0);
        return relativeHumidity;
    }

    /**
     * <b>Main Description</b> : Calculates relative humdity from pressure,
     * temperature and specific humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param specificHumidity
     *            (g/Kg)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return relativeHumidity (0...100) <br>
     * <br>
     *         relativeHumidity is the computed array
     */
    public static float[] calcrh2(float[] pressure, float[] temperature,
            float[] specificHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] relativeHumidity = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calcrh2(pressure, 0, temperature, 0, specificHumidity, 0,
                mni, 0, ni, 0, nj, 0, relativeHumidity, 0);
        return relativeHumidity;
    }

    /**
     * <b>Main Description</b> : Calculates dewpoint from temperature and
     * relative humidity
     * 
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return dewpoint (K) <br>
     * <br>
     *         dewpoint is the computed array
     */
    public static float[] calctd(float[] temperature, float[] relativeHumidity,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] dewpoint = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calctd(temperature, 0, relativeHumidity, 0, mni, 0, ni, 0,
                nj, 0, dewpoint, 0);
        return dewpoint;
    }

    /**
     * <b>Main Description</b> : Calculates dewpoint from pressure, temperature,
     * and specific humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param specificHumidity
     *            (g/Kg)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return dewpoint (K) <br>
     * <br>
     *         dewpoint is the computed array
     */
    public static float[] calctd2(float[] pressure, float[] temperature,
            float[] specificHumidity, int IGridDimension, int JGridDimension) {
        float[] dewpoint = new float[IGridDimension * JGridDimension];
        int mni[] = { IGridDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calctd2(pressure, 0, temperature, 0, specificHumidity, 0,
                mni, 0, ni, 0, nj, 0, dewpoint, 0);
        return dewpoint;
    }

    /**
     * <b>Main Description</b> : Calculates equivalent potential temperature
     * from pressure, temperature, and relative humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (C or K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return thetae (K) <br>
     * <br>
     *         thetae is the computed array
     */
    public static float[] calcthetae(float[] pressure, float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] thetae = new float[totalDimension];
        int mni[] = { totalDimension };
        int[] ni = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calcthetae(pressure, 0, temperature, 0, relativeHumidity,
                0, mni, 0, ni, 0, nj, 0, thetae, 0);
        return thetae;
    }

    /**
     * <b>Main Description</b> : Calculates equivalent potential temperature
     * from pressure, temperature, and dewpoint
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (C or K)
     * @param dewpoint
     *            (C or K)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return thetae (K) <br>
     * <br>
     *         thetae is the computed array
     */
    public static float[] calcthetae2(float[] pressure, float[] temperature,
            float[] dewpoint, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] thetae = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calcthetae2(pressure, 0, temperature, 0, dewpoint, 0, mni,
                0, ni, 0, nj, 0, thetae, 0);
        return thetae;
    }

    /**
     * <b>Main Description</b> : Calculates the total index
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     * @param dewpoint
     * @param numOfLevel
     * @return Total_Index <br>
     * <br>
     *         total is the computed value
     */
    public static float calctotidx(float pressure, float temperature,
            float dewpoint, int numOfLevel) {
        float total = 0;
        float press[] = { pressure };
        float temp[] = { temperature };
        float td[] = { dewpoint };
        float ttotal[] = { total };
        Meteolibrary
                .calctotidx(press, 0, temp, 0, td, 0, numOfLevel, ttotal, 0);
        return ttotal[0];
    }

    /**
     * <b>Main Description</b> : Calculates the virtual temperature from the
     * pressure, temperature, and relative humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return virtualTemperature <br>
     * <br>
     *         virtualTemperature is the computed array
     */
    public static float[] calctv(float[] pressure, float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] virtualTemperature = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calctv(pressure, 0, temperature, 0, relativeHumidity, 0,
                mni, 0, ni, 0, nj, 0, virtualTemperature, 0);
        return virtualTemperature;
    }

    /**
     * <b>Main Description</b> : Calculates virtual temperature from temperature
     * and specific humidity
     * 
     * @param temperature
     *            (K)
     * @param specificHumidity
     *            (g/kg)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return virtualTemperature (K) <br>
     * <br>
     *         virtualtemperature is the computed array
     */
    public static float[] calctv2(float[] temperature,
            float[] specificHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] virtualTemperature = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calctv2(temperature, 0, specificHumidity, 0, mni, 0, ni,
                0, nj, 0, virtualTemperature, 0);
        return virtualTemperature;
    }

    /**
     * <b>Main Description</b> : Calculates wet bulb temperature from
     * temperature and relative humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return wetBulbTemp (K) <br>
     * <br>
     *         wetBulbTemp is the computed array
     */
    public static float[] calctw(float[] pressure, float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] wetBulbTemp = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.calctw(pressure, 0, temperature, 0, relativeHumidity, 0,
                mni, 0, ni, 0, nj, 0, wetBulbTemp, 0);
        return wetBulbTemp;
    }

    /**
     * ** NO INFORMATION WAS GIVEN ON PARAMETERS AND/OR UNITS OF SAID
     * PARAMETERS, THEREFORE INFORMATION IS RETRIEVED PENDING CORRECT PARAMETERS
     * **
     * 
     * @param usetv
     * @param p_dat
     * @param tve_dat
     * @param p0
     * @param th0
     * @param sh0
     * @return
     */
    public static PHT capeFunc(float usetv, float[] p_dat, float[] tve_dat,
            float p0, float th0, float sh0, int mnx, int nx, int ny, int nz) {
        PHT pht = new PHT();

        FloatBuffer[] tp_dat = { FloatBuffer.wrap(p_dat) };
        FloatBuffer[] ttve_dat = { FloatBuffer.wrap(tve_dat) };
        float[] tp0 = { p0 };
        float[] tth0 = { th0 };
        float[] tsh0 = { sh0 };
        float[] cape_dat = new float[1];
        float[] cin_dat = new float[1];
        Meteolibrary.capeFunc(usetv, tp_dat, ttve_dat, tp0, 0, tth0, 0, tsh0,
                0, mnx, nx, ny, nz, cape_dat, 0, cin_dat, 0);

        pht.setPositiveEnergy(cape_dat[0]);
        pht.setCin(cin_dat[0]);
        return pht;
    }

    /**
     * <b>Main Description</b> : Calculates the pressure, height, and
     * temperature of the Convective Condensation Level (CCL) from a sounding
     * 
     * @param mixRatio
     *            (g/kg)
     * @param soundingPressure
     *            (mb)
     * @param soundingHeight
     *            (m asl)
     * @param soundingTemperature
     *            (K)
     * @param numSoundingLevels
     * @param convectiveCondensationLevelPressure
     *            (mb)
     * @param convectiveCondensationLevelTemperature
     *            (K)
     * @param convectiveCondensationLevelHeight
     *            (m asl) <br>
     * <br>
     *            1) The low level mean mixing ratio is input to this routine...
     *            computed outside.<br>
     *            2) On days with a strong low level inversion, the convective
     *            temperature may seem low because the strict definition is used
     *            in the computation (i.e., where the low level mixing ratio
     *            line first intersects the sounding).<br>
     *            convectiveCondensationLevelPressure,
     *            convectiveCondensationLevelTemperature, and
     *            convectiveCondensationLevelHeight are all the computed values
     */
    public static PHT cclpar(float mixRatio, float[] soundingPressure,
            float[] soundingHeight, float[] soundingTemperature) {
        PHT pht = new PHT();
        float mix[] = { mixRatio };
        float pccl[] = new float[1];
        float tccl[] = new float[1];
        float hccl[] = new float[1];
        int nlvls[] = { soundingTemperature.length };
        Meteolibrary.cclpar(mix, 0, soundingPressure, 0, soundingHeight, 0,
                soundingTemperature, 0, nlvls, 0, pccl, 0, tccl, 0, hccl, 0);
        // System.out.println("sp is " + soundingPressure + "\nsh is : " +
        pht.setPressure(pccl[0]);
        pht.setHeight(hccl[0]);
        pht.setTemperature(tccl[0]);
        return pht;
    }

    /**
     * <b>Main Description</b> : Calculates the convective gust potential based
     * on Western Region Technical Attachments
     * 
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param pressure
     *            (mb)
     * @param wetBulbTemp
     *            (K)
     * @param surfacePressure
     *            (mb)
     * @param lastPressure
     *            (mb)
     * @param mixRatio
     *            (>0 water, <0 ice)
     * @param pressureIncrement
     *            (mb)
     * @return Convective_Gust_Potential (1,2,3,4) <br>
     * <br>
     *         All Measurements on the uniform pressure grid<br>
     * <br>
     * 
     *         The original algorithm lifted the 500 mb parcel and used the 700
     *         mb dewpoint depression. Here we find the most unstable parcel
     *         "near" 500 mb and the driest parcel "near" 700 mb. We also need
     *         the environmental temperature "near" 300 mb and 400 mb. "Near" at
     *         500 and 700 mb means a 60 mb interval (470-530 mb, 670-730 mb) or
     *         a deltap interval if the interpolated data is too coarse. "Near"
     *         at 300 and 400 mb means a 20 mb interval (290-310 mb, 390-410 mb)
     *         or a deltap interval if interpolated data is too coarse. This
     *         section determines the intervals. <br>
     * <br>
     * 
     *         Now that the intervals to find things have been determined, find
     *         the most unstable parcel defined as that parcel with the highest
     *         wet bulb potential temperature. Also, find the parcel with the
     *         largest dew point depression and the pressure levels
     *         corresponding to the near 300 and near 400 mb temperatures.<br>
     * <br>
     * 
     *         Lift the most unstable parcel found above to its LCL. Then lift
     *         it pseudo-moist adiabatically up to "near" 400 mb then from there
     *         up to "near" 300 mb. Compare these lifted parcel temperatures to
     *         the environmental temperatures at these levels to calculate the
     *         upper stability index.<br>
     * <br>
     * 
     *         Convective_Gust_Potential is the returned value at an integer 1,
     *         2, 3, or 4
     */
    public static float cgp(float[] temperature, float[] dewpoint,
            float[] pressure, float[] wetBulbTemp, float surfacePressure,
            float lastPressure, int mixRatio, float pressureIncrement) {
        float sfcpres[] = { surfacePressure };
        float toppres[] = { lastPressure };
        int iw[] = { mixRatio };
        float deltap[] = { pressureIncrement };
        return Meteolibrary.cgp(temperature, 0, dewpoint, 0, pressure, 0,
                wetBulbTemp, 0, sfcpres, 0, toppres, 0, iw, 0, deltap, 0);
    }

    /**
     * <b>Main Description</b> : Calculates component of the first vector in the
     * direction of the second
     * 
     * @param xComponent
     * @param yComponent
     * @param x2Component
     * @param y2Component
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param tcontrol
     * @return wind (object with compFirstInSecond and compFirstInKDir) <br>
     * <br>
     * 
     *         tcontrol : Number of degrees to rotate second vector before it is
     *         dotted with the first vector. If not an integer, then do not
     *         normalize by the magnitude of the second vector. If thousands
     *         place is one, output component unsigned. If thousands place is
     *         two output the component as a vector, with comp being the x
     *         component and comp2 being the y-component. If thousands place is
     *         three output a second component in comp2, in the direction of the
     *         k-cross of second vector. <br>
     * <br>
     * 
     *         compFirstInSecond and compFirstInKDir are computed arrays
     */
    public static WindComp comp_by(float[] xComponent, float[] yComponent,
            float[] x2Component, float[] y2Component, int totalDimension,
            int IGridDimension, int JGridDimension, float tcontrol) {
        WindComp wind = new WindComp();
        float[] comp = new float[xComponent.length];
        float[] comp2 = new float[xComponent.length];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        float control[] = { tcontrol };
        Meteolibrary.comp_by(xComponent, 0, yComponent, 0, x2Component, 0,
                y2Component, 0, mni, 0, ni, 0, nj, 0, control, 0, comp, 0,
                comp2, 0);
        wind.setCompFirstInKDir(comp2);
        wind.setCompFirstInSecond(comp);
        return wind;
    }

    /**
     * <b>Main Description</b> : Crosses a field of vectors by another
     * 
     * @param vectorArrayAX
     * @param vectorArrayAY
     * @param vectorArrayBX
     * @param vectorArrayBY
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return result <br>
     * <br>
     *         result is the computed array
     */
    public static float[] crossvectors(float[] vectorArrayAX,
            float[] vectorArrayAY, float[] vectorArrayBX,
            float[] vectorArrayBY, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.crossvectors(vectorArrayAX, 0, vectorArrayAY, 0,
                vectorArrayBX, 0, vectorArrayBY, 0, result, 0, mni, 0, ni, 0,
                nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Estimates the cloud top based on undiluted
     * parcel vertical velocity profile
     * 
     * @param parcelPressure
     *            (mb)
     * @param parcelHeight
     *            (m asl)
     * @param parcelVertVelocity
     *            (m/s)
     * @param equilibriumPressure
     *            (mb)
     * @param numParcelLevels
     * @return cloudTop (m asl) <br>
     * <br>
     *         1) The estimated cloud top is the level where the vertical
     *         velocity drops to zero above the equilibrium level. <br>
     *         2) If the parcel vertical velocity does not drop to zero, a value
     *         of 99999 is returned for the cloud top...meaning that the top is
     *         above the top of the sounding.<br>
     *         estimatedCloudtop is computed value
     */
    public static float ctop(float[] parcelPressure, float[] parcelHeight,
            float[] parcelVertVelocity, float equilibriumPressure,
            int numParcelLevels) {
        float peqlev[] = { equilibriumPressure };
        int npar[] = { numParcelLevels };
        float cldtop[] = new float[1];
        Meteolibrary.ctop(parcelPressure, 0, parcelHeight, 0,
                parcelVertVelocity, 0, peqlev, 0, npar, 0, cldtop, 0);
        return cldtop[0];
    }

    /**
     * <b>Main Description</b> : Calculates Julian day from input dates
     * 
     * @param year
     * @param month
     * @param day
     * @param istatus
     * @return jd[0] <br>
     * <br>
     *         Julian_Day is the computed value<br>
     *         Julian_Day is an integer form of the current day in the form
     *         [ddd].
     */
    public static int cv_date2jul(int year, int month, int day, int istatus) {
        int julianDay = 0;
        int yr[] = { year };
        int mon[] = { month };
        int tday[] = { day };
        int jd[] = { julianDay };
        int tistatus[] = { istatus };
        Meteolibrary.cv_date2jul(yr, 0, mon, 0, tday, 0, jd, 0, tistatus, 0);
        return jd[0];
    }

    /**
     * <b>Main Description</b> : Compute the convective gust potential by
     * locating the 700 mb dewpoint depression and the upper level stability
     * index on the nomogram presented in Western Region Technical Attachment
     * 76-14 (June, 1976).
     * 
     * @param dewpointDepression700mb
     *            (C)
     * @param upperLevelStabIndex
     *            (C)
     * @return microburstPotential <br>
     * <br>
     *         Microburst potential index as follows:<br>
     *         1 = gusts less than 30 kt (low level too moist)<br>
     *         2 = gusts less than 30 kt (upper level too stable)<br>
     *         3 = gusts 30 to 40 kt possible<br>
     *         4 = gusts greater than 40 kt possible<br>
     */
    public static int cvgust(float dewpointDepression700mb,
            float upperLevelStabIndex, int microburstPotential) {
        float dd7[] = { dewpointDepression700mb };
        float ui[] = { upperLevelStabIndex };
        int gstpot[] = { microburstPotential };
        Meteolibrary.cvgust(dd7, 0, ui, 0, gstpot, 0);
        return gstpot[0];
    }

    /**
     * <b>Main Description</b> : Calculates the wind direction (deg) and speed
     * given rectangular wind components
     * 
     * @param uWindComp
     * @param vWindComp
     * @param numWindLevels
     * @return wind (object containing direction and speed) <br>
     * <br>
     *         User may use this routine to convert only one set of components
     *         by passing a value of 1 for numWindLevels.<br>
     * <br>
     * 
     *         windDir and windSpd are computed arrays
     */
    public static WindComp ddff(float[] uWindComp, float[] vWindComp,
            int numWindLevels) {
        WindComp wind = new WindComp();
        float[] windDir = new float[uWindComp.length];
        float[] windSpd = new float[uWindComp.length];
        int nlvls[] = { numWindLevels };
        Meteolibrary.ddff(uWindComp, 0, vWindComp, 0, windDir, 0, windSpd, 0,
                nlvls, 0);
        wind.setWindDirectionArray(windDir);
        wind.setWindSpeedArray(windSpd);
        return wind;
    }

    /**
     * <b>Main Description</b> : Calculates the dry adiabat from the initial
     * parcel pressure to the LCL and the moist adiabat above the LCL. These two
     * curves define the path of a lifted parcel
     * 
     * @param tempAtLCL
     *            (K)
     * @param pressureAtLCL
     *            (mb)
     * @return weather (object with dryAdiabatLCL(K) and moistAdiabatLCL (K)) <br>
     * <br>
     *         dryAdiabatLCL and moistAdiabatLCL are the compute values
     */
    public static PHT deftrk(float tempAtLCL, float pressureAtLCL) {
        PHT weather = new PHT();
        float moistAdiabatLCL = 0;
        float dryAdiabatLCL = 0;
        float tcb[] = { tempAtLCL };
        float pcb[] = { pressureAtLCL };
        float thdpar[] = { dryAdiabatLCL };
        float eptpar[] = { moistAdiabatLCL };
        Meteolibrary.deftrk(tcb, 0, pcb, 0, thdpar, 0, eptpar, 0);
        weather.setDryAdiabat(thdpar[0]);
        weather.setMoistAdiabat(eptpar[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates the air density at each level of a
     * sounding
     * 
     * @param pressure
     *            (mb)
     * @param virtualTemperature
     *            (C)
     * @param numLevels
     * @return airDensity (kg/cu m) <br>
     * <br>
     *         airDensity (rho) is the computed array
     */
    public static float[] density(float[] pressure, float[] virtualTemperature) {
        int numLevels = virtualTemperature.length;
        float[] airDensity = new float[numLevels];
        int nlvls[] = { numLevels };

        Meteolibrary.density(pressure, 0, virtualTemperature, 0, nlvls, 0,
                airDensity, 0);

        return airDensity;
    }

    /**
     * <b>Main Description</b> : Calculate the derivateive of a with respect to
     * b
     * 
     * @param a1
     * @param a2
     * @param b1
     * @param b2
     * @return result
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * <br>
     * <br>
     *            result is the computed array
     */
    public static float[] derivative(float[] a1, float[] a2, float[] b1,
            float[] b2, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.derivative(a1, 0, a2, 0, b1, 0, b2, 0, result, 0, mni, 0,
                ni, 0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description:</b><br>
     * 
     * Computes a smoothed array. The xoff,yoff and nx,ny parameters allow a
     * subgrid to be specified.
     * 
     * @param input
     *            input data
     * @param npts
     *            smoothing distance in grid cells
     * @param mnx
     *            x dimension of input grid
     * @param xoff
     *            x index of starting grid cell to be processed
     * @param yoff
     *            y index of starting grid cell to be processed
     * @param nx
     *            number of cells in x direction to be processed
     * @param ny
     *            number of cells in y direction to be processed
     * @return output data
     */
    public synchronized static float[] dist_filter(float[] input, float npts,
            int mnx, int xoff, int yoff, int nx, int ny) {

        float tnpts[] = { npts };
        int tmnx[] = { mnx };
        int tnx[] = { nx };
        int tny[] = { ny };
        float output[] = new float[input.length];
        int offset = yoff * mnx + xoff;
        Meteolibrary.dist_filter(input, offset, tnpts, 0, output, offset, tmnx,
                0, tnx, 0, tny, 0);

        return output;
    }

    /**
     * <b>Main Description</b> : Divide one field by another. Each i,j in one
     * array is divided by the corresponding i,j in the other array
     * 
     * @param a
     * @param b
     * @return result
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * <br>
     * <br>
     *            result = a/b<br>
     *            result is the computed array
     */
    public static float[] div_aray(float[] a, float[] b, int totalDimension,
            int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        int[] mni = { totalDimension };
        int[] ni = { IGridDimension };
        int[] nj = { JGridDimension };
        Meteolibrary.div_aray(a, 0, b, 0, result, 0, mni, 0, ni, 0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Calculates the water vapor mixing ratio with
     * respect to either water or ice.
     * 
     * @param temperature
     *            (K)
     * @param pressure
     *            (mb)
     * @param iceOrWater
     *            (>0 mixing ratio water, <0 mixing ratio ice)
     * @return Mixing_Ratio(g/kg) <br>
     * <br>
     *         Mixing Ratio is the computed value
     */
    public static float dmixr(float temperature, float pressure, int iceOrWater) {
        float temp[] = { temperature };
        float pres[] = { pressure };
        int iw[] = { iceOrWater };
        return Meteolibrary.dmixr(temp, 0, pres, 0, iw, 0);
    }

    /**
     * <b>Main Description</b> : Dot a field of vectors by another. Each i,j in
     * one array of vectors is dotted with the corresponding i,j in the other
     * array of vectors
     * 
     * @param ax
     * @param ay
     * @param bx
     * @param by
     * @return result
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * <br>
     * <br>
     *            ax, ay, bx, by are arrays of vectors<br>
     *            result is the computed array
     */
    public static float[] dotvectors(float[] ax, float[] ay, float[] bx,
            float[] by, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.dotvectors(ax, 0, ay, 0, bx, 0, by, 0, result, 0, mni, 0,
                ni, 0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Calculates the rate of change of height versus
     * the log of pressure
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (C or K)
     * @param dewpoint
     *            (same as temperature)
     * @return Height_Rate_Of_Change <br>
     * <br>
     */
    public static float dzdlnp(float pressure, float temperature, float dewpoint) {
        float p[] = { pressure };
        float t[] = { temperature };
        float td[] = { dewpoint };
        return Meteolibrary.dzdlnp(p, 0, t, 0, td, 0);
    }

    /**
     * <b>Main Description</b> : Approximates the mixing ratio (g of water vapor
     * per kg of dry air) given pressure (mb) and the temperature (C)
     * 
     * @param temperature
     *            (C)
     * @param dewpoint
     *            (C)
     * @param pressure
     *            (mb)
     * @return Mixing_Ratio (g/kg) <br>
     * <br>
     */
    public static float ept(float temperature, float dewpoint, float pressure) {
        float t[] = { temperature };
        float td[] = { dewpoint };
        float p[] = { pressure };
        return Meteolibrary.ept(t, 0, td, 0, p, 0);
    }

    /**
     * <b>Main Description</b> : This routine computes the pressure, height, and
     * temperature of the equilibrium level, defined as the level where a
     * parcel, rising from the level of free convection, intersects the sounding
     * and becomes negatively buoyant.
     * 
     * @param pressure
     *            (mb)
     * @param height
     *            (m asl)
     * @param parcelTemp
     *            (K)
     * @param enviroTemp
     *            (K)
     * @param pressureLFC
     *            (mb)
     * @param potentialTemp
     *            (K)
     * @param numLevels
     * @return weather (object with
     *         equilibriumLevelPressure(mb),equilibriumLevelHeight(m
     *         asl),equilibriumLevelTemperature(K)) <br>
     * <br>
     *         equilibriumLevelPressure, equilibriumLevelHeight, and
     *         equilibriumLevelTemperature are the computed values
     */
    public static PHT eqlev(float[] pressure, float[] height,
            float[] parcelTemp, float[] enviroTemp, float pressureLFC,
            float potentialTemp, int numLevels) {
        PHT weather = new PHT();
        float equilibriumLevelPressure = 0;
        float equilibriumLevelHeight = 0;
        float equilibriumLevelTemperature = 0;
        float plfc[] = { pressureLFC };
        float eptpar[] = { potentialTemp };
        int npar[] = { numLevels };
        float peqlev[] = { equilibriumLevelPressure };
        float heqlev[] = { equilibriumLevelHeight };
        float teqlev[] = { equilibriumLevelTemperature };
        Meteolibrary
                .eqlev(pressure, 0, height, 0, parcelTemp, 0, enviroTemp, 0,
                        plfc, 0, eptpar, 0, npar, 0, peqlev, 0, heqlev, 0,
                        teqlev, 0);
        weather.setPressure(peqlev[0]);
        weather.setTemperature(teqlev[0]);
        weather.setHeight(heqlev[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates sounding temperature and dewpoint
     * data at pressure intervals given by pressureInterval
     * 
     * @param pressureInterval
     *            (mb)
     * @param pressure
     *            (mb)
     * @param height
     *            (m)
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param numLevels
     * @param pressureAtInterval
     *            (mb)
     * @param heightAtInterval
     *            (m)
     * @param temperatureAtInterval
     *            (K)
     * @param dewpointAtInterval
     *            (K)
     * @param newNumLevels
     * <br>
     * <br>
     *            heightAtInterval,temperatureAtInterval,dewpointAtInterval, and
     *            newNumLevels are computed arrays/values
     */
    public static PHT eqp(float pressureInterval, float[] pressure,
            float[] height, float[] temperature, float[] dewpoint,
            int numLevels, float[] pressureAtInterval,
            float[] heightAtInterval, float[] temperatureAtInterval,
            float[] dewpointAtInterval, int newNumLevels) {
        PHT weather = new PHT();

        float deltap[] = { pressureInterval };
        int n[] = { numLevels };
        int nn[] = { newNumLevels };
        Meteolibrary.eqp(deltap, 0, deltap, 0, height, 0, temperature, 0,
                dewpoint, 0, n, 0, pressureAtInterval, 0, heightAtInterval, 0,
                temperatureAtInterval, 0, dewpointAtInterval, 0, nn, 0);
        weather.setPressureArray(pressureAtInterval);
        weather.setTemperatureArray(temperatureAtInterval);
        weather.setDewpointArray(dewpointAtInterval);
        weather.setNumLevels(newNumLevels);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates saturation vapor pressure in
     * millibars as a function of either Kelvin of Celsius temperature.
     * 
     * @param temperature
     *            (K or C)
     * @return <br>
     * <br>
     *         Is based upon a variation of the integrated form of the
     *         Clausius-Clapeyron equation. Has an additional linear term in it
     *         and is fit to data in the Smithsonian Meterological Tables. Is
     *         accurate to one part in a thousand over the range from -25C to
     *         +35C. Its main advantage is that it is invertable. <br>
     * <br>
     */
    public static float esat(float temperature) {
        float[] temp = { temperature };
        return Meteolibrary.esat(temp, 0);
    }

    /**
     * <b>Main Description</b> : Calculates the exponential of a field
     * 
     * @param a
     * @return result
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * <br>
     * <br>
     *            result = exp(a) OR b = Math.exp(a)<br>
     *            result is computed array
     */
    public static float[] exp_aray(float[] a, int totalDimension,
            int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.exp_aray(a, 0, result, 0, mni, 0, ni, 0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : This routine computes the divergence of the
     * normal-to-isotherm component of the Q-vector for a specified layer in the
     * atmosphere.
     * 
     * @param zmid
     * @param topHeightGridPoint
     *            (m asl)
     * @param bottomHeightGridPoint
     *            (m asl)
     * @param topHeightPressureLevel
     *            (mb)
     * @param bottomHeightPressureLevel
     *            (mb)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param gridSpacingX
     *            (m)
     * @param gridSpacingY
     *            (m)
     * @param coriolisParam
     *            (/s)
     * @param fnx
     * @param fny
     * @param w1
     * @param dtdx
     * @param dtdy
     * @param qVectorXWorkArray
     * @param qVectorYWorkArray
     * @return fnVectorDivergence (K/m^2/s) <br>
     * <br>
     *         fnx, fny, w1, dtdx, dtdy, qVectorXWorkArray, qVectorYWorkArray
     *         are all work arrays<br>
     * <br>
     *         fnVectorDivergence is the computed array
     */
    public static float[] fndiverg(float[] zmid, float[] topHeightGridPoint,
            float[] bottomHeightGridPoint, float topHeightPressureLevel,
            float bottomHeightPressureLevel, int totalDimension,
            int IGridDimension, int JGridDimension, float[] gridSpacingX,
            float[] gridSpacingY, float[] coriolisParam, float[] fnx,
            float[] fny, float[] w1, float[] dtdx, float[] dtdy,
            float[] qVectorXWorkArray, float[] qVectorYWorkArray) {
        float[] fnVectorDivergence = new float[totalDimension];
        float ptop[] = { topHeightPressureLevel };
        float pbot[] = { bottomHeightPressureLevel };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.fndiverg(zmid, 0, topHeightGridPoint, 0,
                bottomHeightGridPoint, 0, ptop, 0, pbot, 0, mni, 0, ni, 0, nj,
                0, gridSpacingX, 0, gridSpacingY, 0, coriolisParam, 0, fnx, 0,
                fny, 0, w1, 0, dtdx, 0, dtdy, 0, qVectorXWorkArray, 0,
                qVectorYWorkArray, 0, fnVectorDivergence, 0);
        return fnVectorDivergence;
    }

    /**
     * <b>Main Description</b> : Forecast the maximum temperature for a sounding
     * station givent he 12Z sounding and the thermodynamic profile
     * 
     * @param year
     * @param month
     * @param day
     * @param hour
     * @param minute
     * @param ICAOIdentifier
     * @param snowDepth
     *            (in)
     * @param latitude
     *            (deg)
     * @param longitude
     *            (deg)
     * @param pressure
     *            (mb)
     * @param height
     *            (m)
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param numLevels
     * @return weather (object with forecastMaximum(K) status) <br>
     * <br>
     *         1) The maximum temperature in this model is computed based only
     *         on radiative heating/cooling therefore the technique may be
     *         expected perform poorly under the following conditions: a) over
     *         or near large bodies of water due to lack of heat storage b) over
     *         wet soil due to lack of evaportation c) when a different air mass
     *         is advected into the area during the time of radiative heating
     *         (i.e. between 12Z and mid afternoon) d) when an inversion is
     *         eroded away by mountain wave activity (dynamically) rather than
     *         by solar heating (thermodynamically).<br>
     * 
     *         forecastMaximum and status are the computed values
     */
    public static float forecast(int year, int month, int day, int hour,
            int minute, String ICAOIdentifier, int snowDepth, float latitude,
            float longitude, float[] pressure, float[] height,
            float[] temperature, float[] dewpoint) {
        int yr[] = { year };
        int mon[] = { month };
        int tday[] = { day };
        int thour[] = { hour };
        int min[] = { minute };
        byte icao[] = ICAOIdentifier.getBytes();
        int snow[] = { snowDepth };
        float slat[] = { latitude };
        float slon[] = { longitude };
        float p[] = pressure;
        float ht[] = height;
        float t[] = temperature;
        float td[] = dewpoint;
        int nlvls[] = { temperature.length };
        float ftmax[] = new float[1];
        int tstatus[] = new int[1];
        Meteolibrary.forecast(yr, 0, mon, 0, tday, 0, thour, 0, min, 0, icao,
                0, snow, 0, slat, 0, slon, 0, p, 0, ht, 0, t, 0, td, 0, nlvls,
                0, ftmax, 0, tstatus, 0);
        return ftmax[0];
    }

    /**
     * Assumes user sets up world coordinates from 1.0 to float(nx) and 1.0 to
     * float(ny).
     * 
     * @param contourArray
     *            Array to contour
     * @param workspace
     *            Workspace.
     * @param totalDimension
     *            First dimension of input arrays.
     * @param IGridDimension
     *            First (horizontal) dimension of input grid.
     * @param JGridDimension
     *            Second (vertical) dimension of input grid.
     * @param scale
     *            Contour grid as if it were multiplied by this ....
     * @param offset
     *            and then added to by this before contouring.
     * @param mode
     *            If mode<0 then -mode is approximate number of contours and
     *            seed is minimum contour increment. If mode=0 then seed is
     *            contour increment. If mode>0 seed is array of contour values
     *            to use, mode is number of contours.
     * @param contourControl
     *            Value(s) that control contouring (see mode).
     * @param xpoints
     *            x-coords in grid space of contours.
     * @param ypoints
     *            y-coords in grid space of contours.
     * @param numBufferPoints
     *            Number of points in the buffer.
     * @param badlow
     *            Smallest
     * @param badhigh
     *            and largest values which will be ignored upon contouring. If
     *            badlow>badhigh, then no missing value handling is done.
     * @return status Logically true if input was meaningful
     */
    public static int fortconbuf(float[] contourArray, int[] workspace,
            int totalDimension, int IGridDimension, int JGridDimension,
            float scale, float offset, int mode, float[] contourControl,
            float[] xpoints, float[] ypoints, int[] numBufferPoints,
            float badlow, float badhigh) {
        int status = 0;
        int mnx[] = { totalDimension };
        int nx[] = { IGridDimension };
        int ny[] = { JGridDimension };
        float tscale[] = { scale };
        float toffset[] = { offset };
        int tmode[] = { mode };
        float badlo[] = { badlow };
        float badhi[] = { badhigh };
        int tstatus[] = { status };
        Meteolibrary.fortconbuf(contourArray, 0, workspace, 0, mnx, 0, nx, 0,
                ny, 0, tscale, 0, toffset, 0, tmode, 0, contourControl, 0,
                xpoints, 0, ypoints, 0, numBufferPoints, 0, badlo, 0, badhi, 0,
                tstatus, 0);
        return status;
    }

    /**
     * <b>Main Description</b> : Calculates the QG frontogenesis function
     * througout a specified pressure layer
     * 
     * @param heightMid
     *            (m asl)
     * @param heightTop
     *            (m asl)
     * @param heightBottom
     *            (m asl)
     * @param topPressure
     *            (mb)
     * @param bottomPressure
     *            (mb)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param gridSpacingX
     *            (m)
     * @param gridSpacingY
     *            (m)
     * @param coriolisParam
     *            (/s)
     * @param w1
     * @param w2
     * @param w3
     * @param dtdx
     * @param dtdy
     * @param qVectorArrayX
     * @param qVectorArrayY
     * @return fgen <br>
     * <br>
     * 
     *         fgen is the compted array
     */
    public static float[] frontogen(float[] heightMid, float[] heightTop,
            float[] heightBottom, float topPressure, float bottomPressure,
            int totalDimension, int IGridDimension, int JGridDimension,
            float[] gridSpacingX, float[] gridSpacingY, float[] coriolisParam,
            float[] w1, float[] w2, float[] w3, float[] dtdx, float[] dtdy,
            float[] qVectorArrayX, float[] qVectorArrayY) {
        float[] fgen = new float[totalDimension];
        float ptop[] = { topPressure };
        float pbot[] = { bottomPressure };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.frontogen(heightMid, 0, heightTop, 0, heightBottom, 0,
                ptop, 0, pbot, 0, mni, 0, ni, 0, nj, 0, gridSpacingX, 0,
                gridSpacingY, 0, coriolisParam, 0, w1, 0, w2, 0, w3, 0, dtdx,
                0, dtdy, 0, qVectorArrayX, 0, qVectorArrayY, 0, fgen, 0);
        return fgen;
    }

    /**
     * <b>Main Description</b> : Calculates the pressure and height of the
     * freezing level in a sounding
     * 
     * @param elevation
     * @param pressure
     * @param height
     * @param temperature
     * @param numLevels
     * @return weather (object holding pressure and height) <br>
     * <br>
     *         pressureFrzLvl and heightFrzLvl are the computed values
     */
    public static PHT frzlev(float elevation, float[] pressure, float[] height,
            float[] temperature) {
        float pressureFrzLvl = 0;
        float heightFrzLvl = 0;
        float elev[] = { elevation };
        int hlvls[] = { temperature.length };
        float pfrz[] = { pressureFrzLvl };
        float hfrz[] = { heightFrzLvl };
        Meteolibrary.frzlev(elev, 0, pressure, 0, height, 0, temperature, 0,
                hlvls, 0, pfrz, 0, hfrz, 0);
        PHT weather = new PHT();
        weather.setPressure(pfrz[0]);
        weather.setHeight(hfrz[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates the divergence of the along-isotherm
     * component of the q-vector for a specified layer in the atmosphere
     * 
     * @param midHeight
     *            (m asl)
     * @param topHeight
     *            (m asl)
     * @param bottomHeight
     *            (m asl)
     * @param topPressure
     *            (mb)
     * @param bottomPressure
     *            (mb)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param gridSpacingX
     *            (m)
     * @param gridSpacingY
     *            (m)
     * @param coriolisParam
     *            (/s)
     * @param fsx
     * @param fsy
     * @param w1
     * @param dtdx
     * @param dtdy
     * @param qVectorArrayX
     * @param qVectorArrayY
     * @return fsVectorDiverg (K/m^2/s) <br>
     * <br>
     *         fsVectorDiverg is the computed array
     */
    public static float[] fsdiverg(float[] midHeight, float[] topHeight,
            float[] bottomHeight, float topPressure, float bottomPressure,
            int totalDimension, int IGridDimension, int JGridDimension,
            float[] gridSpacingX, float[] gridSpacingY, float[] coriolisParam,
            float[] fsx, float[] fsy, float[] w1, float[] dtdx, float[] dtdy,
            float[] qVectorArrayX, float[] qVectorArrayY) {
        float[] fsVectorDiverg = new float[totalDimension];
        float pbot[] = { bottomPressure };
        float ptop[] = { topPressure };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.fsdiverg(midHeight, 0, topHeight, 0, bottomHeight, 0,
                ptop, 0, pbot, 0, mni, 0, ni, 0, nj, 0, gridSpacingX, 0,
                gridSpacingY, 0, coriolisParam, 0, fsx, 0, fsy, 0, w1, 0, dtdx,
                0, dtdy, 0, qVectorArrayX, 0, qVectorArrayY, 0, fsVectorDiverg,
                0);
        return fsVectorDiverg;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param uComp
     * @param vComp
     * @param arrayParam
     * @param gridSpacingX
     * @param gridSpacingY
     * @param totalDimensionX
     * @param totalDimensionY
     * @param IGridDimension
     * @param JGridDimension
     * @param choice
     * @param scalar
     * <br>
     * <br>
     *            choice=1, vorticity;<br>
     *            choice=2, divergence, ignore Q;<br>
     *            choice=3, vorticity advection;<br>
     *            choice=4, divergence of Q;<br>
     *            choice=5, advection of Q.<br>
     *            choice=6, laplacian of Q, ignore U and V.<br>
     *            choice=7, Scalar is input, ignore Q, work back to d/dx in U
     *            and d/dy in V.<br>
     *            choice=8, Scalar is height, Q is coriolis, work back to Ug and
     *            Vg.<br>
     *            choice=9, Total deformation into scalar.<br>
     *            choice=10, Deformation components, Scalar is x comp and Q is y
     *            comp.<br>
     *            choice=11, Scalar is height, Q is coriolis, geo def in Udx.<br>
     *            choice=12, Scalar is height, Q is coriolis, work back to geo
     *            def vectors.<br>
     *            choice=13, Scalar is height, Q is coriolis, input wind
     *            components changed to Uag and Vag. <br>
     *            choice=14, Scalar is input, U and V is ridge/trough vector.<br>
     *            choice=15, Scalar is input, U and V is ridge vector.<br>
     *            choice=16, Scalar is input, U and V is trough vector.<br>
     *            choice=17, U and V is input, Scalar is vector continuity.<br>
     * <br>
     *            Note for options one and three:<br>
     *            To work with relative vorticity, fill first level of array Q
     *            with zeroes. For absolute vorticity, fill first level of Q
     *            with coriolis parameter values.
     */
    public static void g2gkinematics(float[] uComp, float[] vComp,
            float[] arrayParam, float[] gridSpacingX, float[] gridSpacingY,
            int totalDimensionX, int totalDimensionY, int IGridDimension,
            int JGridDimension, int choice, float[] scalar) {
        int mnx[] = { totalDimensionX };
        int mny[] = { totalDimensionY };
        int nx[] = { IGridDimension };
        int ny[] = { JGridDimension };
        int tchoice[] = { choice };
        Meteolibrary.g2gkinematics(uComp, 0, vComp, 0, arrayParam, 0,
                gridSpacingX, 0, gridSpacingY, 0, mnx, 0, mny, 0, nx, 0, ny, 0,
                tchoice, 0, scalar, 0);
        return;
    }

    /**
     * <b>Main Description</b> : Calculates the convective gust potential
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     * @param dewpoint
     * @param numPoints
     * @return gustPotential <br>
     * <br>
     */
    public static int gusts(float[] pressure, float[] temperature,
            float[] dewpoint) {
        int np[] = { temperature.length };
        int gstpot[] = new int[1];
        Meteolibrary.gusts(pressure, 0, temperature, 0, dewpoint, 0, np, 0,
                gstpot, 0);
        return gstpot[0];
    }

    /**
     * <b>Main Description</b> : Calculates the largest hailstone that can be
     * supported by the undiluted parcel updraft
     * 
     * @param vertVelocity
     * @return maxSize (cm) <br>
     * <br>
     */
    public static float hailsiz(float vertVelocity) {
        float vvmax[] = { vertVelocity };
        float hsize[] = new float[1];
        Meteolibrary.hailsiz(vvmax, 0, hsize, 0);
        return hsize[0];
    }

    /**
     * <b>Main Description</b> : Calculates the pressure given the height based
     * on a standard atmosphere
     * 
     * @param height
     *            (m)
     * @param totalDimension
     * @return pressure (mb) <br>
     * <br>
     */
    public static float hgt2pres(float height) {
        float[] input = new float[] { height };
        float[] pressure = new float[1];
        int mni[] = { 1 };
        int ni[] = { 1 };
        int nj[] = { 1 };
        Meteolibrary.hgt2pres(input, 0, pressure, 0, mni, 0, ni, 0, nj, 0);
        return pressure[0];
    }

    /**
     * <b>Main Description</b> : Does interpolation on given inputs
     * 
     * @param pressure1
     *            (mb)
     * @param pressure2
     *            (mb)
     * @param temp1
     *            (K)
     * @param temp2
     *            (K)
     * @param dewpoint1
     *            (K)
     * @param dewpoint2
     *            (K)
     * @param levelP
     * @param interTemp
     *            (K)
     * @param interDewpoint
     *            (K)
     * 
     */
    public static PHT interp(float pressure1, float pressure2, float temp1,
            float temp2, float dewpoint1, float dewpoint2, float levelP,
            float interTemp, float interDewpoint) {
        PHT inter = new PHT();
        float interT[] = { interTemp };
        float interTD[] = { interDewpoint };
        Meteolibrary.interp(pressure1, pressure2, temp1, temp2, dewpoint1,
                dewpoint2, levelP, interT, 0, interTD, 0);
        inter.setTemperature(interT[0]);
        inter.setDewpoint(interTD[0]);
        return inter;
    }

    /**
     * <b>Main Description</b> : Interpolates y2 given y1 at x1 and y3 at x3
     * 
     * @param y1
     * @param y3
     * @param x1
     * @param x2
     * @param x3
     * @return y2 (using the interp1 function)
     * 
     */
    public static float interp1(float y1, float y3, float x1, float x2, float x3) {
        float y2 = 0;
        float arg0[] = { y1 };
        float arg1[] = { y3 };
        float arg2[] = { x1 };
        float arg3[] = { x2 };
        float arg4[] = { x3 };
        y2 = Meteolibrary.interp1(arg0, 0, arg1, 0, arg2, 0, arg3, 0, arg4, 0);
        return y2;
    }

    /**
     * <b>Main Description</b> : Interplates data between the level of free
     * convection and the equilibrium level to a finer resolution for the
     * compuation of positive energy
     * 
     * @param thickness
     * @param height
     *            (m asl)
     * @param pressure
     *            (mb)
     * @param temperature
     *            (C)
     * @param numLevels
     * 
     */
    public static PHT intpos(float thickness, float[] height, float[] pressure,
            float[] temperature, int numLevels) {
        PHT weather = new PHT();
        float vdif[] = { thickness };
        int nlvls[] = { numLevels };
        Meteolibrary.intpos(vdif, 0, height, 0, pressure, 0, temperature, 0,
                nlvls, 0);
        weather.setHeightArray(height);
        weather.setPressureArray(pressure);
        weather.setTemperatureArray(temperature);
        weather.setNumLevels(nlvls[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param tlo
     * @param pzlo
     * @param thi
     * @param pzhi
     * @param tvc
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return lapse
     * 
     */
    public static float[] lapserate(float[] tlo, float[] pzlo, float[] thi,
            float[] pzhi, int tvc, int totalDimension, int IGridDimension,
            int JGridDimension, float[] lapse) {
        int vc[] = { tvc };
        int mnx[] = { totalDimension };
        int nx[] = { IGridDimension };
        int ny[] = { JGridDimension };
        Meteolibrary.lapserate(tlo, 0, pzlo, 0, thi, 0, pzhi, 0, vc, 0, mnx, 0,
                nx, 0, ny, 0, lapse, 0);
        return lapse;
    }

    /**
     * <b>Main Description</b> : Calculates the pressure, height, and
     * temperature of the lifting condensation level (LCL) from a sounding
     * 
     * @param mixRatio
     *            (g/kg)
     * @param surfaceTemp
     *            (K)
     * @param pressure
     *            (mb)
     * @param height
     *            (m asl)
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param numLevels
     * @return weather (object with
     *         liftingCondensationPressure(mb),liftingCondensationTemperature
     *         (K),liftingCondensationHeight(m asl)) <br>
     * <br>
     *         liftingCondensationPressure, liftingCondensationTemperature, and
     *         liftingCondensationHeight are the computed arrays
     */
    public static PHT lclpar(float mixRatio, float surfaceTemp,
            float[] pressure, float[] height, float[] temperature,
            float[] dewpoint) {

        PHT weather = new PHT();
        float mix[] = { mixRatio };
        float ts[] = { surfaceTemp };
        float plcl[] = new float[1];
        float tlcl[] = new float[1];
        float hlcl[] = new float[1];
        int nlvls[] = { temperature.length };
        Meteolibrary.lclpar(mix, 0, ts, 0, pressure, 0, height, 0, temperature,
                0, dewpoint, 0, nlvls, 0, plcl, 0, tlcl, 0, hlcl, 0);
        weather.setPressure(plcl[0]);
        weather.setTemperature(tlcl[0]);
        weather.setHeight(hlcl[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates the level of free convection of a
     * rising parcel.
     * 
     * @param moistAdiabat
     *            (K)
     * @param LCLPressure
     *            (mb)
     * @param LCLTemperature
     *            (K)
     * @param LCLHeight
     *            (m asl)
     * @param parcelTemp
     *            (K)
     * @param soundingTemp
     *            (K)
     * @param liftedParcelPressure
     *            (mb)
     * @param liftedParcelHeight
     *            (m asl)
     * @param numLevels
     * @return weather(object with
     *         levelFreeConvectionPressure1(mb),levelFreeConvectionHeight1(m
     *         asl)
     *         ,levelFreeConvectionTemp1(K),levelFreeConvectionPressure2(mb),
     *         levelFreeConvectionHeight2(m asl),levelFreeConvectionTemp2(K)) <br>
     * <br>
     */
    public static PHT lfcpar(float moistAdiabat, float LCLPressure,
            float LCLTemperature, float LCLHeight, float[] parcelTemp,
            float[] soundingTemp, float[] liftedParcelPressure,
            float[] liftedParcelHeight, int numParcelLevels) {
        float levelFreeConvectionPressure1 = 0;
        float levelFreeConvectionHeight1 = 0;
        float levelFreeConvectionTemp1 = 0;
        float levelFreeConvectionPressure2 = 0;
        float levelFreeConvectionHeight2 = 0;
        float levelFreeConvectionTemp2 = 0;
        PHT weather = new PHT();
        float eptpar[] = { moistAdiabat };
        float pcb[] = { LCLPressure };
        float tcb[] = { LCLTemperature };
        float hcb[] = { LCLHeight };
        int npar[] = { numParcelLevels };
        float plfc1[] = { levelFreeConvectionPressure1 };
        float hlfc1[] = { levelFreeConvectionHeight1 };
        float tlfc1[] = { levelFreeConvectionTemp1 };
        float plfc2[] = { levelFreeConvectionPressure2 };
        float hlfc2[] = { levelFreeConvectionHeight2 };
        float tlfc2[] = { levelFreeConvectionTemp2 };
        Meteolibrary.lfcpar(eptpar, 0, pcb, 0, tcb, 0, hcb, 0, parcelTemp, 0,
                soundingTemp, 0, liftedParcelPressure, 0, liftedParcelHeight,
                0, npar, 0, plfc1, 0, hlfc1, 0, tlfc1, 0, plfc2, 0, hlfc2, 0,
                tlfc2, 0);
        weather.setPressure(plfc1[0]);
        weather.setPressure1(plfc2[0]);
        weather.setTemperature(tlfc1[0]);
        weather.setTemperature1(tlfc2[0]);
        weather.setHeight(hlfc1[0]);
        weather.setHeight1(hlfc2[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculate lifted parcel arrays and
     * corresponding environmental arrays such that an exact one-to-one
     * correspondance exists between lifted parcel parameters and envrironmental
     * parameters.
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param height
     *            (m asl)
     * @param virtualTemp
     *            (K)
     * @param numSoundingLevels
     * @param numParcelLevels
     * @param LCLPressure
     *            (mb)
     * @param LCLHeight
     *            (m asl)
     * @param LCLTemperature
     *            (K)
     * @param LCLMixRatio
     *            (g/kg)
     * @param LCLPotentialTemp
     *            (K)
     * @param LCLEquivTemp
     *            (K)
     * @param InitialParcelPressure
     *            (mb)
     * @param initialParcelTemp
     *            (K)<br>
     * <br>
     */
    public static PHT liftedp(float[] pressure, float[] temperature,
            float[] height, float[] virtualTemp, int numParcelLvls,
            float LCLPressure, float LCLHeight, float LCLTemperature,
            float LCLMixRatio, float LCLPotentialTemp, float LCLEquivTemp,
            float InitialParcelPressure, float initialParcelTemp) {
        float[] parcelPress = new float[numParcelLvls];
        float[] parcelHeights = new float[numParcelLvls];
        float[] parcelTemp = new float[numParcelLvls];
        float[] virtualTemps = new float[numParcelLvls];
        float[] soundingTemps = new float[numParcelLvls];
        float[] soundingVirtTemps = new float[numParcelLvls];
        PHT weather = new PHT();
        int nlvls[] = { virtualTemp.length };
        int npar[] = { numParcelLvls };
        float pcb[] = { LCLPressure };
        float hcb[] = { LCLHeight };
        float tcb[] = { LCLTemperature };
        float wcb[] = { LCLMixRatio };
        float thdpar[] = { LCLPotentialTemp };
        float eptpar[] = { LCLEquivTemp };
        float pl[] = { InitialParcelPressure };
        float tl[] = { initialParcelTemp };
        int nparcel[] = new int[1];
        Meteolibrary.liftedp(pressure, 0, temperature, 0, height, 0,
                virtualTemp, 0, nlvls, 0, npar, 0, pcb, 0, hcb, 0, tcb, 0, wcb,
                0, thdpar, 0, eptpar, 0, pl, 0, tl, 0, parcelPress, 0,
                parcelHeights, 0, parcelTemp, 0, virtualTemps, 0,
                soundingTemps, 0, soundingVirtTemps, 0, nparcel, 0);
        weather.setPressureArray(parcelPress);
        weather.setHeightArray(parcelHeights);
        weather.setTemperatureArray(parcelTemp);
        weather.setNumLevels(nparcel[0]);
        weather.setVirtualTemps(virtualTemps);
        weather.setSoundingTemps(soundingTemps);
        weather.setSoundingVirtTemps(soundingVirtTemps);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates the linear translation on an array.
     * Each i,j in the array is multiplied by 'mult' and then added to by 'add'
     * 
     * @param array
     * @param mult
     * @param add
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return result <br>
     * <br>
     */
    public static float[] lintrans(float[] array, float mult, float add,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        float tmult[] = { mult };
        float tadd[] = { add };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.lintrans(array, 0, tmult, 0, tadd, 0, result, 0, mni, 0,
                ni, 0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param array
     * @param yvector
     * @param work
     * @param soln
     * @param sizeOfArrays
     * @param sizeOfSystem
     * @param status
     */
    public static VectorVars matsln(float[] array, float[] yvector, int[] work,
            int sizeOfArrays, int sizeOfSystem) {
        VectorVars var = new VectorVars();
        int status = 0;
        float soln[] = new float[sizeOfSystem];
        int n[] = { sizeOfSystem };
        int mn[] = { sizeOfArrays };
        int ok[] = { status };
        Meteolibrary.matsln(array, 0, yvector, 0, work, 0, soln, 0, mn, 0, n,
                0, ok, 0);
        var.setSolutionVector(soln);
        var.setStatus(ok[0]);
        return var;
    }

    /**
     * <b>Main Description</b> : Finds the max or min of each i,j in the arrays
     * 
     * @param arrayOne
     * @param arrayTwo
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param tmode
     * @return <br>
     * <br>
     *         mode > 0 means max<br>
     *         mode < 0 means min
     */
    public static float[] max_min(float[] arrayOne, float[] arrayTwo,
            int totalDimension, int IGridDimension, int JGridDimension,
            int tmode) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        int mode[] = { tmode };
        Meteolibrary.max_min(arrayOne, 0, arrayTwo, 0, result, 0, mni, 0, ni,
                0, nj, 0, mode, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Calculates the mixing ratio from the pressure,
     * temperature, and relative humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return mixRatio (g/kg) <br>
     * <br>
     */
    public static float[] mixrat(float[] pressure, float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] mixRatio = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.mixrat(pressure, 0, temperature, 0, relativeHumidity, 0,
                mni, 0, ni, 0, nj, 0, mixRatio, 0);
        return mixRatio;
    }

    /**
     * <b>Main Description</b> : Estimates 1000 mb to 500 mb thickness from 500
     * height and mean sea level pressure
     * 
     * @param meanSeaLevelPressure
     * @param height
     *            (m)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return thickness (mb)
     */
    public static float[] mslp2thkns(float[] meanSeaLevelPressure,
            float[] height, float[] thickness, int totalDimension,
            int IGridDimension, int JGridDimension) {
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.mslp2thkns(meanSeaLevelPressure, 0, height, 0, thickness,
                0, mni, 0, ni, 0, nj, 0);
        return thickness;
    }

    /**
     * <b>Main Description</b> : Multiply a field by another. Each i,j in one
     * array is multiplied by the corresponding i,j in the other array
     * 
     * @param arrayOne
     * @param arrayTwo
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return result <br>
     * <br>
     *         result = a*b
     */
    public static float[] mult_aray(float[] arrayOne, float[] arrayTwo,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.mult_aray(arrayOne, 0, arrayTwo, 0, result, 0, mni, 0, ni,
                0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Multiplies each value in the array by a real
     * constant. Each i,j in the array is multiplied by the constant
     * 
     * @param array
     * @param cnst
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return result <br>
     * <br>
     */
    public static float[] mult_by_cnst(float[] array, float cnst,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        float tcnst[] = { cnst };
        Meteolibrary.mult_by_cnst(array, 0, tcnst, 0, result, 0, mni, 0, ni, 0,
                nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Forecasts the maximum temperature based on the
     * total amount of energy available for heating and sounding temperature
     * profile
     * 
     * @param totalHeat
     *            (*)
     * @param pressureIncrement
     * @param surfacePressure
     *            (mb)
     * @param pressure2
     *            (mb)
     * @param interpolatedTemperatureArray
     *            (C)
     * @param thickness
     * @param level
     * @return forecastMaxTemp (C)
     * 
     */
    public static float mxtp(float totalHeat, float pressureIncrement,
            float surfacePressure, float pressure2,
            float[] interpolatedTemperatureArray, float[] thickness, int level) {
        float forecastMaxTemp = 0.0f;
        float ansol[] = { totalHeat };
        float deltap[] = { pressureIncrement };
        float sfcp[] = { surfacePressure };
        float p2[] = { pressure2 };
        int lvl[] = { level };
        float ctmax[] = { forecastMaxTemp };
        Meteolibrary
                .mxtp(ansol, 0, deltap, 0, sfcp, 0, p2, 0,
                        interpolatedTemperatureArray, 0, thickness, 0, lvl, 0,
                        ctmax, 0);
        return forecastMaxTemp;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param temperature
     * @param dewpoint
     * @param pressure
     * @return mytw
     * 
     */
    public static float mytw(float temperature, float dewpoint, float pressure) {
        float t[] = { temperature };
        float td[] = { dewpoint };
        float p[] = { pressure };
        return Meteolibrary.mytw(t, 0, td, 0, p, 0);
    }

    /**
     * <b>Main Description</b> : Calculates the non-advective local change of an
     * arbitrary conservative parameter
     * 
     * @param uComp
     * @param vComp
     * @param arbConserveParam
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param gridSpacingX
     * @param gridSpacingY
     * @return var (VectorVars object with dadxdt and dadydt) <br>
     * <br>
     *         dadxdt and dadydt are computed arrays
     */
    public static VectorVars nadgdt(float[] uComp, float[] vComp,
            float[] arbConserveParam, int totalDimension, int IGridDimension,
            int JGridDimension, float[] gridSpacingX, float[] gridSpacingY) {
        VectorVars var = new VectorVars();
        float[] dadxdt = new float[totalDimension];
        float[] dadydt = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.nadgdt(uComp, 0, vComp, 0, arbConserveParam, 0, mni, 0,
                ni, 0, nj, 0, gridSpacingX, 0, gridSpacingY, 0, dadxdt, 0,
                dadydt, 0);
        var.setDadxdt(dadxdt);
        var.setDadydt(dadydt);
        return var;
    }

    /**
     * <b>Main Description</b> : Calculates the natural log of a field.
     * 
     * @param a
     * @return b
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * <br>
     * <br>
     *            b = ln(a)
     */
    public static float[] natlog(float[] a, int totalDimension,
            int IGridDimension, int JGridDimension) {
        float[] b = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.natlog(a, 0, b, 0, mni, 0, ni, 0, nj, 0);
        return b;
    }

    /**
     * <b>Main Description</b> : Calculates the negative buoyant energy between
     * the surface and level of free convection in a sounding
     * 
     * @param pcb
     * @param tcb
     * @param hcb
     * @param pressureLFC
     *            (mb)
     * @param heightLFC
     *            (m asl)
     * @param tempLFC
     *            (C)
     * @param thdpar
     * @param equivalentPotentialTemp
     *            (C)
     * @param pressure
     *            (mb)
     * @param height
     *            (m asl)
     * @param soundingTemp
     *            (C)
     * @param parcelTemp
     *            (C)
     * @param npar
     * @param cinfrmcap
     * @return negbuoy[0]
     * 
     */
    public static float negarea(float pcb, float tcb, float hcb,
            float pressureLFC, float heightLFC, float tempLFC, float thdpar,
            float equivalentPotentialTemp, float[] pressure, float[] height,
            float[] soundingTemp, float[] parcelTemp, int npar, float cinfrmcap) {
        float negativeEnergy = 0;
        float tpcb[] = { pcb };
        float ttcb[] = { tcb };
        float thcb[] = { hcb };
        float plfc[] = { pressureLFC };
        float hlfc[] = { heightLFC };
        float tlfc[] = { tempLFC };
        float tthdpar[] = { thdpar };
        float eptpar[] = { equivalentPotentialTemp };
        int tnpar[] = { npar };
        float tcinfrmcap[] = { cinfrmcap };
        float negbuoy[] = { negativeEnergy };
        Meteolibrary.negarea(tpcb, 0, ttcb, 0, thcb, 0, plfc, 0, hlfc, 0, tlfc,
                0, tthdpar, 0, eptpar, 0, pressure, 0, height, 0, soundingTemp,
                0, parcelTemp, 0, tnpar, 0, tcinfrmcap, 0, negbuoy, 0);
        return negbuoy[0];
    }

    /**
     * <b>Main Description</b> : Calculates the positive buoyant energy between
     * the level of free convection and equilibrium level in a sounding
     * 
     * @param pressureLevelFreeConvection
     *            (mb)
     * @param equilibriumLevel
     *            (mb)
     * @param tempLFC
     *            (C)
     * @param tempEL
     *            (C)
     * @param heightLFC
     *            (m asl)
     * @param heightEL
     *            (m asl)
     * @param equivalentPotentialTemp
     *            (C)
     * @param pressure
     *            (mb)
     * @param height
     *            (m asl)
     * @param soundingTemp
     *            (C)
     * @param parcelTemp
     *            (C)
     * @param numLevels
     * @return weather(PHT object with positiveBuoyEnergy(J/kg) and cin)
     */
    public static PHT posarea(float pressureLevelFreeConvection,
            float equilibriumLevel, float tempLFC, float tempEL,
            float heightLFC, float heightEL, float equivalentPotentialTemp,
            float[] pressure, float[] height, float[] soundingTemp,
            float[] parcelTemp, int numLevels) {
        PHT weather = new PHT();
        float positiveBuoyEnergy = 0;
        float cin = 0;
        float plfc[] = { pressureLevelFreeConvection };
        float peqlev[] = { equilibriumLevel };
        float tlfc[] = { tempLFC };
        float teqlev[] = { tempEL };
        float hlfc[] = { heightLFC };
        float heqlev[] = { heightEL };
        float eptpar[] = { equivalentPotentialTemp };
        int npar[] = { numLevels };
        float buoy[] = { positiveBuoyEnergy };
        float tcin[] = { cin };
        Meteolibrary.posarea(plfc, 0, peqlev, 0, tlfc, 0, teqlev, 0, hlfc, 0,
                heqlev, 0, eptpar, 0, pressure, 0, height, 0, soundingTemp, 0,
                parcelTemp, 0, npar, 0, buoy, 0, tcin, 0);
        weather.setPositiveEnergy(buoy[0]);
        weather.setCin(tcin[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates the potential temperature
     * 
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param pressure
     *            (mb)
     * @param mixRatioIce_Water
     * @return Potential Temperature (K) <br>
     * <br>
     */
    public static float pottemp(float temperature, float dewpoint,
            float pressure, int mixRatioIce_Water) {
        float temp[] = { temperature };
        float dwpt[] = { dewpoint };
        float pres[] = { pressure };
        int iw[] = { mixRatioIce_Water };
        return Meteolibrary.pottemp(temp, 0, dwpt, 0, pres, 0, iw, 0);
    }

    /**
     * <b>Main Description</b> : Raises each item in the field a to the power in
     * field b
     * 
     * @param a
     * @param b
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return result <br>
     * <br>
     *         result = a^b
     */
    public static float[] powercalc(float[] a, float[] b, int totalDimension,
            int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.powercalc(a, 0, b, 0, result, 0, mni, 0, ni, 0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Calculates the temperature along a pseudo-moist
     * adiabat by solving, as an initial value problem, the differential
     * equation describing the pseudo-moist adiabat.
     * 
     * @param numPoints
     * @param initialPressure
     *            (mb)
     * @param finalPressure
     *            (mb)
     * @return result <br>
     * <br>
     *         Initial temperature at result[0]
     */
    public static float[] pseudolift(int numPoints, float initialPressure,
            float finalPressure) {
        float[] result = new float[numPoints];
        int n[] = { numPoints };
        float pstart[] = { initialPressure };
        float pfinish[] = { finalPressure };
        Meteolibrary.pseudolift(n, 0, pstart, 0, pfinish, 0, result, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Calculates the height in a standard atmosphere
     * from the pressure in millibars
     * 
     * @param pressure
     *            (mb) <br>
     * <br>
     */
    public static float ptozsa(float pressure) {
        float p[] = { pressure };
        return Meteolibrary.ptozsa(p, 0);
    }

    /**
     * <b>Main Description</b> : Calculate the value of some parameter in a
     * sounding at a given pressure level using log pressure interpolation
     * 
     * @param desiredPressure
     * @param soundingPressure
     * @param numLevels
     * @param param
     * @return <br>
     * <br>
     */
    public static float pvalue(float desiredPressure, float[] soundingPressure,
            int numLevels, float[] param) {
        float tvalue = 0;
        float pres[] = { desiredPressure };
        int np[] = { numLevels };
        float value[] = { tvalue };
        Meteolibrary.pvalue(pres, 0, soundingPressure, 0, np, 0, param, 0,
                value, 0);
        return value[0];
    }

    /**
     * <b>Main Description</b> : Calculates the isobaric potential vorticity
     * through a layer
     * 
     * @param upperTheta
     *            (K)
     * @param lowerTheta
     *            (K)
     * @param upperPressure
     *            (mb)
     * @param lowerPressure
     *            (mb)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param uUpper
     *            (m/s)
     * @param vUpper
     *            (m/s)
     * @param uLower
     *            (m/s)
     * @param vLower
     *            (m/s)
     * @param avort1
     * @param avort2
     * @param dtdx1
     * @param dtdy1
     * @param dtdx2
     * @param dtdy2
     * @param dx
     * @param dy
     * @param coriolis
     *            (/s)
     * @return <br>
     * <br>
     */
    public static float[] pvpres(float[] upperTheta, float[] lowerTheta,
            float[] upperPressure, float[] lowerPressure, int totalDimension,
            int IGridDimension, int JGridDimension, float[] uUpper,
            float[] vUpper, float[] uLower, float[] vLower, float[] avort1,
            float[] avort2, float[] dtdx1, float[] dtdy1, float[] dtdx2,
            float[] dtdy2, float[] dx, float[] dy, float[] coriolis) {
        float[] pvort = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.pvpres(upperTheta, 0, lowerTheta, 0, upperPressure, 0,
                lowerPressure, 0, pvort, 0, mni, 0, ni, 0, nj, 0, uUpper, 0,
                vUpper, 0, uLower, 0, vLower, 0, avort1, 0, avort2, 0, dtdx1,
                0, dtdy1, 0, dtdx2, 0, dtdy2, 0, dx, 0, dy, 0, coriolis, 0);
        return pvort;
    }

    /**
     * <b>Main Description</b> : Calculates the q-vector components at a
     * specified pressure level
     * 
     * @param midHeight
     * @param topHeight
     * @param bottomHeight
     * @param topHeightPressure
     * @param bottomHeightPressure
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param dx
     * @param dy
     * @param f
     * @param dugdx
     * @param dvgdx
     * @param dugdy
     * @param dvgdy
     * @param dtdx
     * @param dtdy
     * @param qx
     * @param qy
     * <br>
     * <br>
     */
    public static VectorVars qvector(float[] midHeight, float[] topHeight,
            float[] bottomHeight, float topHeightPressure,
            float bottomHeightPressure, int totalDimension, int IGridDimension,
            int JGridDimension, float[] dx, float[] dy, float[] f,
            float[] dugdx, float[] dvgdx, float[] dugdy, float[] dvgdy,
            float[] dtdx, float[] dtdy, float[] qx, float[] qy) {
        VectorVars v = new VectorVars();
        float ptop[] = { topHeightPressure };
        float pbot[] = { bottomHeightPressure };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.qvector(midHeight, 0, topHeight, 0, bottomHeight, 0, ptop,
                0, pbot, 0, mni, 0, ni, 0, nj, 0, dx, 0, dy, 0, f, 0, dugdx, 0,
                dvgdx, 0, dugdy, 0, dvgdy, 0, dtdx, 0, dtdy, 0, qx, 0, qy, 0);
        v.setQx(qx);
        v.setQy(qy);
        return v;
    }

    /**
     * <b>Main Description</b> : Calculate the mean solar radiation at ground
     * 
     * @param latitude
     * @param longitude
     * @param longitudeStandardMeridian
     * @param julianDay
     * @param localTime
     * @param rayleightCoeff
     * @param opticalDepth
     * @return <br>
     * <br>
     */
    public static float radiation(float latitude, float longitude,
            float longitudeStandardMeridian, int julianDay, float localTime,
            float rayleightCoeff, float opticalDepth) {
        float tsolrad = 0;
        float lat[] = { latitude };
        float lng[] = { longitude };
        float lsm[] = { longitudeStandardMeridian };
        int jd[] = { julianDay };
        float hr[] = { localTime };
        float bext[] = { rayleightCoeff };
        float od[] = { opticalDepth };
        float solrad[] = { tsolrad };
        Meteolibrary.radiation(lat, 0, lng, 0, lsm, 0, jd, 0, hr, 0, bext, 0,
                od, 0, solrad, 0);
        return solrad[0];
    }

    /**
     * <b>Main Description</b> : Calculates the range of values ina 2D data
     * grid, missing data flags not being included
     * 
     * @param data
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return var (VectorVars object with mindata and maxdata) <br>
     * <br>
     */
    public static VectorVars rang2d(float[] data, int totalDimension,
            int IGridDimension, int JGridDimension) {
        VectorVars var = new VectorVars();
        float tmindata = 0;
        float tmaxdata = 0;
        float range = 0;
        int mnx[] = { totalDimension };
        int nx[] = { IGridDimension };
        int ny[] = { JGridDimension };
        float mindata[] = { tmindata };
        float maxdata[] = { tmaxdata };
        range = Meteolibrary.rang2d(data, 0, mnx, 0, nx, 0, ny, 0, mindata, 0,
                maxdata, 0);
        var.setMinimum(mindata[0]);
        var.setMaximum(maxdata[0]);
        var.setRange(range);
        return var;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param a
     * @param ttsttyp
     * @param lo
     * @param hi
     * @param repl
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return
     */
    public static float[] replinrange(float[] a, int ttsttyp, float[] lo,
            float[] hi, float[] repl, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] result = new float[totalDimension];
        int tsttyp[] = { ttsttyp };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.replinrange(a, 0, tsttyp, 0, lo, 0, hi, 0, repl, 0,
                result, 0, mni, 0, ni, 0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param endlvl
     * @param tnclvr
     * @param tsfcp
     * @param p
     * @param tl
     * @param tdl
     * @return
     */
    public static int[] rhbar(float[] endlvl, int tnclvr, float tsfcp,
            float[] p, float[] tl, float[] tdl) {
        int[] mrh = new int[endlvl.length];
        int nclvr[] = { tnclvr };
        float sfcp[] = { tsfcp };
        Meteolibrary.rhbar(endlvl, 0, mrh, 0, nclvr, 0, sfcp, 0, p, 0, tl, 0,
                tdl, 0);
        return mrh;
    }

    /**
     * 
     * <b>Main Description</b> : Calculate the dimensionless bulk Richardson
     * number as defined by Weisman and Klemp (1982)
     * 
     * @param height
     *            (m asl)
     * @param heightWindReport
     *            (m asl)
     * @param uComp
     *            (m/s)
     * @param vComp
     *            (m/s)
     * @param airDensity
     *            (kg/m^3)
     * @param numLevels
     * @param numWindLevels
     * @param buoyantEnergy
     *            (J/kg)
     * @return <br>
     * <br>
     *         Computes the bulk Richardson number
     */
    public static float richno(float[] height, float[] heightWindReport,
            float[] uComp, float[] vComp, float[] airDensity, int numLvls,
            int numWindLvls, float buoyantEnergy) {
        int nlvls[] = { numLvls };
        int nw[] = { numWindLvls };
        float buoy[] = { buoyantEnergy };
        float richnum[] = new float[1];
        Meteolibrary.richno(height, 0, heightWindReport, 0, uComp, 0, vComp, 0,
                airDensity, 0, nlvls, 0, nw, 0, buoy, 0, richnum, 0);
        return richnum[0];
    }

    /**
     * <b>Main Description</b> : Rotates a field of vectors "ax", outputing the
     * field of vectors "b". An angle of +90 is the same as doing a "k cross"
     * operation.
     * 
     * @param ax
     * @param ay
     * @param angle
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return var(VectorVars object with bx and by)
     */
    public static VectorVars rotvectors(float[] ax, float[] ay, float angle,
            int totalDimension, int IGridDimension, int JGridDimension) {
        VectorVars var = new VectorVars();
        float[] bx = new float[totalDimension];
        float[] by = new float[totalDimension];
        float tangle[] = { angle };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.rotvectors(ax, 0, ay, 0, tangle, 0, bx, 0, by, 0, mni, 0,
                ni, 0, nj, 0);
        var.setBx(bx);
        var.setBy(by);
        return var;
    }

    /**
     * <b>Main Description</b> : performs an objective analysis of point data to
     * create a grid
     * 
     * NOTE: this routine is not thread safe since it contains a large amount of
     * static data
     * 
     * @param xind
     *            x location of input values in grid cells
     * @param yind
     *            y location of input values in grid cells
     * @param values
     *            input values
     * @param nv
     *            number of input values
     * @param nx
     *            x dimension of output grid
     * @param ny
     *            y dimension of output grid
     * @param grid
     *            output grid
     * @return
     */
    public static synchronized int scaleless_analysis(float[] xind,
            float[] yind, float[] values, int nv, int nx, int ny, float[] grid) {
        // validate parameters to avoid overindexing arrays
        if (xind.length < nv || yind.length < nv || values.length < nv) {
            throw new IndexOutOfBoundsException(
                    "xind, yind, and values must contain at least tnv entries");
        }

        if (nx * ny > grid.length) {
            throw new IndexOutOfBoundsException(
                    "grid must contain at least tnx*tny entries");
        }

        int tnv[] = { nv };
        int tnx[] = { nx };
        int tny[] = { ny };
        return Meteolibrary.scaleless_analysis(xind, 0, yind, 0, values, 0,
                tnv, 0, tnx, 0, tny, 0, grid, 0);
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param passes
     * @param smoothness
     * 
     */
    public static void setqsmooth(int passes, float smoothness) {
        int npass[] = { passes };
        float tsmthwgt[] = { smoothness };
        Meteolibrary.setqsmooth(npass, 0, tsmthwgt, 0);
        return;
    }

    /**
     * <b>Main Description</b> : Calculates the QG frontogenesis function on a
     * single level using just that level's data
     * 
     * @param height
     *            (m)
     * @param temperature
     *            (K)
     * @param pressure
     *            (mb)
     * @param dx
     *            (m)
     * @param dy
     *            (m)
     * @param coriolis
     *            (/s)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param slqx
     * @param slqy
     * @param w1
     * @param w2
     * @param w3
     * @param dtdx
     * @param dtdy
     * @return <br>
     * <br>
     */
    public static float[] slfront(float[] height, float[] temperature,
            float pressure, float[] dx, float[] dy, float[] coriolis,
            int totalDimension, int IGridDimension, int JGridDimension,
            float[] slqx, float[] slqy, float[] w1, float[] w2, float[] w3,
            float[] dtdx, float[] dtdy) {
        float[] fgen = new float[totalDimension];
        float p[] = { pressure };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.slfront(height, 0, temperature, 0, p, 0, dx, 0, dy, 0,
                coriolis, 0, mni, 0, ni, 0, nj, 0, fgen, 0, slqx, 0, slqy, 0,
                w1, 0, w2, 0, w3, 0, dtdx, 0, dtdy, 0);
        return fgen;
    }

    /**
     * <b>Main Description</b> : Calculates the Q vector on a single level using
     * just that level's data
     * 
     * @param height
     *            (m)
     * @param temperature
     *            (K)
     * @param pressure
     *            (mb)
     * @param dx
     *            (mb)
     * @param dy
     *            (mb)
     * @param coriolis
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param dugdx
     * @param dugdy
     * @param dvgdx
     * @param dvgdy
     * @param dtdx
     * @param dtdy
     * @return v (VectorVars object with slqx and slqy) <br>
     * <br>
     */
    public static VectorVars slqvect(float[] height, float[] temperature,
            float pressure, float[] dx, float[] dy, float[] coriolis,
            int totalDimension, int IGridDimension, int JGridDimension,
            float[] dugdx, float[] dugdy, float[] dvgdx, float[] dvgdy,
            float[] dtdx, float[] dtdy) {
        VectorVars v = new VectorVars();
        float[] slqx = new float[totalDimension];
        float[] slqy = new float[totalDimension];
        float p[] = { pressure };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.slqvect(height, 0, temperature, 0, p, 0, dx, 0, dy, 0,
                coriolis, 0, mni, 0, ni, 0, nj, 0, slqx, 0, slqy, 0, dugdy, 0,
                dugdy, 0, dvgdx, 0, dvgdy, 0, dtdx, 0, dtdy, 0);
        v.setSlqy(slqy);
        v.setSlqx(slqx);
        return v;
    }

    /**
     * <b>Main Description</b> : Calculates the total solar radiation incident
     * at the top of the atmosphere (undepleted).
     * 
     * @param julianDay
     * @param month
     * @param latitude
     *            (deg)
     * @param timeIncrement
     *            (min)
     * @param timeStart
     *            (hr)
     * @param timeStop
     *            (hr)
     * @return solarRadiation (lightyears) <br>
     * <br>
     *         time is based on the 24 hour clock
     */
    public static float solax(int julianDay, int month, float latitude,
            int timeIncrement, int timeStart, int timeStop) {
        float solarRadiation = 0;
        int julday[] = { julianDay };
        int tmonth[] = { month };
        float tslat[] = { latitude };
        int tyminc[] = { timeIncrement };
        int tstart[] = { timeStart };
        int tstop[] = { timeStop };
        float tsrad[] = { solarRadiation };
        Meteolibrary.solax(julday, 0, tmonth, 0, tslat, 0, tyminc, 0, tstart,
                0, tstop, 0, tsrad, 0);
        solarRadiation = tsrad[0];
        return solarRadiation;
    }

    /**
     * <b>Main Description</b> : Calculates the specific humidity from the
     * pressure, temperature, and relative humidity
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param relativeHumidity
     *            (0...100)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return <br>
     * <br>
     */
    public static float[] spechum(float[] pressure, float[] temperature,
            float[] relativeHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] q = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.spechum(pressure, 0, temperature, 0, relativeHumidity, 0,
                mni, 0, ni, 0, nj, 0, q, 0);
        return q;
    }

    /**
     * <b>Main Description</b> : Calculates the saturation specific humidity
     * from the dewpoint and pressure
     * 
     * @param pressure
     *            (mb)
     * @param dewpoint
     *            (C or K)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return <br>
     * <br>
     */
    public static float[] spechum2(float[] pressure, float[] dewpoint) {
        int mni[] = { dewpoint.length };
        int ni[] = { dewpoint.length };
        int nj[] = { 1 };
        float[] q = new float[dewpoint.length];
        Meteolibrary.spechum2(pressure, 0, dewpoint, 0, mni, 0, ni, 0, nj, 0,
                q, 0);
        return q;
    }

    /**
     * Method call into the strmpak Fortan routine that fills xpoints and
     * ypoints with the grid relative points for streamlines. npoints contains
     * the number of points for both xpoints and ypoints.
     * 
     * @param uComp
     *            Float array for the u grid
     * @param vComp
     *            Float array for the v grid
     * @param work
     *            A working array
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @param arrowsize
     * @param xpoints
     * @param ypoints
     * @param npoints
     * @param minSpace
     * @param maxSpace
     * @param badLo
     * @param badHi
     */
    public static void strmpak(float[] uComp, float[] vComp, int[] work,
            int totalDimension, int IGridDimension, int JGridDimension,
            float arrowsize, float[] xpoints, float[] ypoints, int[] npoints,
            float minSpace, float maxSpace, float badLo, float badHi) {
        int mnx[] = { totalDimension };
        int nx[] = { IGridDimension };
        int ny[] = { JGridDimension };
        float arrowSize[] = { arrowsize };
        float min[] = { minSpace };
        float max[] = { maxSpace };
        float badlo[] = { badLo };
        float badhi[] = { badHi };
        Meteolibrary.strmpak(uComp, 0, vComp, 0, work, 0, mnx, 0, nx, 0, ny, 0,
                arrowSize, 0, xpoints, 0, ypoints, 0, npoints, 0, min, 0, max,
                0, badlo, 0, badhi, 0);
        return;
    }

    /**
     * <b>Main Description</b> : Takes the difference of the two fields. Each
     * i,j of arrayTwo is subtracted from the corresponding i,j of arrayOne
     * 
     * @param arrayOne
     * @param arrayTwo
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return <br>
     * <br>
     *         result = arrayOne-arrayTwo
     */
    public static float[] sub_aray(float[] arrayOne, float[] arrayTwo,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] result = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.sub_aray(arrayOne, 0, arrayTwo, 0, result, 0, mni, 0, ni,
                0, nj, 0);
        return result;
    }

    /**
     * <b>Main Description</b> : Calculates the severe weather threat (SWEAT)
     * index from sounding data
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param numLevels
     * @param windLevelPressures
     *            (mb)
     * @param uComp
     *            (m/s)
     * @param vComp
     *            (m/s)
     * @param numWindLevels
     * @return <br>
     * <br>
     */
    public static float sweat(float[] pressure, float[] temperature,
            float[] dewpoint, int numLevels, float[] windLevelPressures,
            float[] uComp, float[] vComp, int numWindLevels) {
        float tswidx = 0;
        int nlvls[] = { numLevels };
        int nw[] = { numWindLevels };
        float swidx[] = { tswidx };
        Meteolibrary.sweat(pressure, 0, temperature, 0, dewpoint, 0, nlvls, 0,
                windLevelPressures, 0, uComp, 0, vComp, 0, nw, 0, swidx, 0);
        return swidx[0];
    }

    /**
     * <b>Main Description</b> : Calculates sweat index from the total totals,
     * 850 dewpoint, and wind components at 850 and 500.
     * 
     * @param totalTotals
     * @param dewpoint850
     * @param uComp850
     * @param vComp850
     * @param uComp500
     * @param vComp500
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return <br>
     * <br>
     */
    public static float[] sweatidx(float[] totalTotals, float[] dewpoint850,
            float[] uComp850, float[] vComp850, float[] uComp500,
            float[] vComp500, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] q = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.sweatidx(totalTotals, 0, dewpoint850, 0, uComp850, 0,
                vComp850, 0, uComp500, 0, vComp500, 0, mni, 0, ni, 0, nj, 0, q,
                0);
        return q;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param p
     * @param taflgp
     * @param t
     * @param taflgt
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return
     */
    public static float[] temp2theta(float[] p, int taflgp, float[] t,
            int taflgt, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] theta = new float[totalDimension];
        int aflgp[] = { taflgp };
        int aflgt[] = { taflgt };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.temp2theta(p, 0, aflgp, 0, t, 0, aflgt, 0, theta, 0, mni,
                0, ni, 0, nj, 0);
        return theta;
    }

    /**
     * <b>Main Description</b> : Calculate the temperature at a given pressure
     * and mixing ratio
     * 
     * @param pressure
     * @param mixRatio
     * @return <br>
     * <br>
     */
    public static float temp_mixratio(float pressure, float mixRatio) {
        float ttempmr = 0;
        float press[] = { pressure };
        float mixratio[] = { mixRatio };
        float tempmr[] = { ttempmr };
        Meteolibrary.temp_mixratio(press, 0, mixratio, 0, tempmr, 0);
        return tempmr[0];
    }

    /**
     * <b>Main Description</b> : Calculates the saturation temperature of an
     * equivalent temperature at given pressure using the adiabatic definition
     * 
     * @param temperature
     * @param pressure
     * @return temp_of_te
     * 
     */
    public static float temp_of_te(float temperature, float pressure) {
        float te[] = { temperature };
        float press[] = { pressure };
        return Meteolibrary.temp_of_te(te, 0, press, 0);
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param p
     * @param taflgp
     * @param theta
     * @param taflgth
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return
     */
    public static float[] theta2temp(float[] p, int taflgp, float[] theta,
            int taflgth, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] temp = new float[totalDimension];
        int aflgp[] = { taflgp };
        int aflgth[] = { taflgth };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.theta2temp(p, 0, aflgp, 0, theta, 0, aflgth, 0, temp, 0,
                mni, 0, ni, 0, nj, 0);
        return temp;
    }

    /**
     * <b>Main Description</b> : Calculates the wet bulb potential temperature
     * via the adiabatic method
     * 
     * @param temperature
     *            (C)
     * @param dewpoint
     *            (C)
     * @param pressure
     *            (mb)
     * @param ice_water
     *            (> 0 mixing ratio respect to water, < 0 mixing ration respect
     *            to ice)
     * @return weather (object containing status [completion] and wetBulbTemp)
     * 
     */
    public static PHT thetawa(float temperature, float dewpoint,
            float pressure, int ice_water) {
        float theta = 0.0f;
        int status = 0;
        PHT weather = new PHT();
        float temp[] = { temperature };
        float dwpt[] = { dewpoint };
        float pres[] = { pressure };
        int iw[] = { ice_water };
        int ier[] = { status };
        theta = Meteolibrary.thetawa(temp, 0, dwpt, 0, pres, 0, iw, 0, ier, 0);
        weather.setCompletion(ier[0]);
        weather.setWetBulbTemp(theta);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculates the totals index, cross totals
     * index, and the vertical totals index from a sounding
     * 
     * @param pressure
     *            (mb)
     * @param temperature
     *            (C)
     * @param dewpoint
     *            (C)
     * @param numLevels
     * @return i (object containing totalIndex,crosstTotalIndex,and
     *         verticaltotalsIndex)
     * 
     */
    public static Index totals(float[] pressure, float[] temperature,
            float[] dewpoint, int numLevels) {
        float totalIndex = 0;
        float crossIndex = 0;
        float verticalIndex = 0;
        Index i = new Index();
        int nlvls[] = { numLevels };
        float totidx[] = { totalIndex };
        float crstot[] = { crossIndex };
        float vertot[] = { verticalIndex };
        Meteolibrary.totals(pressure, 0, temperature, 0, dewpoint, 0, nlvls, 0,
                totidx, 0, crstot, 0, vertot, 0);
        i.setTotalIndex(totalIndex);
        i.setCrossTotalIndex(crossIndex);
        i.setVerticalTotalsIndex(verticalIndex);
        return i;
    }

    /**
     * <b>Main Description</b> : Calculate the temperature and pressure at the
     * LCL given the parcel's initial temperature, dewpoint, and pressure
     * 
     * @param temperature
     *            (C)
     * @param dewpoint
     *            (C)
     * @param pressure
     * @return weather (PHT object containing pressure, temperature, and
     *         completion) <br>
     * <br>
     *         completion > 0 if successful, < 0 if failure
     */
    public static PHT tplcl(float temperature, float dewpoint, float pressure) {
        PHT weather = new PHT();
        float temperatureLCL = 0;
        float pressureLCL = 0;
        int completion = 0;
        float tk[] = { temperature };
        float td[] = { dewpoint };
        float pinit[] = { pressure };
        float tl[] = { temperatureLCL };
        float pl[] = { pressureLCL };
        int ier[] = { completion };
        Meteolibrary.tplcl(tk, 0, td, 0, pinit, 0, tl, 0, pl, 0, ier, 0);
        weather.setPressure(pressureLCL);
        weather.setTemperature(temperatureLCL);
        weather.setCompletion(completion);
        return weather;
    }

    /**
     * <b>Main Description</b> : Calculate the temperature at the lifted
     * condensation level. The computation of equivalent potential temperature
     * is solved iteratively using Newton's method. Also calculates the pressure
     * at the LCL and the vertical distance to the LCL from the initial lifting
     * level
     * 
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param pressure
     *            (mb)
     * @param ice_water
     *            (> 0 water, < 0 ice)
     * @return weather (object containing pressure,temperature,height, and
     *         completion)
     */
    public static PHT tpzlcl(float temperature, float dewpoint, float pressure,
            int ice_water) {
        PHT weather = new PHT();
        float temperatureLCL = 0;
        float pressureLCL = 0;
        float distanceInitial_LCL = 0;
        int completion = 0;
        float tk[] = { temperature };
        float tdk[] = { dewpoint };
        float pinit[] = { pressure };
        int iw[] = { ice_water };
        float tl[] = { temperatureLCL };
        float pl[] = { pressureLCL };
        float zl[] = { distanceInitial_LCL };
        int ier[] = { completion };
        Meteolibrary.tpzlcl(tk, 0, tdk, 0, pinit, 0, iw, 0, tl, 0, pl, 0, zl,
                0, ier, 0);
        weather.setPressure(pl[0]);
        weather.setTemperature(tl[0]);
        weather.setHeight(zl[0]);
        weather.setCompletion(ier[0]);
        return weather;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param tos
     * @param pressure
     * @return
     */
    public static float tsa(float tos, float pressure) {
        float os[] = { tos };
        float pres[] = { pressure };
        return Meteolibrary.tsa(os, 0, pres, 0);
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param elevation
     * @param pressure
     *            (mb)
     * @param height
     *            (m)
     * @param temperature
     *            (C)
     * @param potentialTemperatures
     *            (C)
     * @param numLevels
     * @param forecastMaxTemp
     *            (C)
     * @return t (Tsoar object with
     *         potentialTempForecast(C),heightMaxThermalAlt(
     *         m),tempMinimumEffectiveConvection
     *         (C),heightMinimumEffectiveConvection
     *         (m),tempMaxThermalAlt(C),soarIndex(ft/min),triggerTemperature(C))
     */
    public static Tsoar tsoar(float elevation, float[] pressure,
            float[] height, float[] temperature, float[] potentialTemperatures,
            float forecastMaxTemp) {
        Tsoar t = new Tsoar();
        float potentialTempForecast = 0;
        float heightMinimumEffectiveConvection = 0;
        float tempMinimumEffectiveConvection = 0;
        float heightMaxThermalAlt = 0;
        float tempMaxThermalAlt = 0;
        float soarIndex = 0;
        float triggerTemperature = 0;
        float elev[] = { elevation };
        int nl[] = { temperature.length };
        float tpmax[] = { forecastMaxTemp };
        float ptlxec[] = { potentialTempForecast };
        float zlnec[] = { heightMinimumEffectiveConvection };
        float tlnec[] = { tempMinimumEffectiveConvection };
        float zlxec[] = { heightMaxThermalAlt };
        float tlxec[] = { tempMaxThermalAlt };
        float soarindx[] = { soarIndex };
        float trigtemp[] = { triggerTemperature };
        Meteolibrary.tsoar(elev, 0, pressure, 0, height, 0, temperature, 0,
                potentialTemperatures, 0, nl, 0, tpmax, 0, ptlxec, 0, zlnec, 0,
                tlnec, 0, zlxec, 0, tlxec, 0, soarindx, 0, trigtemp, 0);
        t.setPotentialTempForecast(ptlxec[0]);
        t.setHeightMinimumEffectiveConvection(zlnec[0]);
        t.setTempMinimumEffectiveConvection(tlnec[0]);
        t.setHeightMaxThermalAlt(zlxec[0]);
        t.setTempMaxThermalAlt(tlxec[0]);
        t.setSoarIndex(soarindx[0]);
        t.setTriggerTemperature(trigtemp[0]);
        return t;
    }

    /**
     * <b>Main Description</b> : Calculate temperature from the virtual
     * temperature and specific humidity
     * 
     * @param virtualTemp
     *            (K)
     * @param specificHumidity
     *            (g/kg)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return t (tv2temp)
     * 
     */
    public static float[] tv2temp(float[] virtualTemp,
            float[] specificHumidity, int totalDimension, int IGridDimension,
            int JGridDimension) {
        float[] t = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.tv2temp(virtualTemp, 0, specificHumidity, 0, mni, 0, ni,
                0, nj, 0, t, 0);
        return t;
    }

    /**
     * <b>Main Description</b> : Calculates rectangular wind components given
     * wind direction and speed
     * 
     * @param windDir
     *            (deg clockwise from north)
     * @param windSpeed
     *            (m/s)
     * @param numLevels
     * 
     */
    public static WindComp uvcomp(float[] windDir, float[] windSpeed) {
        float[] uComp = new float[windDir.length];
        float[] vComp = new float[windDir.length];
        WindComp wind = new WindComp();
        int nlvls[] = { windDir.length };
        Meteolibrary.uvcomp(windDir, 0, windSpeed, 0, uComp, 0, vComp, 0,
                nlvls, 0);
        wind.setUCompArray(uComp);
        wind.setVCompArray(vComp);
        return wind;
    }

    /**
     * <b>Main Description</b> : Adds one to each point in count for each point
     * in inp that is not marked undefined. If init is non zero, will start with
     * count of zero.
     * 
     * @param a
     * @param init
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return count
     */
    public static float[] ver_pts(float[] a, int init, int totalDimension,
            int IGridDimension, int JGridDimension) {
        float[] count = new float[totalDimension];
        int tinit[] = { init };
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.ver_pts(a, 0, count, 0, tinit, 0, mni, 0, ni, 0, nj, 0);
        return count;
    }

    /**
     * <b>Main Description</b> : Calculates virtual temperature as a function of
     * temperature, dewpoint, and pressure
     * 
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param pressure
     *            (mb)
     * @return virttemp
     * 
     */
    public static float virttemp(float temperature, float dewpoint,
            float pressure) {
        float t[] = { temperature };
        float td[] = { dewpoint };
        float p[] = { pressure };
        return Meteolibrary.virttemp(t, 0, td, 0, p, 0);
    }

    /**
     * <b>Main Description</b> : Calculates virtual temperature at all sounding
     * levels
     * 
     * @param temperature
     *            (K)
     * @param dewpoint
     *            (K)
     * @param pressure
     *            (mb)
     * @param numLevels
     * @return tvir (virtualt function)
     * 
     */
    public static float[] virtualt(float[] temperature, float[] dewpoint,
            float[] pressure, int numLevels) {
        float[] tvir = new float[temperature.length];
        int nlvls[] = { numLevels };
        Meteolibrary.virtualt(temperature, 0, dewpoint, 0, pressure, 0, nlvls,
                0, tvir, 0);
        return tvir;
    }

    /**
     * <b> Main Description</b> : Calculates the vaport pressure using the
     * temperature and depending on the user's choice of "for ice" or "for
     * water"
     * 
     * @param temperature
     *            (K)
     * @param iceORwater
     *            (>0 water <0 ice) <br>
     * <br>
     *            iceORwater > 0 for vapor pressure with respect to water <br>
     *            iceORwater < 0 for vapor pressure with respect to ice <br>
     * <br>
     *            vapor pressure is computed and returned
     */
    public static float vp(float temperature, int iceORwater) {
        float t[] = { temperature };
        int w[] = { iceORwater };
        return Meteolibrary.vp(t, 0, w, 0);
    }

    /**
     * <b>Main Description</b> : Uses a 1-dimensional cloud model to calculate
     * the vertical velocity profile of a lifted parcel
     * 
     * @param pcb
     * @param equilibriumLevelPressure
     *            (mb)
     * @param pressure
     *            (mb)
     * @param height
     *            (m asl)
     * @param temperature
     *            (K)
     * @param soundingVirtualTemp
     *            (K)
     * @param parcelVirtualTemp
     *            (K)
     * @param mixRatio
     *            (g/kg)
     * @param numLevels
     * @return v (object holding verticalVelocity and maxVerticalVelocity)
     * 
     */
    public static Velocity vvel(float pcb, float equilibriumLevelPressure,
            float[] pressure, float[] height, float[] temperature,
            float[] soundingVirtualTemp, float[] parcelVirtualTemp,
            float mixRatio, int numParcelLvls) {
        Velocity v = new Velocity();
        float verticalVelocity[] = new float[temperature.length + 1];
        float vvmax[] = new float[1];
        float tpcb[] = { pcb };
        float peqlev[] = { equilibriumLevelPressure };
        float wlcl[] = { mixRatio };
        int npar[] = { numParcelLvls };
        Meteolibrary.vvel(tpcb, 0, peqlev, 0, pressure, 0, height, 0,
                temperature, 0, soundingVirtualTemp, 0, parcelVirtualTemp, 0,
                wlcl, 0, npar, 0, verticalVelocity, 0, vvmax, 0);
        v.setVerticalVelocity(verticalVelocity);
        v.setMaxVerticalVelocity(vvmax[0]);
        return v;
    }

    /**
     * <b>Main Description</b> : Determines the pressure, temperature, and
     * height of the wet-bulb zero.
     * 
     * @param elevation
     *            (m asl)
     * @param pressure
     *            (mb)
     * @param height
     *            (m asl)
     * @param temperature
     *            (C)
     * @param dewpoint
     *            (C)
     * @param numLevels
     * @return weather (object holding pressure, height, temperature)
     * 
     */
    public static PHT wbzero(float elevation, float[] pressure, float[] height,
            float[] temperature, float[] dewpoint) {
        float tpwbz = 0;
        float thwbz = 0;
        float ttwbz = 0;
        float elev[] = { elevation };
        int nlvls[] = { temperature.length };
        float pwbz[] = { tpwbz };
        float hwbz[] = { thwbz };
        float twbz[] = { ttwbz };
        Meteolibrary.wbzero(elev, 0, pressure, 0, height, 0, temperature, 0,
                dewpoint, 0, nlvls, 0, pwbz, 0, hwbz, 0, twbz, 0);
        PHT weather = new PHT();
        weather.setPressure(pwbz[0]);
        weather.setHeight(hwbz[0]);
        weather.setTemperature(twbz[0]);
        return weather;
    }

    /**
     * Main Description : Calculates the wind direction from the u an v
     * components
     * 
     * @param uComp
     * @param vComp
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return ff
     * 
     */
    public static float[] winddir(float[] uComp, float[] vComp,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] ff = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.winddir(uComp, 0, vComp, 0, ff, 0, mni, 0, ni, 0, nj, 0);
        return ff;
    }

    /**
     * <b>Main Description</b> : Calculate the wind speed from the wind
     * components
     * 
     * @param uComp
     *            (m/s)
     * @param vComp
     *            (m/s)
     * @param totalDimension
     * @param IGridDimension
     * @param JGridDimension
     * @return ff (m/s)
     * 
     */
    public static float[] windspeed(float[] uComp, float[] vComp,
            int totalDimension, int IGridDimension, int JGridDimension) {
        float[] ff = new float[totalDimension];
        int mni[] = { totalDimension };
        int ni[] = { IGridDimension };
        int nj[] = { JGridDimension };
        Meteolibrary.windspeed(uComp, 0, vComp, 0, ff, 0, mni, 0, ni, 0, nj, 0);
        return ff;
    }

    /**
     * <b>Main Description</b> :
     * 
     * @param rho
     * @param height
     * @param numLvls
     * @param heightWindObs
     * @param numWindObs
     * @return
     */
    public static float[] windrho(float[] rho, float[] height, int numLvls,
            float[] heightWindObs, int numWindObs) {
        float[] rhoAtHeights = new float[rho.length];
        int nlvls[] = { numLvls };
        int nw[] = { numWindObs };
        Meteolibrary.wndrho(rho, 0, height, 0, nlvls, 0, heightWindObs, 0, nw,
                0, rhoAtHeights, 0);
        return rhoAtHeights;
    }

    /**
     * <b>Main Description</b> : Calculates a pressure in standard atmoshpere in
     * millibars from height in meters
     * 
     * @param height
     *            (m)
     */
    public static float ztopsa(float height) {
        float z[] = { height };
        return Meteolibrary.ztopsa(z, 0);
    }
}
