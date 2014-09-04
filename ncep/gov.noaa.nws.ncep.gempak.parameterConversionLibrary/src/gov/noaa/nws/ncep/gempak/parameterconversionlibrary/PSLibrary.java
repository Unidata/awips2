/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameterconversionlibrary;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer
 * 
 * This java class provides sounding data data structure for used with NC sounding query.
 * Each NcSoundingLayer contain one layer of sounding information (pressure, height, temp, dewpt, windS, windD) for
 * one point (lat/lon) at a particular time (timeLine) and particular height.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer        Description
 * -------      -------     --------        -----------
 *                          A. Subramanian  Created
 * 03/2014      1116        T. Lee          Added DPD to NcSoundingLayer
 *
 * </pre>
 */

public class PSLibrary {

    /**
     * Computes the cross totals index
     * 
     * @param td850
     *            - dewpoint at 850 mb ( in Celsius )
     * @param t500
     *            - temperature at 500 mb ( in Celsius )
     * @return the difference between the dewpoint and the temperature if
     *         neither of them are missing and RMISSD ( -9999 ) otherwise.
     */
    public static float psCtot(float td850, float t500) {

        /*
         * Compute the cross totals index by subtracting 500 mb temperature from
         * the 850 mb dewpoint.
         */
        return ((!MissingValueTester.isDataValueMissing(td850) && !MissingValueTester
                .isDataValueMissing(t500)) ? (td850 - t500)
                : GempakConstants.RMISSD);
    }

    /**
     * Computes low, middle, and high elevation Haines indices from the
     * temperature and the dewpoint.
     * 
     * @param tc1
     *            - temperature ( in Celsius )
     * @param tc2
     *            - temperature ( in Celsius )
     * @param dwpc
     *            - dewpoint ( in Celsius )
     * @param itype
     *            - Haines index: 1- Low 2 - Middle 3 - High
     * 
     * @return
     */
    public static float psHans(float tc1, float tc2, float dwpc, float itype) {
        float pshans = GempakConstants.RMISSD;
        if (!MissingValueTester.isDataValueMissing(tc1)
                && !MissingValueTester.isDataValueMissing(tc2)
                && !MissingValueTester.isDataValueMissing(dwpc)) {
            float a = GempakConstants.RMISSD;
            float b = GempakConstants.RMISSD;

            /* Compute the Haines index */
            if (itype == 1) {
                a = ((tc2 - tc1) - 3) * (2 / 5) + 1;
                b = ((tc1 - dwpc) - 5) * (2 / 5) + 1;
            } else if (itype == 2) {
                a = ((tc1 - tc2) - 5) * (2 / 6) + 1;
                b = ((tc1 - dwpc) - 5) * (2 / 8) + 1;
            } else if (itype == 3) {
                a = ((tc1 - tc2) - 17) * (2 / 5) + 1;
                b = ((tc1 - dwpc) - 14) * (2 / 7) + 1;
            }

            a = (a > 0.9f ? a : 0.9f);
            a = (a < 3.1f ? a : 3.1f);
            b = (b > 0.9f ? b : 0.9f);
            b = (b < 3.1f ? b : 3.1f);
            pshans = a + b;
        }
        return pshans;
    }

    /**
     * 
     * Computes the 'K' index
     * 
     * @param t850
     *            - 850 mb temperature ( in Celsius )
     * @param t700
     *            - 700 mb temperature ( in Celsius )
     * @param t500
     *            - 500 mb temperature ( in Celsius )
     * @param td850
     *            - 850 mb dewpoint ( in Celsius )
     * @param td700
     *            - 700 mb dewpoint ( in Celsius )
     * @return returns the 'K' index if all the input values are valid and
     *         RMISSD ( -9999 ) otherwise
     */
    public static float pskinx(float t850, float t700, float t500, float td850,
            float td700) {
        float pskinx = GempakConstants.RMISSD;
        if (!MissingValueTester.isDataValueMissing(td700)
                && !MissingValueTester.isDataValueMissing(td850)
                && !MissingValueTester.isDataValueMissing(t500)
                && !MissingValueTester.isDataValueMissing(t700)
                && !MissingValueTester.isDataValueMissing(t850)) {
            pskinx = (t850 - t500) + td850 - (t700 - td700);
        }
        return pskinx;
    }

    /**
     * Computes the Showalter index
     * 
     * @param t850
     *            - 850 mb temperature ( in Celsius )
     * @param td850
     *            - 850 mb dewpoint ( in Celsius )
     * @param t500
     *            - 500 mb temperature ( in Celsius )
     * @return the Showalter index if all the three input parameters are valid
     *         and the parcel temperature is computed correctly. Otherwise, it
     *         returns RMISSD (-9999).
     */
    public static float psShow(float t850, float td850, float t500) {
        float psshow = GempakConstants.RMISSD;
        if (!MissingValueTester.isDataValueMissing(t500)
                && !MissingValueTester.isDataValueMissing(td850)
                && !MissingValueTester.isDataValueMissing(t850)) {
            float p850 = 850;
            float p500 = 500;
            float guess = 0;
            /*
             * Find equivalent potential temperature at the LCL using 850 mb
             * temperature and dewpoint.
             */
            float thtlcl = PRLibrary.prThte(p850, t850, td850);

            /*
             * Find parcel temperature along pseudoadiabat at 500 mb. The parcel
             * temperature tp is the temperature in Celsius at 500 mb of a
             * parcel, which lies on the moist adiabat determined by the
             * sounding at 850 mb
             */
            float tp = PRLibrary.prTmst(thtlcl, p500, guess);

            /*
             * Subtract the parcel temperature from the temperature at 500 mb,
             * is the parcel temperature is valid
             */
            if (!MissingValueTester.isDataValueMissing(tp)) {
                psshow = PRLibrary.prTmck(t500) - tp;
            }
        }
        return psshow;
    }

    /**
     * Computes the vertical totals index
     * 
     * @param t850
     *            - 850 mb temperature ( in Celsius )
     * @param t500
     *            - 500 mb temperature ( in Celsius )
     * @return the verticl totals index as a difference of the temperature at
     *         850 mb and the temperature at 500 mb, if neither value is
     *         missing. Else it returns RMISSD (-9999)
     */
    public static float psVtot(float t850, float t500) {
        return ((!MissingValueTester.isDataValueMissing(t500) && !MissingValueTester
                .isDataValueMissing(t850)) ? t850 - t500
                : GempakConstants.RMISSD);
    }

    /**
     * Computes the total Totals index from the temperature and dewpoint at 850
     * mb and the temperature at 500 mb
     * 
     * @param t850
     *            - 850 mb temperature ( in Celsius )
     * @param td850
     *            - 850 mb dewpoint ( in Celsius )
     * @param t500
     *            - 500 mb temperature ( in Celsius )
     * @return the total totals index if none of the input parameters are
     *         missing and RMISSD (-9999) otherwise
     */
    public static float psTotl(float t850, float td850, float t500) {
        float pstotl = GempakConstants.RMISSD;
        if (!MissingValueTester.isDataValueMissing(t500)
                && !MissingValueTester.isDataValueMissing(td850)
                && !MissingValueTester.isDataValueMissing(t850)) {

            /* Compute the vertical totals */
            float vtot = psVtot(t850, t500);

            /* Compute the cross totals */
            float ctot = psCtot(td850, t500);

            if (!MissingValueTester.isDataValueMissing(ctot)
                    && !MissingValueTester.isDataValueMissing(vtot)) {
                pstotl = ctot + vtot;
            }
        }
        return pstotl;
    }

    /**
     * Computes the SWEAT index. Winds must be input in m/sec
     * 
     * @param t850
     *            - 850 mb temperature
     * @param td850
     *            - 850 mb dewpoint
     * @param t500
     *            - 500 mb temperature
     * @param spd850
     *            - 850 mb windspeed ( in m/sec )
     * @param spd500
     *            - 500 mb windspeed ( in m/sec )
     * @param dir850
     *            - 850 mb wind direction
     * @param dir500
     *            - 500 mb wind direction
     * @return the SWEAT index if none of the inputs are missing and RMISSD (
     *         -9999 ) otherwise
     */
    public static float psSwet(float t850, float td850, float t500,
            float spd850, float spd500, float dir850, float dir500) {
        float pssweat = GempakConstants.RMISSD;
        if (!MissingValueTester.isDataValueMissing(dir500)
                && !MissingValueTester.isDataValueMissing(dir850)
                && !MissingValueTester.isDataValueMissing(spd500)
                && !MissingValueTester.isDataValueMissing(spd850)
                && !MissingValueTester.isDataValueMissing(t500)
                && !MissingValueTester.isDataValueMissing(td850)
                && !MissingValueTester.isDataValueMissing(t850)) {
            /*
             * (Non-Javadoc): All computations are from Miller, R.C., 1972:
             * Notes on Severe Storm Forecasting Procedures of the Air Force
             * Global Weather Central, AWS Tech. Report 200
             */
            /* Convert meters per second to knots */
            float skt850 = PRLibrary.prMskn(spd850);
            float skt500 = PRLibrary.prMskn(spd500);

            /* Compute the total totals index. If < 49, set term to zero. */
            float total = psTotl(t850, td850, t500);
            float term2 = total - 49;
            if (total < 49)
                term2 = 0;

            /* Compute shear term. */

            float dif = dir500 - dir850;
            float s = (float) (Math.sin(dif * GempakConstants.DTR));
            float shear = 125 * (s + 0.2f);

            /* Make various wind checks. */
            if ((dir850 < 130.) || (dir850 > 250.))
                shear = 0;
            if ((dir500 < 210.) || (dir500 > 310.))
                shear = 0;
            if ((skt500 < 15.) || (skt850 < 15.))
                shear = 0;
            if (dif <= 0)
                shear = 0;

            /* Check for sub-zero dewpoint */
            float dwp850 = td850;
            if (dwp850 < 0)
                dwp850 = 0;

            /* Calculate SWEAT index */
            pssweat = 12 * dwp850 + 20 * term2 + 2 * skt850 + skt500 + shear;
        }
        return pssweat;
    }

    /**
     * Finds the most unstable level of a sounding from the surface to the input
     * pressure level.
     * 
     * @param listOfNcSoundingLayer
     *            - the list of NcSoundingLayer to search
     * @param plev
     *            - input pressure level
     * @return the most unstable level of a sounding from the surface to the
     *         input pressure level (plev), if plev is not -1 and all
     *         computations fall through correctly.Else it returns an empty
     *         NcSoundingLayer object
     */
    public static NcSoundingLayer psUstb(
            List<NcSoundingLayer> listOfNcSoundingLayer, float plev) {
        // TODO: update to find pressure value between 2 levels
        NcSoundingLayer outputNcSoundingLayer = new NcSoundingLayer(
                GempakConstants.RMISSD, GempakConstants.RMISSD,
                GempakConstants.RMISSD, GempakConstants.RMISSD,
                GempakConstants.RMISSD, GempakConstants.RMISSD,
                GempakConstants.RMISSD, GempakConstants.RMISSD,
                GempakConstants.RMISSD, GempakConstants.RMISSD,
                GempakConstants.RMISSD, GempakConstants.RMISSD);
        class PressureComparator implements Comparator<NcSoundingLayer> {
            public int compare(NcSoundingLayer n1, NcSoundingLayer n2) {
                return Float.compare(n2.getPressure(), n1.getPressure());
            }
        }
        if (listOfNcSoundingLayer != null && listOfNcSoundingLayer.size() > 0) {
            Collections.sort(listOfNcSoundingLayer, new PressureComparator());
            NcSoundingLayer surfaceLevelNcSoundingLayer = listOfNcSoundingLayer
                    .get(0);
            float pressureAtSurfaceLevel = surfaceLevelNcSoundingLayer
                    .getPressure();
            NcSoundingLayer topLevelNcSoundingLayer = PCLibrary
                    .pcFtop(listOfNcSoundingLayer);
            float pressureAtTopLevel = topLevelNcSoundingLayer.getPressure();

            if (plev > pressureAtSurfaceLevel)
                return outputNcSoundingLayer;
            else if (plev == -1 || plev <= pressureAtTopLevel)
                plev = pressureAtTopLevel;

            int sizeOfList = listOfNcSoundingLayer.size();
            boolean done = false;
            int lev = 0;
            float eps = GempakConstants.RMISSD;

            while (!done && lev < sizeOfList) {
                NcSoundingLayer currNcSoundingLayer = listOfNcSoundingLayer
                        .get(lev);

                float pressure = currNcSoundingLayer.getPressure();
                float tmpc = currNcSoundingLayer.getTemperature();
                float dwpc = currNcSoundingLayer.getDewpoint();
                float thwc = PRLibrary.prThwc(pressure, tmpc, dwpc);
                System.out.println("pressure = " + pressure);
                System.out.println("thwc = " + thwc);
                if (thwc > eps && (pressure >= plev)) {
                    eps = thwc;
                    outputNcSoundingLayer.setPressure(currNcSoundingLayer
                            .getPressure());
                    outputNcSoundingLayer.setTemperature(currNcSoundingLayer
                            .getTemperature());
                    outputNcSoundingLayer.setDewpoint(currNcSoundingLayer
                            .getDewpoint());
                    outputNcSoundingLayer.setWindSpeed(currNcSoundingLayer
                            .getWindSpeed());
                    outputNcSoundingLayer.setWindDirection(currNcSoundingLayer
                            .getWindDirection());
                    outputNcSoundingLayer.setWindU(currNcSoundingLayer
                            .getWindU());
                    outputNcSoundingLayer.setWindV(currNcSoundingLayer
                            .getWindV());
                    outputNcSoundingLayer.setGeoHeight(currNcSoundingLayer
                            .getGeoHeight());
                    outputNcSoundingLayer.setOmega(currNcSoundingLayer
                            .getOmega());
                    outputNcSoundingLayer.setSpecHumidity(currNcSoundingLayer
                            .getSpecHumidity());
                    System.out.println("Outdat: "
                            + outputNcSoundingLayer.getPressure() + " "
                            + outputNcSoundingLayer.getTemperature() + " "
                            + outputNcSoundingLayer.getDewpoint() + " "
                            + outputNcSoundingLayer.getWindSpeed() + " "
                            + outputNcSoundingLayer.getWindDirection() + " "
                            + outputNcSoundingLayer.getGeoHeight());
                }
                lev++;
                if (pressure <= plev)
                    done = true;
            }
        }
        System.out.println("From PS_USTB Outdat: "
                + outputNcSoundingLayer.getPressure() + " "
                + outputNcSoundingLayer.getTemperature() + " "
                + outputNcSoundingLayer.getDewpoint() + " "
                + outputNcSoundingLayer.getWindSpeed() + " "
                + outputNcSoundingLayer.getWindDirection() + " "
                + outputNcSoundingLayer.getGeoHeight());

        return outputNcSoundingLayer;
    }

}
