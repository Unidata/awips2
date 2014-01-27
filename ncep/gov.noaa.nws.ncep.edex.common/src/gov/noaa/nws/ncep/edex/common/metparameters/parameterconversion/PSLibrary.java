/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion;

import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.WetBulbPotentialTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

////import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
/**
 * @author archana
 * 
 */
public final class PSLibrary {

    /**
     * Computes the cross totals index
     * 
     * @param td850
     *            - dewpoint at 850 mb ( in Celsius )
     * @param t500
     *            - temperature at 500 mb ( in Celsius )
     * @return the difference between the dewpoint and the temperature if
     *         neither of them are missing and RMISSD ( -9999 ) otherwise.
     * @throws InvalidValueException
     * @throws NullPointerException
     */

    public final static Amount psCtot(Amount td850, Amount t500) {

        /*
         * Compute the cross totals index by subtracting 500 mb temperature from
         * the 850 mb dewpoint.
         */
        if (!PRLibrary.checkNullOrInvalidValue(td850)
                || !PRLibrary.checkNullOrInvalidValue(t500)) {
            return new Amount(Unit.ONE);
        }
        Amount psCtot = new Amount(td850.getValue().floatValue()
                - t500.getValue().floatValue(), Unit.ONE);
        return psCtot;
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
     * @throws InvalidValueException
     * @throws NullPointerException
     */
    public static Amount psHans(Amount tc1Amt, Amount tc2Amt, Amount dwpcAmt,
            Amount itypeAmt) {
        Amount pshans = new Amount();
        if (!PRLibrary.checkNullOrInvalidValue(tc1Amt)
                || !PRLibrary.checkNullOrInvalidValue(tc2Amt)
                || !PRLibrary.checkNullOrInvalidValue(dwpcAmt)
                || !PRLibrary.checkNullOrInvalidValue(itypeAmt))
            return new Amount(Unit.ONE);
        float a = GempakConstants.RMISSD;
        float b = GempakConstants.RMISSD;
        float tc1 = tc1Amt.getValue().floatValue();
        float tc2 = tc2Amt.getValue().floatValue();
        float dwpc = dwpcAmt.getValue().floatValue();
        float itype = itypeAmt.getValue().floatValue();
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
        pshans = new Amount(a + b, Unit.ONE);
        // }
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
     * @throws InvalidValueException
     * @throws NullPointerException
     */
    public final static Amount pskinx(Amount t850, Amount t700, Amount t500,
            Amount td850, Amount td700) {
        Amount pskinx = new Amount();
        if (!PRLibrary.checkNullOrInvalidValue(t850)
                || !PRLibrary.checkNullOrInvalidValue(t700)
                || !PRLibrary.checkNullOrInvalidValue(t500)
                || !PRLibrary.checkNullOrInvalidValue(td850)
                || !PRLibrary.checkNullOrInvalidValue(td700))
            return new Amount(Unit.ONE);
        pskinx = new Amount(
                (t850.getValue().floatValue() - t500.getValue().floatValue())
                        + td850.getValue().floatValue()
                        - (t700.getValue().floatValue() - td700.getValue()
                                .floatValue()), Unit.ONE);

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
    public final static Amount psShow(Amount t850Amt, Amount td850Amt,
            Amount t500Amt) {
        float psshow = GempakConstants.RMISSD;

        if (!PRLibrary.checkNullOrInvalidValue(t850Amt)
                || !PRLibrary.checkNullOrInvalidValue(td850Amt)
                || !PRLibrary.checkNullOrInvalidValue(t500Amt))
            return new Amount(Unit.ONE);
        float p850 = 850;
        float p500 = 500;
        float guess = 0;
        /*
         * Find equivalent potential temperature at the LCL using 850 mb
         * temperature and dewpoint.
         */
        Amount thtlcl = PRLibrary.prThte(new Amount(p850, NcUnits.MILLIBAR),
                t850Amt, td850Amt);
        if (!PRLibrary.checkNullOrInvalidValue(thtlcl))
            return new Amount(Unit.ONE);
        /*
         * Find parcel temperature along pseudoadiabat at 500 mb. The parcel
         * temperature tp is the temperature in Celsius at 500 mb of a parcel,
         * which lies on the moist adiabat determined by the sounding at 850 mb
         */
        Amount tp = PRLibrary.prTmst(thtlcl,
                new Amount(p500, NcUnits.MILLIBAR), new Amount(0, Unit.ONE));

        /*
         * Subtract the parcel temperature from the temperature at 500 mb, is
         * the parcel temperature is valid
         */

        if (!PRLibrary.checkNullOrInvalidValue(tp))
            return new Amount(Unit.ONE);
        t500Amt = PRLibrary.checkAndConvertInputAmountToExpectedUnits(t500Amt,
                SI.KELVIN);
        float t500 = t500Amt.getValue().floatValue();
        psshow = t500 - tp.getValue().floatValue();

        return new Amount(psshow, Unit.ONE);
    }

    /**
     * Computes the vertical totals index
     * 
     * @param t850
     *            - 850 mb temperature ( in Celsius )
     * @param t500
     *            - 500 mb temperature ( in Celsius )
     * @return the vertical totals index as a difference of the temperature at
     *         850 mb and the temperature at 500 mb, if neither value is
     *         missing. Else it returns RMISSD (-9999)
     * @throws InvalidValueException
     * @throws NullPointerException
     */
    public final static Amount psVtot(Amount t850, Amount t500) {
        if (!PRLibrary.checkNullOrInvalidValue(t850)
                || !PRLibrary.checkNullOrInvalidValue(t500))
            return new Amount(Unit.ONE);

        return new Amount(t850.getValue().floatValue()
                - t500.getValue().floatValue(), Unit.ONE);
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
     * @throws InvalidValueException
     * @throws NullPointerException
     */
    public final static Amount psTotl(Amount t850, Amount td850, Amount t500) {
        float pstotl = GempakConstants.RMISSD;

        if (!PRLibrary.checkNullOrInvalidValue(t500)
                || !PRLibrary.checkNullOrInvalidValue(td850)
                || !PRLibrary.checkNullOrInvalidValue(t850))
            return new Amount(Unit.ONE);

        /* Compute the vertical totals */
        Amount vtot = psVtot(t850, t500);

        /* Compute the cross totals */
        Amount ctot = psCtot(td850, t500);

        if (!PRLibrary.checkNullOrInvalidValue(vtot)
                || !PRLibrary.checkNullOrInvalidValue(ctot))
            return new Amount(Unit.ONE);
        pstotl = vtot.getValue().floatValue() + ctot.getValue().floatValue();

        return new Amount(pstotl, Unit.ONE);
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
     * @throws InvalidValueException
     * @throws NullPointerException
     */
    public final static Amount psSwet(Amount t850, Amount td850, Amount t500,
            Amount spd850, Amount spd500, Amount dir850, Amount dir500) {
        float pssweat = GempakConstants.RMISSD;
        if (!PRLibrary.checkNullOrInvalidValue(t500)
                || !PRLibrary.checkNullOrInvalidValue(td850)
                || !PRLibrary.checkNullOrInvalidValue(t500)
                || !PRLibrary.checkNullOrInvalidValue(spd850)
                || !PRLibrary.checkNullOrInvalidValue(spd500)
                || !PRLibrary.checkNullOrInvalidValue(dir850)
                || !PRLibrary.checkNullOrInvalidValue(dir500))
            return new Amount(Unit.ONE);
        /*
         * (Non-Javadoc): All computations are from Miller, R.C., 1972: Notes on
         * Severe Storm Forecasting Procedures of the Air Force Global Weather
         * Central, AWS Tech. Report 200
         */
        /* Convert meters per second to knots */
        Amount skt850 = PRLibrary.checkAndConvertInputAmountToExpectedUnits(
                spd850, NonSI.KNOT);
        Amount skt500 = PRLibrary.checkAndConvertInputAmountToExpectedUnits(
                spd500, NonSI.KNOT);

        /* Compute the total totals index. If < 49, set term to zero. */
        float total = psTotl(t850, td850, t500).getValue().floatValue();
        float term2 = total - 49;
        if (total < 49)
            term2 = 0;

        /* Compute shear term. */

        float dif = dir500.getValue().floatValue()
                - dir850.getValue().floatValue();
        float s = (float) (Math.sin(dif * GempakConstants.DTR));
        float shear = 125 * (s + 0.2f);

        /* Make various wind checks. */
        float dir850Val = dir850.getValue().floatValue();
        float dir500Val = dir500.getValue().floatValue();
        float skt500Val = skt500.getValue().floatValue();
        float skt850Val = skt850.getValue().floatValue();

        if ((dir850Val < 130.) || (dir850Val > 250.))
            shear = 0;
        if ((dir500Val < 210.) || (dir500Val > 310.))
            shear = 0;
        if ((skt500Val < 15.) || (skt850Val < 15.))
            shear = 0;
        if (dif <= 0)
            shear = 0;

        /* Check for sub-zero dewpoint */
        float dwp850 = td850.getValue().floatValue();
        if (dwp850 < 0)
            dwp850 = 0;

        /* Calculate SWEAT index */
        pssweat = 12 * dwp850 + 20 * term2 + 2 * skt850Val + skt500Val + shear;
        return new Amount(pssweat, Unit.ONE);
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
     * @throws Exception
     */
    public static NcSoundingLayer2 psUstb(
            List<NcSoundingLayer2> listOfNcSoundingLayer, PressureLevel plev)
            throws Exception {
        // System.out.println(" PSLibrary/psUstb !!!!!!!!! ");
        // TODO: update to find pressure value between 2 levels
        NcSoundingLayer2 outputNcSoundingLayer = new NcSoundingLayer2();
        class PressureComparator implements Comparator<NcSoundingLayer2> {
            public int compare(NcSoundingLayer2 n1, NcSoundingLayer2 n2) {
                return Float.compare(n2.getPressure().getValue().floatValue(),
                        n1.getPressure().getValue().floatValue());
            }
        }

        // System.out.println(" PSLibrary/psUstb 1 ");

        if (listOfNcSoundingLayer != null && listOfNcSoundingLayer.size() > 0) {
            if (listOfNcSoundingLayer.size() > 1)
                Collections.sort(listOfNcSoundingLayer,
                        new PressureComparator());
            // System.out.println(" PSLibrary/psUstb 2 ");

            NcSoundingLayer2 surfaceLevelNcSoundingLayer = listOfNcSoundingLayer
                    .get(0);
            PressureLevel pressureAtSurfaceLevel = surfaceLevelNcSoundingLayer
                    .getPressure();
            NcSoundingLayer2 topLevelNcSoundingLayer = PCLibrary
                    .pcFtop(listOfNcSoundingLayer);
            PressureLevel pressureAtTopLevel = topLevelNcSoundingLayer
                    .getPressure();
            float plevVal = plev.getValue().floatValue();
            float presAtSurfaceLevelValue = pressureAtSurfaceLevel.getValue()
                    .floatValue();
            float presAtTopLevelVal = pressureAtTopLevel.getValue()
                    .floatValue();
            if (plevVal > presAtSurfaceLevelValue)
                return outputNcSoundingLayer;
            else if (plevVal == -1 || plevVal <= presAtTopLevelVal)
                plev = pressureAtTopLevel;
            // System.out.println(" PSLibrary/psUstb 3 ");

            int sizeOfList = listOfNcSoundingLayer.size();
            boolean done = false;
            int lev = 0;
            float eps = GempakConstants.RMISSD;

            while (!done && lev < sizeOfList) {
                NcSoundingLayer2 currNcSoundingLayer = listOfNcSoundingLayer
                        .get(lev);
                // System.out.println(" PSLibrary/psUstb 4 ");

                Amount pressure = new Amount(currNcSoundingLayer.getPressure()
                        .getValue(), NcUnits.MILLIBAR);
                Amount tmpc = new Amount(currNcSoundingLayer.getTemperature()
                        .getValue(), SI.CELSIUS);
                Amount dwpc = new Amount(currNcSoundingLayer.getDewpoint()
                        .getValue(), SI.CELSIUS);
                if (pressure.hasValidValue() && tmpc.hasValidValue()
                        && dwpc.hasValidValue()) {
                    WetBulbPotentialTemp thwc = new WetBulbPotentialTemp();
                    // System.out.println(" PSLibrary/thwc  " +
                    // thwc.doubleValue()
                    // + " pressure " + pressure.doubleValue() + " tmpc "
                    // + tmpc.doubleValue() + " dwpc "
                    // + dwpc.doubleValue());

                    thwc.setValue(PRLibrary.prThwc(pressure, tmpc, dwpc));
                    if (thwc.getValue().floatValue() > eps
                            && (pressure.getValue().floatValue() >= plevVal)) {
                        eps = thwc.getValue().floatValue();
                        outputNcSoundingLayer.setPressure(currNcSoundingLayer
                                .getPressure());
                        outputNcSoundingLayer
                                .setTemperature(currNcSoundingLayer
                                        .getTemperature());
                        outputNcSoundingLayer.setDewpoint(currNcSoundingLayer
                                .getDewpoint());
                        outputNcSoundingLayer.setWindSpeed(currNcSoundingLayer
                                .getWindSpeed());
                        outputNcSoundingLayer
                                .setWindDirection(currNcSoundingLayer
                                        .getWindDirection());
                        outputNcSoundingLayer.setWindU(currNcSoundingLayer
                                .getWindU());
                        outputNcSoundingLayer.setWindV(currNcSoundingLayer
                                .getWindV());
                        outputNcSoundingLayer.setGeoHeight(currNcSoundingLayer
                                .getGeoHeight());
                        outputNcSoundingLayer.setOmega(currNcSoundingLayer
                                .getOmega());
                        outputNcSoundingLayer
                                .setSpecificHumidity(currNcSoundingLayer
                                        .getSpecificHumidity());
                        // System.out.println("Outdat: "
                        // + outputNcSoundingLayer.getPressure().getValueAs(
                        // NcUnits.MILLIBAR ).floatValue() + " "
                        // + outputNcSoundingLayer.getTemperature().getValueAs(
                        // SI.CELSIUS ).floatValue() + " "
                        // + outputNcSoundingLayer.getDewpoint().getValueAs(
                        // SI.CELSIUS ).floatValue() + " "
                        // + outputNcSoundingLayer.getWindSpeed().getValueAs(
                        // SI.METERS_PER_SECOND ).floatValue() + " "
                        // +
                        // outputNcSoundingLayer.getWindDirection().getValueAs(
                        // NonSI.DEGREE_ANGLE ).floatValue() + " "
                        // + outputNcSoundingLayer.getGeoHeight().getValueAs(
                        // SI.METER ).floatValue() );
                    }

                }
                // System.out.println("Pressure value is " +
                // pressure.getValue().floatValue() + " " +
                // pressure.getUnit().toString() );
                // System.out.println("lev is " + lev );
                lev++;
                if (pressure.hasValidValue()
                        && pressure.getValue().floatValue() <= plevVal) {
                    done = true;
                    break;
                }
            }
        }
        //System.out.println("PSLibrary/psUstb Outdat: "
        //        + outputNcSoundingLayer.getPressure().getValue().floatValue()
        //        + " "
        //        + outputNcSoundingLayer.getTemperature().getValue()
        //                .floatValue()
        //        + " "
        //        + outputNcSoundingLayer.getDewpoint().getValue().floatValue()
        //        + " "
        //        + outputNcSoundingLayer.getWindSpeed().getValue().floatValue()
        //        + " "
        //        + outputNcSoundingLayer.getWindDirection().getValue()
        //                .floatValue() + " "
        //        + outputNcSoundingLayer.getGeoHeight().getValue().floatValue());

        return outputNcSoundingLayer;
    }

}
