/*
 * 
 * PRLibrary
 * 
 * Date created 04 May 2011
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 */
package gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion;

import gov.noaa.nws.ncep.edex.common.metparameters.Amount;

import java.util.Arrays;

import javax.measure.quantity.Temperature;
import javax.measure.quantity.VolumetricDensity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.opengis.coverage.grid.InvalidRangeException;
//import com.raytheon.uf.viz.core.exception.VizException;

public final class PRLibrary {

    /**
     * No-arguments constructor
     */
    public PRLibrary() {

    }

    /**
     * Computes altimeter from the station pressure and elevation
     * 
     * @param pres
     *            - pressure at the station
     * @param selv
     *            - elevation of the station
     * @return the computed altimeter in Inches
     * @throws NullPointerException
     * 
     */
    public static final Amount prAltp(Amount pres, Amount selv)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prAltp:");
        // System.out.println(" pres = " + pres.doubleValue());
        // System.out.println(" selv = " + selv.doubleValue());

        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(selv))
            return new Amount(NonSI.INCH_OF_MERCURY);

        selv = checkAndConvertInputAmountToExpectedUnits(selv, SI.METER);
        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        double seaLevelTempInKelvin = GempakConstants.TMCK + 15;
        double hgtk = selv.getUnit().getConverterTo(SI.KILOMETER)
                .convert(selv.doubleValue());
        double exponent = -(GempakConstants.GRAVTY
                / (GempakConstants.GAMUSD * GempakConstants.RDGAS) * 1000);
        // double base = pres * ( 1.0f - ( hgtk * GempakConstants.GAMUSD /
        // seaLevelTempInKelvin ) );
        // float altm = (float) Math.pow( base, Math.exp( exponent ) );
        // return prAlti( pres * altm );
        double base = (1.0 - (hgtk * GempakConstants.GAMUSD / seaLevelTempInKelvin));
        double altm = Math.pow(base, exponent);
        double altp = pres.doubleValue() * altm;
        return (new Amount(altp, NonSI.INCH_OF_MERCURY));
    }

    /***
     * Computes the Ceiling converted to Mean Sea Level in hundreds of feet
     * 
     * @param ceil
     *            - Ceiling in hundreds of feet
     * @param selv
     *            - Station elevation in meters
     * @return The ceiling converted to Mean Sea Level in hundreds of feet
     * @throws NullPointerException
     * 
     */
    public static final Amount prCmsl(Amount ceil, Amount selv)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prCmsl:");
        // System.out.println(" ceil = " + ceil.doubleValue());
        // System.out.println(" selv = " + selv.doubleValue());
        /* Sanity check */
        // checkNullOrInvalidValue( ceil );
        // checkNullOrInvalidValue( selv );
        if (!checkNullOrInvalidValue(ceil) || !checkNullOrInvalidValue(selv))
            return new Amount(NcUnits.HUNDREDS_OF_FEET);

        selv = checkAndConvertInputAmountToExpectedUnits(selv,
                NcUnits.HUNDREDS_OF_FEET);
        ceil = checkAndConvertInputAmountToExpectedUnits(ceil,
                NcUnits.HUNDREDS_OF_FEET);
        return (new Amount(selv.doubleValue() + ceil.doubleValue(),
                NcUnits.HUNDREDS_OF_FEET));
    }

    /**
     * Computes the wind direction in degrees from the input u and v components
     * of velocity, both of which must be in the same units (either
     * meters/second or knots)
     * 
     * @param uX
     *            - U component of velocity
     * @param vX
     *            - V component of velocity
     * @return The wind direction in degrees
     * @throws NullPointerException
     * 
     */
    public static final Amount prDrct(Amount uX, Amount vX)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prDrct:");
        // System.out.println(" uX = " + uX.doubleValue());
        // System.out.println(" vX = " + vX.doubleValue());

        // checkNullOrInvalidValue(uX);
        // checkNullOrInvalidValue(vX);
        if (!checkNullOrInvalidValue(uX) || !checkNullOrInvalidValue(vX))
            return new Amount(NonSI.DEGREE_ANGLE);

        double vXVal = Double.NaN;
        double uXVal = Double.NaN;
        if (uX.getUnit() != vX.getUnit()
                && uX.getUnit().isCompatible(vX.getUnit())) {
            vXVal = vX.getUnit().getConverterTo(uX.getUnit())
                    .convert(vX.doubleValue());
            vX = new Amount(vXVal, uX.getUnit());
        }
        uXVal = uX.doubleValue();
        double prDrct = 0.0;

        if ((uXVal != 0) || (vXVal != 0)) {
            prDrct = Math.atan2(-uXVal, -vXVal) * GempakConstants.RTD;
            if (prDrct <= 0)
                prDrct += 360;
        }
        return (new Amount(prDrct, NonSI.DEGREE_ANGLE));

    }

    /**
     * Computes the density of dry air given the pressure (in mb) and
     * temperature (in Celsius)
     * 
     * @param pres
     *            - Pressure in millibars
     * @param tmpc
     *            - Temperature in Celsius
     * @return Density of dry air in kg/(m**3)
     * @throws InvalidRangeException
     * @throws NullPointerException
     * 
     */
    public static final Amount prDden(Amount pres, Amount tmpc)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prDden:");
        // System.out.println(" pres = " + pres.doubleValue());
        // System.out.println(" tmpc = " + tmpc.doubleValue());

        double prdden = GempakConstants.RMISSD;
        /* Check for bad data */
        // checkNullOrInvalidValue(pres);
        // checkNullOrInvalidValue(tmpc);

        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(tmpc))
            return new Amount(VolumetricDensity.UNIT);

        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);

        double tmpcVal = tmpc.doubleValue();
        if (tmpcVal < -GempakConstants.TMCK) {
            System.out
                    .println("From prDden: temperature must be greater than or equal to -273.15 ");
            return new Amount(VolumetricDensity.UNIT);
            // throw new
            // InvalidRangeException("From prDden: temperature must be greater than or equal to -273.15 ");
        }
        /* Convert temperature and compute */
        double tmpk = tmpc.getUnit().getConverterTo(SI.KELVIN).convert(tmpcVal);
        prdden = 100 * pres.doubleValue() / (GempakConstants.RDGAS * tmpk);

        return new Amount(prdden, VolumetricDensity.UNIT);
    }

    /***
     * Computes the dewpoint depression, from tmpx and dwpx, both of which must
     * be in the same units (one of Kelvin, Celsius or Farenheit)
     * 
     * @param tmpx
     *            - Air temperature
     * @param dwpx
     *            - Dewpoint temperature
     * @return the dewpoint depresssion (in the same units are tmpx and dwpx) if
     *         both tmpx and dwpx are valid values
     */

    public static final Amount prDdep(Amount tmpx, Amount dwpx)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prDdep:");
        // System.out.println(" tmpx = " + tmpx.doubleValue());
        // System.out.println(" dwpx = " + dwpx.doubleValue());
        // checkNullOrInvalidValue(tmpx);
        // checkNullOrInvalidValue(dwpx);
        if (!checkNullOrInvalidValue(tmpx) || !checkNullOrInvalidValue(dwpx))
            return new Amount(tmpx.getUnit());

        Amount prDdep = null;

        dwpx = checkAndConvertInputAmountToExpectedUnits(dwpx, tmpx.getUnit());
        double dewpointDepression = tmpx.doubleValue() - dwpx.doubleValue();
        prDdep = new Amount(dewpointDepression, tmpx.getUnit());
        return prDdep;
    }

    /**
     * Computes DMAX, the maximum temperature obtained by comparing the 6-hour
     * maximum at 00Z, the 6-hour maximum at 06Z and the local midnight maximum
     * (reported at 00 LST) if either of the 6-hour values is missing, the
     * maximum is set to missing. The inputs are in Celsius, the output in
     * degrees Farenheit
     * 
     * @param t00x
     *            - 6-hour maximum temperature at 00Z, Celsius
     * @param t06x
     *            - 6-hour maximum temperature at 06Z, Celsius
     * @param tdxc
     *            - Local midnight max temperature at 00 LST, Celsius
     * @return The maximum of the 3 temperature values (in Farenheit)
     * @throws NullPointerException
     * 
     */
    public static final Amount prDmax(Amount t00x, Amount t06x, Amount tdxc)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prDmax:");
        // System.out.println(" t00x = " + t00x.doubleValue());
        // System.out.println(" t06x = " + t06x.doubleValue());
        // System.out.println(" tdxc= " + tdxc.doubleValue());

        // checkNullOrInvalidValue(t00x);
        // checkNullOrInvalidValue(t06x);

        if (!checkNullOrInvalidValue(t00x) || !checkNullOrInvalidValue(t06x))
            return new Amount(NonSI.FAHRENHEIT);

        t00x = checkAndConvertInputAmountToExpectedUnits(t00x, SI.CELSIUS);
        t06x = checkAndConvertInputAmountToExpectedUnits(t06x, SI.CELSIUS);

        if (tdxc != null && tdxc.doubleValue() > -9999) {
            tdxc = checkAndConvertInputAmountToExpectedUnits(tdxc, SI.CELSIUS);
            double[] tempArray = { t00x.doubleValue(), t06x.doubleValue(),
                    tdxc.doubleValue() };

            Arrays.sort(tempArray);
            double newTemp = SI.CELSIUS.getConverterTo(NonSI.FAHRENHEIT)
                    .convert(tempArray[2]);
            return (new Amount(newTemp, NonSI.FAHRENHEIT));
        } else {
            double t00xVal = t00x.doubleValue();
            double t06xVal = t06x.doubleValue();
            Amount dmax = (t00xVal > t06xVal ? checkAndConvertInputAmountToExpectedUnits(
                    t00x, NonSI.FAHRENHEIT)
                    : checkAndConvertInputAmountToExpectedUnits(t06x,
                            NonSI.FAHRENHEIT));
            return dmax;
        }

    }

    /**
     * <pre>
     * Computes the minimum temperature obtained by
     * comparing the 6-hour minimum at 12Z and the 6-hour minimum at 18Z.
     * if either of the 6-hour values is missing, the minimum is set to missing.
     * The inputs are in degrees C, the output in degrees F.
     * </pre>
     * 
     * @param t12n
     *            - 6-hour minimum temperature at 12Z, deg Celsius
     * @param t18n
     *            - 6-hour minimum temperature at 18Z, deg Celsius
     * @return the minimum temperature (in Farenheit) after comparing the two
     *         input values, if they exist
     * @throws NullPointerException
     * 
     */
    public static final Amount prDmin(Amount t12n, Amount t18n)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prDmin:");
        // System.out.println(" t12n = " + t12n.doubleValue());
        // System.out.println(" t18n = " + t18n.doubleValue());
        // checkNullOrInvalidValue( t12n );
        // checkNullOrInvalidValue( t18n );
        if (!checkNullOrInvalidValue(t12n) || !checkNullOrInvalidValue(t18n))
            return new Amount(NonSI.FAHRENHEIT);

        t12n = checkAndConvertInputAmountToExpectedUnits(t12n, SI.CELSIUS);
        t18n = checkAndConvertInputAmountToExpectedUnits(t18n, SI.CELSIUS);
        Amount minValue = (t12n.doubleValue() < t18n.doubleValue() ? t12n
                : t18n);
        return (checkAndConvertInputAmountToExpectedUnits(minValue,
                NonSI.FAHRENHEIT));

    }

    /**
     * Computes the dewpoint as the difference between the input temperature and
     * dewpoint depression
     * 
     * @param tmpx
     *            - temperature (in Celsius or Farenheit or Kelvin)
     * @param dpdx
     *            - the dewpoint depression ( in the same units as the
     *            temperature)
     * @return the dewpoint in the same units as ( in the same units as the
     *         temperature)
     * @throws NullPointerException
     * 
     */
    public static final Amount prDwdp(Amount tmpx, Amount dpdx)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prDwdp:");
        // System.out.println(" tmpx = " + tmpx.doubleValue());
        // System.out.println(" dpdx = " + dpdx.doubleValue());

        // checkNullOrInvalidValue(tmpx);
        // checkNullOrInvalidValue(dpdx);

        if (!checkNullOrInvalidValue(tmpx) || !checkNullOrInvalidValue(dpdx))
            return new Amount(tmpx.getUnit());

        Unit<Temperature> tempUnits = (Unit<Temperature>) tmpx.getUnit();
        Unit<Temperature> dewpointDepUnits = (Unit<Temperature>) dpdx.getUnit();
        if (!tempUnits.equals(dewpointDepUnits)
                && tempUnits.isCompatible(dewpointDepUnits)) {
            double dewpointDepValue = (double) dewpointDepUnits.getConverterTo(
                    tempUnits).convert(dpdx.doubleValue());
            dpdx = new Amount(dewpointDepValue, tempUnits);
        }

        Amount dewpointTemperature = new Amount(tmpx.doubleValue()
                - dpdx.doubleValue(), tempUnits);
        return dewpointTemperature;
    }

    /**
     * Computes the dewpoint ( in Celsius ) from the mixing ratio (in
     * grams/kilogram) and the pressure (in mb)
     * 
     * @param rmix
     *            - the mixing ratio (in grams/kilogram)
     * @param pres
     *            - the pressure (in mb)
     * @return the dewpoint (in Celsius), if both the input values are valid
     * @throws InvalidRangeException
     * @throws NullPointerException
     * 
     */
    public static final Amount prDwpt(Amount rmix, Amount pres)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prDwpt:");
        // System.out.println(" rmix = " + rmix.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());
        Amount prDwpt = null;
        // checkNullOrInvalidValue( rmix );
        // checkNullOrInvalidValue( pres );
        if (!checkNullOrInvalidValue(rmix) || !checkNullOrInvalidValue(pres))
            return new Amount(SI.CELSIUS);

        rmix = checkAndConvertInputAmountToExpectedUnits(rmix, Unit.ONE);
        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        double mixingRatioValue = rmix.doubleValue();
        double pressureValue = pres.doubleValue();
        if (mixingRatioValue <= 0) {
            System.out
                    .println("From prDwpt() - mixing ratio must be greater than 0");
            return new Amount(SI.CELSIUS);
            // throw new
            // InvalidRangeException("From prDwpt() - mixing ratio must be greater than 0");
        }
        if (pressureValue <= 0) {
            System.out.println("From prDwpt() - pres must be greater than 0");
            return new Amount(SI.CELSIUS);
            // throw new
            // InvalidRangeException("From prDwpt() - pres must be greater than 0");
        }
        /* Convert gram/kilogram to gram/gram */
        double ratio = mixingRatioValue / 1000;

        /* Calculate vapor pressure from mixing ratio and pressure */
        double vaporPressure = (pressureValue * ratio) / (0.62197 + ratio);

        /* Correct vapor pressure */
        vaporPressure = vaporPressure
                / (1.001 + ((pressureValue - 100.0) / 900) * .0034);

        /* Calculate dewpoint */
        double dewPointValue = (double) (Math.log(vaporPressure / 6.112) * 243.5 / (17.67 - Math
                .log(vaporPressure / 6.112)));
        prDwpt = new Amount(dewPointValue, SI.CELSIUS);
        return prDwpt;
    }

    /***
     * Computes the Fosberg index from the temperature, relative humidity and
     * wind speed at the surface.
     * 
     * @param tmpc
     *            - Temperature in Celsius
     * @param relh
     *            - Relative humidity in percent
     * @param sped
     *            - Wind speed in meters/second
     * @return the Fosberg index
     * @throws NullPointerException
     * 
     */
    public static final Amount prFosb(Amount tmpc, Amount relh, Amount sped)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prFosb:");
        // System.out.println("Temperature ( in K ) is: " + tmpc.doubleValue());
        // System.out.println("Relative Humidity is: " + relh.doubleValue());
        // System.out.println("Wind Speed is: " + sped.doubleValue());
        // checkNullOrInvalidValue( tmpc );
        // checkNullOrInvalidValue( relh );
        // checkNullOrInvalidValue( sped );
        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(relh)
                || !checkNullOrInvalidValue(sped))
            return new Amount(Unit.ONE);

        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        relh = checkAndConvertInputAmountToExpectedUnits(relh, NonSI.PERCENT);
        sped = checkAndConvertInputAmountToExpectedUnits(sped,
                SI.METERS_PER_SECOND);

        /* Change temperature to degrees Fahrenheit */
        double tf = tmpc.getUnit().getConverterTo(NonSI.FAHRENHEIT)
                .convert(tmpc.doubleValue());

        /* Convert wind speed from meters/second to knots */
        double smph = sped.getUnit().getConverterTo(NonSI.MILES_PER_HOUR)
                .convert(sped.doubleValue());

        double A = 0.03229;
        double B = 0.281073;
        double C = 0.000578;
        double D = 2.22749;
        double E = 0.160107;
        double F = 0.014784;
        double G = 21.0606;
        double H = 0.005565;
        double P = 0.00035;
        double Q = 0.483199;
        double R = 0.3002;
        // float T = 9.0f/5.0f;
        // float U = 1.9425f;
        // float V = 0.868976f;
        double fw = GempakConstants.RMISSD;
        double relhVal = relh.doubleValue();
        if (relhVal <= 10) {
            fw = A + B * relhVal - C * relhVal * tf;
        } else if (relhVal <= 50) {
            fw = D + E * relhVal - F * tf;
        } else {
            fw = G + H * relhVal * relhVal - P * relhVal * tf - Q * relhVal;
        }

        double sss = (double) (Math.sqrt(1. + (smph * smph)));
        double fwd = fw / 30;
        double fwd2 = fwd * fwd;
        double fwd3 = fwd2 * fwd;
        double fire = 1 - 2 * fwd + 1.5f * fwd2 - 0.5f * fwd3;

        /* Find the Fosberg Index */

        double prfosb = (fire * sss) / R;

        return (new Amount(prfosb, Unit.ONE));
    }

    /**
     * <pre>
     * Computes the Southern Region/CPC Rothfusz heat index.
     * 
     * The Rothfusz regression is optimal for TMPF > ~80 and RELH > ~40%.   
     * This code applies a simple heat index formula and then resorts to   
     * the Rothfusz regression only if the simple heat index exceeds 80,   
     * implying temperatures near, but slightly below 80.  To make the    
     * simple calculation continuous with the values obtained from the   
     * Rothfusz regression, the simple result is averaged with TMPF in    
     * computing the simple heat index value.                               
     * Source:  NWS Southern Region SSD Technical Attachment SR 90-23  7/1/90.  
     * Heat Index was originally known as the apparent temperature index 
     * (Steadman, JAM, July, 1979).                                                                  
     * This code includes adjustments made by the CPC for low RELH at high  
     * TMPF and high RELH for TMPF in the mid 80's.
     * 
     * 
     * @param tmpf  - the input air temperature
     * @param relh   - the relative humidity 
     * @return the heat index (in deg Farenheit) if both 
     * the input air temperature and relative humidity
     * are valid values
     * </pre>
     * 
     * @throws NullPointerException
     * 
     */
    public static final Amount prHeat(Amount tmpf, Amount relh)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prHeat:");
        // System.out.println(" tmpf = " + tmpf.doubleValue());
        // System.out.println(" relh = " + relh.doubleValue());

        double prheat = GempakConstants.RMISSD;
        // checkNullOrInvalidValue( tmpf );
        // checkNullOrInvalidValue( relh );
        if (!checkNullOrInvalidValue(tmpf) || !checkNullOrInvalidValue(relh))
            return new Amount(Unit.ONE);

        tmpf = checkAndConvertInputAmountToExpectedUnits(tmpf, NonSI.FAHRENHEIT);
        relh = checkAndConvertInputAmountToExpectedUnits(relh, NonSI.PERCENT);
        double tmpfVal = tmpf.doubleValue();
        double relhVal = relh.doubleValue();
        /*
         * If the temperature is less than 40 degrees, set the heat index to the
         * temperature
         */
        if (tmpfVal <= 40)
            prheat = tmpfVal;
        else {
            /*
             * Compute a simple heat index. If the value is less than 80 deg F
             * use it
             */
            prheat = (float) (61 + (tmpfVal - 68) * 1.2 + relhVal * 0.094);
            prheat = (float) ((tmpfVal + prheat) * 0.5);
            /* Else compute the full regression value */
            if (prheat >= 80.0) {
                double t2 = tmpfVal * tmpfVal;
                double r2 = relhVal * relhVal;
                prheat = (float) (-42.379 + 2.04901523 * tmpfVal + 10.14333127
                        * relhVal - 0.22475541 * tmpfVal * relhVal - 0.00683783
                        * t2 - 0.05481717 * r2 + 0.00122874 * t2 * relhVal
                        + 0.00085282 * tmpfVal * r2 - 0.00000199 * t2 * r2);
                /*
                 * Adjust for high regression at low relative humidity for
                 * temperatures above 80 degrees F.
                 */
                if ((relhVal <= 13.0)
                        && ((tmpfVal >= 80.0) && (tmpfVal <= 112.0))) {
                    float adj1 = (float) ((13. - relhVal) / 4);
                    float adj2 = (float) (Math.sqrt((17 - Math
                            .abs(tmpfVal - 95)) / 17));
                    float adj = adj1 * adj2;
                    prheat -= adj;
                }
                /*
                 * Adjust for low regression at high relative humidity and
                 * temperatures in the mid-80s
                 */
                else if ((relhVal > 85)
                        && ((tmpfVal >= 80.0) && (tmpfVal <= 87.0))) {
                    float adj1 = (float) ((relhVal - 85.0) / 10.0);
                    float adj2 = (float) ((87.0 - tmpfVal) / 5);
                    float adj = adj1 * adj2;
                    prheat += adj;
                }
            }
        }
        return (new Amount(prheat, NonSI.FAHRENHEIT));
    }

    /**
     * Computes the humiture index from the air temperature and the dew point
     * temperature using the equation: PR_HMTR = TMPF + ( PR_VAPR ( DWPC ) - 21
     * )
     * 
     * @param tmpf
     *            - the air temperature (in Farenheit)
     * @param dwpf
     *            - the dew point (in Farenheit)
     * @return the humiture index if both the air temperature and the dewpoint
     *         temperature are valid values
     * @throws InvalidRangeException
     * @throws NullPointerException
     * 
     */
    public static final Amount prHmtr(Amount tmpf, Amount dwpf)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prHmtr:");
        // System.out.println(" tmpf = " + tmpf.doubleValue());
        // System.out.println(" dwpf = " + dwpf.doubleValue());

        double prhmtr = GempakConstants.RMISSD;
        if (!checkNullOrInvalidValue(tmpf) || !checkNullOrInvalidValue(dwpf))
            return new Amount(Unit.ONE);

        tmpf = checkAndConvertInputAmountToExpectedUnits(tmpf, NonSI.FAHRENHEIT);
        dwpf = checkAndConvertInputAmountToExpectedUnits(dwpf, NonSI.FAHRENHEIT);

        Amount dwpc = checkAndConvertInputAmountToExpectedUnits(dwpf,
                SI.CELSIUS);
        Amount vapr = prVapr(dwpc);
        if (!checkNullOrInvalidValue(vapr))
            return new Amount(Unit.ONE);

        prhmtr = tmpf.doubleValue() + (vapr.doubleValue() - 21);

        return (new Amount(prhmtr, Unit.ONE));
    }

    /**
     * Computes the rate of ice accretion/growth of ice on a vessel in salt
     * water, in units of inches per 3 hours (the WMO standard) The formula used
     * is IGRO = ( A*pr + B*pr*pr + *pr*pr*pr ) * CVFAC where A = 2.73 * 10e-2 B
     * = 2.91 * 10e-4 C = 1.84 * 10e-6 pr = ( sped * ( -1.7 - tmpc ) ) / ( 1 +
     * 0.4 * ( sstc + 1.7 ) ) (priesendorfer regression) and CVFAC = 1.1811, to
     * convert cm/hr to in/3hr.
     * 
     * @param tmpc
     *            - the observed surface air temperature in Celsius
     * @param sstc
     *            - the observed surface sea temperature in Celsius
     * @param sped
     *            - the observed wind speed
     * @return the rate of ice growth if all the input values are valid and lie
     *         between specific limits and if the rate of ice growth that is
     *         computed is greater than or equal to 0,
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prIgro(Amount tmpc, Amount sstc, Amount sped)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prIgro:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" sstc = " + sstc.doubleValue());
        // System.out.println(" sped = " + sped.doubleValue());

        double prigro = GempakConstants.RMISSD;
        // checkNullOrInvalidValue( tmpc ) ;
        // checkNullOrInvalidValue( sstc ) ;
        // checkNullOrInvalidValue( sped ) ;

        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(sstc)
                || !checkNullOrInvalidValue(sped))
            return new Amount(NcUnits.INCHES_PER_THREE_HOURS);

        checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        checkAndConvertInputAmountToExpectedUnits(sstc, SI.CELSIUS);

        // TODO: verify that wind speed can have any unit
        double tmpcVal = tmpc.doubleValue();
        double sstcVal = sstc.doubleValue();
        double spedVal = sped.doubleValue();

        /* Check that these values are within the valid range */

        if (spedVal < 0 || spedVal > 50) {
            // throw new
            // InvalidRangeException("The wind speed must lie between 0 and 50. Both limits inclusive");
            System.out
                    .println("The wind speed must lie between 0 and 50. Both limits inclusive");
            return new Amount(NcUnits.INCHES_PER_THREE_HOURS);
        }
        if (tmpcVal < -20 || tmpcVal > 0) {
            // throw new
            // InvalidRangeException("The observed surface air temperature must lie between -20 and 0. Both limits inclusive");
            System.out
                    .println("The observed surface air temperature must lie between -20 and 0. Both limits inclusive");
            return new Amount(NcUnits.INCHES_PER_THREE_HOURS);
        }
        if (sstcVal < -1.7f || sstcVal > 12) {
            // throw new
            // InvalidRangeException("The observed surface sea temperature must lie between -1.7 and 12. Both limits inclusive");
        }
        double A = 0.0273f;
        double B = 0.000291f;
        double C = 0.00000184f;
        double cvfac = 1.1811f; // to convert cm/hr to in per 3 hours
        double pr = ((spedVal * (-1.7 - tmpcVal)) / (1 + 0.4 * (sstcVal + 1.7))); // Compute
                                                                                  // the
                                                                                  // Priesendorfer
                                                                                  // regression
        double pr2 = pr * pr;
        prigro = (A * pr + B * pr2 + C * pr * pr2) * cvfac;
        if (prigro < 0) {
            // throw new
            // InvalidRangeException("The rate of ice growth must be greater than or equal to 0");
            System.out
                    .println("The rate of ice growth must be greater than or equal to 0");
            return new Amount(NcUnits.INCHES_PER_THREE_HOURS);
        }
        return (new Amount(prigro, NcUnits.INCHES_PER_THREE_HOURS));
    }

    /**
     * Computes the latent heat of vaporization at constant pressure from the
     * input temperature (in Celsius) using the equation: LHVP = ( 2.500 -
     * .00237 * TMPC ) * 10E6 LHVP is in J/kg.
     * 
     * @param tmpc
     *            - the input temperature (in Celsius)
     * @return the latent heat of vaporization at constant pressure if the input
     *         temperature is valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prLhvp(Amount tmpc)
            throws InvalidValueException, NullPointerException {

        // System.out.println("From prLhvp:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());

        // checkNullOrInvalidValue(tmpc);
        if (!checkNullOrInvalidValue(tmpc))
            return new Amount(NcUnits.JOULES_PER_KILOGRAM);

        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        double latentHeatOfVapr = (float) ((2.500 - 0.00237 * tmpc
                .doubleValue()) * 1000000);
        return (new Amount(latentHeatOfVapr, NcUnits.JOULES_PER_KILOGRAM));
    }

    /**
     * Computes the temperature of a parcel lifted (or sunk) adiabatically to a
     * given pressure.
     * 
     * @param thta
     *            - Potential temperature in Kelvin
     * @param thte
     *            - Equivalent potential temp in Kelvin
     * @param pres
     *            - Lifted pressure in millibar
     * @return the lifted temperature in Celsius, if all the input parameters
     *         are valid
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prLtmp(Amount thta, Amount thte, Amount pres)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prLtmp:");
        // System.out.println(" thta = " + thta.doubleValue());
        // System.out.println(" thte = " + thte.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());

        double prltmp = GempakConstants.RMISSD;
        // checkNullOrInvalidValue(thta);
        // checkNullOrInvalidValue(thte);
        // checkNullOrInvalidValue(pres);

        if (!checkNullOrInvalidValue(thta) || !checkNullOrInvalidValue(thte)
                || !checkNullOrInvalidValue(pres))
            return new Amount(SI.CELSIUS);

        thta = checkAndConvertInputAmountToExpectedUnits(thta, SI.KELVIN);
        thte = checkAndConvertInputAmountToExpectedUnits(thte, SI.KELVIN);

        if (pres.doubleValue() <= 0)
            pres = new Amount(500, NcUnits.MILLIBAR);

        /* Compute parcel temperatures on moist and dry adiabats */
        Amount tmpe = prTmst(thte, pres, new Amount(0, SI.KELVIN));
        Amount tmpd = prTmpk(pres, thta);
        checkNullOrInvalidValue(tmpe);
        checkNullOrInvalidValue(tmpd);
        /*
         * ( Non-Javadoc ) The correct parcel temperature is the warmer of the
         * temperature on the dry adiabat and the temperature on the moist
         * adiabat.
         */

        double tmpeVal = tmpe.doubleValue();
        double tmpdVal = tmpd.doubleValue();
        if (tmpeVal > tmpdVal) {
            prltmp = SI.KELVIN.getConverterTo(SI.CELSIUS).convert(tmpeVal);
        } else {
            prltmp = SI.KELVIN.getConverterTo(SI.CELSIUS).convert(tmpdVal);
        }

        return (new Amount(prltmp, SI.CELSIUS));
    }

    /**
     * Computes the mountain obscuration threshold met indicator
     * 
     * @param cmsl
     *            - Ceiling converted to MSL in 100's of ft
     * @param otval
     *            - Mountain obscuration threshold in 100's of ft
     * @return The mountain obscuration threshold met indicator if the input
     *         values are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prMobs(Amount cmsl, Amount otval)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prMobs:");
        // System.out.println(" cmsl = " + cmsl.doubleValue());
        // System.out.println(" otval = " + otval.doubleValue());

        // checkNullOrInvalidValue( cmsl );
        // checkNullOrInvalidValue( otval );

        if (!checkNullOrInvalidValue(cmsl) || !checkNullOrInvalidValue(otval))
            return new Amount(Unit.ONE);

        cmsl = checkAndConvertInputAmountToExpectedUnits(cmsl,
                NcUnits.HUNDREDS_OF_FEET);
        otval = checkAndConvertInputAmountToExpectedUnits(otval,
                NcUnits.HUNDREDS_OF_FEET);
        return (cmsl.doubleValue() < otval.doubleValue() ? new Amount(1,
                Unit.ONE) : new Amount(1, Unit.ONE));
    }

    /**
     * Computes the mixing ratio in grams/kilograms from the dewpoint ( in
     * Celsius ) and the pressure ( in mb) using the equation: MIXR = .62197 * (
     * e / ( PRES - e ) ) * 1000. where e = VAPR * corr corr = (1.001 + ( ( PRES
     * - 100. ) / 900. ) * .0034) ( University of Wisconsin green sheet ). This
     * method can also be used for the folloiwng computations: MIXS from TMPC
     * and PRES SMXR from DWPC and PALT SMXS from TMPC and PALT
     * 
     * @param dwpc
     *            - the dewpoint ( in Celsius )
     * @param pres
     *            - the pressure ( in mb)
     * @return the missing ratio ( in grams / kilograms ) if both the input
     *         parameters are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prMixr(Amount dwpc, Amount pres) {
        // System.out.println("From prMixr:");
        // System.out.println(" dwpc = " + dwpc.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());

        Amount prmixr = new Amount(-9999.0, Unit.ONE);
        // checkNullOrInvalidValue(pres);
        // checkNullOrInvalidValue(dwpc);

        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(dwpc))
            return new Amount(NcUnits.GRAMS_PER_KILOGRAM);

        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        dwpc = checkAndConvertInputAmountToExpectedUnits(dwpc, SI.CELSIUS);

        /* Calculate vapor pressure */
        Amount vapr = prVapr(dwpc);

        if (!checkNullOrInvalidValue(vapr))
            return new Amount(NcUnits.GRAMS_PER_KILOGRAM);

        vapr = checkAndConvertInputAmountToExpectedUnits(vapr, NcUnits.MILLIBAR);
        double pressureValue = pres.doubleValue();
        double vaporPressureValue = vapr.doubleValue();
        /*
         * (Non-Javadoc) corr is a correction to the vapor pressure since the
         * atmosphere is not an ideal gas.
         */
        double corr = (double) (1.001 + ((pressureValue - 100) / 900) * 0.0034);
        double e = corr * vaporPressureValue;

        /*
         * Test for unphysical case of large E at low PRES
         */
        if (e <= (0.5 * pressureValue)) {
            /* Calculate mixing ratio */
            prmixr = new Amount(
                    (double) (0.62197 * (e / (pressureValue - e)) * 1000),
                    NcUnits.GRAMS_PER_KILOGRAM);
        }
        return prmixr;
    }

    /**
     * Extracts the pressure change ( in millibars ) from the pressure tendency
     * information
     * 
     * @param p03d
     *            - Pressure tendency information
     * @return the pressure change ( in mb )
     * @throws NullPointerException
     * 
     */
    // TODO : remove it to make it a part of display options or let it stay?
    public static final Amount prP03c(Amount p03d)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prP03c:");
        // System.out.println(" p03d = " + p03d.doubleValue());

        double prp03c = GempakConstants.RMISSD;
        // checkNullOrInvalidValue( p03d );
        if (!checkNullOrInvalidValue(p03d))
            return new Amount(NcUnits.MILLIBAR);

        double p03dVal = p03d.doubleValue();
        float[] psign = { 1, 1, 1, 1, 0, -1, -1, -1, -1 };
        int itendc = (int) (p03dVal / 1000);
        float ptend = (float) (((int) p03dVal) % 1000) / 10f;
        // TODO: compare tests with legacy
        if (itendc < psign.length)
            prp03c = psign[itendc] * ptend;

        return (new Amount(prp03c, NcUnits.MILLIBAR));
    }

    /**
     * Computes station pressure from altimeter and station elevation using the
     * equation PALT = ALTM * ( 1 - ( SELK * GAMUSD / To ) ) ** expo where SELK
     * = SELV / 1000 To = US Std. Atmos. sea level temp in Kelvin = TMCK + 15
     * expo = GRAVTY / ( GAMUSD * RDGAS ) * 1000 Wallace and Hobbs.
     * 
     * @param altm
     *            - Altimeter in millibars
     * @param selv
     *            - Station elevation in meters
     * @return the pressure in millibars if none of the input values are missing
     * @throws NullPointerException
     * 
     */
    public static final Amount prPalt(Amount altm, Amount selv)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prPalt:");
        // System.out.println(" altm = " + altm.doubleValue());
        // System.out.println(" selv = " + selv.doubleValue());

        // checkNullOrInvalidValue( altm );
        // checkNullOrInvalidValue( selv );

        if (!checkNullOrInvalidValue(altm) || !checkNullOrInvalidValue(selv))
            return new Amount(NcUnits.MILLIBAR);

        altm = checkAndConvertInputAmountToExpectedUnits(altm, NcUnits.MILLIBAR);
        selv = checkAndConvertInputAmountToExpectedUnits(selv, SI.METER);
        double hgtk = selv.getUnit().getConverterTo(SI.KILOMETER)
                .convert(selv.doubleValue());

        /* Calculate the exponent */
        double expo = (GempakConstants.GRAVTY
                / (GempakConstants.GAMUSD * GempakConstants.RDGAS) * 1000.0f);

        /* Calculate pressure */
        double prpalt = (altm.doubleValue() * Math.pow((1 - (hgtk
                * GempakConstants.GAMUSD / (GempakConstants.TMCK + 15))), expo));

        return (new Amount(prpalt, NcUnits.MILLIBAR));
    }

    /**
     * Computes the lifted condensation level pressure ( in mb ) for a parcel of
     * air from TMPC, PRES, and TLCL. TLCL may be computed using PR_TLCL. The
     * equation used is a modified Poisson equation: PLCL = PRES * ( TLCL / TMPK
     * ) ** ( 1 / RKAPPA )
     * 
     * @param tmpc
     *            - Temperature ( in Celsius ) before lifting the air parcel
     * @param pres
     *            - Pressure ( in mb ) before lifting the air parcel
     * @param tlcl
     *            - Temperature ( in Kelvin ) at the lifted condensation level
     * @return the pressure at the lifted condensation level, if all the inputs
     *         are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prPlcl(Amount tmpc, Amount pres, Amount tlcl)
            throws InvalidValueException, NullPointerException {
        double prplcl = GempakConstants.RMISSD;
        // System.out.println("From prPlcl:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());
        // System.out.println(" tlcl = " + tlcl.doubleValue());

        // checkNullOrInvalidValue(tmpc);
        // checkNullOrInvalidValue(pres);
        // checkNullOrInvalidValue(tlcl);
        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(pres)
                || !checkNullOrInvalidValue(tlcl))
            return new Amount(NcUnits.MILLIBAR);

        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        tlcl = checkAndConvertInputAmountToExpectedUnits(tlcl, SI.KELVIN);
        Amount tmpk = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.KELVIN);
        double tclValue = tlcl.doubleValue();
        double tmpkValue = tmpk.doubleValue();
        double presValue = pres.doubleValue();
        prplcl = (double) (presValue * Math.pow((tclValue / tmpkValue),
                (1 / GempakConstants.RKAPPA)));
        return new Amount(prplcl, NcUnits.MILLIBAR);
    }

    /**
     * <pre>
     *  Computes the mean sea level pressure ( in mb ) from the station pressure ( in mb ),
     *  the temperature ( in deg Celsius), the dewpoint ( in deg Celsius ) and 
     *  the station elevation ( in meters ) using the equation:
     *  	PMSL = PRES * EXP ( ( GRAVTY * SELV ) / ( RDGAS * TVAVE ) ) 
     *     where
     *     		 TVAVE = avg virtual temp between station and sea level 
     *     		        = TVRK + ( DELTV / 2 )
     *     		 DELTV = GAMUSD * SELV / 1000
     *  Wallace and Hobbs.
     * @param pres - the station pressure ( in mb )
     * @param tmpc - the temperature ( in deg Celsius)
     * @param dwpc - the dewpoint ( in deg Celsius )
     * @param selv - the station elevation ( in meters )
     * @return the mean sea level pressure ( in mb ) if all the inputs are valid
     * </pre>
     * 
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prPmsl(Amount pres, Amount tmpc, Amount dwpc,
            Amount selv) throws InvalidValueException, NullPointerException {
        // System.out.println("From prPmsl:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());
        // System.out.println(" dwpc = " + dwpc.doubleValue());
        // System.out.println(" selv = " + selv.doubleValue());
        // checkNullOrInvalidValue( pres );
        // checkNullOrInvalidValue( tmpc );
        // checkNullOrInvalidValue( dwpc );
        // checkNullOrInvalidValue( selv );
        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(pres)
                || !checkNullOrInvalidValue(dwpc)
                || !checkNullOrInvalidValue(selv))
            return new Amount(NcUnits.MILLIBAR);

        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        tmpc = checkAndConvertInputAmountToExpectedUnits(pres, SI.CELSIUS);
        dwpc = checkAndConvertInputAmountToExpectedUnits(pres, SI.CELSIUS);
        selv = checkAndConvertInputAmountToExpectedUnits(pres, SI.METER);

        /* Calculate virtual temperature */
        Amount tv = prTvrk(tmpc, dwpc, pres);

        /* deltaV and tVave */
        double selvVal = selv.doubleValue();
        double deltaV = selvVal * GempakConstants.GAMUSD / 1000;
        double tVave = tv.doubleValue() + (deltaV / 2);
        double mathFormula = (GempakConstants.GRAVTY * selvVal)
                / (GempakConstants.RDGAS * tVave);
        double prpmsl = (pres.doubleValue() * Math.exp(mathFormula));

        return (new Amount(prpmsl, NcUnits.MILLIBAR));
    }

    /**
     * Computes the maximum precipitation amount for upto 4 preciptiation values
     * in inches
     * 
     * @param p01
     *            - First precipitation amount
     * @param p02
     *            - Second precipitation amount
     * @param p03
     *            - Third precipitation amount
     * @param p04
     *            - Fourth precipitation amount
     * @return the maximum precipitation
     * @throws NullPointerException
     * 
     */

    public static final Amount prPr6x(Amount p01, Amount p02, Amount p03,
            Amount p04) throws InvalidValueException, NullPointerException {
        // System.out.println("From prPr6x:");
        // System.out.println(" p01 = " + p01.doubleValue());
        // System.out.println(" p02 = " + p02.doubleValue());
        // System.out.println(" p03 = " + p03.doubleValue());
        // System.out.println(" p04 = " + p04.doubleValue());
        Amount[] tempArray = { p01, p02, p03, p04 };
        int index = 0;
        double[] tempDblArray = new double[4];
        for (Amount thisAmount : tempArray) {
            if (!checkNullOrInvalidValue(thisAmount)) {
                return new Amount(NonSI.INCH);
            }

            if (thisAmount.getUnit() != NonSI.INCH) {
                thisAmount = checkAndConvertInputAmountToExpectedUnits(
                        thisAmount, NonSI.INCH);
                tempArray[index] = thisAmount;
            }
            tempDblArray[index] = thisAmount.doubleValue();
            index++;
        }

        Arrays.sort(tempDblArray);
        return (new Amount(tempDblArray[3], NonSI.INCH));
    }

    /**
     * Computes PR24, the 24-hour precipitation calculated by summing four
     * 6-hour precipitation values
     * 
     * @param p01
     *            - First 6-hour precipitation amount
     * @param p02
     *            - Second 6-hour precipitation amount
     * @param p03
     *            - Third 6-hour precipitation amount
     * @param p04
     *            - Fourth 6-hour precipitation amount
     * @return the total 24-hour precipitation amount
     * @throws InvalidRangeException
     * @throws NullPointerException
     * 
     */
    public static final Amount prPr24(Amount p01, Amount p02, Amount p03,
            Amount p04) throws InvalidValueException, NullPointerException {
        // System.out.println("From prPr24:");
        // System.out.println(" p01 = " + p01.doubleValue());
        // System.out.println(" p02 = " + p02.doubleValue());
        // System.out.println(" p03 = " + p03.doubleValue());
        // System.out.println(" p04 = " + p04.doubleValue());

        // checkNullOrInvalidValue( p01 );
        // checkNullOrInvalidValue( p02 );
        // checkNullOrInvalidValue( p03 );
        // checkNullOrInvalidValue( p04 );

        if (!checkNullOrInvalidValue(p01) || !checkNullOrInvalidValue(p02)
                || !checkNullOrInvalidValue(p03)
                || !checkNullOrInvalidValue(p04))
            return new Amount(NonSI.INCH);

        Amount[] tempArray = { p01, p02, p03, p04 };
        Arrays.sort(tempArray);

        Amount p24 = tempArray[3];
        double p01Val = p01.doubleValue();
        double p02Val = p02.doubleValue();
        double p03Val = p03.doubleValue();
        double p04Val = p04.doubleValue();
        double p24Val = p24.doubleValue();

        if (p24Val > 0) {
            p24Val = 0;
            if (p01Val > 0)
                p24Val += p01Val;

            if (p02Val > 0)
                p24Val += p01Val;

            if (p03Val > 0)
                p24Val += p01Val;

            if (p04Val > 0)
                p24Val += p01Val;

        }

        if (p24Val < 0) {
            // throw new
            // InvalidRangeException("From prPr24: the total 24 hour precipitation amount cannot be less than 0 inches");
            System.out
                    .println("From prPr24: the total 24 hour precipitation amount cannot be less than 0 inches");
            return new Amount(NonSI.INCH);
        }
        return (new Amount(p24Val, NonSI.INCH));
    }

    /**
     * Computes the station pressure ( in mb ) from the temperature ( in deg
     * Celsius ) and the potential temperature ( in Kelvin ) using Poisson's
     * equation: PRES = 1000. * ( PR_TMCK (TMPC) / THTA ) ** (1 / RKAPPA)
     * 
     * @param tmpc
     *            - temperature (in deg Celsius)
     * @param thta
     *            - potential temperature ( in Kelvin )
     * @return the station pressure ( in mb ) if both the inputs are valid
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prPres(Amount tmpc, Amount thta)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prPres:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" thta = " + thta.doubleValue());

        // checkNullOrInvalidValue( tmpc );
        // checkNullOrInvalidValue( thta );

        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(thta))
            return new Amount(NcUnits.MILLIBAR);

        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        thta = checkAndConvertInputAmountToExpectedUnits(thta, SI.KELVIN);
        double tmpcVal = tmpc.doubleValue();
        double thtaVal = thta.doubleValue();
        if (tmpcVal <= -GempakConstants.TMCK) {
            // throw new
            // InvalidRangeException("From prPres: the temperature must be greater than -273.15");
            System.out
                    .println("From prPres: the temperature must be greater than -273.15");
            return new Amount(NcUnits.MILLIBAR);
        }
        if (thtaVal <= 0) {
            // throw new
            // InvalidRangeException("From prPres: the potential temperature must be greater than 0");
            System.out
                    .println("From prPres: the potential temperature must be greater than 0");
            return new Amount(NcUnits.MILLIBAR);
        }
        double tmpkVal = tmpc.getUnit().getConverterTo(SI.KELVIN)
                .convert(tmpcVal);
        double prpres = (float) (1000 * Math.pow(tmpkVal / thtaVal,
                1 / GempakConstants.RKAPPA));

        return (new Amount(prpres, NcUnits.MILLIBAR));
    }

    /**
     * Extracts the symbol code from the pressure tendency information. The code
     * number is returned follow by 999 so that the output is a 4-digit number.
     * 
     * @param p03d
     *            - the pressure tendency information
     * @return the pressure tendency symbol code if the input is valid
     * @throws NullPointerException
     * 
     */
    // TODO add it to the Met Parameters or remove it and make it part of the
    // display options instead?
    public static final Amount prPtsy(Amount p03d)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prPtsy:");
        // System.out.println(" p03d = " + p03d.doubleValue());

        if (!checkNullOrInvalidValue(p03d))
            return new Amount(Unit.ONE);

        double p03dVal = p03d.doubleValue();
        int prptsy = -9999;
        if (!(p03dVal < 0) & !(p03dVal >= 9000)) {
            prptsy = ((int) (p03dVal / 1000)) * 1000 + 999;
        }
        return (new Amount(prptsy, Unit.ONE));
    }

    /**
     * Computes the relative humidity ( in percent ) from the input temperature
     * and dewpoint using the equation: RELH = VAPR / VAPS * 100 where VAPR =
     * vapor pressure = PR_VAPR ( DWPC ) VAPS = saturation vapor pressure =
     * PR_VAPR ( TMPC )
     * 
     * @param tmpc
     *            - temperature ( in Celsius )
     * @param dwpc
     *            - dewpoint ( in Celsius)
     * @return the relative humidity ( in percent ) if both inputs are valid and
     *         RMISSD ( -9999.0 ) otherwise
     * @throws NullPointerException
     * 
     */
    public static final Amount prRelh(Amount tmpc, Amount dwpc)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prRelh:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" dwpc = " + dwpc.doubleValue());

        double prrelh = GempakConstants.RMISSD;
        // checkNullOrInvalidValue(tmpc);
        // checkNullOrInvalidValue(dwpc);

        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(dwpc))
            return new Amount(NonSI.PERCENT);

        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        dwpc = checkAndConvertInputAmountToExpectedUnits(dwpc, SI.CELSIUS);

        /* Find the vapor pressure */
        Amount e = prVapr(dwpc);

        if (!checkNullOrInvalidValue(e))
            return new Amount(NonSI.PERCENT);

        /* Find the saturated vapor pressure */
        Amount es = prVapr(tmpc);

        if (!checkNullOrInvalidValue(es))
            return new Amount(NonSI.PERCENT);

        /* Calculate humidity */
        prrelh = (e.doubleValue() / es.doubleValue()) * 100;

        return new Amount(prrelh, NonSI.PERCENT);
    }

    /**
     * Computes the dewpoint (in Celsius) from the temperature ( in Celsius )
     * and the relative humidity ( in percent ).
     * 
     * @param tmpc
     *            - the temperature ( in deg Celsius )
     * @param relh
     *            - the relative humidity ( in percent )
     * @return the dewpoint in ( deg Celsius), if both inputs are valid and the
     *         value of the vapor pressure computed is greater than ( 1*
     *         e^(-30))
     * @throws InvalidRangeException
     * @throws NullPointerException
     * 
     */
    public static final Amount prRhdp(Amount tmpc, Amount relh)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prRhdp:");
        // System.out.println(" tmpc =" + tmpc.doubleValue());
        // System.out.println(" relh = " + relh.doubleValue());

        // checkNullOrInvalidValue(tmpc);
        // checkNullOrInvalidValue(relh);

        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(relh))
            return new Amount(SI.CELSIUS);

        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        relh = checkAndConvertInputAmountToExpectedUnits(relh, NonSI.PERCENT);

        /* Calculate saturation vapor pressure; test for existence */
        Amount vaps = prVapr(tmpc);

        if (!checkNullOrInvalidValue(vaps))
            return new Amount(SI.CELSIUS);

        /* Calculate vapor pressure */
        double relativeHumidity = relh.doubleValue();
        double saturationVaporPressure = vaps.doubleValue();
        double vapr = relativeHumidity * saturationVaporPressure / 100;

        /* Calculate dewpoint. The VAPR test prevents LOG blowups */
        double prrhdp = -191;
        Amount dewpointAmount = null;
        if (vapr >= (Math.pow(Math.E, -30))) {// legacy checks for 1.E-30
            prrhdp = (double) (243.5 * (Math.log(6.112) - Math.log(vapr)) / (Math
                    .log(vapr) - Math.log(6.112) - 17.67));

            /*
             * If the dew-point is less than -190 degrees C, it is treated as
             * missing data Note: Legacy documents it but does not implement it.
             * However, in CAVE, it was decided to implement it.
             */

            if (prrhdp < -190) {
                System.out
                        .println(" From prRhdp: dewpoint is less than -190 C");
                return new Amount(SI.CELSIUS);
            }
        }
        dewpointAmount = new Amount(prrhdp, SI.CELSIUS);
        return dewpointAmount;
    }

    public static class RZLL // implements ISerializableObject
    {
        // @DynamicSerializeElement
        private static RZLL rzll;

        /** Station latitude in degrees */
        // @DynamicSerializeElement
        Amount stltdg = null;

        /** Station longitude in degrees */
        // @DynamicSerializeElement
        Amount stlndg = null;

        /** Range in kilometers */
        // @DynamicSerializeElement
        Amount range = null;

        /** Geographic azimuth in radians */
        // @DynamicSerializeElement
        Amount azim = null;

        /** Height above the ground in kilometers */
        // @DynamicSerializeElement
        Amount hght = null;

        /** Latitude in degrees */
        // @DynamicSerializeElement
        Amount xlat = null;

        /** Longitude in degrees */
        // @DynamicSerializeElement
        Amount xlon = null;

        /**
         * @return the xlat
         */
        public Amount getXlat() {
            return xlat;
        }

        /**
         * @return the xlon
         */
        public Amount getXlon() {
            return xlon;
        }

        private static RZLL getInstance() {
            if (rzll == null) {
                rzll = new RZLL();
            }
            return rzll;
        }

        /**
         * Computes the actual latitude/longitude given the station
         * latitude/longitude, elevation and azimuth. It uses equations
         * developed for use in the AOIPS radar.
         * 
         * @param instltdg
         *            - Station latitude in degrees
         * @param instlndg
         *            - Station longitude in degrees
         * @param inrange
         *            - Range in kilometers
         * @param inazim
         *            - Geographic azimuth in radians
         * @param inhght
         *            - Height above ground in km
         * @throws NullPointerException
         * 
         */
        public void prRzll(Amount instltdg, Amount instlndg, Amount inrange,
                Amount inazim, Amount inhght) throws InvalidValueException,
                NullPointerException {
            // System.out.println("From prRzll:");
            // System.out.println(" instltdg = " + instltdg.doubleValue());
            // System.out.println(" instlndg = " + instlndg.doubleValue());
            // System.out.println(" inrange = " + inrange.doubleValue());
            // System.out.println(" inazim = " + inazim.doubleValue());
            // System.out.println(" inhght = " + inhght.doubleValue());

            // checkNullOrInvalidValue( instltdg );
            // checkNullOrInvalidValue( instlndg );
            // checkNullOrInvalidValue(inrange);
            // checkNullOrInvalidValue(inazim);
            // checkNullOrInvalidValue(inhght);

            if (!checkNullOrInvalidValue(instltdg)
                    || !checkNullOrInvalidValue(instlndg)
                    || !checkNullOrInvalidValue(inrange)
                    || !checkNullOrInvalidValue(inazim)
                    || !checkNullOrInvalidValue(inhght))
                return;

            instltdg = checkAndConvertInputAmountToExpectedUnits(instltdg,
                    NonSI.DEGREE_ANGLE);
            instlndg = checkAndConvertInputAmountToExpectedUnits(instlndg,
                    NonSI.DEGREE_ANGLE);
            inrange = checkAndConvertInputAmountToExpectedUnits(inrange,
                    SI.KILOMETER);
            inazim = checkAndConvertInputAmountToExpectedUnits(inazim,
                    SI.RADIAN);
            inhght = checkAndConvertInputAmountToExpectedUnits(inhght,
                    SI.KILOMETER);

            this.stltdg = new Amount(instltdg.doubleValue(), NonSI.DEGREE_ANGLE);
            this.stlndg = new Amount(instlndg.doubleValue(), NonSI.DEGREE_ANGLE);
            this.range = new Amount(inrange.doubleValue(), SI.KILOMETER);
            this.azim = new Amount(inazim.doubleValue(), SI.RADIAN);
            this.hght = new Amount(inhght.doubleValue(), SI.KILOMETER);

            double hdr = GempakConstants.RMISSD;
            double elev = GempakConstants.RMISSD;
            double rad = GempakConstants.RMISSD;
            double radp = GempakConstants.RMISSD;

            /* Convert the station lat/lon to radians */
            Amount stlat = checkAndConvertInputAmountToExpectedUnits(stltdg,
                    NonSI.DEGREE_ANGLE);
            Amount stlon = checkAndConvertInputAmountToExpectedUnits(stlndg,
                    NonSI.DEGREE_ANGLE);

            /* Get the elevation angle */
            hdr = (range.doubleValue() == 0.0f ? 0.0f : hght.doubleValue()
                    / range.doubleValue());
            // elev = (float) ( Math.abs(hdr) < 1.0f ? Math.asin(hdr) : 0.0f );
            elev = (Math.abs(hdr) <= 1.0f ? Math.asin(hdr) : 0.0f);

            double temp = (Math.pow(Math.sin(stlat.doubleValue()), 2));

            /* Get the earth's corrected radius */
            rad = (6378.4 / Math.sqrt(1 + (0.00677 * temp)));
            radp = 4 * (rad / 3);

            double dist = GempakConstants.RMISSD;
            double cx = GempakConstants.RMISSD;
            double cy = GempakConstants.RMISSD;
            double mathFormula1 = GempakConstants.RMISSD;
            double mathFormula2 = GempakConstants.RMISSD;

            /* Calculate the distance */
            double rangeVal = range.doubleValue();
            if (elev > 0.2618f)
                dist = (double) (rangeVal * Math.cos(elev));
            else {
                mathFormula1 = (double) ((1 - (Math.pow(elev, 2) / 2)) - rangeVal
                        * elev / radp);
                dist = rangeVal * mathFormula1;
            }

            /* Calculate the latitude and longitude */
            double azimVal = azim.doubleValue();
            cx = (double) (dist * Math.sin(azimVal));
            cy = (double) (dist * Math.cos(azimVal));

            // mathFormula2 = ( float ) ( ( ( 2 * Math.pow( rad, 2 ) ) *
            // Math.tan( stlat ) ));
            // xlat = ( float ) ( stlat + ( cy / rad ) - ( Math.pow(cx, 2) /
            // mathFormula2 ) );
            double stlatVal = stlat.doubleValue();
            mathFormula2 = (double) ((Math.pow(cx, 2) / (2 * Math.pow(rad, 2)) * Math
                    .tan(stlatVal)));
            double xlatVal = (double) (stlatVal + (cy / rad) - mathFormula2);
            double xlonVal = (double) (stlon.doubleValue() + (cx / (rad * Math
                    .cos(xlat.doubleValue()))));

            /* Change lat/lon to degrees */
            xlatVal = SI.RADIAN.getConverterTo(NonSI.DEGREE_ANGLE).convert(
                    xlatVal);
            xlonVal = SI.RADIAN.getConverterTo(NonSI.DEGREE_ANGLE).convert(
                    xlonVal);

            this.xlat = new Amount(xlatVal, NonSI.DEGREE_ANGLE);
            this.xlon = new Amount(xlonVal, NonSI.DEGREE_ANGLE);
        }
    }

    /**
     * Computes the wind speed from the 'U' and 'V' components of the wind
     * velocity. The formula is the square root of ( u^2 + v^2 )
     * 
     * @param uWnd
     *            - U component of velocity
     * @param vWnd
     *            - V component of velocity
     * @return the computed windspeed if both inputs are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prSped(Amount uWnd, Amount vWnd)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prSped:");
        // System.out.println(" uWnd = " +uWnd.doubleValue());
        // System.out.println(" vWnd = " + vWnd.doubleValue());

        // checkNullOrInvalidValue( uWnd );
        // checkNullOrInvalidValue( vWnd );
        if (!checkNullOrInvalidValue(uWnd) || !checkNullOrInvalidValue(vWnd))
            return new Amount(uWnd.getUnit());

        Unit<?> uWndUnits = uWnd.getUnit();
        Unit<?> vWndUnits = vWnd.getUnit();
        if (uWndUnits != vWndUnits && uWndUnits.isCompatible(vWndUnits)) {
            double vWndVal = vWndUnits.getConverterTo(uWndUnits).convert(
                    vWnd.doubleValue());
            vWnd = new Amount(vWndVal, uWndUnits);
        }
        double prsped = (Math.sqrt((Math.pow(uWnd.doubleValue(), 2) + Math.pow(
                vWnd.doubleValue(), 2))));
        return new Amount(prsped, uWndUnits);
    }

    /**
     * Computes the potential temperature ( in Kelvin ) from the temperature (in
     * Celsius ) and the pressure ( in mb ).
     * 
     * @param tmpc
     *            - The temperature ( in Celsius )
     * @param pres
     *            - The pressure ( in mb )
     * @return the potential temperature ( in Kelvin ), if both inputs are valid
     *         .
     * @throws InvalidRangeException
     * @throws NullPointerException
     * 
     */
    public static Amount prThta(Amount tmpc, Amount pres)
            throws InvalidValueException, NullPointerException {
        checkNullOrInvalidValue(tmpc);
        checkNullOrInvalidValue(pres);

        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(pres))
            return new Amount(SI.KELVIN);

        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        double pressureValue = pres.doubleValue();
        if (pressureValue <= 0) {
            System.out.println("From prThta( ) - pressure must be > 0 ");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException("From prThta( ) - pressure must be > 0 ");
        }

        /* Change temperature in degrees Celsius to Kelvin. */
        double temperatureInKelvin = (double) tmpc.getUnit()
                .getConverterTo(SI.KELVIN).convert(tmpc.doubleValue());

        /* Calculate theta using Poisson's equation */
        double prthta = (double) (temperatureInKelvin * Math.pow(
                (1000 / pres.doubleValue()), GempakConstants.RKAPPA));
        return new Amount(prthta, SI.KELVIN);
    }

    /**
     * Computes the equivalent potential temperature ( in Kelvin ) from the
     * pressure ( in mb ), the temperature ( in Celsius ) and the dewpoint ( in
     * Celsius ) using the equation: THTE = THTAM * EXP [ ( 3.376/TLCL - .00254
     * ) * ( MIXR * ( 1 + .81*.001*MIXR ) ) ] where THTAM = potential
     * temperature of moist air = TMPK * (1000 / PRES) ** E E = RKAPPA * ( 1 - (
     * .28 * .001 * MIXR ) ) Bolton.
     * 
     * @param pres
     *            - the pressure ( in mb )
     * @param tmpc
     *            - the temperature ( in Celsius )
     * @param dwpc
     *            - the dewpoint ( in Celsius )
     * @return the the equivalent potential temperature ( in Kelvin ), if all
     *         the input values are valid
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prThte(Amount pres, Amount tmpc, Amount dwpc) {
        // System.out.println("From prThte:");
        // System.out.println(" pres = " + pres.doubleValue());
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" dwpc = " + dwpc.doubleValue());

        // checkNullOrInvalidValue(pres);
        // checkNullOrInvalidValue(tmpc);
        // checkNullOrInvalidValue(dwpc);
        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(pres)
                || !checkNullOrInvalidValue(dwpc))
            return new Amount(SI.KELVIN);

        if (pres.doubleValue() <= 0) {
            System.out
                    .println("From prThte() - Input pressure must be greater than 0 ");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException("From prThte() - Input pressure must be greater than 0 ");
        }

        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        dwpc = checkAndConvertInputAmountToExpectedUnits(dwpc, SI.CELSIUS);
        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);

        /* Find mixing ratio */
        Amount rmix = prMixr(dwpc, pres);
        if (!checkNullOrInvalidValue(rmix))
            return new Amount(SI.KELVIN);
        /* Change degrees Celsius to Kelvin */
        Amount tmpk = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.KELVIN);

        /* Calculate theta for moist air (thtam) */
        double mixingRatioVal = rmix.doubleValue();
        double pressureVal = pres.doubleValue();
        double tempVal = tmpk.doubleValue();

        double e = (double) (GempakConstants.RKAPPA * (1 - (0.28 * 0.001 * mixingRatioVal)));
        double thtam = (double) (tempVal * Math.pow(1000 / pressureVal, e));

        /* Find the temperature at the lifted condensation level */
        Amount tlcl = prTlcl(tmpc, dwpc);

        if (!checkNullOrInvalidValue(tlcl))
            return new Amount(SI.KELVIN);

        double lclTemp = tlcl.doubleValue();
        e = ((3.376f / lclTemp) - 0.00254f)
                * (mixingRatioVal * (1 + 0.81f * 0.001f * mixingRatioVal));
        double prthte = (double) (thtam * Math.exp(e));
        Amount equivPotentialTempAmount = new Amount(prthte, SI.KELVIN);
        return equivPotentialTempAmount;
    }

    /**
     * Computes wet bulb potential temperature ( in Celsius ) from the pressure,
     * temperature and dewpoint. The result is obtained by first computing the
     * equivalent potential temperature (thte) of the the air parcel at level
     * pres. Then the air parcel is brought to 1000 mb moist adiabatically to
     * get the wet bulb potential temperature.
     * 
     * @param pres
     *            - Pressure ( in millibars )
     * @param tmpc
     *            - Temperature ( in Celsius )
     * @param dwpc
     *            - Dewpoint ( in Celsius )
     * @return The wet bulb potential temperature ( in Celsius ) if all inputs
     *         are valid
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prThwc(Amount pres, Amount tmpc, Amount dwpc)
            throws InvalidValueException, NullPointerException {
        // System.out.println(" PRLibrary/prThwc:");
        // System.out.println(" PRLibrary/prThwc. press = " +
        // pres.doubleValue());
        // System.out.println(" PRLibrary/prThwc.tmpc = " + tmpc.doubleValue());
        // System.out.println(" PRLibrary/prThwc.dwpc = " + dwpc.doubleValue());

        /* Check for missing and invalid data */
        // checkNullOrInvalidValue( pres );
        // checkNullOrInvalidValue( tmpc );
        // checkNullOrInvalidValue( dwpc );

        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(pres)
                || !checkNullOrInvalidValue(dwpc))
            return new Amount(SI.CELSIUS);

        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        tmpc = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.CELSIUS);
        dwpc = checkAndConvertInputAmountToExpectedUnits(dwpc, SI.CELSIUS);

        // System.out
        // .println(" PRLibrary/prThwc. press 2 = " + pres.doubleValue());
        // System.out.println(" PRLibrary/prThwc.tmpc 2 = " +
        // tmpc.doubleValue());
        // System.out.println(" PRLibrary/prThwc.dwpc 2 = " +
        // dwpc.doubleValue());

        double presVal = pres.doubleValue();
        // System.out
        // .println(" PRLibrary/prThwc. press 3 = " + pres.doubleValue());

        if (presVal <= 0) {
            System.out
                    .println("From prThwc: Pressure must be greater than 0 mb");
            return new Amount(SI.CELSIUS);
            // throw new
            // InvalidRangeException("From prThwc: Pressure must be greater than 0 mb");
        }
        /* Compute the thte */
        Amount thte = prThte(pres, tmpc, dwpc);
        // System.out.println(" PRLibrary/prThwc. thte 4 = " +
        // thte.doubleValue());

        /* Check for missing 'thte' and compute wet bulb temperature. */
        if (!checkNullOrInvalidValue(thte))
            return new Amount(SI.CELSIUS);
        /* Compute the parcel temperature (in Kelvin) */

        Amount prthwc = prTmst(thte, new Amount(1000, NcUnits.MILLIBAR),
                new Amount(0, SI.KELVIN));
        // System.out.println(" PRLibrary/prThwc. prthwc (K) 5 = "
        // + prthwc.doubleValue());

        if (!checkNullOrInvalidValue(prthwc))
            return new Amount(SI.CELSIUS);
        /* Convert the parcel temperature to Celsius */
        prthwc = checkAndConvertInputAmountToExpectedUnits(prthwc, SI.CELSIUS);
        // System.out.println(" PRLibrary/prThwc. prthwc (C) 6 = "
        // + prthwc.doubleValue());
        return prthwc;
    }

    /**
     * Computes the temperature at the lifted condensation level for a parcel of
     * air given the temperature ( in Celsius ) and the dewpoint (in Celsius)
     * using the equation: TLCL = [ 1 / ( 1 / (DWPK-56) + ALOG (TMPK/DWPK) / 800
     * ) ] + 56 Bolton.
     * 
     * @param tmpc
     *            - the temperature ( in Celsius )
     * @param dwpc
     *            - the dewpoint ( in Celsius )
     * @return the lifted condensation level temperature In Kelvin, if both
     *         input values are valid
     * 
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prTlcl(Amount tmpc, Amount dwpc) {
        // System.out.println("From prTlcl:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" dwpc = " + dwpc.doubleValue());

        // checkNullOrInvalidValue(tmpc);
        // checkNullOrInvalidValue(dwpc);
        if (!checkNullOrInvalidValue(tmpc) || !checkNullOrInvalidValue(dwpc))
            return new Amount(SI.KELVIN);

        if (tmpc.doubleValue() < -GempakConstants.TMCK
                || dwpc.doubleValue() < -GempakConstants.TMCK) {
            System.out
                    .println("From prTlcl: Input temperature cannot be less than -273.15");
            return new Amount(SI.KELVIN);
        }
        Amount tmpk = checkAndConvertInputAmountToExpectedUnits(tmpc, SI.KELVIN);
        Amount dwpk = checkAndConvertInputAmountToExpectedUnits(dwpc, SI.KELVIN);
        double tempVal = tmpk.doubleValue();
        double dewpointVal = dwpk.doubleValue();
        double lclTemp = (double) ((800 * (dewpointVal - 56) / (800 + (dewpointVal - 56)
                * Math.log(tempVal / dewpointVal))) + 56);
        Amount prtlcl = new Amount(lclTemp, SI.KELVIN);

        return prtlcl;
    }

    /**
     * Computes the temperature ( in Kelvin ) from the pressure ( in mb ) and
     * the potential temperature ( in Kelvin ) using the Poisson equation: TMPK
     * = THTA * ( PRES / 1000 ) ** RKAPPA
     * 
     * @param pres
     *            - the pressure ( in mb )
     * @param thta
     *            - the potential temperature ( in Kelvin )
     * @return the temperature ( in Kelvin )
     * @throws InvalidRangeException
     * @throws NullPointerException
     * 
     */
    public static final Amount prTmpk(Amount pres, Amount thta)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prTmpk:");
        // System.out.println(" pres = " + pres.doubleValue());
        // System.out.println("thta = " + thta.doubleValue());

        Amount prtmpk = new Amount(SI.KELVIN);
        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(thta)) {
            return new Amount(SI.KELVIN);
        }

        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        thta = checkAndConvertInputAmountToExpectedUnits(thta, SI.KELVIN);
        double pressureValue = pres.doubleValue();
        double thtaValue = thta.doubleValue();
        if (pressureValue >= 0) {
            double temperature = (double) (thtaValue * (Math.pow(
                    pressureValue / 1000f, GempakConstants.RKAPPA)));
            prtmpk = new Amount(temperature, SI.KELVIN);
            return prtmpk;
        } else {
            System.out
                    .println("From prTmpk() - pressure cannot be less than 0 mb");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException("From prTmpk() - pressure cannot be less than 0 mb");
        }
    }

    /**
     * <pre>
     * Computes the parcel temperature ( in Kelvin ) from the equivalent potential temp ( in Kelvin ),
     * pressure ( in millibars ) and the first guess temperature ( in Kelvin ). 
     * The parcel temperature at level pres on a specified moist adiabat ( thte ). 
     * The computation is an iterative Newton-Raphson technique of the form:
     * <code>
     * x = x(guess) + [ f( x ) - f( x(guess) ) ] / f'( x(guess) )
     * f' is approximated with finite differences
     * f' = [ f( x(guess) + 1 ) - f( x(guess) ) ] / 1
     * </code>
     * If tguess is 0, a reasonable first guess will be made.
     * Convergence is not guaranteed for extreme input values.  If the
     * computation does not converge after 100 iterations, the missing 
     * data value will be returned. 
     * @param thte      - Equivalent potential temp ( in Kelvin )
     * @param pres      - Pressure ( in millibars )
     * @param tguess   - First guess temperature ( in Kelvin )
     * @return the Parcel temperature in Kelvin if all the input values are valid 
     * (without being extreme) and if a convergence is obtained within 100 iterations
     * </pre>
     * 
     * @throws NullPointerException
     * 
     */
    public static final Amount prTmst(Amount thte, Amount pres, Amount tguess) {
        double prtmst = GempakConstants.RMISSD;
        // System.out.println(" PRLibrary/prTmst:");
        // System.out.println(" PRLibrary/prTmst. thte = " +
        // thte.doubleValue());
        // System.out.println(" PRLibrary/prTmst. pres = " +
        // pres.doubleValue());
        // System.out.println(" PRLibrary/prTmst. tguess = "
        // + tguess.doubleValue());
        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(thte)
                || !checkNullOrInvalidValue(tguess)) {
            return new Amount(SI.KELVIN);
        }
        // checkNullOrInvalidValue( thte );
        // checkNullOrInvalidValue( pres );
        // checkNullOrInvalidValue( tguess );
        thte = checkAndConvertInputAmountToExpectedUnits(thte, SI.KELVIN);
        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        tguess = checkAndConvertInputAmountToExpectedUnits(tguess, SI.KELVIN);
        double thteVal = thte.doubleValue();
        double presVal = pres.doubleValue();
        double tguessVal = tguess.doubleValue();

        if (thteVal <= 0) {
            System.out
                    .println(" From prTmst(): Potential temperature must be greater than 0");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException(" From prTmst(): Potential temperature must be greater than 0");
        } else if (presVal <= 0) {
            System.out
                    .println(" From prTmst(): Pressure must be greater than 0");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException(" From prTmst(): Pressure must be greater than 0");
        } else if (tguessVal < 0) {
            System.out
                    .println(" From prTmst(): First guess temperature must be greater than or equal to 0");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException(" From prTmst(): First guess temperature must be greater than 0");
        }
        double tg = tguess.doubleValue();
        // System.out.println(" PRLibrary/prTmst. thteVal 2 = " + thteVal);
        // System.out.println(" PRLibrary/prTmst. presVal 2 = " + presVal);
        // System.out.println(" PRLibrary/prTmst. tguessVal 2 = " + tguessVal);
        // System.out.println(" PRLibrary/prTmst. tg 2 = " + tg);

        /*
         * If tguess is passed as 0. it is computed from an MIT scheme
         */
        if (tg == 0) {
            double diffVar = thte.doubleValue() - 270;
            double mathFormula1 = (double) (diffVar > 0 ? diffVar : 0.0);
            tg = (double) ((thte.doubleValue() - .5f * (Math.pow(mathFormula1,
                    1.05f))) * (Math.pow(pres.doubleValue() / 1000.0f, 0.2f)));
        }

        /* Set convergence and initial guess in degrees Celsius */
        double epsi = 0.01f;
        double tgnu = SI.KELVIN.getConverterTo(SI.CELSIUS).convert(tg);

        /*
         * Set a limit of 100 iterations. Compute tenu,tenup, the thte's at one
         * degree above the guess temperature.
         */
        int index = 0;
        while (index < 100) {
            double tgnup = tgnu + 1;
            Amount tgnuAmount = new Amount(tgnu, SI.CELSIUS);
            Amount tgnupAmount = new Amount(tgnup, SI.CELSIUS);
            Amount tenu = prThte(pres, tgnuAmount, tgnuAmount);
            Amount tenup = prThte(pres, tgnupAmount, tgnupAmount);
            // System.out.println(" PRLibrary/prTmst. tgnuAmount 5 = "
            // + tgnuAmount.getValue().floatValue());
            // System.out.println(" PRLibrary/prTmst. tgnupAmount 5 = "
            // + tgnupAmount.getValue().floatValue());
            // System.out.println(" PRLibrary/prTmst. tenu 5 = "
            // + tenu.getValue().floatValue());
            // System.out.println(" PRLibrary/prTmst. tgnup 5 = "
            // + tenup.getValue().floatValue());
            /* Check that the THTE's exist. */

            if ((!checkNullOrInvalidValue(tenu) || !checkNullOrInvalidValue(tenup))) {
                return new Amount(SI.KELVIN);
                // index++;
                // continue;
            }

            /* Compute the correction */
            double tenuVal = tenu.doubleValue();
            double tenupVal = tenup.doubleValue();
            double cor = (thteVal - tenuVal) / (tenupVal - tenuVal);
            tgnu += cor;
            // System.out.println(" PRLibrary/prTmst. tenuVal 6 = " + tenuVal);
            // System.out.println(" PRLibrary/prTmst. tenupVal 6 = " +
            // tenupVal);

            // System.out.println(" PRLibrary/prTmst. cor 6 = " + cor);
            // System.out.println(" PRLibrary/prTmst. tgnu 6 = " + tgnu);

            if ((cor < epsi) && (-cor < epsi)) {

                /* return on convergence */
                prtmst = tgnuAmount.getUnit().getConverterTo(SI.KELVIN)
                        .convert(tgnu);
                // System.out.println(" PRLibrary/prTmst. prtmst 7 = " +
                // prtmst);

                break;
            }

            index++;
        }
        // System.out.println(" PRLibrary/prTmst. prtmst 8 = "
        // + new Amount(prtmst, SI.KELVIN).getValue().floatValue());

        return new Amount(prtmst, SI.KELVIN);
    }

    /**
     * <pre>
     * Computes wet bulb temperature from the temperature, mixing ratio, and pressure.
     * The result is obtained by solving for the temperature at which saturation occurs,
     *  when the latent heat required to vaporize the water is provided by a cooling of the air.
     *  The equation representing the process is:
     *  <code> ( tmpk - tmwb ) * cp - ( Rsat (tmwb) - rmix ) * lvap = 0 </code> 
     *  This implicit equation is solved by Newton's method, since the 
     *  saturation mixing ratio Rsat is a transcendental function of tmwb.
     *  The expressions for the heat of vaporization (LVAP) and saturation 
     *   vapor pressure are equations (2) and (10) from Bolton (MWR, 1980).
     * </pre>
     * 
     * @param tmpk
     *            - Temperature (K)
     * @param rmix
     *            - Mixing ratio (g/kg)
     * @param pres
     *            - Pressure (mb)
     * @return Wet bulb temperature (K) if all inputs are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prTmwb(Amount tmpk, Amount rmix, Amount pres)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prTmwb:");
        // System.out.println(" tmpk = " + tmpk.doubleValue());
        // System.out.println(" rmix = " + rmix.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());

        Amount prtmwb = null;
        /* Check for missing and invalid data */
        // checkNullOrInvalidValue( tmpk );
        // checkNullOrInvalidValue( rmix );
        // checkNullOrInvalidValue( pres );
        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(tmpk)
                || !checkNullOrInvalidValue(rmix)) {
            return new Amount(SI.KELVIN);
        }
        tmpk = checkAndConvertInputAmountToExpectedUnits(tmpk, SI.KELVIN);
        rmix = checkAndConvertInputAmountToExpectedUnits(rmix,
                NcUnits.GRAMS_PER_KILOGRAM);
        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        double presVal = pres.doubleValue();
        if (presVal <= 0) {
            // System.out
            //         .println("From prTmwb - pressure value must be greater than 0 ");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException("From prTmwb - pressure value must be greater than 0 ");
        }
        /* Change temperature to degrees Celsius. */
        Amount tmp = checkAndConvertInputAmountToExpectedUnits(tmpk, SI.CELSIUS);

        /* Compute the latent heat of vaporization. */
        Amount lvap = prLhvp(tmp);
        if (!checkNullOrInvalidValue(lvap))
            return new Amount(SI.KELVIN);
        /* Compute the specific heat of moist air */
        double rmixVal = rmix.doubleValue() / 1000;
        double cp = (1005.7 * (1.0 + 0.887 * rmixVal));

        double rlocp = lvap.doubleValue() / cp;

        /* Do Newton iteration */
        int iter = 0;
        double twb = tmp.doubleValue();
        boolean isConvrg = false;

        double A = 6.112;
        double B = 17.67;
        double C = 243.5;
        double EPSI = 0.622;
        double G = B * C;
        double ERRMAX = 0.001;
        double tmpVal = tmp.doubleValue();
        while (iter <= 50 && !isConvrg) {
            iter++;
            double bt = B * twb;
            double tpc = twb + C;
            double d = ((presVal / A) * Math.exp((-bt) / tpc));
            double dm1 = d - 1;
            double f = (tmpVal - twb) - rlocp * (EPSI / dm1 - rmixVal);
            double df = (-G) / (tpc * tpc);
            df = d * df * rlocp * EPSI / (dm1 * dm1) - 1;
            double cor = f / df;
            twb = twb - cor;
            if (Math.abs(cor) <= ERRMAX)
                isConvrg = true;
        }

        if (isConvrg) {
            Amount twk = new Amount(twb, SI.KELVIN);
            if (twk.doubleValue() > tmpk.doubleValue())
                twk = new Amount(tmpk.doubleValue(), SI.KELVIN);

            prtmwb = twk;
        }
        // }
        return prtmwb;
    }

    /**
     * Computes the virtual temperature ( in Kelvin ) from the temperature ( in
     * Celsius ), dewpoint ( in Celsius ) and pressure ( in mb ) where DWPC and
     * PRES are used to compute MIXR. The following equation is used: TVRK =
     * TMPK * (1 + .001 * MIXR / .62197) / (1 + .001 * MIXR) If DWPC is missing,
     * dry air is assumed and TMPK is returned.
     * 
     * @param tmpc
     *            - Temperature ( in Celsius )
     * @param dwpc
     *            - Dewpoint ( in Celsius )
     * @param pres
     *            - Pressure ( in mb )
     * @return the virtual temperature ( in Kelvin )
     * @throws NullPointerException
     * 
     */
    public static final Amount prTvrk(Amount tmpc, Amount dwpc, Amount pres)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prTvrk:");
        // System.out.println(" tmpc = " + tmpc.doubleValue());
        // System.out.println(" dwpc = " + dwpc.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());

        Amount prtvrk = null;
        // checkNullOrInvalidValue(tmpc);
        // checkNullOrInvalidValue(pres);

        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(tmpc)) {
            return new Amount(SI.KELVIN);
        }

        /* If dewpoint is missing, return temperature */

        if (!checkNullOrInvalidValue(dwpc))
            return checkAndConvertInputAmountToExpectedUnits(tmpc, SI.KELVIN);

        else {
            /* Change temperature to Kelvin. */
            Amount tmpk = checkAndConvertInputAmountToExpectedUnits(tmpc,
                    SI.KELVIN);

            /* Find mixing ratio in g/kg; if missing, return temperature */
            Amount rmix = prMixr(dwpc, pres);

            double virtualTemp;
            if (rmix.doubleValue() == GempakConstants.RMISSD)
                virtualTemp = (double) tmpc.getUnit().getConverterTo(SI.KELVIN)
                        .convert(tmpc.doubleValue());
            else {
                double mixingRatioVal = rmix.doubleValue();
                double temp = tmpk.doubleValue();
                virtualTemp = (double) (temp
                        * (1 + 0.001 * mixingRatioVal / 0.62197) / (1 + 0.001 * mixingRatioVal));

            }
            prtvrk = new Amount(virtualTemp, SI.KELVIN);
        }
        return prtvrk;
    }

    /**
     * Computes the 'U' component of the wind from its speed and direction
     * 
     * @param sped
     *            - wind speed
     * @param drct
     *            - wind direction
     * @return The 'U' component of the wind if both inputs are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prUwnd(Amount sped, Amount drct)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prUwnd:");
        // System.out.println(" sped = " + sped.doubleValue());
        // System.out.println(" drct = " + drct.doubleValue());
        if (!checkNullOrInvalidValue(drct) || !checkNullOrInvalidValue(sped)) {
            return new Amount(SI.METERS_PER_SECOND);
        }
        drct = checkAndConvertInputAmountToExpectedUnits(drct,
                NonSI.DEGREE_ANGLE);
        double pruwnd = ((-Math.sin(drct.doubleValue() * GempakConstants.DTR)) * sped
                .doubleValue());
        return new Amount(pruwnd, sped.getUnit()); // TODO :verify the units
    }

    /**
     * Computes the 'V' component of the wind from its speed and direction
     * 
     * @param sped
     *            - wind speed
     * @param drct
     *            - wind direction
     * @return The 'V' component of the wind if both inputs are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prVwnd(Amount sped, Amount drct)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prVwnd:");
        // System.out.println(" sped = " + sped.doubleValue());
        // System.out.println(" drct = " + drct.doubleValue());
        if (!checkNullOrInvalidValue(drct) || !checkNullOrInvalidValue(sped)) {
            return new Amount(SI.METERS_PER_SECOND);
        }
        drct = checkAndConvertInputAmountToExpectedUnits(drct,
                NonSI.DEGREE_ANGLE);
        double prvwnd = ((-Math.cos(drct.doubleValue() * GempakConstants.DTR)) * sped
                .doubleValue());
        return new Amount(prvwnd, sped.getUnit()); // TODO :verify the units
    }

    /**
     * Computes the vapor pressure ( in mb) from the input dewpoint temperature
     * in Celsius using the equation: VAPR = 6.112 * EXP [ (17.67 *
     * dewpointValue) / (dewpointValue + 243.5) ]
     * 
     * @param dwpc
     *            - the dewpoint temperature ( in Celsius )
     * @return the vapor pressure ( in mb) from the dewpoint temperature if it
     *         is valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prVapr(Amount dwpc) {
        // System.out.println("From prVapr:");
        // System.out.println(" dwpc = " + dwpc.doubleValue());

        if (!checkNullOrInvalidValue(dwpc))
            return new Amount(NcUnits.MILLIBAR);
        dwpc = checkAndConvertInputAmountToExpectedUnits(dwpc, SI.CELSIUS);
        double dewpointValue = dwpc.doubleValue();
        if (dewpointValue >= -240.0f)
            return (new Amount((6.112 * (Math.exp((17.67 * dewpointValue)
                    / (dewpointValue + 243.5)))), NcUnits.MILLIBAR));
        else {
            // throw new
            // InvalidRangeException("Exception from prVapr() - dewpoint cannot be less than -240 ");
            System.out
                    .println("From prVapr() - dewpoint cannot be less than -240 ");
            return new Amount(NcUnits.MILLIBAR);
        }
    }

    // /**
    // * Computes the visibility ( in nautical miles ) from the input visibility
    // ( in kilometers )
    // * @param vsbk - visibility ( in kilometers )
    // * @return visibility ( in nautical miles ) if the input is valid
    // * @throws NullPointerException
    // *
    // */
    // //TODO: remove this - since the getValueAs(Unit<?>) offers the same
    // facility?
    // public static final Amount prVskn ( Amount vsbk) throws
    // InvalidValueException, NullPointerException {
    // //System.out.println("From prVskn:");
    // //System.out.println(" vsbk = " + vsbk.doubleValue());
    //
    // checkNullOrInvalidValue( vsbk );
    // vsbk = checkAndConvertInputAmountToExpectedUnits( vsbk, SI.KILOMETER );
    // return ( checkAndConvertInputAmountToExpectedUnits(vsbk,
    // NonSI.NAUTICAL_MILE ) );
    // }

    /**
     * Computes the wind chill equivalent temperature ( the temperature with
     * calm winds that produces the same cooling effect as the given temperature
     * with the given wind speed)
     * 
     * @param tmpf
     *            - Air temperature ( in Farenheit )
     * @param sknt
     *            - Wind speed ( in knots )
     * @return the wind chill equivalent temperature ( in Farenheit ), if the
     *         inputs are valid
     * @throws NullPointerException
     * 
     */
    public static final Amount prWceq(Amount tmpf, Amount sknt)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prWceq:");
        // System.out.println(" tmpf = " + tmpf.doubleValue());
        // System.out.println(" sknt = " + sknt.doubleValue());

        double prwceq = GempakConstants.RMISSD;
        if (!checkNullOrInvalidValue(tmpf) || !checkNullOrInvalidValue(sknt)) {
            return new Amount(NonSI.FAHRENHEIT);
        }

        /* Convert input variables to Celsius and meters/second. */
        Amount tmpc = checkAndConvertInputAmountToExpectedUnits(tmpf,
                SI.CELSIUS);
        Amount sped = checkAndConvertInputAmountToExpectedUnits(sknt,
                SI.METERS_PER_SECOND);

        if (sped.doubleValue() <= 1.34)
            /*
             * If the wind speed does not exceed 1.34 m/s ( not much wind to
             * contribute to the wind chill), return the input temperature as
             * the wind chill temperature
             */
            prwceq = tmpc.getUnit().getConverterTo(NonSI.FAHRENHEIT)
                    .convert(tmpc.doubleValue());
        else {
            /*
             * Compute the wind chill temp if the inputs are not missing and and
             * the wind speed is greater than 1.34 m/s. Equations for wind chill
             * computation from R. Falconer,
             * "Windchill, A Useful Wintertime Weather Variable", Weatherwise,
             * Dec 1968.
             */
            if (sped.getUnit() == SI.METERS_PER_SECOND) {
                float windChill = (float) (33.0 - ((33.0 - tmpc.doubleValue())
                        * wci(sped.doubleValue()) / wci(1.34f)));
                prwceq = tmpc.getUnit().getConverterTo(NonSI.FAHRENHEIT)
                        .convert(windChill);
            }
        }

        return (new Amount(prwceq, NonSI.FAHRENHEIT));
    }

    // /**
    // * Computes the numeric total cloud cover for the worst case aviation
    // flight condition,
    // * based on the categorical identification of flight rules for prevailing
    // and temporary / probability conditions.
    // * @param xvfr - Prevailing categorical id of flight rules
    // * @param txvf - Temporary / Probability categorical id of flight rules
    // * @param cfrt - Prevailing numeric total cloud cover
    // * @param tcfr - Temporary / Probability numeric total cloud cover
    // * @return Worst case numeric total cloud cover or RMISSD (-9999) if the
    // computation does not fall through
    // * @throws NullPointerException
    // *
    // */
    // public static final Amount prWcfr ( Amount xvfr, Amount txvf, Amount
    // cfrt, Amount tcfr) throws InvalidValueException, NullPointerException {
    // double prwcfr = GempakConstants.RMISSD;
    // checkNullOrInvalidValue( tcfr );
    // checkNullOrInvalidValue( cfrt );
    // double cfrtVal = cfrt.doubleValue();
    // double tcfrVal = tcfr.doubleValue();
    // if ( ( xvfr == null || xvfr.doubleValue() == GempakConstants.RMISSD )
    // || ( txvf == null || txvf.doubleValue() == GempakConstants.RMISSD ))
    // prwcfr = ( cfrtVal > tcfrVal ? cfrtVal : tcfrVal );
    //
    // else {
    // double txvfVal = txvf.doubleValue();
    // double xvfrVal = xvfr.doubleValue();
    // if ( txvfVal < xvfrVal )
    // prwcfr = tcfrVal;
    // else if ( txvfVal == xvfrVal)
    // prwcfr = ( cfrtVal > tcfrVal ? cfrtVal : tcfrVal );
    // else
    // prwcfr = cfrtVal;
    // }
    // return ( new Amount ( prwcfr ,Unit.ONE));
    // }

    /**
     * Computes the wind chill temperature from the air temperature and the wind
     * speed
     * 
     * @param tmpf
     *            - Air temperature ( in degree Farenheit )
     * @param sknt
     *            - Wind speed ( in knots )
     * @return the wind chill temperature ( in Farenheit ) if none of the inputs
     *         are missing
     * @throws NullPointerException
     * 
     */
    public static final Amount prWcht(Amount tmpf, Amount sknt)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prWcht:");
        // System.out.println(" tmpf = " + tmpf.doubleValue());
        // System.out.println(" sknt = " + sknt.doubleValue());
        double prwrcht = GempakConstants.RMISSD;
        if (!checkNullOrInvalidValue(tmpf) || !checkNullOrInvalidValue(sknt)) {
            return new Amount(NonSI.FAHRENHEIT);
        }

        /* Convert the speed to miles per hour */
        Amount smph = checkAndConvertInputAmountToExpectedUnits(sknt,
                NonSI.MILES_PER_HOUR);

        /*
         * If the inputs are not missing , check if the wind speed is <= 3 miles
         * per hour
         */

        double smphVal = smph.doubleValue();
        double tmpfVal = tmpf.doubleValue();
        if (smphVal <= 3)
            prwrcht = tmpfVal;
        else {
            /*
             * Compute the wind-chill temperature for wind speeds that exceed 3
             * miles per hour
             */
            float wcht = (float) (35.74 + 0.6215 * tmpfVal - 35.75
                    * Math.pow(smphVal, 0.16) + 0.4275 * tmpfVal
                    * Math.pow(smphVal, 0.16));
            prwrcht = (wcht > tmpfVal ? tmpfVal : wcht);
        }
        return (new Amount(prwrcht, NonSI.FAHRENHEIT));
    }

    /**
     * Computes the wind component towards a specific direction from the wind
     * direction, wind speed and direction of desired component.
     * 
     * @param drct
     *            - the wind direction in degrees
     * @param sped
     *            - the wind speed in m/s
     * @param dcmp
     *            - the direction of the desired component
     * @return the component of the wind (in m/s) if none of the input
     *         parameters are missing
     * @throws NullPointerException
     * 
     */
    public static final Amount prWcmp(Amount drct, Amount sped, Amount dcmp)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prWcmp:");
        // System.out.println(" sped = " + sped.doubleValue());
        // System.out.println(" drct = " + drct.doubleValue());
        // System.out.println(" dcmp = " + dcmp.doubleValue());
        /* Check for missing input parameters */
        // checkNullOrInvalidValue( drct );
        // checkNullOrInvalidValue( sped );
        // checkNullOrInvalidValue( dcmp );
        if (!checkNullOrInvalidValue(drct) || !checkNullOrInvalidValue(sped)
                || !checkNullOrInvalidValue(dcmp)) {
            return new Amount(SI.METERS_PER_SECOND);
        }

        drct = checkAndConvertInputAmountToExpectedUnits(drct,
                NonSI.DEGREE_ANGLE);
        dcmp = checkAndConvertInputAmountToExpectedUnits(dcmp,
                NonSI.DEGREE_ANGLE);
        sped = checkAndConvertInputAmountToExpectedUnits(sped,
                SI.METERS_PER_SECOND);

        /* Calculate wind speed toward specified direction */
        double prwcmp = sped.doubleValue()
                * (-Math.cos((drct.doubleValue() - dcmp.doubleValue())
                        * GempakConstants.DTR));

        return new Amount(prwcmp, SI.METERS_PER_SECOND);
    }

    /**
     * Computes the wind component toward a direction 90 degrees
     * counterclockwise of a specified direction. If no direction is specified,
     * the component toward north is returned.
     * 
     * @param drct
     *            - wind direction ( in degrees )
     * @param sped
     *            - wind speed ( in knots or m/s )
     * @param dcmp
     *            - specified wind direction ( in degrees )
     * @return a component of the wind in m/s if the input wind speed and
     *         direction are valid and if the specified wind direction is
     *         between 0 degrees and 360 degrees.
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prWnml(Amount drct, Amount sped, Amount dcmp)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prWnml:");
        // System.out.println(" sped = " + sped.doubleValue());
        // System.out.println(" drct = " + drct.doubleValue());
        // System.out.println(" dcmp = " + dcmp.doubleValue());
        // checkNullOrInvalidValue(sped);
        // checkNullOrInvalidValue(drct);
        // checkNullOrInvalidValue(dcmp);
        if (!checkNullOrInvalidValue(drct) || !checkNullOrInvalidValue(sped)
                || !checkNullOrInvalidValue(dcmp)) {
            return new Amount(SI.METERS_PER_SECOND);
        }

        drct = checkAndConvertInputAmountToExpectedUnits(drct,
                NonSI.DEGREE_ANGLE);
        dcmp = checkAndConvertInputAmountToExpectedUnits(dcmp,
                NonSI.DEGREE_ANGLE);
        sped = checkAndConvertInputAmountToExpectedUnits(sped,
                SI.METERS_PER_SECOND);
        if ((dcmp.doubleValue() < 0) && (dcmp.doubleValue() > 360)) {
            // throw new
            // InvalidRangeException("From prWnml - the wind direction 'dcmp'  mus be greater than or equal to 0 and less than or equal to 360");
            System.out
                    .println("From prWnml - the wind direction 'dcmp'  mus be greater than or equal to 0 and less than or equal to 360");
            return new Amount(SI.METERS_PER_SECOND);
        }
        /*
         * Calculate wind speed 90 degrees to left of given direction.
         */
        double prwnml = (float) (sped.doubleValue() * (-Math.cos((drct
                .doubleValue() - dcmp.doubleValue() - 90)
                * GempakConstants.DTR)));
        return (new Amount(prwnml, SI.METERS_PER_SECOND));
    }

    // /**
    // * Computes the packed wind speed and direction from the input wind speed
    // and wind direction
    // * @param drct - wind direction ( in degrees )
    // * @param sped - wind speed ( in knots or m/s )
    // * @return the packed speed and direction
    // */
    // public static final Amount prWind ( Amount drct, Amount sped ){
    // float prwind = GempakConstants.RMISSD;
    // // if ( !MissingValueTester.isDataValueMissing(drct)
    // // && !MissingValueTester.isDataValueMissing(sped)){
    // /*
    // * (Non-Javadoc)
    // * The packed wind speed and direction are of the form:
    // * SSSDDD, where SSS - wind speed ( in knots or m/s ) and
    // * DDD - wind direction in degrees
    // *
    // */
    // int jdrct = (int ) Math.round( drct.doubleValue() );
    // int jsped = (int) Math.round( sped.doubleValue() );
    // prwind = jdrct + jsped * 1000;
    // // }
    // return prwind;
    // }

    /**
     * Computes the worst case categorical identification of flight rules for
     * prevailing and temporary / probability conditions.
     * 
     * @param xvfr
     *            - Prevailing categorical id of flight rules
     * @param txvf
     *            - Temporary / probability categorical id of flight rules
     * @return The worst case categorical id of flight rules
     * @throws NullPointerException
     * 
     */
    public static final Amount prWxvf(Amount xvfr, Amount txvf)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prWxvf:");
        // System.out.println(" xvfr = " + xvfr.doubleValue());
        // System.out.println(" txvf = " + txvf.doubleValue());

        double prwxvf = GempakConstants.RMISSD;
        if (txvf != null && xvfr != null) {

            double xvfrVal = xvfr.doubleValue();
            double txvfVal = txvf.doubleValue();

            if (txvfVal != GempakConstants.RMISSD
                    && xvfrVal != GempakConstants.RMISSD)
                prwxvf = (xvfrVal < txvfVal ? xvfrVal : txvfVal);

            else if (xvfrVal == GempakConstants.RMISSD
                    && txvfVal != GempakConstants.RMISSD)
                prwxvf = xvfrVal;

            else if (txvfVal != GempakConstants.RMISSD
                    && xvfrVal != GempakConstants.RMISSD)
                prwxvf = txvfVal;
        }

        return (new Amount(prwxvf, Unit.ONE));
    }

    /**
     * <pre>
     * Computes LIFR/IFR/MVFR/VFR flight conditions based on ceiling and visibility.
     * @param ceil - Ceiling in hundreds of feet
     * @param vsby - Visibility in statute miles
     * @return Flight conditions index value: 
     *     0 - LIFR
     *     1 - IFR
     *     2 - MVFR
     *     3 - VFR
     * </pre>
     * 
     * @throws NullPointerException
     * 
     */
    public static final Amount prXvfr(Amount ceil, Amount vsby)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prXvfr:");
        // System.out.println(" ceil = " + ceil.doubleValue());
        // System.out.println(" vsby = " + vsby.doubleValue());

        double prxvfr = GempakConstants.RMISSD;
        double vc = GempakConstants.RMISSD;
        double vs = GempakConstants.RMISSD;
        if (vsby == null)
            return null;

        if (!checkNullOrInvalidValue(ceil))
            return new Amount(Unit.ONE);
        ceil = checkAndConvertInputAmountToExpectedUnits(ceil,
                NcUnits.HUNDREDS_OF_FEET);
        vsby = checkAndConvertInputAmountToExpectedUnits(vsby, NonSI.MILE);
        /* Compute categorical flight rules */

        // Check the ceiling value

        double ceilVal = ceil.doubleValue();

        double vsbyVal = vsby.doubleValue();

        if (ceilVal < 0) {
            // no-op. So vc retains its RMISSD value
        } else if (ceilVal < 5)
            vc = 0;
        else if (ceilVal < 10)
            vc = 1;
        else if (ceilVal <= 30)
            vc = 2;
        else if ((vsbyVal > 5) || (vsbyVal < 0)
                || (vsbyVal == GempakConstants.RMISSD)) {
            prxvfr = 3;
        }

        /* Check the visibility value. */
        if (vsbyVal != GempakConstants.RMISSD) {
            if (vsbyVal < 0) {
                // no-op. So vs retains it RMISSD value
            } else if (vsbyVal < 1)
                vs = 0;
            else if (vsbyVal < 3)
                vs = 1;
            else if (vsbyVal <= 5)
                vs = 2;
            else
                vs = 3;
        }

        /* Determine the more restrictive of the two values. */
        if (vc == GempakConstants.RMISSD)
            prxvfr = vs;
        else if (vs == GempakConstants.RMISSD)
            prxvfr = vc;
        else
            prxvfr = (vc < vs ? vc : vs);

        return (new Amount(prxvfr, Unit.ONE));

    }

    /**
     * Computes station elevation from altimeter and station pressure. It is
     * also used to estimate height at various pressure levels from the
     * altimeter in millibars. The PC library computes zmsl, Z000, Z950, Z850,
     * Z800 by calling this function with pres equal to PMSL, 1000, 950, 850 and
     * 800 respectively.
     * 
     * @param altm
     *            - Altimeter in millibars
     * @param pres
     *            - Pressure in millibars
     * @return the height ( in meters ) if neither input value is missing and
     *         both input values are greater than zero.
     * @throws NullPointerException
     * 
     * @throws InvalidRangeException
     */
    public static final Amount prZalt(Amount altm, Amount pres)
            throws InvalidValueException, NullPointerException {
        // System.out.println("From prZalt:");
        // System.out.println(" altm = " + altm.doubleValue());
        // System.out.println(" pres = " + pres.doubleValue());

        // checkNullOrInvalidValue( pres );
        // checkNullOrInvalidValue( altm );

        if (!checkNullOrInvalidValue(pres) || !checkNullOrInvalidValue(altm)) {
            return new Amount(SI.METER);
        }

        pres = checkAndConvertInputAmountToExpectedUnits(pres, NcUnits.MILLIBAR);
        altm = checkAndConvertInputAmountToExpectedUnits(altm, NcUnits.MILLIBAR);
        if (altm.doubleValue() <= 0) {
            // throw new
            // InvalidRangeException("From prZalt:  altm must be greater than 0 mb");
            System.out.println("From prZalt:  altm must be greater than 0 mb");
            return new Amount(SI.METER);
        }
        if (pres.doubleValue() <= 0) {
            // throw new
            // InvalidRangeException("From prZalt:  pres must be greater than 0 mb");
            System.out.println("From prZalt:  pres must be greater than 0 mb");
            return new Amount(SI.METER);
        }
        double to = GempakConstants.TMCK + 15;
        double gamma = GempakConstants.GAMUSD / 1000;

        /* Calculate the exponent and pressure ratio. */
        double expo = (gamma * GempakConstants.RDGAS) / GempakConstants.GRAVTY;
        double prat = pres.doubleValue() / altm.doubleValue();
        double przalt = (to * (1 - Math.pow(prat, expo))) / gamma;

        return (new Amount(przalt, SI.METER));
    }

    /**
     * Computes the windchill from the wind velocity ( part of the Falconer
     * equation - refer method prWceq)
     * 
     * @param d
     *            - wind velocity ( in meters per second )
     * @return the windchill temperature
     */
    private static double wci(double d) {

        /*
         * from R. Falconer, "Windchill, A Useful Wintertime Weather Variable",
         * Weatherwise, Dec 1968.
         */
        return ((double) (10 * Math.sqrt(d) + 10.45 - d));

    }

    public static final Amount checkAndConvertInputAmountToExpectedUnits(
            Amount amountIn, Unit<?> expectedUnit) {
        Amount amountOut = null;
        if (!amountIn.getUnit().equals(expectedUnit)
                && amountIn.getUnit().isCompatible(expectedUnit)) {
            double newValue = amountIn.getUnit().getConverterTo(expectedUnit)
                    .convert(amountIn.doubleValue());
            amountOut = new Amount(newValue, expectedUnit);
        } else
            // throw new ConversionException("Unable to convert " +
            // amountIn.getUnit().toString() + " to " + expectedUnit.toString()
            // );
            amountOut = amountIn;

        return amountOut;
    }

    // public static final void checkNullOrInvalidValue( Amount amountToCheck )
    // throws InvalidValueException, NullPointerException{
    // if (amountToCheck == null )
    // throw new NullPointerException();
    // else {
    // double amountValue = amountToCheck.doubleValue();
    // if ( amountValue == GempakConstants.RMISSD){
    // throw new InvalidValueException( new String
    // ("Input amount cannot be -9999"));
    // }
    // else if ( Double.isNaN(amountValue))
    // throw new InvalidValueException( new String ("Input amount cannot be NaN"
    // ));
    // }
    // }

    public static final boolean checkNullOrInvalidValue(Amount amountToCheck) {
        if (amountToCheck == null) {
            return false;
            // throw new NullPointerException();
        } else {
            double amountValue = amountToCheck.doubleValue();
            if (amountValue == GempakConstants.RMISSD) {
                System.out.println("Input amount cannot be -9999");
                return false;
                // throw new InvalidValueException( new String
                // ("Input amount cannot be -9999"));
            } else if (Double.isNaN(amountValue)) {
                System.out.println("Input amount cannot be NaN");
                // throw new InvalidValueException( new String
                // ("Input amount cannot be NaN" ));
                return false;
            } else
                return true;
        }
    }

    // public static final class InvalidRangeException extends Exception {
    // /**
    // *
    // */
    // private static final long serialVersionUID = -4962228676211688262L;
    //
    // /**
    // *
    // */
    // public InvalidRangeException( String msg) {
    // super( msg );
    // }
    // }

    public static final class InvalidValueException extends Exception {
        /**
	 * 
	 */
        private static final long serialVersionUID = 3844655201015825508L;

        /**
	 * 
	 */
        public InvalidValueException(String msg) {
            super(msg);
        }
    }

    /**
     * This function computes parcel PRES from THTE and TMPC, where TMPC is the
     * parcel temperature at PRES on a specified moist adiabat (THTE). The
     * computation is an iterative Newton-Raphson technique of the form:
     * 
     * x = x(guess) + [ f( x ) - f( x(guess) ) ] / f'( x(guess) )
     * 
     * f' is approximated with finite differences f' = [ f( x(guess) + 1 ) - f(
     * x(guess) ) ] / 1
     * 
     * Convergence is not guaranteed for extreme input values. If the
     * computation does not converge after 100 iterations, the missing data
     * value will be returned.
     * 
     * @param thte
     *            - Equivalent potential temp in K
     * @param tmpk
     *            - Parcel temperature in Kelvin
     * @return Pressure in millibars
     * 
     * @throws NullPointerException
     * @throws InvalidRangeException
     */

    public static Amount prPmst(Amount thte, Amount tmpk)
            throws InvalidValueException, NullPointerException {
        // checkNullOrInvalidValue(thte);
        // checkNullOrInvalidValue(tmpk);

        if (!checkNullOrInvalidValue(thte) || !checkNullOrInvalidValue(tmpk)) {
            return new Amount(SI.KELVIN);
        }

        thte = checkAndConvertInputAmountToExpectedUnits(thte, SI.KELVIN);
        tmpk = checkAndConvertInputAmountToExpectedUnits(tmpk, SI.KELVIN);

        if (thte.getValue().doubleValue() <= 0) {
            // System.out
            //        .println("From prPmst(): The equivalent potential temperature must be greater than 0");
            return new Amount(SI.KELVIN);
            // throw new
            // InvalidRangeException("The equivalent potential temperature must be greater than 0");
        }
        Amount prpmst = new Amount(NcUnits.MILLIBAR);
        /* Set convergence and initial guess of pressure. */
        double epsi = 0.01;
        Amount tmpc = new Amount(tmpk.getValueAs(SI.CELSIUS), SI.CELSIUS);
        double tempVal = 1000 * Math.pow(tmpk.getValue().doubleValue()
                / thte.getValue().doubleValue(), GempakConstants.AKAPPA);
        Amount pgdn = new Amount(tempVal, NcUnits.MILLIBAR);
        boolean done = false;
        int i = 1;
        while (!done) {
            // Amount pgdn = new Amount ( tempVal, NcUnits.MILLIBAR );
            Amount pgup = new Amount(pgdn.getValueAs(NcUnits.MILLIBAR)
                    .doubleValue() + 1, NcUnits.MILLIBAR);

            Amount tedn = prThte(pgdn, tmpc, tmpc);
            Amount teup = prThte(pgup, tmpc, tmpc);

            if (!tedn.hasValidValue() || !teup.hasValidValue())
                return prpmst;

            /* Compute the correction; return on convergence. */
            double cor = (thte.getValueAs(SI.KELVIN).doubleValue() - tedn
                    .getValueAs(SI.KELVIN).doubleValue())
                    / (teup.getValueAs(SI.KELVIN).doubleValue() - tedn
                            .getValueAs(SI.KELVIN).doubleValue());

            double pgdnVal = (pgdn.getValueAs(NcUnits.MILLIBAR).doubleValue() + cor);
            pgdn = new Amount(pgdnVal, NcUnits.MILLIBAR);
            if (Math.abs(cor) < epsi) {
                prpmst = new Amount(pgdnVal, NcUnits.MILLIBAR);
                return prpmst;
            }

            i++;
            if (i > 100)
                done = true;
        }
        return prpmst;
    }

}
