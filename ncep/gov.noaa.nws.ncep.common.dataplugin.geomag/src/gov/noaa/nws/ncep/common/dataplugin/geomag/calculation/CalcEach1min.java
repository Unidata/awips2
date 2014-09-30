package gov.noaa.nws.ncep.common.dataplugin.geomag.calculation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/*
 * The calculation of k, 1 minute related.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * Date          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * 03/18/2014   #1123       qzhou      Add getHdevOrDDev
 * 04/09/2014   #1123       qzhou      Modified getKIndex for gamma value
 * 06/23/2014   R4152       qzhou      Fixed on getQHAQDC formula
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class CalcEach1min {
    private static final float MISSING_VAL = 99999.99f;

    private static final int MAX_GAP_LENGTH = 15;

    private static final int SMOOTH_WINDOW = 60;

    private static final int TRANSITION_TIME = 60;

    private static final int PHASE_POWER = 3;

    private static final int HARM_ORDER = 5;

    private static int HOURS = 24;

    private static int MINUTES = 60;

    /*
     * @param dataIn -- data of 4320
     */
    public static float[] fillGaps(float[] dataIn) {
        float[] data = dataIn.clone();
        int i = 0;
        int size = data.length;

        while (i < size) {

            // Find the next missing value
            int flag = 0; // flag used for break
            while (i < size && flag == 0) {
                if (data[i] == MISSING_VAL)
                    flag = 1;
                else
                    i++;
            }

            // If a gap was found handle it
            if (i < size) {
                int gapIndex = i; // index of first missing value

                // Find the last missing point
                flag = 0;
                while (i < size && flag == 0) {
                    if (data[i] != MISSING_VAL)
                        flag = 1;
                    else
                        i++;
                }

                // Interpolate the gap if possible. We cannot extrapolate
                if ((gapIndex > 0) && (i < size)) {
                    // Now i is the index of first non-missing value
                    // and GapIndex is the index of first missing value
                    int gapLength = i - gapIndex; // i is index of first
                                                  // non-missing value

                    // Interpolate if the gap is small enough
                    if (gapLength < MAX_GAP_LENGTH) {
                        float value1 = data[gapIndex - 1];
                        float value2 = data[i];
                        for (int j = 1; j < gapLength + 1; j++)
                            data[gapIndex++] = value1 + (j * (value2 - value1))
                                    / (gapLength + 1);
                    }
                }
            }
        }

        return data;
    }

    /*
     * 24 element floating point array. (DefLength + 30 + kLength) Find out how
     * many points are used to get the centered hour average
     */
    public static float[] getFitLength(float[] defLength, float[] kIndex,
            float[] kLength) {
        float[] fitLength = new float[HOURS];
        int[] ind = new int[HOURS];
        float[] curK = new float[HOURS];

        for (int i = 0; i < HOURS; i++) {
            fitLength[i] = 30.0f + defLength[i];
            ind[i] = (int) Math.floor(i / 3.0f);
            curK[i] = kIndex[ind[i]];

            if (curK[i] != MISSING_VAL)
                fitLength[i] += kLength[(int) curK[i]];

            if (fitLength[i] > 1440.0f)
                fitLength[i] = 1440.0f;
        }

        return fitLength;
    }

    /*
     * @param data (hhdata, dddata), float[4320]
     * 
     * @return -- 24 element floating point array. Calculate averages centered
     * on each hour of the day
     */
    public static float[] getCentHourAvg(float[] data, float[] fitLength,
            float[] defLength) {

        float[] HrAvg = new float[HOURS]; // double
        Arrays.fill(HrAvg, MISSING_VAL);

        for (int ihr = 0; ihr < HOURS; ihr++) {
            // take middle interval
            int center = 1440 + ihr * MINUTES + 30;
            int start = center - Math.round(fitLength[ihr]);
            int end = center + Math.round(fitLength[ihr]);
            int missing = 0;
            double sum = 0;

            // if data[i] have no missing value
            for (int i = start; i < end + 1; i++) {

                if (data[i] != MISSING_VAL) {
                    sum += data[i];
                } else {
                    missing++;
                    break;// this loop
                }
            }

            if (missing == 0) // no missing value
                HrAvg[ihr] = (float) sum / (end - start + 1);
        }

        // if HrAvg have missing value
        // Extrapolate the first missing points--missing beginning
        int hr0 = 0;
        int flag = 0;
        while (hr0 < HOURS && flag == 0) {
            if (HrAvg[hr0] != MISSING_VAL)
                flag = 1;
            else {
                hr0++;

            }
        }
        if (hr0 > 0 && hr0 < HOURS)
            for (int i = 0; i < hr0; i++)
                HrAvg[i] = HrAvg[hr0];

        // Extrapolate the last missing points--missing end
        int hr1 = 23;
        while ((hr1 > hr0) && (HrAvg[hr1] == MISSING_VAL))
            hr1--;
        if (hr1 < 23)
            for (int i = hr1 + 1; i < HOURS; i++)
                HrAvg[i] = HrAvg[hr1];

        // Interpolate the missing points between hour0 and hour1
        // Both hour0 and hour1 are hours where data exists
        while (hr0 < hr1) {

            do {
                hr0++;
            } while (hr0 < hr1 && HrAvg[hr0] != MISSING_VAL);

            if (hr0 < hr1) {
                int hr = hr0; // first missing hour
                while ((hr0 < hr1) && (HrAvg[hr0] == MISSING_VAL))
                    hr0++;
                int gapLength = hr0 - hr;
                float value1 = HrAvg[hr - 1];// not missing
                float value2 = HrAvg[hr0]; // not missing

                for (int i = 1; i < gapLength + 1; i++)
                    HrAvg[hr++] = value1 + (i * (value2 - value1))
                            / (gapLength + 1);
            }
        }

        return HrAvg;
    }

    /*
     * @param hrAvg -- QHA data 1440
     */
    public static float[] getHarmonicFit(float[] hrCentAvg) {
        float[] fitCurve = new float[1440];
        int delta = MINUTES; // minutes between points in HrAvg
        int t0 = 30; // time tag for first point in HrAvg
        float t1 = (HOURS - 1) * delta + t0; // time tag for last point in HrAvg

        // Rotate HrAvg so that 1st and last points are equal, store in HA
        float r_coeff = (hrCentAvg[HOURS - 1] - hrCentAvg[0]) / (t1 - t0);
        float[] hrA = new float[HOURS];// 0.0*HrAvg
        for (int i = 0; i < HOURS; i++)
            hrA[i] = hrCentAvg[i] - r_coeff * (i * delta);

        // Calculate first Fourier series coefficients up to Horder
        float[] reA = new float[HARM_ORDER + 1]; // real part of the Fourier
                                                 // Series Coefficients
                                                 // (initially 0)
        float[] imA = new float[HARM_ORDER + 1]; // imaginary part of Fourier
                                                 // Series Coefficients
                                                 // (initially 0)
        for (int i = 0; i < HARM_ORDER + 1; i++) {
            for (int j = 0; j < HOURS; j++) {
                reA[i] += hrA[j] * Math.cos(2 * (Math.PI) * j * i / HOURS);
                imA[i] -= hrA[j] * Math.sin(2 * (Math.PI) * j * i / HOURS);
            }
        }

        // Derive FitCurve as harmonic fit using inverse transform
        for (int t = 0; t < HOURS * delta; t++) { // t is minute of the day
            float theta = (float) (2 * (Math.PI) * (t - t0) / (HOURS * delta));
            fitCurve[t] = reA[0] / HOURS;
            for (int i = 1; i < HARM_ORDER + 1; i++)
                fitCurve[t] += (2 * reA[i] * Math.cos(i * theta) - 2 * imA[i]
                        * Math.sin(i * theta))
                        / HOURS;

            // Derotate FitCurve by same amount as HrAvg
            fitCurve[t] += r_coeff * (t - t0);
        }

        return fitCurve;
    }

    /*
     * @param hdev,ddev -- float[1440]
     */
    public static List getKIndex(float[] hdev, float[] ddev, int[] kLimit,
            int missingFlag) {
        List<float[]> list = new ArrayList<float[]>();

        // Initialize the return data with MissingValue
        float[] kIndex = new float[8];
        float[] hk = new float[8];
        float[] dk = new float[8];
        float[] gamma = new float[8];
        float[] hGamma = new float[8];
        float[] dGamma = new float[8];

        Arrays.fill(kIndex, MISSING_VAL);
        Arrays.fill(hk, MISSING_VAL);
        Arrays.fill(dk, MISSING_VAL);
        Arrays.fill(gamma, MISSING_VAL);
        Arrays.fill(hGamma, MISSING_VAL);
        Arrays.fill(dGamma, MISSING_VAL);

        // Check for bad input data
        int npts = hdev.length;
        if (npts != ddev.length)
            return list;

        if (npts < 1261 || npts > 1440) // 21*60+1
            return list;

        // Step through each three hourly interval
        for (int ipd = 0; ipd < 8; ipd++) {
            int istart = ipd * 180;
            int iend = istart + 180 - 1;
            if (iend >= npts)
                iend = npts - 1; // allow for partial interval on the end
            if (iend < istart)
                continue; // should never happen...

            // Check for missing data
            int i = 0;
            int ii = 0;
            int npdpts = iend - istart + 1; // number of possible points in the
                                            // period, =180
            float[] hhdev = new float[npdpts];
            float[] dddev = new float[npdpts];

            for (int j = istart; j < iend + 1; j++) {
                hhdev[j - istart] = hdev[j];
                dddev[j - istart] = ddev[j];
            }

            // get hdevGood
            for (i = npdpts - 1; i >= 0; i--)
                if (hhdev[i] != MISSING_VAL && hhdev[i] != 0)
                    break;

            for (ii = npdpts - 1; ii >= 0; ii--)
                if (dddev[ii] != MISSING_VAL && dddev[ii] != 0)
                    break;

            // i, ii are the last data that is not missing
            float[] hdevGood = new float[i + 1];
            float[] ddevGood = new float[ii + 1];
            if (i > -1)
                for (int j = 0; j < i + 1; j++)
                    hdevGood[j] = hhdev[j];
            if (ii > -1)
                for (int j = 0; j < ii + 1; j++)
                    ddevGood[j] = dddev[j];

            if (missingFlag == 0 || (i > -1 && ii > -1)) {
                if (hdevGood != null && hdevGood.length != 0)
                    hGamma[ipd] = CalcUtil.maxValue(hdevGood)
                            - CalcUtil.minValue(hdevGood);
                if (ddevGood != null && ddevGood.length != 0)
                    dGamma[ipd] = CalcUtil.maxValue(ddevGood)
                            - CalcUtil.minValue(ddevGood);

                if (hGamma[ipd] != MISSING_VAL)
                    hk[ipd] = CalcUtil.getKfromTable(kLimit, hGamma[ipd]);

                if (dGamma[ipd] != MISSING_VAL)
                    dk[ipd] = CalcUtil.getKfromTable(kLimit, dGamma[ipd]);

                // get bigger one
                if (hGamma[ipd] >= dGamma[ipd] && hGamma[ipd] != MISSING_VAL) {
                    kIndex[ipd] = hk[ipd];
                    gamma[ipd] = hGamma[ipd];
                } else if (dGamma[ipd] >= hGamma[ipd]
                        && dGamma[ipd] != MISSING_VAL) {
                    kIndex[ipd] = dk[ipd];
                    gamma[ipd] = dGamma[ipd];
                }
            }
        }

        list.add(0, kIndex);
        list.add(1, gamma);
        list.add(2, hk);
        list.add(3, hGamma);
        list.add(4, dk);
        list.add(5, dGamma);

        return list;
    }

    /*
     * Force QHAQDC and QDAQDC to be continuous between the last and the first
     * value using a +/- SMOOTH_WINDOW
     */
    public static float[] getQHAQDC(float[] qdc) {
        float[] data = qdc.clone(); // new float[1440];

        if (qdc.length != 1440)
            return data;

        float jump = qdc[0] - qdc[1439];

        for (int i = 0; i < SMOOTH_WINDOW; i++) {

            data[1440 - SMOOTH_WINDOW + i] += ((float) i / (SMOOTH_WINDOW - 1))
                    * 0.5f * jump;
            data[i] -= (1.0f - (float) i / (SMOOTH_WINDOW - 1)) * 0.5f * jump;

        }

        return data;
    }

    /*
     * find index in hhdata that indicates current time currTimeIndex = first
     * 1440 minutes + prev day minutes + curr day minutes
     */
    public static int getCurrTimeIndex(int hour, int min, int epHour) {

        if (epHour == 0)
            epHour = 24;

        int currTimeIndex = HOURS * MINUTES + (HOURS - epHour) * MINUTES + hour
                * MINUTES + min;

        return currTimeIndex;
    }

    /*
     * 
     */
    public static float[] getExtrapolation(float[] dataIn, float[] qhaQdc,
            int currTimeIndex) { // 4320
        float[] data = dataIn.clone();
        int j0 = currTimeIndex;// Last good H or D index

        if (data.length != 4320 || qhaQdc.length != 1440)
            return data;

        if (data[j0] != MISSING_VAL) {
            for (int j = j0 + 1; j < 4320; j++) {
                int w2 = j - j0 - 1; // from .pro
                int w1 = TRANSITION_TIME - w2;

                if (w1 < 0)
                    w1 = 0;

                data[j] = (w1 * data[j0] + w2 * qhaQdc[j % 1440]) / (w1 + w2);
            }
        }

        return data;
    }

    public static float[] getDev(float[] data, float[] qdc) {
        float[] dev = new float[1440];

        if (data.length != 4320 || qdc.length != 1440)
            return dev;

        for (int i = 0; i < 1440; i++) {

            if (data[i + 1440] != MISSING_VAL && qdc[i] != MISSING_VAL)
                dev[i] = data[i + 1440] - qdc[i];
            else
                dev[i] = MISSING_VAL;
        }

        return dev;
    }

    public static float[] adjustHrCentAvg(float[] hcAIn, float[] qha,
            float[] gamma, int[] kLimit) {
        float[] hcA = hcAIn.clone();
        float wh = 0;

        if (hcA.length != HOURS || gamma.length != 8)
            return hcA;

        for (int ipd = 0; ipd < 8; ipd++) {
            if (gamma[ipd] < kLimit[4])
                wh = 1;
            else if (gamma[ipd] >= kLimit[4] && gamma[ipd] < kLimit[6])
                wh = (float) Math.pow(
                        ((kLimit[6] - gamma[ipd]) / (kLimit[6] - kLimit[4])),
                        PHASE_POWER);
            else
                wh = 0;

            for (int j = 0; j < 3; j++) {
                hcA[ipd * 3 + j] = wh * hcA[ipd * 3 + j] + (1 - wh)
                        * qha[ipd * 3 + j];// ?

            }
        }

        return hcA;
    }

    /*
     * wraper function for a few functions in this class.
     * 
     * @param -- hdata, H or D data
     * 
     * @param -- hQdc, H or D quiet day curve. float[1440]
     * 
     * @param -- currTimeIndex, current time index in the array
     * 
     * @return -- hDev or dDev. float[1440]
     */
    public static float[] getHdevOrDDev(float[] hdata, float[] hQdc,
            int currTimeIndex) {
        float[] hDev = null;

        float[] hhdata = CalcEach1min.fillGaps(hdata);

        hDev = CalcEach1min.getDev(hhdata, hQdc);// [1440]

        return hDev;
    }
}
