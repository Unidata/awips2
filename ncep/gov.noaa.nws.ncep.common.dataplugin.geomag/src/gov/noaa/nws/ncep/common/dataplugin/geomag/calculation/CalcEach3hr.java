package gov.noaa.nws.ncep.common.dataplugin.geomag.calculation;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/*
 * The calculation of k, 3 hour related.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * Date          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * 03/18/2014   #1123       qzhou      Add getHQdcOrDQdc
 * 06/23/2014   R4152       qzhou      Touched up functions that do not affect the results
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class CalcEach3hr {
    private static final float MISSING_VAL = 99999.99f;

    private static final int NIGHT_LENGTH = 90; // min

    private static final int DAWN_LENGTH = 60;

    private static final int DAY_LENGTH = 0;

    private static final int DUSK_LENGTH = 60;

    private static int DAYS = 30;

    private static int HOURS = 24;

    private static int MINUTES = 60;

    /*
     * calculate hrAvgs for this hour
     * 
     * @param bestList -- contains 1 hour data
     */
    public static float[] getSimpleHourAvg(List bestList) {
        float[] simpHrAvg = new float[2];
        float simpHrAvg1 = 0;
        float simpHrAvg2 = 0;
        double sum1 = 0;
        double sum2 = 0;
        int rec1 = 0;
        int rec2 = 0;

        for (int i = 0; i < bestList.size(); i++) {

            List<Float> list = (List<Float>) bestList.get(i);

            float comp1 = (Float) list.get(1);
            float comp2 = (Float) list.get(2);

            if (comp1 != MISSING_VAL) {
                sum1 += comp1;
                rec1++;
            }
            if (comp2 != MISSING_VAL) {
                sum2 += comp2;
                rec2++;
            }
        }

        if (rec1 > 30) // less than half missing value
            simpHrAvg1 = (float) sum1 / rec1;
        else
            simpHrAvg1 = MISSING_VAL;

        if (rec2 > 30) // less than half missing value
            simpHrAvg2 = (float) sum2 / rec2;
        else
            simpHrAvg2 = MISSING_VAL;

        simpHrAvg[0] = simpHrAvg1;
        simpHrAvg[1] = simpHrAvg2;

        return simpHrAvg;
    }

    /*
     * calculate hrAvgs for this day.
     * 
     * @param data -- data of one day, 1440
     */
    public static float[] getSimpleHourAvg(float[] data) { // data 1440

        float[] simpHrAvg = new float[HOURS];

        for (int ihr = 0; ihr < HOURS; ihr++) {
            double sum = 0;
            int missing = 0;

            for (int i = ihr * MINUTES; i < ihr * MINUTES + MINUTES; i++) {

                if (data[i] != MISSING_VAL)
                    sum += data[i];
                else
                    missing++;
            }

            if (missing < 30) // less than half missing value
                simpHrAvg[ihr] = (float) sum / (MINUTES - missing);
            else
                simpHrAvg[ihr] = MISSING_VAL;
        }

        return simpHrAvg;
    }

    /*
     * calculate hrAvgs for this hour in data array
     * 
     * @param data -- data of one day, 1440
     */
    public static float getSimpleHourAvg(float[] data, int hour) { // one day
                                                                   // 1440, avg
                                                                   // for hour-1

        float simpHrAvg = 0;
        double sum = 0;
        int rec = 0;

        if (data.length <= hour * MINUTES + MINUTES)
            for (int i = hour * MINUTES; i < data.length; i++) {
                if (data[i] != MISSING_VAL) {
                    sum += data[i];
                    rec++;
                }
            }
        else
            for (int i = hour * MINUTES; i < hour * MINUTES + MINUTES; i++) {
                if (data[i] != MISSING_VAL) {
                    sum += data[i];
                    rec++;
                }
            }

        if (rec > 30) // less than half missing value
            simpHrAvg = (float) sum / (rec);
        else
            simpHrAvg = MISSING_VAL;

        return simpHrAvg;
    }

    /*
     * @param simpHrAvgH -- data of 30 intervals(720 hours)
     * 
     * @return disturbance levels for 30 intervals
     */
    public static float[] getDisturbanceLevel(float[] simpHrAvgH,
            float[] simpHrAvgD) {
        float[] dB = new float[30];

        for (int j = 0; j < DAYS; j++) {
            double sum = 0;
            int count = 0;

            for (int i = 0; i < 23; i++) {
                int ii = j * HOURS + i;

                if (simpHrAvgH[ii] != MISSING_VAL
                        && simpHrAvgD[ii] != MISSING_VAL
                        && simpHrAvgH[ii + 1] != MISSING_VAL
                        && simpHrAvgD[ii + 1] != MISSING_VAL) {
                    sum += Math
                            .sqrt(Math.pow(
                                    (simpHrAvgH[ii + 1] - simpHrAvgH[ii]), 2)
                                    + Math.pow(
                                            (simpHrAvgD[ii + 1] - simpHrAvgD[ii]),
                                            2));
                    count++;
                }
            }

            if (count >= 12) // not 12 or more missing
                dB[j] = (float) sum / count;
            else
                dB[j] = MISSING_VAL;

        }

        return dB;
    }

    /*
     * @param dB -- float[30 ]
     * 
     * @return --5 smallest disturbance levels
     */
    public static Map getSmallDisturbanceLevel(float[] dB) {
        // create a map that key=dBIndex and value=dBValue.
        // create a duplicate array dBDup. Sort it.
        // take 5 smallest dBDup[i]. Then find its index and value from the dB.
        // Put them to the map
        Map<Integer, Float> dBSmall = new HashMap<Integer, Float>();

        float[] dBDup = new float[dB.length];
        for (int i = 0; i < dBDup.length; i++) {
            dBDup[i] = dB[i];
        }

        Arrays.sort(dBDup);

        float dupIndex = (int) MISSING_VAL;
        float wk = 0;
        // take 5 smallest dBDup
        for (int j = 0; j < 5; j++) {
            for (int i = 0; i < dB.length; i++) {
                if (dB[i] == dBDup[j] && i != dupIndex) { // for duplicated
                                                          // values

                    dBSmall.put(i, dB[i]);
                    dupIndex = i;
                    break;
                }
            }
        }

        return dBSmall;
    }

    /*
     * @param -- dBSmall, 5 set map
     * 
     * @param -- simpHrAvg, -- float[720]
     * 
     * @rturn -- quietLevelHourAvg, float[24]
     */
    public static float[] getQuietLevelHourAvg(Map<Integer, Float> dBSmall,
            float[] simpHrAvg) {
        if (dBSmall.entrySet().size() < 5)
            return simpHrAvg;

        float[] quietHrAvg = new float[24];
        Arrays.fill(quietHrAvg, MISSING_VAL);
        int[] index = new int[5];
        float[] dB = new float[5];

        int k = 0;
        Iterator<?> iter = dBSmall.entrySet().iterator();
        while (iter.hasNext()) {
            @SuppressWarnings("unchecked")
            Map.Entry<Integer, Float> mEntry = (Map.Entry<Integer, Float>) iter
                    .next(); // sorted on key

            index[k] = mEntry.getKey();
            dB[k] = mEntry.getValue();

            k++;
        }

        // construct smallHrAvg array (24*5) from simpHrAvg (24*30)
        float[] smallHrAvg = new float[24 * 5];

        for (int j = 0; j < 5; j++) { // k=5
            int endOfArray = smallHrAvg.length;
            int endTime = (endOfArray > j * HOURS + HOURS) ? j * HOURS + HOURS
                    : endOfArray;

            for (int i = j * HOURS; i < endTime; i++) {
                smallHrAvg[i] = simpHrAvg[index[j] * HOURS + i % HOURS]; // 700
            }
        }

        for (int ihr = 0; ihr < HOURS; ihr++) {
            float sumAvg = 0;
            float sumWk = 0;
            float wk = 0;

            for (int jk = 0; jk < 5; jk++) {
                int ind = jk * HOURS + ihr;
                if (dB[jk] < 1)
                    wk = 1;
                else
                    wk = 1.0f / (dB[jk] * dB[jk]);

                if (smallHrAvg[ind] != MISSING_VAL) {
                    sumAvg += wk * smallHrAvg[ind];
                    sumWk += wk;
                }
            }

            if (sumWk > 0)
                quietHrAvg[ihr] = sumAvg / sumWk;

        }

        return quietHrAvg;
    }

    /*
     * @param -- quietHrAvg, float[24]
     * 
     * @return -- shifted quietLevelHourAvg, float[24]
     */
    public static float[] getQHA(float[] quietHrAvg) {
        float[] QHA = new float[24];

        if (quietHrAvg.length != 24)
            return quietHrAvg;

        for (int ihr = 0; ihr < 24; ihr++) {
            QHA[ihr] = quietHrAvg[(ihr + 3) % 24];
        }

        return QHA;
    }

    /*
     * @return -- 24 element floating point array. Default fitting lengths.
     * (one for each hour of the 24 hour interval that ends at EPtime).
     */
    public static float[] getDefLength(String station, int epHour) {
        float[] defLength = new float[24];
        float lon = CalcUtil.getLongitude(station);
        int UTdiff = Math.round(1440.0f * lon / 360.0f);
        int minute0 = epHour * MINUTES;

        for (int ihr = 0; ihr < HOURS; ihr++) {
            float sum = 0;

            for (int imin = 0; imin < MINUTES; imin++) {
                int curMin = (minute0 + ihr * MINUTES + imin) % 1440;
                int localMin = (curMin + UTdiff) % 1440;

                if (localMin >= 0 && localMin < 180)
                    sum += NIGHT_LENGTH;
                else if (localMin >= 180 && localMin < 360)
                    sum += DAWN_LENGTH;
                else if (localMin >= 360 && localMin < 1080)
                    sum += DAY_LENGTH;
                else if (localMin >= 1080 && localMin < 1260)
                    sum += DUSK_LENGTH;
                else if (localMin >= 1260 && localMin < 1440)
                    sum += NIGHT_LENGTH;
            }

            defLength[ihr] = sum / MINUTES;

        }

        return defLength;
    }

    /*
     * wraper function for a few functions in this class.
     * 
     * @param -- hHrAvgs, hourly average for H. float[720]
     * 
     * @param -- dHrAvgs, hourly average for D. float[720]
     * 
     * @return -- if hHrAvgs is first param, return hQdc; if dHrAvgs is first
     * param, return dQdc. float[1440]
     */
    public static float[] getHQdcOrDQdc(float[] hHrAvgs, float[] dHrAvgs) {
        float[] hQdc = null;
        float[] qhaQdc = null;

        float[] dB = CalcEach3hr.getDisturbanceLevel(hHrAvgs, dHrAvgs);

        @SuppressWarnings("unchecked")
        Map<Integer, Float> dBsmall = CalcEach3hr.getSmallDisturbanceLevel(dB);

        float[] quietHHrAvg = CalcEach3hr
                .getQuietLevelHourAvg(dBsmall, hHrAvgs);

        // added from FMIQDCRT11_3hr.pro
        for (int k = 0; k < quietHHrAvg.length; k++) {
            if (quietHHrAvg[k] == MISSING_VAL) {
                quietHHrAvg[k] = CalcUtil.getMedian(quietHHrAvg);
            }
        }

        float[] qha = CalcEach3hr.getQHA(quietHHrAvg);

        hQdc = CalcEach1min.getHarmonicFit(qha);// [1440]

        // qhaQdc = CalcEach1min.getQHAQDC(hQdc);// [1440]

        return hQdc;
    }

}
