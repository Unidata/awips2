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
package com.raytheon.uf.viz.cloudheight.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.cloudheight.data.CloudHeightData;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource;
import com.raytheon.uf.viz.cloudheight.data.SoundingSource.SourceType;

/**
 * Cloud height calculations ported from HH_Grid.C and HH_Climo.C
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CloudHeightCalculatorPorted {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CloudHeightCalculatorPorted.class);

    public static class CloudHeightResult {
        public float value;

        public String status = "";

    }

    private static class Record {

        public float t;

        public int z;

        public int p;
    }

    private static Record[][] SoundingJan = new Record[5][4];

    private static Record[][] SoundingJul = new Record[5][4];

    public static CloudHeightResult getCloudHeightGrid(float cloudTemp,
            VerticalSounding sounding, SoundingSource source,
            float[] parcelTrajectory) {
        CloudHeightResult result = new CloudHeightResult();
        result.status = "S";
        int count = sounding.size();
        float height, warmestTemp, coldestTemp;

        int intersectionCount = 0;
        int i1, i2, i, j, coldestTempSub;
        boolean dwptDrying = false;

        float dd1, dd2, dd3, dz, d2x;
        float d2xparam;
        float P2, P1, T2, T1, Z2, Z1, pres, presCloud = 99999.0f;
        float Tenv500, Tparcel, Tparcel500, presTrajL, presTrajU;
        float presEL;

        // if there are no sounding levels we want to display no data
        if (count < 1) {
            result.value = Float.NaN;
            return result;
        }

        if (source.getType() == SourceType.RAOB) {
            d2xparam = .0005f;
        } else {
            d2xparam = 1e-5f;
        }

        warmestTemp = 0;
        coldestTemp = 999;
        coldestTempSub = count - 1;

        for (i = 0; i < count; i++) {
            float T = sounding.get(i).getTemperature();
            if (T > warmestTemp)
                warmestTemp = T;
            if (T < coldestTemp) {
                coldestTemp = T;
                coldestTempSub = i;
            }
        }

        if (cloudTemp > warmestTemp) {
            result.status = "W";
            result.value = 0;
            return result;
        }
        // Set i2, i1 to default values near tropopause of sounding, so that in
        // event
        // that cloud temp is colder than sounding, a height can be
        // extrapolated.
        i2 = coldestTempSub;
        i1 = coldestTempSub - 1;
        for (i = 0; i < (count - 1); i++) {
            if ((cloudTemp <= sounding.get(i).getTemperature() && cloudTemp > sounding
                    .get(i + 1).getTemperature())
                    || (cloudTemp >= sounding.get(i).getTemperature() && cloudTemp < sounding
                            .get(i + 1).getTemperature())) {
                if (intersectionCount == 0) {
                    i1 = i;
                    i2 = i + 1;
                }
                intersectionCount++;
            }
            // Test for dewpoint drying for finding low-cloud tops.
            // Use 2nd Derivative test on Dewpoint Depressions (DD) from Taylor
            // Series/
            // Finite Differencing. (Fred Mosher). Using x as Heights, and f(x)
            // as
            // Dewpoint Depressions, the 2nd Derivative test of > 0 implies a
            // relative minima in the curve and dewpoint drying.
            if (i > 0 && sounding.get(i).getDewpoint() > 220
                    && sounding.get(i).getDewpoint() < 305) {
                dd1 = sounding.get(i - 1).getTemperature()
                        - sounding.get(i - 1).getDewpoint();
                dd2 = sounding.get(i).getTemperature()
                        - sounding.get(i).getDewpoint();
                dd3 = sounding.get(i + 1).getTemperature()
                        - sounding.get(i + 1).getDewpoint();
                dz = (sounding.get(i + 1).getGeoHeight() - sounding.get(i - 1)
                        .getGeoHeight()) / 2;
                if (dz == 0)
                    dz = 1;
                d2x = (dd3 - 2 * dd2 + dd1) / (dz * dz);
                if (Math.abs(cloudTemp - sounding.get(i).getTemperature()) < 4.5
                        && d2x > d2xparam) {
                    dwptDrying = true;
                    i1 = i;
                    intersectionCount++;
                    break;
                }
            }
        }

        if (dwptDrying)
            height = sounding.get(i1).getGeoHeight();
        else if (Math.abs(sounding.get(i2).getTemperature()
                - sounding.get(i1).getTemperature()) > 0.0000005) {
            height = (sounding.get(i2).getGeoHeight() - sounding.get(i1)
                    .getGeoHeight())
                    * (cloudTemp - sounding.get(i1).getTemperature())
                    / (sounding.get(i2).getTemperature() - sounding.get(i1)
                            .getTemperature())
                    + sounding.get(i1).getGeoHeight();
            presCloud = (sounding.get(i2).getPressure() - sounding.get(i1)
                    .getPressure())
                    * (cloudTemp - sounding.get(i1).getTemperature())
                    / (sounding.get(i2).getTemperature() - sounding.get(i1)
                            .getTemperature()) + sounding.get(i1).getPressure();
        } else {
            height = sounding.get(i1).getGeoHeight(); // Use lower height on
            // isothermal layer.
            presCloud = sounding.get(i1).getPressure();
        }
        ;
        if (intersectionCount > 1)
            result.status = "M"; // Multiple height intersections found -
        // returning lowest height
        if (dwptDrying)
            result.status = "D"; // Height estimated from dewpoint drying
        if (intersectionCount == 0)
            result.status = "C"; // Cloud Temp colder than sounding...Solve for
        // pres/height
        // from parcel trajectory instead

        // Compute LI Tenv-Tparcel at ~500mb
        Tenv500 = 999.0f;
        presEL = -99999;
        Tparcel500 = parcelTrajectory[10];
        for (i = 0; i < count; i++)
            if (sounding.get(i).getPressure() <= 500) {
                Tenv500 = sounding.get(i).getTemperature();
                // find EL...
                if (Tenv500 - Tparcel500 < -.5) {
                    for (j = i + 1; j < count; j++) {
                        // solve for T of Parcel Trajectory at P
                        presTrajL = ((int) ((sounding.get(j).getPressure() + 49.0) / 50)) * 50;
                        presTrajU = presTrajL - 50;
                        Tparcel = (parcelTrajectory[20 - (int) (presTrajU / 50)] - parcelTrajectory[20 - (int) (presTrajL / 50)])
                                / -50
                                * (sounding.get(j).getPressure() - presTrajL)
                                + parcelTrajectory[20 - (int) (presTrajL / 50)];
                        if (sounding.get(j).getTemperature() > Tparcel) {
                            presEL = sounding.get(j - 1).getPressure();
                            break;

                        }
                    }
                }
                break;
            }
        ;
        if (((Tenv500 - Tparcel500) < -0.5 && presEL > presCloud && sounding
                .get(count - 1).getPressure() <= 100)
                || result.status.equals("C")) {
            for (i = 0; i < 19; i++) {
                if (cloudTemp <= parcelTrajectory[i]
                        && cloudTemp > parcelTrajectory[i + 1]) {
                    // linearly solve for pres...although scale is logarithmic
                    P2 = 1000 - (i + 1) * 50;
                    P1 = 1000 - (i) * 50;
                    T2 = parcelTrajectory[i + 1];
                    T1 = parcelTrajectory[i];
                    pres = -50 / (T2 - T1) * (cloudTemp - T1) + P1;
                    // ok find correlating height starting from top of sounding
                    for (j = count - 2; j >= 0; j--) {
                        if (pres <= sounding.get(j).getPressure()
                                && pres > sounding.get(j + 1).getPressure()) {
                            Z2 = sounding.get(j + 1).getGeoHeight();
                            Z1 = sounding.get(j).getGeoHeight();
                            P2 = sounding.get(j + 1).getPressure();
                            P1 = sounding.get(j).getPressure();
                            height = (Z2 - Z1) / (P2 - P1) * (pres - P1) + Z1;
                            break;
                        }
                    }
                    ;
                    break;
                }
            }
            ;
        }
        result.value = height;
        return result;
    }

    private static boolean loadedClimo = false;

    public static int getCloudHeightClimo(final float cloudTemp, float lat,
            int day, VerticalSounding sounding) {
        int[] lats = { 15, 30, 45, 60, 75 };
        final float pi = 3.14159f;
        float zl1jan, zl2jan, zl1jul, zl2jul, zjan, zjul;
        long height = -99999;
        int i, i1, i2, j;

        if (!loadedClimo) {
            File climo = PathManagerFactory.getPathManager().getStaticFile(
                    CloudHeightData.CLOUDHEIGHT_DATA_DIR + File.separator
                            + "HH_ClimoSounding.txt");
            BufferedReader reader = null;
            try {
                reader = new BufferedReader(new FileReader(climo));
                String line;

                for (i = 0; i < 5; i++) {
                    for (j = 0; j < 4; j++) {
                        line = reader.readLine();
                        String[] vals = line.split("[,]");
                        SoundingJan[i][j] = new Record();
                        SoundingJan[i][j].t = Float.parseFloat(vals[0]);
                        SoundingJan[i][j].z = Integer.parseInt(vals[1]);
                        SoundingJan[i][j].p = Integer.parseInt(vals[2]);
                    }
                }
                reader.readLine();
                for (i = 0; i < 5; i++) {
                    for (j = 0; j < 4; j++) {
                        line = reader.readLine();
                        String[] vals = line.split("[,]");
                        SoundingJul[i][j] = new Record();
                        SoundingJul[i][j].t = Float.parseFloat(vals[0]);
                        SoundingJul[i][j].z = Integer.parseInt(vals[1]);
                        SoundingJul[i][j].p = Integer.parseInt(vals[2]);
                    }
                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unable to locate HH_ClimoSounding.txt", e);
                return -1;
            } catch (IOException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error reading HH_ClimoSounding.txt", e);
                return -1;
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                "Error closing BufferedReaer", e);
                    }
                }
            }
            loadedClimo = true;
        }

        float Temp = cloudTemp * 10;
        float wt1, wt2;
        float janwt, julwt;
        if (lat < 0) {
            day += 180;
            lat = Math.abs(lat);
        }

        for (i = 0; i < 5; i++) {
            if (lat < lats[i] || (lat >= 75.0f && i == 4)) {
                i1 = i - 1;
                i2 = i;
                if (lat < 15.0f) {
                    i1 = i;
                }

                // 1:Interpolate Heights to breakpoints in Jan atmos for Lat1 &
                // Lat2
                // 2:Interpolate Heights to breakpoints in Jul atmos for Lat1 &
                // Lat2
                zl1jan = getintht(Temp, SoundingJan[i1]);
                zl2jan = getintht(Temp, SoundingJan[i2]);
                zl1jul = getintht(Temp, SoundingJul[i1]);
                zl2jul = getintht(Temp, SoundingJul[i2]);

                // 3:Interpolate Hts from zl1jan & zl2jan to nearest Latitude
                // for Jan
                // 4:Interpolate Hts from zl1jul & zl2jul to nearest Latitude
                // for Jul
                wt2 = (lat - lats[i1]) / 15;
                wt1 = 1.0f - wt2;
                zjan = wt2 * zl2jan + wt1 * zl1jan;
                zjul = wt2 * zl2jul + wt1 * zl1jul;

                // 5:Given Ht for Jan and Jul...Perform cosine interpolation to
                // day
                // Temps/Heights vary with the inverse of the cosine over a year
                // P=365 days. Using y=A cos (B x)+ C...we want period P of
                // cosine
                // wave to span 365 days. Using trig...Let P=2pi/B...solving for
                // B->B=2pi/365. We want amplitude to be 1/2 the range of the
                // Height variance. So A=(zjul-zjan)/2. The cosine function by
                // itself ranges from -1 to 1...we want the cosine function
                // above
                // the zero line...so add 1 to it. So
                // Z=A(-cos(day*2pi/365)+1)/2+zjan.

                julwt = (float) ((-Math.cos(day * 2 * pi / 365.0) + 1.0) / 2.0);
                janwt = 1.0f - julwt;
                height = (long) (julwt * zjul + janwt * zjan);

                // convert height in 10s of Km to meters
                height *= 100;

                // If the sounding pointer is there, get a climo sounding.
                if (sounding == null) {
                    break;
                }

                for (j = 0; j < 4; j++) {
                    float T = 0.1f * (wt1 * janwt * SoundingJan[i1][j].t + wt1
                            * julwt * SoundingJan[i1][j].t + wt2 * janwt
                            * SoundingJan[i2][j].t + wt2 * julwt
                            * SoundingJan[i2][j].t);
                    float z = 100.0f * (wt1 * janwt * SoundingJan[i1][j].z
                            + wt1 * julwt * SoundingJan[i1][j].z + wt2 * janwt
                            * SoundingJan[i2][j].z + wt2 * julwt
                            * SoundingJan[i2][j].z);
                    float p = (wt1 * janwt * SoundingJan[i1][j].p + wt1 * julwt
                            * SoundingJan[i1][j].p + wt2 * janwt
                            * SoundingJan[i2][j].p + wt2 * julwt
                            * SoundingJan[i2][j].p);

                    SoundingLayer layer = new SoundingLayer();
                    layer.setPressure(p);
                    layer.setGeoHeight(z);
                    layer.setTemperature(T);
                    layer.setDewpoint(T - 10);
                    sounding.addLayer(layer);
                }

                break;

            }
        }

        return (int) height;
    }

    private static float getintht(float Temp, Record[] Sound) {
        float height;
        int i, i1, i2;

        if (Temp > Sound[0].t) {
            return 0;
        }

        i2 = 3;
        i1 = 2;
        for (i = 1; i < 4; i++) {
            if (Temp > Sound[i].t) {
                i1 = i - 1;
                i2 = i;
                break;
            }
        }
        height = (Sound[i2].z - Sound[i1].z) * (Temp - Sound[i1].t)
                / (Sound[i2].t - Sound[i1].t) + Sound[i1].z;

        return height;
    }

    public static void findHighPredLowBrightness(byte[] elements,
            int numElements, int[] values) {
        int highest = 0, pred = 0, lowest = 255;
        int i, j;
        int[] freq = new int[256];
        int[] smoothFreq = new int[256];
        int sum, sMostFrequent, sMostFrequentSub, mostFrequent, mostFrequentSub;

        for (i = 0; i < 256; i++) {
            freq[i] = 0;
            smoothFreq[i] = 0;
        }
        for (i = 0; i < numElements; i++) {
            if (unsignByte(elements[i]) != 0) {
                freq[unsignByte(elements[i])]++;
                if (unsignByte(elements[i]) > highest)
                    highest = unsignByte(elements[i]);
                if (unsignByte(elements[i]) < lowest)
                    lowest = unsignByte(elements[i]);
            }
        }

        for (i = 7; i < 248; i++) {
            sum = 0;
            for (j = -7; j < 8; j++) {
                sum += freq[i + j];
            }
            smoothFreq[i] = (int) (sum / 15.0 + .5);
        }

        sMostFrequent = 0;
        sMostFrequentSub = 0;
        for (i = 7; i < 248; i++) {
            if (smoothFreq[i] > sMostFrequent) {
                sMostFrequent = smoothFreq[i];
                sMostFrequentSub = i;
            }
        }

        mostFrequent = 0;
        mostFrequentSub = 0;
        if (sMostFrequentSub >= 7) {
            for (i = sMostFrequentSub - 7; i < sMostFrequentSub + 8; i++) {
                if (freq[i] > mostFrequent) {
                    mostFrequent = freq[i];
                    mostFrequentSub = i;
                }
            }
        }

        pred = mostFrequentSub;

        values[0] = highest;
        values[1] = pred;
        values[2] = lowest;
    }

    private static int unsignByte(byte b) {
        return (int) (b & 0xFF);
    }
}
