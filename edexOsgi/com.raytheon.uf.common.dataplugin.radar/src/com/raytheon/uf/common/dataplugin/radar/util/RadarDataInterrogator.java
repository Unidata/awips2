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
package com.raytheon.uf.common.dataplugin.radar.util;

import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2011            mschenke     Initial creation
 * 05/02/2013   DR 14587   D. Friedman  Store base velocity
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarDataInterrogator {

    public enum DataType { BASE_VELOCITY }

    private double lastRange;

    private double lastAzimuth;

    private int lastRadialIndex;

    private int lastBin;

    private RadarRecord record;

    public RadarDataInterrogator(RadarRecord record) {
        this.record = record;
    }

    public double getLastRangeInterrogated() {
        return lastRange;
    }

    public double getLastAzimuthInterrogated() {
        return lastAzimuth;
    }

    public int getLastRadialIndexInterrogated() {
        return lastRadialIndex;
    }

    public int getLastBinInterrogated() {
        return lastBin;
    }

    public int getDataValue(RadarRecord record, Coordinate latLon) {
        return getDataValue(record, latLon, (DataType) null);
    }

    public int getDataValue(RadarRecord record, Coordinate latLon, DataType dataType) {
        this.record = record;
        return getDataValue(latLon, dataType);
    }

    public int getDataValue(Coordinate latLon) {
        return getDataValue(latLon, null);
    }

    public int getDataValue(Coordinate latLon, DataType dataType) {
        int[] rval = getDataValues(new Coordinate[] { latLon }, dataType);
        if (rval != null && rval.length > 0) {
            return rval[0];
        }

        return 0;
    }

    public int[] getDataValues(Coordinate[] latLonArray) {
        return getDataValues(latLonArray, null);
    }

    /**
     * Used to return the data value for the lat/lon passed in. Used for raster
     * and radial data. Use the getGraphicDataValue() method for graphic data.
     * 
     * @param latLon
     * @return
     */
    public int[] getDataValues(Coordinate[] latLonArray, DataType dataType) {

        double[] input = new double[latLonArray.length * 2];
        double[] output = new double[input.length];

        int index = 0;
        for (Coordinate latLon : latLonArray) {
            input[index++] = latLon.x;
            input[index++] = latLon.y;
        }

        try {
            MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(
                    record.getCRS());

            mt.transform(input, 0, output, 0, latLonArray.length);
        } catch (Exception e) {
            return null;
        }

        int[] rval = new int[latLonArray.length];

        if ("Graphic".equals(record.getFormat())) {
            // **********************************
            // Graphic case
            // **********************************

            return null;
        } else if ("Radial".equals(record.getFormat())) {
            // **********************************
            // Radial case
            // **********************************
            int x = 0;
            int y = 0;
            for (index = 0; index < latLonArray.length; index++) {
                x = index * 2;
                y = x + 1;

                // Calculate azimuth
                this.lastAzimuth = Math.toDegrees(Math.atan2(output[x],
                        output[y]));
                if (lastAzimuth < 0.0) {
                    lastAzimuth += 360.0;
                }

                // Calculate range
                this.lastRange = Math.hypot(output[x], output[y]);

                if (record.getAngleData() != null) {
                    // Find the correct Radial for the range and azimuth
                    this.lastRadialIndex = -1;
                    for (int i = 0; i < record.getNumRadials(); i++) {
                        double thisAz = record.getRadialAzimuth(i);
                        double nextAz;
                        if (i + 1 == record.getNumRadials()) {
                            float totalDiff = record.getAngleData()[record
                                    .getNumRadials() - 1]
                                    - record.getAngleData()[0];
                            if (totalDiff < 0) {
                                totalDiff += 360;
                            }

                            float average = totalDiff
                                    / (record.getNumRadials() - 1);

                            // determine if average dist is close enough to dist
                            // at [0].
                            float actualDiff = 360 - totalDiff;

                            // if the difference between actual and calculated
                            // is less than half a degree, we can assume the
                            // radial data does in fact wrap all the way in a
                            // circle and we can use the starting angle as our
                            // ending angle. Otherwise, we must assume the data
                            // is a slice of a circle and we should use our
                            // calculated end location
                            nextAz = record.getAngleData()[record
                                    .getNumRadials() - 1] + average;
                            if (Math.abs(actualDiff - average) < 0.5) {
                                nextAz = record.getAngleData()[0] + 360.0f;
                            }
                        } else {
                            nextAz = record.getRadialAzimuth(i + 1);
                        }
                        if (nextAz < thisAz) {
                            if (this.lastAzimuth > thisAz) {
                                nextAz += 360.0;
                            } else {
                                thisAz -= 360.0;
                            }
                        }
                        if ((this.lastAzimuth >= thisAz)
                                && (this.lastAzimuth < nextAz)) {
                            this.lastRadialIndex = i;
                            break;
                        }
                    }

                    // Get the bin number
                    this.lastBin = (int) (this.lastRange / (record
                            .getGateResolution() * Math.cos(Math
                            .toRadians(record.getTrueElevationAngle()))));

                    // Get the data value if the bin is valid
                    int startBin = 0;
                    int endBin = record.getNumBins();
                    if (record.getJstart() != null) {
                        endBin += startBin = record.getJstart();
                    }
                    if (this.lastRadialIndex >= 0 && this.lastBin >= startBin
                            && this.lastRadialIndex < record.getNumRadials()
                            && this.lastBin < endBin) {
                        if (record.srmData != null && dataType != DataType.BASE_VELOCITY) {
                            rval[index] = RadarRecordUtil.getSRMDataValue(
                                    record, this.lastRadialIndex, this.lastBin
                                            - startBin) & 0xFF;
                        } else {
                            rval[index] = record.getRawIntDataValue(
                                    this.lastRadialIndex, this.lastBin
                                            - startBin);
                        }
                    }
                }
            }
        } else if ("Raster".equals(record.getFormat())) {
            // **********************************
            // Raster case
            // **********************************
            int x = 0;
            int y = 0;
            for (index = 0; index < latLonArray.length; index++) {
                x = index * 2;
                y = x + 1;

                int col = (int) (output[x] / record.getGateResolution() + (record
                        .getNumBins() / 2));

                int row = (int) (output[y] / record.getGateResolution() + (record
                        .getNumBins() / 2));

                row = record.getNumBins() - row - 1;

                if ((row >= 0) && (row < record.getNumBins()) && (col >= 0)
                        && (col < record.getNumBins())) {
                    rval[index] = (int) record.getRawDataValue(row, col);
                }
            }
        }

        return rval;
    }
}
