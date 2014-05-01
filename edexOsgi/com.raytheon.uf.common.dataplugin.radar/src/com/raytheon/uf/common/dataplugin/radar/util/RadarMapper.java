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

import java.nio.FloatBuffer;

import javax.measure.converter.UnitConverter;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * Temp class to get initial radar slice working. Needs to be abstracted
 * together with radar tiler. Maps radar data onto a given project/resolution
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2010 #4473      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class RadarMapper {

    private RadarRecord radarData;

    private UnitConverter dataConverter;

    /**
     * Number of points in the x & y directions.
     */
    private int numPoints;

    /**
     * Distance between points in meters.
     */
    private float distance;

    private int actualArrayLength;

    private double maxExtent;

    private boolean srmCreate = false;

    private float nan = -999999;

    private class TileState {

        private FloatBuffer image;

        private int xStart;

        private int yStart;

        private double curRadSin;

        private double curRadCos;

        private double nextRadSin;

        private double nextRadCos;

        private double pixelsPerBin;
    }

    public RadarMapper(RadarRecord data, int numPoints, float distance) {
        this(data, numPoints, distance, null);
    }

    /**
     * Creates an instance of the radar tiler with the given radar record.
     * 
     * @param data
     *            The radar record to tile
     * @param fullImage
     *            Indicates if the tiler should be used to create a full image
     *            instead of tiles
     */
    public RadarMapper(RadarRecord data, int numPoints, float distance,
            UnitConverter dataToImage) {
        radarData = data;
        this.numPoints = numPoints;
        this.distance = distance;
        maxExtent = distance * numPoints / 2;
        actualArrayLength = this.numPoints * this.numPoints;
    }

    public void setDataConverter(UnitConverter dataConverter) {
        this.dataConverter = dataConverter;
    }

    /**
     * 
     */
    public float[] createImage() {
        TileState state = new TileState();
        createImage(state);

        // if it's a raster product just use the raw data
        if ("Raster".equals(radarData.getFormat())) {
            byte[] rawData = radarData.getRawData();
            for (int i = 0; i < rawData.length; i++) {
                // incorrect, need to do all the spatial mapping to move the
                // raster image into the current image size
                state.image.put(convertToImage(rawData[i]));
            }
            return state.image.array();
        }

        for (int i = 0; i < state.image.capacity(); ++i) {
            state.image.put(nan);
        }

        byte[] rawData = radarData.getRawData();
        float[] angleData = radarData.getAngleData();

        // absolute topY and leftX
        int topY = numPoints / 2;
        int leftX = -topY;

        int gateDistance = radarData.getGateResolution();

        // determine number of bins to process
        float distanceRatio = gateDistance / distance;
        int maxBins = (int) (numPoints / 2.0 / distanceRatio);
        int numValidBins = Math.min(radarData.getNumBins(), maxBins);

        state.pixelsPerBin = (double) distanceRatio
                * Math.cos(Math.toRadians(radarData.getTrueElevationAngle()));

        state.xStart = leftX;
        state.yStart = topY;

        // image always spans origin in x & y
        double startAngle = 0.0;
        double endAngle = 360;
        int firstBin = 0;
        int lastBin = numValidBins;

        int numRadials = radarData.getNumRadials();
        for (int radial = 0; radial < numRadials; ++radial) {
            double currentRadial = angleData[radial];
            double nextRadial = angleData[(radial + 1) % numRadials];

            if (nextRadial < currentRadial) {
                if (startAngle == 0.0) {
                    currentRadial -= 360.0;
                } else {
                    nextRadial += 360.0;
                }
            }

            if ((nextRadial < startAngle) || (currentRadial > endAngle)) {
                continue;
            }

            state.curRadSin = Math.sin(Math.toRadians(currentRadial));
            state.curRadCos = Math.cos(Math.toRadians(currentRadial));
            state.nextRadSin = Math.sin(Math.toRadians(nextRadial));
            state.nextRadCos = Math.cos(Math.toRadians(nextRadial));
            int prevBin = firstBin;
            if (this.srmCreate) {
                // Get the value of the radar data at the angle and bin
                byte prevValue = RadarRecordUtil.getSRMDataValue(radarData,
                        radial, prevBin);
                // byte prevValue = 5;
                byte currValue;

                // Check to see how many bins does the value occupy, so the
                // pixel values can be set as a group
                for (int currBin = prevBin + 1; currBin < lastBin; ++currBin) {
                    currValue = RadarRecordUtil.getSRMDataValue(radarData,
                            radial, currBin);
                    // currValue = 5;
                    if (currValue != prevValue) {
                        calculatePixels(state, prevBin, currBin, prevValue);
                        prevBin = currBin;
                        prevValue = currValue;
                    }
                }

                // Catch the end of the radial if it was missed
                if (prevBin < lastBin) {
                    calculatePixels(state, prevBin, lastBin, prevValue);
                }
            } else {
                // Get the value of the radar data at the angle and bin
                byte prevValue = getDataValue(rawData, radial, prevBin);
                byte currValue;

                // Check to see how many bins does the value occupy, so the
                // pixel values can be set as a group
                for (int currBin = prevBin + 1; currBin < lastBin; ++currBin) {
                    currValue = getDataValue(rawData, radial, currBin);
                    if (currValue != prevValue) {
                        calculatePixels(state, prevBin, currBin, prevValue);
                        prevBin = currBin;
                        prevValue = currValue;
                    }
                }

                // Catch the end of the radial if it was missed
                if (prevBin < lastBin) {
                    calculatePixels(state, prevBin, lastBin, prevValue);
                }
            }
        }
        return state.image.array();
    }

    private byte getDataValue(byte[] rawData, int radial, int currBin) {
        return rawData[radial * radarData.getNumBins() + currBin];
    }

    /**
     * @param b
     * @return
     */
    private float convertToImage(byte b) {
        double image = dataConverter.convert((b) & 0xFF);
        if (Double.isNaN(image)) {
            image = nan;
        }

        return (float) image;
    }

    public GridGeometry2D constructGridGeometry() {
        ProjectedCRS crs = radarData.getCRS();

        GridGeometry2D gridGeometry2D = null;

        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        generalEnvelope.setRange(0, -maxExtent, maxExtent);
        generalEnvelope.setRange(1, -maxExtent, maxExtent);

        gridGeometry2D = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
                0, 0 }, new int[] { numPoints, numPoints }, false),
                generalEnvelope);

        return gridGeometry2D;
    }

    public double getMaxExent() {
        return maxExtent;

    }

    /**
     * Returns the actual width of the imagery. This is typically used for the
     * full image.
     * 
     * @return The current width
     */
    public int getWidth() {
        return numPoints;
    }

    /**
     * Returns the actual height of the imagery. This is typically used for the
     * full image.
     * 
     * @return The current height
     */
    public int getHeight() {
        return numPoints;
    }

    private void createImage(TileState state) {
        state.image = FloatBuffer.allocate(actualArrayLength);
    }

    private void calculatePixels(TileState state, int startBin, int endBin,
            byte bValue) {
        if (bValue == 0) {
            return;
        }
        float value = convertToImage(bValue);

        int xBuffer = 1;
        if (state.curRadSin < 0.0) {
            xBuffer = -1;
        }
        int yBuffer = 1;
        if (state.curRadCos < 0.0) {
            yBuffer = -1;
        }

        int minR = RadarUtil.fastRound(startBin * state.pixelsPerBin);
        int maxR = RadarUtil.fastRound(endBin * state.pixelsPerBin);
        for (int r = minR; r < maxR; ++r) {
            double lengthSpan = r;
            double xCoord = state.curRadSin * lengthSpan;
            double yCoord = state.curRadCos * lengthSpan;
            double nextXCoord = state.nextRadSin * lengthSpan;
            double nextYCoord = state.nextRadCos * lengthSpan;

            int trueX = RadarUtil.fastRound(xCoord) - state.xStart;
            int trueY = state.yStart - RadarUtil.fastRound(yCoord);
            int nextTrueX = RadarUtil.fastRound(nextXCoord) - state.xStart;
            int nextTrueY = state.yStart - RadarUtil.fastRound(nextYCoord);

            // setPixel(trueX, trueY, value);

            // if (trueX != nextTrueX || trueY != nextTrueY) {
            if (trueX == nextTrueX) {
                int diff = RadarUtil.fastAbs(trueY - nextTrueY);
                int startingY = trueY > nextTrueY ? nextTrueY : trueY;
                for (int yCount = 0; yCount <= diff; yCount++) {
                    setPixel(state, trueX, startingY + yCount, value);
                }
            } else if (trueY == nextTrueY) {
                int diff = RadarUtil.fastAbs(trueX - nextTrueX);
                int startingX = trueX > nextTrueX ? nextTrueX : trueX;
                for (int xCount = 0; xCount <= diff; xCount++) {
                    setPixel(state, startingX + xCount, trueY, value);
                }
            } else {
                int yDiff = (nextTrueY - trueY);
                int xDiff = (nextTrueX - trueX);
                double dTrueX = trueX;
                double dYDiff = yDiff;
                double dXDiff = xDiff;
                double dTrueY = trueY;
                double b = dTrueY - ((dTrueX * dYDiff) / dXDiff);
                // double b = trueY - ((trueX * yDiff) / xDiff);
                int aYDiff = RadarUtil.fastAbs(yDiff);
                int aXDiff = RadarUtil.fastAbs(xDiff);
                if (aYDiff >= aXDiff) {
                    int yStartValue = trueY > nextTrueY ? nextTrueY : trueY;
                    for (int yCounter = 0; yCounter <= aYDiff; yCounter++) {
                        int newY = yStartValue + yCounter;
                        int x = RadarUtil.fastRound(((newY - b) * xDiff)
                                / yDiff);
                        setPixel(state, x, newY, value);
                        setPixel(state, x, (newY + yBuffer), value);
                        setPixel(state, (x + xBuffer), newY, value);
                        setPixel(state, (x + xBuffer), (newY + yBuffer), value);
                    }
                } else {
                    int xStartValue = trueX > nextTrueX ? nextTrueX : trueX;
                    for (int xCounter = 0; xCounter <= aXDiff; xCounter++) {
                        int newX = xStartValue + xCounter;
                        int y = RadarUtil.fastRound(((newX * yDiff) / xDiff)
                                + b);
                        setPixel(state, newX, y, value);
                        setPixel(state, newX, (y + yBuffer), value);
                        setPixel(state, (newX + xBuffer), y, value);
                        setPixel(state, (newX + xBuffer), (y + yBuffer), value);
                    }
                }
            }
            // }
        }
    }

    private void setPixel(TileState state, int x, int y, float value) {
        if ((x >= 0) && (y >= 0) && (x < numPoints) && (y < numPoints)) {
            state.image.put(y * numPoints + x, value);
        }
    }

    public void setSRMCreate(boolean srmCreate) {
        this.srmCreate = srmCreate;
    }

    public void setNan(float nan) {
        this.nan = nan;
    }

}
