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

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import javax.measure.converter.MultiplyConverter;
import javax.measure.converter.UnitConverter;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.util.BufferUtil;

/**
 * 
 * A tiler class that will allow the user to take a radial container and create
 * tiles at a particular zoom level.
 * 
 * @author brockwoo
 * @version 1.0
 */
public class RadarTiler {

    private static final double TwoPi = 2.0 * Math.PI;

    private int levels;

    private RadarRecord radarData;

    private UnitConverter dataToImage;

    private int tileSize;

    private int fullResolution;

    private int actualArrayLength;

    private double maxExtent;

    private boolean srmCreate = false;

    private class TileState {

        private ByteBuffer bImage;

        private int xStart;

        private int yStart;

        private double curRadSin;

        private double curRadCos;

        private double nextRadSin;

        private double nextRadCos;

        private double pixelsPerBin;
    }

    public RadarTiler(RadarRecord data) {
        this(data, 1, data.getNumBins() * 2, null);
    }

    public RadarTiler(RadarRecord data, int levels, int tileSize) {
        this(data, levels, tileSize, null);
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
    public RadarTiler(RadarRecord data, int levels, int tileSize,
            UnitConverter dataToImage) {
        radarData = data;
        if ("Radial".equals(data.getFormat())) {
            this.levels = levels;
            this.tileSize = tileSize;
        } else if ("Raster".equals(data.getFormat())) {
            this.levels = 1;
            this.tileSize = data.getNumBins();
        }
        this.fullResolution = (int) (this.tileSize * Math.pow(2,
                this.levels - 1));

        if (dataToImage == null) {
            this.dataToImage = UnitConverter.IDENTITY;
            if (data.getNumLevels() <= 16) {
                this.dataToImage = new MultiplyConverter(16);
            }
        } else {
            this.dataToImage = dataToImage;
        }

        actualArrayLength = this.tileSize * this.tileSize;
    }

    /**
     * Creates a tile at the specific x and y location and zoom level starting
     * from the upper left corner.
     * 
     * @param tileX
     *            The horizontal location of the tile
     * @param tileY
     *            the vertical location of the tile
     * @param userZoom
     *            The zoom level to create the imagery
     * @return A byte buffer representation of the image
     */
    public ByteBuffer createTile(int tileX, int tileY, int level, boolean direct) {
        TileState state = new TileState();
        createImage(state, direct);

        // if it's a raster product just use the raw data
        if ("Raster".equals(radarData.getFormat())) {
            byte[] rawData = radarData.getRawData();
            for (int i = 0; i < rawData.length; i++) {
                state.bImage.put(convertToImage(rawData[i]));
            }
            return state.bImage;
        }

        for (int i = 0; i < state.bImage.capacity(); ++i) {
            state.bImage.put((byte) 0);
        }

        byte[] rawData = radarData.getRawData();
        float[] angleData = radarData.getAngleData();

        int zoom = (int) Math.pow(2, level);

        int resolution = fullResolution / zoom;
        int topY = resolution / 2;
        int leftX = -topY;

        int numBins = radarData.getNumBins();
        state.pixelsPerBin = (double) resolution / numBins / 2;

        state.xStart = leftX + (tileSize * tileX);
        state.yStart = topY - (tileSize * tileY);
        int yEnd = state.yStart - tileSize;
        int xEnd = state.xStart + tileSize;

        double startAngle = 0.0;
        double endAngle = 0.0;
        double minDist = 0.0;
        double maxDist = 0.0;

        if (xEnd <= 0.0) {
            // tile is left of origin
            if (state.yStart <= 0) {
                // tile is below origin
                startAngle = Math.atan2(xEnd, yEnd);
                endAngle = Math.atan2(state.xStart, state.yStart);
                minDist = Math.sqrt(xEnd * xEnd + state.yStart * state.yStart);
                maxDist = Math.sqrt(state.xStart * state.xStart + yEnd * yEnd);

            } else if (yEnd >= 0.0) {
                // tile is above origin
                startAngle = Math.atan2(state.xStart, yEnd);
                endAngle = Math.atan2(xEnd, state.yStart);
                minDist = Math.sqrt(xEnd * xEnd + yEnd * yEnd);
                maxDist = Math.sqrt(state.xStart * state.xStart + state.yStart
                        * state.yStart);
            } else {
                // tile spans origin in y
                startAngle = Math.atan2(state.xStart, yEnd);
                endAngle = Math.atan2(xEnd, yEnd);
                minDist = Math.sqrt(xEnd * xEnd + yEnd * yEnd);
                maxDist = Math.sqrt(state.xStart * state.xStart + state.yStart
                        * state.yStart);
            }
        } else if (state.xStart >= 0.0) {
            // tile is right of origin
            if (state.yStart <= 0) {
                // tile is below origin
                startAngle = Math.atan2(xEnd, state.yStart);
                endAngle = Math.atan2(state.xStart, yEnd);
                minDist = Math.sqrt(state.xStart * state.xStart + state.yStart
                        * state.yStart);
                maxDist = Math.sqrt(xEnd * xEnd + yEnd * yEnd);

            } else if (yEnd >= 0.0) {
                // tile is above origin
                startAngle = Math.atan2(state.xStart, state.yStart);
                endAngle = Math.atan2(xEnd, yEnd);
                minDist = Math.sqrt(state.xStart * state.xStart + yEnd * yEnd);
                maxDist = Math.sqrt(xEnd * xEnd + state.yStart * state.yStart);
            } else {
                // tile spans origin in y
                startAngle = Math.atan2(state.xStart, state.yStart);
                endAngle = Math.atan2(state.xStart, yEnd);
                minDist = Math.sqrt(state.xStart * state.xStart + yEnd * yEnd);
                maxDist = Math.sqrt(xEnd * xEnd + state.yStart * state.yStart);
            }
        } else {
            // tile spans origin in x
            if (state.yStart <= 0) {
                // tile is below origin
                startAngle = Math.atan2(xEnd, state.yStart);
                endAngle = Math.atan2(state.xStart, state.yStart);
                minDist = Math.sqrt(state.xStart * state.xStart + state.yStart
                        * state.yStart);
                maxDist = Math.sqrt(xEnd * xEnd + yEnd * yEnd);
            } else if (yEnd >= 0.0) {
                // tile is above origin
                startAngle = Math.atan2(state.xStart, yEnd);
                endAngle = Math.atan2(state.xStart, state.yStart);
                minDist = Math.sqrt(state.xStart * state.xStart + yEnd * yEnd);
                maxDist = Math.sqrt(xEnd * xEnd + state.yStart * state.yStart);
            } else {
                // tile spans origin in y
                startAngle = 0.0;
                endAngle = TwoPi;
                minDist = 0.0;
                maxDist = Math.sqrt(state.xStart * state.xStart + yEnd * yEnd);
            }
        }

        if (startAngle < 0) {
            startAngle += TwoPi;
        }
        if (endAngle < 0) {
            endAngle += TwoPi;
        }

        startAngle = Math.toDegrees(startAngle);
        endAngle = Math.toDegrees(endAngle);

        if (endAngle < startAngle) {
            endAngle += 360.0;
        }

        int firstBin = (int) Math.floor(minDist / state.pixelsPerBin);
        int lastBin = (int) Math.ceil(maxDist / state.pixelsPerBin);

        if (firstBin > numBins) {
            firstBin = numBins;
        }
        if (lastBin > numBins) {
            lastBin = numBins;
        }

        if (lastBin > firstBin) {
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
        }
        return state.bImage;
    }

    private byte getDataValue(byte[] rawData, int radial, int currBin) {
        return rawData[radial * radarData.getNumBins() + currBin];
    }

    /**
     * @param b
     * @return
     */
    private byte convertToImage(byte b) {
        double image = dataToImage.convert((b) & 0xFF);
        if (Double.isNaN(image)) {
            return b;
        } else {
            return (byte) Math.round(image);
        }
    }

    /**
     * Creates a full image of the radar data. The class needs to be constructed
     * with the fullImage flag set to true for this to work.
     * 
     * @return A byte array of the full image
     */
    public byte[] createFullImage() {
        return createTile(0, 0, 0, false).array();
    }

    public GridGeometry2D constructGridGeometry() {
        maxExtent = RadarUtil.calculateExtent(radarData);
        return RadarUtil.constructGridGeometry(radarData.getCRS(), maxExtent,
                fullResolution);
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
        return tileSize;
    }

    /**
     * Returns the actual height of the imagery. This is typically used for the
     * full image.
     * 
     * @return The current height
     */
    public int getHeight() {
        return tileSize;
    }

    private void createImage(TileState state, boolean direct) {
        state.bImage = BufferUtil.createByteBuffer(actualArrayLength, direct);
        state.bImage.order(ByteOrder.nativeOrder());
    }

    private void calculatePixels(TileState state, int startBin, int endBin,
            byte value) {
        if (value == 0) {
            return;
        }
        value = convertToImage(value);

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

    private void setPixel(TileState state, int x, int y, byte value) {
        if ((x >= 0) && (y >= 0) && (x < tileSize) && (y < tileSize)) {
            state.bImage.put(y * tileSize + x, value);
        }
    }

    public int getLevels() {
        return levels;
    }

    public int getTileSize() {
        return tileSize;
    }

    public void setSRMCreate(boolean srmCreate) {
        this.srmCreate = srmCreate;
    }
}
