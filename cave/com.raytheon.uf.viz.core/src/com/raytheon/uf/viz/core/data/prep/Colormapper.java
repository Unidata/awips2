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
package com.raytheon.uf.viz.core.data.prep;

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.WritableRaster;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapDataType;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;

/**
 * Colormapper class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class Colormapper {

    /**
     * This method will color map a Buffer to a RenderedImage given size and
     * parameters
     * 
     * @param buf
     * @param datasetBounds
     * @param parameters
     * @return
     */
    public static RenderedImage colorMap(ColorMapData cmapData,
            ColorMapParameters parameters) {
        int width = cmapData.getDimensions()[0];
        int height = cmapData.getDimensions()[1];
        Buffer buf = cmapData.getBuffer();
        ColorMapDataType dataType = cmapData.getDataType();

        // Parameters ported from raster.glsl
        boolean log = parameters.isLogarithmic();
        double logFactor = parameters.getLogFactor();
        boolean mirror = parameters.isMirror();
        double naturalMin = parameters.getDataMin();
        double naturalMax = parameters.getDataMax();
        double cmapMin = parameters.getColorMapMin();
        double cmapMax = parameters.getColorMapMax();
        int colorMapSz = parameters.getColorMap().getSize();

        double diff = Math.abs(cmapMax - cmapMin);

        int dataSize = buf.capacity();
        byte[] cmapedData = new byte[buf.capacity()];
        for (int i = 0; i < dataSize; ++i) {
            double value = getValue(buf, i, dataType);
            double index = 0.0f;
            if (log) {
                index = findIndexLog(value, logFactor, cmapMin, cmapMax, mirror);
            } else {
                index = ((value - cmapMin) / diff);
            }
            if (index < 0.0) {
                index = 0.0;
            } else if (index > 1.0) {
                index = 1.0;
            }
            cmapedData[i] = findColorIndex(index, logFactor, colorMapSz);
        }

        IndexColorModel cm = ColorMapManager.buildColorModel(parameters
                .getColorMap());

        DataBufferByte byteArray = new DataBufferByte(cmapedData, width
                * height);

        MultiPixelPackedSampleModel sample = new MultiPixelPackedSampleModel(
                DataBuffer.TYPE_BYTE, width, height,
                ColorMapManager.NUMBER_BITS);
        WritableRaster writeRaster = Raster.createWritableRaster(sample,
                byteArray, new Point(0, 0));

        BufferedImage bi = new BufferedImage(width, height,
                BufferedImage.TYPE_BYTE_INDEXED, cm);
        bi.setData(writeRaster);
        return bi;
    }

    private static double getValue(Buffer buffer, int idx,
            ColorMapDataType dataType) {
        switch (dataType) {
        case BYTE: {
            return ((ByteBuffer) buffer).get(idx) & 0xFF;
        }
        case SIGNED_BYTE: {
            return ((ByteBuffer) buffer).get(idx);
        }
        case SHORT: {
            return ((ShortBuffer) buffer).get(idx);
        }
        case UNSIGNED_SHORT: {
            return ((ShortBuffer) buffer).get(idx) & 0xFFFF;
        }
        case INT: {
            return ((IntBuffer) buffer).get(idx);
        }
        case FLOAT: {
            return ((FloatBuffer) buffer).get(idx);
        }
        }
        return 0.0;
    }

    private static double findIndexLog(double value, double logFactor,
            double cmapMin, double cmapMax, boolean mirror) {
        double index = 0.0;
        // is this strictly negative, strictly positive or neg to pos scaling?
        if (cmapMin >= 0.0 && cmapMax >= 0.0 && mirror == false) {
            if (value < cmapMin) {
                index = 0.0;
            } else {
                // simple calculation
                index = ((Math.log(value) - Math.log(cmapMin)) / Math.abs(Math
                        .log(cmapMax) - Math.log(cmapMin)));
            }
        } else if (cmapMin <= 0.0 && cmapMax <= 0.0 && mirror == false) {
            index = ((Math.log(value) - Math.log(cmapMax)) / Math.abs(Math
                    .log(cmapMin) - Math.log(cmapMax)));
        } else {
            // special case, neg to pos:
            double colorMapMin = cmapMin;
            double colorMapMax = cmapMax;
            double zeroVal = Math.max(colorMapMax, Math.abs(colorMapMin)) * 0.0001;
            if (mirror && (colorMapMin > 0.0 || colorMapMax < 0.0)) {
                if (colorMapMax < 0.0) {
                    colorMapMax = -cmapMax;
                    value = -value;
                    zeroVal = -colorMapMin;
                } else {
                    zeroVal = cmapMin;
                }
                colorMapMin = -cmapMax;
            }
            double leftZero = 0.0;
            double rightZero = 0.0;
            double absLogZeroVal = Math.abs(Math.log(zeroVal));

            rightZero = absLogZeroVal + Math.log(colorMapMax);

            double cmapMax2 = Math.abs(colorMapMin);

            leftZero = absLogZeroVal + Math.log(cmapMax2);

            double zeroIndex = leftZero / (leftZero + rightZero);

            // figure out index for texture val
            double absTextureColor = Math.abs(value);
            if (absTextureColor <= zeroVal) {
                index = zeroIndex;
            } else if (value > 0.0) {
                // positive texture color value, find index from 0 to
                // cmapMax:
                double logTexColor = absLogZeroVal + Math.log(value);

                double texIndex = logTexColor / rightZero;
                index = (zeroIndex + ((1.0 - zeroIndex) * texIndex));
            } else {
                // negative texture color value, find index from 0 to
                // cmapMax:
                double logTexColor = absLogZeroVal + Math.log(absTextureColor);

                double texIndex = logTexColor / leftZero;
                index = (zeroIndex - (zeroIndex * texIndex));
            }
        }
        return index;
    }

    private static byte findColorIndex(double index, double logFactor,
            int colorMapSz) {
        if (logFactor > 0.0) {
            double minLog = Math.log(logFactor);
            double maxLog = Math.log(logFactor + 1.0);

            double lg = Math.log(logFactor + index);

            index = (lg - minLog) / (maxLog - minLog);
            if (index < 0.0) {
                index = 0.0;
            } else if (index > 1.0) {
                index = 1.0;
            }
        }
        return (byte) (index * (colorMapSz - 1));
    }
}
