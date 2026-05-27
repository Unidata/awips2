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
package com.raytheon.uf.viz.drawables.image.stipple;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.nio.Buffer;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.image.Colormapper;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.units.UnitConv;

/**
 * 
 * Provides utility methods for applying a stipple to an existing image for use
 * by implementations of {@link IStippledColormappedImageExtension}
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 02, 2016  5957     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class Stippler {
    
    /**
     * Applies a stipple to an existing image. The image is modified in place.
     * This is generally used on an image generated from the {@link Colormapper}
     * .
     * 
     * @param image
     *            An existing image which will have the patterns applied
     * @param parameters
     *            Used for mapping a range of data values to each stipple
     *            pattern provided
     * @param cmapData
     *            the raw data values, this must be the same size as the
     *            bufferedImage.
     * @param fillPatterns
     *            the patterns to apply to the image.
     */
    public static RenderedImage stipple(RenderedImage image,
            ColorMapParameters parameters, ColorMapData cmapData,
            List<byte[]> fillPatterns) {
        BufferedImage bufferedImage = null;
        if (!(image instanceof BufferedImage)) {
            bufferedImage = new BufferedImage(image.getWidth(),
                    image.getHeight(), BufferedImage.TYPE_INT_ARGB);
            bufferedImage.setData(image.getData());
        } else {
            bufferedImage = (BufferedImage) image;
        }
        int width = cmapData.getDimensions()[0];
        Buffer buf = cmapData.getBuffer();
        int dataSize = buf.capacity();
        ColorMapDataType dataType = cmapData.getDataType();

        double noDataValue = parameters.getNoDataValue();
        Unit<?> dataUnit = cmapData.getDataUnit();
        Unit<?> colorMapUnit = parameters.getColorMapUnit();
        UnitConverter converter = null;
        if (dataUnit != null && colorMapUnit != null
                && parameters.getDataMapping() == null
                && dataUnit.equals(colorMapUnit) == false
                && dataUnit.isCompatible(colorMapUnit) == true) {
            converter = UnitConv.getConverterToUnchecked(dataUnit, colorMapUnit);
            
        }

        int numPatterns = fillPatterns.size();
        List<BitSet> bitFillPatterns = new ArrayList<>(fillPatterns.size());
        for (byte[] pattern : fillPatterns) {
            if (pattern == null) {
                BitSet bitSet = new BitSet(32 * 32);
                bitSet.set(0, 32 * 32, true);
                bitFillPatterns.add(bitSet);
            } else {
                bitFillPatterns.add(BitSet.valueOf(pattern));
            }
        }

        for (int i = 0; i < dataSize; ++i) {
            double dataValue = Colormapper.getDataValue(buf, i, dataType);
            if ((!Double.isNaN(dataValue)) && (dataValue != noDataValue)) {
                double cmapValue = dataValue;
                if (converter != null) {
                    cmapValue = converter.convert(dataValue);
                }

                double index = Colormapper.getColorMappingIndex(cmapValue,
                        parameters);
                index = Colormapper.capIndex(index);
                int patternIndex = (int) Math.min(index * numPatterns,
                        numPatterns - 1);
                BitSet pattern = bitFillPatterns.get(patternIndex);
                if (pattern != null) {
                    int x = i % width;
                    int y = i / width;
                    int xp = x % 32;
                    int yp = y % 32;
                    if (!pattern.get(yp * 32 + xp)) {
                        bufferedImage.setRGB(x, y, 0);
                    }
                }
            }
        }
        return bufferedImage;
    }
}
