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
package com.raytheon.viz.core.gl.ext.imaging;

import java.nio.FloatBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.viz.core.gl.dataformat.GLBufferColorMapData;
import com.raytheon.viz.core.gl.dataformat.GLFloatDataFormat;
import com.raytheon.viz.core.gl.images.GLBufferCMTextureData;
import com.raytheon.viz.core.gl.images.GLCMTextureData;

/**
 * Factory class for creation {@link GLDataMapping} objects that can convert
 * between units
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2013 2492       mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLDataMappingFactory {

    /**
     * Key for {@link GLDataMappingFactory#mappingCache}. Stores fields needed
     * to generate unique mappings
     * 
     * @author mschenke
     */
    private static class GLDataMappingKey {

        private final Unit<?> dataUnit;

        private final Unit<?> colorMapUnit;

        private final float colorMapMin;

        private final float colorMapMax;

        private final int colorMapSize;

        public GLDataMappingKey(Unit<?> dataUnit, Unit<?> colorMapUnit,
                float colorMapMin, float colorMapMax, int colorMapSize) {
            this.dataUnit = dataUnit;
            this.colorMapUnit = colorMapUnit;
            this.colorMapMin = colorMapMin;
            this.colorMapMax = colorMapMax;
            this.colorMapSize = colorMapSize;
        }

        public Unit<?> getDataUnit() {
            return dataUnit;
        }

        public Unit<?> getColorMapUnit() {
            return colorMapUnit;
        }

        public float getColorMapMin() {
            return colorMapMin;
        }

        public float getColorMapMax() {
            return colorMapMax;
        }

        public int getColorMapSize() {
            return colorMapSize;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + Float.floatToIntBits(colorMapMax);
            result = prime * result + Float.floatToIntBits(colorMapMin);
            result = prime * result + colorMapSize;
            result = prime * result
                    + ((colorMapUnit == null) ? 0 : colorMapUnit.hashCode());
            result = prime * result
                    + ((dataUnit == null) ? 0 : dataUnit.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            GLDataMappingKey other = (GLDataMappingKey) obj;
            if (Float.floatToIntBits(colorMapMax) != Float
                    .floatToIntBits(other.colorMapMax))
                return false;
            if (Float.floatToIntBits(colorMapMin) != Float
                    .floatToIntBits(other.colorMapMin))
                return false;
            if (colorMapSize != other.colorMapSize)
                return false;
            if (colorMapUnit == null) {
                if (other.colorMapUnit != null)
                    return false;
            } else if (!colorMapUnit.equals(other.colorMapUnit))
                return false;
            if (dataUnit == null) {
                if (other.dataUnit != null)
                    return false;
            } else if (!dataUnit.equals(other.dataUnit))
                return false;
            return true;
        }

    }

    /**
     * GL data mapping object, represents a mapping between units for use in GL
     * 
     * @author mschenke
     */
    public static class GLDataMapping {

        private final GLDataMappingKey key;

        private GLCMTextureData colorMapping;

        private GLCMTextureData dataMapping;

        private int numMappingValues;

        private int refCount = 1;

        private boolean initialized = false;

        public GLDataMapping(GLDataMappingKey key) {
            this.key = key;
        }

        public GLCMTextureData getColorMapping() {
            return colorMapping;
        }

        public GLCMTextureData getDataMapping() {
            return dataMapping;
        }

        public int getNumMappingValues() {
            return numMappingValues;
        }

        public boolean isValid() {
            return numMappingValues > 0 && colorMapping != null
                    && dataMapping != null;
        }

        public void dispose() {
            synchronized (mappingCache) {
                refCount -= 1;
                if (refCount <= 0) {
                    mappingCache.remove(key);
                    if (colorMapping != null) {
                        colorMapping.dispose();
                    }
                    if (dataMapping != null) {
                        dataMapping.dispose();
                    }
                    refCount = 0;
                }
            }
        }

        private void use() {
            if (refCount == 0) {
                throw new IllegalStateException(
                        "GLDataMapping has already been disposed");
            }
            refCount += 1;
        }

        private synchronized void initialize(GL gl) {
            if (initialized) {
                return;
            }
            Unit<?> dataUnit = key.getDataUnit();
            Unit<?> colorMapUnit = key.getColorMapUnit();
            int colorMapSize = key.getColorMapSize();
            double colorMapMin = key.getColorMapMin();
            double colorMapMax = key.getColorMapMax();
            int numMappings = 0;
            if (dataUnit != null && colorMapUnit != null
                    && dataUnit.equals(colorMapUnit) == false
                    && dataUnit.isCompatible(colorMapUnit)) {
                // Worst case scenario, one mapping per color
                double[] colorMapping = new double[colorMapSize];
                Arrays.fill(colorMapping, Float.NaN);
                double[] dataMapping = new double[colorMapping.length];
                Arrays.fill(dataMapping, Float.NaN);

                UnitConverter colorMapToData = colorMapUnit
                        .getConverterTo(dataUnit);
                double dataMin = colorMapToData.convert(colorMapMin);
                double dataMax = colorMapToData.convert(colorMapMax);
                colorMapping[0] = colorMapMin;
                colorMapping[colorMapping.length - 1] = colorMapMax;
                dataMapping[0] = dataMin;
                dataMapping[dataMapping.length - 1] = dataMax;

                numMappings = 2;
                if (colorMapToData.isLinear() == false) {
                    // Populate the dataMapping/colorMapping arrays
                    double increment = (colorMapMax - colorMapMin)
                            / (colorMapping.length - 1);
                    for (int i = 1; i < colorMapping.length - 1; ++i) {
                        colorMapping[i] = colorMapMin + (i * increment);
                        dataMapping[i] = colorMapToData
                                .convert(colorMapping[i]);
                    }

                    // Search for linearness in the dataMappings.
                    int currEndIndex = 1;
                    double currEndValue = dataMapping[currEndIndex];
                    float currDelta = (float) (currEndValue - dataMapping[0]);
                    for (int i = 2; i < dataMapping.length; ++i) {
                        double nextValue = dataMapping[i];
                        // Deltas are compared in float space because it
                        // minimizes the precision errors and the mapping will
                        // occur in floats in GLSL so no need for the extra
                        // precision
                        float nextDelta = (float) ((nextValue - currEndValue) / (i - currEndIndex));
                        if (nextDelta == currDelta) {
                            // Remove linear entries
                            dataMapping[currEndIndex] = colorMapping[currEndIndex] = Double.NaN;
                            currEndValue = nextValue;
                            currEndIndex = i;
                        } else {
                            // Non-linear entry found, add mapping
                            numMappings += 1;
                            currEndIndex = i;
                            currEndValue = nextValue;
                            currDelta = nextDelta;
                        }
                    }
                }

                // Condense the mapping arrays removing nans
                float[] condensedColorMapping = new float[numMappings];
                float[] condensedDataMapping = new float[numMappings];

                int index = 0;
                for (int i = 0; i < colorMapSize && index < numMappings; ++i) {
                    double colorMapVal = colorMapping[i];
                    double dataMapVal = dataMapping[i];
                    if (Double.isNaN(colorMapVal) == false
                            && Double.isNaN(dataMapVal) == false) {
                        condensedColorMapping[index] = (float) colorMapVal;
                        condensedDataMapping[index] = (float) dataMapVal;
                        index += 1;
                    }
                }

                if (index == numMappings) {
                    this.numMappingValues = numMappings;
                    this.colorMapping = new GLBufferCMTextureData(
                            new GLBufferColorMapData(new ColorMapData(
                                    FloatBuffer.wrap(condensedColorMapping),
                                    new int[] { numMappings }),
                                    new GLFloatDataFormat()));
                    this.dataMapping = new GLBufferCMTextureData(
                            new GLBufferColorMapData(new ColorMapData(
                                    FloatBuffer.wrap(condensedDataMapping),
                                    new int[] { numMappings }),
                                    new GLFloatDataFormat()));
                }
            }
            initialized = true;
        }
    }

    private static Map<GLDataMappingKey, GLDataMapping> mappingCache = new HashMap<GLDataMappingKey, GLDataMapping>();

    /**
     * Creates a {@link GLDataMapping} object given the dataUnit, colorMapUnit,
     * color map min/max, and size of the colormap. Object must be disposed of
     * when no longer used
     * 
     * @param gl
     * @param dataUnit
     * @param colorMapUnit
     * @param colorMapMin
     * @param colorMapMax
     * @param colorMapSize
     * @return
     */
    public static GLDataMapping constructGLDataMapping(GL gl, Unit<?> dataUnit,
            Unit<?> colorMapUnit, float colorMapMin, float colorMapMax,
            int colorMapSize) {
        GLDataMapping mapping;
        synchronized (mappingCache) {
            GLDataMappingKey key = new GLDataMappingKey(dataUnit, colorMapUnit,
                    colorMapMin, colorMapMax, colorMapSize);
            mapping = mappingCache.get(key);
            if (mapping == null) {
                mapping = new GLDataMapping(key);
                mappingCache.put(key, mapping);
            } else {
                mapping.use();
            }
        }
        mapping.initialize(gl);
        return mapping;
    }

}
