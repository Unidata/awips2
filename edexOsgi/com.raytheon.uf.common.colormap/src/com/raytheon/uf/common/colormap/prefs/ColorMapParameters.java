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
package com.raytheon.uf.common.colormap.prefs;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.colormap.AbstractColorMap;
import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.image.Colormapper;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;

/**
 * Colormap Parameters
 * 
 * Contains the parameters necessary to apply a colormap to a datatype
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 24, 2007             chammack    Initial Creation.
 *    Feb 14, 2013 1616        bsteffen    Add option for interpolation of
 *                                         colormap parameters, disable colormap
 *                                         interpolation by default.
 *    Jun 14, 2013 DR 16070    jgerth      Utilize data mapping
 *    Aug  2, 2013 2211        mschenke    Backed out 16070 changes, made 
 *                                         dataUnit/imageUnit properly commutative.
 *    Nov  4, 2013 2492        mschenke    Cleaned up variable naming to make purpose
 *                                         clear (image->colormap)
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ColorMapParameters {

    @XmlAccessorType(XmlAccessType.NONE)
    public static class PersistedParameters {

        /** XML Persisted min value */
        @XmlElement
        protected Float colorMapMin;

        /** XML Persisted max value */
        @XmlElement
        protected Float colorMapMax;

        /**
         * @return the colorMapMin
         */
        public Float getColorMapMin() {
            return colorMapMin;
        }

        /**
         * @param colorMapMin
         *            the colorMapMin to set
         */
        public void setColorMapMin(Float colorMapMin) {
            this.colorMapMin = colorMapMin;
        }

        /**
         * @return the colorMapMax
         */
        public Float getColorMapMax() {
            return colorMapMax;
        }

        /**
         * @param colorMapMax
         *            the colorMapMax to set
         */
        public void setColorMapMax(Float colorMapMax) {
            this.colorMapMax = colorMapMax;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#clone()
         */
        @Override
        protected PersistedParameters clone() {
            PersistedParameters params = new PersistedParameters();
            params.colorMapMax = colorMapMax;
            params.colorMapMin = colorMapMin;
            return params;
        }

    }

    protected Set<IColorMapParametersListener> listeners = new HashSet<IColorMapParametersListener>();

    /** Units colormapping should be displayed in */
    protected Unit<?> displayUnit;

    /** Units colormapping will occur in */
    protected Unit<?> colorMapUnit;

    /** Units of the data values to colormap */
    @Deprecated
    protected Unit<?> dataUnit;

    /** The maximum value used to apply the colormap */
    protected float colorMapMax;

    /** The minimum value used to apply the colormap */
    protected float colorMapMin;

    /** The maximum (usually theoretical) value of the data */
    @Deprecated
    protected float dataMax;

    /** The minimum (usually theoretical) value of the data */
    @Deprecated
    protected float dataMin;

    /** The intervals upon which to apply labeling to the color bar */
    protected float[] colorBarIntervals;

    protected boolean logarithmic = false;

    protected ArrayList<LabelEntry> labels = new ArrayList<LabelEntry>();

    /** The colormap (ramp of colors) to use */
    protected IColorMap colorMap;

    /** The name of the colormap */
    @XmlAttribute
    protected String colorMapName;

    /** The converter that converts data values to {@link #displayUnit} * */
    @Deprecated
    protected UnitConverter dataToDisplayConverter;

    /** The converter that converts display values to {@link #dataUnit} * */
    @Deprecated
    protected UnitConverter displayToDataConverter;

    /** The converter that converts data values to {@link #colorMapUnit} */
    @Deprecated
    protected UnitConverter dataToColorMapConverter;

    /** The converter that converts color map unit values to {@link #dataUnit} */
    @Deprecated
    protected UnitConverter colorMapToDataConverter;

    /** The converter that converts color map unit values to {@link #displayUnit} */
    protected UnitConverter colorMapToDisplayConverter;

    /** The converter that converts display values to {@link #colorMapUnit} */
    protected UnitConverter displayToColorMapConverter;

    protected DataMappingPreferences dataMapping;

    protected boolean recomputeLabels = true;

    private String formatString = "0.###";

    private boolean dirty = false;

    private byte[] alphaMask = new byte[0];

    private boolean useMask = false;

    private boolean mirror;

    private double noDataValue = Double.NaN;

    @XmlElement
    private PersistedParameters persisted = new PersistedParameters();

    /** Values >0 enable log scaling of the colormap. */
    private float logFactor = -1.0f;

    /** Specify whether the colormap should be interpolated */
    protected boolean interpolate = false;

    public static class LabelEntry implements Comparable<LabelEntry> {
        private float location;

        private String text;

        public float getLocation() {
            return location;
        }

        public String getText() {
            return text;
        }

        /**
         * Sets a string label and it's location within the color bar.
         * 
         * @param label
         *            String to be displayed
         * @param location
         *            Location on the color bar for the label in the range 0 to
         *            1 where 0 is the far left of the color bar and 1 is the
         *            far right
         */
        public LabelEntry(String label, float location) {
            this.text = label;
            this.location = location;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + Float.floatToIntBits(location);
            result = prime * result + ((text == null) ? 0 : text.hashCode());
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
            LabelEntry other = (LabelEntry) obj;
            if (Float.floatToIntBits(location) != Float
                    .floatToIntBits(other.location))
                return false;
            if (text == null) {
                if (other.text != null)
                    return false;
            } else if (!text.equals(other.text))
                return false;
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(LabelEntry o) {
            return Double.compare(getLocation(), o.getLocation());
        }

    }

    /**
     * Creates a {@link UnitConverter} converting the from unit to the to unit.
     * 
     * @param from
     * @param to
     * @return The unit converter or null of units are not compatible
     */
    private UnitConverter constructConverter(Unit<?> from, Unit<?> to) {
        UnitConverter converter = null;

        if ((from != null) && (to != null) && (to.isCompatible(from))) {
            converter = from.getConverterTo(to);
        }

        return converter;
    }

    /**
     * Adds a label for a {@link #displayUnit} value
     * 
     * @param colorMapValue
     * @param s
     */
    private void addDisplayValueLabel(float dispValue, String s) {
        float colorMapValue = dispValue;
        UnitConverter displayToColorMap = getDisplayToColorMapConverter();
        if (displayToColorMap != null) {
            colorMapValue = (float) displayToColorMap.convert(dispValue);
        }
        addColorMapValueLabel(colorMapValue, s);
    }

    /**
     * Adds a label for a {@link #colorMapUnit} value
     * 
     * @param colorMapValue
     * @param s
     */
    private void addColorMapValueLabel(float colorMapValue, String s) {
        double index = Colormapper.getColorMappingIndex(colorMapValue, this);
        if (index > 1.0 || index < 0.0) {
            return;
        }

        labels.add(new LabelEntry(s, (float) index));
    }

    private void computeLabels() {

        labels.clear();

        DecimalFormat format = new DecimalFormat(formatString);
        if (colorBarIntervals != null) {
            float colorMapRange = colorMapMax - colorMapMin;
            if (colorMapRange != 0.0) {
                for (float label : colorBarIntervals) {
                    String s = format.format(label);
                    addDisplayValueLabel(label, s);
                }
            }
        } else if (dataMapping != null) {
            for (DataMappingEntry entry : dataMapping.getEntries()) {
                String s = entry.getLabel();
                if (entry.getDisplayValue() != null) {
                    float dispValue = entry.getDisplayValue().floatValue();
                    if (s == null) {
                        s = format.format(dispValue);
                    }
                    addDisplayValueLabel(dispValue, s);
                } else if (entry.getPixelValue() != null
                        && !"NO DATA".equals(s)) {
                    float pixelValue = entry.getPixelValue().floatValue();
                    addColorMapValueLabel(pixelValue, s);
                }
            }
        }
        Collections.sort(labels);
        recomputeLabels = false;
    }

    @XmlElement
    public AbstractColorMap getJaxbColorMap() {
        if ((colorMapName == null || dirty)
                && colorMap instanceof AbstractColorMap) {
            return (AbstractColorMap) colorMap;
        } else {
            return null;
        }
    }

    public void setJaxbColorMap(AbstractColorMap colorMap) {
        this.colorMap = colorMap;
    }

    /**
     * @return the colorMap
     */
    public IColorMap getColorMap() {
        return colorMap;
    }

    /**
     * @param colorMap
     *            the colorMap to set
     */
    public void setColorMap(IColorMap colorMap) {
        this.colorMap = colorMap;
        if (colorMap != null) {
            this.colorMapName = colorMap.getName();
            if (colorMap.getSize() != alphaMask.length) {
                alphaMask = Arrays.copyOf(alphaMask, colorMap.getSize());
            }
        }
        notifyListener();
    }

    /**
     * Returns the display unit
     * 
     * @return The unit colormapping should be displayed in
     */
    public Unit<?> getDisplayUnit() {
        return displayUnit;
    }

    /**
     * Sets the display units
     * 
     * @param unit
     *            The unit colormapping should be displayed in
     */
    public void setDisplayUnit(Unit<?> unit) {
        this.displayUnit = unit;
        recomputeLabels = true;

        displayToColorMapConverter = null;
        colorMapToDisplayConverter = null;

        displayToDataConverter = null;
        dataToDisplayConverter = null;
        notifyListener();
    }

    /**
     * Returns the maximum range value the colormapping occurs over
     * 
     * @return the maximum range value in {@link #colorMapUnit}
     */
    public float getColorMapMax() {
        return colorMapMax;
    }

    /**
     * Sets the maximum range value the colormapping occurs over
     * 
     * @param colorMapMax
     *            the maximum range value in {@link #colorMapUnit}
     */
    public void setColorMapMax(float colorMapMax) {
        setColorMapMax(colorMapMax, false);
    }

    /**
     * Sets the maximum range value the colormapping occurs over
     * 
     * @param colorMapMax
     *            the maximum range value in {@link #colorMapUnit}
     * @param persist
     *            true indicates colorMapMax should be persisted through
     *            serialization
     */
    public void setColorMapMax(float colorMapMax, boolean persist) {
        this.colorMapMax = colorMapMax;
        recomputeLabels = true;
        notifyListener();
        if (persist) {
            persisted.colorMapMax = colorMapMax;
        }
    }

    /**
     * Returns the minimum range value the colormapping occurs over
     * 
     * @return the minimum range value in {@link #colorMapUnit}
     */
    public float getColorMapMin() {
        return colorMapMin;
    }

    /**
     * Sets the minimum range value the colormapping occurs over
     * 
     * @param colorMapMin
     *            the minimum range value in {@link #colorMapUnit}
     */
    public void setColorMapMin(float colorMapMin) {
        setColorMapMin(colorMapMin, false);
    }

    /**
     * Sets the minimum range value the colormapping occurs over
     * 
     * @param colorMapMin
     *            the minimum range value in {@link #colorMapUnit}
     * @param persist
     *            true indicates colorMapMin should be persisted through
     *            serialization
     */
    public void setColorMapMin(float colorMapMin, boolean persist) {
        this.colorMapMin = colorMapMin;
        recomputeLabels = true;
        notifyListener();
        if (persist) {
            persisted.colorMapMin = colorMapMin;
        }
    }

    /**
     * @deprecated data max is not important for general colormapping use
     * @return the dataMax
     */
    @Deprecated
    public float getDataMax() {
        return dataMax;
    }

    /**
     * @deprecated data max is not important for general colormapping use
     * @param dataMax
     *            the dataMax to set
     */
    @Deprecated
    public void setDataMax(float dataMax) {
        this.dataMax = dataMax;
        notifyListener();
    }

    /**
     * @deprecated data min is not important for general colormapping use
     * @return the dataMin
     */
    @Deprecated
    public float getDataMin() {
        return dataMin;
    }

    /**
     * @deprecated data min is not important for general colormapping use
     * @param dataMin
     *            the dataMin to set
     */
    @Deprecated
    public void setDataMin(float dataMin) {
        this.dataMin = dataMin;
        notifyListener();
    }

    /**
     * @return the colorBarIntervals
     */
    public float[] getColorBarIntervals() {
        return colorBarIntervals;
    }

    /**
     * @param colorBarIntervals
     *            the colorBarIntervals to set
     */
    public void setColorBarIntervals(float[] colorBarIntervals) {
        this.colorBarIntervals = colorBarIntervals;
        recomputeLabels = true;
        notifyListener();
    }

    /**
     * @return the colorMapName
     */
    public String getColorMapName() {
        return colorMapName;
    }

    /**
     * @param colorMapName
     *            the colorMapName to set
     */
    public void setColorMapName(String colorMapName) {
        // assume we are not dirty if we set a name
        this.colorMapName = colorMapName;
        if (this.colorMap != null) {
            this.colorMap.setName(colorMapName);
        }
        notifyListener();
        dirty = false;
    }

    /**
     * Returns the unit data values to be colormapped are in
     * 
     * @deprecated data unit is not important for general colormapping use
     * @return the dataUnit
     */
    @Deprecated
    public Unit<?> getDataUnit() {
        return dataUnit;
    }

    /**
     * Sets the unit data values to be colormapped are in
     * 
     * @deprecated data unit is not important for general colormapping use
     * @param dataUnit
     *            the dataUnit to set
     */
    @Deprecated
    public void setDataUnit(Unit<?> dataUnit) {
        this.dataUnit = dataUnit;

        if (dataUnit != null && colorMapUnit == null) {
            setColorMapUnit(dataUnit);
        }

        dataToColorMapConverter = null;
        colorMapToDataConverter = null;

        dataToDisplayConverter = null;
        displayToDataConverter = null;

        notifyListener();
    }

    /**
     * Returns the {@link UnitConverter} from {@link #dataUnit} to
     * {@link #displayUnit}
     * 
     * @deprecated data unit is not important for general colormapping use
     * @return the dataToDisplayConverter
     */
    @Deprecated
    public UnitConverter getDataToDisplayConverter() {
        if (dataToDisplayConverter == null) {
            dataToDisplayConverter = constructConverter(dataUnit, displayUnit);
            if (dataToDisplayConverter != null) {
                notifyListener();
            }
        }
        return dataToDisplayConverter;
    }

    /**
     * Returns the {@link UnitConverter} from {@link #displayUnit} to
     * {@link #dataUnit}
     * 
     * @deprecated data unit is not important for general colormapping use
     * @return the displayToDataConverter
     */
    @Deprecated
    public UnitConverter getDisplayToDataConverter() {
        if (displayToDataConverter == null) {
            displayToDataConverter = constructConverter(displayUnit, dataUnit);
            if (displayToDataConverter != null) {
                notifyListener();
            }
        }
        return displayToDataConverter;
    }

    public List<LabelEntry> getLabels() {
        if (recomputeLabels) {
            computeLabels();
            notifyListener();
        }
        return labels;
    }

    /**
     * Returns true if the colormapping is logarithmically scaled from
     * {@link #colorMapMin} to {@link #colorMapMax}
     * 
     * @return
     */
    public boolean isLogarithmic() {
        return logarithmic;
    }

    /**
     * Set to true if the colormapping should logarithmically scaled from
     * {@link #colorMapMin} to {@link #colorMapMax}
     * 
     * @param logarithmic
     */
    public void setLogarithmic(boolean logarithmic) {
        recomputeLabels = recomputeLabels | this.logarithmic != logarithmic;
        this.logarithmic = logarithmic;
        notifyListener();
    }

    /**
     * @deprecated Use {@link #getDataToColorMapConverter()} instead
     * 
     * @return the dataToColorMapConverter
     */
    @Deprecated
    public UnitConverter getDataToImageConverter() {
        return getDataToColorMapConverter();
    }

    /**
     * Returns a {@link UnitConverter} converting {@link #dataUnit} values to
     * the {@link #colorMapUnit} if compatible or null otherwise
     * 
     * @deprecated data unit is not important for general colormapping use
     * @return
     */
    @Deprecated
    public UnitConverter getDataToColorMapConverter() {
        if (dataToColorMapConverter == null) {
            dataToColorMapConverter = constructConverter(dataUnit, colorMapUnit);
            if (dataToColorMapConverter != null) {
                notifyListener();
            }
        }
        return dataToColorMapConverter;
    }

    /**
     * @deprecated Use {@link #getColorMapToDisplayConverter()} instead
     * 
     * @return the colorMapToDisplayConverter
     */
    @Deprecated
    public UnitConverter getImageToDisplayConverter() {
        return getColorMapToDisplayConverter();
    }

    /**
     * Returns a {@link UnitConverter} converting {@link #colorMapUnit} values
     * to the {@link #displayUnit} if compatible or null otherwise
     * 
     * @return
     */
    public UnitConverter getColorMapToDisplayConverter() {
        if (colorMapToDisplayConverter == null) {
            colorMapToDisplayConverter = constructConverter(colorMapUnit,
                    displayUnit);
            if (colorMapToDisplayConverter != null) {
                notifyListener();
            }
        }
        return colorMapToDisplayConverter;
    }

    /**
     * @deprecated Use {@link #getColorMapUnit()} instead
     * 
     * @return the colorMapUnit
     */
    @Deprecated
    public Unit<?> getImageUnit() {
        return getColorMapUnit();
    }

    /**
     * Returns the unit colormapping will occur in
     * 
     * @return
     */
    public Unit<?> getColorMapUnit() {
        return colorMapUnit;
    }

    /**
     * @deprecated Use {@link #setColorMapUnit(Unit)} instead
     * 
     * @param imageUnit
     *            the colorMapUnit to set
     */
    @Deprecated
    public void setImageUnit(Unit<?> imageUnit) {
        setColorMapUnit(imageUnit);
    }

    /**
     * Sets the unit colormapping will occur in. {@link #colorMapMin} and
     * {@link #colorMapMax} are expected to be in this unit
     * 
     * @param colorMapUnit
     */
    public void setColorMapUnit(Unit<?> colorMapUnit) {
        this.colorMapUnit = colorMapUnit;

        if (colorMapUnit != null && dataUnit == null) {
            setDataUnit(colorMapUnit);
        }

        recomputeLabels = true;

        colorMapToDataConverter = null;
        dataToColorMapConverter = null;

        colorMapToDisplayConverter = null;
        displayToColorMapConverter = null;
        notifyListener();
    }

    /**
     * @deprecated Use {@link #getColorMapToDataConverter()} instead
     * 
     * @return the colorMapToDataConverter
     */
    @Deprecated
    public UnitConverter getImageToDataConverter() {
        return getColorMapToDataConverter();
    }

    /**
     * Returns a {@link UnitConverter} converting {@link #colorMapUnit} values
     * to the {@link #dataUnit} if compatible or null otherwise
     * 
     * @deprecated data unit is not important for general colormapping use
     * @return
     */
    @Deprecated
    public UnitConverter getColorMapToDataConverter() {
        if (colorMapToDataConverter == null) {
            colorMapToDataConverter = constructConverter(colorMapUnit, dataUnit);
            if (colorMapToDataConverter != null) {
                notifyListener();
            }
        }
        return colorMapToDataConverter;
    }

    /**
     * @deprecated Use {@link #getDisplayToColorMapConverter()} instead
     * 
     * @return the displayToColorMapConverter
     */
    @Deprecated
    public UnitConverter getDisplayToImageConverter() {
        return getDisplayToColorMapConverter();
    }

    /**
     * Returns a {@link UnitConverter} converting {@link #displayUnit} values to
     * the {@link #colorMapUnit} if compatible or null otherwise
     * 
     * @return
     */
    public UnitConverter getDisplayToColorMapConverter() {
        if (displayToColorMapConverter == null) {
            displayToColorMapConverter = constructConverter(displayUnit,
                    colorMapUnit);
            if (displayToColorMapConverter != null) {
                notifyListener();
            }
        }
        return displayToColorMapConverter;
    }

    /**
     * @param dataMapping
     *            the dataMapping to set
     */
    public void setDataMapping(DataMappingPreferences dataMapping) {
        this.dataMapping = dataMapping;
        recomputeLabels = true;
        if (dataMapping != null && displayUnit != null) {
            setColorMapUnit(dataMapping.getImageUnit(displayUnit));
        }
        notifyListener();
    }

    /**
     * @return the dataMapping
     */
    public DataMappingPreferences getDataMapping() {
        return dataMapping;
    }

    /**
     * @return the formatString
     */
    public String getFormatString() {
        return formatString;
    }

    /**
     * @param formatString
     *            the formatString to set
     */
    public void setFormatString(String formatString) {
        this.formatString = formatString;
        notifyListener();
    }

    public ColorMapParameters clone() {
        ColorMapParameters cmp = new ColorMapParameters();
        cmp.colorBarIntervals = colorBarIntervals;
        cmp.colorMap = colorMap;
        cmp.colorMapMax = colorMapMax;
        cmp.colorMapMin = colorMapMin;
        cmp.colorMapName = colorMapName;
        cmp.dataMapping = dataMapping;
        cmp.dataMax = dataMax;
        cmp.dataMin = dataMin;
        cmp.dataToDisplayConverter = dataToDisplayConverter;
        cmp.dataToColorMapConverter = dataToColorMapConverter;
        cmp.dataUnit = dataUnit;
        cmp.formatString = formatString;
        cmp.colorMapToDataConverter = colorMapToDataConverter;
        cmp.colorMapToDisplayConverter = colorMapToDisplayConverter;
        cmp.colorMapUnit = colorMapUnit;
        cmp.labels = labels;
        cmp.recomputeLabels = recomputeLabels;
        cmp.persisted = persisted.clone();
        return cmp;
    }

    public boolean isDirty() {
        return dirty;
    }

    public void setDirty(boolean dirty) {
        this.dirty = dirty;
        notifyListener();
    }

    private void notifyListener() {
        for (IColorMapParametersListener listener : listeners) {
            listener.colorMapChanged();
        }
    }

    public void addListener(IColorMapParametersListener listener) {
        if (listener != null) {
            listeners.add(listener);
        }
    }

    public void removeListener(IColorMapParametersListener listener) {
        listeners.remove(listener);
    }

    public void setAlphaMask(byte[] alphaMask) {
        this.alphaMask = alphaMask;
    }

    public byte[] getAlphaMask() {
        return alphaMask;
    }

    public boolean isUseMask() {
        return useMask;
    }

    public void setUseMask(boolean useMask) {
        this.useMask = useMask;
    }

    @Override
    public int hashCode() {
        if (listeners.size() > 0) {
            return listeners.hashCode();
        } else if (colorMap != null) {
            return colorMap.hashCode();
        } else {
            return super.hashCode();
        }
    }

    public void setMirror(boolean mirror) {
        this.mirror = mirror;
    }

    public boolean isMirror() {
        return mirror;
    }

    /**
     * Get the Color object of the value
     * 
     * @param value
     *            value to get Color for in {@link #displayUnit}
     * @return
     */
    public Color getColorByValue(float value) {
        UnitConverter displayToColorMap = getDisplayToColorMapConverter();
        if (displayToColorMap != null) {
            value = (float) displayToColorMap.convert(value);
        }
        return Colormapper.getColorByValue(value, this);
    }

    /**
     * @return the logFactor
     */
    public float getLogFactor() {
        return logFactor;
    }

    /**
     * @param logFactor
     *            the logFactor to set
     */
    public void setLogFactor(float logFactor) {
        this.logFactor = logFactor;
    }

    /**
     * @return the persisted
     */
    public PersistedParameters getPersisted() {
        return persisted;
    }

    /**
     * @param persisted
     *            the persisted to set
     */
    public void setPersisted(PersistedParameters persisted) {
        this.persisted = persisted;
    }

    /**
     * Apply any saved settings from the colormap
     */
    public void applyPersistedParameters(PersistedParameters params) {
        this.persisted = params;
        if (params.colorMapMin != null) {
            this.colorMapMin = params.colorMapMin;
        }
        if (params.colorMapMax != null) {
            this.colorMapMax = params.colorMapMax;
        }
    }

    /**
     * @return the noDataValue
     */
    public double getNoDataValue() {
        return noDataValue;
    }

    /**
     * @param noDataValue
     *            the noDataValue to set
     */
    public void setNoDataValue(double noDataValue) {
        this.noDataValue = noDataValue;
    }

    public boolean isInterpolate() {
        return interpolate;
    }

    public void setInterpolate(boolean interpolate) {
        this.interpolate = interpolate;
    }

}
