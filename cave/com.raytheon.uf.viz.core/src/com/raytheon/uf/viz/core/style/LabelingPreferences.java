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

package com.raytheon.uf.viz.core.style;

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;

/**
 * Contains the style preferences related to labeling
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2007            chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LabelingPreferences implements Cloneable {

    private static final String SEPARATOR = " ";

    @XmlElement(name = "values")
    @XmlList
    private float[] values;

    @XmlElement(name = "increment")
    private float increment;

    @XmlAttribute
    private int labelSpacing;

    @XmlAttribute
    private String minLabel;

    @XmlAttribute
    private String maxLabel;

    @XmlAttribute
    private String labelFormat;

    @XmlAttribute
    private String minMaxLabelFormat;

    @XmlAttribute
    private int numberOfContours = -1;

    @XmlAttribute
    private boolean createNegativeValues = false;

    @XmlAttribute
    private int labelTrimLeft = 0;

    @XmlAttribute
    private int maxMinTrimLeft = 0;

    public LabelingPreferences() {

    }

    /**
     * Copy constructor
     * 
     * @param prefs
     */
    public LabelingPreferences(LabelingPreferences prefs) {
        if (prefs.values != null) {
            this.values = new float[prefs.values.length];
            System.arraycopy(prefs.values, 0, this.values, 0,
                    this.values.length);
        }
        this.increment = prefs.increment;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public LabelingPreferences clone() {
        return new LabelingPreferences(this);
    }

    /**
     * @return the values
     */
    public float[] getValues() {
        return values;
    }

    /**
     * @param values
     *            the values to set
     */
    public void setValues(float[] values) {
        this.values = values;
    }

    /**
     * @return the increment
     */
    public float getIncrement() {
        return increment;
    }

    /**
     * @param increment
     *            the increment to set
     */
    public void setIncrement(float increment) {
        this.increment = increment;
    }

    /**
     * Gets the values interpreted as a String (used by JiBX)
     * 
     * @return a space separated list of values
     */
    public String getValuesString() {
        String returnString = null;
        if (values != null) {
            StringBuffer sb = new StringBuffer();
            for (Float f : values) {
                sb.append(f);
                sb.append(SEPARATOR);
            }
            returnString = sb.toString().trim();
        }

        return returnString;
    }

    /**
     * Sets the values from an interpretation of a String (used by JiBX)
     * 
     * @param aValues
     *            a space separated list of values
     */
    public void setValuesString(String aValues) {
        if (aValues != null) {
            String[] floats = aValues.split(SEPARATOR);
            values = new float[floats.length];
            for (int i = 0; i < floats.length; i++) {
                values[i] = Float.parseFloat(floats[i]);
            }
        }
    }

    public String getMinLabel() {
        return minLabel;
    }

    public void setMinLabel(String minLabel) {
        this.minLabel = minLabel;
    }

    public String getMaxLabel() {
        return maxLabel;
    }

    public void setMaxLabel(String maxLabel) {
        this.maxLabel = maxLabel;
    }

    public String getLabelFormat() {
        return labelFormat;
    }

    public void setLabelFormat(String labelFormat) {
        this.labelFormat = labelFormat;
    }

    public String getMinMaxLabelFormat() {
        return minMaxLabelFormat;
    }

    public void setMinMaxLabelFormat(String minMaxLabelFormat) {
        this.minMaxLabelFormat = minMaxLabelFormat;
    }

    public int getLabelSpacing() {
        return labelSpacing;
    }

    public void setLabelSpacing(int labelSpacing) {
        this.labelSpacing = labelSpacing;
    }

    public int getNumberOfContours() {
        return numberOfContours;
    }

    public void setNumberOfContours(int numberOfContours) {
        this.numberOfContours = numberOfContours;
    }

    public boolean isCreateNegativeValues() {
        return createNegativeValues;
    }

    public void setCreateNegativeValues(boolean createNegativeValues) {
        this.createNegativeValues = createNegativeValues;
    }

    public int getLabelTrimLeft() {
        return labelTrimLeft;
    }

    public void setLabelTrimLeft(int labelTrimLeft) {
        this.labelTrimLeft = labelTrimLeft;
    }

    public int getMaxMinTrimLeft() {
        return maxMinTrimLeft;
    }

    public void setMaxMinTrimLeft(int maxMinTrimLeft) {
        this.maxMinTrimLeft = maxMinTrimLeft;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Float.floatToIntBits(increment);
        result = prime * result + Arrays.hashCode(values);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LabelingPreferences other = (LabelingPreferences) obj;
        if (Float.floatToIntBits(increment) != Float
                .floatToIntBits(other.increment)) {
            return false;
        }
        if (!Arrays.equals(values, other.values)) {
            return false;
        }
        return true;
    }

}
