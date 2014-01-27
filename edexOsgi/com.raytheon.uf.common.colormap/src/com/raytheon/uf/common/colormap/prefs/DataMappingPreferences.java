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

import java.util.ArrayList;
import java.util.Collections;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.units.PiecewisePixel;

/**
 * Preference object for specifying data mapping relationships
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  6/2013      DR 16070   jgerth      Interpolation capability
 * Aug 2, 2013  2211       mschenke    Backed out 16070 changes
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataMappingPreferences {

    @XmlAccessorType(XmlAccessType.NONE)
    public static class DataMappingEntry implements
            Comparable<DataMappingEntry> {
        @XmlAttribute
        private Double displayValue;

        @XmlAttribute
        private Double pixelValue;

        @XmlAttribute
        private String label;

        @XmlAttribute
        private String sample;

        @XmlAttribute
        private String operator;

        /**
         * @return the displayValue
         */
        public Double getDisplayValue() {
            return displayValue;
        }

        /**
         * @param displayValue
         *            the displayValue to set
         */
        public void setDisplayValue(Double displayValue) {
            this.displayValue = displayValue;
        }

        /**
         * @return the pixelValue
         */
        public Double getPixelValue() {
            return pixelValue;
        }

        /**
         * @param pixelValue
         *            the pixelValue to set
         */
        public void setPixelValue(Double pixelValue) {
            this.pixelValue = pixelValue;
        }

        /**
         * @return the label
         */
        public String getLabel() {
            return label;
        }

        /**
         * @param label
         *            the label to set
         */
        public void setLabel(String label) {
            this.label = label;
        }

        /**
         * @return the sample
         */
        public String getSample() {
            return sample;
        }

        /**
         * @param sample
         *            the sample to set
         */
        public void setSample(String sample) {
            this.sample = sample;
        }

        /**
         * @param operator
         *            the operator to set
         */
        public void setOperator(String operator) {
            this.operator = operator;
        }

        /**
         * @return the operator
         */
        public String getOperator() {
            return operator;
        }

        /*
         * Compares pixelValue
         * 
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(DataMappingEntry o2) {
            if (o2 == null) {
                return 1;
            }
            Double pixelValue1 = this.pixelValue;
            Double pixelValue2 = o2.pixelValue;
            if (pixelValue1 == null) {
                if (pixelValue2 == null) {
                    return 0;
                }
                return -1;
            }
            if (pixelValue2 == null) {
                return 1;
            }

            if (pixelValue1 == pixelValue2) {
                return 0;
            } else if (pixelValue1 > pixelValue2) {
                return 1;
            }

            return -1;
        }

    }

    private final ArrayList<DataMappingEntry> entries = new ArrayList<DataMappingEntry>();

    private final ArrayList<DataMappingEntry> greaterThanEntries = new ArrayList<DataMappingEntry>();

    private final ArrayList<DataMappingEntry> lessThanEntries = new ArrayList<DataMappingEntry>();

    private final ArrayList<DataMappingEntry> equalsEntries = new ArrayList<DataMappingEntry>();

    private Unit<?> imageUnit;

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public Unit<?> getImageUnit(Unit<?> displayUnit) {
        if (imageUnit == null) {
            ArrayList<Double> pixelValues = new ArrayList<Double>();
            ArrayList<Double> dispValues = new ArrayList<Double>();
            for (DataMappingEntry entry : entries) {
                if (entry.getPixelValue() != null
                        && entry.getDisplayValue() != null) {
                    pixelValues.add(entry.getPixelValue());
                    dispValues.add(entry.getDisplayValue());
                }
            }

            int n = pixelValues.size();
            if (n > 1) {
                double[] pix = new double[n];
                double[] disp = new double[n];

                for (int i = 0; i < n; i++) {
                    pix[i] = pixelValues.get(i);
                    disp[i] = dispValues.get(i);
                }

                imageUnit = new PiecewisePixel(displayUnit, pix, disp);
            }
        }
        return imageUnit;
    }

    /**
     * @return the entries
     */
    @XmlElement(name = "entry")
    public DataMappingEntry[] getSerializableEntries() {
        return entries.toArray(new DataMappingEntry[entries.size()]);
    }

    public void setSerializableEntries(DataMappingEntry[] entryArray) {
        entries.clear();
        greaterThanEntries.clear();
        lessThanEntries.clear();
        equalsEntries.clear();
        for (DataMappingEntry entry : entryArray) {
            addEntry(entry);
        }
    }

    public ArrayList<DataMappingEntry> getEntries() {
        return entries;
    }

    public void addEntry(DataMappingEntry entry) {
        entries.add(entry);
        sortEntry(entry);
    }

    private void sortEntry(DataMappingEntry entry) {
        String operator = entry.getOperator();
        Double pixelValue = entry.getPixelValue();
        if (pixelValue != null) {
            if (operator == null || "=".equals(operator)) {
                equalsEntries.add(entry);
            } else if (">".equals(operator)) {
                greaterThanEntries.add(entry);
                Collections
                        .sort(greaterThanEntries, Collections.reverseOrder());
            } else if ("<".equals(operator)) {
                lessThanEntries.add(entry);
                Collections.sort(lessThanEntries);
            }
        }
    }

    /**
     * Matches a number against the pixelValue in each entry based on the
     * operator until the first match is found.
     * 
     * @param dataValue
     *            the data value to match
     * @return The entry label, if one exists.
     */
    public String getLabelValueForDataValue(double dataValue) {
        for (DataMappingEntry entry : equalsEntries) {
            Double entryValue = entry.getPixelValue();
            if (entryValue.equals(dataValue)) {
                return entry.getLabel();
            }
        }

        for (DataMappingEntry entry : lessThanEntries) {
            Double entryValue = entry.getPixelValue();
            if (dataValue < entryValue) {
                return entry.getLabel();
            }
        }

        for (DataMappingEntry entry : greaterThanEntries) {
            Double entryValue = entry.getPixelValue();
            if (dataValue > entryValue) {
                return entry.getLabel();
            }
        }
        return null;
    }

}
