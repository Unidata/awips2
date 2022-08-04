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

package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.eclipse.swt.graphics.RGB;

/**
 * Color data type sets for the color scale manager.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 18 APR 2013  1790       rferrel     Clean up method interfaces;
 *                                      part of non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ColorDataTypeSets {
    /**
     * TreeMap containing the data types and the associated color scale sets.
     */
    private TreeMap<String, ColorScaleSets> dataTypeMap;

    /**
     * Constructor.
     */
    public ColorDataTypeSets() {
        dataTypeMap = new TreeMap<String, ColorScaleSets>();
    }

    /**
     * Add a new data type and color scale set to the map.
     * 
     * @param dataType
     *            Data type.
     * @param colorSets
     *            Color set data.
     */
    public void addDataTypeColorSets(String dataType, ColorScaleSets colorSets) {
        dataTypeMap.put(dataType, colorSets);
    }

    /**
     * Get the set of data types.
     * 
     * @return Set of data types.
     */
    public Set<String> getDataTypes() {
        Iterator<String> i = dataTypeMap.keySet().iterator();
        Set<String> dataTypes = new TreeSet<String>();

        while (i.hasNext()) {
            String val = i.next();
            if (dataTypeMap.get(val) == null) {
                dataTypeMap.remove(val);
            } else {
                int idx = val.indexOf('_');
                dataTypes.add(val.substring(idx + 1));
            }
        }
        return dataTypes;
    }

    public Set<String> getDurations(String dataType) {
        Iterator<String> i = dataTypeMap.keySet().iterator();
        Set<String> durations = new TreeSet<String>();

        while (i.hasNext()) {
            String val = i.next();
            if (val.contains(dataType)) {
                int idx = val.indexOf('_');
                durations.add(val.substring(0, idx));
            }
        }

        return durations;
    }

    /**
     * Get the color scale set data associated with the data type passed in.
     * 
     * @param dataTypeKey
     *            Data type key.
     * @return ColorScaleSets data class.
     */
    public ColorScaleSets getColorScaleSet(String dataTypeKey) {
        return dataTypeMap.get(dataTypeKey);
    }

    /**
     * Get the array of color set data that has been updated.
     * 
     * @param dataTypeKey
     *            Data type key.
     * @return Array of color set data (updated).
     */
    public List<ColorScaleData> getColorScaleDataArray(String dataTypeKey) {
        ColorScaleSets colorSets = dataTypeMap.get(dataTypeKey);
        if (colorSets == null) {
            return new ArrayList<ColorScaleData>();
        } else {
            return colorSets.getUpdatedColorSetArray();
        }
    }

    /**
     * Get the array of color set data (used color set).
     * 
     * @param dataTypeKey
     *            Data type key.
     * @return Array of color set data (used).
     */
    public List<ColorScaleData> getUsedColorScaleDataArray(String dataTypeKey) {
        ColorScaleSets colorSets = dataTypeMap.get(dataTypeKey);
        if (colorSets == null) {
            return new ArrayList<ColorScaleData>();
        } else {
            return colorSets.getUsedColorSetArray();
        }
    }

    /**
     * Delete a color value pair.
     * 
     * @param dataTypeKey
     *            Data type key.
     * @param dblVal
     *            Value associated with the color.
     */
    public void deleteColorValue(String dataTypeKey, double dblVal) {
        ColorScaleSets colorSets = dataTypeMap.get(dataTypeKey);
        colorSets.deleteColorValue(dblVal);
    }

    /**
     * Add or update the color value pair. If the value does not exist, the
     * value and color are added to the list. If the value exists, then update
     * the color.
     * 
     * @param dataTypeKey
     *            Data type key.
     * @param rgb
     *            RGB color value.
     * @param dblVal
     *            Value associated with the color.
     */
    public void addUpdateColorValue(String dataTypeKey, RGB rgb, double dblVal) {
        dataTypeMap.get(dataTypeKey).addUpdateColorValue(rgb, dblVal);
    }

    /**
     * Add/Update the "Missing" color value pair. The add will only occur when
     * the color/value list is created because the missing value "MSG" cannot be
     * changed. The update part will only change the color.
     * 
     * @param dataTypeKey
     *            Data type key.
     * @param rgb
     *            RGB color value.
     */
    public void addUpdateMissingColor(String dataTypeKey, RGB rgb) {
        ColorScaleSets colorSets = dataTypeMap.get(dataTypeKey);
        colorSets.updateMissingColor(rgb);
    }

    /**
     * Add/Update the "> Min" (less than minimum) color value pair. The add will
     * only occur when the color/value list is created because the missing value
     * "> Min" cannot be changed. The update part will only change the color.
     * 
     * @param dataTypeKey
     *            Data type key.
     * @param rgb
     *            RGB color value.
     */
    public void addUpdateLessThanColor(String dataTypeKey, RGB rgb) {
        ColorScaleSets colorSets = dataTypeMap.get(dataTypeKey);
        colorSets.updateLessThanColor(rgb);
    }

    /**
     * Resets the list of changed color/value to the last saved instance.
     * 
     * @param dataTypeKey
     */
    public void resetColorValueData(String dataTypeKey) {
        dataTypeMap.get(dataTypeKey).resetData();

    }

    /**
     * Get the key set for the type map
     * 
     * @return keyset
     */
    public Set<String> getKeySet() {
        return dataTypeMap.keySet();
    }

}
