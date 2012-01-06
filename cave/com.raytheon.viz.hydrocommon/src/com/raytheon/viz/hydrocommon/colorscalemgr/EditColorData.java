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
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.swt.graphics.RGB;

/**
 * This class manages the edit color data. It acts as a model to access all of
 * the data that will be displayed in combo boxes and color/value labels.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class EditColorData {
    /**
     * Map of all the source entries.
     */
    private TreeMap<String, ColorDataTypeSets> sourceMap;

    /**
     * Constructor.
     */
    public EditColorData() {
        sourceMap = new TreeMap<String, ColorDataTypeSets>();
    }

    /**
     * Add a source and an associated color data type set.
     * 
     * @param key
     *            Source key.
     * @param value
     *            Color data type set data.
     */
    public void addSource(String key, ColorDataTypeSets value) {
        sourceMap.put(key, value);
    }

    /**
     * Get the color data type set associated with the source key passed in.
     * 
     * @param sourceKey
     *            Source key.
     * @return Color data type set.
     */
    public ColorDataTypeSets getColorDataTypeSets(String sourceKey) {
        return sourceMap.get(sourceKey);
    }

    /**
     * Get the set of source keys.
     * 
     * @return A set of source keys.
     */
    public Set<String> getSourceKeys() {
        return sourceMap.keySet();
    }

    /**
     * Get the number of color/value pairs.
     * 
     * @param sourceKey
     *            Source key.
     * @param dataTypeKey
     *            Data type key.
     * @return Number of color/value pairs.
     */
    public int getColorValueCount(String sourceKey, String dataTypeKey) {
        int count = 0;

        count = sourceMap.get(sourceKey).getColorScaleSet(dataTypeKey)
                .colorValueCount();

        return count;
    }

    /**
     * Get the updated color scale data array.
     * 
     * @param sourceKey
     *            Source key.
     * @param dataTypeKey
     *            Data type key.
     * @return Updated color scale data array.
     */
    public ArrayList<ColorScaleData> getColorScaleDataArray(String sourceKey,
            String dataTypeKey) {
        return sourceMap.get(sourceKey).getColorScaleDataArray(dataTypeKey);
    }

    /**
     * Get the used color scale data array.
     * 
     * @param sourceKey
     *            Source key.
     * @param dataTypeKey
     *            Data type key.
     * @return Used color scale data array.
     */
    public ArrayList<ColorScaleData> getUsedColorScaleDataArray(
            String sourceKey, String dataTypeKey) {
        return sourceMap.get(sourceKey).getColorScaleDataArray(dataTypeKey);
    }

    /**
     * Delete a scale color/value pair.
     * 
     * @param sourceKey
     *            Source key.
     * @param dataTypeKey
     *            Data type key.
     * @param dblVal
     *            Scale value.
     */
    public void deleteColorValue(String sourceKey, String dataTypeKey,
            double dblVal) {
        sourceMap.get(sourceKey).deleteColorValue(dataTypeKey, dblVal);
    }

    /**
     * Add/Update a scale color/value pair.
     * 
     * @param sourceKey
     *            Source key.
     * @param dataTypeKey
     *            Data type key.
     * @param rgb
     *            RGB color.
     * @param dblVal
     *            Scale value.
     */
    public void updateColorValue(String sourceKey, String dataTypeKey,
            RGB rgb, double dblVal) {
        sourceMap.get(sourceKey).addUpdateColorValue(dataTypeKey, rgb, dblVal);
    }

    /**
     * Add/Update missing color scale data. Once the "missing" scale color/value
     * pair is added only the color can be updated.
     * 
     * @param sourceKey
     *            Source key.
     * @param dataTypeKey
     *            Data type key.
     * @param rgb
     *            RGB color.
     */
    public void addUpdateMissingColor(String sourceKey, String dataTypeKey,
            RGB rgb) {
        sourceMap.get(sourceKey).addUpdateMissingColor(dataTypeKey, rgb);
    }

    /**
     * Add/Update "less than" color scale data. Once the "less than" scale
     * color/value pair is added only the color can be updated.
     * 
     * @param sourceKey
     *            Source key.
     * @param dataTypeKey
     *            Data type key.
     * @param rgb
     *            RGB color.
     */
    public void addUpdateLessThanColor(String sourceKey, String dataTypeKey,
            RGB rgb) {
        sourceMap.get(sourceKey).addUpdateLessThanColor(dataTypeKey, rgb);
    }

    /**
     * Reset all of the scale color/value pairs back to the original or last
     * save state.
     */
    public void resetColorValueData() {
        Set<String> sourceKeys = sourceMap.keySet();

        for (Iterator<String> sourceIter = sourceKeys.iterator(); sourceIter
                .hasNext();) {
            ColorDataTypeSets dtSet = sourceMap.get(sourceIter.next());

            Set<String> keys = dtSet.getKeySet();

            for (Iterator<String> iterator = keys.iterator(); iterator
                    .hasNext();) {
                ColorScaleSets colorScaleSet = dtSet.getColorScaleSet(iterator
                        .next());
                if (colorScaleSet != null) {
                    colorScaleSet.resetData();
                }
            }
        }
    }
}
