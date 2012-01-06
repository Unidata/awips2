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

import org.eclipse.swt.graphics.RGB;

/**
 * This class contains the arrays of color scale data. There are three arrays
 * used: original, updated, and used.
 * 
 * The original color set array contains the color/value data that serves as a
 * starting point when the data is reset to the original set. If a save occurs
 * the this array is update with the data in the updated array.
 * 
 * The updated color set array is the array that gets all of the updates made by
 * the user. The the color data is reset this array is populated with the data
 * contained in the original color set array.
 * 
 * The used color set array contains the set of "used color". At this time the
 * 8,2 baseline is still being worked so the point of "used color set" is
 * unclear.
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
public class ColorScaleSets {
    /**
     * Array containing the original color set data.
     */
    private ArrayList<ColorScaleData> originalColorSet;

    /**
     * Array containing updates to the color set data.
     */
    private ArrayList<ColorScaleData> updatedColorSet;

    /**
     * Array containing the used color set data.
     */
    private ArrayList<ColorScaleData> usedColorSet;

    /**
     * Constructor.
     */
    public ColorScaleSets() {
        originalColorSet = new ArrayList<ColorScaleData>();
        updatedColorSet = new ArrayList<ColorScaleData>();
        usedColorSet = new ArrayList<ColorScaleData>();
    }

    /**
     * Constructor.
     * 
     * @param originalColorSet
     *            Original color set array.
     * @param usedColorSet
     *            Used color set array.
     */
    public ColorScaleSets(ArrayList<ColorScaleData> originalColorSet,
            ArrayList<ColorScaleData> usedColorSet) {
        this.originalColorSet = new ArrayList<ColorScaleData>(originalColorSet);
        updatedColorSet = new ArrayList<ColorScaleData>(originalColorSet);
        this.usedColorSet = new ArrayList<ColorScaleData>(usedColorSet);
    }

    /**
     * Update the "missing" scale color.
     * 
     * @param rgb
     *            RGB color.
     */
    public void updateMissingColor(RGB rgb) {
        for (int i = 0; i < updatedColorSet.size(); ++i) {
            if (updatedColorSet.get(i).value.compareTo(ColorScaleData.MISSING) == 0) {
                updatedColorSet.get(i).setColor(rgb);
                break;
            }
        }
    }

    /**
     * Update the "less than" scale color.
     * 
     * @param rgb
     *            RGB color.
     */
    public void updateLessThanColor(RGB rgb) {
        for (int i = 0; i < updatedColorSet.size(); ++i) {
            if (updatedColorSet.get(i).value
                    .compareTo(ColorScaleData.LESS_THAN_MIN) == 0) {
                updatedColorSet.get(i).setColor(rgb);
                break;
            }
        }
    }

    /**
     * Add/Update the color/value scale data. If the value exists, the color is
     * updated. If the value does not exist the color and value are added.
     * 
     * @param rgb
     *            RGB color.
     * @param dblVal
     *            Scale value.
     */
    public void addUpdateColorValue(RGB rgb, double dblVal) {

        Double tmpDbl;
        ColorScaleData newData;

        for (int i = 0; i < updatedColorSet.size(); i++) {
            tmpDbl = updatedColorSet.get(i).getDoubleVal();

            if (tmpDbl.isNaN() == false) {
                if (dblVal < tmpDbl.doubleValue()) {
                    newData = new ColorScaleData(rgb, dblVal);
                    updatedColorSet.add(i, newData);

                    // Return since we have inserted
                    // the data into the array
                    return;
                } else if (dblVal == tmpDbl.doubleValue()) {
                    newData = new ColorScaleData(rgb, dblVal);
                    updatedColorSet.set(i, newData);

                    // Return since we have updated
                    // the data in the array
                    return;
                }
            }
        }

        // Add the new color/value at the end of the array since
        // we couldn't find a value less than or equal to the
        // value passed in.
        newData = new ColorScaleData(rgb, dblVal);
        updatedColorSet.add(newData);
    }

    /**
     * Set the original color set array. This also sets the updated color set
     * array as well.
     * 
     * @param array
     *            Array of color scale data.
     */
    public void setOriginalArray(ArrayList<ColorScaleData> array) {
        originalColorSet = new ArrayList<ColorScaleData>(array);
        updatedColorSet = new ArrayList<ColorScaleData>(array);
    }

    /**
     * Set the used color set array.
     * 
     * @param array
     *            Array of color scale data.
     */
    public void setUsedArray(ArrayList<ColorScaleData> array) {
        usedColorSet = new ArrayList<ColorScaleData>(array);
    }

    /**
     * Reset the updated color scale data array to have the same data as the
     * original scale data array.
     */
    public void resetData() {
        updatedColorSet.clear();
        updatedColorSet = new ArrayList<ColorScaleData>(originalColorSet);
    }

    /**
     * Set the original color set array to have the same data as the updated
     * color set array.
     */
    public void saveModifiedData() {
        // ----------------------------------------------------------
        // NOTE: The color data needs to be save to the database.
        // ----------------------------------------------------------

        originalColorSet.clear();
        originalColorSet = new ArrayList<ColorScaleData>(updatedColorSet);
    }

    /**
     * Delete a color/value from the updated color scale data array.
     * 
     * @param dblVal
     *            Scale value.
     */
    public void deleteColorValue(double dblVal) {
        Double tmpDbl;

        for (int i = 0; i < updatedColorSet.size(); i++) {
            tmpDbl = updatedColorSet.get(i).getDoubleVal();

            if (tmpDbl.doubleValue() == dblVal) {
                updatedColorSet.remove(i);
                break;
            }
        }
    }

    /**
     * Get the number of color/value pairs in the update color scale data array.
     * 
     * @return The number of color/value pairs.
     */
    public int colorValueCount() {
        return updatedColorSet.size();
    }

    /**
     * Get the update color scale data array.
     * 
     * @return The update color scale data array.
     */
    public ArrayList<ColorScaleData> getUpdatedColorSetArray() {
        return updatedColorSet;
    }

    /**
     * Get the used color scale data array.
     * 
     * @return The used color scale data array.
     */
    public ArrayList<ColorScaleData> getUsedColorSetArray() {
        return usedColorSet;
    }

}
