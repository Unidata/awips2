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
package com.raytheon.viz.hydrocommon.data;

import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2008            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColorNameData extends HydroDBData implements IHydroDBData {

    private String colorName;

    private RGB colorValue;

    /**
     * default constructor, sets colorName to null
     */
    public ColorNameData() {
        colorName = null;
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public ColorNameData(QueryResultRow data, Map<String, Integer> dataMap) {
        setColorName(getDBValue("color_name", data, dataMap, ""));
    }

    @Override
    public String getConstrainedSelectStatement() {
        return null;
    }

    @Override
    public String getDeleteStatement() {
        return null;
    }

    @Override
    public String getExistsStatement() {
        return null;
    }

    @Override
    public String getInsertStatement() {
        return null;
    }

    @Override
    public String getPKStatement() {
        return null;
    }

    @Override
    public String getSelectStatement() {
        return "SELECT color_name FROM colorname";
    }

    @Override
    public String getUpdateStatement() {
        return null;
    }

    /**
     * get name of color
     * 
     * @return the color name
     */
    public String getColorName() {
        return colorName;
    }

    /**
     * Set name of color
     * 
     * @param colorName
     */
    public void setColorName(String colorName) {
        this.colorName = colorName;
        if (colorName != null) {
            setColorValue(RGBColors.getRGBColor(colorName));
        }
    }

    /**
     * get RGB value corresponding to color name
     * 
     * @return RGB value
     */
    public RGB getColorValue() {
        return colorValue;
    }

    /**
     * Set color value
     * 
     * @param colorValue
     */
    public void setColorValue(RGB colorValue) {
        this.colorValue = colorValue;
    }
}
