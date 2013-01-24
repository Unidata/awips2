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
package com.raytheon.uf.viz.gisdatastore.ui;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * Convert GIS Viewer preference types to/from strings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 27, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class StringConverter {
    private StringConverter() {
        // unused, all methods are static
    }

    /**
     * Convert RGB color value to string
     * 
     * @param color
     *            RGB color value
     * @return string String value
     */
    public static String asString(RGB color) {
        return RGBColors.getColorName(color);
    }

    /**
     * Convert string to RGB color value
     * 
     * @param string
     *            String value
     * @return RGB color value or null if string is not a valid RGB color name
     *         or hex value of the form "#rrggbb"
     */
    public static RGB asRGB(String string) {
        RGB rgb = RGBColors.getRGBColor(string);
        return rgb;
    }

    /**
     * Convert LineStyle value to string
     * 
     * @param opacity
     *            LineStyle value
     * @return string String value
     */
    public static String asString(LineStyle style) {
        return style.name();
    }

    /**
     * Convert string to LineStyle value
     * 
     * @param string
     *            String value
     * @return LineStyle value or null if string is not a valid LineStyle name
     */
    public static LineStyle asLineStyle(String string) {
        LineStyle style = null;

        try {
            style = LineStyle.valueOf(string);
        } catch (Exception e) {
            style = null;
        }

        return style;
    }

    /**
     * Convert integer value to string
     * 
     * @param value
     *            integer value
     * @return string String value
     */
    public static String asString(int value) {
        return Integer.toString(value);
    }

    /**
     * Convert string to Integer value
     * 
     * @param string
     *            String Value
     * @return Integer value or null if string is not a valid integer
     *         representation
     */
    public static Integer asInteger(String string) {
        Integer value = null;

        try {
            value = Integer.parseInt(string);
        } catch (Exception e) {
            value = null;
        }

        return value;
    }

    /**
     * Convert float value to string
     * 
     * @param value
     *            float value
     * @return string String value
     */
    public static String asString(float value) {
        return Float.toString(value);
    }

    /**
     * Convert string to Float value
     * 
     * @param string
     *            String Value
     * @return Float value or null if string is not a valid float representation
     */
    public static Float asFloat(String string) {
        Float value = null;

        try {
            value = Float.parseFloat(string);
        } catch (Exception e) {
            value = null;
        }

        return value;
    }
}
