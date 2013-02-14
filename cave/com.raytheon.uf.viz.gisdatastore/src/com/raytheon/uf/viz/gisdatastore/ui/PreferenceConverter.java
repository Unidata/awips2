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

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * Utility for loading and saving GIS Viewer preferences
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

public class PreferenceConverter {
    private PreferenceConverter() {
        // unused, all methods are static
    }

    /**
     * Get an RGB color preference value. If no preference is stored,
     * defaultValue will be returned
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param defaultValue
     *            the default value
     * @return the RGB color
     */
    public static RGB getRGB(IPreferenceStore prefs, String key,
            String defaultValue) {
        String value = prefs.getString(key);
        RGB newColor = StringConverter.asRGB(value);

        if (newColor == null) {
            newColor = StringConverter.asRGB(defaultValue);
        }
        return newColor;
    }

    /**
     * Store an RGB color preference value.
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param color
     *            the RGB color to be stored
     */
    public static void setValue(IPreferenceStore prefs, String key, RGB color) {
        String value = RGBColors.getColorName(color);
        prefs.setValue(key, value);
    }

    /**
     * Get a LineStyle value. If no preference is stored, defaultValue will be
     * returned
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param defaultValue
     *            the default value
     * @return the LineStyle
     */
    public static LineStyle getLineStyle(IPreferenceStore prefs, String key,
            String defaultValue) {
        String value = prefs.getString(key);
        LineStyle style = StringConverter.asLineStyle(value);

        if (style == null) {
            style = StringConverter.asLineStyle(defaultValue);
        }

        return style;
    }

    /**
     * Store a LineStyle preference value.
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param opacity
     *            the LineStyle to be stored
     */
    public static void setValue(IPreferenceStore prefs, String key,
            LineStyle style) {
        String value = style.name();
        prefs.setValue(key, value);
    }

    /**
     * Get an integer preference value. If no preference is stored, defaultValue
     * will be returned
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param defaultValue
     *            the default value
     * @return the integer value
     */
    public static int getInt(IPreferenceStore prefs, String key,
            String defaultValue) {
        String stringValue = prefs.getString(key);
        Integer value = StringConverter.asInteger(stringValue);
        if (value == null) {
            value = StringConverter.asInteger(defaultValue);
        }
        return value;
    }

    /**
     * Store an integer preference value.
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param value
     *            the integer value to be stored
     */
    public static void setValue(IPreferenceStore prefs, String key, int value) {
        String stringValue = StringConverter.asString(value);
        prefs.setValue(key, stringValue);
    }

    /**
     * Get an float preference value. If no preference is stored, defaultValue
     * will be returned
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param defaultValue
     *            the default value
     * @return the float value
     */
    public static float getFloat(IPreferenceStore prefs, String key,
            String defaultValue) {
        String stringValue = prefs.getString(key);
        Float value = StringConverter.asFloat(stringValue);
        if (value == null) {
            value = StringConverter.asFloat(defaultValue);
        }
        return value;
    }

    /**
     * Store an float preference value.
     * 
     * @param prefs
     *            the preference store
     * @param key
     *            the preference key
     * @param value
     *            the float value to be stored
     */
    public static void setValue(IPreferenceStore prefs, String key, float value) {
        String stringValue = StringConverter.asString(value);
        prefs.setValue(key, stringValue);
    }
}
