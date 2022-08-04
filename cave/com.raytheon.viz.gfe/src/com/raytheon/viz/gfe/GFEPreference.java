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
package com.raytheon.viz.gfe;

import org.eclipse.jface.util.IPropertyChangeListener;

/**
 * GFE Preference wrapper
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer       Description
 * ------------- -------- -------------- ---------------------------------------
 * Feb 08, 2008           Eric Babin     Initial Creation
 * Jun 02, 2009  2159     Richard Peter  Added getArrayPreference
 * Jan 23, 2018  7153     randerso       Changes to allow new GFE config file to
 *                                       be selected when perspective is
 *                                       re-opened.
 *
 * </pre>
 *
 * @author ebabin
 */
public class GFEPreference {

    /**
     * @return the name of the currently loaded GFE config file
     */
    public static String getConfigName() {
        return Activator.getDefault().getPreferenceStore().getConfigName();
    }

    /**
     * Get String preference
     *
     * @param id
     *            preference id
     * @return preference value or "" if id is not defined
     */
    public static String getString(String id) {
        return getString(id, "");
    }

    /**
     * Get float preference
     *
     * @param id
     *            preference id
     * @return preference value or false if id is not defined
     */
    public static boolean getBoolean(String id) {
        return getBoolean(id, false);
    }

    /**
     * Get String preference
     *
     * @param id
     *            preference id
     * @return preference value or 0.0f if id is not defined
     */
    public static float getFloat(String id) {
        return getFloat(id, 0.0f);
    }

    /**
     * Get double preference
     *
     * @param id
     *            preference id
     * @return preference value or 0.0 if id is not defined
     */
    public static double getDouble(String id) {
        return getDouble(id, 0.0);
    }

    /**
     * Get integer preference
     *
     * @param id
     *            preference id
     * @return preference value or 0 if id is not defined
     */
    public static int getInt(String id) {
        return getInt(id, 0);
    }

    /**
     * Get String[] preference
     *
     * @param id
     *            preference id
     * @return preference value or empty String[] if id is not defined
     */
    public static String[] getStringArray(String id) {
        return getStringArray(id, new String[0]);
    }

    /**
     * Get float[] preference
     *
     * @param id
     *            preference id
     * @return preference value or empty float[] if id is not defined
     */
    public static float[] getFloatArray(String id) {
        return getFloatArray(id, new float[0]);
    }

    /**
     * Get double[] preference
     *
     * @param id
     *            preference id
     * @return preference value or empty double[] if id is not defined
     */
    public static double[] getDoubleArray(String id) {
        return getDoubleArray(id, new double[0]);
    }

    /**
     * Get int[] preference
     *
     * @param id
     *            preference id
     * @return preference value or empty int[] if id is not defined
     */
    public static int[] getIntArray(String id) {
        return getIntArray(id, new int[0]);
    }

    /**
     * Get String preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static String getString(String id, String defaultValue) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getString(id);
        }
        return defaultValue;
    }

    /**
     * Get boolean preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static boolean getBoolean(String id, boolean defaultValue) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getBoolean(id);
        }
        return defaultValue;
    }

    /**
     * Get float preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static float getFloat(String id, float defaultValue) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getFloat(id);
        }
        return defaultValue;
    }

    /**
     * Get double preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static double getDouble(String id, double defaultValue) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getDouble(id);
        }
        return defaultValue;
    }

    /**
     * Get integer preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static int getInt(String id, int defaultValue) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getInt(id);
        }
        return defaultValue;
    }

    /**
     * Get String[] preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static String[] getStringArray(String id, String[] defaultValue) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore()
                    .getStringArray(id);
        }

        return defaultValue;
    }

    /**
     * Get float[] preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static float[] getFloatArray(String id, float[] defaultValue) {
        if (contains(id)) {
            Float[] boxedArray = Activator.getDefault().getPreferenceStore()
                    .getFloatArray(id);
            float[] array = new float[boxedArray.length];
            for (int i = 0; i < boxedArray.length; i++) {
                array[i] = boxedArray[i];
            }
            return array;
        }

        return defaultValue;
    }

    /**
     * Get double[] preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static double[] getDoubleArray(String id, double[] defaultValue) {
        if (contains(id)) {
            Double[] boxedArray = Activator.getDefault().getPreferenceStore()
                    .getDoubleArray(id);
            double[] array = new double[boxedArray.length];
            for (int i = 0; i < boxedArray.length; i++) {
                array[i] = boxedArray[i];
            }
            return array;
        }

        return defaultValue;
    }

    /**
     * Get int[] preference
     *
     * @param id
     *            preference id
     * @param defaultValue
     *            value to be returned if preference is not defined
     * @return preference value or def value
     */
    public static int[] getIntArray(String id, int[] defaultValue) {
        if (contains(id)) {
            Integer[] boxedArray = Activator.getDefault().getPreferenceStore()
                    .getIntArray(id);
            int[] array = new int[boxedArray.length];
            for (int i = 0; i < boxedArray.length; i++) {
                array[i] = boxedArray[i];
            }
            return array;
        }

        return defaultValue;
    }

    /**
     * Set String preference
     *
     * @param id
     *            preference id
     * @param value
     *            preference value
     */
    public static void setPreference(String id, String value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    /**
     * Set boolean preference
     *
     * @param id
     *            preference id
     * @param value
     *            preference value
     */
    public static void setPreference(String id, boolean value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    /**
     * Set float preference
     *
     * @param id
     *            preference id
     * @param value
     *            preference value
     */
    public static void setPreference(String id, float value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    /**
     * Set double preference
     *
     * @param id
     *            preference id
     * @param value
     *            preference value
     */
    public static void setPreference(String id, double value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    /**
     * Set integer preference
     *
     * @param id
     *            preference id
     * @param value
     *            preference value
     */
    public static void setPreference(String id, int value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    /**
     * Set String[] preference
     *
     * @param id
     *            preference id
     * @param value
     *            preference value
     */
    public static void setPreference(String id, String[] value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    /**
     * @param id
     *            preference id
     * @return true if preference id is defined
     */
    public static boolean contains(String id) {
        Activator activator = Activator.getDefault();
        if (activator == null) {
            return false;
        }

        return activator.getPreferenceStore().contains(id);
    }

    /**
     * Add a listener to be called if preference is changed
     *
     * @param listener
     */
    public static void addPropertyChangeListener(
            IPropertyChangeListener listener) {
        Activator activator = Activator.getDefault();

        if (activator != null) {
            activator.getPreferenceStore().addPropertyChangeListener(listener);
        }
    }

    /**
     * Remove a property change listener
     *
     * @param listener
     */
    public static void removePropertyChangeListener(
            IPropertyChangeListener listener) {
        Activator activator = Activator.getDefault();

        if (activator != null) {
            activator.getPreferenceStore()
                    .removePropertyChangeListener(listener);
        }
    }

    /**
     * Add a listener to be called if configuration is changed
     *
     * @param listener
     */
    public static void addConfigurationChangeListener(
            IConfigurationChangeListener listener) {
        Activator activator = Activator.getDefault();

        if (activator != null) {
            activator.getPreferenceStore()
                    .addConfigurationChangeListener(listener);
        }
    }

    /**
     * Remove a configuration change listener
     *
     * @param listener
     */
    public static void removeConfigurationChangeListener(
            IConfigurationChangeListener listener) {
        Activator activator = Activator.getDefault();

        if (activator != null) {
            activator.getPreferenceStore()
                    .removeConfigurationChangeListener(listener);
        }
    }
}
