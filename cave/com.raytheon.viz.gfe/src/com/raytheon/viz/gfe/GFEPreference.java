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
 * TODO Add Description GFEPreference.java Feb 8, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * Feb 8, 2008             Eric Babin    Initial Creation
 * Jun 2, 2009  #2159      Richard Peter Added getArrayPreference
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */
public class GFEPreference {

    public static String getPreference(String id) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getString(id);
        }
        return "";
    }

    public static boolean getBooleanPreference(String id) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getBoolean(id);
        }
        return false;
    }

    public static float getFloatPreference(String id) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getFloat(id);
        }
        return 0.0f;
    }
    
    public static double getDoublePreference(String id) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getDouble(id);
        }
        return 0.0;
    }

    public static int getIntPreference(String id) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getInt(id);
        }
        return 0;
    }

    public static String[] getArrayPreference(String id) {
        if (contains(id)) {
            return Activator.getDefault().getPreferenceStore().getStringArray(
                    id);
        }

        return null;
    }

    public static void setPreference(String id, String value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    public static void setPreference(String id, boolean value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    public static void setPreference(String id, float value) {
        Activator.getDefault().getPreferenceStore()
        		.setValue(id, value);
    }
    
    public static void setPreference(String id, double value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    public static void setPreference(String id, int value) {
        Activator.getDefault().getPreferenceStore().setValue(id, value);
    }

    public static boolean contains(String id) {
        Activator activator = Activator.getDefault();
        if (activator == null)
            return false;

        return activator.getPreferenceStore().contains(id);
    }

    public static boolean storeAvailable() {
        if (Activator.getDefault() != null)
            return true;

        return false;
    }

    public static void addPropertyChangeListener(
            IPropertyChangeListener listener) {
        Activator activator = Activator.getDefault();

        if (activator != null) {
            activator.getPreferenceStore().addPropertyChangeListener(listener);
        }
    }

    public static void removePropertyChangeListener(
            IPropertyChangeListener listener) {
        Activator activator = Activator.getDefault();

        if (activator != null) {
            activator.getPreferenceStore().removePropertyChangeListener(
                    listener);
        }
    }
}
