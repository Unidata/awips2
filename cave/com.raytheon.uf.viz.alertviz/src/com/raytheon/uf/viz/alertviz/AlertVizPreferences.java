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
package com.raytheon.uf.viz.alertviz;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Preference object for AlertViz
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 5, 2015            mschenke     Initial creation
 *
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AlertVizPreferences extends AbstractPreferenceInitializer {

    private static final IPreferenceStore prefStore = Activator.getDefault()
            .getPreferenceStore();

    public static final String P_PORT = "port";

    public static final String P_ENABLED = "enabled";

    public static IPreferenceStore getPreferenceStore() {
        return prefStore;
    }

    public static int getAlertVizPort() {
        return prefStore.getInt(P_PORT);
    }

    public static boolean isAlertVizEnabled() {
        return prefStore.getBoolean(P_ENABLED);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
     * initializeDefaultPreferences()
     */
    @Override
    public void initializeDefaultPreferences() {
        prefStore.setDefault(P_PORT, 61998);
        prefStore.setDefault(P_ENABLED, true);
    }
}
