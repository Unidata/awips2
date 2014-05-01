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
package com.raytheon.uf.viz.thinclient.preferences;

import java.io.File;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.thinclient.Activator;

/**
 * Initializes defaults for thin client preferences
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PreferenceInitializer extends AbstractPreferenceInitializer {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
     * initializeDefaultPreferences()
     */
    @Override
    public void initializeDefaultPreferences() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        // Set Defaults for cache directory and cache settings
        store.setDefault(ThinClientPreferenceConstants.P_CACHE_DIR, new File(
                LocalizationManager.getUserDir(), "cache").getAbsolutePath());
        store.setDefault(ThinClientPreferenceConstants.P_CACHE_WEATHER, true);
        store.setDefault(ThinClientPreferenceConstants.P_CACHE_LOCALIZATION,
                true);
        store.setDefault(
                ThinClientPreferenceConstants.P_DISABLE_REMOTE_LOCALIZATION,
                false);
        store.setDefault(ThinClientPreferenceConstants.P_CACHE_MAPS, true);

        // By default keep jms enabled
        store.setDefault(ThinClientPreferenceConstants.P_DISABLE_JMS, false);

        store.setDefault(
                ThinClientPreferenceConstants.P_ENABLE_REQUEST_COMPRESSION,
                true);

        // Menu times will be enabled by default
        store.setDefault(ThinClientPreferenceConstants.P_DISABLE_MENU_TIMES,
                false);
        // Default menu refresh interval
        store.setDefault(
                ThinClientPreferenceConstants.P_MENU_TIME_REFRESH_INTERVAL, 5);

        // Default data refresh rate
        store.setDefault(ThinClientPreferenceConstants.P_DATA_REFRESH_INTERVAL,
                30);

        // By default, no proxy used
        store.setDefault(ThinClientPreferenceConstants.P_USE_PROXIES, false);
    }
}
