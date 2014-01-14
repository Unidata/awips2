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
package com.raytheon.uf.viz.collaboration.ui.prefs;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.jivesoftware.smack.packet.Presence.Mode;

import com.raytheon.uf.viz.collaboration.ui.Activator;

/**
 * Initialization for collaboration preference store properties
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2012            njensen     Initial creation
 * Jan 14, 2014 2630       bclement    added away on idle defaults
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
public class CollabPrefInitializer extends AbstractPreferenceInitializer {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
     * initializeDefaultPreferences()
     */
    @Override
    public void initializeDefaultPreferences() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();

        store.setDefault(CollabPrefConstants.P_SERVER, "");
        store.setDefault(CollabPrefConstants.AUTO_JOIN, true);

        // TODO better default?
        store.setDefault(CollabPrefConstants.P_USERNAME,
                System.getProperty("user.name"));

        store.setDefault(CollabPrefConstants.P_STATUS,
                Mode.available.toString());
        store.setDefault(CollabPrefConstants.P_MESSAGE, "");
        store.setDefault(CollabPrefConstants.AWAY_ON_IDLE, true);
        store.setDefault(CollabPrefConstants.AWAY_TIMEOUT,
                CollabPrefConstants.AWAY_TIMEOUT_DEFAULT);
    }

}
