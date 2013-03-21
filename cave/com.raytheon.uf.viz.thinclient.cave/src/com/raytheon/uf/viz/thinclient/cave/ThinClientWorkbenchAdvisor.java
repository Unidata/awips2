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
package com.raytheon.uf.viz.thinclient.cave;

import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.thinclient.cave.preferences.DynamicPreferenceNode;
import com.raytheon.uf.viz.thinclient.cave.preferences.DynamicPreferenceNode.IPreferencePageFactory;
import com.raytheon.uf.viz.thinclient.cave.preferences.ThinClientCachePreferences;
import com.raytheon.uf.viz.thinclient.cave.preferences.ThinClientConnectionPreferences;
import com.raytheon.uf.viz.thinclient.cave.preferences.ThinClientPreferencePage;
import com.raytheon.uf.viz.thinclient.cave.preferences.ThinClientServerPreferences;
import com.raytheon.viz.ui.personalities.awips.VizWorkbenchAdvisor;

/**
 * Thin Client workbench advisor, adds the thin client preference page
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2011            mschenke    Initial creation
 * Mar 20, 2013       1638 mschenke    Removed overriding of createWorkbenchWindowAdvisor as not needed
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ThinClientWorkbenchAdvisor extends VizWorkbenchAdvisor {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.personalities.awips.VizWorkbenchAdvisor#
     * customizeAppearance()
     */
    @Override
    protected void customizeAppearance() {
        super.customizeAppearance();
        // Disable warngen (May be preference based at some point)
        // Should we have a file that specifies plugins to disable?
        // try {
        // for (Bundle b : Activator.getDefault().getContext().getBundles()) {
        // if ("com.raytheon.viz.warngen".equals(b.getSymbolicName())) {
        // b.uninstall();
        // break;
        // }
        // }
        // } catch (Throwable t) {
        // t.printStackTrace();
        // }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.VizWorkbenchAdvisor#postStartup()
     */
    @Override
    public void postStartup() {
        super.postStartup();
        // Add the ThinClientPreferencePage as a root item to the preference
        // manager only when launching in thin client mode
        PreferenceManager pm = PlatformUI.getWorkbench().getPreferenceManager();
        pm.addToRoot(new DynamicPreferenceNode(
                ThinClientPreferencePage.PAGE_ID, "Thin Client",
                new IPreferencePageFactory() {
                    @Override
                    public IPreferencePage createNewPage() {
                        return new ThinClientPreferencePage();
                    }
                }));

        pm.addTo(ThinClientPreferencePage.PAGE_ID, new DynamicPreferenceNode(
                ThinClientPreferencePage.PAGE_ID + ".ServerPreferences",
                "Servers", new IPreferencePageFactory() {
                    @Override
                    public IPreferencePage createNewPage() {
                        return new ThinClientServerPreferences();
                    }
                }));

        pm.addTo(ThinClientPreferencePage.PAGE_ID, new DynamicPreferenceNode(
                ThinClientPreferencePage.PAGE_ID + ".CachePreferences",
                "Caches", new IPreferencePageFactory() {
                    @Override
                    public IPreferencePage createNewPage() {
                        return new ThinClientCachePreferences();
                    }
                }));

        pm.addTo(ThinClientPreferencePage.PAGE_ID, new DynamicPreferenceNode(
                ThinClientPreferencePage.PAGE_ID + ".ConnectionPreferences",
                "Connections", new IPreferencePageFactory() {
                    @Override
                    public IPreferencePage createNewPage() {
                        return new ThinClientConnectionPreferences();
                    }
                }));
    }
}
