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
package com.raytheon.viz.volumebrowser.vbui;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Observes changes to Volume Browser configuration files and notifies the
 * Volume Browser dialog that updates are available when a change is detected.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 07, 2017 6355       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */

public class VolumeBrowserConfigObserver implements ILocalizationPathObserver {

    private static final String EXTENSION_POINT_ID = "com.raytheon.viz.volumebrowser.config.path";

    private static final VolumeBrowserConfigObserver instance = new VolumeBrowserConfigObserver();

    private VolumeBrowserConfigObserver() {
        super();
        loadFromExtensionPoint();
    }

    public static synchronized VolumeBrowserConfigObserver getInstance() {
        return instance;
    }

    private void loadFromExtensionPoint() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(EXTENSION_POINT_ID);
        IExtension[] extensions = point.getExtensions();

        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();
            for (IConfigurationElement cfg : config) {
                String path = cfg.getValue();
                if (path != null && !path.isEmpty()) {
                    observePaths(path);
                }
            }
        }
    }

    /**
     * Register paths for observance.
     *
     * @param paths
     */
    public void observePaths(String... paths) {
        for (String path : paths) {
            PathManagerFactory.getPathManager()
                    .addLocalizationPathObserver(path, this);
        }
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        VolumeBrowserDlg dlg = VolumeBrowserAction.getVolumeBrowserDlg();
        if (dlg != null) {
            dlg.updatesAvailable();
        }
    }
}
