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
package com.raytheon.uf.viz.core.procedures;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
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

public class AlterBundleFactory {

    private static final String EXTENSION_ID = "com.raytheon.uf.viz.core.alterBundle";

    private static IAlterBundleContributor[] contributors = null;

    public static IAlterBundleContributor[] getContributors() {
        if (contributors == null) {
            List<IConfigurationElement> configs = new ArrayList<IConfigurationElement>();
            // Construct the resource mapping from Eclipse plugins
            IExtensionRegistry registry = Platform.getExtensionRegistry();
            IExtensionPoint point = registry.getExtensionPoint(EXTENSION_ID);
            if (point != null) {
                IExtension[] extensions = point.getExtensions();
                for (int i = 0; i < extensions.length; i++) {
                    IConfigurationElement[] config = extensions[i]
                            .getConfigurationElements();

                    for (int j = 0; j < config.length; j++) {
                        configs.add(config[j]);
                    }
                }
            }

            contributors = new IAlterBundleContributor[configs.size()];
            for (int i = 0; i < configs.size(); ++i) {
                try {
                    contributors[i] = (IAlterBundleContributor) configs.get(i)
                            .createExecutableExtension("class");
                } catch (CoreException e) {
                    UFStatus.getHandler().handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return contributors;
    }

}
