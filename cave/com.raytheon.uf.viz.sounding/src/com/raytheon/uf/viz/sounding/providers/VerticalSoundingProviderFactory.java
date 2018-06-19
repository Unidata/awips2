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
package com.raytheon.uf.viz.sounding.providers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.sounding.Activator;

/**
 * Factory for constructing {@link IVerticalSoundingProvider}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VerticalSoundingProviderFactory {

    private static final String VERTICAL_SOUNDING_ADAPTER_EXTENSION = Activator.PLUGIN_ID
            + ".verticalSoundingProvider";

    private static final String TYPE_EXTENSION_ATTR_ID = "type";

    private static final String CLASS_EXTENSION_ATTR_ID = "class";

    private static Map<String, IConfigurationElement> extensionMap = new HashMap<String, IConfigurationElement>();

    static {
        // Internally uses extension point to look up available providers
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry != null) {
            IExtensionPoint point = registry
                    .getExtensionPoint(VERTICAL_SOUNDING_ADAPTER_EXTENSION);
            if (point != null) {
                IExtension[] extensions = point.getExtensions();

                for (IExtension ext : extensions) {
                    IConfigurationElement[] config = ext
                            .getConfigurationElements();
                    for (IConfigurationElement cfg : config) {
                        String type = cfg.getAttribute(TYPE_EXTENSION_ATTR_ID);
                        if (extensionMap.containsKey(type) == false
                                && cfg.getAttribute(CLASS_EXTENSION_ATTR_ID) != null) {
                            extensionMap.put(type, cfg);
                        }
                    }
                }
            }
        }
    }

    /**
     * Gets an {@link IVerticalSoundingProvider} for the type/constraints. Will
     * return null if none available for the type
     * 
     * @param type
     * @param soundingConstraints
     * @return
     */
    public static IVerticalSoundingProvider getVerticalSoundingProvider(
            String type, Map<String, RequestConstraint> soundingConstraints) {
        AbstractVerticalSoundingProvider<?> provider = null;
        IConfigurationElement element = extensionMap.get(type);
        if (element != null) {
            try {
                provider = (AbstractVerticalSoundingProvider<?>) element
                        .createExecutableExtension(CLASS_EXTENSION_ATTR_ID);
                if (soundingConstraints != null) {
                    provider.setConstraints(new HashMap<String, RequestConstraint>(
                            soundingConstraints));
                }
            } catch (CoreException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        } else {
            Activator.statusHandler.handle(Priority.INFO,
                    "No sounding provider registered for type: " + type);
        }
        return provider;
    }

    /**
     * Checks if an {@link IVerticalSoundingProvider} is registered for the
     * specified sounding type
     * 
     * @param type
     * @return
     */
    public static boolean hasProviderForType(String type) {
        return extensionMap.containsKey(type);
    }
}
