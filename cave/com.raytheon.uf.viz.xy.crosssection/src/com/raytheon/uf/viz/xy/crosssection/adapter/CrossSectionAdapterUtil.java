/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy.crosssection.adapter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Utilities for cross section adapters.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Extracted from CrossSectionResourceData and updated
 *                                     to support regex constraints and prioritize
 *                                     configurations with constraints
 *
 * </pre>
 *
 * @author mapeters
 */
public class CrossSectionAdapterUtil {

    private static final String CROSS_SECTION_ADAPTER_EXTENSION = "com.raytheon.uf.viz.xy.crosssection.crosssectionadapter";

    private static final String ADAPTER = "adapter";

    protected static final String CLASS = "class";

    protected static final String CONSTRAINT = "constraint";

    protected static final String KEY = "key";

    protected static final String VALUE = "value";

    protected static boolean adapterConfigMatchesPdo(IConfigurationElement cfg,
            PluginDataObject object, Map<String, Object> uriFields) {
        boolean useAdapter = false;
        String targetClass = cfg.getAttribute(CLASS);
        for (Class<?> clazz : object.getClass().getInterfaces()) {
            if (clazz.getName().equals(targetClass)) {
                useAdapter = true;
                break;
            }
        }
        if (!useAdapter) {
            for (Class<?> clazz = object
                    .getClass(); clazz != PluginDataObject.class; clazz = clazz
                            .getSuperclass()) {
                if (clazz.getName().equals(targetClass)) {
                    useAdapter = true;
                    break;
                }
            }
        }

        IConfigurationElement[] constraints = cfg.getChildren(CONSTRAINT);
        for (IConfigurationElement constraint : constraints) {
            Object value = uriFields.get(constraint.getAttribute(KEY));
            if (value == null) {
                value = "null";
            }
            if (!Pattern.matches(constraint.getAttribute(VALUE),
                    value.toString())) {
                useAdapter = false;
                break;
            }
        }

        return useAdapter;
    }

    protected static IConfigurationElement selectConfig(
            List<IConfigurationElement> matchingCfgs) {
        IConfigurationElement cfgToUse = null;
        if (matchingCfgs.size() == 1) {
            cfgToUse = matchingCfgs.get(0);
        } else if (matchingCfgs.size() > 1) {
            // Prioritize config with constraints
            for (IConfigurationElement cfg : matchingCfgs) {
                if (cfg.getChildren(CONSTRAINT).length > 0) {
                    if (cfgToUse == null) {
                        cfgToUse = cfg;
                    } else {
                        /*
                         * Already found another config with constraints, so
                         * which one to use is ambiguous
                         */
                        cfgToUse = null;
                        break;
                    }
                }
            }
        }

        return cfgToUse;
    }

    /**
     * Get the adapter to use based on the given sample PDO.
     *
     * @param object
     *            sample PDO
     * @return the adapter
     * @throws VizException
     */
    public static AbstractCrossSectionAdapter<?> getAdapter(
            PluginDataObject object) throws VizException {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry == null) {
            throw new VizException("Error loading ExtensionRegistry");
        }
        IExtensionPoint point = registry
                .getExtensionPoint(CROSS_SECTION_ADAPTER_EXTENSION);
        if (point == null) {
            throw new VizException(
                    "Error loading Extension points for Cross Section Adapters");
        }
        Map<String, Object> uriFields = RecordFactory.getInstance()
                .loadMapFromUri(object.getDataURI());
        IExtension[] extensions = point.getExtensions();

        List<IConfigurationElement> matchingCfgs = new ArrayList<>();
        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();

            for (IConfigurationElement cfg : config) {
                if (adapterConfigMatchesPdo(cfg, object, uriFields)) {
                    matchingCfgs.add(cfg);
                }
            }
        }
        IConfigurationElement cfgToUse = selectConfig(matchingCfgs);

        if (cfgToUse != null) {
            try {
                return (AbstractCrossSectionAdapter<?>) cfgToUse
                        .createExecutableExtension(ADAPTER);
            } catch (CoreException e) {
                throw new VizException(
                        "Error constructing Cross Section adapter", e);
            }

        }

        throw new VizException(
                "Error determining Cross Section adapter to use for: "
                        + object.getClass().getSimpleName());
    }
}
