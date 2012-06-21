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

package com.raytheon.uf.viz.core.catalog;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import com.raytheon.edex.scriptfactory.ScriptFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Creates uEngine scripts on the fly.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    11/25/06                  brockwoo    Initial Creation.
 *    12/06/06      102         brockwoo    Fix for contour script generation error.
 *    10/2/2007     459         grichard    Added createScript method.
 *    10/8/2007     459         grichard    Changed createScript to createQueryScript.
 *    10/8/2007     459         grichard    Added createUpdateScript method.
 *    10/12/2007    482         grichard    Reformatted file.
 *    12/17/2007    639         grichard    Added &quot;fxa&quot; parm to scripts.
 *    3/17/2008     933         grichard    Added support for taf plugin.
 *    04/14/2008                chammack    Complete refactor to Velocity
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
public class ScriptCreator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScriptCreator.class);

    private static final String DEFAULT_SCRIPT_LIBRARY = "BaseRequest";

    /** The extension point where resources are defined */
    private static final String RESOURCE_EXTENSION = "com.raytheon.uf.viz.core.scriptTemplate";

    private static final String PLUGINNAME_TAG = "pluginName";

    private static final String SCRIPT_TEMPLATE_TAG = "scriptTemplateFile";

    private static final String SCRIPT_LIBRARY_TAG = "scriptLibrary";

    private static Map<String, ScriptProperties> pluginToLibraryMap;

    private static File DEFAULT_TEMPLATE;

    static {
        try {
            DEFAULT_TEMPLATE = new File(FileLocator.resolve(
                    FileLocator.find(Activator.getDefault().getBundle(),
                            new Path("scriptTemplates/standardTemplate.vm"),
                            null)).getPath());
        } catch (IOException e) {
            statusHandler
                    .handle(Priority.CRITICAL,
                            "Unable to load the standard script template.  Requesting products will not work until this is fixed.",
                            e);
        }
    }

    private static class ScriptProperties {
        public File scriptTemplate;

        public String scriptLibrary;

        @Override
        public boolean equals(Object obj) {
            if (obj == null || !(obj instanceof ScriptProperties)) {
                return false;
            }

            ScriptProperties props = (ScriptProperties) obj;

            boolean part1 = (scriptTemplate == null && props.scriptTemplate == null)
                    || (scriptTemplate != null && scriptTemplate
                            .equals(props.scriptTemplate));

            boolean part2 = (scriptLibrary == null && props.scriptLibrary == null)
                    || (scriptLibrary != null && scriptLibrary
                            .equals(props.scriptLibrary));

            return part1 && part2;

        }

        @Override
        public String toString() {
            return "(Template: " + scriptTemplate + ", Library: "
                    + scriptLibrary + ")";
        }

    }

    /**
     * Create an internal representation of script properties extensions in
     * memory
     */
    private static synchronized void createPluginToLibraryMap() {
        if (pluginToLibraryMap != null) {
            return;
        }

        pluginToLibraryMap = new HashMap<String, ScriptProperties>();
        IExtensionRegistry registry = Platform.getExtensionRegistry();

        IExtensionPoint point = registry.getExtensionPoint(RESOURCE_EXTENSION);

        if (point == null) {
            return;
        }
        IExtension[] extensions = point.getExtensions();

        List<IExtension> extensionList = new ArrayList<IExtension>();
        extensionList.addAll(Arrays.asList(extensions));

        for (int i = 0; i < extensionList.size(); i++) {
            IConfigurationElement[] config = extensionList.get(i)
                    .getConfigurationElements();

            for (int j = 0; j < config.length; j++) {
                String pluginName = config[j].getAttribute(PLUGINNAME_TAG);
                String template = config[j].getAttribute(SCRIPT_TEMPLATE_TAG);
                String library = config[j].getAttribute(SCRIPT_LIBRARY_TAG);
                IContributor contrib = extensionList.get(i).getContributor();
                Bundle bundle = Platform.getBundle(contrib.getName());
                File templateFile = null;
                try {
                    if (template != null) {
                        URL url = FileLocator.find(bundle, new Path(template),
                                null);
                        if (url == null) {
                            String message = "Error opening the script template for: "
                                    + pluginName;
                            statusHandler.handle(Priority.PROBLEM, message);
                            continue;
                        }

                        templateFile = new File(FileLocator.resolve(url)
                                .getPath());
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error opening resource", e);
                    continue;
                }

                if (pluginName != null) {
                    ScriptProperties newValue = new ScriptProperties();
                    newValue.scriptLibrary = library;
                    newValue.scriptTemplate = templateFile;

                    ScriptProperties existingValue = pluginToLibraryMap
                            .get(pluginName);
                    if (existingValue != null) {
                        // If duplicates, warn to log, and use the first value.
                        statusHandler.handle(Priority.PROBLEM,
                                "Duplicate value for plugin registry: "
                                        + pluginName + " had value: "
                                        + existingValue + ", new value: "
                                        + newValue + ".  Ignoring new value.");

                    } else {
                        pluginToLibraryMap.put(pluginName, newValue);
                    }

                }

            }
        }

    }

    /**
     * Create a script from a LayerProperty class, with mode "select"
     * 
     * This is a convenience method for the more general
     * createScript(LayerProperty, String).
     * 
     * @param layerProperty
     *            the layer property
     * @return the script object
     * @throws VizException
     *             an exception if generation fails.
     */
    public static String createScript(LayerProperty layerProperty)
            throws VizException {
        return createScript(layerProperty, "select");
    }

    /**
     * Create a script from a LayerProperty class with a mode
     * 
     * The mode is a user definable concept, but typically "select", "catalog"
     * and "update" are valid values, however script templates may implement
     * anything they choose to.
     * 
     * @param layerProperty
     *            the layer property
     * @param mode
     *            the mode
     * @return the script
     * @throws VizException
     *             an exception if generation fails.
     */
    public static String createScript(LayerProperty layerProperty, String mode)
            throws VizException {
        if (mode.equals("update") || mode.equals("catalog")
                || mode.equals("plot")) {
            return createScript(layerProperty.getEntryQueryParameters(false),
                    layerProperty.getNumberOfImages(), mode);
        }
        return createScript(layerProperty.getEntryQueryParameters(true),
                layerProperty.getNumberOfImages(), mode);
    }

    /**
     * Create a script from a LayerProperty class, with mode "update"
     * 
     * This is a convenience method for the more general
     * createImageScript(LayerProperty, String).
     * 
     * @param layerProperty
     *            the layer property
     * @return the script object
     * @throws VizException
     *             an exception if generation fails.
     */
    public static String createUpdateScript(LayerProperty layerProperty)
            throws VizException {
        return createScript(layerProperty, "update");
    }

    /**
     * Create a script from metadata
     * 
     * 
     * @param queryTerms
     *            the query terms
     * @param maxRecords
     *            the maximum number of records
     * @param mode
     *            the mode for the script
     * @return the script
     * @throws VizException
     * 
     */
    public static String createScript(
            Map<String, RequestConstraint> queryTerms, int maxRecords,
            String mode) throws VizException {

        String plugin = queryTerms.get("pluginName").getConstraintValue();

        if (plugin == null) {
            throw new IllegalArgumentException(
                    "Metadata does not contain pluginName, which is required.");
        }

        return createScript(plugin, queryTerms, maxRecords, mode);

    }

    public static String createScript(String templateName,
            Map<String, RequestConstraint> queryTerms, int maxRecords,
            String mode) throws VizException {

        // long t0 = System.currentTimeMillis();

        createPluginToLibraryMap();

        if (templateName == null) {
            throw new IllegalArgumentException(
                    "No templateName, which is required.");
        }

        ScriptProperties props = pluginToLibraryMap.get(templateName);

        if (props == null) {
            props = new ScriptProperties();
            props.scriptLibrary = DEFAULT_SCRIPT_LIBRARY;
        }

        File stringTemplate = props.scriptTemplate;
        if (stringTemplate == null) {
            stringTemplate = DEFAULT_TEMPLATE;
        }

        try {
            String script = ScriptFactory.getInstance().createScript(
                    stringTemplate, DEFAULT_TEMPLATE.getParentFile(),
                    maxRecords, mode, props.scriptLibrary, queryTerms,
                    FileUtil.join(LocalizationManager.getUserDir(), "logs"));
            // System.out.println("Script gen: "
            // + (System.currentTimeMillis() - t0));
            return script;
        } catch (Exception e) {
            throw new VizException(e);
        }

    }
}
