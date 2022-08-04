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
package com.raytheon.viz.pointdata.def;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * PlotParameterDefinitionsManager object
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 10/10/2019   71272      Mark Peters   Initial Creation
 * 10/13/2020   73084      ksunil        Code to handle .svg files under burfmos subdir
 *
 * </pre>
 *
 * @author mpeters
 */

public class PlotParameterDefinitionsManager {

    private static final PlotParameterDefinitionsManager instance = new PlotParameterDefinitionsManager();

    private static final String PLOT_MODELS_DIR = "plotModels";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final Map<String, PlotParameterDefinitions> pluginParamDefsMap = new HashMap<>();

    private final SingleTypeJAXBManager<PlotParameterDefinitions> jaxb = SingleTypeJAXBManager
            .createWithoutException(PlotParameterDefinitions.class);

    private final Object lock = new Object();

    private PlotParameterDefinitionsManager() {
    }

    public static PlotParameterDefinitionsManager getInstance() {
        return instance;
    }

    public PlotParameterDefinitions getDefinitions(String plugin) {
        /*
         * The case of an old style .svg file where a plugin is not specified is
         * caught way upstream, in EditPlotResourceAction.
         */
        PlotParameterDefinitions defs;
        synchronized (lock) {
            defs = pluginParamDefsMap.get(plugin);
            if (defs == null) {
                String path = getDefinitionsPath(plugin);
                ILocalizationFile lFile = PathManagerFactory.getPathManager()
                        .getStaticLocalizationFile(path);

                try (InputStream is = lFile.openInputStream()) {
                    defs = jaxb.unmarshalFromInputStream(is);
                } catch (IOException | LocalizationException
                        | SerializationException e) {
                    statusHandler
                            .error("Error reading plot parameter definitions file: "
                                    + path, e);
                }

                if (defs != null) {
                    pluginParamDefsMap.put(plugin, defs);
                    // Or should I add the defs themselves as the observers and
                    // they reload their contents if their file changes? Would
                    // prevent stale references...
                    PathManagerFactory.getPathManager()
                            .addLocalizationPathObserver(path,
                                    new ILocalizationPathObserver() {

                                        @Override
                                        public void fileChanged(
                                                ILocalizationFile file) {
                                            synchronized (pluginParamDefsMap) {
                                                pluginParamDefsMap
                                                        .remove(plugin);
                                            }
                                        }
                                    });
                }
            }
        }

        return defs;
    }

    public String getDefinitionsPath(String plugin) {
        // The plugin could be a simple "obs" or a "bufrmos/GFSbufrmos" for
        // example.
        String pluginStripped = plugin.contains("/")
                ? plugin.substring(plugin.lastIndexOf("/") + 1) : plugin;

        return LocalizationUtil.join(PLOT_MODELS_DIR, plugin,
                "plotParameters_" + pluginStripped + ".xml");
    }
}
