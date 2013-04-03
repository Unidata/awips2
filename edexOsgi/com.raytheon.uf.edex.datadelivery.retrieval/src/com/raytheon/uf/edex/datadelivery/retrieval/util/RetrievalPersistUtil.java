package com.raytheon.uf.edex.datadelivery.retrieval.util;

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

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.mapping.PluginRoute;
import com.raytheon.uf.edex.datadelivery.retrieval.mapping.PluginRouteList;

/**
 * Common class for persistence of plugins
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 26, 2012  1367         dhladky     Common plugin route persistance
 * 
 * </pre>
 * 
 * @version 1.0
 */

public final class RetrievalPersistUtil {

    private final String OVERRIDE_PATH = "mapping" + File.separatorChar
            + "pluginRoutes.xml";

    private static JAXBManager jaxb;

    private final Map<String, String> overrideDestinationUris = new HashMap<String, String>();

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalPersistUtil.class);

    /** Singleton instance of this class */
    private static final RetrievalPersistUtil instance = new RetrievalPersistUtil();

    /* Private Constructor */
    private RetrievalPersistUtil() {
        populateOverrides();
    }

    /**
     * Sends the pdos to the default persist or post processing/persistence
     * route
     * 
     * @param defaultRoute
     * @param pluginName
     * @param pdos
     * @return
     */
    public static synchronized boolean routePlugin(String defaultPeristRoute,
            String pluginName,
            PluginDataObject[] pdos) {

        boolean success = false;

        if (instance.overrideDestinationUris.containsKey(pluginName)) {
            // path if the plugin needs special handling
            try {
                EDEXUtil.getMessageProducer().sendAsyncUri(
                        instance.overrideDestinationUris.get(pluginName), pdos);
                success = true;
            } catch (EdexException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else {
            // general path of route output
            try {
                PluginFactory factory = PluginFactory.getInstance();
                PluginDao pluginDao = factory.getPluginDao(pluginName);
                EDEXUtil.checkPersistenceTimes(pdos);
                StorageStatus status = pluginDao.persistToHDF5(pdos);

                if (status.getExceptions().length > 0) {
                    statusHandler.error(
                            "Exceptions found during persisting HDF5: ",
                            status.getExceptions()[0]);
                } else {

                    pluginDao.persistToDatabase(pdos);
                    EDEXUtil.getMessageProducer().sendAsyncUri(
                            defaultPeristRoute,
                            pdos);
                    success = true;
                }
            } catch (Exception e) {
                statusHandler.error("Could not store " + pluginName
                        + " records...", e);
            }
        }

        return success;
    }

    /**
     * Populate the override route map for plugins
     */
    private void populateOverrides() {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lf = null;
        Map<LocalizationLevel, LocalizationFile> files = pm
                .getTieredLocalizationFile(LocalizationType.EDEX_STATIC,
                        OVERRIDE_PATH);

        if (files.containsKey(LocalizationLevel.SITE)) {
            lf = files.get(LocalizationLevel.SITE);
        } else {
            lf = files.get(LocalizationLevel.BASE);
        }

        if (lf != null) {
            File file = lf.getFile();

            if (!file.exists()) {
                statusHandler
                        .warn("[Data Delivery] Configuration for plugin routes: "
                                + file.getAbsolutePath() + " does not exist.");
            } else {

                PluginRouteList prl = null;

                try {
                    prl = (PluginRouteList) getJaxbManager()
                            .jaxbUnmarshalFromXmlFile(file);
                } catch (Exception e) {
                    statusHandler.error(
                            "[Data Delivery] Configuration for plugin routes failed to load: File: "
                                    + file.getAbsolutePath(), e);
                }

                // construct the map
                if (prl != null) {
                    for (PluginRoute pr : prl.getPluginRoute()) {
                        overrideDestinationUris
                                .put(pr.getName(), pr.getValue());
                    }
                }
            }
        }
    }

    /**
     * get the JAXBManager to serialize the files
     * 
     * @return
     */
    private JAXBManager getJaxbManager() {

        if (jaxb == null) {
            try {
                jaxb = new JAXBManager(PluginRoute.class, PluginRouteList.class);
            } catch (JAXBException e) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Unable to create JAXBManager for the Plugin and PLuginRoute classes.",
                                e);
            }
        }

        return jaxb;
    }
}
