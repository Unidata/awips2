package com.raytheon.uf.common.datadelivery.harvester;

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
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Manages harvester configurations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----              dhladky     Initial creation
 * Oct 23, 2013 2361       njensen     Use JAXBManager for XML
 * Oct 28, 2013 2361       dhladky     Fixed up JAXBManager.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class HarvesterConfigurationManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HarvesterConfigurationManager.class);
    
    private static final Class<?>[] clazzess = new Class<?>[] {
        HarvesterConfig.class, Provider.class, Connection.class,
        ProviderType.class, ServiceType.class, Agent.class,
        CrawlAgent.class, OGCAgent.class, ConfigLayer.class };

    private static JAXBManager jaxb = null;
    
    /**
     * marshall and unmarshall harvester objects
     * 
     * @return
     */
    private static JAXBManager getJaxb() {

        if (jaxb == null) {
            try {
                jaxb = new JAXBManager(clazzess);

            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return jaxb;
    }

    /**
     * Gets site and base level configs for harvesters
     * 
     * @return
     */
    public static List<LocalizationFile> getLocalizedFiles() {
        // first get the Localization directory and find all harvester
        // configs
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile[] files = pm.listStaticFiles("datadelivery/harvester",
                new String[] { "xml" }, false, true);

        return Arrays.asList(files);
    }

    /**
     * Get the crawler configuration
     * 
     * @return
     */
    public static HarvesterConfig getCrawlerConfiguration() {

        HarvesterConfig config = null;

        for (LocalizationFile lf : getLocalizedFiles()) {

            config = getHarvesterFile(lf.getFile());

            if (config != null) {
                Agent agent = config.getAgent();

                if (agent instanceof CrawlAgent) {
                    return config;
                }
            }
        }

        return config;
    }

    /**
     * Get the OGC configuration
     * 
     * @return
     */
    public static HarvesterConfig getOGCConfiguration() {

        HarvesterConfig config = null;

        for (LocalizationFile lf : getLocalizedFiles()) {

            config = getHarvesterFile(lf.getFile());

            if (config != null) {
                Agent agent = config.getAgent();

                if (agent instanceof OGCAgent) {
                    return config;
                } else {
                    config = null;
                }
            }
        }

        return config;
    }

    /**
     * Get this harvester configuration File
     * 
     * @param file
     * @return
     */
    public static HarvesterConfig getHarvesterFile(File file) {

        HarvesterConfig config = null;

        try {
            config = getJaxb()
                    .unmarshalFromXmlFile(HarvesterConfig.class, file);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Can't deserialize harvester config at " + file.getPath(),
                    e);
        }

        return config;

    }

    /**
     * Writes the harvester config files
     * 
     * @param config
     * @param file
     */
    public static void setHarvesterFile(HarvesterConfig config, File file) {
        try {
            getJaxb().marshalToXmlFile(config, file.getAbsolutePath());
        } catch (SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't write Harvester Config file.", e);
        }
    }

}
