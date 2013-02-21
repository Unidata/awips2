package com.raytheon.uf.common.datadelivery.retrieval.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.EnumMap;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Regex manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2012   1163     dhladky     Initial creation
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HarvesterServiceManager implements ILocalizationFileObserver {

    /** Path to Prefix config. */
    private static final String CONFIG_FILE_NAME_PREFIX = "datadelivery"
            + File.separatorChar;

    /** Path to suffix config. */
    private static final String CONFIG_FILE_NAME_SUFFIX = "ServiceConfig.xml";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HarvesterServiceManager.class);

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     * @throws FileNotFoundException
     */
    public static HarvesterServiceManager getInstance() {
        return instance;
    }

    private final Map<ServiceType, ServiceConfig> services = new EnumMap<ServiceType, ServiceConfig>(
            ServiceType.class);

    /** Singleton instance of this class */
    private static final HarvesterServiceManager instance = new HarvesterServiceManager();

    /* Private Constructor */
    private HarvesterServiceManager() {

        try {
            readConfigXml();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Error reading config files: ", e);
        }

    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {

        for (ServiceType serviceType : services.keySet()) {
            if (message.getFileName().equals(getConfigFileName(serviceType))) {
                try {
                    readConfigXml();
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Serialization error reading file: ", e);
                }
            }
        }
    }

    /**
     * Gets the resultant configFileName
     * 
     * @param serviceType
     * @return
     */
    public String getConfigFileName(ServiceType serviceType) {
        return CONFIG_FILE_NAME_PREFIX + serviceType + CONFIG_FILE_NAME_SUFFIX;
    }

    /**
     * Get the Service Configuration
     * 
     * @param serviceType
     * @return
     */
    public ServiceConfig getServiceConfig(ServiceType serviceType) {

        ServiceConfig config = null;

        if (services.containsKey(serviceType)) {
            config = services.get(serviceType);
        } else {
            // try and load it
            try {
                readConfigXml();
                config = services.get(serviceType);
                // If you haven't found it by now, your just out of luck
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        return config;
    }

    /**
     * Read the XML configuration data for the current XML files.
     */
    public void readConfigXml() throws Exception {

        for (ServiceType st : ServiceType.values()) {

            LocalizationFile lf = null;
            IPathManager pm = PathManagerFactory.getPathManager();
            Map<LocalizationLevel, LocalizationFile> files = pm
                    .getTieredLocalizationFile(LocalizationType.COMMON_STATIC,
                            getConfigFileName(st));

            if (files.containsKey(LocalizationLevel.SITE)) {
                lf = files.get(LocalizationLevel.SITE);
            } else {
                lf = files.get(LocalizationLevel.BASE);
            }

            if (lf != null) {
                File file = lf.getFile();
                // System.out.println("Reading -- " + file.getAbsolutePath());
                if (!file.exists()) {
                    statusHandler
                            .warn("[Data Delivery] Configuration for service: "
                                    + file.getAbsolutePath()
                                    + " does not exist.");
                    continue;
                }

                ServiceConfig sc = null;

                try {
                    sc = readServiceConfigXml(file);
                    services.put(st, sc);
                } catch (Exception e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Couldn't read the Service Config file: "
                                    + file.getAbsolutePath());
                }
            } else {
                // We might not be implementing a particular service so we only
                // care about it if we are
                // debugging.
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.handle(
                            Priority.WARN,
                            "No Service Config file for service: " + st
                                    + " exists!");
                }
            }
        }
    }

    /**
     * Read in service config XML
     * 
     * @param file
     * @return
     */
    private ServiceConfig readServiceConfigXml(File file) throws Exception {

        ServiceConfig service = null;

        if (file != null) {
            service = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    ServiceConfig.class, file);
        }

        return service;
    }

}
