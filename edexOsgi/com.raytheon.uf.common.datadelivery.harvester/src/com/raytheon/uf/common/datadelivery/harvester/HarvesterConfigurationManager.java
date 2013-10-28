package com.raytheon.uf.common.datadelivery.harvester;

import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class HarvesterConfigurationManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HarvesterConfigurationManager.class);

    private static final SingleTypeJAXBManager<HarvesterConfig> jaxb = SingleTypeJAXBManager
            .createWithoutException(HarvesterConfig.class);

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

            try {
                config = jaxb.unmarshalFromXmlFile(lf.getFile());
            } catch (SerializationException e) {
                statusHandler.handle(Priority.ERROR,
                        "Can't deserialize harvester config at "
                                + lf.getFile().getPath(), e);
            }

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

            try {
                config = jaxb.unmarshalFromXmlFile(lf.getFile());
            } catch (SerializationException e) {
                statusHandler.handle(Priority.ERROR,
                        "Can't deserialize harvester config at "
                                + lf.getFile().getPath(), e);
            }

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

}
