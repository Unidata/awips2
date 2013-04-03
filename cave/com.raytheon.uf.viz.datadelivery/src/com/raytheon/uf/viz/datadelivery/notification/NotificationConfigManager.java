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
package com.raytheon.uf.viz.datadelivery.notification;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.notification.xml.NotificationConfigXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.NotificationFilterXML;

/**
 * Notification Configuration Manager
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2012            mpduff     Initial creation
 * Feb 23, 2012    418     jpiatt     Added current file save.
 * Aug 15, 2012    430     jpiatt     Added reRead method.
 * Oct 22, 2012   1284     mpduff     Code Cleanup.
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class NotificationConfigManager {

    /** Status handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotificationConfigManager.class);

    /** Configuration Manager instance */
    private static NotificationConfigManager instance = new NotificationConfigManager();

    /** Configuration XML File */
    private final String CONFIG_XML_FILE = "notificationManagerConfig"
            + File.separator + "NotificationConfig.xml";

    /** Configuration XML File */
    private final String FILTER_XML_FILE = "notificationManagerConfig"
            + File.separator + "NotificationFilter.xml";

    /** Configuration XML File */
    private final String DEFAULT_CONFIG_XML_FILE = "notificationManagerConfig"
            + File.separator + "DefaultNotificationConfig.xml";

    /** Configuration file */
    private NotificationConfigXML configXml = null;

    /** Filter Configuration file */
    private NotificationFilterXML filterXml = null;

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /** Unmarshaller object */
    String fileName = null;

    /** Current Configuration File */
    LocalizationFile currentConfigFile = null;

    /** Current Filter File */
    LocalizationFile currentFilterFile = null;

    /** Default Configuration File */
    LocalizationFile defaultConfigFile = null;

    /**
     * Private Constructor
     */
    private NotificationConfigManager() {
        createContext();
        readXML();
    }

    /**
     * Get the instance of this class.
     *
     * @return instance
     */
    public static NotificationConfigManager getInstance() {
        return instance;
    }

    /**
     * Re-read the xml file.
     */
    public void rereadxml() {
        readXML();
    }

    private void readXML() {
        try {
            String fileName = "dataDelivery" + File.separator
                    + DEFAULT_CONFIG_XML_FILE;
            IPathManager pm = PathManagerFactory.getPathManager();

            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            LocalizationFile configLocFile = pm.getLocalizationFile(context,
                    fileName);

            String filterFileName = "dataDelivery" + File.separator
                    + FILTER_XML_FILE;
            LocalizationFile filterLocFile = pm.getLocalizationFile(context,
                    filterFileName);

            File configFile = null;
            File filterFile = null;
            if (configLocFile != null) {
                configFile = configLocFile.getFile();
            }
            if (filterLocFile != null) {
                filterFile = filterLocFile.getFile();
            }

            if (configFile != null && configFile.exists()) {
                configXml = (NotificationConfigXML) unmarshaller
                        .unmarshal(configFile);
            } else {
                configXml = new NotificationConfigXML();
            }
            if (filterFile != null && filterFile.exists()) {
                filterXml = (NotificationFilterXML) unmarshaller
                        .unmarshal(filterFile);
            } else {
                filterXml = new NotificationFilterXML();
            }
        } catch (JAXBException e1) {
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);

            if (configXml == null) {
                configXml = new NotificationConfigXML();
            }
            if (filterXml == null) {
                filterXml = new NotificationFilterXML();
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Save Localization file to disk.
     */
    public void saveXml() {
        if (currentConfigFile == null) {
            fileName = "dataDelivery" + File.separator + CONFIG_XML_FILE;
        } else {
            fileName = currentConfigFile.getName();
        }

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile configLocFile = pm.getLocalizationFile(context,
                fileName);

        fileName = "dataDelivery" + File.separator + FILTER_XML_FILE;
        LocalizationFile filterLocFile = pm.getLocalizationFile(context,
                fileName);

        try {
            if (configXml == null) {
                configXml = new NotificationConfigXML();
            }

            marshaller.marshal(configXml, configLocFile.getFile());
            configLocFile.save();

            if (filterXml == null) {
                filterXml = new NotificationFilterXML();
            }

            marshaller.marshal(filterXml, filterLocFile.getFile());
            filterLocFile.save();
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes = new Class[] { NotificationConfigXML.class,
                NotificationFilterXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();

            // format the output xml file
            this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Set the current Configuration File.
     *
     * @param currentConfigFile
     *            the currentConfigFile to set
     */
    public void setConfigFile(LocalizationFile currentConfigFile) {
        this.currentConfigFile = currentConfigFile;

    }

    /**
     * Set the current Filter File.
     *
     * @param currentFilterFile
     *            the currentFilterFile to set
     */
    public void setFilterFile(LocalizationFile currentFilterFile) {
        this.currentFilterFile = currentFilterFile;

    }

    /**
     * Set the current default Configuration File.
     *
     * @param defaultConfigFile
     *            the currentConfigFile to set
     */
    public void setDefaultConfigFile(LocalizationFile defaultConfigFile) {
        this.defaultConfigFile = defaultConfigFile;

    }

    /**
     * Get the configuration xml.
     *
     * @return the notification config xml
     */
    public NotificationConfigXML getConfigXml() {
        return configXml;
    }

    /**
     * Set the configuration xml.
     *
     * @param xml
     *            the notification config xml
     */
    public void setConfigXml(NotificationConfigXML xml) {
        this.configXml = xml;
    }

    /**
     * Get the filter xml.
     *
     * @return the filterXml
     */
    public NotificationFilterXML getFilterXml() {
        return filterXml;
    }

    /**
     * Set the filter xml.
     *
     * @param filterXml
     *            the filterXml
     */
    public void setFilterXml(NotificationFilterXML filterXml) {
        this.filterXml = filterXml;
    }
}
