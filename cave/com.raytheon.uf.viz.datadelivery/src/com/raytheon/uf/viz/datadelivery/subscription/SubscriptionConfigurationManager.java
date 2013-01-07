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
package com.raytheon.uf.viz.datadelivery.subscription;

import java.io.File;
import java.util.HashMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.swt.SWT;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.subscription.xml.SubscriptionManagerConfigXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.SubColumnNames;

/**
 * Subscription Configuration Manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2012            mpduff     Initial creation
 * Mar 9, 2012    418      jpiatt     Updates for load, save & set default xml.
 * Jun 07, 2012   687      lvenable   Table data refactor.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionConfigurationManager {

    /** Status handler */
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionConfigurationManager.class);

    /** Configuration Manager instance */
    private static SubscriptionConfigurationManager instance = null;

    /** Configuration XML File */
    private static final String XML_FILE = "subscriptionManagerConfig" + File.separator + "subManagerConfig.xml";

    /** Configuration XML File */
    private static final String DEFAULT_CONFIG_XML_FILE = "subscriptionManagerConfig" + File.separator
            + "DefaultSubscriptionConfig.xml";

    /** Map holding column text alignment */
    private final HashMap<String, Integer> alignmentMap = new HashMap<String, Integer>();

    /** Configuration file */
    private SubscriptionManagerConfigXML xml = null;

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /** File name */
    String fileName = null;

    /** Current Configuration File */
    LocalizationFile currentConfigFile = null;

    /** Default Configuration File */
    LocalizationFile defaultConfigFile = null;

    /**
     * Private Constructor
     */
    private SubscriptionConfigurationManager() {
        createContext();
        readXML();
        populateAlignmentMap();
    }

    /**
     * Get the instance of this class.
     * 
     * @return instance
     */
    public static SubscriptionConfigurationManager getInstance() {
        if (instance == null) {
            instance = new SubscriptionConfigurationManager();
        }

        return instance;
    }

    /**
     * Read the xml file.
     */
    private void readXML() {
        try {
            String fileName = "dataDelivery" + File.separator + DEFAULT_CONFIG_XML_FILE;

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

            LocalizationFile locFile = pm.getLocalizationFile(context, fileName);

            if (locFile != null) {
                File file = locFile.getFile();

                if (file != null && file.exists()) {
                    xml = (SubscriptionManagerConfigXML)unmarshaller.unmarshal(file);
                }
                else {
                    xml = new SubscriptionManagerConfigXML();
                }
            }
        } catch (JAXBException e1) {
            statusHandler.handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Save the localization file to disk
     */
    public void saveXml() {

        if (currentConfigFile == null) {
            fileName = "dataDelivery" + File.separator + XML_FILE;
        }
        else {
            fileName = currentConfigFile.getName();
        }

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile locFile = pm.getLocalizationFile(context, fileName);

        File file = null;
        if (locFile != null) {
            file = locFile.getFile();
        }

        try {

            marshaller.marshal(xml, file);
            locFile.save();

        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Populate the text alignment map.
     */
    private void populateAlignmentMap() {
        for (int i = 0; i < SubColumnNames.values().length; i++) {
            if ((i == 2) || (i == 3)) {
                alignmentMap.put(SubColumnNames.values()[i].toString(), SWT.CENTER);
            }
            else {
                alignmentMap.put(SubColumnNames.values()[i].toString(), SWT.LEFT);
            }
        }
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes = new Class[] { SubscriptionManagerConfigXML.class };

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
     * Get the xml.
     * 
     * @return the xml
     */
    public SubscriptionManagerConfigXML getXml() {
        return xml;
    }

    /**
     * Set the xml.
     * 
     * @param xml
     *            the xml to set
     */
    public void setXml(SubscriptionManagerConfigXML xml) {
        this.xml = xml;
    }

    /**
     * Get the alignment map.
     * 
     * @return the alignmentMap
     */
    public HashMap<String, Integer> getAlignmentMap() {
        return alignmentMap;
    }

    /**
     * Get the current configuration file.
     * 
     * @return currentConfigFile
     */
    public LocalizationFile getConfigFile() {
        return currentConfigFile;
    }

    /**
     * Set the current configuration file.
     * 
     * @param currentConfigFile
     */
    public void setConfigFile(LocalizationFile currentConfigFile) {
        this.currentConfigFile = currentConfigFile;
    }

    /**
     * Get the default configuration file.
     * 
     * @return defaultConfigFile
     */
    public LocalizationFile getDefaultConfigFile() {
        return defaultConfigFile;
    }

    /**
     * Set the default configuration file.
     * 
     * @param defaultConfigFile
     */
    public void setDefaultConfigFile(final LocalizationFile defaultConfigFile) {
        this.defaultConfigFile = defaultConfigFile;
    }
}
