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
import java.util.ArrayList;
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
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.common.xml.ColumnXML;
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
 * Jan 03, 2013  1437      bgonzale   Put default configuration file code here.
 * Jun 21, 2013  2130      mpduff     Fix ordering of columns.
 * Nov 06, 2013  2358      mpduff     Remove default configuration code.
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

    /** Path for Subscription Configuration files */
    private final String CONFIG_PATH = FileUtil.join("dataDelivery",
            "subscriptionManagerConfig");

    /** Default Subscription Configuration xml file */
    private final String DEFAULT_CONFIG_XML_FILE = "DefaultSubscriptionConfig.xml";

    /** Default Subscription Configuration xml */
    private final String DEFAULT_CONFIG_XML = FileUtil.join(CONFIG_PATH,
            DEFAULT_CONFIG_XML_FILE);

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
    String fileName;

    /** Current Configuration File */
    private LocalizationFile currentConfigFile = null;

    /**
     * Private Constructor
     */
    private SubscriptionConfigurationManager() {
        fileName = DEFAULT_CONFIG_XML;
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
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

            LocalizationFile locFile = pm
                    .getLocalizationFile(context, fileName);

            if (locFile != null) {
                File file = locFile.getFile();

                if (file != null && file.exists()) {
                    xml = (SubscriptionManagerConfigXML) unmarshaller
                            .unmarshal(file);
                } else {
                    xml = new SubscriptionManagerConfigXML();
                }
            }
        } catch (JAXBException e1) {
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Save the localization file to disk
     */
    public void saveXml() {

        if (fileName == null) {
            setDefaultConfiguration();
        }

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
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
     * Set the default configuration.
     */
    public void setDefaultConfiguration() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

        LocalizationFile locFile = pm.getLocalizationFile(context,
                DEFAULT_CONFIG_XML);

        setConfigFile(locFile);
        saveXml();

    }

    /**
     * Populate the text alignment map.
     */
    private void populateAlignmentMap() {
        for (int i = 0; i < SubColumnNames.values().length; i++) {
            if ((i == 2) || (i == 3)) {
                alignmentMap.put(SubColumnNames.values()[i].toString(),
                        SWT.CENTER);
            } else {
                alignmentMap.put(SubColumnNames.values()[i].toString(),
                        SWT.LEFT);
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
     * Set the current configuration file.
     * 
     * @param configFile
     */
    public void setConfigFile(LocalizationFile configFile) {
        File file = configFile.getFile();
        fileName = configFile.getName();
        this.currentConfigFile = configFile;
        if (!file.exists()) {
            if (xml == null) {
                xml = new SubscriptionManagerConfigXML();
            }
        } else {
            try {
                xml = (SubscriptionManagerConfigXML) unmarshaller
                        .unmarshal(file);
            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Set the hidden and visible columns in the configuration.
     * 
     * @param visibleColumns
     *            List of visible columns
     * @param hiddenColumns
     *            List of hidden columns
     */
    public void setVisibleAndHidden(String[] visibleColumns,
            String[] hiddenColumns) {
        ArrayList<ColumnXML> columnList = new ArrayList<ColumnXML>();

        for (String columnName : visibleColumns) {
            ColumnXML columnXml = getColumnXml(columnName);
            if (columnXml == null) {
                continue;
            }
            columnXml.setVisible(true);
            columnList.add(columnXml);
        }

        for (String columnName : hiddenColumns) {
            ColumnXML columnXml = getColumnXml(columnName);
            if (columnXml == null) {
                continue;
            }
            columnXml.setVisible(false);
            columnList.add(columnXml);
        }

        xml.setColumnList(columnList);
    }

    /**
     * Get the columnXML object for the provided column name.
     * 
     * @param columnName
     *            The column name
     * @return the ColumnXML object or null if no column by that name exists
     */
    private ColumnXML getColumnXml(String columnName) {
        for (ColumnXML col : xml.getColumnList()) {
            if (col.getName().equals(columnName)) {
                return col;
            }
        }

        return null;
    }

    /**
     * Is the passed in LocalizationFile the current config?
     * 
     * @param value
     *            config file to check.
     * @return true if is current config; false otherwise.
     */
    public boolean isCurrentConfig(LocalizationFile value) {
        return value.getName().equals(fileName);
    }

    /**
     * Set the current config to the passed in LocalizationFile and save it.
     * 
     * @param configFile
     *            set as current and save.
     */
    public void saveXml(LocalizationFile configFile) {
        fileName = configFile.getName();
        saveXml();
    }

    /**
     * Get the localization (config) file path
     * 
     * @return
     */
    public String getLocalizationPath() {
        return CONFIG_PATH;
    }

    /**
     * Get the default config file's full path
     * 
     * @return
     */
    public String getDefaultXMLConfig() {
        return DEFAULT_CONFIG_XML;
    }

    /**
     * Get the default config file's name
     * 
     * @return
     */
    public String getDefaultXMLConfigFileName() {
        return DEFAULT_CONFIG_XML_FILE;
    }

    /**
     * Set the sorted Column
     * 
     * @param columnName
     * @param sortDirection
     */
    public void setSortedColumn(String columnName, SortDirection sortDirection) {
        xml.setSortColumn(columnName, sortDirection);
    }

    /**
     * Unmarshal the file.
     * 
     * @param file
     * @return
     * @throws JAXBException
     */
    public SubscriptionManagerConfigXML unmarshall(File file)
            throws JAXBException {
        Object obj = unmarshaller.unmarshal(file);
        if (obj instanceof SubscriptionManagerConfigXML) {
            return (SubscriptionManagerConfigXML) obj;
        }

        return null;
    }

    /**
     * @return the currentConfigFile
     */
    public LocalizationFile getCurrentConfigFile() {
        return currentConfigFile;
    }

    /**
     * @param currentConfigFile
     *            the currentConfigFile to set
     */
    public void setCurrentConfigFile(LocalizationFile currentConfigFile) {
        this.currentConfigFile = currentConfigFile;
    }

    /**
     * Delete the localization file.
     * 
     * @param file
     *            the file to delete
     */
    public void deleteXml(LocalizationFile file) {
        try {
            boolean success = file.delete();
            if (!success) {
                statusHandler.handle(Priority.WARN,
                        "Error deleting " + file.getName());
            }
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting " + file.getName(), e);
        }
    }
}
