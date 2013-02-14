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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

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
            LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

            LocalizationFile locFile = pm
                    .getLocalizationFile(context, fileName);

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

        if (fileName == null) {
            setDefaultConfiguration();
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
     * Set the current configuration file.
     * 
     * @param currentConfigFile
     */
    public void setConfigFile(LocalizationFile configFile) {
        File file = configFile.getFile();
        fileName = configFile.getName();
        try {
            xml = (SubscriptionManagerConfigXML) unmarshaller.unmarshal(file);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Get the available configuration files to list.
     * 
     * @return Map of context:name to LocalizationFile.
     */
    public Map<String, LocalizationFile> getConfigFileNameMap() {
        String[] extensions = new String[] { ".xml" };
        IPathManager pm = PathManagerFactory.getPathManager();
        TreeMap<String, LocalizationFile> locFileMap = new TreeMap<String, LocalizationFile>();
        ArrayList<LocalizationContext> contextList = new ArrayList<LocalizationContext>();
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE));
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.SITE));
        contextList.add(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER));
        LocalizationFile[] locFiles = pm.listFiles(contextList
                .toArray(new LocalizationContext[contextList.size()]),
                CONFIG_PATH, extensions, false, true);

        if (locFiles == null) {
            return new TreeMap<String, LocalizationFile>();
        }

        for (int i = 0; i < locFiles.length; i++) {
            String locFile = locFiles[i].getName();
            int idx = locFile.lastIndexOf("/");
            String newStr = locFile.substring(idx + 1);

            locFileMap.put(locFiles[i].getContext().getLocalizationLevel()
                    + ":" + newStr, locFiles[i]);
        }
        return locFileMap;
    }

    /**
     * Set the hidden and visible columns in the configuration.
     * 
     * @param visibleColumns
     * @param hiddenColumns
     */
    public void setVisibleAndHidden(String[] visibleColumns,
            String[] hiddenColumns) {
        ArrayList<ColumnXML> columnList = new ArrayList<ColumnXML>();
        Arrays.sort(visibleColumns);
        Arrays.sort(hiddenColumns);
        for (ColumnXML column :xml.getColumnList()) {
            int visibleIndex = Arrays.binarySearch(visibleColumns, column.getName());
            if (visibleIndex < 0) {
                // not in visible, check hidden
                int hiddenIndex = Arrays.binarySearch(hiddenColumns, column.getName());
                if (hiddenIndex >= 0) {
                    column.setVisible(false);
                    columnList.add(column);
                }
            } else {
                column.setVisible(true);
                columnList.add(column);
            }
        }
        xml.setColumnList(columnList);
        saveXml();
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

    public String getLocalizationPath() {
        return CONFIG_PATH;
    }

    public String getDefaultXMLConfig() {
        return DEFAULT_CONFIG_XML;
    }

    public void setSortedColumn(String columnName, SortDirection sortDirection) {
        xml.setSortColumn(columnName, sortDirection);
    }
}
