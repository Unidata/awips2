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
package com.raytheon.uf.viz.datadelivery.filter.config;

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
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterSettingsXML;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterTypeXML;

/**
 * The managing class for the filter data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 27, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FilterManager {
	
    /** Single instance of this class */
    private static FilterManager instance = null;

    /** Filter xml default name */
    private final String DEFAULT_XML = "defaultFilter.xml";

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /** XML object holding the saved filter settings */
    private FilterSettingsXML xml;

    /** Current filter File */
    private LocalizationFile currentFile = null;

    /** Default Configuration filter File */
    private LocalizationFile defaultFile = null;

    /** Constructor */
    private FilterManager() {
        createContext();
//        readXML();
    }

    /**
     * Get the single instance of this class.
     * 
     * @return instance of this class
     */
    public static FilterManager getInstance() {
        if (instance == null) {
            instance = new FilterManager();
        }

        return instance;
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes = new Class[] { FilterTypeXML.class, FilterSettingsXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();
            this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        } catch (JAXBException e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            e.printStackTrace();
        }
    }

    private void readXML() {
        String fileName = null;
        LocalizationFile locFile = null;
        if (currentFile == null) {
            fileName = "dataDelivery" + File.separator + "filterConfigs"
                        + File.separator + DEFAULT_XML;
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context =
                    pm.getContext(LocalizationType.CAVE_STATIC,
                            LocalizationLevel.USER);
            locFile = pm.getLocalizationFile(context, fileName);
        } else {
            locFile = currentFile;
//            fileName = currentFile.getName();
        }
        

        File file = null;
        if (locFile != null) {
            file = locFile.getFile();
        }
        try {

            if (file != null && file.exists()) {
                xml = (FilterSettingsXML) unmarshaller.unmarshal(file);
            } else {
                xml = new FilterSettingsXML();
            }

        } catch (JAXBException e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            e.printStackTrace();
            if (xml == null) {
                xml = new FilterSettingsXML();
            }
        }
    }

    /**
     * Save the xml configuration file.
     */
    public void saveXml() {
        String fileName = null;
        LocalizationFile locFile = null;
        if (currentFile == null) {
            fileName =
                    "dataDelivery" + File.separator + "filterConfigs"
                            + File.separator + DEFAULT_XML;
            IPathManager pm = PathManagerFactory.getPathManager();

            LocalizationContext context =
                    pm.getContext(LocalizationType.CAVE_STATIC,
                            LocalizationLevel.USER);
            locFile = pm.getLocalizationFile(context, fileName);
        } else {
            locFile = currentFile;
        }


        try {
            if (xml == null) {
                xml = new FilterSettingsXML();
            }
            
            marshaller.marshal(xml, locFile.getFile());
            locFile.save();
        } catch (JAXBException e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Get the filter xml.
     * 
     * @return the xml
     */
    public FilterSettingsXML getXml() {
        return xml;
    }

    /**
     * Set the filter xml.
     * 
     * @param xml
     *            the xml to set
     */
    public void setXml(FilterSettingsXML xml) {
        this.xml = xml;
    }

    /**
     * Get the current filter xml.
     * 
     * @return currentFile
     */
    public LocalizationFile getCurrentFile() {
        return currentFile;
    }

    /**
     * Set the current filter xml.
     * 
     * @param currentFile the currentFile to set
     */
    public void setCurrentFile(LocalizationFile currentFile) {
        this.currentFile = currentFile;
        readXML();
    }

    /**
     * Get the default filter xml.
     *      
     * @return defaultFile
     */
    public LocalizationFile getDefaultFile() {
        return defaultFile;
    }

    /**
     * Set the default filter xml.
     *      
     * @param defaultFile the defaultFile to set
     */
    public void setDefaultFile(LocalizationFile defaultFile) {
        this.defaultFile = defaultFile;
    }
}
