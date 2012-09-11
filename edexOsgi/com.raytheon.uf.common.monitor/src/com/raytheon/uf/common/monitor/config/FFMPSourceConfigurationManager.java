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
package com.raytheon.uf.common.monitor.config;

import java.io.File;
import java.util.ArrayList;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.xml.FFMPSourceConfigXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * FFMPSourceConfigurationManager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2012-09-04   DR 14404   gzhang      Fixing ConcurrentModificationException
 * 
 * </pre>
 * 
 */

public class FFMPSourceConfigurationManager implements
        ILocalizationFileObserver {

    /** Path to FFMP Source config. */
    private static final String CONFIG_FILE_NAME = "ffmp" + File.separatorChar
            + "FFMPSourceConfig.xml";

    /**
     * FFMP Source Configuration XML object.
     */
    protected FFMPSourceConfigXML configXml;

    /** Singleton instance of this class */
    private static FFMPSourceConfigurationManager instance = new FFMPSourceConfigurationManager();

    private ArrayList<String> virtuals = null;

    private ArrayList<String> rates = null;

    private ArrayList<String> guidances = null;

    private ArrayList<String> forecasts = null;

    private ArrayList<String> accumulators = null;

    private LocalizationFile lf = null;
    
    private java.util.concurrent.CopyOnWriteArrayList<MonitorConfigListener> listeners = new java.util.concurrent.CopyOnWriteArrayList<MonitorConfigListener>();// DR 14404
    //private ArrayList<MonitorConfigListener> listeners = new ArrayList<MonitorConfigListener>();// DR 14404

    /* Private Constructor */
    private FFMPSourceConfigurationManager() {
        configXml = new FFMPSourceConfigXML();
        readConfigXml();
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static FFMPSourceConfigurationManager getInstance() {
        return instance;
    }

    public void addListener(MonitorConfigListener fl) {
        listeners.add(fl);
    }

    public void removeListener(MonitorConfigListener fl) {
        listeners.remove(fl);
    }

    /**
     * Read the XML configuration data for the current XML file name.
     */
    public synchronized void readConfigXml() {

        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            lf = pm.getLocalizationFile(lc, CONFIG_FILE_NAME);
            lf.addFileUpdatedObserver(this);
            File file = lf.getFile();
            // System.out.println("Reading -- " + file.getAbsolutePath());

            FFMPSourceConfigXML configXmltmp = (FFMPSourceConfigXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(file.getAbsolutePath());

            configXml = configXmltmp;

        } catch (Exception e) {
            System.err.println("No SITE FFMP Source configuration file found");

            // fall back to BASE
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            lf = pm.getLocalizationFile(lc, CONFIG_FILE_NAME);
            lf.addFileUpdatedObserver(this);
            File file = lf.getFile();
            // System.out.println("Reading -- " + file.getAbsolutePath());

            FFMPSourceConfigXML configXmltmp = null;
            try {
                configXmltmp = (FFMPSourceConfigXML) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(file.getAbsolutePath());
            } catch (SerializationException e1) {
                e1.printStackTrace();
            }

            configXml = configXmltmp;
            // writes one to site
            saveConfigXml();
        }
    }

    /**
     * Save the XML configuration data to the current XML file name.
     */
    private void saveConfigXml() {
        // Save the xml object to disk
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                CONFIG_FILE_NAME);

        if (newXmlFile.getFile().getParentFile().exists() == false) {
            // System.out.println("Creating new directory");

            if (newXmlFile.getFile().getParentFile().mkdirs() == false) {
                // System.out.println("Could not create new directory...");
            }
        }

        try {
            // System.out.println("Saving -- "
            // + newXmlFile.getFile().getAbsolutePath());
            SerializationUtil.jaxbMarshalToXmlFile(configXml, newXmlFile
                    .getFile().getAbsolutePath());
            newXmlFile.save();

            lf = newXmlFile;
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Gets you your sources for this product
     * 
     * @param productName
     * @return
     */
    public ArrayList<SourceXML> getSources() {
        return configXml.getSource();
    }

    /**
     * Get the rate sourceXML by product
     * 
     * @return
     */
    public SourceXML getSource(String source) {
        return configXml.getSource(source);
    }

    /**
     * source by display name
     * 
     * @param displayName
     * @return
     */
    public SourceXML getSourceByDisplayName(String displayName) {
        return configXml.getSourceByDisplayName(displayName);
    }

    /**
     * Get the virtual gage basin implementations
     * 
     * @param name
     * @return
     */
    public ArrayList<String> getVirtuals() {
        if (virtuals == null) {
            virtuals = new ArrayList<String>();
        }
        for (SourceXML xml : configXml.getSource()) {
            if (xml.getSourceType().equals(SOURCE_TYPE.GAGE.getSourceType())) {
                virtuals.add(xml.getSourceName());
            }
        }
        return virtuals;
    }

    /**
     * Get the Guidance sources
     * 
     * @return
     */
    public ArrayList<String> getGuidances() {
        if (guidances == null) {
            guidances = new ArrayList<String>();
        }
        for (SourceXML xml : configXml.getSource()) {
            if (xml.getSourceType()
                    .equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
                guidances.add(xml.getSourceName());
            }
        }
        return guidances;
    }

    /**
     * Get the Guidance Display Names
     * 
     * @return List of display names
     */
    public ArrayList<String> getGuidanceDisplayNames() {
        ArrayList<String> displayNames = new ArrayList<String>();
        for (SourceXML xml : configXml.getSource()) {
            if (xml.getSourceType()
                    .equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
                if (displayNames.contains(xml.getDisplayName())) {
                    continue;
                }
                displayNames.add(xml.getDisplayName());
            }
        }

        return displayNames;
    }

    /**
     * Get the QPE sources
     * 
     * @return
     */
    public ArrayList<String> getQPESources() {
        if (accumulators == null) {
            accumulators = new ArrayList<String>();
        }
        for (SourceXML xml : configXml.getSource()) {
            if (xml.getSourceType().equals(SOURCE_TYPE.QPE.getSourceType())) {
                accumulators.add(xml.getSourceName());
            }
        }
        return accumulators;
    }

    /**
     * Get the QPE sources
     * 
     * @return
     */
    public ArrayList<String> getRates() {
        if (rates == null) {
            rates = new ArrayList<String>();
        }
        for (SourceXML xml : configXml.getSource()) {
            if (xml.getSourceType().equals(SOURCE_TYPE.RATE.getSourceType())) {
                rates.add(xml.getSourceName());
            }
        }
        return accumulators;
    }

    /**
     * Get the QPF sources
     * 
     * @return
     */
    public ArrayList<String> getQPFSources() {
        if (forecasts == null) {
            forecasts = new ArrayList<String>();
        }
        for (SourceXML xml : configXml.getSource()) {
            if (xml.getSourceType().equals(SOURCE_TYPE.QPF.getSourceType())) {
                forecasts.add(xml.getSourceName());
            }
        }
        return forecasts;
    }

    /**
     * 
     * Enumeration of the types of processible data
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum DATA_TYPE {

        RADAR("RADAR"), XMRG("XMRG"), GRID("GRID"), PDO("PDO"), DB("DB");

        private final String dataType;

        private DATA_TYPE(String name) {
            dataType = name;
        }

        public String getDataType() {
            return dataType;
        }
    };

    /**
     * 
     * Enumeration of the types of data types
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum SOURCE_TYPE {

        RATE("RATE"), QPE("QPE"), QPF("QPF"), GUIDANCE("GUIDANCE"), GAGE("GAGE");

        private final String sourceType;

        private SOURCE_TYPE(String name) {
            sourceType = name;
        }

        public String getSourceType() {
            return sourceType;
        }
    };

    /**
     * 
     * Enumeration of the types of data types
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum RATEORACCCUM {

        RATE("RATE"), ACCUM("ACCUM");

        private final String rateOrAccum;

        private RATEORACCCUM(String name) {
            rateOrAccum = name;
        }

        public String getRateOrAccum() {
            return rateOrAccum;
        }
    };

    /**
     * 
     * Enumeration of the guidance of data types
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum GUIDANCE_TYPE {

        RFC("RFC");

        private final String rfc;

        private GUIDANCE_TYPE(String name) {
            rfc = name;
        }

        public String getGuidanceType() {
            return rfc;
        }
    };

    /**
     * 
     * Enumeration of the types of data processable
     * 
     * @author dhladky
     * @version 1.0
     */
    public DATA_TYPE getDataType(String type) {

        if (type.equals(DATA_TYPE.RADAR.getDataType())) {
            return DATA_TYPE.RADAR;
        } else if (type.equals(DATA_TYPE.XMRG.getDataType())) {
            return DATA_TYPE.XMRG;
        } else if (type.equals(DATA_TYPE.GRID.getDataType())) {
            return DATA_TYPE.GRID;
        } else if (type.equals(DATA_TYPE.PDO.getDataType())) {
            return DATA_TYPE.PDO;
        } else if (type.equals(DATA_TYPE.DB.getDataType())) {
            return DATA_TYPE.DB;
        }
        return null;
    }

    public SOURCE_TYPE getSourceType(String sourceName) {
        SOURCE_TYPE source = null;
        if (getSource(sourceName) != null) {
            if (getSource(sourceName).getSourceType().equals(
                    SOURCE_TYPE.RATE.getSourceType())) {
                source = SOURCE_TYPE.RATE;
            } else if (getSource(sourceName).getSourceType().equals(
                    SOURCE_TYPE.QPE.getSourceType())) {
                source = SOURCE_TYPE.QPE;
            } else if (getSource(sourceName).getSourceType().equals(
                    SOURCE_TYPE.QPF.getSourceType())) {
                source = SOURCE_TYPE.QPF;
            } else if (getSource(sourceName).getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                source = SOURCE_TYPE.GUIDANCE;
            } else if (getSource(sourceName).getSourceType().equals(
                    SOURCE_TYPE.GAGE.getSourceType())) {
                source = SOURCE_TYPE.GAGE;
            }
        }
        return source;
    }

    /**
     * Gets the product XML
     * 
     * @param productName
     * @return
     */
    public ProductXML getProduct(String primarySourceName) {
        for (ProductXML product : getProducts()) {
            if (product.getPrimarySource().equals(primarySourceName)) {
                return product;
            }
        }
        return null;
    }

    public void setProducts(ArrayList<ProductXML> products) {
        configXml.setProducts(products);
    }

    public ArrayList<ProductXML> getProducts() {
        return configXml.getProducts();
    }

    /**
     * Finds the primary source this source is within
     * 
     * @param source
     * @return
     */
    public String getPrimarySource(SourceXML source) {
        for (ProductXML product : getProducts()) {
            if (product.containsSource(source.getSourceName())) {
                return product.getPrimarySource();
            }
        }
        return null;
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        if (message.getFileName().equals(CONFIG_FILE_NAME)) {
            readConfigXml();
            // inform listeners
            //synchronized (listeners) {// DR 14404
                for (MonitorConfigListener fl : listeners) {
                    fl.configChanged(new MonitorConfigEvent(this));
                }
            //}// DR 14404
        }
    }

}
