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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.xml.FFMPSourceConfigXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * FFMPSourceConfigurationManager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 04, 2012  14404    gzhang    Fixing ConcurrentModificationException
 * Apr 26, 2013  1954     bsteffen  Minor code cleanup throughout FFMP.
 * Aug 18, 2013  1742     dhladky   Concurrent mod exception on update fixed
 * Oct 02, 2013  2361     njensen   Use JAXBManager for XML
 * Aug 15, 2015  4722     dhladky   Added new types to be used for new Guidance
 *                                  sources, etc
 * Sep 17, 2015  4756     dhladky   Fixed bugs for multiple guidance sources.
 * Feb 15, 2016  5244     nabowle   Replace deprecated LocalizationFile methods.
 * Aug 08, 2016  5728     mapeters  Added getConfigFileName()
 * Aug 11, 2016  5819     mapeters  Save config file to CONFIGURED instead of
 *                                  SITE
 * Jul 10, 2018  6695     njensen   Made CONFIG_FILE_NAME public
 * Jul 30, 2018  6720     njensen   Update for changed method names
 * Aug 06, 2018  6720     njensen   Added getSourceBySourceFamily(String)
 * Aug 14, 2018  6720     njensen   Simplified enums
 * 
 * </pre>
 * 
 */

public class FFMPSourceConfigurationManager
        implements ILocalizationPathObserver {

    /** Path to FFMP Source config. */
    public static final String CONFIG_FILE_NAME = "ffmp"
            + IPathManager.SEPARATOR + "FFMPSourceConfig.xml";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPSourceConfigurationManager.class);

    /*
     * This needs to initialize before the instance since the constructor will
     * makes use of JAXB. JVM spec 12.4.2 step 9 indicates this will initialize
     * ahead of the instance since it is earlier in in the text source.
     */
    private static final SingleTypeJAXBManager<FFMPSourceConfigXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(FFMPSourceConfigXML.class);

    /**
     * Enumeration of the data types FFMP can process
     */
    public enum DataType {
        RADAR, XMRG, GRID, PDO, DB, NETCDF
    }

    /**
     * Enumeration of the source types
     */
    public enum SourceType {
        RATE, QPE, QPF, GUIDANCE, GAGE
    }

    /**
     * Enumeration indicating whether a source contains rate or accumulation
     * values
     */
    public enum RateOrAccum {
        RATE, ACCUM
    }

    /**
     * Enumeration of Guidance data types
     */
    public enum GuidanceType {
        RFC, ARCHIVE
    }

    /**
     * FFMP Source Configuration XML object.
     */
    protected FFMPSourceConfigXML configXml;

    /** Singleton instance of this class */
    private static FFMPSourceConfigurationManager instance = new FFMPSourceConfigurationManager();

    private List<String> guidances = null;

    private List<String> forecasts = null;

    private List<String> accumulators = null;

    private List<MonitorConfigListener> listeners = new CopyOnWriteArrayList<>();

    /* Private Constructor */
    private FFMPSourceConfigurationManager() {
        configXml = new FFMPSourceConfigXML();
        IPathManager pm = PathManagerFactory.getPathManager();
        pm.addLocalizationPathObserver(CONFIG_FILE_NAME, this);
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
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile lf = pm.getStaticLocalizationFile(
                LocalizationType.COMMON_STATIC, CONFIG_FILE_NAME);
        try (InputStream is = lf.openInputStream()) {
            FFMPSourceConfigXML configXmltmp = jaxb
                    .unmarshalFromInputStream(is);

            configXml = configXmltmp;
        } catch (SerializationException | LocalizationException
                | IOException e) {
            statusHandler.error("Error reading file: " + lf.getPath(), e);
        }

        // If only base file exists, save a copy of it to configured
        if (lf.getContext().getLocalizationLevel()
                .equals(LocalizationLevel.BASE)) {
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
                LocalizationLevel.CONFIGURED);
        ILocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                CONFIG_FILE_NAME);

        try (SaveableOutputStream sos = newXmlFile.openOutputStream()) {
            jaxb.marshalToStream(configXml, sos);
            sos.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Couldn't save config file.",
                    e);
        }

    }

    /**
     * Get the sources for this product
     * 
     * @return
     */
    public List<SourceXML> getSources() {
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
     * source by family name
     * 
     * @param familyName
     * @return
     */
    public SourceXML getSourceBySourceFamily(String familyName) {
        return configXml.getSourceBySourceFamily(familyName);
    }

    /**
     * Get the Guidance sources
     * 
     * @return
     */
    public List<String> getGuidanceSourceNames() {
        if (guidances == null) {
            guidances = new ArrayList<>();

            for (SourceXML xml : configXml.getSource()) {
                if (xml.isGuidance()) {
                    guidances.add(xml.getSourceName());
                }
            }
        }
        return guidances;
    }

    /**
     * Get the Guidance Display Names
     * 
     * @return List of display names
     */
    public List<String> getGuidanceDisplayNames() {
        List<String> displayNames = new ArrayList<>();
        for (SourceXML xml : configXml.getSource()) {
            if (xml.isGuidance()) {
                // getGuidanceDisplayNames() is only called by SettingComp,
                // which is a GUI
                String dispName = xml.getDisplayName();
                if (displayNames.contains(dispName)) {
                    continue;
                }
                displayNames.add(dispName);
            }
        }

        return displayNames;
    }

    /**
     * Get the QPE sources
     * 
     * @return
     */
    public List<String> getQPESourceNames() {
        if (accumulators == null) {
            accumulators = new ArrayList<>();

            for (SourceXML xml : configXml.getSource()) {
                if (xml.getSourceType() == SourceType.QPE) {
                    accumulators.add(xml.getSourceName());
                }
            }
        }
        return accumulators;
    }

    /**
     * Get the QPF sources
     * 
     * @return
     */
    public List<String> getQPFSourceNames() {
        if (forecasts == null) {
            forecasts = new ArrayList<>();

            for (SourceXML xml : configXml.getSource()) {
                if (xml.getSourceType() == SourceType.QPF) {
                    forecasts.add(xml.getSourceName());
                }
            }
        }
        return forecasts;
    }

    public SourceType getSourceType(String sourceName) {
        SourceXML sourceXml = getSource(sourceName);
        if (sourceXml != null) {
            return sourceXml.getSourceType();
        }
        return null;
    }

    /**
     * Gets the product XML
     * 
     * @param primarySourceName
     * @return
     */
    public ProductXML getProductByPrimarySourceName(String primarySourceName) {
        for (ProductXML product : getProducts()) {
            if (product.getPrimarySource().equals(primarySourceName)) {
                return product;
            }
        }
        return null;
    }

    public List<ProductXML> getProducts() {
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
    public void fileChanged(ILocalizationFile file) {
        if (file.getPath().equals(CONFIG_FILE_NAME)) {
            try {
                readConfigXml();
                // inform listeners
                for (MonitorConfigListener fl : listeners) {
                    fl.configChanged(new MonitorConfigEvent(this));
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "FFMPSourceConfigurationManager: " + file.getPath()
                                + " couldn't be updated.",
                        e);
            }
        }
    }

    public String getConfigFileName() {
        return CONFIG_FILE_NAME;
    }
}
