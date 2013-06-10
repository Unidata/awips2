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
import java.io.FileNotFoundException;
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
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunConfigXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.SourceOverrideXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;


/**
 * FFMPRunConfigurationManager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2012-09-04   DR 14404   gzhang      Fixing ConcurrentModificationException
 * Apr 26, 2013 1954       bsteffen    Minor code cleanup throughout FFMP.
 * 
 * </pre>
 * 
 */

public class FFMPRunConfigurationManager implements ILocalizationFileObserver {

    /** Path to FFMP Source config. */
    private static final String CONFIG_FILE_NAME = "ffmp"
            + IPathManager.SEPARATOR
            + "FFMPRunConfig.xml";

    /**
     * FFMP Run Configuration XML object.
     */
    protected FFMPRunConfigXML configXml;

    protected boolean isPopulated;

    private LocalizationFile lf = null;
    
    private java.util.concurrent.CopyOnWriteArrayList<MonitorConfigListener> listeners = new java.util.concurrent.CopyOnWriteArrayList<MonitorConfigListener>();// DR 14404
    //private ArrayList<MonitorConfigListener> listeners = new ArrayList<MonitorConfigListener>();// DR 14404

    /** Singleton instance of this class */
    private static FFMPRunConfigurationManager instance = new FFMPRunConfigurationManager();

    /* Private Constructor */
    private FFMPRunConfigurationManager() {
        isPopulated = false;

        try {
            readConfigXml();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     * @throws FileNotFoundException
     */
    public static FFMPRunConfigurationManager getInstance() {
        return instance;
    }

    public boolean isPopulated() {
        return isPopulated;
    }

    public void setPopulated(boolean isPopulated) {
        this.isPopulated = isPopulated;
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
    public void readConfigXml() throws SerializationException {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        lf = pm.getLocalizationFile(lc, CONFIG_FILE_NAME);
        lf.addFileUpdatedObserver(this);
        File file = lf.getFile();
        // System.out.println("Reading -- " + file.getAbsolutePath());
        if (!file.exists()) {
            System.out.println("WARNING [FFMP] FFMPRunConfigurationManager: "
                    + file.getAbsolutePath() + " does not exist.");
            return;
        }

        FFMPRunConfigXML configXmltmp = null;

        configXmltmp = (FFMPRunConfigXML) SerializationUtil
                .jaxbUnmarshalFromXmlFile(file.getAbsolutePath());

        configXml = configXmltmp;
        isPopulated = true;
        applySourceConfigOverrides();
    }

    /**
     * Save the XML configuration data to the current XML file name.
     */
    public void saveConfigXml() {
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
     * Get the FFMP runners
     * 
     * @return
     */
    public ArrayList<FFMPRunXML> getFFMPRunners() {
        if (!this.isPopulated) {
            return null;
        }

        return configXml.getFFMPRun();
    }

    /**
     * Sets the run configuration for FFMP
     * 
     * @param runners
     */
    public void setFFMPRunners(ArrayList<FFMPRunXML> runners) {
        if (configXml == null) {
            configXml = new FFMPRunConfigXML();
        }
        configXml.setFFMPRun(runners);
    }

    public void addFFMPRunner(FFMPRunXML runner) {
        if (configXml == null) {
            configXml = new FFMPRunConfigXML();
        }
        configXml.add(runner);
    }

    public DomainXML getDomain(String domainname) {
        for (FFMPRunXML runner : getFFMPRunners()) {
            for (DomainXML domain : runner.getDomains()) {
                if (domain.getCwa().equals(domainname)) {
                    return domain;
                }
            }
        }
        return null;
    }

    public FFMPRunXML getRunner(String domainName) {
        for (FFMPRunXML runner : getFFMPRunners()) {
            if (runner.getPrimaryDomain().getCwa().equals(domainName)) {
                return runner;
            }
        }
        return null;
    }

    public ArrayList<DomainXML> getDomains() {

        ArrayList<DomainXML> domains = new ArrayList<DomainXML>();
        for (FFMPRunXML runner : getFFMPRunners()) {
            domains.addAll(runner.getDomains());
        }
        return domains;
    }

    public ArrayList<ProductRunXML> getProducts() {
        ArrayList<ProductRunXML> products = new ArrayList<ProductRunXML>();
        for (FFMPRunXML runner : getFFMPRunners()) {
            products.addAll(runner.getProducts());
        }
        return products;

    }

    public ProductRunXML getProduct(String productKey) {
        for (FFMPRunXML runner : getFFMPRunners()) {
            for (ProductRunXML product : runner.getProducts()) {
                if (product.getProductKey().equals(productKey)) {
                    return product;
                }
            }
        }

        return null;
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {

        if (message.getFileName().equals(CONFIG_FILE_NAME)) {
            try {
                readConfigXml();
                // inform listeners
                //synchronized (listeners) {// DR 14404
                    for (MonitorConfigListener fl : listeners) {
                        fl.configChanged(new MonitorConfigEvent(this));
                    }
                //}// DR 14404
            } catch (SerializationException e) {
                e.printStackTrace();
            }
        }

    }

    /**
     * Flags the existence of source config param overrides
     */
    public void applySourceConfigOverrides() {
        FFMPSourceConfigurationManager fscm = FFMPSourceConfigurationManager
                .getInstance();
        for (FFMPRunXML runner : getFFMPRunners()) {
            if (runner.isOverride()) {
                for (String source : runner.getOverrideSources()) {
                    SourceXML osource = fscm.getSource(source);
                    osource.setOverrride(true);
                    SourceOverrideXML override = runner
                            .getSourceOverride(source);
                    osource.setOverrideDataKeys(override
                            .getSourceOverrideDataKeys());
                }
            }
        }
    }
}
