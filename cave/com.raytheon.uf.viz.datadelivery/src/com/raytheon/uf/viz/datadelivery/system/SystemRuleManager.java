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
package com.raytheon.uf.viz.datadelivery.system;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
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
import com.raytheon.uf.viz.datadelivery.subscription.xml.LatencyRuleXML;
import com.raytheon.uf.viz.datadelivery.subscription.xml.LatencyRulesXML;
import com.raytheon.uf.viz.datadelivery.subscription.xml.PriorityRuleXML;
import com.raytheon.uf.viz.datadelivery.subscription.xml.PriorityRulesXML;
import com.raytheon.uf.viz.datadelivery.subscription.xml.RuleXML;
import com.raytheon.uf.viz.datadelivery.subscription.xml.RulesXML;
import com.raytheon.uf.viz.datadelivery.utils.DataSetFrequency;
import com.raytheon.uf.viz.datadelivery.utils.NameOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.TypeOperationItems;

/**
 * System Rule Manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012    730      jpiatt     Initial creation.
 * Oct 23, 2012   1286      djohnson   Hook into bandwidth management.
 * Jan 04, 2013   1420      mpduff     Move rules into a single file.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemRuleManager {

    /** SystemRuleManager instance */
    private static final SystemRuleManager instance = new SystemRuleManager();

    /** Directory Path to rules */
    private final String RULE_PATH = "datadelivery" + File.separator
            + "systemManagement" + File.separator + "rules" + File.separator;

    private final String LATENCY_RULE_FILE = RULE_PATH + "latencyRules.xml";

    private final String PRIORITY_RULE_FILE = RULE_PATH + "priorityRules.xml";

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SystemRuleManager.class);

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    private IBandwidthService bandwidthService;

    /**
     * Constructor.
     */
    private SystemRuleManager() {
        createContext();
    }

    /**
     * Get an instance of the SystemRuleManager.
     * 
     * @return instance
     */
    public static SystemRuleManager getInstance() {
        return instance;
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes = new Class[] { RuleXML.class, PriorityRuleXML.class,
                LatencyRuleXML.class, LatencyRulesXML.class,
                PriorityRulesXML.class, RulesXML.class, OperatorTypes.class,
                TypeOperationItems.class, NameOperationItems.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();
            this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Get the names of the priority rules
     * 
     * @return String[] of names
     * @throws JAXBException
     */
    public List<String> getPriorityRuleNames() {
        return getPriorityRules().getRuleNames();
    }

    public PriorityRuleXML loadPriorityRule(String name) {
        PriorityRulesXML priorityRules = getPriorityRules();
        for (PriorityRuleXML rule : priorityRules.getRules()) {
            if (rule.getRuleName().equals(name)) {
                return rule;
            }
        }

        return new PriorityRuleXML();
    }

    public LatencyRuleXML loadLatencyRule(String name) {
        LatencyRulesXML latencyRules = getLatencyRules();
        for (LatencyRuleXML rule : latencyRules.getRules()) {
            if (rule.getRuleName().equals(name)) {
                return rule;
            }
        }

        return new LatencyRuleXML();
    }

    /**
     * Get latency file names
     * 
     * @return String[] Array of latency rule names
     * @throws JAXBException
     */
    public List<String> getLatencyRuleNames() {
        return getLatencyRules().getRuleNames();
    }

    public boolean savePriorityRules(PriorityRulesXML xmlObj) {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile priorityRulesLocFile = pm.getLocalizationFile(context,
                this.PRIORITY_RULE_FILE);

        try {
            marshaller.marshal(xmlObj, priorityRulesLocFile.getFile());
            priorityRulesLocFile.save();
            return true;
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        return false;
    }

    public boolean saveLatencyRules(LatencyRulesXML xmlObj) {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile latencyRulesLocFile = pm.getLocalizationFile(context,
                this.LATENCY_RULE_FILE);

        try {
            marshaller.marshal(xmlObj, latencyRulesLocFile.getFile());
            latencyRulesLocFile.save();
            return true;
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        return false;
    }

    public void deleteLatencyRule(String ruleName) {
        LatencyRulesXML latencyRules = getLatencyRules();

        for (LatencyRuleXML rule : latencyRules.getRules()) {
            if (rule.getRuleName().equals(ruleName)) {
                latencyRules.removeRuleByName(ruleName);
                saveLatencyRules(latencyRules);
                return;
            }
        }
    }

    public void deletePriorityRule(String ruleName) {
        PriorityRulesXML priorityRules = getPriorityRules();

        for (PriorityRuleXML rule : priorityRules.getRules()) {
            if (rule.getRuleName().equals(ruleName)) {
                priorityRules.removeRuleByName(ruleName);
                savePriorityRules(priorityRules);
                return;
            }
        }
    }

    public boolean updateRule(LatencyRuleXML rule) {
        LatencyRulesXML rulesXml = getLatencyRules();
        boolean saved = rulesXml.updateRule(rule);
        if (saved) {
            return saveLatencyRules(rulesXml);
        }

        return false;
    }

    public boolean updateRule(PriorityRuleXML rule) {
        PriorityRulesXML rulesXml = getPriorityRules();
        boolean saved = rulesXml.updateRule(rule);
        if (saved) {
            saved = savePriorityRules(rulesXml);
        }

        if (!saved) {
            this.statusHandler.warn("Error saving rules.");
        }

        return saved;
    }

    public boolean saveRule(PriorityRuleXML rule) {
        PriorityRulesXML rulesXml = getPriorityRules();
        boolean saved = rulesXml.addRule(rule);
        if (saved) {
            saved = savePriorityRules(rulesXml);
        }

        if (!saved) {
            this.statusHandler.warn("Error saving Priority rules.");
        }

        return saved;
    }

    public boolean saveRule(LatencyRuleXML rule) {
        LatencyRulesXML rulesXml = getLatencyRules();
        boolean saved = rulesXml.addRule(rule);
        if (saved) {
            saved = saveLatencyRules(rulesXml);
        }

        if (!saved) {
            this.statusHandler.warn("Error saving Latency rules.");
        }

        return saved;
    }

    private LatencyRulesXML getLatencyRules() {
        LocalizationFile lfile = getRules(this.LATENCY_RULE_FILE);

        LatencyRulesXML latencyRules = new LatencyRulesXML();
        if (lfile != null && lfile.exists()) {
            try {
                latencyRules = (LatencyRulesXML) unmarshaller.unmarshal(lfile
                        .getFile());
            } catch (JAXBException e) {
                statusHandler
                        .handle(Priority.ERROR, e.getLocalizedMessage(), e);
            }
        }

        return latencyRules;
    }

    private PriorityRulesXML getPriorityRules() {
        LocalizationFile lfile = getRules(this.PRIORITY_RULE_FILE);

        PriorityRulesXML priorityRules = new PriorityRulesXML();
        if (lfile != null && lfile.exists()) {
            try {
                priorityRules = (PriorityRulesXML) unmarshaller.unmarshal(lfile
                        .getFile());
            } catch (JAXBException e) {
                statusHandler
                        .handle(Priority.ERROR, e.getLocalizedMessage(), e);
            }
        }
        return priorityRules;
    }

    private LocalizationFile getRules(String name) {
        IPathManager pm = PathManagerFactory.getPathManager();
        return pm.getStaticLocalizationFile(name);
    }

    /**
     * Get the default latency.
     * 
     * @param DataSetFrequency
     *            Frequency of the data
     * 
     * @return The default latency
     */
    public int getDefaultLatency(DataSetFrequency freq) {
        int frequency = 40;
        switch (freq) {
        case HOURLY:
            frequency = 40;
            break;
        case SIX_HOURLY:
            frequency = 115;
            break;
        }

        return frequency;
    }

    /**
     * Get the default latency value given the cycleTimes.
     * 
     * @param cycleTimes
     * @return
     */
    public int getDefaultLatency(List<Integer> cycleTimes) {
        DataSetFrequency freq = DataSetFrequency.fromCycleTimes(cycleTimes);

        return getDefaultLatency(freq);
    }

    /**
     * Return the lowest latency value defined by the rules.
     * 
     * @param sub
     *            The subscription
     * @param cycleTimes
     *            The available cycle times
     * @return
     */
    public int getLatency(Subscription sub, Set<Integer> cycleTimes) {
        LatencyRulesXML rulesXml = this.getLatencyRules();
        int latency = 999;
        boolean found = false;
        for (LatencyRuleXML rule : rulesXml.getRules()) {
            if (rule.matches(sub, cycleTimes)) {
                if (rule.getLatency() < latency) {
                    latency = rule.getLatency();
                    found = true;
                }
            }
        }

        // Set default if none found
        if (!found) {
            latency = this
                    .getDefaultLatency(new ArrayList<Integer>(cycleTimes));
        }

        return latency;
    }

    /**
     * Return the lowest priority value defined by the rules.
     * 
     * @param sub
     * @param cycleTimes
     * @return
     */
    public int getPriority(Subscription sub, Set<Integer> cycleTimes) {
        PriorityRulesXML rulesXml = this.getPriorityRules();
        int priority = 3;
        boolean found = false;
        for (PriorityRuleXML rule : rulesXml.getRules()) {
            if (rule.matches(sub, cycleTimes)) {
                if (rule.getPriority() < priority) {
                    priority = rule.getPriority();
                    found = true;
                }
            }
        }

        // Default to normal priority
        if (!found) {
            priority = 2;
        }

        return priority;
    }

    /**
     * Set the {@link IBandwidthService}.
     * 
     * @param bandwidthService
     *            the bandwidthService to set
     */
    public void setBandwidthService(IBandwidthService bandwidthService) {
        this.bandwidthService = bandwidthService;
    }

    /**
     * Get the available bandwidth for a {@link Network}.
     * 
     * @param network
     *            the network
     * @return the available bandwidth
     */
    public static int getAvailableBandwidth(Network network) {
        return getInstance().bandwidthService
                .getBandwidthForNetworkInKilobytes(network);
    }

    /**
     * Propose setting the available bandwidth for a {@link Network}. If the
     * bandwidth amount will not change the scheduled subscriptions it is
     * immediately applied, otherwise the set of subscriptions that would be
     * unscheduled is returned.
     * 
     * @param network
     *            the network
     * @param bandwidth
     *            the available bandwidth
     * @return empty list if successfully applied, otherwise the set of
     *         subscriptions that would be unscheduled
     */
    public static Set<Subscription> setAvailableBandwidth(Network network,
            int bandwidth) {
        return getInstance().bandwidthService
                .proposeBandwidthForNetworkInKilobytes(network, bandwidth);
    }

    /**
     * Sets the available bandwidth for a {@link Network}.
     * 
     * @param network
     *            the network
     * @param bandwidth
     *            the bandwidth
     * @return true if successfully applied the change, false otherwise
     */
    public static boolean forceSetAvailableBandwidth(Network network,
            int bandwidth) {
        return getInstance().bandwidthService
                .setBandwidthForNetworkInKilobytes(network, bandwidth);
    }
}
