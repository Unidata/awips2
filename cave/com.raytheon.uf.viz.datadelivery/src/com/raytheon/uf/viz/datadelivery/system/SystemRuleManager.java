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
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

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
import com.raytheon.uf.viz.datadelivery.subscription.xml.SubscriptionRuleXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

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
 * Oct 23, 2012 1286        djohnson   Hook into bandwidth management.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemRuleManager {

    /** SystemRuleManager instance */
    private static SystemRuleManager instance = new SystemRuleManager();

    /** Directory Path to rules */
    private final String RULE_PATH = "dataDelivery" + File.separator + "rules" + File.separator;

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus.getHandler(SystemRuleManager.class);

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /** Array of rule localization files */
    private LocalizationFile[] ruleFiles;

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
        Class[] classes = new Class[] { SubscriptionRuleXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();
            this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Get priority file names
     * 
     * @return String[]
     *           Array of priority rule names
     * @throws JAXBException 
     */
    public String[] getPriorityRules() throws JAXBException {

        ArrayList<String> priorityList = new ArrayList<String>();
        ruleFiles = getRules();

        //determine which are priority files
        for (LocalizationFile lf : ruleFiles) {
            try {
                SubscriptionRuleXML xml = (SubscriptionRuleXML) unmarshaller.unmarshal(lf.getFile());
                Integer priority = xml.getRulePriority();

                if (priority != null) {
                    String lfName = lf.getFile().getName();
                    priorityList.add(lfName);
                } 

            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            }
        }

        String[] priorityRules = null;

        if (priorityList.size() > 0) {
            priorityRules = priorityList.toArray(new String[priorityList.size()]);
        }

        return priorityRules;

    }

    /**
     * Get latency file names
     * 
     * @return String[]
     *           Array of latency rule names
     * @throws JAXBException 
     */
    public String[] getLatencyRules() throws JAXBException {

        ArrayList<String> latencyList = new ArrayList<String>();
        ruleFiles = getRules();

        //need to determine which are latency files
        for (LocalizationFile lf : ruleFiles) {
            try {
                SubscriptionRuleXML xml = (SubscriptionRuleXML) unmarshaller.unmarshal(lf.getFile());
                Integer latency = xml.getRuleLatency();

                if (latency != null) {
                    String lfName = lf.getFile().getName();
                    latencyList.add(lfName);
                }


            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            }
        }

        String[] latencyRules = null;

        if (!latencyList.isEmpty()) {
            latencyRules = latencyList.toArray(new String[latencyList.size()]);
        }

        return latencyRules;

    }

    /**
     * Get rule files
     * 
     * @return String[]
     *           Array of latency rule names
     * @throws JAXBException 
     */
    public LocalizationFile[] getRules() throws JAXBException {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        String[] extensions = new String[] { "xml" };
        ruleFiles = pm.listFiles(context, RULE_PATH, extensions, false, true);

        return ruleFiles;

    }

    /**
     * Load a saved rule into memory
     * 
     * @param ruleName
     *            The subset name
     * 
     * @return The SubscriptionRuleXML object
     */
    public SubscriptionRuleXML loadRule(String ruleName) {
        // Load the rule file
        if (ruleFiles == null || ruleFiles.length == 0) {
            try {
                getRules();
            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            }
        }

        for (LocalizationFile lf : ruleFiles) {
            if (lf.getFile().getName().equals(ruleName + ".xml")) {
                try {
                    return (SubscriptionRuleXML) unmarshaller.unmarshal(lf.getFile());
                } catch (JAXBException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
                }
            }
        }

        return null;
    }

    /**
     * Save a rule xml object.
     * 
     * @param rule
     *            the object to save
     * @param shell 
     * @param create false for edit
     * @return true if successfully saved
     */
    public boolean saveRule(SubscriptionRuleXML rule, Shell shell) {

        String ruleName = rule.getRuleName();

        if (!ruleName.endsWith("xml")) {
            rule.setRuleName((ruleName) + ".xml");
        }

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile subscriptionRuleLocFile = pm.getLocalizationFile(context, RULE_PATH + rule.getRuleName());

        if (subscriptionRuleLocFile.getFile().exists()) {
                String msg = "The file " + subscriptionRuleLocFile.getFile().getName()
                + " already exists.\n\nWould you like to overwrite the file?";
                int response = DataDeliveryUtils.showMessage(shell, SWT.YES | SWT.NO, "File Exists", msg);
                if (response == SWT.NO) {
                    return false;
                }
        } 

        try {
            marshaller.marshal(rule, subscriptionRuleLocFile.getFile());
            subscriptionRuleLocFile.save();
            return true;
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        return false;
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
