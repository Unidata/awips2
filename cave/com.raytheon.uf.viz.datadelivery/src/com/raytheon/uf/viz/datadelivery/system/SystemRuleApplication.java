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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;
import com.raytheon.uf.viz.datadelivery.subscription.xml.SubscriptionRuleXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.NameOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.TypeOperationItems;
import com.raytheon.viz.ui.presenter.IDisplay;

/**
 * Class to apply saved system rules to subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 0730       jpiatt      Initial creation.
 * Nov 09, 2012 1286       djohnson    Hook into bandwidth management.
 * Nov 20, 2012 1286       djohnson    Use IDisplay for subscription service.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemRuleApplication {

    /** Status Handler */
    private final static IUFStatusHandler statusHandler = UFStatus
            .getHandler(SystemRuleApplication.class);

    /** Marshaller object */
    private static Marshaller marshaller;

    /** Unmarshaller object */
    private static Unmarshaller unmarshaller;

    /** JAXB context */
    private static JAXBContext jax;

    /** Size constant */
    private final static String DATASET_SIZE = OpsNetFieldNames.SIZE.toString();

    /** Frequency constant */
    private final static String DATASET_FREQ = OpsNetFieldNames.FREQUENCY
            .toString();

    /**
     * Get the rule list.
     * 
     * @param display
     * @param displayPromptStrategy
     * 
     * @return a message to display, or null if no message should be displayed
     */
    public static ISubscriptionServiceResult applyRules(IDisplay display,
            IForceApplyPromptDisplayText displayPromptStrategy) {
        LocalizationFile[] ruleFiles = null;

        try {
            ruleFiles = SystemRuleManager.getInstance().getRules();
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error applying rules to subscriptions", e);
        }

        createContext();

        // get all subscriptions
        List<Subscription> subscriptions = Collections.emptyList();
        try {
            subscriptions = DataDeliveryHandlers.getSubscriptionHandler()
                    .getAll();
        } catch (RegistryHandlerException t) {
            statusHandler.handle(Priority.PROBLEM, t.getMessage(), t);
        }

        // need to determine which are latency files
        if (ruleFiles != null) {
            List<Subscription> subscriptionsModified = new ArrayList<Subscription>();
            for (LocalizationFile lf : ruleFiles) {
                try {
                    SubscriptionRuleXML xml = (SubscriptionRuleXML) unmarshaller
                            .unmarshal(lf.getFile());
                    Integer priority = xml.getRulePriority();
                    Integer latency = xml.getRuleLatency();
                    // search subscriptions for criteria
                    List<Subscription> matchingSubscriptions = getMatchingSubscriptions(
                            xml, subscriptions);
                    // replace subscriptions with the rule value
                    applyRule(matchingSubscriptions, priority, latency);
                    for (Subscription subscription : matchingSubscriptions) {
                        if (!subscriptionsModified.contains(subscription)) {
                            subscriptionsModified.add(subscription);
                        }
                    }
                } catch (JAXBException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            try {
                final ISubscriptionServiceResult result = DataDeliveryServices
                        .getSubscriptionService().update(subscriptionsModified,
                                displayPromptStrategy);
                return result;
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to apply the rules files to subscriptions.", e);
            }
        }

        return null;

    }

    /**
     * Get a list of the subscriptions matching the rule criteria.
     * 
     * @param subscriptions
     */
    private static List<Subscription> getMatchingSubscriptions(
            SubscriptionRuleXML xml, List<Subscription> subscriptions) {
        List<Subscription> modifyList = new ArrayList<Subscription>();

        String field = xml.getRuleField();
        String operator = xml.getRuleOperator();
        String value = xml.getRuleValue();

        if (DATASET_SIZE.equals(field) || DATASET_FREQ.equals(field)) {
            String unit = xml.getRuleUnit();
        }

        for (Subscription sub : subscriptions) {
            // If Data Name
            if (OpsNetFieldNames.NAME.toString().equals(field)) {
                // get subscription names matching criteria
                if (NameOperationItems.LIKE.toString().equals(operator)) {
                    String dsName = sub.getDataSetName();
                    if (dsName.contains(value)) {
                        modifyList.add(sub);
                    }
                }

            }

            // If Data Type
            if (OpsNetFieldNames.TYPE.toString().equals(field)) {
                String dsType = sub.getDataSetType().toString();
                // get subscription names matching criteria
                if (TypeOperationItems.IN.toString().equals(operator)) {
                    if (dsType.contains(value)) {
                        modifyList.add(sub);
                    }
                } else if (!TypeOperationItems.NOT_IN.toString().equals(
                        operator)) {
                    if (!dsType.contains(value)) {
                        modifyList.add(sub);
                    }
                }
            }

            // If Data Size
            if (DATASET_SIZE.equals(field)) {
                long dsSize = sub.getDataSetSize();
                int ruleValue = Integer.parseInt(value);
                OperatorTypes operatorType = OperatorTypes.fromString(operator);
                // get subscription names matching criteria
                if (operatorType.evaluate(dsSize, ruleValue)) {
                    modifyList.add(sub);
                }
            }

            // If Data Frequency
            if (DATASET_FREQ.equals(field)) {
                // TODO Determine Dataset Frequency
            }
        }

        return modifyList;

    }

    /**
     * Get a list of the subscriptions matching the rule criteria.
     * 
     * @param modifyList
     * @return the set of subscription names that are unscheduled after applying
     *         the rules
     */
    private static void applyRule(List<Subscription> modifyList,
            Integer priority, Integer latency) {
        // Apply the priority to the subscriptions returned
        if ((priority != null || latency != null) && !modifyList.isEmpty()) {
            for (Subscription subscription : modifyList) {
                if (priority != null) {
                    // replace the priority
                    subscription.setPriority(priority - 1);
                }

                if (latency != null) {
                    subscription.setLatencyInMinutes(latency);
                }
            }

        }
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private static void createContext() {
        Class[] classes = new Class[] { SubscriptionRuleXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            unmarshaller = jax.createUnmarshaller();
            marshaller = jax.createMarshaller();

            // format the output xml file
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

}
