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

import java.util.Set;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;

/**
 * {@link IForceApplyPromptDisplayText} that allows the user to select from all
 * three values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2012  1286      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class CancelForceApplyAndIncreaseLatencyDisplayText implements
        IForceApplyPromptDisplayText {

    private final String actionText;

    private final String titleCaseActionText;

    private final Shell shell;

    /**
     * Constructor.
     * 
     * @param actionText
     *            the action that will be displayed in the "Do not 'action'"
     *            message
     * @param shell
     *            the shell reference
     */
    public CancelForceApplyAndIncreaseLatencyDisplayText(String actionText,
            Shell shell) {
        this.actionText = actionText.toLowerCase();
        this.titleCaseActionText = Character.toUpperCase(this.actionText
                .charAt(0)) + this.actionText.substring(1);
        this.shell = shell;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getOptionDisplayText(ForceApplyPromptResponse option,
            int requiredLatency, Subscription subscription,
            Set<String> wouldBeUnscheduledSubscriptions) {
        final String name = subscription.getName();
        switch (option) {
        case CANCEL:
            return "Do not " + actionText + " " + name;
        case FORCE_APPLY:
            if (wouldBeUnscheduledSubscriptions.size() == 1
                    && wouldBeUnscheduledSubscriptions.contains(name)) {
                return titleCaseActionText + " " + name
                        + " and leave in an unscheduled status";
            }
            return titleCaseActionText + " " + name
                    + " and unschedule the others";
        case INCREASE_LATENCY:
            return "Increase the latency on " + name + " to " + requiredLatency
                    + " minutes";
        default:
            throw new IllegalArgumentException(
                    "Don't know how to handle option [" + option + "]");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Shell getShell() {
        return shell;
    }
}
