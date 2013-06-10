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

import java.util.List;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;

/**
 * System Management Main Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012   730       jpiatt     Initial creation.
 * Oct 03, 2012  1241       djohnson   Use {@link DataDeliveryPermission} and registry handlers.
 * Jan 04, 2012  1420       mpduff     Add delete rule function.
 * Jan 14, 2013  1286       djohnson   Rule list is single item selectable.
 * May 17, 2013  2000       djohnson   Consolidate duplicate code into {@link SystemRulesTab}.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemLatencyTab extends SystemRulesTab {

    /** Rule type constant */
    private static final String LATENCY_TYPE = "latency";

    /**
     * Constructor.
     * 
     * @param parentComp
     *            the parent comp
     */
    public SystemLatencyTab(Composite parentComp) {
        super(parentComp, LATENCY_TYPE);
    }

    /**
     * Get the tab text.
     * 
     * @return the tab text
     */
    @Override
    public String getTabText() {
        return "Latency Rules";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected List<String> getRuleNames() {
        return SystemRuleManager.getInstance().getLatencyRuleNames();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void deleteRule(String ruleName) {
        SystemRuleManager.getInstance().deleteLatencyRule(ruleName);
    }
}