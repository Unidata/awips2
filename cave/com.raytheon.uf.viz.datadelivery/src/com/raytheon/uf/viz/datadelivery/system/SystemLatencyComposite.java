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

/**
 * System Latency Rules composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2013    2180    mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class SystemLatencyComposite extends SystemRulesComposite {

    /**
     * Constructor
     * 
     * @param parent
     *            Parent Composite
     * @param style
     *            Style bits
     */
    public SystemLatencyComposite(Composite parent, int style) {
        super(parent, style, "latency");
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