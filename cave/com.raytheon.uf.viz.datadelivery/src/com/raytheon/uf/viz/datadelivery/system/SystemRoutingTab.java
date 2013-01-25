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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * System Management Routing Tab.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012   730       jpiatt     Initial creation.
 * Jan 04, 2013  1420       mpduff     Remove autoApply of rules.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemRoutingTab {

    /** Parent Composite */
    private final Composite parentComp;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The Composite holding these controls
     */
    public SystemRoutingTab(Composite parentComp) {
        this.parentComp = parentComp;
        init();
    }

    /**
     * Initialize the tab.
     */
    private void init() {

        createRoutingRulesTab();

    }

    /**
     * Create the bar that may be expanded depending on item count.
     */
    private void createRoutingRulesTab() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Composite priorityTabComp = new Composite(parentComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        priorityTabComp.setLayoutData(gd);
        priorityTabComp.setLayout(gl);

        // Data Route Combo Box
        Label routeHelp = new Label(priorityTabComp, SWT.NONE);
        routeHelp
                .setText("This tab allows rules to default\nproducts to a specific route.");

    }

}
