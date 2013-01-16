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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.presenter.IDisplay;

/**
 * System Management Main Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012   730      jpiatt      Initial creation.
 * Oct 23, 2012 1286       djohnson    Hook into bandwidth management.
 * Nov 20, 2012 1286       djohnson    Implement IDisplay.
 * Jan 04, 2013 1420       mpduff      Remove applying of rules.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemManagementDlg extends CaveSWTDialog implements IDisplay,
        IForceApplyPromptDisplayText, IRulesUpdateListener {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SystemManagementDlg.class);

    /** Enumeration to use for List of Data Routes */
    public static enum DataRoutes {
        /** Operations Network */
        OPSNET("OpsNet");

        /** Data Route */
        private final String dataRoute;

        private DataRoutes(String dataRoute) {
            this.dataRoute = dataRoute;
        }

        /**
         * Get data route.
         * 
         * @return operation
         */
        public String getRoute() {
            return dataRoute;
        }

        @Override
        public String toString() {
            return dataRoute;
        }
    }

    /** Data Route combination box */
    private Combo dataRouteCombo;

    /** TabFolder object */
    private TabFolder tabFolder;

    /** Priority Tab constant */
    private final String PRIORITY_TAB = "priorityTab";

    /** Latency Tab constant */
    private final String LATENCY_TAB = "latencyTab";

    /** Routing Tab constant */
    private final String ROUTING_TAB = "routingTab";

    /** Tab text map */
    private final Map<String, String> tabTextMap = new HashMap<String, String>();

    /** Apply button */
    private Button applyBtn;

    /** OK button */
    private Button okBtn;

    /** Available bandwidth modified flag */
    private boolean availableBandwidthModified;

    /** Available bandwidth spinner widget */
    private Spinner availBandwidthSpinner;

    /** The system latency tab */
    private SystemLatencyTab lTab;

    /** The system priority tab */
    private SystemPriorityTab pTab;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     */
    public SystemManagementDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.NONE);
        setText("Data Delivery System Management");
        SystemRuleManager.getInstance().registerAsListener(this);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;

        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {

        createRouteInfo();
        createSeparator(true);

        getTabText();
        createTabFolder();
        createSystemTabs(tabFolder);

        createSeparator(false);
        createBottomButtons();

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        super.disposed();
        SystemRuleManager.getInstance().deregisterAsListener(this);
    }

    /**
     * Create top bar route information.
     */
    private void createRouteInfo() {
        GridData gd = new GridData(400, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, true);

        gl.horizontalSpacing = 2;

        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayoutData(gd);
        topComp.setLayout(gl);

        Composite dataRouteComp = new Composite(topComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gd = new GridData(200, SWT.DEFAULT);
        dataRouteComp.setLayoutData(gd);
        dataRouteComp.setLayout(gl);

        // Data Route
        Label dataRoute = new Label(dataRouteComp, SWT.NONE);
        dataRoute.setText("Data Route:");

        gd = new GridData(100, SWT.DEFAULT);
        dataRouteCombo = new Combo(dataRouteComp, SWT.READ_ONLY);
        dataRouteCombo.setLayoutData(gd);
        dataRouteCombo.setToolTipText("Select a data route");

        dataRouteCombo.removeAll();
        DataRoutes[] dataRoutes = DataRoutes.values();
        for (DataRoutes dr : dataRoutes) {
            dataRouteCombo.add(dr.getRoute());
        }

        dataRouteCombo.select(0);

        Composite bandwithComp = new Composite(topComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gd = new GridData(250, SWT.DEFAULT);
        bandwithComp.setLayoutData(gd);
        bandwithComp.setLayout(gl);

        // Bandwidth spinner
        gd = new GridData(165, SWT.DEFAULT);
        Label availBandwith = new Label(bandwithComp, SWT.NONE);
        availBandwith.setLayoutData(gd);
        availBandwith.setText("Available Bandwidth (KB):");

        gd = new GridData(100, SWT.DEFAULT);
        final Spinner availBandwidthSpinner = new Spinner(bandwithComp,
                SWT.BORDER);
        availBandwidthSpinner.setMinimum(0);
        availBandwidthSpinner.setMaximum(10000);
        availBandwidthSpinner.setToolTipText("Select bandwidth in Kilobytes");
        final int availableBandwidth = SystemRuleManager
                .getAvailableBandwidth(Network.OPSNET);
        availBandwidthSpinner.setSelection(availableBandwidth);
        availBandwidthSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int selection = availBandwidthSpinner.getSelection();
                availableBandwidthModified = selection != availableBandwidth;
            }
        });
        this.availBandwidthSpinner = availBandwidthSpinner;
    }

    /**
     * Create dialog separator.
     */
    private void createSeparator(boolean labelCreate) {

        Label separatorBar = new Label(shell, SWT.SEPARATOR | SWT.SHADOW_OUT
                | SWT.HORIZONTAL);
        separatorBar.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        if (labelCreate) {
            Label opsLabel = new Label(shell, SWT.NONE);
            opsLabel.setLayoutData(new GridData(
                    GridData.HORIZONTAL_ALIGN_CENTER));
            String route = dataRouteCombo.getItem(dataRouteCombo
                    .getSelectionIndex());
            opsLabel.setText(route);
        }

    }

    /**
     * Create tabs.
     */
    private void createSystemTabs(TabFolder tabFolder) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        // Priority Tab
        TabItem priorityTab = new TabItem(tabFolder, SWT.NONE);
        priorityTab.setText(tabTextMap.get(PRIORITY_TAB));
        priorityTab.setData("valid", false);
        Composite priorityComp = new Composite(tabFolder, SWT.NONE);
        priorityComp.setLayout(gl);
        priorityComp.setLayoutData(gd);
        priorityTab.setControl(priorityComp);
        pTab = new SystemPriorityTab(priorityComp);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);

        // Latency Tab
        TabItem latencyTab = new TabItem(tabFolder, SWT.NONE);
        latencyTab.setText(tabTextMap.get(LATENCY_TAB));
        latencyTab.setData("valid", false);
        Composite latencyComp = new Composite(tabFolder, SWT.NONE);
        latencyComp.setLayout(gl);
        latencyComp.setLayoutData(gd);
        latencyTab.setControl(latencyComp);
        lTab = new SystemLatencyTab(latencyComp);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);

        // Routing Tab
        TabItem routingTab = new TabItem(tabFolder, SWT.NONE);
        routingTab.setText(tabTextMap.get(ROUTING_TAB));
        Composite routingComp = new Composite(tabFolder, SWT.NONE);
        routingComp.setLayout(gl);
        routingComp.setLayoutData(gd);
        routingTab.setControl(routingComp);
        SystemRoutingTab rTab = new SystemRoutingTab(routingComp);

        lTab.loadList();
        pTab.loadList();
    }

    /**
     * Create the bottom buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        // OK Button
        int btnWidth = 100;
        GridData btnData = new GridData(btnWidth, SWT.DEFAULT);
        okBtn = new Button(bottomComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (handleOK()) {
                    close();
                }
            }
        });

        // Apply button
        btnData = new GridData(btnWidth, SWT.DEFAULT);
        applyBtn = new Button(bottomComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(btnData);
        applyBtn.setToolTipText("Apply system changes");
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOK();
            }

        });

        // Cancel Button
        btnData = new GridData(btnWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(bottomComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

    }

    /**
     * Create tab folder.
     */
    private void createTabFolder() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        tabFolder = new TabFolder(shell, SWT.NONE);
        tabFolder.setLayoutData(gd);
        tabFolder.pack();

    }

    /**
     * Discover tab.
     */
    private void getTabText() {
        this.tabTextMap.put(PRIORITY_TAB, "Priority Rules");
        tabTextMap.put(LATENCY_TAB, "Latency Rules");
        tabTextMap.put(ROUTING_TAB, "Routing Rules");
    }

    /**
     * Apply all rules to all subscriptions.
     */
    private boolean handleOK() {
        if (availableBandwidthModified) {
            final int bandwidth = availBandwidthSpinner.getSelection();
            // TODO: Currently only OPSNET is available, change to be inspecific
            // when others are added
            Set<Subscription> unscheduledSubscriptions = SystemRuleManager
                    .setAvailableBandwidth(Network.OPSNET, bandwidth);
            if (!unscheduledSubscriptions.isEmpty()) {
                Set<String> subscriptionNames = new TreeSet<String>();
                for (Subscription subscription : unscheduledSubscriptions) {
                    subscriptionNames.add(subscription.getName());
                }

                StringBuilder sb = new StringBuilder(
                        StringUtil
                                .createMessage(
                                        "Changing the default bandwidth for route "
                                                + DataRoutes.OPSNET
                                                + " will unschedule the following subscriptions:",
                                        subscriptionNames));
                sb.append(StringUtil.NEWLINE).append(StringUtil.NEWLINE);
                sb.append("Would you like to change the bandwidth anyways?.");
                int response = DataDeliveryUtils.showMessage(getShell(),
                        SWT.YES | SWT.NO, "Bandwidth Amount", sb.toString());
                if (response == SWT.YES) {
                    boolean forceApplied = SystemRuleManager
                            .forceSetAvailableBandwidth(Network.OPSNET,
                                    bandwidth);
                    if (!forceApplied) {
                        statusHandler
                                .handle(Priority.ERROR,
                                        "Bandwidth Change",
                                        "Unable to change the bandwidth for network "
                                                + Network.OPSNET
                                                + ".  Please check the server for details.");
                        return false;
                    }
                }
                return false;
            }
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean displayYesNoPopup(String title, String message) {
        return DataDeliveryUtils.showYesNoMessage(shell, title, message) == SWT.YES;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getOptionDisplayText(ForceApplyPromptResponse option,
            int requiredLatency, Subscription subscription,
            Set<String> wouldBeUnscheduledSubscriptions) {
        switch (option) {
        case CANCEL:
            return "Do not update the rules";
        case FORCE_APPLY:
            if (wouldBeUnscheduledSubscriptions.size() == 1) {
                return "Update the rules and unschedule "
                        + wouldBeUnscheduledSubscriptions.iterator().next();
            }
            return "Update the rules and unschedule the subscriptions";
        case INCREASE_LATENCY:
            // Signifies it should not be an option
            return null;
        default:
            throw new IllegalArgumentException(
                    "Don't know how to handle option [" + option + "]");
        }
    }

    @Override
    public void update() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (!shell.isDisposed()) {
                    lTab.loadList();
                    pTab.loadList();
                }
            }
        });
    }
}
