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

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
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
 * Jan 17, 2013 1501       djohnson    Close the dialog when force apply occurs, 
 *                                     and check whether changes have already been applied when OK is pressed.
 * May 17, 2013 2000       djohnson    Move bandwidth configuration into its own tab, add subscription overlap rules.
 * Jul 16, 2013 1655       mpduff      Add system status tab.
 * Aug 08, 2013 2180       mpduff      Redesigned UI.
 * Oct 03, 2013 2386       mpduff      Implemented multiple data types for overlap rules
 * Nov 19, 2013 2387       skorolev    Add timer for status refresh.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemManagementDlg extends CaveSWTDialog implements IDisplay,
        IForceApplyPromptDisplayText, IRulesUpdateListener {

    private enum SystemManagementSettings {
        PRIORITY_RULES("Priority Rules"), LATENCY_RULES("Latency Rules"), SUBSCRIPTION_RULES(
                "Subscription Rules"), GRID_SUBSCRIPTION_RULES(
                "Grid Subscription Rules"), POINT_SUBSCRIPTION_RULES(
                "Point Subscription Rules"), BANDWIDTH("Bandwidth"), DATA_PROVIDER_PASSWORD(
                "Data Provider Password"), REGISTRY_PROVIDER_STATUS(
                "Registry/Provider Status");

        private final String name;

        private SystemManagementSettings(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    /**
     * The Stack Layout.
     */
    private final StackLayout stackLayout = new StackLayout();

    /**
     * The rule stack.
     */
    private final StackLayout ruleStackLayout = new StackLayout();

    /**
     * Tree Composite
     */
    private Composite treeComp;

    /** Tree */
    private Tree tree;

    /** Priority rules composite */
    private SystemPriorityComposite systemPriorityComp;

    /** System status composite */
    private StatusComposite systemStatusComp;

    /** Latency rules composite */
    private SystemLatencyComposite systemLatencyComp;

    /** Bandwith settings composite */
    private BandwidthComposite bandwidthComp;

    /** Composite map */
    private final Map<String, Composite> compMap = new HashMap<String, Composite>();

    /** Close button */
    private Button closeBtn;

    /** Stack Composite */
    private Composite stackComp;

    /** Data Provider username password composite */
    private DataProviderPasswordComposite passwdComp;

    /** Subscription rules definition composite */
    private SubscriptionRuleDefinitionComposite subComp;

    /** Subscription rules composite for Gridded rules */
    private SubscriptionComposite griddedSubRuleComp;

    /** Subscription rules composite for Point rules */
    private SubscriptionComposite pointSubRuleComp;

    /** Rule stack composite */
    private Composite ruleStack;

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
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
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
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 220;
        treeComp = new Composite(comp, SWT.BORDER);
        treeComp.setLayout(gl);
        treeComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tree = new Tree(treeComp, SWT.NO_SCROLL);
        tree.setLayout(gl);
        tree.setLayoutData(gd);
        populateTree();

        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite c = new Composite(comp, SWT.BORDER);
        c.setLayout(gl);
        c.setLayoutData(gd);

        stackComp = new Composite(c, SWT.NONE);
        stackComp.setLayout(stackLayout);

        ruleStack = new Composite(c, SWT.NONE);
        ruleStack.setLayout(ruleStackLayout);

        createComposites();
        createBottomButtons();

        systemStatusComp.createTimer();

        comp.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                systemStatusComp.timer.shutdown();
                SystemRuleManager.getInstance().deregisterAsRefreshListener(
                        systemStatusComp);
            }
        });
    }

    /**
     * Populate the tree.
     */
    private void populateTree() {
        tree.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleSelection(event);
            }
        });

        final TreeItem ruleNode = new TreeItem(tree, 0);
        ruleNode.setText("Rules");

        TreeItem priorityRule = new TreeItem(ruleNode, 0);
        priorityRule.setText(SystemManagementSettings.PRIORITY_RULES.getName());

        TreeItem latencyRule = new TreeItem(ruleNode, 1);
        latencyRule.setText(SystemManagementSettings.LATENCY_RULES.getName());

        TreeItem subscriptionRuleNode = new TreeItem(ruleNode, 2);
        subscriptionRuleNode
                .setText(SystemManagementSettings.SUBSCRIPTION_RULES.getName());

        TreeItem gridSubscriptionRule = new TreeItem(subscriptionRuleNode, 0);
        gridSubscriptionRule
                .setText(SystemManagementSettings.GRID_SUBSCRIPTION_RULES
                        .getName());

        TreeItem pointSubscriptionRule = new TreeItem(subscriptionRuleNode, 1);
        pointSubscriptionRule
                .setText(SystemManagementSettings.POINT_SUBSCRIPTION_RULES
                        .getName());

        TreeItem settingsNode = new TreeItem(tree, 1);
        settingsNode.setText("Settings");

        TreeItem bandwidthSetting = new TreeItem(settingsNode, 0);
        bandwidthSetting.setText(SystemManagementSettings.BANDWIDTH.getName());

        TreeItem providerPassword = new TreeItem(settingsNode, 1);
        providerPassword
                .setText(SystemManagementSettings.DATA_PROVIDER_PASSWORD
                        .getName());

        TreeItem statusNode = new TreeItem(tree, 2);
        statusNode.setText("Status");

        TreeItem registryProviderStatus = new TreeItem(statusNode, 0);
        registryProviderStatus
                .setText(SystemManagementSettings.REGISTRY_PROVIDER_STATUS
                        .getName());
    }

    /**
     * Handle tree selection
     * 
     * @param event
     *            The selection event
     */
    private void handleSelection(SelectionEvent event) {
        TreeItem item = (TreeItem) event.item;

        if (item.getText().equals(
                SystemManagementSettings.POINT_SUBSCRIPTION_RULES.getName())) {
            ruleStackLayout.topControl = compMap
                    .get(SystemManagementSettings.POINT_SUBSCRIPTION_RULES
                            .getName());
            ruleStack.layout();
        } else if (item.getText().equals(
                SystemManagementSettings.GRID_SUBSCRIPTION_RULES.getName())) {
            ruleStackLayout.topControl = compMap
                    .get(SystemManagementSettings.GRID_SUBSCRIPTION_RULES
                            .getName());
            ruleStack.layout();
        }

        for (String key : compMap.keySet()) {
            if (item.getText().equals(key)) {
                stackLayout.topControl = compMap.get(key);
                stackComp.layout();
                return;
            }
        }

        item.setExpanded(!item.getExpanded());
    }

    /**
     * Create the composites that make up the stack
     */
    private void createComposites() {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        systemStatusComp = new StatusComposite(stackComp, SWT.NONE);
        systemStatusComp.setLayout(gl);
        systemStatusComp.setLayoutData(gd);
        compMap.put(
                SystemManagementSettings.REGISTRY_PROVIDER_STATUS.getName(),
                systemStatusComp);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        bandwidthComp = new BandwidthComposite(stackComp, SWT.NONE);
        bandwidthComp.setLayout(gl);
        bandwidthComp.setLayoutData(gd);
        compMap.put(SystemManagementSettings.BANDWIDTH.getName(), bandwidthComp);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        systemLatencyComp = new SystemLatencyComposite(stackComp, SWT.NONE);
        systemLatencyComp.setLayout(gl);
        systemLatencyComp.setLayoutData(gd);
        compMap.put(SystemManagementSettings.LATENCY_RULES.getName(),
                systemLatencyComp);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        systemPriorityComp = new SystemPriorityComposite(stackComp, SWT.NONE);
        systemPriorityComp.setLayout(gl);
        systemPriorityComp.setLayoutData(gd);
        compMap.put(SystemManagementSettings.PRIORITY_RULES.getName(),
                systemPriorityComp);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        passwdComp = new DataProviderPasswordComposite(stackComp, SWT.NONE);
        passwdComp.setLayout(gl);
        passwdComp.setLayoutData(gd);
        compMap.put(SystemManagementSettings.DATA_PROVIDER_PASSWORD.getName(),
                passwdComp);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        subComp = new SubscriptionRuleDefinitionComposite(stackComp);
        subComp.setLayout(gl);
        subComp.setLayoutData(gd);
        compMap.put(SystemManagementSettings.SUBSCRIPTION_RULES.getName(),
                subComp);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        pointSubRuleComp = new PointSubscriptionRuleComposite(stackComp);
        pointSubRuleComp.setLayout(gl);
        pointSubRuleComp.setLayoutData(gd);
        compMap.put(
                SystemManagementSettings.POINT_SUBSCRIPTION_RULES.getName(),
                pointSubRuleComp);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        griddedSubRuleComp = new GridSubscriptionRuleComposite(stackComp);
        griddedSubRuleComp.setLayout(gl);
        griddedSubRuleComp.setLayoutData(gd);
        compMap.put(SystemManagementSettings.GRID_SUBSCRIPTION_RULES.getName(),
                griddedSubRuleComp);

        TreeItem ti = tree.getItem(0);
        tree.select(ti);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        super.disposed();
        systemStatusComp.timer.shutdownNow();
        SystemRuleManager.getInstance().deregisterAsListener(this);
        SystemRuleManager.getInstance().deregisterAsRefreshListener(
                systemStatusComp);
    }

    /**
     * Create the bottom buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        // Close Button
        int btnWidth = 100;
        GridData btnData = new GridData(btnWidth, SWT.DEFAULT);
        closeBtn = new Button(bottomComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(btnData);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
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
        final boolean singleSubscription = wouldBeUnscheduledSubscriptions
                .size() == 1;
        switch (option) {
        case CANCEL:
            return "Do not update the rules";
        case FORCE_APPLY:
            if (singleSubscription) {
                return "Update the rules and unschedule "
                        + wouldBeUnscheduledSubscriptions.iterator().next();
            }
            return "Update the rules and unschedule the subscriptions";
        case INCREASE_LATENCY:
            // Signifies it should not be an option
            return null;
        case EDIT_SUBSCRIPTIONS:
            return "Edit the "
                    + ((singleSubscription) ? "subscription" : "subscriptions");
        default:
            throw new IllegalArgumentException(
                    "Don't know how to handle option [" + option + "]");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (!shell.isDisposed()) {
                    systemLatencyComp.loadList();
                    systemPriorityComp.loadList();
                    griddedSubRuleComp.loadConfiguration();
                    pointSubRuleComp.loadConfiguration();
                }
            }
        });
    }
}
