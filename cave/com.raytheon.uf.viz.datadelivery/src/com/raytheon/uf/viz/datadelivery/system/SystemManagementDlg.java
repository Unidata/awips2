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

import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

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
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class SystemManagementDlg extends CaveSWTDialog implements IDisplay,
        IForceApplyPromptDisplayText, IRulesUpdateListener {

    /** TabFolder object */
    private TabFolder tabFolder;

    /** OK button */
    private Button closeBtn;

    /** The system latency tab */
    private SystemLatencyTab lTab;

    /** The system priority tab */
    private SystemPriorityTab pTab;

    /** The subscription tab */
    private SubscriptionTab sTab;

    /** The bandwidth tab */
    private BandwidthTab bTab;

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

        createTabFolder();
        createSystemTabs(tabFolder);

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
     * Create tabs.
     */
    private void createSystemTabs(TabFolder tabFolder) {
        // Priority Tab
        pTab = new SystemPriorityTab(getTabComposite(tabFolder));
        createTabItem(tabFolder, pTab);
        pTab.init();

        // Latency Tab
        lTab = new SystemLatencyTab(getTabComposite(tabFolder));
        createTabItem(tabFolder, lTab);
        lTab.init();

        // Subscription Tab
        sTab = new SubscriptionTab(getTabComposite(tabFolder));
        createTabItem(tabFolder, sTab);
        sTab.init();

        // Bandwidth Tab
        bTab = new BandwidthTab(getTabComposite(tabFolder));
        createTabItem(tabFolder, bTab);
        bTab.init();

        lTab.loadList();
        pTab.loadList();
    }

    /**
     * Create the tab composite.
     * 
     * @param tabFolder
     *            the
     * @return
     */
    private Composite getTabComposite(TabFolder tabFolder) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Composite composite = new Composite(tabFolder, SWT.NONE);
        composite.setLayout(gl);
        composite.setLayoutData(gd);

        return composite;
    }

    /**
     * Create a {@link TabItem} for the given tab.
     * 
     * @param tabFolder
     *            the tab folder
     * @param tab
     *            the tab
     * @return the tab item
     */
    private TabItem createTabItem(TabFolder tabFolder, SystemTab tab) {
        TabItem tabItem = new TabItem(tabFolder, SWT.NONE);
        tabItem.setControl(tab.getParentComp());
        tabItem.setText(tab.getTabText());

        return tabItem;
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
     * Create tab folder.
     */
    private void createTabFolder() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        tabFolder = new TabFolder(shell, SWT.NONE);
        tabFolder.setLayoutData(gd);
        tabFolder.pack();

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
