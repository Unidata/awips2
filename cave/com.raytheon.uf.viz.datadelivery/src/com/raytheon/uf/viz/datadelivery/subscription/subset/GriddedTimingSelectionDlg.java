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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import static com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.getMaxLatency;

import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.viz.datadelivery.common.ui.PriorityComp;
import com.raytheon.uf.viz.datadelivery.system.SystemRuleManager;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Gridded data/cycle selection dialog for adhoc queries.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2012   223      mpduff      Initial creation.
 * Oct 11, 2012  1263      jpiatt      Modified for cancel button
 * Nov 20, 2012  1286      djohnson    Implement displayYesNoPopup.
 * Jan 04, 2013  1420      mpduff      Add Priority Composite.
 * Jan 25, 2013  1528      djohnson    Subscription priority is now an enum.
 * Feb 26, 2013  1592      djohnson    When the shell is closed, don't submit the query.
 * Jun 04, 2013   223      mpduff      PriorityComp constructor changed.
 * Oct 11, 2013  2386      mpduff      Refactor DD Front end.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GriddedTimingSelectionDlg extends CaveSWTDialog {

    /** The Main Composite */
    private Composite dateComp;

    /** Use latest data check button */
    private Button useLatestChk;

    /** List of dates/cycles */
    private List dateCycleList;

    /** OK button */
    private Button okBtn;

    /** Cancel button */
    private Button cancelBtn;

    /** Priority Composite */
    private PriorityComp priorityComp;

    /** Callback to the presenter at preopen */
    private Runnable preOpenCallback;

    /** The subscription object */
    private final Subscription subscription;

    /** Cycle times */
    private final Set<Integer> cycleTimes;

    private final GriddedDataSet dataset;

    private final java.util.List<String> dateList;

    /**
     * Constructor
     * 
     * @param parentShell
     * @param dataset
     * @param subscription
     * @param dateStringToDateMap
     */
    public GriddedTimingSelectionDlg(Shell parentShell, GriddedDataSet dataset,
            Subscription subscription, java.util.List<String> dateList) {
        super(parentShell);
        setText("Select Date/Cycle");
        this.cycleTimes = dataset.getCycles();
        this.subscription = subscription;
        this.dataset = dataset;
        this.dateList = dateList;
    }

    /**
     * Open the dialog.
     * 
     * @return the selection
     */
    public GriddedTimeSelection openDlg() {
        return (GriddedTimeSelection) this.open();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        dateComp = new Composite(shell, SWT.NONE);
        dateComp.setLayout(gl);
        dateComp.setLayoutData(gd);

        useLatestChk = new Button(dateComp, SWT.CHECK);
        useLatestChk.setLayoutData(gd);
        useLatestChk.setText("Get Latest Data");
        useLatestChk.setSelection(true);
        useLatestChk.setToolTipText("Use the latest time");
        useLatestChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                dateCycleList.setEnabled(!useLatestChk.getSelection());
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 150;
        this.dateCycleList = new List(dateComp, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);
        dateCycleList.setLayoutData(gd);
        dateCycleList.setEnabled(false);

        // Get latency value
        SystemRuleManager ruleManager = SystemRuleManager.getInstance();

        int latency = ruleManager.getLatency(this.subscription, cycleTimes);
        SubscriptionPriority priority = ruleManager.getPriority(
                this.subscription, cycleTimes);
        priorityComp = new PriorityComp(shell, latency, priority, false);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        int btnWidth = 70;
        gd = new GridData(btnWidth, SWT.DEFAULT);
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setLayoutData(gd);
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOk();
                close();
            }
        });

        gd = new GridData(btnWidth, SWT.DEFAULT);
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setLayoutData(gd);
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                GriddedTimeSelection gts = new GriddedTimeSelection();
                gts.setCancel(true);
                setReturnValue(gts);
                close();
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;

        return mainLayout;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void preOpened() {
        populate();
        shell.layout();
        shell.pack();
    }

    /**
     * Check if the latest data checkbox is enabled.
     * 
     * @return true if enabled.
     */
    public boolean isLatestDataEnabled() {
        return useLatestChk.getSelection();
    }

    /**
     * Set the date/cycle list enabled.
     */
    public void setDateCycleListEnabled() {
        this.dateCycleList.setEnabled(!this.useLatestChk.getSelection());
    }

    private void populate() {
        for (String date : this.dateList) {
            dateCycleList.add(date);
        }
    }

    /**
     * OK Button action method.
     */
    private void handleOk() {
        GriddedTimeSelection data = new GriddedTimeSelection();
        if (!isLatestDataEnabled()) {
            String selection = dateCycleList.getItem(dateCycleList
                    .getSelectionIndex());
            DataDeliveryGUIUtils.latencyValidChk(
                    priorityComp.getLatencyValue(), getMaxLatency(dataset));

            // parse off the date/cycle time selected
            String[] parts = selection.split(" - ");
            String selectedDate = parts[0];
            String cycleStr = parts[1];
            cycleStr = cycleStr.substring(0, cycleStr.indexOf(" Z"));
            data.setCycle(Integer.parseInt(cycleStr));
            data.setDate(selection);
        } else {
            data.setCycle(-999);
            data.setLatest(true);
        }

        setReturnValue(data);
    }
}
