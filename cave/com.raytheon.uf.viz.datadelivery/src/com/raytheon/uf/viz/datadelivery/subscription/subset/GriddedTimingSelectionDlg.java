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

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.viz.datadelivery.common.ui.PriorityComp;
import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.IGriddedTimingSelectionDlgView;
import com.raytheon.uf.viz.datadelivery.system.SystemRuleManager;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.presenter.components.ButtonConf;
import com.raytheon.viz.ui.presenter.components.CheckBoxConf;
import com.raytheon.viz.ui.presenter.components.ListConf;

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
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GriddedTimingSelectionDlg extends CaveSWTDialog implements
        IGriddedTimingSelectionDlgView {

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

    /**
     * Constructor
     * 
     * @param parentShell
     * @param cycleTimes
     */
    protected GriddedTimingSelectionDlg(Shell parentShell,
            Set<Integer> cycleTimes, Subscription subscription) {
        super(parentShell);
        setText("Select Date/Cycle");
        this.cycleTimes = cycleTimes;
        this.subscription = subscription;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer openDlg() {
        return (Integer) this.open();
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

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        this.dateCycleList = new List(dateComp, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);
        dateCycleList.setLayoutData(gd);

        // Get latency value
        SystemRuleManager ruleManager = SystemRuleManager.getInstance();
        int latency = ruleManager.getLatency(this.subscription, cycleTimes);
        int priority = ruleManager.getPriority(this.subscription, cycleTimes);
        priorityComp = new PriorityComp(shell, latency, priority);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        int btnWidth = 70;
        gd = new GridData(btnWidth, SWT.DEFAULT);
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setLayoutData(gd);

        gd = new GridData(btnWidth, SWT.DEFAULT);
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setLayoutData(gd);
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
        preOpenCallback.run();

        shell.layout();
        shell.pack();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setLatestDataCheckBox(final CheckBoxConf checkBoxConf) {
        useLatestChk.setText(checkBoxConf.getDisplayText());
        useLatestChk.setSelection(checkBoxConf.isInitiallyChecked());
        useLatestChk.setToolTipText(checkBoxConf.getToolTipText());
        useLatestChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                checkBoxConf.getOnCheckedChangeAction().run();
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDateCycleList(ListConf dateCycleListConf) {
        GridData gd = new GridData(dateCycleListConf.getWidth(),
                dateCycleListConf.getHeight());
        dateCycleList.setItems(dateCycleListConf.getItems());
        dateCycleList.setLayoutData(gd);
        dateCycleList.setEnabled(false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setOkButton(final ButtonConf okBtnConf) {
        okBtn.setText(okBtnConf.getDisplayText());
        okBtn.setToolTipText(okBtnConf.getToolTipText());
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                okBtnConf.getOnClickAction().run();
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setCancelButton(final ButtonConf cancelBtnConf) {
        cancelBtn.setText(cancelBtnConf.getDisplayText());
        cancelBtn.setToolTipText(cancelBtnConf.getToolTipText());
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
                cancelBtnConf.getOnClickAction().run();
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setPreOpenCallback(Runnable callback) {
        this.preOpenCallback = callback;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isLatestDataEnabled() {
        return useLatestChk.getSelection();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDateCycleListEnabled() {
        this.dateCycleList.setEnabled(!this.useLatestChk.getSelection());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSelection() {
        if (dateCycleList.isEnabled()) {
            return dateCycleList.getItem(dateCycleList.getSelectionIndex());
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() {
        // not used

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void displayPopup(String title, String message) {
        DataDeliveryUtils.showMessage(getShell(), SWT.OK, title, message);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void displayErrorPopup(String title, String message) {
        DataDeliveryUtils.showMessage(shell, SWT.ERROR, title, message);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean displayOkCancelPopup(String title, String message) {
        return DataDeliveryUtils.showMessage(shell, SWT.CANCEL | SWT.OK, title,
                message) == SWT.OK;
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
    public void closeDlg() {
        close();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLatency() {
        return priorityComp.getLatencyValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getPriority() {
        return priorityComp.getPriorityIndex();
    }
}
