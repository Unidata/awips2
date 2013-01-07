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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetQuery;
import com.raytheon.uf.viz.datadelivery.common.ui.ActivePeriodComp;
import com.raytheon.uf.viz.datadelivery.common.ui.DeliveryOptionsComp;
import com.raytheon.uf.viz.datadelivery.common.ui.DurationComp;
import com.raytheon.uf.viz.datadelivery.common.ui.GroupSelectComp;
import com.raytheon.uf.viz.datadelivery.common.ui.PriorityComp;
import com.raytheon.uf.viz.datadelivery.subscription.view.ICreateSubscriptionDlgView;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.presenter.components.ButtonConf;
import com.raytheon.viz.ui.presenter.components.CheckBoxConf;
import com.raytheon.viz.ui.presenter.components.ComboBoxConf;

/**
 * The Data Delivery Create Subscription Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 9, 2012             mpduff       Initial creation.
 * Mar 29, 2012 431        jpiatt       Edit name update.
 * Jun 21, 2012 736        djohnson     Change OPERATION_STATUS to OperationStatus.
 * Jul 10, 2012 455        djohnson     Disallow creating subscriptions with empty names,
 *                                      don't pull all subscriptions to check for an existing one by name.
 * Jul 16, 2012 702        jpiatt       Modifications for group name.
 * Aug 02, 2012 955        djohnson     Type-safe registry query/responses.
 * Aug 10, 2012 1002       mpduff       Implementing dataset size estimation.
 * Aug 10, 2012 1022       djohnson     {@link DataSetQuery} requires provider name, set NO_GROUP if user doesn't specify a group.
 * Aug 21, 2012 712        mpduff       Add registry ID to subscription objects at creation time
 * Aug 29, 2012 223        mpduff       Add Cycle times for gridded data.  Made to follow MVP pattern.
 * Sep 17, 2012 223        mpduff       Add wait cursor to ok button click.
 * Oct  3, 2012 1103       jpiatt       Changed label.
 * Nov 20, 2012 1286       djohnson     Implement IDisplay to display yes/no prompt.
 * Dec 13, 2012 1391       bgonzale     Added cancel/ok selection status.
 * Jan 02, 2013 1441       djohnson     Add isGroupSelected().
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class CreateSubscriptionDlg extends CaveSWTDialog implements
        ICreateSubscriptionDlgView {

    /** The Main Composite */
    private Composite mainComp;

    /** The Subscription Group Information Composite */
    private GroupSelectComp groupSelectComp;

    /** The Subscription Delivery Options Composite */
    private DeliveryOptionsComp deliverComp;

    /** The Subscription Duration Composite */
    private DurationComp durComp;

    /** The Subscription Duration Composite */
    private ActivePeriodComp activePeriodComp;

    /** The Subscription Duration Composite */
    private PriorityComp priorityComp;

    /** Description text field */
    private Text descNameTxt;

    /** Change reason text field */
    private Text changeReasonTxt;

    /** Subscription Name text field */
    private Text subNameTxt;

    /** Subscription Name label */
    private Label editSubNameTxt;

    /** Create/edit flag */
    private final boolean create;

    /** OK button */
    private Button okBtn;

    /** Hour button */
    private Button[] hourBtnArr;

    /** Cycle composite */
    private Composite cycleComp;

    /** Open callback */
    private Runnable preOpenCallback;

    /** Deselect All button */
    private Button deselectAllBtn;

    /** Select All button */
    private Button selectAllBtn;

    /** Did the user Status.OK or SWT.CANCEL subscription creation */
    private int status = SWT.NONE;

    /**
     * Constructor.
     *
     * @param parent
     *            The parent shell
     * @param create
     *            true for new subscription, false for edit
     */
    public CreateSubscriptionDlg(Shell parent, boolean create) {
        super(parent, SWT.DIALOG_TRIM, CAVE.INDEPENDENT_SHELL
                | CAVE.PERSPECTIVE_INDEPENDENT);
        this.create = create;

        if (create) {
            setText("Create Subscription");
        } else {
            setText("Edit Subscription");
        }
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
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        createSubscriptionInfoGroup();

        groupSelectComp = new GroupSelectComp(mainComp, true);
        deliverComp = new DeliveryOptionsComp(mainComp);

        durComp = new DurationComp(mainComp);
        activePeriodComp = new ActivePeriodComp(mainComp);
        priorityComp = new PriorityComp(mainComp);

        this.createCycleGroup();
        if (create == false) {
            createChangeText();
        }

        createButtons();
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

    /**
     * Create the Subscription Information Group
     */
    private void createSubscriptionInfoGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Group subInfoGroup = new Group(mainComp, SWT.NONE);
        subInfoGroup.setLayout(gl);
        subInfoGroup.setLayoutData(gd);
        subInfoGroup.setText("  Subscription Information  ");

        Label subName = new Label(subInfoGroup, SWT.NONE);
        subName.setText("Name: ");

        // If in Edit mode do not allow Subscription Name to be changed
        if (create) {
            subNameTxt = new Text(subInfoGroup, SWT.BORDER);
            subNameTxt.setLayoutData(new GridData(250, SWT.DEFAULT));
        } else {
            editSubNameTxt = new Label(subInfoGroup, SWT.BORDER);
            editSubNameTxt.setLayoutData(new GridData(250, SWT.DEFAULT));
        }

        Label descName = new Label(subInfoGroup, SWT.NONE);
        descName.setText("Description: ");

        descNameTxt = new Text(subInfoGroup, SWT.BORDER);
        descNameTxt.setLayoutData(new GridData(250, SWT.DEFAULT));

    }

    private void createChangeText() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group reasonGroup = new Group(mainComp, SWT.NONE);
        reasonGroup.setLayout(gl);
        reasonGroup.setLayoutData(gd);
        reasonGroup.setText("  Reason for Change  ");

        Label descName = new Label(reasonGroup, SWT.NONE);
        descName.setText("Reason for Requesting Change: ");

        changeReasonTxt = new Text(reasonGroup, SWT.BORDER);
        changeReasonTxt.setLayoutData(new GridData(375, SWT.DEFAULT));
    }

    /**
     * Create the bottom buttons
     */
    private void createButtons() {
        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);
        Composite btnComp = new Composite(mainComp, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        okBtn = new Button(btnComp, SWT.PUSH);
        okBtn.setLayoutData(btnData);

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                status = SWT.CANCEL;
                close();
            }
        });
    }

    @Override
    public void createCycleGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group cycleGroup = new Group(mainComp, SWT.NONE);
        cycleGroup.setLayout(gl);
        cycleGroup.setLayoutData(gd);
        cycleGroup.setText("  Model Cycle Times  ");

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite selectAllComp = new Composite(cycleGroup, SWT.NONE);
        selectAllComp.setLayout(gl);
        selectAllComp.setLayoutData(gd);

        int width = 95;
        GridData btnData = new GridData(width, SWT.DEFAULT);
        selectAllBtn = new Button(selectAllComp, SWT.PUSH);
        selectAllBtn.setLayoutData(btnData);

        btnData = new GridData(width, SWT.DEFAULT);
        deselectAllBtn = new Button(selectAllComp, SWT.PUSH);
        deselectAllBtn.setLayoutData(btnData);

        gl = new GridLayout(8, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        cycleComp = new Composite(cycleGroup, SWT.NONE);
        cycleComp.setLayout(gl);
        cycleComp.setLayoutData(gd);
    }

    @Override
    protected void preOpened() {
        preOpenCallback.run();

        shell.layout();
        shell.pack();
    }

    @Override
    public void openDlg() {
        this.open();
    }

    @Override
    public int getDeliverySelection() {
        return this.deliverComp.getDeliverSetting();
    }

    @Override
    public String getSubscriptionName() {
        if (create) {
            return this.subNameTxt.getText().trim();
        } else {
            return this.editSubNameTxt.getText();
        }
    }

    @Override
    public void setSubscriptionName(String subscriptionName) {
        if (subscriptionName != null) {
            if (create) {
                this.subNameTxt.setText(subscriptionName);
            } else {
                this.editSubNameTxt.setText(subscriptionName);
            }
        }
    }

    @Override
    public String getSubscriptionDescription() {
        return this.descNameTxt.getText().trim();
    }

    @Override
    public void setSubscriptionDescription(String subscriptionDescription) {
        descNameTxt.setText(subscriptionDescription);
    }

    @Override
    public String getGroupName() {
        return this.groupSelectComp.getGroupName();
    }

    @Override
    public void setDeliverySelection(int idx) {
        deliverComp.setDeliverSetting(idx);
    }

    @Override
    public boolean isNoExpirationDate() {
        return this.durComp.isIndefiniteChk();
    }

    @Override
    public void setNoExpiration(boolean noExpiration) {
        durComp.setNoExpiration(noExpiration);
    }

    @Override
    public String getStartText() {
        return this.durComp.getStartText();
    }

    @Override
    public void setStartDate(Date startDate) {
        durComp.setStartDate(startDate);
    }

    @Override
    public String getExpirationText() {
        return this.durComp.getEndText();
    }

    @Override
    public void setExpirationDate(Date expDate) {
        durComp.setEndDate(expDate);
    }

    @Override
    public boolean isAlwaysActive() {
        return this.activePeriodComp.isAlwaysChk();
    }

    @Override
    public void setAlwaysActive(boolean active) {
        activePeriodComp.setAlwaysActive(active);
    }

    @Override
    public String getActiveStartText() {
        return this.activePeriodComp.getActiveStartText();
    }

    @Override
    public void setActiveStartDate(Date activeStartDate) {
        activePeriodComp.setStartDate(activeStartDate);
    }

    @Override
    public String getActiveEndText() {
        return this.activePeriodComp.getActiveEndText();
    }

    @Override
    public void setActiveEndDate(Date activeEndDate) {
        activePeriodComp.setEndDate(activeEndDate);
    }

    @Override
    public int getPriority() {
        return priorityComp.getPriorityIndex();
    }

    @Override
    public void setPriority(int i) {
        priorityComp.setPriorityIndex(i);
    }

    @Override
    public void setGroupName(String groupName) {
        groupSelectComp.setGroupName(groupName);
    }

    @Override
    public void setSubscriptionDatesEnabled(boolean enabled) {
        this.durComp.resetTextBoxes(enabled);
    }

    @Override
    public void setStartDateBtnEnabled(boolean enabled) {
        durComp.setStartBtnEnabled(enabled);
    }

    @Override
    public void setEndDateBtnEnabled(boolean enabled) {
        durComp.setEndBtnEnabled(enabled);
    }

    @Override
    public void setActiveDatesEnabled(boolean enabled) {
        this.activePeriodComp.resetTextBoxes(enabled);
    }

    @Override
    public void setActiveEndDateBtnEnabled(boolean enabled) {
        this.activePeriodComp.setEndBtnEnabled(enabled);
    }

    @Override
    public void setActiveStartDateBtnEnabled(boolean enabled) {
        this.activePeriodComp.setStartBtnEnabled(enabled);
    }

    @Override
    public void setDeliveryOptionsComboConf(ComboBoxConf deliveryCombo) {
        this.deliverComp.setDeliveryConfig(deliveryCombo);
    }

    @Override
    public void setDeliveryOptions(String[] deliveryOptions) {
        this.deliverComp.setDeliveryOptions(deliveryOptions);
    }

    @Override
    public void setOkConf(final ButtonConf okConf) {
        okBtn.setText(okConf.getDisplayText());
        okBtn.setEnabled(okConf.isEnabled());
        okBtn.setToolTipText(okConf.getToolTipText());
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                status = Status.OK;
                getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                okConf.getOnClickAction().run();
                if (!getShell().isDisposed()) {
                    getShell().setCursor(null);
                }
            }
        });

    }

    @Override
    public boolean isCreate() {
        return this.create;
    }

    @Override
    public void selectAllSubscriptionName() {
        this.subNameTxt.selectAll();
    }

    @Override
    public String getChangeReason() {
        return this.changeReasonTxt.getText().trim();
    }

    @Override
    public void closeDlg() {
        this.close();
    }

    @Override
    public void init() {
    }

    @Override
    public void displayPopup(String title, String message) {
        DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                title, message);
    }

    @Override
    public boolean displayOkCancelPopup(String title, String message) {
        return DataDeliveryUtils.showMessage(shell, SWT.CANCEL | SWT.OK, title,
                message) == SWT.OK;
    }

    @Override
    public void displayErrorPopup(String title, String message) {
        DataDeliveryUtils.showMessage(shell, SWT.ERROR,
                title, message);
    }

    @Override
    public List<Integer> getCycleTimes() {
        ArrayList<Integer> cycleList = new ArrayList<Integer>();
        for (Button b : hourBtnArr) {
            if (b.getSelection()) {
                cycleList.add(Integer.parseInt(b.getText()));
            }
        }

        return cycleList;
    }

    @Override
    public void setCycleConf(List<CheckBoxConf> checkboxConfList) {
        hourBtnArr = new Button[checkboxConfList.size()];
        if (checkboxConfList.isEmpty()) {
            Label lbl = new Label(cycleComp, SWT.BOLD);
            lbl.setText("Dataset does not have cycles.");
        } else {
            int i = 0;
            for (CheckBoxConf cb : checkboxConfList) {
                Button btn = new Button(cycleComp, SWT.CHECK);
                btn.setText(cb.getDisplayText());
                btn.setToolTipText(cb.getToolTipText());
                hourBtnArr[i] = btn;
                i++;
            }
        }
    }

    @Override
    public void setSelectAllButton(ButtonConf selectAllConf) {
        selectAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (Button btn : hourBtnArr) {
                    btn.setSelection(true);
                }
            }
        });
        selectAllBtn.setText(selectAllConf.getDisplayText());
        selectAllBtn.setToolTipText(selectAllConf.getToolTipText());
        selectAllBtn.setEnabled(selectAllConf.isEnabled());
    }

    @Override
    public void setDeselectAllButton(ButtonConf deselectAllConf) {
        deselectAllBtn.setSelection(true);
        deselectAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (Button btn : hourBtnArr) {
                    btn.setSelection(false);
                }
            }
        });
        deselectAllBtn.setText(deselectAllConf.getDisplayText());
        deselectAllBtn.setToolTipText(deselectAllConf.getToolTipText());
        deselectAllBtn.setEnabled(deselectAllConf.isEnabled());
    }

    @Override
    public void setDateTxtFieldsEnabled(boolean flag) {
        this.durComp.resetTextBoxes(flag);
    }

    @Override
    public void setActiveTextFieldsEnabled(boolean flag) {
        this.activePeriodComp.resetTextBoxes(flag);
    }

    @Override
    public void selectCycles(List<String> cycleStrings) {
        for (Button b: this.hourBtnArr) {
            if (cycleStrings.contains(b.getText())) {
                b.setSelection(true);
            }
        }
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
    public boolean displayYesNoPopup(String title, String message) {
        return DataDeliveryUtils.showYesNoMessage(shell, title, message) == SWT.YES;
    }

    /**
     * @return the status
     */
    @Override
    public int getStatus() {
        return status;
    }

    /**
     * @param status
     *            the status to set
     */
    @Override
    public void setStatus(int status) {
        this.status = status;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isGroupSelected() {
        return groupSelectComp.isGroupSelected();
    }
}
