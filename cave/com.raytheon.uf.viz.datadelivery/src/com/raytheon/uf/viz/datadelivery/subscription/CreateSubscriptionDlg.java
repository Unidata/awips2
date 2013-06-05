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
import java.util.Set;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetQuery;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datadelivery.common.ui.ActivePeriodComp;
import com.raytheon.uf.viz.datadelivery.common.ui.DurationComp;
import com.raytheon.uf.viz.datadelivery.common.ui.GroupSelectComp;
import com.raytheon.uf.viz.datadelivery.common.ui.PriorityComp;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.view.ICreateSubscriptionDlgView;
import com.raytheon.uf.viz.datadelivery.system.SystemRuleManager;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.presenter.components.ButtonConf;
import com.raytheon.viz.ui.presenter.components.CheckBoxConf;

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
 * Jan 04, 2013 1420       mpduff       Add latency.
 * Jan 25, 2013 1528       djohnson    Use priority enum instead of raw integers.
 * Apr 08, 2013 1826       djohnson    Remove delivery options.
 * May 15, 2013 1040       mpduff      Add Shared sites.
 * Jun 04, 2013  223       mpduff       Modify for point data.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class CreateSubscriptionDlg extends CaveSWTDialog implements
        ICreateSubscriptionDlgView {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CreateSubscriptionDlg.class);

    /** The Main Composite */
    private Composite mainComp;

    /** The Subscription Group Information Composite */
    private GroupSelectComp groupSelectComp;

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

    /** The subscription object */
    private Subscription subscription;

    /** Available cycle times */
    private Set<Integer> cycleTimes;

    private String[] sharedSites;

    private Label selectedSiteLbl;

    private final Font font;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param create
     *            true for new subscription, false for edit
     */
    public CreateSubscriptionDlg(Shell parent, boolean create) {
        super(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.INDEPENDENT_SHELL | CAVE.PERSPECTIVE_INDEPENDENT);
        this.create = create;

        if (create) {
            setText("Create Subscription");
        } else {
            setText("Edit Subscription");
        }

        font = new Font(this.getDisplay(), "Monospace", 9, SWT.NORMAL);
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

        durComp = new DurationComp(mainComp);
        activePeriodComp = new ActivePeriodComp(mainComp);

        // Get latency value
        int latency = 15;
        SubscriptionPriority priority = SubscriptionPriority.NORMAL;
        SystemRuleManager ruleManager = SystemRuleManager.getInstance();

        if (this.subscription.getDataSetType() == DataType.GRID) {
            latency = ruleManager.getLatency(subscription, cycleTimes);
            priority = ruleManager.getPriority(subscription, cycleTimes);
            priorityComp = new PriorityComp(mainComp, latency, priority, false);
        } else if (this.subscription.getDataSetType() == DataType.POINT) {
            // For point the latency is the retrieval interval
            latency = ((PointTime) subscription.getTime()).getInterval();
            priority = ruleManager.getPointDataPriority(subscription);
            priorityComp = new PriorityComp(mainComp, latency, priority, true);
        }

        if (this.subscription.getDataSetType() == DataType.GRID) {
            this.createCycleGroup();
        }

        createSiteSelection();

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
     * Create the site selection widgets.
     */
    private void createSiteSelection() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        if (DataDeliveryConstants.PHASE3_ENABLED) {
            final Group group = new Group(mainComp, SWT.NONE);
            group.setLayout(gl);
            group.setLayoutData(gd);
            group.setText(" Shared Sites ");

            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
            gl = new GridLayout(2, false);
            final Composite c = new Composite(group, SWT.NONE);
            c.setLayout(gl);
            c.setLayoutData(gd);

            gl = new GridLayout(1, false);
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            final Button btn = new Button(c, SWT.NONE);
            btn.setLayoutData(new GridData(95, SWT.DEFAULT));
            btn.setText("Select Sites...");
            btn.setToolTipText("Select sites for sharing");
            btn.setEnabled(false);

            final DataDeliveryPermission permission = DataDeliveryPermission.SHARED_SUBSCRIPTION_CREATE;
            final IUser user = UserController.getUserObject();
            final String msg = user.uniqueId()
                    + " is not authorized to create shared subscriptions. "
                    + StringUtil.NEWLINE + "Permission: " + permission;
            try {
                if (DataDeliveryServices.getPermissionsService()
                        .checkPermission(user, msg, permission).isAuthorized()) {
                    btn.setEnabled(true);
                } else {
                    c.addMouseTrackListener(new MouseTrackAdapter() {

                        @Override
                        public void mouseExit(MouseEvent e) {
                            DataDeliveryGUIUtils.hideToolTip();
                        }

                        @Override
                        public void mouseHover(MouseEvent e) {
                            handleMouseEvent(e, msg, group.getBounds());
                        }

                        @Override
                        public void mouseEnter(MouseEvent e) {
                            handleMouseEvent(e, msg, group.getBounds());
                        }
                    });
                }
            } catch (VizException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        e1.getLocalizedMessage(), e1);
            }
            btn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    SiteSelectionDlg dlg = new SiteSelectionDlg(shell, "OAX",
                            sharedSites);
                    dlg.setCloseCallback(new ICloseCallback() {
                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof String[]) {
                                String[] sites = (String[]) returnValue;
                                processSites(sites);
                            }
                        }
                    });
                    dlg.open();
                }
            });

            selectedSiteLbl = new Label(group, SWT.BORDER);
            selectedSiteLbl.setFont(font);
            selectedSiteLbl.setText("");
            selectedSiteLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                    true, false));

            if (!create) {
                if (subscription != null
                        && subscription.getOfficeIDs().size() > 0) {
                    String[] siteArr = subscription.getOfficeIDs().toArray(
                            new String[subscription.getOfficeIDs().size()]);
                    processSites(siteArr);
                }
            }
        }
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

    /**
     * {@inheritDoc}
     */
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
     * Handle the mouse event and display the tooltip.
     * 
     * @param e
     *            MouseEvent
     * @param msg
     *            Message to display
     * @param bounds
     *            Bounds
     */
    private void handleMouseEvent(MouseEvent e, String msg, Rectangle bounds) {
        Point pos = shell.toDisplay(bounds.x + e.x + 15, bounds.y + e.y + 15);
        DataDeliveryGUIUtils.showTooltip(this.shell, pos.x, pos.y, msg);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void openDlg() {
        this.open();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        super.disposed();
        if (font != null && !font.isDisposed()) {
            font.dispose();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSubscriptionName() {
        if (create) {
            return this.subNameTxt.getText().trim();
        } else {
            return this.editSubNameTxt.getText();
        }
    }

    /**
     * {@inheritDoc}
     */
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

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSubscriptionDescription() {
        return this.descNameTxt.getText().trim();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSubscriptionDescription(String subscriptionDescription) {
        descNameTxt.setText(subscriptionDescription);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getGroupName() {
        return this.groupSelectComp.getGroupName();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isNoExpirationDate() {
        return this.durComp.isIndefiniteChk();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setNoExpiration(boolean noExpiration) {
        durComp.setNoExpiration(noExpiration);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStartText() {
        return this.durComp.getStartText();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setStartDate(Date startDate) {
        durComp.setStartDate(startDate);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getExpirationText() {
        return this.durComp.getEndText();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setExpirationDate(Date expDate) {
        durComp.setEndDate(expDate);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isAlwaysActive() {
        return this.activePeriodComp.isAlwaysChk();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setAlwaysActive(boolean active) {
        activePeriodComp.setAlwaysActive(active);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getActiveStartText() {
        return this.activePeriodComp.getActiveStartText();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setActiveStartDate(Date activeStartDate) {
        activePeriodComp.setStartDate(activeStartDate);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getActiveEndText() {
        return this.activePeriodComp.getActiveEndText();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setActiveEndDate(Date activeEndDate) {
        activePeriodComp.setEndDate(activeEndDate);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SubscriptionPriority getPriority() {
        return priorityComp.getPriority();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setPriority(SubscriptionPriority priority) {
        priorityComp.setPriority(priority);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setGroupName(String groupName) {
        groupSelectComp.setGroupName(groupName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSubscriptionDatesEnabled(boolean enabled) {
        this.durComp.resetTextBoxes(enabled);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setStartDateBtnEnabled(boolean enabled) {
        durComp.setStartBtnEnabled(enabled);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setEndDateBtnEnabled(boolean enabled) {
        durComp.setEndBtnEnabled(enabled);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setActiveDatesEnabled(boolean enabled) {
        this.activePeriodComp.resetTextBoxes(enabled);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setActiveEndDateBtnEnabled(boolean enabled) {
        this.activePeriodComp.setEndBtnEnabled(enabled);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setActiveStartDateBtnEnabled(boolean enabled) {
        this.activePeriodComp.setStartBtnEnabled(enabled);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setOkConf(final ButtonConf okConf) {
        okBtn.setText(okConf.getDisplayText());
        okBtn.setEnabled(okConf.isEnabled());
        okBtn.setToolTipText(okConf.getToolTipText());
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    status = Status.OK;
                    getShell().setCursor(
                            getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                    okConf.getOnClickAction().run();
                } finally {
                    if (!getShell().isDisposed()) {
                        getShell().setCursor(null);
                    }
                }
            }
        });

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isCreate() {
        return this.create;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void selectAllSubscriptionName() {
        this.subNameTxt.selectAll();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getChangeReason() {
        return this.changeReasonTxt.getText().trim();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void closeDlg() {
        this.close();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() {
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
    public boolean displayOkCancelPopup(String title, String message) {
        return DataDeliveryUtils.showMessage(shell, SWT.CANCEL | SWT.OK, title,
                message) == SWT.OK;
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
    public List<Integer> getCycleTimes() {
        ArrayList<Integer> cycleList = new ArrayList<Integer>();
        for (Button b : hourBtnArr) {
            if (b.getSelection()) {
                cycleList.add(Integer.parseInt(b.getText()));
            }
        }

        return cycleList;
    }

    /**
     * {@inheritDoc}
     */
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

    /**
     * {@inheritDoc}
     */
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

    /**
     * {@inheritDoc}
     */
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

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDateTxtFieldsEnabled(boolean flag) {
        this.durComp.resetTextBoxes(flag);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setActiveTextFieldsEnabled(boolean flag) {
        this.activePeriodComp.resetTextBoxes(flag);
    }

    @Override
    public void selectCycles(List<String> cycleStrings) {
        for (Button b : this.hourBtnArr) {
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
     * {@inheritDoc}
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

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLatencyValue() {
        return priorityComp.getLatencyValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSubscription(Subscription subscription) {
        this.subscription = subscription;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setCycleTimes(Set<Integer> cycleTimes) {
        this.cycleTimes = cycleTimes;
    }

    /**
     * Process the site list
     * 
     * @param sites
     *            list of sites
     */
    private void processSites(String[] sites) {
        this.sharedSites = sites;
        StringBuilder toolTipText = new StringBuilder();
        StringBuilder labelText = new StringBuilder();
        boolean overflow = false;
        for (int i = 0; i < sites.length; i++) {
            toolTipText.append(sites[i]).append(" ");
            if (i < 8) {
                labelText.append(sites[i]).append(" ");
            } else {
                overflow = true;
            }
        }
        String lt = labelText.toString().trim();
        if (!lt.isEmpty() && overflow) {
            lt = lt.concat("...");
        }

        selectedSiteLbl.setText(lt);
        selectedSiteLbl.setToolTipText(toolTipText.toString());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String[] getSharedSites() {
        return this.sharedSites;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setOfficeIds(Set<String> officeIDs) {
        List<String> list = new ArrayList<String>(officeIDs);
        this.sharedSites = list.toArray(new String[officeIDs.size()]);
    }
}
