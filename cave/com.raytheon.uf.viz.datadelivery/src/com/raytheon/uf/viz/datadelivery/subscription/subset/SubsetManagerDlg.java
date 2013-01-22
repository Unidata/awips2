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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.datadelivery.retrieval.util.DataSizeUtils;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizAppTaskExecutor;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.filter.MetaDataManager;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.CreateSubscriptionDlg;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;
import com.raytheon.uf.viz.datadelivery.subscription.presenter.CreateSubscriptionDlgPresenter;
import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.DataTimingSubsetPresenter;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SpecificDateTimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.TimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.VerticalXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.presenter.IDisplay;

/**
 * Data Delivery Subset Manager Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2012            mpduff       Initial creation.
 * Jun 04, 2012   645      jpiatt       Added tooltips & code clean up.
 * Jun 08, 2012   700      djohnson     Always use subset name for subscribed coverage.
 * Jun 21, 2012   736      djohnson     Change OPERATION_STATUS to OperationStatus.
 * Aug 02, 2012   955      djohnson     Type-safe registry query/responses.
 * Aug 08, 2012 863        jpiatt       Added clean & dirty checks.
 * Aug 10, 2012 1002       mpduff       Implementing dataset size estimation.
 * Aug 10, 2012 1022       djohnson     {@link SubsetXML} requires provider name,  use {@link GriddedDataSet}.
 * Aug 22, 2012 0743       djohnson     Subclass for data type specific operations.
 * Aug 29, 2012 0223       mpduff       Set cycle times in new sub object if not in create mode.
 * Sep 06, 2012 1121       mpduff       Use the DataLevelType.getKey() method.
 * Sep 07, 2012 1102       djohnson     Move setting subscription group name from old subscription into null check.
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission} and registry handlers.
 * Oct  4, 2012 1245       jpiatt       Modify to reference util class & code clean up.
 * Oct 11, 2012 1221       mpduff       Set subscription's fulldataset flag.
 * Oct 31, 2012 1278       mpduff       Integrate SpatialUtils class.
 * Nov 09, 2012 1286       djohnson     Consolidate duplicate subscription handling.
 * Nov 26, 2012 1342       mpduff       Fix the closing of the dialog.
 * Nov 26, 2012 1286       djohnson     Always set registry ID on a subscription.
 * Dec 11, 2012 1405       mpduff       Move close confirmation dialog after event.doit = false.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Dec 11, 2012 1264       mpduff       Changes to AreaComp.
 * Dec 12, 2012 1391       bgonzale     Mark this dialog as busy when the create subscription
 *                                      dialog is opened. Changed handleOK() to use create subscription
 *                                      dialog status in return result.
 * Dec 17, 2012 1434       mpduff       Don't allow underscores in name.
 * Dec 18, 2012 1439       mpduff       Redo subscription name validation.
 * Jan 02, 2012 1345       djohnson     Use gui thread task executor.
 * Jan 04, 2012 1420       mpduff       Pass the subscription in to the GriddedTimingSelectionDlg.
 * Jan 10, 2013 1444       mpduff       Fix the loading of saved subsets from the saved subset tab.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public abstract class SubsetManagerDlg<DATASET extends DataSet, PRESENTER extends DataTimingSubsetPresenter<DATASET, ?, ?, TIMEXML, ?>, TIMEXML extends TimeXML>
        extends CaveSWTDialog implements ITabAction, IDataSize, IDisplay,
        IForceApplyPromptDisplayText {
    /** constant */
    private final static String DATASETS_NOT_SUPPORTED = "Datasets of type [%s] are currently not supported!";

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubsetManagerDlg.class);

    /** Subset Name text box */
    private Text nameText;

    /** Estimated size informational label */
    protected Label sizeLbl;

    /** TabFolder object */
    private TabFolder tabFolder;

    /** DataSet object */
    protected DATASET dataSet;

    /** Create a subscription dialog */
    private CreateSubscriptionDlgPresenter subDlg;

    /** Saved subset tab */
    private SavedSubsetTab subsetTab;

    /** Vertical subset tab */
    protected VerticalSubsetTab vTab;

    /**
     * The presenter class separates as much of the logic from the SWT code as
     * possible. It also allows for unit testing of the logic since SWT is
     * typically not ran headless.
     **/
    protected PRESENTER timingTabControls;

    /** Spatial subset tab */
    protected SpatialSubsetTab spatialTabControls;

    /** Subset XML file object */
    private SubsetXML<TIMEXML> subsetXml;

    /** Load dataset flag */
    private boolean loadDataSet = false;

    /** Edit flag */
    private boolean create = true;

    /** Subscription object */
    protected Subscription subscription;

    /** Utility for calculating bandwidth */
    protected DataSizeUtils dataSize = null;

    /** Dialog initialized flag */
    protected boolean initialized = false;

    /** Subset manager constant */
    private final String DD_SUBSET_MANAGER = "Data Delivery Subset Manager - ";

    private final String VERTICAL_TAB = "Vertical Levels/Parameters";

    private final String TIMING_TAB = "Forecast Hours";

    private final String SPATIAL_TAB = "Spatial";

    private final ISubscriptionService subscriptionService = DataDeliveryServices
            .getSubscriptionService();

    /**
     * Constructor
     * 
     * @param shell
     *            The parent Shell
     * @param dataSet
     *            The DataSetMetaData
     * @param loadDataSet
     *            Populate the dialog if true
     * @param subsetXml
     *            The SubsetXML object to load
     */
    public SubsetManagerDlg(Shell shell, DATASET dataSet, boolean loadDataSet,
            SubsetXML<TIMEXML> subsetXml) {
        super(shell, SWT.RESIZE | SWT.DIALOG_TRIM | SWT.MIN,
                CAVE.INDEPENDENT_SHELL);
        if (dataSet != null) {
            setText(DD_SUBSET_MANAGER + dataSet.getDataSetName());
        } else {
            setText(DD_SUBSET_MANAGER);
        }

        this.dataSet = dataSet;
        this.subsetXml = subsetXml;
        this.loadDataSet = loadDataSet;

        this.dataSize = new DataSizeUtils(dataSet);

    }

    /**
     * Constructor
     * 
     * @param shell
     *            The parent Shell
     * 
     * @param dataSet
     *            The DataSetMetaData
     */
    public SubsetManagerDlg(Shell shell, DATASET dataSet) {
        this(shell, dataSet, false, null);
    }

    /**
     * Constructor for editing a subscription.
     * 
     * @param shell
     *            The parent Shell
     * @param loadDataSet
     *            Populate the dialog if true
     * @param subscription
     *            The subscription to edit
     */
    @SuppressWarnings("unchecked")
    public SubsetManagerDlg(Shell shell, boolean loadDataSet,
            Subscription subscription) {
        super(shell, SWT.RESIZE | SWT.DIALOG_TRIM | SWT.MIN,
                CAVE.INDEPENDENT_SHELL);
        this.create = false;
        this.loadDataSet = true;
        // TODO: Is there a cleaner way to do this other than casting?
        this.dataSet = (DATASET) MetaDataManager.getInstance().getDataSet(
                subscription.getDataSetName(), subscription.getProvider());
        this.subscription = subscription;
        setText(DD_SUBSET_MANAGER + "Edit: " + subscription.getName());

        this.dataSize = new DataSizeUtils(dataSet);
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
        createInfoComp();
        createButtons();

        if (loadDataSet) {
            if (subsetXml != null) {
                loadFromSubsetXML(subsetXml);
            } else if (subscription != null) {
                loadFromSubscription(subscription);
            }
        }

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                event.doit = false;
                displayMessage();
            }
        });

        initialized = true;
        updateDataSize();
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
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /** Create the TabFolder */
    private void createTabFolder() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        tabFolder = new TabFolder(shell, SWT.NONE);
        tabFolder.setLayoutData(gd);
        createTabs(tabFolder);
        tabFolder.pack();

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#opened()
     */
    @Override
    protected void opened() {
        // Set the min size of the shell to the current
        // shell size since it has been packed before this call
        // shell.setMinimumSize(shell.getSize());
    }

    /** Create the tabs */
    private void createTabs(TabFolder tabFolder) {
        createGridTabs(tabFolder);

        TabItem savedSetsTab = new TabItem(tabFolder, SWT.NONE);
        savedSetsTab.setText("Saved Subsets");
        Composite savedSetsComp = new Composite(tabFolder, SWT.NONE);
        savedSetsTab.setControl(savedSetsComp);
        subsetTab = new SavedSubsetTab(savedSetsComp, this);
    }

    /** Create the grid tab */
    protected void createGridTabs(TabFolder tabFolder) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        TabItem verticalTab = new TabItem(tabFolder, SWT.NONE);
        verticalTab.setText(VERTICAL_TAB);
        verticalTab.setData("valid", false);
        Composite vertComp = new Composite(tabFolder, SWT.NONE);
        vertComp.setLayout(gl);
        vertComp.setLayoutData(gd);
        verticalTab.setControl(vertComp);
        vTab = new VerticalSubsetTab(vertComp, dataSet, this);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);

        TabItem timingTab = new TabItem(tabFolder, SWT.NONE);
        timingTab.setText(TIMING_TAB);
        timingTab.setData("valid", false);
        Composite timingComp = new Composite(tabFolder, SWT.NONE);
        timingComp.setLayout(gl);
        timingComp.setLayoutData(gd);
        timingTab.setControl(timingComp);
        timingTabControls = getDataTimingSubsetPresenter(timingComp, dataSet,
                this, shell);
        timingTabControls.init();

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);

        TabItem spatialTab = new TabItem(tabFolder, SWT.NONE);
        spatialTab.setText(SPATIAL_TAB);
        Composite spatialComp = new Composite(tabFolder, SWT.NONE);
        spatialComp.setLayout(gl);
        spatialComp.setLayoutData(gd);
        spatialTab.setControl(spatialComp);
        spatialTabControls = new SpatialSubsetTab(spatialComp, dataSet, this);
    }

    /**
     * Construct and return the presenter class.
     * 
     * @param shell
     * @param subsetManagerDlg
     * @param dataSet2
     * @param timingComp
     * 
     * @return the presenter instance
     */
    protected abstract PRESENTER getDataTimingSubsetPresenter(
            Composite parentComp, DATASET dataSet, IDataSize callback,
            Shell shell);

    /** Create the information composite */
    private void createInfoComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Composite subsetComp = new Composite(shell, SWT.NONE);
        subsetComp.setLayout(gl);
        subsetComp.setLayoutData(gd);

        Label sizeLabelLbl = new Label(subsetComp, SWT.NONE);
        sizeLabelLbl.setText("Est. File Size: ");

        sizeLbl = new Label(subsetComp, SWT.BORDER);
        sizeLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label nameLbl = new Label(subsetComp, SWT.NONE);
        nameLbl.setText("Subset Name: ");

        nameText = new Text(subsetComp, SWT.BORDER);
        nameText.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        nameText.setToolTipText("Enter a subset name");
        nameText.setEnabled(create);
    }

    /** Create the buttons */
    private void createButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        int buttonWidth = 87;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        Button okBtn = new Button(bottomComp, SWT.PUSH);
        if (!create) {
            okBtn.setText("Continue...");
            okBtn.setToolTipText("Click to continue editing");
        } else {
            okBtn.setText("Subscribe...");
            okBtn.setToolTipText("Click to subscribe to subset");
        }
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DataDeliveryGUIUtils.markBusyInUIThread(shell);
                if (handleOK()) {
                    close();
                } else {
                    DataDeliveryGUIUtils.markNotBusyInUIThread(shell);
                }
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button queryBtn = new Button(bottomComp, SWT.PUSH);
        queryBtn.setText("Query");
        queryBtn.setLayoutData(btnData);
        queryBtn.setToolTipText("Click to query subset data");
        queryBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleQuery();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(bottomComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayMessage();
            }
        });
    }

    /**
     * OK button action handler
     * 
     * @return true if data are valid
     */
    private boolean handleOK() {
        if (this.validated(true)) {

            Subscription sub = createSubscription(new Subscription());
            if (subDlg != null && !subDlg.isDisposed()) {
                subDlg.setSubscriptionData(sub);
                subDlg.bringToTop();
            } else {
                subDlg = new CreateSubscriptionDlgPresenter(
                        new CreateSubscriptionDlg(shell, create), dataSet,
                        create, new VizAppTaskExecutor());
                subDlg.setSubscriptionData(sub);
                subDlg.open();
            }
            return subDlg.getStatus() == Status.OK;
        }

        return false;
    }

    /**
     * Query button action handler.
     */
    private void handleQuery() {
        boolean valid = this.validated(false);

        if (valid) {

            AdhocSubscription as = createSubscription(new AdhocSubscription());
            // null means the user hit cancel on the date/cycle selection dialog
            if (as == null) {
                return;
            }
            try {
                ISubscriptionServiceResult result = subscriptionService.store(
                        as, this);

                if (result.hasMessageToDisplay()) {
                    DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                            "Query Scheduled", result.getMessageToDisplay());
                }
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error requesting adhoc data.", e);
            }
        }
    }

    /**
     * Create the subscription.
     * 
     * @param <T>
     *            The subscription object reference type
     * @param sub
     *            The subscription to populate
     * 
     * @return the populated subscription
     */
    protected <T extends Subscription> T createSubscription(T sub) {
        ArrayList<Parameter> selectedParameterObjs = vTab.getParameters();

        sub.setName(nameText.getText());
        sub.setOfficeID(LocalizationManager.getInstance().getCurrentSite());
        if (create) {
            sub.setOwner(LocalizationManager.getInstance().getCurrentUser());
            sub.setPriority(1);
        } else {
            sub.setOwner(this.subscription.getOwner());
            sub.setGroupName(this.subscription.getGroupName());
            sub.setSubscriptionEnd(this.subscription.getSubscriptionEnd());
            sub.setSubscriptionStart(this.subscription.getSubscriptionStart());
            sub.setActivePeriodEnd(this.subscription.getActivePeriodEnd());
            sub.setActivePeriodStart(this.subscription.getActivePeriodStart());
            sub.setActive(this.subscription.isActive());
            sub.setPriority(this.subscription.getPriority());
        }
        sub.setProvider(dataSet.getProviderName());
        sub.setDataSetName(dataSet.getDataSetName());
        sub.setParameter(selectedParameterObjs);
        sub.setDataSetSize(dataSize.getDataSetSize());
        sub.setDataSetType(dataSet.getDataSetType());
        Time dataSetTime = dataSet.getTime();

        Time time = dataSetTime;
        Time newTime = new Time();
        newTime.setEnd(time.getEnd());
        newTime.setFormat(time.getFormat());
        newTime.setNumTimes(time.getNumTimes());
        newTime.setRequestEnd(time.getRequestEnd());
        newTime.setRequestStart(time.getRequestStart());
        newTime.setStart(time.getStart());
        newTime.setStep(time.getStep());
        newTime.setStepUnit(time.getStepUnit());

        if (sub instanceof AdhocSubscription) {
            newTime = setupDataSpecificTime(newTime, sub);
        } else if (!create) {
            newTime.setCycleTimes(this.subscription.getTime().getCycleTimes());
        }

        // Catch the case where the user closes this dialog.
        if (this.isDisposed()) {
            return null;
        }

        if (newTime == null) {
            return null;
        }

        sub.setTime(newTime);

        // TODO Phase 1 is only gridded coverage
        GriddedCoverage cov = (GriddedCoverage) dataSet.getCoverage();
        cov.setModelName(dataSet.getDataSetName());
        cov.setGridName(nameText.getText());
        GridCoverage coverage = cov.getGridCoverage();
        coverage.setName(nameText.getText());

        if (spatialTabControls.useDataSetSize()) {
            cov.setRequestEnvelope(cov.getEnvelope());
            sub.setFullDataSet(true);
        } else {
            cov.setRequestEnvelope(spatialTabControls.getEnvelope());
            sub.setFullDataSet(false);
        }

        sub.setCoverage(cov);
        sub.setDataSetName(dataSet.getDataSetName());
        sub.setSubscriptionId("AdHocID");
        if (this.subscription != null) {
            sub.setNotify(this.subscription.isNotify());

            if (this.subscription.getDescription() != null) {
                sub.setDescription(subscription.getDescription());
            }
            sub.setGroupName(subscription.getGroupName());
        }

        sub.setId(RegistryUtil.getRegistryObjectKey(sub));

        return sub;
    }

    /**
     * Setup the timing information specific to the data type.
     */
    protected abstract Time setupDataSpecificTime(Time newTime, Subscription sub);

    /**
     * Display cancel changes message.
     */
    private void displayMessage() {
        if (!isDirty()) {
            close();
        } else {
            int choice = DataDeliveryUtils
                    .showMessage(shell, SWT.YES | SWT.NO, "Cancel Changes?",
                            "Are you sure you wish to close without saving your subset?");
            if (choice == SWT.YES) {
                close();
            }
        }
    }

    /**
     * Validate user has selected the necessary items.
     * 
     * @param subscription
     *            true for subscription, false for adhoc query
     * @return true if filled out correctly
     */
    private boolean validated(boolean subscription) {
        String name = nameText.getText();

        // Is Subset Name entered
        if ((name == null) || (name.isEmpty())) {
            DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                    DataDeliveryGUIUtils.NAME_REQUIRED_TITLE,
                    DataDeliveryGUIUtils.NAME_REQUIRED_MESSAGE);
            return false;
        }

        if (DataDeliveryGUIUtils.INVALID_CHAR_PATTERN.matcher(name.trim())
                .find()) {
            DataDeliveryUtils.showMessage(getShell(), SWT.ERROR,
                    DataDeliveryGUIUtils.INVALID_CHARS_TITLE,
                    DataDeliveryGUIUtils.INVALID_CHARS_MESSAGE);

            return false;
        }

        if (!validateArea()) {
            return false;
        }

        Collection<String> invalidTabs = getInvalidTabs();

        if (!invalidTabs.isEmpty()) {
            StringBuilder message = new StringBuilder(
                    "The following tabs do not have valid entries:\n\n");
            for (String tab : invalidTabs) {
                message.append(tab + "\n");
            }
            DataDeliveryUtils.showMessage(shell, getStyle(), "Invalid Entries",
                    message.toString());
            return false;
        }

        return true;
    }

    protected Collection<String> getInvalidTabs() {
        Collection<String> invalidTabs = new ArrayList<String>(3);

        // Get the tabs to validate
        // TODO Hardcoding the tabs for now, fix this later

        // Validate the vertical tab
        if (!vTab.isValid()) {
            invalidTabs.add(VERTICAL_TAB);
        }

        if (!timingTabControls.isValid()) {
            invalidTabs.add(TIMING_TAB);
        }

        // Next is spatial subset tab
        if (!spatialTabControls.isValid()) {
            invalidTabs.add(SPATIAL_TAB);
        }

        return invalidTabs;
    }

    /**
     * Validate the area.
     */
    private boolean validateArea() {
        ReferencedEnvelope envelope = spatialTabControls.getEnvelope();

        if (envelope == null) {
            StringBuilder sb = new StringBuilder();
            sb.append("The defined area is invalid.\n\n");
            sb.append("Adjust the selected area and try again.");

            DataDeliveryUtils.showMessage(getShell(), SWT.ICON_ERROR,
                    "Validation Error", sb.toString());

            return false;
        }
        boolean valid = false;
        try {
            Envelope intersection = MapUtil.reprojectAndIntersect(envelope,
                    dataSet.getCoverage().getEnvelope());
            if (intersection != null && intersection.getSpan(0) > 0
                    && intersection.getSpan(1) > 0) {
                valid = true;
            }
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        if (!valid) {
            StringBuilder sb = new StringBuilder();
            sb.append("The defined area is invalid for ");
            sb.append(dataSet.getDataSetName());
            sb.append(".\nThe areas do not intersect.\n\n");

            sb.append("Adjust the selected area and try again.");

            int answer = DataDeliveryUtils.showMessage(getShell(), SWT.OK
                    | SWT.CANCEL, "Validation Error", sb.toString());

            return answer == SWT.OK ? true : false;
        }

        return true;
    }

    @Override
    public void handleSaveSubset() {

        if (!DataDeliveryGUIUtils.hasText(this.nameText)) {
            DataDeliveryUtils.showMessage(getShell(), SWT.OK, "Name Required",
                    "Name requred. A subset name must be entered.");
            return;
        }

        if (DataDeliveryGUIUtils.INVALID_CHAR_PATTERN.matcher(
                nameText.getText().trim()).find()) {
            DataDeliveryUtils
                    .showMessage(getShell(), SWT.ERROR, "Invalid Characters",
                            "Invalid characters. The Subset Name may only contain letters/numbers/dashes.");
            return;
        }

        SubsetXML<TIMEXML> subset = new SubsetXML<TIMEXML>();
        populateSubsetXML(subset);

        // Have all the info, now save the file
        SubsetFileManager.getInstance().saveSubset(subset, this.shell);
        setClean();
        subsetTab.enableButtons(nameText);
    }

    protected void populateSubsetXML(SubsetXML<TIMEXML> subset) {
        subset.setBaseSubsetName(nameText.getText());
        subset.setDatasetName(dataSet.getDataSetName());
        subset.setProviderName(dataSet.getProviderName());

        // Save area info
        AreaXML area = spatialTabControls.getSaveInfo();
        subset.setArea(area);

        // next save vertical layer/parameter info
        ArrayList<VerticalXML> vertList = vTab.getSaveInfo();
        subset.setVerticalList(vertList);

        // finally the date/cycle/forecast data
        TIMEXML time = timingTabControls.getSaveInfo();
        subset.setTime(time);
    }

    /**
     * Load saved subset button action handler. This action takes the settings
     * from the saved subset and applies them to the current data set.
     * 
     * @param subsetName
     *            Name of the subset to load
     */
    @SuppressWarnings("unchecked")
    @Override
    public void handleLoadSubset(String subsetName) {

        // TODO: How else to do this other than casting?
        SubsetXML<TIMEXML> loadedSubsetXml = (SubsetXML<TIMEXML>) SubsetFileManager
                .getInstance().loadSubset(subsetName);

        loadFromSubsetXML(loadedSubsetXml);
    }

    protected void loadFromSubsetXML(SubsetXML<TIMEXML> subsetXml) {
        ArrayList<VerticalXML> vertList = subsetXml.getVerticalList();
        vTab.populate(vertList, dataSet);

        TIMEXML time = subsetXml.getTime();
        this.timingTabControls.populate(time, dataSet);

        if (this.subsetXml == subsetXml) {
            // only populate area and name if subsetXml is loading from initial
            // load, not from the saved subsets tab.
            AreaXML area = subsetXml.getArea();
            spatialTabControls.setDataSet(this.dataSet);
            spatialTabControls.populate(area);

            this.nameText.setText(subsetXml.getBaseSubsetName());
        }
    }

    protected void loadFromSubscription(Subscription subscription) {
        this.nameText.setText(this.subscription.getName());

        // Cycle time
        TIMEXML timeXml = getTimeXmlFromSubscription();

        timeXml.setLatestData(true);

        this.timingTabControls.populate(timeXml, dataSet);

        // Area
        AreaXML area = new AreaXML();

        ReferencedEnvelope envelope = this.subscription.getCoverage()
                .getEnvelope();
        ReferencedEnvelope requestEnvelope = this.subscription.getCoverage()
                .getRequestEnvelope();

        if (requestEnvelope != null && !requestEnvelope.isEmpty()) {
            area.setEnvelope(requestEnvelope);
        } else {
            area.setEnvelope(envelope);
        }

        spatialTabControls.setDataSet(this.dataSet);
        spatialTabControls.populate(area);

        // Vertical/Parameters
        Map<String, VerticalXML> levelMap = new HashMap<String, VerticalXML>();
        ArrayList<Parameter> paramaterList = this.subscription.getParameter();

        for (Parameter p : paramaterList) {
            for (DataLevelType levelType : p.getLevelType()) {
                if (!levelMap.containsKey(levelType.getKey())) {
                    VerticalXML v = new VerticalXML();
                    if (levelType.getUnit() == null) {
                        v.setLayerType(String.valueOf(levelType
                                .getDescription()));
                    } else {
                        v.setLayerType(levelType.getDescription() + " ("
                                + levelType.getUnit() + "" + ")");
                    }
                    levelMap.put(levelType.getKey(), v);
                }
                VerticalXML v = levelMap.get(levelType.getKey());
                v.addParameter(p.getProviderName());

                // TODO - This is set up to only have one level type with
                // Multiple parameters. This will need to change if other
                // Data providers have parameters with multiple level types
                // containing multiple levels
                if (levelType.getId() == 100) {
                    for (int index : p.getLevels().getSelectedLevelIndices()) {
                        v.addLevel(String.valueOf(p.getLevels().getLevel()
                                .get(index)));
                    }
                }
            }
        }

        ArrayList<VerticalXML> vertList = new ArrayList<VerticalXML>(
                levelMap.values());
        vTab.populate(vertList, dataSet);
    }

    /**
     * Get the Time object.
     * 
     * @return The time object
     */
    protected abstract TIMEXML getTimeXmlFromSubscription();

    /**
     * If any mods have been made to the composite selections, set dirty true.
     */
    protected boolean isDirty() {

        if (vTab.isDirty()) {
            return true;
        }

        if (timingTabControls.isDirty()) {
            return true;
        }

        if (spatialTabControls.isSpatialDirty()) {
            return true;
        }

        return false;
    }

    /**
     * Reset the dirty flags.
     */
    protected void setClean() {
        vTab.setClean();
        timingTabControls.setDirty(false);
        spatialTabControls.setSpatialDirty(false);
    }

    /**
     * Constructs the appropriate subset dialog based upon the {@link DataSet}
     * type.
     * 
     * @param shell
     *            the current dialog shell
     * @param data
     *            the data set
     * @return the dialog
     */
    public static SubsetManagerDlg<?, ?, ?> fromDataSet(Shell shell,
            DataSet data) {
        if (data instanceof GriddedDataSet) {
            return new GriddedSubsetManagerDlg(shell, (GriddedDataSet) data);
        }
        throw new IllegalArgumentException(String.format(
                DATASETS_NOT_SUPPORTED, data.getClass().getName()));
    }

    /**
     * Constructs the appropriate subset dialog based upon the
     * {@link Subscription#getDataSetType()}.
     * 
     * @param shell
     *            the current dialog shell
     * @param loadDataSet
     *            true if load the data set
     * @param subscription
     *            the subscription object
     * @return SubsetManagerDlg
     */
    public static SubsetManagerDlg<?, ?, ?> fromSubscription(Shell shell,
            boolean loadDataSet, Subscription subscription) {
        if (DataType.GRID == subscription.getDataSetType()) {
            return new GriddedSubsetManagerDlg(shell, loadDataSet, subscription);
        }

        throw new IllegalArgumentException(String.format(
                DATASETS_NOT_SUPPORTED, subscription.getDataSetType()));
    }

    /**
     * Constructs the appropriate subset dialog based upon the {@link DataSet}
     * type.
     * 
     * @param shell
     *            the current dialog shell
     * @param data
     *            the DataSet obj
     * @param loadDataSet
     *            true if load the data set
     * @param subset
     *            Subset XML
     * @return SubsetManagerDlg
     */
    @SuppressWarnings("unchecked")
    public static SubsetManagerDlg<?, ?, ?> fromSubsetXML(Shell shell,
            DataSet data, boolean loadDataSet, SubsetXML<?> subset) {
        if (data instanceof GriddedDataSet) {
            return new GriddedSubsetManagerDlg(shell, (GriddedDataSet) data,
                    loadDataSet, (SubsetXML<SpecificDateTimeXML>) subset);
        }
        throw new IllegalArgumentException(String.format(
                DATASETS_NOT_SUPPORTED, data.getClass().getName()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean displayYesNoPopup(String title, String message) {
        return DataDeliveryUtils.showYesNoMessage(getShell(), title, message) == SWT.YES;
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
            return "Do not store the adhoc query";
        case FORCE_APPLY:
            if (wouldBeUnscheduledSubscriptions.size() == 1
                    && wouldBeUnscheduledSubscriptions.contains(subscription
                            .getName())) {
                // Can't force apply a query that won't ever be processed
                return null;
            }
            return "Store the adhoc query and unschedule the subscriptions";
        case INCREASE_LATENCY:
            return "Increase the latency on the adhoc query to "
                    + requiredLatency + " minutes";
        default:
            throw new IllegalArgumentException(
                    "Don't know how to handle option [" + option + "]");
        }
    }
}
