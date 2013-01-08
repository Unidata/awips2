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
package com.raytheon.uf.viz.datadelivery.browser;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryAuthRequest;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datadelivery.common.ui.IDataLoad;
import com.raytheon.uf.viz.datadelivery.common.ui.LoadSaveConfigDlg;
import com.raytheon.uf.viz.datadelivery.common.ui.LoadSaveConfigDlg.DialogType;
import com.raytheon.uf.viz.datadelivery.common.ui.TableCompConfig;
import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.filter.FilterExpandBar;
import com.raytheon.uf.viz.datadelivery.filter.MetaDataManager;
import com.raytheon.uf.viz.datadelivery.filter.config.FilterManager;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterSettingsXML;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterTypeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetManagerDlg;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * This is the main dialog for the Data Browser. This holds the menus and the
 * main composites and controls.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2012            lvenable     Initial creation.
 * Apr 3,  2012            jpiatt       Correct Exit-Save Changes dlg.
 * May 22, 2012    645     jpiatt       Added help dialog & tooltips.
 * Jun 21, 2012    736     djohnson     Change OPERATION_STATUS to OperationStatus.
 * Jul 24, 2012    955     djohnson     Matching datasets are returned in a {@link Set}.
 * Aug  7, 2012    863     jpiatt       Corrected Save Changes message functionality.
 * Aug 10, 2012   1022     djohnson     Use GriddedDataSet.
 * Aug 22, 2012   0743     djohnson     Convert back to DataSet.
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission}.
 * Dec 07, 2012 1278       bgonzale     fixed issue with clear where cursor could always
 *                                      show busy.
 * Dec 11, 2012 1405       mpduff       Move close confirmation dialog after event.doit = false.
 * Dec 10, 2012   1259     bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Dec 12, 2012 1391       bgonzale     Added job for dataset retrieval.
 * Jan 08, 2012 1436       bgonzale     Fixed area text box display update check.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataBrowserDlg extends CaveSWTDialog implements IDataTableUpdate,
        IUpdate, IDataLoad {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataBrowserDlg.class);

    /** Window Title string. */
    private final String WINDOW_TITLE = "Dataset Discovery Browser";

    /** Configuration path string. */
    private static final String CONFIG_PATH = FileUtil.join("dataDelivery",
            "dataBrowser");

    /** Default browser configuration. */
    private static final String DEFAULT_CONFIG = FileUtil.join(CONFIG_PATH,
            "DefaultBrowserConfig.xml");

    /** Help Dialog */
    private final DataBrowserHelpDlg help = null;

    /** Filter expand bar. */
    private FilterExpandBar filterExpandBar;

    /** Data table that will show the list of datasets. */
    private BrowserTableComp dataTableComp;

    /** Retrieve/Subscribe button. */
    private Button retrieveSubscribeBtn;

    /** Load/Save/Delete dialog. */
    private LoadSaveConfigDlg loadSaveDlg;

    /** List to choose the data types from. */
    private DualList dataTypesDualList;

    /** Label indicating how many entries are in the table. */
    private Label tableEntriesLabel;

    /** Table entries label prefix. */
    private final String tableEntriesPrefix = "Datasets Listed: ";

    /** Update minutes. */
    private int updateMinutes = 0;

    /** ArealSelectionDlg object. */
    private ArealSelectionDlg arealDlg;

    /** The selected config file that is currently loaded. */
    private String selectedFile;

    /** The area selected label. */
    private Label areaSelectedLbl;

    /** The LocalizationFile object of the currently loaded file. */
    private LocalizationFile locFile;

    /** Referenced Envelope. */
    private ReferencedEnvelope envelope = null;

    /** Filter Settings xml. */
    private FilterSettingsXML xml;

    /** Update Results button. */
    private Button updateResultsBtn;

    /** Clear button. */
    private Button clearBtn;

    /** Set Area button. */
    private Button areaBtn = null;

    /** Data Types array list. */
    private ArrayList<String> dataTypes;

    /** Flag for data type dirty. */
    private boolean dataTypeDirty = false;

    /** Flag for area dirty. */
    private boolean areaDirty = false;

    /** Turn off & on tooltips */
    private boolean toolTipFlag = false;

    /** Time label. */
    private Label updateTimeLabel = null;

    /** Timer object. */
    private Timer timer;

    /** TimerTask object. */
    private TimerTask timerTask;

    /** Area group. */
    private Group areaGrp = null;

    /** Data Type group. */
    private Group dataTypeGrp = null;

    /** Filter group. */
    private Group filterGroup = null;

    /** Composite for main table. */
    private Composite mainTableComp = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public DataBrowserDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL
                | CAVE.PERSPECTIVE_INDEPENDENT);

        setText(WINDOW_TITLE);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void disposed() {
        stopUpdateTimer();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createMenus();

        createAreaControls();
        createDataTypeControls();
        createSashForm();
        createRetSubsControl();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                if (isDirty() == false) {
                    close();
                } else {
                    event.doit = false;
                    int answer = DataDeliveryGUIUtils
                            .showSettingsHaveChangedPopup(getShell());
                    if (answer == SWT.YES) {
                        close();
                    }
                }
            }
        });
    }

    /**
     * Create the menu bar and then call methods to create the menus.
     */
    public void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     * 
     * @param menuBar
     *            Menu (on the menu bar).
     */
    private void createFileMenu(Menu menuBar) {

        MenuItem fileMI = new MenuItem(menuBar, SWT.CASCADE);
        fileMI.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMI.setMenu(fileMenu);

        // Create the settings menu
        MenuItem settingsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        settingsMenuItem.setText("&Settings");

        Menu settingsMenu = new Menu(menuBar);
        settingsMenuItem.setMenu(settingsMenu);

        // Tooltips
        MenuItem tooltipMI = new MenuItem(settingsMenu, SWT.CHECK);
        tooltipMI.setText("&Tooltips");
        tooltipMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                // Toggle tooltips on/off
                if (!toolTipFlag) {
                    toolTipFlag = true;
                } else {
                    toolTipFlag = false;
                }

                // Add tooltips to bottom table
                dataTableComp.showToolTips(toolTipFlag);

                handleTooltipSelection();
            }

        });

        // -------------------------------------------------
        // Create all the items in the dropdown menu
        // -------------------------------------------------

        MenuItem newConfigMI = new MenuItem(fileMenu, SWT.NONE);
        newConfigMI.setText("New Configuration");
        newConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleNewConfigurationAction();
            }
        });

        MenuItem loadConfigMI = new MenuItem(fileMenu, SWT.NONE);
        loadConfigMI.setText("Load Configuration...");
        loadConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayLoadSaveConfigDlg(DialogType.OPEN);
                setClean();
            }
        });

        MenuItem saveConfigMI = new MenuItem(fileMenu, SWT.NONE);
        saveConfigMI.setText("Save Configuration");
        saveConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveFilters();
            }
        });

        MenuItem saveAsConfigMI = new MenuItem(fileMenu, SWT.NONE);
        saveAsConfigMI.setText("Save Configuration As...");
        saveAsConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayLoadSaveConfigDlg(DialogType.SAVE_AS);
            }
        });

        MenuItem deleteConfigMI = new MenuItem(fileMenu, SWT.NONE);
        deleteConfigMI.setText("Delete Configuration...");
        deleteConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayLoadSaveConfigDlg(DialogType.DELETE);
            }

        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (isDirty() == false) {
                    close();
                } else {
                    int answer = DataDeliveryGUIUtils
                            .showSettingsHaveChangedPopup(getShell());
                    if (answer == SWT.YES) {
                        close();
                    }
                }
            }
        });
    }

    /**
     * Create the Help menu
     * 
     * @param menuBar
     *            Menu (on the menu bar).
     */
    private void createHelpMenu(Menu menuBar) {

        MenuItem helpMI = new MenuItem(menuBar, SWT.CASCADE);
        helpMI.setText("&Help");

        // Create the File menu item with a File "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMI.setMenu(helpMenu);

        // -------------------------------------------------
        // Create all the items in the Filter dropdown menu
        // -------------------------------------------------

        MenuItem contentsMI = new MenuItem(helpMenu, SWT.NONE);
        contentsMI.setText("About Dataset Discovery Browser...");
        contentsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleHelp();
            }

        });
    }

    /**
     * Create the controls for selecting an area.
     */
    private void createAreaControls() {
        areaGrp = new Group(shell, SWT.NONE);
        areaGrp.setLayout(new GridLayout(1, false));
        areaGrp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        areaGrp.setText(" Areal Coverage: ");

        Composite areaComp = new Composite(areaGrp, SWT.NONE);
        areaComp.setLayout(new GridLayout(4, false));
        areaComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label areaLabel = new Label(areaComp, SWT.NONE);
        areaLabel.setText("Area:");
        areaLabel.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false,
                false));

        areaSelectedLbl = new Label(areaComp, SWT.BORDER);
        areaSelectedLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                true));

        clearBtn = new Button(areaComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setEnabled(false);
        clearBtn.setLayoutData(new GridData(90, SWT.DEFAULT));
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleClearArea();
            }
        });

        areaBtn = new Button(areaComp, SWT.PUSH);
        areaBtn.setText("Set Area...");
        areaBtn.setLayoutData(new GridData(90, SWT.DEFAULT));
        areaBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleAreaSelection();
            }
        });
    }

    /**
     * Create the controls for selecting the data types.
     */
    private void createDataTypeControls() {
        dataTypeGrp = new Group(shell, SWT.NONE);
        dataTypeGrp.setLayout(new GridLayout(1, false));
        dataTypeGrp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        dataTypeGrp.setText(" Data Types: ");

        loadDataTypes();

        DualListConfig config = new DualListConfig();
        config.setAvailableListLabel("Available:");
        config.setSelectedListLabel("Selected:");
        config.setListHeight(70);
        config.setListWidth(150);

        dataTypesDualList = new DualList(dataTypeGrp, SWT.NONE, config, this);
    }

    /**
     * Create the SashForm that will allow the composite sizes to be adjusted.
     */
    private void createSashForm() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite sashComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        sashComp.setLayout(gl);
        sashComp.setLayoutData(gd);

        SashForm sashForm = new SashForm(sashComp, SWT.VERTICAL);
        sashForm.setLayout(new GridLayout(1, false));
        sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sashForm.SASH_WIDTH = 5;
        sashForm.setBackground(getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));

        createFilterGroup(sashForm);

        createDataTable(sashForm);

        sashForm.setWeights(new int[] { 60, 40 });
    }

    /**
     * Create the filter group that will contain the filter expand bar.
     * 
     * @param sashForm
     *            The SashForm the controls/containers will be put on.
     */
    private void createFilterGroup(SashForm sashForm) {
        filterGroup = new Group(sashForm, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 3;
        gl.marginWidth = 3;
        gl.verticalSpacing = 0;
        filterGroup.setLayout(gl);
        filterGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                true));
        filterGroup.setText(" Filters ");

        filterExpandBar = new FilterExpandBar(filterGroup);
    }

    /**
     * Create the data table. The table will contain the data sets that are
     * returned by applying the filters to the query.
     * 
     * @param sashForm
     *            The SashForm the controls/containers will be put on.
     */
    private void createDataTable(SashForm sashForm) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainTableComp = new Composite(sashForm, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        mainTableComp.setLayout(gl);
        mainTableComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(mainTableComp, SWT.NONE);
        gl = new GridLayout(3, false);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        // Update results button
        updateResultsBtn = new Button(buttonComp, SWT.PUSH);
        updateResultsBtn.setText(" Update Results ");
        updateResultsBtn.setEnabled(false);
        updateResultsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                startUpdateTimer();
                populateDataTable();
            }
        });

        // Update time label
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.widthHint = 175;
        updateTimeLabel = new Label(buttonComp, SWT.LEFT);
        updateTimeLabel.setText("");
        updateTimeLabel.setLayoutData(gd);

        // Table entries label
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        tableEntriesLabel = new Label(buttonComp, SWT.RIGHT);
        tableEntriesLabel.setLayoutData(gd);

        TableCompConfig tableConfig = new TableCompConfig(TABLE_TYPE.BROWSER);
        dataTableComp = new BrowserTableComp(mainTableComp, tableConfig, this);
    }

    /**
     * Update the label telling the user how many entries are in the table.
     */
    private void updateTableEntriesLabel() {
        tableEntriesLabel.setText(tableEntriesPrefix
                + dataTableComp.getTableItemCount() + " ");
    }

    /**
     * Create the Retrieve / Subscribe control.
     */
    private void createRetSubsControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);

        retrieveSubscribeBtn = new Button(shell, SWT.PUSH);
        retrieveSubscribeBtn.setText(" Subset... ");
        retrieveSubscribeBtn.setLayoutData(gd);
        retrieveSubscribeBtn.setEnabled(false);
        retrieveSubscribeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRetrieveSubscribeAction();
            }
        });
    }

    /**
     * Handle retrieving of subscription subset.
     */
    private void handleRetrieveSubscribeAction() {
        final DataDeliveryPermission permission = DataDeliveryPermission.SUBSCRIPTION_CREATE;
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId()
                + " is not authorized to Create Subscriptions/Queries\nPermission: "
                + permission;
        DataDeliveryAuthRequest request = new DataDeliveryAuthRequest();
        request.setUser(user);
        request.addRequestedPermissions(permission);
        request.setNotAuthorizedMessage(msg);

        try {
            if (DataDeliveryUtils.sendAuthorizationRequest(request)
                    .isAuthorized()) {

                DataSet data = dataTableComp.getSelectedDataset();

                if (data == null) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Warning");
                    mb.setMessage("Metadata not available for dataset selection.");
                    mb.open();
                    return;
                }

                SubsetManagerDlg<?, ?, ?> dlg = SubsetManagerDlg.fromDataSet(
                        shell, data);
                dlg.open();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Handle clear the area text.
     */
    private void handleClearArea() {
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        areaSelectedLbl.setText("");
        clearBtn.setEnabled(false);
        envelope = null;

        if (dataTypesDualList.getSelectedListItems().length != 0) {
            if (filterExpandBar != null) {
                filterExpandBar.setEnvelope(null);
            }

            resetTableAndControls();
        }
        getShell().setCursor(null);
    }

    /**
     * Handle the area selection.
     */
    private void handleAreaSelection() {
        if (arealDlg == null || arealDlg.isDisposed()) {
            arealDlg = new ArealSelectionDlg(getShell(), envelope);
            arealDlg.open();
        } else {
            arealDlg.bringToTop();
        }

        if (arealDlg.getReturnValue() == null
                || (Boolean) arealDlg.getReturnValue() == false) {
            return;
        }

        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        updateAreaLabel(arealDlg.getEnvelope());

        if (filterExpandBar != null) {
            filterExpandBar.setEnvelope(envelope);
        }

        getShell().setCursor(null);

        resetTableAndControls();
    }

    /**
     * Handle the help display dialog.
     */
    private void handleHelp() {

        if (help == null || help.isDisposed()) {
            DataBrowserHelpDlg help = new DataBrowserHelpDlg(shell);
            help.open();
        } else {
            help.bringToTop();
        }

    }

    /**
     * Update the area label.
     */
    private void updateAreaLabel(ReferencedEnvelope envelope) {

        this.envelope = envelope;

        if (envelope == null || envelope.isEmpty()) {
            areaSelectedLbl.setText("");
            clearBtn.setEnabled(false);
        } else {
            NumberFormat formatter = new DecimalFormat(".0000");

            Coordinate ul = EnvelopeUtils.getUpperLeftLatLon(envelope);
            Coordinate lr = EnvelopeUtils.getLowerRightLatLon(envelope);

            // Check for empty values
            if (ul.x == 0 && ul.y == 0 && lr.x == 0 && lr.y == 0) {
                return;
            }

            StringBuilder sb = new StringBuilder("UL: ");
            sb.append(formatter.format(ul.x) + "," + formatter.format(ul.y));
            sb.append(", LR: " + formatter.format(lr.x) + ","
                    + formatter.format(lr.y));

            areaSelectedLbl.setText(sb.toString());

            if (areaSelectedLbl.getText().length() > 0) {
                clearBtn.setEnabled(true);
            }
        }

        this.areaDirty = true;
    }

    /**
     * Start the update timer.
     */
    private void startUpdateTimer() {
        if (timer != null) {
            stopUpdateTimer();
        }

        timer = new Timer();
        updateMinutes = 0;

        timerTask = new TimerTask() {
            @Override
            public void run() {
                StringBuilder sb = new StringBuilder("Last Update: ");
                sb.append(updateMinutes);

                if (updateMinutes == 1) {
                    sb.append(" min");
                } else {
                    sb.append(" mins");
                }
                final String updateString = sb.toString();

                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        if (!updateTimeLabel.isDisposed()) {
                            updateTimeLabel.setText(updateString);
                            ++updateMinutes;
                        }
                    }
                });
            }
        };

        timer.schedule(timerTask, 0, 60000);
    }

    /**
     * Stop the update timer.
     */
    private void stopUpdateTimer() {
        if (timer != null) {
            timer.cancel();
            timer = null;
        }

        timerTask = null;

        if (updateTimeLabel != null && updateTimeLabel.isDisposed() == false) {
            updateTimeLabel.setText("");
        }
    }

    /**
     * Load the data types.
     */
    private void loadDataTypes() {
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        BrowserDataTypeThread dataTypeThread = new BrowserDataTypeThread(this);
        Thread loadThread = new Thread(dataTypeThread);
        loadThread.start();
    }

    /**
     * Turn off and on tool tips.
     */
    private void handleTooltipSelection() {

        // Apply tooltips if toggle is on
        if (toolTipFlag) {
            areaBtn.setToolTipText("Click to enter area filter selection");
            updateResultsBtn
                    .setToolTipText("Click to view the subscription list");
            retrieveSubscribeBtn.setToolTipText("Click to retrieve subset");
            clearBtn.setToolTipText("Clear area");

        } else {
            areaBtn.setToolTipText("");
            updateResultsBtn.setToolTipText("");
            retrieveSubscribeBtn.setToolTipText("");
            clearBtn.setToolTipText("");
        }

    }

    /**
     * Apply the new configuration selections to the Data Browser dialog.
     */
    private void handleNewConfigurationAction() {

        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES
                | SWT.NO);
        mb.setText("New Configuration");
        mb.setMessage("Creating a new configuration will discard any changes you have "
                + "made to Area, Data Types, and Filters.\n\nDo you wish to continue?");
        int result = mb.open();

        if (result == SWT.NO) {
            return;
        }

        setText(WINDOW_TITLE);

        // Clear the area.
        areaSelectedLbl.setText("");
        clearBtn.setEnabled(false);
        envelope = null;

        // Clear the data type.
        dataTypesDualList.clearSelection();

        // Clear the filters.
        filterExpandBar.updateFilters(dataTypesDualList.getSelectedListItems(),
                envelope);

        // Clear the table and disable the buttons.
        resetTableAndControls();
    }

    /**
     * Display the Load/Save configuration dialog.
     * 
     * @param type
     *            Dialog type.
     */
    private void displayLoadSaveConfigDlg(DialogType type) {
        if (loadSaveDlg == null || loadSaveDlg.isDisposed()) {
            FilterManager fm = FilterManager.getInstance();

            loadSaveDlg = new LoadSaveConfigDlg(shell, type, CONFIG_PATH,
                    DEFAULT_CONFIG, true);
            loadSaveDlg.open();

            if (type == DialogType.SAVE_AS) {
                this.locFile = (LocalizationFile) loadSaveDlg.getReturnValue();
                if (locFile != null) {
                    xml = new FilterSettingsXML();

                    // Save data type
                    String[] dataTypes = dataTypesDualList
                            .getSelectedListItems();
                    FilterTypeXML ftx = new FilterTypeXML();
                    ftx.setFilterType("Data Type");
                    for (String dataType : dataTypes) {
                        ftx.addValue(dataType);
                    }

                    xml.addFilterType(ftx);

                    // Save area settings
                    AreaXML area = new AreaXML();
                    if (envelope != null) {
                        area.setEnvelope(envelope);
                    }
                    xml.setArea(area);

                    setText(WINDOW_TITLE + " - (" + locFile.getFile().getName()
                            + ")");

                    // Save filter settings
                    filterExpandBar.populateFilterSettingsXml(xml);

                    fm.setCurrentFile(locFile);
                    fm.setXml(xml);
                    fm.saveXml();
                }

                setClean();
            } else if (type == DialogType.OPEN) {
                this.locFile = (LocalizationFile) loadSaveDlg.getReturnValue();
                if (locFile != null) {
                    setText(WINDOW_TITLE + " - (" + locFile.getFile().getName()
                            + ")");
                    fm.setCurrentFile(locFile);
                    xml = fm.getXml();
                    updateFilters();
                    this.selectedFile = locFile.getFile().getName();
                }
            } else if (type == DialogType.DELETE) {
                this.locFile = (LocalizationFile) loadSaveDlg.getReturnValue();
                try {
                    if (locFile != null) {
                        locFile.delete();
                    }
                } catch (LocalizationOpFailedException e) {
                    // statusHandler.handle(Priority.PROBLEM,
                    // e.getLocalizedMessage(), e);
                    e.printStackTrace();
                }
                return;
            }
        } else {
            loadSaveDlg.bringToTop();
        }
    }

    /**
     * Save the current filter settings.
     */
    private void saveFilters() {
        xml = new FilterSettingsXML();

        // Save data type
        String[] dataTypes = dataTypesDualList.getSelectedListItems();
        FilterTypeXML ftx = new FilterTypeXML();
        ftx.setFilterType("Data Type");
        for (String dataType : dataTypes) {
            ftx.addValue(dataType);
        }

        xml.addFilterType(ftx);

        // Save area settings
        AreaXML area = new AreaXML();
        if (envelope != null) {
            area.setEnvelope(envelope);
        }
        xml.setArea(area);

        filterExpandBar.populateFilterSettingsXml(xml);

        if (locFile != null) {
            selectedFile = locFile.getFile().getName();
        }

        if (this.selectedFile == null) {
            displayLoadSaveConfigDlg(DialogType.SAVE_AS);
        } else {
            FilterManager fm = FilterManager.getInstance();
            fm.setCurrentFile(locFile);
            fm.setXml(xml);
            fm.saveXml();
            setClean();
        }
    }

    /**
     * Update the filter settings.
     */
    private void updateFilters() {
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        // Clear the filters.
        filterExpandBar.updateFilters(new String[0], envelope);
        dataTypesDualList.clearSelection();

        areaSelectedLbl.setText("");
        clearBtn.setEnabled(false);
        envelope = null;

        // update the area settings
        AreaXML area = xml.getArea();
        if (area != null) {
            updateAreaLabel(area.getEnvelope());
        }

        dataTypesDualList.setAvailableItems(dataTypes);

        ArrayList<FilterTypeXML> filterTypeList = xml.getFilterTypeList();
        for (FilterTypeXML ftx : filterTypeList) {
            if (ftx.getFilterType().equals("Data Type")) {
                ArrayList<String> valueList = ftx.getValues();
                String[] values = new String[valueList.size()];
                int i = 0;
                for (String s : valueList) {
                    values[i] = s;
                    i++;
                }
                dataTypesDualList.selectItems(values);
            }
        }

        // this.filterExpandBar.updateFilters(dataTypesDualList.getSelectedListItems(),
        // coordArray);
        filterExpandBar.setFilterSettingsXml(xml);

        getShell().setCursor(null);

        resetTableAndControls();

        this.updateResultsBtn.setEnabled(true);
    }

    /**
     * Populate the data table.
     */
    private void populateDataTable() {
        // Get selected filter settings
        xml = new FilterSettingsXML();
        filterExpandBar.populateFilterSettingsXml(xml);
        final List<DataSet> matchingDataSets = new ArrayList<DataSet>();
        final Shell jobParent = this.getShell();

        final Job job = new Job("Updating Results...") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                DataDeliveryGUIUtils.markBusyInUIThread(jobParent);
                MetaDataManager dataManager = MetaDataManager.getInstance();
                dataManager.setArea(envelope);
                matchingDataSets.addAll(dataManager.getMatchingDataSets(xml));
                return Status.OK_STATUS;
            }
        };

        job.addJobChangeListener(new JobChangeAdapter() {
            @Override
            public void done(IJobChangeEvent event) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        if (!dataTableComp.isDisposed()) {
                            dataTableComp.updateTable(matchingDataSets);
                            updateTableEntriesLabel();
                        }
                        DataDeliveryGUIUtils.markNotBusyInUIThread(jobParent);
                    }
                });
            }
        });
        job.schedule();
    }

    /**
     * Reset control options.
     */
    private void resetTableAndControls() {
        boolean hasFilters = filterExpandBar.hasFilters();

        updateResultsBtn.setEnabled(hasFilters);
        dataTableComp.clearTableEntries();
        retrieveSubscribeBtn.setEnabled(false);
        stopUpdateTimer();
    }

    /**
     * Set isDirty flag.
     */
    private boolean isDirty() {

        if (this.dataTypeDirty || this.areaDirty) {
            return true;
        }

        if (this.filterExpandBar.isDirty()) {
            return true;
        }

        return false;
    }

    /**
     * Set all areas clean.
     */
    private void setClean() {
        this.dataTypeDirty = false;
        this.areaDirty = false;
        filterExpandBar.setClean();
    }

    /**
     * Set flag for table selection.
     */
    @Override
    public void tableSelectionChanged(boolean hasData) {
        this.retrieveSubscribeBtn.setEnabled(hasData);
    }

    /**
     * Set flag for table rows changed.
     */
    @Override
    public void tableRowsChanged() {
        this.updateTableEntriesLabel();
    }

    /**
     * Set flag for has entries.
     */
    @Override
    public void hasEntries(boolean entries) {
        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        if (dataTypesDualList != null) {
            if (filterExpandBar != null) {
                this.filterExpandBar.updateFilters(
                        dataTypesDualList.getSelectedListItems(), envelope);
                this.filterExpandBar.addListener(SWT.SetData, new Listener() {
                    @Override
                    public void handleEvent(Event event) {
                        resetTableAndControls();
                    }
                });
                dataTypeDirty = true;
            }
        }
        shell.setCursor(null);

        resetTableAndControls();
    }

    /**
     * Load the complete list of data types .
     * 
     * @param list
     *            of strings the list of data Types
     */
    @Override
    public void loadDataTypeComplete(final List<String> dataTypesList) {

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                dataTypes = (ArrayList<String>) dataTypesList;
                dataTypesDualList.setFullList(dataTypes);
                getShell().setCursor(null);
            }
        });

    }

    @Override
    public void selectionChanged() {
        // unused
    }
}
