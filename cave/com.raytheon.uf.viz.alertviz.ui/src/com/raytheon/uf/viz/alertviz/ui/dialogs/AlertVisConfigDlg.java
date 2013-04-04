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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.ConfigContext;
import com.raytheon.uf.viz.alertviz.ConfigurationManager;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.IConfigurationChangedListener;
import com.raytheon.uf.viz.alertviz.INeedsSaveListener;
import com.raytheon.uf.viz.alertviz.IRestartListener;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.ConfigurationItem;
import com.raytheon.uf.viz.alertviz.config.ConfigurationMonitor;
import com.raytheon.uf.viz.alertviz.config.MonitorMetadata;
import com.raytheon.uf.viz.alertviz.config.Source;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.ui.dialogs.ConfigurationFileDlg.Function;

/**
 * This class displays the Alert Visualization configuration dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 02 Apr 2009             lvenable    TTR fixes.
 * 22 Nov 2010   2235      cjeanbap    Add ToolTip Text to Components.
 * 23 Nov 2010   6660      cjeanbap    Fixed misspelled word.
 * 14 Dec 2010   5633      rferrel     Added isDiposed and resetMinimize to
 *                                     test dialog shell's dispose state and
 *                                     to redisplay when minimized. The open
 *                                     method now uses a dispose listener to
 *                                     perform its clean up and no longer 
 *                                     waits for the dialog shell to close.
 * 24 Jan 2011   1978      cjeanbap    Add Monitor Tooltip functionality.
 * 04 Feb 2011   4617      cjeanbap    Assign Layout Mode.
 * 04 Mar 2011   7950      rferrel     Allow saving to base configuration.
 * 04 Mar 2011   6532      rferrel     Implemented restart button
 * 24 Mar 2011   5853	   cjeanbap	   Check MonitorMetadata image file for null.
 * 08 Sep 2012   13528     Xiaochuan   Confirmation message is not necessary when
 * 									   close. Add setNewConfig and run in common
 * 									   setting group to perform the updating.
 * 20 Dec 2012	 13746	   Xiaochuan   Add setNewConfig in omitMenuItem.addSelectionListener
 * 									   to send notify for the changing.	
 * 									   
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertVisConfigDlg extends Dialog implements
        IConfigurationChangedListener, INeedsSaveListener {

    private static final String TITLE = "Alert Visualization Configuration";

    private static final String CONFIG_LABEL = "Current Config: ";

    private static final RGB OMITTED_RGB = new RGB(255, 255, 100);

    private static final RGB INCLUDED_RGB = new RGB(175, 238, 238);

    private static final RGB NOTMONITOR_RGB = new RGB(190, 190, 190);

    private static final RGB RED_RGB = new RGB(155, 0, 0);

    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Return value when the shell is disposed.
     */
    private final Boolean returnValue = false;

    /** Composite for Category list */
    private LayoutControlsComp layoutControls;

    /**
     * Show priority check box.
     */
    private Button showPriorityChk;

    /**
     * Show Source key check box.
     */
    private Button showSourceKeyChk;

    /**
     * Show category check box.
     */
    private Button showCategoryChk;

    /**
     * Expand pop-up check box.
     */
    private Button expandPopupChk;

    /**
     * Message length spinner.
     */
    private Spinner msgLengthSpnr;

    /**
     * Blink duration spinner.
     */
    private Spinner blinkDurSpnr;

    /**
     * Audio duration spinner.
     */
    private Spinner audioDurSpnr;

    /**
     * Table control containing the available sources.
     */
    private Table sourcesList;

    /**
     * Button to create a new source.
     */
    private Button sourcesNewBtn;

    /**
     * Button to delete a selected source.
     */
    private Button sourcesDeleteBtn;

    /**
     * Array of priority controls for configuring messages.
     */
    private ArrayList<PriorityControls> priorityControls;

    /**
     * New source dialog.
     */
    private NewSourceCategoryDlg newSrcCatDlg;

    /**
     * Configuration data.
     */
    private Configuration configData;

    /**
     * Map of source data.
     */
    private Map<String, Source> sourceMap;

    /**
     * Control font.
     */
    private Font controlFont;

    private Menu popupMenuSourceList;

    /**
     * Label font.
     */
    private Font labelFont;

    /** Configuration list dialog */
    private ConfigurationFileDlg configurationDialog;
    
    private boolean fetchLog;
    
    /**
     * Label default configuration.
     */
    Label defaultConLbl;

    /**
     * X indicating save needed.
     */
    Label saveLbl;

    @SuppressWarnings("unused")
    private MonitorToolTip mttLayout;

    @SuppressWarnings("unused")
    private MonitorToolTip mttCommonSetting;

    @SuppressWarnings("unused")
    private MonitorToolTip mttSource;

    @SuppressWarnings("unused")
    private MonitorToolTip mttPriorities;

    private MonitorControls monitorControls;

    private int lastSelectedIndex;

    private IConfigurationChangedListener configurationChangedListener;

    private final AlertMessageDlg alertMsgDlg;

    private ConfigContext configContext;

    private Button save;

    protected IRestartListener restartListener;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param configData
     *            Configuration data.
     */
    public AlertVisConfigDlg(Shell parent, final AlertMessageDlg alertMsgDlg,
            Configuration configData, ConfigContext configContext,
            IConfigurationChangedListener configurationChangeListener,
            IRestartListener restartListener) {
        super(parent, 0);
        this.configData = configData;
        this.configContext = configContext;
        sourceMap = configData.getSources();
        this.alertMsgDlg = alertMsgDlg;
        this.configurationChangedListener = configurationChangeListener;
        this.restartListener = restartListener;
    }

    /**
     * Get the shell dialog's dispose state.
     * 
     * @return Dialog shell dispose state
     */
    public boolean isDisposed() {
        return shell.isDisposed();
    }

    /**
     * Redisplay a minimized already open shell dialog.
     */
    public void resetMinimized() {
        shell.setMinimized(false);
        shell.forceActive();
        shell.forceFocus();
    }

    /**
     * Open method used to display the dialog.
     * 
     * @return True/False.
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(display, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE
                | SWT.MAX);
        shell.setText(TITLE);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(2, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // initialize data, fonts, and arrays
        initalizeData();

        // Initialize all of the controls and layouts
        initializeComponents();

        ConfigurationManager.getInstance().addListener(this);

        shell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                labelFont.dispose();
                controlFont.dispose();
                ConfigurationManager.getInstance().removeListener(
                        AlertVisConfigDlg.this);
            }
        });
        shell.pack();

        shell.open();
        return returnValue;
    }

    /**
     * Initialize the data.
     */
    private void initalizeData() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        labelFont = new Font(shell.getDisplay(), "Monospace", 14, SWT.NORMAL);

        sourceMap = configData.getSources();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createLayoutGroup();

        createCommonSettingsGroup();

        createPrioritiesGroup();

        createBottomButtons();
    }

    /**
     * Create the message layout controls.
     */
    private void createLayoutGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.horizontalSpacing = 10;
        Group layoutGroup = new Group(shell, SWT.NONE);
        layoutGroup.setLayout(gl);
        layoutGroup.setLayoutData(gd);
        layoutGroup.setText(" Layout ");
        layoutGroup.setData(MonitorToolTip.tooltipTextKey,
                getLayoutToolTipText());

        mttLayout = new MonitorToolTip(layoutGroup, true);

        // layoutGroup.addMouseTrackListener(new MouseTrackAdapter() {
        // public void mouseHover(MouseEvent e) {
        // mttLayout.open();
        // }
        // });

        layoutControls = new LayoutControlsComp(layoutGroup, configData, this);
    }

    /**
     * Create the common settings group.
     */
    private void createCommonSettingsGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        Group commonSettingsGroup = new Group(shell, SWT.NONE);
        commonSettingsGroup.setLayout(gl);
        commonSettingsGroup.setLayoutData(gd);
        commonSettingsGroup.setText(" Common Settings ");
        commonSettingsGroup.setData(MonitorToolTip.tooltipTextKey,
                getCommonSettingToolTipText());

        mttCommonSetting = new MonitorToolTip(commonSettingsGroup, true);

        // commonSettingsGroup.addMouseTrackListener(new MouseTrackAdapter() {
        // public void mouseHover(MouseEvent e) {
        // mttCommonSetting.open();
        // }
        // });

        createCommonSettingsControls(commonSettingsGroup);
    }

    /**
     * Create the common settings controls.
     * 
     * @param parent
     *            Parent group container.
     */
    private void createCommonSettingsControls(Group parent) {
        // -----------------------------------------------
        // Create the text message group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        Group controlGroup = new Group(parent, SWT.NONE);
        controlGroup.setLayout(gl);
        controlGroup.setLayoutData(gd);
        controlGroup.setText(" Text Message ");

        showPriorityChk = new Button(controlGroup, SWT.CHECK);
        showPriorityChk.setText("Show Priority");
        showPriorityChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	setNewConfig();  
            	saveNeeded(true);
            }
        });

        showSourceKeyChk = new Button(controlGroup, SWT.CHECK);
        showSourceKeyChk.setText("Show Source Key");
        showSourceKeyChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	setNewConfig();
            	saveNeeded(true);
            	
            }
        });

        showCategoryChk = new Button(controlGroup, SWT.CHECK);
        showCategoryChk.setText("Show Category");
        showCategoryChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	setNewConfig();
            	saveNeeded(true);
            	
            }
        });

        // -----------------------------------------------
        // Create the remaining controls
        // -----------------------------------------------
        Composite controlComp = new Composite(parent, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));

        gd = new GridData();
        gd.horizontalSpan = 2;
        expandPopupChk = new Button(controlComp, SWT.CHECK);
        expandPopupChk.setText("Expand Popup information");
        expandPopupChk.setLayoutData(gd);
        expandPopupChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	setNewConfig();
            	saveNeeded(true);
            	
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label msgLengthLbl = new Label(controlComp, SWT.NONE);
        msgLengthLbl.setText("Length of Message Log: ");
        msgLengthLbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        msgLengthSpnr = new Spinner(controlComp, SWT.BORDER);
        msgLengthSpnr.setDigits(0);
        msgLengthSpnr.setIncrement(1);
        msgLengthSpnr.setPageIncrement(3);
        msgLengthSpnr.setMinimum(10);
        msgLengthSpnr.setMaximum(100);
        msgLengthSpnr.setSelection(10);
        msgLengthSpnr.setLayoutData(gd);
        msgLengthSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	setNewConfig();
            	saveNeeded(true);
            
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label blinkLbl = new Label(controlComp, SWT.NONE);
        blinkLbl.setText("Blink Duration (in seconds): ");
        blinkLbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        blinkDurSpnr = new Spinner(controlComp, SWT.BORDER);
        blinkDurSpnr.setDigits(0);
        blinkDurSpnr.setIncrement(1);
        blinkDurSpnr.setPageIncrement(3);
        blinkDurSpnr.setMinimum(0);
        blinkDurSpnr.setMaximum(60);
        blinkDurSpnr.setSelection(1);
        blinkDurSpnr.setLayoutData(gd);
        blinkDurSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	setNewConfig();
            	saveNeeded(true);
            	
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label audioLbl = new Label(controlComp, SWT.NONE);
        audioLbl.setText("Audio Duration (in seconds): ");
        audioLbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        audioDurSpnr = new Spinner(controlComp, SWT.BORDER);
        audioDurSpnr.setDigits(0);
        audioDurSpnr.setIncrement(1);
        audioDurSpnr.setPageIncrement(3);
        audioDurSpnr.setMinimum(0);
        audioDurSpnr.setMaximum(60);
        audioDurSpnr.setSelection(1);
        audioDurSpnr.setLayoutData(gd);
        audioDurSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	setNewConfig();
            	saveNeeded(true);
            	

            }
        });

        // --------------------------------------------------------
        // Populate the controls with the initial global settings
        // --------------------------------------------------------
        initCommonSettingControls();
    }

    /**
     * Create the priorities group.
     */
    private void createPrioritiesGroup() {
        // -----------------------------------------------
        // Create the text message group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 30;
        Group srcPriGroup = new Group(shell, SWT.NONE);
        srcPriGroup.setLayout(gl);
        srcPriGroup.setLayoutData(gd);
        srcPriGroup.setText(" Source && Priorities ");

        createSourcesControls(srcPriGroup);
        createPriorityControls(srcPriGroup);
        createSourcesKey(srcPriGroup);
        createMonitorControls(srcPriGroup);
    }

    private void createSourcesKey(Group parent) {
        GridData gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        GridLayout gl = new GridLayout(1, true);
        Composite comp = new Composite(parent, SWT.NONE);

        comp.setLayout(gl);

        Label label = new Label(comp, SWT.LEFT);
        label.setLayoutData(gd);
        label.setText("Regular");
        label.setBackground(new Color(display, NOTMONITOR_RGB));

        label = new Label(comp, SWT.LEFT);
        label.setLayoutData(gd);
        label.setText("Monitor");
        label.setBackground(new Color(display, INCLUDED_RGB));

        label = new Label(comp, SWT.LEFT);
        label.setLayoutData(gd);
        label.setText("Omitted Monitor");
        label.setBackground(new Color(display, OMITTED_RGB));
    }

    /**
     * Create the source controls.
     * 
     * @param parentGroup
     *            Parent group.
     */
    private void createSourcesControls(Group parentGroup) {
        Composite sourcesComp = new Composite(parentGroup, SWT.NONE);
        sourcesComp.setLayout(new GridLayout(1, false));

        Label sourcesLbl = new Label(sourcesComp, SWT.NONE);
        sourcesLbl.setText(" Sources:");
        sourcesLbl.setData(MonitorToolTip.tooltipTextKey,
                getSourcesToolTipText());

        mttSource = new MonitorToolTip(sourcesLbl, true);

        // sourcesLbl.addMouseTrackListener(new MouseTrackAdapter() {
        // public void mouseHover(MouseEvent e) {
        // mttSource.open();
        // }
        // });

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 125;
        gd.heightHint = 225;
        gd.horizontalSpan = 2;
        sourcesList = new Table(sourcesComp, SWT.VIRTUAL | SWT.BORDER
                | SWT.SINGLE | SWT.V_SCROLL);
        sourcesList.setLayoutData(gd);
        sourcesList.setFont(controlFont);

        // Populate the source list
        populateSourceList();

        sourcesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                    handleSourceSelection();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        sourcesNewBtn = new Button(sourcesComp, SWT.PUSH);
        sourcesNewBtn.setText("New...");
        sourcesNewBtn.setLayoutData(gd);
        sourcesNewBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                createNewSource();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        sourcesDeleteBtn = new Button(sourcesComp, SWT.PUSH);
        sourcesDeleteBtn.setText("Delete");
        sourcesDeleteBtn.setLayoutData(gd);
        sourcesDeleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteSource();
            }
        });

        handleSourceSelection();

        // Right-click pop-up menu for sourceList
        popupMenuSourceList = new Menu(sourcesList);

        sourcesList.setMenu(popupMenuSourceList);

        popupMenuSourceList.addListener(SWT.Show, new Listener() {
            public void handleEvent(Event event) {
                MenuItem[] menuItems = popupMenuSourceList.getItems();

                for (int i = 0; i < menuItems.length; i++) {
                    menuItems[i].dispose();
                }
                MenuItem updateMenuItem;
                updateMenuItem = new MenuItem(popupMenuSourceList, SWT.PUSH);
                updateMenuItem.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent ev) {
                        try {
                            saveWithConfirmDlg();
                        } catch (Exception e) {
                            Container
                                    .logInternal(
                                            Priority.ERROR,
                                            "AlertVizConfigDlg: exception saving Source changes.",
                                            e);
                        }
                    }
                });
                updateMenuItem.setText("Update");

                int index = sourcesList.getSelectionIndex();
                TableItem item = sourcesList.getItem(index);
                Source s = sourceMap.get(item.getText());
                setSourceColor(s, item);

                if (s.isMonitor()) {
                    MenuItem omitMenuItem;
                    omitMenuItem = new MenuItem(popupMenuSourceList, SWT.PUSH);
                    omitMenuItem.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent ev) {
                            try {
                                int index = sourcesList.getSelectionIndex();
                                lastSelectedIndex = index;
                                TableItem item = sourcesList.getItem(index);
                                Source s = sourceMap.get(item.getText());
                                MonitorMetadata mm = s
                                        .getConfigurationMonitor()
                                        .getMonitorMetadata();

                                if (!mm.getOmit()) {
                                    MessageBox warningPopup = new MessageBox(
                                            shell, SWT.ICON_QUESTION | SWT.NO
                                                    | SWT.YES);
                                    warningPopup
                                            .setText("AlertViz: Omit Monitor");
                                    warningPopup
                                            .setMessage("This will remove the "
                                                    + s.getName()
                                                    + " monitor\nicon from the main AlertViz Panel.\n"
                                                    + "Are you sure you want to omit this monitor");
                                    if (warningPopup.open() == SWT.YES) {
                                        s.getConfigurationMonitor()
                                                .getMonitorMetadata()
                                                .switchOmit();
                                        saveNeeded(true);
                                    }
                                } else {
                                    s.getConfigurationMonitor()
                                            .getMonitorMetadata().switchOmit();
                                    saveNeeded(true);
                                }
                                setSourceColor(s, item);
                                sourcesList.update();
                                setNewConfig();
                                
                            } catch (Exception e) {
                                Container
                                        .logInternal(
                                                Priority.ERROR,
                                                "AlertVizConfigDlg: exception changing/omitting monitor.",
                                                e);
                            }
                        }
                    });
                    String menuType = s.getConfigurationMonitor()
                            .getMonitorMetadata().getOmit() ? "Include"
                            : "Omit";
                    omitMenuItem.setText(menuType + " Monitor");
                }

                if (sourceMap.get(getListIndexToKey()).isLocked() != true) {
                    MenuItem delMenuItem;
                    delMenuItem = new MenuItem(popupMenuSourceList, SWT.PUSH);
                    delMenuItem.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            deleteSource();
                        }
                    });
                    delMenuItem.setText("Delete");
                }
            }
        });
    }

    @Override
    public void saveNeeded(boolean shouldSave) {
        if (!configContext.isBaseOrConfiguredLevel()) {
            String toolTip = shouldSave ? "The Current Configuration has Unsaved Changes"
                    : null;
            String saveLblText = shouldSave ? "X" : "";

            saveLbl.setText(saveLblText);
            saveLbl.setToolTipText(toolTip);
            saveLbl.update();

            save.setEnabled(shouldSave);
            save.setToolTipText(toolTip);
            save.update();
        }
    }

    private void setSourceColor(Source source, TableItem item) {
        if (source.isMonitor()) {
            MonitorMetadata mm = source.getConfigurationMonitor()
                    .getMonitorMetadata();
            if (mm.getOmit()) {
                // lighter yellow
                item.setBackground(new Color(display, OMITTED_RGB));
            } else {
                // lighter blue
                item.setBackground(new Color(display, INCLUDED_RGB));
            }
        } else {
            // gray
            item.setBackground(new Color(display, NOTMONITOR_RGB));
        }
    }

    /**
     * Create the priority controls.
     * 
     * @param parentGroup
     *            Parent group.
     */
    private void createPriorityControls(Group parentGroup) {
        GridData gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        Composite prioritiesComp = new Composite(parentGroup, SWT.NONE);
        GridLayout gl = new GridLayout(7, false);
        gl.horizontalSpacing = 25;
        gl.verticalSpacing = 0;
        prioritiesComp.setLayout(gl);
        prioritiesComp.setLayoutData(gd);

        // -------------------------------------------
        // Create the priority controls
        // -------------------------------------------
        priorityControls = new ArrayList<PriorityControls>();

        for (int i = 0; i < 6; i++) {
            priorityControls.add(new PriorityControls(shell, prioritiesComp, i,
                    this, this));
        }

        // ----------------------------------------------
        // Put the priority label on the display
        // ----------------------------------------------

        // Filler
        new Label(prioritiesComp, SWT.NONE);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 6;
        Label priorityLbl = new Label(prioritiesComp, SWT.CENTER);
        priorityLbl.setText(getPriorityLabelText());
        priorityLbl.setFont(labelFont);
        priorityLbl.setLayoutData(gd);
        priorityLbl.setData(MonitorToolTip.tooltipTextKey,
                getPrioritiesToolTipText());

        mttPriorities = new MonitorToolTip(priorityLbl, true);

        // priorityLbl.addMouseTrackListener(new MouseTrackAdapter() {
        // public void mouseHover(MouseEvent e) {
        // mttPriorities.open();
        // }
        // });

        // ---------------------------------------------------------
        // Put the priority canvases on the display
        // ---------------------------------------------------------

        // Filler
        new Label(prioritiesComp, SWT.NONE);

        // Loop and put the priority canvases on the display
        for (int i = 0; i < 6; i++) {
            priorityControls.get(i).createPriorityCanvas();
        }

        addSeparator(prioritiesComp);

        // ---------------------------------------------------------
        // Put the text label and check boxes on the display
        // ---------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label textLbl = new Label(prioritiesComp, SWT.RIGHT);
        textLbl.setText("Text: ");
        textLbl.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {
        	priorityControls.get(i).createTextCheckbox();
        }

        addSeparator(prioritiesComp);

        // ---------------------------------------------------------
        // Put the blink label and check boxes on the display
        // ---------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label blinkLbl = new Label(prioritiesComp, SWT.RIGHT);
        blinkLbl.setText("Blink: ");
        blinkLbl.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {
        	priorityControls.get(i).createBlinkCheckbox();
            
        }

        addSeparator(prioritiesComp);

        // ---------------------------------------------------------
        // Put the popup label and check boxes on the display
        // ---------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label popupLbl = new Label(prioritiesComp, SWT.RIGHT);
        popupLbl.setText("Popup: ");
        popupLbl.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {
            priorityControls.get(i).createPopupCheckbox();
        }

        addSeparator(prioritiesComp);

        // ---------------------------------------------------------
        // Put the audio label and check boxes on the display
        // ---------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label audioLbl = new Label(prioritiesComp, SWT.RIGHT);
        audioLbl.setText("Audio: ");
        audioLbl.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {
            priorityControls.get(i).createAudioControls();
        }

        addSeparator(prioritiesComp);

        // ---------------------------------------------------------
        // Put the action label and check boxes on the display
        // ---------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label actionLbl = new Label(prioritiesComp, SWT.RIGHT);
        actionLbl.setText("Action: ");
        actionLbl.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {
            priorityControls.get(i).createActionControls();
        }

        addSeparator(prioritiesComp);

        // ---------------------------------------------------------
        // Put the log label and check boxes on the display
        // ---------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label logLbl = new Label(prioritiesComp, SWT.RIGHT);
        logLbl.setText("Log: ");
        logLbl.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {
            priorityControls.get(i).createLogCheckbox();
        }

        addSeparator(prioritiesComp);

        // ---------------------------------------------------------
        // Put the foreground/background label and
        // check boxes on the display
        // ---------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label fgbg = new Label(prioritiesComp, SWT.RIGHT);
        fgbg.setText("Foreground/\nBackground: ");
        fgbg.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {
            priorityControls.get(i).createFgBgControls();
        }
        handleSourceSelection();
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 7;
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(8, false));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        this.save = new Button(buttonComp, SWT.PUSH);
        save.setEnabled(!this.configContext.isBaseOrConfiguredLevel());
        save.setText("Save");
        save.setLayoutData(gd);
        save.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveWithConfirmDlg();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        Button saveAsBtn = new Button(buttonComp, SWT.PUSH);
        saveAsBtn.setText("Save As...");
        saveAsBtn.setLayoutData(gd);
        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openConfigurationList(Function.SAVE);
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        Button retrieveBtn = new Button(buttonComp, SWT.PUSH);
        retrieveBtn.setText("Retrieve...");
        retrieveBtn.setLayoutData(gd);
        retrieveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Function retrieveMode = save.isEnabled() ? Function.RETRIEVE_WITH_UNSAVED_CHANGES
                        : Function.RETRIEVE;
                openConfigurationList(retrieveMode);
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete...");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openConfigurationList(Function.DELETE);
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	close();
           
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        gd.widthHint = buttonWidth;
        Button restartBtn = new Button(buttonComp, SWT.PUSH);
        restartBtn.setText("Restart");
        restartBtn.setToolTipText("Restart AlertViz");
        restartBtn.setLayoutData(gd);
        restartBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION
                        | SWT.YES | SWT.NO);
                mb.setMessage("Any unsaved changes will be lost. Restart anyway?");
                if (mb.open() == SWT.YES) {
                    close();
                    if (configurationChangedListener != null) {
                        restartListener.restart();
                    }
                }
            }
        });

        saveLbl = new Label(buttonComp, SWT.BOLD);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.widthHint = 10;
        saveLbl.setForeground(new Color(display, RED_RGB));
        saveLbl.setText("");
        saveLbl.setLayoutData(gd);

        defaultConLbl = new Label(buttonComp, SWT.NONE);
        defaultConLbl.setText(CONFIG_LABEL + configContext);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        defaultConLbl.setLayoutData(gd);
        saveNeeded(false);
    }

    /**
     * Get the text for the priorities label.
     * 
     * @return Label text.
     */
    private String getPriorityLabelText() {
        String format = "%S";

        String text = "High      <---       PRIORITIES      -->      Low";

        String labelStr = String.format(format, text);

        return labelStr;
    }

    /**
     * Add a separator to the display.
     * 
     * @param parent
     *            Parent composite.
     */
    private void addSeparator(Composite parent) {
        // Filler
        new Label(parent, SWT.NONE);

        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 6;
        Label sepLbl = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create a new source.
     */
    private void createNewSource() {
        if (newSrcCatDlg == null) {
            newSrcCatDlg = new NewSourceCategoryDlg(shell, false,
                    sourceMap.keySet());
            Boolean saveInfo = (Boolean) newSrcCatDlg.open();

            if (saveInfo != null && saveInfo == true) {
                String name = newSrcCatDlg.getTextKey();
                String desc = newSrcCatDlg.getDescription();

                Source newSource = new Source(desc, name);

                newSource.setConfigurationItem(createConfigurationItem());

                sourceMap.put(name, newSource);
                // AlertViz Customization Update
                ConfigurationManager.getInstance()
                        .addToCustomization(newSource);
                populateSourceList();
            }

            newSrcCatDlg = null;
        }
    }

    /**
     * Delete an existing source.
     */
    private void deleteSource() {
        if (sourceMap.get(getListIndexToKey()).isLocked() == true) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Delete");
            mb.setMessage("The selected Source is locked and cannot be deleted.");
            mb.open();
            return;
        }

        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.YES | SWT.NO);
        mb.setText("Delete");
        mb.setMessage("Do you wish to delete the selected Source?");
        int val = mb.open();

        if (val == SWT.YES) {
            // AlertViz Customization Update
            Source removedSource = sourceMap.remove(getListIndexToKey());

            ConfigurationManager.getInstance().removeFromCustomization(
                    removedSource);
            populateSourceList();
            populatePriorityControls();
        }
    }

    /**
     * Create a new user configuration item.
     * 
     * @return The new user configuration item.
     */
    private ConfigurationItem createConfigurationItem() {
        ConfigurationItem uci = new ConfigurationItem();
        Map<Priority, AlertMetadata> alertMetadataMap = new HashMap<Priority, AlertMetadata>();

        // use the currently selected Source's AlertMetadata
        int selectedSourceIndex = sourcesList.getSelectionIndex();
        String selectedSourceName = selectedSourceIndex < 0 ? "DEFAULT"
                : sourcesList.getItem(selectedSourceIndex).getText();
        Source selectedSource = sourceMap.get(selectedSourceName);
        Map<Priority, AlertMetadata> selectedSourcePriorityMap = selectedSource
                .getConfigurationItem().getPreferenceMapping();

        for (Priority pri : Priority.values()) {
            AlertMetadata selectedSourceAlertMetadata = selectedSourcePriorityMap
                    .get(pri);
            alertMetadataMap.put(pri, selectedSourceAlertMetadata.clone());
        }

        uci.setPreferenceMapping(alertMetadataMap);

        return uci;
    }

    /**
     * Initialize the common setting controls.
     */
    private void initCommonSettingControls() {
        TrayConfiguration gConfig = configData.getGlobalConfiguration();

        audioDurSpnr.setSelection(gConfig.getAudioDuration());
        blinkDurSpnr.setSelection(gConfig.getBlinkDuration());
        showCategoryChk.setSelection(gConfig.isCategoryShown());
        showPriorityChk.setSelection(gConfig.isPriorityShown());
        showSourceKeyChk.setSelection(gConfig.isSourceKeyShown());
        msgLengthSpnr.setSelection(gConfig.getLogLength());
        expandPopupChk.setSelection(gConfig.isExpandedPopup());
    }

    /**
     * Update the global configuration data.
     */
    private void updateGlobalConfiguration() {
        TrayConfiguration gConfig = configData.getGlobalConfiguration();
        updateGlobalConfiguration(gConfig);
    }

    /**
     * Update configuration data.
     * 
     * @param gConfig
     */
    private void updateGlobalConfiguration(final TrayConfiguration gConfig) {
        gConfig.setAudioDuration(audioDurSpnr.getSelection());
        gConfig.setBlinkDuration(blinkDurSpnr.getSelection());
        gConfig.setCategoryShown(showCategoryChk.getSelection());
        gConfig.setExpandedPopup(expandPopupChk.getSelection());
        int newLogLength = msgLengthSpnr.getSelection();
        int oldLogLength = gConfig.getLogLength();
        fetchLog = newLogLength > oldLogLength;
        gConfig.setLogLength(msgLengthSpnr.getSelection());
        gConfig.setPriorityShown(showPriorityChk.getSelection());
        gConfig.setSourceKeyShown(showSourceKeyChk.getSelection());
        gConfig.setMode(layoutControls.getSelectedLayoutTrayMode()); // Mandatory
                                                                     // to be
                                                                     // HERE!
        if (alertMsgDlg != null) {
            gConfig.setPosition(alertMsgDlg.getCurrentLocation());
        }
    }

    /**
     * Populate the priority controls.
     */
    private void populatePriorityControls() {
        if (priorityControls == null)
            return;

        int index = sourcesList.getSelectionIndex();

        if (index < 0) {
            return;
        }

        ConfigurationItem uci = new ConfigurationItem();
        AlertMetadata amd;
        Source selectedSource = sourceMap.get(sourcesList.getItem(index)
                .getText());

        uci = selectedSource.getConfigurationItem();

        for (Priority pri : Priority.values()) {
        	amd = uci.lookup(pri);
            priorityControls.get(pri.ordinal()).setAlertMetadata(amd);
        }
    }

    /**
     * Populate the source list.
     */
    private void populateSourceList() {
        TableItem selectedItem;
        int index = sourcesList.getSelectionIndex();

        if (index < 0) {
            selectedItem = null;
        } else {
            selectedItem = sourcesList.getItem(index);
        }
        
        sourcesList.removeAll();
        java.util.List<String> keys = new ArrayList<String>(sourceMap.keySet());
        Collections.sort(keys);

        for (String key : keys) {
            Source source = sourceMap.get(key);
            TableItem newItem = new TableItem(sourcesList, SWT.NONE);
            newItem.setText(key);
            setSourceColor(source, newItem);
        }

        if (selectedItem != null) {
//            sourcesList.select(sourcesList.indexOf(selectedItem));
        	sourcesList.select(index);
        } else {
            sourcesList.select(0);
        }
        handleSourceSelection();
    }

    /**
     * Respond to change in the source list's selection by populating the
     * priority controls and enabling or disabling the Delete button.
     */
    private void handleSourceSelection() {
        populatePriorityControls();
        Source source = sourceMap.get(getListIndexToKey());
        if (sourcesDeleteBtn != null) {
            boolean enable = false;
            try {
                enable = !source.isLocked();
            } catch (RuntimeException e) {
                // ignore
            }
            sourcesDeleteBtn.setEnabled(enable);
        }

        if (monitorControls != null) {
            ConfigurationMonitor cm = source.getConfigurationMonitor();
            if (cm == null) {
                source.setConfigurationMonitor(new ConfigurationMonitor(
                        new MonitorMetadata()));
                cm = source.getConfigurationMonitor();
                saveNeeded(true);
            }
            MonitorMetadata mm = cm.getMonitorMetadata();
            String imageText = null;
            boolean omit = true;

            if (mm != null) {
                if (mm.getImageFile() != null) {
                    imageText = mm.getImageFile();
                }
                omit = mm.getOmit();
            }
            monitorControls.setMonitor(mm, imageText, omit);
        }
    }

    /**
     * 
     */
    public void setNewConfig() {
    	updateGlobalConfiguration();
        ConfigurationManager.getInstance().setNewConfiguration(configContext, configData);
        
    }
    /**
     * Get the source key associated with the selected source in the source list
     * control.
     * 
     * @return Source key.
     */
    private String getListIndexToKey() {
        int index = sourcesList.getSelectionIndex();
        if (index < 0) {
            // get last selected index
            index = lastSelectedIndex;
            sourcesList.setSelection(index);
        }
        TableItem key = sourcesList.getItem(index);

        return key.getText();
    }

    private void saveWithConfirmDlg() {
        if (ConfigurationFileDlg.validateNotDelivered(shell,
                configContext.getName())) {
            if (ConfigurationManager.isDefaultConfig(configContext)) {
                if (!ConfigurationFileDlg.confirmDefaultChange(shell, "save",
                        configContext.toString())) {
                    return;
                }
            }
            save();
        }
    }

    private void save() {
        updateGlobalConfiguration();
        ConfigurationManager.getInstance().saveCurrentConfiguration(
                configContext, configData);
        configurationChanged();
    }

    private void openConfigurationList(Function function) {
        if (configurationDialog != null) {
            return;
        }
        configurationDialog = new ConfigurationFileDlg(shell, function,
                this.configContext);
        ConfigContext configContext = configurationDialog.open();
        if (configContext != null) {
            this.configContext = configContext;
            switch (function) {
            case SAVE: {
                saveAs(configContext);
                break;
            }
            case RETRIEVE:
            case RETRIEVE_WITH_UNSAVED_CHANGES: {
                retrieve(configContext);
                break;
            }
            case DELETE: {
                delete(configContext);
                break;
            }
            }
        }
        configurationDialog = null;
    }

    private void saveAs(ConfigContext configuration) {
        Configuration config = configData.clone();
        config.setName(configuration.getName());
        updateGlobalConfiguration(config.getGlobalConfiguration());
        if (ConfigurationManager.getInstance().saveCurrentConfiguration(
                configuration, config)) {
            updateGlobalConfiguration();
            configurationChanged();
        }
    }

    private void retrieve(ConfigContext configuration) {
        if (save.isEnabled()) {
            // reload from the file
            this.configData = ConfigurationManager.getInstance()
                    .retrieveConfiguration(configuration);
        }
        ConfigurationManager.getInstance().loadAsCurrent(configuration);
        configurationChanged();
    }

    private void delete(ConfigContext configuration) {
        if (ConfigurationManager.isDefaultConfig(configContext)) {
            if (!ConfigurationFileDlg.confirmDefaultChange(shell, "delete",
                    configContext.toString())) {
                return;
            }
        }
        ConfigurationManager.getInstance().deleteConfiguration(configuration);
        configurationChanged();
    }

    public void close() {
        shell.dispose();
    }

    @Override
    public void configurationChanged() {
    	configData = ConfigurationManager.getInstance()
        		.getCurrentConfiguration();
    	configContext = ConfigurationManager.getInstance().getCurrentContext();
        sourceMap = configData.getSources();
        layoutControls.reloadConfig(configData);

        alertMsgDlg.setMaxLogSize(configData.getGlobalConfiguration()
                .getLogLength());

        initCommonSettingControls();
        populateSourceList();
        populatePriorityControls();

        shell.setText(TITLE);
        defaultConLbl.setText(CONFIG_LABEL + configContext);
        save.setEnabled(!this.configContext.isBaseOrConfiguredLevel());
        saveNeeded(false);

        if (fetchLog) {
            // System.err.println("Need to fetch log here");
            fetchLog = false;
            alertMsgDlg.resetTabControl();
        }
    }

    /**
     * Get the ToolTipText for the Layout Label.
     * 
     * @return String a text String.
     */
    private String getLayoutToolTipText() {
        StringBuilder sb = new StringBuilder();

        sb.append("This is the Layout section, where you choose which\n");
        sb.append("layout you want the main AlertViz GUI to use and\n");
        sb.append("which Categories' text messages will be put into\n");
        sb.append("which Text Line.  You can click-and-drag Category\n");
        sb.append("names back and forth between the Category List to\n");
        sb.append("the right and the Layout Sections to the left\n");
        sb.append("If a Category is already in use, it will be gray.");

        return sb.toString();
    }

    /**
     * Get the ToolTipText for the Common Setting Label.
     * 
     * @return String a text String.
     */
    private String getCommonSettingToolTipText() {
        StringBuilder sb = new StringBuilder();

        sb.append("This are general settings.  The left\n");
        sb.append("side describes how the text message\n");
        sb.append("representations will be affected\n");
        sb.append("in the main GUI (if text is turned\n");
        sb.append(" on for thekey/priority) and the Pop-ups\n");
        sb.append("and how long thetext blinking and system\n");
        sb.append("audio execution willlast (again, if turned\n");
        sb.append("on).  The right side definesother, general\n");
        sb.append("behavior.");
        sb.append("NOTE: to make blinking or audio \n");
        sb.append("responses perpetual, set the duration to 0.");

        return sb.toString();
    }

    /**
     * Get the ToolTipText for the Sources Label.
     * 
     * @return String a text String.
     */
    private String getSourcesToolTipText() {
        StringBuilder sb = new StringBuilder();

        sb.append("Source:\n");
        sb.append("This is the list of Source Keys that AlertViz\n");
        sb.append("recognized.  Click on one to see its Priority\n");
        sb.append("settings to the right.  To create a new Source,\n");
        sb.append("select \'New...\'.\n");
        sb.append("The color coding is explained in the Legend below.");

        return sb.toString();
    }

    /**
     * Get the ToolTipText for the Priorities Label.
     * 
     * @return String a text String.
     */
    private String getPrioritiesToolTipText() {
        StringBuilder sb = new StringBuilder();

        sb.append("Priorities:\n");
        sb.append("These are the Priority settings for the selected\n");
        sb.append("Source Key.  This is where you control how\n");
        sb.append("messages of various priorities for the various\n");
        sb.append("Source Keys get communicated to you.\n");
        sb.append("Remember, zero is highest priority and five is\n");
        sb.append("lowest priority.\n");
        sb.append("Most are toggles, but some allow you to enter\n");
        sb.append("a file name or select a color.\n");
        sb.append("For definitions of the priority numbers, see\n");
        sb.append("the main GUI Tips (i-button).");

        return sb.toString();
    }

    /**
     * Create the Monitor Control group.
     * 
     * @param parentGroup
     *            the parent composite.
     */
    private void createMonitorControls(Group parentGroup) {
        monitorControls = new MonitorControls(parentGroup, this);
        monitorControls.createImageControls();
    }
}
