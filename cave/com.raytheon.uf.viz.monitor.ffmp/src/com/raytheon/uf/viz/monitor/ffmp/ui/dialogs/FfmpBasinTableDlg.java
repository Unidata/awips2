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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.CLICK_TYPE;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.ThreshColNames;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.LoadSaveConfigDlg.DialogType;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPAutoRefreshEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPCWAChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPFieldChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPHUCChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPMaintainLayerEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPParentBasinEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPScreenCenterEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPStreamTraceEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPTimeChangeEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.FFMPWorstCaseEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPDataLoader.LOADER_TYPE;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPGraphData;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPLoaderStatus;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResource;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPTableDataLoader;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPTableDataUpdate;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Main FFMP dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2009            lvenable     Initial creation
 * Jul 31, 2012 14517      mpduff       Fix map blanking on updates and table updates
 *                                      for rapid slider changes.
 * Aug 01, 2012 14168      mpduff       Only allow items into the Thresholds menu if 
 *                                      ColorCell is true.
 * Dec 06, 2012 1353       rferrel      Code clean up.
 *                                       Changes for non-blocking AttributesDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FfmpBasinTableDlg extends CaveSWTDialog implements
        ITimeDurationAction, IAttributeDisplay, IThreshDisplay,
        ITableSelection, IMonitorListener, ISourceUpdate {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FfmpBasinTableDlg.class);

    private List<FFMPTableDataLoader> retrievalQueue = new ArrayList<FFMPTableDataLoader>();

    private MenuItem linkToFrameMI;

    private MenuItem worstCaseMI;

    private MenuItem autoRefreshMI;

    private MenuItem qpeMI;

    private MenuItem ratioMI;

    private MenuItem diffMI;

    @SuppressWarnings("unused")
    private MenuItem rfcffgMI;

    private MenuItem allOnlySmallBasinsMI;

    private MenuItem countyMI;

    private MenuItem maintainLayerMI;

    private MenuItem basinsInParentMI;

    private MenuItem upDownStreamBasinTraceMI;

    private MenuItem upStreamBasinTraceMI;

    private MenuItem downStreamBasinTraceMI;

    private MenuItem basinTrendMI;

    private Button refreshD2DBtn;

    private Label validTimeLbl;

    private Button configSummaryBtn;

    private Label gapValueLbl;

    private Label timeDurLbl;

    private Label dataLoadingLbl;

    private TimeDurScaleComp timeDurScale;

    private Label groupLbl;

    private final List<MenuItem> cwaMenuItems = new ArrayList<MenuItem>();

    private final List<MenuItem> layerMenuItems = new ArrayList<MenuItem>();

    private final List<MenuItem> d2dMenuItems = new ArrayList<MenuItem>();

    private final List<MenuItem> clickMenuItems = new ArrayList<MenuItem>();

    private final List<FFMPListener> ffmpListeners = new ArrayList<FFMPListener>();

    private final String timeDurationStr = "Time Duration (hrs.)";

    private FFMPTableComp ffmpTable;

    private Menu popupMenu;

    private FFMPTableData mainTableData;

    private Button thresholdsBtn;

    private AttributesDlg attributeDlg;

    private AttributeThresholdDlg attrThreshDlg;

    private boolean killDialog = false;

    private final List<String> cwas = new ArrayList<String>();

    private List<MenuItem> sourceMenuItems = new ArrayList<MenuItem>();

    private Date date = null;

    private BasinTrendDlg basinTrendDlg;

    private double time = Double.MIN_VALUE;

    private boolean allowNewTableUpdate = true;

    private final FFMPConfig ffmpConfig;

    private final SimpleDateFormat dateFmt = new SimpleDateFormat(
            "MMM dd yy HH:mm:ss z");

    private FFMPResource resource = null;

    private boolean sourceUpdate = false;

    private Font timeDurFont;

    private Font dataLoadFont;

    private AttributesDlgData attrData = null;

    Composite dataLoadComp;

    private Color refreshColor;

    private boolean dialogInitialized = false;

    private Composite tableComp;

    private FFMPTableDataLoader dataRetrieveThread = null;

    private boolean groupLabelFlag = true;

    public FfmpBasinTableDlg(Shell parent, FFMPTableData tData,
            FFMPResource resource) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);

        String siteName = resource.getSiteKey();
        if (resource.getPrimarySource().startsWith("B")
                && siteName.equals("hpe")) {
            siteName = "Bias " + siteName;

        }
        // Set the text in the dialog title bar
        setText("FFMP Basin Table " + siteName);

        this.mainTableData = tData;

        this.ffmpConfig = FFMPConfig.getInstance();

        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));

        this.resource = resource;

        for (DomainXML domain : resource.getDomains()) {
            cwas.add(domain.getCwa());
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
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

        dataLoadFont.dispose();
        timeDurFont.dispose();
        FFMPConfig.getInstance().disposeResources();
        FFMPConfig.unloadConfig();

        if (refreshColor != null) {
            refreshColor.dispose();
        }

        if (dataRetrieveThread != null && dataRetrieveThread.isAlive()) {
            dataRetrieveThread.interrupt();
            dataRetrieveThread = null;
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                if (killDialog == false) {
                    e.doit = false;
                    displayCloseInstructions();
                }
            }
        });

        refreshColor = new Color(getDisplay(), 255, 255, 153);

        dataLoadFont = new Font(getDisplay(), "Arial", 10, SWT.BOLD);
        timeDurFont = new Font(getDisplay(), "Arial", 10, SWT.NORMAL);

        createMenus();

        createTopControls();
        createGapTimeDurControls();

        addSeparator(shell);

        createTableControls();

        createTable();

        createDataLoadLabel();

        initTimeDuration();

        // Fire the change event to set the "Click" on startup
        fireTraceChangedEvent(CLICK_TYPE.UP_DOWN);

        /*
         * Refreshing the display
         */
        refreshDisplay(true);
    }

    public void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createConfigMenu(menuBar);
        createD2DMenu(menuBar);
        createLayerMenu(menuBar);
        createZoomMenu(menuBar);
        createCwaMenu(menuBar);
        createClickMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        MenuItem retrieveDefConfigMI = new MenuItem(fileMenu, SWT.NONE);
        retrieveDefConfigMI.setText("Retrieve Default Configuration");
        retrieveDefConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                ffmpConfig.loadDefaultConfig();
                refreshDisplay(false);

            }
        });

        MenuItem retrieveConfigMI = new MenuItem(fileMenu, SWT.NONE);
        retrieveConfigMI.setText("Retrieve Configuration...");
        retrieveConfigMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
                retrieveConfiguration();
            }
        });

        MenuItem saveConfigAsMI = new MenuItem(fileMenu, SWT.NONE);
        saveConfigAsMI.setText("Save Configuration As...");
        saveConfigAsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveConfiguration();
            }
        });
    }

    private void createConfigMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem configMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        configMenuItem.setText("Config");

        // Create the menu item with a "dropdown" menu
        Menu configMenu = new Menu(menuBar);
        configMenuItem.setMenu(configMenu);

        // -------------------------------------------------
        // Create all the items in the dropdown menu
        // -------------------------------------------------

        linkToFrameMI = new MenuItem(configMenu, SWT.CHECK);
        linkToFrameMI.setText("Link to Frame");
        linkToFrameMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ffmpConfig.getFFMPConfigData().setLinkToFrame(
                        linkToFrameMI.getSelection());
                fireConfigUpdateEvent();
            }
        });

        worstCaseMI = new MenuItem(configMenu, SWT.CHECK);
        worstCaseMI.setText("Worst Case for Aggregate");
        worstCaseMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ffmpConfig.getFFMPConfigData().setWorstCase(
                        worstCaseMI.getSelection());
                updateD2DRefresh();
                fireWorstCaseEvent(worstCaseMI.getSelection());
            }
        });

        autoRefreshMI = new MenuItem(configMenu, SWT.CHECK);
        autoRefreshMI.setText("Auto-Refresh");
        autoRefreshMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ffmpConfig.getFFMPConfigData().setAutoRefresh(
                        autoRefreshMI.getSelection());
                fireAutoRefreshEvent(autoRefreshMI.getSelection());
            }
        });
    }

    private void createD2DMenu(Menu menuBar) {
        // -------------------------------------
        // Create the D2D menu
        // -------------------------------------
        MenuItem d2dMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        d2dMenuItem.setText("D2D");

        // Create the menu item with a "dropdown" menu
        Menu d2dMenu = new Menu(menuBar);
        d2dMenuItem.setMenu(d2dMenu);

        // -------------------------------------------------
        // Create all the items in the dropdown menu
        // -------------------------------------------------

        qpeMI = new MenuItem(d2dMenu, SWT.RADIO);
        qpeMI.setText("qpe");
        qpeMI.setSelection(true);
        qpeMI.setData(FFMPRecord.FIELDS.QPE);
        qpeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (qpeMI.getSelection() == true) {
                    // If 0 hrs is not selected then update the display
                    if (timeDurScale.getSelectedHoursValue() != 0) {
                        updateD2DRefresh();
                        ffmpConfig.getFFMPConfigData().setD2dType(
                                FFMPRecord.FIELDS.QPE.name());
                        fireFieldChangedEvent(FFMPRecord.FIELDS.QPE, false);
                        clearMenuItems(sourceMenuItems);
                    }
                }
            }
        });

        d2dMenuItems.add(qpeMI);

        ratioMI = new MenuItem(d2dMenu, SWT.RADIO);
        ratioMI.setText("ratio");
        ratioMI.setData(FFMPRecord.FIELDS.RATIO);
        ratioMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // If 0 hrs is not selected then update the display
                if (timeDurScale.getSelectedHoursValue() != 0) {
                    if (ratioMI.getSelection() == true) {
                        updateD2DRefresh();
                        ffmpConfig.getFFMPConfigData().setD2dType(
                                FFMPRecord.FIELDS.RATIO.name());

                        if (FFMPSourceConfigurationManager.getInstance()
                                .getSourceByDisplayName(
                                        ffmpConfig.getFFMPConfigData()
                                                .getGuidSrc()) != null) {
                            for (MenuItem mi : sourceMenuItems) {
                                if (mi.getText().equals(
                                        ffmpConfig.getFFMPConfigData()
                                                .getGuidSrc())) {
                                    mi.setSelection(true);
                                    break;
                                }
                            }
                        } else {
                            sourceMenuItems.get(0).setSelection(true);
                            ffmpConfig.getFFMPConfigData().setGuidSrc(
                                    sourceMenuItems.get(0).getText());
                        }

                        fireFieldChangedEvent(FFMPRecord.FIELDS.RATIO, false);
                    }
                }
            }
        });
        d2dMenuItems.add(ratioMI);

        diffMI = new MenuItem(d2dMenu, SWT.RADIO);
        diffMI.setText("diff");
        diffMI.setData(FFMPRecord.FIELDS.DIFF);
        diffMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // If 0 hrs is not selected then update the display
                if (timeDurScale.getSelectedHoursValue() != 0) {
                    if (diffMI.getSelection() == true) {
                        updateD2DRefresh();
                        ffmpConfig.getFFMPConfigData().setD2dType(
                                FFMPRecord.FIELDS.DIFF.name());

                        if (FFMPSourceConfigurationManager.getInstance()
                                .getSourceByDisplayName(
                                        ffmpConfig.getFFMPConfigData()
                                                .getGuidSrc()) != null) {
                            for (MenuItem mi : sourceMenuItems) {
                                if (mi.getText().equals(
                                        ffmpConfig.getFFMPConfigData()
                                                .getGuidSrc())) {
                                    mi.setSelection(true);
                                }
                            }
                        }

                        fireFieldChangedEvent(FFMPRecord.FIELDS.DIFF, false);
                    }
                }
            }
        });
        d2dMenuItems.add(diffMI);

        new MenuItem(d2dMenu, SWT.SEPARATOR);

        List<String> guidanceList = resource.getProduct()
                .getAvailableGuidanceTypes();
        sourceMenuItems.clear();

        for (String name : guidanceList) {
            MenuItem guidMenu = new MenuItem(d2dMenu, SWT.RADIO);
            guidMenu.setText(name);
            guidMenu.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    MenuItem mi = (MenuItem) e.getSource();

                    if (mi.getSelection() == true) {
                        String guidSrc = mi.getText();
                        for (int i = 0; i < sourceMenuItems.size(); i++) {
                            String rdo = sourceMenuItems.get(i).getText();
                            if (rdo.equals(guidSrc)) {
                                ffmpConfig.getFFMPConfigData().setGuidSrc(
                                        guidSrc);
                                fireConfigUpdateEvent();
                                break;
                            }
                        }
                    }
                }
            });
            if (sourceMenuItems.contains(guidMenu.getText()) == false) {
                sourceMenuItems.add(guidMenu);
                // selects at least one as default
                ffmpConfig.getFFMPConfigData().setGuidSrc(guidMenu.getText());
                fireConfigUpdateEvent();
            }
        }
    }

    private void createLayerMenu(Menu menuBar) {

        FFMPTemplateConfigurationManager templateManager = FFMPTemplateConfigurationManager
                .getInstance();

        int hucNumber = templateManager.getNumberOfHuc();

        // -------------------------------------
        // Create the Layer menu
        // -------------------------------------
        MenuItem layerMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        layerMenuItem.setText("Layer");

        // Create the menu item with a "dropdown" menu
        Menu layerMenu = new Menu(menuBar);
        layerMenuItem.setMenu(layerMenu);

        // -------------------------------------------------
        // Create all the items in the dropdown menu
        // -------------------------------------------------

        allOnlySmallBasinsMI = new MenuItem(layerMenu, SWT.RADIO);
        allOnlySmallBasinsMI.setText("All && Only Small Basins");
        allOnlySmallBasinsMI.setData("ALL");
        allOnlySmallBasinsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (allOnlySmallBasinsMI.getSelection() == true) {
                    updateLayer(event);
                }
            }
        });
        layerMenuItems.add(allOnlySmallBasinsMI);

        countyMI = new MenuItem(layerMenu, SWT.RADIO);
        countyMI.setText("County");
        countyMI.setData("COUNTY");
        countyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (countyMI.getSelection() == true) {
                    groupLbl.setText("");
                    updateLayer(event);
                }
            }
        });
        layerMenuItems.add(countyMI);

        /*
         * Loop over the number of huc levels and create the number of menu
         * items needed.
         */
        StringBuilder hucMenuName = new StringBuilder();

        for (int i = 0; i < hucNumber; i++) {
            hucMenuName.setLength(0);
            MenuItem hucMI = new MenuItem(layerMenu, SWT.RADIO);

            hucMenuName.append("HUC_").append(i);
            if (i == 0) {
                hucMenuName.append("   (Biggest Basins, Most Aggregation)");
            } else if (i == hucNumber - 1) {
                hucMenuName.append("   (Smallest Basins, Least Aggregation)");
            }

            hucMI.setText(hucMenuName.toString());
            hucMI.setData("HUC" + i);
            hucMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    MenuItem mi = (MenuItem) event.getSource();

                    if (mi.getSelection() == true) {
                        updateLayer(event);
                    }
                }
            });
            layerMenuItems.add(hucMI);
        }
    }

    private void createZoomMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Zoom menu
        // -------------------------------------
        MenuItem zoomMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        zoomMenuItem.setText("Zoom");

        // Create the menu item with a "dropdown" menu
        Menu zoomMenu = new Menu(menuBar);
        zoomMenuItem.setMenu(zoomMenu);

        // -------------------------------------------------
        // Create all the items in the dropdown menu
        // -------------------------------------------------

        maintainLayerMI = new MenuItem(zoomMenu, SWT.CHECK);
        maintainLayerMI.setText("Maintain Layer");
        maintainLayerMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                fireMaintainLayerEvent(maintainLayerMI.getSelection());
            }
        });

        basinsInParentMI = new MenuItem(zoomMenu, SWT.CHECK);
        basinsInParentMI.setText("Only Basins in Parent");
        basinsInParentMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                fireParentBasinEvent(basinsInParentMI.getSelection());
            }
        });
    }

    private void createCwaMenu(Menu menuBar) {
        // -------------------------------------
        // Create the CWA menu
        // -------------------------------------
        MenuItem cwaMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        cwaMenuItem.setText("CWA");

        // Create the menu item with a "dropdown" menu
        Menu cwaMenu = new Menu(menuBar);
        cwaMenuItem.setMenu(cwaMenu);

        // -------------------------------------------------
        // Create all the items in the dropdown menu
        // -------------------------------------------------
        FFMPRunConfigurationManager runConfig = FFMPRunConfigurationManager
                .getInstance();
        FFMPTemplateConfigurationManager templateManager = FFMPTemplateConfigurationManager
                .getInstance();
        try {
            templateManager.readConfigXml();
        } catch (Exception e) {
            e.printStackTrace();
        }

        List<String> cwaList = new ArrayList<String>();

        String cwaName = LocalizationManager.getInstance().getCurrentSite()
                .toUpperCase();
        FFMPRunXML runner = runConfig.getRunner(cwaName);
        cwaList.add(runner.getPrimaryDomain().getCwa());
        if (runner.getBackupDomains() != null) {
            for (DomainXML backup : runner.getBackupDomains()) {
                cwaList.add(backup.getCwa());
            }
        }

        for (String cwa : cwaList) {
            MenuItem cwaMI = new MenuItem(cwaMenu, SWT.CHECK);
            cwaMI.setText(cwa);
            cwaMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    updateD2DRefresh();
                    updateCWA(event);
                }
            });
            cwaMenuItems.add(cwaMI);
        }
    }

    private void createClickMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Click menu
        // -------------------------------------
        MenuItem clickMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        clickMenuItem.setText("Click");

        // Create the menu item with a "dropdown" menu
        Menu clickMenu = new Menu(menuBar);
        clickMenuItem.setMenu(clickMenu);

        // -------------------------------------------------
        // Create all the items in the dropdown menu
        // -------------------------------------------------
        upDownStreamBasinTraceMI = new MenuItem(clickMenu, SWT.RADIO);
        upDownStreamBasinTraceMI.setText("Up/Down-stream Basin Trace");
        upDownStreamBasinTraceMI.setSelection(true);
        upDownStreamBasinTraceMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateD2DRefresh();
                fireTraceChangedEvent(CLICK_TYPE.UP_DOWN);
            }
        });
        clickMenuItems.add(upDownStreamBasinTraceMI);

        upStreamBasinTraceMI = new MenuItem(clickMenu, SWT.RADIO);
        upStreamBasinTraceMI.setText("Upstream Basin Trace");
        upStreamBasinTraceMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateD2DRefresh();
                fireTraceChangedEvent(CLICK_TYPE.UP);
            }
        });
        clickMenuItems.add(upStreamBasinTraceMI);

        downStreamBasinTraceMI = new MenuItem(clickMenu, SWT.RADIO);
        downStreamBasinTraceMI.setText("Downstream Basin Trace");
        downStreamBasinTraceMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateD2DRefresh();
                fireTraceChangedEvent(CLICK_TYPE.DOWN);
            }
        });
        clickMenuItems.add(downStreamBasinTraceMI);

        basinTrendMI = new MenuItem(clickMenu, SWT.RADIO);
        basinTrendMI.setText("Basin Trend");
        basinTrendMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateD2DRefresh();
                fireTraceChangedEvent(CLICK_TYPE.TREND);
            }
        });
        clickMenuItems.add(basinTrendMI);
    }

    private void createTopControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.marginWidth = 2;
        gl.marginHeight = 1;
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = 100;
        refreshD2DBtn = new Button(controlComp, SWT.PUSH);
        refreshD2DBtn.setText("Refresh D2D");
        refreshD2DBtn.setEnabled(false);
        refreshD2DBtn.setLayoutData(gd);
        refreshD2DBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                fireRefreshEvent();
                refreshD2DBtn.setEnabled(false);
                refreshD2DBtn.setBackground(null);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = 100;
        configSummaryBtn = new Button(controlComp, SWT.PUSH);
        configSummaryBtn.setText("Config Summary");
        configSummaryBtn.setLayoutData(gd);
        configSummaryBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                createConfigSummary(e.x, e.y);
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.minimumWidth = 100;
        validTimeLbl = new Label(controlComp, SWT.CENTER);
        validTimeLbl.setLayoutData(gd);

        updateValidTime();

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = 100;
        Button clearTraceBtn = new Button(controlComp, SWT.PUSH);
        clearTraceBtn.setText("Clear Trace");
        clearTraceBtn.setLayoutData(gd);
        clearTraceBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                fireTraceChangedEvent(CLICK_TYPE.CLEAR);
            }
        });
    }

    private void createGapTimeDurControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginWidth = 2;
        gl.marginHeight = 1;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        /*
         * Create Gap labels and Rate control
         */
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 2;
        Composite gapComp = new Composite(controlComp, SWT.NONE);
        gapComp.setLayout(gl);

        Label gapLbl = new Label(gapComp, SWT.NONE);
        gapLbl.setText("Gap: ");

        GridData gd = new GridData(40, SWT.DEFAULT);
        gapValueLbl = new Label(gapComp, SWT.RIGHT);
        gapValueLbl.setLayoutData(gd);
        updateGapValueLabel(0.00);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Label hrsLbl = new Label(gapComp, SWT.NONE);
        hrsLbl.setText("(hrs.)");
        hrsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Button rateBtn = new Button(gapComp, SWT.PUSH);
        rateBtn.setText("Rate");
        rateBtn.setLayoutData(gd);
        rateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                rateAction();
                fireFieldChangedEvent(FFMPRecord.FIELDS.RATE, true);
            }
        });

        /*
         * Create the vertical separator label.
         */
        new Label(controlComp, SWT.SEPARATOR | SWT.VERTICAL);

        /*
         * Create the Time Duration label data update label and time controls
         * composite.
         */
        gl = new GridLayout(1, false);
        gl.verticalSpacing = 2;
        Composite timeDurComp = new Composite(controlComp, SWT.NONE);
        timeDurComp.setLayout(gl);

        /*
         * Create a composite for the time label and the update label.
         */
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        Composite timeLableDataUpdateComp = new Composite(timeDurComp, SWT.NONE);
        timeLableDataUpdateComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        timeLableDataUpdateComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        // gd.widthHint = 325;
        timeDurLbl = new Label(timeLableDataUpdateComp, SWT.NONE);
        timeDurLbl.setText(timeDurationStr);
        timeDurLbl.setFont(timeDurFont);
        timeDurLbl.setLayoutData(gd);

        /*
         * Add the time duration scale to the display
         */
        timeDurScale = new TimeDurScaleComp(timeDurComp, this);
    }

    private void createTableControls() {
        /*
         * Create Group label and attribute button.
         */
        Composite labelAttrComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        labelAttrComp.setLayout(gl);
        labelAttrComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = 200;
        groupLbl = new Label(labelAttrComp, SWT.BORDER | SWT.CENTER);
        groupLbl.setBackground(getDisplay().getSystemColor(SWT.COLOR_GRAY));
        groupLbl.setLayoutData(gd);
        groupLbl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                groupLbl.setText("");
                ffmpTable.clearTableSelection();
                fireTableRestoreEvent();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        thresholdsBtn = new Button(labelAttrComp, SWT.PUSH);
        thresholdsBtn.setText("Thresholds");
        thresholdsBtn.setLayoutData(gd);
        thresholdsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Point dialogLoc = getDisplay().map(thresholdsBtn, null, e.x,
                        e.y + thresholdsBtn.getSize().y);
                popupMenu.setLocation(dialogLoc);
                popupMenu.setVisible(true);
            }
        });

        createPopupMenu();

        Button attrBtn = new Button(labelAttrComp, SWT.PUSH);
        attrBtn.setText("Attributes...");
        attrBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayAttributesDlg();
            }
        });
    }

    private void createTable() {
        tableComp = new Composite(shell, SWT.NONE);
        tableComp.setLayout(new GridLayout(1, false));
        tableComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        ffmpTable = new FFMPTableComp(tableComp, mainTableData, this,
                resource.getSiteKey());
    }

    private void createDataLoadLabel() {
        dataLoadComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataLoadComp.setLayout(gl);
        dataLoadComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataLoadingLbl = new Label(dataLoadComp, SWT.NONE);
        dataLoadingLbl.setText("");
        dataLoadingLbl.setFont(dataLoadFont);
        dataLoadingLbl.setLayoutData(gd);

        ((GridData) dataLoadComp.getLayoutData()).exclude = true;
        dataLoadComp.setVisible(false);
    }

    private void createPopupMenu() {
        popupMenu = new Menu(thresholdsBtn);

        // Loop over enum from config singleton to create menu items
        for (ThreshColNames colName : ThreshColNames.values()) {
            if (ffmpConfig.isColorCell(colName)) {
                // only add a menu item if colorCell is true
                MenuItem mi = new MenuItem(popupMenu, SWT.NONE);
                mi.setText(colName.name());
                mi.setData(colName);
                mi.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        MenuItem mi = (MenuItem) e.getSource();
                        ThreshColNames colName = (ThreshColNames) mi.getData();

                        displayThresholdsDialog(colName);
                    }
                });
            }
        }

        // Set the pop-up menu as the pop-up for the shell
        thresholdsBtn.setMenu(popupMenu);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#opened()
     */
    @Override
    protected void opened() {
        super.opened();
        this.dialogInitialized = true;
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void updateGapValueLabel(double gapVal) {

        if (gapVal > 0.75) {
            gapValueLbl.setBackground(getDisplay()
                    .getSystemColor(SWT.COLOR_RED));
        } else if (gapVal > 0.30) {
            gapValueLbl.setBackground(getDisplay().getSystemColor(
                    SWT.COLOR_YELLOW));
        } else {
            gapValueLbl.setBackground(getDisplay().getSystemColor(
                    SWT.COLOR_GREEN));
        }

        String str = String.format("%2.2f ", gapVal);
        gapValueLbl.setText(str);
    }

    private void initTimeDuration() {
        double tf = ffmpConfig.getTimeFrame();

        if (ffmpConfig.isSplit() == true) {
            timeDurScale.setSplit(ffmpConfig.isSplit());

            if (tf >= 1.00) {
                timeDurScale.setTimeDurationAndUpdate(tf);
            } else {
                timeDurScale.setTimeDurationAndUpdate(1.00);
            }
        } else {
            if (tf == 0.0) {
                rateAction();
            } else {
                timeDurScale.setTimeDurationAndUpdate(tf);
            }
        }
    }

    private void updateTimeDurationLabel(String str) {
        timeDurLbl.setText(str);
    }

    private void updateValidTime() {
        if (validTimeLbl != null && !this.isDisposed()) {
            if (this.date != null) {
                validTimeLbl.setText(dateFmt.format(this.date));
            } else {
                validTimeLbl.setText("NO DATE AVAILABLE");
            }
        }
    }

    private void rateAction() {
        // qpeMI.setSelection(true);
        updateTimeDurationLabel(timeDurationStr + " Rate!");
        time = 0.0;
        timeDurScale.setTimeDuration(0.00);
        this.updateGapValueLabel(0.00);
    }

    /**
     * Display the configuration summary in a pop-up dialog.
     * 
     * @param x
     *            X coordinate of the widget.
     * @param y
     *            Y coordinate of the widget.
     */
    private void createConfigSummary(int x, int y) {
        ConfigSummaryData cfgSumData = createSummaryData();
        Point buttonLoc = getDisplay().map(configSummaryBtn, null, x,
                y + configSummaryBtn.getSize().y);

        ConfigSummaryDlg cfgSumDlg = new ConfigSummaryDlg(shell, buttonLoc,
                cfgSumData);
        cfgSumDlg.open();
    }

    /**
     * Configuration summary data to be displayed in the configuration summary
     * dialog.
     * 
     * @return Configuration summary data.
     */
    private ConfigSummaryData createSummaryData() {
        ConfigSummaryData cfgSumData;
        List<String> includedCWAs = new ArrayList<String>();
        String layer = "";
        String clickAction = "";
        String displayType = "";

        for (MenuItem mi : cwaMenuItems) {
            if (mi.getSelection() == true) {
                includedCWAs.add(mi.getText());
            }
        }

        for (MenuItem mi : layerMenuItems) {
            if (mi.getSelection() == true) {
                layer = mi.getText();
                if (layer.contains("&&")) {
                    layer = layer.replaceAll("&&", "&");
                }
            }
        }

        for (MenuItem mi : clickMenuItems) {
            if (mi.getSelection() == true) {
                clickAction = mi.getText();
            }
        }

        for (MenuItem mi : d2dMenuItems) {
            if (mi.getSelection() == true) {
                displayType = mi.getText();
            }
        }

        cfgSumData = new ConfigSummaryData(layer, linkToFrameMI.getSelection(),
                worstCaseMI.getSelection(), maintainLayerMI.getSelection(),
                basinsInParentMI.getSelection(),
                (ArrayList<String>) includedCWAs, clickAction, displayType,
                autoRefreshMI.getSelection());

        return cfgSumData;
    }

    @Override
    public void timeDurationUpdated(double val, boolean split) {
        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        updateTimeDurationLabel(val, split);
        if (dialogInitialized) {
            fireTimeChangedEvent(val, split, false);
        }
        updateD2DRefresh();
    }

    private void updateD2DRefresh() {
        if ((autoRefreshMI.getSelection() == false) && dialogInitialized) {
            refreshD2DBtn.setEnabled(true);
            refreshD2DBtn.setBackground(refreshColor);
        }
    }

    private void updateTimeDurationLabel(double val, boolean split) {

        if (split == true) {
            StringBuilder sb = new StringBuilder(timeDurationStr);

            sb.append(" : Split! ");
            sb.append(String.format("%1.2f ", (val - 1.00)));
            sb.append("hrs. QPE and 1.0 hrs. QPF.");
            updateTimeDurationLabel(sb.toString());
        } else {
            if (val == 0.0) {
                updateTimeDurationLabel(timeDurationStr + " Rate!");
            } else {
                updateTimeDurationLabel(timeDurationStr);
            }
        }

        // Update the FFMP configuration with the new time duration.
        val = ((double) (Math.round(val * 100))) / 100;
        ffmpConfig.getFFMPConfigData().setTimeFrame(val);
    }

    private void displayCloseInstructions() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
        mb.setText("Information");
        mb.setMessage("To remove the table window,\nyou must clear the D2D display.");
        mb.open();
    }

    private void displayAttributesDlg() {
        if (attributeDlg == null) {
            attrData = ffmpTable.getVisibleColumns();
            attributeDlg = new AttributesDlg(shell, resource, attrData, this);
        }
        attributeDlg.open();
    }

    private void displayThresholdsDialog(ThreshColNames colName) {
        if ((attrThreshDlg == null) || (attrThreshDlg.isDisposed() == true)) {
            attrThreshDlg = new AttributeThresholdDlg(shell, colName, this);
            attrThreshDlg.open();
            attrThreshDlg = null;
        } else {
            attrThreshDlg.newThreshold(colName);
        }
    }

    /**
     * Called when the columns in the table are shown/hidden.
     */
    @Override
    public void attributeDisplayAction(boolean updateData,
            AttributesDlgData attrData) {
        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        this.attrData = attrData;

        // Update the data in the FFMPConfig with the selections
        this.ffmpConfig.setVisibleColumns(attrData);
        this.ffmpConfig.setAttrData(attrData);
        this.ffmpTable.showHideTableColumns();
        boolean changeSplit = false;

        if (timeDurScale.split != ffmpConfig.isSplit()) {
            changeSplit = true;
        }

        timeDurScale.setSplit(ffmpConfig.isSplit());
        updateTimeDurationLabel(timeDurScale.getSelectedHoursValue(),
                ffmpConfig.isSplit());

        if (updateData) {

            if (changeSplit) {
                fireTimeChangedEvent(timeDurScale.getSelectedHoursValue(),
                        ffmpConfig.isSplit(), true);
            }
            resource.clearTables();
            resource.getDrawable(resource.getPaintTime()).setDirty(true);
            FFMPMonitor.getInstance().fireMonitorEvent(
                    this.getClass().getName());

        }

        ffmpTable.calculateTableSize();
        shell.pack();
        shell.redraw();
        resetCursor();
    }

    @Override
    public void thresholdUpdated(ThreshColNames colName) {
        ffmpTable.updateThresholds(colName);

        if ((basinTrendDlg != null)
                && (basinTrendDlg.getCurrentShell().isDisposed() == false)) {
            basinTrendDlg.thresholdChanged();
        }
    }

    public void disposeDialog() {
        killDialog = true;
        if (shell != null) {
            shell.dispose();
        }
    }

    /**
     * Add listener
     * 
     * @param fl
     */
    public synchronized void addListener(FFMPListener fl) {
        ffmpListeners.add(fl);
    }

    /**
     * Remove listener
     * 
     * @param fl
     */
    public synchronized void removeListener(FFMPListener fl) {
        ffmpListeners.remove(fl);
    }

    public void fireTimeChangedEvent(double newTime, boolean split,
            boolean override) {

        FFMPRecord.FIELDS field = FFMPRecord.FIELDS.QPE;

        for (MenuItem mi : d2dMenuItems) {
            if (mi.getSelection() == true) {
                field = (FFMPRecord.FIELDS) mi.getData();
                break;
            }
        }

        if ((time != newTime) || override) {
            FFMPTimeChangeEvent ftce = new FFMPTimeChangeEvent(newTime, split);
            Iterator<FFMPListener> iter = ffmpListeners.iterator();
            while (iter.hasNext()) {
                try {
                    (iter.next()).timeChanged(ftce, field);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Time change failed in resource", e);
                }
            }

            time = newTime;
        } else {
            resetCursor();
        }
    }

    /**
     * Fire whenever the huc changes.
     */
    public void fireHucChangedEvent(String huc) {

        FFMPHUCChangeEvent fhce = new FFMPHUCChangeEvent(huc);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            (iter.next()).hucChanged(fhce);
        }
    }

    /**
     * Fire whenever the field changes.
     */
    private void fireFieldChangedEvent(FFMPRecord.FIELDS field,
            boolean waitCursor) {

        if (waitCursor == true) {
            shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        }

        FFMPFieldChangeEvent ffce = new FFMPFieldChangeEvent(field);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            try {
                (iter.next()).fieldChanged(ffce);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Field Change failed in resource", e);

                if (waitCursor == true) {
                    resetCursor();
                }
            }
        }
    }

    /**
     * Fire whenever the trace changes.
     */
    public void fireTraceChangedEvent(FFMPRecord.CLICK_TYPE trace) {

        FFMPStreamTraceEvent ffce = new FFMPStreamTraceEvent(trace);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            (iter.next()).traceChanged(ffce);
        }
    }

    /**
     * Fire whenever the CWA changes.
     */
    public void fireCWAChangedEvent(String cwa, boolean selected) {

        if (!selected) {
            cwas.remove(cwa);
        } else {
            if (!cwas.contains(cwa)) {
                cwas.add(cwa);
            }
        }

        FFMPCWAChangeEvent fcce = new FFMPCWAChangeEvent(
                (ArrayList<String>) cwas);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            (iter.next()).cwaChanged(fcce);
        }
    }

    /**
     * Fire whenever the Worst case aggregate button changes.
     */
    public void fireWorstCaseEvent(boolean worstCase) {

        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        FFMPWorstCaseEvent fwce = new FFMPWorstCaseEvent(worstCase);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            (iter.next()).isWorstCaseChanged(fwce);
        }
    }

    /**
     * Fire for auto refresh control.
     */
    public void fireAutoRefreshEvent(boolean isAutoRefresh) {

        FFMPAutoRefreshEvent fare = new FFMPAutoRefreshEvent(isAutoRefresh);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            (iter.next()).isAutoRefresh(fare);
        }
    }

    public void fireLinkToFrameUpdate(boolean linkToFrameFlag) {
        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            (iter.next()).setLinkToFrame(linkToFrameFlag);
        }
    }

    /**
     * Fire screen refresh.
     */
    public void fireRefreshEvent() {

        Iterator<FFMPListener> iter = ffmpListeners.iterator();

        while (iter.hasNext()) {
            (iter.next()).manualRefresh();
        }
    }

    /**
     * Fire screen recenter.
     */
    public void fireScreenRecenterEvent(String pfaf, int factor) {

        final String pfafs = pfaf;
        final int factors = factor;

        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                try {
                    FFMPMonitor monitor = FFMPMonitor.getInstance();
                    FFMPScreenCenterEvent fsce = null;
                    try {
                        Long l = Long.parseLong(pfafs);
                        fsce = new FFMPScreenCenterEvent(l, factors);
                    } catch (Exception e) {

                        try {
                            String newPfaf = pfafs.split("-")[0];
                            if (newPfaf != null) {
                                fsce = new FFMPScreenCenterEvent(Long
                                        .parseLong(newPfaf), factors);
                            } else {
                                if (ffmpConfig.getFFMPConfigData().getLayer()
                                        .equals("COUNTY")) {
                                    Long l = monitor
                                            .getTemplates(resource.getSiteKey())
                                            .getVirtualGageBasinMetaData(
                                                    resource.getSiteKey(),
                                                    pfafs).getParentPfaf();
                                    fsce = new FFMPScreenCenterEvent(l, factors);
                                }
                            }
                        } catch (Exception ie) {
                            fsce = new FFMPScreenCenterEvent(pfafs, factors);
                        }
                    }

                    resource.centerChanged(fsce);

                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Screen center failed in resource", e);
                }
            }

        });
    }

    private void updateCWA(SelectionEvent event) {
        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        updateD2DRefresh();
        updateCWAs();
        MenuItem mi = (MenuItem) event.getSource();
        fireCWAChangedEvent(mi.getText(), mi.getSelection());
    }

    private void updateCWA(MenuItem mi) {
        updateD2DRefresh();
        updateCWAs();
        fireCWAChangedEvent(mi.getText(), mi.getSelection());
    }

    private void updateLayer(SelectionEvent event) {

        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        updateD2DRefresh();

        MenuItem mi = (MenuItem) event.getSource();
        String huc = (String) mi.getData();

        ffmpConfig.getFFMPConfigData().setLayer(huc);
        fireHucChangedEvent(huc);
    }

    /**
     * Fire maintain layer event
     */
    private void fireMaintainLayerEvent(boolean maintainLayer) {

        FFMPMaintainLayerEvent fmle = new FFMPMaintainLayerEvent(maintainLayer);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();
        ffmpConfig.getFFMPConfigData().setMaintainLayer(maintainLayer);

        while (iter.hasNext()) {
            (iter.next()).isMaintainLayer(fmle);
        }
    }

    /**
     * Fire maintain layer event
     */
    private void fireParentBasinEvent(boolean parentLayer) {

        FFMPParentBasinEvent fpbe = new FFMPParentBasinEvent(parentLayer);
        Iterator<FFMPListener> iter = ffmpListeners.iterator();
        ffmpConfig.getFFMPConfigData().setOnlyForParent(parentLayer);

        while (iter.hasNext()) {
            (iter.next()).isParent(fpbe);
        }
    }

    /**
     * Fire Table reload event
     */
    private void fireTableRestoreEvent() {

        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        groupLbl.setText("");
        ffmpTable.clearTableSelection();

        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<FFMPListener> iter = ffmpListeners.iterator();
                while (iter.hasNext()) {
                    (iter.next()).restoreTable();
                }
            }
        });
    }

    /**
     * Fire Table reload event
     */
    private void fireConfigUpdateEvent() {
        final IMonitorConfigurationEvent me = new IMonitorConfigurationEvent(
                this);
        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                FFMPMonitor.getInstance().configUpdate(me);
            }
        });
    }

    /**
     * Fire Table reload event
     */
    private void fireGraphDataEvent(final String pfaf,
            final boolean differentPfaf, final Date ffmpDate) {

        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        // This needs to be in sync
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                // Must be a full 11 digit pfaf in order to display the graph.
                if ((pfaf.length() < 11) && pfaf.matches("\\d+")) {
                    resetCursor();
                    return;
                }
                try {
                    setGraphData(resource.getGraphData(pfaf), pfaf,
                            differentPfaf, ffmpDate);
                } catch (VizException e) {
                    shell.setCursor(null);
                    statusHandler.handle(Priority.PROBLEM,
                            "Graph Data request failed in resource", e);
                }

                resetCursor();
            }
        });
    }

    /**
     * Resets the data in the table
     * 
     * @param tData
     */
    public void resetData(FFMPTableData tData) {
        if (!ffmpTable.isDisposed()) {
            this.mainTableData = tData;
            // System.out.println("---" + tData.getTableRows().size());
            ffmpTable.clearTableSelection();
            // long time = System.currentTimeMillis();
            ffmpTable
                    .setCenteredAggregationKey(resource.centeredAggregationKey);
            ffmpTable.setTableData(mainTableData);
            // long time1 = System.currentTimeMillis();

            resetCursor();
            shell.pack();
            shell.redraw();

            // System.out
            // .println("Time to load Data into table " + (time1 - time));
        }
    }

    @Override
    public void tableSelection(String pfaf, String name) {
        if (groupLbl.getText().length() > 0) {
            groupLabelFlag = false;
        }

        if ((groupLbl.getText().length() == 0)
                || allOnlySmallBasinsMI.getSelection()) {
            groupLbl.setText(name);
        }

        shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        fireScreenRecenterEvent(pfaf, 1);
    }

    @Override
    public void displayBasinTrend(String pfaf) {
        if (pfaf != null) {
            shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            fireGraphDataEvent(pfaf, true, this.date);
        }
    }

    private void retrieveConfiguration() {

        LoadSaveConfigDlg loadDlg = new LoadSaveConfigDlg(shell,
                DialogType.OPEN);
        LocalizationFile fileName = (LocalizationFile) loadDlg.open();

        if (fileName == null) {
            return;
        }

        ffmpConfig.loadNewConfig(fileName);

        refreshDisplay(false);
    }

    private void saveConfiguration() {

        LoadSaveConfigDlg saveDlg = new LoadSaveConfigDlg(shell,
                DialogType.SAVE_AS);
        LocalizationFile fileName = (LocalizationFile) saveDlg.open();

        if (fileName == null) {
            return;
        }

        if (fileName.getFile().getParentFile().mkdirs() == false) {
            System.out.println("Did not not create directory(ies): "
                    + fileName.toString());
        }

        ffmpConfig.saveFFMPBasinConfig(fileName);
    }

    /**
     * Refresh the display with the new configuration changes.
     */
    private void refreshDisplay(boolean firstTime) {
        /*
         * Turn off auto refresh while we are updating.
         */
        fireAutoRefreshEvent(false);

        /*
         * Link to frame
         */
        linkToFrameMI.setSelection(ffmpConfig.getFFMPConfigData()
                .getLinkToFrame());
        fireLinkToFrameUpdate(ffmpConfig.getFFMPConfigData().getLinkToFrame());

        /*
         * Worst case
         */
        worstCaseMI.setSelection(ffmpConfig.getFFMPConfigData().getWorstCase());
        fireWorstCaseEvent(worstCaseMI.getSelection());

        /*
         * Maintain layer
         */
        maintainLayerMI.setSelection(ffmpConfig.getFFMPConfigData()
                .getMaintainLayer());
        fireMaintainLayerEvent(maintainLayerMI.getSelection());

        /*
         * Only for parent
         */
        basinsInParentMI.setSelection(ffmpConfig.getFFMPConfigData()
                .getOnlyForParent());
        fireParentBasinEvent(basinsInParentMI.getSelection());

        /*
         * Time duration
         */
        timeDurScale.setTimeDurationAndUpdate(ffmpConfig.getFFMPConfigData()
                .getTimeFrame());
        fireTimeChangedEvent(ffmpConfig.getFFMPConfigData().getTimeFrame(),
                false, false);

        /*
         * Layer
         */
        String layer = ffmpConfig.getFFMPConfigData().getLayer();

        try {
            clearMenuItems(layerMenuItems);

            for (int i = 0; i < layerMenuItems.size(); i++) {
                MenuItem mi = layerMenuItems.get(i);
                String menuDataType = (String) mi.getData();

                if (menuDataType.compareTo(layer) == 0) {
                    mi.setSelection(true);
                    break;
                }
            }

            fireHucChangedEvent(layer);
        } catch (IllegalArgumentException iae) {
            // Reset to county and select the county menu item.
            ffmpConfig.getFFMPConfigData().setLayer("COUNTY");
            layerMenuItems.get(1).setSelection(true);
            fireHucChangedEvent("COUNTY");
        }

        /*
         * D2D type
         */
        fireAutoRefreshEvent(true);
        String d2dType = ffmpConfig.getFFMPConfigData().getD2dType();
        FIELDS fieldCfg = null;
        try {
            clearMenuItems(d2dMenuItems);
            clearMenuItems(sourceMenuItems);
            fieldCfg = FFMPRecord.FIELDS.valueOf(d2dType);
            FfmpTableConfig ftc = FfmpTableConfig.getInstance();
            ftc.rereadConfigData(resource.getSiteKey());
            String guidSrc = ffmpConfig.getFFMPConfigData().getGuidSrc();

            for (int i = 0; i < d2dMenuItems.size(); i++) {
                FIELDS menuCfg = (FIELDS) (d2dMenuItems.get(i).getData());

                if (fieldCfg == menuCfg) {
                    d2dMenuItems.get(i).setSelection(true);
                    if ((menuCfg == FIELDS.DIFF) || (menuCfg == FIELDS.RATIO)) {
                        List<String> guidanceList = resource.getProduct()
                                .getAvailableGuidanceTypes();
                        for (int j = 0; j < guidanceList.size(); j++) {
                            if (guidanceList.get(j).equals(guidSrc)) {
                                this.sourceMenuItems.get(j).setSelection(true);
                                break;
                            }
                        }
                    }
                    break;
                }
            }

            fireFieldChangedEvent(fieldCfg, false);
        } catch (IllegalArgumentException iae) {
            // Reset to county and select the county menu item.
            ffmpConfig.getFFMPConfigData().setD2dType(
                    FFMPRecord.FIELDS.QPE.name());
            d2dMenuItems.get(0).setSelection(true);
            fireFieldChangedEvent(FFMPRecord.FIELDS.QPE, false);
        }

        fireAutoRefreshEvent(false);

        /*
         * CWAs
         * 
         * Note: On the first pass through, if the config file does not specify
         * a CWA the current site will be selected. If the config file is set to
         * have a selected CWA(s) they will be used.
         */
        String cwaStr = ffmpConfig.getFFMPConfigData().getIncludedCWAs();

        // Check if there are selected CWAs
        if (cwaStr.trim().length() > 0) {
            String[] cwaArray = cwaStr.trim().split(",");
            clearMenuItems(cwaMenuItems);

            if (cwaArray.length > 0) {
                for (String cwa : cwaArray) {
                    for (int i = 0; i < cwaMenuItems.size(); i++) {
                        if (cwaMenuItems.get(i).getText().compareTo(cwa) == 0) {
                            cwaMenuItems.get(i).setSelection(true);
                            break;
                        }
                    }
                }
            }
        } else {
            // If this is the first time through and there are no selected CWAs
            // then select the current site.
            if (firstTime == true) {
                String site = LocalizationManager.getInstance()
                        .getCurrentSite().trim().toUpperCase();

                for (MenuItem mi : cwaMenuItems) {
                    if (site.compareTo(mi.getText().trim().toUpperCase()) == 0) {
                        mi.setSelection(true);
                        updateCWA(mi);
                    }
                }
            }
        }

        /*
         * Auto refresh (do this last)
         */
        autoRefreshMI.setSelection(ffmpConfig.getFFMPConfigData()
                .getAutoRefresh());
        fireAutoRefreshEvent(autoRefreshMI.getSelection());

        if (autoRefreshMI.getSelection() == false) {
            // Fire a refresh since the auto-refresh is turned off
            fireRefreshEvent();
        }

        ffmpTable.sortTableUsingConfig();
    }

    /**
     * Update the config with the current selection of comma delimited CWAs.
     */
    private void updateCWAs() {
        StringBuilder sb = new StringBuilder();

        Iterator<MenuItem> iter = cwaMenuItems.iterator();

        // Create a list of selected CWAs
        while (iter.hasNext() == true) {
            MenuItem mi = iter.next();
            if (mi.getSelection() == true) {
                if (sb.length() != 0) {
                    sb.append(",");
                }

                sb.append(mi.getText().trim());
            }
        }

        // Set the list of CWAs
        ffmpConfig.getFFMPConfigData().setIncludedCWAs(sb.toString());
    }

    /**
     * Clear the selection of the menu items.
     * 
     * @param menuArray
     *            Array of menu items.
     */
    private void clearMenuItems(List<MenuItem> menuArray) {
        for (int i = 0; i < menuArray.size(); i++) {
            menuArray.get(i).setSelection(false);
        }
    }

    /**
     * I hate when we use calendar, time zone is always an issue
     * 
     * @param date
     */
    private void setValidTime(Date date) {
        this.date = date;
        if (date != null) {
            updateValidTime();
        }
    }

    /** set the graph Data by pfaf **/
    private void setGraphData(FFMPGraphData graphData, String pfaf,
            boolean differentPfaf, Date ffmpDate) {

        // this.graphData = graphData;
        // this.graphData.printGraphData();
        if ((shell != null) && !isDisposed()) {
            resetCursor();
        }

        if ((basinTrendDlg != null) && (!basinTrendDlg.isDisposed())
                && (basinTrendDlg.getShell().isDisposed() == false)) {
            // Create a new graph if the pfaf has changed
            if (differentPfaf == true) {
                basinTrendDlg.removeListener(this);
                basinTrendDlg.getShell().dispose();
                basinTrendDlg = null;
                basinTrendDlg = new BasinTrendDlg(shell, resource, ffmpDate,
                        pfaf, ffmpTable.isVGB(), graphData);
                basinTrendDlg.open();
                basinTrendDlg.addListener(this);
            } else {
                basinTrendDlg.setGraphData(graphData, ffmpDate, pfaf);
            }
        } else {
            if (!shell.isDisposed() && (ffmpTable != null)) {
                basinTrendDlg = new BasinTrendDlg(shell, resource, ffmpDate,
                        pfaf, ffmpTable.isVGB(), graphData);
                basinTrendDlg.addListener(this);
                basinTrendDlg.open();
            }
        }
    }

    public void allowNewTableUpdate() {
        allowNewTableUpdate = true;
    }

    public void updateLoadingLabel(FFMPLoaderStatus status) {
        if (dataLoadComp == null) {
            return;
        }

        GridData gd = (GridData) dataLoadComp.getLayoutData();

        // System.out.println("Status message...");

        if (gd.exclude == true) {
            ((GridData) dataLoadComp.getLayoutData()).exclude = false;
            dataLoadComp.setVisible(true);
            shell.pack();
        }

        String prefix = null;

        if (status.getLoaderType() == LOADER_TYPE.SECONDARY) {
            prefix = " Secondary Data Load: ";
        } else {
            prefix = " Tertiary Data Load: ";
        }

        if (status.isDone() == false) {
            dataLoadingLbl.setText(prefix + status.getMessage());
            dataLoadingLbl.setBackground(getDisplay().getSystemColor(
                    SWT.COLOR_CYAN));
        } else {
            dataLoadingLbl.setText("");
            dataLoadingLbl.setBackground(getDisplay().getSystemColor(
                    SWT.COLOR_WIDGET_BACKGROUND));

            ((GridData) dataLoadComp.getLayoutData()).exclude = true;
            dataLoadComp.setVisible(false);
            shell.pack();
        }

        resource.manageLoaders(status);
    }

    @Override
    public void notify(IMonitorEvent me) {

        if (!this.isDisposed()) {
            shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            FFMPTableDataLoader tableLoader = new FFMPTableDataLoader(me,
                    resource, basinTrendDlg, allowNewTableUpdate, sourceUpdate,
                    date, this);

            synchronized (retrievalQueue) {
                if (dataRetrieveThread == null || dataRetrieveThread.isDone()) {
                    retrievalQueue.clear();
                    dataRetrieveThread = tableLoader;
                    dataRetrieveThread.start();
                } else {
                    retrievalQueue.add(tableLoader);
                }
            }
        }
    }

    /**
     * Get the latest TableDataLoader and clear all previous loaders
     * 
     * @return
     */
    private FFMPTableDataLoader getLoader() {
        synchronized (retrievalQueue) {
            FFMPTableDataLoader loader = retrievalQueue.get(retrievalQueue
                    .size() - 1);
            retrievalQueue.clear();
            return loader;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.ISourceUpdate#
     * fireSourceUpdateEvent()
     */
    @Override
    public void fireSourceUpdateEvent() {
        sourceUpdate = true;
        fireConfigUpdateEvent();
    }

    /**
     * resets cursor from callbacks
     */
    public void resetCursor() {
        shell.setCursor(null);
    }

    /**
     * Rejoin the UI thread
     * 
     * @param updateData
     */
    public void tableDataUpdateComplete(FFMPTableDataUpdate updateData) {

        final FFMPTableDataUpdate fupdateData = updateData;

        if (!this.isDisposed()) {

            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    processUpdate(fupdateData);
                }
            });
        }
    }

    /**
     * Process the update
     */
    private void processUpdate(FFMPTableDataUpdate fupdateData) {
        allowNewTableUpdate = fupdateData.isAllowNewTableUpdate();
        sourceUpdate = fupdateData.isSourceUpdate();

        if (retrievalQueue.size() > 0) {
            dataRetrieveThread = getLoader();
            dataRetrieveThread.start();
            return;
        }

        if (fupdateData.getTableData() != null && groupLabelFlag) {
            resetData(fupdateData.getTableData());
        }

        groupLabelFlag = true;
        if (fupdateData.isFireGraph()) {
            fireGraphDataEvent(fupdateData.getGraphPfaf(), false,
                    fupdateData.getGraphTime());
        }

        setValidTime(fupdateData.getValidTime());
        updateGapValueLabel(fupdateData.getGapValueLabel());

        resetCursor();

    }

    /**
     * used to blank the group label when channging HUC while in an aggregate.
     */
    public void blankGroupLabel() {
        if (groupLbl != null) {
            groupLbl.setText("");
        }
    }
}
