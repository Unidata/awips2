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
package com.raytheon.viz.hydro.timeseries;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.shef.tables.RejecteddataId;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.ToggleTimeSeriesDlg.ITraceSelectionCB;
import com.raytheon.viz.hydro.timeseries.util.GraphData;
import com.raytheon.viz.hydro.timeseries.util.GraphInfo;
import com.raytheon.viz.hydro.timeseries.util.GraphInfo.DERIVE_PP;
import com.raytheon.viz.hydro.timeseries.util.GroupData;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo.TraceMode;
import com.raytheon.viz.hydro.timeseries.util.PageData;
import com.raytheon.viz.hydro.timeseries.util.PageInfo;
import com.raytheon.viz.hydro.timeseries.util.StageDischargeUtils;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesPoint.MODE;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.ForecastData;
import com.raytheon.viz.hydrocommon.data.LoadMaxFcst;
import com.raytheon.viz.hydrocommon.util.HydroUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.LineSegment;

/**
 * This class displays the Time Series Display dialog for Hydroview.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Nov 29, 2007  373         lvenable     Initial creation
 * Nov 11, 2010  5831        lbousaid     Fixed Graphical PgDown & PgUp
 * Jan 19, 2011  5281        lbousaidi    make the graph window larger data was
 *                                        too compressed.
 * Jan 24, 2011  7799        bkowal       ensured that "Data Only" would be the
 *                                        default selection under both the Graph
 *                                        - Scale Stages sub-menu and the
 *                                        Options - Batch Scale Stages sub-menu.
 * Jan 26, 2011  5557        bkowal       finished the implementation of and
 *                                        enabled "reverse video" printing.
 *                                        Updated the printing capability so
 *                                        that it would place the entire time
 *                                        series within the printable area of
 *                                        the page.
 * Mar 04, 2011  7644        lbousaid     fixed Zoom in feature
 * May 30, 2012  14967       wkwock       fix insert deleted data to
 *                                        rejecteddata table
 * Jul 23, 2012  15195       mpduff       Fix dates for displaying groups
 * Dec 06, 2012  15066       wkwock       Fix "ctrl+r" not work in group mode
 * Jan 22, 2013  14903       lbousaidi    Fix display error after save to DB.
 * Jan 30, 2012  15459       mpduff       Redmine 1560 - Make graph canvases
 *                                        redraw on page up/down.
 * Feb 06, 2013  1578        rferrel      Code cleanup for non-blocking dialogs.
 * Apr 24, 2013  1921        mpduff       Fix zoom reset to only reset the
 *                                        "active" graph
 * May 14, 2014  16388       xwei         updated the insertion of rejecteddata
 *                                        table.
 * Jan 29, 2016  89          tgurney      Add missing maximize button in trim
 * Feb 16, 2016  5342        bkowal       Ensure rejected data keys are unique.
 * Oct 24, 2016  5955        randerso     Code/JavaDoc cleanup
 * Mar 08, 2017  17643       jdeng        Fix errors when deleting/setting data
 *                                        to missing
 * Mar 23, 2017  6117        bsteffen     Workaround crash when printing images.
 * Jan 16, 2018  6772        lvenable     Retrieved the data after saving so the
 *                                        displayed data matches the data saved
 *                                        to the DB.
 * Apr 16, 2018  6619        randerso     Add some border so you can find the
 *                                        edge.
 * May 16, 2018  6749        randerso     Fixed graph printing and saving. Code
 *                                        cleanup.
 * Jun 07, 2018  6640        dgilling     Replace Page menu with root Page Up
 *                                        and Page Down menu items.
 * Jun 27, 2018  6748        randerso     Major re-write to better match A1
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class TimeSeriesDisplayDlg extends CaveSWTDialog
        implements ITraceSelectionCB {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeSeriesDisplayDlg.class);

    private enum ADD_LOCATION {
        NONE, LEFT, MIDDLE, RIGHT;
    };

    private static class MoveInfo {
        private boolean valid;

        private int x, y, x1, y1, x2, y2;

        private int leftLimit, rightLimit;

        private int leftIndex, rightIndex;

        private int traceIndex;

        private ADD_LOCATION addLoc;

        private int numLegs;
    }

    private enum EDIT_STATE {
        START, ACTIVE, RESET
    };

    private static final int EDIT_COLOR = 20;

    private static final String FEET = "ft";

    private static final String CFS = "cfs";

    private static final String KCFS = "kcfs";

    /**
     * print margin in inches
     */
    private static final float PRINT_MARGIN = 0.5f;

    /** Location and size of dialog. */
    private static Rectangle bounds = null;

    /**
     * Show latest forecast menu item.
     */
    private MenuItem showLatestFcstMI;

    private MenuItem scaleStagesMI;

    private MenuItem showPcSubMI;

    /**
     * Data Only menu item.
     */
    private MenuItem dataOnlyMI;

    /**
     * Data Only, Show Categories menu item.
     */
    private MenuItem dataOnlyShowCatMI;

    /**
     * Data and Categories menu item.
     */
    private MenuItem dataAndCategoriesMI;

    /**
     * Grid Lines menu item.
     */
    private MenuItem gridLinesMI;

    /**
     * Batch Data Only menu item.
     */
    private MenuItem batchDataOnlyMI;

    /**
     * Batch Data Only, Show Categories menu item.
     */
    private MenuItem batchDataOnlyShowCatMI;

    /**
     * Batch Data and Categories menu item.
     */
    private MenuItem batchDataAndCategoriesMI;

    /**
     * Select Trace menu item.
     */
    private MenuItem selectTraceMI;

    /**
     * Insert menu item.
     */
    private MenuItem insertMI;

    /**
     * Delete menu item.
     */
    private MenuItem deleteMI;

    /**
     * Move menu item.
     */
    private MenuItem moveMI;

    /**
     * Set Missing menu item.
     */
    private MenuItem setMissingMI;

    /**
     * Save to Database menu item.
     */
    private MenuItem saveToDatabaseMI;

    /**
     * Cancel Changes menu item.
     */
    private MenuItem cancelChangesMI;

    private TimeSeriesPoint.MODE editMode = MODE.NONE;

    private EDIT_STATE editState = EDIT_STATE.RESET;

    private int editCount = 0;

    /** The group data */
    private GroupData groupData;

    /** The group info */
    private GroupInfo groupInfo;

    /**
     * Display canvas.
     */
    private TimeSeriesDisplayCanvas displayCanvas;

    /** A flag for currently zooming or not */
    private boolean zooming = false;

    /**
     * The main graph Composite.
     */
    private Composite mainComp = null;

    /** parent dialog for this display */
    private TimeSeriesDlg parentDialog = null;

    private ToggleTimeSeriesDlg toggleTimeSeriesDlg;

    private Point mouseDownPoint;

    private Point crossHairPoint;

    private Rectangle rubberBand;

    private MoveInfo moveInfo;

    private int ppDur;

    /**
     * Constructor.
     *
     * @param parentDialog
     *            Parent dialog.
     * @param groupData
     *            the GroupData for this graph
     */
    public TimeSeriesDisplayDlg(TimeSeriesDlg parentDialog,
            GroupData groupData) {
        super(parentDialog.getShell(),
                SWT.DIALOG_TRIM | SWT.MIN | SWT.MAX | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL);
        setText("Time Series Display");

        this.parentDialog = parentDialog;
        this.groupData = groupData;
        this.groupInfo = groupData.getGroupInfo();
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.verticalSpacing = 0;// 3;
        mainLayout.marginHeight = 0;// 1;
        mainLayout.marginWidth = 0;// 1;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        // Initialize all of the controls and layouts
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createPageMenu(menuBar);
        createGraphMenu(menuBar);
        createOptionsMenu(menuBar);
        createEditMenu(menuBar);

        mainComp = new Composite(shell, SWT.NONE);
        GridLayout mainGridLayout = new GridLayout(1, true);
        mainGridLayout.verticalSpacing = 0;
        mainGridLayout.horizontalSpacing = 0;
        mainComp.setLayout(mainGridLayout);
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        createDisplayCanvas();

        shell.setMenuBar(menuBar);

        shell.setMinimumSize(new Point(900, 900));

        if (!groupInfo.isGroupSelected()) {
            // Get default values from Apps Defaults
            getDefaultShowcat();
        }

        GraphData activeGraph = groupData.getActiveGraph();
        if (activeGraph == null) {
            /* set the first graph as the active graph */
            activeGraph = groupData.getPageDataList().get(0).getGraphDataList()
                    .get(0);
        }
        setActiveGraph(activeGraph);
    }

    @Override
    public boolean shouldClose() {
        synchronized (TimeSeriesDisplayDlg.class) {
            /*
             * need to save shell bounds before shell is disposed since
             * getBounds does not return the correct x, y shell is closed.
             */
            bounds = shell.getBounds();
        }
        return super.shouldClose();
    }

    @Override
    protected void preOpened() {
        super.preOpened();

        if (bounds != null) {
            shell.setBounds(bounds);
        }
    }

    /**
     * This dialog's parent dialog.
     *
     * @return parentDialog
     */
    public TimeSeriesDlg getParentDialog() {
        return this.parentDialog;
    }

    /**
     * Create the File menu.
     *
     * @param menuBar
     *            The main menu bar.
     */
    private void createFileMenu(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the file menu
        // ----------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // --------------------------------------------
        // Create the Time Series Control menu item
        // --------------------------------------------
        MenuItem timeSeriesControlMI = new MenuItem(fileMenu, SWT.NONE);
        timeSeriesControlMI.setText("&Time Series Control\tCtrl+T");
        timeSeriesControlMI.setAccelerator(SWT.CTRL | 'T');
        timeSeriesControlMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getParent().setFocus();
            }
        });

        // -----------------------------------------
        // Create the Save menu item
        // -----------------------------------------
        MenuItem saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("&Save\tCtrl+S");
        saveMI.setAccelerator(SWT.CTRL | 'S');
        saveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveGraph();
            }
        });

        // -----------------------------------------
        // Create the Print menu item and sub menu
        // -----------------------------------------
        MenuItem printSubMI = new MenuItem(fileMenu, SWT.CASCADE);
        printSubMI.setText("Print");

        Menu printSubMenu = new Menu(shell, SWT.DROP_DOWN);
        printSubMI.setMenu(printSubMenu);

        createPrintSubMenu(printSubMenu);

        // -----------------------------------------
        // Create the Quit menu item
        // -----------------------------------------
        MenuItem quitMI = new MenuItem(fileMenu, SWT.NONE);
        quitMI.setText("&Quit\tCtrl+Q");
        quitMI.setAccelerator(SWT.CTRL | 'Q');
        quitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Create the Page Up and Page Down menu items.
     *
     * @param menuBar
     *            The main menu bar.
     */
    private void createPageMenu(Menu menuBar) {
        /*
         * Not sure if SWT/GTK intended for root menu items to not contain a
         * menu that appears when clicked. To activate these menu items you need
         * to double-click on them. Once to select the menu item and another to
         * launch the SelectionEvent.
         */
        MenuItem pageUpMI = new MenuItem(menuBar, SWT.NONE);
        pageUpMI.setText("Page Up");
        pageUpMI.setAccelerator(SWT.PAGE_UP);
        pageUpMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                changePage(-1);
            }
        });

        MenuItem pageDnMI = new MenuItem(menuBar, SWT.NONE);
        pageDnMI.setText("Page Down");
        pageDnMI.setAccelerator(SWT.PAGE_DOWN);
        pageDnMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                changePage(+1);
            }
        });
    }

    /**
     * Create the Graph menu.
     *
     * @param menuBar
     *            The main menu bar.
     */
    private void createGraphMenu(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the Graph menu
        // ----------------------------------------
        MenuItem graphMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        graphMenuItem.setText("Graph");

        // Create the File menu item with a Graph "dropdown" menu
        Menu graphMenu = new Menu(menuBar);
        graphMenuItem.setMenu(graphMenu);

        // -----------------------------------------
        // Create the Zoom menu item and sub menu
        // -----------------------------------------
        MenuItem zoomSubMI = new MenuItem(graphMenu, SWT.CASCADE);
        zoomSubMI.setText("Zoom");

        Menu zoomSubMenu = new Menu(shell, SWT.DROP_DOWN);
        zoomSubMI.setMenu(zoomSubMenu);

        createZoomSubMenu(zoomSubMenu);

        showPcSubMI = new MenuItem(graphMenu, SWT.CASCADE);
        showPcSubMI.setText("Show PC as 1Hr. PP");
        showPcSubMI.setEnabled(false);

        Menu showPcSubMenu = new Menu(shell, SWT.DROP_DOWN);
        showPcSubMI.setMenu(showPcSubMenu);

        createShowPcSubMenu(showPcSubMenu);

        // ------------------------------------------
        // Create Show Latest Forecast menu item
        // ------------------------------------------
        showLatestFcstMI = new MenuItem(graphMenu, SWT.CHECK);
        showLatestFcstMI.setText("Show Latest &Forecast Only\tCtrl-F");
        showLatestFcstMI.setAccelerator(SWT.CTRL | 'F');
        showLatestFcstMI.setEnabled(false);
        showLatestFcstMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GraphData activeGraph = groupData.getActiveGraph();
                if (activeGraph != null) {
                    activeGraph.getGraphInfo()
                            .setLatestfcstonly(showLatestFcstMI.getSelection());
                    activeGraph.findMinMax(true, groupData.getBeginDate(),
                            groupData.getEndDate());
                    displayCanvas.redraw();
                }
            }
        });

        scaleStagesMI = new MenuItem(graphMenu, SWT.CASCADE);
        scaleStagesMI.setText("Scale Stages");
        scaleStagesMI.setEnabled(false);

        Menu scaleStagesSubMenu = new Menu(shell, SWT.DROP_DOWN);
        scaleStagesMI.setMenu(scaleStagesSubMenu);

        createScaleStagesSubMenu(scaleStagesSubMenu);
    }

    /**
     * Create the Options menu.
     *
     * @param menuBar
     *            The main menu bar.
     */
    private void createOptionsMenu(Menu menuBar) {
        // ------------------------------------------
        // Create all the items in the Options menu
        // ------------------------------------------
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("Options");

        // Create the File menu item with a Graph "dropdown" menu
        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        // ------------------------------------------
        // Create Grid Lines menu item
        // ------------------------------------------
        gridLinesMI = new MenuItem(optionsMenu, SWT.CHECK);
        gridLinesMI.setText("&Grid Lines\tCtrl-G");
        gridLinesMI.setAccelerator(SWT.CTRL | 'G');

        gridLinesMI.setSelection(groupInfo.isGridLines());
        gridLinesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                groupInfo.setGridLines(gridLinesMI.getSelection());
                displayCanvas.redraw();
            }
        });

        // ------------------------------------------------------
        // Create the Plot menu item and sub menu
        // ------------------------------------------------------
        MenuItem plotMI = new MenuItem(optionsMenu, SWT.CASCADE);
        plotMI.setText("Plot");

        Menu plotSubMenu = new Menu(shell, SWT.DROP_DOWN);
        plotMI.setMenu(plotSubMenu);

        createPlotSubMenu(plotSubMenu);

        // --------------------------------------------------------
        // Create the Batch Scale Stages menu item and sub menu
        // --------------------------------------------------------
        MenuItem batchScaleStagesMI = new MenuItem(optionsMenu, SWT.CASCADE);
        batchScaleStagesMI.setText("Batch Scale Stages");

        Menu batchScaleStagesSubMenu = new Menu(shell, SWT.DROP_DOWN);
        batchScaleStagesMI.setMenu(batchScaleStagesSubMenu);

        createBatchScaleSubMenu(batchScaleStagesSubMenu);
    }

    /**
     * Create the Edit menu.
     *
     * @param menuBar
     *            The main menu bar.
     */
    private void createEditMenu(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the Graph menu
        // ----------------------------------------
        MenuItem editMI = new MenuItem(menuBar, SWT.CASCADE);
        editMI.setText("Edit");

        // Create the File menu item with a Edit "dropdown" menu
        Menu editMenu = new Menu(menuBar);
        editMI.setMenu(editMenu);

        // ------------------------------------------
        // Create Select Trace menu item
        // ------------------------------------------
        selectTraceMI = new MenuItem(editMenu, SWT.CHECK);
        selectTraceMI.setText("S&elect Trace\tCtrl-E");
        selectTraceMI.setAccelerator(SWT.CTRL | 'E');
        selectTraceMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (((MenuItem) e.widget).getSelection()) {
                    zooming = false;
                    crossHairPoint = null;
                    editState = EDIT_STATE.START;
                } else if (groupData.getSelectedTrace() == null) {
                    resetMenuItemsAndLists();
                }
            }
        });

        // Add a menu separator.
        new MenuItem(editMenu, SWT.SEPARATOR);

        /* Selection Adapter for edit mode buttons */
        SelectionAdapter modeSA = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                MenuItem item = (MenuItem) e.widget;
                if (item.getSelection()) {
                    editMode = (MODE) item.getData();
                }
            }

        };

        // ------------------------------------------
        // Create Insert menu item
        // ------------------------------------------
        insertMI = new MenuItem(editMenu, SWT.RADIO);
        insertMI.setText("I&nsert\tCtrl-N");
        insertMI.setData(MODE.ADD);
        insertMI.setAccelerator(SWT.CTRL | 'N');
        insertMI.setEnabled(false);
        insertMI.addSelectionListener(modeSA);

        // ------------------------------------------
        // Create Delete menu item
        // ------------------------------------------
        deleteMI = new MenuItem(editMenu, SWT.RADIO);
        deleteMI.setText("Delete\tCtrl-Y");
        deleteMI.setData(MODE.DELETE);
        deleteMI.setAccelerator(SWT.CTRL | 'Y');
        deleteMI.setEnabled(false);
        deleteMI.addSelectionListener(modeSA);

        // ------------------------------------------
        // Create Move menu item
        // ------------------------------------------
        moveMI = new MenuItem(editMenu, SWT.RADIO);
        moveMI.setText("&Move\tCtrl-M");
        moveMI.setData(MODE.MOVE);
        moveMI.setAccelerator(SWT.CTRL | 'M');
        moveMI.setEnabled(false);
        moveMI.addSelectionListener(modeSA);

        // ------------------------------------------
        // Create Set Missing menu item
        // ------------------------------------------
        setMissingMI = new MenuItem(editMenu, SWT.RADIO);
        setMissingMI.setText("&Set Missing\tCtrl-S");
        setMissingMI.setData(MODE.SETMISSING);
        setMissingMI.setAccelerator(SWT.CTRL | 'S');
        setMissingMI.setEnabled(false);
        setMissingMI.addSelectionListener(modeSA);

        // Add a menu separator.
        new MenuItem(editMenu, SWT.SEPARATOR);

        // ------------------------------------------
        // Create Save to Database menu item
        // ------------------------------------------
        saveToDatabaseMI = new MenuItem(editMenu, SWT.NONE);
        saveToDatabaseMI.setText("Save to &Database\tCtrl-D");
        saveToDatabaseMI.setAccelerator(SWT.CTRL | 'D');
        saveToDatabaseMI.setEnabled(false);
        saveToDatabaseMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveCancelEdits(true);
            }
        });

        // ------------------------------------------
        // Create Save to Database menu item
        // ------------------------------------------
        cancelChangesMI = new MenuItem(editMenu, SWT.NONE);
        cancelChangesMI.setText("&Cancel Changes\tCtrl-C");
        cancelChangesMI.setAccelerator(SWT.CTRL | 'C');
        cancelChangesMI.setEnabled(false);
        cancelChangesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveCancelEdits(false);
            }
        });
    }

    /**
     * Create the Print sub menu.
     *
     * @param printSubMenu
     *            The print menu.
     */
    private void createPrintSubMenu(Menu printSubMenu) {
        MenuItem reverseVideoMI = new MenuItem(printSubMenu, SWT.NONE);
        reverseVideoMI.setText("Reverse Video");
        reverseVideoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                print(true);
            }
        });

        MenuItem normalMI = new MenuItem(printSubMenu, SWT.NONE);
        normalMI.setText("Normal");
        normalMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                print(false);
            }
        });
    }

    /**
     * Create the Zoom sub menu.
     *
     * @param zoomSubMenu
     *            The Zoom menu.
     */
    private void createZoomSubMenu(Menu zoomSubMenu) {
        MenuItem setMI = new MenuItem(zoomSubMenu, SWT.NONE);
        setMI.setText("Set\tCtrl+Z");
        setMI.setAccelerator(SWT.CTRL | 'Z');
        setMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                zooming = true;
                displayCanvas.setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_CROSS));
            }
        });

        MenuItem resetMI = new MenuItem(zoomSubMenu, SWT.NONE);
        resetMI.setText("&Reset\tCtrl+R");
        resetMI.setAccelerator(SWT.CTRL | 'R');
        resetMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                zooming = false;
                unZoom();
            }
        });
    }

    /**
     * Create the Show PC as 1Hr PP sub menu.
     *
     * @param showPcSubMenu
     *            The Show PC as 1Hr PP menu.
     */
    private void createShowPcSubMenu(Menu showPcSubMenu) {
        SelectionAdapter selectionAdapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setPcAsPP((DERIVE_PP) e.widget.getData());
            }
        };

        MenuItem offMI = new MenuItem(showPcSubMenu, SWT.RADIO);
        offMI.setData(DERIVE_PP.NO_PC_TO_PP);
        offMI.setText("&Off\tCtrl+O");
        offMI.setAccelerator(SWT.CTRL | 'O');
        offMI.setSelection(true);
        offMI.addSelectionListener(selectionAdapter);

        MenuItem interpolateMI = new MenuItem(showPcSubMenu, SWT.RADIO);
        interpolateMI.setData(DERIVE_PP.INTERPOLATE);
        interpolateMI.setText("&Interpolate\tCtrl+I");
        interpolateMI.setAccelerator(SWT.CTRL | 'I');
        interpolateMI.addSelectionListener(selectionAdapter);

        MenuItem assignMI = new MenuItem(showPcSubMenu, SWT.RADIO);
        assignMI.setData(DERIVE_PP.ASSIGN);
        assignMI.setText("&Assign\tCtrl+A");
        assignMI.setAccelerator(SWT.CTRL | 'A');
        assignMI.addSelectionListener(selectionAdapter);
    }

    protected void setPcAsPP(DERIVE_PP data) {
        GraphData activeGraph = groupData.getActiveGraph();
        if (activeGraph != null) {
            activeGraph.getGraphInfo().setDerivepp(data);
        }
        displayCanvas.redraw();
    }

    /**
     * Create the Scale Stages sub menu.
     *
     * @param scaleStagesSubMenu
     *            The Scale Stages menu.
     */
    private void createScaleStagesSubMenu(Menu scaleStagesSubMenu) {
        dataOnlyMI = new MenuItem(scaleStagesSubMenu, SWT.RADIO);
        dataOnlyMI.setText("Data Only\tCtrl+1");
        dataOnlyMI.setAccelerator(SWT.CTRL | '1');
        dataOnlyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                batchDataOnlyShowCatMI.setSelection(false);
                batchDataOnlyMI.setSelection(false);
                batchDataAndCategoriesMI.setSelection(false);

                setScaleAndCat(true, false, false);
                displayCanvas.redraw();
            }
        });

        dataOnlyShowCatMI = new MenuItem(scaleStagesSubMenu, SWT.RADIO);
        dataOnlyShowCatMI.setText("Data Only, Show Categories\tCtrl+2");
        dataOnlyShowCatMI.setAccelerator(SWT.CTRL | '2');
        dataOnlyShowCatMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                batchDataOnlyShowCatMI.setSelection(false);
                batchDataOnlyMI.setSelection(false);
                batchDataAndCategoriesMI.setSelection(false);

                setScaleAndCat(true, true, false);
                displayCanvas.redraw();
            }
        });

        dataAndCategoriesMI = new MenuItem(scaleStagesSubMenu, SWT.RADIO);
        dataAndCategoriesMI.setText("Data and Categories\tCtrl+3");
        dataAndCategoriesMI.setAccelerator(SWT.CTRL | '3');
        dataAndCategoriesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                batchDataOnlyShowCatMI.setSelection(false);
                batchDataOnlyMI.setSelection(false);
                batchDataAndCategoriesMI.setSelection(false);

                setScaleAndCat(false, true, false);
                displayCanvas.redraw();
            }
        });
    }

    /**
     * Create the Plot sub menu.
     *
     * @param plotSubMenu
     *            The Plot menu.
     */
    private void createPlotSubMenu(Menu plotSubMenu) {
        SelectionAdapter selectionAdapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                groupData.getGroupInfo()
                        .setTraceMode((TraceMode) e.widget.getData());
                displayCanvas.redraw();
            }
        };

        MenuItem pointsMI = new MenuItem(plotSubMenu, SWT.RADIO);
        pointsMI.setText("&Points\tCtrl+P");
        pointsMI.setAccelerator(SWT.CTRL | 'P');
        pointsMI.addSelectionListener(selectionAdapter);
        pointsMI.setData(TraceMode.POINTS);

        MenuItem linesMI = new MenuItem(plotSubMenu, SWT.RADIO);
        linesMI.setText("&Lines\tCtrl+L");
        linesMI.setAccelerator(SWT.CTRL | 'L');
        linesMI.addSelectionListener(selectionAdapter);
        linesMI.setData(TraceMode.LINES);

        MenuItem bothMI = new MenuItem(plotSubMenu, SWT.RADIO);
        bothMI.setText("&Both\tCtrl+B");
        bothMI.setAccelerator(SWT.CTRL | 'B');
        bothMI.setSelection(true);
        bothMI.addSelectionListener(selectionAdapter);
        bothMI.setData(TraceMode.BOTH);

        /* initialize the menu selection */
        pointsMI.setSelection(false);
        linesMI.setSelection(false);
        bothMI.setSelection(false);

        TraceMode traceMode = groupInfo.getTraceMode();
        switch (traceMode) {
        case POINTS:
            pointsMI.setSelection(true);
            break;

        case LINES:
            linesMI.setSelection(true);
            break;

        case BOTH:
        default:
            bothMI.setSelection(true);
            break;
        }

    }

    /**
     * Create the Batch Scale Stages sub menu.
     *
     * @param batchScaleStagesSubMenu
     *            The Batch Scale Stages menu.
     */
    private void createBatchScaleSubMenu(Menu batchScaleStagesSubMenu) {
        batchDataOnlyMI = new MenuItem(batchScaleStagesSubMenu, SWT.RADIO);
        batchDataOnlyMI.setText("Data Only\tCtrl+F1");
        batchDataOnlyMI.setAccelerator(SWT.CTRL | SWT.F1);
        batchDataOnlyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setScaleAndCat(true, false, true);
                displayCanvas.redraw();
            }
        });

        batchDataOnlyShowCatMI = new MenuItem(batchScaleStagesSubMenu,
                SWT.RADIO);
        batchDataOnlyShowCatMI.setText("Data Only, Show Categories\tCtrl+F2");
        batchDataOnlyShowCatMI.setAccelerator(SWT.CTRL | SWT.F2);
        batchDataOnlyShowCatMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setScaleAndCat(true, true, true);
                displayCanvas.redraw();
            }
        });

        batchDataAndCategoriesMI = new MenuItem(batchScaleStagesSubMenu,
                SWT.RADIO);
        batchDataAndCategoriesMI.setText("Data and Categories\tCtrl+F3");
        batchDataAndCategoriesMI.setAccelerator(SWT.CTRL | SWT.F3);
        batchDataAndCategoriesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setScaleAndCat(false, true, true);
                displayCanvas.redraw();
            }
        });
    }

    /**
     * @param yScale
     *            true for Data, false for Category
     * @param showCat
     *            true to show categories
     * @param allGraphs
     *            true for all graphs, false for current graph
     */
    private void setScaleAndCat(boolean yScale, boolean showCat,
            boolean allGraphs) {

        GraphData activeGraph = groupData.getActiveGraph();
        for (PageData pageData : groupData.getPageDataList()) {
            for (GraphData graphData : pageData.getGraphDataList()) {
                if (allGraphs || graphData == activeGraph) {
                    graphData.getGraphInfo().setYscaleToData(yScale);
                    graphData.getGraphInfo().setShowcat(showCat);
                }
            }
        }
    }

    /**
     * Reset the menu items under the edit menu and clear the insert, edit, and
     * delete lists.
     */
    private void resetMenuItemsAndLists() {
        insertMI.setEnabled(false);
        insertMI.setSelection(false);
        deleteMI.setEnabled(false);
        deleteMI.setSelection(false);
        moveMI.setEnabled(false);
        moveMI.setSelection(false);
        setMissingMI.setEnabled(false);
        setMissingMI.setSelection(false);
        saveToDatabaseMI.setEnabled(false);
        cancelChangesMI.setEnabled(false);
        selectTraceMI.setEnabled(true);

        groupData.setSelectedTrace(null);
        editState = EDIT_STATE.RESET;
        displayCanvas.setCursor(null);
    }

    /**
     * From Apps defaults get the showCatValue.
     */
    private void getDefaultShowcat() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String gadValue = appsDefaults.getToken("timeseries_showcat", null);
        int tokenShowCat;
        if (gadValue != null) {
            try {
                tokenShowCat = Integer.parseInt(gadValue) - 1;
            } catch (NumberFormatException e) {
                tokenShowCat = -1;
            }

            if (tokenShowCat < 0 || tokenShowCat > 2) {
                statusHandler.error(String.format(
                        "The current value \"%s\" set for the token \"timeseries_showcat\" "
                                + "is not valid. Using default value:\"scale by data, show categories\".",
                        gadValue));
                tokenShowCat = 1;
            }
        } else {
            statusHandler
                    .warn("There is no value currently set for the token \"timeseries_showcat\". "
                            + "Using default value: \"scale by data, show categories\".");
            tokenShowCat = 1;
        }

        /* update graph info to match selection */
        /* NOTE: this method is only called in STATION mode */
        boolean yScaleToData = false;
        boolean showcat = false;
        if (tokenShowCat == 0) {
            yScaleToData = true;
        } else if (tokenShowCat == 1) {
            yScaleToData = true;
            showcat = true;
        } else {
            showcat = true;
        }

        for (PageInfo pageInfo : groupInfo.getPageInfoList()) {
            for (GraphInfo graphInfo : pageInfo.getGraphInfoList()) {
                graphInfo.setYscaleToData(yScaleToData);
                graphInfo.setShowcat(showcat);
            }
        }
    }

    /**
     * If pointList is for a Max Forecast perform an update.
     *
     * @param pointList
     */
    private void updateMaxFcst(List<ForecastData> pointList) {
        ForecastData point = pointList.get(0);
        String pe = point.getPe();
        String ts = point.getTs();

        if (ts.startsWith("F") || ts.startsWith("C")) {
            // call Load Max Forecast if update or insert of H or Q PE's
            if (pe.toUpperCase().startsWith("H")
                    || pe.toUpperCase().startsWith("Q")) {
                String lid = point.getLid();

                // call Load Max Forecast if update or insert of H or Q PE's
                if (pe.toUpperCase().startsWith("H")
                        || pe.toUpperCase().startsWith("Q")) {
                    try {
                        LoadMaxFcst.loadMaxFcstItem(lid, pe, ts);
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error updating MaxFcst: " + e);
                    }
                }
            }
        }
    }

    /**
     * Create the canvas that will be displaying the time series information.
     */
    private void createDisplayCanvas() {
        displayCanvas = new TimeSeriesDisplayCanvas(mainComp, groupData,
                groupData.getBeginDate(), groupData.getEndDate());
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        displayCanvas.setLayoutData(layoutData);

        /*
         * Add a key listener for up and down arrows to move up and down through
         * the pages of the graph
         */
        displayCanvas.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_UP) {
                    changePage(-1);
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    changePage(+1);
                }
            }
        });

        displayCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                handleMouseDownEvent(e);
            }

            @Override
            public void mouseUp(MouseEvent e) {
                handleMouseUpEvent(e);
            }
        });

        displayCanvas.addMouseMoveListener(new MouseMoveListener() {

            @Override
            public void mouseMove(MouseEvent e) {
                handleMouseMove(e);
            }
        });

        displayCanvas.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                paintOverlays(e.gc);
            }
        });
    }

    private void handleMouseDownEvent(MouseEvent e) {
        GraphData lastActiveGraph = groupData.getActiveGraph();
        GraphData clickedGraph = null;
        for (GraphData graphData : groupData.getCurrentPage()
                .getGraphDataList()) {
            if (graphData.getGraphArea().contains(e.x, e.y)) {
                clickedGraph = graphData;
                if (clickedGraph != lastActiveGraph) {
                    setActiveGraph(clickedGraph);
                }
                break;
            }
        }

        if (clickedGraph == null) {
            getDisplay().beep();
            return;
        }
        if (e.button == 2 || e.button == 3) {
            if (toggleTimeSeriesDlg != null) {
                toggleTimeSeriesDlg.close();
                toggleTimeSeriesDlg = null;
            }
            Point p = new Point(e.x, e.y);
            p = displayCanvas.toDisplay(p);

            toggleTimeSeriesDlg = new ToggleTimeSeriesDlg(getShell(),
                    clickedGraph.getTraces(), p, this);
            toggleTimeSeriesDlg.open();
        } else if (e.button == 1) {
            if (editState == EDIT_STATE.START) {
                int traceNum;
                if (clickedGraph.getNumTraces() == 1) {
                    traceNum = 0;
                } else {
                    traceNum = findEditTrace(e.x, e.y);
                }

                if (traceNum >= 0) {
                    TraceData selectedTrace = clickedGraph
                            .getTraceData(traceNum);
                    groupData.setSelectedTrace(selectedTrace);
                    editState = EDIT_STATE.ACTIVE;
                    selectTraceMI.setSelection(false);
                    editCount = 0;
                    enableEditMenus();
                    displayCanvas.setCursor(null);
                    displayCanvas.redraw();

                    ppDur = convPpDur2Min(groupData.getActiveGraph(),
                            selectedTrace);
                } else {
                    getDisplay().beep();
                }

            } else if (editState == EDIT_STATE.ACTIVE) {
                switch (editMode) {
                case ADD:
                    moveInfo = addCheckEdit(e.x, e.y);
                    if (moveInfo.valid) {
                        if (moveInfo.addLoc == ADD_LOCATION.MIDDLE) {
                            findInsertPoint(e.x, e.y);
                        }

                        moveInfo.x = e.x;
                        moveInfo.y = e.y;
                    }
                    break;

                case MOVE:
                    moveInfo = findMoveIndex(e.x, e.y);
                    if (moveInfo.valid) {
                        moveInfo.y = e.y;
                        displayCanvas.setCursor(getDisplay()
                                .getSystemCursor(SWT.CURSOR_SIZENS));
                    }
                    break;

                case DELETE:
                case SETMISSING:
                    moveInfo = new MoveInfo();
                    moveInfo.valid = true;
                    moveInfo.x1 = moveInfo.x2 = e.x;
                    moveInfo.y1 = moveInfo.y2 = e.y;

                    rubberBand = new Rectangle(e.x, e.y, 0, 0);
                    break;

                case NONE:
                default:
                    break;
                }
            } else {
                mouseDownPoint = new Point(e.x, e.y);
                if (zooming) {
                    rubberBand = new Rectangle(e.x, e.y, 0, 0);
                }
            }
        }
    }

    private void handleMouseUpEvent(MouseEvent e) {

        if (editState == EDIT_STATE.ACTIVE) {
            if (moveInfo != null && moveInfo.valid) {
                displayCanvas.setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_HAND));

                switch (editMode) {
                case ADD:
                    updateAddData();
                    break;

                case MOVE:
                    updateMoveData();
                    break;

                case DELETE:
                case SETMISSING:
                    updateMissingDeletedData(editMode);
                    break;

                case NONE:
                default:
                    displayCanvas.setCursor(null);
                    break;
                }

                selectTraceMI.setEnabled(false);
                editCount++;
                displayCanvas.redraw();
            }
        } else {
            displayCanvas.setCursor(null);
        }
        crossHairPoint = null;
        displayCanvas.redraw();

        if (zooming) {
            doZoom(rubberBand);
            zooming = false;
            rubberBand = null;
        }

        mouseDownPoint = null;
        moveInfo = null;
    }

    private void handleMouseMove(MouseEvent e) {
        if (mouseDownPoint != null) {
            if (zooming) {
                rubberBand = computeRubberBandBox(mouseDownPoint.x,
                        mouseDownPoint.y, e.x, e.y);
                displayCanvas.redraw();
            } else {
                displayCanvas.setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_CROSS));
                GraphData activeGraph = groupData.getActiveGraph();
                if (activeGraph != null) {
                    Rectangle graphArea = activeGraph.getGraphArea();
                    if (graphArea.contains(e.x, e.y)) {
                        crossHairPoint = new Point(e.x, e.y);
                        displayCanvas.redraw();
                    }
                }
            }
        } else {
            switch (editState) {
            case START:
                int traceid = findEditTrace(e.x, e.y);
                if (traceid >= 0) {
                    displayCanvas.setCursor(
                            getDisplay().getSystemCursor(SWT.CURSOR_HAND));
                } else {
                    displayCanvas.setCursor(
                            getDisplay().getSystemCursor(SWT.CURSOR_SIZEALL));
                }
                break;
            case ACTIVE:
                GraphData activeGraph = groupData.getActiveGraph();
                Rectangle graphArea = activeGraph.getGraphArea();
                switch (editMode) {
                case ADD:
                    if (moveInfo == null) {
                        if (addCheckEdit(e.x, e.y).valid) {
                            displayCanvas.setCursor(getDisplay()
                                    .getSystemCursor(SWT.CURSOR_HAND));
                        } else {
                            displayCanvas.setCursor(null);
                        }
                    } else {
                        if (!graphArea.contains(e.x, e.y)) {
                            break;
                        }
                        if (moveInfo.addLoc == ADD_LOCATION.LEFT
                                && e.x >= moveInfo.leftLimit) {
                            break;
                        }
                        if (moveInfo.addLoc == ADD_LOCATION.RIGHT
                                && e.x <= moveInfo.rightLimit) {
                            break;
                        }
                        if (moveInfo.addLoc == ADD_LOCATION.MIDDLE
                                && (e.x < moveInfo.leftLimit
                                        || e.x > moveInfo.rightLimit)) {
                            break;
                        }
                        moveInfo.x = e.x;
                        moveInfo.y = e.y;
                    }
                    break;

                case MOVE:
                    if (moveInfo == null) {
                        if (findMoveIndex(e.x, e.y).valid) {
                            displayCanvas.setCursor(getDisplay()
                                    .getSystemCursor(SWT.CURSOR_SIZENS));
                        } else {
                            displayCanvas.setCursor(getDisplay()
                                    .getSystemCursor(SWT.CURSOR_HAND));
                        }
                    } else if (graphArea.contains(e.x, e.y)) {
                        moveInfo.y = e.y;
                    }
                    break;

                case DELETE:
                case SETMISSING:
                    if (moveInfo != null && moveInfo.valid) {
                        rubberBand = computeRubberBandBox(moveInfo.x1,
                                moveInfo.y1, e.x, e.y);
                        if (graphArea.contains(e.x, e.y)) {
                            moveInfo.x2 = e.x;
                            moveInfo.y2 = e.y;
                        }
                    }
                    break;

                case NONE:
                default:
                    break;
                }

                displayCanvas.redraw();
                break;
            case RESET:
            default:
                break;
            }
        }
    }

    private Rectangle computeRubberBandBox(int x1, int y1, int x2, int y2) {
        int width = x2 - x1;
        int height = y2 - y1;

        if (height > 0 && width > 0) {
            displayCanvas
                    .setCursor(getDisplay().getSystemCursor(SWT.CURSOR_SIZESE));
        } else if (height > 0 && width < 0) {
            displayCanvas
                    .setCursor(getDisplay().getSystemCursor(SWT.CURSOR_SIZESW));
        } else if (height < 0 && width > 0) {
            displayCanvas
                    .setCursor(getDisplay().getSystemCursor(SWT.CURSOR_SIZENE));
        } else if (height < 0 && width < 0) {
            displayCanvas
                    .setCursor(getDisplay().getSystemCursor(SWT.CURSOR_SIZENW));
        } else {
            displayCanvas
                    .setCursor(getDisplay().getSystemCursor(SWT.CURSOR_CROSS));
        }

        int x = Math.min(x1, x2);
        int y = Math.min(y1, y2);
        width = Math.abs(width);
        height = Math.abs(height);
        Rectangle rect = new Rectangle(x, y, width, height);

        Rectangle graphArea = groupData.getActiveGraph().getGraphArea();
        rect = graphArea.intersection(rect);

        return rect;
    }

    private int convPpDur2Min(GraphData graphData, TraceData traceData) {

        if (!HydroConstants.PP.equalsIgnoreCase(traceData.getPe())) {
            return -1;
        }

        int dur = traceData.getDur();
        int iValue = dur / 1000;
        int jValue = dur - iValue * 1000;

        int durInMinute = 0;
        switch (iValue) {
        case 0:
            durInMinute = 0;
            break;

        case 1:
            durInMinute = jValue * TimeUtil.MINUTES_PER_HOUR;
            break;

        case 2:
            durInMinute = jValue * TimeUtil.MINUTES_PER_DAY;
            break;

        case 3:
            durInMinute = jValue * TimeUtil.MINUTES_PER_DAY * 30;
            break;

        case 4:
            durInMinute = jValue * TimeUtil.MINUTES_PER_DAY * 30 * 12;
            break;

        default:
            durInMinute = 0;
            break;
        }

        return durInMinute;
    }

    private void updateAddData() {
        GraphData activeGraph = groupData.getActiveGraph();
        TraceData traceData = groupData.getSelectedTrace();

        long xMillis = displayCanvas.pixel2x(activeGraph, moveInfo.x).getTime();
        double yValue = displayCanvas.pixel2y(activeGraph, moveInfo.y);

        /* Check for duplicate points in three cases: */
        /* First, Middle, and End point */
        long snapValue = 5 * TimeUtil.MILLIS_PER_MINUTE;

        long ppDurMillis = 0;
        if (ppDur > 0) {
            ppDurMillis = ppDur * TimeUtil.MILLIS_PER_MINUTE;
        } else {
            /* 250 seconds */
            xMillis += 250 * TimeUtil.MILLIS_PER_SECOND;
        }

        if (moveInfo.addLoc == ADD_LOCATION.LEFT) {
            /* PP trace is treated differently */
            TimeSeriesPoint point = traceData.getTsData().get(0);
            if (ppDur > 0) {
                long nGaps = (xMillis - point.getX().getTime()) / ppDurMillis;
                xMillis = point.getX().getTime() - nGaps * ppDurMillis;
            } else {
                xMillis = xMillis / snapValue * snapValue;
            }

            if (xMillis >= point.getX().getTime()) {
                return;
            }

            TimeSeriesPoint newPoint = new TimeSeriesPoint();
            newPoint.setX(new Date(xMillis));
            newPoint.setY(yValue);
            newPoint.setMode(MODE.ADD);
            traceData.getTsData().add(0, newPoint);

        } else if (moveInfo.addLoc == ADD_LOCATION.RIGHT) {
            /* PP trace is treated differently */
            TimeSeriesPoint point = traceData.getTsData()
                    .get(traceData.getNpts() - 1);
            if (ppDur > 0) {
                long nGaps = (xMillis - point.getX().getTime()) / ppDurMillis;
                xMillis = point.getX().getTime() + nGaps * ppDurMillis;
            } else {
                xMillis = xMillis / snapValue * snapValue;
            }

            if (xMillis <= point.getX().getTime()) {
                return;
            }

            TimeSeriesPoint newPoint = new TimeSeriesPoint();
            newPoint.setX(new Date(xMillis));
            newPoint.setY(yValue);
            newPoint.setMode(MODE.ADD);
            traceData.getTsData().add(newPoint);

        } else if (moveInfo.addLoc == ADD_LOCATION.MIDDLE) {
            xMillis = xMillis / snapValue * snapValue;

            int leftIndex = moveInfo.leftIndex;
            int rightIndex = moveInfo.rightIndex;

            TimeSeriesPoint leftPoint = traceData.getTsData().get(leftIndex);
            TimeSeriesPoint rightPoint = traceData.getTsData().get(rightIndex);

            if (xMillis <= leftPoint.getX().getTime()
                    || xMillis >= rightPoint.getX().getTime()) {
                return;
            }

            /* PP trace is treated differently */
            if (ppDur > 0) {

                long ngaps = (rightPoint.getX().getTime()
                        - leftPoint.getX().getTime()) / ppDurMillis;
                if (ngaps == 1) {
                    return;
                }
                if (ngaps == 2) {
                    xMillis = leftPoint.getX().getTime() + ppDurMillis;
                } else {
                    long nduration = (xMillis - leftPoint.getX().getTime())
                            / ppDurMillis;
                    xMillis = leftPoint.getX().getTime()
                            + nduration * ppDurMillis;
                }

                if (xMillis > rightPoint.getX().getTime()) {
                    return;
                }

            } else {
                xMillis = xMillis / snapValue * snapValue;
            }

            TimeSeriesPoint newPoint = new TimeSeriesPoint();
            newPoint.setX(new Date(xMillis));
            newPoint.setY(yValue);
            newPoint.setMode(MODE.ADD);
            traceData.getTsData().add(rightIndex, newPoint);
        }
    }

    private void updateMoveData() {
        GraphData activeGraph = groupData.getActiveGraph();
        TraceData selectedTrace = groupData.getSelectedTrace();

        int traceIndex = moveInfo.traceIndex;
        double yValue = displayCanvas.pixel2y(activeGraph, moveInfo.y);

        TimeSeriesPoint point = selectedTrace.getTsData().get(traceIndex);
        point.setY(yValue);
        point.setMode(MODE.MOVE);
    }

    private void updateMissingDeletedData(MODE mode) {
        if (rubberBand == null) {
            return;
        }

        Rectangle rect = new Rectangle(rubberBand.x - 1, rubberBand.y - 1,
                rubberBand.width + 2, rubberBand.height + 2);

        GraphData activeGraph = groupData.getActiveGraph();
        TraceData selectedTrace = groupData.getSelectedTrace();

        for (TimeSeriesPoint point : selectedTrace.getTsData()) {
            int xPix = displayCanvas.x2pixel(activeGraph, point.getX());
            int yPix = displayCanvas.y2pixel(activeGraph, point.getY());

            if (rect.contains(xPix, yPix)) {
                point.setMode(mode);
            }
        }
    }

    private MoveInfo addCheckEdit(int x, int y) {
        GraphData activeGraph = groupData.getActiveGraph();
        TraceData traceData = groupData.getSelectedTrace();

        int nPts = traceData.getNpts();

        MoveInfo moveInfo = new MoveInfo();
        moveInfo.valid = true;
        moveInfo.leftLimit = displayCanvas.x2pixel(activeGraph,
                traceData.getTsData().get(0).getX());
        moveInfo.rightLimit = displayCanvas.x2pixel(activeGraph,
                traceData.getTsData().get(nPts - 1).getX());

        if (x > moveInfo.leftLimit && x < moveInfo.rightLimit) {
            moveInfo.addLoc = ADD_LOCATION.MIDDLE;

        } else if (x < moveInfo.leftLimit) {
            moveInfo.addLoc = ADD_LOCATION.LEFT;
            moveInfo.x1 = moveInfo.leftLimit;
            moveInfo.y1 = displayCanvas.y2pixel(activeGraph,
                    traceData.getTsData().get(0).getY());

        } else if (x > moveInfo.rightLimit) {
            moveInfo.addLoc = ADD_LOCATION.RIGHT;
            moveInfo.x1 = moveInfo.rightLimit;
            moveInfo.y1 = displayCanvas.y2pixel(activeGraph,
                    traceData.getTsData().get(nPts - 1).getY());

        } else {
            moveInfo.addLoc = ADD_LOCATION.NONE;
            moveInfo.valid = false;
        }

        return moveInfo;
    }

    private void findInsertPoint(int x, int y) {
        GraphData activeGraph = groupData.getActiveGraph();
        TraceData traceData = groupData.getSelectedTrace();

        Date xMin = activeGraph.getXmin();
        Date xMax = activeGraph.getXmax();
        double yMin = activeGraph.getYmin();
        double yMax = activeGraph.getYmax();

        int nPts = traceData.getNpts();

        moveInfo.valid = false;

        for (int n = 0; n < nPts; n++) {
            TimeSeriesPoint point = traceData.getTsData().get(n);
            if (point.getMode() == MODE.DELETE
                    || point.getMode() == MODE.SETMISSING) {
                continue;
            }

            Date xValue = point.getX();
            double yValue = point.getY();

            if (xValue.before(xMin) || xValue.after(xMax) || yValue < yMin
                    || yValue > yMax) {
                continue;
            }

            int xPix = displayCanvas.x2pixel(activeGraph, xValue);

            /* Find first valid right point */
            if (xPix > x) {
                moveInfo.traceIndex = n;
                int rightIndex = n;
                int leftIndex = findLeftIndex(traceData, n);

                moveInfo.x1 = displayCanvas.x2pixel(activeGraph,
                        traceData.getTsData().get(leftIndex).getX());
                moveInfo.y1 = displayCanvas.y2pixel(activeGraph,
                        traceData.getTsData().get(leftIndex).getY());

                moveInfo.x2 = displayCanvas.x2pixel(activeGraph,
                        traceData.getTsData().get(rightIndex).getX());
                moveInfo.y2 = displayCanvas.y2pixel(activeGraph,
                        traceData.getTsData().get(rightIndex).getY());

                moveInfo.leftLimit = moveInfo.x1;
                moveInfo.rightLimit = moveInfo.x2;
                moveInfo.leftIndex = leftIndex;
                moveInfo.rightIndex = rightIndex;

                moveInfo.valid = true;
                break;
            }
        }
    }

    private MoveInfo findMoveIndex(int x, int y) {
        GraphData activeGraph = groupData.getActiveGraph();
        TraceData traceData = groupData.getSelectedTrace();

        Date xMin = activeGraph.getXmin();
        Date xMax = activeGraph.getXmax();
        double yMin = activeGraph.getYmin();
        double yMax = activeGraph.getYmax();

        Rectangle rect = new Rectangle(x - 10, y - 10, 20, 20);
        MoveInfo moveInfo = new MoveInfo();
        moveInfo.valid = false;

        int nPts = traceData.getNpts();
        for (int n = 0; n < nPts; n++) {
            TimeSeriesPoint point = traceData.getTsData().get(n);

            if (point.getMode() == MODE.DELETE
                    || point.getMode() == MODE.SETMISSING) {
                continue;
            }

            Date xValue = point.getX();
            double yValue = point.getY();

            if (xValue.before(xMin) || xValue.after(xMax) || yValue < yMin
                    || yValue > yMax) {
                continue;
            }

            int xPix = displayCanvas.x2pixel(activeGraph, xValue);
            int yPix = displayCanvas.y2pixel(activeGraph, yValue);

            if (rect.contains(xPix, yPix)) {
                moveInfo.traceIndex = n;
                moveInfo.x = xPix;

                if (n == 0) {
                    int rightIndex = findRightIndex(traceData, n);
                    moveInfo.x2 = displayCanvas.x2pixel(activeGraph,
                            traceData.getTsData().get(rightIndex).getX());
                    moveInfo.y2 = displayCanvas.y2pixel(activeGraph,
                            traceData.getTsData().get(rightIndex).getY());
                    moveInfo.numLegs = 1;

                    moveInfo.valid = true;

                } else if (n == nPts - 1) {
                    int leftIndex = findLeftIndex(traceData, n);
                    moveInfo.x2 = displayCanvas.x2pixel(activeGraph,
                            traceData.getTsData().get(leftIndex).getX());
                    moveInfo.y2 = displayCanvas.y2pixel(activeGraph,
                            traceData.getTsData().get(leftIndex).getY());
                    moveInfo.numLegs = 1;

                    moveInfo.valid = true;

                } else {
                    int rightIndex = findRightIndex(traceData, n);
                    int leftIndex = findLeftIndex(traceData, n);

                    moveInfo.x1 = displayCanvas.x2pixel(activeGraph,
                            traceData.getTsData().get(leftIndex).getX());
                    moveInfo.y1 = displayCanvas.y2pixel(activeGraph,
                            traceData.getTsData().get(leftIndex).getY());
                    moveInfo.x2 = displayCanvas.x2pixel(activeGraph,
                            traceData.getTsData().get(rightIndex).getX());
                    moveInfo.y2 = displayCanvas.y2pixel(activeGraph,
                            traceData.getTsData().get(rightIndex).getY());
                    moveInfo.numLegs = 2;

                    /* Check if one leg is off graph then draw only one leg */
                    Rectangle graphArea = activeGraph.getGraphArea();
                    if (moveInfo.x1 <= graphArea.x - 5
                            || moveInfo.y1 <= graphArea.y - 5
                            || moveInfo.y1 >= graphArea.y + graphArea.height
                                    + 5) {
                        moveInfo.numLegs = 1;
                    }

                    if (moveInfo.x2 >= graphArea.x + graphArea.width + 5
                            || moveInfo.y2 <= graphArea.y - 5
                            || moveInfo.y2 >= graphArea.y + graphArea.height
                                    + 5) {
                        moveInfo.x2 = moveInfo.x1;
                        moveInfo.y2 = moveInfo.y1;
                        moveInfo.numLegs = 1;
                    }

                    moveInfo.valid = true;
                }

                break;
            }
        }

        return moveInfo;
    }

    private int findRightIndex(TraceData traceData, int n) {
        int m;
        for (m = n + 1; m < traceData.getNpts(); m++) {
            TimeSeriesPoint point = traceData.getTsData().get(m);
            if (point.getMode() != MODE.DELETE
                    && point.getMode() != MODE.SETMISSING) {
                break;
            }
        }
        return m;
    }

    private int findLeftIndex(TraceData traceData, int n) {
        int m;
        for (m = n - 1; m > 0; m--) {
            TimeSeriesPoint point = traceData.getTsData().get(m);
            if (point.getMode() != MODE.DELETE
                    && point.getMode() != MODE.SETMISSING) {
                break;
            }
        }
        return m;
    }

    /**
     *
     * @param x
     *            : location x (of mouse pointer)
     * @param y
     *            : location y (of mouse pointer)
     * @return the nearest trace. -999 if x,y is too far away
     */
    private int findEditTrace(int x, int y) {
        int closestTrace = -1;
        double closestDist = 20;
        for (GraphData graphData : groupData.getCurrentPage()
                .getGraphDataList()) {
            List<TraceData> traceList = graphData.getTraces();

            Date xmax = graphData.getXmax();
            Date xmin = graphData.getXmin();
            double ymax = graphData.getYmax();
            double ymin = graphData.getYmin();

            Coordinate cursorPt = new Coordinate(x, y);
            LineSegment seg = new LineSegment();
            for (int n = 0; n < traceList.size(); n++) {
                TraceData traceData = traceList.get(n);
                if (traceData.isTraceOn()) {
                    boolean firstPoint = true;
                    for (TimeSeriesPoint point : traceData.getTsData()) {
                        if (point.getMode() == MODE.DELETE
                                || point.getMode() == MODE.SETMISSING) {
                            continue;
                        }

                        Date xval = point.getX();
                        double yval = point.getY();
                        if (xval.before(xmin) || xval.after(xmax) || yval < ymin
                                || yval > ymax) {
                            continue;
                        }

                        int xpix = displayCanvas.x2pixel(graphData, xval);
                        int ypix = displayCanvas.y2pixel(graphData, yval);

                        if (!firstPoint) {

                            seg.p1.x = xpix;
                            seg.p1.y = ypix;

                            double dist = seg.distance(cursorPt);
                            if (dist < closestDist) {
                                closestDist = dist;
                                closestTrace = n;
                            }
                        }

                        firstPoint = false;
                        seg.p0.x = xpix;
                        seg.p0.y = ypix;
                    }
                }
            }
        }

        return closestTrace;
    }

    private void paintOverlays(GC gc) {
        if (crossHairPoint != null) {
            drawCrossHairs(gc, crossHairPoint.x, crossHairPoint.y);
            displayPointString(gc, crossHairPoint.x, crossHairPoint.y);
        }

        if (zooming) {
            drawBox(gc, rubberBand);
        }

        if (editState == EDIT_STATE.ACTIVE) {
            showActiveTraceLabel(gc);

            if (moveInfo != null && moveInfo.valid) {
                switch (editMode) {
                case ADD:
                    drawAdd(gc);
                    displayPointString(gc, moveInfo.x, moveInfo.y);
                    break;

                case MOVE:
                    drawMove(gc);
                    displayPointString(gc, moveInfo.x, moveInfo.y);
                    break;

                case DELETE:
                case SETMISSING:
                    drawBox(gc, rubberBand);
                    break;

                case NONE:
                default:
                    break;
                }
            }
        }
    }

    private void showActiveTraceLabel(GC gc) {
        TraceData selectedTrace = groupData.getSelectedTrace();
        if (selectedTrace != null) {
            String s = String.format(" ACTIVE TRACE: %s ",
                    selectedTrace.getPEDTSE());

            Color color = new Color(getDisplay(),
                    HydroUtils.getColor(EDIT_COLOR));
            gc.setForeground(color);

            GraphData activeGraph = groupData.getActiveGraph();
            Rectangle graphArea = activeGraph.getGraphArea();
            gc.drawString(s, graphArea.x + graphArea.width / 2,
                    graphArea.y - gc.textExtent(s).y * 3);
            color.dispose();
        }
    }

    /**
     * Draw the cross hairs at the specified point
     *
     * @param gc
     * @param x
     *            crossHair x coordinate
     * @param y
     *            crossHair y coordinate
     */
    protected void drawCrossHairs(GC gc, int x, int y) {
        GraphData activeGraph = groupData.getActiveGraph();
        if (activeGraph != null) {
            Rectangle graphArea = activeGraph.getGraphArea();
            gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
            gc.setLineWidth(1);
            gc.drawLine(graphArea.x, y, graphArea.x + graphArea.width, y);
            gc.drawLine(x, graphArea.y, x, graphArea.y + graphArea.height);
        }
    }

    /**
     * Build the point string that is displayed when clicking on the graph
     *
     * @param x
     *            crossHair x coordinate
     * @param y
     *            crossHair y coordinate
     *
     */
    private void displayPointString(GC gc, int x, int y) {
        GraphData activeGraph = groupData.getActiveGraph();

        StringBuilder sb = new StringBuilder();

        Date xValue = displayCanvas.pixel2x(activeGraph, x);
        SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy HH:mm'Z'");
        format.setTimeZone(TimeZone.getTimeZone("GMT"));
        sb.append(format.format(xValue));

        double yValue = displayCanvas.pixel2y(activeGraph, y);

        String units = FEET;
        boolean isRiverData = true;
        boolean isStage = true;
        List<TraceData> traces = activeGraph.getTraces();
        for (TraceData trace : traces) {
            if (!trace.getPe().toUpperCase().startsWith("H")
                    && !trace.getPe().toUpperCase().startsWith("Q")) {
                isRiverData = false;
            }
            if (trace.getPe().toUpperCase().startsWith("Q")) {
                isStage = false;
            }
        }

        NumberFormat twoDecimalFormat = new DecimalFormat("0.00");
        String lid = activeGraph.getTraces().get(0).getLid();
        if (isRiverData) {
            if (isStage) {
                /*
                 * Convert the stage to discharge for the location and stage
                 * value passed in.
                 */
                double q = StageDischargeUtils.stage2discharge(lid, yValue);
                // check for rating curve
                if (q != HydroConstants.RATING_CONVERT_FAILED) {
                    if (q > 10_000) {
                        units = KCFS;
                        q = q / 1000;
                    } else {
                        units = CFS;
                    }
                    sb.append(" value=" + twoDecimalFormat.format(yValue) + " "
                            + FEET + " ");
                    sb.append(String.format("%8.1f", q) + " " + units);
                } else {
                    sb.append(" value=" + twoDecimalFormat.format(yValue));
                }

            } else {
                /*
                 * Convert the discharge to stage for the location and discharge
                 * value passed in.
                 */
                double q = StageDischargeUtils.discharge2stage(lid, yValue);
                // check for rating curve
                if (q != HydroConstants.RATING_CONVERT_FAILED) {
                    sb.append(" value=" + twoDecimalFormat.format(yValue) + " "
                            + CFS + " ");
                    sb.append(String.format("%8.1f", q) + " " + FEET);
                } else {
                    sb.append(" value=" + twoDecimalFormat.format(yValue));
                }
            }

        } else {
            sb.append("  value=" + twoDecimalFormat.format(yValue));
        }

        Rectangle graphArea = activeGraph.getGraphArea();
        String pointString = sb.toString();
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_CYAN));
        FontMetrics fm = gc.getFontMetrics();
        int fontHeight = fm.getHeight();
        int fontWidth = gc.textExtent("0").x;
        gc.drawText(pointString, graphArea.x - fontWidth * 7,
                graphArea.y - fontHeight * 3);
    }

    private void drawAdd(GC gc) {
        int lineWidth = AppsDefaults.getInstance()
                .getInt(HydroConstants.TS_LINEWIDTH, 1);
        int diameter = lineWidth + 5;
        int offset = diameter / 2;

        Color color = new Color(getDisplay(), HydroUtils.getColor(EDIT_COLOR));
        gc.setForeground(color);

        gc.drawLine(moveInfo.x1, moveInfo.y1, moveInfo.x, moveInfo.y);
        if (moveInfo.addLoc == ADD_LOCATION.MIDDLE) {
            gc.drawLine(moveInfo.x, moveInfo.y, moveInfo.x2, moveInfo.y2);
        }

        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.fillOval(moveInfo.x - offset, moveInfo.y - offset, diameter,
                diameter);

        gc.drawOval(moveInfo.x - offset, moveInfo.y - offset, diameter,
                diameter);

        gc.setBackground(displayCanvas.getBackground());

        color.dispose();
    }

    private void drawMove(GC gc) {
        int lineWidth = AppsDefaults.getInstance()
                .getInt(HydroConstants.TS_LINEWIDTH, 1);
        int diameter = lineWidth + 5;
        int offset = diameter / 2;

        Color color = new Color(getDisplay(), HydroUtils.getColor(EDIT_COLOR));
        gc.setForeground(color);

        gc.drawLine(moveInfo.x2, moveInfo.y2, moveInfo.x, moveInfo.y);
        if (moveInfo.numLegs == 2) {
            gc.drawLine(moveInfo.x, moveInfo.y, moveInfo.x1, moveInfo.y1);
        }

        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.fillOval(moveInfo.x - offset, moveInfo.y - offset, diameter,
                diameter);

        gc.drawOval(moveInfo.x - offset, moveInfo.y - offset, diameter,
                diameter);

        gc.setBackground(displayCanvas.getBackground());

        color.dispose();
    }

    private void drawBox(GC gc, Rectangle rect) {
        if (rect != null) {
            gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_RED));
            gc.drawRectangle(rect);
        }
    }

    private void doZoom(Rectangle rect) {
        if (rect != null && rect.width >= 10 && rect.height >= 10) {
            GraphData activeGraph = groupData.getActiveGraph();
            if (activeGraph != null) {
                Date xMin = displayCanvas.pixel2x(activeGraph, rect.x);
                Date xMax = displayCanvas.pixel2x(activeGraph,
                        rect.x + rect.width);
                double yMax = displayCanvas.pixel2y(activeGraph, rect.y);
                double yMin = displayCanvas.pixel2y(activeGraph,
                        rect.y + rect.height);

                activeGraph.setXmin(xMin);
                activeGraph.setXmax(xMax);
                activeGraph.setYmin(yMin);
                activeGraph.setYmax(yMax);

                activeGraph.setOldXmin(xMin);
                activeGraph.setOldXmax(xMax);
                activeGraph.setOldYmin(yMin);
                activeGraph.setOldYmax(yMax);

                activeGraph.setZoomed(true);

                double yDiff = yMax - yMin;
                if (yDiff >= 1.0) {
                    activeGraph.adjustYmaxYmin(yMin, yMax);
                } else {
                    activeGraph.setDataInc(yDiff / 5.0);
                }
                displayCanvas.redraw();
            }
        }
    }

    private void unZoom() {
        GraphData activeGraph = groupData.getActiveGraph();
        if (activeGraph != null) {
            activeGraph.setXmin(activeGraph.getOrigXmin());
            activeGraph.setXmax(activeGraph.getOrigXmax());
            activeGraph.setYmin(activeGraph.getOrigYmin());
            activeGraph.setYmax(activeGraph.getOrigYmax());

            activeGraph.setOldXmin(activeGraph.getOrigXmin());
            activeGraph.setOldXmax(activeGraph.getOrigXmax());
            activeGraph.setOldYmin(activeGraph.getOrigYmin());
            activeGraph.setOldYmax(activeGraph.getOrigYmax());

            activeGraph.setZoomed(false);

            displayCanvas.redraw();
        }
    }

    @Override
    public void traceSelected() {
        showLatestFcstMI.setSelection(false);
        groupData.getActiveGraph().findMinMax(false, groupData.getBeginDate(),
                groupData.getEndDate());
        displayCanvas.redraw();
    }

    private void setActiveGraph(GraphData activeGraph) {
        showLatestFcstMI.setEnabled(true);
        showLatestFcstMI.setSelection(activeGraph.isLatestfcstonly());

        scaleStagesMI.setEnabled(true);
        if (activeGraph.isYscaleToData() && !activeGraph.isShowcat()) {
            dataOnlyMI.setSelection(true);
            dataOnlyShowCatMI.setSelection(false);
            dataAndCategoriesMI.setSelection(false);
        } else if (activeGraph.isYscaleToData() && activeGraph.isShowcat()) {
            dataOnlyMI.setSelection(false);
            dataOnlyShowCatMI.setSelection(true);
            dataAndCategoriesMI.setSelection(false);
        } else {
            dataOnlyMI.setSelection(false);
            dataOnlyShowCatMI.setSelection(false);
            dataAndCategoriesMI.setSelection(true);
        }

        showPcSubMI.setEnabled(true);
        for (MenuItem item : showPcSubMI.getMenu().getItems()) {
            item.setSelection(item.getData() == activeGraph.getDerivepp());
        }

        groupData.setActiveGraph(activeGraph);
        displayCanvas.redraw();

    }

    /**
     * Cancel the edits
     */
    private void saveCancelEdits(boolean isSave) {
        boolean doIt = true;
        if (editCount > 0) {
            String title;
            String message;
            if (isSave) {
                title = "Save Edits";
                message = "Are you sure you want to save the changes?";
            } else {
                title = "Cancel Changes?";
                message = "Are you sure you want to cancel your changes?";
            }

            doIt = MessageDialog.openConfirm(shell, title, message);
        }

        if (doIt) {
            GraphData ag = groupData.getActiveGraph();
            double yMin = ag.getYmin();
            double yMax = ag.getYmax();
            Date xMin = ag.getXmin();
            Date xMax = ag.getXmax();
            double dataInc = ag.getDataInc();

            double oldYMin = ag.getOldYmin();
            double oldYMax = ag.getOldYmax();
            Date oldXMin = ag.getOldXmin();
            Date oldXMax = ag.getOldXmax();
            double oldDataInc = ag.getOldDataInc();

            if (isSave) {
                updateDatabase(groupData.getSelectedTrace());
            }

            editCount = 0;
            editState = EDIT_STATE.RESET;
            groupData.setSelectedTrace(null);

            groupData.getCurrentPage().loadData(groupData.getBeginDate(),
                    groupData.getEndDate());

            ag.setYmin(yMin);
            ag.setYmax(yMax);
            ag.setXmin(xMin);
            ag.setXmax(xMax);
            ag.setDataInc(dataInc);

            ag.setOldYmin(oldYMin);
            ag.setOldYmax(oldYMax);
            ag.setOldXmin(oldXMin);
            ag.setOldXmax(oldXMax);
            ag.setOldDataInc(oldDataInc);

            displayCanvas.redraw();
            resetMenuItemsAndLists();
        }
    }

    private void updateDatabase(TraceData traceData) {
        List<ForecastData> editList = new ArrayList<>();
        List<ForecastData> deleteList = new ArrayList<>();
        List<ForecastData> insertList = new ArrayList<>();

        for (TimeSeriesPoint point : traceData.getTsData()) {
            switch (point.getMode()) {
            case ADD:
                insertList.add(createPoint(traceData, point));
                break;

            case MOVE:
            case SETMISSING:
                editList.add(createPoint(traceData, point));
                break;

            case DELETE:
                deleteList.add(createPoint(traceData, point));
                break;

            case NONE:
            default:
                break;
            }
        }

        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        Map<RejecteddataId, Integer> rejectedSecondsMap = new HashMap<>(
                deleteList.size(), 1.0f);
        /*
         * The insertion start time is remembered prior to any inserts into the
         * rejected data table. This will ensure that insertion times will
         * remain unique across both potential inserts into the rejected data
         * table below.
         */
        Date insertStartTime = new Date();
        if (!deleteList.isEmpty()) {
            try {
                dataManager.insertDelRejectedData(deleteList,
                        rejectedSecondsMap, insertStartTime);
                dataManager.delete(deleteList);
                updateMaxFcst(deleteList);
            } catch (VizException e) {
                statusHandler.error("Error deleting points: ", e);
            }
        }

        if (!insertList.isEmpty()) {
            try {
                dataManager.insert(insertList);
                updateMaxFcst(insertList);
            } catch (VizException e) {
                statusHandler.error("Insert Error: ", e);
            }
        }

        if (!editList.isEmpty()) {
            try {
                dataManager.insertSetMRejectedData(editList, rejectedSecondsMap,
                        insertStartTime);
                dataManager.edit(editList);
                updateMaxFcst(editList);
            } catch (VizException e) {
                statusHandler.error("Error editing points: ", e);
            }
        }
    }

    private ForecastData createPoint(TraceData td, TimeSeriesPoint point) {
        ForecastData data = new ForecastData();
        data.setDur(td.getDur());
        data.setExtremum(td.getExtremum());
        data.setLid(td.getLid());

        if (td.getTs().toUpperCase().startsWith("F")
                || td.getTs().toUpperCase().startsWith("C")) {
            data.setValidTime(point.getX());
            data.setBasisTime(td.getBasistime());
            data.setPreviousValue(point.getY());

            data.setProductTime(td.getProductTime());
            data.setProductID(td.getProductId());
        } else {
            data.setValidTime(point.getX());
            data.setBasisTime(td.getBasistime());
            data.setPreviousValue(point.getY());

            data.setObsTime(point.getX());
            data.setValidTime(td.getProductTime());
            data.setProductTime(td.getProductTime());
            data.setProductID(td.getProductId());
        }
        data.setPe(td.getPe());
        data.setTs(td.getTs());
        if (point.getMode() == MODE.SETMISSING) {
            data.setValue((double) HydroConstants.MISSING_VALUE);
        } else {
            data.setValue(TimeSeriesUtil.round(point.getY(), 2));
        }
        data.setPostingTime(new Date());

        return data;
    }

    /**
     * Enable the edit menus
     */
    public void enableEditMenus() {
        insertMI.setEnabled(true);
        deleteMI.setEnabled(true);
        moveMI.setEnabled(true);
        setMissingMI.setEnabled(true);
        saveToDatabaseMI.setEnabled(true);
        cancelChangesMI.setEnabled(true);
    }

    /**
     * Save the graph as an image
     */
    private void saveGraph() {
        FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        dialog.setText("Save Time Series graph as:");
        dialog.setFilterNames(new String[] { "PNG (*.png)",
                "JPEG (*.jpg;*.jpeg)", "Windows BMP (*.bmp)" });
        dialog.setFilterExtensions(
                new String[] { "*.png", "*.jpg;*.jpeg", "*.bmp" });
        String filename = dialog.open();
        if (filename == null) {
            return;
        }
        saveCanvas(filename);
    }

    /**
     * Captures the window contents and saves the result into a file in a format
     * determined by the filename extension.
     *
     * @param filePath
     *            The path of the image to be saved
     */
    private void saveCanvas(String filePath) {
        /* Split directory, file name, and extension */
        Path path = Paths.get(filePath);
        String directory = path.getParent().toString();
        String fileName = path.getFileName().toString();
        String extension = null;
        int p = fileName.lastIndexOf('.');
        if (p >= 0 && p < fileName.length()) {
            extension = fileName.substring(p);
            fileName = fileName.substring(0, p);
        }

        /* Determine image format */
        int format;
        if (extension == null || ".png".equals(extension)) {
            format = SWT.IMAGE_PNG;
            extension = ".png";
        } else if (".jpg".equals(extension) || ".jpeg".equals(extension)) {
            format = SWT.IMAGE_JPEG;
        } else if (".bmp".equals(extension)) {
            format = SWT.IMAGE_BMP;
        } else {
            statusHandler.error("Invalid file extension: \"" + extension
                    + "\". Must be one of (.png, .jpg, .jpeg, .bmp)");
            return;
        }

        Image image = displayCanvas.getImage();

        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { image.getImageData() };
        loader.save(FileUtil.join(directory, fileName + extension), format);

        image.dispose();
    }

    /**
     * Print the graph
     *
     * @param inverseVideo
     *            if true print in inverse video
     */
    private void print(boolean inverseVideo) {
        PrintDialog dialog = new PrintDialog(shell);
        dialog.setText("Print Time Series");
        PrinterData printerData = dialog.open();

        if (printerData != null) {
            /* Create the printer object */
            Printer printer = new Printer(printerData);
            printer.startJob("Time Series Display");

            /* Get the display image and convert it to a printer image */
            Image image = displayCanvas.getImage();
            ImageData imageData = image.getImageData();
            if (inverseVideo) {
                invert(imageData);
            }
            image = new Image(printer, imageData);

            /* Define a print area with a margin around the edge */
            Rectangle printerBounds = printer.getClientArea();
            Point dpi = printer.getDPI();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);

            float leftMargin = PRINT_MARGIN * dpi.x + trim.x;
            float topMargin = PRINT_MARGIN * dpi.y + trim.y;
            float rightMargin = printerBounds.width + trim.x + trim.width
                    - PRINT_MARGIN * dpi.x;
            float bottomMargin = printerBounds.height + trim.y + trim.height
                    - PRINT_MARGIN * dpi.y;

            /*
             * Determine the scale factor to scale the display image to the
             * print area using the more constrained dimension
             */
            Rectangle imageBounds = image.getBounds();
            float scaleX = (rightMargin - leftMargin) / imageBounds.width;
            float scaleY = (bottomMargin - topMargin) / imageBounds.height;
            float scale = Math.min(scaleX, scaleY);

            /*
             * Create a transform to translate the image to the upper left
             * corner of the print area and scale it to fit
             */
            Transform transform = new Transform(printer);
            transform.translate(leftMargin, topMargin);
            transform.scale(scale, scale);

            GC gc = new GC(printer);
            gc.setTransform(transform);

            /* Draw the image on the page */
            if (printer.startPage()) {
                gc.drawImage(image, 0, 0);
                printer.endPage();
            }
            printer.endJob();
            printer.dispose();
            image.dispose();
            gc.dispose();
        }
    }

    private void invert(ImageData imageData) {
        PaletteData pallete = imageData.palette;
        if (pallete.isDirect) {
            // invert each pixel
            int mask = pallete.redMask | pallete.greenMask | pallete.blueMask;
            for (int x = 0; x < imageData.width; x++) {
                for (int y = 0; y < imageData.height; y++) {
                    imageData.setPixel(x, y, imageData.getPixel(x, y) ^ mask);
                }
            }
        } else {
            // invert each RGB value in the pallete
            for (RGB rgb : pallete.getRGBs()) {
                rgb.red ^= 0xFF;
                rgb.green ^= 0xFF;
                rgb.blue ^= 0xFF;
            }
        }
    }

    /**
     * change the current page;
     *
     * @param direction
     *            +1 to page down or -1 to page up
     */
    protected void changePage(int direction) {
        int numPages = groupInfo.getNumPages();
        if (numPages > 1) {
            int currentPageNum = groupData.getCurrentPageNum();
            currentPageNum += direction;

            // if only Java had a proper modulus function/operator...
            if (currentPageNum < 0) {
                currentPageNum += numPages;
            } else if (currentPageNum >= numPages) {
                currentPageNum -= numPages;
            }

            groupData.setCurrentPageNum(currentPageNum);
            groupData.getCurrentPage().loadData(groupData.getBeginDate(),
                    groupData.getEndDate());
            displayCanvas.redraw();
        }
    }
}
