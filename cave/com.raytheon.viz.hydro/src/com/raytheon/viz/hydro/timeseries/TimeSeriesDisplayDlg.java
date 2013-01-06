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

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.util.GraphData;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo;
import com.raytheon.viz.hydro.timeseries.util.PageInfo;
import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.hydro.util.LoadMaxFcst;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.ForecastData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Time Series Display dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 11 NOV 2010  5831       lbousaid    Fixed Graphical PgDown & PgUp
 * 19 Jan 2011  5281       lbousaidi   make the graph window larger
 * 		data was too compressed.
 * 24 Jan 2011  7799       bkowal      ensured that "Data Only" would be
 *                                     the default selection under both
 *                                     the Graph - Scale Stages sub-menu
 *                                     and the Options - Batch Scale Stages
 *                                     sub-menu.
 * 26 Jan 2011  5557       bkowal      finished the implementation of and
 *                                     enabled "reverse video" printing.
 *                                     Updated the printing capability
 *                                     so that it would place the
 *                                     entire time series within the
 *                                     printable area of the page.
 * 04 Mar 2011 7644      lbousaid      fixed Zoom in feature       
 * 30 May 2012 14967     wkwock        fix insert deleted data to rejecteddata table    
 * 23 Jul 2012 15195     mpduff        Fix dates for displaying groups
 * 06 Dec 2012 15066     wkwock        Fix "ctrl+r" not work in group mode                   
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TimeSeriesDisplayDlg extends CaveSWTDialog {

    /**
     * Time Series Control menu item.
     */
    private MenuItem timeSeriesControlMI;

    /**
     * Save menu item.
     */
    private MenuItem saveMI;

    /**
     * Print sub menu item.
     */
    private MenuItem printSubMI;

    /**
     * Reverse video menu item.
     */
    private MenuItem reverseVideoMI;

    /**
     * Normal menu item.
     */
    private MenuItem normalMI;

    /**
     * Quit menu item.
     */
    private MenuItem quitMI;

    /**
     * Zoom menu item.
     */
    private MenuItem zoomSubMI;

    /**
     * Set menu item.
     */
    private MenuItem setMI;

    /**
     * Reset menu item.
     */
    private MenuItem resetMI;

    /**
     * Show PC sub menu item.
     */
    private MenuItem showPcSubMI;

    /**
     * Off menu item.
     */
    private MenuItem offMI;

    /**
     * Interpolate menu item.
     */
    private MenuItem interpolateMI;

    /**
     * Assign menu item.
     */
    private MenuItem assignMI;

    /**
     * Show latest forecast menu item.
     */
    private MenuItem showLatestFcstMI;

    /**
     * Scale Stages menu item.
     */
    private MenuItem scaleStagesMI;

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
     * Plot menu item.
     */
    private MenuItem plotMI;

    /**
     * Points menu item.
     */
    private MenuItem pointsMI;

    /**
     * Lines menu item.
     */
    private MenuItem linesMI;

    /**
     * Both points and lines menu item.
     */
    private MenuItem bothMI;

    /**
     * Batch scale stages menu item.
     */
    private MenuItem batchScaleStagesMI;

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

    /**
     * Page Up menu item.
     */
    private MenuItem pageUpMI;

    /**
     * Page Down menu item.
     */
    private MenuItem pageDnMI;

    /** Location ID */
    private String lid = null;

    /** Selected start time */
    private Date beginDate = null;

    /** Selected end time */
    private Date endDate = null;

    /** Selected site name */
    private String siteName = null;

    /** The group info */
    private GroupInfo groupInfo;

    /** Flag to determine if zoomed in or not */
    private boolean zoomSet = false;

    /** Flag to determine if Select Trace is set or not */
    private boolean selectTrace = false;

    /** Flag to determine if Zoom is selected or not */
    private boolean selectZoom = false;

    /** Cancel flag */
    private boolean cancel = false;

    /** Save Edit flag */
    private boolean saveEdit = false;

    /**
     * Display canvas.
     */
    private TimeSeriesDisplayCanvas displayCanvas;

    /**
     * Display Canvas List.
     */
    private ArrayList<TimeSeriesDisplayCanvas> canvasList = new ArrayList<TimeSeriesDisplayCanvas>();

    /** List of page composites */
    private ArrayList<Composite> pageCompList = new ArrayList<Composite>();

    /**
     * The page currently displayed.
     */
    private int currentPage = 0;
    
    /**
     * A flag if Zoom In or not .
     */
    private boolean zoomIn= false;

    /** Flag for inverse printing */
    private boolean inverseVideo = false;

    /**
     * The Stack Composite.
     */
    private Composite stackGridComp;

    /** The total number of pages */
    private int totalPages = 1;

    /** A flag for currently zooming or not */
    private boolean zoomAction = false;

    /**
     * Holds the list of edited points.
     */
    private List<ForecastData> editList = new ArrayList<ForecastData>();

    /**
     * Holds the list of deleted points.
     */
    private List<ForecastData> deleteList = new ArrayList<ForecastData>();

    /**
     * Holds the list of inserted points.
     */
    private List<ForecastData> insertList = new ArrayList<ForecastData>();

    /**
     * The main graph Composite.
     */
    private Composite mainComp = null;

    private Composite canvasComp = null;

    private Rectangle bounds = null;

    private TimeSeriesDlg parentDialog = null;
    
    private int showCatValue = 0;
    
    /** 
     * Zoom reset flag
     * 
     * true when click the zoom reset menu, then false 
     * when a graph has been reset
     */
    private boolean reset = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public TimeSeriesDisplayDlg(Shell parent, Rectangle bounds,
            TimeSeriesDlg parentDialog) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE, CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL);
        setText("Time Series Display");

        this.bounds = bounds;
        this.parentDialog = parentDialog;
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
    protected void disposed() {
        setReturnValue(bounds);
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        // Get default values from Apps Defaults
        loadAppsDefaults();
        
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
        mainGridLayout.marginHeight = 0;
        mainGridLayout.marginWidth = 0;
        mainComp.setLayout(mainGridLayout);
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        createDisplayCanvas();

        shell.setMenuBar(menuBar);

        shell.setMinimumSize(new Point(900, 900));
    }

    @Override
    protected void opened() {
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                bounds = shell.getBounds();
            }

        });
    }

    @Override
    protected void preOpened() {
        if (bounds != null) {
            shell.setBounds(bounds);
        }
    }

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
        timeSeriesControlMI = new MenuItem(fileMenu, SWT.NONE);
        timeSeriesControlMI.setText("&Time Series Control\tCtrl+T");
        timeSeriesControlMI.setAccelerator(SWT.CTRL + 'T');
        timeSeriesControlMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getParent().setFocus();
            }
        });

        // -----------------------------------------
        // Create the Save menu item
        // -----------------------------------------
        saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("&Save\tCtrl+S");
        saveMI.setAccelerator(SWT.CTRL + 'S');
        saveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveGraph();
            }
        });

        // -----------------------------------------
        // Create the Print menu item and sub menu
        // -----------------------------------------
        printSubMI = new MenuItem(fileMenu, SWT.CASCADE);
        printSubMI.setText("Print");

        Menu printSubMenu = new Menu(shell, SWT.DROP_DOWN);
        printSubMI.setMenu(printSubMenu);

        createPrintSubMenu(printSubMenu);

        // -----------------------------------------
        // Create the Quit menu item
        // -----------------------------------------
        quitMI = new MenuItem(fileMenu, SWT.NONE);
        quitMI.setText("&Quit\tCtrl+Q");
        quitMI.setAccelerator(SWT.CTRL + 'Q');
        quitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Create the Page menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createPageMenu(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the file menu
        // ----------------------------------------
        MenuItem pageMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        pageMenuItem.setText("Page");

        // Create the File menu item with a File "dropdown" menu
        Menu pageMenu = new Menu(menuBar);
        pageMenuItem.setMenu(pageMenu);

        // -----------------------------------------------------
        // Create the Page Down menu item in the main menu bar
        // -----------------------------------------------------
        pageUpMI = new MenuItem(pageMenu, SWT.NONE);
        pageUpMI.setText("Page Up\tPageUp");
        pageUpMI.setAccelerator(SWT.PAGE_UP);
        pageUpMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pageUpAction();
            }
        });

        pageDnMI = new MenuItem(pageMenu, SWT.NONE);
        pageDnMI.setText("Page Down\tPageDown");
        pageDnMI.setAccelerator(SWT.PAGE_DOWN);
        pageDnMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pageDownAction();
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
        zoomSubMI = new MenuItem(graphMenu, SWT.CASCADE);
        zoomSubMI.setText("Zoom");

        Menu zoomSubMenu = new Menu(shell, SWT.DROP_DOWN);
        zoomSubMI.setMenu(zoomSubMenu);

        createZoomSubMenu(zoomSubMenu);

        // ------------------------------------------------------
        // Create the Show PC as 1hr. PP menu item and sub menu
        // ------------------------------------------------------
        showPcSubMI = new MenuItem(graphMenu, SWT.CASCADE);
        showPcSubMI.setText("Show PC as 1Hr. PP");

        Menu showPcSubMenu = new Menu(shell, SWT.DROP_DOWN);
        showPcSubMI.setMenu(showPcSubMenu);

        createShowPcSubMenu(showPcSubMenu);

        // ------------------------------------------
        // Create Show Latest Forecast menu item
        // ------------------------------------------
        showLatestFcstMI = new MenuItem(graphMenu, SWT.CHECK);
        showLatestFcstMI.setText("Show Latest &Forecast Only\tCtrl-F");
        showLatestFcstMI.setAccelerator(SWT.CTRL + 'F');
        showLatestFcstMI.setSelection(true);
        showLatestFcstMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                redrawCanvases(true);
            }
        });

        // ------------------------------------------------------
        // Create the Scale Stages menu item and sub menu
        // ------------------------------------------------------
        scaleStagesMI = new MenuItem(graphMenu, SWT.CASCADE);
        scaleStagesMI.setText("Scale Stages");

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
        gridLinesMI.setAccelerator(SWT.CTRL + 'G');

        // TODO - Get this default from the apps_default
        gridLinesMI.setSelection(true);
        gridLinesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MenuItem mi = (MenuItem) event.getSource();
                for (TimeSeriesDisplayCanvas c : canvasList) {
                    c.showGridLines(mi.getSelection());
                }
            }
        });

        // ------------------------------------------------------
        // Create the Plot menu item and sub menu
        // ------------------------------------------------------
        plotMI = new MenuItem(optionsMenu, SWT.CASCADE);
        plotMI.setText("Plot");

        Menu plotSubMenu = new Menu(shell, SWT.DROP_DOWN);
        plotMI.setMenu(plotSubMenu);

        createPlotSubMenu(plotSubMenu);

        // --------------------------------------------------------
        // Create the Batch Scale Stages menu item and sub menu
        // --------------------------------------------------------
        batchScaleStagesMI = new MenuItem(optionsMenu, SWT.CASCADE);
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
        selectTraceMI = new MenuItem(editMenu, SWT.NONE);
        selectTraceMI.setText("S&elect Trace\tCtrl-E");
        selectTraceMI.setAccelerator(SWT.CTRL + 'E');
        selectTraceMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setSelectTrace(true);
                setCancel(false);
            }
        });

        // ------------------------------------------
        // Create Insert menu item
        // ------------------------------------------
        insertMI = new MenuItem(editMenu, SWT.RADIO);
        insertMI.setText("I&nsert\tCtrl-N");
        insertMI.setAccelerator(SWT.CTRL + 'N');
        insertMI.setEnabled(false);
        insertMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteMI.setSelection(false);
                moveMI.setSelection(false);
                setMissingMI.setSelection(false);
            }
        });

        // ------------------------------------------
        // Create Delete menu item
        // ------------------------------------------
        deleteMI = new MenuItem(editMenu, SWT.RADIO);
        deleteMI.setText("Delete\tCtrl-Y");
        deleteMI.setAccelerator(SWT.CTRL + 'Y');
        deleteMI.setEnabled(false);
        deleteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                insertMI.setSelection(false);
                deleteMI.setSelection(true);
                moveMI.setSelection(false);
                setMissingMI.setSelection(false);
            }
        });

        // ------------------------------------------
        // Create Move menu item
        // ------------------------------------------
        moveMI = new MenuItem(editMenu, SWT.RADIO);
        moveMI.setText("&Move\tCtrl-M");
        moveMI.setAccelerator(SWT.CTRL + 'M');
        moveMI.setEnabled(false);
        moveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                insertMI.setSelection(false);
                deleteMI.setSelection(false);
                moveMI.setSelection(true);
                setMissingMI.setSelection(false);
            }
        });

        // ------------------------------------------
        // Create Set Missing menu item
        // ------------------------------------------
        setMissingMI = new MenuItem(editMenu, SWT.RADIO);
        setMissingMI.setText("&Set Missing\tCtrl-S");
        setMissingMI.setAccelerator(SWT.CTRL + 'S');
        setMissingMI.setEnabled(false);
        setMissingMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                insertMI.setSelection(false);
                deleteMI.setSelection(false);
                moveMI.setSelection(false);
                setMissingMI.setSelection(true);
            }
        });

        // Add a menu separator.
        new MenuItem(editMenu, SWT.SEPARATOR);

        // ------------------------------------------
        // Create Save to Database menu item
        // ------------------------------------------
        saveToDatabaseMI = new MenuItem(editMenu, SWT.NONE);
        saveToDatabaseMI.setText("Save to &Database\tCtrl-D");
        saveToDatabaseMI.setAccelerator(SWT.CTRL + 'D');
        saveToDatabaseMI.setEnabled(false);
        saveToDatabaseMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                boolean save = MessageDialog.openConfirm(shell, "Save Edits",
                        "Are you sure you want to save the changes?");
                if (save) {
                    setSelectTrace(false);
                    TimeSeriesDataManager dataManager = TimeSeriesDataManager
                            .getInstance();
                    if (deleteList.size() > 0) {
                        try {
                            dataManager.insertRejectedData(deleteList);
                            dataManager.delete(deleteList);
                            updateMaxFcst(deleteList);
                        } catch (VizException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                            System.err.println("Error deleting points");
                        }
                    }

                    if (insertList.size() > 0) {
                        try {
                            dataManager.insert(insertList);
                            updateMaxFcst(insertList);
                        } catch (VizException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                            MessageBox mb = new MessageBox(shell,
                                    SWT.ICON_ERROR | SWT.OK);
                            mb.setText("Insert Error");
                            mb.setMessage("An error occured during insert.  Not all points were inserted.");
                            mb.open();

                        }
                    }

                    if (editList.size() > 0) {
                        try {
                            dataManager.edit(editList);
                            updateMaxFcst(editList);
                        } catch (VizException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                            System.err.println("Error editing points");
                        }
                    }

                    setSelectTrace(false);
                    setCancel(true);
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

                    // clear the edit point lists
                    insertList.clear();
                    editList.clear();
                    deleteList.clear();

                    for (TimeSeriesDisplayCanvas canvas : canvasList) {
                        if (canvas.getTraceArray() != null) {
                            for (TraceData td : canvas.getTraceArray()) {
                                /* Reset the selection */
                                td.setSelected(false);
                            }
                        }
                        canvas.setGetAgain(true);
                        canvas.redraw();
                        canvas.update();
                    }
                }
            }
        });

        // ------------------------------------------
        // Create Save to Database menu item
        // ------------------------------------------
        cancelChangesMI = new MenuItem(editMenu, SWT.NONE);
        cancelChangesMI.setText("&Cancel Changes\tCtrl-C");
        cancelChangesMI.setAccelerator(SWT.CTRL + 'C');
        cancelChangesMI.setEnabled(false);
        cancelChangesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cancelEdits();
            }
        });
    }

    /**
     * Create the Print sub menu.
     * 
     * @param menuBar
     *            The print menu.
     */
    private void createPrintSubMenu(Menu printSubMenu) {
        reverseVideoMI = new MenuItem(printSubMenu, SWT.NONE);
        reverseVideoMI.setText("Reverse Video");
        reverseVideoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                inverseVideo = true;
                print();
            }
        });

        normalMI = new MenuItem(printSubMenu, SWT.NONE);
        normalMI.setText("Normal");
        normalMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                inverseVideo = false;
                print();
            }
        });
    }

    /**
     * Create the Zoom sub menu.
     * 
     * @param menuBar
     *            The Zoom menu.
     */
    private void createZoomSubMenu(Menu zoomSubMenu) {
        setMI = new MenuItem(zoomSubMenu, SWT.NONE);
        setMI.setText("Set\tCtrl+Z");
        setMI.setAccelerator(SWT.CTRL + 'Z');
        setMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	
            	if (zoomIn == false) {             	
            		setZoom(false);
            		setZoomAction(true);
            		setSelectZoom(true);
            		zoomIn=true;
            	} else {            	
            		setZoom(true);
            		setZoomAction(true);
            		setSelectZoom(true);
            	}
            }
        });

        resetMI = new MenuItem(zoomSubMenu, SWT.NONE);
        resetMI.setText("&Reset\tCtrl+R");
        resetMI.setAccelerator(SWT.CTRL + 'R');
        resetMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	if (groupInfo.isGroupSelected()) {
            	    setZoom(false);
            	    setZoomAction(false);
            	    setSelectZoom(false);
                    reset = true;
                    for (TimeSeriesDisplayCanvas dc :canvasList){
                    	dc.setZoomed(false);
                    	dc.redraw();
                    }
            	} else {
            		displayCanvas.resetTS();
            	}
            }
        });
    }

    /**
     * Create the Show PC as 1Hr PP sub menu.
     * 
     * @param menuBar
     *            The Show PC as 1Hr PP menu.
     */
    private void createShowPcSubMenu(Menu showPcSubMenu) {
        // TODO - Complete the PC as PP interpolation
        offMI = new MenuItem(showPcSubMenu, SWT.RADIO);
        offMI.setText("&Off\tCtrl+O");
        offMI.setAccelerator(SWT.CTRL + 'O');
        offMI.setSelection(true);
        offMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // notImplementedYet("PC as PP Off");
                offMI.setSelection(true);
                interpolateMI.setSelection(false);
                assignMI.setSelection(false);
                displayCanvas.setPcAsPP();
                redrawCanvases();
            }
        });

        interpolateMI = new MenuItem(showPcSubMenu, SWT.RADIO);
        interpolateMI.setText("&Interpolate\tCtrl+I");
        interpolateMI.setAccelerator(SWT.CTRL + 'I');
        interpolateMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // notImplementedYet("PC as PP Interpolate");
                offMI.setSelection(false);
                interpolateMI.setSelection(true);
                assignMI.setSelection(false);
                displayCanvas.setPcAsPP();
                redrawCanvases();
            }
        });

        assignMI = new MenuItem(showPcSubMenu, SWT.RADIO);
        assignMI.setText("&Assign\tCtrl+A");
        assignMI.setAccelerator(SWT.CTRL + 'A');
        assignMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // notImplementedYet("PC as PP Assign");
                offMI.setSelection(false);
                interpolateMI.setSelection(false);
                assignMI.setSelection(true);
                displayCanvas.setPcAsPP();
                redrawCanvases();
            }
        });
    }

    /**
     * Create the Scale Stages sub menu.
     * 
     * @param menuBar
     *            The Scale Stages menu.
     */
    private void createScaleStagesSubMenu(Menu scaleStagesSubMenu) {
        dataOnlyMI = new MenuItem(scaleStagesSubMenu, SWT.RADIO);
        dataOnlyMI.setText("Data Only\tCtrl+1");
        dataOnlyMI.setAccelerator(SWT.CTRL + '1');
        dataOnlyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                batchDataOnlyShowCatMI.setSelection(false);
                batchDataOnlyMI.setSelection(true);
                batchDataAndCategoriesMI.setSelection(false);
                redrawCanvases();
            }
        });

        dataOnlyShowCatMI = new MenuItem(scaleStagesSubMenu, SWT.RADIO);
        dataOnlyShowCatMI.setText("Data Only, Show Categories\tCtrl+2");
        dataOnlyShowCatMI.setAccelerator(SWT.CTRL + '2');
        dataOnlyShowCatMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                batchDataOnlyShowCatMI.setSelection(true);
                batchDataOnlyMI.setSelection(false);
                batchDataAndCategoriesMI.setSelection(false);
                redrawCanvases();
            }
        });

        dataAndCategoriesMI = new MenuItem(scaleStagesSubMenu, SWT.RADIO);
        dataAndCategoriesMI.setText("Data and Categories\tCtrl+3");
        dataAndCategoriesMI.setAccelerator(SWT.CTRL + '3');
        dataAndCategoriesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                batchDataOnlyShowCatMI.setSelection(false);
                batchDataOnlyMI.setSelection(false);
                batchDataAndCategoriesMI.setSelection(true);
                redrawCanvases();
            }
        });
        
        if (showCatValue == 1) {
            dataOnlyMI.setSelection(true);
        } else if (showCatValue == 2) {
            dataOnlyShowCatMI.setSelection(true);
        } else {
            dataAndCategoriesMI.setSelection(true);
        }
    }

    /**
     * Create the Plot sub menu.
     * 
     * @param menuBar
     *            The Plot menu.
     */
    private void createPlotSubMenu(Menu plotSubMenu) {
        pointsMI = new MenuItem(plotSubMenu, SWT.RADIO);
        pointsMI.setText("&Points\tCtrl+P");
        pointsMI.setAccelerator(SWT.CTRL + 'P');
        pointsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pointsMI.setSelection(true);
                bothMI.setSelection(false);
                linesMI.setSelection(false);
                redrawCanvases();
            }
        });

        linesMI = new MenuItem(plotSubMenu, SWT.RADIO);
        linesMI.setText("&Lines\tCtrl+L");
        linesMI.setAccelerator(SWT.CTRL + 'L');
        linesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pointsMI.setSelection(false);
                bothMI.setSelection(false);
                linesMI.setSelection(true);
                redrawCanvases();
            }
        });

        bothMI = new MenuItem(plotSubMenu, SWT.RADIO);
        bothMI.setText("&Both\tCtrl+B");
        bothMI.setAccelerator(SWT.CTRL + 'B');
        bothMI.setSelection(true);
        bothMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pointsMI.setSelection(false);
                bothMI.setSelection(true);
                linesMI.setSelection(false);
                redrawCanvases();
            }
        });
    }

    /**
     * Create the Batch Scale Stages sub menu.
     * 
     * @param menuBar
     *            The Batch Scale Stages menu.
     */
    private void createBatchScaleSubMenu(Menu batchScaleStagesSubMenu) {
        batchDataOnlyMI = new MenuItem(batchScaleStagesSubMenu, SWT.RADIO);
        batchDataOnlyMI.setText("Data Only\tCtrl+F1");
        batchDataOnlyMI.setAccelerator(SWT.F1);
        batchDataOnlyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataOnlyShowCatMI.setSelection(false);
                dataAndCategoriesMI.setSelection(false);
                dataOnlyMI.setSelection(true);
                redrawCanvases();
            }
        });

        batchDataOnlyShowCatMI = new MenuItem(batchScaleStagesSubMenu,
                SWT.RADIO);
        batchDataOnlyShowCatMI.setText("Data Only, Show Categories\tCtrl+F2");
        batchDataOnlyShowCatMI.setAccelerator(SWT.F2);
        batchDataOnlyShowCatMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataOnlyShowCatMI.setSelection(true);
                dataAndCategoriesMI.setSelection(false);
                dataOnlyMI.setSelection(false);
                redrawCanvases();
            }
        });

        batchDataAndCategoriesMI = new MenuItem(batchScaleStagesSubMenu,
                SWT.RADIO);
        batchDataAndCategoriesMI.setText("Data and Categories\tCtrl+F3");
        batchDataAndCategoriesMI.setAccelerator(SWT.F3);
        batchDataAndCategoriesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataOnlyShowCatMI.setSelection(false);
                dataAndCategoriesMI.setSelection(true);
                dataOnlyMI.setSelection(false);
                redrawCanvases();
            }
        });
        
        if (showCatValue == 1) {
            batchDataOnlyMI.setSelection(true);
        } else if (showCatValue == 2) {
            batchDataOnlyShowCatMI.setSelection(true);
        } else {
            batchDataAndCategoriesMI.setSelection(true);
        }
    }
    
    private void loadAppsDefaults() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        this.showCatValue = appsDefaults.getInt("timeseries_showcat", 0);
    }
    
    private void updateMaxFcst(List<ForecastData> pointList) {
        ForecastData point = pointList.get(0);
        String pe = point.getPe();
        String ts = point.getTs();

        if (ts.startsWith("F") || ts.startsWith("C")) {
            /* call Load Max Forecast if update or insert of H or Q PE's */
            if (pe.toUpperCase().startsWith("H")
                    || pe.toUpperCase().startsWith("Q")) {
                String lid = point.getLid();
                
                /* call Load Max Forecast if update or insert of H or Q PE's */
                if (pe.toUpperCase().startsWith("H")
                        || pe.toUpperCase().startsWith("Q")) {
                    try {
                        LoadMaxFcst.loadMaxFcstItem(lid, pe, ts);
                    } catch (VizException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    /**
     * Create the canvas that will be displaying the time series information.
     */
    private void createDisplayCanvas() {

        /* Determine the graphing configuration */
        if (groupInfo.isGroupSelected()) {
            ArrayList<PageInfo> pageList = groupInfo.getPageInfoList();
            pageList.trimToSize();
            totalPages = pageList.size();
            PageInfo[] pageArray = pageList.toArray(new PageInfo[pageList
                    .size()]);

            if (pageList.size() == 1) {
                pageUpMI.setEnabled(false);
                pageDnMI.setEnabled(false);
            } else {
                pageUpMI.setEnabled(true);
                pageDnMI.setEnabled(true);
            }

            stackGridComp = new Composite(mainComp, SWT.NONE);
            GridLayout gl = new GridLayout(1, false);
            gl.marginHeight = 0;
            gl.marginWidth = 0;
            stackGridComp.setLayout(gl);
            stackGridComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                    true));

            int idx = -1;
            for (int i = 0; i < pageArray.length; i++) {
                PageInfo pi = pageArray[i];

                if (pi.getGraphDataList().size() == 0) {
                    continue;
                }
                
                idx++; 
                
                /* Graphs on each page */
                ArrayList<GraphData> graphList = pi.getGraphDataList();
                GraphData[] graphArray = graphList
                        .toArray(new GraphData[graphList.size()]);
                graphList.trimToSize();

                Composite pageComp = new Composite(stackGridComp, SWT.NONE);
                GridLayout pageGL = new GridLayout(6, true);
                pageGL.verticalSpacing = 1;
                pageGL.horizontalSpacing = 1;
                pageGL.marginHeight = 0;
                pageGL.marginWidth = 0;

                pageComp.setLayout(pageGL);
                
                GridData pageGridData = new GridData(SWT.FILL, SWT.FILL, true,
                        true);
                pageComp.setLayoutData(pageGridData);

                for (int j = 0; j < graphArray.length; j++) {
                    GraphData gd = graphArray[j];

                    /* Check the precip menu options */
                    if (interpolateMI.getSelection() || assignMI.getSelection()) {
                        gd.setShowpp(true);
                        if (interpolateMI.getSelection()) {
                            gd.setDerivepp(HydroConstants.INTERPOLATE);
                        } else {
                            gd.setDerivepp(HydroConstants.ASSIGN);
                        }
                    }

                    Composite canvasComp = new Composite(pageComp, SWT.NONE);
                    GridLayout gridLayout = new GridLayout(1, false);
                    gridLayout.verticalSpacing = 0;
                    gridLayout.horizontalSpacing = 0;
                    gridLayout.marginHeight = 0;
                    gridLayout.marginWidth = 0;
                    canvasComp.setLayout(gridLayout);

                    GridData graphGridData = new GridData(SWT.FILL, SWT.FILL,
                            true, true);
                    graphGridData.horizontalSpan = gd.getXsize();
                    graphGridData.verticalSpan = gd.getYsize();
                    canvasComp.setLayoutData(graphGridData);

                    gridLinesMI.setSelection(groupInfo.isGridLines());
                    if (!gd.getShowcat()) {
                        batchDataOnlyMI.setSelection(true);
                        batchDataAndCategoriesMI.setSelection(false);
                        batchDataOnlyShowCatMI.setSelection(false);
                        dataOnlyMI.setSelection(true);
                        dataAndCategoriesMI.setSelection(false);
                        dataOnlyShowCatMI.setSelection(false);
                    } else {
                    	String showCat = AppsDefaults.getInstance().getToken("timeseries_showcat");
                    	int sc = Integer.parseInt(showCat);
                    	System.out.println(showCat);
                    	if (sc == 1) {
                            batchDataOnlyShowCatMI.setSelection(false);
                            batchDataOnlyMI.setSelection(true);
                            batchDataAndCategoriesMI.setSelection(false);
                            dataOnlyShowCatMI.setSelection(false);
                            dataOnlyMI.setSelection(true);
                            dataAndCategoriesMI.setSelection(false);
                    	} else if (sc == 2) {
                            batchDataOnlyShowCatMI.setSelection(true);
                            batchDataOnlyMI.setSelection(false);
                            batchDataAndCategoriesMI.setSelection(false);
                            dataOnlyShowCatMI.setSelection(true);
                            dataOnlyMI.setSelection(false);
                            dataAndCategoriesMI.setSelection(false);                    		
                    	} else {
	                        batchDataOnlyShowCatMI.setSelection(false);
	                        batchDataOnlyMI.setSelection(false);
	                        batchDataAndCategoriesMI.setSelection(true);
	                        dataOnlyShowCatMI.setSelection(false);
	                        dataOnlyMI.setSelection(false);
	                        dataAndCategoriesMI.setSelection(true);
                    	}
                    }
                    
                    String traceMode = groupInfo.getTraceMode().trim();
                    if (traceMode.equalsIgnoreCase("B")) {
                        bothMI.setSelection(true);
                        pointsMI.setSelection(false);
                        linesMI.setSelection(false);
                    } else if (traceMode.equalsIgnoreCase("P")) {
                        bothMI.setSelection(false);
                        pointsMI.setSelection(true);
                        linesMI.setSelection(false);
                    } else if (traceMode.equalsIgnoreCase("L")) {
                        bothMI.setSelection(false);
                        pointsMI.setSelection(false);
                        linesMI.setSelection(true);
                    }

                    displayCanvas = new TimeSeriesDisplayCanvas(this,
                            canvasComp, gd, beginDate,
                            endDate, groupInfo.isGroupSelected());
                    displayCanvas.setHorizontalSpan(gd.getXsize());
                    displayCanvas.setVerticalSpan(gd.getYsize());
                    displayCanvas.showGridLines(groupInfo.isGridLines());
                    displayCanvas.setRatingCurveExist(false);
                    displayCanvas.redraw();
                    displayCanvas.update();
                    canvasList.add(displayCanvas);
                }
                pageCompList.add(pageComp);
                ((GridData) pageCompList.get(idx).getLayoutData()).exclude = true;
                pageCompList.get(idx).setVisible(false);
                stackGridComp.layout();
            }
            
            if (pageCompList.size() > 0) {
                ((GridData) pageCompList.get(0).getLayoutData()).exclude = false;
                pageCompList.get(0).setVisible(true);
            }

            stackGridComp.layout();
        } else {
            /* if not a group selection then only a single page is displayed */
            PageInfo pageInfo = groupInfo.getPageInfoList().get(0);

            /* Disable the page up/down menu items, not needed for single page */
            pageUpMI.setEnabled(false);
            pageDnMI.setEnabled(false);

            /* Get the list of GraphData objects */
            ArrayList<GraphData> graphDataList = pageInfo.getGraphDataList();
            for (int i = 0; i < graphDataList.size(); i++) {
                GraphData gd = graphDataList.get(i);

                canvasComp = new Composite(mainComp, SWT.NONE);
                GridLayout gridLayout = new GridLayout(1, false);
                gridLayout.verticalSpacing = 0;
                gridLayout.horizontalSpacing = 0;
                gridLayout.marginHeight = 0;
                gridLayout.marginWidth = 0;
                canvasComp.setLayout(gridLayout);

                GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
                gridData.horizontalSpan = 6;
                gridData.verticalSpan = 2;
                canvasComp.setLayoutData(gridData);

                displayCanvas = new TimeSeriesDisplayCanvas(this, canvasComp,
                        gd, beginDate, endDate, groupInfo.isGroupSelected());
                displayCanvas.setHorizontalSpan(6);
                displayCanvas.setRatingCurveExist(false);

                /* If only one graphData object then it takes up the whole space */
                if (graphDataList.size() == 1) {
                    displayCanvas.setVerticalSpan(2);
                } else {
                    displayCanvas.setVerticalSpan(1);
                }
                canvasList.add(displayCanvas);
            }
        }
        mainComp.layout();
        shell.layout();
    }

    /**
     * Cancel the edits
     */
    private void cancelEdits() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES
                | SWT.NO);
        mb.setText("Cancel Changes?");
        mb.setMessage("Are you sure you want to cancel your changes?");
        int choice = mb.open();

        if (choice == SWT.YES) {

            setSelectTrace(false);
            setCancel(true);
            for (TimeSeriesDisplayCanvas canvas : canvasList) {
                if (canvas != null) {
                    canvas.cancelEdit();
                    canvas.redraw();
                }
            }
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

            // clear the edit point lists
            if (insertList != null) {
                insertList.clear();
            }
            if (editList != null) {
                editList.clear();
            }
            if (deleteList != null) {
                deleteList.clear();
            }
        }
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
    public void saveGraph() {
        FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        String filename = dialog.open();
        if (filename == null) {
            return;
        }
        saveCanvas(filename);
    }

    /**
     * Captures the canvas and saves the result into a file in a format
     * determined by the filename extension .
     * 
     * @param control
     *            The control to save
     * @param fileName
     *            The name of the image to be saved
     */
    public void saveCanvas(String filename) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < canvasList.size(); i++) {
            Display display = canvasList.get(i).getDisplay();
            Image image = new Image(display,
                    canvasList.get(i).getBounds().width, canvasList.get(i)
                            .getBounds().height);
            GC gc = new GC(image);

            canvasList.get(i).drawCanvas(gc);

            /* Default to PNG */
            int style = SWT.IMAGE_PNG;

            if ((filename.endsWith(".jpg") == true)
                    || filename.endsWith("jpeg")) {
                style = SWT.IMAGE_JPEG;
            } else if (filename.endsWith(".bmp") == true) {
                style = SWT.IMAGE_BMP;
            } else {
                filename += ".png";
            }

            String[] sa = filename.split(FileUtil.fileSeparatorRegex);

            for (int j = 0; j < sa.length; j++) {
                if (j < sa.length - 1) {
                    sb.append(sa[j] + File.separator);
                } else {
                    sb.append(i + sa[j]);
                }
            }

            ImageLoader loader = new ImageLoader();
            loader.data = new ImageData[] { image.getImageData() };
            loader.save(sb.toString(), style);

            sb.setLength(0);
            image.dispose();
            gc.dispose();
        }
    }

    /**
     * Print the graph
     */
    private void print() {
        PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
        PrinterData printerData = dialog.open();

        if (printerData != null) {
            for (int i = 0; i < canvasList.size(); i++) {

                // Create the printer object
                Printer printer = new Printer(printerData);
                printer.startJob("jj");

                // Save the original bounds of our Time Series so that we
                // will be able to restore it.
                Rectangle originalBounds = canvasList.get(i).getBounds();

                // Generate an image based on the Time Series that is displayed.
                Display display = canvasList.get(i).getDisplay();
                Image image = new Image(display,
                        printer.getClientArea().height,
                        printer.getClientArea().width);
                GC gc = new GC(image);

                canvasList.get(i).updateGraphBounds(
                        this.simulateLandscape(printer.getClientArea()));
                canvasList.get(i).drawCanvas(gc);
                gc.dispose();

                // Rotate our image - returns data that can be used to construct
                // an image.
                ImageData destData = this.rotateTimeSeriesImage(image
                        .getImageData());
                image.dispose();

                // Create our new image
                gc = new GC(printer);
                Image printerImage = new Image(printer, destData);
                Rectangle trim = printer.computeTrim(0, 0, 0, 0);

                // Place the image on the page
                if (printer.startPage()) {
                    gc.drawImage(printerImage, -trim.x, -trim.y);
                    printer.endPage();
                }
                printerImage.dispose();
                printer.endJob();
                printer.dispose();
                gc.dispose();

                /* Refresh the Image */
                this.inverseVideo = false;
                canvasList.get(i).updateGraphBounds(originalBounds);
                canvasList.get(i).redraw();
            }
        } else {
            this.inverseVideo = false;
        }
    }

    /**
     * Swaps the width and height of the printable page area to simulate a page
     * in landscape mode.
     * 
     * @param pageBoundaries
     *            the printable area to adjust
     * @return the adjusted printable area
     */
    private Rectangle simulateLandscape(Rectangle pageBoundaries) {
        int saveWidth = pageBoundaries.width;
        pageBoundaries.width = pageBoundaries.height;
        pageBoundaries.height = saveWidth;

        return pageBoundaries;
    }

    /**
     * Rotates the Time Series image 90 degrees. Used to simulate printing the
     * time series in landscape mode.
     * 
     * @param srcData
     *            data from the original image
     * @return the data that is generated after adjustments
     */
    private ImageData rotateTimeSeriesImage(ImageData srcData) {
        int bytesPerPixel = srcData.bytesPerLine / srcData.width;
        int destBytesPerLine = srcData.height * bytesPerPixel;
        byte[] destDataArray = new byte[srcData.data.length];
        int width = 0;
        int height = 0;
        for (int srcY = 0; srcY < srcData.height; srcY++) {
            for (int srcX = 0; srcX < srcData.width; srcX++) {
                int destX = 0;
                int destY = 0;
                int destIndex = 0;
                int srcIndex = 0;

                destX = srcData.height - srcY - 1;
                destY = srcX;
                width = srcData.height;
                height = srcData.width;

                destIndex = (destY * destBytesPerLine)
                        + (destX * bytesPerPixel);
                srcIndex = (srcY * srcData.bytesPerLine)
                        + (srcX * bytesPerPixel);
                System.arraycopy(srcData.data, srcIndex, destDataArray,
                        destIndex, bytesPerPixel);
            }
        }

        return new ImageData(width, height, srcData.depth, srcData.palette,
                destBytesPerLine, destDataArray);
    }

    public void createNewGraph() {
        if (stackGridComp != null) {
            stackGridComp.dispose();
        }
        if (canvasComp != null) {
            canvasComp.dispose();
        }
        canvasList.clear();
        createDisplayCanvas();
        redrawCanvases();

        shell.layout();
        shell.pack();
        shell.redraw();
        shell.update();
    }
    
    protected void pageUpAction() {
        if (currentPage == 0) {
            currentPage = groupInfo.getPageInfoList().size() - 1;
        } else {
            currentPage--;
        }
        for (int i = 0; i < pageCompList.size(); i++) {
            if (currentPage == i) {
                ((GridData) pageCompList.get(i).getLayoutData()).exclude = false;
                pageCompList.get(i).setVisible(true);
            } else {
                ((GridData) pageCompList.get(i).getLayoutData()).exclude = true;
                pageCompList.get(i).setVisible(false);
            }
            stackGridComp.layout();
            stackGridComp.setFocus();
        }
    }
    
    protected void pageDownAction() {
        if (currentPage == groupInfo.getPageInfoList().size() - 1) {
            currentPage = 0;
        } else {
            currentPage++;
        }
        for (int i = 0; i < pageCompList.size(); i++) {
            if (currentPage == i) {
                ((GridData) pageCompList.get(i).getLayoutData()).exclude = false;
                pageCompList.get(i).setVisible(true);
            } else {
                ((GridData) pageCompList.get(i).getLayoutData()).exclude = true;
                pageCompList.get(i).setVisible(false);
            }
            stackGridComp.layout();
            stackGridComp.setFocus();
        }
    }

    /**
     * Redraw each Canvas in the list.
     */
    private void redrawCanvases(boolean getData) {
        for (TimeSeriesDisplayCanvas c : canvasList) {
            c.setGetAgain(getData);
            c.redraw();
        }
    }

    private void redrawCanvases() {
        redrawCanvases(false);
    }

    /**
     * Set if we're zoomed or not.
     * 
     * @param zoom
     *            true if we're currently zoomed in
     */
    public void setZoom(boolean zoom) {
        zoomSet = zoom;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the beginDate
     */
    public Date getBeginDate() {
        return beginDate;
    }

    /**
     * @param beginDate
     *            the beginDate to set
     */
    public void setBeginDate(Date beginDate) {
        this.beginDate = beginDate;
    }

    /**
     * @return the endDate
     */
    public Date getEndDate() {
        return endDate;
    }

    /**
     * @param endDate
     *            the endDate to set
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    /**
     * @return the siteName
     */
    public String getSiteName() {
        return siteName;
    }

    /**
     * @param siteName
     *            the siteName to set
     */
    public void setSiteName(String siteName) {
        this.siteName = siteName;
    }

    /**
     * @return the groupInfo
     */
    public GroupInfo getGroupInfo() {
        return groupInfo;
    }

    /**
     * @param groupInfo
     *            the groupInfo to set
     */
    public void setGroupInfo(GroupInfo groupInfo) {
        this.groupInfo = groupInfo;
    }

    /**
     * @return the pointsMI
     */
    public MenuItem getPointsMI() {
        return pointsMI;
    }

    /**
     * @return the linesMI
     */
    public MenuItem getLinesMI() {
        return linesMI;
    }

    /**
     * @return the bothMI
     */
    public MenuItem getBothMI() {
        return bothMI;
    }

    /**
     * @return the batchDataOnlyMI
     */
    public MenuItem getBatchDataOnlyMI() {
        return batchDataOnlyMI;
    }

    /**
     * @return the batchDataOnlyShowCatMI
     */
    public MenuItem getBatchDataOnlyShowCatMI() {
        return batchDataOnlyShowCatMI;
    }

    /**
     * @return the batchDataAndCategoriesMI
     */
    public MenuItem getBatchDataAndCategoriesMI() {
        return batchDataAndCategoriesMI;
    }

    public MenuItem getScaleStagesDataOnlyMI() {
        return dataOnlyMI;
    }

    public MenuItem getScaleStagesDataOnlyShowCategoreMI() {
        return dataOnlyShowCatMI;
    }

    public MenuItem getScaleStagesDataAndCategoriesMI() {
        return dataAndCategoriesMI;
    }

    /**
     * Are we zoomed or not?
     * 
     * @return the zoomSet
     */
    public boolean isZoomSet() {
        return zoomSet;
    }

    /**
     * Get the current page
     * 
     * @return the current page
     */
    public int getCurrentPage() {
        return currentPage;
    }

    /**
     * Get the total number of pages
     * 
     * @return the number of pages
     */
    public int getTotalPages() {
        return totalPages;
    }

    /**
     * @return the selectTraceMI
     */
    public MenuItem getSelectTraceMI() {
        return selectTraceMI;
    }

    /**
     * @return the insertMI
     */
    public MenuItem getInsertMI() {
        return insertMI;
    }

    /**
     * @return the deleteMI
     */
    public MenuItem getDeleteMI() {
        return deleteMI;
    }

    /**
     * @return the moveMI
     */
    public MenuItem getMoveMI() {
        return moveMI;
    }

    /**
     * @return the setMissingMI
     */
    public MenuItem getSetMissingMI() {
        return setMissingMI;
    }

    /**
     * @return the selectTrace
     */
    public boolean isSelectTrace() {
        return selectTrace;
    }

    /**
     * @param selectTrace
     *            the selectTrace to set
     */
    public void setSelectTrace(boolean selectTrace) {
        this.selectTrace = selectTrace;
    }

    /**
     * @return the selectMove
     */
    public boolean isSelectMove() {
        return moveMI.getSelection();
    }

    /**
     * @param selectMove
     *            the selectMove to set
     */
    public void setSelectMove(boolean selectMove) {
        moveMI.setSelection(selectMove);
    }

    /**
     * @return the cancel
     */
    public boolean isCancel() {
        return cancel;
    }

    /**
     * @param cancel
     *            the cancel to set
     */
    public void setCancel(boolean cancel) {
        this.cancel = cancel;
    }

    /**
     * Are we currently zooming?
     * 
     * @return the zoomAction
     */
    public boolean isZoomAction() {
        return zoomAction;
    }

    /**
     * Set if we're currently zooming or not.
     * 
     * @param zoomAction
     *            the zoomAction to set
     */
    public void setZoomAction(boolean zoomAction) {
        this.zoomAction = zoomAction;
    }

    /**
     * @return the selectZoom
     */
    public boolean isSelectZoom() {
        return selectZoom;
    }

    /**
     * @param selectZoom
     *            the selectZoom to set
     */
    public void setSelectZoom(boolean selectZoom) {
        this.selectZoom = selectZoom;
    }

    /**
     * @return the insert
     */
    public boolean isInsert() {
        return insertMI.getSelection();
    }

    /**
     * @param insert
     *            the insert to set
     */
    public void setInsert(boolean insert) {
        insertMI.setSelection(insert);
    }

    /**
     * @return the delete
     */
    public boolean isDelete() {
        return deleteMI.getSelection();
    }

    /**
     * @param delete
     *            the delete to set
     */
    public void setDelete(boolean delete) {
        deleteMI.setSelection(delete);
    }

    /**
     * @return the setMissing
     */
    public boolean isSetMissing() {
        return setMissingMI.getSelection();
    }

    /**
     * @param setMissing
     *            the setMissing to set
     */
    public void setSetMissing(boolean setMissing) {
        setMissingMI.setSelection(setMissing);
    }

    /**
     * @return the saveEdit
     */
    public boolean isSaveEdit() {
        return saveEdit;
    }

    /**
     * @param saveEdit
     *            the saveEdit to set
     */
    public void setSaveEdit(boolean saveEdit) {
        this.saveEdit = saveEdit;
    }

    /**
     * Is the Display PC as PP Off
     * 
     * @return true if off
     */
    public boolean isPcasPPOff() {
        return offMI.getSelection();
    }

    /**
     * Is the Display PC as PP Interpolate
     * 
     * @return true if Interpolate
     */
    public boolean isPcasPPInterpolate() {
        return interpolateMI.getSelection();
    }

    /**
     * Is the Display PC as PP Assign
     * 
     * @return true if Assign
     */
    public boolean isPcasPPAssign() {
        return assignMI.getSelection();
    }

    /**
     * Set the Display PC as PP Off
     * 
     * @param off
     */
    public void setPcasPPOff(boolean off) {
        offMI.setSelection(off);
    }

    /**
     * Set the Display PC as PP Interpolate
     * 
     * @param interpolate
     */
    public void setPcasPPInterpolate(boolean interpolate) {
        interpolateMI.setSelection(interpolate);
    }

    /**
     * Set the Display PC as PP Assign
     * 
     * @param assign
     */
    public void setPcasPPAssign(boolean assign) {
        assignMI.setSelection(assign);
    }

    /**
     * @return the inverseVideo
     */
    public boolean isInverseVideo() {
        return inverseVideo;
    }

    /**
     * @param inverseVideo
     *            the inverseVideo to set
     */
    public void setInverseVideo(boolean inverseVideo) {
        this.inverseVideo = inverseVideo;
    }

    /**
     * @return the editList
     */
    public List<ForecastData> getEditList() {
        return editList;
    }

    /**
     * @param editList
     *            the editList to set
     */
    public void setEditList(List<ForecastData> editList) {
        this.editList = editList;
    }

    /**
     * Add a point to be edited.
     * 
     * @param td
     *            The TraceData object for the point
     */
    public void addEditPoint(ForecastData data) {
        editList.add(data);
    }

    /**
     * @return the deleteList
     */
    public List<ForecastData> getDeleteList() {
        return deleteList;
    }

    /**
     * Add a point to be deleted.
     * 
     * @param td
     *            The Data object to delete
     */
    public void addDeletePoint(ForecastData data) {
        deleteList.add(data);
    }

    /**
     * @return the insertList
     */
    public List<ForecastData> getInsertList() {
        return insertList;
    }

    /**
     * @param insertList
     *            the insertList to set
     */
    public void setInsertList(List<ForecastData> insertList) {
        this.insertList = insertList;
    }

    /**
     * Add a point to be added.
     * 
     * @param td
     *            The TraceData object to add
     */
    public void addInsertPoint(ForecastData data) {
        insertList.add(data);
    }

    /**
     * Show the latest forecast only?
     * 
     * @return the true if displaying the latest forecast only
     */
    public boolean showLatestFcst() {
        return showLatestFcstMI.getSelection();
    }

    public void disposeDialog() {
        shell.dispose();
    }

    public Rectangle getDialogBounds() {
        if (shell.isDisposed()) {
            return bounds;
        } else {
            return shell.getBounds();
        }
    }

	/**
	 * @param reset the reset to set
	 */
	public void setReset(boolean reset) {
		this.reset = reset;
	}

	/**
	 * @return the reset
	 */
	public boolean isReset() {
		return reset;
	}
}
