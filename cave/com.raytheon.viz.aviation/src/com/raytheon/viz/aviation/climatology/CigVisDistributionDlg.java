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
package com.raytheon.viz.aviation.climatology;

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.python.multiprocessing.PyProcessListener;
import com.raytheon.uf.common.python.multiprocessing.PythonProcess;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.utility.ImageUtil;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Distribution dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 4/10/2008    934        grichard    Populated site lists with icaos. 
 * 8/28/2008    1506       grichard    Added resizeability to dialog shell.
 * 9/12/2008    1444       grichard    Accommodate separate message logs.
 * 1/18/2010    3823       njensen    retrieveData() and objReceived()
 * 3/31/2011    8774       rferrel     killProcess when doing a disposed
 * 4/4/2011     8896       rferrel     Made timeout configurable
 * 4/14/2011    8861       rferrel     Use SaveImageDlg class
 * 04/08/2012   1229       rferrel     Made dialog non-blocking.
 * 10/15/2012   1229       rferrel      Changes for non-blocking HelpUsageDlg.
 * 16 Aug 2013  #2256      lvenable    Fixed image and cursor memory leaks.
 * 19Mar2014    #2925       lvenable    Added dispose checks for runAsync.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CigVisDistributionDlg extends CaveSWTDialog implements
        PyProcessListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CigVisDistributionDlg.class);

    /**
     * Site list control.
     */
    private List siteList;

    /**
     * Visibility radio button.
     */
    private Button visibilityRdo;

    /**
     * Ceiling radio button.
     */
    private Button ceilingRdo;

    /**
     * Joint radio button.
     */
    private Button jointRdo;

    /**
     * Y-Axis scale control.
     */
    private Scale yAxisScale;

    /**
     * Y-Axis modified scale value.
     */
    private int yAxisScaleVal = 100;

    /**
     * Auto check box.
     */
    private Button autoChk;

    /**
     * Ceiling & Visibility tab folder.
     */
    private TabFolder cigVisTabFolder;

    /**
     * Month tab item.
     */
    private TabItem byMonthTab;

    /**
     * Hour tab item.
     */
    private TabItem byHourTab;

    /**
     * Wind Direction tab item.
     */
    private TabItem byWindDirTab;

    private CigVisDistDataManager data;

    /**
     * Y Axis label canvas.
     */
    private Canvas yAxisLblCanvas;

    /**
     * Font used for the canvas.
     */
    private Font canvasFont;

    /**
     * The ICAOs to use to populate the station list.
     */
    private java.util.List<String> icaos;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 30;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 150;

    /**
     * Status message type.
     */
    private StatusMessageType msgType;

    /**
     * Message status comp background color.
     */
    private RGB statusCompRGB;

    private Button drawBtn;

    private boolean firstReceived;

    private String dataSite;

    /**
     * Dialog to get the Save Image file name and image style.
     */
    private SaveImageDlg siDlg;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param icaos
     *            Array of ICAOs
     * @param msgType
     *            Status message type.
     * @param statusCompRGB
     *            Message status comp background color.
     */
    public CigVisDistributionDlg(Shell parent, java.util.List<String> icaos,
            StatusMessageType msgType, RGB statusCompRGB) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS - Ceiling/Visibility Distribution");

        this.icaos = icaos;
        this.msgType = msgType;
        this.statusCompRGB = statusCompRGB;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        if (pythonScript != null) {
            pythonScript.killProcess();
        }
        canvasFont.dispose();
        super.disposed();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize all of the controls and layouts
        canvasFont = new Font(getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Create the data that will be graphed.
        data = new CigVisDistDataManager();

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        // Create the main Wind Rose controls and drawing canvas.
        createMainControls();

        // Create the controls that will display the incoming messages
        createBottomMessageControls();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Create the main menu bar and menu items.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
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

        // Save Image menu item
        MenuItem saveImageMI = new MenuItem(fileMenu, SWT.NONE);
        saveImageMI.setText("&Save Image...");
        saveImageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveImage();
            }
        });

        // Print Image menu item
        MenuItem printImageMI = new MenuItem(fileMenu, SWT.NONE);
        printImageMI.setText("&Print Image...");
        printImageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printImage();
            }
        });

        // Save Stats menu item
        MenuItem saveStatsMI = new MenuItem(fileMenu, SWT.NONE);
        saveStatsMI.setText("S&ave Stats...");
        saveStatsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveStats();
            }
        });

        // Menu Separator
        new MenuItem(fileMenu, SWT.SEPARATOR);

        // Close menu item
        MenuItem quitMI = new MenuItem(fileMenu, SWT.NONE);
        quitMI.setText("Q&uit");
        quitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Create the Help menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the File menu item with a File "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        // -------------------------------------------------
        // Create all the items in the Help dropdown menu
        // -------------------------------------------------

        // Usage menu item
        MenuItem usageMI = new MenuItem(helpMenu, SWT.NONE);
        usageMI.setText("&Usage...");
        usageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "AvnFPS - Ceiling and Visibility Display Help";

                    String helpText = "This application displays ceiling and visiblity distribution for\nselected time range.\n\nThe distribution is plotted in a form of stacked histogram. \nPlot Type \n    By Month    - the X axis is month. The category frequency is \n                  calculated for a selected range of hours.\n    By Hour     - the X axis is hour of day. The category frequency \n                  is calculated for a selected range of months.\n    By Wind Dir - the X axis is 16 point wind direction. The category \n                  frequency is calculated for a selected range of \n                  months and hours.\n\nAuto Redraw \n    Forces screen redraw every time a new sites selected or date/time \n    widgets are modified.\n\nElement\n    Visibility - only visibility thresholds are checked to determine \n                 flight category\n    Ceiling -    only ceiling thresholds are checked to determine \n                 flight category\n    Joint -      both ceiling and visibility thresholds are checked \n                 to determine flight category\n\ny-axis scale\n    Sets maximum value for the vertical axis. If 'auto' toggle is set,\n    the value is computed. Behaves sensibly if selected value is too \n    low.";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the main controls on the display.
     */
    private void createMainControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        mainComp.setLayoutData(gd);

        createSiteControls(mainComp);

        populateStations(icaos);

        createCigVisTabFolder(mainComp);
    }

    /**
     * Create the site controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createSiteControls(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite siteComp = new Composite(parentComp, SWT.BORDER);
        siteComp.setLayout(new GridLayout(1, false));
        siteComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label stationLbl = new Label(siteComp, SWT.CENTER);
        stationLbl.setText("Sites");
        stationLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 330;
        siteList = new List(siteComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        siteList.setLayoutData(gd);

        // ----------------------------------------------
        // Create the element group
        // ----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.NONE, true, false);
        Group elementGroup = new Group(siteComp, SWT.NONE);
        elementGroup.setText("Element");
        elementGroup.setLayout(new GridLayout(1, false));
        elementGroup.setLayoutData(gd);

        visibilityRdo = new Button(elementGroup, SWT.RADIO);
        visibilityRdo.setText("Visibility");
        visibilityRdo.setSelection(true);
        visibilityRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayedGraph();
            }
        });

        ceilingRdo = new Button(elementGroup, SWT.RADIO);
        ceilingRdo.setText("Ceiling");
        ceilingRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayedGraph();
            }
        });

        jointRdo = new Button(elementGroup, SWT.RADIO);
        jointRdo.setText("Joint");
        jointRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayedGraph();
            }
        });

        // ----------------------------------------------
        // Create the element group and controls
        // ----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.NONE, true, false);
        Group yAxisGroup = new Group(siteComp, SWT.NONE);
        yAxisGroup.setText("Y-Axis Scale");
        yAxisGroup.setLayout(new GridLayout(2, false));
        yAxisGroup.setLayoutData(gd);

        yAxisLblCanvas = new Canvas(yAxisGroup, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.RIGHT, SWT.NONE, true, false);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;
        yAxisLblCanvas.setLayoutData(gd);
        yAxisLblCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                e.gc.setFont(canvasFont);
                drawYAxisCanvas(e.gc);
            }
        });

        gd = new GridData(SWT.LEFT, SWT.NONE, true, false);
        gd.heightHint = CANVAS_HEIGHT;
        yAxisScale = new Scale(yAxisGroup, SWT.VERTICAL);
        yAxisScale.setMinimum(0);
        yAxisScale.setMaximum(95);
        yAxisScale.setIncrement(5);
        yAxisScale.setPageIncrement(5);
        yAxisScale.setSelection(0);
        yAxisScale.setLayoutData(gd);
        yAxisScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (yAxisScale.getSelection() % 5 != 0) {
                    updateDisplayedGraph();
                    return;
                }
                yAxisScaleVal = 100 - yAxisScale.getSelection();
                yAxisLblCanvas.redraw();
                updateDisplayedGraph();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.NONE, true, false);
        gd.horizontalSpan = 2;
        autoChk = new Button(yAxisGroup, SWT.CHECK);
        autoChk.setText("Auto");
        autoChk.setLayoutData(gd);

        // ----------------------------------------------
        // Create the draw button
        // ----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.NONE, true, false);
        drawBtn = new Button(siteComp, SWT.PUSH);
        drawBtn.setText("Draw");
        drawBtn.setLayoutData(gd);
        drawBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (siteList != null && siteList.getItemCount() != 0) {
                    String site = siteList.getItem(siteList.getSelectionIndex());
                    if (!site.equals(dataSite)) {
                        retrieveData(site);
                    } else {
                        updateDisplayedGraph();
                    }
                }
            }
        });

    }

    /**
     * Create the Ceiling & Visibility tab folder
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createCigVisTabFolder(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite tabComp = new Composite(parentComp, SWT.BORDER);
        tabComp.setLayout(new GridLayout(1, false));
        tabComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        cigVisTabFolder = new TabFolder(tabComp, SWT.NONE);
        cigVisTabFolder.setLayoutData(gd);

        if (siteList != null && siteList.getItemCount() != 0) {
            data.setSite(siteList.getItem(siteList.getSelectionIndex()));
        }
        byMonthTab = new TabItem(cigVisTabFolder, SWT.NONE);
        byMonthTab.setText("By Month");
        byMonthTab.setControl(new CigVisByMonthTabComp(cigVisTabFolder, data,
                this));
        if (siteList != null && siteList.getItemCount() != 0) {
            data.setSite(siteList.getItem(siteList.getSelectionIndex()));
        }
        byHourTab = new TabItem(cigVisTabFolder, SWT.NONE);
        byHourTab.setText("By Hour");
        byHourTab.setControl(new CigVisByHourTabComp(cigVisTabFolder, data,
                this));
        if (siteList != null && siteList.getItemCount() != 0) {
            data.setSite(siteList.getItem(siteList.getSelectionIndex()));
        }
        byWindDirTab = new TabItem(cigVisTabFolder, SWT.NONE);
        byWindDirTab.setText("By Wind Dir");
        byWindDirTab.setControl(new CigVisByWindDirTabComp(cigVisTabFolder,
                data, this));

        cigVisTabFolder.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDisplayedGraph();
            }
        });
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        new MessageStatusComp(shell, msgType, statusCompRGB, null);
    }

    /**
     * Close the display.
     */
    public void closeDisplay() {
        shell.dispose();
    }

    /**
     * Update the graph canvas.
     */
    private void updateDisplayedGraph() {
        TabItem ti = cigVisTabFolder.getItem(cigVisTabFolder
                .getSelectionIndex());

        ((ICigVisTabComp) ti.getControl())
                .setMaxPercentOccurrence(yAxisScaleVal);
        ((ICigVisTabComp) ti.getControl()).redrawCanvas();
    }

    protected CigVisDistDataManager.Element getSelectedElement() {
        CigVisDistDataManager.Element element = null;
        if (visibilityRdo.getSelection()) {
            element = CigVisDistDataManager.Element.VISIBILITY;
        } else if (ceilingRdo.getSelection()) {
            element = CigVisDistDataManager.Element.CEILING;
        } else if (jointRdo.getSelection()) {
            element = CigVisDistDataManager.Element.JOINT;
        }
        return element;
    }

    /**
     * Draw the Y axis label canvas.
     * 
     * @param gc
     *            Graphical context.
     */
    private void drawYAxisCanvas(GC gc) {
        double pixPerInc = 130 / 100.0;

        gc.setBackground(getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        int newYCoord = (int) Math.round(yAxisScale.getSelection() * pixPerInc);

        gc.drawString(String.format("%3S", yAxisScaleVal), 2, newYCoord + 5,
                true);
    }

    /**
     * Populate the stations in the site list for this dialog.
     */
    private void populateStations(java.util.List<String> theStationList) {
        siteList.removeAll();
        for (String s : theStationList) {
            siteList.add(s);
        }
        siteList.select(0);
    }

    /**
     * Save the CigVisDist diagram to file.
     */
    private void saveImage() {
        if (siDlg == null) {
            siDlg = new SaveImageDlg(shell);
        }
        String filename = siDlg.open();
        if (filename == null) {
            return;
        }

        int style = siDlg.getStyle();
        TabItem item = cigVisTabFolder.getItem(cigVisTabFolder
                .getSelectionIndex());
        ICigVisTabComp comp;

        if (item.getText().equals("By Month")) {
            comp = (CigVisByMonthTabComp) byMonthTab.getControl();
        } else if (item.getText().equals("By Hour")) {
            comp = (CigVisByHourTabComp) byHourTab.getControl();
        } else {
            comp = (CigVisByWindDirTabComp) byWindDirTab.getControl();
        }

        Image image = comp.getCigVisDistImage();

        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { image.getImageData() };
        loader.save(filename, style);
    }

    private void printImage() {
        PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
        PrinterData printerData = dialog.open();
        Image tmpImage = null;
        Image rotatedImage = null;
        if (printerData != null) {
            // set the orientation to landscape
            printerData.orientation = PrinterData.PORTRAIT;

            // Create the printer object
            Printer printer = new Printer(printerData);
            printer.startJob("cigvisdistjob");
            GC gc = new GC(printer);
            TabItem item = cigVisTabFolder.getItem(cigVisTabFolder
                    .getSelectionIndex());
            ICigVisTabComp comp;
            if (printer.startPage()) {
                if (item.getText().equals("By Month")) {
                    comp = (ICigVisTabComp) byMonthTab.getControl();
                } else if (item.getText().equals("By Hour")) {
                    comp = (ICigVisTabComp) byHourTab.getControl();
                } else {
                    comp = (ICigVisTabComp) byWindDirTab.getControl();
                }

                Image image = comp.getCigVisDistImage();
                tmpImage = new Image(gc.getDevice(), image.getImageData()
                        .scaledTo(printer.getBounds().height,
                                printer.getBounds().width));

                // rotate the image
                rotatedImage = ImageUtil.rotateImage(tmpImage);

                gc.drawImage(rotatedImage, 0, 0);
                printer.endPage();
            }
            gc.dispose();
            printer.endJob();
            printer.dispose();

            if (tmpImage != null) {
                tmpImage.dispose();
            }

            if (rotatedImage != null) {
                rotatedImage.dispose();
            }
        }
    }

    private void saveStats() {
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        String filename = dlg.open();
        if (filename == null) {
            return;
        }

        TabItem item = cigVisTabFolder.getItem(cigVisTabFolder
                .getSelectionIndex());

        CigVisDistDataManager.Element element = getSelectedElement();
        if (item.getText().equals("By Month")) {
            int startHour = ((CigVisByMonthTabComp) byMonthTab.getControl())
                    .getStartHour();
            int endHour = ((CigVisByMonthTabComp) byMonthTab.getControl())
                    .getEndHour();
            data.saveStatsByMonth(filename, element, startHour, endHour);
        } else if (item.getText().equals("By Hour")) {
            int startMonth = ((CigVisByHourTabComp) byHourTab.getControl())
                    .getStartMonth();
            int endMonth = ((CigVisByHourTabComp) byHourTab.getControl())
                    .getEndMonth();
            data.saveStatsByHour(filename, element, startMonth, endMonth);
        } else {
            int startHour = ((CigVisByWindDirTabComp) byWindDirTab.getControl())
                    .getStartHour();
            int endHour = ((CigVisByWindDirTabComp) byWindDirTab.getControl())
                    .getEndHour();
            int startMonth = ((CigVisByWindDirTabComp) byWindDirTab
                    .getControl()).getStartMonth();
            int endMonth = ((CigVisByWindDirTabComp) byWindDirTab.getControl())
                    .getEndMonth();
            data.saveStatsByWindDir(filename, element, startHour, endHour,
                    startMonth, endMonth);
        }
    }

    // TODO remove timing once it's good
    private long t0;

    private long t1;

    private long t2;

    private PythonProcess pythonScript = null;

    private void retrieveData(final String site) {
        final int timeout = ClimateTimeoutManager.getInstance()
                .getCigVisDistTimeout();

        setBusyCursor(true);
        data = new CigVisDistDataManager();
        data.setSite(site);
        firstReceived = false;
        dataSite = site;
        t0 = System.currentTimeMillis();

        Runnable run = new Runnable() {

            @Override
            public void run() {
                PythonProcess myPythonScript = null;
                try {
                    String climateFile = ClimatePython.getClimateFilePath(site);

                    myPythonScript = ClimatePython.getClimateInterpreter();
                    if (pythonScript != null) {
                        pythonScript.killProcess();
                        pythonScript = null;
                    }
                    pythonScript = myPythonScript;

                    Map<String, Object> args = new HashMap<String, Object>();
                    args.put("id_", site);
                    args.put("fname", climateFile);
                    pythonScript.execute("get_cvdata", args,
                            CigVisDistributionDlg.this, timeout);
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving cvdata", e);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
                } finally {
                    if (myPythonScript != null) {
                        myPythonScript.dispose();
                        if (pythonScript == myPythonScript) {
                            pythonScript = null;
                        }
                    }

                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }

                            setBusyCursor(false);
                        }
                    });
                }
            }
        };
        Thread t = new Thread(run);
        t.start();
    }

    private int busyCount = 0;

    private synchronized void setBusyCursor(boolean state) {
        if (state == true) {
            ++busyCount;
            drawBtn.setEnabled(false);
            shell.setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        } else {
            --busyCount;
            if (busyCount == 0) {
                drawBtn.setEnabled(true);
                shell.setCursor(null);
            }
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public void objReceived(Object obj) {
        if (isDisposed() == true) {
            return;
        }
        if (!firstReceived) {
            t1 = System.currentTimeMillis();
            firstReceived = true;
            java.util.List<Integer> years = (java.util.List<Integer>) obj;
            int startYear = years.get(0);
            int endYear = years.get(1);
            data.setYears(startYear, endYear);
        } else {
            if (obj instanceof java.util.List) {
                java.util.List<?> list = (java.util.List<?>) obj;
                int month = (Integer) list.get(0);
                int hour = (Integer) list.get(1);
                int windDir = (Integer) list.get(2);
                int flightCat = (Integer) list.get(3);
                float cig = (Float) list.get(4);
                float vis = (Float) list.get(5);
                float jnt = Math.min(cig, vis);

                data.set(month, hour, windDir, flightCat, vis, cig, jnt);
            } else {
                t2 = System.currentTimeMillis();
                System.out.println("Total process time: " + (t2 - t0));
                System.out.println("Separate process time: " + (t1 - t0));
                System.out.println("Transferring back time: " + (t2 - t1));
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        if (isDisposed()) {
                            return;
                        }

                        ((ICigVisTabComp) byMonthTab.getControl())
                                .setCigVisData(data);
                        ((ICigVisTabComp) byHourTab.getControl())
                                .setCigVisData(data);
                        ((ICigVisTabComp) byWindDirTab.getControl())
                                .setCigVisData(data);
                        updateDisplayedGraph();
                    }
                });
            }
        }
    }
}
