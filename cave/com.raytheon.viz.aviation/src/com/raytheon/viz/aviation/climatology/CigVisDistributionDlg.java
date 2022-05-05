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

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.aviation.climatedata.ClimateDataManager;
import com.raytheon.viz.aviation.climatology.CigVisDistDataManager.Element;
import com.raytheon.viz.aviation.climatology.CigVisDistDataManager.GraphType;
import com.raytheon.viz.aviation.climatology.ClimatePythonTask.ClimatePythonListener;
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
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 28, 2008  938      lvenable  Initial creation.
 * Apr 10, 2008  934      grichard  Populated site lists with icaos.
 * Aug 28, 2008  1506     grichard  Added resizeability to dialog shell.
 * Sep 12, 2008  1444     grichard  Accommodate separate message logs.
 * Jan 18, 2010  3823     njensen   retrieveData() and objReceived()
 * Mar 31, 2011  8774     rferrel   killProcess when doing a disposed
 * Apr 04, 2011  8896     rferrel   Made timeout configurable
 * Apr 14, 2011  8861     rferrel   Use SaveImageDlg class
 * Apr 08, 2012  1229     rferrel   Made dialog non-blocking.
 * Oct 15, 2012  1229     rferrel   Changes for non-blocking HelpUsageDlg.
 * Aug 16, 2013  2256     lvenable  Fixed image and cursor memory leaks.
 * Mar 19, 2014  2925     lvenable  Added dispose checks for runAsync.
 * Dec 22, 2015  18342    zhao      Modified code for 'jnt' in objReceived()
 * Jan 26, 2016  5054     randerso  Allow dialog to be parented by display
 * Nov 02, 2016  5979     njensen   Cast to Number where applicable
 * Jul 25, 2018  6748     randerso  Fixed to work with changes in
 *                                  CaveSWTDialog.shouldClose(). Code cleanup.
 * Aug 06, 2019  7878     tgurney   Rewrite data retrieval logic for Python 3
 * May 14, 2020  8067     randerso  Major refactor and code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class CigVisDistributionDlg extends CaveSWTDialog
        implements ClimatePythonListener {
    private static final String AUTO_LABEL = "auto";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CigVisDistributionDlg.class);

    private static final int CANVAS_HEIGHT = 150;

    private org.eclipse.swt.widgets.List siteList;

    private Scale yAxisScale;

    /** Y-Axis modified scale value. */
    private int yAxisScaleVal = 100;

    private TabFolder cigVisTabFolder;

    private CigVisDistDataManager data;

    private Canvas yAxisLblCanvas;

    private Font canvasFont;

    /** The ICAOs to use to populate the station list. */
    private List<String> icaos;

    private StatusMessageType msgType;

    private RGB statusCompRGB;

    private Button drawBtn;

    private AtomicBoolean firstReceived = new AtomicBoolean();

    private String dataSite;

    /** Dialog to get the Save Image file name and image style. */
    private SaveImageDlg siDlg;

    private HelpUsageDlg usageDlg;

    private volatile ClimatePythonTask pythonTask = null;

    /* access this only from synchronized block */
    private int busyCount = 0;

    private Element selectedElement = Element.VISIBILITY;

    /**
     * Constructor for parented dialog
     *
     * @param parent
     * @param icaos
     * @param msgType
     * @param statusCompRGB
     */
    public CigVisDistributionDlg(Shell parent, List<String> icaos,
            StatusMessageType msgType, RGB statusCompRGB) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS - Ceiling/Visibility Distribution");

        this.icaos = icaos;
        this.msgType = msgType;
        this.statusCompRGB = statusCompRGB;
    }

    /**
     * Constructor for top level dialog
     *
     * @param display
     * @param icaos
     * @param msgType
     * @param statusCompRGB
     */
    public CigVisDistributionDlg(Display display, List<String> icaos,
            StatusMessageType msgType, RGB statusCompRGB) {
        super(display, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS - Ceiling/Visibility Distribution");

        this.icaos = icaos;
        this.msgType = msgType;
        this.statusCompRGB = statusCompRGB;
    }

    @Override
    protected Layout constructShellLayout() {
        /* Create the main layout for the shell. */
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    private void cancelPythonTask() {
        if (pythonTask != null) {
            pythonTask.cancel();
            pythonTask = null;
        }
    }

    @Override
    protected void disposed() {
        cancelPythonTask();
        canvasFont.dispose();
        super.disposed();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        /* Initialize all of the controls and layouts */
        canvasFont = new Font(getDisplay(), "Monospace", 10, SWT.NORMAL);

        /* Create the data that will be graphed. */
        data = new CigVisDistDataManager();
        if (!icaos.isEmpty()) {
            data.setSite(icaos.get(0));
        }

        /*
         * Create the menus at the top of the dialog.
         */
        createMenus();

        /* Create the main Wind Rose controls and drawing canvas. */
        createMainControls();

        /* Create the controls that will display the incoming messages */
        createBottomMessageControls();
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

    private void createFileMenu(Menu menuBar) {
        /*
         * Create the file menu
         */
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        /* Create the File menu item with a File "dropdown" menu */
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        /*
         * Create all the items in the File dropdown menu
         */

        /* Save Image menu item */
        MenuItem saveImageMI = new MenuItem(fileMenu, SWT.NONE);
        saveImageMI.setText("&Save Image...");
        saveImageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveImage();
            }
        });

        /* Print Image menu item */
        MenuItem printImageMI = new MenuItem(fileMenu, SWT.NONE);
        printImageMI.setText("&Print Image...");
        printImageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printImage();
            }
        });

        /* Save Stats menu item */
        MenuItem saveStatsMI = new MenuItem(fileMenu, SWT.NONE);
        saveStatsMI.setText("S&ave Stats...");
        saveStatsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveStats();
            }
        });

        /* Menu Separator */
        new MenuItem(fileMenu, SWT.SEPARATOR);

        /* Close menu item */
        MenuItem quitMI = new MenuItem(fileMenu, SWT.NONE);
        quitMI.setText("Q&uit");
        quitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    private void createHelpMenu(Menu menuBar) {
        /*
         * Create the Help menu
         */
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        /*
         * Create the File menu item with a File "dropdown" menu
         */
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        /*
         * Create all the items in the Help dropdown menu
         */

        /* Usage menu item */
        MenuItem usageMI = new MenuItem(helpMenu, SWT.NONE);
        usageMI.setText("&Usage...");
        usageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "AvnFPS - Ceiling and Visibility Display Help";

                    String[] helpText = new String[] {
                            "This application displays ceiling and visibility distribution for",
                            "selected time range.", "",
                            "The distribution is plotted in a form of stacked histogram.",
                            "Plot Type",
                            "    By Month    - the X axis is month. The category frequency is",
                            "                  calculated for a selected range of hours.",
                            "    By Hour     - the X axis is hour of day. The category frequency",
                            "                  is calculated for a selected range of months.",
                            "    By Wind Dir - the X axis is 16 point wind direction. The category",
                            "                  frequency is calculated for a selected range of",
                            "                  months and hours.", "",
                            "Auto Redraw",
                            "    Forces screen redraw every time a new sites selected or date/time",
                            "    widgets are modified.", "", "Element",
                            "    Visibility - only visibility thresholds are checked to determine",
                            "                 flight category",
                            "    Ceiling    - only ceiling thresholds are checked to determine",
                            "                 flight category",
                            "    Joint      - both ceiling and visibility thresholds are checked",
                            "                 to determine flight category", "",
                            "y-axis scale",
                            "    Sets maximum value for the vertical axis. If 'auto' toggle is set,",
                            "    the value is computed. Behaves sensibly if selected value is too",
                            "    low." };

                    usageDlg = new HelpUsageDlg(shell, description,
                            String.join("\n", helpText));
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    private void createMainControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        mainComp.setLayoutData(gd);

        createSiteControls(mainComp);

        populateStations(icaos);

        createCigVisTabFolder(mainComp);
    }

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
        siteList = new org.eclipse.swt.widgets.List(siteComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);

        /*
         * Add an item temporarily to work around eclipse bug:
         * https://bugs.eclipse.org/bugs/show_bug.cgi?id=563189
         */
        siteList.add(" ");
        gd.heightHint = 14 * siteList.getItemHeight();
        siteList.removeAll();
        siteList.setLayoutData(gd);
        siteList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                String site = siteList.getItem(siteList.getSelectionIndex());
                if (!site.equals(dataSite)) {
                    retrieveData(site);
                }
            }
        });

        /*
         * Create the selectedElement group
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group elementGroup = new Group(siteComp, SWT.NONE);
        elementGroup.setText("Element");
        elementGroup.setLayout(new GridLayout(1, false));
        elementGroup.setLayoutData(gd);

        SelectionListener elementSelectionListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (((Button) e.widget).getSelection()) {
                    selectedElement = (Element) e.widget.getData();
                    for (TabItem item : cigVisTabFolder.getItems()) {
                        ((CigVisTabComp) item.getControl())
                                .setElement(selectedElement);
                    }
                }
            }
        };

        for (Element e : Element.values()) {
            Button button = new Button(elementGroup, SWT.RADIO);
            button.setText(e.getButtonLabel());
            button.setData(e);
            button.setSelection(e == selectedElement);
            button.addSelectionListener(elementSelectionListener);
        }

        /*
         * Create the selectedElement group and controls
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group yAxisGroup = new Group(siteComp, SWT.NONE);
        yAxisGroup.setText("Y-Axis Scale");
        yAxisGroup.setLayout(new GridLayout(2, false));
        yAxisGroup.setLayoutData(gd);

        yAxisLblCanvas = new Canvas(yAxisGroup, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        GC gc = new GC(yAxisLblCanvas);
        gc.setFont(canvasFont);
        int width = gc.textExtent(AUTO_LABEL).x;
        width = Math.max(width, gc.textExtent("100").x);
        gc.dispose();

        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = width;
        yAxisLblCanvas.setLayoutData(gd);
        yAxisLblCanvas.addPaintListener(e -> {
            drawYAxisCanvas(e.gc);
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.heightHint = CANVAS_HEIGHT;
        yAxisScale = new Scale(yAxisGroup, SWT.VERTICAL);
        yAxisScale.setMinimum(0);
        yAxisScale.setMaximum(19);
        yAxisScale.setIncrement(1);
        yAxisScale.setPageIncrement(1);
        yAxisScale.setSelection(0);
        yAxisScale.setLayoutData(gd);
        yAxisScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                yAxisScaleVal = 100 - 5 * yAxisScale.getSelection();
                yAxisLblCanvas.redraw();
                getDisplayedTab().setMaxPercent(yAxisScaleVal);
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Button autoChk = new Button(yAxisGroup, SWT.CHECK);
        autoChk.setText("Auto");
        autoChk.setLayoutData(gd);
        autoChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (((Button) e.widget).getSelection()) {
                    yAxisScale.setEnabled(false);
                    yAxisScaleVal = -1;
                } else {
                    yAxisScale.setEnabled(true);
                    yAxisScaleVal = 100 - 5 * yAxisScale.getSelection();
                }
                yAxisLblCanvas.redraw();
                getDisplayedTab().setMaxPercent(yAxisScaleVal);
            }
        });

        /*
         * Create the draw button
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        drawBtn = new Button(siteComp, SWT.PUSH);
        drawBtn.setText("Draw");
        drawBtn.setLayoutData(gd);
        drawBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getDisplayedTab().forceUpdate();
            }
        });

    }

    private void createCigVisTabFolder(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite tabComp = new Composite(parentComp, SWT.BORDER);
        tabComp.setLayout(new GridLayout(1, false));
        tabComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        cigVisTabFolder = new TabFolder(tabComp, SWT.NONE);
        cigVisTabFolder.setLayoutData(gd);

        TabItem byMonthTab = new TabItem(cigVisTabFolder, SWT.NONE);
        byMonthTab.setText("By Month");
        byMonthTab.setControl(new CigVisTabComp(cigVisTabFolder,
                GraphType.MONTH, data, selectedElement));
        TabItem byHourTab = new TabItem(cigVisTabFolder, SWT.NONE);
        byHourTab.setText("By Hour");
        byHourTab.setControl(new CigVisTabComp(cigVisTabFolder, GraphType.HOUR,
                data, selectedElement));
        TabItem byWindDirTab = new TabItem(cigVisTabFolder, SWT.NONE);
        byWindDirTab.setText("By Wind Dir");
        byWindDirTab.setControl(new CigVisTabComp(cigVisTabFolder,
                GraphType.WIND_DIR, data, selectedElement));
    }

    /** Create the message status composite. */
    private void createBottomMessageControls() {
        new MessageStatusComp(shell, msgType, statusCompRGB, null);
    }

    private CigVisTabComp getDisplayedTab() {
        return (CigVisTabComp) cigVisTabFolder
                .getItem(cigVisTabFolder.getSelectionIndex()).getControl();
    }

    /**
     * Draw the Y axis label canvas.
     *
     * @param gc
     *            Graphical context.
     */
    private void drawYAxisCanvas(GC gc) {
        double pixPerInc = 130 / 100.0;

        gc.setBackground(
                getDisplay().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(yAxisLblCanvas.getBounds());

        String label;
        int yCoord;
        if (yAxisScaleVal > 0) {
            yCoord = (int) Math.round((100 - yAxisScaleVal) * pixPerInc);
            label = String.format("%3S", yAxisScaleVal);
        } else {
            yCoord = (int) Math.round(50 * pixPerInc);
            label = AUTO_LABEL;
        }

        gc.setFont(canvasFont);
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawString(label, 0, yCoord + 5, true);
    }

    /** Populate the stations in the site list for this dialog. */
    private void populateStations(List<String> theStationList) {
        siteList.removeAll();
        for (String s : theStationList) {
            siteList.add(s);
        }
    }

    /** Save the CigVisDist diagram to file. */
    private void saveImage() {
        if (siDlg == null) {
            siDlg = new SaveImageDlg(shell);
        }
        String filename = siDlg.open();
        if (filename == null) {
            return;
        }

        int style = siDlg.getStyle();
        TabItem item = cigVisTabFolder
                .getItem(cigVisTabFolder.getSelectionIndex());
        CigVisTabComp comp = (CigVisTabComp) item.getControl();

        Image image = comp.getCigVisDistImage();

        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { image.getImageData() };
        loader.save(filename, style);
        image.dispose();
    }

    private void printImage() {
        PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
        PrinterData printerData = dialog.open();
        Image tmpImage = null;
        Image rotatedImage = null;
        if (printerData != null) {
            /* set the orientation to landscape */
            printerData.orientation = PrinterData.PORTRAIT;

            /* Create the printer object */
            Printer printer = new Printer(printerData);
            printer.startJob("cigvisdistjob");
            GC gc = new GC(printer);
            TabItem item = cigVisTabFolder
                    .getItem(cigVisTabFolder.getSelectionIndex());
            CigVisTabComp comp = (CigVisTabComp) item.getControl();
            if (printer.startPage()) {
                Image image = comp.getCigVisDistImage();
                tmpImage = new Image(gc.getDevice(),
                        image.getImageData().scaledTo(
                                printer.getBounds().height,
                                printer.getBounds().width));

                /* rotate the image */
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

        TabItem item = cigVisTabFolder
                .getItem(cigVisTabFolder.getSelectionIndex());
        CigVisTabComp comp = (CigVisTabComp) item.getControl();

        try (BufferedWriter buf = new BufferedWriter(
                new FileWriter(filename))) {
            switch (comp.getGraphType()) {
            case MONTH:
                data.saveStatsByMonth(buf, selectedElement, comp.getStartHour(),
                        comp.getNumHours());
                break;
            case HOUR:
                data.saveStatsByHour(buf, selectedElement, comp.getStartMonth(),
                        comp.getNumMonths());
                break;
            case WIND_DIR:
                data.saveStatsByWindDir(buf, selectedElement,
                        comp.getStartHour(), comp.getNumHours(),
                        comp.getStartMonth(), comp.getNumMonths());
                break;
            }
        } catch (IOException e) {
            statusHandler.error("Error writing stats to " + filename, e);
        }
    }

    private void retrieveData(final String site) {
        data = new CigVisDistDataManager();
        data.setSite(site);
        firstReceived.set(false);
        dataSite = site;
        try {
            String climateFile = ClimateDataManager.getClimateFilePath(site);
            Map<String, Object> args = new HashMap<>();
            args.put("id_", site);
            args.put("fname", climateFile);
            int timeout = ClimateTimeoutManager.getInstance()
                    .getCigVisDistTimeout();
            cancelPythonTask();
            pythonTask = ClimatePythonTask.execute("get_cvdata", args, this,
                    timeout);
        } catch (Exception e) {
            statusHandler.warn("Error retrieving cvdata", e);
        }
    }

    private synchronized void setBusyCursor(boolean on) {
        if (on) {
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

    @Override
    @SuppressWarnings("unchecked")
    public void sendObj(Object obj) {
        if (isDisposed()) {
            return;
        }
        if (firstReceived.compareAndSet(false, true)) {
            List<Long> years = (List<Long>) obj;
            int startYear = years.get(0).intValue();
            int endYear = years.get(1).intValue();
            data.setYears(startYear, endYear);
        } else {
            if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                int month = ((Number) list.get(0)).intValue();
                int hour = ((Number) list.get(1)).intValue();
                int windDir = ((Number) list.get(2)).intValue();
                int flightCat = ((Number) list.get(3)).intValue();
                float cig = ((Number) list.get(4)).floatValue();
                float vis = ((Number) list.get(5)).floatValue();
                float jnt = ((Number) list.get(6)).floatValue();

                data.set(month, hour, windDir, flightCat, vis, cig, jnt);
            } else {
                VizApp.runAsync(() -> {
                    if (isDisposed()) {
                        return;
                    }

                    for (TabItem item : cigVisTabFolder.getItems()) {
                        ((CigVisTabComp) item.getControl()).setCigVisData(data);
                    }
                });
            }
        }
    }

    @Override
    public void started() {
        VizApp.runAsync(() -> {
            if (!isDisposed()) {
                setBusyCursor(true);
            }
        });
    }

    @Override
    public void finished() {
        pythonTask = null;
        VizApp.runAsync(() -> {
            if (!isDisposed()) {
                setBusyCursor(false);
            }
        });
    }
}
