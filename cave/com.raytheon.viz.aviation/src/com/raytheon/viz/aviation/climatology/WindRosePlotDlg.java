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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Calendar;
import java.util.TimeZone;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.ITafSiteConfig;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.avnconfig.TafSiteData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * WindRosePlotDlg class displays the Wind Rose dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation. 
 * 3/27/2008    1033       grichard    Changed hours to num hours.
 * 4/10/2008    934        grichard    Populated site lists with icaos.
 * 18 JUN 2008  1119       lvenable    Updated for wind rose drawing.
 * 7/29/2008    1337       grichard    Auto redraw checked by default.
 * 7/29/2008    1342       grichard    Get calendar instance in UTC time zone.
 * 9/12/2008    1444       grichard    Accommodate separate message logs.
 * 3/31/2011    8774       rferrel     killProcess when doing a disposed
 * 4/14/2011    8861       rferrel     Use SaveImageDlg class
 * 23JUL2012    15169      zhao        Use Combo for 'Month' and 'Number of Months'
 *                                     & disabled site controls while drawing
 * 04OCT2012    1229       rferrel     Changes for non-blocking WindRoseConfigDlg.
 * 08OCT2012    1229       rferrel     Made non-blocking.
 * 10/15/2012   1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 03Dec2013    16754      zhao        Modified printImage()
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WindRosePlotDlg extends CaveSWTDialog {

    /**
     * Composite containing message status controls.
     */
    @SuppressWarnings("unused")
    private MessageStatusComp msgStatusComp;

    /**
     * Site list control.
     */
    private List siteList;

    /**
     * Month
     */
    private Combo monthCbo;

    /**
     * Number of months.
     */
    private Combo numMonthsCbo;

    /**
     * Hours spinner.
     */
    private Spinner hourSpnr;

    /**
     * Number of hours spinner.
     */
    private Spinner numHoursSpnr;

    /**
     * Auto redraw check box.
     */
    private Button autoRedrawChk;

    /**
     * Flight Category combo box.
     */
    private Combo flightCatCbo;

    /**
     * Wind Rose drawing canvas component.
     */
    private WindRoseCanvasComp windRoseCanvasComp;

    /**
     * Wind Rose configuration data.
     */
    private WindRoseConfigData windRoseConfigData;

    /**
     * Wind Rose diagram header.
     */
    private String windRoseHeader = "";

    /**
     * Array of 3 letter months.
     */
    public static final String[] MONTHS = { "Jan", "Feb", "Mar", "Apr", "May",
            "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    /**
     * Last directory visited.
     */
    private String saveDir = null;

    /**
     * Dialog to get the Save Image file name and image style.
     */
    private SaveImageDlg siDlg;

    /**
     * The ICAOs to use to populate the station list.
     */
    private java.util.List<String> icaos;

    /**
     * Status message type.
     */
    private StatusMessageType msgType;

    private Button drawBtn;

    private Cursor waitCursor;

    private Cursor defaultCursor;

    /**
     * Message status comp background color.
     */
    private RGB statusCompRGB;

    private WindRoseConfigDlg configDlg;

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
    public WindRosePlotDlg(Shell parent, java.util.List<String> icaos,
            StatusMessageType msgType, RGB statusCompRGB) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Wind Rose Plot");

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
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize all of the controls and layouts
        windRoseConfigData = new WindRoseConfigData();

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        // Create the main Wind Rose controls and drawing canvas.
        createMainControls();

        // Create the controls that will display the incoming messages
        createBottomMessageControls();

        populateStations(icaos);

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Create the menus at the top of the display.
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

        // Save Stats menu item
        MenuItem saveStatsMI = new MenuItem(fileMenu, SWT.NONE);
        saveStatsMI.setText("&Save Stats...");
        saveStatsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveStats();
            }
        });

        // Save Image menu item
        MenuItem saveImageMI = new MenuItem(fileMenu, SWT.NONE);
        saveImageMI.setText("S&ave Image...");
        saveImageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveImage(false);
            }
        });

        // Save Image for Google Maps
        MenuItem saveKmlMI = new MenuItem(fileMenu, SWT.NONE);
        saveKmlMI.setText("Save for &Google Earth");
        saveKmlMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveKml();
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

        // Configure menu item
        MenuItem configureMI = new MenuItem(fileMenu, SWT.NONE);
        configureMI.setText("&Configure...");
        configureMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (configDlg == null || configDlg.getShell() == null
                        || configDlg.isDisposed()) {
                    configDlg = new WindRoseConfigDlg(shell, windRoseConfigData);
                    configDlg.open();
                } else {
                    configDlg.bringToTop();
                }
            }
        });

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
                    String description = "AvnFPS - Wind Rose Display Help";

                    String helpText = "This application displays wind rose for selected month and hour,\nor range of hours. \n\nTime selection\n    Month - selects month.\n    Num Months - select number of months of data to display\n    Hour - selects hour. \n    Num Hours - selects number of hours of data to display\n\nFlight Cat\n    This option menu restricts the search to flight category\n    conditions at or below the selected value. \"All\" means no\n    restrictions.\n\nIf Auto Redraw is selected, changing month, hour, or number of hours\nfields will cause the wind rose to be redrawn for any valid value in\nthese fields.\n\nUse the\"Draw\" button to display wind rose after selecting new site,\nor flight category.\n\nThe displayed image can be printed or stored in a graphic file.\nUse the options under the \"File\" menu for that purpose.";
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

        createDrawingCanvas(mainComp);
    }

    /**
     * Create the site controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createSiteControls(Composite parentComp) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite siteComp = new Composite(parentComp, SWT.BORDER);
        siteComp.setLayout(new GridLayout(1, false));
        siteComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label stationLbl = new Label(siteComp, SWT.CENTER);
        stationLbl.setText("Sites");
        stationLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 321;
        siteList = new List(siteComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        siteList.setLayoutData(gd);

        // ----------------------------------------------------------
        // Create the month/hour control with auto redraw
        // ----------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite monthHourComp = new Composite(siteComp, SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        monthHourComp.setLayout(gl);
        monthHourComp.setLayoutData(gd);

        Label monthLbl = new Label(monthHourComp, SWT.NONE);
        monthLbl.setText("Month:");

        gd = new GridData(66, SWT.DEFAULT);
        monthCbo = new Combo(monthHourComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        monthCbo.setLayoutData(gd);
        for (int i = 1; i <= 12; i++) {
            monthCbo.add("" + i, i - 1);
        }
        monthCbo.select(cal.get(Calendar.MONTH));
        monthCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawWindRose();
                }
            }
        });

        Label numMonthLbl = new Label(monthHourComp, SWT.NONE);
        numMonthLbl.setText("Num Months:");

        gd = new GridData(66, SWT.DEFAULT);
        numMonthsCbo = new Combo(monthHourComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        numMonthsCbo.setLayoutData(gd);
        for (int i = 1; i <= 12; i++) {
            numMonthsCbo.add("" + i, i - 1);
        }
        numMonthsCbo.select(0);
        numMonthsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawWindRose();
                }
            }
        });

        Label hoursLbl = new Label(monthHourComp, SWT.NONE);
        hoursLbl.setText("Hour:");

        gd = new GridData(40, SWT.DEFAULT);
        hourSpnr = new Spinner(monthHourComp, SWT.BORDER);
        hourSpnr.setDigits(0);
        hourSpnr.setIncrement(1);
        hourSpnr.setPageIncrement(3);
        hourSpnr.setMinimum(0);
        hourSpnr.setMaximum(23);
        hourSpnr.setSelection(cal.get(Calendar.HOUR_OF_DAY));
        hourSpnr.setLayoutData(gd);
        hourSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawWindRose();
                }
            }
        });

        Label numHoursLbl = new Label(monthHourComp, SWT.NONE);
        numHoursLbl.setText("Num Hours:");

        gd = new GridData(40, SWT.DEFAULT);
        numHoursSpnr = new Spinner(monthHourComp, SWT.BORDER);
        numHoursSpnr.setDigits(0);
        numHoursSpnr.setIncrement(1);
        numHoursSpnr.setPageIncrement(3);
        numHoursSpnr.setMinimum(1);
        numHoursSpnr.setMaximum(24);
        numHoursSpnr.setSelection(0);
        numHoursSpnr.setLayoutData(gd);
        numHoursSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawWindRose();
                }
            }
        });

        autoRedrawChk = new Button(monthHourComp, SWT.CHECK);
        autoRedrawChk.setText("Auto Redraw");
        autoRedrawChk.setSelection(true);

        // ----------------------------------------------------------
        // Create the flight category and draw controls
        // ----------------------------------------------------------
        Label flightCatLbl = new Label(siteComp, SWT.NONE);
        flightCatLbl.setText("Flight Cat:");

        gd = new GridData(120, SWT.DEFAULT);
        flightCatCbo = new Combo(siteComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        flightCatCbo.add("LIFR");
        flightCatCbo.add("IFR");
        flightCatCbo.add("MVFR");
        flightCatCbo.add("All");
        flightCatCbo.select(3);
        flightCatCbo.setLayoutData(gd);
        flightCatCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawWindRose();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 35;
        drawBtn = new Button(siteComp, SWT.PUSH);
        drawBtn.setText("Draw");
        drawBtn.setLayoutData(gd);
        drawBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                redrawWindRose();
            }
        });
    }

    /**
     * Create the Wind Rose drawing canvas.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createDrawingCanvas(Composite parentComp) {
        windRoseCanvasComp = new WindRoseCanvasComp(parentComp, null);
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        msgStatusComp = new MessageStatusComp(shell, msgType, statusCompRGB,
                null);
    }

    /**
     * Close the display.
     */
    public void closeDisplay() {
        shell.dispose();
    }

    /**
     * Redraw the Wind Rose diagram.
     */
    private void redrawWindRose() {
        if (siteList != null && siteList.getItemCount() != 0) {
            if (waitCursor == null) {
                waitCursor = new Cursor(shell.getDisplay(), SWT.CURSOR_WAIT);
                defaultCursor = shell.getCursor();
            }
            setBusyCursor(true);
            generateWindRoseHeader();

            windRoseCanvasComp.updateAndRedraw(windRoseConfigData.cloneData(),
                    windRoseHeader, monthCbo.getText(), numMonthsCbo.getText(),
                    hourSpnr.getText(), numHoursSpnr.getText(),
                    flightCatCbo.getSelectionIndex(),
                    siteList.getItem(siteList.getSelectionIndex()), this);
        }
    }

    int busyCount = 0;

    private synchronized void setBusyCursor(boolean state) {
        if (state == true) {
            ++busyCount;
            shell.setCursor(waitCursor);
            monthCbo.setEnabled(false);
            numMonthsCbo.setEnabled(false);
            hourSpnr.setEnabled(false);
            numHoursSpnr.setEnabled(false);
            flightCatCbo.setEnabled(false);
            drawBtn.setEnabled(false);
        } else {
            --busyCount;
            if (busyCount == 0) {
                shell.setCursor(defaultCursor);
                monthCbo.setEnabled(true);
                numMonthsCbo.setEnabled(true);
                hourSpnr.setEnabled(true);
                numHoursSpnr.setEnabled(true);
                flightCatCbo.setEnabled(true);
                drawBtn.setEnabled(true);
            }
        }
    }

    public void resetCursor() {
        setBusyCursor(false);
    }

    @Override
    public void disposed() {
        WindRoseDataMgr.getInstance().killProcess();
        if (waitCursor != null) {
            waitCursor.dispose();
        }
        super.disposed();
    }

    /**
     * Generate the Wind Rose diagram header based on the user inputs.
     */
    private void generateWindRoseHeader() {
        String hourFmt = "%02dZ";

        StringBuilder header = new StringBuilder();

        // Add the site name to the header.
        header.append(siteList.getItem(siteList.getSelectionIndex()));
        header.append(" ");

        header.append(MONTHS[monthCbo.getSelectionIndex()]);

        if (numMonthsCbo.getSelectionIndex() == 0) {
            header.append(" ");
        } else {
            header.append("-");

            int endMonth = 0;

            if ((numMonthsCbo.getSelectionIndex() + monthCbo
                    .getSelectionIndex()) > 11) {
                endMonth = numMonthsCbo.getSelectionIndex()
                        + monthCbo.getSelectionIndex() - 12;
                header.append(MONTHS[endMonth]);
            } else {
                endMonth = numMonthsCbo.getSelectionIndex()
                        + monthCbo.getSelectionIndex();
                header.append(MONTHS[endMonth]);
            }
            header.append(" ");
        }

        header.append(String.format(hourFmt, hourSpnr.getSelection()));

        if (numHoursSpnr.getSelection() == 1) {
            header.append(" ");
        } else {
            header.append("-");

            int endHour = 0;

            if (((numHoursSpnr.getSelection() - 1) + hourSpnr.getSelection()) > 23) {
                endHour = (numHoursSpnr.getSelection() - 1)
                        + hourSpnr.getSelection() - 24;
                header.append(String.format(hourFmt, endHour));
            } else {
                endHour = (numHoursSpnr.getSelection() - 1)
                        + hourSpnr.getSelection();
                header.append(String.format(hourFmt, endHour));
            }
            header.append(" ");
        }

        if (flightCatCbo.getItem(flightCatCbo.getSelectionIndex()).compareTo(
                "All") != 0) {
            header.append(flightCatCbo.getItem(flightCatCbo.getSelectionIndex()));
            header.append(" or below");
        }

        windRoseHeader = header.toString();
    }

    /**
     * Save the Wind Rose diagram to file.
     */
    private String saveImage(boolean kml) {
        if (siDlg == null) {
            siDlg = new SaveImageDlg(shell);
        }

        String filename = siDlg.open(kml);
        if (filename == null) {
            return "";
        }
        int style = siDlg.getStyle();

        Image image = windRoseCanvasComp.getWindRoseImage();

        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { image.getImageData() };
        loader.save(filename, style);

        return filename;
    }

    private void printImage() {
        Image image = windRoseCanvasComp.getWindRoseImage();
        ImageData imageData = image.getImageData();
        PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
        PrinterData printerData = dialog.open();

        if (printerData != null) {
            Printer printer = new Printer(printerData);
            Point screenDPI = shell.getDisplay().getDPI();
            Point printerDPI = printer.getDPI();
            Rectangle bounds = printer.getBounds();
            int destX = (screenDPI.x*bounds.width - printerDPI.x*imageData.width)/screenDPI.x/2;
            if (destX < 0) {
                destX = 0;
            }
            int destY = (screenDPI.x*bounds.height - printerDPI.x*imageData.height)/screenDPI.x*80/100/2;
            if (destY < 0) {
                destY = 0;
            }
            printer.startJob("jj");
            GC gc = new GC(printer);
            Image printerImage = new Image(printer, imageData);

            if (printer.startPage()) {
                gc.drawImage(printerImage, 0, 0, imageData.width, imageData.height, destX, destY, 
                                printerDPI.x*imageData.width/screenDPI.x, 
                                printerDPI.x*imageData.height/screenDPI.x);
                printer.endPage();
            }

            printerImage.dispose();
            gc.dispose();
            printer.endJob();
            printer.dispose();
        }
    }

    /**
     * Populate the stations.
     */
    private void populateStations(java.util.List<String> theStationList) {
        siteList.removeAll();
        for (String s : theStationList) {
            siteList.add(s);
        }
        siteList.select(0);
    }

    public void saveStats() {
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        dlg.setText("Save Stats");
        dlg.setFilterPath(saveDir);
        String filename = dlg.open();
        if (filename == null) {
            return;
        }

        File dir = (new File(filename)).getParentFile();
        saveDir = dir.getAbsolutePath();

        if (dir.canWrite() == false) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Selection");
            mb.setMessage("Do not have permission to create a file in the selected directory.");
            mb.open();
            return;
        }

        windRoseCanvasComp.saveStats(filename);
    }

    private void saveKml() {
        double kmlSize = 10;
        double kmlOffset = 0.1;

        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            String station = siteList.getItem(siteList.getSelectionIndex());
            TafSiteData site = config.getSite(station);
            double center_lat = Double.parseDouble(site.latitude)
                    - Math.toDegrees(kmlOffset * kmlSize / 6378.0);
            double center_lon = Double.parseDouble(site.longitude);
            double aspect = 1.0;
            double lat_offset = Math.toDegrees(kmlSize / (2.0 * 6378.0));
            double lon_offset = aspect * lat_offset
                    / Math.cos(Math.toRadians(center_lat));
            double north_lat = center_lat + lat_offset;
            double south_lat = center_lat - lat_offset;
            double east_lon = center_lon + lon_offset;
            double west_lon = center_lon - lon_offset;
            String image_name = saveImage(true);
            if (!"".equals(image_name)) {
                File file = new File(image_name);
                image_name = file.getName();
                String kmlFilePath = file.getParent() + "/" + station
                        + "_Wind_Rose.kml";
                StringBuilder sb = new StringBuilder();
                sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
                sb.append("<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n");
                sb.append("  <Document>\n");
                sb.append("    <name>" + station
                        + " Wind Rose Overlay</name>\n");
                sb.append("    <open>1</open>\n");
                sb.append("    <description>" + station
                        + " Wind Rose Overlay</description>\n");
                sb.append("    <Folder>\n");
                sb.append("      <name>" + station + "</name>\n");
                sb.append("      <visibility>1</visibility>\n");
                sb.append("      <description>" + station
                        + " Wind Rose Overlay</description>\n");
                sb.append("      <GroundOverlay>\n");
                sb.append("        <name>" + station
                        + " Wind Rose Overlay on Terrain</name>\n");
                sb.append("        <visibility>1</visibility>\n");
                sb.append(" <description>Overlay shows " + station
                        + " Wind Rose</description>\n");
                sb.append("        <LookAt>\n");
                sb.append("          <longitude>" + center_lon
                        + "</longitude>\n");
                sb.append("          <latitude>" + center_lat + "</latitude>\n");
                sb.append("          <altitude>0</altitude>\n");
                sb.append("          <heading>0</heading>\n");
                sb.append("          <tilt>0</tilt>\n");
                sb.append("          <range>20000</range>\n");
                sb.append("        </LookAt>\n");
                sb.append("        <Icon>\n");
                sb.append("          <href>file:///c:/windows/path/to/"
                        + image_name + "</href>\n");
                sb.append("   <!-- href>/linux/path/to/" + image_name
                        + "</href -->\n");
                sb.append("        </Icon>\n");
                sb.append("        <LatLonBox>\n");
                sb.append("          <north>" + north_lat + "</north>\n");
                sb.append("          <south>" + south_lat + "</south>\n");
                sb.append("          <east>" + east_lon + "</east>\n");
                sb.append("          <west>" + west_lon + "</west>\n");
                sb.append("          <rotation>0</rotation>\n");
                sb.append("        </LatLonBox>\n");
                sb.append("      </GroundOverlay>\n");
                sb.append("    </Folder>\n");
                sb.append("  </Document>\n");
                sb.append("</kml>\n");
                file = new File(kmlFilePath);
                FileWriter writer = new FileWriter(file);
                BufferedWriter buf = new BufferedWriter(writer);
                buf.write(sb.toString());
                buf.close();
                writer.close();
            }
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
            System.err.print(e.getStackTrace());
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
