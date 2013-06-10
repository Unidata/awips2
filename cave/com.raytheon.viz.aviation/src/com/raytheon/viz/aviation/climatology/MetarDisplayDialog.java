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
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.python.multiprocessing.PyProcessListener;
import com.raytheon.uf.common.python.multiprocessing.PythonProcess;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.editor.HeaderTextComp;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * MetarDisplayDialog class displays the METAR display dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 3/27/2008    1033       grichard    Corrected dayLbl.
 * 4/10/2008    934        grichard    Populated site lists with icaos.
 * 7/29/2008    1337       grichard    Auto redraw checked by default.
 * 9/12/2008    1444       grichard    Accommodate separate message logs.
 * 1/14/2010    3823       njensen    Retrieval of data in separate process
 * 12/9/2010    7380       rferrel     Changed HeaderTextComp constructor
 *                                     call.
 * 3/22/2011    7864       rferrel     Cleanup to properly dispaly results.
 * 3/31/2011    8774       rferrel     killProcess when doing a disposed
 * 4/4/2011     8896       rferrel     Made timeout configurable
 * 4/8/2011     8838       rferrel     Properly set up "Show Display"
 * 4/12/2011    8861       rferrel     Added file permission check in savedata
 * 10/09/2012   1229       rferrel     Change to non-blocking dialog.
 * 10/15/2012   1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class MetarDisplayDialog extends CaveSWTDialog implements
        PyProcessListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetarDisplayDialog.class);

    /**
     * Station List control.
     */
    private List stationList;

    /**
     * Year spinner control.
     */
    private Spinner yearSpnr;

    /**
     * Month spinner control.
     */
    private Spinner monthSpnr;

    /**
     * Day spinner control.
     */
    private Spinner daySpnr;

    /**
     * Number of Days spinner control.
     */
    private Spinner numDaysSpnr;

    /**
     * Auto redraw check box.
     */
    private Button autoRedrawChk;

    /**
     * The ICAOs to use to populate the station list.
     */
    private java.util.List<String> icaos;

    /**
     * Status message type.
     */
    private StatusMessageType msgType;

    /**
     * Message status comp background color.
     */
    private RGB statusCompRGB;

    /**
     * Last directory visited.
     */
    private String saveDataDir = null;

    private HeaderTextComp headerTextComp;

    private boolean showDecoded;

    private Cursor waitCursor;

    private Cursor defaultCursor;

    private Button showBtn;

    private int busyCnt = 0;

    private PythonProcess pythonScript = null;

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
    public MetarDisplayDialog(Shell parent, java.util.List<String> icaos,
            StatusMessageType msgType, RGB statusCompRGB) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS - METAR Display");

        this.icaos = icaos;
        this.msgType = msgType;
        this.statusCompRGB = statusCompRGB;
        this.showDecoded = true;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 5;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        if (pythonScript != null) {
            pythonScript.killProcess();
        }
        if (waitCursor != null) {
            waitCursor.dispose();
        }
        super.disposed();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        defaultCursor = this.shell.getCursor();

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        createMainControls();

        createBottomMessageControls();

        populateStations(icaos);
    }

    /**
     * Create the menus at the top of the display.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createOptionsMenu(menuBar);
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

        // Print menu item
        MenuItem printMI = new MenuItem(fileMenu, SWT.NONE);
        printMI.setText("&Print...");
        printMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PrintDialog pd = new PrintDialog(shell);
                PrinterData printerData = pd.open();

                if (printerData != null) {
                    Printer printer = new Printer(printerData);
                    printer.startJob("metar text");

                    Rectangle clientArea = printer.getClientArea();
                    Rectangle trim = printer.computeTrim(0, 0, 0, 0);
                    Point dpi = printer.getDPI();
                    int leftMargin = dpi.x + trim.x;
                    int rightMargin = clientArea.width - dpi.x + trim.x
                            + trim.width;
                    int topMargin = dpi.y + trim.y;
                    int bottomMargin = clientArea.height - dpi.y + trim.y
                            + trim.height;

                    int tabSize = 4;
                    StringBuffer tabBuffer = new StringBuffer(tabSize);
                    for (int i = 0; i < tabSize; i++) {
                        tabBuffer.append(' ');
                    }
                    String tabs = tabBuffer.toString();

                    GC gc = new GC(printer);

                    Font font = new Font(getDisplay(), "Courier", 6, SWT.NORMAL);

                    FontData fontData = font.getFontData()[0];
                    Font printerFont = new Font(printer, fontData.getName(),
                            fontData.getHeight(), fontData.getStyle());
                    gc.setFont(printerFont);
                    int tabWidth = gc.stringExtent(tabs).x;
                    int lineHeight = gc.getFontMetrics().getHeight();

                    RGB rgb = getDisplay().getSystemColor(SWT.COLOR_BLACK)
                            .getRGB();
                    Color printerForegroundColor = new Color(printer, rgb);
                    gc.setForeground(printerForegroundColor);

                    rgb = getDisplay().getSystemColor(SWT.COLOR_WHITE).getRGB();
                    Color printerBackgroundColor = new Color(printer, rgb);
                    gc.setBackground(printerBackgroundColor);

                    String textToPrint = headerTextComp.getHeaderStTxt()
                            .getText();
                    textToPrint += "\n";
                    textToPrint += headerTextComp.getDataStTxt().getText();

                    printer.startPage();
                    StringBuffer wordBuffer = new StringBuffer();
                    int x = leftMargin;
                    int y = topMargin;
                    int index = 0;
                    int end = textToPrint.length();
                    while (index < end) {
                        char c = textToPrint.charAt(index);
                        index++;
                        if (c != 0) {
                            if (c == 0x0a || c == 0x0d) {
                                if (c == 0x0d && index < end
                                        && textToPrint.charAt(index) == 0x0a) {
                                    index++;
                                }
                                if (wordBuffer.length() > 0) {
                                    String word = wordBuffer.toString();
                                    int wordWidth = gc.stringExtent(word).x;
                                    if (x + wordWidth > rightMargin) {
                                        x = leftMargin;
                                        y += lineHeight;
                                        if (y + lineHeight > bottomMargin) {
                                            printer.endPage();
                                            if (index + 1 < end) {
                                                y = topMargin;
                                                printer.startPage();
                                            }
                                        }
                                    }
                                    gc.drawString(word, x, y, false);
                                    x += wordWidth;
                                    wordBuffer = new StringBuffer();
                                }
                                x = leftMargin;
                                y += lineHeight;
                                if (y + lineHeight > bottomMargin) {
                                    printer.endPage();
                                    if (index + 1 < end) {
                                        y = topMargin;
                                        printer.startPage();
                                    }
                                }
                            } else {
                                if (c != '\t') {
                                    wordBuffer.append(c);
                                }
                                if (Character.isWhitespace(c)) {
                                    if (wordBuffer.length() > 0) {
                                        String word = wordBuffer.toString();
                                        int wordWidth = gc.stringExtent(word).x;
                                        if (x + wordWidth > rightMargin) {
                                            x = leftMargin;
                                            y += lineHeight;
                                            if (y + lineHeight > bottomMargin) {
                                                printer.endPage();
                                                if (index + 1 < end) {
                                                    y = topMargin;
                                                    printer.startPage();
                                                }
                                            }
                                        }
                                        gc.drawString(word, x, y, false);
                                        x += wordWidth;
                                        wordBuffer = new StringBuffer();
                                    }
                                    if (c == '\t') {
                                        x += tabWidth;
                                    }
                                }
                            }
                        }
                    }
                    if (y + lineHeight <= bottomMargin) {
                        printer.endPage();
                    }

                    printer.endJob();
                    printerForegroundColor.dispose();
                    printerBackgroundColor.dispose();
                    gc.dispose();
                    font.dispose();
                    printer.dispose();
                }
            }
        });

        // Save As menu item
        MenuItem saveAsMI = new MenuItem(fileMenu, SWT.NONE);
        saveAsMI.setText("&Save As...");
        saveAsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveData();
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
     * Create the Options menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createOptionsMenu(Menu menuBar) {
        // ----------------------------------------
        // Create the options menu
        // ----------------------------------------
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("&Options");

        // Create the Options menu item with a Options "dropdown" menu
        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        // ----------------------------------------------------
        // Create all the items in the Options dropdown menu
        // ----------------------------------------------------

        // Show Decoded menu item
        final MenuItem showDecodedMI = new MenuItem(optionsMenu, SWT.CHECK);
        showDecodedMI.setText("&Show Decoded");
        showDecodedMI.setSelection(showDecoded);
        showDecodedMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showDecoded = showDecodedMI.getSelection();
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
                    String description = "METAR Display Help";

                    String helpText = "This dialog is used to display METARs reconstructed from climate data.\n\nMenu Bar\nFile:\n    Print:      invokes printer selection dialog.\n    Save As:    invokes file selection dialog.\n\nOptions:\n    Show Decoded:   toggles between METAR and decoded (ARONET) display \n                format\n    Update on Selection: when selected, \"Station\", \"Month\", \"Day\" \n                and \"Num Days\" changes update the display without \n                pressing \"Show\"\n\nDate Selection\n    Year:       select start year.\n    Month:      select start month.\n    Day:        select start day. Range of days is always 1-31, \n                year 2000, month 2, day 31 results in data for \n                March 2, 2000.\n    Num Days:   number of days of data to display.\n\nShow:   displays reconstructed METARs for the selected dates and \n        display format.";

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

        createStationList(mainComp);

        createDateAndTextControls(mainComp);
    }

    /**
     * Create the Station List control.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createStationList(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite stationComp = new Composite(parentComp, SWT.BORDER);
        stationComp.setLayout(new GridLayout(1, false));
        stationComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label stationLbl = new Label(stationComp, SWT.CENTER);
        stationLbl.setText("Stations");
        stationLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;
        stationList = new List(stationComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        stationList.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        showBtn = new Button(stationComp, SWT.PUSH);
        showBtn.setText("Show");
        showBtn.setLayoutData(gd);
        showBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                retrieveClimateData();
            }
        });
    }

    /**
     * Create the Date and Text controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createDateAndTextControls(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite dateTextComp = new Composite(parentComp, SWT.BORDER);
        dateTextComp.setLayout(new GridLayout(2, false));
        dateTextComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.NONE, true, false);
        Group dateGroup = new Group(dateTextComp, SWT.NONE);
        dateGroup.setText("Date Selection");
        GridLayout gl = new GridLayout(10, false);
        dateGroup.setLayout(gl);
        dateGroup.setLayoutData(gd);

        Label yearLbl = new Label(dateGroup, SWT.NONE);
        yearLbl.setText("Year:");

        gd = new GridData(50, SWT.DEFAULT);
        yearSpnr = new Spinner(dateGroup, SWT.BORDER);
        yearSpnr.setDigits(0);
        yearSpnr.setIncrement(1);
        yearSpnr.setPageIncrement(5);
        yearSpnr.setMaximum(3000);
        yearSpnr.setSelection(2008);
        yearSpnr.setLayoutData(gd);
        yearSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    retrieveClimateData();
                }
            }
        });

        Label monthLbl = new Label(dateGroup, SWT.NONE);
        monthLbl.setText("Month:");

        gd = new GridData(50, SWT.DEFAULT);
        monthSpnr = new Spinner(dateGroup, SWT.BORDER);
        monthSpnr.setDigits(0);
        monthSpnr.setIncrement(1);
        monthSpnr.setPageIncrement(3);
        monthSpnr.setMinimum(1);
        monthSpnr.setMaximum(12);
        monthSpnr.setSelection(1);
        monthSpnr.setLayoutData(gd);
        monthSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    retrieveClimateData();
                }
            }
        });

        Label dayLbl = new Label(dateGroup, SWT.NONE);
        dayLbl.setText("Day:");

        gd = new GridData(50, SWT.DEFAULT);
        daySpnr = new Spinner(dateGroup, SWT.BORDER);
        daySpnr.setDigits(0);
        daySpnr.setIncrement(1);
        daySpnr.setPageIncrement(5);
        daySpnr.setMinimum(1);
        daySpnr.setMaximum(31);
        daySpnr.setSelection(1);
        daySpnr.setLayoutData(gd);
        daySpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    retrieveClimateData();
                }
            }
        });

        Label numDaysLbl = new Label(dateGroup, SWT.NONE);
        numDaysLbl.setText("Num Days:");

        gd = new GridData(50, SWT.DEFAULT);
        numDaysSpnr = new Spinner(dateGroup, SWT.BORDER);
        numDaysSpnr.setDigits(0);
        numDaysSpnr.setIncrement(1);
        numDaysSpnr.setPageIncrement(5);
        numDaysSpnr.setMinimum(1);
        numDaysSpnr.setMaximum(31);
        numDaysSpnr.setSelection(1);
        numDaysSpnr.setLayoutData(gd);
        numDaysSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    retrieveClimateData();
                }
            }
        });

        gd = new GridData(50, SWT.DEFAULT);
        Label filler = new Label(dateGroup, SWT.NONE);
        filler.setLayoutData(gd);

        autoRedrawChk = new Button(dateTextComp, SWT.CHECK);
        autoRedrawChk.setText("Auto Redraw");
        autoRedrawChk.setSelection(true);

        // ----------------------------------------------
        // Create the text (and header) controls
        // ----------------------------------------------

        Composite textComp = new Composite(dateTextComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        textComp.setLayout(new GridLayout(1, false));
        textComp.setLayoutData(gd);

        headerTextComp = new HeaderTextComp(textComp, true, true);
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
    private void closeDisplay() {
        shell.dispose();
    }

    /**
     * Populate the station list.
     * 
     * @param theStationList
     *            -- the list of stations.
     */
    private void populateStations(java.util.List<String> theStationList) {
        stationList.removeAll();
        for (String s : theStationList) {
            stationList.add(s);
        }
        stationList.select(0);
    }

    private void retrieveClimateData() {
        if (stationList != null && stationList.getItemCount() != 0) {
            final int timeout = ClimateTimeoutManager.getInstance()
                    .getClimateMetarTimeout();
            final String site = stationList.getItem(stationList
                    .getSelectionIndex());
            final String year = yearSpnr.getText();
            final String month = monthSpnr.getText();
            final String day = daySpnr.getText();
            final String numDays = numDaysSpnr.getText();
            headerTextComp.getHeaderStTxt().setText("");
            headerTextComp.getDataStTxt().setText("");
            if (waitCursor == null) {
                waitCursor = new Cursor(shell.getDisplay(), SWT.CURSOR_WAIT);
            }
            setCursorBusy(true);
            showBtn.setEnabled(false);
            Runnable run = new Runnable() {

                @Override
                public void run() {
                    PythonProcess myPythonScript = null;

                    try {
                        long t0 = System.currentTimeMillis();
                        String climateFile = ClimatePython
                                .getClimateFilePath(site);

                        myPythonScript = ClimatePython.getClimateInterpreter();

                        if (pythonScript != null) {
                            pythonScript.killProcess();
                            pythonScript = null;
                        }
                        pythonScript = myPythonScript;

                        Map<String, Object> args = new HashMap<String, Object>();
                        args.put("id_", site);
                        args.put("year", year);
                        args.put("month", month);
                        args.put("day", day);
                        args.put("ndays", numDays);
                        args.put("decoded", showDecoded);
                        args.put("fname", climateFile);
                        myPythonScript.execute("get_metars", args,
                                MetarDisplayDialog.this, timeout);

                        long t1 = System.currentTimeMillis();
                        System.out
                                .println("Metar retrieval time: " + (t1 - t0));
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM, e.getMessage(),
                                e);
                    } catch (JepException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error retrieving metar climate data", e);
                    } finally {
                        if (myPythonScript != null) {
                            myPythonScript.dispose();
                            if (pythonScript == myPythonScript) {
                                pythonScript = null;
                            }
                        }

                        if (isDisposed() == false) {
                            VizApp.runAsync(new Runnable() {
                                @Override
                                public void run() {
                                    if (!headerTextComp.isDisposed()) {
                                        showBtn.setEnabled(true);
                                        setCursorBusy(false);
                                    }
                                }
                            });
                        }
                    }
                }
            };
            Thread t = new Thread(run);
            t.start();
        }
    }

    private synchronized void setCursorBusy(boolean state) {
        if (state) {
            ++busyCnt;
            shell.setCursor(waitCursor);
            headerTextComp.getHeaderStTxt().setCursor(waitCursor);
            headerTextComp.getDataStTxt().setCursor(waitCursor);
        } else {
            --busyCnt;
            if (busyCnt == 0) {
                shell.setCursor(defaultCursor);
                headerTextComp.getHeaderStTxt().setCursor(defaultCursor);
                headerTextComp.getDataStTxt().setCursor(defaultCursor);
            }
        }
    }

    private void saveData() {
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        dlg.setText("Save Data");
        dlg.setFilterPath(saveDataDir);
        String filename = dlg.open();
        if (filename == null) {
            return;
        }

        File dir = (new File(filename)).getParentFile();
        saveDataDir = dir.getAbsolutePath();

        if (dir.canWrite() == false) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Selection");
            mb.setMessage("Do not have permission to create a file in the selected directory.");
            mb.open();
            return;
        }

        File file = new File(filename);
        FileWriter writer;
        try {
            writer = new FileWriter(file);
            BufferedWriter buf = new BufferedWriter(writer);

            buf.write(headerTextComp.getHeaderStTxt().getText());
            buf.write("\n");
            buf.write(headerTextComp.getDataStTxt().getText());

            buf.close();
            writer.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Override
    public void objReceived(final Object obj) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                String result = (String) obj;
                if (!headerTextComp.isDisposed()) {
                    if (result.contains("METARs for")
                            || result.startsWith("Date")) {
                        headerTextComp.getHeaderStTxt().setText(result);
                        headerTextComp.getDataStTxt().setText("");
                    } else {
                        String current = headerTextComp.getDataStTxt()
                                .getText();
                        String update = current + result + "\n";
                        headerTextComp.getDataStTxt().setText(update);
                    }
                }
            }
        });
    }
}
