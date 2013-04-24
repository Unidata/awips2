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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.utility.ImageUtil;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Ceiling & Visibility Trend dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 4/10/2008    934        grichard    Populated site lists with icaos.
 * 8/28/2008    1506       grichard    Added resizeability to dialog shell. 
 * 9/12/2008    1444       grichard    Accommodate separate message logs.
 * 3/31/2011    8774       rferrel     killProcess when doing a disposed
 * 4/14/2011    8861       rferrel     Use SaveImageDlg class
 * 10/09/2912   1229       rferrel     Made non-blocking
 * 10/15/2012   1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CigVisTrendDlg extends CaveSWTDialog {

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
     * Hours combo box.
     */
    private Combo hoursCbo;

    /**
     * Yes radio button.
     */
    private Button yesRdo;

    /**
     * No radio button.
     */
    private Button noRdo;

    /**
     * Either radio button.
     */
    private Button eitherRdo;

    /**
     * Draw button
     */
    private Button drawBtn;

    private Cursor waitCursor;

    private Cursor defaultCursor;

    /**
     * Font used in text controls.
     */
    private Font textFont;

    /**
     * Decode styled text control.
     */
    private StyledText decodeST;

    /**
     * The ICAOs to use to populate the station list.
     */
    private java.util.List<String> icaos;

    /**
     * Graph data.
     */
    private TrendCigVisGraphData graphData;

    /**
     * Ceiling & Visibility Trend drawing canvas.
     */
    private TrendCigVisCanvasComp trendCigVisCanvas;

    /**
     * Date canvas composite
     */
    private TrendDateCanvasComp trendDateComp;

    /**
     * Hour canvas composite.
     */
    private TrendHourCanvasComp trendHourComp;

    /**
     * Wind Direction canvas composite
     */
    private TrendWindDirCanvasComp trendWindDirComp;

    /**
     * Wind Speed canvas composite
     */
    private TrendWindSpdCanvasComp trendWindSpdComp;

    /**
     * Ceiling canvas composite
     */
    private TrendCeilingCanvasComp trendCigComp;

    /**
     * Visibility canvas composite
     */
    private TrendVisCanvasComp trendVisComp;

    /**
     * Status message type.
     */
    private StatusMessageType msgType;

    /**
     * Status message composite.
     */
    private MessageStatusComp statusMessageComp;

    /**
     * Message status comp background color.
     */
    private RGB statusCompRGB;

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
    public CigVisTrendDlg(Shell parent, java.util.List<String> icaos,
            StatusMessageType msgType, RGB statusCompRGB) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS - Ceiling/Visibility Trend");

        this.icaos = icaos;
        this.msgType = msgType;
        this.statusCompRGB = statusCompRGB;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        textFont.dispose();
        CigVisTrendDataManager.getInstance().killProcess();
        super.disposed();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        textFont = new Font(getDisplay(), "Monospace", 10, SWT.NORMAL);
        graphData = new TrendCigVisGraphData();

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        // Create the main Wind Rose controls and drawing canvas.
        createMainControls();

        // Create the controls that will display the incoming messages
        createBottomMessageControls();

        populateStations(icaos);
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
                    String helpText = "This application displays ceiling/visibility trend based\non selected initial conditions.\n\nUse \"Get\" button to retrieve METAR for a selected site.\nThe METAR can be modified.\nUse \"Decode\" button to initialize selection widgets.\nThe initial conditions can be adjusted either by typing\nin the \"value\" and \"range\" windows, or by mouse actions.\nLeft button moves value or an edge of range (red area on\nthe element widget). Middle button is used to move both\nvalue and range. In the \"Wind Direction\" widget use right\nbutton to toggle between wind arrow and a circle representing\ncalm and variable wind.\nUse \"Element\" radiobuttons to select forecasted element.\nPress \"Draw\" to display the forecast.\n\nThe displayed image can be printed or stored in a graphic file.\nUse \"File\" menu for that purpose.";
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

        createCanvasControls(mainComp);

        createGetDecodeTextControls(mainComp);

        createTrendCigVisCanvas(mainComp);
    }

    /**
     * Create the Site controls.
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
        gd.heightHint = 185;
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
                dataReceived();
            }
        });

        ceilingRdo = new Button(elementGroup, SWT.RADIO);
        ceilingRdo.setText("Ceiling");
        ceilingRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                dataReceived();
            }
        });

        jointRdo = new Button(elementGroup, SWT.RADIO);
        jointRdo.setText("Joint");
        jointRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                dataReceived();
            }
        });

        // ----------------------------------------------
        // Create the Hours combo box
        // ----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite hoursComp = new Composite(siteComp, SWT.NONE);
        hoursComp.setLayout(new GridLayout(2, false));
        hoursComp.setLayoutData(gd);

        Label hoursLbl = new Label(hoursComp, SWT.NONE);
        hoursLbl.setText("Hours:");

        hoursCbo = new Combo(hoursComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        hoursCbo.add("3");
        hoursCbo.add("6");
        hoursCbo.add("12");
        hoursCbo.select(0);
        hoursCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dataReceived();
            }
        });

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
                retrieveData();
            }
        });
    }

    /**
     * Create all of the canvas/control composites on the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createCanvasControls(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite canvasComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        canvasComp.setLayout(gl);
        canvasComp.setLayoutData(gd);

        trendDateComp = new TrendDateCanvasComp(canvasComp);

        trendHourComp = new TrendHourCanvasComp(canvasComp);

        trendWindDirComp = new TrendWindDirCanvasComp(canvasComp);

        trendWindSpdComp = new TrendWindSpdCanvasComp(canvasComp);

        trendCigComp = new TrendCeilingCanvasComp(canvasComp);

        trendVisComp = new TrendVisCanvasComp(canvasComp);

        // ------------------------------------------------
        // Create the Precip controls
        // ------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite precipComp = new Composite(canvasComp, SWT.NONE);
        precipComp.setLayout(new GridLayout(4, false));
        precipComp.setLayoutData(gd);

        Label precipLbl = new Label(precipComp, SWT.NONE);
        precipLbl.setText("Precip: ");

        yesRdo = new Button(precipComp, SWT.RADIO);
        yesRdo.setText("Yes");

        noRdo = new Button(precipComp, SWT.RADIO);
        noRdo.setText("No");

        eitherRdo = new Button(precipComp, SWT.RADIO);
        eitherRdo.setText("Either");
        eitherRdo.setSelection(true);
    }

    /**
     * Create the Get and Decode buttons and text control.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createGetDecodeTextControls(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite decodeComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        decodeComp.setLayout(gl);
        decodeComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button getBtn = new Button(decodeComp, SWT.NONE);
        getBtn.setText("Get");
        getBtn.setLayoutData(gd);
        getBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (siteList != null && siteList.getItemCount() != 0) {
                    CigVisTrendDataManager dataMgr = CigVisTrendDataManager
                            .getInstance();
                    dataMgr.getMetar(siteList.getItem(siteList
                            .getSelectionIndex()));
                    decodeST.setText(dataMgr.getMetarText());
                }
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button decodeBtn = new Button(decodeComp, SWT.NONE);
        decodeBtn.setText("Decode");
        decodeBtn.setLayoutData(gd);
        decodeBtn.addSelectionListener(new SelectionAdapter() {
            @SuppressWarnings("unchecked")
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (siteList != null && siteList.getItemCount() != 0) {
                    CigVisTrendDataManager dataMgr = CigVisTrendDataManager
                            .getInstance();

                    if (dataMgr.hasRetrievedMetar()) {
                        Map<String, Object> metarDcd = dataMgr.getMetarDcd();

                        // itime
                        Map<String, Object> tmp = (HashMap<String, Object>) metarDcd
                                .get("itime");
                        long itime = (long) ((Float) tmp.get("value") * 1000);
                        Calendar tms = Calendar.getInstance(TimeZone
                                .getTimeZone("GMT"));
                        tms.setTimeInMillis(itime);
                        int hour = tms.get(Calendar.HOUR_OF_DAY);
                        int minutes = tms.get(Calendar.MINUTE);
                        int h0 = hour - 1;
                        int h1 = hour + 1;
                        trendHourComp.setValueText(hour * 15
                                + Math.round(minutes / 4.0));
                        trendHourComp.validateValueInputs();
                        trendHourComp.setRangeText(
                                h0 * 15 + Math.round(minutes / 4.0), h1 * 15
                                        + Math.round(minutes / 4.0));
                        trendHourComp.validateRangeInputs();

                        int yday = tms.get(Calendar.DAY_OF_YEAR);
                        int yday0 = yday - 30;
                        int yday1 = yday + 30;
                        trendDateComp.setValueText(yday);
                        trendDateComp.validateValueInputs();
                        trendDateComp.setRangeText(yday0, yday1);
                        trendDateComp.validateRangeInputs();

                        // vsby
                        tmp = (Map<String, Object>) metarDcd.get("vsby");
                        double vsby = (Float) tmp.get("value");
                        String[] range = trendVisComp.getRange().split("-");
                        double vis0 = Double.parseDouble(range[0]);
                        double vis1;

                        if (range[1].endsWith("+")) {
                            vis1 = 20;
                        } else {
                            vis1 = Double.parseDouble(range[1]);
                        }

                        if (vis0 > vsby || vsby >= vis1) {
                            double[] visArray = { 0.0, 0.4, 1.1, 3.1, 6.1,
                                    Integer.MAX_VALUE };

                            for (int i = 1; i < visArray.length; i++) {
                                vis0 = visArray[i - 1];
                                vis1 = visArray[i];

                                if (vis0 <= vsby && vsby < vis1) {
                                    break;
                                }
                            }
                        }

                        trendVisComp.setValueText(vsby);
                        trendVisComp.setRangeText(vis0, vis1);

                        // sky
                        tmp = (Map<String, Object>) metarDcd.get("sky");
                        int cig = (Integer) tmp.get("cig");
                        range = trendCigComp.getRange().split("-");
                        int cig0;
                        int cig1;

                        if (range[0].endsWith("+")) {
                            cig0 = Integer.parseInt(range[0].substring(0,
                                    (range[0].length() - 1)));
                        } else {
                            cig0 = Integer.parseInt(range[0]);
                        }

                        if (range[1].equals("UNL")) {
                            cig1 = 99999;
                        } else if (range[1].endsWith("+")) {
                            cig1 = 25000;
                        } else {
                            cig1 = Integer.parseInt(range[1]);
                        }

                        if (cig0 > cig || cig >= cig1) {
                            int[] cigArray = { 0, 210, 610, 1020, 3130, 5050,
                                    Integer.MAX_VALUE };

                            for (int i = 1; i < cigArray.length; i++) {
                                cig0 = cigArray[i - 1];
                                cig1 = cigArray[i];

                                if (cig0 <= cig && cig < cig1) {
                                    break;
                                }
                            }
                        }

                        trendCigComp.setValueText((double) cig);
                        trendCigComp.setRangeText((double) cig0, (double) cig1);

                        // wind
                        tmp = (Map<String, Object>) metarDcd.get("wind");
                        Object ddObj = tmp.get("dd");
                        String dd = "";

                        if (ddObj instanceof java.lang.String) {
                            dd = (String) ddObj;
                        } else {
                            dd = Integer.toString((Integer) ddObj);
                        }

                        int dd0;
                        int dd1;

                        if (dd.equals("0") || dd.equals("VRB")) {
                            dd = "CALM";
                        }

                        if (dd.equals("CALM")) {
                            trendWindDirComp.setCalm();
                        } else {
                            dd0 = Integer.parseInt(dd) - 30;

                            if (dd0 < 1) {
                                dd0 += 360;
                            }

                            dd1 = Integer.parseInt(dd) + 30;

                            if (dd1 > 360) {
                                dd1 -= 360;
                            }

                            trendWindDirComp.setValueText(Integer.parseInt(dd));
                            trendWindDirComp.validateValueInputs();
                            trendWindDirComp.setRangeText(dd0, dd1);
                            trendWindDirComp.validateRangeInputs();
                        }

                        int ff = (Integer) tmp.get("ff");
                        range = trendWindSpdComp.getRange().split("-");
                        double ff0 = Double.parseDouble(range[0]);
                        double ff1;

                        if (range[1].endsWith("+")) {
                            ff1 = 50.0;
                        } else {
                            ff1 = Double.parseDouble(range[1]);
                        }

                        if (ff0 > ff || ff >= ff1) {
                            double[] ffArray = { 0, 5, 12, 32,
                                    Integer.MAX_VALUE };

                            for (int i = 0; i < (ffArray.length - 1); i++) {
                                ff0 = ffArray[i];
                                ff1 = ffArray[i + 1];

                                if (ff0 <= ff && ff < ff1) {
                                    break;
                                }
                            }
                        }

                        trendWindSpdComp.setValueText((double) ff);
                        trendWindSpdComp.setRangeText(ff0, ff1);

                        // pcp
                        tmp = (Map<String, Object>) metarDcd.get("pcp");
                        boolean pcp = false;

                        if (tmp != null) {
                            String pcpStr = (String) tmp.get("str");

                            if (pcpStr != null) {
                                pcp = true;
                            }
                        }

                        if (pcp) {
                            yesRdo.setSelection(true);
                            noRdo.setSelection(false);
                            eitherRdo.setSelection(false);
                        } else {
                            yesRdo.setSelection(false);
                            noRdo.setSelection(true);
                            eitherRdo.setSelection(false);
                        }
                    }
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 50;
        decodeST = new StyledText(decodeComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        decodeST.setWordWrap(true);
        decodeST.setFont(textFont);
        decodeST.setEditable(false);
        decodeST.setLayoutData(gd);
    }

    /**
     * Create the Ceiling & Visibility drawing canvas composite.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createTrendCigVisCanvas(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite canvasComp = new Composite(parentComp, SWT.NONE);
        canvasComp.setLayout(new GridLayout(1, false));
        canvasComp.setLayoutData(gd);

        trendCigVisCanvas = new TrendCigVisCanvasComp(canvasComp, null);
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        statusMessageComp = new MessageStatusComp(shell, msgType,
                statusCompRGB, null);
    }

    /**
     * Close the display.
     */
    public void closeDisplay() {
        shell.dispose();
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
     * Draw the data graph.
     */
    private void drawGraph() {
        if (graphData == null) {
            graphData = new TrendCigVisGraphData();
        }

        graphData.setNumberOfHours(Integer.valueOf(
                hoursCbo.getItem(hoursCbo.getSelectionIndex())).intValue());

        int[] hoursMinutes = trendHourComp.getHoursAndMinutes();

        if (hoursMinutes[1] > 50) {
            if (hoursMinutes[0] == 23) {
                graphData.setStartingHour(0);
            } else {
                graphData.setStartingHour(hoursMinutes[0] + 1);
            }
        } else {
            graphData.setStartingHour(hoursMinutes[0]);
        }

        if (visibilityRdo.getSelection() == true) {
            graphData
                    .setSelectedElement(TrendCigVisGraphData.trendElement.Visibility);
        } else if (ceilingRdo.getSelection() == true) {
            graphData
                    .setSelectedElement(TrendCigVisGraphData.trendElement.Ceiling);
        } else {
            graphData
                    .setSelectedElement(TrendCigVisGraphData.trendElement.Joint);
        }

        graphData.setSite(CigVisTrendDataManager.getInstance().getDataSite());

        trendCigVisCanvas.updateDataAndRedraw(graphData.cloneData());
    }

    private void retrieveData() {
        if (siteList != null && siteList.getItemCount() != 0) {
            statusMessageComp.setMessageText(
                    "Retrieving data for "
                            + siteList.getItem(siteList.getSelectionIndex())
                            + ", will take a while.", getDisplay()
                            .getSystemColor(SWT.COLOR_YELLOW).getRGB());
            CigVisTrendDataManager dataMgr = CigVisTrendDataManager
                    .getInstance();
            Map<String, Object> selectionMap = new HashMap<String, Object>();
            java.util.List<Float> list;

            // cig
            list = new ArrayList<Float>();
            String[] cigRange = trendCigComp.getCigRange();
            list.add(Float.valueOf(cigRange[0]));
            list.add(Float.valueOf(cigRange[1]));
            selectionMap.put("cig", list);

            // vsby
            list = new ArrayList<Float>();
            String[] vsbyRange = trendVisComp.getVsbyRange();
            list.add(Float.valueOf(vsbyRange[0]));
            list.add(Float.valueOf(vsbyRange[1]));
            selectionMap.put("vsby", list);

            // wind_speed
            list = new ArrayList<Float>();
            String[] windSpdRange = trendWindSpdComp.getWindSpdRange();
            list.add(Float.valueOf(windSpdRange[0]));
            list.add(Float.valueOf(windSpdRange[1]));
            selectionMap.put("wind_speed", list);

            // wind_dir
            list = new ArrayList<Float>();
            String[] windDirRange = trendWindDirComp.getWindDirRange();
            list.add(Float.valueOf(windDirRange[0]));
            list.add(Float.valueOf(windDirRange[1]));
            selectionMap.put("wind_dir", list);

            // hour
            list = new ArrayList<Float>();
            String[] hourRange = trendHourComp.getHourRange();
            list.add(Float.valueOf(hourRange[0]));
            list.add(Float.valueOf(hourRange[1]));
            selectionMap.put("hour", list);

            // day
            list = new ArrayList<Float>();
            String[] dayRange = trendDateComp.getDayRange();
            list.add(Float.valueOf(dayRange[0]));
            list.add(Float.valueOf(dayRange[1]));
            selectionMap.put("yday", list);

            // pcp
            String pcp;
            if (yesRdo.getSelection()) {
                pcp = "yes";
            } else if (noRdo.getSelection()) {
                pcp = "no";
            } else {
                pcp = "either";
            }
            selectionMap.put("pcp", pcp);

            // cur_hour
            int[] time = trendHourComp.getHoursAndMinutes();
            int hour = time[0];
            if (time[1] > 30) {
                hour++;
            }
            if (hour >= 24) {
                hour -= 24;
            }
            selectionMap.put("cur_hour", hour);

            if (waitCursor == null) {
                waitCursor = new Cursor(shell.getDisplay(), SWT.CURSOR_WAIT);
                defaultCursor = shell.getCursor();
            }
            shell.setCursor(waitCursor);
            drawBtn.setEnabled(false);
            // Always get 12 hours worth
            dataMgr.getData(siteList.getItem(siteList.getSelectionIndex()),
                    selectionMap, 12, this);
        }
    }

    public void resetCursor() {
        drawBtn.setEnabled(true);
        shell.setCursor(defaultCursor);
    }

    public void dataReceived() {
        if (isDisposed() == true) {
            return;
        }
        CigVisTrendDataManager dataMgr = CigVisTrendDataManager.getInstance();
        int hours = Integer.valueOf(
                hoursCbo.getItem(hoursCbo.getSelectionIndex())).intValue() + 1;
        float[][] data = new float[5][hours];
        float[][] tempData = null;

        if (visibilityRdo.getSelection()) {
            tempData = dataMgr.getVisData();
        } else if (ceilingRdo.getSelection()) {
            tempData = dataMgr.getCigData();
        } else {
            tempData = dataMgr.getJointData();
        }

        if (tempData != null) {
            for (int i = 0; i < hours; i++) {
                for (int j = 0; j < 4; j++) {
                    data[j][i] = tempData[i][j];
                }
            }

            data[4] = dataMgr.getTotalData();

            graphData.setDataArray(data);

            drawGraph();
        }
    }

    /**
     * Save the CigVisTrend diagram to file.
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
        Image image = trendCigVisCanvas.getCigVisTrendImage();

        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { image.getImageData() };
        loader.save(filename, style);
    }

    private void printImage() {
        PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
        PrinterData printerData = dialog.open();

        if (printerData != null) {
            // Create the printer object
            Printer printer = new Printer(printerData);
            printer.startJob("cigvistrendjob");
            GC gc = new GC(printer);

            if (printer.startPage()) {
                Image image = trendCigVisCanvas.getCigVisTrendImage();
                image = new Image(gc.getDevice(), image.getImageData()
                        .scaledTo(printer.getBounds().height,
                                printer.getBounds().width));
                // rotate the image
                image = ImageUtil.rotateImage(image);
                gc.drawImage(image, 0, 0);
                printer.endPage();
            }

            gc.dispose();
            printer.endJob();
            printer.dispose();
        }
    }
}
