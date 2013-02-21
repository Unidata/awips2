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

package com.raytheon.viz.hydro.pointdatacontrol;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.QueryMode;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDlg;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.RiverStat;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.util.RatingUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Tabular Display dialog for Point Data Control.
 * 
 * <pre>
 * It
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 05 Feb 2013  1578       rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TabularDisplayDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TabularDisplayDlg.class);

    /**
     * Font used by the list control.
     */
    private Font font;

    /**
     * Time Series graph button.
     */
    private Button timeSeriesGraphBtn;

    /**
     * Print button.
     */
    private Button printBtn;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Close button.
     */
    private Button closeBtn;

    /**
     * Time Series table button.
     */
    private Button timeSeriesTableBtn;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Is this being formatted for a file?
     */
    private boolean isFile = false;

    /**
     * The currently selected lid.
     */
    private String lid = null;

    /**
     * Buffer to hold data for printing or saving.
     */
    private StringBuilder dataBuffer = null;

    /**
     * List of data records displayed in table.
     */
    private java.util.List<GageData> recordList = new ArrayList<GageData>();

    /* Vars used in the printing methods */
    private Printer printer;

    private int lineHeight = 0;

    private int tabWidth = 0;

    private int leftMargin;

    private int rightMargin;

    private int topMargin;

    private int bottomMargin;

    private int x, y;

    private int index, end;

    private StringBuffer wordBuffer;

    private GC gc;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public TabularDisplayDlg(Shell parent) {
        super(parent);
        setText("Point Data Tabular Display");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        if ((font != null) && (font.isDisposed() == false)) {
            font.dispose();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createDataListControlLabels();
        createListBoxControl();
        createBottomButtons();

        getData();
    }

    /**
     * Create the labels above the data list controls.
     */
    private void createDataListControlLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        labelComp.setLayout(gl);
        Label label = new Label(labelComp, SWT.NONE);

        label.setFont(font);
        label.setText(String.format(
                "%-7s %-20s %s [%s %s] %-13s %-2s %-2s %4s %s [%s %s]",
                "Station", "Name", "Value", "Stg", "Flow", "Time", "PE", "TS",
                "Dur", "Extr", "Fld,", "Depart"));
    }

    /**
     * Create the data list control.
     */
    private void createListBoxControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 900;
        gd.heightHint = 600;
        dataList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        dataList.setLayoutData(gd);

        dataList.setFont(font);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        // -------------------------------
        // Top buttons
        // -------------------------------
        Composite topBtnComp = new Composite(shell, SWT.NONE);
        RowLayout topLayout = new RowLayout();
        topLayout.spacing = 30;
        topBtnComp.setLayout(topLayout);

        RowData rd = new RowData(150, SWT.DEFAULT);
        timeSeriesGraphBtn = new Button(topBtnComp, SWT.PUSH);
        timeSeriesGraphBtn.setText("Time Series Graph");
        timeSeriesGraphBtn.setLayoutData(rd);
        timeSeriesGraphBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GageData gageData = getSelection();
                TimeSeriesDlg.getInstance().updateAndOpen(gageData, true);
            }
        });

        rd = new RowData(80, SWT.DEFAULT);
        printBtn = new Button(topBtnComp, SWT.PUSH);
        printBtn.setText("Print");
        printBtn.setLayoutData(rd);
        printBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendTableToPrinter();
            }
        });

        // -------------------------------
        // Bottom buttons
        // -------------------------------
        Composite bottomBtnComp = new Composite(shell, SWT.NONE);
        RowLayout bottomLayout = new RowLayout();
        bottomLayout.spacing = 30;
        bottomBtnComp.setLayout(bottomLayout);

        rd = new RowData(150, SWT.DEFAULT);
        timeSeriesTableBtn = new Button(bottomBtnComp, SWT.PUSH);
        timeSeriesTableBtn.setText("Time Series Table");
        timeSeriesTableBtn.setLayoutData(rd);
        timeSeriesTableBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                GageData gageData = getSelection();
                TimeSeriesDlg.getInstance().updateAndOpen(gageData, false);
            }
        });

        rd = new RowData(80, SWT.DEFAULT);
        saveBtn = new Button(bottomBtnComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(rd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                isFile = true;
                saveTable();
            }
        });

        rd = new RowData(80, SWT.DEFAULT);
        closeBtn = new Button(bottomBtnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(rd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get the data to display.
     * 
     * @param isList
     *            true if data are displayed in the list, false if save or print
     * @return String[] of formatted data
     */
    private void getData() {
        PDCDataManager dataManager = PDCDataManager.getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        final String actionMsg = " >ACTION!! ";
        String actionText = "";
        double floodLevel = 0;
        double actionLevel = 0;
        double floodDepart = 0;
        String stageFlowText = "";
        String abbrevTime = "";
        double flow;
        double stage;

        if (pcOptions.getQueryMode() == QueryMode.AD_HOC_MODE.getQueryMode()) {
            pdcManager.scheduleRequest(true,
                    PointDataControlManager.REQUEST_TYPE.REQUEST_AD_HOC);
        } else {
            pdcManager.scheduleRequest(true,
                    PointDataControlManager.REQUEST_TYPE.REQUEST_TIME_STEP);
        }

        java.util.List<GageData> reportList = pdcManager.getObsReportList();

        Collections.sort(reportList);

        DateFormat tabDateFormat = new SimpleDateFormat("MM-dd HH:mm");
        tabDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        StringBuilder sb = new StringBuilder();
        dataBuffer = new StringBuilder();

        for (GageData gd : reportList) {
            if (gd.isUse()) {
                if ((gd.getValue() != PDCConstants.MISSING_VALUE)
                        && (pcOptions.getTimeMode() != PDCConstants.TimeModeType.VALUE_CHANGE
                                .getTimeMode())) {

                    /*
                     * get the riverstat info if a river station and use it to
                     * set certain fields
                     */
                    RiverStat rsInfo = dataManager.getRiverStatus(gd.getLid());
                    if (rsInfo != null) {
                        if (gd.getPe().startsWith("H")) {
                            if ((rsInfo.getFs() != PDCConstants.MISSING_VALUE)
                                    && (rsInfo.getAs() != 0)) {
                                floodLevel = rsInfo.getFs();
                            } else {
                                floodLevel = PDCConstants.MISSING_VALUE;
                            }

                            if ((rsInfo.getAs() != PDCConstants.MISSING_VALUE)
                                    && (rsInfo.getAs() != 0)) {
                                actionLevel = rsInfo.getAs();
                            } else {
                                actionLevel = PDCConstants.MISSING_VALUE;
                            }
                        } else if (gd.getPe().startsWith("Q")) {
                            if ((rsInfo.getFq() != PDCConstants.MISSING_VALUE)
                                    && (rsInfo.getAs() != 0)) {
                                floodLevel = rsInfo.getFq();
                            } else {
                                floodLevel = PDCConstants.MISSING_VALUE;
                            }
                            if ((rsInfo.getAq() != PDCConstants.MISSING_VALUE)
                                    && (rsInfo.getAs() != 0)) {
                                actionLevel = rsInfo.getAq();
                            } else {
                                actionLevel = PDCConstants.MISSING_VALUE;
                            }
                        }
                    }

                    if ((actionLevel != PDCConstants.MISSING_VALUE)
                            && (actionLevel != 0)) {
                        if (gd.getValue() >= actionLevel) {
                            actionText = actionMsg;
                        }
                    }

                    if ((floodLevel != PDCConstants.MISSING_VALUE)
                            && (actionLevel != 0)) {
                        floodDepart = gd.getValue() - floodLevel;
                    }

                    /*
                     * Check if the PE is of type "HG" or "QR". If it is, then
                     * attempt to derive a flow value if the PE is "HG" or a
                     * stage value if the PE is "QR".
                     */
                    if (gd.getPe().equalsIgnoreCase("HG")
                            || gd.getPe().equalsIgnoreCase("QR")) {
                        if (gd.getPe().equalsIgnoreCase("HG")) {
                            flow = RatingUtils.stage2discharge(gd.getLid(),
                                    gd.getValue());

                            if (flow != PDCConstants.RATING_CONVERT_FAILED) {
                                stageFlowText = String
                                        .format("[%#-6.0f]", flow);
                            }
                        } else if (gd.getPe().equalsIgnoreCase("QR")) {
                            stage = RatingUtils.discharge2stage(gd.getLid(),
                                    gd.getValue());

                            if (stage != PDCConstants.RATING_CONVERT_FAILED) {
                                stageFlowText = String
                                        .format("[%-6.2f]", stage);
                            }
                        } else {
                            // TODO Log message
                            // fprintf ( stderr ,
                            // "\nIn routine \"load_pointtable\":\n"
                            // "Error in switch statement which attempts\n"
                            // "to find a derived stage/flow value.\n"
                            // "Value %c is invalid.\n" , rPtr->pe [ 0 ] ) ;
                            stageFlowText = "M";
                        }
                    }
                }

                recordList.add(gd);

                /* format the time */
                if (gd.getValidtime() != null) {
                    abbrevTime = tabDateFormat.format(gd.getValidtime());
                }

                /* Shrink the name to fit */
                String name = gd.getName();
                if (name == null) {
                    name = "";
                } else if (isFile) {
                    if (name.length() > 20) {
                        name = name.substring(0, 20);
                    }
                } else {
                    if (name.length() > 15) {
                        name = name.substring(0, 15);
                    }
                }

                /* write the information */
                if (gd.getValue() == PDCConstants.MISSING_VALUE) {
                    if (isFile) {
                        sb.append(String.format("%-7s %-15s %-6s ",
                                gd.getLid(), name, "m"));
                    } else {
                        sb.append(String.format("%-7s %-20s %-6s ",
                                gd.getLid(), name, "m"));
                    }
                } else {
                    if ((floodLevel != PDCConstants.MISSING_VALUE)
                            && (pcOptions.getTimeMode() != PDCConstants.TimeModeType.VALUE_CHANGE
                                    .getTimeMode())) {
                        if (isFile) {
                            sb.append(String
                                    .format("%-7s %-15s %-6.2f %-8s %-13s %-2s %-2s %4s %4s [%6.1f %5.1f]",
                                            gd.getLid(), name, gd.getValue(),
                                            stageFlowText, abbrevTime,
                                            gd.getPe(), gd.getTs(),
                                            gd.getDur(), gd.getExtremum(),
                                            floodLevel, floodDepart));

                            if (actionText.length() > 0) {
                                sb.append(actionText + "\n");
                            }
                        } else {
                            sb.append(String
                                    .format("%-7s %-20s %-6.2f %-8s %s %-13s %-2s %-2s %4s %4s [%6.1f %5.1f]",
                                            gd.getLid(), name, gd.getValue(),
                                            stageFlowText, actionText,
                                            abbrevTime, gd.getPe(), gd.getTs(),
                                            gd.getDur(), gd.getExtremum(),
                                            floodLevel, floodDepart));
                        }
                    } else {
                        if (isFile) {
                            sb.append(String
                                    .format("%-7s %-15s %-6.2f %-8s %-13s %-2s %-2s %4s %4s",
                                            gd.getLid(), name, gd.getValue(),
                                            stageFlowText, abbrevTime,
                                            gd.getPe(), gd.getTs(),
                                            gd.getDur(), gd.getExtremum()));

                            if (actionText.length() > 0) {
                                sb.append(actionText + "\n");
                            }
                        } else {
                            sb.append(String
                                    .format("%-7s %-20s %-6.2f %-8s %s %-13s %-2s %-2s %4s %4s",
                                            gd.getLid(), name, gd.getValue(),
                                            stageFlowText, "", abbrevTime,
                                            gd.getPe(), gd.getTs(),
                                            gd.getDur(), gd.getExtremum()));
                        }
                    }
                }
            }
            if (sb.length() > 0) {
                dataList.add(sb.toString());
                dataBuffer.append(sb.toString() + "\n");
            }

            sb.setLength(0);
            /* Reset the variables */
            actionText = "";
            floodLevel = 0;
            actionLevel = 0;
            floodDepart = 0;
            stageFlowText = "";
            abbrevTime = "";
        }
    }

    /**
     * Handle the print table selection
     */
    private void sendTableToPrinter() {
        final String text = dataBuffer.toString();
        if (text != null) {
            PrintDialog dialog = new PrintDialog(shell, SWT.NONE);
            PrinterData data = dialog.open();

            if (data == null) {
                return;
            }

            printer = new Printer(data);

            /*
             * Do the printing in a background thread so that spooling does not
             * freeze the UI.
             */
            Thread printingThread = new Thread("PrintTable") {
                @Override
                public void run() {
                    print(printer, text);
                    printer.dispose();
                }
            };
            printingThread.start();
        }
    }

    /**
     * Send the text to the printer
     * 
     * @param printer
     *            The printer
     * @param text
     *            The text to print
     */
    private void print(Printer printer, String text) {
        if (printer.startJob("Text")) {
            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);
            Point dpi = printer.getDPI();

            // one inch from left side of paper
            leftMargin = dpi.x + trim.x;

            // one inch from right side of paper
            rightMargin = clientArea.width - dpi.x + trim.x + trim.width;

            // one inch from top edge of paper
            topMargin = dpi.y + trim.y;

            // one inch from bottom edge of paper
            bottomMargin = clientArea.height - dpi.y + trim.y + trim.height;

            /* Create a buffer for computing tab width. */
            int tabSize = 4; // is tab width a user setting in your UI?
            StringBuffer tabBuffer = new StringBuffer(tabSize);
            for (int i = 0; i < tabSize; i++) {
                tabBuffer.append(' ');
            }
            String tabs = tabBuffer.toString();

            /*
             * Create printer GC, and create and set the printer font &
             * foreground color.
             */
            gc = new GC(printer);

            Font printerFont = new Font(printer, "Monospace", 8, SWT.NORMAL);

            Color printerForegroundColor = new Color(printer, new RGB(0, 0, 0));
            Color printerBackgroundColor = new Color(printer, new RGB(255, 255,
                    255));

            gc.setFont(printerFont);
            gc.setForeground(printerForegroundColor);
            gc.setBackground(printerBackgroundColor);
            tabWidth = gc.stringExtent(tabs).x;
            lineHeight = gc.getFontMetrics().getHeight();

            /* Print text to current gc using word wrap */
            printText(text);

            printer.endJob();

            /* Cleanup graphics resources used in printing */
            printerFont.dispose();
            printerForegroundColor.dispose();
            printerBackgroundColor.dispose();
            gc.dispose();
        }
    }

    /**
     * Print the text
     * 
     * @param text
     *            The text to be printed
     */
    private void printText(String text) {
        printer.startPage();
        wordBuffer = new StringBuffer();
        x = leftMargin;
        y = topMargin;
        index = 0;
        end = text.length();
        while (index < end) {
            char c = text.charAt(index);
            index++;
            if (c != 0) {
                if ((c == 0x0a) || (c == 0x0d)) {
                    if ((c == 0x0d) && (index < end)
                            && (text.charAt(index) == 0x0a)) {
                        index++; // if this is cr-lf, skip the lf
                    }
                    printWordBuffer();
                    newline();
                } else {
                    if (c != '\t') {
                        wordBuffer.append(c);
                    }
                    if (Character.isWhitespace(c)) {
                        printWordBuffer();
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
    }

    /**
     * Word buffer for formating lines on the printed page
     */
    private void printWordBuffer() {
        if (wordBuffer.length() > 0) {
            String word = wordBuffer.toString();
            int wordWidth = gc.stringExtent(word).x;
            if (x + wordWidth > rightMargin) {
                /* word doesn't fit on current line, so wrap */
                newline();
            }
            gc.drawString(word, x, y, false);
            x += wordWidth;
            wordBuffer = new StringBuffer();
        }
    }

    /**
     * New line on the printed page
     */
    private void newline() {
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

    /**
     * Get the selection from the dataList.
     */
    private GageData getSelection() {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        java.util.List<GageData> reportList = pdcManager.getObsReportList();
        String pe = null;

        String[] array = dataList.getSelection();
        if (array.length != 1) {
            MessageDialog.openWarning(shell, "Invalid Selection",
                    "A single Station must be selected");

            return null;
        }
        String[] parts = array[0].split(" ");
        lid = parts[0];

        displayManager.setCurrentData(lid);

        return recordList.get(dataList.getSelectionIndex());
    }

    /**
     * Save the table data in a text file
     */
    private void saveTable() {
        String text = dataBuffer.toString();
        FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        String filename = dialog.open();
        if (filename == null) {
            return;
        }

        try {
            BufferedWriter out = new BufferedWriter(new FileWriter(filename));
            out.write(text);
            out.close();
        } catch (IOException e) {
            e.printStackTrace();
            statusHandler.handle(Priority.PROBLEM, "saveTable()"
                    + " Error saving the tabluar data to file");
        }
    }
}
