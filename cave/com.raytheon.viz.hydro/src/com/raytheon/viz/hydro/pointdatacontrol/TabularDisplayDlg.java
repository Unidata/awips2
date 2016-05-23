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

import java.io.IOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.common.util.StringUtil;
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
 * 10 May 2016  5483       dgilling    Code cleanup, fix layout on hi-dpi systems.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TabularDisplayDlg extends CaveSWTDialog {

    private class SaveTabularDataJob extends Job {

        private final Path outputFile;

        private final Collection<GageData> records;

        protected SaveTabularDataJob(Path outputFile,
                Collection<GageData> records) {
            super("Save point data tabular data Job");
            setSystem(true);
            this.outputFile = outputFile;
            this.records = records;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            Collection<Pair<GageData, List<String>>> formattedRecords = formatRecords(
                    records, true);

            try (Writer out = Files.newBufferedWriter(outputFile,
                    StandardCharsets.UTF_8)) {
                for (Pair<GageData, List<String>> record : formattedRecords) {
                    out.write(StringUtil.join(record.getSecond(), ' '));
                    out.write(System.getProperty("line.separator"));
                }
            } catch (IOException e) {
                statusHandler.error("saveTable()"
                        + " Error saving the tabluar data to file", e);
            }

            return Status.OK_STATUS;
        }
    }

    private class PrintTabularDataJob extends Job {

        /** The connection to the printer. */
        private final Printer printer;

        private final Collection<GageData> records;

        /** Line height for printing. */
        private int lineHeight = 0;

        /** Tab width for printing. */
        private int tabWidth = 0;

        /** Printer's left margin. */
        private int leftMargin;

        /** Printer's right margin. */
        private int rightMargin;

        /** Printer's top margin. */
        private int topMargin;

        /** Printer's bottom margin. */
        private int bottomMargin;

        /** Printer's current horizontal location. */
        private int x;

        /** Printer's current vertical location. */
        private int y;

        /**
         * Index into the text of the current character being processed for
         * printing.
         */
        private int index;

        /** Length of the text being printed. */
        private int end;

        /** The currently line to send to the printer. */
        private StringBuilder wordBuffer;

        /** Used to draw the characters for the printer. */
        private GC gc;

        protected PrintTabularDataJob(Printer printer,
                Collection<GageData> records) {
            super("Print point data tabular data Job");
            setSystem(true);
            this.printer = printer;
            this.records = records;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            Collection<Pair<GageData, List<String>>> formattedRecords = formatRecords(
                    records, false);

            StringBuilder sb = new StringBuilder();
            for (Pair<GageData, List<String>> record : formattedRecords) {
                for (String column : record.getSecond()) {
                    sb.append(column);
                    sb.append(' ');
                }
                sb.append('\n');
            }

            try {
                print(printer, sb.toString());
            } finally {
                printer.dispose();
                if (gc != null) {
                    gc.dispose();
                }
            }
            return Status.OK_STATUS;
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
                Font printerFont = null;
                Color printerForegroundColor = null;
                Color printerBackgroundColor = null;

                try {
                    /*
                     * Get the bounds of the paper for computing margins
                     */
                    Rectangle bounds = printer.getBounds();
                    Point dpi = printer.getDPI();

                    // one inch from left side of paper
                    leftMargin = dpi.x + bounds.x;

                    // one inch from right side of paper
                    rightMargin = (bounds.x + bounds.width) - dpi.x;

                    // one inch from top edge of paper
                    topMargin = dpi.y + bounds.y;

                    // one inch from bottom edge of paper
                    bottomMargin = (bounds.y + bounds.height) - dpi.y;

                    /* Create a buffer for computing tab width. */
                    int tabSize = 4; // is tab width a user setting in your UI?
                    StringBuilder tabBuffer = new StringBuilder(tabSize);
                    for (int i = 0; i < tabSize; i++) {
                        tabBuffer.append(' ');
                    }
                    String tabs = tabBuffer.toString();

                    /*
                     * Create printer GC, and create and set the printer font &
                     * foreground color.
                     */
                    gc = new GC(printer);

                    /*
                     * Get the minimum margins (unprintable area) of the page
                     */
                    Rectangle trim = printer.computeTrim(0, 0, 0, 0);

                    /*
                     * Translate origin of the GC from upper left of the
                     * printable area to upper left of the paper since margins
                     * were calculated relative to the edge of the paper.
                     */
                    Transform transform = new Transform(gc.getDevice());
                    transform.translate(trim.x, trim.y);
                    gc.setTransform(transform);

                    printerFont = new Font(printer, "Monospace", 8, SWT.NORMAL);

                    printerForegroundColor = new Color(printer,
                            new RGB(0, 0, 0));
                    printerBackgroundColor = new Color(printer, new RGB(255,
                            255, 255));

                    gc.setFont(printerFont);
                    gc.setForeground(printerForegroundColor);
                    gc.setBackground(printerBackgroundColor);
                    tabWidth = gc.stringExtent(tabs).x;
                    lineHeight = gc.getFontMetrics().getHeight();

                    /* Print text to current gc using word wrap */
                    printText(text);

                    printer.endJob();
                } finally {
                    /* Cleanup graphics resources used in printing */
                    if (printerFont != null) {
                        printerFont.dispose();
                    }
                    if (printerForegroundColor != null) {
                        printerForegroundColor.dispose();
                    }
                    if (printerBackgroundColor != null) {
                        printerBackgroundColor.dispose();
                    }
                }
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
            wordBuffer = new StringBuilder();
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
                wordBuffer = new StringBuilder();
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
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TabularDisplayDlg.class);

    private static final String ACTION_MSG = " >ACTION!! ";

    private static final DateFormat DATE_FORMAT = new SimpleDateFormat(
            "MM-dd HH:mm") {
        {
            setTimeZone(TimeUtil.GMT_TIME_ZONE);
        }
    };

    private static final String INVALID_PE_ERROR_FMT = "In routine \"formatRecords\":"
            + "Error in switch statement which attempts"
            + "to find a derived stage/flow value." + " Value %s is invalid.";

    /**
     * Data list control.
     */
    private Table dataList;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public TabularDisplayDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Point Data Tabular Display");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        createDataListControl();
        createBottomButtons();

        Collection<Pair<GageData, List<String>>> formattedRecords = formatRecords(
                getData(), false);
        populateDataList(formattedRecords);
    }

    /**
     * Create the data list control.
     */
    private void createDataListControl() {
        dataList = new Table(shell, SWT.BORDER | SWT.SINGLE
                | SWT.FULL_SELECTION | SWT.V_SCROLL | SWT.H_SCROLL);
        dataList.setHeaderVisible(true);
        dataList.setLinesVisible(false);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.heightHint = dataList.getHeaderHeight()
                + (dataList.getItemHeight() * 29);
        dataList.setLayoutData(gd);

        String[] headerTitles = { "Station", "Name", "Value", "[Stg Flow]", "",
                "Time", "PE", "TS", "Dur", "Extr", "[Fld, Depart]" };
        for (String header : headerTitles) {
            TableColumn column = new TableColumn(dataList, SWT.DEFAULT);
            column.setText(header);
        }

        for (TableColumn column : dataList.getColumns()) {
            column.pack();
        }
    }

    private void populateDataList(
            Collection<Pair<GageData, List<String>>> formattedRecords) {
        for (Pair<GageData, List<String>> record : formattedRecords) {
            TableItem tableItem = new TableItem(dataList, SWT.NONE);
            tableItem.setData(record.getFirst());
            tableItem.setFont(JFaceResources.getTextFont());
            tableItem.setText(record.getSecond().toArray(new String[0]));
        }

        GC gc = new GC(dataList);
        gc.setFont(JFaceResources.getTextFont());
        int columnPad = gc.textExtent("  ").x;
        gc.dispose();

        for (TableColumn column : dataList.getColumns()) {
            column.pack();
            column.setWidth(column.getWidth() + columnPad);
        }
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 30;
        gl.verticalSpacing = 10;
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                false));

        int dpi = getDisplay().getDPI().x;

        // -------------------------------
        // Top buttons
        // -------------------------------
        Button timeSeriesGraphBtn = new Button(buttonComp, SWT.PUSH);
        timeSeriesGraphBtn.setText("Time Series Graph");
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = dpi * 2;
        timeSeriesGraphBtn.setLayoutData(gd);
        timeSeriesGraphBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayTimeSeries(true);
            }
        });

        Button printBtn = new Button(buttonComp, SWT.PUSH);
        printBtn.setText("Print");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        gd.minimumWidth = dpi * 5 / 4;
        printBtn.setLayoutData(gd);
        printBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendTableToPrinter();
            }
        });

        // -------------------------------
        // Bottom buttons
        // -------------------------------;
        Button timeSeriesTableBtn = new Button(buttonComp, SWT.PUSH);
        timeSeriesTableBtn.setText("Time Series Table");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = dpi * 2;
        timeSeriesTableBtn.setLayoutData(gd);
        timeSeriesTableBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayTimeSeries(false);
            }
        });

        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = dpi * 5 / 4;
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveTable();
            }
        });

        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = dpi * 5 / 4;
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
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
    private Collection<GageData> getData() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();

        if (pcOptions.getQueryMode() == QueryMode.AD_HOC_MODE.getQueryMode()) {
            pdcManager.scheduleRequest(true,
                    PointDataControlManager.REQUEST_TYPE.REQUEST_AD_HOC);
        } else {
            pdcManager.scheduleRequest(true,
                    PointDataControlManager.REQUEST_TYPE.REQUEST_TIME_STEP);
        }

        List<GageData> reportList = pdcManager.getObsReportList();
        Collections.sort(reportList);

        return reportList;
    }

    private Collection<Pair<GageData, List<String>>> formatRecords(
            Collection<GageData> records, boolean isForFile) {
        PDCDataManager dataManager = PDCDataManager.getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        Collection<Pair<GageData, List<String>>> formattedRecords = new ArrayList<>();
        for (GageData gd : records) {
            String actionText = "";
            double floodLevel = 0;
            double actionLevel = 0;
            double floodDepart = 0;
            String stageFlowText = "";
            String abbrevTime = "";
            double flow;
            double stage;

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
                            actionText = ACTION_MSG;
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
                            String msg = String.format(INVALID_PE_ERROR_FMT,
                                    gd.getPe());
                            statusHandler.warn(msg);
                            stageFlowText = "M";
                        }
                    }
                }

                /* format the time */
                if (gd.getValidtime() != null) {
                    abbrevTime = DATE_FORMAT.format(gd.getValidtime());
                }

                /* Shrink the name to fit */
                String name = gd.getName();
                if (name == null) {
                    name = "";
                } else if (isForFile) {
                    if (name.length() > 20) {
                        name = name.substring(0, 20);
                    }
                } else {
                    if (name.length() > 15) {
                        name = name.substring(0, 15);
                    }
                }

                List<String> formattedRecord = new ArrayList<>(11);

                /* write the information */
                if (gd.getValue() == PDCConstants.MISSING_VALUE) {
                    formattedRecord.add(String.format("%-7s", gd.getLid()));
                    String nameFormat = (isForFile) ? "%-20s" : "%-15s";
                    formattedRecord.add(String.format(nameFormat, name));
                    formattedRecord.add(String.format("%-6s", "m"));
                } else {
                    if ((floodLevel != PDCConstants.MISSING_VALUE)
                            && (pcOptions.getTimeMode() != PDCConstants.TimeModeType.VALUE_CHANGE
                                    .getTimeMode())) {
                        formattedRecord.add(String.format("%-7s", gd.getLid()));
                        String nameFormat = (isForFile) ? "%-20s" : "%-15s";
                        formattedRecord.add(String.format(nameFormat, name));
                        formattedRecord.add(String.format("%-6.2f",
                                gd.getValue()));
                        formattedRecord.add(String
                                .format("%-8s", stageFlowText));
                        formattedRecord.add(String.format("%-13s", abbrevTime));
                        formattedRecord.add(String.format("%-2s", gd.getPe()));
                        formattedRecord.add(String.format("%-2s", gd.getTs()));
                        formattedRecord.add(String.format("%4s", gd.getDur()));
                        formattedRecord.add(String.format("%4s",
                                gd.getExtremum()));
                        formattedRecord.add(String.format("[%6.1f %5.1f]",
                                floodLevel, floodDepart));

                        int actionTextIndex = (isForFile) ? formattedRecord
                                .size() : 4;
                        formattedRecord.add(actionTextIndex, actionText);
                    } else {
                        formattedRecord.add(String.format("%-7s", gd.getLid()));
                        String nameFormat = (isForFile) ? "%-20s" : "%-15s";
                        formattedRecord.add(String.format(nameFormat, name));
                        formattedRecord.add(String.format("%-6.2f",
                                gd.getValue()));
                        formattedRecord.add(String
                                .format("%-8s", stageFlowText));
                        formattedRecord.add(String.format("%-13s", abbrevTime));
                        formattedRecord.add(String.format("%-2s", gd.getPe()));
                        formattedRecord.add(String.format("%-2s", gd.getTs()));
                        formattedRecord.add(String.format("%4s", gd.getDur()));
                        formattedRecord.add(String.format("%4s",
                                gd.getExtremum()));

                        int actionTextIndex = (isForFile) ? formattedRecord
                                .size() : 4;
                        formattedRecord.add(actionTextIndex, actionText);
                    }
                }

                if (!formattedRecord.isEmpty()) {
                    formattedRecords.add(new Pair<>(gd, formattedRecord));
                }
            }
        }

        return formattedRecords;
    }

    /**
     * Handle the print table selection
     */
    private void sendTableToPrinter() {
        if (dataList.getItemCount() > 0) {
            PrintDialog dialog = new PrintDialog(shell, SWT.NONE);
            PrinterData data = dialog.open();

            if (data != null) {
                /*
                 * Do the printing in a background thread so that spooling does
                 * not freeze the UI.
                 */
                Printer printer = new Printer(data);

                TableItem[] rows = dataList.getItems();
                Collection<GageData> records = new ArrayList<>(rows.length);
                for (TableItem row : rows) {
                    records.add((GageData) row.getData());
                }

                new PrintTabularDataJob(printer, records).schedule();
            }
        }
    }

    private void displayTimeSeries(boolean isGraph) {
        GageData selectedRecord = null;
        TableItem[] selectedRows = dataList.getSelection();
        if (selectedRows.length > 0) {
            selectedRecord = (GageData) selectedRows[0].getData();
        }

        if (selectedRecord == null) {
            MessageDialog.openWarning(shell, "Invalid Selection",
                    "A single Station must be selected");
        }

        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        displayManager.setCurrentData(selectedRecord.getLid());
        TimeSeriesDlg.getInstance().updateAndOpen(selectedRecord, isGraph);
    }

    /**
     * Save the table data in a text file
     */
    private void saveTable() {
        FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        String filename = dialog.open();
        if (filename != null) {
            TableItem[] rows = dataList.getItems();
            Collection<GageData> records = new ArrayList<>(rows.length);
            for (TableItem row : rows) {
                records.add((GageData) row.getData());
            }

            new SaveTabularDataJob(Paths.get(filename), records).schedule();
        }
    }
}
