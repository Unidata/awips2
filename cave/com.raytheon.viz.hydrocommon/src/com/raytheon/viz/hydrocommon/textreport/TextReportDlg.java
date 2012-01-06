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
package com.raytheon.viz.hydrocommon.textreport;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
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
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.hydrocommon.textreport.TextReportConstants.E19Pages;
import com.raytheon.viz.hydrocommon.textreport.TextReportConstants.ServiceBackupSort;
import com.raytheon.viz.hydrocommon.textreport.TextReportConstants.StationListSort;
import com.raytheon.viz.hydrocommon.textreport.TextReportConstants.TextReportType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Text Report dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 9, 2008				lvenable	Initial creation
 * Dec 16, 2008 1787        askripsk    Started report generation.
 * Sep 23, 2009 2260        mpduff      Finished the dialog,
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TextReportDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextReportDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Station ID.
     */
    private String stationId;

    /**
     * Report combo box.
     */
    private Combo reportCbo;

    /**
     * Page combo box.
     */
    private Combo pageCbo;

    /**
     * Page Combobox label.
     */
    private Label pageLbl;

    /**
     * Text viewer.
     */
    private StyledText textViewer;

    private TextReport report;

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
     * @param stationId
     *            Station ID.
     */
    public TextReportDlg(Shell parent, String stationId) {
        super(parent);

        this.stationId = stationId;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        createComboControls();

        createTextViewerControl();

        // Create a separator
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        setDialogTitle();
        handleReportSelection();
    }

    /**
     * Create the report and page combo controls.
     */
    private void createComboControls() {
        Composite comboComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        comboComp.setLayout(gl);

        Label reportLbl = new Label(comboComp, SWT.RIGHT);
        reportLbl.setText("Report");

        reportCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        TextReportType[] reportTypes = TextReportType.values();
        for (int i = 0; i < reportTypes.length; i++) {
            reportCbo.add(reportTypes[i].getReportName());
        }
        reportCbo.select(0);
        reportCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleReportSelection();
                setDialogTitle();

                // loadReport();
            }
        });

        GridData gd = new GridData(120, SWT.DEFAULT);
        pageLbl = new Label(comboComp, SWT.RIGHT);
        pageLbl.setText("   Page");
        pageLbl.setLayoutData(gd);

        pageCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pageCbo.select(0);
        pageCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                String selectedReport = reportCbo.getItem(reportCbo
                        .getSelectionIndex());
                if (selectedReport.equalsIgnoreCase(TextReportType.E19
                        .getReportName())) {
                    if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(E19Pages.COVER.getPageName())) {
                        scroll(E19Pages.COVER);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(
                                    E19Pages.MAP_GAGE_LOCATION.getPageName())) {
                        scroll(E19Pages.MAP_GAGE_LOCATION);
                    } else if (pageCbo
                            .getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(E19Pages.BENCHMARKS.getPageName())) {
                        scroll(E19Pages.BENCHMARKS);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(E19Pages.GAGES.getPageName())) {
                        scroll(E19Pages.GAGES);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(E19Pages.HISTORY.getPageName())) {
                        scroll(E19Pages.HISTORY);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(E19Pages.CRESTS.getPageName())) {
                        scroll(E19Pages.CRESTS);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(
                                    E19Pages.LOW_WATER_RECORDS.getPageName())) {
                        scroll(E19Pages.LOW_WATER_RECORDS);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(
                                    E19Pages.CONDITIONS_AFFECTING_FLOW
                                            .getPageName())) {
                        scroll(E19Pages.CONDITIONS_AFFECTING_FLOW);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(E19Pages.DAMAGE.getPageName())) {
                        scroll(E19Pages.DAMAGE);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(
                                    E19Pages.RIVER_STAGE_DATA.getPageName())) {
                        scroll(E19Pages.RIVER_STAGE_DATA);
                    } else if (pageCbo.getItem(pageCbo.getSelectionIndex())
                            .equalsIgnoreCase(E19Pages.CONTACTS.getPageName())) {
                        scroll(E19Pages.CONTACTS);
                    }
                } else if (selectedReport
                        .equalsIgnoreCase(TextReportType.SERVICE_BACKUP
                                .getReportName())) {
                    textViewer.setText("");
                    textViewer.setText(report.getText(pageCbo
                            .getSelectionIndex()));

                } else if (selectedReport
                        .equalsIgnoreCase(TextReportType.SORTED_STATION_LIST
                                .getReportName())) {
                    textViewer.setText("");
                    textViewer.setText(report.getText(pageCbo
                            .getSelectionIndex()));
                }
            }
        });
    }

    /**
     * Create the text view control.
     */
    private void createTextViewerControl() {
        GridData gd = new GridData(GridData.FILL_BOTH);
        Composite textViewerComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        textViewerComp.setLayout(gridLayout);
        textViewerComp.setLayoutData(gd);

        gd = new GridData(1000, 600);
        textViewer = new StyledText(textViewerComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        textViewer.setWordWrap(false);
        textViewer.setFont(controlFont);
        textViewer.setEditable(false);
        textViewer.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        GridData mainBtnData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(shell, SWT.NONE);
        GridLayout btnGl = new GridLayout(3, false);
        btnComp.setLayout(btnGl);
        btnComp.setLayoutData(mainBtnData);

        GridData gd = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
        gd.widthHint = 110;
        Button closeBtn = new Button(btnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 110;
        Button saveBtn = new Button(btnComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                AppsDefaults ad = AppsDefaults.getInstance();
                String reportDir = ad.getToken("whfs_report_dir");

                FileDialog dialog = new FileDialog(shell, SWT.SAVE);
                String[] filterNames = new String[] { "All Files (*)" };
                String[] filterExtensions = new String[] { "*" };
                String filterPath = reportDir;
                String platform = SWT.getPlatform();
                if (platform.equals("win32") || platform.equals("wpf")) {
                    filterNames = new String[] { "All Files (*.*)" };
                    filterExtensions = new String[] { "*.*" };
                    filterPath = "c:\\";
                }
                dialog.setFilterNames(filterNames);
                dialog.setFilterExtensions(filterExtensions);
                dialog.setFilterPath(filterPath);

                // make file name TextReports/TEXT/textrept_show.c
                String fileName = stationId;
                String selectedReport = reportCbo.getItem(reportCbo
                        .getSelectionIndex());
                if (selectedReport.equalsIgnoreCase("E-19"))
                    fileName += ".e19";
                else if (selectedReport.equalsIgnoreCase("E-19A (Summary)"))
                    fileName += ".e19a";
                else if (selectedReport.equalsIgnoreCase("B-44A (Cooperative)"))
                    fileName += ".b44a";
                else if (selectedReport.equalsIgnoreCase("Sorted Station List")) {
                    fileName = "station_list.";
                    String selectedPage = pageCbo.getItem(pageCbo
                            .getSelectionIndex());
                    if (selectedPage.equalsIgnoreCase("Location Id"))
                        fileName += "lid";
                    else if (selectedPage.equalsIgnoreCase("Name"))
                        fileName += "name";
                    else if (selectedPage.equalsIgnoreCase("County"))
                        fileName += "county";
                    else if (selectedPage.equalsIgnoreCase("Basin"))
                        fileName += "basin";
                    else if (selectedPage.equalsIgnoreCase("Observer"))
                        fileName += "observer";
                    else
                        fileName += "rpt";
                } else if (selectedReport.equalsIgnoreCase("Station Class"))
                    fileName = "stnclass.rpt";
                else if (selectedReport.equalsIgnoreCase("Service Backup")) {
                    fileName = "service_backup.";
                    String selectedPage = pageCbo.getItem(pageCbo
                            .getSelectionIndex());
                    if (selectedPage.equalsIgnoreCase("Station"))
                        fileName += "id";
                    else if (selectedPage.equalsIgnoreCase("WFO"))
                        fileName += "wfo";
                    else if (selectedPage.equalsIgnoreCase("HSA"))
                        fileName += "hsa";
                    else
                        fileName += "rpt";
                }

                dialog.setFileName(fileName);
                String filename = dialog.open();
                if (filename == null)
                    return; // user canceled save

                try {
                    BufferedWriter out = new BufferedWriter(new FileWriter(
                            filename));
                    out.write(textViewer.getText());
                    out.close();
                } catch (IOException ex) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error saving report", ex);
                }
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        gd.widthHint = 110;
        Button printBtn = new Button(btnComp, SWT.PUSH);
        printBtn.setText("Print");
        printBtn.setLayoutData(gd);
        printBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                final String text = textViewer.getText();

                if (text != null) {
                    PrintDialog dialog = new PrintDialog(shell, SWT.NONE);
                    PrinterData data = dialog.open();

                    if (data == null) {
                        return;
                    }

                    printer = new Printer(data);

                    /*
                     * Do the printing in a background thread so that spooling
                     * does not freeze the UI.
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
        });
    }

    /**
     * Set the dialog title.
     */
    private void setDialogTitle() {
        if (reportCbo.getSelectionIndex() < 3) {
            setText(reportCbo.getText() + " Report - " + stationId);
        } else {
            setText(reportCbo.getText() + " Report");
        }
    }

    /**
     * Event handler for Report combobox. This populates the page combobox and
     * displays the report
     */
    private void handleReportSelection() {
        // Get the selected report type
        TextReportType type = getSelectedReportType();
        pageCbo.removeAll();

        switch (type) {
        case E19:
            pageCbo.setVisible(true);
            pageLbl.setVisible(true);
            E19Pages[] pages = E19Pages.values();
            for (int i = 0; i < pages.length; i++) {
                pageCbo.add(pages[i].getPageName());
            }
            pageCbo.select(0);
            pageLbl.setText("   Page");

            report = new E19Report(stationId);
            textViewer.setText(report.loadTextWidget());
            break;
        case E19A:
            pageCbo.setVisible(false);
            pageLbl.setVisible(false);

            report = new E19AReport(stationId);
            textViewer.setText("");
            textViewer.setText(report.loadTextWidget());
            break;
        case B44A:
            pageCbo.setVisible(false);
            pageLbl.setVisible(false);

            report = new B44AReport(stationId);
            textViewer.setText("");
            textViewer.setText(report.loadTextWidget());
            break;
        case SORTED_STATION_LIST:
            pageCbo.setVisible(true);
            pageLbl.setVisible(true);
            StationListSort[] sorts = StationListSort.values();
            for (int i = 0; i < sorts.length; i++) {
                pageCbo.add(sorts[i].getStationSortType());
            }
            pageCbo.select(0);
            pageLbl.setText("Sort By");

            report = new StationListReport();
            textViewer.setText("");
            textViewer.setText(report.loadTextWidget());
            break;
        case STATION_CLASS:
            pageCbo.setVisible(false);
            pageLbl.setVisible(false);

            report = new StationClassReport();
            textViewer.setText("");
            textViewer.setText(report.loadTextWidget());
            break;
        case SERVICE_BACKUP:
            pageCbo.setVisible(true);
            pageLbl.setVisible(true);
            ServiceBackupSort[] backupSort = ServiceBackupSort.values();
            for (int i = 0; i < backupSort.length; i++) {
                pageCbo.add(backupSort[i].getServiceSort());
            }
            pageCbo.select(0);
            pageLbl.setText("Sort By");

            report = new ServiceBackupReport(stationId);
            textViewer.setText("");
            textViewer.setText(report.loadTextWidget());
            break;
        }
    }

    /**
     * Get the selected report type.
     * 
     * @return The TextReportType that is selected
     */
    private TextReportType getSelectedReportType() {
        TextReportType selectedType = null;
        String selection = reportCbo.getItem(reportCbo.getSelectionIndex())
                .trim();
        TextReportType[] types = TextReportType.values();
        for (int i = 0; i < types.length; i++) {
            if (types[i].getReportName().equals(selection)) {
                selectedType = types[i];
            }
        }

        return selectedType;
    }

    /**
     * Scroll the E19 report page into view.
     * 
     * @param page
     *            The page to scroll to
     */
    private void scroll(E19Pages page) {
        int offset = 0;
        String search = null;

        switch (page) {
        case COVER:
            search = TextReportConstants.E19_HDR_COVER;
            break;
        case MAP_GAGE_LOCATION:
            search = TextReportConstants.E19_HDR_MAPPAGE;
            break;
        case BENCHMARKS:
            search = TextReportConstants.E19_HDR_BENCHMARKS;
            break;
        case GAGES:
            search = TextReportConstants.E19_HDR_GAGES;
            break;
        case HISTORY:
            search = TextReportConstants.E19_HDR_HISTORY;
            break;
        case CRESTS:
            search = TextReportConstants.E19_HDR_CRESTS;
            break;
        case LOW_WATER_RECORDS:
            search = TextReportConstants.E19_HDR_LOWWATER;
            break;
        case CONDITIONS_AFFECTING_FLOW:
            search = TextReportConstants.E19_HDR_CONDITIONS;
            break;
        case DAMAGE:
            search = TextReportConstants.E19_HDR_DAMAGE;
            break;
        case RIVER_STAGE_DATA:
            search = TextReportConstants.E19_HDR_STAFFGAGE;
            break;
        case CONTACTS:
            search = TextReportConstants.E19_HDR_CONTACTS;
            break;
        }

        // Get the text
        String text = textViewer.getText();

        // Get the offset of the text
        offset = text.indexOf(search);

        // Scroll to the end of the text
        textViewer.setSelection(text.length() - 1);

        // Now scroll to the text, this puts the
        // text at the top of the page
        textViewer.setSelection(offset);
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
            leftMargin = dpi.x + trim.x; // one inch from left side of paper
            rightMargin = clientArea.width - dpi.x + trim.x + trim.width; // one
            // inch
            // from
            // right
            // side
            // of
            // paper
            topMargin = dpi.y + trim.y; // one inch from top edge of paper
            bottomMargin = clientArea.height - dpi.y + trim.y + trim.height; // one
            // inch
            // from
            // bottom
            // edge
            // of
            // paper

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
}
