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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
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
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * generic print menu for reports.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012 13781      wkwock     Initial creation
 * Jul 16, 2013 2088       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */

public class PrintTextReportDlg extends CaveSWTDialog {
    /**
     * Text viewer.
     */
    protected TextReport report;

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

    private final StringBuilder wordBuffer = new StringBuilder();

    private GC gc;

    /**
     * A non-blocking modal constructor so user cannot change report while
     * dialog is open.
     * 
     * @param parentShell
     * @param report
     */
    protected PrintTextReportDlg(Shell parentShell, TextReport report) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);
        this.report = report;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createBottomButtons();
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

            // Create a buffer for computing tab with width of 4.
            String tabs = "    ";

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
        wordBuffer.setLength(0);
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
            wordBuffer.setLength(0);
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
     * Get data for selected contents for printing
     * 
     * @return
     */
    protected String getPrintData() {
        return "";
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    protected void createBottomButtons() {
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
                close();
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
                final String text = getPrintData();

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
                    Job job = new Job("PrintTable") {

                        @Override
                        protected IStatus run(IProgressMonitor monitor) {
                            print(printer, text);
                            printer.dispose();
                            printer = null;
                            return Status.OK_STATUS;
                        }

                    };
                    job.schedule();
                }
            }
        });
    }
}
