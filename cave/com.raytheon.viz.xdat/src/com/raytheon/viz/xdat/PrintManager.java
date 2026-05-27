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
package com.raytheon.viz.xdat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.printing.Printer;

/**
 * This class manages printing text/graphics.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 15 Jan 2008             lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PrintManager {

    /**
     * Printer.
     */
    private Printer printer;

    /**
     * Line height.
     */
    private int lineHeight = 0;

    /**
     * Tab width;
     */
    private int tabWidth = 0;

    /**
     * Left margin.
     */
    private int leftMargin;

    /**
     * Right margin.
     */
    private int rightMargin;

    /**
     * Top margin.
     */
    private int topMargin;

    /**
     * Bottom margin.
     */
    private int bottomMargin;

    /**
     * X & Y coordinates.
     */
    private int x, y;

    /**
     * Index and end variables.
     */
    private int index, end;

    /**
     * Word buffer.
     */
    private StringBuffer wordBuffer;

    /**
     * Graphic context.
     */
    private GC gc;

    /**
     * Constructor.
     * 
     * @param printer
     *            The printer.
     */
    public PrintManager(Printer printer) {
        this.printer = printer;
    }

    /**
     * Setup the printer and print.
     * 
     * @param text
     *            Text to print.
     */
    public void print(String text) {

        if (printer.startJob("Text")) {

            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);
            Point dpi = printer.getDPI();
            leftMargin = dpi.x + trim.x; // one inch from left side of paper
            rightMargin = clientArea.width - dpi.x + trim.x + trim.width; // one
                                                                          // inch
                                                                          // right
            topMargin = dpi.y + trim.y; // one inch from top edge of paper
            bottomMargin = clientArea.height - dpi.y + trim.y + trim.height; // one
                                                                             // inch
                                                                             // from
                                                                             // bottom

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

            Font printerFont = new Font(printer, "Monospace", 6, SWT.NORMAL);

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
     * Print the text.
     * 
     * @param text
     *            Text to print.
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
