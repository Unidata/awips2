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
package com.raytheon.viz.texteditor.print;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Common class for handling printing of text.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2011 10557      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PrintDisplay {
    /**
     * Print the text/document to the default print.
     * 
     * @param autoPrint
     *            true send text directly to printer without prompting user for
     *            selecting a different printer.
     * @param printedText
     *            the text to print.
     */
    public static void print(boolean autoPrint, String printedText,
            IUFStatusHandler statusHandler) {
        if (printedText == null || printedText.trim().length() == 0) {
            // Do not waste paper on blank text.
            return;
        }

        PrintDisplay printer = new PrintDisplay(autoPrint, statusHandler);
        printer.printIt(printedText);
    }

    private boolean autoPrint;

    private IUFStatusHandler statusHandler;

    private PrintDisplay(boolean autoPrint, IUFStatusHandler statusHandler) {
        this.autoPrint = autoPrint;
        this.statusHandler = statusHandler;
    }

    private void printIt(String printedText) {
        PrinterJob printerJob = PrinterJob.getPrinterJob();

        if (autoPrint || printerJob.printDialog()) {
            printerJob.setPrintable(new Document(printedText));
            try {
                printerJob.print();
            } catch (PrinterException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    private class Document implements Printable {

        private final String[] printedText;

        private int numPages = 0;

        public Document(String printedText) {
            super();
            this.printedText = printedText.split("\n");
        }

        @Override
        public int print(Graphics graphics, PageFormat pageFormat, int pageIndex)
                throws PrinterException {
            // --- Create the Graphics2D object
            Graphics2D g2d = (Graphics2D) graphics;

            // --- Set the drawing color to black
            g2d.setPaint(Color.black);
            java.awt.Font font = g2d.getFont();
            String name = font.getName();
            int style = font.getStyle();
            int size = font.getSize();

            g2d.setFont(new java.awt.Font(name, style, size - 2));

            // --- Translate the origin to 0,0 for the top left corner
            g2d.translate(pageFormat.getImageableX(),
                    pageFormat.getImageableY());

            // Determine the lines per page and number of pages to print.
            java.awt.FontMetrics metrics = g2d.getFontMetrics();
            int lineHeight = metrics.getHeight();
            int linesPerPage = (int) Math.floor(pageFormat.getImageableHeight()
                    / lineHeight);
            numPages = ((printedText.length - 1) / linesPerPage) + 1;

            // Calculate the start line for this page.
            int startLine = pageIndex * linesPerPage;
            int endLine = startLine + linesPerPage - 1;
            if (endLine >= printedText.length) {
                endLine = printedText.length - 1;
            }

            // Tell the PrinterJob if the page number is not a legal one.
            if (pageIndex >= numPages) {
                return NO_SUCH_PAGE;
            }

            int y = 0;
            for (int i = startLine; i <= endLine; i++) {
                y += lineHeight;
                g2d.drawString(printedText[i], 0, y);
            }

            g2d.dispose();
            // --- Validate the page
            return (PAGE_EXISTS);
        }
    }
}