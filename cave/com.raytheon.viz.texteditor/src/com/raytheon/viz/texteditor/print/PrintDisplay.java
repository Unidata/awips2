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

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;

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
 * Jul 17, 2012 14274      rferrel     Now use eclipse Printer instead of awt.
 *                                      Text is printed using same font as the GUI
 * Dec 31, 2012 15651	   mgamazaychikov	Added setFont method to scale font for printing
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PrintDisplay {
    public static void print(final String printedText, final FontData fontData,
            int aLineWidth, IUFStatusHandler statusHandler) {
        PrinterData data = Printer.getDefaultPrinterData();
        if (data == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "No default printer specified.");
            return;
        }

        if (printedText == null || printedText.trim().length() == 0) {
            // Do not waste paper when nothing to print.
            return;
        }

        final Printer printer = new Printer(data);
        PrintDisplay pd = new PrintDisplay(printer, printedText, fontData, aLineWidth);
        pd.printJob();
    }
    
    private int lineWidth;

    private Printer printer;

    private String textToPrint;

    private FontData printerFontData;

    private int leftMargin;

    private int rightMargin;

    private int topMargin;

    private int bottomMargin;

    private String tabs;

    private GC gc;

    private int tabWidth;

    private int lineHeight;

    StringBuilder wordBuffer;

    int x;

    int y;

    int index;

    int end;

    private PrintDisplay(Printer printer, String text, FontData fontData, int aWidth) {
        this.printer = printer;
        this.textToPrint = text;
        this.printerFontData = fontData;
    	if (aWidth == -1) {
    		this.lineWidth = 69;
    	}
    	else {
    		this.lineWidth = aWidth;
    	}
    }

    private void printJob() {
        Thread thread = new Thread("Printing") {
            public void run() {
                printIt();
                printer.dispose();
            }
        };
        thread.start();
    }

    protected void setFont() {
    	/*
    	 * get the original font size and set the gc font.
    	 */
    	float origFontSize = printerFontData.getHeight();
        Font printerFont = new Font(printer, printerFontData);
        gc.setFont(printerFont);
        
        /*
         * Create a buffer for computing line width in pixels.
         */
        StringBuilder aBuffer = new StringBuilder(lineWidth);
        for (int i = 0; i < lineWidth; i++) {
        	aBuffer.append(' ');
        }
        /*
         * Get the line width in pixels and the device's width in pixels
         */
        int lineWidthPixels = gc.stringExtent(aBuffer.toString()).x;
    	int deviceWidthPixels = rightMargin - leftMargin;
        printerFont.dispose();
    	/*
    	 * Scale the original font size;
    	 */
    	float fontSize = (float)deviceWidthPixels / (float)lineWidthPixels * (float)origFontSize;
    	/*
    	 * Set the printerFont Data font to the scaled font
    	 */
    	printerFontData.setHeight((int)(fontSize));
    	gc.setFont(new Font(printer, printerFontData));
	}

	private void printIt() {
        if (printer.startJob("Text")) { // the string is the job name - shows up
                                        // in the printer's job list
            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);            
            Point dpi = printer.getDPI();

            // one inch from left side of paper
            leftMargin = dpi.x + trim.x;
            // one inch from right side of paper
            rightMargin = clientArea.width - dpi.x + trim.x + trim.width;
            topMargin = dpi.y + trim.y; // one inch from top edge of paper
            // one inch from bottom edge of paper
            bottomMargin = clientArea.height - dpi.y + trim.y + trim.height;

            // Create a buffer for computing tab width.
            int tabSize = 4;
            StringBuilder tabBuffer = new StringBuilder(tabSize);
            for (int i = 0; i < tabSize; i++)
                tabBuffer.append(' ');
            tabs = tabBuffer.toString();

            /*
             * Create printer GC, and create and set the printer font &
             * foreground color.
             */
            gc = new GC(printer);
            setFont();            
            Color printerForegroundColor = new Color(printer, new RGB(0, 0, 0));
            Color printerBackgroundColor = new Color(printer, new RGB(255, 255,
                    255));
            gc.setForeground(printerForegroundColor);
            gc.setBackground(printerBackgroundColor);
            tabWidth = gc.stringExtent(tabs).x;
            lineHeight = gc.getFontMetrics().getHeight();

            // Print text to current gc using word wrap
            printText();
            printer.endJob();

            // Cleanup graphics resources used in printing
            
            printerForegroundColor.dispose();
            printerBackgroundColor.dispose();
            gc.dispose();
        }
    }

    private void printText() {
        printer.startPage();
        wordBuffer = new StringBuilder();
        x = leftMargin;
        y = topMargin;
        index = 0;
        end = textToPrint.length();
        while (index < end) {
            char c = textToPrint.charAt(index);
            index++;
            if (c != 0) {
                if (c == 0x0a || c == 0x0d) {
                    if (c == 0x0d && index < end
                            && textToPrint.charAt(index) == 0x0a) {
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

    private void printWordBuffer() {
        if (wordBuffer.length() > 0) {
            String word = wordBuffer.toString();
            int wordWidth = gc.stringExtent(word).x;
            if (x + wordWidth > rightMargin) {
                // word doesn't fit on current line, so wrap
                newline();
            }
            gc.drawString(word, x, y, false);
            x += wordWidth;
            wordBuffer.setLength(0);
        }
    }

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