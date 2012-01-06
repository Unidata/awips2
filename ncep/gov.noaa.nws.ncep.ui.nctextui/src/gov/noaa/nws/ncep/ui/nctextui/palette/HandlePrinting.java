/**
 * 
 * gov.noaa.nws.ncep.ui.nctextui.palette.HandlePrinting
 * 
 * This java class performs the NCTEXT GUI text printing handling.
 * It should be able to be used by other project as well.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 1/21/2010		TBD		Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nctextui.palette;


import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

public class HandlePrinting {
	private Printer printer;
	private StringBuffer wordBuffer;
	private int lineHeight = 0;
	private int tabWidth = 0;
	private int leftMargin, rightMargin, topMargin, bottomMargin;
	private int x, y;
	private int index, end;		
	private String text;
	private GC gc;

	private static  HandlePrinting printHandle=null;
	public static HandlePrinting getPrintHandle() {
		if(printHandle==null)
			printHandle = new HandlePrinting();
		return printHandle;
	}
	public void handlePrint(String intext) {
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		text = intext;
		PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
		PrinterData data = dialog.open();
		if (data == null) return;
		if (data.printToFile) {
			data.fileName = "print.out"; // you probably want to ask the user for a filename
		}

		/* Do the printing in a background thread so that spooling does not freeze the UI. */
		printer = new Printer(data);
		Thread printingThread = new Thread("Printing") {
			public void run() {
				print(printer);
				printer.dispose();
			}
		};
		printingThread.start();
	}

	private void print(Printer printer) {
		String tabs;
		Font printerFont;
		Color printerForegroundColor, printerBackgroundColor;

		if (printer.startJob("Nctext")) {   // the string is the job name - shows up in the printer's job list
			Rectangle clientArea = printer.getClientArea();
			Rectangle trim = printer.computeTrim(0, 0, 0, 0);
			Point dpi = printer.getDPI();
			leftMargin = dpi.x + trim.x; // one inch from left side of paper
			rightMargin = clientArea.width - dpi.x + trim.x + trim.width; // one inch from right side of paper
			topMargin = dpi.y + trim.y; // one inch from top edge of paper
			bottomMargin = clientArea.height - dpi.y + trim.y + trim.height; // one inch from bottom edge of paper

			/* Create a buffer for computing tab width. */
			int tabSize = 4; // is tab width a user setting in your UI?
			StringBuffer tabBuffer = new StringBuffer(tabSize);
			for (int i = 0; i < tabSize; i++) tabBuffer.append(' ');
			tabs = tabBuffer.toString();

			/* Create printer GC, and create and set the printer font & foreground color. */
			gc = new GC(printer);
			printerFont = new Font(printer, "Courier", 8, SWT.NORMAL);
			gc.setFont(printerFont);
			tabWidth = gc.stringExtent(tabs).x;
			lineHeight = gc.getFontMetrics().getHeight();

			RGB rgb = new RGB(0,0,0);//Black
			printerForegroundColor = new Color(printer, rgb);
			gc.setForeground(printerForegroundColor);
			rgb = new RGB(255,255,255);//white
			printerBackgroundColor = new Color(printer, rgb);
			gc.setBackground(printerBackgroundColor);

			/* Print text to current gc using word wrap */
			printText();
			printer.endJob();

			/* Cleanup graphics resources used in printing */
			printerFont.dispose();
			printerForegroundColor.dispose();
			printerBackgroundColor.dispose();
			gc.dispose();
		}
	}

	void printText() {
		String textToPrint;
		/* Get the text to print */
		textToPrint = text;
		printer.startPage();
		wordBuffer = new StringBuffer();
		x = leftMargin;
		y = topMargin;
		index = 0;
		end = textToPrint.length();
		while (index < end) {
			char c = textToPrint.charAt(index);
			index++;
			if (c != 0) {
				if (c == 0x0a || c == 0x0d) {
					if (c == 0x0d && index < end && textToPrint.charAt(index) == 0x0a) {
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

	void printWordBuffer() {
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

	void newline() {
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

