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
package com.raytheon.viz.hydro.gagedisplay;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The Gage Legend Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 3, 2008              mpduff      Initial creation
 * Apr 18,2013  1790        rferrel     Minor code clean up.
 * 09 Jul 2013  #1928       lvenable    Replaced SWT browser with a canvas.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class GageLegend extends CaveSWTDialog {

    /** Font for the canvas. */
    private Font font;

    /** Canvas to display the legend. */
    private Canvas legendCanvas;

    /** Canvas width. */
    private final int CANVAS_WIDTH = 280;

    /** Canvas height. */
    private final int CANVAS_HEIGHT = 270;

    /** Map of label text and colors. */
    private Map<String, RGB> textColorMap = new LinkedHashMap<String, RGB>();

    /**
     * Protected constructor.
     * 
     * @param parentShell
     *            Shell of the opener
     */
    protected GageLegend(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Gage Color Legend");
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {

        // Setup font and create color map.
        setReturnValue(false);
        font = new Font(shell.getDisplay(), "Monospace", 16, SWT.BOLD);
        createColorMap();

        // Setup the canvas.
        legendCanvas = new Canvas(shell, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        legendCanvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);

        legendCanvas.setLayoutData(gd);
        legendCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        createCloseButton();
    }

    /**
     * Draw the canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawCanvas(GC gc) {

        // Setup the GC
        gc.setAntialias(SWT.ON);
        gc.setLineWidth(2);
        gc.setFont(font);
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        // Draw the background
        gc.setBackground(getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        // Setup the coordinates and color.
        int yCoord = 10;
        int xCoord = 10;
        int width = CANVAS_WIDTH - (xCoord * 2);
        int height = 50;
        Color c;

        // Loop over and draw the legend label and colors.
        for (Entry<String, RGB> entry : textColorMap.entrySet()) {
            c = new Color(getDisplay(), entry.getValue());

            gc.setBackground(c);
            gc.fillRectangle(xCoord, yCoord, width, height);
            gc.drawRectangle(xCoord, yCoord, width, height);

            gc.drawString(entry.getKey(), xCoord + 10, yCoord + 12, true);

            yCoord += height;
            c.dispose();
        }
    }

    /**
     * Create the color map with text and colors.
     */
    private void createColorMap() {
        textColorMap.put("Above Flood Stage", new RGB(255, 0, 0));
        textColorMap.put("Above Action Stage", new RGB(255, 255, 0));
        textColorMap.put("Below Action Stage", new RGB(0, 255, 0));
        textColorMap.put("Missing Data", new RGB(192, 192, 192));
        textColorMap.put("Missing Stage Data", new RGB(34, 139, 34));
    }

    /**
     * Create the close button.
     */
    private void createCloseButton() {
        /* Close button */
        GridLayout layout = new GridLayout(1, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(layout);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }
}
