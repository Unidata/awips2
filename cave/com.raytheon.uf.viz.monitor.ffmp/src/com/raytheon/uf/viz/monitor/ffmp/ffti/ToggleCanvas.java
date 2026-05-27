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
package com.raytheon.uf.viz.monitor.ffmp.ffti;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * 
 * Toggle canvas to emulate toggle switches.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ????????????            lvenable    Initial creation
 * Oct 10, 2013 #2464      lvenable    Fix font memory leak.
 * Nov 05, 2015 #5070      randerso    Changed to use system font name (not AWT)
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ToggleCanvas {
    private Composite parentComp;

    private Display display;

    private String text;

    private boolean toggleState = false;

    private boolean toggleEnabled = true;

    /*
     * Canvas information
     */
    private Canvas canvas;

    private final int CANVAS_WIDTH = 70;

    private final int CANVAS_HEIGHT = 20;

    private int horizontalSpan = 1;

    /*
     * Font/text information
     */
    private Font labelFont;

    /*
     * for change the QPE toggle and the total slider color
     */
    SettingComp ownerComp = null;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param text
     *            Label text.
     * @param toggleState
     *            Toggle state (on/off)
     */
    public ToggleCanvas(Composite parentComp, String text, boolean toggleState) {
        this(parentComp, text, toggleState, 1);
    }

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param text
     *            Label text.
     * @param toggleState
     *            Toggle state (on/off)
     * @param horizontalSpan
     *            How many columns to span.
     */
    public ToggleCanvas(Composite parentComp, String text, boolean toggleState,
            int horizontalSpan) {
        this.parentComp = parentComp;

        this.text = text;
        this.toggleState = toggleState;
        this.horizontalSpan = horizontalSpan;

        display = this.parentComp.getDisplay();

        init();
        createCanvas();
    }

    private void init() {
        labelFont = new Font(display, "Monospace", 10, SWT.BOLD);

        parentComp.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                labelFont.dispose();
            }
        });
    }

    private void createCanvas() {
        canvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.horizontalSpan = horizontalSpan;
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        canvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);

        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        canvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (toggleEnabled == false) {
                    return;
                }

                if ((ownerComp != null) && text.equals("QPF")) {
                    toggleState = !toggleState;
                    canvas.redraw();
                    ownerComp.qpfSrcToggled(toggleState);
                }
            }
        });
    }

    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);
        gc.setFont(labelFont);
        gc.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        /*
         * Draw toggle box
         */
        if (toggleState == true) {
            gc.setBackground(display.getSystemColor(SWT.COLOR_RED));
            gc.fillRectangle(5, 5, 10, 10);
        }

        gc.setLineWidth(2);
        gc.drawRectangle(4, 4, 12, 12);

        /*
         * Draw string
         */
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawString(text, 20, 2, true);
    }

    public void setToggleState(boolean state) {
        toggleState = state;
        canvas.redraw();
    }

    public boolean getToggleState() {
        return toggleState;
    }

    public void setOwner(SettingComp owner) {
        this.ownerComp = owner;
    }

    public void setToggleEnabled(boolean enabledFlag) {
        this.toggleEnabled = enabledFlag;
    }
}
