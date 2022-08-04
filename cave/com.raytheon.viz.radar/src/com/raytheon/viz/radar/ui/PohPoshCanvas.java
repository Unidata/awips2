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
package com.raytheon.viz.radar.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * This class is a canvas that displays either an open or filled triangle.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PohPoshCanvas extends Canvas {
    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 16;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 16;

    /**
     * Flag indicating if the triangle should be open or filled.
     */
    private boolean openTriangle = true;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param openTriangle
     *            Open triangle flag.
     */
    public PohPoshCanvas(Composite parent, boolean openTriangle) {
        super(parent, SWT.DOUBLE_BUFFERED);
        parentComp = parent;
        this.openTriangle = openTriangle;
        setupCanvas();
    }

    /**
     * Setup/Initialize the canvas.
     */
    private void setupCanvas() {
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        this.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);
        this.setLayoutData(gd);

        this.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e);
            }
        });
    }

    /**
     * Draw the open/filled triangle on the canvas.
     * 
     * @param e
     */
    private void drawCanvas(PaintEvent e) {
        e.gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        e.gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        e.gc.setForeground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_BLACK));

        e.gc.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_BLACK));

        // Triangle points.
        int[] lineArray = { 8, 2, 2, 14, 14, 14 };

        // Draw the triangle outline.
        e.gc.drawPolygon(lineArray);

        // If the triangle is not an open triangle then fill it in.
        if (openTriangle == false) {
            e.gc.fillPolygon(lineArray);
        }
    }

    /**
     * Draws an open triangle if the flag passed in is true or a filled triangle
     * if the flag is false.
     * 
     * @param flag
     *            True to draw an open triangle, false for a filled triangle.
     */
    public void drawOpenTriangle(boolean flag) {
        openTriangle = flag;

        this.redraw();
    }
}
