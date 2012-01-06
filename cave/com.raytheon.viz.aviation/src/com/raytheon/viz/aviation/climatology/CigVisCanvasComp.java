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
package com.raytheon.viz.aviation.climatology;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * This class contains the "base" canvas used to draw Ceiling/Visibility graphs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
abstract public class CigVisCanvasComp extends Composite {
    protected final String[] months = { "null", "Jan", "Feb", "Mar", "Apr",
            "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    protected final String[] elements = { "Visibility", "Ceiling",
            "Flight Category" };

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Large canvas font.
     */
    private Font largeFont;

    /**
     * Medium canvas font.
     */
    private Font mediumFont;

    /**
     * Small canvas font.
     */
    private Font smallFont;

    /**
     * Canvas width.
     */
    protected final int CANVAS_WIDTH = 900;

    /**
     * Canvas height.
     */
    protected final int CANVAS_HEIGHT = 650;

    /**
     * Ceiling & Visibility canvas.
     */
    private Canvas cigVisCanvas;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public CigVisCanvasComp(Composite parent, int style) {
        super(parent, style);

        this.parent = parent;

        init();
    }

    /**
     * Initialize the canvas and composite.
     */
    private void init() {
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        this.setLayout(gl);
        this.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));

        largeFont = new Font(parent.getDisplay(), "Monospace", 14, SWT.NORMAL);
        mediumFont = new Font(parent.getDisplay(), "Monospace", 12, SWT.NORMAL);
        smallFont = new Font(parent.getDisplay(), "Monospace", 9, SWT.NORMAL);

        setupCanvas();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                largeFont.dispose();
                mediumFont.dispose();
                smallFont.dispose();
            }
        });
    }

    /**
     * Setup the drawing canvas.
     */
    private void setupCanvas() {
        cigVisCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        cigVisCanvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        cigVisCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setAntialias(SWT.OFF);
                drawCanvas(e.gc);
            }
        });
    }

    /**
     * Redraw the canvas.
     */
    public void redrawCigVisCanvas() {
        cigVisCanvas.redraw();
    }

    /**
     * Get the canvas width.
     * 
     * @return The canvas width.
     */
    public int getCanvasWidth() {
        return CANVAS_WIDTH;
    }

    /**
     * Get the canvas height.
     * 
     * @return The canvas height.
     */
    public int getCanvasHeight() {
        return CANVAS_HEIGHT;
    }

    /**
     * Get the small font.
     * 
     * @return The small font.
     */
    public Font getSmallFont() {
        return smallFont;
    }

    /**
     * Get the medium font.
     * 
     * @return The medium font.
     */
    public Font getMediumFont() {
        return mediumFont;
    }

    /**
     * Get hte large font.
     * 
     * @return The large font.
     */
    public Font getLargeFont() {
        return largeFont;
    }

    /**
     * The text color returned is either white or black depending on how dark
     * the background RGB value is. The color returned does not get disposed.
     * 
     * @param rgb
     *            RGB background color.
     * @return White or Black system color.
     */
    public Color getTextColor(RGB rgb) {
        if (rgb.red < 165 && rgb.green < 165 && rgb.blue < 165) {
            return parent.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        if (rgb.red < 165 && rgb.green < 165 && rgb.blue > 165) {
            return parent.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        return parent.getDisplay().getSystemColor(SWT.COLOR_BLACK);
    }

    /**
     * Draw the legend on the graph.
     * 
     * @param gc
     *            Graphical context.
     * @param graphXCoord
     *            Graph's X coordinate.
     * @param graphYCoord
     *            Graph's Y coordinate.
     * @param graphWidth
     *            Graph width.
     * @param mvfrColor
     *            MVFR color.
     * @param ifrColor
     *            IFR color.
     * @param lifrColor
     *            LIFR color.
     * @param vlifrColor
     *            VLIFR color.
     */
    public void drawGraphLegend(GC gc, int graphXCoord, int graphYCoord,
            int graphWidth, Color mvfrColor, Color ifrColor, Color lifrColor,
            Color vlifrColor) {
        int legendWidth = 100;
        int legendHeight = 75;
        int legendXCoord = graphXCoord + graphWidth - legendWidth - 10;
        int legendYCoord = graphYCoord + 10;
        int legendXOffset = 5;
        int legendYOffset = 5;
        int legendBarWidth = 50;
        int legendBarHeight = 12;
        //
        // Draw Legend Box
        //
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(legendXCoord, legendYCoord, legendWidth, legendHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(legendXCoord, legendYCoord, legendWidth, legendHeight);

        //
        // Draw Legend information
        //

        // MVFR
        gc.setBackground(mvfrColor);
        gc.fillRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset, legendBarWidth, legendBarHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset, legendBarWidth, legendBarHeight);

        gc.drawString("MVFR",
                legendXCoord + legendXOffset * 2 + legendBarWidth, legendYCoord
                        + legendYOffset, true);

        // IFR
        gc.setBackground(ifrColor);
        gc.fillRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset * 2 + legendBarHeight, legendBarWidth,
                legendBarHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset * 2 + legendBarHeight, legendBarWidth,
                legendBarHeight);

        gc.drawString("IFR", legendXCoord + legendXOffset * 2 + legendBarWidth,
                legendYCoord + legendYOffset * 2 + legendBarHeight, true);

        // LIFR
        gc.setBackground(lifrColor);
        gc.fillRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset * 3 + legendBarHeight * 2, legendBarWidth,
                legendBarHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset * 3 + legendBarHeight * 2, legendBarWidth,
                legendBarHeight);

        gc.drawString("LIFR",
                legendXCoord + legendXOffset * 2 + legendBarWidth, legendYCoord
                        + legendYOffset * 3 + legendBarHeight * 2, true);

        // VLIFR
        gc.setBackground(vlifrColor);
        gc.fillRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset * 4 + legendBarHeight * 3, legendBarWidth,
                legendBarHeight);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(legendXCoord + legendXOffset, legendYCoord
                + legendYOffset * 4 + legendBarHeight * 3, legendBarWidth,
                legendBarHeight);

        gc.drawString("VLIFR", legendXCoord + legendXOffset * 2
                + legendBarWidth, legendYCoord + legendYOffset * 4
                + legendBarHeight * 3, true);
    }

    /**
     * This method is implemented by the class extending this abstract class and
     * is used to draw information on the canvas.
     * 
     * @param gc
     *            The graphical context.
     */
    abstract void drawCanvas(GC gc);

    public abstract float getMaxPercentInData();
}
