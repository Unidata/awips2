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

import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.viz.aviation.xml.PlotViewerCfg;
import com.raytheon.viz.aviation.xml.WxPlotCfg;

/**
 * This abstract class draws the basic graph lines on the canvas.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 JUN 2008  1119        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
abstract class WeatherCanvasComp extends Composite {
    /**
     * Parent composite.
     */
    protected Composite parent;

    /**
     * Canvas width.
     */
    protected int CANVAS_WIDTH = 1115;

    /**
     * Canvas height.
     */
    protected int CANVAS_HEIGHT = 170;

    /**
     * Drawing canvas.
     */
    protected Canvas canvas;

    /**
     * Zoom level.
     */
    protected int zoomLevel = 1;

    /**
     * Header information.
     */
    private String headerStr;

    /**
     * Current time
     */
    protected long currentTime = 0;

    /**
     * Current hour.
     */
    protected int currentHour = 0;

    /**
     * Total hours display is the number of hours before and after the current
     * hour PLUS the current hour. The default is 24 hours before + 24 hours
     * after + the current hour = 49 hours displayed. Eventually this will be
     * configurable.
     */
    protected int totalHoursDisplayed;

    protected int hoursBack;

    protected int hoursForeward;

    /**
     * X coordinate of the graph.
     */
    protected int graphXCoord = 40;

    /**
     * Y coordinate of the graph.
     */
    protected int graphYCoord = 20;

    /**
     * Rectangle used for drawing the graph boundaries.
     */
    protected Rectangle graphRect;

    /**
     * Y coordinate of the time labels at the top of the graph.
     */
    protected int topTimeYcoord = 5;

    /**
     * Y coordinate of the time labels at the bottom of the graph.
     */
    protected int bottomTimeYcoord = 0;

    /**
     * Default space between the time lines.
     */
    protected int timeLineXCoordSpaceDefault = 21;

    /**
     * Space between the time lines -- will vary with zoom level changes.
     */
    protected int timeLineXCoordSpace = timeLineXCoordSpaceDefault;

    /**
     * X offset of the first line in the time line.
     */
    protected int timeLineXoffset = 15;

    protected int x0;

    /**
     * Small canvas font.
     */
    protected Font smallFont;

    protected List<PlotViewerCfg> plotViewers;

    private Image image;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param headerStr
     *            Header string.
     * @param currentHour
     *            Current hour.
     */
    public WeatherCanvasComp(Composite parent, String headerStr,
            long currentTime, WxPlotCfg wxPlotCfg) {
        super(parent, SWT.NONE);

        this.parent = parent;

        this.headerStr = headerStr;

        this.currentTime = currentTime;
        Calendar c = Calendar.getInstance();
        c.setTimeZone(TimeZone.getTimeZone("GMT"));
        c.setTimeInMillis(currentTime);

        this.hoursBack = wxPlotCfg.getHoursBack();
        this.hoursForeward = wxPlotCfg.getHoursForeward();
        this.totalHoursDisplayed = hoursBack + 1 + hoursForeward;
        c.add(Calendar.HOUR_OF_DAY, -1 * hoursBack);
        this.currentHour = c.get(Calendar.HOUR_OF_DAY);

        plotViewers = wxPlotCfg.getPlotViewers();

        init();
    }

    /**
     * Initialization method.
     */
    private void init() {
        calculateCoordinates();

        smallFont = new Font(parent.getDisplay(), "Monospace", 9, SWT.NORMAL);

        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 2;
        gl.marginWidth = 0;
        this.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        this.setLayoutData(gd);

        // ---------------------------------------
        // Add the header label.
        // ---------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label headerLbl = new Label(this, SWT.NONE);
        headerLbl.setText(headerStr);
        headerLbl.setLayoutData(gd);

        // ---------------------------------------
        // Setup the drawing canvas.
        // ---------------------------------------
        setupCanvas();

        // Add a dispose listener to the canvas so the colors and
        // font can get cleaned up.
        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                smallFont.dispose();
            }
        });
    }

    /**
     * Setup the canvas.
     */
    private void setupCanvas() {
        canvas = new Canvas(this, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        canvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        canvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });
    }

    /**
     * Draw the main components on the canvas. Then call the abstract method
     * drawCanvasData to draw any additional information.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);

        // -------------------------------
        // Fill in the canvas background
        // -------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        // -------------------------------
        // Draw the graph boundaries
        // -------------------------------
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(graphRect);

        // -------------------------------------------
        // Draw the time labels
        // -------------------------------------------
        gc.setFont(smallFont);
        int smallFontWidth = gc.getFontMetrics().getAverageCharWidth();

        int timeXcoord = graphRect.x + timeLineXoffset - smallFontWidth;
        int hour = currentHour;

        gc.setLineStyle(SWT.LINE_DOT);

        for (int i = 0; i < totalHoursDisplayed; i++) {
            if (hour > 23) {
                hour = 0;
            }

            gc.drawText(String.format("%02d", hour), timeXcoord, topTimeYcoord,
                    true);

            gc.drawText(String.format("%02d", hour), timeXcoord,
                    bottomTimeYcoord, true);

            gc.drawLine(timeXcoord + smallFontWidth, graphRect.y, timeXcoord
                    + smallFontWidth, graphRect.y + graphRect.height);

            timeXcoord += timeLineXCoordSpace;

            ++hour;
        }

        gc.setLineStyle(SWT.LINE_SOLID);

        x0 = graphRect.x + timeLineXoffset;

        // -------------------------------------------
        // Draw the data defined by the class that
        // extends this abstract class
        // -------------------------------------------
        drawCanvasData(gc);
    }

    /**
     * Calculate the graph coordinates.
     */
    public void calculateCoordinates() {
        timeLineXCoordSpace = timeLineXCoordSpaceDefault * zoomLevel;

        CANVAS_WIDTH = graphXCoord * 2 + (totalHoursDisplayed - 1)
                * timeLineXCoordSpace + timeLineXoffset * 2;

        graphRect = new Rectangle(graphXCoord, graphYCoord,
                (totalHoursDisplayed - 1) * timeLineXCoordSpace
                        + timeLineXoffset * 2, CANVAS_HEIGHT - graphYCoord * 2);

        bottomTimeYcoord = graphRect.y + graphRect.height + 5;

        if (canvas != null) {
            canvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
            this.layout();
        }
    }

    /**
     * Draw additional data on the canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    abstract void drawCanvasData(GC gc);

    public Image getImage() {
        // Redraw the wind rose on an image.
        if (image != null) {
            image.dispose();
        }

        image = new Image(parent.getDisplay(), CANVAS_WIDTH, CANVAS_HEIGHT);

        GC gc = new GC(image);
        drawCanvas(gc);

        gc.dispose();

        return image;

    }

    /**
     * Update and redraw the canvas.
     * 
     * @param zoomLevel
     *            Zoom level.
     */
    abstract void updateAndRedraw(int zoomLevel);

    public int getWidth() {
        return CANVAS_WIDTH;
    }

    public int getHeight() {
        return CANVAS_HEIGHT;
    }
}
