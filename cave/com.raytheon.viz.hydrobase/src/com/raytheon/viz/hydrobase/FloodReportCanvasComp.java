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
package com.raytheon.viz.hydrobase;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This is the canvas where the flood report data is drawn.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008	2259    	lvenable	Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FloodReportCanvasComp {
    private static SimpleDateFormat sdf = null;

    static {
        sdf = new SimpleDateFormat("MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Canvas containing the "STAGE IN FEET" label and y graph axis.
     */
    private Canvas labelCanvas;

    /**
     * Canvas where the flood data is drawn.
     */
    private Canvas graphCanvas;

    /**
     * Canvas composite containing the 2 canvases.
     */
    private Composite canvasComp;
    
    /**
     * The scrolling composite that holds the graph
     */
    private ScrolledComposite scrolledComp;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 350;

    /**
     * Label canvas width.
     */
    private final int LABEL_CANVAS_WIDTH = 75;

    /**
     * Scrolled composite width.
     */
    private final int SCROLLED_COMP_WIDTH = 350;

    /**
     * Graph canvas width.
     */
    private final int CANVAS_WIDTH = 350;
    
    /** Pixels per hour */
    private final int PIXELS_PER_HOUR = 7;
    
    /**
     * Scrolled composite height.
     */
    private final int SCROLLED_COMP_HEIGHT = CANVAS_HEIGHT;

    /** Top of graph offset */
    private final int TOP_OFFSET = 10;

    /** Bottom of graph offset */
    private final int BOTTOM_OFFSET = 50;

    /** Left side of graph offset */
    private final int LEFT_OFFSET = 0;

    /** The line offset */
    private final int LINE_OFFSET = 6;

    /** The number of vertical intervals */
    private final int NUM_VERTICAL_INTERVALS = 5;

    /** The label offset */
    private final int LABEL_OFFSET = 58;

    /** The max y value */
    private double maxY = 10;

    /** The min y value */
    private double minY = 5;

    /** The font */
    private Font canvasFont = null;

    /**
     * The selected lid.
     */
    private String selectedLid = null;

    /**
     * The selected data key.
     */
    private String selectedKey = null;

    /** The current x position */
    private int currentX = 0;

    /** The current y position */
    private int currentY = 0;

    /** The Cross Hair cursor */
    private Cursor crossHairCursor;

    /** The standard arrow cursor */
    private Cursor arrowCursor;

    /** Flood data, 0 = level, 1 = date/time of measurement */
    private ArrayList<Object[]> floodData = null;

    /** The rectangle that is the graph area of the canvas */
    private Rectangle graphAreaRectangle;

    /** Draw the crosshairs flag */
    private boolean drawCrossHairs = false;

    /** The parent dialog */
    private FloodReportDlg parentDlg;

    /** Flood Stage */
    private double fs = 0;
    
    /**
     * Calculated width of the graph canvas
     */
    private int canvasWidth = CANVAS_WIDTH;
    
    private long zeroTime = 0;
    
    private long lastTime = 0;
    
    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     */
    public FloodReportCanvasComp(Composite parentComp, FloodReportDlg parentDlg) {
        this.parentComp = parentComp;
        this.parentDlg = parentDlg;

        canvasComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        canvasComp.setLayout(gl);

        crossHairCursor = new Cursor(parentComp.getDisplay(), SWT.CURSOR_CROSS);
        arrowCursor = new Cursor(parentComp.getDisplay(), SWT.CURSOR_ARROW);

        setupLabelCanvas();
        setupGraphCanvas();

        /* Graph Area Rectangle */
        // Set to 1 to prevent cross hairs from staying on
        // graph after mouse has left the area
        graphAreaRectangle = new Rectangle(1, 0, SCROLLED_COMP_WIDTH, CANVAS_HEIGHT
                - BOTTOM_OFFSET);

        canvasFont = new Font(parentComp.getDisplay(), "Monospace", 10,
                SWT.NORMAL);
    }

    /**
     * Setup the label canvas.
     */
    private void setupLabelCanvas() {
        labelCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = LABEL_CANVAS_WIDTH;

        labelCanvas.setSize(LABEL_CANVAS_WIDTH, CANVAS_HEIGHT);

        labelCanvas.setLayoutData(gd);

        labelCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawLabelCanvas(e.gc);
            }
        });
    }

    /**
     * Setup the graph canvas.
     */
    private void setupGraphCanvas() {
        scrolledComp = new ScrolledComposite(canvasComp,
                SWT.H_SCROLL | SWT.V_SCROLL);
        GridLayout gl = new GridLayout(1, false);
        scrolledComp.setLayout(gl);
        GridData gd = new GridData(SCROLLED_COMP_WIDTH, SCROLLED_COMP_HEIGHT);
        scrolledComp.setLayoutData(gd);

        graphCanvas = new Canvas(scrolledComp, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = CANVAS_WIDTH;

        graphCanvas.setSize(CANVAS_WIDTH, CANVAS_HEIGHT);        
        
        graphCanvas.setLayoutData(gd);
        graphCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawGraphCanvas(e.gc);
            }
        });

        graphCanvas.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                handleMouseMoveEvent(e);
            }
        });
        
        graphCanvas.addListener(SWT.MouseExit, new Listener() {

            @Override
            public void handleEvent(Event e) {
                handleMouseExitEvent(e);
            }
            
        });
        
        scrolledComp.setContent(graphCanvas);
    }

    /**
     * Draw the label canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawLabelCanvas(GC gc) {
        gc.setBackground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));

        gc.fillRectangle(0, 0, LABEL_CANVAS_WIDTH, CANVAS_HEIGHT);

        setYAxisMaxMin();
        drawYAxis(gc);
    }

    /**
     * Draw the graph canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawGraphCanvas(GC gc) {
        gc.setBackground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_BLACK));

        gc.fillRectangle(0, 0, canvasWidth, CANVAS_HEIGHT);

        if (FloodReportDataManager.getInstance().isDrawGraph()) {
            drawXAxis(gc);
            drawPoints(gc);
            drawFloodStageLine(gc);

            if (drawCrossHairs) {
                drawCrossHairs(gc);
            }
        }
    }

    /**
     * Set the Y axis min and max values.
     */
    private void setYAxisMaxMin() {
        if (FloodReportDataManager.getInstance().isDrawGraph()) {
            FloodReportDataManager dman = FloodReportDataManager.getInstance();
            int roundFactor = 5;
            long lmin;
            long lmax;

            String lid = dman.getSelectedLid();
            FloodReportData data = dman.getReportData().get(selectedKey);

            double fs = data.getFloodStage();

            if (fs > 0) {
                minY = fs;
                maxY = fs;
            } else {
                minY = 5;
                maxY = 10;
            }

            floodData = dman.getFloodEventData(lid, data.getFloodEventId());
            for (Object[] oa : floodData) {
                if ((Double) oa[0] < minY) {
                    minY = (Double) oa[0];
                }

                if ((Double) oa[0] > maxY) {
                    maxY = (Double) oa[0];
                }
            }

            /*
             * Round min down. If the original min data > 0, then don't round
             * below 0.
             */
            lmin = (long) (minY / roundFactor);

            if (lmin >= 0) {
                minY = (lmin - 1) * roundFactor;
                if (minY < 0) {
                    minY = 0;
                }
            } else { /* lmin < 0 */
                minY = (lmin - 1) * roundFactor;
            }

            /* Round max up */
            lmax = (long) maxY / roundFactor;
            maxY = (lmax + 1) * roundFactor;

            /*
             * If the difference between max_y and min_y < 10, round max_y up
             * again.
             */
            if ((maxY - minY) < 10) {
                maxY = (lmax + 2) * roundFactor;
            }
        }
    }

    /**
     * Draw the points on the graph.
     * 
     * @param gc
     *            The GC
     */
    private void drawPoints(GC gc) {
        if (floodData != null) {
            int x;
            int y;

            gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_YELLOW));
            for (Object[] oa : floodData) {
                Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                Date d = (Date) oa[1];

                c.setTime(d);

                x = x2pixel(c.getTime());
                y = y2pixel((Double) oa[0]);
                gc.drawText("o", x - 4, y - 9, true);
            }
        }
    }

    private void drawFloodStageLine(GC gc) {
        if (selectedLid != null) {
            int y = y2pixel(fs);

            gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_RED));
            gc.drawLine(0, y, canvasWidth - PIXELS_PER_HOUR, y);
            gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_WHITE));
        }
    }

    /**
     * Get the pixel value corresponding to the date
     * 
     * @param date The date to convert to pixel value
     * @return
     *      The pixel value
     */
    private int x2pixel(Date date) {
        double millisPerPixel = (60 * 60 * 1000) / PIXELS_PER_HOUR;
        double xValue = (date.getTime() - zeroTime) / millisPerPixel;
        return (int) Math.round(xValue);
    }
    
    /**
     * Get the Date (X) value corresponding to the pixel value
     * passed in
     * 
     * @param xpix The pixel value to convert to Date
     * 
     * @return
     *      The date corresponding to the pixel value
     */
    private Date pixel2x(int xpix) {
        double millisPerPixel = (60 * 60 * 1000) / PIXELS_PER_HOUR;
        double millisTime = xpix * millisPerPixel + zeroTime;
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis((long)millisTime);
        if (cal.get(Calendar.SECOND) >= 30) {
            cal.add(Calendar.MINUTE, 1);
        }
        cal.set(Calendar.SECOND, 0);
        
        return cal.getTime();
    }
    
    /**
     * convert real Y value to pixel value.
     * 
     * @param gd
     *            The Graph Data
     * @param y
     *            The y data value to convert
     * @return The y pixel value
     */
    private int y2pixel(double y) {
        double yDiff = this.maxY - this.minY;
        double yValue = ((CANVAS_HEIGHT - BOTTOM_OFFSET - TOP_OFFSET)/ yDiff) * (y - minY);

        return (int) ((CANVAS_HEIGHT - BOTTOM_OFFSET) - Math.round(yValue));
    }

    /**
     * convert pixel value to real Y value
     * 
     * @param gd
     *            The GraphData object
     * @param ypix
     *            The y pixel value
     * @return The y value
     */
    private double pixel2y(int ypix) {
        double ydiff = maxY - minY;
        double pixPerUnit = (CANVAS_HEIGHT - BOTTOM_OFFSET - TOP_OFFSET) / ydiff;

        return (maxY - ((ypix - TOP_OFFSET) / pixPerUnit));
    }

    /**
     * Draw the y axis.
     * 
     * @param gc
     *            The GC
     */
    private void drawYAxis(GC gc) {
        if (FloodReportDataManager.getInstance().isDrawGraph()) {
            gc.setFont(canvasFont);
            gc.setForeground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_WHITE));

            /* Get min, max, and x position. */
            if (minY == maxY) {
                minY -= 1.0;
                maxY += 1.0;
            }

            int min = y2pixel(minY);
            int max = y2pixel(maxY);
            int x = LABEL_CANVAS_WIDTH - LINE_OFFSET;

            /* Draw the vertical line. */
            gc.drawLine(x, min, x, max);

            /* Draw the horizontal lines and labels for the regular intervals. */
            int stageInc = (int) (maxY - minY) / NUM_VERTICAL_INTERVALS;
            x = LABEL_CANVAS_WIDTH - LINE_OFFSET;
            int y;
            StringBuilder buffer = new StringBuilder();

            for (double curStage = minY; curStage <= maxY; curStage += stageInc) {
                y = y2pixel(curStage);
                gc.drawLine(x - 5, y, x + 5, y);

                buffer.setLength(0);
                buffer.append(String.format("%6.1f", curStage));
                gc.drawString(buffer.toString(), x - LABEL_OFFSET, y - 7);
            }

            /* Draw the Axis Label. */
            String label = "STAGE IN FEET";

            int yoffset = ((CANVAS_HEIGHT - 10 * label.length()) / 2);
            char[] ca = label.toCharArray();
            for (int i = 0; i < ca.length; i++) {
                gc.drawText(Character.toString(ca[i]), 5, 10 * i + yoffset,
                        true);
            }
        }
    }

    /**
     * Draw the X axis.
     * 
     * @param gc
     *            The GC
     */
    private void drawXAxis(GC gc) {
        gc.setFont(canvasFont);
        gc.setForeground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_WHITE));

        int x = 0;

        /* Set the y position. */
        int y = CANVAS_HEIGHT - BOTTOM_OFFSET;

        /* Draw the horizontal line. */
        gc.drawLine(0, y, canvasWidth - PIXELS_PER_HOUR, y);

        String format = "%02d";
        /* Loop through time range & draw the tick marks. */
        /* Times are 24 hours around the actual range */
        for (long loopTime = zeroTime; loopTime < lastTime; loopTime += HydroConstants.MILLIS_PER_HOUR) {
            Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            c.setTimeInMillis(loopTime);
            gc.drawLine(x, y, x, y + 4);
            if ((c.get(Calendar.HOUR_OF_DAY) % 6) == 0) {
                gc.drawLine(x, y - 4, x, y);

                if (c.get(Calendar.HOUR_OF_DAY) == 0) {
                    /* Draw the hour text */
                    gc.drawString("00Z", x - 5, y + 15);

                    /* Draw the month/day text. */
                    gc.drawString(String.format(format,
                            c.get(Calendar.MONTH) + 1)
                            + "/"
                            + String.format(format, c
                                    .get(Calendar.DAY_OF_MONTH)), x - 19,
                            y + 28);
                } else { /* just a regular 6 hour period */
                    /* Draw the hour text */
                    gc.drawString(c.get(Calendar.HOUR_OF_DAY) + "", x - 5,
                            y + 15);
                }
            }
            
            x += PIXELS_PER_HOUR;
        }
    }

    /**
     * Draw the cross hairs on the graph.
     * 
     * @param gc
     *            The GC
     */
    protected void drawCrossHairs(GC gc) {
        int x1 = LEFT_OFFSET;
        int y1 = currentY;
        int x2 = currentX;
        int y2 = currentY;
        gc.setForeground(parentComp.getDisplay()
                .getSystemColor(SWT.COLOR_WHITE));
        int lineWidth = gc.getLineWidth();
        gc.setLineWidth(1);
        gc.drawLine(x1, y1, x2, y2);

        x1 = currentX;
        y1 = currentY;
        x2 = canvasWidth - PIXELS_PER_HOUR;
        y2 = currentY;
        gc.drawLine(x1, y1, x2, y2);

        x1 = currentX;
        y1 = TOP_OFFSET;
        x2 = currentX;
        y2 = currentY;
        gc.drawLine(x1, y1, x2, y2);

        x1 = currentX;
        y1 = currentY;
        x2 = currentX;
        y2 = CANVAS_HEIGHT - BOTTOM_OFFSET;
        gc.drawLine(x1, y1, x2, y2);

        gc.setLineWidth(lineWidth);
    }
    
    /**
     * Reset the graph canvas
     */
    protected void resetGraphCanvas() {
        getFloodData();

        Date startDate = (Date) floodData.get(0)[1];
        Date endDate = (Date) floodData.get(floodData.size() - 1)[1];
        long start = startDate.getTime() - HydroConstants.MILLIS_PER_DAY;
        long end = endDate.getTime() + HydroConstants.MILLIS_PER_DAY;
        
        TimeZone gmt = TimeZone.getTimeZone("GMT");
        Calendar startCal = Calendar.getInstance();
        startCal.setTimeZone(gmt);
        startCal.setTimeInMillis(start);
        startCal.add(Calendar.HOUR, 1);
        startCal.set(Calendar.MINUTE, 0);
        startCal.set(Calendar.SECOND, 0);
        startCal.set(Calendar.MILLISECOND, 0);

        Calendar endCal = Calendar.getInstance();
        endCal.setTimeZone(gmt);
        endCal.setTimeInMillis(end);
        endCal.set(Calendar.MINUTE, 0);
        endCal.set(Calendar.SECOND, 0);
        endCal.set(Calendar.MILLISECOND, 0);

        this.zeroTime = startCal.getTimeInMillis();
        this.lastTime = endCal.getTimeInMillis();

        int hours = (int) ((lastTime - zeroTime)/HydroConstants.MILLIS_PER_HOUR);
        // 1 hour border at the right
        canvasWidth = hours * PIXELS_PER_HOUR - PIXELS_PER_HOUR;
        if (canvasWidth < CANVAS_WIDTH) {
            canvasWidth = CANVAS_WIDTH - PIXELS_PER_HOUR;
        }
        
        graphAreaRectangle = new Rectangle(1, 0, canvasWidth, CANVAS_HEIGHT
                - BOTTOM_OFFSET);

        graphCanvas.setSize(canvasWidth, CANVAS_HEIGHT);        
        graphCanvas.layout();
        labelCanvas.layout();
        canvasComp.layout();
    }
    
    private void getFloodData() {
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        Map<String, FloodReportData> dataMap = dman.getReportData();
        FloodReportData data = dataMap.get(selectedKey);
        
        floodData = dman.getFloodEventData(data.getLid(), data.getFloodEventId());
    }

    /**
     * Update the stage label with the stage corresponding to the mouse pointer.
     */
    private void updateStageLbl() {
        Date date = pixel2x(currentX);
        String display = sdf.format(date);

        double y = pixel2y(currentY);
        parentDlg.setStageLbl(String.format("Stage:  %6.2f at %s Z", y, display));
    }

    /**
     * Handle the Mouse Move Event.
     * 
     * @param e
     *            The MouseEvent
     */
    private void handleMouseMoveEvent(MouseEvent e) {
        /* Set the cursor location and cursor type */
        Rectangle bounds = graphCanvas.getBounds();
        if (((e.x > 0) && //bounds.contains(e.x, e.y) && 
                (e.y >= 10) && (e.y < CANVAS_HEIGHT - BOTTOM_OFFSET) &&
                (e.x < bounds.width - PIXELS_PER_HOUR))) {

            if (selectedLid != null) {
                graphCanvas.setCursor(crossHairCursor);

                currentX = e.x;
                currentY = e.y;
                
                drawCrossHairs = true;
                graphCanvas.redraw();
                updateStageLbl();
            }
        } else {
            graphCanvas.setCursor(arrowCursor);
            drawCrossHairs = false;
            parentDlg.setStageLbl("Stage:");
            graphCanvas.redraw();
        }
    }
    
    /**
     * Remove the crosshairs when mouse leaves the graph.
     * 
     * @param Event The mouse Event
     */
    private void handleMouseExitEvent(Event e) {
        drawCrossHairs = false;
        graphCanvas.redraw();
    }

    /**
     * Redraw the canvases.
     */
    public void redraw() {
        graphCanvas.redraw();
        labelCanvas.redraw();
    }

    /**
     * @param selectedLid
     *            the selectedLid to set
     */
    public void setSelectedKey(String selectedKey) {
        this.selectedKey = selectedKey;
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        Map<String, FloodReportData> dataMap = dman.getReportData();
        FloodReportData data = dataMap.get(selectedKey);
        selectedLid = data.getLid();
        
        if (data != null) {
            fs = data.getFloodStage();
            floodData = dman.getFloodEventData(selectedLid, data.getFloodEventId());
        }
    }

    protected void dispose() {
        if (canvasFont != null) {
            canvasFont.dispose();
            canvasFont = null;
        }
    }
}
