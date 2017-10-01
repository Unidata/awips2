package gov.noaa.gsd.viz.ensemble.display.chart;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;

/**
 * 
 * Implement the basic chart view of a data set to display an x-y chart (may
 * work for tables too). It draws the axes, X-Y lines, and plots string markers
 * for each control point.
 * 
 * Most features are configurable.
 * 
 * This chart framework uses the pattern "chart = chart data + chart view", the
 * BasicChartData and the BasicXYChartData are the top level classes to support
 * this pattern.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2015    12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class BasicChartView {

    /* Axes gap to define space for drawing tick marks, labels and name */
    protected final int AXES_GAP = 20;

    /* Limit the data set to 23 in this version */
    protected final int DATASET_LIMIT = 23;

    /*
     * The minimum tick interval on the Y axis, is used to automatically adjust
     * the y label density.
     */
    protected final int Y_AXIS_TICK_LABEL_MIN_INIERVAL = 15;

    /*
     * The minimum tick interval on the X axis is used to automatically adjust
     * the x label density.
     */
    protected final int X_AXIS_TICK_LABEL_MIN_INIERVAL = 45;

    protected final int DEFAULT_LEGEND_SPACE = 0;

    /* The x or y screen value as empty screen value */
    protected final int NONE_SCREEN_VALUE = -99999;

    /* The default draw color */
    protected final Color defaultDrawColor = Display.getCurrent()
            .getSystemColor(SWT.COLOR_BLACK);

    /* The color to draw the mean line */
    protected final Color meanLineColor = Display.getCurrent().getSystemColor(
            SWT.COLOR_DARK_RED);

    /* The color to draw the median line */
    protected final Color medianLineColor = Display.getCurrent()
            .getSystemColor(SWT.COLOR_DARK_YELLOW);

    /* The color to draw the grid lines */
    protected final Color gridLineColor = Display.getCurrent().getSystemColor(
            SWT.COLOR_WHITE);

    /*
     * Application colors. They need to be disposed. Limited to 23 colors in
     * this version.
     */
    protected final Color[] itemColors = new Color[] {
            new Color(Display.getDefault(), 90, 90, 255), // blue
            // new Color(Display.getDefault(), 0, 240, 0), // green
            new Color(Display.getDefault(), 50, 150, 50), // dark green
            new Color(Display.getDefault(), 255, 0, 0), // red
            new Color(Display.getDefault(), 0, 255, 255), // cyan
            new Color(Display.getDefault(), 255, 80, 255), // magenta
            new Color(Display.getDefault(), 200, 200, 0), // yellow
            new Color(Display.getDefault(), 200, 150, 0), // brown
            new Color(Display.getDefault(), 150, 255, 150), // light green
            new Color(Display.getDefault(), 200, 80, 80), // dark red
            new Color(Display.getDefault(), 30, 150, 150), // dark cyan
            new Color(Display.getDefault(), 200, 200, 255), // light blue
            new Color(Display.getDefault(), 0, 120, 0), // dark green
            new Color(Display.getDefault(), 255, 150, 150), // lighter red
            new Color(Display.getDefault(), 140, 80, 140), // dark magenta
            new Color(Display.getDefault(), 150, 100, 50), // brown
            new Color(Display.getDefault(), 255, 80, 80), // light red
            new Color(Display.getDefault(), 200, 200, 200), // light grey
            new Color(Display.getDefault(), 255, 200, 80), // orange
            new Color(Display.getDefault(), 255, 255, 80), // pale yellow
            new Color(Display.getDefault(), 255, 200, 200), // pale red
            new Color(Display.getDefault(), 255, 200, 255), // pale magenta
            new Color(Display.getDefault(), 255, 255, 200), // pale pale yellow
            new Color(Display.getDefault(), 200, 255, 255), // pale pale blue
    };

    /*
     * XY dataset list to draw chart lines, plot labels, and points. It is
     * prepared by ChartData. The XYDataSet is the drawable data.
     */
    protected List<XYDataSet> data;

    /* Drawable data for axes, prepared by a ChartData. */
    protected AxesData axisData;

    /* The chart drawing area */
    protected Canvas canvas;

    /* The configuration of the chart display */
    protected ChartConfig config = null;

    /**
     * Screen parameters for drawing.
     * 
     */

    /* The space for drawing tick marks, labels, and the name of the X axis */
    protected int xGap;

    /* The space for drawing tick marks, labels, and the name of the Y axis */
    protected int yGap;

    /*
     * The 'windowHeight x windowWidth' is the available window area for the
     * chart (not including axes) .
     */
    protected int windowHight;

    protected int windowWidth;

    /* Coefficient for converting a x value to screen x */
    protected double cx;

    /* Coefficient for converting a y value to screen y */
    protected double cy;

    /* The legend space on the right of drawing area */
    protected int legendSpace;

    /* Name of the chart is on the top, and is optional */
    protected String title = "";

    /*
     * The flag if the process is still painting; is used to control the
     * updating frequency.
     */
    protected boolean isPainting = false;

    /**
     * Constructor
     */
    public BasicChartView() {

    }

    /**
     * Check if still is painting.
     * 
     * @return
     */
    public boolean isPainting() {
        return isPainting;
    }

    /**
     * Set if still is painting.
     * 
     * @param isPainting
     */
    public void setPainting(boolean isPainting) {
        this.isPainting = isPainting;
    }

    /**
     * Constructor with canvas and configuration.
     * 
     * @param canvas
     *            - The canvas to draw chart
     * @param config
     *            - The chart configuration
     */
    public BasicChartView(Canvas canvas, ChartConfig config) {
        this.config = config;
        this.canvas = canvas;

    }

    /**
     * Constructor with all arguments
     * 
     * @param canvas
     *            -The canvas to draw chart
     * @param config
     *            - The chart configuration
     * @param title
     *            - chart name
     * @param axesData
     *            - data to draw axes
     * @param data
     *            - data to draw axes lines, marks, labels...
     */
    public BasicChartView(Canvas canvas, ChartConfig config, String title,
            AxesData axesData, List<XYDataSet> data) {
        setChartData(title, axesData, data);
    }

    /**
     * TODO: Reserved for future use.
     * 
     * Sets data to draw the chart.
     * 
     * @param title
     *            - chart name
     * @param axisData
     *            - data to draw axes
     * @param data
     *            - data to draw axes lines, marks, labels...
     */
    public void setChartData(String title, AxesData axisData,
            List<XYDataSet> data) {

        if (data == null) {
            return;
        }

        this.data = data;
        this.axisData = axisData;
        this.title = title;

        /* Checks whether the drawable data is correct. */
        if (!isReadyForPaint()) {

            /*
             * TODO: May need to recalculate the axes data or log error.
             * Currently there is no need for this.
             */

        }

    }

    /**
     * Returns chart configuration.
     * 
     * @return configuration.
     */
    public ChartConfig getConfig() {
        return config;
    }

    /**
     * Checks whether the drawable data is correct. Return true if correct.
     */
    public boolean isReadyForPaint() {
        if (this.config == null && this.canvas == null && this.axisData == null
                && !this.axisData.isRangesCorrect() && this.data == null
                && this.data.isEmpty()) {
            return false;
        }

        return true;
    }

    /**
     * Paint the basic chart.
     * 
     * @param g
     *            -GC object to control draw attributes.
     */
    public void paint(GC g) {
        if (!isReadyForPaint()) {
            return;
        }

        getDrawAreaParameters();

        drawAxes(g);

        Color c = g.getForeground();

        if (this.config.isPlotLines()) {
            drawLinesAndPlotPoints(g);
        }

        g.setForeground(c);
    }

    /**
     * Draws lines and plots markers,
     * 
     * @param g
     *            -GC object to control draw attributes.
     */
    public void drawLinesAndPlotPoints(GC g) {
        int dataNum = data.size();
        if (dataNum > DATASET_LIMIT) {
            dataNum = DATASET_LIMIT;
        }

        int saveLineWidth = g.getLineWidth();
        g.setLineWidth(2);

        for (int i = 0; i < dataNum; i++) {

            g.setForeground(itemColors[i]);
            XYDataSet dataI = data.get(i);
            if (dataI == null) {
                continue;
            }

            int x0 = 0, x1 = 0;
            int y0 = 0, y1 = 0;

            /* Draw lines with a line function */
            if (dataI.isDrawLine() && dataI.getLineFunction() != null
                    && dataI.getX().length > 1) {
                int nPoints = dataI.getX().length * 3;

                double xMax = StatisticsUtilities.calculateMax(dataI.getX(),
                        dataI.getX().length);
                double xMin = StatisticsUtilities.calculateMin(dataI.getX(),
                        dataI.getX().length);
                double dx = (xMax - xMin) / nPoints;

                for (int j = 0; j <= nPoints; j++) {

                    double x = xMin + j * dx;

                    double y;
                    if (dataI.getLineFunction() instanceof CubicFunction) {
                        y = ((CubicFunction) dataI.getLineFunction()).at(x,
                        // false);
                                dataI.isIncreasing());
                    } else if (dataI.getLineFunction() instanceof BSplineFunction) {
                        y = ((BSplineFunction) dataI.getLineFunction()).at(x,
                        // false);
                                dataI.isIncreasing());
                    } else {
                        y = (dataI.getLineFunction()).at(x, false);
                    }

                    x1 = getScreenX(x);

                    y1 = getScreenY(y);

                    if (j > 0) {
                        g.drawLine(x0, y0, x1, y1);
                    }

                    x0 = x1;
                    y0 = y1;

                }
            }

            /* Plot points and path */
            for (int j = 0; j < dataI.getX().length; j++) {

                x1 = getScreenX(dataI.getX()[j]);
                y1 = getScreenY(dataI.getY()[j]);

                /* Plot marker points */
                if (dataI.isPlotPoints()) {

                    g.drawString(dataI.getPlotMark(), x1 - 5, y1 - 5);
                }

                /* Plot path if there is no line function */
                if (dataI.isDrawLine() && dataI.getLineFunction() == null
                        && j > 0) {
                    g.drawLine(x0, y0, x1, y1);
                }

                x0 = x1;
                y0 = y1;

            }

        }

        g.setLineWidth(saveLineWidth);
    }

    /**
     * Draw axes with the drawable data.
     * 
     * @param g
     *            -GC object to control draw attributes.
     */
    public void drawAxes(GC g) {
        /* Draw the X axis line */
        g.drawLine(xGap, windowHight, windowWidth + xGap, windowHight);

        /* Draw the Y axis line */
        g.drawLine(yGap, windowHight, yGap, 0);

        /* Draw tick marks on X axis */
        int sxp = 0;
        int sxp0 = 0;

        double dw = (double) windowWidth / (double) axisData.getxNPoints();

        for (int i = 0; i <= axisData.getxNPoints(); i++) {

            sxp = (int) (xGap + i * dw + .5);

            /* x axis tick markers */
            g.drawLine(sxp, windowHight, sxp, windowHight + 5);

            /* x grid lines */
            if (this.config.isGridLines() && i != 0) {
                Color saveColor = g.getForeground();
                g.setForeground(gridLineColor);
                g.drawLine(sxp, windowHight, sxp, 0);
                g.setForeground(saveColor);
            }

        }

        /* Draw labels on x axis */
        if (axisData.getxLabels() != null && axisData.getxLabels().length > 0
                && axisData.getxLabelLocations() != null
                && axisData.getxLabelLocations().length > 0) {

            /*
             * TODO: Add x label resolution control according to the x data
             * value. For example add sub-ticks, and more labels when window
             * size is increased.
             */
            for (int i = 0; i < axisData.getxLabels().length; i++) {

                sxp = getScreenX(axisData.getxLabelLocations()[i]);

                /* Control label density */
                if (i != 0 && i != axisData.getxLabels().length
                        && (sxp - sxp0) < X_AXIS_TICK_LABEL_MIN_INIERVAL) {

                    continue;
                }

                if (sxp > (xGap + windowWidth)) {
                    continue;
                }
                sxp0 = sxp;

                g.drawString(axisData.getxLabels()[i], sxp - 5, windowHight + 5);
            }
        }

        /* Draw tick marks on y axis */
        int syp = 0;
        int syp0 = 0;

        double dh = (double) windowHight / (double) axisData.getyNPoints();

        for (int i = 0; i <= axisData.getyNPoints(); i++) {
            syp = (int) (i * dh + .5);

            /* y axis tick markers */
            g.drawLine(yGap, windowHight - syp, yGap - 5, windowHight - syp);

            /* y grid lines */
            if (this.config.isGridLines() && i != 0) {
                Color saveColor = g.getForeground();
                g.setForeground(gridLineColor);
                g.drawLine(yGap, windowHight - syp, windowWidth + yGap,
                        windowHight - syp);
                g.setForeground(saveColor);

            }

        }

        /* Draw labels on y axis */
        if (axisData.getyLabels() != null && axisData.getyLabels().length > 0
                && axisData.getyLabelLocations() != null
                && axisData.getyLabelLocations().length > 0) {

            /*
             * TODO: Add y label resolution control according to the y data
             * value. For example add sub-ticks, and more labels when window
             * size is increased.
             */

            syp = 0;
            syp0 = 0;
            for (int i = 0; i < axisData.getyLabels().length; i++) {

                syp = getScreenY(axisData.getyLabelLocations()[i]);

                /* TODO: Control the label density. */
                //
                // if (i != 0 && i != axisData.getyLabels().length && (syp0 -
                // syp) < Y_AXIS_TICK_LABEL_MIN_INIERVAL) {
                //
                // continue; }
                //
                syp0 = syp;

                g.drawString(axisData.getyLabels()[i], 0, syp - 2);

            }
        }
    }

    /**
     * Projects an x value onto screen x.
     * 
     * @param x
     *            - x value to be converted.
     * @return screen x value.
     */
    protected int getScreenX(double x) {
        if (Double.isNaN(x)) {
            return NONE_SCREEN_VALUE;
        }

        return (int) ((x - axisData.getxMin()) * cx + xGap + .5);
    }

    /**
     * Projects a y value onto screen y. Real screen y is windowHight -
     * getScreenY(double y).
     * 
     * @param y
     *            - y value to be converted.
     * @return The screen y value.
     */
    protected int getScreenY(double y) {
        if (Double.isNaN(y)) {
            return NONE_SCREEN_VALUE;
        }

        return (int) (windowHight - (y - axisData.getyMin()) * cy + .5);
    }

    /**
     * Cleans the draw area to erase any graphics before drawing new things.
     * 
     * @param g
     *            -GC object to control draw attributes.
     */
    public void clearDrawingArea(GC g) {

        Color saveColor = g.getForeground();
        g.setForeground(g.getBackground());
        g.fillRectangle(canvas.getBounds());
        g.setForeground(saveColor);

    }

    /**
     * Prepares parameters for axes calculations.
     */
    protected void getDrawAreaParameters() {
        Rectangle d = getSize();

        xGap = 2 * AXES_GAP;
        yGap = 2 * AXES_GAP;
        legendSpace = DEFAULT_LEGEND_SPACE;

        if ((this.config.isPlotLines())) {
            legendSpace = 3 * DEFAULT_LEGEND_SPACE;
        }

        windowHight = d.height - AXES_GAP;
        /* TODO: an Option be used later */
        // windowWidth = d.width - (3 * gap + legendSpace);
        windowWidth = d.width - legendSpace;

        cx = windowWidth / (axisData.getxMax() - axisData.getxMin());
        cy = windowHight / (axisData.getyMax() - axisData.getyMin());
    }

    /**
     * Get height and width of the canvas
     * 
     * @return
     */
    private Rectangle getSize() {
        return canvas.getBounds();

    }

    /**
     * Dispose processing.
     */
    public void dispose() {

        for (Color c : itemColors) {
            c.dispose();
        }

        data.clear();
    }

}
