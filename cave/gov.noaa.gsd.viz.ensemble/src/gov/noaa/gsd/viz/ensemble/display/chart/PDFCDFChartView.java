package gov.noaa.gsd.viz.ensemble.display.chart;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;

/**
 * Displays the PDF-CDF charts with PDF-CDF drawable data. Extends the
 * BasicChartView and adds the display features: mean line, median line, member
 * value plotting, Histogram bars, and CDF probabilities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 23, 2015  12301     jing        Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class PDFCDFChartView extends BasicChartView {

    /** The space between two histogram bars */
    private final int BAR_SPACE = 2;

    /** The minimum space labels on Y axis to control label density */
    protected final int Y_AXIS_TICK_LABEL_MIN_INIERVAL = 10;

    /** The minimum space labels on X axis to control label density */
    protected final int X_AXIS_TICK_LABEL_MIN_INIERVAL = 40;

    /** The color for histogram bars */
    protected final Color hisBackgroundColor = Display.getCurrent()
            .getSystemColor(SWT.COLOR_GRAY);

    /** The color for mean line */
    private final Color meanLineColor = Display.getCurrent().getSystemColor(
            SWT.COLOR_DARK_RED);

    /** The color for median line */
    private final Color medianLineColor = Display.getCurrent().getSystemColor(
            SWT.COLOR_DARK_YELLOW);

    /** The mean of one source x data set */
    private double mean;

    /** The median of one source x data set */
    private double median;

    /** The member data to plot against x axis */
    private double[] memberValues;

    /** The minimum of one source x data set */
    private double xMinSource;

    /** The maximum of one source x data set */
    private double xMaxSource;

    /** TODO: Keep the histogram bins for editing size interactively */
    private List<ChartHistBin> binList;

    /** The string format to make PDF-CDF labels on Y axis */
    private java.text.DecimalFormat yPercentFormat = new java.text.DecimalFormat(
            "###%");

    /** The string format to make PDF-CDF labels on X axis */
    private java.text.DecimalFormat xFormat = new java.text.DecimalFormat(
            "###.##");

    /** CDF Reading screen point under mouse cursor */
    private int mX = -9999;

    private int mY = -9999;

    /** The flag whether display the PDF chart */
    private boolean showPDF = true;

    /** The flag whether display the CDF chart */
    private boolean showCDF = true;

    /**
     * The constuctor with the canvas and configuration.
     * 
     * @param canvas
     *            - The canvas to draw the chart
     * @param config
     *            - The chart display configuration
     */
    public PDFCDFChartView(Canvas canvas, ChartConfig config) {
        super(canvas, config);
    }

    public void paint(GC g) {
        Color c = g.getForeground();

        /* Clean the drawing area first */
        clearDrawingArea(g);

        // Have to get the draw area parameters before any drawing. Need not do
        // this if super.point(g) is first.

        getDrawAreaParameters();

        /* Draw histogram bars before other drawings */
        if (config.isHistogramBar() == true && (showPDF || showCDF)) {
            drawHistogramBars(g);
        }

        super.paint(g);

        int saveLineWidth = g.getLineWidth();
        g.setLineWidth(2);

        /* Draw mean line */
        if (config.isMeanLine() && !Double.isNaN(mean)) {
            c = g.getForeground();
            g.setForeground(meanLineColor);

            int screenXmean = getScreenX(mean);
            if (screenXmean >= xGap && screenXmean <= (xGap + windowWidth)) {
                g.drawLine(screenXmean, windowHight, screenXmean, 0);
                g.drawString("Mean", screenXmean, windowHight / 3, true);
            }

        }

        /* Draw median line */
        if (config.isMedianLine() && !Double.isNaN(median)) {
            g.setForeground(medianLineColor);

            int screenXmedian = getScreenX(median);
            if (screenXmedian >= xGap && screenXmedian <= (xGap + windowWidth)) {
                g.drawLine(screenXmedian, windowHight, screenXmedian, 0);
                g.drawString("Median", screenXmedian, windowHight / 2, true);
            }

        }

        g.setLineWidth(saveLineWidth);

        /* Do CDF mouse reading */
        if (!isShowPDF() || config.isMouseReadCDFMove()
                || config.isMouseDownReadCDF()) {
            drawAndReadCdf(g);
        }

        /* Draw members with default color */
        if (this.config.isMembers()) {
            drawMembers(g);
        }

        g.setForeground(c);
    }

    /**
     * Displays pdf histogram bars in the PDF chart or PDF with CDF chart.
     * Displays CDF histogram bars in the CDF chart only.
     * 
     * @param g
     *            - The canvas draw attributes.
     */
    private void drawHistogramBars(GC g) {
        /* Draws PDF bars or CDF bars */
        int index = -1;
        if (showPDF && data.get(0) != null) {
            index = 0; // draw PDF histogram bars
        } else if (showCDF && data.get(1) != null) {
            index = 1;// draw CDF histogram bars
        } else {
            /* No right data to draw histogram bars */
            return;
        }

        /* How many bars */
        int numberOfBars = (data.get(index).getX()).length - 2;
        Color bc = g.getBackground();
        g.setBackground(hisBackgroundColor);// for filling bars

        double xp = xGap;

        /* Finds the minimum and maximum of the source x data */
        xMinSource = StatisticsUtilities.calculateMin(memberValues,
                memberValues.length);
        xMaxSource = StatisticsUtilities.calculateMax(memberValues,
                memberValues.length);

        if ((xMaxSource - xMinSource) < 0.01) {
            /* This can deal with the case xMax - xMin = 0 */
            xMaxSource = xMinSource + 0.01;
        }

        /* Gets the start and end screen x on the X axis */
        int xStartPoint = getScreenX(xMinSource);
        int xEndPoint = getScreenX(xMaxSource);

        int barWidth = (xEndPoint - xStartPoint) / numberOfBars - BAR_SPACE;
        if (barWidth <= 0) {
            barWidth = 1;
        }

        /* Draws the histogram bars */
        double dw = (xEndPoint - xStartPoint) / (double) numberOfBars;
        for (int j = 0; j < numberOfBars; j++) {
            xp = xStartPoint + j * dw + .5;

            double yp = Double.NaN;

            yp = data.get(index).getY(j + 1);
            if (yp == Double.NaN) {
                continue;
            }

            yp = getScreenY(yp);

            Rectangle rect = new Rectangle((int) xp,

            (int) yp, barWidth, windowHight - (int) yp);
            g.fillRectangle(rect);
        }

        /* Sets the color back to its default */
        g.setBackground(bc);
    }

    /**
     * Interactively reads the CDF probabilities of an x value. The x value is
     * specified with the mouse. There are two ways to read the probabilities,
     * one is move mouse continuously read, and another is mouse drag, or left
     * click.
     * 
     * @param g
     *            - The canvas draw attributes.
     */
    public void drawAndReadCdf(GC g) {

        /* Do nothing cases */
        if (!isShowCDF()
                || data.size() != 2 // there is not CDF line
                || data.get(1) == null || !data.get(1).isDrawLine()
                || mX < xGap || mX > (windowWidth + xGap)// mouse is out of x
                                                         // range
                || mY < 0 || mY > windowHight // mouse is out of y range
        ) {
            return;

        }

        int sx = mX;
        if (sx < xGap) {
            sx = xGap;
            mX = xGap;
        }

        double x = (sx - xGap) / cx + axisData.getxMin();
        double y;
        if (data.get(1).getLineFunction() instanceof CubicFunction) {
            y = ((CubicFunction) data.get(1).getLineFunction()).at(x, true);
        } else if (data.get(1).getLineFunction() instanceof BSplineFunction) {
            y = ((BSplineFunction) data.get(1).getLineFunction()).at(x, true);
        } else {
            y = (data.get(1).getLineFunction()).at(x, true);
        }

        /* Sets y value for out of range case */
        if (x < xMinSource) {
            y = 0.0;
        }
        if (x > xMaxSource) {
            y = 1.0;
        }

        int sy = getScreenY(y);

        /* Draws read x-y selection lines */
        if ((itemColors != null) && (itemColors.length >= DATASET_LIMIT)) {
            g.setForeground(itemColors[1]);
        }
        int saveLineStayle = g.getLineStyle();
        g.setLineStyle(SWT.LINE_DASH);
        g.drawLine(xGap, sy, sx, sy);
        g.drawLine(sx, windowHight, sx, sy);
        g.setLineStyle(saveLineStayle);

        /* Makes the probabilities string */
        String readLable = "[" + yPercentFormat.format(y) + ", "
                + xFormat.format(x) + ", " + yPercentFormat.format(1.0 - y)
                + "]";

        if (sx > (windowWidth / 2 + xGap)) {
            sx = sx - g.getFontMetrics().getAverageCharWidth()
                    * readLable.length();
        }
        if (sy < windowHight / 2) {
            sy = sy + 5;
        } else {
            sy = sy - 5;
        }

        /* Draws the probabilities string */
        g.setForeground(defaultDrawColor);
        g.drawString(readLable, sx, sy);
    }

    /**
     * Draws the values of ensemble member on the x axis.
     * 
     * @param g
     *            - The canvas draw attributes.
     */
    private void drawMembers(GC g) {
        if (memberValues == null || memberValues.length < 1) {
            return;
        }

        g.setForeground(meanLineColor);

        for (int i = 0; i < memberValues.length; i++) {

            if (Double.isNaN(memberValues[i])) {
                continue;
            }

            int sx = getScreenX(memberValues[i]);

            g.drawLine(sx, windowHight, sx, windowHight - 5);
        }
    }

    /** The Setters and Getters for some members of this class */
    public boolean isShowPDF() {
        return showPDF;
    }

    public void setShowPDF(boolean showPDF) {
        this.showPDF = showPDF;
    }

    public boolean isShowCDF() {
        return showCDF;
    }

    public void setShowCDF(boolean showCDF) {
        this.showCDF = showCDF;
    }

    public void setMean(double mean) {
        this.mean = mean;
    }

    public void setMedian(double median) {
        this.median = median;
    }

    public void setMemberValues(double[] memberValues) {
        this.memberValues = memberValues;
    }

    public void setmX(int mX) {
        this.mX = mX;
    }

    public void setmY(int mY) {
        this.mY = mY;
    }

    public List<ChartHistBin> getBinList() {
        return binList;
    }

    public void setBinList(List<ChartHistBin> binList) {
        this.binList = binList;
    }

    /**
     * The dispose method of this class.
     */
    public void dispose() {
        super.dispose();
        if (binList != null) {
            binList.clear();
        }
    }
}
