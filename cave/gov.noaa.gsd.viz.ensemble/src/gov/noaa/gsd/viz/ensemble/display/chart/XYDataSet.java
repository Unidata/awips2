package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * 
 * A set of point data. The x[] and y[] must be same length. Notes: x[] is
 * limited to monotonic increasing if there is a line function.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 23, 2015  12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class XYDataSet {
    /** Name of the xy data set */
    private String name;

    /** The x values of the points */
    private double x[];

    /** The y values of the points */
    private double y[];

    /**
     * Plots the control points with a marker string. Can implement table chart
     * with this feature, or by extending this class for rich table.
     */
    private String plotMark = "*";

    /** Needs to plot the control points? */
    private boolean isPlotPoints = false;

    /**
     * Creates LingFunction with the control points for drawing line. Check x[]
     * values if they are monotonic increasing before using the function.
     */
    private LineFunction lineFunction;

    /** Needs to draw line? */
    private boolean isDrawLine = false;

    /** The line is increasing only */
    private boolean increasing = false;

    /**
     * Constructor with name and control points.
     * 
     * @param name
     *            - The data set name
     * @param x
     *            - The x values
     * @param y
     *            - The y values
     */
    public XYDataSet(String name, double[] x, double[] y) {
        setData(name, x, y);

    }

    /**
     * Clears the data.
     */
    public void clear() {
        this.name = null;
        this.x = null;
        this.y = null;
    }

    /**
     * Sets the data.
     * 
     * @param name
     *            - The data set name
     * @param x
     *            - The x values
     * @param y
     *            - The y values
     */
    public void setData(String name, double[] x, double[] y) {
        clear();
        if (x != null && y != null && x.length == y.length) {
            this.name = name;
            this.x = x;
            this.y = y;
        }
    }

    /**
     * Gets the data set name.
     * 
     * @return The data set name.
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the data set name.
     * 
     * @param name
     *            - The data name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets all X values of the control points.
     * 
     * @return The control points
     */
    public double[] getX() {
        return x;
    }

    /**
     * Gets the x value with an index.
     * 
     * @param index
     *            - A data index
     * @return The x value
     */
    public double getX(int index) {
        if (x != null && x.length > 0 && index >= 0 && index < x.length) {
            return x[index];
        }
        return Double.NaN;
    }

    /**
     * Sets the x values.
     * 
     * @param x
     *            - The x values.
     */
    public void setX(double[] x) {
        this.x = x;
    }

    /**
     * Gets all y values of the control points.
     * 
     * @return The y values of the control points.
     */
    public double[] getY() {
        return y;
    }

    /**
     * Gets the y value with an index;
     * 
     * @param index
     *            - A y data value index
     * @return The y data value
     */
    public double getY(int index) {
        if (y != null && y.length > 0 && index >= 0 && index < y.length) {
            return y[index];
        }
        return Double.NaN;
    }

    /**
     * Sets y values.
     * 
     * @param y
     *            - The y values
     */
    public void setY(double[] y) {
        this.y = y;
    }

    /**
     * Checks if plot control points is requested.
     * 
     * @return true or false
     */
    public boolean isPlotPoints() {
        return isPlotPoints;
    }

    /**
     * Sets if plot control points is requested.
     * 
     * @param isPlotPoints
     *            - true or false
     */
    public void setPlotPoints(boolean isPlotPoints) {
        this.isPlotPoints = isPlotPoints;
    }

    /**
     * Gets the line function.
     * 
     * @return The line function
     */
    public LineFunction getLineFunction() {
        return lineFunction;
    }

    /**
     * Sets the line function.
     * 
     * @param lineFunction
     *            -The line function
     */
    public void setLineFunction(LineFunction lineFunction) {

        this.lineFunction = lineFunction;
    }

    /**
     * Checks if drawing the line is needed.
     * 
     * @return true or false
     */
    public boolean isDrawLine() {
        return isDrawLine;
    }

    /**
     * Sets if drawing the line is needed.
     * 
     * @param isDrawLine
     *            - true or false
     */
    public void setDrawLine(boolean isDrawLine) {
        this.isDrawLine = isDrawLine;
    }

    /**
     * Gets the plot marker string.
     * 
     * @return The plot marker
     */
    public String getPlotMark() {
        return plotMark;
    }

    /**
     * Sets the plot marker string.
     * 
     * @param plotMark
     *            - The plot marker
     */
    public void setPlotMark(String plotMark) {
        this.plotMark = plotMark;
    }

    /**
     * Checks if the line is increasing only.
     * 
     * @return true if the line is increasing only.
     */
    public boolean isIncreasing() {
        return increasing;
    }

    /**
     * Sets if the line is increasing only.
     * 
     * @param increasing
     *            -true, increasing only.
     */
    public void setIncreasing(boolean increasing) {
        this.increasing = increasing;
    }

}