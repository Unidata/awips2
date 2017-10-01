package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * 
 * Provides related drawable data to draw the X-Y axes of a chart.
 * 
 * After changing xMin, xMax, yMin, yMax, xNPoints or yNPoints, updates the
 * related labels, or auto generates the default labels.
 * 
 * Usually the ChartData prepares an AxesData and passes it into a ChartView.
 * The ChartView uses it to draw the X-Y Axes. There is a default generator for
 * axes Labels depend on the x or y source data set. The AutoBetterScale
 * supports this.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 24, 2015   12301    jing        Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class AxesData {
    /** The minimum value on the X axis */
    private double xMin;

    /** The maximum value on the X axis */
    private double xMax;

    /** The minimum value on the Y axis */
    private double yMin;

    /** The maximum value on the Y axis */
    private double yMax;

    /** The name of the X axis */
    private String xAxisTitle;

    /** The name of the Y axis */
    private String yAxisTitle;

    /** Labels on x axis */
    private String[] xLabels;

    /** Labels on y axis */
    private String[] yLabels;

    /** Locations of the Labels on x axis same number as xLabels */
    private double[] xLabelLocations;

    /** Locations of the Labels on y axis same number as yLabels */
    private double[] yLabelLocations;

    /** How many ticks on x axis, depend on the x data */
    private int xNPoints;

    /** How many ticks on y axis */
    private int yNPoints = 10;

    /** A flag if the x and y range is correct */
    private boolean isRangesCorrect;

    /**
     * Decimal formats for generating default X labels. The "Auto formating" may
     * be added in the AutoBetterScale class to replace the fixed formating.
     */
    private java.text.DecimalFormat format1 = new java.text.DecimalFormat(
            "###.###");

    private java.text.DecimalFormat format2 = new java.text.DecimalFormat(
            "###.E0");

    /**
     * Constructor with basic axes data.
     * 
     * @param xMin
     *            - The minimum of x.
     * @param xMax
     *            - The maximum of x.
     * @param yMin
     *            - The minimum of y.
     * @param yMax
     *            - The maximum of y
     * @param xNPoints
     *            - The number of ticks on x axis.
     * @param yNPoints
     *            - the number of ticks on y axis.
     */
    public AxesData(double xMin, double xMax, double yMin, double yMax,
            int xNPoints, int yNPoints) {
        setAxisData(xMin, xMax, yMin, yMax, xNPoints, yNPoints);
    }

    /**
     * Set the basic axes data and generate the default labels.
     * 
     * @param xMin
     *            - The minimum of x.
     * @param xMax
     *            - The maximum of x.
     * @param yMin
     *            - The minimum of y.
     * @param yMax
     *            - The maximum of y
     * @param xNPoints
     *            - The number of ticks on x axis.
     * @param yNPoints
     *            - the number of ticks on y axis.
     */
    public void setAxisData(double xMin, double xMax, double yMin, double yMax,
            int xNPoints, int yNPoints) {
        if (xMin <= xMax && yMin <= yMax) {
            isRangesCorrect = true;
        } else {
            isRangesCorrect = false;
        }

        this.xMin = xMin;
        this.xMax = xMax;
        this.yMin = yMin;
        this.yMax = yMax;
        this.xNPoints = xNPoints;
        this.yNPoints = yNPoints;

        /* Generate default labels on axes */
        generateDefaultXLabels();
        generateDefaultYLabels();
    }

    /**
     * Generate default X labels and locations.
     */
    private void generateDefaultXLabels() {
        if (!isRangesCorrect) {
            return;
        }

        /*
         * This is for the weather data case , less than 0.01 is not easy for
         * reading
         */

        if ((xMax - xMin) < 0.01) {
            xMax = xMin + 0.01;
        }

        /* With the automatic scale and ticks */
        AutoBetterScale numScale = new AutoBetterScale(xMin, xMax);

        xMin = numScale.getBestMin();
        xMax = numScale.getBestMax() + numScale.getTickInterval();

        xNPoints = (int) ((xMax - xMin) / numScale.getTickInterval() + .5);

        xLabelLocations = new double[xNPoints + 1];
        xLabels = new String[xNPoints + 1];
        for (int i = 0; i <= xNPoints; i++) {
            xLabelLocations[i] = xMin + i * numScale.getTickInterval();
            xLabels[i] = valueToLabel(numScale.getTickInterval(),
                    xLabelLocations[i]);
        }

    }

    /**
     * Generate default Y labels and locations.
     */
    private void generateDefaultYLabels() {
        if (!isRangesCorrect) {
            return;
        }

        /*
         * This is for the weather data case , less the 0.01 is not easy for
         * reading.
         */

        if ((yMax - yMin) < 0.01) {
            yMax = yMin + 0.01;
        }

        /* With the automatic scale and ticks */
        AutoBetterScale numScale = new AutoBetterScale(yMin, yMax);

        yMin = numScale.getBestMin();
        yMax = numScale.getBestMax() + numScale.getTickInterval();
        yNPoints = (int) ((yMax - yMin) / numScale.getTickInterval() + .5);
        yLabelLocations = new double[yNPoints + 1];
        yLabels = new String[yNPoints + 1];
        for (int i = 0; i <= yNPoints; i++) {
            yLabelLocations[i] = yMin + i * numScale.getTickInterval();
            yLabels[i] = valueToLabel(numScale.getTickInterval(),
                    yLabelLocations[i]);
        }

    }

    /**
     * Converts a value to a label string.
     * 
     * TODO: Add nice auto formating later.
     * 
     * @param dv
     *            - The tick interval.
     * @param value
     *            - The value to be converted.
     * @return A label string
     */
    private String valueToLabel(double dv, double value) {

        String label = "";
        if (dv > 1) {
            label = String.valueOf((int) (value + .5));
        } else if (dv >= 0.001) {
            label = format1.format(value);
        } else {
            label = format2.format(value);
        }
        return label;
    }

    /** Getters and setters of the members */

    public double getxMin() {
        return xMin;
    }

    public void setxMin(double xMin) {
        this.xMin = xMin;
    }

    public double getxMax() {
        return xMax;
    }

    public void setxMax(double xMax) {
        this.xMax = xMax;
    }

    public double getyMin() {
        return yMin;
    }

    public void setyMin(double yMin) {
        this.yMin = yMin;
    }

    public double getyMax() {
        return yMax;
    }

    public void setyMax(double yMax) {
        this.yMax = yMax;
    }

    public String getyAxisTitle() {
        return yAxisTitle;
    }

    public void setyAxisTitle(String yAxisTitle) {
        this.yAxisTitle = yAxisTitle;
    }

    public String getxAxisTitle() {
        return xAxisTitle;
    }

    public void setxAxisTitle(String xAxisTitle) {
        this.xAxisTitle = xAxisTitle;
    }

    public String[] getxLabels() {
        return xLabels;
    }

    public void setxLabels(String[] xLabels) {
        this.xLabels = xLabels;
    }

    public String[] getyLabels() {
        return yLabels;
    }

    public void setyLabels(String[] yLabels) {
        this.yLabels = yLabels;
    }

    public double[] getxMarks() {
        return xLabelLocations;
    }

    public void setxMarks(double[] xMarks) {
        this.xLabelLocations = xMarks;
    }

    public double[] getyMarks() {
        return yLabelLocations;
    }

    public void setyMarks(double[] yMarks) {
        this.yLabelLocations = yMarks;
    }

    public boolean isRangesCorrect() {
        return isRangesCorrect;
    }

    public double[] getxLabelLocations() {
        return xLabelLocations;
    }

    public void setxLabelLocations(double[] xLabelLocations) {
        this.xLabelLocations = xLabelLocations;
    }

    public double[] getyLabelLocations() {
        return yLabelLocations;
    }

    public void setyLabelLocations(double[] yLabelLocations) {
        this.yLabelLocations = yLabelLocations;
    }

    public int getxNPoints() {
        return xNPoints;
    }

    public void setxNPoints(int xNPoints) {
        this.xNPoints = xNPoints;
        generateDefaultXLabels();
    }

    public int getyNPoints() {
        return yNPoints;
    }

    public void setyNPoints(int yNPoints) {
        this.yNPoints = yNPoints;
        generateDefaultYLabels();
    }

}