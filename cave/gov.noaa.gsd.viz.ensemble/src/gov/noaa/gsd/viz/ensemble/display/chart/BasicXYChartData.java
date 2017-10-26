package gov.noaa.gsd.viz.ensemble.display.chart;

import java.util.List;

/**
 * 
 * This class saves basic XY source point data for a chart,and generates
 * drawables to the BasicChartView to display. The methods createViewData and
 * createViewAxisData should be implemented in derived classes. This chart
 * framework uses pattern "chart = chart data + chart view". The BasicChartData
 * and the BasicXYChartData are the top level support classes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 30, 2015    12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public abstract class BasicXYChartData {
    /**
     * The source points data
     */
    protected double[] sourceX;

    protected double[] sourceY;

    /**
     * Constructor
     */
    public BasicXYChartData() {

    }

    /**
     * Sets the source XY points with double arrays.
     * 
     * @param x
     *            - The source x values.
     * @param y
     *            - The source y values.
     */
    public void setData(double[] x, double[] y) {
        this.sourceX = x;
        this.sourceY = y;
    }

    /**
     * Sets the source X values with float list. The PDF and CDF only need the X
     * values.
     * 
     * @param x
     *            values as float list.
     */
    public void setDatax(List<Float> x) {
        this.sourceX = StatisticsUtilities.floatListToDoubleArray(x);
    }

    /**
     * Sets the source XY points with float lists.
     * 
     * @param x
     *            -x values as float list.
     * @param y
     *            -y values as float list.
     */
    public void setDataxy(List<Float> x, List<Float> y) {
        this.sourceX = StatisticsUtilities.floatListToDoubleArray(x);
        this.sourceY = StatisticsUtilities.floatListToDoubleArray(y);

    }

    /**
     * Creates drawables for a chart view in order to display the chart.
     * 
     * @return drawables.
     */
    abstract protected List<XYDataSet> createViewData();

    /**
     * Creates drawable axes data for the chart view in order to display the
     * chart.
     * 
     * @return Axes drawables.
     */
    abstract protected AxesData createViewAxisData();

}
