package gov.noaa.gsd.viz.ensemble.display.distribution;

import gov.noaa.gsd.viz.ensemble.display.chart.ChartConfig;
import gov.noaa.gsd.viz.ensemble.display.chart.IChart;
import gov.noaa.gsd.viz.ensemble.display.chart.PDFCDFChart;
import gov.noaa.gsd.viz.ensemble.display.chart.SingleSampleInfo;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.GC;

/**
 * 
 * Displays the charts in the Distribution view.
 * 
 * It keeps an available chart list for selecting. There are only three chart
 * styles supported by the PDFCDFChart class in the current release.
 * 
 * The package gov.noaa.gsd.ensemble.display.chart is a basic framework to
 * implement the charts we need.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2015  12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */

public class DistributionDisplay {
    /**
     * A list of charts with different styles. This release provides PDF, CDF
     * and PDF-CDF chart styles only. The PDFCDFChart class supports these three
     * styles
     */
    private List<IChart> charts;

    /** Current selected ChartStyle for quick identifying chart */
    private ChartConfig.ChartStyle currentChartStyle;

    /** Current selected chart */
    private IChart currentChart;

    /**
     * The distribution View GUI refers to the configuration object and drawing
     * area.
     */
    private DistributionViewUI ui;

    /**
     * Constructor
     * 
     * @param ui
     *            - The Distribution view GUI
     */
    public DistributionDisplay(DistributionViewUI ui) {

        this.ui = ui;
        charts = new ArrayList<>();

        /* Constructs a PDFCDFChart object when initialing */
        currentChart = new PDFCDFChart(ui.getChartCanvas(), ui.getConfig());

        /* Initials the chart style to PDF_CDF */
        currentChartStyle = ChartConfig.ChartStyle.CHART_PDF_CDF;

        charts.add(currentChart);

        currentChartStyle = ChartConfig.ChartStyle.CHART_PDF_CDF;
    }

    /**
     * Action after changing the chartStyle.
     * 
     * @param selectedChart
     *            - The new selected ChartStyle
     */
    public void changeChartStyle(ChartConfig.ChartStyle selectedChart) {

        // Cleans the old chart if there is more then one chart object.

        if (currentChart != null && charts.size() > 1) {
            charts.remove(currentChart);
            currentChart.dispose();
        }

        // Checks if the chart is in the chart list. Construct the selected
        // chart if there is none. Sets the current chart as selected.

        IChart chart = findChart(selectedChart);

        if (chart == null) {
            /* Creates a chart object with the selected chart style */
            chart = new PDFCDFChart(ui.getChartCanvas(), ui.getConfig());
            charts.add(chart);
        }
        setupPDFCDFChart(((PDFCDFChart) chart), selectedChart);

        currentChart = chart;
        currentChartStyle = selectedChart;
    }

    /**
     * Returns the current chart.
     * 
     * @return The chart
     */
    public IChart getChart() {
        return currentChart;
    }

    /**
     * Generates a single sample information data set to the example chart to
     * initial the GUI. Also used for setting up special cases. Very useful for
     * developing new chart solutions and bug fixing.
     * 
     * @return single sample information data set.
     */
    public static SingleSampleInfo makeTestraphicsHistogramInfo() {

        List<Float> values = new ArrayList<Float>();
        values.add((float) 566);
        values.add((float) 567.0);
        values.add((float) 577.0);
        values.add((float) 556);
        values.add((float) 567.0);
        values.add((float) 570.0);
        SingleSampleInfo info = new SingleSampleInfo("GEFS Hight ",
                "89.4W 45.9N \n(Example Chart)", "500mb", "dam", values);

        return info;
    }

    /**
     * Displays chart with a data set.
     * 
     * @param data
     *            - The single sample information data set.
     */
    public void displayChart(SingleSampleInfo data) {

        /** Sets the chart title on the GUI */
        String chartStyle = "PDF-CDF:";
        if (currentChartStyle == ChartConfig.ChartStyle.CHART_CDF_ONLY) {
            chartStyle = "CDF:";

        } else if (currentChartStyle == ChartConfig.ChartStyle.CHART_PDF_ONLY) {
            chartStyle = "PDF:";

        } else if (currentChartStyle == ChartConfig.ChartStyle.CHART_PDF_CDF) {
            chartStyle = "PDF-CDF:";

        }

        ui.setDistributionTitle(chartStyle + data.getLevel() + " "
                + data.getUnit() + " " + data.getLocation()
                + "                     ");

        ui.getChartCanvas().setVisible(true);

        /* Clears the draw area */
        currentChart.clearData();

        /* Do nothing if no data is available */
        if (data == null || data.getValues() == null
                || data.getValues().size() == 0) {
            return;
        }

        /* Updates bin number with the data set and chart configuration */
        if (ui.getConfig().getBinChoicer() == ChartConfig.BinChooser.BIN_ALPS) {
            ui.getConfig().setBinNumber(ui.getConfig(),
                    data.getValuesFloatArray());
        } else {
            ui.getConfig()
                    .setBinNumber(ui.getConfig(), data.getValues().size());
        }

        // Updates chart data. The Chart object is currently only of type
        // PDFCDFChart

        ((PDFCDFChart) currentChart).setData(data);

        /* Draws the chart in the chart window */
        GC g = new GC(ui.getChartCanvas());
        currentChart.paint(g);
        g.dispose();
    }

    /**
     * Paints the current chart.
     * 
     * @param g
     *            - A drawing attributes
     */
    public void paint(GC g) {
        currentChart.paint(g);
    }

    /**
     * Clears the current chart.
     * 
     */
    public  void  clearDistributionViewer() {
        ui.setDistributionTitle("No data.                                        ");
        currentChart.clearData();
        ui.getChartCanvas().setVisible(false);

    }

    /**
     * Searches a chart object supporting the selected chart style.
     * 
     * @param selectedChart
     *            - The selected chart style
     * @return The chart object supporting the selected chart style. return null
     *         if can't find one.
     */
    private IChart findChart(ChartConfig.ChartStyle selectedChart) {
        if (charts == null || charts.size() == 0) {
            return null;
        }

        for (IChart chart : charts) {

            // For PDFCDFChart, set up Flags. The Chart_CDF_ONLY, Chart_PDF_ONLY
            // and Chart_PDF_CDF share PDFCDFChart. It's the only chart
            // implementation in the first release, more charts will be added
            // later.

            if (chart instanceof PDFCDFChart) {

                if (selectedChart == ChartConfig.ChartStyle.CHART_CDF_ONLY
                        || selectedChart == ChartConfig.ChartStyle.CHART_PDF_ONLY
                        || selectedChart == ChartConfig.ChartStyle.CHART_PDF_CDF) {
                    setupPDFCDFChart((PDFCDFChart) chart, selectedChart);
                    return chart;
                }
            }

        }
        return null;
    }

    /**
     * Sets up a PDFCDFChart object with specified style which can be
     * CHART_CDF_ONLY, CHART_PDF_ONLY or CHART_PDF_CDF.
     * 
     * @param pDFCDFChart
     *            - A PDFCDFChart object.
     * @param selectedChart
     *            - The specified chart style.
     */
    private void setupPDFCDFChart(PDFCDFChart pDFCDFChart,
            ChartConfig.ChartStyle selectedChart) {
        pDFCDFChart.getChartView().setShowCDF(
                selectedChart != ChartConfig.ChartStyle.CHART_PDF_ONLY);
        pDFCDFChart.getChartView().setShowPDF(
                selectedChart != ChartConfig.ChartStyle.CHART_CDF_ONLY);

    }

    /**
     * Releases the resources.
     */
    public void dispose() {
        for (IChart chart : charts) {
            chart.getChartView().dispose();
            chart.dispose();
        }
        charts.clear();
        currentChart = null;

    }

    synchronized public void setViewEditable(boolean enabled) {
        /**
         * TODO: Need to reflect the enabled/disabled state in the
         * look-and-feel.
         */
    }

}
