package gov.noaa.gsd.viz.ensemble.display.chart;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Canvas;

/**
 * 
 * This class implements Probability Density Function(PDF)and Cumulative
 * Distribution Function(CDF) charts. It support three chart styles, the PDF
 * only, CDF only and PDF-CDF styles.
 * 
 * The source input data is a single sampled data set from all ensemble member
 * grids having the same level and same unit.
 * 
 * PDFCDFChart = PDFCDFChartData + PDFCDFChartView
 * 
 * It implements the {@link IChart}, has a PDFCDFChartData which extends BasicChartData
 * and PDFCDFChartView which extends BasicXYChartData.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2016  12301         jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class PDFCDFChart implements IChart {
    /**
     * The chart data to covert the sampled data (source data) into PDF and CDF
     * drawable data.
     */
    protected PDFCDFChartData chartData;

    /** The chart view to display the CDF and PDF charts with drawable data */
    protected PDFCDFChartView chartView;

    /**
     * Constructor with the canvas and chart configuration.
     * 
     * @param canvas
     *            -The canvas
     * @param config
     *            - The configuration
     */
    public PDFCDFChart(Canvas canvas, ChartConfig config) {
        this.chartView = new PDFCDFChartView(canvas, config);
        this.chartData = new PDFCDFChartData(this.chartView);

        /* Sets to the default to PDF and CDF */
        this.chartView.setShowCDF(true);
        this.chartView.setShowPDF(true);

    }

    /**
     * Passes a simple sampled data set into the PDF-CDF chart data for
     * processing.
     * 
     * @param data
     *            - The single sampled data
     */
    public void setData(SingleSampleInfo data) {
        this.chartData.addHistgramData(data);
    }

    /**
     * Handles chart configuration change by re-creating the drawable for the
     * chart view.
     * 
     */
    @Override
    public void configChanged() {
        this.chartData.recreateData();
    }

    /**
     * Cleans the PDF and/or CDF chart.
     */
    @Override
    public void clearData() {
        chartData.setData(null, null);
    }

    /**
     * Draws the PDF and/or CDF chart.
     */
    @Override
    public void paint(GC g) {
        this.chartView.paint(g);
    }

    /**
     * Gets the PDF-CDF chart data.
     * 
     * @return
     */
    public PDFCDFChartData getChartData() {
        return chartData;
    }

    /**
     * Gets the PDF-CDF chart view.
     */
    @Override
    public PDFCDFChartView getChartView() {
        return chartView;
    }

    /**
     * Dispose the chart if needed, currently not need, since there is one chart
     * object only.
     */
    @Override
    public void dispose() {

        chartView.dispose();
    }

}
