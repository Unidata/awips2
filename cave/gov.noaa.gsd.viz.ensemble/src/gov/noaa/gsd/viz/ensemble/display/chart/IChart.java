package gov.noaa.gsd.viz.ensemble.display.chart;

import org.eclipse.swt.graphics.GC;

/**
 * 
 * The interface of chart. We developed a generic chart framework for the charts
 * of this release and future charts as well. The package
 * gov.noa.gsd.ensemble.display.chart is reusable and extensible, will be
 * improved and enhanced gradually when (e.g. one by one) new charts are added
 * on. The chart uses the pattern "chart = chart data + chart view". The
 * BasicChartData and the BasicXYChartData are the top level classes.
 * 
 * A typical example is the PDFCDFChart:
 * 
 * PDFCDFChart = PDFCDFChartData + PDFCDFChartView
 * 
 * It implements the IChart, has a PDFCDFChartData which extends BasicChartData
 * and PDFCDFChartView which extends BasicXYChartData.
 * 
 * Similarly, we can implement a Multiple Distribution Chart as:
 * 
 * MultiDistributionChart = MultiDistributionChartData +
 * MultiDistributionChartView
 * 
 * Generic charting tools, such as the JfreeChat, were considered, but they are
 * too big and too complicated, and bring too much overhead into AWIPS II. Its
 * features and interfaces may not be sufficient for this project. So we
 * developed our own simple chart frame work.
 * 
 * The package is decoupled from any baseline CAVE code, and depends only on
 * SWT(Eclipse) and Java, so can easily be used across CAVE perspectives, even
 * outside A,2 to develop chart applications.
 * 
 * The package gov.noa.gsd.ensemble.display.distribution is a application of the
 * gov.noa.gsd.ensemble.display.chart package.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2016   12301       jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public interface IChart {

    /**
     * Called when the chart configuration is changed. It includes the
     * re-calculating and re-painting.
     */
    public void configChanged();

    /**
     * Cleans the draw area before updating.
     */
    public void clearData();

    /** Paints the chart */
    public void paint(GC g);

    /** Returns the Chart View */
    public BasicChartView getChartView();

    /** Disposes the chart if needed. */
    public void dispose();
}
