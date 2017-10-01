package gov.noaa.gsd.viz.ensemble.display.chart;

import java.util.ArrayList;
import java.util.List;

/**
 * This class uses a simple sampled data set to generate drawables for the
 * PDFCDFChartView, in order to display the PDF and CDF charts. The methods
 * CreateViewdData() and CreateViewAxisData() create the drawable.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2016   12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class PDFCDFChartData extends BasicXYChartData {
    /* The minimum number of bins to limit the bin number */
    private final int MIN_BINS = 3;

    /* The maximum number of bins to limit the bin number */
    private final int MAX_BINS = 50;

    /* The number of ticks on the Y axis */
    private final int Y_AXIS_MARKS = 10;

    /* The PDF-CDF chart view to display prepared drawable */
    protected PDFCDFChartView view;

    /* How many bins for PDF-CDF chart */
    protected int binNum;

    /* Minimum x value on the X axis */
    private double xMin;

    /* Maximum x value on the X axis */
    private double xMax;

    /* Minimum x value in the source x data set */
    private double xMinSource;

    /* Maximum x value in the source x data set */
    private double xMaxSource;

    /**
     * Constructor with the PDF-CDF chart view.
     * 
     * @param view
     *            - The PDF-CDF chart view
     */
    public PDFCDFChartData(PDFCDFChartView view) {

        this.view = view;

        /* The configured bin number */
        binNum = view.getConfig().getBinNumber().getCurrentBinNum();
    }

    /**
     * Adds the single sampled data set, the PDF-CDF chart data.
     * 
     * @param data
     *            - The data
     */
    public void addHistgramData(SingleSampleInfo data) {
        /* Clean the source x values */
        sourceX = null;

        /* No data case, do nothing */
        if (data.getValues() == null || data.getValues().size() < 1) {
            return;
        }

        /* Saves the source x values */
        this.setDatax(data.getValues());

        recreateData();
    }

    /**
     * Re-created the PDF_CDF chart drawables using the source data. It includes
     * the axes drawables data and distributions drawable.
     */
    public void recreateData() {
        /* The configured bin number */
        binNum = view.getConfig().getBinNumber().getCurrentBinNum();
        if (binNum < MIN_BINS || binNum > MAX_BINS) {
            return;
        }

        /* Generated drawable data of the PDF and or CDF */
        view.setChartData("", this.createViewAxisData(), this.createViewData());
    }

    /**
     * Creates the PDF_CDF chart distribution drawables .
     */
    @Override
    protected List<XYDataSet> createViewData() {

        /* Is there any source x data? do nothing if not */
        if (sourceX == null || sourceX.length < 1) {
            return null;
        }
        List<XYDataSet> viewData = new ArrayList<XYDataSet>();

        /* The minimum and maximum of the source x values */
        xMinSource = StatisticsUtilities.calculateMin(sourceX, sourceX.length);
        xMaxSource = StatisticsUtilities.calculateMax(sourceX, sourceX.length);

        if ((xMaxSource - xMinSource) < 0.01) {
            /** This can deal with the case xMax - xMin = 0 */
            xMaxSource = xMinSource + 0.01;
        }

        /*
         * To be certain that the distribution line starts from minimum source x
         * and ends at maximum source x, they are added on the first and last in
         * the xBinValues[]. xBinValues[0] is the minimum source x value,
         * xBinValues[binNum] is median x value of each bin,
         * xBinValues[binNum+1] is the maximum of source x value.
         */
        double[] xBinValues = new double[binNum + 2];
        double dx = (xMaxSource - xMinSource) / binNum;
        xBinValues[0] = xMinSource;
        xBinValues[1] = xBinValues[0] + dx / 2;
        for (int i = 2; i <= binNum; i++) {
            xBinValues[i] = xBinValues[i - 1] + dx;
        }
        xBinValues[binNum + 1] = xMaxSource;

        /* Gets bin data for calculating the pdf or cdf */
        List<ChartHistBin> bins = calculateBins();

        /* PDF and CDF chart data calculation */
        double[] yPdf = new double[binNum + 2];
        double[] yCdf = new double[binNum + 2];
        calculatePdfCdfRelativeFrequency(yPdf, yCdf, bins);

        /* Sets up PDF view data */
        XYDataSet pdf = new XYDataSet("pdf", xBinValues, yPdf);
        if (view.isShowPDF()) {
            pdf.setDrawLine(true);
            pdf.setLineFunction(new CubicFunction(xBinValues, yPdf));

            // TODO: turn on control-point plotting. pdf.setPlotPoints(true);

            viewData.add(pdf);
        } else {
            viewData.add(null);
        }

        /* Sets up CDF view drawable data */
        XYDataSet cdf = new XYDataSet("cdf", xBinValues, yCdf);
        if (view.isShowCDF()) {
            cdf.setDrawLine(true);
            cdf.setIncreasing(true);
            cdf.setLineFunction(new CubicFunction(xBinValues, yCdf));
            viewData.add(cdf);
        } else {
            viewData.add(null);
        }

        /* Sets member values, the inputed source data */
        view.setMemberValues(sourceX);

        /* The mean value for the mean line */
        view.setMean(StatisticsUtilities.calculateMean(sourceX));

        /* The median value for the median line */
        view.setMedian(StatisticsUtilities.calculateMedian(sourceX));

        return viewData;
    }

    /**
     * Creates axes drawable data for PDF-CDF chart view.
     */
    @Override
    protected AxesData createViewAxisData() {

        /* Is there any source x data? */
        if (sourceX == null || sourceX.length < 1) {
            return null;
        }

        /*
         * Minimum and maximum x data values to define the range of x axis,
         * which are adjustable depending upon how to display labels of x axis
         */

        xMin = StatisticsUtilities.calculateMin(sourceX, sourceX.length);
        xMax = StatisticsUtilities.calculateMax(sourceX, sourceX.length);

        /* Minimum and maximum y axis values */
        double yMin = 0.0;
        double yMax = 1.0;

        /* Tick mark number on the x and y axes */
        int xNPoints = binNum;
        int yNPoints = Y_AXIS_MARKS;

        /*
         * Builds the axes drawable. The Labels on x axis will be created too,
         * depending upon the bins and source.
         */

        AxesData axisData = new AxesData(xMin, xMax, yMin, yMax, xNPoints,
                yNPoints);

        /** Labels on the y axis */
        String[] yLabels = { "0%", "20%", "40%", "60%", "80%", "100%" };
        double[] yLocations = { 0.0, 0.2, 0.4, 0.6, 0.8, 1.0 };
        axisData.setyLabels(yLabels);
        axisData.setyLabelLocations(yLocations);

        return axisData;
    }

    /**
     * Calculates the bins with source x data and configuration.
     * 
     * @return The bin list
     */
    private List<ChartHistBin> calculateBins() {
        double[] xData = new double[binNum];

        double binWidth = (xMaxSource - xMinSource) / binNum;
        xData[0] = xMinSource + binWidth / 2;
        for (int i = 1; i < binNum; i++) {
            xData[i] = xData[i - 1] + binWidth;
        }

        double lower = xMinSource;
        double upper;

        /* Counts bins */
        List<ChartHistBin> binList = new ArrayList<ChartHistBin>(binNum);

        for (int i = 0; i < binNum; i++) {
            ChartHistBin bin;

            /*
             * Make sure last bin's upper boundary ends at maximum to avoid the
             * rounding issue. The first bin's lower boundary is guaranteed to
             * start from valid minimum.
             */

            if (i == binNum - 1) {
                bin = new ChartHistBin(lower, xMaxSource);
            } else {
                upper = xMinSource + (i + 1) * binWidth;
                bin = new ChartHistBin(lower, upper);
                lower = upper;
            }
            binList.add(bin);
        }

        /* Fills the bins with the source x values */
        for (int i = 0; i < sourceX.length; i++) {
            if (Double.isNaN(sourceX[i])) {
                continue;
            }

            int binIndex = binNum - 1;
            if (sourceX[i] < xMaxSource) {
                double fraction = (sourceX[i] - xMinSource)
                        / (xMaxSource - xMinSource);
                if (fraction < 0.0) {
                    fraction = 0.0;
                }
                binIndex = (int) (fraction * binNum);

                if (binIndex >= binNum) {
                    binIndex = binNum - 1;
                }
            }
            ChartHistBin bin = (ChartHistBin) binList.get(binIndex);
            bin.increase();
        }

        /*
         * TODO: pass the bin list to support 'bin size' editing in the view.
         * view.setBinList(binList);
         */

        return binList;
    }

    /**
     * Calculates the PDF-CDF relative frequencies with the source x values and
     * bin list.
     * 
     * @param yPdf
     *            - The PDF relative frequencies.
     * @param yCdf
     *            - The CDF relative frequencies.
     * @param binList
     *            -The bin list.
     */
    private void calculatePdfCdfRelativeFrequency(double[] yPdf, double[] yCdf,
            List<ChartHistBin> binList) {

        /* Data number of the source x (input) data */
        int sourceNum = 0;
        for (double x : sourceX) {
            if (!Double.isNaN(x)) {
                sourceNum++;
            }
        }
        if (sourceNum == 0) {
            return;
        }

        /* Calculates PDF values */
        yPdf[0] = 0.0;
        for (int i = 0; i < binList.size(); i++) {
            yPdf[i + 1] = binList.get(i).getCount() / (double) sourceNum;
        }

        yPdf[binList.size() + 1] = 0.0;

        /* Calculate CDF values */
        yCdf[0] = 0.0;
        int countC = 0;
        for (int i = 0; i < binList.size(); i++) {
            countC += binList.get(i).getCount();
            yCdf[i + 1] = countC / (double) sourceNum;

        }

        yCdf[binList.size() + 1] = yCdf[binList.size()];
    }

}