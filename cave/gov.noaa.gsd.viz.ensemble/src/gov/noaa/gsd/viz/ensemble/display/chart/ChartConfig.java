package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * 
 * Holds the configuration of the chart display to control the chart features.
 * It can be extended to support more charts, like time series.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov17, 2015    12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */

public class ChartConfig {

    /** Defines the chart style */
    public enum ChartStyle {
        CHART_PDF_CDF, // both PDF and CDF
        CHART_PDF_ONLY, // PDF only
        CHART_CDF_ONLY, // CDF only
        CHART_DISRTIBUTION, // histogram, bar, line, grid, mid
        CHART_MULTI_DISRTIBUTIONS, // whisker bars, lines, grid, mid
        CHART_WHISKER_DISRTIBUTIONS, // whisker/fill, between lines
        CHART_SLOPE, // points plot, slope line
        CHART_MULTI_SLOPES, // slope lines,color fill between lines
        CHART_WHISKER, // box and whisker plot
        CHART_TIMESERIES;
    }

    /** Defines the frequency types for the histogram */
    public enum HistFrequencyType {
        FREQUENCY_PERCENT, // 0%...100%
        FREQUENCY_RATE, // 0.0 ...1.0
        FREQUENCY_NUMBER// counting

    }

    /** Defines the bin number generating methods */
    public enum BinChooser {
        BIN_5, BIN_7, BIN_9, BIN_11, // 5, 7, 9... selected bin number;
        BIN_ALPS, // from ALPS text histogram
        SQUARE_ROOT, // square of n, but greater than 3,
        STURGES_FORMULA, // log2 n+1
        RISE_RULE // 2 n^(1/3)
        // others...

    }

    /** The flag if the mean line is available */
    private boolean isMeanLine = true;

    /** The flag if the median line is available */
    private boolean isMedianLine = true;

    /** The flag if the grid lines are available */
    private boolean isGridLines = true;

    /** The flag if the histogram bars are available */
    private boolean isHistogramBar = true;

    /** The flag if need to draw distribution lines */
    private boolean isPlotLines = true;

    /** The flag if need to draw the CDF lines */
    private boolean isPlotCDFLine = true;

    /**
     * The flag if "mouse-moving continue reading CDF value" is available. When
     * moving mouse over CDF line, it gets a x value, the y value is generated
     * with the CDF function, then calculates probabilities of less and greater
     * than this x value.
     * 
     */
    private boolean isMouseReadCDFMove = false;

    /**
     * The flag if "reading CDF value with mouse down-moving or clicking" is
     * available.
     */
    private boolean isMouseDownReadCDF = true;

    /**
     * The flag if drawing all the sampled x values of the ensemble members on
     * the x axis to shown the real distribution.
     */
    private boolean isMembers = true;

    /**
     * TODO:The flag if plotting the point markers, to be used in other charts
     * later.
     */
    private boolean isPlotPoints = true;

    /** The bin number object to generate a bin number. Uses BinChooser method. */
    private BinNumber binNumber = new BinNumber();

    /**
     * Initial value for how many labels on the X axis. This number will be
     * regenerated dynamically.
     */
    private int xLabelNumber = 5;

    /** Current frequency type for histogram */
    private HistFrequencyType hisYType = HistFrequencyType.FREQUENCY_PERCENT;

    /**
     * Initial value for how many labels on the Y axis. This number will be
     * regenerated dynamically.
     */
    private int yLabelNumber = 9;

    /** Configured method to generate the bin number */
    private BinChooser binChooser = BinChooser.BIN_7;

    /** The initial default chart style */
    private ChartStyle chartStyle = ChartStyle.CHART_PDF_CDF;

    /**
     * Constructor.
     */
    public ChartConfig() {

    }

    /**
     * The setters and getters to access all members.
     * 
     */

    public boolean isMouseReadCDFMove() {
        return isMouseReadCDFMove;
    }

    public void setMouseReadCDFMove(boolean isMouseReadCDFMove) {
        this.isMouseReadCDFMove = isMouseReadCDFMove;
    }

    public boolean isPlotCDFLine() {
        return isPlotCDFLine;
    }

    public void setPlotCDFLine(boolean isPlotCDFLine) {
        this.isPlotCDFLine = isPlotCDFLine;
    }

    public boolean isMembers() {
        return isMembers;
    }

    public void setMembers(boolean isMembers) {
        this.isMembers = isMembers;
    }

    public BinNumber getBinNumber() {
        return binNumber;
    }

    public void setBinNumber(int binNumber) {
        this.binNumber.setCurrentBinNum(binNumber);
    }

    public void setBinNumber(ChartConfig config, int dataSize) {
        this.binNumber.setCurrentBinNum(config, dataSize);
    }

    public void setBinNumber(ChartConfig config, float[] data) {
        if (config.getBinChoicer() == ChartConfig.BinChooser.BIN_ALPS) {
            this.binNumber.setCurrentBinNum(config, data);
        } else {
            this.binNumber.setCurrentBinNum(config, data.length);
        }
    }

    public int getxLabelNumber() {
        return xLabelNumber;
    }

    public void setxLabelNumber(int xLabelNumber) {
        this.xLabelNumber = xLabelNumber;
    }

    public HistFrequencyType getHisYType() {
        return hisYType;
    }

    public void setHisYType(HistFrequencyType hisYType) {
        this.hisYType = hisYType;
    }

    public int getyLabelNumber() {
        return yLabelNumber;
    }

    public void setyLabelNumber(int yLabelNumber) {
        this.yLabelNumber = yLabelNumber;
    }

    public boolean isMeanLine() {
        return isMeanLine;
    }

    public void setMeanLine(boolean isMeanLine) {
        this.isMeanLine = isMeanLine;
    }

    public boolean isMedianLine() {
        return isMedianLine;
    }

    public void setMedianLine(boolean isMedianLine) {
        this.isMedianLine = isMedianLine;
    }

    public boolean isGridLines() {
        return isGridLines;
    }

    public void setGridLines(boolean isGridLines) {
        this.isGridLines = isGridLines;
    }

    public boolean isHistogramBar() {
        return isHistogramBar;
    }

    public void setHistogramBar(boolean isHistogramBar) {
        this.isHistogramBar = isHistogramBar;
    }

    public boolean isPlotLines() {
        return isPlotLines;
    }

    public void setPlotLines(boolean isPlotLines) {
        this.isPlotLines = isPlotLines;
    }

    public boolean isPlotPoints() {
        return isPlotPoints;
    }

    public void setPlotPoints(boolean isPlotPoints) {
        this.isPlotPoints = isPlotPoints;
    }

    public BinChooser getBinChoicer() {
        return binChooser;
    }

    public void setBinChoicer(BinChooser binChoicer) {
        this.binChooser = binChoicer;
    }

    public ChartStyle getChartStyle() {
        return chartStyle;
    }

    public void setChartStyle(ChartStyle chartStyle) {
        this.chartStyle = chartStyle;
    }

    public boolean isMouseDownReadCDF() {
        return isMouseDownReadCDF;
    }

    public void setMouseDownReadCDF(boolean isMouseDownReadCDF) {
        this.isMouseDownReadCDF = isMouseDownReadCDF;
    }
    
}
