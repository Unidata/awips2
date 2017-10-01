package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * The bin to support the distribution chart calculation. Currently is used in
 * pdf and cdf calculations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 23, 2015   12301       jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class ChartHistBin {

    /** Keeps count of the number of data values within this bin. */
    private int count;

    /** The start boundary. */
    private double startBoundary;

    /** The end boundary. */
    private double endBoundary;

    /**
     * Bin constructor.
     * 
     * @param startBoundary
     *            - the start boundary.
     * @param endBoundary
     *            - the end boundary.
     */
    public ChartHistBin(double startBoundary, double endBoundary) {
        if (startBoundary > endBoundary) {
            this.startBoundary = endBoundary;
            this.endBoundary = startBoundary;
        } else {
            this.startBoundary = startBoundary;
            this.endBoundary = endBoundary;
        }

        this.count = 0;
    }

    /**
     * Gets the number of data values in the bin.
     * 
     * @return The count.
     */
    public int getCount() {
        return this.count;
    }

    /**
     * Increases the count.
     */
    public void increase() {
        this.count++;
    }

    /**
     * Gets the start boundary.
     * 
     * @return The start boundary.
     */
    public double getStart() {
        return this.startBoundary;
    }

    /**
     * Changes the start boundary for supporting bar editing.
     * 
     * @param start
     *            -The start boundary
     */
    public void setStart(double start) {
        /** No change if great the end boundary */
        if (start >= this.endBoundary) {
            return;
        }
        this.startBoundary = start;
    }

    /**
     * Gets the end boundary.
     * 
     * @return The end boundary.
     */
    public double getEnd() {
        return this.endBoundary;
    }

    /**
     * Changes the end boundary for supporting bar editing.
     * 
     * @param end
     *            - The end boundary
     */
    public void setEnd(double end) {
        /** no change if great the end boundary */
        if (end <= this.startBoundary) {
            return;
        }
        this.endBoundary = end;
    }

    /**
     * Returns the bin width.
     * 
     * @return The bin width.
     */
    public double getBinWidth() {
        return this.endBoundary - this.startBoundary;
    }

    /**
     * Returns true if this bin object equals an arbitrary bin object.
     * 
     * @param obj
     *            - The object to test against.
     * 
     * @return true or false.
     */
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (obj == this) {
            return true;
        }
        if (obj instanceof ChartHistBin) {
            ChartHistBin bin = (ChartHistBin) obj;
            boolean b0 = bin.startBoundary == this.startBoundary;
            boolean b1 = bin.endBoundary == this.endBoundary;
            boolean b2 = bin.count == this.count;
            return b0 && b1 && b2;
        }
        return false;
    }

}