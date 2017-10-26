package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * Automatically scales one axis based on an original minimum and maximum.
 * Figures out the best tick interval, using the minimum and maximum data
 * values. The optimal numbers, to create ticks and labels on the axis, are
 * decimal 1, 2 and 5 and multiple powers of ten.
 * 
 * This is an implementation of the algorithm "Nice Labels for Graphing".
 * 
 * TODO: The "Auto formating" may be added in the this class to replace the
 * fixed formating.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 24, 2015   12301         jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class AutoBetterScale {
    /** The default maximum number of ticks */
    private final double MAX_TICKS_NUMBER = 10;

    /** The original minimum */
    private double minOrig;

    /** The original maximum */
    private double maxOrig;

    /** The maximum number of ticks */
    private double maxTicks;

    /** The optimal tick interval */
    private double tickInterval;

    /** The range */
    private double range;

    /** The optimal minimum */
    private double betterMin;

    /** The optimal maximum */
    private double betterMax;

    /**
     * Constructor with the original minimum and maximum of an axis.
     * 
     * @param minOrig
     *            - The original minimum
     * @param maxOrig
     *            - The original maximum
     */
    public AutoBetterScale(double minOrig, double maxOrig) {
        this.minOrig = minOrig;
        this.maxOrig = maxOrig;
        this.maxTicks = MAX_TICKS_NUMBER;
        findBetterScaleAndInterval();
    }

    /**
     * Uses the original minimum and maximum to figure out the optimal tick
     * interval.
     */
    private void findBetterScaleAndInterval() {
        this.range = betterNum(maxOrig - minOrig, false);
        this.tickInterval = betterNum(range / (maxTicks - 1), true);
        this.betterMin = Math.floor(minOrig / tickInterval) * tickInterval;
        this.betterMax = Math.ceil(maxOrig / tickInterval) * tickInterval;
    }

    /**
     * Finds the best rounded numbers nearest to the original range ticks. This
     * is better for human readability.
     * 
     * @param range
     *            - The range.
     * @param round
     *            - the flag if round.
     * @return The better number
     */
    private double betterNum(double range, boolean round) {
        double exponent;
        double fraction;
        double betterFraction;
        exponent = Math.floor(Math.log10(range));
        fraction = range / Math.pow(10, exponent);

        if (round) {
            if (fraction < 1.5) {
                betterFraction = 1;
            } else if (fraction < 3) {
                betterFraction = 2;
            } else if (fraction < 7) {
                betterFraction = 5;
            } else {
                betterFraction = 10;
            }
        } else {
            if (fraction <= 1) {
                betterFraction = 1;
            } else if (fraction <= 2) {
                betterFraction = 2;
            } else if (fraction <= 5) {
                betterFraction = 5;
            } else {
                betterFraction = 10;
            }
        }

        return betterFraction * Math.pow(10, exponent);
    }

    /**
     * Sets the maximum tick number.
     * 
     * @param maxTicks
     */
    public void setTicks(double maxTicks) {
        this.maxTicks = maxTicks;
        findBetterScaleAndInterval();
    }

    /**
     * Sets the original minimum and maximum for reusing the object.
     * 
     * @param minOrig
     *            - The original minimum.
     * @param maxOrig
     *            - The original maximum.
     */
    public void setMinMaxPoints(double minOrig, double maxOrig) {
        this.minOrig = minOrig;
        this.maxOrig = maxOrig;
        findBetterScaleAndInterval();
    }

    /**
     * Gets the optimal interval.
     * 
     * @return -the optimal interval.
     */
    public double getTickInterval() {
        return tickInterval;
    }

    /**
     * Get the optimal minimum.
     * 
     * @return The optimal minimum.
     */
    public double getBestMin() {
        return betterMin;
    }

    /**
     * Get the optimal maximum.
     * 
     * @return The optimal maximum.
     */
    public double getBestMax() {
        return betterMax;
    }

}
