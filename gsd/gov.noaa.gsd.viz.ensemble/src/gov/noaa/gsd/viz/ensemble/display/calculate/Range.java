package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * The Range is used to pass information between GUI and display in the
 * interactive ensemble calculations.
 * 
 * 
 * @author polster
 * 
 *         <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 1, 2014   5056          polster     Initial creation
 * 
 * </pre>
 * 
 */
public class Range {

    private double lowerThreshold = Integer.MIN_VALUE;

    private double higherThreshold = Integer.MAX_VALUE;

    private double onlyThreshold = Double.MIN_NORMAL;

    private RangeType rangeType = RangeType.NONE;

    public Range(RangeType rt) {
        rangeType = rt;
    }

    public void setRange(double lowerValue, double higherValue)
            throws IllegalArgumentException {
        if (lowerValue >= higherValue) {
            throw new IllegalArgumentException(
                    "Low value must be less than high value.");
        }
        lowerThreshold = lowerValue;
        higherThreshold = higherValue;
    }

    public RangeType getRangeType() {
        return rangeType;
    }

    public double getLowerRangeThreshold() {
        return lowerThreshold;
    }

    public double getUpperRangeThreshold() {
        return higherThreshold;
    }

    public void setThreshold(double t) throws IllegalArgumentException {

        if ((rangeType == RangeType.ABOVE_THRESHOLD)
                || (rangeType == RangeType.BELOW_THRESHOLD)) {
            onlyThreshold = t;
        } else {
            throw new IllegalArgumentException(
                    "RangeType requires two arguments.");
        }
    }

    public double getThreshold() {
        return onlyThreshold;
    }

}
