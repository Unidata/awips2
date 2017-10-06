package gov.noaa.gsd.viz.ensemble.display.calculate;

import java.util.Arrays;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

/**
 * Value of Relative Frequency of an ensemble set. The range values for the
 * calculation, such as minimum, maximum... are in the GUI, should be passed in.
 * The definition of the minimum, maximum is the condition, "minimum < maximum"
 * as the input for the REF. The concept is from AWIPS I ALPS ensemble GUI.
 * 
 * In the current Ensemble Tool, the GUI use Range, which is passed in and
 * converted into the "minimum < maximum" condition.
 * 
 * @author jing
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 2, 2014    5056        jing     Initial creation
 * 
 * </pre>
 */

public class ERFCalculator extends EnsembleCalculator {

    /**
     * None value flag
     */
    public final static double MAX_OR_MIN_NO_VALUE = -999999.0;

    /**
     * Minimum value of the condition from GUI
     */
    private double min = 0.25;

    /**
     * Maximum value of the condition from GUI
     */
    private double max = 0.0;

    // Segregation Strategies can be default, altSREF, ekdmos25, ekdmos10
    private String SegregationStrategies = "default";

    /**
     * Range description
     */
    private String rangeDescription = null;

    /**
     * Range from GUI
     */
    private Range range = null;

    /**
     * Set the Range description.
     * 
     * @param rd
     *            - Range description
     */
    private void setRangeDescription(String rd) {
        rangeDescription = rd;
    }

    /**
     * Get the Range description.
     * 
     * @return - the Range description.
     */
    public String getRangeDescription() {
        return rangeDescription;
    }

    /**
     * Initial the ERF calculator by processing the ERF condition.
     * 
     * @param r
     *            - rang to be pass in when constructing the object
     */
    public ERFCalculator(Range r) {

        // Set the Calculation flag
        super(Calculation.ENSEMBLE_RELATIVE_FREQUENCY);

        resultUnit = NonSI.PERCENT;

        range = r;

        min = Double.MIN_VALUE;
        max = Double.MAX_VALUE;

        // Convert the range to the minimum and maximum and process the
        // condition for ERF
        if ((range.getRangeType() == RangeType.INNER_RANGE)
                || (range.getRangeType() == RangeType.OUTER_RANGE)) {
            min = range.getLowerRangeThreshold();
            max = range.getUpperRangeThreshold();

            if (range.getRangeType() == RangeType.INNER_RANGE) {

                setRangeDescription("ERF >" + min + " & <" + max);
            } else {

                setRangeDescription("ERF <" + min + " or >" + max);
            }
        } else if (range.getRangeType() == RangeType.ABOVE_THRESHOLD) {
            min = MAX_OR_MIN_NO_VALUE;
            max = range.getThreshold();
            setRangeDescription("ERF >" + max);
        } else if (range.getRangeType() == RangeType.BELOW_THRESHOLD) {
            min = range.getThreshold();
            max = MAX_OR_MIN_NO_VALUE;
            setRangeDescription("ERF <" + min);
        } else {
            min = MAX_OR_MIN_NO_VALUE;
            max = MAX_OR_MIN_NO_VALUE;
            setRangeDescription("ERF Median ");
        }

    }

    /**
     * The range unit is data display unit entering bu user. Do unit matching by
     * converting range into the data unit if they are different.
     */
    public void matchUnit() {
        min = Double.MIN_VALUE;
        max = Double.MAX_VALUE;

        // Convert the range to the minimum and maximum.
        if ((range.getRangeType() == RangeType.INNER_RANGE)
                || (range.getRangeType() == RangeType.OUTER_RANGE)) {
            min = range.getLowerRangeThreshold();
            max = range.getUpperRangeThreshold();

        } else if (range.getRangeType() == RangeType.ABOVE_THRESHOLD) {
            min = MAX_OR_MIN_NO_VALUE;
            max = range.getThreshold();
        } else if (range.getRangeType() == RangeType.BELOW_THRESHOLD) {
            min = range.getThreshold();
            max = MAX_OR_MIN_NO_VALUE;
        } else {
            min = MAX_OR_MIN_NO_VALUE;
            max = MAX_OR_MIN_NO_VALUE;
        }

        if (dispUnit != null && dataUnit != null && !dispUnit.equals(dataUnit)) {

            if (min != MAX_OR_MIN_NO_VALUE) {
                min = dispUnit.getConverterTo(dataUnit).convert(min);
            }
            if (max != MAX_OR_MIN_NO_VALUE) {
                max = dispUnit.getConverterTo(dataUnit).convert(max);
            }

        }
    }

    public Range getRange() {
        return range;
    }

    /**
     * Do ERF calculation for one point.
     * 
     * (non-Javadoc)
     * 
     * @see gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator#calculatePoint(float[],
     *      int)
     */
    @Override
    protected float calculatePoint(float[] workValue, int length) {

        float[] poitValues = new float[length];
        int totalNum = 0;

        // Sum of the data set
        for (int i = 0; i < length; i++) {
            if (!Float.isNaN(workValue[i])) {
                poitValues[totalNum] = workValue[i];
                totalNum++;
            }

        }

        // What is the ERF of this point

        // Do nothing if no data
        if (totalNum == 0) {
            return Float.NaN;
        }

        // How many data match with the conditions?
        int matchedNum = 0;

        /**
         * The median case, current is not functioned. Need do more proccessing
         * later, such as unit rematching at hiher level.
         */
        // jing float median = 0;
        if (this.min == MAX_OR_MIN_NO_VALUE && this.max == MAX_OR_MIN_NO_VALUE) {
            float median = getMedian(poitValues);
            // Most close to mean
            setRangeDescription("ERF Median ");
            return median;
        }

        for (int k = 0; k < totalNum; k++) {
            if (range.getRangeType() == RangeType.ABOVE_THRESHOLD
                    && poitValues[k] > this.max) { // great or above case
                matchedNum++;
            } else if (range.getRangeType() == RangeType.BELOW_THRESHOLD
                    && poitValues[k] < this.min) { // less or below case
                matchedNum++;
            } else if (range.getRangeType() == RangeType.INNER_RANGE
                    && poitValues[k] >= this.min && poitValues[k] <= this.max) { // inter
                matchedNum++;
            } else if (range.getRangeType() == RangeType.OUTER_RANGE
                    && (poitValues[k] < this.min || poitValues[k] > this.max)) { // outer
                matchedNum++;

            }

        }

        // ERF of this point in %
        float erf = (Float) (matchedNum * 1.0f / totalNum);
        erf = (float) Math.round(erf * 100);

        return erf;

    }

    /**
     * Calculate the median of a data set.
     * 
     * @param a
     *            - A data set as the input data.
     * @return
     */
    private float getMedian(float[] a) {
        float[] b = new float[a.length];
        System.arraycopy(a, 0, b, 0, b.length);
        Arrays.sort(b);

        if (a.length % 2 == 0) {
            return (b[(b.length / 2) - 1] + b[b.length / 2]) / 2.0f;
        } else {
            return b[b.length / 2];
        }

    }

    @Override
    public String getName() {

        StringBuilder cobbledName = new StringBuilder();
        if (getCalculation() == Calculation.ENSEMBLE_RELATIVE_FREQUENCY) {
            cobbledName.append(calculationType.getTitle()).append(" ")
                    .append(getRangeDescription());
        }
        return cobbledName.toString();
    }
}
