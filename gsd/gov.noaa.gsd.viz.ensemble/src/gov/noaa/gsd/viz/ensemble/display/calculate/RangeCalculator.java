package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate range.
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
 * Jun 1, 2014    5056       jing     Initial creation
 * 
 * </pre>
 * 
 */

public class RangeCalculator extends EnsembleCalculator {
    public RangeCalculator() {
        super(Calculation.RANGE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator#calculatePoint
     * (float[], int)
     */
    @Override
    protected float calculatePoint(float[] workValue, int length) {
        if (workValue == null || length < 1)
            return Float.NaN;

        float range = Float.NaN;

        float min = Float.NaN;
        float max = Float.NaN;
        for (int i = 0; i < length; i++) {

            if (Float.isNaN(workValue[i]))
                continue;

            if (Float.isNaN(max) && Float.isNaN(min)) {
                max = min = workValue[i];
                continue;
            }

            if (max < workValue[i])
                max = workValue[i];
            if (min > workValue[i])
                min = workValue[i];

        }
        if (Float.isNaN(max) || Float.isNaN(min)) {
            range = Float.NaN;
        } else {
            range = max - min;
        }

        return range;
    }

}
