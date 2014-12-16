package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate maximum for different display types.
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
 * Jun 1, 2014    5056        jing     Initial creation
 * 
 * </pre>
 */

public class MaxCalculator extends EnsembleCalculator {
    public MaxCalculator() {
        super(Calculation.MAX);
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

        float max = Float.NaN;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(max)) {
                max = workValue[i];
                continue;
            }
            if (max < workValue[i])
                max = workValue[i];
        }

        return max;
    }
}
