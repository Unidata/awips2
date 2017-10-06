package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate minimum for different display types.
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

public class MinCalculator extends EnsembleCalculator {
    public MinCalculator() {
        super(Calculation.MIN);
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

        float min = Float.NaN;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(min)) {
                min = workValue[i];
                continue;
            }
            if (min > workValue[i])
                min = workValue[i];
        }

        return min;
    }
}
