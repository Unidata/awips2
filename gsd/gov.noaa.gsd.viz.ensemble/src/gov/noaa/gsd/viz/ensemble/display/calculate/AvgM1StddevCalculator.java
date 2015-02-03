package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate mean - std dev for different display types. The algorithm is
 * referred to ALPS ensemble.
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
 * Jun 1, 2014   5056        jing     Initial creation
 * 
 * 
 * 
 * </pre>
 */

public class AvgM1StddevCalculator extends EnsembleCalculator {
    public AvgM1StddevCalculator() {
        super(Calculation.AVG_MINUS_STD_DEV);
    }

    /**
     * Do the "mean - std dev" calculation.
     * 
     * (non-Javadoc)
     * 
     * @see gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator#calculatePoint(float[],
     *      int)
     */

    @Override
    protected float calculatePoint(float[] workValue, int length) {

        int count = 0;
        float result = Float.NaN;
        float sum = 0;
        float scr0 = 0;
        /**
         * Prepare the sum, sum by sum, and how many data available.
         */
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(workValue[i])) {
                continue;
            }
            count++;
            sum += workValue[i];
            scr0 = workValue[i] * workValue[i];

        }

        if (count == 0) {
            return result;
        }

        /**
         * What is the "mean - std dev".
         */
        sum /= count;
        scr0 = scr0 / count - sum * sum;
        result = (scr0 > 0) ? (float) (sum - Math.sqrt(scr0)) : sum;

        return result;
    }
}
