package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate STDDEV for different display types. The algorithm is referred to
 * ALPS ensemble.
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
 */

public class StddevCalculator extends EnsembleCalculator {
    public StddevCalculator() {
        super(Calculation.STANDARD_DEVIATION);
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

        float stddev;

        int countJ = 0;
        float sum = 0;
        float scr0 = 0;
        for (int i = 0; i < length; i++) {

            if (!Float.isNaN(workValue[i])) {
                countJ++;
                sum += workValue[i];
                scr0 += workValue[i] * workValue[i];

            }
        }

        if (countJ == 0) {
            stddev = Float.NaN;
        } else {
            sum /= countJ;
            scr0 = scr0 / countJ - sum * sum;
            stddev = scr0 > 0 ? (float) Math.sqrt(scr0) : 0;
        }

        return stddev;
    }

}
