package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate sum.
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

public class SumCalculator extends EnsembleCalculator {
    public SumCalculator() {
        super(Calculation.SUMMATION);
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

        float sum = 0;

        for (int i = 0; i < length; i++) {
            // This solution is for same GridGeometry grids
            if (!Float.isNaN(workValue[i]))
                sum += workValue[i];

        }

        return sum;
    }
}
