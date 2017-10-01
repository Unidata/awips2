package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate mean for different display types.
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

public class MeanCalculator extends EnsembleCalculator {
    public MeanCalculator() {
        super(Calculation.MEAN);
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

        float mean = Float.NaN;
        int count = 0;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(workValue[i]))
                continue;
            count++;
            if (Float.isNaN(mean)) {
                mean = workValue[i];

                continue;
            }
            mean += workValue[i];
        }
        if (!Float.isNaN(mean) && count > 0)
            mean = mean / count;

        return mean;

    }

}
