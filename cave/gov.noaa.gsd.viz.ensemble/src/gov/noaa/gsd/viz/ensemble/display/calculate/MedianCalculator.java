package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate median for different display types.
 * 
 * The algorithm is referred to ALPS ensemble.
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

public class MedianCalculator extends EnsembleCalculator {
    public MedianCalculator() {
        super(Calculation.MEDIAN);
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
        float[] workValue2 = new float[length];
        int length2 = 0;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(workValue[i]))
                continue;
            workValue2[length2++] = workValue[i];
        }
        if (length2 < 1)
            return Float.NaN;
        if (length2 == 1)
            return workValue2[0];

        int ni = length2 - 1;
        float tempValue;
        for (int i = 0; i < ni; i++) {
            for (int k = i + 1; k < ni; k++) {
                if (workValue[i] < workValue[k])
                    continue;
                tempValue = workValue[i];
                workValue[i] = workValue[k];
                workValue[k] = tempValue;
            }
        }

        if (length % 2 == 0) {
            return (workValue[(length / 2) - 1] + workValue[length / 2]) / 2.0f;
        }
        return workValue[length / 2];

    }
}
