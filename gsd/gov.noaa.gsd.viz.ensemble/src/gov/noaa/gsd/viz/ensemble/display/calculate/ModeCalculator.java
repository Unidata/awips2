package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Calculate mode for different display types. The algorithm is referred to ALPS
 * ensemble developed by James Ramer. It's good for some products in AWIPS, but
 * isn't generic. We are checking on other algorithms such as in GEMPAK, to
 * determine the final one in later release.
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
 * Jun 1, 2014   5056         jing     Initial creation
 * 
 * </pre>
 */

public class ModeCalculator extends EnsembleCalculator {
    public ModeCalculator() {
        super(Calculation.MODE);
    }

    @Override
    protected float calculatePoint(float[] workValue, int length) {

        float mode = Float.NaN;
        float vmax = Float.MIN_VALUE;
        float vmin = Float.MAX_VALUE;
        int nok = 0;
        for (int i = 0; i < length; i++) {
            if (Float.isNaN(workValue[i]))
                continue;
            nok++;
            if (workValue[i] > vmax)
                vmax = workValue[i];
            if (workValue[i] < vmin)
                vmin = workValue[i];
        }

        if (nok == 0)
            return mode;
        if (vmax <= vmin)
            return vmax;

        short[] counts = new short[144];
        float vstep = (vmax - vmin) / 11;
        vmin -= vstep;
        vstep /= 11;
        int nmax = 0, icen;
        for (int j = 0; j < length; j++) {
            if (Float.isNaN(workValue[j]))
                continue;
            icen = (int) ((workValue[j] - vmin) / vstep);
            for (int i = icen - 5; i <= icen + 5; i++) {
                counts[i]++;
                if (counts[i] > nmax)
                    nmax = counts[i];
            }
        }

        int i1b, i2b;
        i1b = i2b = 0;
        for (int i1 = 0; i1 < 144; i1++) {
            if (counts[i1] < nmax)
                continue;
            for (int i2 = i1 + 1; counts[i1] >= nmax; i2++) {
                if (i2 - i1 < i2b - i1b)
                    continue;
                i2b = i2;
                i1b = i1;
            }
        }
        mode = (float) ((i2b + i1b) * 0.5 * vstep + vmin);

        return mode;
    }
}
