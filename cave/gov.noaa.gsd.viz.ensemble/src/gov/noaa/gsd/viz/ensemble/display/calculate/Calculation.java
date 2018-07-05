package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Define the calculation types as constants used in ensemble display and GUI.
 * 
 * @author polster
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1  2014  5056          polster     Initial creation
 * Jan 12 2016  12301         jing        Added The distribution view
 * Dec 29 2016  19325         jing        Ensemble calculation image Types
 *          </pre>
 * 
 */
public enum Calculation {

    AVG_MINUS_STD_DEV("Avg - 1 Std dev"), //
    AVG_MINUS_STD_DEV_IMAGE("Avg - 1 Std dev Image"), //
    AVG_PLUS_STD_DEV("Avg + 1 Std dev"), //
    AVG_PLUS_STD_DEV_IMAGE("Avg + 1 Std dev Image"), //
    COMBINED_ENS_REL_FREQ("Combined ERF"), //
    COMBINED_ENS_REL_FREQ_IMAGE("Combined ERF Image"), //
    DIFFERENCE("Diff"), //
    DIFFERENCE_IMAGE("Diff Image"), //
    ENSEMBLE_RELATIVE_FREQUENCY("Ens Rel Freq"), //
    ENSEMBLE_RELATIVE_FREQUENCY_IMAGE("Ens Rel Freq Image"), //
    HISTOGRAM_SAMPLING("Histogram Sampling"), //
    HISTOGRAM_GRAPHICS("Distribution Viewer"), //
    MAX("Max"), //
    MAX_IMAGE("Max Image"), //
    MEAN("Mean"), //
    MEAN_IMAGE("Mean Image"), //
    MIN("Min"), //
    MIN_IMAGE("Min Image"), //
    MEDIAN("Median"), //
    MEDIAN_IMAGE("Median Image "), //
    MODE("Mode"), //
    MODE_IMAGE("Mode Image"), //
    NONE("<undefined>"), //
    RANGE("Range"), //
    RANGE_IMAGE("Range Image"), //
    SUMMATION("Sum"), //
    SUMMATION_IMAGE("Sum Image"), //
    STANDARD_DEVIATION("Std dev"), //
    STANDARD_DEVIATION_IMAGE("Std dev Image"), //
    TRIPLET_ENS_REL_FREQ("Triplet ERF"), //
    VALUE_SAMPLING("Sampling"); //

    private String title;

    private Calculation(String t) {
        title = t;
    }

    public String getTitle() {
        return title;
    }
}
