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
 * July 1, 2014  5056          polster     Initial creation
 * 
 * </pre>
 * 
 */
public enum Calculation {

    AVG_MINUS_STD_DEV("Avg - 1 Std dev"), //
    AVG_PLUS_STD_DEV("Avg + 1 Std dev"), //
    COMBINED_ENS_REL_FREQ("Combined ERF"), //
    DIFFERENCE("Diff"), //
    ENSEMBLE_RELATIVE_FREQUENCY("Ens Rel Freq"), //
    HISTOGRAM_SAMPLING("Sampling"), //
    HISTOGRAM_TEXT("Histogram Sampling"), //
    MAX("Max"), //
    MEAN("Mean"), //
    MIN("Min"), //
    MEDIAN("Median"), //
    MODE("Mode"), //
    NONE("<undefined>"), //
    RANGE("Range"), //
    SUMMATION("Sum"), //
    STANDARD_DEVIATION("Std dev"), //
    TRIPLET_ENS_REL_FREQ("Triplet ERF"); //

    private String title;

    private Calculation(String t) {
        title = t;
    }

    public String getTitle() {
        return title;
    }
}
