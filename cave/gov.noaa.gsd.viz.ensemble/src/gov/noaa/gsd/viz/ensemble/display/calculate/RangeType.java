package gov.noaa.gsd.viz.ensemble.display.calculate;

/**
 * Define the range types which are the interactive calculation conditions.
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
 * July 1, 2014   5056       polster     Initial creation
 * 
 * </pre>
 * 
 */
public enum RangeType {
    NONE, // no value
    INNER_RANGE, // low-value <= x && high-value >= x
    OUTER_RANGE, // low-value >= x || high-value <= x
    ABOVE_THRESHOLD, // x >= value
    BELOW_THRESHOLD; // x <= value
}
