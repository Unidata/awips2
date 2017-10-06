package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

/***
 * 
 * This interface allows a mechanism to request an update to memory usage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2016   13211     polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public interface IMemoryUsageProvider {

    public void updateMemoryUsage();
}
