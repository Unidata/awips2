package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

/**
 * The element set listener is used to allow the implementer to be informed when
 * a new model family has been requested to be loaded.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2015  12566      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public interface IModelFamilyListener {

    public void addModelFamily(ResolvedModelFamily modelFamily);

}
