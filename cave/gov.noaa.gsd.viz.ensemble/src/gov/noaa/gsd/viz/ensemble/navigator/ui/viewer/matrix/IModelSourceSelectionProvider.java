package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

/***
 * 
 * This interface allows a listener to be notified when a model source is
 * selected.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2016  12371      polster     Initial creation
 * Dec 01, 2017   41520     polster     Renamed class ModelSources to ModelSourceKind
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public interface IModelSourceSelectionProvider {

    ModelSourceKind getSelected();
}
