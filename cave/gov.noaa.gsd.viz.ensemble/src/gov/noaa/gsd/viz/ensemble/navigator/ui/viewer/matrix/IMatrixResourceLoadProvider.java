package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

/**
 * For defining the policy for loading and unloading resources.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2016  19443      polster     Refactoring for 17.1.1 release
 *
 * </pre>
 *
 * @author polster
 */
public interface IMatrixResourceLoadProvider {

    void loadResources(FieldPlanePair fpp);

    void unloadResources(FieldPlanePair fpp);

}
