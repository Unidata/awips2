package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * For defining the policy for accessing the model family abstraction and other
 * model family utility conveniences.
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
public interface IModelFamilyProvider {

    public ResolvedModelFamily getActiveModelFamily();

    /*
     * Does the resource belong to the currently selected/visible model source?
     */
    public boolean isResourceInVisibleSource(AbstractVizResource<?, ?> rsc);
}
