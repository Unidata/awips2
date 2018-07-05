package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import java.util.Set;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix.FieldPlanePairChooserControl.FieldPlanePairControl;

/**
 * For defining the policy for matching resources by field/plane pair.
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
public interface IMatrixResourceMatcher {

    /*
     * Change a set of resources to match the capability of the time basis
     * resource
     */
    public void matchDefaultProductCapability(FieldPlanePairControl fpp);

    /*
     * Change the other resources in the active editor that have the given
     * field/plane pair.
     */
    public void matchProductCapabilityByFieldPlanePair(
            FieldPlanePairControl fppc, Object capabilityClass);

    /*
     * Match the capabilities of the given resource to the associated resources.
     * Make certain the resources are of the same display/product type.
     */
    public void matchProductCapabilityByResource(
            AbstractVizResource<?, ?> matchToRsc,
            Set<AbstractVizResource<?, ?>> assocRscs,
            FieldPlanePairControl fppc);

}
