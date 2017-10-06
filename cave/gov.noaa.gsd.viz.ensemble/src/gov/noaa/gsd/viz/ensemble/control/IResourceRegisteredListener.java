package gov.noaa.gsd.viz.ensemble.control;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * 
 * Let's a consumer know if a resource has been registered with the Ensemble
 * Tool (EnsembleResourceManager).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  Feb 23 2016    13211     polster     Initial Creation.
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public interface IResourceRegisteredListener {

    public void resourceRegistered(AbstractVizResource<?, ?> rsc);
}
