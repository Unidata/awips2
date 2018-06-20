package gov.noaa.gsd.viz.ensemble.navigator.ui.layer;

import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;

/*
 * Tell a listener if a specific ensemble tool layer has had a 
 * resource change event.
 * 
 * @author polster
 * @author jing
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2015               polster     Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */
public interface IToolLayerChanged {

    public void resourceChanged(EnsembleToolLayer rsc, ChangeType type,
            Object object);

}
