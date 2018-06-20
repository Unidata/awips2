package gov.noaa.gsd.viz.ensemble.control;

/**
 * 
 * Let's the consumer add itself as a listener ro the "new resource registered"
 * event. This event can only come from the <code>EnsembleResourceManager</code>
 * .
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  Feb 23 2016   13211      polster     Initial Creation.
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public interface IResourceRegisteredProvider {

    public void addResourceRegisteredListener(
            IResourceRegisteredListener listener);
}
