package gov.noaa.nws.ncep.viz.common.display;


/**
 * MapRenderableDisplay for NatlCntrs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/19/13      #972      ghull        Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public interface IPaneLayoutable {

    public abstract INcPaneID getPaneId();
    
    public abstract void setPaneId( INcPaneID pid);
}
