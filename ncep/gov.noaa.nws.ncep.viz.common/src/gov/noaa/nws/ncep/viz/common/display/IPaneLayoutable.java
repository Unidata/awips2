package gov.noaa.nws.ncep.viz.common.display;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayName.NcPaneName;


/**
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

	// used for RBDs and editors to let the pane (Renderable Display) objects
	// have access to the container.
	// could use IDisplayPaneContainer but there is more stuff in there that isn't applicable
	// to RBDs.
	//
//	public static interface INcPaneContainer {
//		public abstract IPaneLayoutable getPane( INcPaneID pid );	
//		
//	}
	
    public abstract INcPaneID getPaneId();
    
    public abstract void setPaneId( INcPaneID pid);
    
	public abstract NcPaneName getPaneName();
	
//    public abstract INcPaneContainer getPaneContainer();
    public abstract INatlCntrsPaneManager getPaneManager();
    
    public abstract void setPaneManager( INatlCntrsPaneManager pm );
}
