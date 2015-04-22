package gov.noaa.nws.ncep.viz.common.display;

import java.util.List;


// Nothing right now since NcPaneLayout is the only implementation
// but in the future we might have more complex layouts and different
// PaneManagers. We'll wait til then to provide the methods needed in this
// interface.
public interface INcPaneLayout {
	public abstract int getNumberOfPanes(); 
	
	public abstract int getPaneIndex( INcPaneID pid );
	
	public abstract List<INcPaneID> getPaneIDs();
	
	public abstract Boolean containsPaneId( INcPaneID pid );
	
	public abstract int compare( INcPaneLayout playout);

	public abstract INcPaneID createPaneId( int paneIndex );
	
	// the panes/renderable displays area stored in an array.
	// return the index of this paneId in the layout.
//	public abstract int getPaneIndex( IPaneID pid );
}