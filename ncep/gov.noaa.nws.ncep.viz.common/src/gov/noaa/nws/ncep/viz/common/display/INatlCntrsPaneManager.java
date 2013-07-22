package gov.noaa.nws.ncep.viz.common.display;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayName.NcPaneName;


// This is created in common so that PGEN can determine the displayType of an editor 
// even if the rest of the NCP code is not installed. 
//
public interface INatlCntrsPaneManager {

	public abstract NcDisplayType getDisplayType();
	
	public abstract NcDisplayName getDisplayName();
	
    public abstract INcPaneLayout getPaneLayout();

	public abstract IPaneLayoutable getPane( INcPaneID pid );	
}
