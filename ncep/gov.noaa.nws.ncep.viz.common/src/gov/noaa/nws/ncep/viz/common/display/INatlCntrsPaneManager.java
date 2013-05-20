package gov.noaa.nws.ncep.viz.common.display;


// This is created in common so that PGEN can determine the displayType of an editor 
// even if the rest of the NCP code is not installed. 
//
public interface INatlCntrsPaneManager {

	public abstract NcDisplayType getDisplayType();
	
	public abstract NcDisplayName getDisplayName();
	
    public abstract INcPaneLayout getPaneLayout();

}
