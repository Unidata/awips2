package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;

/**
// This is implemented by resources that can provide/create 
// a predefined area. The geometry is created by an INcAreaProvider but this
// will define which one and how to map the resource name to the names of the 
// areas.
//    This is also used as a capabilityInterface for the contextualMenu ext pt so that any
// AbstractRequestableResource implementing this will have legend menus for 
// "Zoom To Size Of Image" and "Change Area to Fit Image" 
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  05/14/2013    #862       Greg Hull   Initial Creation.
 *  
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public interface IAreaProviderCapable {

	abstract public AreaSource getSourceProvider();
	
	abstract public String getAreaName();
	
}
