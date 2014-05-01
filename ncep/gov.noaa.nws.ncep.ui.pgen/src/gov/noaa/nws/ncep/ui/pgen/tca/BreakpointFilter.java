/*
 * BreakpointFilter
 * 
 * Date created 23 OCTOBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

/**
 * This Filter is used when searching for tropical cyclone breakpoints.
 * Users can set the filter to accept "official" breakpoints only or breakpoints only 
 * part of a specified coastline. 
 * @author sgilbert
 *
 */
public class BreakpointFilter {

	private boolean official;
	private String coastName;
	private BreakpointManager bmgr;
	
	/**
	 * Default constructor - no breakpoints are filtered
	 */
	public BreakpointFilter(){
		bmgr = BreakpointManager.getInstance();
		official = false;
		coastName = null;
	}
	
	/**
	 * Set the filter to accept only "official" breakpoints.
	 */
	public void setOfficialOnly() {
		official = true;
	}
	
	/**
	 * Set the filter to accept breakpoints belonging to the given coast.
	 * @param name Coast name
	 */
	public void filterCoastName(String name){
		coastName = name;
	}
	
	/**
	 * Determine if the given breakpoint should be filtered out.
	 * @param bkpt Breakpoint to test
	 * @return true, if breakpoint is not filtered out.
	 */
	public boolean isAccepted( Breakpoint bkpt ) {
		
		if ( official && ! bkpt.isOfficial() ) {
			return false;
		}
		
		if ( coastName != null ){
			if ( ! bmgr.findCoastName(bkpt).equals(coastName)) {
				return false;
			}
		}
		
		return true;
	}
	
}
