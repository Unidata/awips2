package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener4;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;

public class NsharpPerspectiveListener implements IPerspectiveListener4{
	private NsharpResourceHandler rscHandler;
	private String myPerspectiveId;
	@Override
	public void perspectiveOpened(IWorkbenchPage page,
			IPerspectiveDescriptor perspective) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void perspectiveClosed(IWorkbenchPage page,
			IPerspectiveDescriptor perspective) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void perspectiveDeactivated(IWorkbenchPage page,
			IPerspectiveDescriptor perspective) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void perspectiveSavedAs(IWorkbenchPage page,
			IPerspectiveDescriptor oldPerspective,
			IPerspectiveDescriptor newPerspective) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void perspectiveChanged(IWorkbenchPage page,
			IPerspectiveDescriptor perspective,
			IWorkbenchPartReference partRef, String changeId) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void perspectiveActivated(IWorkbenchPage page,
			IPerspectiveDescriptor perspective) {
		if(perspective.getId().equals(myPerspectiveId))
			rscHandler.repopulateSndgData();
	}

	@Override
	public void perspectiveChanged(IWorkbenchPage page,
			IPerspectiveDescriptor perspective, String changeId) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void perspectivePreDeactivate(IWorkbenchPage page,
			IPerspectiveDescriptor perspective) {
		// TODO Auto-generated method stub
		
	}

	public void setRscHandler(NsharpResourceHandler rscHandler) {
		this.rscHandler = rscHandler;
	}

	public void setMyPerspectiveId(String myPerspectiveId) {
		this.myPerspectiveId = myPerspectiveId;
	}
	
	
}