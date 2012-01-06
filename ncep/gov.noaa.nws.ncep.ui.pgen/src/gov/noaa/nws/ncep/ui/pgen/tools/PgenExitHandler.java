/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenExitHandler
 * 
 * 31 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Define a handler to close the PGEN palette view.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/09		#158		s. gilbert  Initial Creation.
 *
 * </pre>
 * 
 * @author	s. gilbert
 * @version	0.0.1
 */
public class PgenExitHandler extends AbstractPgenTool {
	
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    protected void activateTool() {       
        
        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( PgenUtil.VIEW_ID );

        wpage.hideView(vpart);
        
        NmapUiUtils.setPanningMode();
    }

	@Override
	public IInputHandler getMouseHandler() {
		return null;        // no user interaction
	}
    
}