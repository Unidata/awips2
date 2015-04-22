/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenExitHandler
 * 
 * 31 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

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
public class PgenExitHandler extends AbstractHandler {
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( PgenUtil.VIEW_ID );

        wpage.hideView(vpart);
        
		return null;
	}
    
}