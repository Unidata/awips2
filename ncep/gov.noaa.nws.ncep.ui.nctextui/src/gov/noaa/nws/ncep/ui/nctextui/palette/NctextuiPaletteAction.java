/*
 * gov.noaa.nws.ncep.ui.nctextui.palette.NctextuiPaletteAction
 * 
 * 12/24/2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * @author Chin Chen
 * @version 1.0
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/28/2011    T402       X. Guo     Re-format NCTEXT view panel, check
 *                                     the click action on nctext legend
 */

package gov.noaa.nws.ncep.ui.nctextui.palette;


import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;

import gov.noaa.nws.ncep.ui.nctextui.rsc.NctextuiResource;

public class NctextuiPaletteAction extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException { 
		
		/*
		 *  The viewID string is in the XML file for NCTEXTUI extension point. 
		 */

        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.NCTEXTUI" );

        try {

            if ( vpart == null ){
            	
                vpart = wpage.showView( "gov.noaa.nws.ncep.ui.NCTEXTUI" );
                if (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage() != null) {
                	NctextuiResource nctextMapResource = NctextuiResource.getNctextuiResource();
                	nctextMapResource.setPoints(null);
                }                
            }
            else {
        
                if ( ! wpage.isPartVisible(vpart) ) vpart = wpage.showView( "gov.noaa.nws.ncep.ui.NCTEXTUI" );
                
            }
        }
        catch (Exception e) {
        	
        	e.printStackTrace();
        	
        }
           
		return null;
	}

}
