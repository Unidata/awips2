/*
 * edu.ucar.unidata.ui.ncradarui.palette.NcradaruiPaletteAction
 * 
 * 12/24/2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * @author Chin Chen
 * @version 1.0
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/28/2011    T402       X. Guo     Re-format NCRADAR view panel, check
 *                                     the click action on nctext legend
 */

package edu.ucar.unidata.ui.ncradarui.palette;


import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;

import edu.ucar.unidata.ui.ncradarui.rsc.NcradaruiResource;


public class NcradaruiPaletteAction extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException { 
		
		/*
		 *  The viewID string is in the XML file for NCRADARUI extension point. 
		 */

        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( "edu.ucar.unidata.ui.NCRADARUI" );

        try {

            if ( vpart == null ){
            	
                vpart = wpage.showView( "edu.ucar.unidata.ui.NCRADARUI" );
                if (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage() != null) {
                	NcradaruiResource ncradarMapResource = NcradaruiResource.getNcradaruiResource();
                	ncradarMapResource.setPoints(null);
                }                
            }
            else {
        
                if ( ! wpage.isPartVisible(vpart) ) vpart = wpage.showView( "edu.ucar.unidata.ui.NCRADARUI" );
                
            }
        }
        catch (Exception e) {
        	
        	e.printStackTrace();
        	
        }
           
		return null;
	}

}
