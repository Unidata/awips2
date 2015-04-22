/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaletteAction
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.view;

 

import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

public class NsharpPaletteAction extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException { 
		
		/*
		 *  The viewID string is in the XML file for NSHARP extension point. 
		 */

        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.nsharp" );

        try {

            if ( vpart == null ){
            	
                vpart = wpage.showView( "gov.noaa.nws.ncep.ui.nsharp" );
                //Chin MERGE moved this here from the NsharpPaletteWindow so we can open the view without an editor.
                if (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage() != null) {
                    NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();
                    nsharpMapResource.setPoints(null);
                }
                
            }
            else {
        
                if ( ! wpage.isPartVisible(vpart) ) vpart = wpage.showView( "gov.noaa.nws.ncep.ui.nsharp" );
                
            }
        }
        catch (Exception e) {
        	
        	e.printStackTrace();
        	
        }
           
		return null;
	}

}
