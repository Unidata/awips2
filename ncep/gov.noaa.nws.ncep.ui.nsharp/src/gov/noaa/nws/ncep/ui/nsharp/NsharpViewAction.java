/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpViewAction
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 06/25/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;

 

import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

public class NsharpViewAction extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException { 
		NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
		NsharpConfigStore configStore = configMgr.retrieveNsharpConfigStoreFromFs();
		NsharpGraphProperty graphConfigProperty = configStore.getGraphProperty();
		String paneConfigurationName = graphConfigProperty.getPaneConfigurationName();
		System.out.println("paneConfigurationName ="+paneConfigurationName);
		String viewid = "gov.noaa.nws.ncep.ui.nsharp.defaultview1";
		if(!paneConfigurationName.equals(NsharpConstants.PANE_LEGACY_CFG_STR)){
		/*
		 *  The viewID string is in the XML file for NSHARP extension point. 
		 */
			// multiple panes
			viewid = "gov.noaa.nws.ncep.ui.nsharp.defaultview1";
		}
		else {
			//legacy configuration
			/*
			 *  The viewID string is in the XML file for NSHARP extension point. 
			 */
				viewid = "gov.noaa.nws.ncep.ui.nsharp.defaultview2";
		}

        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
        IViewPart vpart = wpage.findView( viewid);

        try {

            if ( vpart == null ){
            	
                vpart = wpage.showView(viewid );
                //Chin MERGE moved this here from the NsharpPaletteWindow so we can open the view without an editor.
                if (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage() != null) {
                    NsharpMapResource nsharpMapResource = NsharpMapResource.getOrCreateNsharpMapResource();
                    nsharpMapResource.setPoints(null);
                }
                
            }
            else {
        
                if ( ! wpage.isPartVisible(vpart) ) vpart = wpage.showView( viewid );
                
            }
        }
        catch (Exception e) {
        	
        	e.printStackTrace();
        	
        }
		
           
		return null;
	}

}
