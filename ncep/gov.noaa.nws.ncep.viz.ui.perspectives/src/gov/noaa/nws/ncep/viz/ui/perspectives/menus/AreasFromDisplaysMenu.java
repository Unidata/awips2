package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea.AreaSource;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.viz.core.IDisplayPane;

/**
 * Create the Menu Items for the Area from currently loaded Displays/panes
 *
 * THIS IS CURRENTLY NOT CONFIGURED AS A MENU BUT COULD BE TO IMPLEMENT
 * NMAP's Get SETTING FUNCTIONALITY.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/29/12       630        G. Hull      created but currently not used 
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class AreasFromDisplaysMenu extends CompoundContributionItem {
	
	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.loadPredefinedArea";
 
	@Override
    protected IContributionItem[] getContributionItems() {
    	
        List<IContributionItem> items = new ArrayList<IContributionItem>();
        Map<String, String> cmdParams = new HashMap<String, String>();

		cmdParams.put("areaName", "xxx"); // 2 command parameters to be filled in below
		cmdParams.put("areaType", AreaSource.DISPLAY_AREA.toString() );
        
        NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();

        for( NCMapEditor ed : NmapUiUtils.getAllNCDisplays() ) {
			// if this is the current editor then we only need to show an area option
        	// if there are multiple, non-geosynced panes.
        	//
			if( ed == currEditor && 
				(ed.arePanesGeoSynced() || ed.getNumberofPanes() == 1 ) ) {
				continue;
			}

    		IDisplayPane[] panes = ed.getDisplayPanes();

    		for( IDisplayPane p : panes ) {
    			NCDisplayPane ncp = (NCDisplayPane)p;
    			NCMapRenderableDisplay rdisp = (NCMapRenderableDisplay)ncp.getRenderableDisplay();
    				
// the provider name is the paneName but is set before loading and so doesn't have the display ID
// which is helpfull/needed when looking up the display/pane to get the area. 
// so for now just set the name here.  			
//    			cmdParams.put("areaName", rdisp.getCurrentArea().getProviderName() );
    			
    			if( ed.getNumberofPanes() == 1 ) {
    				cmdParams.put("areaName", ed.getDisplayName() );
    			}
    			else {
    				cmdParams.put("areaName", 
    						ed.getDisplayName()+"("+rdisp.getPaneId().toString()+")" );        	            
    			}
    			
    			CommandContributionItem item = new CommandContributionItem(
    					new CommandContributionItemParameter(
    							PlatformUI.getWorkbench(), null,
    							commandId,
    							cmdParams, null, null, null, 
    							cmdParams.get("areaName"),//rdisp.getCurrentArea().getProviderName(), 
    							null, null,
    							CommandContributionItem.STYLE_PUSH, null, true));
    			items.add(item);
    		}    		
    	}
    	
        return items.toArray(new IContributionItem[0]);
    }
}

