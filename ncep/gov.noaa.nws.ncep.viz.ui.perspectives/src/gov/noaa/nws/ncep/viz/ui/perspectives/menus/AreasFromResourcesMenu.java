package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.PredefinedAreasMngr;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
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
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Create the Menu Items for the Area from Resources menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/29/12       630        G. Hull      created.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class AreasFromResourcesMenu extends CompoundContributionItem {
	
	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.loadPredefinedArea";
    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
    	
        List<IContributionItem> items = new ArrayList<IContributionItem>();
        Map<String, String> cmdParams = new HashMap<String, String>();

		cmdParams.put("areaName", "xxx"); // 2 command parameters to be filled in below
		cmdParams.put("areaType", AreaSource.RESOURCE_DEFINED.toString() );
        
        NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();
    	
    	if( currEditor != null ) {
    		
    		// TODO : should we first check the geoSync flag and only include the active pane 
    		// resources
    		IDisplayPane[] panes;
    		
    		// if geoSynced then let the user choose from all panes and otherwise only from
    		// the currently selected panes.
    		if( currEditor.arePanesGeoSynced() ) {
    			panes = currEditor.getDisplayPanes();
			}
    		else {
    			panes = currEditor.getSelectedPanes();
    		}
    		
    		for( IDisplayPane p : panes ) {
    			ResourceList rList = p.getDescriptor().getResourceList();
    			for( int r=0 ; r<rList.size() ; r++ ) {
    				if( rList.get(r).getResourceData() instanceof IGridGeometryProvider ) {
        				AbstractNatlCntrsRequestableResourceData rsc = 
        					(AbstractNatlCntrsRequestableResourceData) rList.get(r).getResourceData();
        				String rscLabel;
        				
        				// Abbreviate since the category and attrSet shouldn't affect the given area.
        				//
        				if( rsc.getResourceName().getRscGroup().isEmpty() ) { 
        					rscLabel = rsc.getResourceName().getRscCategory()+ File.separator+rsc.getResourceName();
        				}
        				else {
        					rscLabel = rsc.getResourceName().getRscType()+File.separator+rsc.getResourceName().getRscGroup();
        				}

        				cmdParams.put("areaName", rsc.getResourceName().toString() );
        	            
        				CommandContributionItem item = new CommandContributionItem(
        	                    new CommandContributionItemParameter(
        	                            PlatformUI.getWorkbench(), null,
        	                            commandId,
        	                            cmdParams, null, null, null, 
        	                            rscLabel, null, null,
        	                            CommandContributionItem.STYLE_PUSH, null, true));
        	            items.add(item);
    				}
    			}
    		}    		
    	}
    	
        return items.toArray(new IContributionItem[0]);
    }
}

