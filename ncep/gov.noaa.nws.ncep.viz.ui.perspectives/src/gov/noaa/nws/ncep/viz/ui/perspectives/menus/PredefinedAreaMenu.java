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
 * Create the Menu Items for the Predefined Area menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *  4/15/11                  G. Hull      created.
 * 07/28/11       450        G. Hull      Use PredefinedAreasMngr
 * 03/29/12                  B. Hebbard   refactor courtesy of Matt Nash (RTS)
 *                                        to extend CompoundContributionItem
 *                                        (instead of ContributionItem); fixes
 *                                        menu items disappearing (after first
 *                                        menu use) in OB12.4
 * 11/28/12       630        G. Hull      add areaType parameter to the command                                    
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PredefinedAreaMenu extends CompoundContributionItem {
	
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
        cmdParams.put("areaType", AreaSource.PREDEFINED_AREA.toString() );
            	    	
        String predefinedAreas[] = PredefinedAreasMngr.getAvailPredefinedAreas();
        
        for (String areaName : predefinedAreas) {
			
        	cmdParams.put("areaName", areaName );
			
            CommandContributionItem item = new CommandContributionItem(
                    new CommandContributionItemParameter(
                            PlatformUI.getWorkbench(), null,
                            commandId,
                            cmdParams, null, null, null, areaName, null, null,
                            CommandContributionItem.STYLE_PUSH, null, true));

            items.add(item);
        }
        return items.toArray(new IContributionItem[0]);
    }
}

