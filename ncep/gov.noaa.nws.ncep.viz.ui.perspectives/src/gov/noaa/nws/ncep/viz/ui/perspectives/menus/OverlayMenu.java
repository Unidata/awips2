package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceCategory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Create the Menu Items for the Overlays menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *  4/15/11                  G. Hull      created.
 * 12/06/11                  B. Hebbard   sort menu entries alphabetically
 * 03/29/12                  B. Hebbard   refactor courtesy of Matt Nash (RTS)
 *                                        to extend CompoundContributionItem
 *                                        (instead of ContributionItem); fixes
 *                                        menu items disappearing (after first
 *                                        menu use) in OB12.4
 * 06/05/12      #816        G. Hull      update for ResourceDefns returned by getResourceDefnsForCategory()                                        
 * 02/22/13      #972        G. Hull      only for supported display types
 * 04/09/13      #864        G. Hull      add sub-menus for filters and isEnabled.
 * 01/21/14					 M. James	  reconfigured overlay menu to toggle/unload.
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class OverlayMenu extends CompoundContributionItem {

	@Override
	protected IContributionItem[] getContributionItems() {
		
		IMenuManager ovrlyMenuMngr = new MenuManager( "Overlays", 
        		OverlayMenu.class.getName() );
		
		List<String> enabledFiltersList = new ArrayList<String>();
		
        List<String> disabledFiltersList = new ArrayList<String>();
        
        List<ResourceDefinition> enabledOvrlyRscDfns = 
        		new ArrayList<ResourceDefinition>();
        
        List<ResourceDefinition> disabledOvrlyRscDfns = 
        		new ArrayList<ResourceDefinition>();
        
        NcDisplayType dispType = NcEditorUtil.getNcDisplayType( 
        			NcDisplayMngr.getActiveNatlCntrsEditor() );
        if( dispType != NcDisplayType.NMAP_DISPLAY ) { // ???
            return new IContributionItem[0];
        }

        List<ResourceDefinition> ovrlyRscDfns = null;
       
		try {
			// get all
			ovrlyRscDfns = ResourceDefnsMngr.getInstance().
			getResourceDefnsForCategory( 
			ResourceCategory.OverlayRscCategory, "", dispType, 
			false, true );
			
		} catch (VizException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} // no gen types, include disabled defns.
        Collections.sort( ovrlyRscDfns, new Comparator<ResourceDefinition>() { // alphabetize
        	
        	private Integer getCategory( ResourceDefinition o ) {
        		if( o.getRscImplementation().equals( "LatlonOverlay" ) ) {
        			return 1;
        		}
        		else if( o.getRscImplementation().equals( "Locator" ) ) {
        			return 2;
        		}
        		else if( o.getRscImplementation().equals( "ScaleOverlay" ) ) {
        			return 3;
        		}
        		else {
        			return 4;
        		}
        	}
        	
        	public int compare(ResourceDefinition o1, ResourceDefinition o2) { // ...case
        		if( getCategory(o1) != getCategory(o2) ) {
        			return getCategory( o1 )-getCategory( o2 );
        		}
        		return o1.getResourceDefnName().compareToIgnoreCase( o2.getResourceDefnName() );
        	}
        });
        
        for( ResourceDefinition rd : ovrlyRscDfns ) { 
        	if( rd.getResourceDefnName().equals( dispType.getBaseResource() ) ) {
        		continue;
        	}
        	List<String> filtList = ( rd.isEnabled() ? enabledFiltersList : disabledFiltersList );
        	List<ResourceDefinition> ovrlyRDs = 
        		                    ( rd.isEnabled() ? enabledOvrlyRscDfns : disabledOvrlyRscDfns );
        	ovrlyRDs.add( rd );
        		
        	List<String> ordFiltList = null;
			try {
				ordFiltList = ResourceDefnsMngr.getInstance().
				getResourceDefnFilter( rd.getResourceDefnName() ).getFilters();
			} catch (VizException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        	
        	for( String filtStr : ordFiltList ) { 
        		if( !filtList.contains( filtStr ) ) {
        			filtList.add( filtStr );
        		}
        	}
        }
        
        for( ResourceDefinition ord : enabledOvrlyRscDfns ) {
            ovrlyMenuMngr.add( createOverlayMenuItem( ord ) );            	
        }  
        
		return ovrlyMenuMngr.getItems();

    }
	
    private CommandContributionItem createOverlayMenuItem( ResourceDefinition ovrlyRsc ) {
		Map<String, String> params = new HashMap<String, String>();
		ovrlyRsc.getResourceParameters(isEnabled());
		params.put("overlayName", ovrlyRsc.getResourceDefnName() );
		params.put("resourceName", ovrlyRsc.getMapName() );
		
		CommandContributionItemParameter param = new CommandContributionItemParameter(
				PlatformUI.getWorkbench(), null,
				"gov.noaa.nws.ncep.viz.ui.actions.loadOverlay", params,
				null, null, null, ovrlyRsc.getMapName(), null, null,
				CommandContributionItem.STYLE_CHECK, null, true);
		
		return new CommandContributionItem( param );
		
    }

}

