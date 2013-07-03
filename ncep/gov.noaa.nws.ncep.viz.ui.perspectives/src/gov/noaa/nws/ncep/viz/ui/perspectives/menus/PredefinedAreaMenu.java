package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.common.area.AreaMenus.AreaMenuItem;
import gov.noaa.nws.ncep.viz.common.area.AreaMenusMngr;
import gov.noaa.nws.ncep.viz.common.area.IAreaProviderCapable;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.util.ArrayList;
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

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 * 04/17/13       #863       G. Hull      moved code from static Menus to be
 *                                        dynamic MenuManagers here                               
 * 05/15/13       #862       G. Hull      support areaSources from new AreaMenus file.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PredefinedAreaMenu extends CompoundContributionItem {

	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.loadPredefinedArea";

    @Override
    protected IContributionItem[] getContributionItems() {
    	IMenuManager areaMenuMngr = new MenuManager( "Areas", PredefinedAreaMenu.class.getName() );
    	
        AbstractEditor ncEd = NcDisplayMngr.getActiveNatlCntrsEditor();
        NcDisplayType dt = NcEditorUtil.getNcDisplayType( ncEd );
        
        if( dt != NcDisplayType.NMAP_DISPLAY ) {
        	return areaMenuMngr.getItems();
        }

    	addResourceAreaMenuItems( areaMenuMngr );
    	
    	addOtherDisplayAreasMenuItems( areaMenuMngr );

    	addAreaMenuItems( areaMenuMngr );
    	
    	return areaMenuMngr.getItems();
    }
    
    private void addAreaMenuItems( IMenuManager areaMenuMngr ) {

        Map<String,List<AreaMenuItem>> areaMenuItems = 
        	AreaMenusMngr.getInstance().getPredefinedAreasForMenus();
            	    	
		List<String> subMenus = new ArrayList<String>( areaMenuItems.keySet() );
        
		// TODO : order this list according to how we want the submenus to be displayed.
		//
		
		// loop thru the subMenus and 
        for( String subMenuName : subMenus ) {
        	if( subMenuName.isEmpty() ) {
        		continue;
        	}
//        	System.out.println("subMenuName "+ subMenuName );
        	
        	List<AreaMenuItem> menuItems = areaMenuItems.get( subMenuName ); 
        	if( menuItems == null ||  menuItems.isEmpty() ) {
        		continue;
        	}
    		IMenuManager subMenu  = new MenuManager( subMenuName,
    										areaMenuMngr.getId() + "." + subMenuName );	
    		areaMenuMngr.add( subMenu );
        
    		addAreaMenuItems( subMenu, menuItems );
        }        
			
		if( areaMenuItems.containsKey( "" ) ) {
    		addAreaMenuItems( areaMenuMngr, areaMenuItems.get("") );
		}			
    }
			
	private void addAreaMenuItems( IMenuManager menuMngr, String subMenuName,
								   List<IAreaProviderCapable> areaProvList ) {
		IMenuManager subMenu  = new MenuManager( subMenuName, menuMngr.getId() + "." + subMenuName );	
		menuMngr.add( subMenu );

		List<AreaMenuItem> amiList = new ArrayList<AreaMenuItem>();
		
		for( IAreaProviderCapable ap : areaProvList ) {
			amiList.add( new AreaMenuItem( ap.getAreaName(), 
					subMenuName, ap.getAreaName(), ap.getSourceProvider().toString() ));
		}
		
		addAreaMenuItems( subMenu, amiList );
	}
    
	private void addAreaMenuItems( IMenuManager subMenuMngr, List<AreaMenuItem> menuItems ) {
	    Map<String, String> cmdParams = new HashMap<String, String>();

	    for( AreaMenuItem ami : menuItems ) {
//	    	if( ami.getSubMenuName().equals( subMenuMngr.getId() ) ) { } // sanity check
			cmdParams.put("areaName", ami.getAreaName() );
			cmdParams.put("areaSource", ami.getSource() );
//			cmdParams.put("recenterOnly", Boolean.toString( ami.getRecenterOnly() ) );
			
	    	subMenuMngr.add( new CommandContributionItem(
                    new CommandContributionItemParameter( PlatformUI.getWorkbench(), null,
							commandId, cmdParams, null, null, null, 
							ami.getMenuName(), null, null,
                            CommandContributionItem.STYLE_PUSH, null, true)) );
        }        
    }


	private void addResourceAreaMenuItems( IMenuManager areaMenuMngr ) {

		if( !AreaMenusMngr.getInstance().showImageBasedResourceAreas() ) {
			return;
		}

		AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	if( currEditor == null ) {
    		return;
    	}
    	
		// if geoSynced then let the user choose from all panes and otherwise only from
		// the currently selected panes.
		IDisplayPane[] panes = 
			( NcEditorUtil.arePanesGeoSynced(currEditor) ? 
					currEditor.getDisplayPanes() : NcEditorUtil.getSelectedPanes(currEditor) );
    		
		List<IAreaProviderCapable> rscList = new ArrayList<IAreaProviderCapable>();

		for( IDisplayPane p : panes ) {
			ResourceList rList = p.getDescriptor().getResourceList();
			for( int r=0 ; r<rList.size() ; r++ ) {
				if( rList.get(r).getResourceData() instanceof IAreaProviderCapable ) {
					rscList.add( (IAreaProviderCapable) rList.get(r).getResourceData() );
				}
			}
		} // end loop thru panes
		
		if( !rscList.isEmpty() ) {
    		addAreaMenuItems( areaMenuMngr, "From Resource", rscList );
		}
	}
	
	private void addOtherDisplayAreasMenuItems( IMenuManager areaMenuMngr ) {

		System.out.println(" show disp areas:"+AreaMenusMngr.getInstance().showDisplayAreas() );
		
		if( !AreaMenusMngr.getInstance().showDisplayAreas() ) {
			return;
		}
        
		AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

		List<IAreaProviderCapable> dispList = new ArrayList<IAreaProviderCapable>();

        for( AbstractEditor ed : NcDisplayMngr.getAllNcDisplays() ) {

			Boolean panesSynced = NcEditorUtil.arePanesGeoSynced( ed );
    		IDisplayPane[] panes = ( panesSynced ? 
    				NcEditorUtil.getSelectedPanes(ed) : ed.getDisplayPanes() );

    		int numPaneMenus = panes.length;
    		String dispMenuName = NcEditorUtil.getDisplayName(ed).toString();
    		
    		if( numPaneMenus == 1 ) {
    			// if this is the current editor then we only need to show an area option
    			// if there are multiple, non-geosynced panes.
    			//
				if( ed != currEditor && 
						panes[0].getRenderableDisplay() instanceof IAreaProviderCapable ) {
					dispList.add( (IAreaProviderCapable)panes[0].getRenderableDisplay() );
    			}
    		}
    		else {
    			for( IDisplayPane p : panes ) {
    				if( p.getRenderableDisplay() instanceof IAreaProviderCapable ) {
    					dispList.add( (IAreaProviderCapable)p.getRenderableDisplay() );
    				}
    			}
    		}
        }
        
		if( !dispList.isEmpty() ) {
    		addAreaMenuItems( areaMenuMngr, "From Display", dispList );
        }
	}
}

