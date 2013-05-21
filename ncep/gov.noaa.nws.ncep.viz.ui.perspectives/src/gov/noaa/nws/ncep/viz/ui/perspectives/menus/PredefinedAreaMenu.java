package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.common.display.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.display.PredefinedAreasMngr;
import gov.noaa.nws.ncep.viz.common.display.PredefinedArea.AreaSource;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.io.File;
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
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PredefinedAreaMenu extends CompoundContributionItem {

	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.loadPredefinedArea";
    private Map<String, String> cmdParams = new HashMap<String, String>();

    @Override
    protected IContributionItem[] getContributionItems() {
    	IMenuManager areaMenuMngr = new MenuManager( "Areas", PredefinedAreaMenu.class.getName() );
    	
    	addResourceAreaMenuItems( areaMenuMngr );
    	
    	addOtherDisplayAreasMenuItems( areaMenuMngr );

    	addPredefinedAreaMenuItems( areaMenuMngr );
    	
    	return areaMenuMngr.getItems();
    }
    
    private void addPredefinedAreaMenuItems( IMenuManager areaMenuMngr ) {

		cmdParams.put("areaName", "xxx"); // 2 command parameters to be filled in below
        cmdParams.put("areaType", AreaSource.PREDEFINED_AREA.toString() );
            	    	
        AbstractEditor ncEd = NcDisplayMngr.getActiveNatlCntrsEditor();
        
        String predefinedAreas[] = 
        	PredefinedAreasMngr.getPredefinedAreasForMenus( NcEditorUtil.getNcDisplayType( ncEd ) );
        
        for( String areaName : predefinedAreas ) {
			
        	cmdParams.put("areaName", areaName );
			
        	areaMenuMngr.add( new CommandContributionItem(
                    new CommandContributionItemParameter( PlatformUI.getWorkbench(), null,
                    		commandId, cmdParams, null, null, null, areaName, null, null,
                            CommandContributionItem.STYLE_PUSH, null, true)) );
        }        
    }

	private void addResourceAreaMenuItems( IMenuManager areaMenuMngr ) {
		AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	if( currEditor == null ) {
    		return;
    	}
    	
		cmdParams.put("areaName", "xxx"); // 2 command parameters to be filled in below
		cmdParams.put("areaType", AreaSource.RESOURCE_DEFINED.toString() );
        
		// if geoSynced then let the user choose from all panes and otherwise only from
		// the currently selected panes.
		IDisplayPane[] panes = 
			( NcEditorUtil.arePanesGeoSynced(currEditor) ? 
					currEditor.getDisplayPanes() : NcEditorUtil.getSelectedPanes(currEditor) );
    		
		IMenuManager rscAreaMenu = null;	

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

					// don't create a subMenu unless there are actually rscs 
					if( rscAreaMenu == null ) {
						rscAreaMenu = new MenuManager( "From Resource",
								areaMenuMngr.getId() + "." + "From Resource" );	
					}

					cmdParams.put("areaName", rsc.getResourceName().toString() );

					rscAreaMenu.add( new CommandContributionItem(
							new CommandContributionItemParameter( PlatformUI.getWorkbench(), null,
									commandId, cmdParams, null, null, null, 
									rscLabel, null, null,
									CommandContributionItem.STYLE_PUSH, null, true)) );
				}
			}
		} // end loop thru panes
		
		if( rscAreaMenu != null ) {
			areaMenuMngr.add( rscAreaMenu );
		}
	}
	
	private void addOtherDisplayAreasMenuItems( IMenuManager areaMenuMngr ) {

		cmdParams.put("areaName", "xxx"); // 2 command parameters to be filled in below
		cmdParams.put("areaType", AreaSource.DISPLAY_AREA.toString() );
        
		AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

		IMenuManager dispsAreaSubMenu  = new MenuManager( "From Display",
				areaMenuMngr.getId() + "." + "From Display" );	

        for( AbstractEditor ed : NcDisplayMngr.getAllNcDisplays() ) {

			Boolean panesSynced = NcEditorUtil.arePanesGeoSynced( ed );
    		IDisplayPane[] panes = ( panesSynced ? 
    				NcEditorUtil.getSelectedPanes(ed) : ed.getDisplayPanes() );

    		int numPaneMenus = panes.length;
    		String dispMenuName = NcEditorUtil.getDisplayName(ed).toString();
    		
    		if( numPaneMenus == 1 ) {
				cmdParams.put("areaName", dispMenuName );

    			// if this is the current editor then we only need to show an area option
    			// if there are multiple, non-geosynced panes.
    			//
				if( ed != currEditor ) {
    				NCMapRenderableDisplay rdisp = 
    					(NCMapRenderableDisplay)panes[0].getRenderableDisplay();
    				
    				cmdParams.put("areaName", dispMenuName );

    				dispsAreaSubMenu.add( new CommandContributionItem(
    						new CommandContributionItemParameter( PlatformUI.getWorkbench(), null,
    								commandId, cmdParams, null, null, null, 
    								cmdParams.get("areaName"),//rdisp.getCurrentArea().getProviderName(), 
    								null, null,
    								CommandContributionItem.STYLE_PUSH, null, true)) );
    			}
    		}
    		else {
    			// create a subMenu
    			IMenuManager panesAreaSubMenu = new MenuManager( dispMenuName,
    					dispsAreaSubMenu.getId() + "." + dispMenuName );	
    			
    			for( IDisplayPane p : panes ) {
    				NCMapRenderableDisplay rdisp = (NCMapRenderableDisplay)p.getRenderableDisplay();

    				// the provider name is the paneName but is set before loading and so doesn't have the display ID
    				// which is helpfull/needed when looking up the display/pane to get the area. 
    				// so for now just set the name here.  			
    				//    			cmdParams.put("areaName", rdisp.getCurrentArea().getProviderName() );

//    				if( dispsAreaMenu == null ) {
//    					dispsAreaMenu = new MenuManager( "From Display",
//    							areaMenuMngr.getId() + "." + "From Display" );	
//    				}

    				// The areaName is the name of the pane in the display.
    				// TODO : fold the paneName into the DisplayName class for parsing/generating
    				//
    				cmdParams.put("areaName", 
    						NcEditorUtil.getDisplayName(ed).toString()+"("+rdisp.getPaneId().toString()+")" );

    				panesAreaSubMenu.add( new CommandContributionItem(
    						new CommandContributionItemParameter( PlatformUI.getWorkbench(), null,
    								commandId, cmdParams, null, null, null, 
    								cmdParams.get("areaName"),//rdisp.getCurrentArea().getProviderName(), 
    								null, null,
    								CommandContributionItem.STYLE_PUSH, null, true)) );
    			}
    			
    			dispsAreaSubMenu.add( panesAreaSubMenu );
    		}
        }
        
        if( dispsAreaSubMenu.getItems().length > 0 ) { // != null ) {
            areaMenuMngr.add( dispsAreaSubMenu );
        }
	}
}

