package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.SubStatusLineManager;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.services.ISourceProviderService;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;

//import gov.noaa.nws.ncep.viz.resources.time_match;

/**
 * Methods that used to be in NatlCntrsEditor but did little more than delegate to the 
 * PaneManager. This helped in migrating the code that called these methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *      
 * Date            Ticket#     Engineer    Description
 * ------------    ----------  ----------- --------------------------
 *  01/20/12        #972       Greg Hull   Initial Creation.
 * 
 * </pre>
 * 
 * @author ghull
 * 
 */
public class NcEditorUtil {
	 	
    public static AbstractNcPaneManager getNcPaneManager( AbstractEditor vized ) {
    	if( vized == null ) return null;
    	
    	
    	IEditorInput edin = vized.getEditorInput();
    	
    	if( edin instanceof EditorInput &&
    		((EditorInput)edin).getPaneManager() instanceof AbstractNcPaneManager ) {
    		
            return (AbstractNcPaneManager)((EditorInput)edin).getPaneManager();    		
    	}
    	else {
    		// throw error ??  what should be the behaviour if called with a non-NC editor?
    		return null;
    	}
    }
    
    public static NcDisplayType getNcDisplayType( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? pm.getDisplayType() : null );
    }
    
    public static INcPaneLayout getPaneLayout( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm == null ) {
    		return null;
    	}
    	else if( pm instanceof NCPaneManager ) {
    		return ((NCPaneManager)pm).getPaneLayout();
    	}
    	else {
    		System.out.println("calling getPaneLayout on non NCPaneManager???");
    		return null;
    	}
    }

    public static int getNumberOfPanes( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? pm.getNumberofPanes() : 0 );
    }
    
    public static Boolean getAutoUpdate( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? pm.getAutoUpdate() : null );
    }
    
    public static void setAutoUpdate( AbstractEditor vized, Boolean aup ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.setAutoUpdate( aup );    		
    	}
    }

    public static Boolean isDisplayAvailableToLoad( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? pm.isDisplayAvailableToLoad() : true );
    }

    public static void setDisplayAvailable( AbstractEditor vized, Boolean av ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.setDisplayAvailable( av );    		
    	}
    }

    public static NcDisplayName getDisplayName( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? pm.getDisplayName() : new NcDisplayName( 0, "error") );    	      
    }
    
    public static void setDisplayName( AbstractEditor vized, NcDisplayName dn ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.setDisplayName( dn );    	      
    	}
    }

    public static Boolean arePanesGeoSynced( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? pm.arePanesGeoSynced() : false );
    }
    
    public static void setGeoSyncPanesEnabled( AbstractEditor vized, Boolean gsync ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.setGeoSyncPanesEnabled( gsync );    		
    	}
    }

    public static void toggleHideShow( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.setHideShow( !pm.getHideShow() );
    	}
    }
    
    public static Boolean getHideShow( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return ( pm != null ? pm.getHideShow() : false );
    }

    public static void setHideShow( AbstractEditor vized, Boolean hs ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.setHideShow( hs );
    	}
    }

    // Convenience method to get the map descriptor
    public static INatlCntrsDescriptor getDescriptor( AbstractEditor vized ) {
        IDescriptor descriptor = null;
        IRenderableDisplay display = vized.getActiveDisplayPane()
                .getRenderableDisplay();
        if (display != null) {
            descriptor = display.getDescriptor();
        }
        
        return (descriptor instanceof INatlCntrsDescriptor ? (INatlCntrsDescriptor)descriptor : null );
    }
	
    public static void addSelectedPaneChangedListener(
    		AbstractEditor vized, ISelectedPanesChangedListener listener ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.addSelectedPaneChangedListener( listener );
    	}
    }

    public static void removeSelectedPaneChangedListener(
    		AbstractEditor vized, ISelectedPanesChangedListener listener ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm != null ) {
    		pm.removeSelectedPaneChangedListener( listener );
    	}
    }
    
    public static IDisplayPane[] getSelectedPanes( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? 
    			pm.getSelectedPanes( NCPaneManager.NC_PANE_SELECT_ACTION) : null );    	
    }
    
    public static Boolean isSelectedPane( AbstractEditor vized, IDisplayPane pane ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	return (pm != null ? 
    			pm.isSelectedPane( NCPaneManager.NC_PANE_SELECT_ACTION, pane ) : false);    	
    }

    public static void refreshGUIElements( AbstractEditor vized) {
    	if( !(vized instanceof AbstractNcEditor) ) {
    		return;
    	}
    	
        ICommandService service = (ICommandService)vized.getSite().getService(
                ICommandService.class);

        // wanted to put this in Common but this created cyclic dependencies
        // on the projects.
        String[] guiUpdateElementCommands = {
                // "gov.noaa.nws.ncep.viz.tools.pan",
                "gov.noaa.nws.ncep.viz.ui.options.SyncPanes",
                "gov.noaa.nws.ncep.viz.ui.actions.loopBackward",
                "gov.noaa.nws.ncep.viz.ui.actions.loopForward",
                "gov.noaa.nws.ncep.viz.ui.actions.rock",
                "gov.noaa.nws.ncep.viz.ui.actions.frameTool",
                "gov.noaa.nws.ncep.viz.ui.autoUpdate",
                "gov.noaa.nws.ncep.viz.ui.actions.hideFrames" };
        // Update the GUI elements on the menus and toolbars
        for (String toolbarID : guiUpdateElementCommands) {
            service.refreshElements(toolbarID, null);
        }

        // the IVizEditorChangedListener will trigger on a close editor which can 
        // happen on exit. In this case there is no active page
        IWorkbenchPage actPage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        
        if( actPage != null && actPage.getActiveEditor() != null ) {

        	SubStatusLineManager bar = (SubStatusLineManager) actPage.getActiveEditor().
        	     getEditorSite().getActionBars().getStatusLineManager();

        	IContributionItem items[] = bar.getParent().getItems();
        	for (IContributionItem it : items) {
        		it.update();
        	}
        }
        
        refreshGUIElementsForSelectedPanes( vized );
    }
    
    // this is called directly when a pane is selected (ie. not through a PaneChangedListener, 
    // although it could be). 
    //    It is meant to refresh any GUI elements that are based on values from the selected
    // pane(s). To begin with it is only used for the Zoom Lock(Suspend) which will disable
    // the Zoom/UnZoom buttons by setting the state in the ZoomStateSourceProvider.
    //
    public static void refreshGUIElementsForSelectedPanes( AbstractEditor vized ) {
    	AbstractNcPaneManager pm = getNcPaneManager( vized );
    	if( pm == null ) {
    		return;
    	}

    	boolean lockZoom = false;
    	
    	for( IDisplayPane pane : getSelectedPanes( vized ) ) {
    		if( ((INatlCntrsDescriptor)pane.getDescriptor()).getSuspendZoom() ) {
    			lockZoom = true;
    		}
    	}
    	
        IWorkbenchWindow ww = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        ISourceProviderService spService = 
        	(ISourceProviderService) ww.getService(ISourceProviderService.class);
        ZoomStateSourceProvider zoomStateSourceProvider = 
        	(ZoomStateSourceProvider) spService.getSourceProvider(ZoomStateSourceProvider.ZOOM_STATE);
        zoomStateSourceProvider.setZoomSuspended( lockZoom );
    }
   
    // Use the array of displayPanes stored in the EditorInput
    // and returned by AbstractEditor
    //
    public static IDisplayPane getDisplayPane( AbstractEditor vized, INcPaneID pid) {

        IDisplayPane[] displayPanes = vized.getDisplayPanes();

        if (displayPanes == null || displayPanes.length == 0) {
            return null;
        } 
        else if( getPaneLayout( vized ).getPaneIndex(pid) < 0) {
            return null;
        }

        return displayPanes[getPaneLayout( vized ).getPaneIndex(pid)];
    }

    // Called in NC perspective to allow for deselecting panes by
    // way of a radio behaviour.
    public static void selectPane( AbstractEditor vized, IDisplayPane selPane, boolean radioBehaviour) {

    	// get a list of the panes to be selected.
    	//
    	List<IDisplayPane> seldPanes;
    	
        if( radioBehaviour ) {
        	seldPanes = new ArrayList<IDisplayPane>();
    		seldPanes.add( selPane );
        }
        else {
        	IDisplayPane[] dispPanes = getSelectedPanes( vized ); //getDisplayPanes();
        	seldPanes = new ArrayList<IDisplayPane>( Arrays.asList( dispPanes ) );
        	
        	if( seldPanes.contains( selPane ) &&  
        		seldPanes.size() > 1 ) {
        		seldPanes.remove( selPane );
            }
        	else {
        		seldPanes.add( selPane );
        	}
        }
        
        if( !seldPanes.isEmpty() ) {
        	AbstractNcPaneManager pm = NcEditorUtil.getNcPaneManager(vized);
        	if( pm != null ) {
        		pm.selectPanes( seldPanes );
        	}
        }
        
        refreshGUIElementsForSelectedPanes( 
        		(AbstractEditor)selPane.getRenderableDisplay().getContainer() );       
    }

    // Currently this just sets the listener for 1 (the active) pane.
    // This works now because all panes have the same timeline and this 
    // is used just for the FramdeDataDisplay. 
    // TODO : if each pane will need to be notified for a frameChange then 
    // we will need to change this to add the listener to all descriptors.
    // 
    public static void addFrameChangedListener(AbstractEditor vized, IFrameChangedListener lstnr) {
        vized.getActiveDisplayPane().getDescriptor().addFrameChangedListener(lstnr);
    }

    // remove from all descriptors
    public static void removeFrameChangedListener(AbstractEditor vized, IFrameChangedListener lstnr) {

    	for( IDisplayPane pane : vized.getDisplayPanes() ) {    		
    		IDescriptor descriptor = pane.getDescriptor();

    		if( descriptor != null ) {
    			descriptor.removeFrameChangedListener( lstnr );
    		}
        } 
    }
}

