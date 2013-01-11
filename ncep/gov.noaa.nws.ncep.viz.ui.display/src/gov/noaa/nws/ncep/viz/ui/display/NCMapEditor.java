/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.viz.ui.display;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import gov.noaa.nws.ncep.viz.common.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.common.EditorManager;
import gov.noaa.nws.ncep.viz.common.preferences.NcepGeneralPreferencesPage;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.SubStatusLineManager;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.services.ISourceProviderService;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IVizEditorChangedListener;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.ui.editor.VizMultiPaneEditor;
import com.raytheon.viz.ui.panes.PaneManager;

//import gov.noaa.nws.ncep.viz.resources.time_match;

/**
 * Defines the GL-based map editor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *      
 * Date            Ticket#     Engineer    Description
 * ------------    ----------  ----------- --------------------------
 * 7/1/06                      chammack    Initial Creation.
 * 12/3/07         461         bphillip    Added Time Display to Status bar
 * Oct 21, 2008     #1450      randerso    Moved multipanel support down into GLMapEditor
 * 04/09/09        2228        rjpeter     Removed recursive listener adding.
 * Aug 31, 2009    2920        rjpeter     Moved MapContext Activation/Deactivation to include when window loses focus.
 * Dec 16, 2009                ghull       add PaneListener
 * Feb 18, 2010    #226        ghull       add PaneLayout
 * April 1, 2010  #238,#239    archana     Modified the method refreshGuiElements()
 *                                         to update the contribution items
 *                                         in the status bar.
 *                                         Added methods addFrameChangedListener(IFrameChangedListener)
 *                                         and removeFrameChangedListener(IFrameChangedListener)
 * 05/27/10                    ghull       get/setEditorInput()
 * 10/21/10		  #314		   Q. Zhou     added get/set for Hide/Show status
 * 10/29/10       #307         ghull       added get/setAutoUpdate()
 * 11/04/10       migration    ghull       override isDirty() and return false.
 * 14/01/11      #289         archana     moved the logic to activate contexts from NCMapEditor
 *                                                    to the NCPerspectiveManager (plugin.xml)
 *  02/10/2011                 Chin Chen   handle multiple editor copies dispose issue         
 *  
 * 03/07/11    migration       ghull       extend from AbstractMultiPaneEditor ; remove displayPaneMap  
 * 04/19/11      #434          ghull       on dispose(), don't let the user close the last editor.              
 * 04/26/11       #416         M. Gao      fix a potential bug in on dealing with parsing String to Int in the method setDisplayName(...) method
 * 06/22/11    migration       ghull       add back @Override of isDirty to prevent dirty editors.
 * 07/15/11                    C Chen      add implements AbstractNcEditor. fix looping buttons not coordinated issue
 * 11/11/11                    ghull       remove frameChangeListener from all descriptors
 * 12/02/11       #571         ghull       check for activePage in refreshGUIElements and in dispose()
 * 07/12/12       ###          ghull       call refreshGUIElements on paneChange. Select all panes at once instead of selecting/deselecting.
 * 07/31/12       #631         ghull       check promptOnClose preference to set isDirty.
 * 12/12/12       #630         ghull       refreshGUIElementsForSelectedPanes()
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
// bsteffen changed to VizMultiPaneEditor
// public class NCMapEditor extends AbstractMultiPaneEditor { //GLMapEditor {
public class NCMapEditor extends VizMultiPaneEditor implements AbstractNcEditor {

    /** The activated context, else null if not activated. */
    // protected IContextActivation contextActivation;

    private String applicationName = "NA";

    private int editorNum;

    // private NatlCntrsDisplayID displayID = null;
    //
    // public NatlCntrsDisplayID getDisplayID() {
    // return displayID;
    // }

    public int getEditorNum() {
        return editorNum;
    }

    // private NCMapRenderableDisplay editorOwnDisplay;

    // /** The display pane */
    // // protected ArrayList<NCDisplayPane> displayPanes;
    // protected HashMap<String, NCDisplayPane> displayPaneMap;
    //
    @Override
    // bsteffen changed to PaneManager
    // protected IPaneManager getNewPaneManager() {
    protected PaneManager getNewPaneManager() {
        return new NCPaneManager();
    }

    // bsteffen changed to PaneManager
    // public IPaneManager getPaneManager() {
    public PaneManager getPaneManager() {
        return editorInput.getPaneManager();
    }

    // Convenience method to get the map descriptor
    public IMapDescriptor getDescriptor() {
        // bsteffen ported this function from NCPaneManager
        // return (IMapDescriptor) getPaneManager().getDescriptor();
        IMapDescriptor descriptor = null;
        IRenderableDisplay display = getActiveDisplayPane()
                .getRenderableDisplay();
        if (display != null) {
            descriptor = (IMapDescriptor) display.getDescriptor();
        }
        return descriptor;
    }

    // use name member in getEditorName
    // private String displayName = null;

    private boolean geoSyncPanesEnabled;

    private boolean timeSyncPanesEnabled = true;

    //
    // /** Is hide loop rsc or show loop rsc? */
    private boolean isHide = false; // means loop rsc on, and Hide btn displayed

    public boolean getHideShow() {
        return isHide;
    }

    public boolean setHideShow(boolean isHide) {
        return this.isHide = isHide;
    }

    public boolean getAutoUpdate() {
        NCMapDescriptor desc = (NCMapDescriptor) getDescriptor();
        return desc.isAutoUpdate();
    }

    public void setAutoUpdate(boolean autoUpdate) {
        IDisplayPane[] dispPanes = getDisplayPanes();
        for (IDisplayPane pane : dispPanes) {
            if (pane instanceof NCDisplayPane) {
                ((NCMapDescriptor) pane.getDescriptor())
                        .setAutoUpdate(autoUpdate);
            }
        }
    }

    public void setGeoSyncPanesEnabled(boolean s) {
        geoSyncPanesEnabled = s;
    }

    public boolean arePanesGeoSynced() {
        return geoSyncPanesEnabled;
    }

    public void setTimeSyncPanesEnabled(boolean s) {
        timeSyncPanesEnabled = s;
    }

    public boolean arePanesTimeSynced() {
        return timeSyncPanesEnabled;
    }

    public PaneLayout getPaneLayout() {
        return ((NCPaneManager) editorInput.getPaneManager()).getPaneLayout();
    }

    public void refreshGUIElements() {
        ICommandService service = (ICommandService) getSite().getService(
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
        
        refreshGUIElementsForSelectedPanes();
    }

    // this is called directly when a pane is selected (ie. not through a PaneChangedListener, 
    // although it could be). 
    //    It is meant to refresh any GUI elements that are based on values from the selected
    // pane(s). To begin with it is only used for the Zoom Lock(Suspend) which will disable
    // the Zoom/UnZoom buttons by setting the state in the ZoomStateSourceProvider.
    //
    public void refreshGUIElementsForSelectedPanes() {
    	boolean lockZoom = false;
    	
    	for( IDisplayPane pane : getSelectedPanes() ) {
    		if( ((NCMapDescriptor)pane.getDescriptor()).getSuspendZoom() ) {
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
    

    public void setDisplayName(String dispName) {
        editorInput.setName(dispName);
        setTabTitle(dispName);

        String numStr = dispName.substring(0, dispName.indexOf("-"));
        if (numStr != null)
            editorNum = Integer.parseInt(numStr);
    }

    public String getDisplayName() {
        return editorInput.getName();
    }

    // override to not include # panels in title
    @Override
    public void setTabTitle(String title) {
        updateTitle();
    }

    @Override
    protected void updateTitle() {
        // set the name on the tab
        String name = getEditorName();
        setPartName(name);
    }

    @Override
    public String getDefaultTool() {
        return "gov.noaa.nws.ncep.viz.tools.pan";
    }

    // Override AbstractEditor implementation which will loop thru the
    // renderable displays and call addPane.
    // Since we already know the paneLayout, we can create the gridLayout
    // here.
    @Override
    public void createPartControl(Composite parent) {

        editorInput.getPaneManager().initializeComponents(this, parent);

        for (IRenderableDisplay display : displaysToLoad) {
            addPane(display);

            if (getNumberofPanes() == 1) {
                selectPane(getDisplayPanes()[0], true);
            }
        }

        contributePerspectiveActions();
    }

    //
	@Override
	public void dispose() {
		
		EditorManager.unregisterEditorNumber(editorNum);
		super.dispose();

        IWorkbenchPage actPage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        
        // if this is null then we are exiting
        if( actPage == null ) {
        	return;
        }
				
		IWorkbenchPage page = this.getSite().getPage();

		if( page.getEditorReferences().length == 0 ) {
			ICommandService service = (ICommandService) getSite().getService(
					ICommandService.class);
			Command cmd = service.getCommand("gov.noaa.nws.ncep.viz.ui.newMapEditor");
			if( cmd == null ) {
				System.out.println("Can't find Command to create a new Display");
				return;
			}
			
			HashMap<String, Object> cmdParams = new HashMap<String, Object>();
			
			cmdParams.put("promptForName", "false" );
			
			ExecutionEvent event = new ExecutionEvent(
					cmd, cmdParams, null, null);
			try {
				cmd.executeWithChecks(event);
			} catch (ExecutionException e) {
				e.printStackTrace();
			} catch (NotDefinedException e) {
				e.printStackTrace();
			} catch (NotEnabledException e) {
				e.printStackTrace();
			} catch (NotHandledException e) {
				e.printStackTrace();
			}			
		}
	}


    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        super.init(site, input);

        // //System.out.println("NCMapEditor display name "+ this.displayName +
        // " edNum " + this.editorNum);
        EditorManager.registerEditorNumber(editorNum);
    }

    // Use the array of displayPanes stored in the EditorInput
    // and returned by AbstractEditor
    //
    public IDisplayPane getDisplayPane(PaneID pid) {

        IDisplayPane[] displayPanes = super.getDisplayPanes();

        if (displayPanes == null || displayPanes.length == 0) {
            return null;
        } else if (getPaneLayout().getPaneIndex(pid) < 0) {
            return null;
        }

        return displayPanes[getPaneLayout().getPaneIndex(pid)];
    }

    //
    // @Override
    // public void addStatusLineContribution(ContributionItem contributionItem)
    // {
    //
    // // bar requires the group to be available.. so check and add
    // // if not present...
    // IStatusLineManager bar = getEditorSite().getActionBars()
    // .getStatusLineManager();
    // IContributionItem item = bar.find(contributionItem.getId());
    // if (item == null) {
    // bar.add(contributionItem);
    // }
    //
    // bar.update(true);
    // }
    //
    // @Override
    // public void clearStatusLineContributions() {
    // IStatusLineManager bar = getEditorSite().getActionBars()
    // .getStatusLineManager();
    // IContributionItem items[] = bar.getItems();
    // for (IContributionItem item : items) {
    // bar.remove(item);
    // item.dispose();
    // }
    // bar.update(true);
    // }

    // Convenience methods using the NC_PANE_SELECT_ACTION
    public boolean isSelectedPane(IDisplayPane pane) {
        return super.isSelectedPane(NCPaneManager.NC_PANE_SELECT_ACTION, pane);
    }

    // Only get the first if multiple selected
    public IDisplayPane getSelectedPane() {
        return super.getSelectedPane(NCPaneManager.NC_PANE_SELECT_ACTION);
    }

    public IDisplayPane[] getSelectedPanes() {
        return super.getSelectedPanes(NCPaneManager.NC_PANE_SELECT_ACTION);
    }

    // Called in NC perspective to allow for deselecting panes by
    // way of a radio behaviour.
    public void selectPane(IDisplayPane selPane, boolean radioBehaviour) {

    	// get a list of the panes to be selected.
    	//
    	List<IDisplayPane> seldPanes;

        if (radioBehaviour) {
        	seldPanes = new ArrayList<IDisplayPane>();
    		seldPanes.add( selPane );
        }
        else {
        	IDisplayPane[] dispPanes = getSelectedPanes(); //getDisplayPanes();
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
        	((NCPaneManager) editorInput.getPaneManager())
        							.selectPanes( seldPanes );        	
        }
        
        refreshGUIElementsForSelectedPanes();        
    }

    // Currently this just sets the listener for 1 (the active) pane.
    // This works now because all panes have the same timeline and this 
    // is used just for the FramdeDataDisplay. 
    // TODO : if each pane will need to be notified for a frameChange then 
    // we will need to change this to add the listener to all descriptors.
    // 
    public void addFrameChangedListener(IFrameChangedListener lstnr) {
        ((NCMapDescriptor) this.getActiveDisplayPane().getDescriptor())
                .addFrameChangedListener(lstnr);
    }

    // remove from all descriptors
    public void removeFrameChangedListener(IFrameChangedListener lstnr) {

    	for( IDisplayPane pane : getDisplayPanes() ) {    		
    		IDescriptor descriptor = pane.getDescriptor();

    		if( descriptor != null ) {
    			descriptor.removeFrameChangedListener( lstnr );
    		}
        } 
    }

    //
    // // Note: this will not get called unless the editor is dirty and
    // // Also Note that this will bypass raytheon's disableClose so that if we
    // // implement
    // // isDirty and still want to allow some of our Editors to not be closed
    // // (nsharp?), then
    // // we will need to override disableClose.
    @Override
    public int promptToSaveOnClose() {
    	return super.promptToSaveOnClose();
    }
    // @Override
    // public int promptToSaveOnClose() {
    // if (PlatformUI.getWorkbench().isClosing()) {
    // return ISaveablePart2.NO;
    // }
    // Shell shell = getSite().getShell();
    //
    // boolean close = MessageDialog.openQuestion(shell, "Close Editor?",
    // "Are you sure you want to close this Display?");
    // return close ? ISaveablePart2.NO : ISaveablePart2.CANCEL;
    // }
    //
    // // We could implement an isDirty method in AbstractNatlCntrsResource
    // // except currently there can't be a dependency from the display project
    // to
    // // the
    // // resources project. If we did this then the pgen resource (others?)
    // could
    // // implement isDirty to allow the user to cancel an editor close (above).
    // //
     @Override
     public boolean isDirty() {
    	 
//    	 for( IDisplayPane pane : getDisplayPanes() ) {
//    		 IRenderableDisplay display = pane.getRenderableDisplay();
//    		 if (display != null) {
//    			 for (ResourcePair rp : display.getDescriptor()
//    					 .getResourceList()) {
//    				 if( rp.getResource() instanceof AbstractNatlCntrsResource ) {
//    					 if( ((AbstractNatlCntrsResource)rp.getResource()).isDirty() ) {
//    						 return true;
//    					 }
//    				 }
//    				 // raytheons test...
//    				 ResourceProperties props = rp.getProperties();
//    				 if (!props.isSystemResource() && !props.isMapLayer()) {
//    					 return true;
//    				 }
//    			 }
//    		 }
//    	 }
 		return NmapCommon.getNcepPreferenceStore().getBoolean( NcepGeneralPreferencesPage.PromptOnDisplayClose );
     }

    public String getApplicationName() {
        return applicationName;
    }

    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }
}
