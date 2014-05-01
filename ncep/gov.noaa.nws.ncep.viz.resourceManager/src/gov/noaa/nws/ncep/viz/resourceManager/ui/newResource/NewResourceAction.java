package gov.noaa.nws.ncep.viz.resourceManager.ui.newResource;

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.ResourceSelectionDialog;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.ResourceSelectionControl.IResourceSelectedListener;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 *  Select a new resource from the main menu and add to the active editor. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/18/11      #           Greg Hull    Initial Creation.
 * 06/07/11       #445       Xilin Guo   Data Manager Performance Improvements
 * 10/22/11      #467        Greg Hull   add to all panes option
 * 02/22/13      #972        Greg Hull   NcDisplayType
 * 10/24/13      #1043       Greg Hull   init to prev selected rsc
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NewResourceAction extends AbstractHandler {

	private Shell shell=null;
	
	private static ResourceName prevSeldRscName = null;
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		shell = NcDisplayMngr.getCaveShell();
		
		final AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
		
		if( editor == null ) {
    		MessageDialog errDlg = new MessageDialog( 
    				NcDisplayMngr.getCaveShell(), 
    				"Error", null, 
    				"Can't load resource to this type of Display",
    				MessageDialog.ERROR, new String[]{"OK"}, 0);
    		errDlg.open();
    		return null;
		}
		
		IRenderableDisplay display = 
			   (IRenderableDisplay)NcEditorUtil.getSelectedPanes(editor)[0].getRenderableDisplay();
		
		final AbstractTimeMatcher tm = display.getDescriptor().getTimeMatcher();
		
		if( tm == null || !(tm instanceof NCTimeMatcher) ) { // sanity check
			
			// TODO ? show an info message that there is no timeline and that the
			// selected resource will be dominant and use its default timeline?
//			timeMatcher.getFrameTimes() == null ||
//			timeMatcher.getFrameTimes().isEmpty()  ) {
			
    		MessageDialog errDlg = new MessageDialog( 
    				NcDisplayMngr.getCaveShell(), 
    				"Error", null, 
    				"Can't load resource to a Display with/out a timeline",
    				MessageDialog.ERROR, new String[]{"OK"}, 0);
    		errDlg.open();
    		return null;
		}

		final NCTimeMatcher timeMatcher = (NCTimeMatcher)tm;
		
		final boolean useNewTimeMatcher = 
			( timeMatcher.getFrameTimes() == null ||
			  timeMatcher.getFrameTimes().isEmpty() );
				
		final NcDisplayType dispType = NcEditorUtil.getNcDisplayType( editor );

		// Create the Selection Dialog and add a listener for when a resource is selected.
		// 
		final ResourceSelectionDialog rscSelDlg = new ResourceSelectionDialog( shell ); 
				
   		rscSelDlg.addResourceSelectionListener( new IResourceSelectedListener() {
   			@Override
   			public void resourceSelected( ResourceName rscName, 
   								boolean replace, // ignore the replace option 
   							    boolean addAllPanes, boolean done ) {
   				try {
   					
   					prevSeldRscName = rscName;
   					
//   	   			System.out.println("Loading Resource " + rscName );   	   				
   					ResourceSelection rscSel = ResourceFactory.createResource( rscName );
   					ResourcePair rscPair = rscSel.getResourcePair();
   					AbstractResourceData rscData = rscPair.getResourceData();
   					LoadProperties ldProp = rscPair.getLoadProperties();
   					ResourceProperties rscProp = rscPair.getProperties();
   	   				
   					// if there is no data for this resource then don't load the resource.
   					// 
   					if( rscData instanceof AbstractNatlCntrsRequestableResourceData ) {

   						List<DataTime> availTimes = 
   							((AbstractNatlCntrsRequestableResourceData) rscData).getAvailableDataTimes();

   						if( availTimes == null || availTimes.isEmpty() ) {

   							MessageDialog msgDlg = new MessageDialog( 
   									shell, "No Data", null, 
   									"There is no data available for this resource.",
   									MessageDialog.INFORMATION, new String[]{"Ok"}, 0);
   							msgDlg.open();
   							rscSelDlg.close();
   							return;
   						}
   					
   	   					// if no timeline is set then use the selected resource as the dominant 
   	   					// resource.
   						// This timeMatcher is shared between all panes so there is no need 
   						// to this timeMatcher to other panes.
   	   					if( useNewTimeMatcher ) {
   	   						
   	   						timeMatcher.setDominantResourceData( 
   	   							(AbstractNatlCntrsRequestableResourceData) rscData );
   	   						timeMatcher.updateFromDominantResource();   	   						
   	   					}
   	   					

   	   					{ // check that available data time matches to the timeline....

   						}

   					}   	   					

   					IDisplayPane[] panesToLoad = (addAllPanes ? 
   							editor.getDisplayPanes() : NcEditorUtil.getSelectedPanes(editor) );
   					
   	   				// add the selected resource to the resource list for each pane
   					//
   	   				for( IDisplayPane pane : panesToLoad ) {    			

   	   					IDescriptor mapDescr = pane.getDescriptor();
   	   					
   	   					if( useNewTimeMatcher ) {
   	   						mapDescr.setTimeMatcher( timeMatcher ); 
   	   						mapDescr.setNumberOfFrames( timeMatcher.getNumFrames() );
   	   						DataTime[] dataTimes = timeMatcher.getFrameTimes().toArray( new DataTime[0] );

   	   						if( dataTimes == null || dataTimes.length == 0 ) {
   	   							
   	   						}
   	   						else {
   	   							mapDescr.setDataTimes( dataTimes );
   	   						}
   	   						
   	   						if( timeMatcher.isForecast() ) {
   	   							mapDescr.setFrame( 0 );
   	   						}
   	   						else {
   	   							mapDescr.setFrame( dataTimes.length-1 );
   	   						}

   	   					}
   	   					
   	   					ResourceList rscList = mapDescr.getResourceList();

   	   					AbstractVizResource<?,?> rsc = rscData.construct( ldProp, mapDescr);
   	   					
   	   					rscList.add( rsc, rscProp );
   	   				}   

   	   				editor.refresh();
   	   				NcEditorUtil.refreshGUIElements(editor);
   				}
   				catch (VizException e ) {
   					System.out.println( "Error Adding Resource to List: " + e.getMessage() );
					MessageDialog errDlg = new MessageDialog( 
							shell, "Error", null, 
							"Error Creating Resource:"+rscName.toString()+"\n\n"+e.getMessage(),
							MessageDialog.ERROR, new String[]{"OK"}, 0);
					errDlg.open();
   				}
   				 
   				if( done ) {
   					rscSelDlg.close(); 
   				}
   			}
   		});

   		boolean isMultipane = (NcEditorUtil.getPaneLayout(editor).getNumberOfPanes() > 1 );
   		
   	    rscSelDlg.open( false, // no replaceResource option  
   						false, // replace button not enabled
   						prevSeldRscName, isMultipane,
   						dispType,
   						SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL );
		return null;
	}
}
