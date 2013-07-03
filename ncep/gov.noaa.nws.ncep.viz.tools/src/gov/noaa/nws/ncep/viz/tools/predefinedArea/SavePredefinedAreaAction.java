package gov.noaa.nws.ncep.viz.tools.predefinedArea;

import java.util.Iterator;

import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;
import gov.noaa.nws.ncep.viz.common.area.IAreaProviderCapable;
import gov.noaa.nws.ncep.viz.common.area.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.common.area.NcAreaProviderMngr;
import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.common.ui.UserEntryDialog;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.core.CorePlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Save a PredefinedArea using the current area in the currently selected
 * renderable display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/20/13     #862     G. Hull         created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class SavePredefinedAreaAction extends AbstractHandler {

    public SavePredefinedAreaAction() {
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

    	AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
    	NcDisplayType dt = NcEditorUtil.getNcDisplayType( editor );

    	try {    	
    		if( dt != NcDisplayType.NMAP_DISPLAY ) {
    			throw new VizException( "Can't save the area of a non-NcMap Display" );
    		}

    		IDisplayPane panes[] = NcEditorUtil.getSelectedPanes( editor );
    		IRenderableDisplay disp = panes[0].getRenderableDisplay();

    		if( !(disp instanceof IAreaProviderCapable) ||
    			!(disp instanceof INatlCntrsRenderableDisplay) ) {
    			throw new VizException( "Unable to get area from display.");
    		}
    		
    		// could also just get the area straight from the ncrenderable displa but 
    		// this is how its designed so stick with it.
    		IAreaProviderCapable aprov = (IAreaProviderCapable)disp;
    		AreaName aname = new AreaName( aprov.getSourceProvider(), aprov.getAreaName() );
    		
    		IGridGeometryProvider geomProv = NcAreaProviderMngr.createGeomProvider( aname );
    		
    		if( geomProv == null ) {
    			throw new VizException( "Error creating area for: "+aname.toString() );
    		}

			UserEntryDialog entryDlg = new UserEntryDialog( NcDisplayMngr.getCaveShell(),
					"Save Predefined Area", 
					"Enter the name of the new Predefined Area:", 
					aprov.getAreaName() );

			String pareaName = entryDlg.open();

			if( pareaName == null || // cancel pressed
				pareaName.isEmpty() ) {
				return null;
			}
			aname = new AreaName( aprov.getSourceProvider(), pareaName );
			
			// if the area doesn't exist
			if( NcAreaProviderMngr.getSourceProviderFactory(
					aprov.getSourceProvider()).getAvailableAreaNames().contains( aname ) ) {
		    	MessageDialog confirmDlg = new MessageDialog( 
		    		NcDisplayMngr.getCaveShell(), "Confirm", null, 
		    			"A Predefined Area already exists with this name.\n\n"+
		    			"Do you want overwrite it?",
			    			MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
		    	confirmDlg.open();

		    	if( confirmDlg.getReturnCode() != MessageDialog.OK ) {
		    		return null;
		    	}           				   			    					    		
			}
			
			PredefinedArea newArea = PredefinedAreaFactory.savePredefinedArea( geomProv, pareaName, true );
    		
    		// change the initial area of this display to the newly saved area.
			// (If user imports this display they would expect the area name to be the newly 
			// saved one instead of the name of the display, and will also keep them from having
			// to save the area again if importing/saving the RBD.)
    		((INatlCntrsRenderableDisplay)disp).setInitialArea( newArea );
    	}
    	catch ( VizException e ) {
			MessageDialog errDlg = new MessageDialog( 
					NcDisplayMngr.getCaveShell(), "Error", null, 
					e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
    	}        	
        	
        return null;
    }
}