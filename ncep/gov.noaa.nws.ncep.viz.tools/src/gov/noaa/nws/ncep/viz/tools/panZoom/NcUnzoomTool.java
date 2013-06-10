/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * May 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.panZoom;


import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.PredefinedArea;
import gov.noaa.nws.ncep.viz.tools.predefinedArea.PredefinedAreaAction;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * 
 * <pre>
 *  
 *   SOFTWARE HISTORY
 *  
 * Date         Ticket#        Engineer        Description
 * ------------ ----------    -----------     --------------------------
 * 09/29/09     #169         Greg Hull       Initial Creation.
 * 12/02/09                  Greg Hull       broke out from combined PanZoomTool
 * 10/22/10		#32          Gang Zhang		 Modified for Unzoom
 * 11/28/12     #630         Greg Hull       fix for satellite/resource-defined areas
 * 02/22/13     #972         Greg Hull       AbstractNcEditor, INatlCntrsRenderableDisplay
 *   
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcUnzoomTool extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {	    	
    	AbstractEditor mapEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

    	try {
    		if( mapEditor != null ) {
    			IDisplayPane[] panes = (NcEditorUtil.arePanesGeoSynced( mapEditor ) ? 
    					mapEditor.getDisplayPanes() : NcEditorUtil.getSelectedPanes(mapEditor) );

    			for( IDisplayPane pane : panes )  {	
    				INatlCntrsRenderableDisplay disp = (INatlCntrsRenderableDisplay) pane.getRenderableDisplay();
    		    	
    		    	PredefinedArea origArea = (PredefinedArea)disp.getInitialArea();
    				
    				PredefinedAreaAction.setPredefinedArea( pane, origArea );
    			}
    		}
    	} 
    	catch( VizException e ) {
        	MessageDialog errDlg = new MessageDialog( 
        			NcDisplayMngr.getCaveShell(), "Error", null, 
        			"Error Unzooming to Original Area:\n\n"+e.getMessage(),
        			MessageDialog.ERROR, new String[]{"OK"}, 0);
        	errDlg.open();
    	}
    	
    	mapEditor.refresh();
    	//mapEditor.registerMouseHandler(inputHandler);
    	return null;
    }
    
}
