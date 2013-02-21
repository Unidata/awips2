/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * May 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.panZoom;


import gov.noaa.nws.ncep.viz.tools.predefinedArea.PredefinedAreaAction;
import gov.noaa.nws.ncep.viz.ui.display.NCDisplayPane;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * <pre>
 *  
 *   SOFTWARE HISTORY
 *  
 * Date         Ticket#         Engineer        Description
 * ------------ ----------      -----------     --------------------------
 * 09/29/09     #169        	Greg Hull       Initial Creation.
 * 12/02/09                   	Greg Hull       broke out from combined PanZoomTool
 * 10/22/10		#329		  	Gang Zhang		Modified for Unzoom
 * 11/28/12     #630            Greg Hull       fix for satellite/resource-defined areas
 *   
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcUnzoomTool extends AbstractHandler{

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {	
        //super.execute(arg0);
    	
    	NCMapEditor mapEditor = (NCMapEditor)NmapUiUtils.getActiveNatlCntrsEditor();

    	try {
    		if( mapEditor != null ) {
    			IDisplayPane[] panes = (mapEditor.arePanesGeoSynced() ? 
    					mapEditor.getDisplayPanes() : mapEditor.getSelectedPanes() );

	    for( IDisplayPane pane : panes )  {	
    		    	NCMapRenderableDisplay disp = (NCMapRenderableDisplay) pane.getRenderableDisplay();
	    	
    		    	PredefinedArea origArea = disp.getInitialArea();
	    	
    				PredefinedAreaAction.setPredefinedArea( pane, origArea );
	    }
    	}
    }
    	catch( VizException e ) {
        	MessageDialog errDlg = new MessageDialog( 
        			NmapUiUtils.getCaveShell(), "Error", null, 
        			"Error Unzooming to Original Area:\n\n"+e.getMessage(),
        			MessageDialog.ERROR, new String[]{"OK"}, 0);
        	errDlg.open();
		}
		
    	mapEditor.refresh();
    	//mapEditor.registerMouseHandler(inputHandler);
    	return null;
    }

}
