/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * May 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.panZoom;


import gov.noaa.nws.ncep.viz.tools.predefinedArea.PredefinedAreaAction;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

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
 *   
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcUnzoomTool extends AbstractHandler{//AbstractNCModalMapTool {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {	
        //super.execute(arg0);
    	
    	NCMapEditor mapEditor = (NCMapEditor)NmapUiUtils.getActiveNatlCntrsEditor();
    	if( mapEditor != null )
    	{
    	IDisplayPane[] panes = ( mapEditor.arePanesGeoSynced() 
	    							? mapEditor.getDisplayPanes() 
	    							: mapEditor.getSelectedPanes() );

	    for( IDisplayPane pane : panes )  {	
	    	
	    	String pdaName = getPDAName(pane);
	    	PredefinedAreaAction.setGeographicArea(pdaName);
	    	
	    }
    	}
                                      

       // mapEditor.refresh();    mapEditor.registerMouseHandler(inputHandler);
        return null;
    }
    
    /**
     * get the predefinedAreaName
     * @param idPane: the pane to be unzoomed
     * @return: predefinedAreaName
     */
    private String getPDAName(IDisplayPane idPane){
    	
    	String pdaName = "";
		
		if( idPane == null )
			return pdaName;			

		IRenderableDisplay disp = idPane.getRenderableDisplay();
		if( disp == null ) 
			return pdaName;
				
		if(disp instanceof NCMapRenderableDisplay){
			pdaName = ((NCMapRenderableDisplay)disp).getPredefinedAreaName();
		}
		
		return pdaName;
    }

}
