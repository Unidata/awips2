/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpDataPaneMouseHandler
 * 
 * This java class performs the NSHARP NsharpDataPaneMouseHandler functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 03/09/2011               Chin Chen   Updated for R1G2-9
 * 06/14/2011   11-5        Chin Chen   migration
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;


import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpDataPaneResource;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpDataPaneMouseHandler extends NsharpAbstractMouseHandler{

    
    public NsharpDataPaneMouseHandler(NsharpEditor editor, IDisplayPane pane) {
    	super(editor,pane);
   }
 


    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
    	theLastMouseX = x;
    	theLastMouseY = y;
    	if (getPaneDisplay() == null) {
    		return false;
    	}
    	else if (mouseButton == 1) {
    		//System.out.println("handleMouseDown");
    		this.mode = Mode.CREATE;
    		Coordinate c = editor.translateClick(x, y);  
    		NsharpDataPaneResource rsc = (NsharpDataPaneResource)getDescriptor().getPaneResource();
    		if((rsc.getDataPanel1Background().contains(c) == true || rsc.getDataPanel2Background().contains(c) == true) && rsc.isSumP1Visible() == true) {
    			this.mode = Mode.PARCELLINE_DOWN;
    			//changeMouse(this.mode);
    		}
    		editor.refresh();
    	}

    	return false;
    }


    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
    	//System.out.println("skewtRsc handleMouseUp");
    	if (getPaneDisplay() == null) {
    		return false;
    	}
    	if(editor!=null){
    		// button 1 is left mouse button 
    		if (mouseButton == 1 ){
    			Coordinate c = editor.translateClick(x, y);
    			NsharpDataPaneResource rsc = (NsharpDataPaneResource)getDescriptor().getPaneResource();
    			if((rsc.getDataPanel1Background().contains(c) == true || rsc.getDataPanel2Background().contains(c) == true )&& this.mode == Mode.PARCELLINE_DOWN) {
    				//System.out.println("skewtRsc handleMouseUp panels");
    				rsc.setUserPickedParcelLine(c);
    				
    			}
    			this.mode = Mode.CREATE;
    		} else if(mouseButton == 3){
    			//right mouse button
    			//System.out.println("skewtRsc handleMouseUp right button");
    			NsharpMapResource.bringMapEditorToTop();
    		}
    		editor.refresh();
    	}
    	return false;
    }
}
