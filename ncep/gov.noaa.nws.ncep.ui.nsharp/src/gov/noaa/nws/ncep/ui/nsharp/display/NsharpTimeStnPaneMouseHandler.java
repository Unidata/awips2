/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpTimeStnPaneMouseHandler
 * 
 * This java class performs the NSHARP NsharpTimeStnPaneMouseHandler functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/22/2012	229			Chin Chen	Initial coding for multiple Panes implementations
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;


import org.eclipse.swt.SWT;

import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSkewTPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpTimeStnPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpShowTextDialog;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpTimeStnPaneMouseHandler extends NsharpAbstractMouseHandler{

    
    public NsharpTimeStnPaneMouseHandler(NsharpEditor editor, IDisplayPane pane) {
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
            NsharpTimeStnPaneResource timeStnRsc = (NsharpTimeStnPaneResource)getDescriptor().getPaneResource();
            if(timeStnRsc.getTimeLineRectangle().contains((int) c.x, (int) c.y)== true) {
            	this.mode = Mode.TIMELINE_DOWN;
            }
            else if(timeStnRsc.getStnIdRectangle().contains((int) c.x, (int) c.y) == true) {
            	this.mode = Mode.STATIONID_DOWN;
            }
            else if(timeStnRsc.getSndRectangle().contains((int) c.x, (int) c.y) == true) {
            	this.mode = Mode.SNDTYPE_DOWN;
            }
            editor.refresh();
        }

        return false;
    }


    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
    	if (getPaneDisplay() == null) {
            return false;
        }
    	if(editor!=null){		
        	// button 1 is left mouse button 
    		if (mouseButton == 1 ){
    			NsharpTimeStnPaneResource timeStnRsc = (NsharpTimeStnPaneResource)getDescriptor().getPaneResource();
    			Coordinate c = editor.translateClick(x, y);
    			 if(timeStnRsc.getTimeLineRectangle().contains((int) c.x, (int) c.y) == true && this.mode == Mode.TIMELINE_DOWN) {
    				//data time line has been touched, and may be changed
    				timeStnRsc.getRscHandler().handleUserClickOnTimeLine(c,shiftDown);// FixMark:clickOnTimeStnPane
    				handleMouseMove(x,y);

    				NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
    				if(textarea != null){
    					textarea.refreshTextData();
    				}
    				
    			}
    			else if(timeStnRsc.getStnIdRectangle().contains((int) c.x, (int) c.y) == true && this.mode == Mode.STATIONID_DOWN) {
    				//stn id line has been touched, and may be changed
    				timeStnRsc.getRscHandler().handleUserClickOnStationId(c,shiftDown);// FixMark:clickOnTimeStnPane
    				handleMouseMove(x,y);
    			}
    			else if(timeStnRsc.getSndRectangle().contains((int) c.x, (int) c.y) == true && this.mode == Mode.SNDTYPE_DOWN) {
    				//stn id line has been touched, and may be changed
    				timeStnRsc.getRscHandler().handleUserClickOnSndLine(c,shiftDown);// FixMark:clickOnTimeStnPane
    				handleMouseMove(x,y);
    			}
    			
    			this.mode = Mode.CREATE;
    		} else if(mouseButton == 3){
    			//right mouse button
    			//System.out.println("hodo handleMouseUp right button");
    			NsharpMapResource.bringMapEditorToTop();
    		}
    		editor.refresh();
    	}
        return false;
    }
    // FixMark:clickOnTimeStnPane
    @Override
    public boolean handleKeyDown(int keyCode) {
    	//System.out.println("key down="+(char)keyCode+ " code ="+keyCode);
    	
    	if ((keyCode & SWT.SHIFT) != 0)  {
    		shiftDown = true;
            
            return true;
        } 
    	return false;
    }

    @Override
    public boolean handleKeyUp(int keyCode) {
    	//String s = "key up="+(char)keyCode;
    	//System.out.println(s+ " code ="+keyCode);
    	if (getPaneDisplay() == null) {
    		return false;
    	}
    	if (keyCode == SWT.SHIFT) {
            shiftDown = false;
            return true;
        }
        return false;
    }
 //End FixMark:clickOnTimeStnPane
}
