package gov.noaa.nws.ncep.ui.nsharp.display;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpHodoPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpShowTextDialog;

import org.eclipse.swt.graphics.Point;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpHodoPaneMouseHandler extends NsharpAbstractMouseHandler{
    private boolean cursorInHodo = false;

    public NsharpHodoPaneMouseHandler(NsharpEditor editor, IDisplayPane pane) {
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
           // changeMouse(this.mode);
            //System.out.println("current cursor x " + display.getCursorLocation().x + " y "+ display.getCursorLocation().y);
            //System.out.println("picked pt before translate x " + x + " y "+ y);
            Coordinate c = editor.translateClick(x, y);   		
            NsharpHodoPaneResource hodoRsc = (NsharpHodoPaneResource)getDescriptor().getPaneResource();
            boolean graphEditOn = hodoRsc.getRscHandler().isEditGraphOn();
            if(hodoRsc.getHodoBackground().contains(c) == true) {
            	
            	if(graphEditOn){
            		Point curPoint = display.getCursorLocation();
                	int xdiff = x- curPoint.x;
                	int ydiff = y- curPoint.y;
                	Coordinate anchoredPtC = hodoRsc.getRscHandler().getClosestHodoPoint(c);
                	if(anchoredPtC.x != 0 && anchoredPtC.y !=0){
                		anchorPointxy = editor.translateInverseClick(anchoredPtC);
                		display.setCursorLocation((int)anchorPointxy[0]-xdiff,(int)anchorPointxy[1]-ydiff);
                		this.mode = Mode.HODO_DOWN_MOVE;
                	}
            	}
            	else{
            		this.mode = Mode.HODO_DOWN;
            	}
            	//changeMouse(this.mode);
            }
            
           /* else if(hodoRsc.getTimeLineRectangle().contains((int) c.x, (int) c.y)== true) {
            	this.mode = Mode.TIMELINE_DOWN;
            	//changeMouse(this.mode);
            }
            else if(hodoRsc.getStnIdRectangle().contains((int) c.x, (int) c.y) == true) {
            	this.mode = Mode.STATIONID_DOWN;
            	//changeMouse(this.mode);
            }*/
            editor.refresh();
        }
        
        return false;
    }
    @Override
    public boolean handleMouseDownMove(int aX, int aY, int button) {
    	
    	if (getPaneDisplay() == null) {
    		return false;
    	}
    	else if (button == 1) {

    		Coordinate c = editor.translateClick(aX, aY);
    		//make sure it is clicked within skewt area
    		NsharpHodoPaneResource hodoRsc = (NsharpHodoPaneResource)getDescriptor().getPaneResource();
    		boolean graphEditOn = hodoRsc.getRscHandler().isEditGraphOn();
    		if(this.mode == Mode.HODO_DOWN_MOVE && graphEditOn){
    			if(hodoRsc.getHodoBackground().contains(c) == true ) {
    				c = editor.translateClick(aX, aY);   		
    				hodoRsc.getRscHandler().setInteractiveHodoPointCoordinate(c);
    				editor.refresh();

    			}
    			return false;
    		}
    		if (prefManager.handleLongClick(ZOOMIN_PREF, button)
    				|| prefManager.handleLongClick(ZOOMOUT_PREF, button)) {
    			theLastMouseX = aX;
    			theLastMouseY = aY;
    		}
    		if ((!prefManager.handleDrag(PAN_PREF, button)) ||  currentPane == null)
    			return false;
    		IView tmpView = (IView)currentPane.getRenderableDisplay().getView().clone();
    		tmpView.shiftExtent(new double[] { aX, aY }, new double[] {
    				theLastMouseX, theLastMouseY },currentPane.getTarget());
    		IExtent tmpExtent = tmpView.getExtent();
    		double percentage = getPanningPercentage();
    		double xMinThreshold = tmpExtent.getMinX()
    		+ (tmpExtent.getMaxX() - tmpExtent.getMinX()) * percentage;
    		double xMaxThreshold = tmpExtent.getMinX()
    		+ (tmpExtent.getMaxX() - tmpExtent.getMinX())
    		* (1.0 - percentage);
    		double yMinThreshold = tmpExtent.getMinY()
    		+ (tmpExtent.getMaxY() - tmpExtent.getMinY()) * percentage;
    		double yMaxThreshold = tmpExtent.getMinY()
    		+ (tmpExtent.getMaxY() - tmpExtent.getMinY())
    		* (1.0 - percentage);

    		double height = currentPane.getRenderableDisplay().getWorldHeight();
    		double width = currentPane.getRenderableDisplay().getWorldWidth();

    		int aX2 = aX, aY2 = aY;

    		if ((0 <= xMinThreshold && width >= xMaxThreshold) == false) {
    			if (((width < xMaxThreshold && theLastMouseX < aX) || (0 > xMinThreshold && theLastMouseX > aX)) == false) {
    				aX2 = (int) theLastMouseX;
    			}
    		}

    		if ((0 <= yMinThreshold && height >= yMaxThreshold) == false) {
    			if (((height < yMaxThreshold && theLastMouseY < aY) || (0 > yMinThreshold && theLastMouseY > aY)) == false) {
    				aY2 = (int) theLastMouseY;
    			}
    		}

    		if (aX2 != theLastMouseX || aY2 != theLastMouseY) {
    			currentPane.shiftExtent(new double[] { aX2, aY2 }, new double[] {
    					theLastMouseX, theLastMouseY });
    		}
    		theLastMouseX = aX;
    		theLastMouseY = aY;
    		return true;
    	}
    	return false;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
    	//System.out.println("hodoPanemouseHandler " + this+" handleMouseMove with editor"+editor);
    	if(editor != null) {
         	editor.setFocus();
    	}
    	if (getPaneDisplay() == null) {
    		return false;
    	}
        //System.out.println("mouseHandler " + this+" handleMouseMove with editor"+editor);
        this.mode = Mode.CREATE;
        //changeMouse();
        if(editor != null) {
        	Coordinate c = editor.translateClick(x, y);
        	//System.out.println(" Hodo-handleMouseMove! x="+x+" y="+y+" C.x="+c.x + " c.y="+c.y);
        	NsharpHodoPaneResource hodoRsc = (NsharpHodoPaneResource)getDescriptor().getPaneResource();
        	if (hodoRsc.getHodoBackground().contains(c)){
        		cursorInHodo=true;
        		hodoRsc.setCursorInHodo(true);
        		try {
        			hodoRsc.updateDynamicData(c);
        			editor.refresh();
        		} catch (VizException e) {
        			// TODO Auto-generated catch block
        			e.printStackTrace();
        		}
        	}
        	else {
        		if(cursorInHodo== true){
        			cursorInHodo=false;
        			hodoRsc.setCursorInHodo(false);
        		}
        	}
        }
        return false;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
    	//System.out.println("hodo handleMouseUp");
    	if (getPaneDisplay() == null) {
            return false;
        }
    	if(editor!=null){		
        	// button 1 is left mouse button 
    		if (mouseButton == 1 ){
    			NsharpHodoPaneResource hodoRsc = (NsharpHodoPaneResource)getDescriptor().getPaneResource();
    			Coordinate c = editor.translateClick(x, y);
    			 if(hodoRsc.getHodoBackground().contains(c) == true ) {
    				//make sure it is clicked within hodo area
    				boolean graphEditOn = hodoRsc.getRscHandler().isEditGraphOn();
    				if(graphEditOn && this.mode == Mode.HODO_DOWN_MOVE){
    					NsharpShowTextDialog osDia =  NsharpShowTextDialog.getAccess( );    
    					if(osDia != null)
    						osDia.refreshTextData();
    				}else if (this.mode == Mode.HODO_DOWN){
    					hodoRsc.getRscHandler().setHodoStmCenter(c);
    				}
    			}
    			/*else if(hodoRsc.getTimeLineRectangle().contains((int) c.x, (int) c.y) == true && this.mode == Mode.TIMELINE_DOWN) {
    				//data time line has been touched, and may be changed
    				hodoRsc.getRscHandler().handleUserClickOnTimeLine(c);
    				handleMouseMove(x,y);

    				NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
    				if(textarea != null){
    					textarea.refreshTextData();
    				}
    				
    			}
    			else if(hodoRsc.getStnIdRectangle().contains((int) c.x, (int) c.y) == true && this.mode == Mode.STATIONID_DOWN) {
    				//stn id line has been touched, and may be changed
    				hodoRsc.getRscHandler().handleUserClickOnStationId(c);
    				handleMouseMove(x,y);
    			}*/
    			
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
}
