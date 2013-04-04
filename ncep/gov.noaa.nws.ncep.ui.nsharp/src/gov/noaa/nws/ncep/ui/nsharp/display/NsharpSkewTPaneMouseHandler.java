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

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSkewTPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpWitoPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpShowTextDialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpSkewTPaneMouseHandler extends NsharpAbstractMouseHandler{
	private  Cursor editingCursor ;
	private  Cursor movingCursor ;
    //private boolean cursorInSkewT = false;
    public NsharpSkewTPaneMouseHandler(NsharpEditor editor, IDisplayPane pane) {
    	super(editor,pane);
    	editingCursor = new Cursor( display, SWT.CURSOR_CROSS);
    	movingCursor = new Cursor( display, SWT.CURSOR_SIZEWE);
    }
    @Override
    public boolean handleKeyDown(int keyCode) {
    	//System.out.println("key down="+(char)keyCode+ " code ="+keyCode);
    	if ((keyCode & SWT.SHIFT) != 0)  {
            shiftDown = true;
            //System.out.println("shift pressed");
            return true;
        } else if (shiftDown && keyCode == KEY_Z ) {
        	zDownWhileShiftDown=true;
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
    	NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
    	// Arrow key handling is now under NsharpSkewTPaneDescriptor.frameCoordinator.changeFrame()
    	/*if (keyCode == SWT.ARROW_DOWN)  {
            skewRsc.getRscHandler().setSteppingStnIdList(FrameChangeOperation.NEXT) ;
            return true;
        }else if (keyCode == SWT.ARROW_UP)  {
            //System.out.println("Arrow up");
        	skewRsc.getRscHandler().setSteppingStnIdList(FrameChangeOperation.PREVIOUS) ;
            return true;
        }else if (keyCode == SWT.ARROW_LEFT)  {
            //System.out.println("Arrow left");
        	skewRsc.getRscHandler().setSteppingTimeLine(FrameChangeOperation.PREVIOUS, FrameChangeMode.TIME_ONLY) ;
            return true;
        }else if (keyCode == SWT.ARROW_RIGHT)  {
            //System.out.println("Arrow right");
            skewRsc.getRscHandler().setSteppingTimeLine(FrameChangeOperation.NEXT, FrameChangeMode.TIME_ONLY) ;
            return true;
        } else */if (keyCode == SWT.SHIFT) {
            shiftDown = false;
            return true;
        }else if (zDownWhileShiftDown && keyCode == KEY_Z ) {
            //System.out.println("Shift+Z is entered");
            zDownWhileShiftDown = false;
            skewRsc.toggleCurseDisplay();
            return true;
        }
        return false;
    }
    /*public static void startEditingCursor(){
    	editingCursor = new Cursor( display, SWT.CURSOR_CROSS);
		cursorControl = display.getCursorControl();
		if(cursorControl!=null && editingCursor!=null)
			cursorControl.setCursor(editingCursor);
   	}
   	public static void stopEditingCursor(){
   		if(editingCursor!=null&&cursorControl!=null ){
   			cursorControl.setCursor(null);
   		}
   		if(editingCursor!=null){
   			editingCursor.dispose();
   			editingCursor= null;
   		}
   	}*/
    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
    	//ystem.out.println("handleMouseDown x="+ x+ " y="+y);
    	theLastMouseX = x;
		theLastMouseY = y;
        if (getPaneDisplay() == null) {
            return false;
        }
        else if (editor != null && mouseButton == 1) {
        	//System.out.println("handleMouseDown");
        	
            this.mode = Mode.CREATE;
           // changeMouse(this.mode);
            //System.out.println("current cursor x " + display.getCursorLocation().x + " y "+ display.getCursorLocation().y);
            //System.out.println("picked pt before translate x " + x + " y "+ y);
            Coordinate c = editor.translateClick(x, y);   		
            NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
            boolean graphEditOn = skewRsc.getRscHandler().isEditGraphOn();
            if(skewRsc.getSkewTBackground().contains(c) == true &&  graphEditOn) {
            	//make sure it is clicked within skewt area
            	//save current cursor coordinate difference between display and view point
            	Point curPoint = display.getCursorLocation();
            	int xdiff = x- curPoint.x;
            	int ydiff = y- curPoint.y;
            	Coordinate anchoredPtC;
            	//get editing cursor point
            	anchoredPtC= skewRsc.getPickedTempPoint(c);
            	if(anchoredPtC.x == 0 && anchoredPtC.y==0)
            		//cursor is not within editing range ( i.e within 4 degree range from temp/dew line)
            		return false;
            	skewRsc.getRscHandler().setInteractiveTempPointCoordinate(anchoredPtC);
            	//System.out.println("returned pt before reverse translate x " + anchoredPtC.x + " y "+ anchoredPtC.y);

            	//Translate  world screen coordinate to screen (x,y) coordinate
            	anchorPointxy = editor.translateInverseClick(anchoredPtC);
            	
            	//System.out.println("returned pt after reverse translate x " + (int)anchorPointxy[0] + " y "+ (int)anchorPointxy[1]);

            	display.setCursorLocation((int)anchorPointxy[0]-xdiff,(int)anchorPointxy[1]-ydiff);
            	this.mode = Mode.SKEWT_DOWN;
            	//changeMouse(this.mode);
            	
            	//System.out.println("handleMouseDown x "+((int)pointxy[0]-xdiff)+ " y "+((int)pointxy[1]-ydiff));
            }
            editor.refresh();
        }

        return false;
    }
    @Override
    public boolean handleMouseDownMove(int aX, int aY, int button) {
    	//System.out.println("handleMouseDownMove x="+ aX+ " y="+aY);
    	if (getPaneDisplay() == null || editor == null) {
    		return false;
    	}
    	else if (button == 1) {

    		Coordinate c = editor.translateClick(aX, aY);
    		//make sure it is clicked within skewt area
    		NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
    		if(skewRsc==null)
    			return false;
    		boolean graphEditOn =skewRsc.getRscHandler().isEditGraphOn();
    		if(this.mode == Mode.SKEWT_DOWN && graphEditOn){
    			if(skewRsc.getSkewTBackground().contains(c) == true) {
    				//NOTE::::keep y axis un-changed when moving mouse cursor
    				c = editor.translateClick(aX, anchorPointxy[1]);   		
    				skewRsc.getRscHandler().setInteractiveTempPointCoordinate(c);
    				editor.refresh();
    			}
    			return false;
    		} 
    		if (prefManager.handleLongClick(ZOOMIN_PREF, button)
    				|| prefManager.handleLongClick(ZOOMOUT_PREF, button)) {
    			theLastMouseX = aX;
    			theLastMouseY = aY;
    		}
    		//VizDisplayPane currentPane=((NsharpEditor)editor).getSelectedPane();
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
    		NsharpWitoPaneResource witoRsc = skewRsc.getRscHandler().getWitoPaneRsc();
    		if(witoRsc != null)
    			witoRsc.createAllWireFrameShapes();
    		return true;

    	}
    	return false;
    }


    @Override
    public boolean handleMouseMove(int x, int y) {
    	if(editor != null) {
    		editor.setFocus();
    	}
    	if (getPaneDisplay() == null) {
    		return false;
    	}
    	//System.out.println("skewtPanemouseHandler " + this+" handleMouseMove with editor"+editor);
    	this.mode = Mode.CREATE;
    	NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
    	if(editor != null && skewRsc !=null) {
    		Coordinate c = editor.translateClick(x, y);  
    		//System.out.println(" skewt-handleMouseMove! x="+x+" y="+y+" C.x="+c.x + " c.y="+c.y);
    		if (skewRsc.getSkewTBackground().contains(c)){
    			//always update coordinate C to SkewT editor
    			boolean graphEditOn = skewRsc.getRscHandler().isEditGraphOn();
    			if(graphEditOn == true){
    				//Point curPoint = display.getCursorLocation();
                	//int xdiff = x- curPoint.x;
                	//int ydiff = y- curPoint.y;
                	Coordinate anchoredPtC;
                	//get editing cursor point
                	anchoredPtC= skewRsc.getPickedTempPoint(c);
                	int currentSkewTEditMode = skewRsc.getCurrentSkewTEditMode();
                	if(anchoredPtC.x != 0 || anchoredPtC.y!=0){
                		//cursor is  within editing range ( i.e within 2 degree range from temp/dew line)
                		if(currentSkewTEditMode == NsharpConstants.SKEWT_EDIT_MODE_EDITPOINT)
                		display.getCursorControl().setCursor(editingCursor);      	
                		else if(currentSkewTEditMode == NsharpConstants.SKEWT_EDIT_MODE_MOVELINE)
                			display.getCursorControl().setCursor(movingCursor); 
                	}
                	else
                		display.getCursorControl().setCursor(null);
    			}
    			//cursorInSkewT=true;
    			skewRsc.setCursorInSkewT(true);
    			try {
    				skewRsc.updateDynamicData(c);
    				editor.refresh();
    			} catch (VizException e) {
    				e.printStackTrace();
    			}
    		}
    		else{
    			skewRsc.setCursorInSkewT(false);
    		}
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
			NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
        	// button 1 is left mouse button 
    		if (mouseButton == 1 ){
    			Coordinate c = editor.translateClick(x, y);
    			if(skewRsc.getSkewTBackground().contains(c) == true && this.mode == Mode.SKEWT_DOWN) {// && mouseDownMove == true) {
    				//stopEditingCursor();
    				skewRsc.getRscHandler().setPlotInteractiveTemp(false);
    				int currentSkewTEditMode = skewRsc.getCurrentSkewTEditMode();
    				if(currentSkewTEditMode == NsharpConstants.SKEWT_EDIT_MODE_EDITPOINT)
    				skewRsc.getRscHandler().applyInteractiveTempPoint();
    				else if(currentSkewTEditMode == NsharpConstants.SKEWT_EDIT_MODE_MOVELINE)
    					skewRsc.getRscHandler().applyMovingTempLine();
    				NsharpShowTextDialog osDia =  NsharpShowTextDialog.getAccess( );    
    				if(osDia != null)
    					osDia.refreshTextData();
    			}
    			this.mode = Mode.CREATE;
    		} else if(mouseButton == 3){
    			//right mouse button
    			boolean graphEditOn = skewRsc.getRscHandler().isEditGraphOn();
    			if(!graphEditOn)
    			NsharpMapResource.bringMapEditorToTop();
    			else {
    				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    				Menu menu = new Menu(shell, SWT.POP_UP);
    				menu.setVisible(true);
    				MenuItem item1 = new MenuItem(menu, SWT.PUSH);
    				item1.setText("Edit Point");
    				item1.addListener(SWT.Selection, new Listener() {
    					@Override
    					public void handleEvent(Event event) {
    						MenuItem selItem = (MenuItem) event.widget;
    						String string = selItem.getText();
    						if ("Edit Point".equals(string)) {
    							int currentSkewTEditMode = NsharpConstants.SKEWT_EDIT_MODE_EDITPOINT;
    							NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
    							skewRsc.setCurrentSkewTEditMode(currentSkewTEditMode);
    						}
    					}

    				});
    				MenuItem item2 = new MenuItem(menu, SWT.PUSH);
    				item2.setText("Move Line");
    				item2.addListener(SWT.Selection, new Listener() {
    					@Override
    					public void handleEvent(Event event) {
    						MenuItem selItem = (MenuItem) event.widget;
    						String string = selItem.getText();
    						if ("Move Line".equals(string)) {
    							int currentSkewTEditMode = NsharpConstants.SKEWT_EDIT_MODE_MOVELINE;
    							NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
    							skewRsc.setCurrentSkewTEditMode(currentSkewTEditMode);    							
    						}
    					}

    				});
    				
    			}
    		}
    		editor.refresh();
    	}
        return false;
    }


 	@Override
	public boolean handleMouseExit(Event event) {
 		//System.out.println("skewtRsc handleMouseExit");
 		cursorInPane=false;    
 		NsharpSkewTPaneResource skewRsc = (NsharpSkewTPaneResource)getDescriptor().getPaneResource();
        if (skewRsc != null) {
            skewRsc.setCursorInSkewT(false);
        }
		return false;
	}
 	public void disposeCursor(){
 		editingCursor.dispose();
 	}
}
