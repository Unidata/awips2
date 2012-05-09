/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTMouseHandler
 * 
 * This java class performs the NSHARP NsharpSkewTMouseHandler functions.
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
package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;

import java.io.IOException;

import gov.noaa.nws.ncep.ui.nsharp.maprsc.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpShowTextDialog;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDisplay;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpSkewTMouseHandler extends InputHandlerDefaultImpl{//implements IInputHandler {

    private enum Mode {
        CREATE, SKEWT_DOWN, HODO_DOWN, HODO_DOWN_MOVE, TIMELINE_DOWN, STATIONID_DOWN, PARCELLINE_DOWN, HODO_WIND_MOTION_DOWN
    };

    private Display display;

    protected AbstractEditor editor;

    public void setEditor(AbstractEditor editor) {
		this.editor = editor;
	}

    private Mode mode = Mode.CREATE;
    private boolean cursorInSkewT = false, cursorInHodo=false;
    private double[] anchorPointxy;
    private NsharpBackgroundResource skewBkRsc;
    private MousePreferenceManager prefManager = MousePreferenceManager.getInstance();
    private static final String PAN_PREF = "com.raytheon.viz.ui.input.pan";
    private static final String PAN_PERCENTAGE = "panningPercentage";
    private static final String ZOOMIN_PREF = "com.raytheon.viz.ui.input.zoomin";

    private static final String ZOOMOUT_PREF = "com.raytheon.viz.ui.input.zoomout";

    private static final String CLEAR_PREF = "com.raytheon.viz.ui.clear";
    protected float theLastMouseX = 0;

    protected float theLastMouseY = 0;
    protected Double panningPercentage = null;
    private double getPanningPercentage() {
        if (panningPercentage == null) {
            HierarchicalPreferenceStore store = UiPlugin.getDefault()
                    .getPreferenceStore();
            panningPercentage = UiPlugin.getDefault().getPreferenceStore()
                    .getDouble(PAN_PERCENTAGE);

            if (panningPercentage < 0.0 || panningPercentage > 1.0) {
                // bad value set, reset and store
                panningPercentage = panningPercentage < 0.0 ? 0.0 : 1.0;

                store.setValue(PAN_PERCENTAGE, panningPercentage.doubleValue());
                try {
                    store.save();
                } catch (IOException e) {
                    UFStatus.getHandler().handle(Priority.PROBLEM, "Error saving panning percentage preference", e);
                }
            }
        }
        return panningPercentage.doubleValue();
    }
    public NsharpSkewTMouseHandler(AbstractEditor editor) {
        this.editor = editor;
        display = Display.getCurrent();
        //Chin MERGE skewBkRsc = NsharpBackgroundResource.getInstance();
        skewBkRsc = getDescriptor().getSkewTBkGResource();
    }
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseWheel(int, int)
     */
    public boolean handleMouseWheel(Event event, int x, int y) {
    	if(editor == null) {
            return false;
        }
    	//System.out.println("handleMouseWheel");
    	//if set to false, preempt other handler, so zooming is disabled in SkewT editor.
        com.raytheon.viz.ui.input.preferences.MouseEvent SCROLL_FORWARD = com.raytheon.viz.ui.input.preferences.MouseEvent.SCROLL_FORWARD;
        com.raytheon.viz.ui.input.preferences.MouseEvent SCROLL_BACK = com.raytheon.viz.ui.input.preferences.MouseEvent.SCROLL_BACK;
        if ((event.stateMask & SWT.SHIFT) == 0
                && editor.translateClick(x, y) != null) {
            IDisplayPane[] panes = editor.getDisplayPanes();
            if ((event.count < 0 && prefManager.handleEvent(ZOOMIN_PREF,
                    SCROLL_FORWARD))
                    || (event.count > 0 && prefManager.handleEvent(
                            ZOOMOUT_PREF, SCROLL_BACK))) {
                for (IDisplayPane pane : panes) {
                    pane.zoom(event.count, event.x, event.y);
                }
                return true;
            } else if ((event.count > 0 && prefManager.handleEvent(
                    ZOOMOUT_PREF, SCROLL_FORWARD))
                    || (event.count < 0 && prefManager.handleEvent(ZOOMIN_PREF,
                            SCROLL_BACK))) {
                for (IDisplayPane pane : panes) {
                    pane.zoom(-event.count, event.x, event.y);
                }
                return true;
            } else if ((event.count > 0 && prefManager.handleEvent(CLEAR_PREF,
                    SCROLL_FORWARD))
                    || (event.count < 0 && prefManager.handleEvent(CLEAR_PREF,
                            SCROLL_BACK))) {

                for (IDisplayPane pane : panes) {
                    pane.clear();
                }
                return true;
            }
        }
        return false;
   }

    @Override
    public boolean handleDoubleClick(int x, int y, int button) {
        return false;
    }

    private boolean shiftDown=false;
    private boolean zDownWhileShiftDown=false;
    private int KEY_Z= 122;
    @Override
    public boolean handleKeyDown(int keyCode) {
    	//System.out.println("key down="+(char)keyCode+ " code ="+keyCode);
    	if ((keyCode & SWT.SHIFT) != 0)  {
            shiftDown = true;
           // System.out.println("shift pressed");
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
    	if (keyCode == SWT.SHIFT) {
            shiftDown = false;
            return true;
        }else if (zDownWhileShiftDown && keyCode == KEY_Z ) {
            //System.out.println("Shift+Z is entered");
            zDownWhileShiftDown = false;
            NsharpSkewTResource skewRsc = getDescriptor().getSkewtResource();
            skewRsc.toggleCurseDisplay();
            return true;
        }
        return false;
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
    	theLastMouseX = x;
		theLastMouseY = y;
        if (getSkewtDisplay() == null) {
            return false;
        }
        else if (mouseButton == 1) {
        	//System.out.println("handleMouseDown");
            this.mode = Mode.CREATE;
           // changeMouse(this.mode);
            //System.out.println("current cursor x " + display.getCursorLocation().x + " y "+ display.getCursorLocation().y);
            //System.out.println("picked pt before translate x " + x + " y "+ y);
            Coordinate c = editor.translateClick(x, y);   		
            //Chin MERGE NsharpSkewTResource skewRsc = NsharpSkewTResource.getOrCreateSkewtResource();
            NsharpSkewTResource skewRsc = getDescriptor().getSkewtResource();
            //Chin MERGE NsharpBackgroundResource bkRsc = NsharpBackgroundResource.getOrCreateSkewTBkGResource();
            NsharpBackgroundResource bkRsc = getDescriptor().getSkewTBkGResource();
            boolean graphEditOn = skewRsc.isEditGraphOn();
            if(bkRsc.getSkewTBackground().contains(c) == true &&  graphEditOn) {
            	//make sure it is clicked within skewt area
            	//save current cursor coordinate difference between display and view point
            	Point curPoint = display.getCursorLocation();
            	int xdiff = x- curPoint.x;
            	int ydiff = y- curPoint.y;
            	Coordinate anchoredPtC;
            	anchoredPtC= skewRsc.getPickedTempPoint(c);
            	skewRsc.setInteractiveTempPointCoordinate(anchoredPtC);
            	//System.out.println("returned pt before reverse translate x " + anchoredPtC.x + " y "+ anchoredPtC.y);

            	//Translate  world screen coordinate to screen (x,y) coordinate
            	anchorPointxy = editor.translateInverseClick(anchoredPtC);
            	
            	//System.out.println("returned pt after reverse translate x " + (int)anchorPointxy[0] + " y "+ (int)anchorPointxy[1]);

            	display.setCursorLocation((int)anchorPointxy[0]-xdiff,(int)anchorPointxy[1]-ydiff);
            	this.mode = Mode.SKEWT_DOWN;
            	//changeMouse(this.mode);
            	
            	//System.out.println("handleMouseDown x "+((int)pointxy[0]-xdiff)+ " y "+((int)pointxy[1]-ydiff));
            }
            
            else if(bkRsc.getHodoBackground().contains(c) == true) {
            	
            	if(graphEditOn){
            		Point curPoint = display.getCursorLocation();
                	int xdiff = x- curPoint.x;
                	int ydiff = y- curPoint.y;
                	Coordinate anchoredPtC = skewRsc.getClosestHodoPoint(c);
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
            else if(bkRsc.getDataPanel1Background().contains(c) == true && skewRsc.getParcelLinesInPhysicalPanelNumber() == 1 ) {
            	this.mode = Mode.PARCELLINE_DOWN;
            	//changeMouse(this.mode);
            }
            //else if(bkRsc.getDataPanel3Background().contains(c) == true && skewRsc.getParcelLinesInPhysicalPanelNumber() == 3 ) {
            	//this.mode = Mode.PARCELLINE_DOWN;
            	//changeMouse(this.mode);
            //}
            else if(bkRsc.getDataTimelineBackground().contains(c) == true) {
            	this.mode = Mode.TIMELINE_DOWN;
            	//changeMouse(this.mode);
            }
            else if(bkRsc.getStationIdBackground().contains(c) == true) {
            	this.mode = Mode.STATIONID_DOWN;
            	//changeMouse(this.mode);
            }
            editor.refresh();
        }

        return false;
    }

    public boolean handleMouseDownMove(int aX, int aY, int button) {
    	
    	if (getSkewtDisplay() == null) {
    		return false;
    	}
    	else if (button == 1) {

    		Coordinate c = editor.translateClick(aX, aY);
    		//make sure it is clicked within skewt area
    		NsharpSkewTResource skewRsc = getDescriptor().getSkewtResource();
    		NsharpBackgroundResource bkRsc = getDescriptor().getSkewTBkGResource();
    		boolean graphEditOn = skewRsc.isEditGraphOn();
    		if(bkRsc.getSkewTBackground().contains(c) == true && this.mode == Mode.SKEWT_DOWN && graphEditOn) {

    			//NOTE::::keep y axis un-changed when moving mouse cursor
    			c = editor.translateClick(aX, anchorPointxy[1]);   		
    			skewRsc.setInteractiveTempPointCoordinate(c);
    			editor.refresh();
    			return false;
    		} else if(bkRsc.getHodoBackground().contains(c) == true && this.mode == Mode.HODO_DOWN_MOVE && graphEditOn) {
    			c = editor.translateClick(aX, aY);   		
    			skewRsc.setInteractiveHodoPointCoordinate(c);
    			editor.refresh();
    			return false;
    		}
    		/*else if(this.mode != Mode.HODO_DOWN_MOVE && this.mode != Mode.SKEWT_DOWN ){*/
    			if (prefManager.handleLongClick(ZOOMIN_PREF, button)
    					|| prefManager.handleLongClick(ZOOMOUT_PREF, button)) {
    				theLastMouseX = aX;
    				theLastMouseY = aY;
    			}
    			if ((!prefManager.handleDrag(PAN_PREF, button)) || editor == null)
    				return false;
    			IDisplayPane[] panes = editor.getDisplayPanes();
    			for (IDisplayPane p : panes) {
    				IView tmpView = (IView) p.getRenderableDisplay().getView().clone();
    				tmpView.shiftExtent(new double[] { aX, aY }, new double[] {
    						theLastMouseX, theLastMouseY }, p.getTarget());
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

    				double height = p.getRenderableDisplay().getWorldHeight();
    				double width = p.getRenderableDisplay().getWorldWidth();

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
    					p.shiftExtent(new double[] { aX2, aY2 }, new double[] {
    							theLastMouseX, theLastMouseY });
    				}
    			}
    			theLastMouseX = aX;
    			theLastMouseY = aY;
    			return true;
    		//}
    		
    			/*
    			IDisplayPane[] panes = editor.getDisplayPanes();
    			for( IDisplayPane p : panes ) {
    				p.shiftExtent(new double[] { aX, aY }, new double[] {
    						theLastMouseX, theLastMouseY });
    			}

    			editor.refresh();
    		

    			theLastMouseX = aX;
    			theLastMouseY = aY;
    			return true;*/

    	}
    	return false;
    }

    @Override
    public boolean handleMouseHover(int x, int y) {
        //System.out.println("mouseHandler handleMouseHover");
        return true;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
    	
        if (getSkewtDisplay() == null) {
            return false;
        }
        //System.out.println("mouseHandler " + this+" handleMouseMove with editor"+editor);
        this.mode = Mode.CREATE;
        //changeMouse();
        if(editor != null) {
        	 //Chin Merge next two lines commented out, the display pane should
             // be handling this
        	//editor.getActiveDisplayPane().setLastMouseX(x);
        	//editor.getActiveDisplayPane().setLastMouseY(y);
        	
        	// getSkewtDisplay().handleMouseMove(lastMouseX, lastMouseY);
        	Coordinate c = editor.translateClick(x, y);
        	NsharpBackgroundResource bkRsc = getDescriptor().getSkewTBkGResource();
        	NsharpSkewTResource skewRsc = getDescriptor().getSkewtResource();
        	if (bkRsc.getSkewTBackground().contains(c)){
        		//always update coordinate C to SkewT editor
        		cursorInSkewT=true;
        		skewRsc.setCursorInSkewT(true,c);
        		skewRsc.setCursorInHodo(false);
        		try {
        			skewBkRsc.updateDynamicData(c);
        			editor.refresh();

        		} catch (VizException e) {
        			// TODO Auto-generated catch block
        			e.printStackTrace();
        		}
        	}
        	else if (bkRsc.getHodoBackground().contains(c)){
        		//always update coordinate C to SkewT editor
        		cursorInHodo=true;
        		skewRsc.setCursorInHodo(true);
        		skewRsc.setCursorInSkewT(false,c);
        		try {
        			skewBkRsc.updateDynamicData(c);
        			editor.refresh();

        		} catch (VizException e) {
        			// TODO Auto-generated catch block
        			e.printStackTrace();
        		}
        	}
        	else {
        		if(cursorInSkewT== true){
        			cursorInSkewT=false;
        			skewRsc.setCursorInSkewT(false,c);
        		}
        		if(cursorInHodo== true){
        			cursorInHodo=false;
        			skewRsc.setCursorInHodo(false);
        		}
        	}
        	
			
        }
        
        return false;

    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
    	//System.out.println("skewtRsc handleMouseUp");
    	if(editor!=null){
			NsharpSkewTResource skewRsc = getDescriptor().getSkewtResource();
			NsharpBackgroundResource bkRsc = getDescriptor().getSkewTBkGResource();

        	// button 1 is left mouse button 
    		if (mouseButton == 1 ){
    			Coordinate c = editor.translateClick(x, y);
    			 if(bkRsc.getHodoBackground().contains(c) == true ) {
    				//make sure it is clicked within hodo area
    				boolean graphEditOn = skewRsc.isEditGraphOn();
    				if(graphEditOn && this.mode == Mode.HODO_DOWN_MOVE){
    					NsharpShowTextDialog osDia =  NsharpShowTextDialog.getAccess( );    
    					if(osDia != null)
    						osDia.refreshTextData();
    				}else if (this.mode == Mode.HODO_DOWN){
    					skewRsc.setHodoHouseC(c);
    					/* fixed TTR 6191 Coordinate c1 = bkRsc.getHodoBackground().getWorld().unMap(c.x, c.y);
    					c1 = WxMath.speedDir((float) c1.x, (float) c1.y);
    					skewRsc.getNsharpNative().nsharpLib.set_storm((float) c1.x, (float) c1.y);		*/
    				}
    				
    			}
    			else if(bkRsc.getDataTimelineBackground().contains(c) == true && this.mode == Mode.TIMELINE_DOWN) {
    				//data time line has been touched, and may be changed
    				skewRsc.setUserPickedDataTimeLine(c);
    				handleMouseMove(x,y);

    				NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
    				if(textarea != null){
    					textarea.refreshTextData();
    				}
    				
    			}
    			else if(bkRsc.getStationIdBackground().contains(c) == true && this.mode == Mode.STATIONID_DOWN) {
    				//data time line has been touched, and may be changed
    				skewRsc.setUserPickedStationId(c);
    				handleMouseMove(x,y);

    				
    			}
    			else if(bkRsc.getSkewTBackground().contains(c) == true && this.mode == Mode.SKEWT_DOWN) {// && mouseDownMove == true) {
    				skewRsc.setPlotInteractiveTemp(false);
    				skewRsc.applyInteractiveTempPoint();
    				//System.out.println("skewtRsc handleMouseUp MOVE_POINT");
    				NsharpShowTextDialog osDia =  NsharpShowTextDialog.getAccess( );    
    				if(osDia != null)
    					osDia.refreshTextData();
    				
    			}
    			else if((bkRsc.getDataPanel1Background().contains(c) == true /*|| bkRsc.getDataPanel3Background().contains(c) == true*/)&& this.mode == Mode.PARCELLINE_DOWN) {
    				//System.out.println("skewtRsc handleMouseUp panels");
    				NsharpDrawPanels drawPanel = NsharpDrawPanels.getInstance();
    				if(drawPanel != null){
    					drawPanel.setUserPickedParcelLine(c);
    				}
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

    //private void notifyResourceMiddleClicked(int x, int y) {
    //}

    /**
     * Gets the display you are using
     * 
     * @return SkewTDisplay
     */
    private NsharpSkewTDisplay getSkewtDisplay() {
    	if (editor != null) {
    		if (editor.getActiveDisplayPane().getDescriptor() instanceof NsharpSkewTDescriptor) {
    			// the ordering in the editor is paramount here!!!!!
    			if (editor.getActiveDisplayPane().getRenderableDisplay() instanceof NsharpSkewTDisplay) {
    				return ((NsharpSkewTDisplay) (editor.getActiveDisplayPane()
    						.getRenderableDisplay()));
    			}
    		}
    	}
    	return null;
    }

 	@Override
	public boolean handleMouseExit(Event event) {
		// TODO Auto-generated method stub
		return false;
	}
 	
    public NsharpSkewTDescriptor getDescriptor() {
        return (NsharpSkewTDescriptor) editor.getActiveDisplayPane()
                .getDescriptor();
    }
    
    @Override
    public boolean handleMouseEnter(Event event) {
        // TODO Auto-generated method stub
        return false;
    }
}
