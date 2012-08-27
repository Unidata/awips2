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
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;

import java.io.IOException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;
import com.raytheon.viz.ui.panes.VizDisplayPane;

public class NsharpAbstractMouseHandler extends InputHandlerDefaultImpl{//implements IInputHandler {

	protected enum Mode {
        CREATE, SKEWT_DOWN, HODO_DOWN, HODO_DOWN_MOVE, TIMELINE_DOWN, STATIONID_DOWN, PARCELLINE_DOWN, HODO_WIND_MOTION_DOWN
    };
    protected static Display display;
    protected NsharpEditor editor;
    public void setEditor(NsharpEditor editor) {
		this.editor = editor;
	}
    protected Mode mode = Mode.CREATE;
    protected boolean cursorInPane=false;
    protected double[] anchorPointxy;
    protected VizDisplayPane currentPane;
    protected MousePreferenceManager prefManager = MousePreferenceManager.getInstance();
    protected static final String PAN_PREF = "com.raytheon.viz.ui.input.pan";
    protected static final String PAN_PERCENTAGE = "panningPercentage";
    protected static final String ZOOMIN_PREF = "com.raytheon.viz.ui.input.zoomin";
    protected static final String ZOOMOUT_PREF = "com.raytheon.viz.ui.input.zoomout";
    protected static final String CLEAR_PREF = "com.raytheon.viz.ui.clear";
    protected float theLastMouseX = 0;
    protected float theLastMouseY = 0;
    protected Double panningPercentage = null;
    protected boolean shiftDown=false;
    protected boolean zDownWhileShiftDown=false;
    protected int KEY_Z= 122;
    
    protected double getPanningPercentage() {
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
    public NsharpAbstractMouseHandler(NsharpEditor editor, IDisplayPane pane) {
        this.editor = editor;
        display = Display.getCurrent();
        currentPane = (VizDisplayPane)pane;
    }
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseWheel(int, int)
     */
    public boolean handleMouseWheel(Event event, int x, int y) {
    	if(editor == null || cursorInPane == false) {
            return false;
        }
    	//if set to false, preempt other handler, so zooming is disabled in SkewT editor.
        com.raytheon.viz.ui.input.preferences.MouseEvent SCROLL_FORWARD = com.raytheon.viz.ui.input.preferences.MouseEvent.SCROLL_FORWARD;
        com.raytheon.viz.ui.input.preferences.MouseEvent SCROLL_BACK = com.raytheon.viz.ui.input.preferences.MouseEvent.SCROLL_BACK;
        if ((event.stateMask & SWT.SHIFT) == 0
                && editor.translateClick(x, y) != null && currentPane != null) {
            if ((event.count < 0 && prefManager.handleEvent(ZOOMIN_PREF,
                    SCROLL_FORWARD))
                    || (event.count > 0 && prefManager.handleEvent(
                            ZOOMOUT_PREF, SCROLL_BACK))) {
            	currentPane.zoom(event.count, event.x, event.y);
                return true;
            } else if ((event.count > 0 && prefManager.handleEvent(
                    ZOOMOUT_PREF, SCROLL_FORWARD))
                    || (event.count < 0 && prefManager.handleEvent(ZOOMIN_PREF,
                            SCROLL_BACK))) {
            	currentPane.zoom(-event.count, event.x, event.y);
                return true;
            } else if ((event.count > 0 && prefManager.handleEvent(CLEAR_PREF,
                    SCROLL_FORWARD))
                    || (event.count < 0 && prefManager.handleEvent(CLEAR_PREF,
                            SCROLL_BACK))) {
            	currentPane.clear();
                return true;
            }
        }
        return false;
   }

    @Override
    public boolean handleDoubleClick(int x, int y, int button) {
        return false;
    }

    /*
    @Override
    public boolean handleKeyUp(int keyCode) {
    	//String s = "key up="+(char)keyCode;
    	//System.out.println(s+ " code ="+keyCode);
    	if (getPaneDisplay() == null) {
    		return false;
    	}
    	NsharpAbstractPaneResource paneRsc = getDescriptor().getPaneResource();
    	if (keyCode == SWT.ARROW_DOWN)  {
            paneRsc.getRscHandler().setSteppingStnIdList(FrameChangeOperation.NEXT) ;
            return true;
        }else if (keyCode == SWT.ARROW_UP)  {
            //System.out.println("Arrow up");
        	paneRsc.getRscHandler().setSteppingStnIdList(FrameChangeOperation.PREVIOUS) ;
            return true;
        }else if (keyCode == SWT.ARROW_LEFT)  {
            //System.out.println("Arrow left");
        	paneRsc.getRscHandler().setSteppingTimeLine(FrameChangeOperation.PREVIOUS, FrameChangeMode.TIME_ONLY) ;
            return true;
        }else if (keyCode == SWT.ARROW_RIGHT)  {
            //System.out.println("Arrow right");
            paneRsc.getRscHandler().setSteppingTimeLine(FrameChangeOperation.NEXT, FrameChangeMode.TIME_ONLY) ;
            return true;
        } 
        return false;
    }*/

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
    	if(editor != null &&  mouseButton== 1) {
    		//editor.setFocus();
    		editor.setSelectedPane(currentPane);
        }
		cursorInPane=true;
		
		if (!prefManager.handleDrag(PAN_PREF, mouseButton)
                && !prefManager.handleClick(ZOOMIN_PREF, mouseButton)
                && !prefManager.handleClick(ZOOMOUT_PREF, mouseButton))
            return false;
        
        theLastMouseX = x;
        theLastMouseY = y;
        return false;
    }
    @Override
    public boolean handleMouseDownMove(int aX, int aY, int button) { 	
    	if (getPaneDisplay() == null || editor == null) {
    		return false;
    	}
    	else if (button == 1) {
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
    		return true;

    	}
    	return false;
    }

    @Override
    public boolean handleMouseHover(int x, int y) {
        //System.out.println("mouseHandler handleMouseHover");
    	if(editor != null) {
         	editor.setFocus();
    	}
        return true;
    }
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
    	if (getPaneDisplay() == null) {
    		return false;
    	}
    	if(editor!=null){
    		// button 1 is left mouse button 
    		if(mouseButton == 3){
    			//right mouse button
    			//System.out.println("skewtRsc handleMouseUp right button");
    			NsharpMapResource.bringMapEditorToTop();
    		}
    		editor.refresh();
    	}
    	return false;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        return false;
    }

    @Override
	public boolean handleMouseExit(Event event) {
 		//System.out.println("handleMouseExit pane="+ getPaneDisplay().getPaneName());
 		cursorInPane=false;    
 		return false;
	}
    @Override
    public boolean handleMouseEnter(Event event) {
    	//System.out.println("handleMouseEnter pane="+ getPaneDisplay().getPaneName());
    	if(editor != null ) {
    		editor.setFocus();
    		editor.setSelectedPane(currentPane);
        }
		cursorInPane=true;
        return false;
    }

    //private void notifyResourceMiddleClicked(int x, int y) {
    //}

    /**
     * Gets the display you are using
     * 
     * @return SkewTDisplay
     */
    public NsharpAbstractPaneDisplay getPaneDisplay() {
    	return((NsharpAbstractPaneDisplay) currentPane.getRenderableDisplay());
    }

    public NsharpAbstractPaneDescriptor getDescriptor() {
    	return((NsharpAbstractPaneDescriptor) currentPane.getDescriptor());
    }
    
}
