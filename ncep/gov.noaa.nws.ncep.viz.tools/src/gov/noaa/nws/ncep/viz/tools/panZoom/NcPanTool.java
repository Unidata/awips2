package gov.noaa.nws.ncep.viz.tools.panZoom;

import gov.noaa.nws.ncep.viz.ui.display.AbstractNcModalTool;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.viz.ui.input.PanHandler;

/**
 * 
 * <pre>
 *  
 *   SOFTWARE HISTORY
 *  
 * Date         Ticket#         Engineer        Description
 * ------------ ----------      -----------     --------------------------
 * 09/29/09       #169        Greg Hull         Initial Creation.
 * 12/02/09                   Greg Hull         broke out from combined PanZoomTool
 * 03/19/11                   Greg Hull         Copied from PanTool, us NcPanHandler
 *   
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcPanTool extends AbstractNcModalTool {

    /** The mouse handler */
    private NcPanHandler panHandler;

    @Override
    protected void activateTool() {
        if (panHandler == null) {
            panHandler = new NcPanHandler(this.editor);
        } else {
            panHandler.setContainer(editor);
        }
        editor.registerMouseHandler(panHandler, InputPriority.LOWEST);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
        if (editor != null)
            editor.unregisterMouseHandler(panHandler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#setEditor(com.raytheon.viz.ui.
     * editor.AbstractEditor)
     */
    @Override
    public void setEditor(IDisplayPaneContainer editor) {
        super.setEditor(editor);
        if (panHandler == null) {
            panHandler = new NcPanHandler(editor);
        }
        panHandler.setContainer(editor);
    }


//    private final IInputHandler inputHandler;
//    
//    protected   int prevButton = 0;
//    protected NCDisplayPane prevPane = null;
//    
//    protected int prevMouseX = 0;
//    protected int prevMouseY = 0;
//
//    protected int mouseDownX = 0;
//    protected int mouseDownY = 0;
//
//    
//    public NcPanTool() {
//        super();
//        
//        // Create an input handler to 
//        inputHandler = new InputHandlerDefaultImpl() {
//            @Override
//            public boolean handleMouseDown(int x, int y, int button) {
//                prevButton = button;
//                mouseDownX = x;
//                mouseDownY = y;
//                prevMouseX = x;
//                prevMouseY = y;
//                if( mapEditor != null )
//                	prevPane = (NCDisplayPane) mapEditor.getActiveDisplayPane();
//                
//                if( button != 1 ) {
//                    return false;
//                }
//
//                return false;
//            }
//
//            public boolean handleMouseDownMove(int x, int y, int button) {
//                if( button != 1 ) {
//                    prevMouseX = x;
//                    prevMouseY = y;
//                    return false;
//                }
//                if( mapEditor != null ){
//                	IDisplayPane[] panes = ( mapEditor.arePanesGeoSynced() ?
//                			mapEditor.getDisplayPanes() : mapEditor.getSelectedPanes() );
//
//                	for( IDisplayPane p : panes ) {
//                		p.shiftExtent(new double[] { x, y }, new double[] {
//                				prevMouseX, prevMouseY });
//                	}
//
//                	mapEditor.refresh();
//                }
//                
//                prevMouseX = x;
//                prevMouseY = y;
//                prevButton = button;
//                return true;
//            }
//
//            @Override
//            public boolean handleMouseUp(int x, int y, int button ) {
//                if( button != 1 ) {
//                    return false;
//                }
//                if( mouseDownX == x && mouseDownY == y ) {
//                    prevMouseX = x;
//                    prevMouseY = y;
//                        prevButton = button;
//                    return false;                
//                }
//
////                IDisplayPane[] panes = ( mapEditor.arePanesGeoSynced() ?
////                		mapEditor.getDisplayPanes() : mapEditor.getSelectedPanes() );
//
//                // TODO : Raytheon's panHandler had code to zoom(.05) when the middle 
//                // mouse button is clicked. This was not implemented for us.
//                // nothing to do.
//                if( mapEditor != null )
//                	mapEditor.refresh();
//                return true;
//            }
//            
//            // nothing to do on mouseMove, mouseHover, mouseWheel, KeyUp, keyDown or doubleClick
//        };
//    }
//
//    @Override
//    protected void activateTool() {
//
//    	super.activateTool();
//    	
//        if( mapEditor != null )
//        	mapEditor.registerMouseHandler(inputHandler);
//
//    }
//
//    @Override
//    public void deactivateTool() {
//        if( mapEditor != null ) {
//            mapEditor.unregisterMouseHandler(inputHandler);
//        }
//    }
//    
//    // we may need to do this if the buttons become pushbuttons but as toggles it seems we 
//    // don't need to refresh the gui elements. executing a command seems to update the display
//    @Override
//    public void updateElement(UIElement element, Map parameters) {
//    	super.updateElement(element, parameters);    
//    	//element.setChecked(this.isEnabled);
//    }

}
