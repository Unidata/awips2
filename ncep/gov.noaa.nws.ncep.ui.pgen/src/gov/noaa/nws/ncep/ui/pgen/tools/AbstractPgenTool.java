/*
 * gov.noaa.nws.ncep.ui.pgen.tools.AbstractPgenTool
 * 
 * 29 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.AbstractNCModalMapTool;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * The abstract super class for all PGEN tools.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/09					B. Yin   	Initial Creation.
 * 05/09		79			B. Yin		Added 'delete obj' flag
 * 										Set the flag in the execute method
 * 29/09        169         Greg Hull   Use AbstractNCModalMapTool
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

  public abstract class AbstractPgenTool extends AbstractModalTool{
  //public abstract class AbstractPgenTool extends AbstractNCModalMapTool{
	
//	protected String buttonName = null;
	protected AbstractEditor mapEditor = null;
	
	protected String buttonName = null;
	private static boolean delObjFlag;
	private IInputHandler inputHandler = null;
	
	/**
	 * A handler to the current drawing layer.
	 */
    protected PgenResource drawingLayer;
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {
    	if ( PgenSession.getInstance().getPgenPalette() == null ) return;
//    	super.activateTool(event);
//    	super.activateTool();
    	
        if( editor instanceof AbstractEditor ) {
        	this.mapEditor = (AbstractEditor)super.editor;
        }

    	
    	String param;
    	param = event.getParameter("name");
    	if ( param != null ) buttonName = param;

    	//  Set "active" icon for the palette button corresponding to this tool
    	PgenSession.getInstance().getPgenPalette().setActiveIcon(buttonName);
    	
    	// Get a PGEN Resource
        drawingLayer = PgenSession.getInstance().getPgenResource();
    	
        if ( this instanceof PgenDeleteObj ){
        	delObjFlag = true;
        }
        else if ( (this instanceof PgenSelectingTool ) || 
        		 !(this instanceof AbstractPgenDrawingTool) ){
        	delObjFlag = false;
        }
        
        /*
         * load appropriate input handler
         */
        if ( this.inputHandler != null ) {
            mapEditor.unregisterMouseHandler( this.inputHandler );
        }

        this.inputHandler = getMouseHandler();
        if ( this.inputHandler != null ) mapEditor.registerMouseHandler( this.inputHandler );
        
        //  Turn off, so tool doesn't exihibit toggle behavior
        setEnabled(false);
    }
    
    abstract public IInputHandler getMouseHandler();
    
    /**
     * Clean up: remove ghost line and handle bars.
     */
    public void deactivateTool() {
    	
    	//  Reset the original icon for the palette button corresponding to this tool
    	if ( buttonName != null && PgenSession.getInstance().getPgenPalette() != null) 
    		PgenSession.getInstance().getPgenPalette().resetIcon(buttonName);
    	
    	if ( drawingLayer != null ) {
    		
    		drawingLayer.removeGhostLine();
    		drawingLayer.removeSelected();
    		PgenUtil.refresh();
    		
    	}
    	
        if (mapEditor != null && this.inputHandler != null)
            mapEditor.unregisterMouseHandler( this.inputHandler );
        
    }
    
    /**
     * Check if the 'delete obj' flag is set
     * @return the 'delete obj' flag
     */
    protected boolean isDelObj(){
    	
    	return delObjFlag;
    	
    }

    /**
     * Get the PGEN resource
     * @return
     */
	public PgenResource getDrawingLayer() {
		return drawingLayer;
	}

	/**
	 * Set the PGEN resource
	 * @param drawingLayer
	 */
	public void setDrawingLayer(PgenResource drawingLayer) {
		this.drawingLayer = drawingLayer;
	}
	
	/**
	 * Check if the PGEN resource is editable.
	 * @return
	 */
	protected boolean isResourceEditable(){
		if ( drawingLayer == null ) return false;
		else return drawingLayer.isEditable();
	}
}
