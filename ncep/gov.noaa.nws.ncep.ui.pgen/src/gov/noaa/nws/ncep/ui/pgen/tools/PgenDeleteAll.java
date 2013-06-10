/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenDeleteAll
 * 
 * 23 March 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Implements PGEN "Delete All" function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/09					B. Yin   	Initial Creation.
 * 04/09            72      S. Gilbert  Modified to use PgenSession and PgenCommands
 * 04/09			103		B. Yin		Extends from AbstractPgenTool
 * 07/09			131		J. Wu		Modify to work on the active layer only
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenDeleteAll extends AbstractPgenTool {
	
    public PgenDeleteAll(){
    	
    	super();
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {

    	super.activateTool();
    	if ( !isResourceEditable() ) return;

        /*
         * Confirm "delete All" request
         */
    	String msg = "Are you sure you want to delete all?";   	
    	
    	String name = PgenSession.getInstance().getPgenResource().getActiveLayer().getName();
    	if ( !name.equalsIgnoreCase("Default") ) {
    	    msg = "Are you sure you want to delete all on Layer - " + name + "?";   	
    	}
        
    	MessageDialog confirmDlg = new MessageDialog( 
        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
        		"Confirm Delete", null, msg,
        		MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
        confirmDlg.open();
        
        if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
        
        	//drawingLayer.removeAllProducts();
        	drawingLayer.removeAllActiveDEs();
        	editor.refresh();
        	
        }
        
        //NmapUiUtils.setPanningMode();
        PgenUtil.setSelectingMode();
        
    }

	@Override
	public IInputHandler getMouseHandler() {
		return null;
	}

}