/*
 * gov.noaa.nws.ncep.ui.pgen.action
 * 
 * 20 March 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.ui.pgen.action;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.tools.AbstractPgenDrawingTool;
import gov.noaa.nws.ncep.ui.pgen.tools.AbstractPgenTool;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.jface.action.Action;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * This class is to perform the PGEN contextual menu actions.
 *  
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/13		927			B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenAction extends Action {
	
	private String actionName;
	
	/**
	 * Public constructor
	 * @param name - action name
	 */
	public PgenAction(String name ){
		super(name);
		actionName = name;
	}

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
    	doPgenAction( actionName );
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return actionName;
    }
    

    /**
     * This method gets the current PGEN drawing tool and the action handler from the action name. 
     * Then creates an instance of the handler and set the handler for the current PGEN tool.
     * @param actionName
     */
	private void doPgenAction( String actionName ){
		try {
			AbstractPgenDrawingTool pgenTool = null;
			AbstractVizPerspectiveManager mgr = VizPerspectiveListener.getCurrentPerspectiveManager();
			 for ( AbstractModalTool tool : mgr.getToolManager().getSelectedModalTools() ) {
				 if ( tool instanceof AbstractPgenDrawingTool ){
						 pgenTool = (AbstractPgenDrawingTool)tool;
					 }
					 break;
			 }
			 
			 if ( pgenTool != null ){
				IConfigurationElement ice = PgenSession.getInstance().getPgenPalette().getItemMap().get( actionName );
				 Object obj = Class.forName(ice.getAttribute("actionHandlerClass") ).
				 					getConstructor( AbstractPgenTool.class).newInstance( pgenTool );
				 
				 if ( obj != null && obj instanceof IInputHandler ){
					 pgenTool.setHandler( (IInputHandler) obj );
				 }
			 }
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}

}
