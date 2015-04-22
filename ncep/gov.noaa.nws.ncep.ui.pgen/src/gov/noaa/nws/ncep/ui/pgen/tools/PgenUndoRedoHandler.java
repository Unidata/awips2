/*
 * PgenUndoRedoHandler
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.tools;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * This Command Handler is used to request that the PgenCommandManager execute an
 * "undo" or a "redo" of the previous command.
 * @author sgilbert
 *
 */
public class PgenUndoRedoHandler extends AbstractTool {

	/**
	 * This method is sends either an "undo" or "redo" request to the current PgenCommandManager
	 * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		if ( PgenSession.getInstance().getPgenResource() != null &&
				PgenSession.getInstance().getPgenResource().isEditable() ) {
			if ( event.getApplicationContext().equals("Undo")) 
				PgenSession.getInstance().getCommandManager().undo();

			else if ( event.getApplicationContext().equals("Redo") ) 
				PgenSession.getInstance().getCommandManager().redo();

			else{
				if(event.getParameter("action") != null && !event.getParameter("action").isEmpty()){
					String actionToDo = new String(event.getParameter("action"));
					if(actionToDo.equals("Undo")){
						PgenSession.getInstance().getCommandManager().undo();
					}else if(actionToDo.equals("Redo")){
						PgenSession.getInstance().getCommandManager().redo();
					}
				}
			}
			PgenUtil.refresh();

			//de-activate drawing tools 
			PgenUtil.setSelectingMode();	
		}
		return null;
	}

}
